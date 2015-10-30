library(dplyr)
library(reshape2)

setwd('C:/Users/cloud/Desktop/数据分析/工业数据库/工业企业')
load("all_data.RData")

#原始数据生成-----------------------

##PPI数据生成
ppi <- read.csv("ppi.csv",header=T,stringsAsFactors=F)
ppi <- ppi%>%select(-area)%>%melt(id.vars='province',value.name='ppi',variable.name='year')
ppi$year <- as.numeric(substr(ppi$year,2,5))


data <- transmute(subset(data_2004,行政区代码>1e+10),code=法人代码,
                       name=法人单位,out_s=工业产值当,
                       output=工业产值当-中间投入+应交增值税,
                       worker=全部从业人-研究生-本科-专科,
                       skill=研究生+本科+专科,
                       capital=固定资产净,province=as.integer(行政区代码%/%1e+10))

###分类及删除无效数据
data$scale <- '0'
data$scale[data$out_s>=400000 & data$worker>=1000] <- "l"
data$scale[with(data,out_s>=20000 & worker>=300 & scale!='l')] <- "m"
data$scale[with(data,out_s>=3000 & worker>=20 & !scale%in%c('l','m'))] <- "s"
##54为西藏，46为海南
data <- subset(data,scale!='0' & capital>1 & output>1 & province!=54 & province!=46 & skill>0)
data$code <- tolower(data$code)
data <- arrange(data,code)

data <- select(data,-out_s)
data$output <- as.numeric(data$output)
data$worker <- as.numeric(data$worker)
data$capital <- as.numeric(data$capital)
data$year <- 2004

rm(list=paste("data",1998:2007,sep="_"));gc(T)
ppi_back <- ppi
data_back <- data

#最优模型代入----------------
ppi <- ppi_back
data <- data_back

##部门的累计
scale_sum <- data%>%group_by(year,province,scale)%>%
  summarise(output=sum(output),worker=sum(worker),skill=sum(skill),capital=sum(capital))

##求p,pi,pij
alp <- 1/3
phi <- 1/3;nphi <- 1-phi
sig <- 2/3;nsig <- 1-sig

##p即全国的ppi
p <- subset(ppi,province==0)
p <- select(p,-province)

##pi为省的ppi
pi <- subset(ppi,province!=0)

##计算每个部门的ppi
scale_sum <- merge(scale_sum,pi,by=c("province","year"))
scale_sum <- scale_sum%>%group_by(province,year)%>%
  mutate(ppi=ppi*(sum(output)/output)^(phi/nphi))
rm(ppi)


##求取每个部门的实际变量
scale_sum$output_c <- scale_sum$output/scale_sum$ppi

##每个省的加总
province_sum <- scale_sum%>%group_by(year,province)%>%
  summarise(output_c=sum(output_c^nphi)^(1/nphi),worker=sum(worker),skill=sum(skill),
            capital=sum(capital))
province_sum <- merge(province_sum,pi,by=c("year","province"))
rm(pi)

#计算权重w
province_sum <- province_sum%>%group_by(year)%>%
  mutate(weight=ppi*(output_c^sig)/sum(ppi*(output_c^sig)))


##全国加总
total_sum <- province_sum%>%group_by(year)%>%
  summarise(output_c=sum(weight*(output_c^nsig))^(1/nsig),
            worker=sum(worker),skill=sum(skill),capital=sum(capital))

total_sum <- merge(total_sum,p,by="year")
rm(p)

##计算全国,省，规模的TFP
tfp_cal <- function(x) with(x,output_c/((worker^alp)*(skill^alp)*(capital^alp)))
scale_sum$tfp <- tfp_cal(scale_sum)
province_sum$tfp <- tfp_cal(province_sum)
total_sum$tfp <- tfp_cal(total_sum)

##计算有效的TFP
###省际
province_sum <- scale_sum%>%group_by(province,year)%>%
  summarise(etfp=sum(tfp^(nphi/phi))^(phi/nphi))%>%
  merge(province_sum,.,by=c("province","year"))

###全国
total_sum <- province_sum%>%group_by(year)%>%
  summarise(etfp=sum((weight^(1/sig))*(etfp^(nsig/sig)))^(sig/nsig))%>%
  merge(total_sum,.,by="year")

#扭曲分解模型--------------------------------
##扭曲分解
scale_sum <- scale_sum%>%
  mutate(tile_l=ppi*output_c/worker,
         tile_k=ppi*output_c/capital,
         tile_s=ppi*output_c/skill,
         tile_a=tfp/((tile_l^alp)*(tile_s^alp)*(tile_k^alp)))

temp <- scale_sum%>%group_by(year,province)%>%
  summarise(tile_an=sum(tile_a^(nphi/phi))^(phi/nphi),
            tile_l=sum(tile_a^(nphi/phi)/tile_l),
            tile_k=sum(tile_a^(nphi/phi)/tile_k),
            tile_s=sum(tile_a^(nphi/phi)/tile_s))

temp <- rename(temp,tile_a=tile_an)
temp <- temp%>%mutate(tile_l=tile_a^(nphi/phi)/tile_l,
                      tile_k=tile_a^(nphi/phi)/tile_k,
                      tile_s=tile_a^(nphi/phi)/tile_s)
province_sum <- merge(temp,province_sum,by=c("year","province"))

# province_sum <- province_sum%>%group_by(year)%>%
#   transmute(tile_y=with(total_sum[total_sum$year==unique(year),],ppi*output_c^sig)*
#            (weight*output_c^(-sig)/ppi))

for(i in unique(province_sum$year))
{
  ch=with(province_sum,year==i)
  province_sum$tile_y[ch]=with(subset(total_sum,year==i),ppi*output_c^sig)*with(province_sum[ch,],weight*output_c^(-sig)/ppi)
}
rm(ch,i,temp)

with(province_sum,tile_a*(tile_l^alp)*(tile_s^alp)*(tile_k^alp)-tfp)

## 独立生成tile_y的方法
# temp=sapply(province_sum$year,function(x) which(x==total_sum$year))
# province_sum$pt_y=with(total_sum[temp,],ppi*output_c^sig)*with(province_sum,weight*output_c^(-sig)/ppi)

temp <- province_sum%>%group_by(year)%>%
  summarise(tile_an=sum(weight^(1/sig)*(tile_a/tile_y)^(nsig/sig))^(sig/nsig),
            tile_l=sum(tile_a^(nsig/sig)*(weight/tile_y)^(1/sig)/tile_l),
            tile_k=sum(tile_a^(nsig/sig)*(weight/tile_y)^(1/sig)/tile_k),
            tile_s=sum(tile_a^(nsig/sig)*(weight/tile_y)^(1/sig)/tile_s))
temp <- rename(temp,tile_a=tile_an)
temp <- temp%>%
  mutate(tile_l=tile_a^(nsig/sig)/tile_l,
         tile_k=tile_a^(nsig/sig)/tile_k,
         tile_s=tile_a^(nsig/sig)/tile_s)

total_sum <- merge(temp,total_sum,by="year")
rm(temp)
with(total_sum,tile_a*(tile_l^alp)*(tile_s^alp)*(tile_k^alp)-tfp)

##消除扭曲
###通过消除省内的扭曲，即损失由省际造成。

##此处有问题，函数内设定alp等参数似乎无用
scale_dis=scale_nia;province_dis=province_sum;total_dis=total_sum
remove_ni=function(scale_dis,province_dis,total_dis)
{
  temp <- scale_dis%>%group_by(year,province)%>%
    summarise(tile_an=sum(tile_a^(nphi/phi))^(phi/nphi),
              tile_l=sum(tile_a^(nphi/phi)/tile_l),
              tile_k=sum(tile_a^(nphi/phi)/tile_k),
              tile_s=sum(tile_a^(nphi/phi)/tile_s))
  
  temp <- rename(temp,tile_a=tile_an)
  temp <- temp%>%
    mutate(tile_l=tile_a^(nphi/phi)/tile_l,
           tile_k=tile_a^(nphi/phi)/tile_k,
           tile_s=tile_a^(nphi/phi)/tile_s)
  
  province_dis <- merge(temp,select(province_dis,-tile_a,-tile_l,-tile_k,-tile_s),
                        by=c("year","province"))
  for(i in unique(province_dis$year))
  {
    ch <- with(province_dis,year==i)
    province_dis$tile_y[ch] <- with(subset(total_sum,year==i),ppi*output_c^sig)*with(province_dis[ch,],weight*output_c^(-sig)/ppi)
  }
  
  total_dis <- province_dis%>%group_by(year)%>%
    summarise(tile_an=sum(weight^(1/sig)*(tile_a/tile_y)^(nsig/sig))^(sig/nsig),
              tile_l=sum(tile_a^(nsig/sig)*(weight/tile_y)^(1/sig)/tile_l),
              tile_k=sum(tile_a^(nsig/sig)*(weight/tile_y)^(1/sig)/tile_k),
              tile_s=sum(tile_a^(nsig/sig)*(weight/tile_y)^(1/sig)/tile_s))
  
  total_dis <- rename(total_dis,tile_a=tile_an)
  total_dis <- total_dis%>%
    mutate(tile_l=tile_a^(nsig/sig)/tile_l,
           tile_k=tile_a^(nsig/sig)/tile_k,
           tile_s=tile_a^(nsig/sig)/tile_s)
  with(total_dis,tile_a*(tile_l^alp)*(tile_s^alp)*(tile_k^alp))
}

scale_nia <- scale_sum%>%group_by(province,year)%>%
  mutate(tile_l=sum(tile_l*worker)/sum(worker),
         tile_k=sum(tile_k*capital)/sum(capital),
         tile_s=sum(tile_s*skill)/sum(skill))

scale_nik <- scale_sum%>%group_by(province,year)%>%
  mutate(tile_k=sum(tile_k*capital)/sum(capital))

scale_nil <- scale_sum%>%group_by(province,year)%>%
  mutate(tile_l=sum(tile_l*worker)/sum(worker))

scale_nis <- scale_sum%>%group_by(province,year)%>%
  mutate(tile_s=sum(tile_s*skill)/sum(skill))

scale_nia$tile_a <- with(scale_nia,{tfp/((tile_l^alp)*(tile_s^alp)*(tile_k^alp))})
scale_nik$tile_a <- with(scale_nik,{tfp/((tile_l^alp)*(tile_s^alp)*(tile_k^alp))})
scale_nil$tile_a <- with(scale_nil,{tfp/((tile_l^alp)*(tile_s^alp)*(tile_k^alp))})
scale_nis$tile_a <- with(scale_nis,{tfp/((tile_l^alp)*(tile_s^alp)*(tile_k^alp))})


total_sum$tfp_nia <- remove_ni(scale_nia,province_sum,total_sum)
total_sum$tfp_nik <- remove_ni(scale_nik,province_sum,total_sum)
total_sum$tfp_nil <- remove_ni(scale_nil,province_sum,total_sum)
total_sum$tfp_nis <- remove_ni(scale_nis,province_sum,total_sum)

rm(scale_nia,scale_nik,scale_nil,remove_ni)

###消除省际摩擦，即剩余损失为省内造成
remove_ib=function(province_dis)
{
  total_dis <- province_dis%>%group_by(year)%>%
    summarise(tile_an=sum(weight^(1/sig)*(tile_a/tile_y)^(nsig/sig))^(sig/nsig),
              tile_l=sum(tile_a^(nsig/sig)*(weight/tile_y)^(1/sig)/tile_l),
              tile_k=sum(tile_a^(nsig/sig)*(weight/tile_y)^(1/sig)/tile_k),
              tile_s=sum(tile_a^(nsig/sig)*(weight/tile_y)^(1/sig)/tile_s))
  total_dis <- rename(total_dis,tile_a=tile_an)
  total_dis <- mutate(total_dis,
                      tile_l=tile_a^(nsig/sig)/tile_l,
                      tile_k=tile_a^(nsig/sig)/tile_k,
                      tile_s=tile_a^(nsig/sig)/tile_s)
  with(total_dis,tile_a*(tile_l^alp)*(tile_s^alp)*(tile_k^alp))
}
province_nba <- province_sum%>%group_by(year)%>%
  mutate(temp_a=tile_a*(tile_l^alp)*(tile_s^alp)*(tile_k^alp),
         tile_l=sum(tile_l*worker)/sum(worker),
         tile_k=sum(tile_k*capital)/sum(capital),
         tile_s=sum(tile_s*skill)/sum(skill),
         tile_a=temp_a/((tile_l^alp)*(tile_s^alp)*(tile_k^alp)))

province_nbl <- province_sum%>%group_by(year)%>%
  mutate(temp_a=tile_a*(tile_l^alp)*(tile_s^alp)*(tile_k^alp),
         tile_l=sum(tile_l*worker)/sum(worker),
         tile_a=temp_a/((tile_l^alp)*(tile_s^alp)*(tile_k^alp)))

province_nbk <- province_sum%>%group_by(year)%>%
  mutate(temp_a=tile_a*(tile_l^alp)*(tile_s^alp)*(tile_k^alp),
         tile_k=sum(tile_k*capital)/sum(capital),
         tile_a=temp_a/((tile_l^alp)*(tile_s^alp)*(tile_k^alp)))

province_nbs <- province_sum%>%group_by(year)%>%
  mutate(temp_a=tile_a*(tile_l^alp)*(tile_s^alp)*(tile_k^alp),
         tile_s=sum(tile_s*skill)/sum(skill),
         tile_a=temp_a/((tile_l^alp)*(tile_s^alp)*(tile_k^alp)))
### 此处没有调整tile_y，无摩擦tile_y取PY^\sig
#province_dis$tile_y[ch]=with(subset(total_dis,year==i),ppi*output_c^sig)*with(province_dis[ch,],weight*output_c^(-sig)/ppi)

total_sum$tfp_nba <- remove_ib(province_nba)
total_sum$tfp_nbk <- remove_ib(province_nbk)
total_sum$tfp_nbl <- remove_ib(province_nbl)
total_sum$tfp_nbs <- remove_ib(province_nbs)

#添加省份标记----------------------
east <- c('北京','天津','河北','辽宁','上海','江苏','浙江','福建','山东','广东','海南')
middle <- c('黑龙江','吉林','山西','安徽','江西','河南','湖北','湖南')
west <- c('四川','重庆','贵州','云南','西藏','陕西','甘肃','青海','宁夏','新疆','广西','内蒙古')
emw <- data.frame(area=c(east,middle,west),
                  emw=c(rep("east",length(east)),rep("middle",length(middle)),
                        rep("west",length(west))))
rm(east,middle,west)

temp <- read.csv('province_code.csv',header=T,stringsAsFactors=F)%>%
  merge(emw,by='area')

data_s <- select(data,province,output,worker,capital,year,scale)%>%merge(temp,by='province')
scale_sum_s <- merge(scale_sum,temp,by='province')
province_sum_s <- merge(province_sum,temp,by='province')
total_sum_s <- total_sum

##保存数据
rm(list=ls()[!ls()%in%c('data_s','scale_sum_s','province_sum_s','total_sum_s')]);gc(T)
save.image("result_skill.RData")

# ##测试
# 
# scale_dis=mutate(group_by(scale_sum,province,year),
#                  tile_l=sum(tile_l*worker)/sum(worker),
#                  tile_k=sum(tile_k*capital)/sum(capital))
# scale_dis$tile_a=with(scale_dis,{tfp/((tile_l^alp)*(tile_k^nalp))})
# temp=summarise(group_by(scale_dis,year,province),
#                tile_an=sum(tile_a^(nphi/phi))^(phi/nphi),
#                tile_l=sum(tile_a^(nphi/phi)/tile_l),
#                tile_k=sum(tile_a^(nphi/phi)/tile_k))
# temp=rename(temp,tile_a=tile_an)
# temp=mutate(temp,
#             tile_l=tile_a^(nphi/phi)/tile_l,
#             tile_k=tile_a^(nphi/phi)/tile_k)
# province_dis=merge(temp,select(province_sum,-tile_a,-tile_l,-tile_k),by=c("year","province"))
# for(i in unique(province_dis$year))
# {
#   ch=with(province_dis,year==i)
#   province_dis$tile_y[ch]=with(subset(total_sum,year==i),ppi*output_c^sig)*with(province_dis[ch,],weight*output_c^(-sig)/ppi)
# }
# with(province_dis,tile_a*(tile_l^alp)*(tile_k^nalp)-etfp)
# 
# province_dis=mutate(group_by(province_dis,year),
#                     temp_a=tile_a*(tile_l^alp)*(tile_k^nalp),
#                     tile_l=sum(tile_l*worker)/sum(worker),
#                     tile_k=sum(tile_k*capital)/sum(capital),
#                     tile_a=temp_a/((tile_l^alp)*(tile_k^nalp)))
# 
# 
# total_dis=summarise(group_by(province_dis,year),
#                     tile_an=sum(weight^(1/sig)*(tile_a/tile_y)^(nsig/sig))^(sig/nsig),
#                     tile_l=sum(tile_a^(nsig/sig)*(weight/tile_y)^(1/sig)/tile_l),
#                     tile_k=sum(tile_a^(nsig/sig)*(weight/tile_y)^(1/sig)/tile_k))
# total_dis=rename(total_dis,tile_a=tile_an)
# total_dis=mutate(total_dis,tile_l=tile_a^(nsig/sig)/tile_l,tile_k=tile_a^(nsig/sig)/tile_k)
# 
# with(total_dis,tile_a*(tile_l^alp)*(tile_k^nalp))-total_sum$etfp


# 企业的合并（效果不佳）
# ##按照相同名称的企业重新分为一组
# name_code <- data%>%group_by(name,code)%>%summarise(times=length(name))
# dup_name <- name_code%>%group_by(name)%>%summarise(times=length(name))%>%subset(times>1)
# dup_name <- subset(name_code,name%in%dup_name$name)
# 
# ##建立对应规则
# rule <- dup_name%>%group_by(name)%>%do(data.frame(init=.$code[1],substi=.$code[-1]))%>%data.frame
# 
# ##消除重复对应的规则
# dup_rule <- rule[rule$init%in%rule$substi,]
# for(i in unique(dup_rule$init))
# {
#   rule$init[rule$init==i] <- (rule$init[rule$substi==i])[1]
# }
# 
# rule <- rule[,2:3]%>%unique
# ##部分法人代码是在substi中重复出现的，因此跟init交换
# dup_rule <- rule$substi[duplicated(rule$substi)]%>%unique
# for(i in dup_rule)
# {
#   temp <- rule$init[rule$substi==i]
#   rule$init[rule$substi==i] <- rule$substi[rule$substi==i]
#   rule$substi[rule$substi==i] <- temp
# }
# 
# ##再次消除重复对应的规则
# dup_rule <- rule[rule$init%in%rule$substi,]
# for(i in unique(dup_rule$init))
# {
#   rule$init[rule$init==i] <- (rule$init[rule$substi==i])[1]
# }
# 
# ##按照规则修正分类
# temp_d1 <- subset(data,code%in%rule$substi)
# temp_d2 <- subset(data,!code%in%rule$substi)
# temp_d1 <- temp_d1%>%group_by(code)%>%mutate(ncode = subset(rule,substi==unique(code))$init[1])
# 
# ad_data <- rbind(temp_d1,data.frame(temp_d2,ncode=temp_d2$code))
# 
# ## 检验
# name_code <- ad_data%>%group_by(name,ncode)%>%summarise(times=length(name))
# name_code%>%group_by(name)%>%summarise(times=length(name))%>%subset(times>1)
# 
# 
# ##剩余样本中公司出现的次数不能超过10次（仅一家，因此直接删除）
# num <- ad_data%>%group_by(ncode)%>%summarise(times=length(code))
# # num%>%group_by(times)%>%summarise(number=length(times))%>%data.frame
# # subset(num,times==13)
# # subset(ad_data,ncode=='145405260')
# data <- subset(ad_data,!ncode%in%subset(num,times>10)$ncode)
# rm(num)
# 
# ##检测年份是否有重叠
# data%>%group_by(ncode)%>%do(subset(.,duplicated(.$year))) ->temp
# for(i in unique(data$ncode))
# {
#   if(duplicated(subset(data,ncode==i)$year)%>%sum>1)
#     break
# }
