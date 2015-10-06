library(dplyr)
library(maptools)
library(ggplot2)
library(reshape2)
library(grid)
setwd('C:/Users/cloud/Desktop/数据分析/工业数据库/工业企业')
load("result.RData")
##地图数据
mydat <- readShapePoly("C:/Users/cloud/Desktop/数据分析/中国地图信息/国家基础地理信息系统数据/国界与省界/bou2_4p.shp")
mysh <- fortify(mydat, region = 'NAME')## fortify可以转化为geom_map能使的函数
names(mysh)[1:2] <- c("x","y")
mysh$id <- substr(mysh$id,1,2)
mysh$id[mysh$id=='内蒙']='内蒙古'
mysh$id[mysh$id=='黑龙']='黑龙江'
###提取每个面积最大的部分作为中心坐标，coordinates能够提取中心
myname <- coordinates(mydat)%>%data.frame
myname$name <- mydat$NAME
myname$name <- substr(myname$name,1,2)
myname$name[myname$name=='内蒙']='内蒙古'
myname$name[myname$name=='黑龙']='黑龙江'
myname$area <- mydat$AREA
myname <- do.call('rbind',by(myname,myname$name,function(x)
  x[which.max(x$area),]))
myname$X1[myname$name=='甘肃'] <- 102
myname$X2[myname$name=='内蒙古'] <- 43.5
myname$X2[myname$name=='河北'] <- 38.3
myname <- subset(myname,!name%in%c('西藏','海南','台湾','香港'))

##部门间要素分布---------------------
factor_scale <- scale_sum%>%group_by(year,scale)%>%
  summarise(output=sum(output_c),worker=sum(worker),capital=sum(capital))

###图一 产出劳动力和资本
####产出（w=600,h=300）
ggplot(factor_scale,aes(x=year,y=log(output),linetype=scale))+geom_line(size=1.2)+
  ggtitle('不同规模下产出分布')+labs(y='log(产出)')+theme_classic()+
  scale_x_continuous(breaks=seq(1998,2007,by=2))+
  scale_linetype(name="企业规模",
                       breaks=c("l", "m", "s"),
                       labels=c("大型", "中型", "小型"))+
  theme(text=element_text(size=15),title=element_text(face='bold'))
####劳动力（w=600,h=300）
ggplot(factor_scale,aes(x=year,y=log(worker),linetype=scale))+geom_line(size=1.2)+
  ggtitle('不同规模下劳动力分布')+labs(y='log(劳动力)')+theme_classic()+
  scale_x_continuous(breaks=seq(1998,2007,by=2))+
  scale_linetype(name="企业规模",
                       breaks=c("l", "m", "s"),
                       labels=c("大型", "中型", "小型"))+
  theme(text=element_text(size=15),title=element_text(face='bold'))
####资本（w=600,h=300）
ggplot(factor_scale,aes(x=year,y=log(capital),linetype=scale))+geom_line(size=1.2)+
  ggtitle('不同规模下资本分布')+labs(y='log(资本)')+theme_classic()+
  scale_x_continuous(breaks=seq(1998,2007,by=2))+
  scale_linetype(name="企业规模",
                       breaks=c("l", "m", "s"),
                       labels=c("大型", "中型", "小型"))+
  theme(text=element_text(size=15),title=element_text(face='bold'))
####数量（w=600,h=300）
# num_scale <- data%>%group_by(year,scale)%>%summarise(num=length(scale))
# ggplot(num_scale,aes(x=year,y=log(num),linetype=scale))+geom_line(size=1.2)+
#   ggtitle('不同规模下企业数量')+labs(y='log(数量)')+theme_classic()+
#   scale_x_continuous(breaks=seq(1998,2007,by=2))+
#   scale_linetype(name="企业规模",
#                  breaks=c("l", "m", "s"),
#                  labels=c("大型", "中型", "小型"))+
#   theme(text=element_text(size=15),title=element_text(face='bold'))

##图二 不同部门间的TFP增长状况
alp <- 0.4;nalp <- 1-alp
factor_scale$tfp <- with(factor_scale,output/((worker^alp)*(capital^nalp)))
ggplot(factor_scale,aes(x=year,y=log(tfp),linetype=scale))+geom_line(size=1.2)+
  ggtitle('不同规模下TFP')+labs(y='log(TFP)')+theme_classic()+
  scale_x_continuous(breaks=seq(1998,2007,by=2))+
  scale_linetype(name="企业规模",
                       breaks=c("l", "m", "s"),
                       labels=c("大型", "中型", "小型"))+
  theme(text=element_text(size=15),title=element_text(face='bold'))


###图三 不同省份的大型企业
###2007年省份大型企业占比(w:500 h:250)
myepidat <- subset(scale_sum,year==2007)%>%group_by(area)%>%summarise(per=output[scale=='l']/sum(output))

ggplot(myepidat) +
  geom_map(aes(map_id = area, fill = per), color = "white", map = mysh)+
  scale_fill_gradient(name='比例',high = "#1A1A1A",low = "#E6E6E6")+
  expand_limits(mysh) + coord_map() +theme_classic()+ylim(c(20,52))+ 
  geom_text(aes(x = X1,y =X2,label = as.character(name)), 
            data = myname,size=3) + ggtitle('2007年各省大型企业产出占比')+
  theme(plot.margin=unit(rep(0,4),'lines'),panel.margin=unit(rep(0,4),'lines'),
        axis.line=element_blank(),
        text=element_text(size=10),title=element_text(face='bold'),
        axis.ticks=element_blank(),axis.text=element_blank())+labs(x=NULL,y=NULL)


#要素在不同省份间的分布状况--------------------------
##图4 产出的分布(w 600 h 300)
ggplot(subset(province_sum,year!=1998),aes(x=year,y=log(output_c),group=province,linetype=emw))+
  geom_line(size=1.2)+theme_classic()+
  scale_x_continuous(breaks=seq(1998,2007,by=2))+
  ggtitle('不同区域产出分布')+labs(y='log(实际产出)')+
  theme(text=element_text(size=15),title=element_text(face='bold'))+
  scale_linetype(name="区域",
                       breaks=c("east", "middle", "west"),
                       labels=c("东部", "中部", "西部"))
##资本的分布(w 600 h 300)
ggplot(subset(province_sum,year!=1998),aes(x=year,y=log(capital),group=province,linetype=emw))+
  geom_line(size=1.2)+theme_classic()+
  scale_x_continuous(breaks=seq(1998,2007,by=2))+
  ggtitle('不同区域资本分布')+labs(y='log(资本)')+
  theme(text=element_text(size=15),title=element_text(face='bold'))+
  scale_linetype(name="区域",
                       breaks=c("east", "middle", "west"),
                       labels=c("东部", "中部", "西部"))
##劳动力的分布(w 600 h 300)
ggplot(subset(province_sum,year!=1998),aes(x=year,y=log(worker),group=province,linetype=emw))+
  geom_line(size=1.2)+theme_classic()+
  scale_x_continuous(breaks=seq(1998,2007,by=2))+
  ggtitle('不同区域劳动力分布')+labs(y='log(劳动力)')+
  theme(text=element_text(size=15),title=element_text(face='bold'))+
  scale_linetype(name="区域",
                       breaks=c("east", "middle", "west"),
                       labels=c("东部", "中部", "西部"))

##图五 tfp的分布(w 600 h 300)
ggplot(subset(province_sum,year!=1998),aes(x=year,y=log(tfp),group=province,linetype=emw))+
  geom_line(size=1.2)+theme_classic()+
  scale_x_continuous(breaks=seq(1998,2007,by=2))+
  ggtitle('不同区域TFP分布')+labs(y='log(TFP)')+
  theme(text=element_text(size=15),title=element_text(face='bold'))+
  scale_linetype(name="区域",
                       breaks=c("east", "middle", "west"),
                       labels=c("东部", "中部", "西部"))
temp <- subset(province_sum,year==2007,select=c(tfp,area))%>%arrange(tfp)
temp <- merge(temp,subset(scale_sum,year==2007)%>%group_by(area)%>%summarise(per=output[scale=='l']/sum(output)),id='area')
cor(temp$tfp,temp$per,method='spearman')
lm(tfp~per,data=temp)%>%summary
# 损失的测度--------------------------
##图6 全国的全要素生产率(w 600 h 300)
total_sum%>%subset(year!=1998,select=c(year,tfp,etfp))%>%melt(id='year')%>%
  ggplot(aes(x=year,y=log(value),linetype=variable))+
  geom_line(size=1.2)+theme_classic()+
  scale_x_continuous(breaks=seq(1999,2007,by=2))+
  ggtitle('全国TFP变化情况')+labs(y='log(TFP)')+
  theme(text=element_text(size=15),title=element_text(face='bold'))

##图7 全国TFP缺口(w 600 h 300)
total_sum%>%subset(year!=1998)%>%
  ggplot(aes(x=year,y=log(etfp/tfp)))+
  geom_line(size=1.2)+theme_classic()+
  scale_x_continuous(breaks=seq(1999,2007,by=2))+
  ggtitle('全国TFP损失')+labs(y=expression('log(A*/A)'))+
  theme(text=element_text(size=15),title=element_text(face='bold'))

##图8 各区域TFP缺口(w 600 h 300)
ggplot(subset(province_sum,year!=1998),aes(x=year,y=log(etfp/tfp),group=province,linetype=emw,color=emw))+
  geom_line(size=1.2)+theme_classic()+
  scale_x_continuous(breaks=seq(1998,2007,by=2))+
  ggtitle('不同区域TFP损失')+labs(y=expression('log(A*/A)'))+
  theme(text=element_text(size=15),title=element_text(face='bold'))+
  scale_color_manual(values=grey((0:2)/3),name="区域",
                     breaks=c("east", "middle", "west"),
                     labels=c("东部", "中部", "西部"))+
  scale_linetype_discrete(name="区域",
                       breaks=c("east", "middle", "west"),
                       labels=c("东部", "中部", "西部"))

# 省际扭曲的分解-----------------
##图9 全国TFP的分解
total_sum%>%subset(year!=1998)%>%
  mutate(over_all=log(etfp/tfp),betw_province=log(etfp/tfp_nia),
                   in_province=log(etfp/tfp_nba))%>%
  select(year,over_all,betw_province,in_province)%>%
  melt(id='year')%>%
  ggplot(aes(x=year,y=value,linetype=variable))+geom_line(size=1.2)+theme_classic()+
  scale_x_continuous(breaks=seq(1998,2007,by=2))+
  ggtitle('全国TFP损失的分解')+labs(y='TPF损失')+
  theme(text=element_text(size=15),title=element_text(face='bold'))+
  scale_linetype_discrete(name="扭曲源",
                       breaks=c("over_all", "betw_province", "in_province"),
                       labels=c("整体", "省际", "省内"))

##图10 省际TFP的分解
total_sum%>%subset(year!=1998)%>%
  mutate(betw_province=log(etfp/tfp_nia),betw_l=log(tfp_nbl/tfp),
         betw_k=log(tfp_nbk/tfp))%>%
  select(year,betw_province,betw_l,betw_k)%>%
  melt(id='year')%>%
  ggplot(aes(x=year,y=value,linetype=variable))+geom_line(size=1.2)+theme_classic()+
  scale_x_continuous(breaks=seq(1998,2007,by=2))+
  ggtitle('省际TFP损失的分解')+labs(y='TPF损失')+
  theme(text=element_text(size=15),title=element_text(face='bold'))+
  scale_linetype_discrete(name="扭曲源",
                       breaks=c("betw_province", "betw_l", "betw_k"),
                       labels=c("省际", "劳动力", "资本"))



##图11 各省劳动力扭曲的情况(w:500 h:250)
###构建1999年劳动扭曲，并提取异常值
myepidat <- subset(province_sum,year==1999)
# myepidat%>%select(tile_l,area)%>%arrange(tile_l)
myepidat$tile_l <- scale(myepidat$tile_l)
myepidat$tile_l[myepidat$tile_l>1] <- 1
myepidat$tile_l[myepidat$tile_l<(-1)] <- (-1)


ggplot(myepidat) +
  geom_map(aes(map_id = area, fill = tile_l), color = "white", map = mysh)+
  scale_fill_gradient(name='扭曲程度',high = "#1A1A1A",low = "#E6E6E6")+
  expand_limits(mysh) + coord_map() +theme_classic()+ylim(c(20,52))+ 
  geom_text(aes(x = X1,y =X2,label = as.character(name)), 
            data = myname,size=3) + ggtitle('1999年全国劳动扭曲分布状况')+
  theme(plot.margin=unit(rep(0,4),'lines'),panel.margin=unit(rep(0,4),'lines'),
        axis.line=element_blank(),
        text=element_text(size=10),title=element_text(face='bold'),
        axis.ticks=element_blank(),axis.text=element_blank())+labs(x=NULL,y=NULL)

###构建2007年劳动扭曲，并提取异常值
myepidat <- subset(province_sum,year==2007)
# myepidat%>%select(tile_l,area)%>%arrange(tile_l)
myepidat$tile_l <- scale(myepidat$tile_l)
myepidat$tile_l[myepidat$tile_l>1] <- 1
myepidat$tile_l[myepidat$tile_l<(-1)] <- (-1)
##计算劳动扭曲与企业规模的相关性
temp <- scale_sum%>%subset(year==2007)%>%group_by(area)%>%summarise(per=output[scale=='l']/sum(output))%>%
  merge(subset(province_sum,year==2007,select=c(area,tile_l)))
cor(temp$per,temp$tile_l,method="spearman")
##作图
ggplot(myepidat) +
  geom_map(aes(map_id = area, fill = tile_l), color = "white", map = mysh)+
  scale_fill_gradient(name='扭曲程度',high = "#1A1A1A",low = "#E6E6E6")+
  expand_limits(mysh) + coord_map() +theme_classic()+ylim(c(20,52))+ 
  geom_text(aes(x = X1,y =X2,label = as.character(name)), 
            data = myname,size=3) + ggtitle('2007年全国劳动扭曲分布状况')+
  theme(plot.margin=unit(rep(0,4),'lines'),panel.margin=unit(rep(0,4),'lines'),
        axis.line=element_blank(),
        text=element_text(size=10),title=element_text(face='bold'),
        axis.ticks=element_blank(),axis.text=element_blank())+labs(x=NULL,y=NULL)

##图12 各省资本扭曲的情况(w:500 h:250)
###构建1999年资本扭曲，并提取异常值
myepidat <- subset(province_sum,year==1999)
# myepidat%>%select(tile_k,area)%>%arrange(tile_k)
myepidat$tile_k <- scale(myepidat$tile_k)
myepidat$tile_k[myepidat$tile_k>1] <- 1
myepidat$tile_k[myepidat$tile_k<(-1)] <- (-1)

myepidat <- subset(province_sum,year%in%c(1999,2007))

myepidat%>%group_by(year)%>%mutate(tile_k=scale(tile_k))%>%select(area,emw,tile_k,year)%>%
  ggplot(aes(x=emw,y=tile_k))+geom_boxplot()+geom_text(aes(label=area),position='jitter')+facet_grid(.~year)



ggplot(myepidat) +
  geom_map(aes(map_id = area, fill = tile_k), color = "white", map = mysh)+
  scale_fill_gradient(name='扭曲程度',high = "#1A1A1A",low = "#E6E6E6")+
  expand_limits(mysh) + coord_map() +theme_classic()+ylim(c(20,52))+ 
  geom_text(aes(x = X1,y =X2,label = as.character(name)), 
            data = myname,size=3) + ggtitle('1999年全国资本扭曲分布状况')+
  theme(plot.margin=unit(rep(0,4),'lines'),panel.margin=unit(rep(0,4),'lines'),
        axis.line=element_blank(),
        text=element_text(size=10),title=element_text(face='bold'),
        axis.ticks=element_blank(),axis.text=element_blank())+labs(x=NULL,y=NULL)

###构建2007年资本扭曲，并提取异常值
myepidat <- subset(province_sum,year==2007)
# myepidat%>%select(tile_k,area)%>%arrange(tile_k)
myepidat$tile_k <- scale(myepidat$tile_k)
myepidat$tile_k[myepidat$tile_k>1] <- 1
myepidat$tile_k[myepidat$tile_k<(-1)] <- (-1)


ggplot(myepidat) +
  geom_map(aes(map_id = area, fill = tile_k), color = "white", map = mysh)+
  scale_fill_gradient(name='扭曲程度',high = "#1A1A1A",low = "#E6E6E6")+
  expand_limits(mysh) + coord_map() +theme_classic()+ylim(c(20,52))+ 
  geom_text(aes(x = X1,y =X2,label = as.character(name)), 
            data = myname,size=3) + ggtitle('2007年全国资本扭曲分布状况')+
  theme(plot.margin=unit(rep(0,4),'lines'),panel.margin=unit(rep(0,4),'lines'),
        axis.line=element_blank(),
        text=element_text(size=10),title=element_text(face='bold'),
        axis.ticks=element_blank(),axis.text=element_blank())+labs(x=NULL,y=NULL)


##各省的产出资本比
myepidat <- subset(province_sum,year==2007)%>%mutate(out_cap=capital/output_c)
ggplot(myepidat) +
  geom_map(aes(map_id = area, fill = out_cap), color = "white", map = mysh)+
  scale_fill_gradient(name='资本产出比',high = "#1A1A1A",low = "#E6E6E6")+
  expand_limits(mysh) + coord_map() +theme_classic()+ylim(c(20,52))+ 
  geom_text(aes(x = X1,y =X2,label = as.character(name)), 
            data = myname,size=3) + ggtitle('2007年各省份资本-产出比')+
  theme(plot.margin=unit(rep(0,4),'lines'),panel.margin=unit(rep(0,4),'lines'),
        axis.line=element_blank(),
        text=element_text(size=10),title=element_text(face='bold'),
        axis.ticks=element_blank(),axis.text=element_blank())+labs(x=NULL,y=NULL)


#省内扭曲分解--------------------------
##图13 省内扭曲分解（w 600 h 300）
total_sum%>%subset(year!=1998)%>%
  mutate(in_province=log(etfp/tfp_nba),in_l=log(tfp_nil/tfp),
         in_k=log(tfp_nik/tfp))%>%
  select(year,in_province,in_l,in_k)%>%
  melt(id='year')%>%
  ggplot(aes(x=year,y=value,linetype=variable))+geom_line(size=1.2)+theme_classic()+
  scale_x_continuous(breaks=seq(1998,2007,by=2))+
  ggtitle('省内TFP损失的分解')+labs(y='TPF损失')+
  theme(text=element_text(size=15),title=element_text(face='bold'))+
  scale_linetype(name="扭曲源",
                       breaks=c("in_province", "in_l", "in_k"),
                       labels=c("省际", "劳动力", "资本"))

###表1 劳动力扭曲状况
myepidat <- subset(scale_sum,year%in%c(1999,2007))%>%group_by(area,year)%>%
  summarise(tile_l=tile_l[scale=='l']/tile_l[scale=='s'],emw=emw%>%unique)
dcast(myepidat,area+emw~year,value.var='tile_l')%>%write.csv('in_province_l.csv',row.names=F)
####相关性
temp <- scale_sum%>%subset(year==1999)%>%group_by(area)%>%summarise(per=output[scale=='l']/sum(output))%>%
  merge(subset(myepidat,year==1999),by='area')
cor(temp$per,temp$tile_l)

###表2 资本扭曲状况
myepidat <- subset(scale_sum,year%in%c(1999,2007))%>%group_by(area,year)%>%
  summarise(tile_k=tile_k[scale=='l']/tile_k[scale=='s'],emw=emw%>%unique)
dcast(myepidat,area+emw~year,value.var='tile_k')%>%write.csv('in_province_k.csv',row.names=F)
####相关性
temp <- scale_sum%>%subset(year==1999)%>%group_by(area)%>%summarise(per=output[scale=='l']/sum(output))%>%
  merge(subset(myepidat,year==1999),by='area')
cor(temp$per,temp$tile_l)