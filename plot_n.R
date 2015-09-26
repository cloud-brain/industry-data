library(dplyr)
library(maptools)
library(ggplot2)
library(reshape2)
library(grid)
# load("C:/Users/kaijun.lkj/Desktop/lkj/工业数据库/result.RData")
#load("I:/lkj/result.RData")
load("C:/Users/cloud/Desktop/工作目录/阿里/AliDrive/lkj/工业数据库/result.RData")
##要素在不同部门间的分布状况
factor_scale <- scale_sum%>%group_by(year,scale)%>%
  summarise(output=sum(output_c),worker=sum(worker),capital=sum(capital))

##产出
ggplot(factor_scale,aes(x=year,y=log(output),color=scale))+geom_line(size=1.2)+
  ggtitle('不同规模下产出分布')+labs(y='log(产出)')+theme_classic()+
  scale_x_continuous(breaks=seq(1998,2007,by=2))+
  scale_color_discrete(name="企业规模",
                      breaks=c("l", "m", "s"),
                      labels=c("大型", "中型", "小型"))+
  theme(text=element_text(size=15),title=element_text(face='bold'))
##劳动力
ggplot(factor_scale,aes(x=year,y=log(worker),color=scale))+geom_line(size=1.2)+
  ggtitle('不同规模下劳动力分布')+labs(y='log(劳动力)')+theme_classic()+
  scale_x_continuous(breaks=seq(1998,2007,by=2))+
  scale_color_discrete(name="企业规模",
                       breaks=c("l", "m", "s"),
                       labels=c("大型", "中型", "小型"))+
  theme(text=element_text(size=15),title=element_text(face='bold'))
##资本
ggplot(factor_scale,aes(x=year,y=log(capital),color=scale))+geom_line(size=1.2)+
  ggtitle('不同规模下资本分布')+labs(y='log(资本)')+theme_classic()+
  scale_x_continuous(breaks=seq(1998,2007,by=2))+
  scale_color_discrete(name="企业规模",
                       breaks=c("l", "m", "s"),
                       labels=c("大型", "中型", "小型"))+
  theme(text=element_text(size=15),title=element_text(face='bold'))

##不同省份的中小企业规模比例
mydat <- readShapePoly("C:/Users/cloud/Desktop/数据分析/中国地图信息/国家基础地理信息系统数据/国界与省界/bou2_4p.shp")
## mydat <- readShapePoly("C:/Users/kaijun.lkj/Desktop/国家基础地理信息系统数据/国界与省界/bou2_4p.shp")
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

###计算2007年省份大型企业占比
myepidat <- subset(scale_sum,year==2007)%>%group_by(area)%>%summarise(per=output[scale=='l']/sum(output))
# myepidat <- province_sum%>%group_by(area)%>%summarise(tile_l=mean(tile_l),tile_k=mean(tile_k))
# arrange(myepidat,per)%>%data.frame
myepidat$tile_l <- scale(myepidat$tile_l)
myepidat$tile_l[myepidat$tile_l>1] <- 1
myepidat$tile_l[myepidat$tile_l<(-1)] <- (-1)

##作图
ggplot(myepidat) +
  geom_map(aes(map_id = area, fill = per), color = "white", map = mysh)+
  scale_fill_gradient(name='比例',high = "darkgreen",low = "lightgreen")+
  expand_limits(mysh) + coord_map() +theme_classic()+ylim(c(20,52))+ 
  geom_text(aes(x = X1,y =X2,label = as.character(name)), 
            data = myname,size=3) + ggtitle('2007年各省大型企业产出占比')+
  theme(plot.margin=unit(rep(0,4),'lines'),panel.margin=unit(rep(0,4),'lines'),
        axis.line=element_blank(),
        text=element_text(size=10),title=element_text(face='bold'),
        axis.ticks=element_blank(),axis.text=element_blank())+labs(x=NULL,y=NULL)


##不同部门间的TFP增长状况
alp <- 0.4;nalp <- 1-alp
factor_scale$tfp <- with(factor_scale,output/((worker^alp)*(capital^nalp)))
ggplot(factor_scale,aes(x=year,y=log(tfp),color=scale))+geom_line(size=1.2)+
  ggtitle('不同规模下TFP')+labs(y='log(TFP)')+theme_classic()+
  scale_x_continuous(breaks=seq(1998,2007,by=2))+
  scale_color_discrete(name="企业规模",
                       breaks=c("l", "m", "s"),
                       labels=c("大型", "中型", "小型"))+
  theme(text=element_text(size=15),title=element_text(face='bold'))

##要素在不同省份间的分布状况
##产出的分布
ggplot(subset(province_sum,year!=1998),aes(x=year,y=log(output_c),group=province,color=emw))+
  geom_line(size=1.2)+theme_classic()+
  scale_x_continuous(breaks=seq(1998,2007,by=2))+
  ggtitle('不同区域产出分布')+labs(y='log(实际产出)')+
  theme(text=element_text(size=15),title=element_text(face='bold'))+
  scale_color_discrete(name="区域",
                       breaks=c("east", "middle", "west"),
                       labels=c("东部", "中部", "西部"))
##资本的分布
ggplot(subset(province_sum,year!=1998),aes(x=year,y=log(capital),group=province,color=emw))+
  geom_line(size=1.2)+theme_classic()+
  scale_x_continuous(breaks=seq(1998,2007,by=2))+
  ggtitle('不同区域资本分布')+labs(y='log(资本)')+
  theme(text=element_text(size=15),title=element_text(face='bold'))+
  scale_color_discrete(name="区域",
                       breaks=c("east", "middle", "west"),
                       labels=c("东部", "中部", "西部"))
##劳动力的分布
ggplot(subset(province_sum,year!=1998),aes(x=year,y=log(worker),group=province,color=emw))+
  geom_line(size=1.2)+theme_classic()+
  scale_x_continuous(breaks=seq(1998,2007,by=2))+
  ggtitle('不同区域劳动力分布')+labs(y='log(劳动力)')+
  theme(text=element_text(size=15),title=element_text(face='bold'))+
  scale_color_discrete(name="区域",
                       breaks=c("east", "middle", "west"),
                       labels=c("东部", "中部", "西部"))

##tfp的分布
ggplot(subset(province_sum,year!=1998),aes(x=year,y=log(tfp),group=province,color=emw))+
  geom_line(size=1.2)+theme_classic()+
  scale_x_continuous(breaks=seq(1998,2007,by=2))+
  ggtitle('不同区域TFP分布')+labs(y='log(TFP)')+
  theme(text=element_text(size=15),title=element_text(face='bold'))+
  scale_color_discrete(name="区域",
                       breaks=c("east", "middle", "west"),
                       labels=c("东部", "中部", "西部"))
# subset(province_sum,year==1999,select=c(tfp,area))%>%arrange(tfp)

## 损失的测度--------------------------
##全国的全要素生产率
total_sum%>%subset(year!=1998,select=c(year,tfp,etfp))%>%melt(id='year')%>%
  ggplot(aes(x=year,y=log(value),color=variable))+
  geom_line(size=1.2)+theme_classic()+
  scale_x_continuous(breaks=seq(1999,2007,by=2))+
  ggtitle('全国TFP变化情况')+labs(y='log(TFP)')+
  theme(text=element_text(size=15),title=element_text(face='bold'))

##全国TFP缺口
total_sum%>%subset(year!=1998)%>%
  ggplot(aes(x=year,y=log(etfp/tfp)))+
  geom_line(size=1.2)+theme_classic()+
  scale_x_continuous(breaks=seq(1999,2007,by=2))+
  ggtitle('全国TFP损失')+labs(y=expression('log(A*/A)'))+
  theme(text=element_text(size=15),title=element_text(face='bold'))

##各区域TFP缺口
ggplot(subset(province_sum,year!=1998),aes(x=year,y=log(etfp/tfp),group=province,color=emw))+
  geom_line(size=1.2)+theme_classic()+
  scale_x_continuous(breaks=seq(1998,2007,by=2))+
  ggtitle('不同区域TFP损失')+labs(y=expression('log(A*/A)'))+
  theme(text=element_text(size=15),title=element_text(face='bold'))+
  scale_color_discrete(name="区域",
                       breaks=c("east", "middle", "west"),
                       labels=c("东部", "中部", "西部"))

## 扭曲的分解-----------------
##全国TFP的分解
total_sum%>%subset(year!=1998)%>%
  mutate(over_all=log(etfp/tfp),betw_province=log(etfp/tfp_nia),
                   in_province=log(etfp/tfp_nba))%>%
  select(year,over_all,betw_province,in_province)%>%
  melt(id='year')%>%
  ggplot(aes(x=year,y=value,color=variable))+geom_line(size=1.2)+theme_classic()+
  scale_x_continuous(breaks=seq(1998,2007,by=2))+
  ggtitle('全国TFP损失的分解')+labs(y='TPF损失')+
  theme(text=element_text(size=15),title=element_text(face='bold'))+
  scale_color_discrete(name="扭曲源",
                       breaks=c("over_all", "betw_province", "in_province"),
                       labels=c("整体", "省际", "省内"))

##省际TFP的分解
total_sum%>%subset(year!=1998)%>%
  mutate(betw_province=log(etfp/tfp_nia),betw_l=log(tfp_nbl/tfp),
         betw_k=log(tfp_nbk/tfp))%>%
  select(year,betw_province,betw_l,betw_k)%>%
  melt(id='year')%>%
  ggplot(aes(x=year,y=value,color=variable))+geom_line(size=1.2)+theme_classic()+
  scale_x_continuous(breaks=seq(1998,2007,by=2))+
  ggtitle('省际TFP损失的分解')+labs(y='TPF损失')+
  theme(text=element_text(size=15),title=element_text(face='bold'))+
  scale_color_discrete(name="扭曲源",
                       breaks=c("betw_province", "betw_l", "betw_k"),
                       labels=c("省际", "劳动力", "资本"))



##各省劳动力扭曲的情况
mydat <- readShapePoly("C:/Users/cloud/Desktop/数据分析/中国地图信息/国家基础地理信息系统数据/国界与省界/bou2_4p.shp")
## mydat <- readShapePoly("C:/Users/kaijun.lkj/Desktop/国家基础地理信息系统数据/国界与省界/bou2_4p.shp")
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

###构建1999年劳动扭曲，并提取异常值
myepidat <- subset(province_sum,year==1999)
# myepidat <- province_sum%>%group_by(area)%>%summarise(tile_l=mean(tile_l),tile_k=mean(tile_k))
# arrange(myepidat,tile_k)%>%data.frame
myepidat$tile_l <- scale(myepidat$tile_l)
myepidat$tile_l[myepidat$tile_l>1] <- 1
myepidat$tile_l[myepidat$tile_l<(-1)] <- (-1)

##作图
ggplot(myepidat) +
  geom_map(aes(map_id = area, fill = tile_l), color = "white", map = mysh)+
  scale_fill_gradient(name='扭曲程度',high = "darkgreen",low = "lightgreen")+
  expand_limits(mysh) + coord_map() +theme_classic()+ylim(c(20,52))+ 
  geom_text(aes(x = X1,y =X2,label = as.character(name)), 
            data = myname,size=3) + ggtitle('1999年全国劳动扭曲分布状况')+
  theme(plot.margin=unit(rep(0,4),'lines'),panel.margin=unit(rep(0,4),'lines'),
        axis.line=element_blank(),
        text=element_text(size=10),title=element_text(face='bold'),
        axis.ticks=element_blank(),axis.text=element_blank())+labs(x=NULL,y=NULL)

###构建2007年劳动扭曲，并提取异常值
myepidat <- subset(province_sum,year==2007)
# myepidat <- province_sum%>%group_by(area)%>%summarise(tile_l=mean(tile_l),tile_k=mean(tile_k))
# arrange(myepidat,tile_k)%>%data.frame
myepidat$tile_l <- scale(myepidat$tile_l)
myepidat$tile_l[myepidat$tile_l>1] <- 1
myepidat$tile_l[myepidat$tile_l<(-1)] <- (-1)

##作图
ggplot(myepidat) +
  geom_map(aes(map_id = area, fill = tile_l), color = "white", map = mysh)+
  scale_fill_gradient(name='扭曲程度',high = "darkgreen",low = "lightgreen")+
  expand_limits(mysh) + coord_map() +theme_classic()+ylim(c(20,52))+ 
  geom_text(aes(x = X1,y =X2,label = as.character(name)), 
            data = myname,size=3) + ggtitle('2007年全国劳动扭曲分布状况')+
  theme(plot.margin=unit(rep(0,4),'lines'),panel.margin=unit(rep(0,4),'lines'),
        axis.line=element_blank(),
        text=element_text(size=10),title=element_text(face='bold'),
        axis.ticks=element_blank(),axis.text=element_blank())+labs(x=NULL,y=NULL)

gg_1999_k <- ggplot(myepidat) +
  geom_map(aes(map_id = area, fill = tile_k), color = "white", map = mysh)+
  scale_fill_gradient(high = "darkgreen",low = "lightgreen")+
  expand_limits(mysh) + coord_map() +theme_classic()+ 
  geom_text(aes(x = X1,y =X2,label = as.character(name)), 
            data = myname,size=3) + 
  theme(plot.margin=unit(rep(0,4),'lines'),axis.line=element_blank(),
        axis.ticks=element_blank(),axis.text=element_blank())+labs(x=NULL,y=NULL)

###构建2007年扭曲，并提取异常值
myepidat <- subset(province_sum,year==2007)
# myepidat <- province_sum%>%group_by(area)%>%summarise(tile_l=mean(tile_l),tile_k=mean(tile_k))
# arrange(myepidat,tile_k)%>%data.frame
myepidat$tile_l <- scale(myepidat$tile_l)
myepidat$tile_l[myepidat$tile_l>1] <- 1
myepidat$tile_l[myepidat$tile_l<(-1)] <- (-1)

myepidat$tile_k <- scale(myepidat$tile_k)
myepidat$tile_k[myepidat$tile_k>1] <- 1
myepidat$tile_k[myepidat$tile_k<(-1)] <- (-1)

##作图
gg_2007_l <- ggplot(myepidat) +
  geom_map(aes(map_id = area, fill = tile_l), color = "white", map = mysh)+
  scale_fill_gradient(high = "darkgreen",low = "lightgreen")+
  expand_limits(mysh) + coord_map() +theme_classic()+ 
  geom_text(aes(x = X1,y =X2,label = as.character(name)), 
            data = myname,size=3) + 
  theme(plot.margin=unit(rep(0,4),'lines'),axis.line=element_blank(),
        axis.ticks=element_blank(),axis.text=element_blank())+labs(x=NULL,y=NULL)

gg_2007_k <- ggplot(myepidat) +
  geom_map(aes(map_id = area, fill = tile_k), color = "white", map = mysh)+
  scale_fill_gradient(high = "darkgreen",low = "lightgreen")+
  expand_limits(mysh) + coord_map() +theme_classic()+ 
  geom_text(aes(x = X1,y =X2,label = as.character(name)), 
            data = myname,size=3) + 
  theme(plot.margin=unit(rep(0,4),'lines'),axis.line=element_blank(),
        axis.ticks=element_blank(),axis.text=element_blank())+labs(x=NULL,y=NULL)



# ## kmean 分解
# temp <- province_sum%>%subset(year!=1998)%>%mutate(dis=log(etfp/tfp))%>%
#   select(area,dis,year)%>%dcast(area~year,value.var='dis')
# temp$cluster <- (temp[,2:10]%>%kmeans(centers=5))$cluster
# temp <- temp%>%melt(id=c('cluster','area'))
# temp$variable <- as.integer(as.character(temp$variable))
# temp%>%ggplot(aes(x=variable,y=value,group=factor(area)))+
#   geom_line()+facet_grid(.~cluster)
