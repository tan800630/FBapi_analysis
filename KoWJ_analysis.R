require(dplyr)
require(ggplot2)
require(scales)
require(Rfacebook)
require(gridExtra)
require(lubridate)

#使用token的人要記得改一下"token=fb.oauth"這段
#token="#your facebook API token#"
#需要自己抓資料的人可以google  使用R分析Facebook   相關教學

start_date <- "2013/01/01"
end_date <- "2017/05/31"
page.id <- "DoctorKoWJ"

dir="F:"

page <- getPage(page.id,token=fb.oauth,n=3000,since=start_date,until=end_date)

#看一下資料
str(page)

#將資料存出
write.csv(page,file=paste0(dir,"/data/page_KoWJ.csv"))



####資料視覺化#####
#讀取檔案
dat=read.csv(paste0(dir,"/data/page_KoWJ.csv"))


##資料前處理##

#把沒有留言Po文的刪掉----看過原始資料，皆為上傳照片
dat=dat[-which(is.na(dat$message)),]

#變更變項類型
dat$message=as.character(dat$message)
dat$type=as.character(dat$type)

#把note類型的po文刪掉-只有一項
dat=dat[-which(dat$type=="note"),]

dat=dat %>% mutate(created_time = parse_date_time(
	substring(created_time,1, 19), "ymd HMS"))
##資料表##

#table-文章類型/平均按讚次數/數量
dat %>% group_by(type) %>% summarise(平均讚數=mean(likes_count),
平均留言數=mean(comments_count),平均分享=mean(shares_count),
內容長度=mean(nchar(message)),數量=length(likes_count))


##資料作圖##

#觀看此粉絲專頁的文章類型數量
barplot(table(dat$type),main="柯文哲臉書粉絲專頁文章類型",xlab="文章類型",ylab="次數")


#觀看指標-按讚、回應、分享數量之關聯

plot1=ggplot(dat,aes(x=log(comments_count+1),y=log(likes_count+1)))+
geom_point(aes(color=type,shape=type))+labs(title="按讚-回應人數",x="回應人數(log)",y="按讚人數(log)")+
theme(plot.title = element_text(hjust = 0.5),legend.position=c(.1,.85))
#geom_smooth(aes(comments_count,likes_count,group=type,color=type), method=lm, se=FALSE)

plot2=ggplot(dat,aes(x=log(shares_count+1),y=log(likes_count+1)))+
geom_point(aes(color=type,shape=type))+labs(title="按讚-分享人數",x="分享人數(log)",y="按讚人數(log)")+
theme(plot.title = element_text(hjust = 0.5),legend.position=c(.18,.85))

plot3=ggplot(dat,aes(x=log(comments_count+1),y=log(shares_count+1)))+
geom_point(aes(color=type,shape=type))+labs(title="回應-分享人數",x="回應人數(log)",y="分享人數(log)")+
theme(plot.title = element_text(hjust = 0.5),legend.position=c(.1,.85))
#
grid.arrange(plot1,plot2,plot3,nrow=1,ncol=3)




##ggplot-文章類型/讚數-時間趨勢
ggplot(dat,aes(x=created_time,y=log(likes_count)))+
geom_point(aes(color=type,shape=type))+
geom_vline(aes(xintercept=as.numeric(as.POSIXct("2014-01-19"))),colour="red",linetype="dashed")+
geom_vline(aes(xintercept=as.numeric(as.POSIXct("2014-11-29"))),colour="blue",linetype="dashed")+
annotate("text",x=as.POSIXct("2014-01-19"),y=4,label="宣布參選",colour="red")+
annotate("text",x=as.POSIXct("2014-11-29"),y=4,label="當選市長",colour="blue")+
labs(title="文章類型-讚數-時間趨勢",x="時間",y="讚數(log)")+
facet_grid(type~.)+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
scale_x_datetime(labels = date_format("%Y-%m-%d"))

##ggplot-文章類型/回應數-時間趨勢
####與按讚差異不大###
ggplot(dat,aes(x=created_time,y=log(comments_count)))+
geom_point(aes(color=type,shape=type))+
geom_vline(aes(xintercept=as.numeric(as.POSIXct("2014-01-19"))),colour="red",linetype="dashed")+
geom_vline(aes(xintercept=as.numeric(as.POSIXct("2014-11-29"))),colour="blue",linetype="dashed")+
annotate("text",x=as.POSIXct("2014-01-19"),y=4,label="宣布參選",colour="red")+
annotate("text",x=as.POSIXct("2014-11-29"),y=4,label="當選市長",colour="blue")+
labs(title="文章類型-回應數-時間趨勢",x="時間",y="回應數(log)")+
facet_grid(type~.)+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
scale_x_datetime(labels = date_format("%Y-%m-%d"))


##ggplot-文章類型/分享數-時間趨勢
####photo內無明顯回應較低之文章，另event類型皆無分享###
ggplot(dat,aes(x=created_time,y=log(shares_count+1)))+
geom_point(aes(color=type,shape=type))+
geom_vline(aes(xintercept=as.numeric(as.POSIXct("2014-01-19"))),colour="red",linetype="dashed")+
geom_vline(aes(xintercept=as.numeric(as.POSIXct("2014-11-29"))),colour="blue",linetype="dashed")+
annotate("text",x=as.POSIXct("2014-01-19"),y=4,label="宣布參選",colour="red")+
annotate("text",x=as.POSIXct("2014-11-29"),y=4,label="當選市長",colour="blue")+
labs(title="文章類型-分享數-時間趨勢",x="時間",y="分享數+1(log)")+
facet_grid(type~.)+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
scale_x_datetime(labels = date_format("%Y-%m-%d"))



####
##把按讚數較低的幾則po文拉出來看
datl=dat %>% filter(log(likes_count)<6.7,type=="photo") %>%
select(X,message,created_time,type) %>% arrange(as.Date(created_time))

##把其中在2014年的文章(since 1/28 to 7/5)拉出來看，頭尾各增加30篇文章
dat_2014=dat %>% filter(X<datl$X[1]+30 & X>datl$X[25]-30,type=="photo")

##畫圖觀察可不可以用簡單的方式區辨這些文章

#作圖-按讚數與文章日期
ggplot(dat_2014,aes(y=log(likes_count),x=as.Date(created_time)))+geom_point()+
geom_hline(aes(yintercept=6.7),colour="red",linetype="dashed")+
labs(x="日期",y="按讚數(log)")

#作圖-按讚數與文章字數
ggplot(dat_2014,aes(y=log(likes_count),x=nchar(message)))+geom_point()+
geom_hline(aes(yintercept=6.7),colour="red",linetype="dashed")+
labs(x="文章字數",y="按讚數(log)")

#作圖-文章字數與日期,以紅色標記按讚數低的點
ggplot(dat_2014,aes(y=nchar(message),x=as.Date(created_time)))+
geom_point(aes(color=log(likes_count)>6.7))+theme(legend.position=c(.3,.85))+
labs(x="日期",y="文章字數")

#雖按讚數較低的文章文章字數皆較少，但仍無法用簡單的方式作區隔




####後續發現是Rfacebook抓取資料時，"創造相簿"的Po文資料抓取會抓到相片而非文章的按讚資料###
##相關說明參照ptt R_language版
##https://www.ptt.cc/bbs/R_Language/M.1497333230.A.A9A.html

#從story中找到有 "add __ photo to album: ___" 描述的文章
dat$album=0
dat$album[which(grepl("album",dat$story)&grepl("add",dat$story))]=1


#作圖-文章/讚數-時間趨勢，並以較深的顏色標記是create album的文章
ggplot(dat,aes(x=created_time,y=log(likes_count)))+
geom_point(aes(color=type,shape=type,alpha=as.factor(album)))+
geom_vline(aes(xintercept=as.numeric(as.POSIXct("2014-01-19"))),colour="red",linetype="dashed")+
geom_vline(aes(xintercept=as.numeric(as.POSIXct("2014-11-29"))),colour="blue",linetype="dashed")+
annotate("text",x=as.POSIXct("2014-01-19"),y=4,label="宣布參選",colour="red")+
annotate("text",x=as.POSIXct("2014-11-29"),y=4,label="當選市長",colour="blue")+
labs(title="文章類型-讚數-時間趨勢-II",x="時間",y="讚數(log)")+
facet_grid(type~.)+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
scale_x_datetime(labels = date_format("%Y-%m-%d"))+
scale_alpha_discrete(range=c(0.3,1))+guides(alpha=FALSE)

