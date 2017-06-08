require(dplyr)
require(ggplot2)
require(scales)
require(Rfacebook)

#使用token的人要記得改一下"token=fb.oauth"這段
#token="#your facebook API token#"
#需要自己抓資料的人可以google  使用R分析Facebook   相關教學

start_date <- "2013/01/01"
end_date <- "2017/05/31"
page.id <- "DoctorKoWJ"

page <- getPage(page.id,token=fb.oauth,n=3000,since=start_date,until=end_date)

#看一下資料
str(page)

#將資料存出
write.csv(page,file="page_KoWJ.csv")

#rm(list=ls())


####資料視覺化#####
#讀取檔案
dat=read.csv("page_KoWJ.csv")


##資料前處理##

#把沒有留言Po文的刪掉----看過原始資料，皆為上傳照片
dat=dat[-which(is.na(dat$message)),]

#變更變項類型
dat$message=as.character(dat$message)
dat$type=as.character(dat$type)

#把note類型的po文刪掉-只有一項
dat=dat[-which(dat$type=="note"),]

##資料表##

#table-文章類型/平均按讚次數/數量
dat %>% group_by(type) %>% summarise(平均讚數=mean(likes_count),
平均留言數=mean(comments_count),平均分享=mean(shares_count),
內容長度=mean(nchar(message)),數量=length(likes_count))


##資料作圖##

#觀看此粉絲專頁的文章類型數量
barplot(table(dat$type),main="柯文哲臉書粉絲專頁文章類型",xlab="文章類型",ylab="次數")

##ggplot-文章類型-讚數-時間趨勢
ggplot(dat,aes(x=as.Date(created_time),y=log(likes_count)))+
geom_point(aes(color=type,shape=type))+
geom_vline(aes(xintercept=as.numeric(as.Date("2014-01-19"))),colour="red",linetype="dashed")+
geom_vline(aes(xintercept=as.numeric(as.Date("2014-11-29"))),colour="blue",linetype="dashed")+
annotate("text",x=as.Date("2014-01-19"),y=4,label="宣布參選",colour="red")+
annotate("text",x=as.Date("2014-11-29"),y=4,label="當選市長",colour="blue")+
labs(title="文章類型-讚數-時間趨勢",x="時間",y="讚數_LOG")+
facet_grid(type~.)+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
scale_x_date(labels = date_format("%Y-%m-%d"))

