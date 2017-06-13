<<<<<<< HEAD
require(dplyr)
require(ggplot2)
require(scales)
require(Rfacebook)
require(gridExtra)

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




##ggplot-文章類型-讚數-時間趨勢
ggplot(dat,aes(x=as.Date(created_time),y=log(likes_count)))+
geom_point(aes(color=type,shape=type))+
geom_vline(aes(xintercept=as.numeric(as.Date("2014-01-19"))),colour="red",linetype="dashed")+
geom_vline(aes(xintercept=as.numeric(as.Date("2014-11-29"))),colour="blue",linetype="dashed")+
annotate("text",x=as.Date("2014-01-19"),y=4,label="宣布參選",colour="red")+
annotate("text",x=as.Date("2014-11-29"),y=4,label="當選市長",colour="blue")+
labs(title="文章類型-讚數-時間趨勢",x="時間",y="讚數(log)")+
facet_grid(type~.)+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
scale_x_date(labels = date_format("%Y-%m-%d"))

##ggplot-文章類型-回應數-時間趨勢
####與按讚差異不大###
ggplot(dat,aes(x=as.Date(created_time),y=log(comments_count)))+
geom_point(aes(color=type,shape=type))+
geom_vline(aes(xintercept=as.numeric(as.Date("2014-01-19"))),colour="red",linetype="dashed")+
geom_vline(aes(xintercept=as.numeric(as.Date("2014-11-29"))),colour="blue",linetype="dashed")+
annotate("text",x=as.Date("2014-01-19"),y=4,label="宣布參選",colour="red")+
annotate("text",x=as.Date("2014-11-29"),y=4,label="當選市長",colour="blue")+
labs(title="文章類型-回應數-時間趨勢",x="時間",y="回應數(log)")+
facet_grid(type~.)+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
scale_x_date(labels = date_format("%Y-%m-%d"))


##ggplot-文章類型-分享數-時間趨勢
####photo內無明顯回應較低之文章，另event類型皆無分享###
ggplot(dat,aes(x=as.Date(created_time),y=log(shares_count+1)))+
geom_point(aes(color=type,shape=type))+
geom_vline(aes(xintercept=as.numeric(as.Date("2014-01-19"))),colour="red",linetype="dashed")+
geom_vline(aes(xintercept=as.numeric(as.Date("2014-11-29"))),colour="blue",linetype="dashed")+
annotate("text",x=as.Date("2014-01-19"),y=4,label="宣布參選",colour="red")+
annotate("text",x=as.Date("2014-11-29"),y=4,label="當選市長",colour="blue")+
labs(title="文章類型-分享數-時間趨勢",x="時間",y="分享數+1(log)")+
facet_grid(type~.)+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
scale_x_date(labels = date_format("%Y-%m-%d"))



####

##把按讚數較低的幾則po文拉出來看
datl=dat %>% filter(log(likes_count)<6.7,type=="photo") %>%
select(X,message,created_time,type) %>% arrange(as.Date(created_time))

##把其中在2014年的文章(since 1/28 to 7/5)拉出來看，頭尾各增加30篇文章
dat_2014=dat %>% filter(X<datl$X[1]+30 & X>datl$X[25]-30,type=="photo")

##畫圖觀察可不可以用簡單的方式區辨這些文章

#原本觀察到的圖
ggplot(dat_2014,aes(y=log(likes_count),x=as.Date(created_time)))+geom_point()+
geom_hline(aes(yintercept=6.7),colour="red",linetype="dashed")

#用文章字數當作x軸
ggplot(dat_2014,aes(y=log(likes_count),x=nchar(message)))+geom_point()+
geom_hline(aes(yintercept=6.7),colour="red",linetype="dashed")

#
ggplot(dat_2014,aes(y=nchar(message),x=as.Date(created_time)))+
geom_point(aes(color=log(likes_count)>6.7))+theme(legend.position=c(.3,.85))



####後續發現是Rfacebook抓取資料時，"創造相簿"的Po文資料抓取會抓到相片而非文章的按讚資料###

page$album=0
page$album[which(grepl("album",page$story)&grepl("add",page$story))]=1


ggplot(dat,aes(x=as.Date(created_time),y=log(likes_count)))+
geom_point(aes(color=type,shape=type,alpha=as.factor(album)))+
geom_vline(aes(xintercept=as.numeric(as.Date("2014-01-19"))),colour="red",linetype="dashed")+
geom_vline(aes(xintercept=as.numeric(as.Date("2014-11-29"))),colour="blue",linetype="dashed")+
annotate("text",x=as.Date("2014-01-19"),y=4,label="宣布參選",colour="red")+
annotate("text",x=as.Date("2014-11-29"),y=4,label="當選市長",colour="blue")+
labs(title="文章類型-讚數-時間趨勢-II",x="時間",y="讚數(log)")+
facet_grid(type~.)+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
scale_x_date(labels = date_format("%Y-%m-%d"))+
scale_alpha_discrete(range=c(0.3,1))+guides(alpha=FALSE)






#######text_mining######

##目前並沒有特別的結果

Sys.setlocale(category = "LC_ALL", locale = "cht")

require(jiebaR)
require(text2vec)

text_min=worker()

a=sapply(dat$message,function(x) segment(x,text_min))
a=filter_segment(a,c("的","是","也","在","為","和","有","了"))


a.token=itoken(a)
a.vocab=create_vocabulary(a.token,ngram=c(1,1))

a.vocab.ns=a.vocab
a.vocab.ns$vocab=a.vocab$vocab[-which(nchar(a.vocab$vocab$terms)==1),]

#刪除出現數量過少的字詞
pruned_vocab=prune_vocabulary(a.vocab.ns,term_count_min=10,
doc_proportion_max =0.5,doc_proportion_min=0.001)

a.vectorizer=vocab_vectorizer(pruned_vocab,grow_dtm=T,skip_grams_window=5)


#term co-occurrence matrix
a.tcm=create_tcm(a.token,a.vectorizer)

a.dtm=create_dtm(a.token,a.vectorizer)

#製作glove model
fit=GlobalVectors$new(word_vectors_size=100,vocabulary=pruned_vocab,x_max=30)
fit$fit(a.tcm,n_iter=15)

word.vec=fit$get_word_vectors()
rownames(word.vec)=rownames(a.tcm)

Encoding(rownames(word.vec))="UTF-8"

#View(word.vec)


a.dtm@Dimnames[[1]]=as.character(c(1:length(a.dtm@Dimnames[[1]])))
#對文章做MDS，不好呈現
a.dist=dist(as.matrix(a.dtm), method = "cosine")
a.mds=cmdscale(a.dist)
plot(a.mds,type="n")
text(a.mds,labels=c(1:dim(a.mds)[1]))

#####################################################a.hclu=hclust


#對字詞作MDS，無明顯pattern
word.mds=cmdscale(dist(as.matrix(word.vec), method = "cosine"))
plot(word.mds,type="n")
text(word.mds,labels=rownames(word.vec))


#get analogy
get_analogy=function(king,man,woman){
	#Hint: establish an analogy logic,vec(queen)=vec(king)~vec(man)+vec(woman)
	queen=word.vec[king,,drop=F]-word.vec[man,,drop=F]+word.vec[woman,,drop=F]

	#Hint: calculate the cosine-similarity among vec(queen) and other word vectors
	cos.dist=text2vec:::sim2(x=queen,y=word.vec,method="cosine",norm="l2")

	#please show the top-10 words for this analogy task
	head(sort(cos.dist[1,],decreasing=T),10)
}




####後續
##加入回應資訊-->
# 1. 以按讚的人分類文章
# 2. 以按讚的文章分類人
=======
require(dplyr)
require(ggplot2)
require(scales)
require(Rfacebook)
require(gridExtra)

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




##ggplot-文章類型-讚數-時間趨勢
ggplot(dat,aes(x=as.Date(created_time),y=log(likes_count)))+
geom_point(aes(color=type,shape=type))+
geom_vline(aes(xintercept=as.numeric(as.Date("2014-01-19"))),colour="red",linetype="dashed")+
geom_vline(aes(xintercept=as.numeric(as.Date("2014-11-29"))),colour="blue",linetype="dashed")+
annotate("text",x=as.Date("2014-01-19"),y=4,label="宣布參選",colour="red")+
annotate("text",x=as.Date("2014-11-29"),y=4,label="當選市長",colour="blue")+
labs(title="文章類型-讚數-時間趨勢",x="時間",y="讚數(log)")+
facet_grid(type~.)+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
scale_x_date(labels = date_format("%Y-%m-%d"))

##ggplot-文章類型-回應數-時間趨勢
####與按讚差異不大###
ggplot(dat,aes(x=as.Date(created_time),y=log(comments_count)))+
geom_point(aes(color=type,shape=type))+
geom_vline(aes(xintercept=as.numeric(as.Date("2014-01-19"))),colour="red",linetype="dashed")+
geom_vline(aes(xintercept=as.numeric(as.Date("2014-11-29"))),colour="blue",linetype="dashed")+
annotate("text",x=as.Date("2014-01-19"),y=4,label="宣布參選",colour="red")+
annotate("text",x=as.Date("2014-11-29"),y=4,label="當選市長",colour="blue")+
labs(title="文章類型-回應數-時間趨勢",x="時間",y="回應數(log)")+
facet_grid(type~.)+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
scale_x_date(labels = date_format("%Y-%m-%d"))


##ggplot-文章類型-分享數-時間趨勢
####photo內無明顯回應較低之文章，另event類型皆無分享###
ggplot(dat,aes(x=as.Date(created_time),y=log(shares_count+1)))+
geom_point(aes(color=type,shape=type))+
geom_vline(aes(xintercept=as.numeric(as.Date("2014-01-19"))),colour="red",linetype="dashed")+
geom_vline(aes(xintercept=as.numeric(as.Date("2014-11-29"))),colour="blue",linetype="dashed")+
annotate("text",x=as.Date("2014-01-19"),y=4,label="宣布參選",colour="red")+
annotate("text",x=as.Date("2014-11-29"),y=4,label="當選市長",colour="blue")+
labs(title="文章類型-分享數-時間趨勢",x="時間",y="分享數+1(log)")+
facet_grid(type~.)+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
scale_x_date(labels = date_format("%Y-%m-%d"))


####

##把按讚數較低的幾則po文拉出來看
datl=dat %>% filter(log(likes_count)<6.7,type=="photo") %>%
select(X,message,created_time,type) %>% arrange(as.Date(created_time))

##把其中在2014年的文章(since 1/28 to 7/5)拉出來看，頭尾各增加30篇文章
dat_2014=dat %>% filter(X<datl$X[1]+30 & X>datl$X[25]-30,type=="photo")

##畫圖觀察可不可以用簡單的方式區辨這些文章

#原本觀察到的圖
ggplot(dat_2014,aes(y=log(likes_count),x=as.Date(created_time)))+geom_point()+
geom_hline(aes(yintercept=6.7),colour="red",linetype="dashed")

#用文章字數當作x軸
ggplot(dat_2014,aes(y=log(likes_count),x=nchar(message)))+geom_point()+
geom_hline(aes(yintercept=6.7),colour="red",linetype="dashed")

#
ggplot(dat_2014,aes(y=nchar(message),x=as.Date(created_time)))+
geom_point(aes(color=log(likes_count)>6.7))+theme(legend.position=c(.3,.85))



##單以文章字數和文章時間無法區辨出按讚人數低的文章，可能需要做text_mining

>>>>>>> dff8e0623e44e548608a547d4cd21842a1cbd38c
