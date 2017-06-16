require(data.table)
require(dplyr)
require(lubridate)
require(Matrix)
require(text2vec)
require(scales)

current_wd=getwd()
dir="F:/data/KoWJ"
setwd(dir)

like_DT=lapply(list.files(".", full.names = TRUE),function(fn){
  load(fn)
  data.table(post_ID=post$post$id,post_date = parse_date_time(
	substring(post$post$created_time,1, 19), "ymd HMS"),post$likes)
}) %>% rbindlist %>% arrange(from_id)

comment_DT=lapply(list.files(".", full.names = TRUE),function(fn){
  load(fn)
  if(dim(post$comments)[1]!=0){
  data.table(post_ID=post$post$id,post_date = parse_date_time(
	substring(post$post$created_time,1, 19), "ymd HMS"),
	from_id=post$comments$from_id,message=post$comments$message,
	created_time=parse_date_time(
	substring(post$comments$created_time,1, 19), "ymd HMS"))
  }
}) %>% rbindlist


load("F:/data/like_DT_KoWJ.RData")

####資料視覺化#####
#讀取檔案
dat=read.csv("F:/data/page_KoWJ.csv")


##資料前處理##

#把沒有留言Po文的刪掉----看過原始資料，皆為上傳照片
dat=dat[-which(is.na(dat$message)),]

#變更變項類型
dat$message=as.character(dat$message)
dat$type=as.character(dat$type)

#把note類型的po文刪掉-只有一項
dat=dat[-which(dat$type=="note"),]

#把新增相簿的資料刪除--按讚與回應資料不準確
dat= dat[-which(grepl("album",dat$story)&grepl("add",dat$story)),]


###################### 第一部分 #####################
####
####看按讚者的相似度

like_matrix=sparseMatrix(i=as.numeric(like_DT$from_id),
j=as.numeric(like_DT$post_ID),dimnames=list(levels(like_DT$from_id),
levels(like_DT$post_ID)))


like_matrix_n=like_matrix[which(rowSums(like_matrix)>10),]
post_matrix=t(like_matrix_n)

post_sim=sim2(post_matrix,method="cosine")


#mds --- 按讚人口流失與新增->與文章時間有高關聯
post.mds=cmdscale(1-post_sim)
plot(post.mds,type="n")
text(post.mds,labels=c(1:dim(post.mds)[1]))

mds_plot=as.data.frame(post.mds)

ggplot(data=mds_plot,aes(x=V1,y=V2))+
geom_point(aes(colour=c(1:dim(mds_plot)[1])))


#hierarchical clustering --- 與文章順序(發文時間)有高關聯
post.hclu=hclust(as.dist(1-post_sim))
plot(post.hclu)

tree=cutree(post.hclu,k=6)
plot(c(1:length(tree)),tree)



###################### 第二部分 #####################
####
####對按讚者第一次與最後一次按讚的時間作探討
####主要方向定義在one-shot liker中  (進出場難以定義)

load(file="F:/data/liker_fl.RData")

time_first_count=data.frame(table(outDT[outDT$count!=1,]$first))
time_last_count=data.frame(table(outDT[outDT$count!=1,]$last))
time_one_count=data.frame(table(outDT[outDT$count==1,]$first))

time_count=list(time_first_count,time_last_count,time_one_count)%>%
Reduce(f=function(x,y) merge(x,y,by="Var1",all.x=T,all.y=T))

time_count[is.na(time_count)]=0

colnames(time_count)=c("time","entry","leave","one_shot")
time_count$time=as.POSIXct(as.character(time_count$time),tz="UTC")

dat$created_time=parse_date_time(substring(dat$created_time,1, 19),
"ymd HMS",tz="UTC")
merge_dat=merge(dat[,-c(2,3,4,7)],time_count,by.x="created_time",by.y="time",all.x=F)


merge_dat_l=melt(merge_dat%>%select(id,type,created_time,entry,leave,one_shot,
likes_count),id.vars=c("id","type","created_time"))

ggplot(merge_dat,aes(x=created_time,y=likes_count))+
geom_col(color="lightblue",alpha=0.5)


## entry leave one_shot
merge_dat_l %>% #filter(created_time>as.POSIXct("2014-01-01",tz="UTC")) %>%
mutate(created_time=as.factor(created_time)) %>%
ggplot(.,aes(x=created_time,y=value))+
geom_col(data=. %>% filter(variable=="likes_count"),
color="lightblue",alpha=0.3)+
geom_col(data=. %>% filter(variable!="likes_count"),
aes(fill=variable,color=variable),position="stack")


## one_shot 數量---by type
merge_dat_l %>% filter(type!="event") %>%
#mutate(created_time=as.factor(created_time)) %>%
ggplot(.,aes(x=created_time,y=value))+
geom_col(data=. %>% filter(variable=="likes_count"),color="lightblue",alpha=0.3)+
geom_col(data=. %>% filter(variable=="one_shot"))+facet_grid(type~.)

#facet_grid(type~.)

#以月份作為單位
merge_dat_pro=merge_dat%>%mutate(created_time=format(created_time,
format="%Y-%m"))%>%
group_by(created_time)%>%summarise(count_l=sum(likes_count),
count_o=sum(one_shot))%>%mutate(por=count_o/count_l)


## 以alpha顯示one_shot的比例
merge_dat%>% filter(type!="event",created_time>as.POSIXct("2015-01-01",tz="UTC")) %>%
mutate(created_time=as.factor(created_time),
pro=one_shot/likes_count) %>%
ggplot(.,aes(x=created_time,y=likes_count))+
geom_col(aes(alpha=pro))+scale_alpha_continuous(range=c(0.3,1))+
facet_grid(type~.)


a=merge_dat%>% filter(type!="event") %>%
mutate(created_time=as.factor(created_time),
pro=one_shot/likes_count)


#######################################做出正確的月份人數統計##################

month_count=like_DT %>% mutate(post_month=format(post_date,format="%Y-%m")) %>%
dcast(.,post_month+from_id~1,length) %>% dcast(post_month~1,length)

month_dat=merge(month_count,
	merge_dat%>%mutate(created_time=format(created_time,format="%Y-%m"))%>%
	select(created_time,type,likes_count,one_shot) %>%
	group_by(created_time) %>%summarise(sum_one=sum(one_shot)),
	by.x="post_month",by.y="created_time") %>% setnames("1","sum_total")


#作圖--不考慮po文類型
month_dat %>% mutate(pro=sum_one/sum_total)%>%
ggplot(.,aes(x=post_month,y=sum_total))+
geom_col(fill="red",aes(alpha=pro))+
scale_alpha_continuous(range=c(0.8,0))+
labs(title="One-Shot Liker比率-時間趨勢",x="月份",y="讚數")+
geom_vline(aes(xintercept=12),colour="blue",linetype="dashed")+
geom_vline(aes(xintercept=23),colour="darkgreen",linetype="dashed")+
annotate("text",x=12,y=800000,label="宣布參選",colour="blue")+
annotate("text",x=23,y=800000,label="當選市長",colour="darkgreen")+
scale_alpha_continuous(name="Proportion of\none_shot liker")+
theme(axis.text.x=element_text(angle=60,hjust=1))
