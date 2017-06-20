require(data.table)
require(dplyr)
require(lubridate)
require(Matrix)
require(text2vec)
require(scales)
require(ggplot2)

my_path="F:"

current_wd=getwd()
dir=paste0(my_path,"/data/KoWJ")
setwd(dir)

like_DT=lapply(list.files(".", full.names = TRUE),function(fn){
  load(fn)
  data.table(post_ID=post$post$id,type=post$post$type,
      post_date = parse_date_time(
	substring(post$post$created_time,1, 19), "ymd HMS")+hours(8),post$likes)
}) %>% rbindlist

outDT <- list(
  dcast(like_DT, from_id + from_name ~ 1, min, value.var = "post_date") %>%
    setnames(".", "first"),
  dcast(like_DT, from_id + from_name ~ 1, max, value.var = "post_date") %>%
    setnames(".", "last"),
  dcast(like_DT, from_id + from_name ~ 1, length, value.var = "post_date") %>%
    setnames(".", "count")
) %>%
  Reduce(f = function(x, y) merge(x, y, by = c("from_id", "from_name")))



#目前分析尚未用到
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

#將like_DT存檔
save(like_DT,file=paste0(my_path,"/data/like_DT_KoWJ.RData"))
load(paste0(my_path,"/data/like_DT_KoWJ.RData"))

#將outDT存檔----其中記錄了每一個id按讚的次數與最初/最後時間
save(outDT,file=paste0(my_path,"/data/liker_fl.RData"))
#load(file=paste0(my_path,"/data/liker_fl.RData"))

####資料視覺化#####
#讀取檔案
dat=read.csv(paste0(my_path,"/data/page_KoWJ.csv"))


##資料前處理##
#變更變項類型
dat$message=as.character(dat$message)
dat$type=as.character(dat$type)
dat$id=as.character(dat$id)
#變更時間格式
dat$created_time=parse_date_time(substring(dat$created_time,1, 19),
"ymd HMS",tz="UTC")+hours(8)


#把沒有留言Po文的刪掉----看過原始資料，皆為上傳照片

no_mes_id=dat$id[which(is.na(dat$message))]
dat=dat[-which(is.na(dat$message)),]

#把note類型的po文刪掉-只有一項
note_id=dat$id[which(dat$type=="note")]
dat=dat[-which(dat$type=="note"),]

#把新增相簿的資料刪除--按讚與回應資料不準確
album_id=dat$id[which(grepl("album",dat$story)&grepl("add",dat$story))]
dat=dat[-which(grepl("album",dat$story)&grepl("add",dat$story)),]



###################### 第一部分 #####################
####
####看按讚者的相似度

like_DT=like_DT %>% mutate(ID=as.factor(ID),from_id=as.factor(from_id))

like_matrix=sparseMatrix(i=as.numeric(like_DT$from_id),
j=as.numeric(like_DT$ID),dimnames=list(levels(like_DT$from_id),
levels(like_DT$ID)))


like_matrix_n=like_matrix[which(rowSums(like_matrix)>10),]
post_matrix=t(like_matrix_n)

post_sim=sim2(post_matrix,method="cosine")


#mds --- 按讚人口流失與新增->與文章時間有高關聯
post.mds=cmdscale(1-post_sim)
plot(post.mds,type="n")
text(post.mds,labels=c(1:dim(post.mds)[1]))


#hierarchical clustering --- 與文章順序(發文時間)有高關聯
post.hclu=hclust(as.dist(1-post_sim))
plot(post.hclu)

tree=cutree(post.hclu,k=6)
plot(c(1:length(tree)),tree)


###################### 第二部分 #####################
####
####對按讚者第一次與最後一次按讚的時間作探討
####主要方向定義在one-shot liker中  (進出場難以定義)

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



## one_shot 數量---by type
merge_dat_l %>% filter(type!="event") %>%
#mutate(created_time=as.factor(created_time)) %>%
ggplot(.,aes(x=created_time,y=value))+
geom_col(data=. %>% filter(variable=="likes_count"),color="lightblue",alpha=0.3)+
geom_col(data=. %>% filter(variable=="one_shot"))+facet_grid(type~.)


## 以alpha顯示one_shot的比例----時間點過細
merge_dat%>% filter(type!="event") %>%
mutate(created_time=as.factor(created_time),
pro=one_shot/likes_count) %>%
ggplot(.,aes(x=created_time,y=likes_count))+
geom_col(aes(alpha=pro,fill=type))+scale_alpha_continuous(range=c(1,0.2))+
facet_grid(type~.)


###################### 第二部分-2 #####################
######做出正確的月份人數統計

month_count=like_DT %>% mutate(post_month=format(post_time,format="%Y-%m")) %>%
dcast(post_month+from_id~1,length) %>% dcast(post_month~1,length)

month_dat=merge(month_count,
	merge_dat%>%mutate(created_time=format(created_time,format="%Y-%m"))%>%
	select(created_time,type,likes_count,one_shot) %>%
	group_by(created_time) %>%summarise(sum_one=sum(one_shot)),
	by.x="post_month",by.y="created_time") %>% setnames("1","sum_total")


#作圖--不考慮po文類型
month_dat %>% mutate(pro=sum_one/sum_total)%>%
ggplot(.,aes(x=post_month,y=sum_total))+
geom_col(fill="red",aes(alpha=pro))+
scale_alpha_continuous(range=c(0.8,0),name="Proportion of\none_shot liker")+
labs(title="One-Shot Liker比率-時間趨勢",x="月份",y="讚數")+
geom_vline(aes(xintercept=12),colour="blue",linetype="dashed")+
geom_vline(aes(xintercept=23),colour="darkgreen",linetype="dashed")+
annotate("text",x=12,y=800000,label="宣布參選",colour="blue")+
annotate("text",x=23,y=800000,label="當選市長",colour="darkgreen")+
theme(axis.text.x=element_text(angle=60,hjust=1))




#依照type-月份 count one-liker

month_type=merge(like_DT %>% filter(!(ID %in% c(album_id,no_mes_id,note_id))) %>%
mutate(post_month=format(post_time,format="%Y-%m")) %>%
dcast(post_month+from_id+type~1,length) %>%
dcast(post_month+type~1,length,value.var="from_id") %>% setnames("1","count"),
merge_dat %>% mutate(post_month=format(created_time,format="%Y-%m")) %>%
dcast(post_month+type~1,sum,value.var="one_shot") %>%
setnames("1","one_shot_count"),by=c("post_month","type"),all.x=T)


##作圖-like_count/type-時間趨勢+one_shot_liker
month_type %>% mutate(pro=one_shot_count/count) %>%
ggplot(aes(x=post_month,y=count))+
geom_col(aes(alpha=pro,fill=type))+
labs(title="One-Shot比率/文章類型-時間趨勢",x="時間",y="讚數")+
geom_vline(aes(xintercept=12),colour="blue",linetype="dashed")+
geom_vline(aes(xintercept=23),colour="darkgreen",linetype="dashed")+
annotate("text",x=12,y=600000,label="宣布參選",colour="blue")+
annotate("text",x=23,y=600000,label="當選市長",colour="darkgreen")+
scale_alpha_continuous(name="Proportion of\none_shot liker",range=c(1,0.2))+
theme_bw()+
theme(axis.text.x=element_text(angle=60,hjust=1))+facet_grid(type~.)


month_type %>% mutate(pro=one_shot_count/count) %>%
ggplot(aes(x=post_month,y=pro))+
geom_point(aes(color=type,shape=type))+facet_grid(type~.)+theme_bw()+
theme(axis.text.x=element_text(angle=60,hjust=1))

##看最多one_shot_liker比例的文章
pro_dat=merge_dat %>% mutate(pro=one_shot/likes_count) %>% 
arrange(-pro) %>% select(type,id,likes_count,one_shot,pro)
head(pro_dat)

#pro 1st---高雄六龜合唱團(video)
dat[dat$id==pro_dat$id[1],] %>% select(message,created_time,type,likes_count)

#pro 2rd, 4th---洪仲丘相關發文(status)
dat[dat$id %in% pro_dat$id[c(2,4)],] %>%
select(message,created_time,type,likes_count)

#pro 3rd---海陸隊員虐殺小白狗新聞相關發文(photo)
dat[dat$id==pro_dat$id[3],] %>% select(message,created_time,type,likes_count)

#pro 5th---上任一年，反省影片(video)--->分享數量最高
dat[dat$id==pro_dat$id[5],] %>% select(message,created_time,type,likes_count)


#看這幾則文章
merge_dat%>% mutate(h_pro=id %in% pro_dat$id[1:5])%>%
ggplot(aes(x=shares_count,y=likes_count))+
geom_point(aes(color=h_pro))+labs(title="按讚-分享人數",x="分享人數(log)",y="按讚人數(log)")+
theme(plot.title = element_text(hjust = 0.5),legend.position=c(.18,.85))

