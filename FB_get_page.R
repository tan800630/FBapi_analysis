<<<<<<< HEAD



require("Rfacebook")

#這邊是用我的fb.oauth直接授權的，用token的人要記得改一下"token=fb.oauth"這段
#token="#your facebook API token#"

start_date <- "2013/01/01"
end_date <- "2017/05/31"

page.id <- "DoctorKoWJ"
page.id2 <-"pxmartchannel"
page.id3 <-"greenpeace.org.tw"
page.id4 <-"tsaiingwen"
page.id5 <-"kikuChen"
page.id6 <-"MaYingjeou"
page.id7 <-"llchu"

page <- getPage(page.id,token=fb.oauth,n=2000,since=start_date,until=end_date)
str(page)

page2 <- getPage(page.id2,token=fb.oauth,n=1000,since=start_date,until=end_date)
page3 <- getPage(page.id3,token=fb.oauth,n=1000,since=start_date,until=end_date)
=======



require("Rfacebook")

#這邊是用我的fb.oauth直接授權的，用token的人要記得改一下"token=fb.oauth"這段
#token="#your facebook API token#"



start_date <- "2013/01/01"
end_date <- "2013/12/31"

page.id <- "DoctorKoWJ"
page.id2 <-"pxmartchannel"
page.id3 <-"greenpeace.org.tw"
page.id4 <-"tsaiingwen"
page.id5 <-"kikuChen"
page.id6 <-"MaYingjeou"
page.id7 <-"llchu"

page <- getPage(page.id,token=fb.oauth,n=1000,since=start_date,until=end_date)
str(page)

page2 <- getPage(page.id2,token=fb.oauth,n=1000,since=start_date,until=end_date)
page3 <- getPage(page.id3,token=fb.oauth,n=1000,since=start_date,until=end_date)


#####  儲存post檔案  KoWJ

for(i in 93:dim(page)[1]){
	post=getPost(page$id[i],token=fb.oauth,likes=T,reactions=F,
	n=max(page$likes_count))
	save(post,file=paste0("F:/data/KoWJ/",
	strsplit(post$post$id,"_")[[1]][2],".RData"))
	}


