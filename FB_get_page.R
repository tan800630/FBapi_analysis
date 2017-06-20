require("Rfacebook")

#這邊是用我的fb.oauth直接授權的，用token的人要記得改一下"token=fb.oauth"這段
#token="#your facebook API token#"

start_date <- "2013/01/01"
end_date <- "2017/05/31"
page.id <- "DoctorKoWJ"

dir="F:/data/KoWJ"


page <- getPage(page.id,token=fb.oauth,n=2000,since=start_date,until=end_date)
str(page)


#####  儲存每則文章檔案至dir變數指定的資料夾中
##     需要蠻長一段時間，建議可以分段擷取資料(更改start_date & end_date)

for(i in 1:dim(page)[1]){
	post=getPost(page$id[i],token=fb.oauth,likes=T,reactions=T,
	n=max(page$likes_count))
	save(post,file=paste0(dir,"/",strsplit(post$post$id,"_")[[1]][2],".RData"))
	}

