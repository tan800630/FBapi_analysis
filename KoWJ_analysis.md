# 柯文哲粉絲專頁分析 #
#

## 目標 ##
使用R軟體擷取柯文哲Facebook粉絲專頁相關資料，並呈現相關的視覺化圖表。

## 事前準備 ##
1. 取得Facebook API Account與tokens (可參照此[教學部落格](https://blog.gtwang.org/r/facebook-social-media-mining-with-r/))
 
2. **Required R packages**：
*Rfacebook, dplyr, lubridate, ggplot2, scales, gridExtra*

ex.若在擷取資料上有遇到困難無法處理或暫時不考慮申請FBAPI帳號的，可e-mail聯絡我，我會提供手邊的資料檔案(後續也將找時間上傳到開放空間)


-------------------------------------------------
-------------------------------------------------
## 開始  ## 
#載入需要的套件  

```r
require(dplyr)
require(ggplot2)
require(scales)
require(Rfacebook)
require(gridExtra)
require(lubridate)
```

#定義參數  

```r
start_date <- "2013/01/01"
end_date <- "2017/05/31"
page.id <- "DoctorKoWJ"

dir="F:"
```

#讀取資料-存檔  
**注意!**  
使用token的人要記得改一下"token=fb.oauth"這段  
token="your facebook API token"

```r
page <- getPage(page.id, token=fb.oauth, n=3000, since=start_date, until=end_date)

write.csv(page,file=paste0(dir,"/data/page_KoWJ.csv"))

#後續只需讀入檔案即可
dat=read.csv(paste0(dir,"/data/page_KoWJ.csv"))
```

看一下取得的資料欄位有哪一些  
```r
str(dat)
```
>'data.frame':   1145 obs. of  12 variables:
>$ X             : int  1 2 3 4 5 6 7 8 9 10 ...  
>$ from_id       : num  1.37e+14 1.37e+14 1.37e+14 1.37e+14 1.37e+14 ...  
>$ from_name     : Factor w/ 1 level "柯文哲": 1 1 1 1 1 1 1 1 1 1 ...  
>$ message       : Factor w/ 1122 levels "#向搶修人員致敬\n\n八德...  
>$ created_time  : Factor w/ 1145 levels "2013-02-22T16:27:45+0000",...  
>$ type          : Factor w/ 6 levels "event","link"...  
>$ link          : Factor w/ 1077 levels "http://7stareco2015.blogspot.tw/",...   
>$ id            : Factor w/ 1145 levels "136845026417486_1000325016736145",...  
>$ story         : Factor w/ 106 levels "柯文哲 added 10 new photos to the albu...  
>$ likes_count   : int  21136 8352 16135 8638 8240 13164 19980 17273 56261 21894 ...  
>$ comments_count: int  217 95 306 125 178 125 292 271 541 571 ...  
>$ shares_count  : int  122 55 599 50 132 165 0 2958 931 1801 ...  


可以看到資料中紀錄了2013年到2017年5月之間的文章(共_1145_篇)  
每一篇文章之資料包含  
 - 文章ID  
 - 類型(*Event,Note,Link,Photo,status,video*)  
 - 內文  
 - 註記  
 - 按讚人數  
 - 回應人數  
 - 分享人數   

## 資料處理 ##

```r
#把沒有留言Po文的刪掉  
dat=dat[-which(is.na(dat$message)),]  

#變更變項類型  
dat$message=as.character(dat$message)  
dat=dat %>% mutate(created_time = parse_date_time(
	substring(created_time,1, 19), "ymd HMS"))  
```

整理完資料後，就可以來分析了。  

## 指標之間的關聯 ##  
問題：

1. 在專頁中何種類型的文章較多？  
2. 我們可以預期按讚的人數多，回應與分享的人數就多嗎？_(看按讚-回應-分享之間的關聯)_  

```r
#觀看此粉絲專頁的文章類型數量
barplot(table(dat$type),main="柯文哲臉書粉絲專頁文章類型",xlab="文章類型",ylab="次數")
```
![](http://FBapi_analysis/pic/bar_plot.png)
**barplot**  
  
從圖中我們可以發現**Photo**類型的文章最多，接著是**Video**、**Link**、**Status**、**Event**次數依序降低，符合了「發文不附圖，此風不可長」與「沒圖沒真相」的現代趨勢。    
另外Note類型的文章非常少，在資料中只有一則，後續將其從資料中刪除。  

```r
#把note類型的po文刪掉-只有一篇     
dat=dat[-which(dat$type=="note"),]  
```
  
   
```r
#觀看指標-按讚、回應、分享數量之關聯  
#由於三類指標的分配皆為
  
plot1=ggplot(dat,aes(x=log(comments_count+1),y=log(likes_count+1)))+  
geom_point(aes(color=type,shape=type))+labs(title="按讚-回應人數",x="回應人數(log)",y="按讚人數(log)")+  
theme(plot.title = element_text(hjust = 0.5),legend.position=c(.1,.85))  
  
plot2=ggplot(dat,aes(x=log(shares_count+1),y=log(likes_count+1)))+  
geom_point(aes(color=type,shape=type))+labs(title="按讚-分享人數",x="分享人數(log)",y="按讚人數(log)")+  
theme(plot.title = element_text(hjust = 0.5),legend.position=c(.18,.85))  
  
plot3=ggplot(dat,aes(x=log(comments_count+1),y=log(shares_count+1)))+  
geom_point(aes(color=type,shape=type))+labs(title="回應-分享人數",x="回應人數(log)",y="分享人數(log)")+  
theme(plot.title = element_text(hjust = 0.5),legend.position=c(.1,.85))  
  
grid.arrange(plot1,plot2,plot3,nrow=1,ncol=3)  
```

![](http://FBapi_analysis/pic/ggplot_like_comment_share_xyplot.png)
**ggplot_like_respond_share_xyplot**  
  
上圖呈現按讚-回應-分享人數兩兩配對的x-y plot，另外也以不同顏色和形狀表示不同的文章類型，可以看到按讚與回應的人數有非常高的線性正相關，另外分享與其他兩個指標也有正向的關聯，然而三張圖左下角都有一小群的離群值，在此先留意一下。  

　  

## 時間趨勢 ##

另外，我們當然也有興趣了解柯文哲的人氣狀況_(按讚/回應/分享人次都是能夠反映人氣的指標，由於按讚-回應-分享彼此之間有不錯的正相關，這邊即只取按讚人數作為反映人氣的指標)_  
然而除了直接計算粉絲專頁文章的平均按讚人數，我們更有興趣的是，**柯文哲的人氣是否隨著時間有明顯的上升/下降趨勢**  

相關新聞：[TVBS：民調／市政爭議不斷！柯P滿意度僅剩36%](http://news.tvbs.com.tw/politics/671861)   
相關連結：[台北市長柯文哲滿意度追蹤](http://tsjh301.blogspot.tw/2016/07/2016-taipei-mayor-satisfaction.html)  
(以上相關網站不代表個人立場，純粹想提供大家相關的資訊，以及為何會以這個角度切入分析資料)  

注意!!