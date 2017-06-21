#柯文哲粉絲專頁分析
#

##目標
使用R軟體擷取柯文哲Facebook粉絲專頁相關資料，並呈現相關的視覺化圖表。

>**R packages**：
>*Rfacebook, dplyr, lubridate, ggplot2, scales, gridExtra*




-------------------------------------------------
###載入需要的套件

```r
require(dplyr)
require(ggplot2)
require(scales)
require(Rfacebook)
require(gridExtra)
require(lubridate)
```

###定義參數

```r
start_date <- "2013/01/01"
end_date <- "2017/05/31"
page.id <- "DoctorKoWJ"

dir="F:"
```

###讀取資料-存檔
注意!
使用token的人要記得改一下"token=fb.oauth"這段
token="<font color="blue">your facebook API token*<font>"
需要自己抓資料的人可以google  使用R分析Facebook   相關教學

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
 $ X             : int  1 2 3 4 5 6 7 8 9 10 ...
 $ from_id       : num  1.37e+14 1.37e+14 1.37e+14 1.37e+14 1.37e+14 ...  
 $ from_name     : Factor w/ 1 level "柯文哲": 1 1 1 1 1 1 1 1 1 1 ...  
 $ message       : Factor w/ 1122 levels "#向搶修人員致敬\n\n八德...  
 $ created_time  : Factor w/ 1145 levels "2013-02-22T16:27:45+0000",...  
 $ type          : Factor w/ 6 levels "event","link"...  
 $ link          : Factor w/ 1077 levels "http://7stareco2015.blogspot.tw/",...  
 $ id            : Factor w/ 1145 levels "136845026417486_1000325016736145",..: 42 41 40 39 38 37 36 35 34 31 ...
 $ story         : Factor w/ 106 levels "柯文哲 added 10 new photos to the album: 【懶人包】土壤液化知識Q&A.",..: 19 23 NA 80 23 NA 91 NA NA NA ...
 $ likes_count   : int  21136 8352 16135 8638 8240 13164 19980 17273 56261 21894 ...
 $ comments_count: int  217 95 306 125 178 125 292 271 541 571 ...
 $ shares_count  : int  122 55 599 50 132 165 0 2958 931 1801 ...