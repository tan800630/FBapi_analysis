#柯文哲粉絲專頁分析
#

##目標
使用R軟體擷取柯文哲Facebook粉絲專頁相關資料，並呈現相關的視覺化圖表。

- R packages：
*Rfacebook, dplyr, lubridate, ggplot2, scales, gridExtra*



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

###讀取資料
>注意!
>使用token的人要記得改一下"token=fb.oauth"這段
>token="<font color="blue">your facebook API token*<font>"
>
>需要自己抓資料的人可以google  使用R分析Facebook   相關教學

```r
page <- getPage(page.id,token=fb.oauth,n=3000,since=start_date,until=end_date)```

