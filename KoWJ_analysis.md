#柯文哲粉絲專頁分析
#

##目標
使用R軟體擷取柯文哲Facebook粉絲專頁相關資料，並呈現相關的視覺化圖表。

- R packages：
*Rfacebook, dplyr, lubridate, ggplot2, scales, gridExtra*



####載入需要的套件

```
require(dplyr)
require(ggplot2)
require(scales)
require(Rfacebook)
require(gridExtra)
require(lubridate)
```

####定義參數

```
start_date <- "2013/01/01"
end_date <- "2017/05/31"
page.id <- "DoctorKoWJ"

dir="F:"
```