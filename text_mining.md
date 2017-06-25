# 柯文哲粉絲專頁分析-文字探勘 ##尚未完工
#  
## 說明
這次的分析希望使用真實的社群媒體資料(柯文哲粉絲專頁)中的文字資料(**貼文內容**)進行文字探勘，又依照使用套件的不同而分為兩個部分(各自獨立)。第一個部分使用tmcn與Rwordseg套件做出基本的term-document matrix以利後續使用，這兩個套件在網路上的教學資源非常多，有許多人做得更完整，建議可以多去google看看；第二部分則是jiebaR與text2vec兩個套件，主要參考的資料來源為資料科學愛好者協會舉辦的相關文字探勘課程內容。 

如上一篇所示，擷取出來的文章數量為1000出頭篇，老實說1000多則文章並不算是非常大量的資料，因此在除了文字雲(單純計算次數)以外的字詞關聯模型結果可能不會非常漂亮，但還是希望能夠以這次的code當作範本供大家參考。當然上述只是個人看法，非常歡迎給我一些回饋。


## 事前準備(同KoWJ_analysis)
1. 取得Facebook API Account與tokens (可參照此[教學部落格](https://blog.gtwang.org/r/facebook-social-media-mining-with-r/))
 
2. **Required R packages**：
*Rfacebook, dplyr*


　ex.若在擷取資料上有遇到困難無法處理或暫時不考慮申請FBAPI帳號的，可直接點選下面連結到data資料夾中下載檔案　　

　[資料夾連結點我](https://github.com/tan800630/FBapi_analysis/blob/master/data)

-------------------------------------------------  


```r
#載入套件與讀取檔案(使用KoWJ_analysis的post檔案)
require(dplyr)
require(Rfacebook)

dir="F:"
dat=read.csv(paste0(dir,"/data/page_KoWJ.csv"))

#把沒有留言Po文的刪掉
dat=dat[-which(is.na(dat$message)),]
#轉換變項類型
dat$message=as.character(dat$message)
```
注意!! 在此只把沒有留言的文章刪掉，未做其他的處理(ex.刪掉note類型的文章)

##1. 使用tmcn & Rwordseg套件進行字詞分析  

####主要分析流程參考國立高雄大學資管所 陳嘉葳 之文章[連結](http://rstudio-pubs-static.s3.amazonaws.com/12422_b2b48bb2da7942acaca5ace45bd8c60c.html)

事先安裝rJava,tmcn,以及Rwordseg之方法可參考[此篇](http://jianl.org/cn/R/Rwordseg.html)
  
文字探勘順序  
 - 將文字檔案轉換成語料庫格式
 - *英文大小寫轉換
 - *清除標點符號、英文字
 - 文本斷詞
 - 清除停止詞(不重要或無意義的字詞)
 - 製作字詞-文本矩陣(term-document matrix)

```r
require(tm)
require(tmcn)
require(Rwordseg)
require(slam)

#製作語料庫
#有興趣的人可在做完後打str(d.corpus)看看資料存放的方式
d.corpus=SimpleCorpus(VectorSource(dat$message),control=list(language = "CN"))

#在此只刪除標點符號
d.corpus = d.corpus %>% tm_map(removePunctuation)

#加入辭典中可能沒有的詞彙以增加斷詞正確率(此部分與文本的內容有極高的關聯)
insertWords(c("世大運","士林","內湖","大同","松山","萬華","大安","信義","南港","文山","中山","北投","柯P","捷運"))

#將每一篇文章做斷詞
d.corpus=tm_map(d.corpus, segmentCN, nature = TRUE)

#停止詞清除(停止詞指的是在文章中經常出現但可能對當下的作業判斷沒有幫助的詞，例如"啊"、"啦"、"的")
#stopwordsCN()為套件內預先列出的常用停止詞，需要自行增加時只要更改removeWords的words參數即可

#直接用tm_map做字詞清除會出現
#Error in UseMethod("removeWords", x) : 
#沒有適用的方法可將 'removeWords' 套用到 "list" 類別的物件

#使用lapply處理
d.corpus = lapply(d.corpus,function(x) removeWords(x,words=stopwordsCN()))
d.corpus = Corpus(VectorSource(d.corpus))
```  

>製作term-document matrix時若R版本過新(R 3.3.1以後的版本)程式碼會出現問題，需要下載此[code]()並放在資料夾中(需slam套件)
>[來源網址](http://mylearnho.blogspot.tw/2015/09/chinese-termdocumentmatrix-in-r-tm.html?m=1)

```r
#這邊預設放在同一資料夾下
source("TermDocumentMatrixCN.r")

#製作Term-Document matrix，並要求字詞長度必須在2以上(刪除單字詞)
tdm=TermDocumentMatrixCN(d.corpus,control=list(wordLengths = c(2,Inf)))

#tdm紀錄的是每個文件(Po文內容)中各個詞出現的次數，讓我們來看一下
inspect(tdm)

針對字詞共同出現的頻率與次數，可以以簡單的code呈現與目標詞有較高關聯的其他字詞
findAssocs(tdm,"大家",0.3)
```
>$大家
一起 
0.34  

到此為止最簡單的文本分析就結束了，接著可以使用文字雲的方式將最常出現的字詞以圖像呈現出來  
```r
m1 <- as.matrix(tdm)
v <- sort(rowSums(m1), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
wordcloud(d$word,d$freq,max.words=200,min.freq=10,random.order=F,
    ordered.colors=F,colors=rainbow(length(row.names(m1))))
```

![](https://raw.githubusercontent.com/tan800630/FBapi_analysis/master/pic/text_mining_wordcloud.png)

由於本次的文章來源為單一粉絲專頁，可以發現多半的文章內容仍是與台北市相關公共議題的宣導為主，因此主要仍圍繞在**北市**、**市府**、**市民**中。另外**我們**、**大家**、**一起**出現頻率也非常高，也有想要凝聚市民向心力的作用。  

##2. 使用jiebaR與text2vec套件進行字詞分析

文字探勘順序  

```r
require(jiebaR)
require(text2vec)
```