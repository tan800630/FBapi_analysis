# 柯文哲粉絲專頁分析-文字探勘 ##施工中
#  
## 說明
這次的分析希望使用真實的社群媒體資料(柯文哲粉絲專頁)中的文字資料(**貼文內容**)進行文字探勘，又依照使用套件的不同而分為兩個部分(各自獨立)。第一個部分使用tmcn與Rwordseg套件做出基本的term-document matrix以利後續使用，這兩個套件在網路上的教學資源非常多，有許多人做得更完整，建議可以多去google看看；第二部分則是jiebaR與text2vec兩個套件，主要參考的資料來源為資料科學愛好者協會舉辦的相關文字探勘課程內容。 

如上一篇所示，擷取出來的文章數量為1000出頭篇，老實說1000多則文章並不算是非常大量的資料，因此在除了文字雲(單純計算次數)以外的字詞關聯模型結果可能不會非常漂亮，但還是希望能夠以這次的code當作範本供大家參考。當然上述只是個人看法，非常歡迎給我一些回饋。


## 事前準備(同KoWJ_analysis)
1. 取得Facebook API Account與tokens (可參照此[教學部落格](https://blog.gtwang.org/r/facebook-social-media-mining-with-r/))
 
2. Required R packages：
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

## 1. 使用tmcn & Rwordseg套件進行字詞分析  

#### 主要分析流程參考國立高雄大學資管所 陳嘉葳 之文章[連結](http://rstudio-pubs-static.s3.amazonaws.com/12422_b2b48bb2da7942acaca5ace45bd8c60c.html)
事先安裝rJava,tmcn,以及Rwordseg之方法可參考[此篇](http://jianl.org/cn/R/Rwordseg.html)
  
文字探勘的流程包含了資料前處理、斷詞、後續模型建立。前處理的過程主要依照語言的特性而有所不同，最基本的如去除標點符號、清除無參考意義的停止詞。接著即是中文會遇到的斷詞問題(英文則以space作為詞跟詞之間的分隔即可)，目前的做法大多是以事先建立好的詞庫做判斷依據，在此以Rwordseg套件執行，並在必要時自行增加詞庫。最後，在此使用的模型為向量空間模型(vector-space model)，以字詞在文本的出現頻率做為字詞自身的向量(未考慮權重的狀況下，例如總共有10個文章，"分析"一詞在第1,5,7篇文章各出現一次，則"分析"一詞的向量即為1,0,0,0,1,0,1,0,0,0)，後續可以向量差異比較字詞概念之間的關聯性(也可以反過來去判斷文本之間的關聯)，算是文本分析中較基本簡單的模型。

順序
 - 將文字檔案轉換成語料庫格式
 - *英文大小寫轉換、時態轉換(英文狀況下)
 - *清除標點符號、英文字(中文狀況下)
 - 文本斷詞
 - 清除停止詞(不重要或無意義的字詞)
 - 模型建立：製作字詞-文本矩陣(term-document matrix)

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

>製作term-document matrix時若R版本過新(R 3.3.1以後的版本)程式碼會出現問題，需要下載[此code](https://raw.githubusercontent.com/tan800630/FBapi_analysis/master/TermDocumentMatrixCN.r)並放在資料夾中(需slam套件)
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

由於本次的文章來源為單一粉絲專頁，可以發現多半的文章內容仍是與台北市相關公共議題的宣導為主，因此主要仍圍繞在**北市**、**市府**、**市民**中。另外，台北、臺北、台北市、臺北市四個詞指的都是非常類似的概念(可能在使用的情境上略有不同)，然而本次分析中未將這四個詞統一化，因此這四個詞仍被分開計算詞頻。

## 2. 使用jiebaR與text2vec套件進行字詞分析
使用此兩套件進行分析的順序則相反，先進行jiebaR斷詞(會自動去除標點符號)，再依照條件清除字詞。另外，在此除了最簡單的term-document matrix之外，也會額外設定移動窗格建立字詞之間的關聯矩陣，以及建立GloVe模型。

順序  
 - 製造斷詞器(worker)進行斷詞
 - 移除停止詞
 - 製作目前的詞組字典
 - 字詞清除(出現頻率過低、單字詞等)
 - 製作模型
 	1.字詞關聯矩陣(term-corpus matrix)
 	2.文本字詞矩陣(documemnt-term matrix)
 	3.GloVe 模型

```r
#設定環境為中文
Sys.setlocale(category = "LC_ALL", locale = "cht")

require(jiebaR)
require(text2vec)

#從頭開始
dir="F:"
dat=read.csv(paste0(dir,"/data/page_KoWJ.csv"))

#把沒有留言Po文的刪掉
dat=dat[-which(is.na(dat$message)),]
#轉換變項類型
dat$message=as.character(dat$message)
```
```r  
#製造一個斷詞的工作點
#user參數將指定字典路徑，預設為USERPATH，需要自行新增詞庫時自行更改即可
text_min=worker(user=USERPATH)

#jiebaR斷詞
a=sapply(dat$message,function(x) segment(x,text_min))

#將停止詞從文本中刪除(不知為何即使在worker設定了停止詞條件也不會自動刪除)
a=filter_segment(a,c("的","是","也","在","為","和","有","了"))

#製作詞組字典
a.token=itoken(a)
a.vocab=create_vocabulary(a.token,ngram=c(1,1))

#刪除"單字詞"
a.vocab.ns=a.vocab
a.vocab.ns$vocab=a.vocab$vocab[-which(nchar(a.vocab$vocab$terms)==1),]

#刪除出現數量過少的字詞
pruned_vocab=prune_vocabulary(a.vocab.ns,term_count_min=50,
doc_proportion_max =0.5,doc_proportion_min=0.001)

pruned_vocab
```  
```
#a.vocab, pruned_vocab資料同樣記錄了每個詞的出現次數

Number of docs: 1127 
0 stopwords:  ... 
ngram_min = 1; ngram_max = 1 
Vocabulary: 
         terms terms_counts doc_counts
   1: 蘇花公路           14          3
   2:     超車           12          3
   3:     主義           12          3
   4:   監察員           10          2
   5:   連勝文           20          7
  ---                                 
2057:     法律           13         10
2058:     信心           20         19
2059:     長官           19         13
2060:     正在           32         27
2061:     質詢           13          9
```
```r
#準備製作向量模型，移動窗口設置為五個詞組
a.vectorizer=vocab_vectorizer(pruned_vocab,grow_dtm=T,skip_grams_window=5)


#1. 製作term-corpus co-occurrence matrix
a.tcm=create_tcm(a.token,a.vectorizer)

#2. 製作document-term co-occurrence matrix
a.dtm=create_dtm(a.token,a.vectorizer)
```
```
str(a.tcm):字詞之間的共同出現關係  
Formal class 'dgTMatrix' [package "Matrix"] with 6 slots  
  ..@ i       : int [1:124282] 421 756 225 756 920 479 553 1011 1139 1168 ...  
  ..@ j       : int [1:124282] 897 1721 2010 1590 1125 1139 1011 1139 1996 1306 ...  
  ..@ Dim     : int [1:2] 2061 2061  
  ..@ Dimnames:List of 2  
  .. ..$ : chr [1:2061] "蘇花公路" "超車" "主義" "監察員" ...  
  .. ..$ : chr [1:2061] "蘇花公路" "超車" "主義" "監察員" ...  
  ..@ x       : num [1:124282] 0.333 1 0.2 0.2 0.583 ...  
  ..@ factors : list()
```
```
str(a.dtm):字詞與文本之間的關聯(與第一部分最後的tdm相同)  
Formal class 'dgCMatrix' [package "Matrix"] with 6 slots  
  ..@ i       : int [1:57425] 1113 1116 1117 1113 1117 1123 946 973 979 901 ...  
  ..@ p       : int [1:2062] 0 3 6 9 11 18 21 37 46 54 ...  
  ..@ Dim     : int [1:2] 1127 2061  
  ..@ Dimnames:List of 2  
  .. ..$ : chr [1:1127] "早安，今天是端午節，大家難免又要戰一下哪一種粽子比較好，雖然我是臺北市長，但我想是這樣啦，不管是鹹中帶香的北部"...   
  .. ..$ : chr [1:2061] "蘇花公路" "超車" "主義" "監察員" ...  
  ..@ x       : num [1:57425] 1 3 10 1 9 2 2 5 5 9 ...  
.  ..@ factors : list()  
```
```r
#以MDS方法將字詞向量(from a.dtm)投影在二維平面中
word.mds=cmdscale(1-sim2(t(as.matrix(a.dtm)),method="cosine"))
word.mds_df=as.data.frame(word.mds)
row.names(word.mds_df)=row.names(word.mds)

ggplot(word.mds_df,aes(x=V1,y=V2))+
labs(x="Dimension 1",y="Dimension 2")+
annotate("text",x=word.mds_df$V1,y=word.mds_df$V2,
label=row.names(word.mds_df),size=3) 
```
![](https://raw.githubusercontent.com/tan800630/FBapi_analysis/master/pic/KoWJ_word_MDS.png)
*建議點選圖片放大服用*  

此次文字資料的清理上沒有非常細緻，因此圖片看起來並沒有非常的漂亮。然而仍看的到相關的字詞被放在附近的地方，例如**台大、醫院、醫師、醫療**。另外可以看到柯文哲似乎被斷詞成"柯文"，目前嘗試過即使在辭典裡加入"柯文哲"一詞仍然是相同的結果，若想要更精確的結果似乎需要再做微調。  

```r
#3. 製作GloVe model
fit=GlobalVectors$new(word_vectors_size=100,vocabulary=pruned_vocab,x_max=30)
fit$fit(a.tcm,n_iter=15)

#產生GloVe model下每一個字詞的向量
word.vec=fit$get_word_vectors()
rownames(word.vec)=rownames(a.tcm)

Encoding(rownames(word.vec))="UTF-8"

#看一下字詞向量資料
View(word.vec)

#在此同樣也可以以向量之間的關係作為詞彙彼此之間的關聯(相似性)

#字詞類比的函數(國王-皇后 v.s.男性-女性)
get_analogy=function(king,man,woman){
	#Hint: establish an analogy logic,vec(queen)=vec(king)~vec(man)+vec(woman)
	queen=word.vec[king,,drop=F]-word.vec[man,,drop=F]+word.vec[woman,,drop=F]

	#Hint: calculate the cosine-similarity among vec(queen) and other word vectors
	cos.dist=text2vec:::sim2(x=queen,y=word.vec,method="cosine",norm="l2")

	#please show the top-10 words for this analogy task
	head(sort(cos.dist[1,],decreasing=T),10)
}

#這裡的結果不大穩定，個人認為是因為資料量不夠的問題
get_analogy("台灣","市府","市民")
```

## 其他相關資源

[R軟體與FB api-Text mining 北醫生統研究中心 江奕副統計分析師](http://biostat.tmu.edu.tw/enews/ep_download/15rb.pdf)  
[GloVe model 另一種word embedding方法](http://www.pengfoo.com/machine-learning/2017-04-11)

##### tmcn/Rwordseg 套件
[R語言推廣_中文文字探勘 0419 陳嘉葳](https://docs.google.com/presentation/d/1IP5vFmBlGPBp32bWDqSpGYLox5QVmenFAfPwcOseQhQ/edit#slide=id.g271c06b53_0109)
[Mr. Opengate-中文文本探勘初探:TF-IDF in R](http://mropengate.blogspot.tw/2016/04/tf-idf-in-r-language.html)  

##### jiebaR/text2vec 套件
[jiebaR中文分詞文檔](https://qinwenfeng.com/jiebaR/)
[R語言中文分詞包jiebaR](http://blog.fens.me/r-word-jiebar/)
[20170113手把手教你R語言分析實務](https://www.slideshare.net/tw_dsconf/r-70971199)  