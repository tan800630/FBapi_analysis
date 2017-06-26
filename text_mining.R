
require(dplyr)
require(Rfacebook)
require(ggplot2)

#使用token的人要記得改一下"token=fb.oauth"這段
#token="#your facebook API token#"
#需要自己抓資料的人可以google  使用R分析Facebook   相關教學
start_date <- "2013/01/01"
end_date <- "2017/05/31"
page.id <- "DoctorKoWJ"

dir="C:/Users/tan/Documents"

page <- getPage(page.id,token=fb.oauth,n=3000,since=start_date,until=end_date)

#將資料存出
write.csv(page,file=paste0(dir,"/data/page_KoWJ.csv"))
#讀取檔案
dat=read.csv(paste0(dir,"/data/page_KoWJ.csv"))

#把沒有留言Po文的刪掉
dat=dat[-which(is.na(dat$message)),]

dat$message=as.character(dat$message)


#########1.使用tmcn & Rwordseg套件進行字詞分析#########
##參考國立高雄大學資管所 陳嘉葳 之文章
##http://rstudio-pubs-static.s3.amazonaws.com/12422_b2b48bb2da7942acaca5ace45bd8c60c.html

require(tm)
require(tmcn)
require(Rwordseg)

#製作語料庫
d.corpus=SimpleCorpus(VectorSource(dat$message),control=list(language = "CN"))

insertWords(c("世大運","士林","內湖","大同","松山","萬華","大安","信義","南港","文山","中山","北投","柯P","捷運"))

#只刪除標點符號
d.corpus = d.corpus %>% tm_map(removePunctuation)

#文本斷詞
d.corpus=tm_map(d.corpus, segmentCN, nature = TRUE)


#停止詞清除

#直接用tm_map做字詞清除將出現
#Error in UseMethod("removeWords", x) : 
#沒有適用的方法可將 'removeWords' 套用到 "list" 類別的物件

#使用lapply處理
d.corpus = lapply(d.corpus,function(x) removeWords(x,words=stopwordsCN()))
d.corpus = Corpus(VectorSource(d.corpus))


#製作term-document matrix時會出現問題，需要用以下的code
#參考網址：
#http://mylearnho.blogspot.tw/2015/09/chinese-termdocumentmatrix-in-r-tm.html?m=1

#########################################################
library("slam")
##  Necessary function basedon "tm"packages.
##  Product TermDocumentMatrix for Chinese on R after version 3.0.2
##
##  Modified command "words" on package NLP 
wordsCN<-function(x,...){
  words<-unlist(segmentCN(x))
  return(words)
}
##  Modified command "termFreq" on package tm
termFreqCN<-
  function (doc, control = list()) 
  {
    #stopifnot(inherits(doc, "TextDocument"), is.list(control))
    .tokenize <- control$tokenize
    if (is.null(.tokenize) || identical(.tokenize, "wordsCN")) 
      .tokenize <- wordsCN
    else if (identical(.tokenize, "MC")) 
      .tokenize <- MC_tokenizer
    else if (identical(.tokenize, "scan")) 
      .tokenize <- scan_tokenizer
    else if (NLP::is.Span_Tokenizer(.tokenize)) 
      .tokenize <- NLP::as.Token_Tokenizer(.tokenize)
    if (is.function(.tokenize)) 
      txt <- .tokenize(doc)
    else stop("invalid tokenizer")
    .tolower <- control$tolower
    if (is.null(.tolower) || isTRUE(.tolower)) 
      .tolower <- tolower
    if (is.function(.tolower)) 
      txt <- .tolower(txt)
    .removePunctuation <- control$removePunctuation
    if (isTRUE(.removePunctuation)) 
      .removePunctuation <- removePunctuation
    else if (is.list(.removePunctuation)) 
      .removePunctuation <- function(x) do.call(removePunctuation, 
                                                c(list(x), control$removePunctuation))
    .removeNumbers <- control$removeNumbers
    if (isTRUE(.removeNumbers)) 
      .removeNumbers <- removeNumbers
    .stopwords <- control$stopwords
    if (isTRUE(.stopwords)) 
      .stopwords <- function(x) x[is.na(match(x, stopwords(meta(doc, 
                                                                "language"))))]
    else if (is.character(.stopwords)) 
      .stopwords <- function(x) x[is.na(match(x, control$stopwords))]
    .stemming <- control$stemming
    if (isTRUE(.stemming)) 
      .stemming <- function(x) stemDocument(x, meta(doc, "language"))
    or <- c("removePunctuation", "removeNumbers", "stopwords", 
            "stemming")
    nc <- names(control)
    n <- nc[nc %in% or]
    for (name in sprintf(".%s", c(n, setdiff(or, n)))) {
      g <- get(name)
      if (is.function(g)) 
        txt <- g(txt)
    }
    if (is.null(txt)) 
      return(setNames(integer(0), character(0)))
    dictionary <- control$dictionary
    tab <- if (is.null(dictionary)) 
      table(txt)
    else table(factor(txt, levels = dictionary))
    if (names(tab[1])=="") tab <- tab[-1]
    bl <- control$bounds$local
    if (length(bl) == 2L && is.numeric(bl)) 
      tab <- tab[(tab >= bl[1]) & (tab <= bl[2])]
    nc <- nchar(names(tab), type = "chars")
    wl <- control$wordLengths
    lb <- if (is.numeric(wl[1])) wl[1] else 3
    ub <- if (is.numeric(wl[2])) wl[2] else Inf
    tab <- tab[(nc >= lb) & (nc <= ub)]
    storage.mode(tab) <- "integer"
    class(tab) <- c("term_frequency", class(tab))
    tab
  }

## Useful for TermDocumentMatrix
TermDocumentMatrix_classes <-
  c("TermDocumentMatrix", "simple_triplet_matrix")
## Useful for TermDocumentMatrix
.TermDocumentMatrix <-
  function(x, weighting)
  {
    x <- as.simple_triplet_matrix(x)
    if(!is.null(dimnames(x)))
      names(dimnames(x)) <- c("Terms", "Docs")
    class(x) <- TermDocumentMatrix_classes
    ## <NOTE>
    ## Note that if weighting is a weight function, it already needs to
    ## know whether we have a term-document or document-term matrix.
    ##
    ## Ideally we would require weighting to be a WeightFunction object
    ## or a character string of length 2.  But then
    ##   dtm <- DocumentTermMatrix(crude,
    ##                             control = list(weighting =
    ##                                            function(x)
    ##                                            weightTfIdf(x, normalize =
    ##                                                        FALSE),
    ##                                            stopwords = TRUE))
    ## in example("DocumentTermMatrix") fails [because weightTfIdf() is
    ## a weight function and not a weight function generator ...]
    ## Hence, for now, instead of
    ##   if(inherits(weighting, "WeightFunction"))
    ##      x <- weighting(x)
    ## use
    if(is.function(weighting))
      x <- weighting(x)
    ## and hope for the best ...
    ## </NOTE>
    else if(is.character(weighting) && (length(weighting) == 2L))
      attr(x, "weighting") <- weighting
    else
      stop("invalid weighting")
    x
  }
##  Modified command "TermDocumentMatrix" on package tm
##  and defined "TermDocumentMatrixCN"
TermDocumentMatrixCN<-
  function (x, control = list()) 
  {
    stopifnot(is.list(control))
    tflist <- lapply(unname(content(x)), termFreqCN, control)
    tflist <- lapply(tflist, function(y) y[y > 0])
    v <- unlist(tflist)
    i <- names(v)
    allTerms <- sort(unique(as.character(if (is.null(control$dictionary)) i else control$dictionary)))
    i <- match(i, allTerms)
    j <- rep(seq_along(x), sapply(tflist, length))
    docs <- as.character(meta(x, "id", "local"))
    if (length(docs) != length(x)) {
      warning("invalid document identifiers")
      docs <- NULL
    }
    m <- simple_triplet_matrix(i = i, j = j, v = as.numeric(v), 
                               nrow = length(allTerms), ncol = length(x), dimnames = list(Terms = allTerms, 
                                                                                          Docs = docs))
    bg <- control$bounds$global
    if (length(bg) == 2L && is.numeric(bg)) {
      rs <- row_sums(m > 0)
      m <- m[(rs >= bg[1]) & (rs <= bg[2]), ]
    }
    weighting <- control$weighting
    if (is.null(weighting)) 
      weighting <- weightTf
    .TermDocumentMatrix(m, weighting)
  }
################################################

#可將上面的程式碼存成一個r檔，需要時在source即可
#source("TermDocumentMatrixCN.r")

#製作Term-Document matrix
tdm=TermDocumentMatrixCN(d.corpus,control=list(wordLengths = c(2,Inf)))

#文字雲
require(wordcloud)

m1 <- as.matrix(tdm)
v <- sort(rowSums(m1), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
wordcloud(d$word,d$freq,max.words=200,min.freq=10,random.order=F,
    ordered.colors=F,colors=rainbow(length(row.names(m1))))


#字詞關聯
findAssocs(tdm,"大家",0.3)


#########2.使用jiebaR & text2vec套件進行字詞分析#########

Sys.setlocale(category = "LC_ALL", locale = "cht")

require(jiebaR)
require(text2vec)


text_min=worker(user="C:/Users/user/Documents/R/win-library/3.4/jiebaRD/dict/jieba_dictn.utf8")

#jiebaR斷詞
a=sapply(dat$message,function(x) segment(x,text_min))
a=filter_segment(a,c("的","是","也","在","為","和","有","了"))

#製作詞組字典
a.token=itoken(a)
a.vocab=create_vocabulary(a.token,ngram=c(1,1))

a.vocab.ns=a.vocab
a.vocab.ns$vocab=a.vocab$vocab[-which(nchar(a.vocab$vocab$terms)==1),]

#刪除出現數量過少的字詞
pruned_vocab=prune_vocabulary(a.vocab.ns,term_count_min=50,
doc_proportion_max =0.5,doc_proportion_min=0.001)

a.vectorizer=vocab_vectorizer(pruned_vocab,grow_dtm=T,skip_grams_window=5)


#term-corpus co-occurrence matrix
a.tcm=create_tcm(a.token,a.vectorizer)

#document-term co-occurrence matrix
a.dtm=create_dtm(a.token,a.vectorizer)

#製作glove model
fit=GlobalVectors$new(word_vectors_size=100,vocabulary=pruned_vocab,x_max=30)
fit$fit(a.tcm,n_iter=15)

word.vec=fit$get_word_vectors()
rownames(word.vec)=rownames(a.tcm)


Encoding(rownames(word.vec))="UTF-8"

#View(word.vec)

#對字詞作MDS
word.mds=cmdscale(1-sim2(t(as.matrix(a.dtm)),method="cosine"))
word.mds_df=as.data.frame(word.mds)
row.names(word.mds_df)=row.names(word.mds)

ggplot(word.mds_df,aes(x=V1,y=V2))+
labs(x="Dimension 1",y="Dimension 2")+
annotate("text",x=word.mds_df$V1,y=word.mds_df$V2,
label=row.names(word.mds_df),size=3) 



#######待續###########

#get analogy
get_analogy=function(king,man,woman){
	#Hint: establish an analogy logic,vec(queen)=vec(king)~vec(man)+vec(woman)
	queen=word.vec[king,,drop=F]-word.vec[man,,drop=F]+word.vec[woman,,drop=F]

	#Hint: calculate the cosine-similarity among vec(queen) and other word vectors
	cos.dist=text2vec:::sim2(x=queen,y=word.vec,method="cosine",norm="l2")

	#please show the top-10 words for this analogy task
	head(sort(cos.dist[1,],decreasing=T),10)
}

