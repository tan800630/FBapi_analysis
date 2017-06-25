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