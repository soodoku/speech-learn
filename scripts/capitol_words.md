### Capitol Words

Estimate relationship between words that congressmembers use and vote based ideology estimates. 

Load the libs


```r
# Load libs
library(readr) 	# read in big data a bit quicker
library(tm)	   	# pre_process text
library(glmnet) # for lasso
library(knitr)
```

To begin, get the data for the 112th congress using the [Capitol Words API](capitol_speech.py). Then 
[preprocess the data](https://github.com/soodoku/text-as-data/tree/master/preprocess_csv) and merge it with [DW-Nominate data](capitol_vote.R). 
Once you have done that, select only the relevant columns (text, party and first dimension of ideology scores of the speakers).


```r
# Load Capitol Words
cong 			<- read_csv("data/capitolwords_112_clean_dw.csv")
cong			<- as.data.frame(cong)
colnames(cong) 	<- make.names(colnames(cong))

# Select columns you need, and only Republican and Democrat speech
cong 		   <- subset(cong, subset=party!=328, select=c("bioguide_id", "speaking", "party", "dwnom1"))
colnames(cong) <- c("bioguide_id", "text", "party", "dwnom1")
```


```r
# Check text field length
cong$nletters <- sapply(cong$text, nchar)
summary(cong$nletters)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     150     624    1495    2524    3019  212600
```

```r
# lots of variation

# Let us see what the short 'speeches' are about
head(cong$text[cong$nletters < 150], 10)
```

```
## character(0)
```

```r
# This can be used to build a database of procedural statements that can
# later be taken out. For now, we are filtering out just these statements. This 
# leaves procedural language in rest of the rows intact
cong <- subset(cong, cong$nletters > 149)
```

Next, it makes sense to collapse data by congressperson. Otherwise, we are exploiting within congressperson speech correlations. For other kinds of data, it is common to collapse into 5,000--10,000 word chunks. And then to create test and training data sets.


```r
# First aggregate text by congressperson
bymem 	 	 <- with(cong, tapply(text, bioguide_id, paste, collapse= ' '))
bymem 	 	 <- data.frame(text=bymem, bioguide_id=rownames(bymem))
bymem$nchars <- sapply(bymem$text, nchar)

# Clumsy: Merge with dwnom1, and party
congsmall <- cong[!duplicated(cong$bioguide_id), c("bioguide_id", "dwnom1", "party")]
cong2     <- merge(bymem, congsmall , by="bioguide_id", all.x=T, all.y=F)
```

```r
# Stratified sample
# Takes data
# column name of column with labels

stratified <- function(data, labels, n_per_label, p=NA, seed=314159265){
	dsample <- NULL
	set.seed(seed)

	for(i in levels(as.factor(data[,labels]))) 
	{
	  dsub 		<- data[data[,labels] == i,]
	  if(is.na(p)){
	  	dsub 	<- dsub[sample(1:nrow(dsub), n_per_label), ]
	  }
	  else{
	  	dsub 	<- dsub[sample(1:nrow(dsub), ceiling(nrow(dsub) * p)), ]
	  }
	  dsample 	<- c(dsample, row.names(dsub))
  	}
	dsample
}

# Training data
train_rows	<- stratified(cong2, "party", n_per_label=200)
data_train 	<- cong2[train_rows,]

# Test data 
data_test <- cong2[-as.numeric(train_rows),]
```

Next, preprocess the text data, removing stop words, punctuations, numbers and converting text to lower case.


```r
# An abstract function to preprocess a text column
preprocess <- function(text_column)
{
	# Use tm to get a doc matrix
		corpus <- Corpus(VectorSource(text_column))
	# all lower case
		corpus <- tm_map(corpus, content_transformer(tolower))
	# remove punctuation
		corpus <- tm_map(corpus, content_transformer(removePunctuation))
	# remove numbers
		corpus <- tm_map(corpus, content_transformer(removeNumbers))
	# remove stopwords
		common <- tolower(read.csv("data/common_words.txt", header=F)$V1)
		corpus <- tm_map(corpus, removeWords, c(stopwords("english"), common))
	# stem document
		corpus <- tm_map(corpus, stemDocument)
	# strip white spaces (always at the end)
		corpus <- tm_map(corpus, stripWhitespace)
	# return
		corpus	
}

# Get preprocess training and test data
train_corpus <- preprocess(data_train$text)
test_corpus  <- preprocess(data_test$text)
```

Next, create bigrams and trigrams and then dtms. Typically four-grams and larger n-grams will be very sparse. (Try it on a smaller dataset.)


```r
# Bi-Trigram tokenizer Func.
bitrigramtokeniser <- function(x, n) {
	RWeka:::NGramTokenizer(x, RWeka:::Weka_control(min = 2, max = 3))
}

# Create a Document Term Matrix for train and test
# Bi- and Tri-	
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre7/')
library(rJava)
library(RWeka)

train_dtm  <- DocumentTermMatrix(train_corpus, control=list(wordLengths=c(2, Inf), tokenize = bitrigramtokeniser, 
											   bounds=list(global=c(floor(length(test_corpus)*.15), Inf))))
train_dtm2 <- removeSparseTerms(train_dtm, 0.90)

test_dtm  <- DocumentTermMatrix(test_corpus,  control=list(wordLengths=c(2, Inf), tokenize = bitrigramtokeniser, 
											  bounds=list(global=c(floor(length(test_corpus)*.05), Inf))))
```

Remove frequent phrases and get test matrix to have same colnames as train.


```r
# Test matrix maker
testmat <- function(train_mat_cols, test_mat){	
	# train_mat_cols <- colnames(train_mat); test_mat <- as.matrix(test_dtm)
	test_mat 	<- test_mat[, colnames(test_mat) %in% train_mat_cols]
	
	miss_names 	<- train_mat_cols[!(train_mat_cols %in% colnames(test_mat))]
	if(length(miss_names)!=0){
		colClasses  <- rep("numeric", length(miss_names))
		df 			<- read.table(text = '', colClasses = colClasses, col.names = miss_names)
		df[1:nrow(test_mat),] <- 0
		test_mat 	<- cbind(test_mat, df)
	}
	as.matrix(test_mat)
}

# Train and test matrices
train_mat  <- as.matrix(train_dtm2)
train_mat  <- train_mat/rowSums(train_mat)
test_mat   <- testmat(colnames(train_mat), as.matrix(test_dtm))
test_mat   <- test_mat/rowSums(test_mat)
```

Now, lets model the relationship between vote based ideology measures and text. 


```r
# Fit the model (cross-validated lambda)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#library(doMC);registerDoMC(cores=8)
fit_lasso 	<- cv.glmnet(train_mat, data_train$dwnom1, nfolds=5, alpha=1)
pred 		<- predict(fit_lasso, newx= test_mat, s = "lambda.min")

# Prediction Accuracy
cor(pred, data_test$dwnom1)
```

```
##        [,1]
## 1 0.8403285
```

```r
cor(pred[data_test$party==100], data_test$dwnom1[data_test$party==100])
```

```
## [1] 0.5496548
```

```r
cor(pred[data_test$party==200], data_test$dwnom1[data_test$party==200])
```

```
## [1] 0.3801268
```

Predicting the party as that is where the information is: 


```r
# Predict R/D
fit_lasso 	<- cv.glmnet(train_mat, data_train$party, nfolds=5, alpha=1, family = "binomial", type.measure="class")
pred 		<- predict(fit_lasso, newx= test_mat, s = "lambda.min", type="response")

# Prediction Accuracy
table(pred > .5, data_test$party)
```

```
##        
##         100 200
##   FALSE  40   4
##   TRUE    2  73
```

```r
sum(diag(table(pred > .5, data_test$party)))/sum(table(pred > .5, data_test$party))
```

```
## [1] 0.9495798
```

```r
# Prediction Accuracy w/ dwnom1
cor(pred, data_test$dwnom1)
```

```
##        [,1]
## 1 0.8622778
```

```r
cor(pred[data_test$party==100], data_test$dwnom1[data_test$party==100])
```

```
## [1] 0.3597082
```

```r
cor(pred[data_test$party==200], data_test$dwnom1[data_test$party==200])
```

```
## [1] 0.3131367
```

Using PCA


```r
# PCA
# pca <- prcomp(train_mat, scale=TRUE)

# Sparse PCA
library(irlba)
pc <- train_mat %*% irlba(train_mat, nv=5, nu=0)$v
cor(pc[,1], data_train$dwnom1)
```

```
## [1] 0.0726645
```

```r
# CA -- later
```

Using MNLM


```r
# textir
library(textir)
cl <- makeCluster(detectCores())
fits <- mnlm(cl, data_train$party, train_mat, bins=5,nlambda=10)
stopCluster(cl)
 
## extract coefficients
B <- coef(fits)
mean(B[-1,]==0) # sparsity in loadings
```

```
## [1] 1
```

```r
## some big loadings on `overall'
B[2,order(B[2,])[1:10]]
```

```
##    abil get   abil make  abl afford    abl come abl continu    abl find 
##           0           0           0           0           0           0 
##     abl get      abl go    abl keep    abl make 
##           0           0           0           0
```

```r
B[2,order(-B[2,])[1:10]]
```

```
##    abil get   abil make  abl afford    abl come abl continu    abl find 
##           0           0           0           0           0           0 
##     abl get      abl go    abl keep    abl make 
##           0           0           0           0
```

```r
B@Dimnames[[2]][order(-B@x)][1:50]
```

```
##  [1] "unit state"        "rise today"        "health care"      
##  [4] "american peopl"    "small busi"        "high school"      
##  [7] "urg colleagu"      "feder govern"      "make sure"        
## [10] "creat job"         "today honor"       "men women"        
## [13] "colleagu join"     "back balanc"       "rise today honor" 
## [16] "ask colleagu"      "year ago"          "rollcal vote"     
## [19] "hard work"         "right now"         "side aisl"        
## [22] "world war"         "balanc budget"     "today recogn"     
## [25] "th anniversari"    "across countri"    "ask consent"      
## [28] "nation secur"      "social secur"      "ask colleagu join"
## [31] "will continu"      "middl class"       "war ii"           
## [34] "world war ii"      "rise today recogn" "air forc"         
## [37] "work togeth"       "want thank"        "colleagu support" 
## [40] "reserv balanc"     "everi day"         "job creation"     
## [43] "look forward"      "present vote"      "million american" 
## [46] "tax cut"           "privat sector"     "move forward"     
## [49] "last year"         "pay tribut"
```
Using partial least squares: 


```r
# Partial least squares
fit <- pls(train_mat, data_train$dwnom1, K=5)
```

```
## Directions 1, 2, 3, 4, 5, done.
```

```r
summary(fit)
```

```
## 
## A pls(5) object, reduced from 5509 input variables. 
## 
## Forward regression summary:
## 
## Call:
## lm(formula = as.numeric(y) ~ z)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.143321 -0.022337 -0.000052  0.019455  0.151272 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 0.043775   0.001968   22.25   <2e-16 ***
## z1          0.426498   0.001970  216.50   <2e-16 ***
## z2          0.139898   0.001970   71.01   <2e-16 ***
## z3          0.115503   0.001970   58.63   <2e-16 ***
## z4          0.074257   0.001970   37.69   <2e-16 ***
## z5          0.044548   0.001970   22.61   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.03935 on 394 degrees of freedom
## Multiple R-squared:  0.9932,	Adjusted R-squared:  0.9931 
## F-statistic: 1.146e+04 on 5 and 394 DF,  p-value: < 2.2e-16
```

```r
plot(fit, pch=21, bg=c(6,5,4,3,2)[data_train$party])
```

![plot of chunk pls](figure/pls-1.png) 

```r
ppred <- predict(fit, newdata=test_mat)
cor(ppred, data_test$dwnom1)
```

```
##           [,1]
## [1,] 0.8348391
```

```r
cor(ppred[data_test$party==100], data_test$dwnom1[data_test$party==100])
```

```
## [1] 0.4448893
```

```r
cor(ppred[data_test$party==200], data_test$dwnom1[data_test$party==200])
```

```
## [1] 0.5032315
```
