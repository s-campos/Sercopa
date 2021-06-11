#.libPaths("c://Local/R/libs")
# Including needed libraries
library(qdap)
library(XML)
library(tm)
library(splitstackshape)
library(caret)
library(stringr)
library(purrr)

start.time <- Sys.time()

# Preparing parameters
n <- 1000      # Number of words in the vocabulary. Usually used 1000 or 10000
k <- 5        # Number of folds in cross-validation. Usually used 10
r <- 3        # Number of repeats in cross-validation. Usually used 3
path_training <- "C:/Users/pabponro/Downloads/Text Mining en Social Media/pan21-author-profiling-training-2021-03-14/en"	# Your training path
path_test <- "C:/Users/pabponro/Downloads/Text Mining en Social Media/pan21-author-profiling-test-without-gold-for-participants/en"			# Your test path
lang <- "en"



# Auxiliar functions
# * GenerateVocabulary: Given a corpus (training set), obtains the n most frequent words
# * GenerateBoW: Given a corpus (training or test), and a vocabulary, obtains the bow representation

# GenerateVocabulary: Given a corpus (training set), obtains the n most frequent words
GenerateVocabulary <- function(path, n = 1000, lowcase = TRUE, long2 = TRUE, url = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", verbose = TRUE) {
  setwd(path)
  
  # Reading corpus list of files
  files = list.files(pattern="*.xml")
  
  # Reading files contents and concatenating into the corpus.raw variable
  corpus.raw <- NULL
  i <- 0
  for (file in files) {
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
    i <- i + 1
    if (verbose) print(paste(i, " ", file))
  }
  
  # Preprocessing the corpus
  corpus.preprocessed <- corpus.raw
  
  if (lowcase) {
    if (verbose) print("Tolower...")
    corpus.preprocessed <- tolower(corpus.preprocessed)
  }
  
  if (long2) {
    corpus.preprocessed <- keep(.x = corpus.preprocessed, .p = function(x){str_length(x) > 1})
  }
  
  if (url) {
    corpus.preprocessed <- str_replace_all(corpus.preprocessed,"http\\S*","")
  }
  
  if (punctuations) {
    if (verbose) print("Removing punctuations...")
    corpus.preprocessed <- removePunctuation(corpus.preprocessed)
  }
  
  if (numbers) {
    if (verbose) print("Removing numbers...")
    corpus.preprocessed <- removeNumbers(corpus.preprocessed)
  }
  
  if (whitespaces) {
    if (verbose) print("Stripping whitestpaces...")
    corpus.preprocessed <- stripWhitespace(corpus.preprocessed)
  }
  
  if (swlang!="")	{
    if (verbose) print(paste("Removing stopwords for language ", swlang , "..."))
    corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang))
  }
  
  if (swlist!="") {
    if (verbose) print("Removing provided stopwords...")
    corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
  }
  
  # Generating the vocabulary as the n most frequent terms
  if (verbose) print("Generating frequency terms")
  corpus.frequentterms <- freq_terms(corpus.preprocessed, n)
  if (verbose) plot(corpus.frequentterms)
  
  return (corpus.frequentterms)
}

# GenerateBoW: Given a corpus (training or test), and a vocabulary, obtains the bow representation
GenerateBoW <- function(path, vocabulary, btruth = TRUE, n = 100000, lowcase = TRUE, long2 = TRUE, url = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", verbose = TRUE) {
  setwd(path)
  
  # Reading the truth file
  if (btruth) {
    truth <- read.csv("truth.txt", sep=":", header=FALSE)
    truth <- truth[,c(1,4)]
    colnames(truth) <- c("author", "class")
  }
  
  i <- 0
  bow <- NULL
  # Reading the list of files in the corpus
  files = list.files(pattern="*.xml")
  for (file in files) {
    # Obtaining truth information for the current author
    author <- gsub(".xml", "", file)
    
    if (btruth)
      class <- truth[truth$author==author,"class"]
    else
      class <- 1
    
    if (class==1) {
      class = "faker"
      
    } else {
      class = "normal"
    }
    
    # Reading contents for the current author
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    txtdata <- xpathApply(xmlfile, "//document", function(x) xmlValue(x))
    
    # Preprocessing the text
    if (lowcase) {
      txtdata <- tolower(txtdata)
    }
    
    if (long2) {
      txtdata <- keep(.x = txtdata, .p = function(x){str_length(x) > 1})
    }
    
    if (url) {
      txtdata <- str_replace_all(txtdata,"http\\S*","")
    }
    
    if (punctuations) {
      txtdata <- removePunctuation(txtdata)
    }
    
    if (numbers) {
      txtdata <- removeNumbers(txtdata)
    }
    
    if (whitespaces) {
      txtdata <- stripWhitespace(txtdata)
    }
    
    # Building the vector space model. For each word in the vocabulary, it obtains the frequency of occurrence in the current author.
    line <- author
    freq <- freq_terms(txtdata, n)
    for (word in vocabulary$WORD) {
      thefreq <- 0
      if (length(freq[freq$WORD==word,"FREQ"])>0) {
        thefreq <- freq[freq$WORD==word,"FREQ"]
      }
      line <- paste(line, ",", thefreq, sep="")
    }
    
    line <- paste(class, ",", line, sep="")
    
    # New row in the vector space model matrix
    bow <- rbind(bow, line)
    i <- i + 1
    
    if (verbose) {
      print(paste(i, author, class))
    }
  }
  
  return (bow)
}



# GENERATE VOCABULARY
vocabulary <- GenerateVocabulary(path_training, n, swlang=lang)
vocabulary <- vocabulary[11:nrow(vocabulary),]

# GENERATING THE BOW FOR THE TRAINING SET
bow_training <- GenerateBoW(path_training, vocabulary)

# PREPARING THE VECTOR SPACE MODEL FOR THE TRAINING SET
training <- concat.split(bow_training, "V1", ",")
training <- cbind(training[,2], training[,4:ncol(training)])
names(training)[1] <- "theclass"


#SEPARAMOS POR HATER Y NO HATERS PARA AÑADIR UNA COLUMNA QUE SEA SI USA (O CUANTO USA) LAS PALABRAS DE LOS HATERS
library(dplyr)
fakers <- training %>% filter(theclass=="faker")
normal <- training %>% filter(theclass=="normal")

fakerswsum <- fakers %>% group_by(theclass) %>% summarise_each(funs(sum))
fakerswsum <- fakerswsum[2:91]
fakerswsum <- sort(fakerswsum, decreasing = TRUE)
fakersmfw <- fakerswsum[1:5]
#INCLUIMOS AHORA EN CADA FILA UNA VARIABLE MAS QUE ES SI USA LAS PALABRAS HATER
#fakecolumns <- training[,str(unlist(as.list(fakersmfw)))]
colnames <- names(fakersmfw)
fakecolumns <- training %>% select(contains(names(fakersmfw)))
fakecolsum <-  rowSums (fakecolumns[ , 1:5])
training <- cbind(training,fakecolsum)
#VEMOS QUE NO MEJORA MUCHO EL TEMA

# HACEMOS PCA SOBRE EL TRAINING
# trpca = prcomp(data.matrix(training))

# Learning a SVM and evaluating it with k-fold cross-validation
train_control <- trainControl( method="repeatedcv", number = k , repeats = n)
print("SVM...")
model_SVM <- train( theclass~., data= training, trControl = train_control, method = "svmLinear")
print(model_SVM)
#print("GLM...")
#model_GLM <- train( theclass~., data= training, trControl = train_control, method = "gaussprLinear")
#print(model_GLM)


# Learning a SVM with the whole training set and without evaluating it
train_control <- trainControl(method="none")
model_SVM <- train( theclass~., data= training, trControl = train_control, method = "svmLinear")

# GENERATING THE BOW FOR THE TEST SET
bow_test <- GenerateBoW(path_test, vocabulary, btruth = FALSE)
#
# Preparing the vector space model and truth for the test set
test <- concat.split(bow_test, "V1", ",")
truth <- unlist(test[,3])
test <- test[,4:ncol(test)]


# AÑADIMOS LA COLUMNA DE PALABRAS HATERS AL TEST
fakecolumns <- test %>% select(contains(names(fakersmfw)))
fakecolsum <-  rowSums (fakecolumns[ , 1:5])
test <- cbind(test,fakecolsum)
#VEMOS QUE NO MEJORA MUCHO EL TEMA

# Predicting and evaluating the prediction
pred_SVM <- predict(model_SVM, test)
#confusionMatrix(pred_SVM, truth)

# GENERAMOS LOS XML CON LA PREDICCION
clase <- NULL
for (i in 1:length(pred_SVM)){
  if (pred_SVM[i]=="normal"){
    clase[i]<-0
  } else {
    clase[i]<-1
  }
}
resultado<-data.frame(truth,clase, row.names = NULL)
setwd("C:/Users/pabponro/Downloads/Text Mining en Social Media/resultados")

textoxml<-NULL
for (i in 1:nrow(resultado)) {
  textoxml[i] <- paste0("<author id=","\"",resultado$truth[i],"\"", "\n",
                        "lang=\"en\"", "\n",
                        "type=\"",resultado$clase[i], "\"", "\n",
                        "/",">")
  name <- paste0(resultado$truth[i], ".xml")
  fileConn<-file(name)
  writeLines(textoxml[i], fileConn)
  close(fileConn)
  
}


end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)




