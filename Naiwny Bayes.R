library(tm)
library(dplyr)
library(SnowballC)
library(naivebayes)
library(Matrix)

data = read.csv('data\\wikipedia.csv', row.names = "X")
data = data[, c('type', 'title', 'summary')]
summary(data)

corpus=VCorpus(VectorSource(data$summary)) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>% 
  tm_map(removeWords, stopwords()) %>% 
  tm_map(content_transformer(function(x){
    x=gsub('\n', ' ', x)
    x=gsub('\\\\n', ' ', x)
    })) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace)

dtm=DocumentTermMatrix(corpus)
rm(corpus) #przy tylu danych trzeba oszczędzać pamięć

dtm_sparse = sparseMatrix( #za dużo danych, trzeba przejść na macierze rzadkie
  i=dtm$i,
  j=dtm$j,
  x=as.numeric(dtm$v>0),
  dims = c(dtm$nrow, dtm$ncol), 
  dimnames = dimnames(dtm)
)
rm(dtm)

corpus_title=VCorpus(VectorSource(data$title)) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace) #tytuły zostały pobrane z małych liter, bez znaków interpunkcyjnych

dtm_title=DocumentTermMatrix(corpus_title)
rm(corpus_title)
dtm_title_sparse=sparseMatrix(
  i=dtm_title$i,
  j=dtm_title$j,
  x=as.numeric(dtm_title$v>0),
  dims = c(dtm_title$nrow, dtm_title$ncol), 
  dimnames = dimnames(dtm_title)
)
rm(dtm_title)

colnames(dtm_title_sparse)=paste("title", colnames(dtm_title_sparse), sep='_') #trzeba odróżnić tytuł na treści

nrow(data) #jest 28627 obserwacji, niech 5000 pójdzie na zbiór testowy
test_sample=sample(1:nrow(data), size = 5000, replace= F, set.seed(42))
data=data[, c('type', 'title')]
data_test=data[test_sample, ]
data_train=data[-test_sample, ]
rm(data)

dtm_test=dtm_sparse[test_sample, ]
dtm_train=dtm_sparse[-test_sample,]
rm(dtm_sparse)

word_count=colSums(dtm_train)
freq_word=names(word_count[word_count>=50])
dtm_train=dtm_train[, freq_words]
dtm_test=dtm_test[, freq_words]
dim(dtm_test) #zostało 16216 najważniejszych słów

dtm_title_test=dtm_title_sparse[test_sample, ]
dtm_title_train=dtm_title_sparse[-test_sample,]
rm(dtm_title_sparse)
word_count_title=colSums(dtm_title_train)
freq_words_title=names(word_count_title[word_count_title>=5])
dtm_title_train=dtm_title_train[, freq_words_title]
dtm_title_test=dtm_title_test[, freq_words_title]
dim(dtm_title_train) #zostało 1984 najważniejszych słów

train=cbind(dtm_train, dtm_title_train) #ostateczny zbiór uczący i testowy
test=cbind(dtm_test, dtm_title_test)

model=bernoulli_naive_bayes(x=train, data_train$type) #wyświetla ostrzeżenie bez wygładzenia
model_laplace=bernoulli_naive_bayes(x=train, data_train$type, laplace = 1) #spróbujmy dodać wygładzenie Laplace'a
pred=predict(model, test, type='class')
pred_laplace=predict(model_laplace, test, type='class')
gmodels::CrossTable(pred, data_test$type, prop.chisq = F, prop.c = F, prop.r = F, dnn=c('predicted', 'actual'))
sum(as.character(pred)==data_test[['type']])/50
#dokładność na poziomie 65% bez wygładzenia
gmodels::CrossTable(pred_laplace, data_test$type, prop.chisq = F, prop.c = F, prop.r = F, dnn=c('predicted', 'actual'))
sum(as.character(pred_laplace)==data_test[['type']])/50
#dokładność na poziomie 63,42% stosując wygładzenie