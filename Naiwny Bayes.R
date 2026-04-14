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

nrow(data) #jest 28627 obserwacji, niech 5000 pójdzie na zbiór testowy i 2500 na walidacyjny
test_sample=sample(1:nrow(data), size = 5000, replace= F, set.seed(42))
valid_sample = sample(1:(nrow(data)-5000), size = 2500, replace = F)

data=data[, c('type', 'title')]
data_test=data[test_sample, ]
data = data[-test_sample, ]
data_valid = data[valid_sample, ]
data_train=data[-valid_sample, ]
rm(data)

dtm_test=dtm_sparse[test_sample, ]
dtm_sparse = dtm_sparse[-test_sample, ]
dtm_valid = dtm_sparse[valid_sample, ]
dtm_train=dtm_sparse[-valid_sample, ]
rm(dtm_sparse)

#Ile słów wybrać? W tym celu pomoże nam zbiór walidacyjny

word_count1 = colSums(dtm_train)
freq_words1 = names(word_count1[word_count1>=25])
dtm_train1 = dtm_train[, freq_words1]
dtm_valid1 = dtm_valid[, freq_words1]
dtm_test1 = dtm_test[, freq_words1]
dim(dtm_test1) # zostało 15823 najważniejszych

word_count2 = colSums(dtm_train)
freq_words2 = names(word_count2[word_count2>=100])
dtm_train2 = dtm_train[, freq_words2]
dtm_valid2 = dtm_valid[, freq_words2]
dtm_test2 = dtm_test[, freq_words2]
dim(dtm_test2) #zostało 6292 najważniejszych słów

dtm_title_test = dtm_title_sparse[test_sample, ]
dtm_title_sparse = dtm_title_sparse[-test_sample, ]
dtm_title_valid = dtm_title_sparse[valid_sample, ]
dtm_title_train = dtm_title_sparse[-valid_sample, ]
rm(dtm_title_sparse)

word_count_title = colSums(dtm_title_train) #dla tytułów ustawmy stałą liczbę, bez walidacji
freq_words_title = names(word_count_title[word_count_title>=5])
dtm_title_train = dtm_title_train[, freq_words_title]
dtm_title_valid = dtm_title_valid[, freq_words_title]
dtm_title_test = dtm_title_test[, freq_words_title]
dim(dtm_title_train) #zostało 1797 najważniejszych słów

train1 = cbind(dtm_train1, dtm_title_train) #ostateczne zbiory uczące, testowe i walidacyjne
valid1 = cbind(dtm_valid1, dtm_title_valid)
test1 = cbind(dtm_test1, dtm_title_test)

train2 = cbind(dtm_train2, dtm_title_train)
valid2 = cbind(dtm_valid2, dtm_title_valid)
test2 = cbind(dtm_test2, dtm_title_test)

model1 = bernoulli_naive_bayes(x=train1, data_train$type) #wyświetla ostrzeżenie bez wygładzenia
model_laplace1 = bernoulli_naive_bayes(x=train1, data_train$type, laplace = 1) #spróbujmy dodać wygładzenie Laplace'a
pred1 = predict(model1, valid1, type='class')
pred_laplace1 = predict(model_laplace1, valid1, type='class')

model2 = bernoulli_naive_bayes(x=train2, data_train$type) #sprawdźmy drugi model
model_laplace2 = bernoulli_naive_bayes(x=train2, data_train$type, laplace = 1)
pred2 = predict(model2, valid2, type='class')
pred_laplace2 = predict(model_laplace2, valid2, type='class')

#teraz testujemy wszystkie 4 modele na zbiorze walidacyjnym
print("model 1:")
gmodels::CrossTable(pred1, data_valid$type, prop.chisq = F, prop.c = F, prop.r = F, dnn=c('predicted', 'actual'))
print(sum(as.character(pred1)==data_valid[['type']])/25) #dokładność 66.04%

print('model 1 lablace:')
gmodels::CrossTable(pred_laplace1, data_valid$type, prop.chisq = F, prop.c = F, prop.r = F, dnn=c('predicted', 'actual'))
print(sum(as.character(pred_laplace1)==data_valid[['type']])/25) #dokładność 64.20%

print('model 2:')
gmodels::CrossTable(pred2, data_valid$type, prop.chisq = F, prop.c = F, prop.r = F, dnn=c('predicted', 'actual'))
print(sum(as.character(pred2)==data_valid[['type']])/25) #dokładność 64.24%

print('model 2 laplace')
gmodels::CrossTable(pred_laplace2, data_valid$type, prop.chisq = F, prop.c = F, prop.r = F, dnn=c('predicted', 'actual'))
print(sum(as.character(pred_laplace2)==data_valid[['type']])/25) #dokładność 61.36%

#najlepszy okazał się być model pierwszy, wybieramy go i sprawdzamy ostateczną dokładność na zbiorze testowym
gmodels::CrossTable(pred1, data_test$type, prop.chisq = F, prop.c = F, prop.r = F, dnn=c('predicted', 'actual'))
sum(as.character(pred1)==data_test[['type']])/25 #ostateczna dokładność na zbiorze testowym - 65.6