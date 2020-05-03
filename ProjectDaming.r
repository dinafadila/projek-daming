data = read.csv('~/googleplaystore.csv',header=TRUE, sep=',')

#---------------- 1. Pra Proses Data--------------------------
# Eksplorasi Data
summary(data)
str(data)

# Cek missing value
library(mice)
md.pattern(data) 

# Menghilangkan missing value
data<-data[complete.cases(data),] 
md.pattern(data)

# Menghilangkan char + dan ,
library(stringr)
data$Installs<-str_replace_all(data$Installs,"[+]","")
data$Installs<-str_replace_all(data$Installs,",","")

# Menghilangkan satuan M pada size
size_m <- data[grep('M', data$Size),]$Size 
size_m <- gsub('M', '', size_m) 
size_m <- as.numeric(size_m) 

# Menghilangkan satuan K pada size dan diubah ke MB dibagi 1024
size_k <- data[grep('k', data$Size),]$Size 
size_k <- gsub('k', '', size_k) 
size_k <- as.numeric(size_k)/1024 

# Menggabungkan kedua size tersebut
data$Size_norm = NA
data[grep('k', data$Size),]$Size_norm <- size_k
data[grep('M', data$Size),]$Size_norm <- size_m

# Menghapus variable size yg lama
data$Size <- NULL 

# Menghilangkan string $
data$Price <- gsub('[$]', '', data$Price)

data<-data[complete.cases(data),] 


# Ubah factor ke numeric
data$Installs<-as.numeric(as.character(data$Installs))
data$Price <- as.numeric(as.character(data$Price))
data$Reviews <- as.numeric(as.character(data$Reviews))

data<-data[complete.cases(data),] 

str(data)

# Korelasi
library(corrplot)
c<-data.frame(data)
correlation<-c[,-c(1,2,6,8:12)] #kecuali yg non numeric
m<-cor(correlation)
corrplot(m)

#diskretisasi
library(infotheo)
ef.install <- discretize(data$Installs,"equalfreq", 5)
ef.install$X = as.factor(ef.install$X)
data$class = ef.install$X

str(data)

set.seed(1234)

ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))
trainData <- data[ind==1,]
testData <- data[ind==2,]

library(party)
myFormula <- class ~ Rating + Reviews + Price + Size_norm
data_ctree <- ctree(myFormula, data = trainData, controls = ctree_control(minsplit = 500))

print(data_ctree)
plot(data_ctree)
plot(data_ctree, type="simple")

# Memprediksi kelas data pada data testing
ctree_pred <- predict(data_ctree, newdata = testData)
library("caret")
confusionMatrix(ctree_pred, testData$class)


