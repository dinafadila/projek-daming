data = read.csv('~/googleplaystore.csv',header=TRUE, sep=',')


#---------------- 1. Pra Proses Data--------------------------
# Eksplorasi Data
dim(data)
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

# Replace "Varies with device" to NA since it is unknown
data$Min.Android.Ver = gsub("Varies with device", NA, data$Android.Ver)

# Keep only version number to 1 decimal
data$Min.Android.Ver = as.numeric(substr(data$Android.Ver, start = 1, stop = 3))
# Drop old Android version column
data$Android.Ver = NULL

data$Curr.Ver = gsub("Varies with device", NA, data$Current.Ver)
data$Curr.Ver = as.numeric(substr(data$Current.Ver, start = 1, stop = 3))
# Menghilangkan string $
data$Current.Ver = NULL

#Menghilangkan noise Price
data$Price <- gsub('[$]', '', data$Price)

# Ubah factor ke numeric
data$Installs<-as.numeric(as.character(data$Installs))
data$Price <- as.numeric(as.character(data$Price))
data$Reviews <- as.numeric(as.character(data$Reviews))

#Label Encoding Content Rating
data$ContentR[data$Content.Rating == ""] <- 0
data$ContentR[data$Content.Rating == "Adults only 18+"] <- 1
data$ContentR[data$Content.Rating == "Everyone"] <- 2
data$ContentR[data$Content.Rating == "Everyone 10+"] <- 3
data$ContentR[data$Content.Rating == "Mature 17+"] <- 4
data$ContentR[data$Content.Rating == "Teen"] <- 5
data$ContentR[data$Content.Rating == "Unrated"] <- 6
data$ContentR = as.numeric(data$ContentR)

#Label encoding Type
data$newType[data$Type == "0"] <- 0
data$newType[data$Type == "Free"] <- 0
data$newType[data$Type == "Paid"] <- 1
data$newType[data$Type == NaN] <- 0

data$App = NULL
data$Content.Rating = NULL
data$Type = NULL
str(data)

#Erase null values
data<-data[complete.cases(data),] 
md.pattern(data) 

str(data)

data$class[data$Rating <= "2"] <- "TP"
data$class[data$Rating > "2" & data$Rating <= "4"] <- "P"
data$class[data$Rating > "4"] <- "SP"

data$class <- as.factor(data$class)

str(data)

#Put all numerical variabel to X
x = data  
x$Category = NULL
x$Rating = NULL
x$Genres = NULL
x$Last.Updated = NULL
x$class = NULL
str(x)

#Correlation Analysis
# load library
library(mlbench)
library(caret)
set.seed(7)

# calculate correlation matrix
correlationMatrix <- cor(x[,1:8])

# summarize the correlation matrix
print(correlationMatrix)

# find attributes with correlation above 0.75
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)

# print the highly correlated attributes
print(highlyCorrelated)

# Oversampling
set.seed(1029)
final <- data[!(is.na(data$class)),]
final$class <- factor(final$class)

library(caTools)

split <- sample.split(final$class, SplitRatio = 0.75)

dresstrain <- subset(final, split == TRUE)
dresstest <- subset(final, split == FALSE)

## Let's check the count of unique value in the target variable
as.data.frame(table(dresstrain$class))

## Loading DMwr to balance the unbalanced class, ,, 
library(DMwR)

## Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification
balanced.data <- SMOTE(class ~., dresstrain, k=5, perc.under = 300 ,perc.over = 10000 )

as.data.frame(table(balanced.data$class))

set.seed(1234)

ind <- sample(2, nrow(balanced.data), replace=TRUE, prob=c(0.7, 0.3))
trainData <- balanced.data[ind==1,]
testData <- balanced.data[ind==2,]

str(data)

library(party)
myFormula <- class ~ Installs + Size_norm + Min.Android.Ver + Price + ContentR + newType + Curr.Ver
data_ctree <- ctree(myFormula, data = trainData, controls = ctree_control(maxdepth = 12))

print(data_ctree)
plot(data_ctree)
plot(data_ctree, type="simple")

# Confusion matrix data train
ctree_pred <- predict(data_ctree, newdata = trainData)
confusionMatrix(ctree_pred, trainData$class)

# Memprediksi kelas data pada data testing
ctree_pred <- predict(data_ctree, newdata = testData)
library("caret")

# Confusion matrix data test
confusionMatrix(ctree_pred, testData$class)
