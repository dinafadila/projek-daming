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

# Menghilangkan string + dan , serta mengubah ke numeric
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
data$size_norm = NA
data[grep('k', data$Size),]$size_norm <- size_k
data[grep('M', data$Size),]$size_norm <- size_m

# Menghapus variable size yg lama
data$Size <- NULL 

# Menghilangkan string $
data$Price <- gsub('[$]', '', data$Price)

# Ubah factor ke numeric
data$Installs<-as.numeric(as.character(data$Installs))
data$Price <- as.numeric(as.character(data$Price))
data$Reviews <- as.numeric(as.character(data$Reviews))

data<-data[complete.cases(data),] 

str(data)

# Korelasi
library(corrplot)
c<-data.frame(data)
correlation<-c[,-c(1,2,7,9:13)] #kecuali atribut 9,10
m<-cor(correlation)
corrplot(m)

