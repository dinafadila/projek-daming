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


# Menghilangkan string M dan k 
data$Size<-str_replace_all(data$Size,"M","")
data$Size<-str_replace_all(data$Size,"k","")

# Menghilangkan string $
data$Price <- gsub('[$]', '', data$Price)

# Ubah factor ke numeric
data$Installs<-as.numeric(as.character(data$Installs))
data$Size<-as.numeric(as.character(data$Size))
data$Price <- as.numeric(as.character(data$Price))
data$Reviews <- as.numeric(as.character(data$Reviews))

data<-data[complete.cases(data),] 

str(data)

