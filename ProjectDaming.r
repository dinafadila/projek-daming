data = read.csv('~/googleplaystore.csv',header=TRUE, sep=',')

# Eksplorasi Data
summary(data)
str(data)

# Cek missing value
library(mice)
md.pattern(data) 

# Mengisi Missing Value
data$Rating[is.na(data$Rating)] <- names(sort(-table(data$Rating)))[1]
md.pattern(data)

