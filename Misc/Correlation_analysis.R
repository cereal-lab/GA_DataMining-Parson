library("readxl")
library("Hmisc")
library("corrplot")
library("PerformanceAnalytics")
library("stringr")


setwd("~/Desktop")

par(mfrow=c(2,2))

data_1 <- read_excel("28631_out.xlsx")

correlation_test <- data_1[, c('Silhouette', 'AIC', 'Dispersion','R_squared')]
chart.Correlation(correlation_test, histogram=TRUE, pch=19)

data_2 <- read_excel("28695_out.xlsx")

correlation_test <- data_2[, c('Silhouette', 'AIC', 'Dispersion','R_squared')]
chart.Correlation(correlation_test, histogram=TRUE, pch=19)


data_3 <- read_excel("28696_out.xlsx")

correlation_test <- data_3[, c('Silhouette', 'AIC', 'Dispersion','R_squared')]
chart.Correlation(correlation_test, histogram=TRUE, pch=19)

data_4 <- read.csv("28631_best_final_output.csv")
data_4$sequence <- as.character(data_4$sequence)
data_4$new_sequence <- as.character(data_4$new_sequence)

data_4$num_T <- str_count(data_4$sequence, "T")

# Whether the sequence contains three T in a row
data_4$TT <- NA

for (i in 1:nrow(data_4)){
  if(grepl("TT", data_4$sequence[i]) == TRUE){
    data_4$TT[i] <- 1
  }
  else{
    data_4$TT[i] <- 0
  }
}

rm(i)

# Whether the sequence contains two P in a row
data_4$PP <- NA

for (i in 1:nrow(data_4)){
  if(grepl("PP", data_4$sequence[i]) == TRUE){
    data_4$PP[i] <- 1
  }
  else{
    data_4$PP[i] <- 0
  }
}

rm(i)

correlation_test <- data_4[, c('time_spent', 'Extra_Steps', 'num_T', 'TT', 'PP')]
chart.Correlation(correlation_test, histogram=TRUE, pch=19)








# res <- cor(correlation_test)
# round(res, 2)
# 
# corrplot(res, type = "upper", order = "hclust", 
#          tl.col = "black", tl.srt = 45)
# 
# res2 <- rcorr(as.matrix(correlation_test))
# res2
