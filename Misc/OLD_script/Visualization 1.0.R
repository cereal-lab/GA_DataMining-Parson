setwd("~/Documents/GitHub/Personal Projects/Research/Parsons Puzzles - Amruth/Misc")

library("TraMineR")
library("stringr")

require("pscl") # alternatively can use package ZIM for zero-inflated models
library("lmtest")
library("MASS")
library("rsq")

virtualization_seq <- function(df){

cl1.3 <- df$Clusters
max.cluster <- max(cl1.3)
temp_2 <- strsplit(df$final_sequence, "")
type_label <- sprintf("Type %d",1:max.cluster)
max.length <- max(sapply(temp_2, length))
temp_2 <- lapply(temp_2, function(v) { c(v, rep(NA, max.length-length(v)))})
temp <- data.frame(do.call(rbind, temp_2))

temp.alphabet <- c("T","P","O","I","V","F","E", "R");
temp.labels <- c("Trash","Parenthesis","Output","Input","Variable","If","Else", "Reorder")
temp.scodes <- c("T","P","O","I","V","F","E", "R")
data_1_E.seq <- seqdef(temp, 0:max.length, alphabet = temp.alphabet, states=temp.scodes, labels=temp.labels)

#idxs used to find interval representing the range to be plotted.
seqlegend(data_1_E.seq) 
seqiplot(data_1_E.seq, with.legend=F, main = "First 10 Sequence" ) 
seqfplot(data_1_E.seq, pbarw=T, with.legend=F, main = "Most Frequent Sequences") 
seqdplot(data_1_E.seq, with.legend=F,  main = "State Distribution") 

cl1.3fac <- factor(cl1.3, labels = type_label)

seqdplot(data_1_E.seq, group=cl1.3fac,main = "Type State Distribution")
seqfplot(data_1_E.seq, group=cl1.3fac, pbarw=T, main = "Most Frequent Sequences")

}

## Check for over/underdispersion in the model
dispersion_calc <- function(a, df) {
  E2 <- resid(a, type = "pearson")
  N  <- nrow(df)
  p  <- length(coef(a)) 
  d <- sum(E2^2) / (N - p)
  
  return(d)
}

## Check for rsq of the model
rsq_calc <- function(a, b) {
  sse <- sum((fitted(a) - b$Extra_Steps)^2)
  ssr <- sum((fitted(a) - mean(b$Extra_Steps))^2)
  sst <- ssr + sse
  r2 <- (ssr/sst)
  
  return(r2)
}

linear_regression <- function(df, out, summary_show = 0){
  ### Build the first regression
  lm.out_1 <- lm(Extra_Steps~as.factor(Clusters), data=df)
  if(summary_show == 1) { summary(lm.out_1) }
  
  rows <- vector()
  rows <- append(rows, dispersion_calc(lm.out_1, df))
  rows <- append(rows, AIC(lm.out_1))
  rows <- append(rows, rsq_calc(lm.out_1, df))
  
  out[nrow(out) + 1,] <- rows
  
  return(out)
  
}

generalized_regression <- function(df, out, summary_show = 0){
  
  # Poisson GLM
  glm.out_1 <- glm(Extra_Steps~as.factor(Clusters)+Residual, data=df, family = poisson())
  if(summary_show == 1) { summary(glm.out_1) }
  
  rows <- vector()
  rows <- append(rows, dispersion_calc(glm.out_1, df))
  rows <- append(rows, AIC(glm.out_1))
  rows <- append(rows, rsq_calc(glm.out_1, df))
  out[nrow(out) + 1,] <- rows
  
  # Negative Binomial GLM
  glm.out_2 <- glm.nb(Extra_Steps~as.factor(Clusters)+Residual, data=df)
  if(summary_show == 1) { summary(glm.out_2) }
  
  rows <- vector()
  rows <- append(rows, dispersion_calc(glm.out_2, df))
  rows <- append(rows, AIC(glm.out_2))
  rows <- append(rows, rsq_calc(glm.out_2, df))
  out[nrow(out) + 1,] <- rows
  
  return(out)
  
}

zero_infl_regression <- function(df, out, summary_show = 0){
  
  # Zero-Inflated Poisson GLM
  glm.out_3 <- zeroinfl(Extra_Steps~as.factor(Clusters)+Residual | ## Predictor for the Poisson process
                          as.factor(Clusters), ## Predictor for the Bernoulli process;
                        dist = 'poisson',
                        data = df)
  
  if(summary_show == 1) { summary(glm.out_3) }
  
  rows <- vector()
  rows <- append(rows, dispersion_calc(glm.out_3, df))
  rows <- append(rows, AIC(glm.out_3))
  rows <- append(rows, rsq_calc(glm.out_3, df))
  out[nrow(out) + 1,] <- rows
  
  # Zero-Inflated Negative Binomial GLM
  glm.out_4 <- zeroinfl(Extra_Steps~as.factor(Clusters)+Residual | ## Predictor for the Poisson process
                          as.factor(Clusters),
                        dist = 'negbin',
                        data = df)
  
  if(summary_show == 1) { summary(glm.out_4) }
  
  
  rows <- vector()
  rows <- append(rows, dispersion_calc(glm.out_4, df))
  rows <- append(rows, AIC(glm.out_4))
  rows <- append(rows, rsq_calc(glm.out_4, df))
  out[nrow(out) + 1,] <- rows
  
  return(out)
  
}


par(mfrow=c(2,2))
df_new <- read.csv("Puzzles_Output/standard_best_final_output.csv")
df_new$final_sequence <- as.character(df_new$new_sequence)

virtualization_seq(df_new)

# output <- data.frame(matrix(ncol = 3, nrow = 0))
# x <- c("Dispersion", "AIC", "R_squared")
# colnames(output) <- x
# 
# data <- df_new
# 
# output <- linear_regression(data, output)
# 
# lm.out <- lm(Extra_Steps~as.factor(Clusters), data=data)
# data["Residual"] <- resid(lm.out)
# 
# output <- generalized_regression(data, output)
# 
# output <- zero_infl_regression(data, output)
# 
# x <- c('LM', 'Poisson GLM', 'Negative Binomial GLM', 'Zero-infl Poisson GLM', 'Zero-infl NB GLM')
# row.names(output) <- x
# 
# print(output)

