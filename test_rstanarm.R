diabetes <- read.csv("~/Downloads/diabetes.csv", header = TRUE)
diabetes$Outcome <- factor(diabetes$Outcome)

# removing those observation rows with 0 in any of the variables
for (i in 2:6) {
  diabetes <- diabetes[-which(diabetes[, i] == 0), ]
}
# scale the covariates for easier comparison of coefficient posteriors
for (i in 1:8) {
  diabetes[i] <- scale(diabetes[i])
}

# modify the data column names slightly for easier typing
names(diabetes)[7] <- "dpf"
names(diabetes) <- tolower(names(diabetes))

str(diabetes)
print(paste0("number of observations = ", dim(diabetes)[1]))
print(paste0("number of predictors = ", dim(diabetes)[2]))
x <- model.matrix(outcome ~ . - 1, data = diabetes)
y <- diabetes$outcome
library(rstanarm)
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
post1 <- stan_glm(outcome ~ ., data = diabetes,
                  family = binomial(link = "logit"), 
                  prior = t_prior, prior_intercept = t_prior,
                  seed = 1)

library(loo)

linpred <- posterior_linpred(post1)
preds <- posterior_linpred(post1, transform=TRUE)
pred <- colMeans(preds)
pr <- as.integer(pred >= 0.5)

library(caret)
# confusion matrix
confusionMatrix(pr, y)[2:3]
# posterior classification accuracy
round(mean(xor(pr,as.integer(y))),3)
# posterior balanced classification accuracy
round((mean(xor(pr[y==0]>0.5,as.integer(y[y==0])))+mean(xor(pr[y==1]>0.5,as.integer(y[y==1]))))/2,3)


library(pROC)
plot.roc(y,pred,percent=TRUE,col="#1c61b6",  print.auc=TRUE)
#####
file_s = "lStan.stan"

dat = list(n = NROW(cbind(x,y)),
           y = as.integer(y)-1,
           X = x,
           k = 8)
fit_s <- stan(file = file_s, data = dat,
              chains = chains, iter = iter, warmup = warmup, thin = thin)

predecidas = as.vector(colMeans(extract (fit_s, "prob",permuted=F)))
