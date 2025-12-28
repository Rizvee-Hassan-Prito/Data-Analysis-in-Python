data <- read.csv(file.choose())
str(data)
data_new <- data.frame(data[,-length(data)])

colnames(data_new) <- c(
  "CF",
  "SL",
  "SU",
  "FU",
  "FS",
  "DN",
  "Age",
  "CV"
)

head(data_new)

S<- cov(data_new)
S
r <- cor(data_new)
r


evl.evc <- eigen(S)

# Eigenvalues
lambda <- evl.evc$values
lambda

# Eigenvectors
e <- evl.evc$vectors
e

# Total variance check
sum(diag(S))
sum(lambda)


pca <- prcomp(data_new, scale = TRUE)
pca
summary(pca)

plot(pca, type = "l", main = "Scree Plot for Customer Churn Data")


#############################


# Eigenvalues to determine number of factors
eigen_values <- eigen(r)$values
eigen_values

# Scree plot
plot(eigen_values, type="b", main="Scree Plot for Factor Analysis",
     xlab="Factor Number", ylab="Eigenvalue")
abline(h=1, col="red")


fa_result <- factanal(data_new, factors = 2, rotation = "varimax")

fa_result
fa_result$loadings

############################

library(MASS)
library(caret)
data <- read.csv(file.choose())
str(data)

colnames(data) <- c(
  "CF",
  "SL",
  "SU",
  "FU",
  "FS",
  "DN",
  "Age",
  "CV",
  "Churn"
)

head(data)

# TRAINING and TEST sets
set.seed(111)
obs <- sample(1:nrow(data), round(0.7 * nrow(data)))

train.data <- data[obs, ]
test.data  <- data[-obs, ]

# Normalize predictor variables
train.data[,1:8] <- scale(train.data[,1:8])
test.data[,1:8]  <- scale(test.data[,1:8])

# Fit Linear Discriminant Analysis model
lda.model <- lda(Churn ~ ., data = train.data)
lda.model$scaling

# Make predictions
predictions <- predict(lda.model, test.data)$class

# Confusion matrix
conf <- table(predicted = predictions, observed = test.data$Churn)
conf

# Classification accuracy
confusionMatrix(conf)
