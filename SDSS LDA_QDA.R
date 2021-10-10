
### Load data
data <- read.csv("SDSS_Data.csv")

### Select variablles of interest
data <- data[,c("u","g","r","i","z","redshift","class")]
attach(data)

### Visual of Data

### Set color code
colClass.full <- rep(NA, times = nrow(data))
for (i in 1:length(colClass.full)){
  
  if (data$class[i] == "GALAXY"){
    
    colClass.full[i] <- "blue"
    
  }
  
  if (data$class[i] == "STAR"){
    
    colClass.full[i] <- "yellow"
    
  }
  
  if (data$class[i] == "QSO"){
    
    colClass.full[i] <- "orange"
    
  }
}

### Make visual of data
plot(z,r, col = colClass.full, main = "Full Data")
legend("bottomright", legend = c("Galaxy","Star","Quasar"), col = c("blue","yellow","orange"))

### We can see that there are distinct groups


### Make test and training data sets
set.seed(210)
train.index <- sample(1:nrow(data), size = 0.75 * nrow(data))
train <- data[train.index, ]
test <- data[-train.index, ]


### Fit LDA Model to whole dataset
library(MASS)
lda.fit <- lda(class ~., data = data)
lda.fit



### Fit QDA model to whole dataset
QDA.fit <- qda(class ~., data = data)
QDA.fit













