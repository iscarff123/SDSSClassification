
### Ian Scarff
### Spring 2019
### STAT 485

### Sloan Digital Sky Survey Classification Project


### The purpose of this project is to develop a classification model using various
### machine learning methods including discriminant analysis and tree based methods.

### Classify an observation as a star, galaxy, or quasar object.

### Using data release #14

### Import data
setwd("D:/Ian/Desktop/Spring 2019/STAT 485")
data <- read.csv("SDSS_Data.csv")

View(data)

names(data)

### Variables:
    
    ### objid
      
        ### Object Identifier

    ### ra

        ### J2000 Right Ascension (r-band)
        ### Right ascension (abbreviated RA) is the angular distance measured eastward 
        ### along the celestial equator from the Sun at the March equinox to the hour circle 
        ### of the point above the earth in question.

    ### dec

        ### Declination (abbreviated dec)
        ### The angular distance of a point north or south of the celestial equator.

    ### u

    ### g

    ### r

    ### i

    ### z

### Thuan-Gunn System is a type of star maginitude system using various set of broad-band filters.
### the variables u,g,r,i,z are the repsonses from each filter 
### It is an extention of the UBVRI system, with filters optimized for faint galaxies by rejecting
###   night sky lines

    ### run

        ### Run number

    ### rerun

        ### Rerun number

    ### camcol

        ### Camera column

    ### field

        ### Field number

#### Run, rerun, camcol and field are features which describe a field within an image
#### taken by the SDSS. A field is basically a part of the entire image corresponding to 
#### 2048 by 1489 pixels. A field can be identified by: - run number, which identifies the 
#### specific scan, - the camera column, or "camcol," a number from 1 to 6, identifying the scanline 
#### within the run, and - the field number. The field number typically starts at 11 
#### (after an initial rampup time), and can be as large as 800 for particularly long runs.
#### - An additional number, rerun, specifies how the image was processed.

    ### specobjid

        ### Object identifier

    ### class
    
        ### object class (galaxy, star or quasar object)

    ### redshift

        ### Final Redshift
        ### redshift happens when light or other electromagnetic radiation 
        ### from an object is increased in wavelength, or shifted to the red end of the spectrum.

    ### plate

        ### plate number

    ### mjd

        ### Modified Julian Date
        ### Used to indicate the date that a given piece of SDSS data (image or spectrum) was taken.

    ### fiberid

        ### Fiber ID
        ### The SDSS spectrograph uses optical fibers to direct the light at the focal
        ### plane from individual objects to the slithead. Each object is assigned a corresponding 
        ### fiberID


### For the purposes of this project the variables:

  ### u
  ### g
  ### r
  ### i
  ### z
  ### redshift
  ### class

### will be used in developing a model.

data <- data[,c("u","g","r","i","z","redshift","class")]
attach(data)


STAR <- data[data$class == "STAR",]
GALAXY <- data[data$class == "GALAXY",]
QSO <- data[data$class == "QSO",]

####################################### EXPLORATORY ANALYSIS ##########################################

dim(data)

### 10,000 observations

which(is.na(data))

### There are no missing data


### make pie chart
library(plotly)
COLORS <- c('rgb(248, 255, 61)','rgb(17, 56, 255)')
plot_ly(data = data, labels = ~class, type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#000000', size = 18),
        hoverinfo = 'text',
        marker = list(
          colors = COLORS, line = list(color = '#000000', width = 1)),
        showlegend = F) %>%
  layout(title = '# of Observations by Category',
         xaxis = list(showgrid = F, zeroline = F, showticklabels = F))

### 50% of the data set is galaxies, 41.5% stars, 8.5% quasars

table(data$class)
### Create separate data frames for stars, galaxies, and quasars


### Display correlation
write.csv(cor(data[,-7]), file = "CorrelationMatrix.csv")

library(corrplot)
corrplot(cor(data[,-7]), method = "circle", type = "upper",
         tl.cex = 1.7, cl.cex = 1.15)

### Plot data set
axis = list(showline = F,
            zeroline = F,
            gridcolor = '#ffff',
            ticklen = 4)
pl_colorscale=list(c(0.0, '#1138ff'),
                   c(0.333, '#1138ff'),
                   c(0.333, '#ea8e04'),
                   c(0.666, '#ea8e04'),
                   c(0.666, '#f8ff3d'),
                   c(1, '#f8ff3d'))
data[-7] %>% plot_ly() %>%
  add_trace(type = 'splom',
            dimensions = list(
              list(label = 'u', values = ~data$u),
              list(label = 'g', values = ~data$g),
              list(label = 'r', values = ~data$r),
              list(label = 'i', values = ~data$i),
              list(label = 'z', values = ~data$z),
              list(label = 'redshift', values = ~data$redshift)
            ),
            text = ~data$class, 
            marker = list(
              color = as.integer(data$class),
              colorscale = pl_colorscale,
              size = 5,
              line = list(width = 1, color = 'rgb(230,230,230)'))) %>%
  layout(title = "Variables of Interest",
    hovermode = 'closest',
    dragmode = 'select',
    plot_bgcolor = 'rgba(240,240,240, 0.95)',
    xaxis = list(domain = NULL, showline = F, zeroline = F, gridcolor = '#ffff', ticklen = 4),
    yaxis = list(domain = NULL, showline = F, zeroline = F, gridcolor = '#ffff', ticklen = 4),
    xaxis2 = axis,
    xaxis3 = axis,
    xaxis4 = axis,
    xaxis5 = axis,
    xaxis6 = axis,
    yaxis2 = axis,
    yaxis3 = axis,
    yaxis4 = axis,
    yaxis5 = axis,
    yaxis6 = axis) %>% style(diagonal = list(visible = F))


### Create histograms for each variable
Hist.u <- plot_ly(data = data, alpha = 0.6, color = ~class,
                  colors = c("blue","orange","yellow2")) %>%
  add_histogram(x = ~u) %>%
  layout(barmode = "overlay", title = "Histogram of u", xaxis = list(title = "u"))

Hist.g <- plot_ly(data = data, alpha = 0.6, color = ~class,
                  colors = c("blue","orange","yellow2")) %>%
  add_histogram(x = ~g) %>%
  layout(barmode = "overlay", title = "Histogram of g", xaxis = list(title = "g"))

Hist.r <- plot_ly(data = data, alpha = 0.6, color = ~class,
                  colors = c("blue","orange","yellow2")) %>%
  add_histogram(x = ~r) %>%
  layout(barmode = "overlay", title = "Histogram of r", xaxis = list(title = "r"))

Hist.i <- plot_ly(data = data, alpha = 0.6, color = ~class,
                  colors = c("blue","orange","yellow2")) %>%
  add_histogram(x = ~i) %>%
  layout(barmode = "overlay", title = "Histogram of i", xaxis = list(title = "i"))

Hist.z <- plot_ly(data = data, alpha = 0.6, color = ~class,
                  colors = c("blue","orange","yellow2")) %>%
  add_histogram(x = ~z) %>%
  layout(barmode = "overlay", title = "Histogram of 5 Filter Responses", xaxis = list(title = "z"))


subplot(Hist.u, Hist.g, Hist.r,Hist.i,Hist.z, nrows = 2,titleX = T, 
        margin = 0.04)


Hist.red <- plot_ly(data = data, alpha = 0.6, color = ~class,
                    colors = c("blue","orange","yellow2")) %>%
  add_histogram(x = ~redshift) %>%
  layout(barmode = "overlay", title = "Histogram of redshift", xaxis = list(title = "redshift"))
Hist.red

### Separate plots
plot_ly(data = data, x = ~z, y = ~r, color = ~class, 
        colors = c("blue","orange","yellow"), symbol = ~class, 
        symbols = c("circle","x","star")) %>% 
  layout(title = 'Full Data', 
         plot_bgcolor = "lightgrey")


plot_ly(data = data, x = ~redshift, y = ~z, color = ~class, 
        colors = c("blue","orange","yellow"), symbol = ~class, 
        symbols = c("circle","x","star")) %>% 
  layout(title = 'Full Data', 
         plot_bgcolor = "lightgrey")


plot_ly(data = data, x = ~z, y = ~r, z = ~redshift, color = ~class,
        colors = c("blue","orange","yellow"), symbol = ~class,
        symbols = c("circle","square","diamond")) %>% 
  layout(title = 'Full Data')




################################### MODEL BUILDING #################################


### Make test and training data sets

set.seed(210)
train.index <- sample(1:nrow(data), size = 0.75 * nrow(data))


train <- data[train.index, ]
test <- data[-train.index, ]

table(train$class)
table(test$class)


######################################## LDA ########################################

library(MASS)
attach(train)



#### Apply LDA to the whole dataset first to get the decision boundry
### take mu_hat of only the variables on the graph



### Fit LDA model on training data
lda.fit <- lda(class ~., data = train)

### Predict over training data
lda.predict <- predict(lda.fit)$class

### Plot LDA 

colClass.train <- rep(NA, times = nrow(train))
for (k in 1:nrow(train)){
  
  if (train$class[k] == "STAR"){
    
    colClass.train[k] <- "yellow"
    
  }
  
  if (train$class[k] == "GALAXY"){
    
    colClass.train[k] <- "blue"
    
  }
  
  if (train$class[k] == "QSO"){
    
    colClass.train[k] <- "orange"
    
  }
  
}


plot(lda.fit, abbrev = TRUE, col = colClass.train)
plot(lda.fit, abbrev = TRUE, col = colClass.train, xlim = c(-5,5), ylim = c(-3,3))


### Confusion matrix
ConfuMat.lda <- table(lda.predict, train$class)
ConfuMat.lda


### Make functions get the accuracy for each class. 
### w x sens + (1 - w) x spec
### Where w is a sequence from 0.01 to 0.99
ClassAcc <- function(ConMat, modelDataName){

  ### Create weight variable
  w <- seq(0,1,0.01)
  
  ### Calculate sensitivity and specificity of each class
  ### Galaxies
  GALsens <- ConMat[1,1] / sum(ConMat[,1]) ### Sensitivity
  GALspec <- (ConMat[2,2] + ConMat[3,3]) / sum(ConMat[,c(2,3)]) ### Specificity
  
  ### Quasars
  QSOsens <- ConMat[2,2] / sum(ConMat[,2]) ### Sensitivity
  QSOspec <- (ConMat[1,1] + ConMat[3,3]) / sum(ConMat[,c(1,3)]) ### Specificity
  
  ### Stars
  STARsens <- ConMat[3,3] / sum(ConMat[,3]) ### Sensitivity
  STARspec <- (ConMat[1,1] + ConMat[2,2]) / sum(ConMat[,c(1,2)]) ### Specificity
  
  ### Create dataframe to hold accuracy measurments 
  AccData <- data.frame(c(w,w,w))
  
  ### Calculate accuracies for each class
  ### Galaxies
  
  ### Create variable to hold accuracies
  GALacc <- rep(NA,times = length(w))
  for(i in 1:length(w)){
    GALacc[i] <-  (w[i]*GALsens) + ((1 - w[i])*GALspec)
  }
  rm(i)
  
  ### Quasars
  ### Create variable to hold accuracies
  QSOacc <- rep(NA,times = length(w))
  for(i in 1:length(w)){
    QSOacc[i] <-  (w[i]*QSOsens) + ((1 - w[i])*QSOspec)
  }
  rm(i)
  
  ### Stars
  ### Create variable to hold accuracies
  STARacc <- rep(NA,times = length(w))
  for(i in 1:length(w)){
    STARacc[i] <-  (w[i]*STARsens) + ((1 - w[i])*STARspec)
  }
  rm(i)
  
  ### Add accuracies to dataframe
  Accs <- data.frame(c(GALacc,QSOacc,STARacc))
  AccData <- cbind(AccData,Accs)
  
  ### Make markers
  CLASSES <- c(rep("GALAXY",times = 101),rep("QUASAR",times = 101),rep("STAR",times = 101))
  AccData <- cbind(AccData, CLASSES)
  
  ### Make column names
  colnames(AccData) <- c("Weight","Accuracy","Class")
  
  ### Make graph
  p <- plot_ly(data = AccData, x = ~Weight, y = ~Accuracy, color = ~Class, 
          colors = c("blue","orange","yellow"), symbol = ~Class, 
          symbols = c("circle","x","star")) %>% 
    layout(title = modelDataName, 
           plot_bgcolor = "lightgrey")
  p
  
  ### Make summary table
  GALSUM <- c(GALsens,GALspec,AccData$Weight[which(AccData$Accuracy == max(AccData$Accuracy[1:101]))],
                       max(AccData$Accuracy[1:101]))
  QSOSUM <- c(QSOsens,QSOspec,AccData$Weight[which(AccData$Accuracy == max(AccData$Accuracy[102:202]))],
                       max(AccData$Accuracy[102:202]))
  STARSUM <- c(STARsens,STARspec,AccData$Weight[which(AccData$Accuracy == max(AccData$Accuracy[203:303]))],
                       max(AccData$Accuracy[203:303]))
  SUMMARY <- as.data.frame(rbind(GALSUM,QSOSUM,STARSUM), row.names = c("Galaxy","Quasar","Star"))
  colnames(SUMMARY) <- c("Sensitivity","Specificity","Weight","Max Accuracy")
  
  ### Return table and graph
  return(list(SUMMARY,p))
}


### Accuracy
LDA.Train <- ClassAcc(ConMat = ConfuMat.lda, modelDataName = 'LDA Training Data')
LDA.Train

### Make prediction for the test data

lda.predict2 <- predict(lda.fit, newdata = test)$class

### Confusion matrix
ConfuMat.lda2 <- table(lda.predict2, test$class)
ConfuMat.lda2

### Accuracy
LDA.Test <- ClassAcc(ConMat = ConfuMat.lda2, modelDataName = 'LDA Test Data')
LDA.Test


######################################## QDA ##############################


### Fit QDA model on training data

QDA.fit <- qda(class ~., data = train)

### Predict over training data
QDA.predict <- predict(QDA.fit, newdata = train[-7])$class

### Confusion matrix
ConfuMat.QDA <- table(QDA.predict, train$class)
ConfuMat.QDA

### Accuracy
QDA.Train <- ClassAcc(ConMat = ConfuMat.QDA, modelDataName = 'QDA Training Data')
QDA.Train

### Make prediction for the test data

QDA.predict2 <- predict(QDA.fit, newdata = test[-7])$class

### Confusion matrix
ConfuMat.QDA2 <- table(QDA.predict2, test$class)
ConfuMat.QDA2

### Accuracy
QDA.Test <- ClassAcc(ConMat = ConfuMat.QDA2, modelDataName = 'QDA Test Data')
QDA.Test




#################################### TREE METHODS ###########################

################################### BASIC TREE #################################

library(tree)
par(mfrow=c(1,1))
tree.fit <- tree(class ~ u + g + r + i + z + redshift, data = train)
plot(tree.fit, main = "Basic Tree Model")
text(tree.fit)
### The tree model only uses redshift to predict class. Can't prune tree


### Plot decision boundries

plot_ly(data = data, x = ~redshift, y = ~z, color = ~class, 
        colors = c("blue","orange","yellow"), symbol = ~class, 
        symbols = c("circle","x","star")) %>% 
  layout(title = 'Full Data', 
         plot_bgcolor = "lightgrey",
         shapes = list(
           list(type = "line", line = list(color = "black"), x0 = 0.00427776,
                x1 = 0.00427776, xref = "x", y0 = 0, y1 = 26, yref = "y"),
           list(type = "line", line = list(color = "black"), x0 =0.217677,
                x1 = 0.217677, xref = "x", y0 = 0, y1 = 26, yref = "y")))

### Predict across training data set

tree.preTrain <- predict(tree.fit, newdata = train[-7], type = "class")

### Confusion matrix
ConfuMat.tree <- table(tree.preTrain, train$class)
ConfuMat.tree


### Accuracy
tree.Train <- ClassAcc(ConMat = ConfuMat.tree, modelDataName = 'Tree Training Data')
tree.Train

### Make prediction for the test data

tree.preTest <- predict(tree.fit, newdata = test[-7], type = "class")

### Confusion matrix
ConfuMat.tree2 <- table(tree.preTest, test$class)
ConfuMat.tree2

### Accuracy
tree.Test <- ClassAcc(ConMat = ConfuMat.tree2, modelDataName = 'Tree Test Data')
tree.Test





################################# RANDOM FOREST ################################# 

### topic 11

library(randomForest)

### Fit random forest model
rf.fit <- randomForest(class~., data = train, importance  = T)
rf.fit
rf.fit$importance
plot(rf.fit)


### Predict across training data set

rf.preTrain <- predict(rf.fit, newdata = train[-7], type = "class")

### Confusion matrix
ConfuMat.rf <- table(rf.preTrain, train$class)
ConfuMat.rf

### Accuracy is 100% for each class


### Predict across test data set

rf.preTest <- predict(rf.fit, newdata = test[-7], type = "class")

### Confusion matrix
ConfuMat.rf2 <- table(rf.preTest, test$class)
ConfuMat.rf2


### Accuracy
rf.Test <- ClassAcc(ConMat = ConfuMat.rf2, modelDataName = 'Random Forest Test Data')
rf.Test








