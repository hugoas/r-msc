#Libs
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(R.matlab)
library(class)
library(wavelets)
library(jpeg)
library(ggplot2)
library(randomForest)
library(forecast)
library(dtw)

#Split dos dados
TrainData <- read.csv("train.csv")
TrainData=na.exclude(TrainData)

TestData <- read.csv("test.csv")
TestData=na.exclude(TestData)

#Vamos classificar
DecisionTree <- rpart(Controle ~ classe + sexo + idade, data = TrainData, method = "class")

#Visualize the decision tree using plot() and text()
plot(DecisionTree)
text(DecisionTree)


fancyRpartPlot(DecisionTree)


DecisionTree2 <- rpart(Controle ~ classe + sexo + idade + SibSp + Parch + Fare + Embarked, 
                   data = TrainData, method = "class", control = rpart.control(minsplit = 50, cp = 0))
fancyRpartPlot(myTreeTwo)

#Testar com outros classificadores
fit=randomForest(TrainData[,c(3,5,6,7,8,10)],y=as.factor(TrainData$Controle),ntree=2000)
prediction=predict(fit, TestData[,c(2,4,5,6,7,9)],type = "response")
prediction=knn(TrainData[,c(3,6,7,8,10)], TestData[,c(2,5,6,7,9)], TrainData$Controle, k = 5)


#rsvg()
#img=readPNG()
img=readJPEG("ultrassom.jpg")
imgDm=dim(img)
imgRGB <- data.frame(
  x = rep(1:imgDm[2], each = imgDm[1]),
  y = rep(imgDm[1]:1, imgDm[2]),
  R = as.vector(img[,,1]),
  G = as.vector(img[,,2]),
  B = as.vector(img[,,3]))

plotTheme <- function() {theme(panel.background = element_rect(size = 3,colour = "black",fill = "white"),
                               axis.ticks = element_line(size = 2),panel.grid.major = element_line(colour = "gray80",linetype = "dotted"),
                               panel.grid.minor = element_line(colour = "gray90",linetype = "dashed"),axis.title.x = element_text(size = rel(1.2),face = "bold"), axis.title.y = element_text(size = rel(1.2),face = "bold"),plot.title = element_text(size = 20,  face = "bold",vjust = 1.5))}

ggplot(data = imgRGB, aes(x = x, y = y)) +geom_point(colour = rgb(imgRGB[c("R", "G", "B")])) +labs(title = "Imagem Original") + xlab("x") +ylab("y") +plotTheme()


#SérieTemporal
foot <- read.csv("foot1.csv")
plot(foot)

fit <- auto.arima(foot)
fore <- predict(fit, n.ahead=24)
E <- fore$pred + 2*fore$se
D <- fore$pred - 2*fore$se
timeseries.plot(foot, fore$pred, E, D, col=c(1,2,4,4), lty = c(1,1,2,2))
legend("topleft", c("Actual", "Forecast", "Error"), col=c(1,2,4), lty=c(1,1,2),pt.cex=3)

mm <- matrix(runif(12),ncol=3)
dm <- dist(mm,method="DTW")

#for asymetric step-patterns
dm <- dist(mm,mm,method="DTW",step=asymmetric)  #non-symmetric matrix
dm <- (dm+t(dm))/2 # symmetrize


wt=dwt(foot,filter = "haar", boundary = "periodo",fast = T)
features=unlist(c(wt@W,wt@V[[wt@level]]))


# Matlab
matData0 <- readMat('')
data0 = as.data.frame(matData0[[1]][[1]])
matData1 <- readMat('')
data1 = as.data.frame(matData1[[1]][[1]])

plot(1:1000,dat1[,1][1:1000],type = 'l')
plot(1:1000,apply(data1,1,mean)[1:1000],type = 'l')

data1_time=timeseries(apply(data1,1,mean),frequency=300)
data0_time=timeseries(apply(data0,1,mean),frequency=300)

plot(decompose(data1_time))