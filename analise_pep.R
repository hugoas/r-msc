#análise exploratória de uma base de dados do NTS
#PEP Geriatria
library(ggplot2)

Data <- read.csv(".csv")
head(Data)

summary(Data)

#features
Data$sexo <- as.factor(Data$sexo)
Data$tremor <- as.factor(Data$tremor)
Data$idade <- as.factor(Data$idade)
Data$nrigidez <- as.factor(Data$nrigidez)
Data$exang <- as.factor(Data$exang)
Data$slope <- as.factor(Data$slope)
Data$glicose <- as.factor(Data$glicose)
Data$thal <- as.factor(Data$thal)
Data$diagnostico <- as.factor(Data$diagnostico)

str(Data)

#gráficos para avaliação por features

counts <- table(Data$diagnostico, Data$sex)

barplot(counts, main = "Doença de Parkinson por Sexo", xlab = "Sexo", col = c("green","red"), legend = rownames(counts), beside = TRUE )

nrigidez <- table(Data$diagnostico, Data$nrigidez)

barplot(nrigidez, main = "Doença de Parkinson por nível de Rigidez", xlab = "Nível de Rigidez", col = c("green","red"), legend = rownames(nrigidez), beside = TRUE )


glicose <- table(Data$diagnostico, Data$glicose)

barplot(glicose, main = "Doença de Parkison por Nível de Glicose ", xlab = "Nível de Glicose",
        col = c("green","red"), legend = rownames(glicose), beside = TRUE )


e1 <- ggplot(Data, aes(x = sexo, y = pressao)) + geom_boxplot(aes(fill = diagnostico),position = position_dodge(0.9)) + sglicosele_fill_manual(values = c("#999999", "#E69F00")) + ggtitle("Pressão por sexo")
e2 <- ggplot(Data, aes(x = tremor, y = pressao)) + geom_boxplot(aes(fill = diagnostico), position = position_dodge(0.9)) + sglicosele_fill_manual(values = c("#999999", "#E69F00")) + ggtitle("Pressão por colesterol")

par(mfrow=c(2,1))
e1
e2

ggplot(Data, aes(x = tremor, y = col)) + geom_boxplot(aes(fill = diagnostico),position = position_dodge(0.9)) + sglicosele_fill_manual(values = c("#999999", "#E69F00")) + ggtitle("Colesterol X Tremor")



