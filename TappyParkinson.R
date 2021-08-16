#libs
library(data.table)
library(magrittr)
library(zoo)
library(stringr)
library(ggplot2)
library(ggplot)
library(e1071) 

##funções
yule <- function(x){
  numerator <- 0.5 * (quantile(x,0.75) + quantile(x,0.25))-median(x) 
  denominator <- 0.5* IQR(x)
  c(numerator/denominator, use.names=FALSE)
}


#Ler o arquivo
setwd("TappyData.csv")
getwd()

arquivo.names <- dir(getwd(), pattern =".txt")

DS<-""
for(i in 1:length(arquivo.names)){
  arquivo <- read.table(arquivo.names[i],header=F, sep='\t', stringsAsFactors=FALSE)
  DS <- rbind(DS, arquivo)
}

write.csv(DS,"TappySummary.csv")

TappyData = read.table('C:/R Data/R Course PROJECT/TappyData/TappySummary.csv', header = T,sep=',')
colunas(TappyData) <- c("IdUsuario", "Data", "Tempostamp", "Mao", "Espera Tempo", "Direction", "Latencia", "Suspensao Tempo")

#tags do usuário
setwd("UserData")

DadosUsuarios<-data.table(
  IdUsuario = character(),
  AnoNascimento= character(),
  Genero= character(),
  Parkinsons= logical(),
  Tremores= logical(),
  AnoDiag= character(),
  Sided=character(),
  UPDRS = character(),
  Quedas= character(),
  Levadopa= logical(), 
  DA= logical(), 
  MAOB= logical(), 
  Outros= logical()
  
)

arquivo.names2 <- dir(getwd(), pattern =".txt")

for(i in 1:length(arquivo.names2)){
  
  arquivo <- read.table(arquivo.names2[i],header=F,quote = "", sep=":",row.names=c("AnoNascimento", "Genero", "Parkinsons", "Tremores",
                                                                             "AnoDiag", "Sided", "UPDRS", "Quedas", 
                                                                             "Levadopa", "DA", "MAOB", "Outros")
                     , stringsAsFactors=FALSE,fill=TRUE)
  IdUsuario<-substring(arquivo.names2[i], 6, nchar(arquivo.names2[i]) -4)
  Resualt_B <- data.table(IdUsuario,  t(arquivo[2]))
  
  DadosUsuarios <- rbind(Resualt_B, DadosUsuarios,fill=TRUE)
}

write.table(DadosUsuarios,"DadosUsuariosSummary.txt",col.names=UserDataNames)


##teste em usuarios

setwd("testDadosUsuarios")

DadosUsuarios2<-c("AnoNascimento", "Genero")

TestDadosUsuarios<-data.table(
  IdUsuario = character(),
  AnoNascimento= numeric(),
  Genero= factor(),
  ParkinsonsF= logical()
)

arquivo.names2 <- dir(getwd(), pattern =".txt")

for(i in 1:length(arquivo.names2)){
  
  arquivo <- read.table(arquivo.names2[i],header=F,quote = "", sep=":",row.names=c("AnoNascimento", "Genero")
                     , stringsAsFactors=FALSE,fill=TRUE)
  IdUsuario<-substring(arquivo.names2[i], 6, nchar(arquivo.names2[i]) -4)
  Resualt_B <- data.table(IdUsuario,  t(arquivo[2]))
  
  TestDadosUsuarios <- rbind(Resualt_B, TestDadosUsuarios,fill=TRUE)
  
}

write.table(TestDadosUsuarios,"C:/R Data/R Course PROJECT/TestDadosUsuarios/TestDadosUsuariosSummary.txt")

#migração

#Dados dos usuários
DadosUsuarios= fread('DadosUsuariosSummary.txt', header =F)
DadosUsuariosNames<-c("1","IdUsuario","AnoNascimento", "Genero", "Parkinsons", "Tremores",
                  "AnoDiag", "Sided", "UPDRS", "Quedas", 
                  "Levadopa", "DA", "MAOB", "Outros")
colunas(DadosUsuarios)<-DadosUsuariosNames
DadosUsuarios[,1:=NULL]

##AnoNascimento
DadosUsuarios[,AnoNascimento:=as.numeric(DadosUsuarios[,AnoNascimento])]

class(DadosUsuarios[,AnoNascimento])
DadosUsuarios[is.na(AnoNascimento),AnoNascimento:=mean(DadosUsuarios[,AnoNascimento],na.rm=TRUE)] 
hist(DadosUsuarios[,AnoNascimento])
##Genero
DadosUsuarios[,Genero,Genero]
DadosUsuarios[,Genero:=as.factor(DadosUsuarios[,Genero])]
DadosUsuarios[,.N, Genero] 
##Parkinsons
DadosUsuarios[,Parkinsons,] %>% table

DadosUsuarios[,Parkinsons:=str_trim(chartr(old = "True", new = "TRUE", DadosUsuarios$Parkinsons))]
DadosUsuarios[,Parkinsons:=str_trim(chartr(old = "FalsE", new = "FALSE", DadosUsuarios$Parkinsons))]
DadosUsuarios[,Parkinsons:=as.logical(DadosUsuarios[,Parkinsons])]

DadosUsuarios[,.N, Genero] 

class(DadosUsuarios$Parkinsons)

#######Tremores
DadosUsuarios[,Tremores,Tremores]
DadosUsuarios[,Tremores:=as.factor(DadosUsuarios[,Tremores])]


####  AnoDiag
DadosUsuarios[,AnoDiag,AnoDiag]
DadosUsuarios[,AnoDiag:=as.numeric(DadosUsuarios[,AnoDiag])]
class(DadosUsuarios[,AnoDiag])

hist(DadosUsuarios[,AnoDiag])

DadosUsuarios[is.na(AnoDiag),AnoDiag:=mean(DadosUsuarios[,AnoDiag],na.rm=TRUE)] 
hist(DadosUsuarios[,AnoDiag])




TestDadosUsuarios= fread('TestDadosUsuariosSummary.txt', header =F)

TestDadosUsuariosNames<-c("1","IdUsuario","AnoNascimento", "Genero", "ParkinsonsF")

colunas(TestDadosUsuarios)<-TestDadosUsuariosNames
TestDadosUsuarios[,1:=NULL]

##AnoNascimento
TestDadosUsuarios[,AnoNascimento:=as.numeric(TestDadosUsuarios[,AnoNascimento])]

class(TestDadosUsuarios[,AnoNascimento])

hist(TestDadosUsuarios[,AnoNascimento])

##Genero
TestDadosUsuarios[,Genero,Genero]
TestDadosUsuarios[,Genero:=as.factor(TestDadosUsuarios[,Genero])]
TestDadosUsuarios[,.N, Genero] 


TappyData= fread('C:/R Data/R Course PROJECT/TappyData/TappySummary.csv', header =F)

colunas(TappyData) <- c("1","IdUsuario", "Data", "Tempostamp", "Mao",
                         "Espera Tempo", "Direction", "Latencia",
                         "Suspensao Tempo","2")
TappyData[,10:=NULL]
TappyData[,1:=NULL]
TappyData[,Data:=NULL]
TappyData[,Tempostamp:=NULL]
TappyData<-TappyData[3:.N,]


##Espera 
TappyData[,New:=as.numeric(TappyData[,TappyData$"Espera"])]
TappyData[,"Espera Tempo":=New]
TappyData[,New:=NULL]

TappyData<-TappyData[!is.na(TappyData$"Espera Tempo")]

TappyData<-TappyData[`Espera Tempo`>0 &`Espera Tempo`<6000,]

#plot(TappyData[,.N,`Espera Tempo`])


##Latencia

TappyData[,New:=as.numeric(TappyData[,TappyData$"Latencia"])]
TappyData[,"Latencia":=New]
TappyData[,New:=NULL]

TappyData<-TappyData[!is.na(TappyData$"Latencia")]
TappyData<-TappyData[`Latencia`>0 &`Latencia`<6000,]

#hist(TappyData$"Latencia")

##Suspensao Tempo

TappyData[,New:=as.numeric(TappyData[,TappyData$"Suspensao Tempo"])]
TappyData[,"Suspensao Tempo":=New]
TappyData[,New:=NULL]

TappyData<-TappyData[!is.na(TappyData$"Suspensao Tempo")]


#############
#TRNSFORM INTO ONE ROW PER USER##
TappyData_ALL<-TappyData[,.N, IdUsuario]
colunas(TappyData_ALL) <- c("IdUsuario","CountRows")

TappyData_L_Mao<-TappyData[Mao=="L",.(mean_L_Espera_Tempo=mean(`Espera Tempo`)
                                        ,sd_L_Espera_Tempo=sd(`Espera Tempo`)
                                        ,yule_L_Espera_Tempo=yule(`Espera Tempo`)
                                        ,IQR_L_Espera_Tempo=IQR(`Espera Tempo`)
                                        ,median_L_Espera_Tempo=median(`Espera Tempo`)
                                        
                                        ,skewness_L_Espera_Tempo=skewness(`Espera Tempo`)
                                        ,Max_L_Espera_Tempo=max(`Espera Tempo`)
                                        
                                        ,Min_L_Espera_Tempo=min(`Espera Tempo`)
                                        
                                        ,Count_L=.N
                                        
),by=list(IdUsuario) ]

TappyData_R_Mao<-TappyData[Mao=="R",.(mean_R_Espera_Tempo=mean(`Espera Tempo`)
                                        ,sd_R_Espera_Tempo=sd(`Espera Tempo`)
                                        ,yule_R_Espera_Tempo=yule(`Espera Tempo`)
                                        ,IQR_R_Espera_Tempo=IQR(`Espera Tempo`)
                                        ,median_R_Espera_Tempo=median(`Espera Tempo`)
                                        ,skewness_R_Espera_Tempo=skewness(`Espera Tempo`)
                                        ,Max_R_Espera_Tempo=max(`Espera Tempo`)
                                        
                                        ,Min_R_Espera_Tempo=min(`Espera Tempo`)
                                        
                                        ,Count_R=.N
                                        
),by=list(IdUsuario) ]

TappyData_S_Mao<-TappyData[Mao=="S",.(mean_S_Espera_Tempo=mean(`Espera Tempo`)
                                        ,sd_S_Espera_Tempo=sd(`Espera Tempo`)
                                        ,yule_S_Espera_Tempo=yule(`Espera Tempo`)
                                        ,IQR_S_Espera_Tempo=IQR(`Espera Tempo`)
                                        ,median_S_Espera_Tempo=median(`Espera Tempo`)
                                        ,skewness_S_Espera_Tempo=skewness(`Espera Tempo`)
                                        
                                        ,Max_S_Espera_Tempo=max(`Espera Tempo`)
                                        
                                        ,Min_S_Espera_Tempo=min(`Espera Tempo`)
                                        
                                        ,Count_S=.N
                                        
),by=list(IdUsuario) ]




TappyData_SS_Direction<-TappyData[Direction=="SS",.(
  mean_SS_Latencia_Tempo=mean(`Latencia`)
  ,sd_SS_Latencia_Tempo=sd(`Latencia`)
  ,yule_SS_Latencia_Tempo=yule(`Latencia`)
  ,IQR_SS_Latencia_Tempo=IQR(`Latencia`)
  ,median_SS_Latencia_Tempo=median(`Latencia`)
  ,skewness_SS_Latencia_Tempo=skewness(`Latencia`)
  ,Max_SS_Latencia_Tempo=max(`Latencia`)
  ,Min_SS_Latencia_Tempo=min(`Latencia`)
  ,Count_SS=.N
  
  
  
  ,mean_SS_Suspensao_Tempo=mean(`Suspensao Tempo`)
  ,sd_SS_Suspensao_Tempo=sd(`Suspensao Tempo`)
  ,yule_SS_Suspensao_Tempo=yule(`Suspensao Tempo`)
  ,IQR_SS_Suspensao_Tempo=IQR(`Suspensao Tempo`)
  ,median_SS_Suspensao_Tempo=median(`Suspensao Tempo`)
  ,skewness_SS_Suspensao_Tempo=skewness(`Suspensao Tempo`)
  ,Max_SS_Suspensao_Tempo=max(`Suspensao Tempo`)
  ,Min_SS_Suspensao_Tempo=min(`Suspensao Tempo`)
  
  
),by=IdUsuario ]



TappyData_SR_Direction<-TappyData[Direction=="SR",.(
  mean_SR_Latencia_Tempo=mean(`Latencia`)
  ,sd_SR_Latencia_Tempo=sd(`Latencia`)
  ,yule_SR_Latencia_Tempo=yule(`Latencia`)
  ,IQR_SR_Latencia_Tempo=IQR(`Latencia`)
  ,median_SR_Latencia_Tempo=median(`Latencia`)
  ,skewness_SR_Latencia_Tempo=skewness(`Latencia`)
  ,Max_SR_Latencia_Tempo=max(`Latencia`)
  
  ,Min_SR_Latencia_Tempo=min(`Latencia`)
  
  ,Count_SR=.N
  
  
  
  ,mean_SR_Suspensao_Tempo=mean(`Suspensao Tempo`)
  ,sd_SR_Suspensao_Tempo=sd(`Suspensao Tempo`)
  ,yule_SR_Suspensao_Tempo=yule(`Suspensao Tempo`)
  ,IQR_SR_Suspensao_Tempo=IQR(`Suspensao Tempo`)
  ,median_SR_Suspensao_Tempo=median(`Suspensao Tempo`)
  ,skewness_SR_Suspensao_Tempo=skewness(`Suspensao Tempo`)
  ,Max_SR_Suspensao_Tempo=max(`Suspensao Tempo`)
  ,Min_SR_Suspensao_Tempo=min(`Suspensao Tempo`)
  
),by=IdUsuario ]





TappyData_RS_Direction<-TappyData[Direction=="RS",.(
  mean_RS_Latencia_Tempo=mean(`Latencia`)
  ,sd_RS_Latencia_Tempo=sd(`Latencia`)
  ,yule_RS_Latencia_Tempo=yule(`Latencia`)
  ,IQR_RS_Latencia_Tempo=IQR(`Latencia`)
  ,median_RS_Latencia_Tempo=median(`Latencia`)
  ,skewness_RS_Latencia_Tempo=skewness(`Latencia`)
  ,Max_RS_Latencia_Tempo=max(`Latencia`)
  ,Min_RS_Latencia_Tempo=min(`Latencia`)
  ,Count_RS=.N
  
  ,Max_RS_Suspensao_Tempo=max(`Suspensao Tempo`)
  ,Min_RS_Suspensao_Tempo=min(`Suspensao Tempo`)
  ,mean_RS_Suspensao_Tempo=mean(`Suspensao Tempo`)
  ,sd_RS_Suspensao_Tempo=sd(`Suspensao Tempo`)
  ,yule_RS_Suspensao_Tempo=yule(`Suspensao Tempo`)
  ,IQR_RS_Suspensao_Tempo=IQR(`Suspensao Tempo`)
  ,median_RS_Suspensao_Tempo=median(`Suspensao Tempo`)
  ,skewness_RS_Suspensao_Tempo=skewness(`Suspensao Tempo`)
),by=IdUsuario ]



TappyData_RL_Direction<-TappyData[Direction=="RL",.(
  mean_RL_Latencia_Tempo=mean(`Latencia`)
  ,sd_RL_Latencia_Tempo=sd(`Latencia`)
  ,yule_RL_Latencia_Tempo=yule(`Latencia`)
  ,IQR_RL_Latencia_Tempo=IQR(`Latencia`)
  ,median_RL_Latencia_Tempo=median(`Latencia`)
  ,skewness_RL_Latencia_Tempo=skewness(`Latencia`)
  ,Max_RL_Latencia_Tempo=max(`Latencia`)
  ,Min_RL_Latencia_Tempo=min(`Latencia`)
  ,Count_RL=.N
  ,Max_RL_Suspensao_Tempo=max(`Suspensao Tempo`)
  ,Min_RL_Suspensao_Tempo=min(`Suspensao Tempo`)
  
  ,mean_RL_Suspensao_Tempo=mean(`Suspensao Tempo`)
  ,sd_RL_Suspensao_Tempo=sd(`Suspensao Tempo`)
  ,yule_RL_Suspensao_Tempo=yule(`Suspensao Tempo`)
  ,IQR_RL_Suspensao_Tempo=IQR(`Suspensao Tempo`)
  ,median_RL_Suspensao_Tempo=median(`Suspensao Tempo`)
  ,skewness_RL_Suspensao_Tempo=skewness(`Suspensao Tempo`)
),by=IdUsuario ]




TappyData_RR_Direction<-TappyData[Direction=="RR",.(
  mean_RR_Latencia_Tempo=mean(`Latencia`)
  ,sd_RR_Latencia_Tempo=sd(`Latencia`)
  ,yule_RR_Latencia_Tempo=yule(`Latencia`)
  ,IQR_RR_Latencia_Tempo=IQR(`Latencia`)
  ,median_RR_Latencia_Tempo=median(`Latencia`)
  ,skewness_RR_Latencia_Tempo=skewness(`Latencia`)
  ,Max_RR_Latencia_Tempo=max(`Latencia`)
  ,Min_RR_Latencia_Tempo=min(`Latencia`)
  ,Count_RR=.N
  ,Max_RR_Suspensao_Tempo=max(`Suspensao Tempo`)
  ,Min_RR_Suspensao_Tempo=min(`Suspensao Tempo`)
  ,mean_RR_Suspensao_Tempo=mean(`Suspensao Tempo`)
  ,sd_RR_Suspensao_Tempo=sd(`Suspensao Tempo`)
  ,yule_RR_Suspensao_Tempo=yule(`Suspensao Tempo`)
  ,IQR_RR_Suspensao_Tempo=IQR(`Suspensao Tempo`)
  ,median_RR_Suspensao_Tempo=median(`Suspensao Tempo`)
  ,skewness_RR_Suspensao_Tempo=skewness(`Suspensao Tempo`)
),by=IdUsuario ]

TappyData_LR_Direction<-TappyData[Direction=="LR",.(
  mean_LR_Latencia_Tempo=mean(`Latencia`)
  ,sd_LR_Latencia_Tempo=sd(`Latencia`)
  ,yule_LR_Latencia_Tempo=yule(`Latencia`)
  ,IQR_LR_Latencia_Tempo=IQR(`Latencia`)
  ,median_LR_Latencia_Tempo=median(`Latencia`)
  ,skewness_LR_Latencia_Tempo=skewness(`Latencia`)
  ,Max_LR_Latencia_Tempo=max(`Latencia`)
  ,Min_LR_Latencia_Tempo=min(`Latencia`)
  ,Count_LR=.N
  ,Max_LR_Suspensao_Tempo=max(`Suspensao Tempo`)
  ,Min_LR_Suspensao_Tempo=min(`Suspensao Tempo`)
  
  ,mean_LR_Suspensao_Tempo=mean(`Suspensao Tempo`)
  ,sd_LR_Suspensao_Tempo=sd(`Suspensao Tempo`)
  ,yule_LR_Suspensao_Tempo=yule(`Suspensao Tempo`)
  ,IQR_LR_Suspensao_Tempo=IQR(`Suspensao Tempo`)
  ,median_LR_Suspensao_Tempo=median(`Suspensao Tempo`)
  ,skewness_LR_Suspensao_Tempo=skewness(`Suspensao Tempo`)
),by=IdUsuario ]



TappyData_LL_Direction<-TappyData[Direction=="LL",.(
  mean_LL_Latencia_Tempo=mean(`Latencia`)
  ,sd_LL_Latencia_Tempo=sd(`Latencia`)
  ,yule_LL_Latencia_Tempo=yule(`Latencia`)
  ,IQR_LL_Latencia_Tempo=IQR(`Latencia`)
  ,median_LL_Latencia_Tempo=median(`Latencia`)
  ,skewness_LL_Latencia_Tempo=skewness(`Latencia`)
  ,Max_LL_Latencia_Tempo=max(`Latencia`)
  ,Min_LL_Latencia_Tempo=min(`Latencia`)
  ,Count_LL=.N
  ,Max_LL_Suspensao_Tempo=max(`Suspensao Tempo`)
  ,Min_LL_Suspensao_Tempo=min(`Suspensao Tempo`)
  
  ,mean_LL_Suspensao_Tempo=mean(`Suspensao Tempo`)
  ,sd_LL_Suspensao_Tempo=sd(`Suspensao Tempo`)
  ,yule_LL_Suspensao_Tempo=yule(`Suspensao Tempo`)
  ,IQR_LL_Suspensao_Tempo=IQR(`Suspensao Tempo`)
  ,median_LL_Suspensao_Tempo=median(`Suspensao Tempo`)
  ,skewness_LL_Suspensao_Tempo=skewness(`Suspensao Tempo`)
),by=IdUsuario ]




TappyData_LS_Direction<-TappyData[Direction=="LS",.(
  mean_LS_Latencia_Tempo=mean(`Latencia`)
  ,sd_LS_Latencia_Tempo=sd(`Latencia`)
  ,yule_LS_Latencia_Tempo=yule(`Latencia`)
  ,IQR_LS_Latencia_Tempo=IQR(`Latencia`)
  ,median_LS_Latencia_Tempo=median(`Latencia`)
  ,skewness_LS_Latencia_Tempo=skewness(`Latencia`)
  ,Max_LS_Latencia_Tempo=max(`Latencia`)
  ,Min_LS_Latencia_Tempo=min(`Latencia`)
  ,Count_LS=.N
  ,Max_LS_Suspensao_Tempo=max(`Suspensao Tempo`)
  ,Min_LS_Suspensao_Tempo=min(`Suspensao Tempo`)
  ,mean_LS_Suspensao_Tempo=mean(`Suspensao Tempo`)
  ,sd_LS_Suspensao_Tempo=sd(`Suspensao Tempo`)
  ,yule_LS_Suspensao_Tempo=yule(`Suspensao Tempo`)
  ,IQR_LS_Suspensao_Tempo=IQR(`Suspensao Tempo`)
  ,median_LS_Suspensao_Tempo=median(`Suspensao Tempo`)
  ,skewness_LS_Suspensao_Tempo=skewness(`Suspensao Tempo`)
),by=IdUsuario ]




TappyData_SL_Direction<-TappyData[Direction=="SL",.(
  mean_SL_Latencia_Tempo=mean(`Latencia`)
  ,sd_SL_Latencia_Tempo=sd(`Latencia`)
  ,yule_SL_Latencia_Tempo=yule(`Latencia`)
  ,IQR_SL_Latencia_Tempo=IQR(`Latencia`)
  ,median_SL_Latencia_Tempo=median(`Latencia`)
  ,skewness_SL_Latencia_Tempo=skewness(`Latencia`)
  ,Max_SL_Latencia_Tempo=max(`Latencia`)
  ,Min_SL_Latencia_Tempo=min(`Latencia`)
  ,Count_SL=.N
  ,Max_SL_Suspensao_Tempo=max(`Suspensao Tempo`)
  ,Min_SL_Suspensao_Tempo=min(`Suspensao Tempo`)
  
  ,mean_SL_Suspensao_Tempo=mean(`Suspensao Tempo`)
  ,sd_SL_Suspensao_Tempo=sd(`Suspensao Tempo`)
  ,yule_SL_Suspensao_Tempo=yule(`Suspensao Tempo`)
  ,IQR_SL_Suspensao_Tempo=IQR(`Suspensao Tempo`)
  ,median_SL_Suspensao_Tempo=median(`Suspensao Tempo`)
  ,skewness_SL_Suspensao_Tempo=skewness(`Suspensao Tempo`)
),by=IdUsuario ]


setkey(TappyData_ALL, IdUsuario)
setkey(TappyData_L_Mao, IdUsuario)
setkey(TappyData_R_Mao, IdUsuario)
setkey(TappyData_S_Mao, IdUsuario)
setkey(TappyData_SS_Direction, IdUsuario)
setkey(TappyData_SR_Direction, IdUsuario)
setkey(TappyData_RS_Direction, IdUsuario)
setkey(TappyData_RL_Direction, IdUsuario)
setkey(TappyData_RR_Direction, IdUsuario)
setkey(TappyData_LR_Direction, IdUsuario)
setkey(TappyData_LL_Direction, IdUsuario)
setkey(TappyData_LS_Direction, IdUsuario)
setkey(TappyData_SL_Direction, IdUsuario)


Temp<-merge(TappyData_R_Mao,TappyData_S_Mao,all = TRUE)
Temp<-merge(Temp,TappyData_ALL,all = TRUE)
Temp<-merge(Temp,TappyData_L_Mao,all = TRUE)
Temp<-merge(Temp,TappyData_SS_Direction,all = TRUE)
Temp<-merge(Temp,TappyData_SR_Direction,all = TRUE)
Temp<-merge(Temp,TappyData_RS_Direction,all = TRUE)
Temp<-merge(Temp,TappyData_RL_Direction,all = TRUE)
Temp<-merge(Temp,TappyData_RR_Direction,all = TRUE)
Temp<-merge(Temp,TappyData_LR_Direction,all = TRUE)
Temp<-merge(Temp,TappyData_LL_Direction,all = TRUE)
Temp<-merge(Temp,TappyData_LS_Direction,all = TRUE)
Temp<-merge(Temp,TappyData_SL_Direction,all = TRUE)
Temp<-merge(Temp,TappyData_ALL,all = TRUE)
TappyDataSummaryPerUser<-Temp


##
nm1 <- 
  names(TappyDataSummaryPerUser)
nm1<-nm1[2:182]


for(j in nm1){
  indx <- which(is.na(TappyDataSummaryPerUser[[j]]))
  median<-median(TappyDataSummaryPerUser[[j]],na.rm=TRUE)
  set(TappyDataSummaryPerUser, i=indx, j=j, value=median)
}
