#Reproducao artigo de Arthur e prof Marcelo

#libs
library(skimr)
library(stringr)
library(psych)
library(ROSE)
library(ggplot2)
library(caret)

hospData <- read.csv("")
skim(hospData)

hospData$admissao_tipo_id <- as.factor(hospData$admissao_tipo_id)
hospData$disposicao_id <- as.factor(hospData$disposicao_id)
hospData$admissao_source_id <- as.factor(hospData$admissao_source_id)

#Tratar variaveis nulas ou inexistentes
count <- 0
for(i in 1:ncol(hospData)){
  if(is.factor(hospData[,i])){
    for(j in 1:nrow(hospData)){
      if(hospData[j,i]== "?" | hospData[j,i]== "Unknown/Invalid" ){
        count <- count + 1
        hospData[j,i] <- NA  #replace "?" and "Unknown/Invalid" values with NA
      }
    }
    if(count > 0){
      print(c(colnames(hospData)[i],count))
    }
  }
  count <- 0
}

write.csv(hospData, file = "hospData_NA.csv")

hospD <- read.csv("./hospData_NA.csv")
hospD$X <- NULL

#deletar colunas inuteis
hospD$encounter_id <- NULL
hospD$diag_2 <- NULL
hospD$diag_3 <- NULL
dim(hospD)

par(mfrow = c(1,2))
barplot(table(hospD$dispensa_id), main = "Antes")
#tempo de retorno após primeira dispensa
#remove
hospD <- hospD[!hospD$dispensa_id %in% c(11,13,14,19,20,21), ]
barplot(table(hospD$dispensa_id), main = "Depois")

colnames(hospD)[5] <- "admissao_tipo"
barplot(table(hospD$admissao_tipo))

hospD$admissao_tipo <- replace(hospD$admissao_tipo,hospD$admissao_tipo == 2, 1)
hospD$admissao_tipo <- replace(hospD$admissao_tipo,hospD$admissao_tipo == 7, 1)
hospD$admissao_tipo <- replace(hospD$admissao_tipo,hospD$admissao_tipo == 6, 5)
hospD$admissao_tipo <- replace(hospD$admissao_tipo,hospD$admissao_tipo == 8, 5)

barplot(table(hospD$admissao_tipo), main = "Pós colapso")

#mudar os nomes
hospD$admissao_tipo <- str_replace(hospD$admissao_tipo,"1","Emergencia")
hospD$admissao_tipo <- str_replace(hospD$admissao_tipo,"5","Outro")
hospD$admissao_tipo <- str_replace(hospD$admissao_tipo,"3","Eletiva")
hospD$admissao_tipo <- str_replace(hospD$admissao_tipo,"4","Recepcao")

hospD$admissao_tipo <- as.factor(hospD$admissao_tipo)
barplot(table(hospD$admissao_tipo))


colnames(hospD)[7] <- "admissao"
barplot(table(hospD$admissao))

colnames(hospD)[6] <- "disposicao"
barplot(table(hospD$disposicao))


hospD$disposicao <- case_when(hospD$disposicao %in% "1" ~ "Casa",
                                         TRUE ~ "Outro")

hospD$disposicao <- as.factor(hospD$disposicao)
barplot(table(hospD$disposicao), main = "Após pre-diagnostico")

hospD$diag_1 <- as.character(hospD$diag_1)

hospD<- mutate(hospD, diagnostico_primario =
                 ifelse(str_detect(diag_1, "V") | str_detect(diag_1, "E"),"Outros", 
                        # escolher por tipo de enfermidade
                        ifelse(str_detect(diag_1, "250"), "Diabetes",
                               ifelse((as.integer(diag_1) >= 390 & as.integer(diag_1) <= 459) | as.integer(diag_1) == 785, "Circulatorio",
                                      ifelse((as.integer(diag_1) >= 460 & as.integer(diag_1) <= 519) | as.integer(diag_1) == 786, "Respiratorio", 
                                             ifelse((as.integer(diag_1) >= 520 & as.integer(diag_1) <= 579) | as.integer(diag_1) == 787, "Digestivo", 
                                                    ifelse((as.integer(diag_1) >= 580 & as.integer(diag_1) <= 629) | as.integer(diag_1) == 788, "Genital",
                                                           ifelse((as.integer(diag_1) >= 140 & as.integer(diag_1) <= 239), "Neoplasma",  
                                                                  ifelse((as.integer(diag_1) >= 710 & as.integer(diag_1) <= 739), "Muscoesqueleto",          
                                                                         ifelse((as.integer(diag_1) >= 800 & as.integer(diag_1) <= 999), "Injuria",                    
                                                                                "Outro"))))))))))



barplot(table(hospD$idade))



#regroup the "idade" to [0-40],[40-50],[50-60],[60-70],[70-80],[80-100]
hospD$idade <- case_when(hospD$idade %in% c("[0-10)","[10-20)","[20-30)","[30-40)") ~ "[0-40]",
                       hospD$idade %in% c("[80-90)","[90-100)") ~ "[80-100]",
                       hospD$idade %in% "[40-50)" ~ "[40-50]",
                       hospD$idade %in% "[50-60)" ~ "[50-60]",
                       hospD$idade %in% "[60-70)" ~ "[60-70]",
                       TRUE ~ "[70-80]")
barplot(table(hospD$idade), main = "Regroup idade")

hospD$idade <- as.factor(hospD$idade)

#renomear
colnames(hospD)[17] <- "HbA1c"

#remover atributos de medicaao
hospD$repaglinide <- NULL
hospD$nateglinide <- NULL
hospD$chlorpropamide <-NULL
hospD$acetohexamide <- NULL
hospD$tolbutamide <- NULL
hospD$acarbose <- NULL
hospD$miglitol <- NULL
hospD$troglitazone <- NULL
hospD$tolazamide <- NULL

#checar
dim(hospD)

#categorize "readmitted" to 1 --patient was readmitted within 30 days, 0-- readmissao after 30 days and no readmissao
hospD$readmitted <- case_when(hospD$readmitted %in% c(">30","NO") ~ "0",
                              TRUE ~ "1")
hospD$readmitted <- as.factor(hospD$readmitted)
levels(hospD$readmitted)

par(mfrow = c(2,4))
boxplot(hospD$tempo, main = "tempo")
boxplot(hospD$num_lab_procedimentos, main = "num_lab_procedimentos")
boxplot(hospD$num_procedures, main = "num_procedimentos")
boxplot(hospD$num_medicacao, main = "num_medicacao")
boxplot(hospD$num_saida, main = "num_saida")
boxplot(hospD$num_emergencia, main = "num_emergencia")
boxplot(hospD$num_paciente_internado, main = "num_paciente_internado")
boxplot(hospD$num_diagnostico, main = "num_diagnostico")

hospD$num_emergencia <- NULL
hospD$num_paciente_internado <- NULL
hospD$num_saida <- NULL
dim(hospD)



#remover outliers
outliers_remover <- function(a){
  df <- a
  aa <- c()
  count <- 1
  for(i in 1:ncol(df)){
    if(is.integer(df[,i])){
      Q3 <- quantile(df[,i], 0.75, na.rm = TRUE)
      Q1 <- quantile(df[,i], 0.25, na.rm = TRUE) 
      IQR <- Q3 - Q1  #IQR(df[,i])
      upper <- Q3 + 1.5 * IQR
      lower <- Q1 - 1.5 * IQR
      for(j in 1:nrow(df)){
        if(is.na(df[j,i]) == TRUE){
          next
        }
        else if(df[j,i] > upper | df[j,i] < lower){
          aa[count] <- j
          count <- count+1                  
        }
      }
    }
  }
  df <- df[-aa,]
}
hospD <- outliers_remover(hospD)


