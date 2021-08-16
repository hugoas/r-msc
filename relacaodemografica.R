library(ggplot2)
library(tidyverse)
library(caret)
library(mlbench)
library(tidyr)
library(Boruta)
library(data.table)
library(modelr)
library(cowplot)
library(ROSE)
library(e1071)
library(Matrix)
library(mlr)
library(MLeval)
library(ggcorrplot)

# Dados demográficos e relação parkinson

demo <- read_csv("")
labs <- read_csv("")
exam <- read_csv("")

# Criando dataframes
demo_dataset <- demo %>% select(SEQN , "genero" = RIidadeNDR, "idade" = RIDidadeYR)

#Dados de Lab
lab_dataset <- labs %>% select(SEQN, "HbA1c" = LBXGH)

# Join dos dados
dataset <- lab_dataset %>% inner_join(demo_dataset, by = "SEQN") %>%
  inner_join(exam, by = "SEQN")

summary(dataset)

dataset <- dataset[apply(dataset,2,function(x) mean(is.na(x))<=.50)] 

write_csv(dataset, "")

# Remover dados irrelevantes
dataset1 <- dataset %>% select(SEQN, HbA1c,  idade, "peso" = BMXWT, "altura" = BMXHT,
                     "BMI" = BMXBMI, "TREMOR_PERNA" = BMXLEG, 
                     "TREMOR_MAO" = BMXARML, "CIRCULACAO" = BMXARMC, 
                     "CIRCUNFERENCIAABD" = BMXWAIST, "OBSESIDADEABD" = BMDAVSAD)


# Variacao por idade
dataset1 %>%
  filter(idade<60) %>%
  ggplot(aes(x = as.factor(idade), y = altura)) + 
  geom_boxplot(color = "blue", fill = "blue", 
               alpha = 0.2, naotch = TRUE, naotchwidth = 0.8, outlier.colour = "red",
               outlier.fill = "red", outlier.size = 3, outlier.alpha = 1) + 
  labs(title = "Variacao de altura por idade",
       x= "idade",
       y = "altura em cm")


# Remover dados de mais de 5 anos
dataset1 <- subset(dataset1, idade>=5)


f=function(x){
  x<-as.numeric(as.character(x))
  x[is.na(x)] =median(x, na.rm=TRUE) #converter o item por media 
}

Final_dataset = data.frame(apply(dataset1,2,f))
f_dataset <- data.frame(apply(dataset1,2,f))

summary(Final_dataset)

Final_dataset$HbA1c <- cut(Final_dataset$HbA1c,
                      breaks = c(-Inf, 6.5, Inf),
                      labels = c("nao", "sim"),
                      right = FALSE) # 

names(Final_dataset)[names(Final_dataset) == "HbA1c"] <- "Parkinson"
str(Final_dataset)

#PEssoas que tem a doença
Final_dataset %>%
  ggplot(aes(x = Parkinson)) + geom_bar(data = Final_dataset, fill = "blue", 
                                       color = "black", alpha  = 0.7) +
  labs(title = "Pessoas que não tem Parkinson")

#Fazendo um dataset só com doentes
Parkinson_data <- Final_dataset %>%
  filter(Parkinson == "sim")

x1 <- Parkinson_data %>%
  filter(idade>5) %>%
  filter(idade<=15) %>%
  count(Parkinson == "sim")

x2 <- Parkinson_data %>%
  filter(idade>15) %>%
  filter(idade<=25) %>%
  count(Parkinson == "sim")

x3 <- Parkinson_data %>%
  filter(idade>25) %>%
  filter(idade<=35) %>%
  count(Parkinson == "sim")

x4 <- Parkinson_data %>%
  filter(idade>35) %>%
  filter(idade<=45) %>%
  count(Parkinson == "sim")

x5 <- Parkinson_data %>%
  filter(idade>45) %>%
  filter(idade<=55) %>%
  count(Parkinson == "sim")

x6 <- Parkinson_data %>%
  filter(idade>55) %>%
  filter(idade<=65) %>%
  count(Parkinson == "sim")

x7 <- Parkinson_data %>%
  filter(idade>65) %>%
  count(Parkinson == "sim")

idade_grupos <- x1 %>% full_join(x2)%>% full_join(x3)%>% 
  full_join(x4)%>% full_join(x5)%>% full_join(x6)%>% full_join(x7)

col_names <- c("16 to 25", "26 to 35", "36 to 45", "46 to 55", "56 to 65",
               "66 to 80" )
idade_g <- c( 7, 13, 71, 124, 155, 244)

x8 <- data.frame(col_names, idade_g)
x8 <- x8 %>%
  group_by(idade_g)


#Idade com maior frequencia da idade
ggplot(x8, aes(x=col_names,y = idade_g, fill = idade_g)) +  geom_col() + 
  scale_fill_viridis_c(alpha = 0.95) +
  geom_text(aes(label = idade_g), vjust = 0.01, size = 5) + 
  labs(title = "Casos por idade",
       x = "Grupos de idade",
       y = "Numero de casos")

# Distribuicao por idade

ggplot(Final_dataset, aes(x = Parkinson, y = idade)) + 
  geom_boxplot(color = "blue", fill = "blue", alpha = 0.2, 
               naotch = TRUE, naotchwidth = 0.8, outlier.colour = "red",
               outlier.fill = "red",
               outlier.size = 3, outlier.alpha = 1) + 
  labs(title = "Distribution of people having Parkinson with idade",
       x = "Parkinson",
       y = "idade")

# Relacao amostra e casos
bmi_under <- Parkinson_data %>%
  filter(BMI<18.5) %>%
  count(Parkinson == "sim")

bmi_naormal <- Parkinson_data %>%
  filter(BMI>=18.5) %>%
  filter(BMI<25) %>%
  count(Parkinson == "sim")

bmi_over <- Parkinson_data %>%
  filter(BMI>=25) %>%
  filter(BMI<30) %>%
  count(Parkinson == "sim")

bmi_obese <- Parkinson_data %>%
  filter(BMI>=30) %>%
  count(Parkinson == "sim")

bmi_grupos <- bmi_under %>% full_join(bmi_naormal)%>% full_join(bmi_over)%>%
  full_join(bmi_obese)

bx <- c("Subpeso", "Normal", "Sobrepeso", "Obeso")
by <- c( 3, 67,187, 347)

bd <- data.frame(bx, by)

ggplot(bd, aes(x=bx,y = by, fill = bx)) +  geom_col() + 
  scale_fill_viridis_d(alpha = 0.95) +
  geom_text(aes(label = by), vjust = 0.01, size = 5) + 
  labs(title = "Casos de Parkinson por Peso",
       x = "Grupos de peso",
       y = "Numero de casos")
#correlacao plot
corr <- round(cor(f_dataset), 1)

ggcorrplot(corr, hc.order = TRUE,
           lab = TRUE,
           lab_size = 3,
           method = "circle",
           colors = c("tomato2", "white", "springgreen3"),
           title = "Correlacao",
           ggtheme = theme_bw())

## Importancia de Atributos com a lib Boruta

boruta_output <- Boruta(Parkinson ~ . , data=na.omit(Final_dataset), maxRuns = 200,
                        doTrace=2)
print(boruta_output)
#names(boruta_output)

boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)

roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejeitado', c('meanImp', 'decisao')]
imps2[order(-imps2$meanImp), ] 
setDT(imps2, keep.rownames = TRUE)[]

# Plot relevancia
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Atributos importantes") 