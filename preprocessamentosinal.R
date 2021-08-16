#ler os arquivos e extraindo o sinal
Lista=list.files()
library(signal)
Filtro = butter(2, 0.6, type = 'low', plane='z')  


for(i in 1:length(L))
{
  print(i)
  Extracao=read.table(Lista[i])
  Extracao=data.matrix(Extracao)
  Extracao=Extracao[,-1]
  
  for(j in 1:ncol(Extracao))
  {
    s=E[,j]
    yfiltro = signal:::filter(Filtro, s)
    n=norm(yfiltro,type = "2")
    Extracao[,j]=yfiltro/n
  }
  
  lab=paste("P",substr(Lista[i],start = 1,stop = 9),"(0.6)",".csv",sep = "")
  write.csv(Extracao,lab,row.names = FALSE)
}