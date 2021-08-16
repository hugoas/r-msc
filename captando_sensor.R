#captando atributos de um sensor

feat_extraction<-function(senso)
{
print("Extraindo atributos")
  
L=list.files()

#L=L[1:10]
  
sensor=senso

sinal=matrix(ncol = 9,nrow = length(L))
cl=vector("numeric",length = length(L))


for(k in 1:length(L))
{
 # print(k)
  M=read.csv(L[k])
  s=M[,sensor]

  #class
  if(substr(L[k],start = 4,stop=5)==c("Pt"))
  {
    cl[k]=1
  }
 # print(cl[k])
  
  #extract different features
  
  #média
  sinal[k,1]=mean(s)
  
  #dv
  sinal[k,2]=sd(s)
  
  #variancia
  sinal[k,3]=max(s)-min(s)
  
  #assimetria
  sinal[k,4]=skewness(s)
  
  #curtose
  sinal[k,5]=kurtosis(s)

  #mediana
  sinal[k,6]=median(s)
  
  #interquartil
  sinal[k,7]=IQR(s)
}


#colunanomes(sinal)[1]=c("media")
#colunanomes(sinal)[2]=c("dv")
#colunanomes(sinal)[3]=c("variancia")
#colunanomes(sinal)[4]=c("assimetria")
#colunanomes(sinal)[5]=c("curtose")
#colunanomes(sinal)[6]=c("mediana")
#colunanomes(sinal)[7]=c("Interquartil")

sinal=cbind(sinal,cl)

#nul=which(is.na(sinal[,1]))
#nul=as.vector(nul)

print(dim(sinal))

return(sinal)
}
