paquetes <- c("tidyverse","ggplot2","cluster","dplyr","Rtsne","bindrcpp")
sapply(paquetes,require,character.only=TRUE)

indicadores <- function(agnes4,datos,imprime=p) {
  
  
  cat('                   ','ANALISIS DE CLUSTER', date(), sep='                ','\n')
  cat('    ','\n')  #ES RUSTICO pero...pa espaciar la impresion un poco
  
  
  
  agnes4<-unlist(agnes4)
  agnes4<-matrix(agnes4,length(agnes4)/2) #unlisteo agnes4 y la reconstruyo en forma de matriz
  p<-nrow(agnes4)
  
  TT<-as.matrix(datos)
  rsqden<-sum(diag(var(TT,use='pairwise.complete.obs')))*(nrow(TT)-1)
  
  
  guardo <- matrix(0,p,4)
  
  rsqnum<-0
  vec<-0
  aver<-matrix(0,300,1)
  DH <- 0/0
  
  
  for(i in 1:p)
  { h<-agnes4[i,]
  g1<-h[1]
  g2<-h[2]
  
  n1 <- 1
  n2 <- 1
  
  if((g1<0)&(g2<0))
  {vec<-c(vec,list(cbind(-g1,-g2)))
  cluster1 <- c(0,0)
  cluster2 <- c(0,0)
  g12<-c(-g1,-g2)
  union<-datos[g12,]}
  else if((g1<0)&(g2>0))
  {vec<-c(vec,list(c(vec[g2+1],-g1,recursive=T)))
  elements2<-c(vec[g2+1],recursive=T)
  cluster2 <- datos[elements2,]
  cluster1 <- c(0,0)
  g12<-c(elements2,-g1)
  union<-datos[g12,]
  aver[g2] <- 0
  n2 <- nrow(cluster2)
  }
  else if((g1>0)&(g2<0))
  {vec<-c(vec,list(c(vec[g1+1],-g2,recursive=T)))
  elements1<-c(vec[g1+1],recursive=T)
  cluster1 <- datos[elements1,]
  cluster2 <- c(0,0)
  g12<-c(elements1,-g2)
  union<-datos[g12,]
  aver[g1] <- 0
  n1 <- nrow(cluster1)
  }
  else if((g1>0)&(g2>0))
  {vec<-c(vec,list(c(vec[g1+1],vec[g2+1],recursive=T)))
  elements1<-c(vec[g1+1],recursive=T)
  elements2<-c(vec[g2+1],recursive=T)
  cluster1 <- datos[elements1,]
  cluster2 <- datos[elements2,]
  g12<-c(elements1,elements2)
  union<-datos[g12,]
  aver[g1] <- 0
  aver[g2] <- 0
  n1 <- nrow(cluster1)
  n2 <- nrow(cluster2)
  }
  else
    end
  
  Freq <- length(unlist(g12))    ####cuenta la cantidad de elementos en el grupo creado en ese paso
  
  tWg12<-sum(diag(var(union,use='pairwise.complete.obs')))*(nrow(union)-1) 
  tWg1<-sum(diag(var(cluster1,use='pairwise.complete.obs')))*(n1 -1)
  tWg2<-sum(diag(var(cluster2,use='pairwise.complete.obs')))*(n2 -1)
  
  DH<-(tWg1+tWg2)/tWg12         ####Duda-Hart
  pst<-((1/DH)-1)*(n1+n2-2)     ####pseudot^2
  
  aver[i]<-tWg12       
  rsqnum<-sum(aver)
  rsq<-1-(rsqnum/rsqden)        ####R^2
  
  G<-nrow(datos)-i
  psF<-(rsq/(G-1))/((1-rsq)/(nrow(datos)-G))   ####pseudoF(kalinski-harabazs)
  
  guardo[i,] <- cbind(Freq,rsq,psF,pst)     
  }
  
  salida <- as.data.frame(cbind(agnes4,guardo))  #cbind(agnes4[,1],agnes4[,2],guardo)
  attr(salida,'names') <- c('.','history','Freq','Rcuad','psF','psT')
  
  
  print(salida[(p-imprime):p,])
  
  
  write.table(salida, file = "IND_salida.txt", append = FALSE, quote = TRUE, sep = " \t", eol = "\n", na = "NA", dec = ".", row.names = TRUE,  col.names = TRUE, qmethod = c("escape", "double"))
  
  
  salida[(p-imprime):p,]     #para poder hacer II<-indicadores(...); II
}

# datos <- read.table("fcea1.txt",header=TRUE) %>% as_data_frame() %>% 
#   mutate(
#   trab=recode(trab,'trabSI'='si',"trabNO"="no"),
#   hstrab=recode(hstrab,"notrab"="0","menos20"="<20h","mas40"=">20h"),
#   instituto=tituto,
#   edad=recode(edadr,"mayores20"=">20","menores20"="<20")
#   ) %>% dplyr::select(-c(edadr,tituto))

rempl <- function(X) as.numeric(str_replace_all(X,c("alto."="2","medio."="1","bajo."="0")))


datos <- read.table("fcea1.txt",header=TRUE) %>% as_data_frame() %>% 
  mutate(
    trab=recode(trab,'trabSI'='si',"trabNO"="no"),
    hstrab=recode(hstrab,"notrab"="0","menos20"="<20h","mas40"=">20h"),
    instituto=tituto,
    mate1=rempl(mate1),
    tot1=rempl(tot1),
    info1=rempl(info1),
    cont1=rempl(cont1),
    comp1=as.numeric(str_replace_all(comp1,c("alto.."="2","medio.."="1","bajo.."="0"))),
    edad=recode(edadr,"mayores20"=">20","menores20"="<20")
  ) %>%
  dplyr::select(-c(edadr,tituto)) %>%
  mutate_at(vars(mate1:cont1),function(x) x/2)

glimpse(datos)
summary(datos)

#####


#Cluster Jerarquico
ClusJer4 <- datos %>% select(c(mate1,cont1,info1,comp1)) %>% agnes(metric = "euclidean")
plot(ClusJer4,which=2)
indicadores(ClusJer4[4],datos %>% select(c(mate1,cont1,info1,comp1)),imprime=20)

datos <- datos %>% mutate(ClusJer=cutree(ClusJer4,2)-1)

datos %>% filter(ClusJer==1) %>% select(mate1:cont1) %>% summary
datos %>% filter(ClusJer==2) %>% select(mate1:cont1) %>% summary

datos %>% select(mate1:cont1,ClusJer) %>% 
  gather(prueba,resultado,-ClusJer) %>% #cbind(ClusJer=datos$ClusJer) %>% 
  ggplot() + aes(x=prueba, y=resultado) + geom_boxplot(fill="tomato") +
  facet_grid(.~ClusJer)

datos %>% filter(ClusJer==1) %>% select(info1) %>% table
datos %>% filter(ClusJer==2) %>% select(info1) %>% table

### Discriminante logistico

modLog <- glm(ClusJer~trab+hstrab+edHOGAR+anhofin+lugar,family=binomial(link="logit"),data=datos)

summary(modLog)
