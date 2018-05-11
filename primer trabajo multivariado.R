paquetes <- c("tidyverse","ggplot2","cluster","dplyr","Rtsne","bindrcpp")


datos <- read.table("fcea1.txt",header=TRUE) %>% as_data_frame()

#Arreglamos un poco nuestra base de datos. Cuantificamos las variables ordinales

trabaja=as.vector(datos$trab)
trabaja[which(trabaja=="trabNO")]=c("NO")
trabaja[which(trabaja=="trabSI")]=c("SI")
trabaja=as.data.frame(trabaja)

hs_semanales=as.vector(datos$hstrab)
hs_semanales[which(hs_semanales=="notrab")]=c("No trabaja")
hs_semanales[which(hs_semanales=="menos20")]=c("Menos de 20 horas")
hs_semanales[which(hs_semanales=="mas40")]=c("M?s de 20 horas")
hs_semanales=as.data.frame(hs_semanales)

educ_madre=as.vector(datos$edmadre)
educ_madre[which(educ_madre=="primaria")]=c(0)
educ_madre[which(educ_madre=="secundaria")]=c(1)
educ_madre[which(educ_madre=="terciaria")]=c(2)
educ_madre=as.data.frame(educ_madre)

educ_padre=as.vector(datos$edpadre)
educ_padre[which(educ_padre=="primaria")]=c(0)
educ_padre[which(educ_padre=="secundaria")]=c(1)
educ_padre[which(educ_padre=="terciaria")]=c(2)
educ_padre=as.data.frame(educ_padre)

educ_hogar=as.vector(datos$edHOGAR)
educ_hogar=as.data.frame(educ_hogar)

ocupacion_madre=as.vector(datos$ocupmadre)
ocupacion_madre[which(ocupacion_madre=="amadecasa")]=c("Ama de casa")
ocupacion_madre[which(ocupacion_madre=="profesional")]=c("Profesional")
ocupacion_madre[which(ocupacion_madre=="jubilado")]=c("Jubilada")
ocupacion_madre[which(ocupacion_madre=="privado")]=c("Trabajo Privado")
ocupacion_madre[which(ocupacion_madre=="publico")]=c("Trabajo P?blico")
ocupacion_madre[which(ocupacion_madre=="otros")]=c("Otros")
ocupacion_madre=as.data.frame(ocupacion_madre)

ocupacion_padre=as.vector(datos$ocuppadre)
ocupacion_padre[which(ocupacion_padre=="profesionalP")]=c("Profesional")
ocupacion_padre[which(ocupacion_padre=="jubiladoP")]=c("Jubilado")
ocupacion_padre[which(ocupacion_padre=="privadoP")]=c("Trabajo Privado")
ocupacion_padre[which(ocupacion_padre=="publicoP")]=c("Trabajo P?blico")
ocupacion_padre[which(ocupacion_padre=="otrosP")]=c("Otros")
ocupacion_padre=as.data.frame(ocupacion_padre)

sexo=as.vector(datos$sexo)
sexo[which(sexo=="F")]=c("Femenino")
sexo[which(sexo=="M")]=c("Masculino")
sexo=as.data.frame(sexo)

procedencia=as.vector(datos$lugar)
procedencia=as.data.frame(procedencia)

finalizo_bachillerato=data.frame('Fin_Bachillerato'=c(datos$anhofin))

tramo_edad=as.vector(datos$edadr)
tramo_edad[which(tramo_edad=="mayores20")]=c("M?s de 20")
tramo_edad[which(tramo_edad=="menores20")]=c("20 o menos")
tramo_edad=as.data.frame(tramo_edad)

instituto=as.vector(datos$tituto)
instituto[which(instituto=="publico")]=c("P?blico")
instituto[which(instituto=="privado")]=c("Privado")
instituto=as.data.frame(instituto)

prueba_matematica=as.vector(datos$mate1)
prueba_matematica[which(prueba_matematica=="bajoM")]=c(0)
prueba_matematica[which(prueba_matematica=="medioM")]=c(1)
prueba_matematica[which(prueba_matematica=="altoM")]=c(2)
prueba_matematica=as.data.frame(prueba_matematica)

prueba_comprension_lectora=as.vector(datos$comp1)
prueba_comprension_lectora[which(prueba_comprension_lectora=="bajoCL")]=c(0)
prueba_comprension_lectora[which(prueba_comprension_lectora=="medioCL")]=c(1)
prueba_comprension_lectora[which(prueba_comprension_lectora=="altoCL")]=c(2)
prueba_comprension_lectora=as.data.frame(prueba_comprension_lectora)

prueba_info_general=as.vector(datos$info1)
prueba_info_general[which(prueba_info_general=="bajoI")]=c(1)
prueba_info_general[which(prueba_info_general=="medioI")]=c(2)
prueba_info_general[which(prueba_info_general=="altoI")]=c(3)
prueba_info_general=as.data.frame(prueba_info_general)

prueba_contabilidad=as.vector(datos$cont1)
prueba_contabilidad[which(prueba_contabilidad=="bajoC")]=c(0)
prueba_contabilidad[which(prueba_contabilidad=="medioC")]=c(1)
prueba_contabilidad[which(prueba_contabilidad=="altoC")]=c(2)
prueba_contabilidad=as.data.frame(prueba_contabilidad)

resultados_pruebas=as.vector(datos$tot1)
resultados_pruebas[which(resultados_pruebas=="bajoT")]=c(0)
resultados_pruebas[which(resultados_pruebas=="medioT")]=c(1)
resultados_pruebas[which(resultados_pruebas=="altoT")]=c(2)
resultados_pruebas=as.data.frame(resultados_pruebas)

estudiante=data.frame('Estudiante'=seq(1,242,by=1))

# Creamos el .dataframe de datos (datostrabajo)

datostrabajo=cbind(estudiante,trabaja,hs_semanales,educ_madre,educ_padre,educ_hogar,ocupacion_madre,ocupacion_padre,sexo,procedencia,finalizo_bachillerato,tramo_edad,instituto,prueba_matematica,prueba_comprension_lectora,prueba_info_general,prueba_contabilidad,resultados_pruebas)

# Observamos las variables

glimpse(datostrabajo)
summary(datostrabajo)

# Hacemos algunos gr?ficos de barras

a1=cbind(data.frame('Prueba'=c("Matem?tica","Comprensi?n Lectora","Info Gral","Contabilidad","Total"),data.frame('Cantidad'=c(66,80,45,90,62)),data.frame('Resultado'=rep("Bajo",5))))
a2=cbind(data.frame('Prueba'=c("Matem?tica","Comprensi?n Lectora","Info Gral","Contabilidad","Total"),data.frame('Cantidad'=c(129,57,166,98,132)),data.frame('Resultado'=rep("Medio",5))))
a3=cbind(data.frame('Prueba'=c("Matem?tica","Comprensi?n Lectora","Info Gral","Contabilidad","Total"),data.frame('Cantidad'=c(47,105,31,54,48)),data.frame('Resultado'=rep("Alto",5))))
a4=rbind(a1,a2,a3)
ggplot(a4, aes(fill=Resultado, y=Cantidad, x=Prueba))+geom_bar(position="dodge", stat="identity",colour="black",lwd=1)+ggtitle("Resultados de pruebas")+scale_fill_brewer(palette = "PuBuGn")

b1=cbind(data.frame('Familiar'=c("Madre","Padre")),data.frame('Cantidad'=c(36,48)),data.frame('Nivel'=c("Primaria","Primaria")))
b2=cbind(data.frame('Familiar'=c("Madre","Padre")),data.frame('Cantidad'=c(109,116)),data.frame('Nivel'=c("Secundaria","Secundaria")))
b3=cbind(data.frame('Familiar'=c("Madre","Padre")),data.frame('Cantidad'=c(97,78)),data.frame('Nivel'=c("Terciaria","Terciaria")))
b4=rbind(b1,b2,b3)
ggplot(b4, aes(fill=Nivel, y=Cantidad, x=Familiar))+geom_bar(position="dodge", stat="identity",colour="black",lwd=1)+ggtitle("Nivel educativo de los padres")+scale_fill_brewer(palette = "PuBuGn")

c1=cbind(data.frame('Familiar'=c("Madre","Padre")),data.frame('Cantidad'=c(38,0)),data.frame('Trabajo'=c("Ama de casa","Ama de casa")))
c2=cbind(data.frame('Familiar'=c("Madre","Padre")),data.frame('Cantidad'=c(30,32)),data.frame('Trabajo'=c("Trabajo P?blico","Trabajo P?blico")))
c3=cbind(data.frame('Familiar'=c("Madre","Padre")),data.frame('Cantidad'=c(54,67)),data.frame('Trabajo'=c("Trabajo Privado","Trabajo Privado")))
c4=cbind(data.frame('Familiar'=c("Madre","Padre")),data.frame('Cantidad'=c(66,36)),data.frame('Trabajo'=c("Profesional","Profesional")))
c5=cbind(data.frame('Familiar'=c("Madre","Padre")),data.frame('Cantidad'=c(12,18)),data.frame('Trabajo'=c("Jubilado","Jubilado")))
c6=cbind(data.frame('Familiar'=c("Madre","Padre")),data.frame('Cantidad'=c(42,89)),data.frame('Trabajo'=c("Otros","Otros")))
c7=rbind(c1,c2,c3,c4,c5,c6)
ggplot(c7, aes(fill=Trabajo, y=Cantidad, x=Familiar))+geom_bar(position="dodge", stat="identity",colour="black",lwd=1)+ggtitle("Profesi?n de los padres")+scale_fill_brewer(palette = "PuBuGn")

ggplot(hs_semanales,aes(x=hs_semanales))+geom_bar(fill="firebrick4",colour="black",lwd=1)+ggtitle("Situaci?n Laboral")

table(cbind(instituto,procedencia))

d1=cbind(data.frame('Lugar'=c("Interior","Montevideo")),data.frame('Cantidad'=c(62,98)),data.frame('Instituto'=c("P?blico","P?blico")))
d2=cbind(data.frame('Lugar'=c("Interior","Montevideo")),data.frame('Cantidad'=c(12,70)),data.frame('Instituto'=c("Privado","Privado")))
d3=rbind(d1,d2)
ggplot(d3, aes(fill=Instituto, y=Cantidad, x=Lugar))+geom_bar(position="dodge", stat="identity",colour="black",lwd=1)+ggtitle("Tipo de Instituto y lugar de procedencia")+scale_fill_brewer(palette = "PuBuGn")

table(cbind(sexo,tramo_edad))

e1=cbind(data.frame('Sexo'=c("Femenino","Masculino")),data.frame('Cantidad'=c(94,72)),data.frame('Edad'=c("20 o menos","20 o menos")))
e2=cbind(data.frame('Sexo'=c("Femenino","Masculino")),data.frame('Cantidad'=c(35,41)),data.frame('Edad'=c("M?s de 20","M?s de 20")))
e3=rbind(e1,e2)
ggplot(e3, aes(fill=Edad, y=Cantidad, x=Sexo))+geom_bar(position="dodge", stat="identity",colour="black",lwd=1)+ggtitle("Tramo de edad seg?n sexo")+scale_fill_brewer(palette = "PuBuGn")

ggplot(finalizo_bachillerato,aes(x=Fin_Bachillerato))+geom_bar(fill="firebrick4",colour="black",lwd=1)+ggtitle("A?o de Finalizaci?n del Bachillerato")+scale_x_continuous(breaks=seq(1980,2006,1))

# CL?STERS (Usando la m?trica de Gower y el algoritmo K-medoides)

# Creamos la matriz de distancias

distanciadegower=daisy(datostrabajo[, -1],metric = "gower",type = list(logratio = 3))
summary(distanciadegower)
matrizdistanciadegower<- as.matrix(distanciadegower)

# Observamos que los estudiantes m?s similares son el 18 y el 33, y los m?s dis?miles el 36 y el 180

datostrabajo[which(matrizdistanciadegower == min(matrizdistanciadegower[matrizdistanciadegower != min(matrizdistanciadegower)]),arr.ind = TRUE)[1, ], ]
datostrabajo[which(matrizdistanciadegower == max(matrizdistanciadegower[matrizdistanciadegower != max(matrizdistanciadegower)]),arr.ind = TRUE)[1, ], ]

# Decidamos la cantidad de cl?sters

numerodeclusters=c(NA)
for(i in 2:10){
  anchodesiluetasmedoides<- pam(distanciadegower,
  diss = TRUE,
  k = i)
  numerodeclusters[i] <- anchodesiluetasmedoides$silinfo$avg.width
}

plot(1:10, numerodeclusters,xlab = "N?mero de cl?sters",ylab = "Ancho de silueta")

# El ancho de silueta se maximiza usando 2 c?sters

# Creamos los cl?sters

clusterskmedoides=pam(distanciadegower, diss = TRUE, k = 2)

clusters <- datostrabajo %>%
  dplyr::select(-Estudiante) %>%
  mutate(cluster = clusterskmedoides$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

# Observamos los cl?sters

table(clusterskmedoides$clustering) # 145 observaciones en el primer cl?ster y 97 en el segundo

clusters$the_summary

# Vemos que los individuos 34 y 118 son los medoides de cada cl?ster

datostrabajo[clusterskmedoides$medoids, ]

# VISUALIZACI?N

# Usamos el m?todo SNE (Stochastic Neighbor Embedding), que es una forma
# de proyecci?n de los datos sobre un espacio de dimensi?n inferior, en este caso R^2

tsne=Rtsne(distanciadegower, is_distance = TRUE)
tsnedatos=tsne$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(clusterskmedoides$clustering),
         Estudiante = datostrabajo$Estudiante)

# Visualizamos ambos cl?sters

ggplot(aes(x = X, y = Y), data = tsnedatos) +geom_point(aes(color = cluster),size=2)

# AN?LISIS DISCRIMINANTE

# Agregamos una nueva columna a nuestra matriz de datos con el cl?ster al cu?l la observaci?n fue asignada (ser? la variable a explicar)
vcluster=data.frame('Cluster'=clusterskmedoides$clustering)-1
datostrabajodiscr=cbind(datostrabajo,vcluster)

# Ajustamos modelo con todas las variables (Estoy usando el script_Logit_bin_multi del EVA)

modelo<-glm(Cluster~.,data=datostrabajodiscr,family = binomial(link="logit"))
summary(modelo)

mod_data <- cbind(tsnedatos[,1:2],vcluster-1)
mod <- glm(Cluster~., data=mod_data,family=binomial(link="logit"))
summary(mod)

data_Y <- cbind(datostrabajo,mod_data$Y)
summary(lm(mod_data$Y~.,data=data_Y))


# Ac? tira un error, probe ponerle 0 al cluster 1 y 1 al cluster 2 pero no, da un problema de convergencia)




