#TAREA EJERCICIO 3.3

mult<-65
incr<-1
modu<-2048

#FUNCIONES ----

mcd <- function(x, y) {
  while(y) {
    temp = y
    y = x %% y
    x = temp
  }
  return(x)
}

initRANDC <- function(semilla=as.numeric(Sys.time()), a=2^16+3, c=0, m=2^31) {
  .semilla <<- as.double(semilla) %% m  #Calculos en doble precision
  .a <<- a
  .c <<- c
  .m <<- m
  return(invisible(list(semilla=.semilla,a=.a,c=.c,m=.m))) #print(initRANDC())
}

RANDC <- function() {
  if (!exists(".semilla", envir=globalenv())) initRANDC()
  .semilla <<- (.a * .semilla + .c) %% .m
  return(.semilla/.m)
}

RANDCN <- function(n=1000) {
  x <- numeric(n)
  for(i in 1:n) x[i]<-RANDC()
  return(x)
  # return(replicate(n,RANDC()))  # Alternativa mas rapida    
}


#LITERAL a. ----

library(numbers)
PeriodoMax<-function(mult,incr,modu){
val=0
if(mcd(incr,modu)==1 && modu%%4==0 ){
  if(mult%%4){
    fac.primo<-primeFactors(modu)
    for(i in 1:length(fac.primo)){
      if(mult%%fac.primo[i]==1){
        val=1
      } 
    }
  } 
} 
return(val) 
}

if(PeriodoMax(mult,incr,modu)==1){
  print("Es de ciclo maximo")
  } else {
  print("No es de ciclo maximo")
}



#LITERAL b.----

N.GRUPO<-1    #Se tomo el numero de grupo = 1
seed<-N.GRUPO*100
initRANDC(seed,mult,incr,modu)

nsim <- 1000  
system.time(u <- RANDCN(nsim)) 

hist(u, freq = FALSE)
abline(h = 1) 

ks.test(u,"punif",0,1)



#LITERAL c.----

x<-u[1:length(u)-1]
y<-u[2:length(u)]
z<-data.frame(x,y)
View(z)
plot(x,y,type="p")
plot(as.ts(u))
plot(u[-nsim],u[-1])
"Al parecer nuestro algoritmo genera numeros pseudoaleatorios sin ningun problema y se sospecha que en efecto, tenemos independencia y se encuentran identicamente distribuidos"
acf(u)
"Se identifica independencia"



#LITERAL d.----

tamano <- 50
pruebas <- 500 
estadistico <- numeric(pruebas)
pvalor <- numeric(pruebas)

# Realizar contrastes
for(isim in 1:pruebas) {
  v <- RANDCN(tamano)   
  tmp <- Box.test(v, lag = 10, type = "Ljung")
  estadistico[isim] <- tmp$statistic
  pvalor[isim] <- tmp$p.value
}
cat("Proporcion de rechazos al 5% =", mean(pvalor < 0.05), "\n")
hist(estadistico, breaks = "FD", freq=FALSE)
