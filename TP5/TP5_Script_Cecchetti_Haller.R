library(tidyverse)


t_max<-200
X_t0=c(200,20,2000)

#v_1=c(-1,1,0);v_2=c(0,-1,0);v_3=c(1,0,0);v_4=c(-1,0,-1);v_5=c(0,0,1);v_6=c(0,0,-1)


dgen=c(-1, 0, 1, -1, 0, 0)
dtem=c(1, -1, 0, 0, 0, 0)
dstruct=c(0, 0, 0, -1, 1, -1)

#agregar Ks


t<-0; iteration<-1

