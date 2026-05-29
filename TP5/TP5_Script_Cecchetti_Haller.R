library(tidyverse)


# Aproximación por diferencias finitas ------------------------------------

K1<-0.025; K2<-0.25; K3<-1; K4<-7.5E-6; K5<-1000; K6<-1.99
tem0<-20; gen0<-200; struct0<-10000; t0<-0; dt<-0.05; t=t0
tem_list<-c(tem0); gen_list<-c(gen0); struct_list<-c(struct0); t_list<-c(t0); v_list<-c(0)


tem<-tem0; gen<-gen0; struct<-struct0; t<-t0

for(i in 1:5000){
  dtem=(K1*gen-K2*tem)
  tem=tem+dtem
  tem_list<-c(tem_list,tem)
  
  dgen=(K3*tem-K1*gen-K4*gen*struct)
  gen=gen+dgen
  gen_list<-c(gen_list,gen)
  
  dstruct=(K5*tem-K6*struct-K4*gen*struct)
  struct=struct+dstruct
  struct_list<-c(struct_list,struct)
  
  t=t+dt
  t_list<-c(t_list,t)
}

data_diferencias_finitas<-data.frame(t_list, tem_list, gen_list, struct_list)
View(data_diferencias_finitas)

plot_tem<-ggplot(data = data_diferencias_finitas,
       aes(x=t_list,
           y=tem_list))+
  geom_point(size=1,
             color="darkgreen")
plot_tem

plot_gen<-ggplot(data = data_diferencias_finitas,
                 aes(x=t_list,
                     y=gen_list))+
  geom_point(size=1,
             color="purple")
plot_gen

plot_struct<-ggplot(data = data_diferencias_finitas,
                    aes(x=t_list,
                        y=struct_list))+
  geom_point(size=1,
             color="orange")
plot_struct

# Estocastica -------------------------------------------------------------

K1<-0.025; K2<-0.25; K3<-1; K4<-7.5E-6; K5<-1000; K6<-1.99
#Los valores de K esan aqui de nuevo para que cada seccion pueda ser ejecutada independientemente.


t_max<-200
X_t0=c(0,1,0)

#Change vectors
v_1<-c(-1,1,0)
v_2<-c(0,-1,0)
v_3<-c(1,0,0)
v_4<-c(-1,0,-1)
v_5<-c(0,0,1)
v_6<-c(0,0,-1)
reactions<-list(v_1,v_2,v_3,v_4,v_5,v_6)

#dgen=c(-1, 0, 1, -1, 0, 0);dtem=c(1, -1, 0, 0, 0, 0);dstruct=c(0, 0, 0, -1, 1, -1)


save_interval<-20

t<-0; iteration<-1

data_stochastic<-data.frame(t,X_t0[1],X_t0[2],X_t0[3],iteration)


while(t<=t_max){
  
  a<-c(K1*X_t0[1],K2*X_t0[2],K3*X_t0[2],K4*X_t0[1]*X_t0[3],K5*X_t0[2],K6*X_t0[3])
  a_0=sum(a)
  

  
  tau=(1/a_0)*log(1/runif(1))
  t=t+tau
  
 X_t0=X_t0+reactions[[
   which(
     cumsum(a)>=runif(1)*a_0)[1]
   ]]
 iteration<-iteration+1
 data_iteration<-data.frame(t,X_t0[1],X_t0[2],X_t0[3],iteration)

 if(iteration %% save_interval==0){ 
 data_stochastic<-rbind(data_stochastic,data_iteration)
  print(t)}
  
}

ggplot(data_stochastic, aes(y=X_t0.3.,
                            x=t))+
  geom_point(size=.5, color="violet")

ggplot(data_stochastic, aes(y=X_t0.1.,
                            x=t))+
  geom_point(size=.5, color="green")

ggplot(data_stochastic, aes(y=X_t0.2.,
                            x=t))+
  geom_point(size=.5, color="orange")