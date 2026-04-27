library(tidyverse)


# Actividad 1: Cinéticas sencillas ----------------------------------------

#a)Escriba un código de R que permita calcular una lista de valores de [R], para diferentes tiempos (guardados también en una lista). Use k = 1, dt = 0.001 y [R]_0 = 100.


#b)¿Cómo se puede escribir una variable extra que exprese la velocidad, v, a cada tiempo t, para la reacción de primer orden? Incluya dicha variable en el código.

#Represente gráficamente [R] vs t y ln [R] vs t.


#c)Represente gráficamente v vs [R].


#d)Repita el procedimiento, pero para una reacción de orden 2 y grafique v vs [R]^2 y 1/[R] vs t.

#e)Compare los gráficos que se obtienen cambiando "dt" a 0.01 y 0.1. ¿Qué ocurre si aumenta "dt" a un valor relativamente grande, por ejemplo, 2?

#al aumentar dt de 0.001 a 0.01 el intervalo de tiempo para el que calculamos las concentraciones de R pasa de 5 segundos a 50 segundos. En los primeros 5 se observa lo mismo que en
#el grafico con dt=0.001 un decaimiento constante de la concentracion de R, sin embargo luego la concentracion de R disminuye hasta 0 y se mantiene constante. lo mismo ocurre con 
#dt=0.1 solo que analizando los primeros 500 segundos de reaccion.

#cuando dt es un valor demasiado grande como 2 la ecuacion que calcula la concentracion de R a partir del tiempo transcurrido deja de tener sentido ya que solo funciona para diferencias peque;as de tiempo 
#si dt es mayor que 1 la diferencia de R sera mayor que R misma y negativa lo cual lleva a que cada iteracion de R sea el mismo valor alternando entre negativo y positivo.


k<-(1)
dt<-0.001
R0<-100
t0<-0
v0=k*R0

R_list<-c(R0)
t_list<-c(t0)
v_list<-c(v0)
  
R=R0
t=t0


for (i in 1:5000){
dR<- -k*R*dt
R<-R+dR
R_list<-c(R_list,R)
t<-t+dt
t_list<-c(t_list,t)
v=k*R
v_list<-c(v_list,v)
}

view(R_list)

data_list_TP2<-data.frame(R_list, t_list, v_list)

ggplot(data=data_list_TP2,
       aes(x=t_list, 
           y=R_list))+
      geom_point(size=1, color="purple")+
theme_bw()

data_list_TP2<-data_list_TP2 %>% 
  mutate(lnR_list=log(R_list))
view(data_list_TP2)


ggplot(data=data_list_TP2,
       aes(x=t_list, 
           y=lnR_list))+
  geom_point(size=1, color="purple")+
  theme_bw()


ggplot(data=data_list_TP2,
       aes(x=R_list, 
           y=v_list))+
  geom_point(size=1, color="purple")+
  theme_bw()

#Calculo para una reaccion de orden 2 vamos a definir las variables con un 1c ya q ue corresponde a la actividad 1c

k_1c<-(1)
dt_1c<-0.001
R0_1c<-100
t0_1c<-0
v0_1c=k_1c*R0_1c^2

R_list_1c<-c(R0_1c)
t_list_1c<-c(t0_1c)
v_list_1c<-c(v0_1c)

R_1c=R0_1c
t_1c=t0_1c


for (i in 1:5000){
  dR_1c<- -2*k_1c*(R_1c^2)*dt_1c
  R_1c<-R_1c+dR_1c
  R_list_1c<-c(R_list_1c,R_1c)
  t_1c<-t_1c+dt_1c
  t_list_1c<-c(t_list_1c,t_1c)
  v_1c=k_1c*R_1c^2
  v_list_1c<-c(v_list_1c,v_1c)
}

data_list_TP2_act_1c<-data.frame(R_list_1c, t_list_1c, v_list_1c,
                                 R_list_squared=R_list_1c^2,
                                 R_list_inverse=1/R_list_1c)

view(data_list_TP2_act_1c)

ggplot(data = data_list_TP2_act_1c,
       aes(x=R_list_1c,
           y=v_list_1c))+
  geom_point(size=1, color="palevioletred2")+
  theme_bw()

ggplot(data = data_list_TP2_act_1c,
       aes(x=R_list_squared,
           y=v_list_1c))+
  geom_point(size=1, color="coral2")+
  theme_bw()
         
ggplot(data = data_list_TP2_act_1c,
       aes(x=t_list_1c,
           y=R_list_inverse))+
  geom_point(size=1, color="darkolivegreen3")+
  theme_bw()

#Compare los graficos que se obtienen cambiando “dt” a 0.01 y 0.1. ¿Que ocurre
#si aumenta “dt” a un valor relativamente grande, por ejemplo, 2?

#Probar con dt=2




# Actividad 2: Cinética enzimática ----------------------------------------

#a) Cree un \textit{script} de R en el que se calcule, para una reacción enzimática, las variables: tiempo, $[S]$, $[E]$, $[ES]$, $[P]$ y $\frac{d[P]}{dt}$. Utilice las siguientes condiciones iniciales: $[E]_0 = 1$; $[S]_0 = 10000$; $dt = 0.00005$; $k_f = 1$; $k_r = 1$; $k_p = 0.1$. ¿Qué ocurre si fija $dt = 0.0002$?
  
  
#b) Grafique $[P]$ vs. $t$. ¿Cuánto tiempo deberá pasar para que la concentración de sustrato sea el $95\%$ de la concentración inicial? ¿Cuánto es dicho tiempo si $k_p = 10$?
  
#calculado en el script  
  
#c) Grafique $[S]$ vs. $T$ ($k_p = 0.1$). ¿A qué se debe que haya una disminución inicial rápida de $S$ y luego un decrecimiento más lento?

#la disminucion inicial rapida de S ocurre ya que el sustrato se une rapidamente con la enzima, una vez que esta se satura la disminucion de S ocurre tan rapido como la transproimacion de ES
#a E+P con una Kp=10 esta ultima transformacion ocurre mas rapido por lo que la disminucion de S luego de la saturacion es mas veloz.

E0=1
S0=1000
dt=0.00005
kf=1
kr=1
kp=10
t0=0

E_list<-c(E0)
S_list<-c(S0)
ES_list<-c(0)
P_list<-c(0)
v_list<-c(0)
t_list<-c(t0)

E=E0
S=S0
P=0
ES=0
v=0
t=t0

for (i in 1:5000){
  dE=(-kf*E*S+kf*ES+kp*ES)*dt
  E=E+dE
  E_list<-c(E_list,E)
  
  dS=(-kf*E*S+kr*ES)*dt
  S=S+dS
  S_list<-c(S_list, S)
  
  dES=(kf*E*S-kr*ES-kp*ES)*dt
  ES=ES+dES
  ES_list<-c(ES_list, ES)
  
  dP=kp*ES*dt
  P=P+dP
  P_list<-c(P_list, P)
  
  v=kp*ES
  v_list<-c(v_list,v)
  
  t=t+dt
  t_list<-c(t_list, t)
  
}

data_list_TP2_A2<-data.frame(E_list, S_list, ES_list, P_list, t_list, v_list)
view(data_list_TP2_A2)

linear_model_of_S_vs_t<-lm(data_list_TP2_A2$S_list~data_list_TP2_A2$t_list,
                           data = data_list_TP2_A2,
                           subset = t_list>0.05)

summary(linear_model_of_S_vs_t)

#la regresion lineal de la parte lineal es:  [S]=9.991e+02-9.490e-02t, si buscamos el valor de t para el que [S]=0.95[S_0] tenemos

cat("El tiempo al que la concentracion de sustrato es el 95% de la inicial es:", ((0.95*S0-coef(linear_model_of_S_vs_t)[1])/coef(linear_model_of_S_vs_t)[2]),"segundos"
)
#para ver como cambia esto con kp=10 se volvio a correr la seccion del script correspondiente cambiando la variable kp y el valor obtenido por la linea anterior es: 5.219329 segundos

ggplot(data=data_list_TP2_A2,
       aes(x=t_list, 
           y=P_list))+
  geom_point(size=1, color="darkgreen")+
theme_bw()

ggplot(data=data_list_TP2_A2,
       aes(x=t_list,
           y=S_list))+
  geom_point(size=1, color="magenta4")+
  theme_bw()


ggplot(data=data_list_TP2_A2,
       aes(x=t_list,
           y=ES_list))+
  geom_point(size=1, color="darkorange2")+
  theme_bw()


ggplot(data=data_list_TP2_A2,
       aes(x=t_list,
           y=v_list))+
  geom_point(size=1, color="cyan4")+
  theme_bw()


# Actividad 3: Reacciones oscilatorias ------------------------------------

#a) Cree un \textit{script} de R en el que se represente el tiempo $t$, $[A]$, $[B]$, $[D]$, $[E]$, $[X]$ y $[Y]$. Realice $10000$ cálculos
#con las siguientes condiciones iniciales: $[A]_0 = 1$; $[B]_0 = 3$; $dt = 0.005$; $k_1 = k_2 = k_3 = k_4 = 1$. Evalúe la variación de las 
#concentraciones de todas las especies en función del tiempo. ¿Oscila la concentración de alguna de las especies?

#no, bajo estas condiciones ninguna de las especies oscila.
  
#b) Mantenga las $[A]$ y $[B]$ en valores constantes (en valores $1$ y $3$, respectivamente).
#¿Qué se observa en este caso? ¿Qué condición debería tener el sistema para mantener $[A]$ y $[B]$ constantes?

#en este caso se observa como la concentracion de X y de Y oscilan. Para que la concentracion de A y de B sean constantes es necesario que la canditdad de A y B que se consumen
#se repongan a la misma velocidad.
  
#c) Grafique [X] vs. [Y]. ¿Qué característica se destaca del gráfico? ¿Cómo se modifica el gráfico si ajusta $[X]_0$ y $[Y]_0$ a $1$ y $0.5$? ¿Y si los ajusta a $2$ y $10$? ¿$2$ y $2$? ¿$1$ y $4$?

#La principal caracteristica del grafico es que es ciclico. al modificar las concentraciones iniciales cambia el punto desde el que comienza el grafico pero vemos como la proporcion 
#entre Y y X se ajusta hasta formar nuevamente el ciclo.
  
A0=1
B0=3
dt=0.005
k1=1
k2=1
k3=1
k4=1
t0=0
X0=0
Y0=0

t=t0
A=A0
B=B0
D=0
E=0
X=X0
Y=Y0

t_list<-c(t0)
A_list<-c(A0)
B_list<-c(B0)
D_list<-c(0)
E_list<-c(0)
X_list<-c(X0)
Y_list<-c(Y0)

for (i in 1:10000) {
  dA=-k1*A*dt
  #A=A+dA
  A_list<-c(A_list, A)
  
  dB=-k3*B*X*dt
 #B=B+dB 
  B_list<-c(B_list, B)
  
  dD=k3*B*X*dt
  D=D+dD
  D_list<-c(D_list, D)
  
  dE=k4*X*dt
  E=E+dE
  E_list<-c(E_list, E)
  
  dX=(k1*A+k2*(X^2)*Y-k3*B*X-k4*X)*dt
  X=X+dX
  X_list<-c(X_list, X)
  
  dY=(-k2*(X^2)*Y+k3*B*X)*dt
  Y=Y+dY
  Y_list<-c(Y_list, Y)
  
  t=t+dt
  t_list<-c(t_list, t)

}

data_list_TP2_A3<-data.frame(A_list, B_list, X_list, Y_list, D_list, E_list, t_list)
View(data_list_TP2_A3)

ggplot(data=data_list_TP2_A3,
       aes(x=t_list, 
           y=A_list))+
  geom_point(size=1, color="deepskyblue")+
  theme_bw()

ggplot(data=data_list_TP2_A3,
       aes(x=t_list,
           y=B_list))+
  geom_point(size=1, color="deeppink2")+
  theme_bw()


ggplot(data=data_list_TP2_A3,
       aes(x=t_list,
           y=D_list))+
  geom_point(size=1, color="darkorchid3")+
  theme_bw()


ggplot(data=data_list_TP2_A3,
       aes(x=t_list,
           y=E_list))+
  geom_point(size=1, color="midnightblue")+
  theme_bw()


ggplot(data=data_list_TP2_A3,
       aes(x=t_list,
           y=Y_list))+
  geom_point(size=1, color="yellowgreen")+
  theme_bw()

ggplot(data=data_list_TP2_A3,
       aes(x=t_list,
           y=X_list))+
  geom_point(size=1, color="firebrick3")+
  theme_bw()


#Gráfica X vs Y

ggplot(data=data_list_TP2_A3,
       aes(x=X_list,
           y=Y_list))+
  geom_point(size=1, color="orchid")+
  theme_bw()



