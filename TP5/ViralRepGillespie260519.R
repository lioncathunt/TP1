library(tidyverse)
library(pbapply)
library(dplyr)


# Finite differences approximation approach -------------------------------

K1<-0.025; K2<-0.25; K3<-1; K4<-7.5E-6; K5<-1000; K6<-1.99
tem0<-5; gen0<-0; struct0<-0; t0<-0; dt<-0.05; t=t0
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


# Plots finite differences ------------------------------------------------


plot_tem<-ggplot(data = data_diferencias_finitas,
                 aes(x=t_list,
                     y=tem_list))+
  geom_point(size=1,
             color="darkgreen")+
  labs(title = "Evolution of the ammount of TEM molecules after Infection",
       subtitle = "With a deterministic approach",
       x="Time(days after infection)",
       y="TEM molecules")
plot_tem

plot_gen<-ggplot(data = data_diferencias_finitas,
                 aes(x=t_list,
                     y=gen_list))+
  geom_point(size=1,
             color="purple")+
  labs(title = "Evolution of the ammount of GEN molecules after Infection",
       subtitle = "With a deterministic approach",
       x="Time(days after infection)",
       y="GEN molecules")
plot_gen

plot_struct<-ggplot(data = data_diferencias_finitas,
                    aes(x=t_list,
                        y=struct_list))+
  geom_point(size=1,
             color="orange")+
  labs(title = "Evolution of the ammount of STRUCT molecules after Infection",
       subtitle = "With a deterministic approach",
       x="Time(days after infection)",
       y="STRUCT molecules")
plot_struct



# One Loop ----------------------------------------------------------------


t_max = 200

GEN_change = c(-1,0,1,-1,0,0)
TEM_change = c(1,-1,0,0,0,0)
STRUCT_change = c(0,0,0,-1,1,-1)

k1 = 0.025; k2 = 0.25; k3 = 1; k4 = 7.5e-6; k5 = 1000; k6 = 1.99 #day-1 (molecule-1)

t <- 0; iteration <- 1

GEN <- 1; TEM <- 1; STRUCT <- 1

first_row <- data.frame(time = t, GEN = GEN, TEM = TEM, STRUCT = STRUCT)

save_interval = 20

output_list <- list()

output_list[[1]] <- first_row

save <- 1

while (t < t_max) {
  iteration = iteration + 1
  propensities = c(k1*GEN, k2*TEM, k3*TEM, k4*GEN*STRUCT, k5*TEM, k6*STRUCT)
  p0 = sum(propensities)
  if (p0 == 0 || is.na(p0)) {
    new_row <- data.frame(time = t, GEN = GEN, TEM = TEM, STRUCT = STRUCT)
    save <- save + 1
    output_list[[save]] <- new_row
    break 
  }
  tau = (1/p0) * log(1/runif(1))
  index <- which(cumsum(propensities) > (runif(1) * p0))[1]
  t = t + tau
  if (t > t_max) break
  GEN    = GEN    + GEN_change[index]
  TEM    = TEM    + TEM_change[index]
  STRUCT = STRUCT + STRUCT_change[index]
  if (iteration %% save_interval == 0) {
    new_row <- data.frame(time = t, GEN = GEN, TEM = TEM, STRUCT = STRUCT)
    save <- save + 1
    output_list[[save]] <- new_row
    print(t)
  }
}


ssa_output <- do.call(rbind, output_list)
filename <- paste0("ssa_result_run_587", ".csv")
write.csv(ssa_output, file = filename, row.names = FALSE)

plot(ssa_output$time,ssa_output$STRUCT,type="l")









plot.g <- ggplot(data=ssa_output, aes(x=time, y=TEM))+
  geom_line()+
  theme_bw()

plot.g



ggplotly(plot.g)


# Many loops --------------------------------------------------------------
run_ssa <- function(run_id) {
  t_max = 200
  GEN_change = c(-1,0,1,-1,0,0)
  TEM_change = c(1,-1,0,0,0,0)
  STRUCT_change = c(0,0,0,-1,1,-1)
  k1 = 0.025; k2 = 0.25; k3 = 1; k4 = 7.5e-6; k5 = 1000; k6 = 1.99 #day-1 (molecule-1)
  t <- 0; iteration <- 1
  GEN <- 1; TEM <- 1; STRUCT <- 1
  first_row <- data.frame(time = t, GEN = GEN, TEM = TEM, STRUCT = STRUCT)
  save_interval = 2000
  output_list <- list()
  output_list[[1]] <- first_row
  save <- 1
  while (t < t_max) {
    iteration = iteration + 1
    propensities = c(k1*GEN, k2*TEM, k3*TEM, k4*GEN*STRUCT, k5*TEM, k6*STRUCT)
    p0 = sum(propensities)
    if (p0 == 0 || is.na(p0)) {
      new_row <- data.frame(time = t, GEN = GEN, TEM = TEM, STRUCT = STRUCT)
      save <- save + 1
      output_list[[save]] <- new_row
      break 
    }
    tau = (1/p0) * log(1/runif(1))
    index <- which(cumsum(propensities) > (runif(1) * p0))[1]
    t = t + tau
    if (t > t_max) break
    GEN    = GEN    + GEN_change[index]
    TEM    = TEM    + TEM_change[index]
    STRUCT = STRUCT + STRUCT_change[index]
    if (iteration %% save_interval == 0) {
      new_row <- data.frame(time = t, GEN = GEN, TEM = TEM, STRUCT = STRUCT)
      save <- save + 1
      output_list[[save]] <- new_row
      #print(t)
    }
  }
  ssa_output <- do.call(rbind, output_list)
  filename <- paste0("ssa_result_run_", run_id, ".csv")
  write.csv(ssa_output, file = filename, row.names = FALSE)
  return(ssa_output)
}


all_runs <- pblapply(1:1202, function(i) {
  run_ssa(run_id = i)
})

all_runs <- pblapply(1:1202, function(i) {
  filename <- paste0("ssa_result_run_", i, ".csv")
  
  # Check if the file exists before trying to read it
  if (file.exists(file.path("Runs files", filename))) {
    return(read.csv(file.path("Runs files", filename)))
  } else {
    warning(paste("File", filename, "not found."))
    return(NULL)
  }
})

common_time <- seq(0, t_max, length.out = 2001)

interpolate_run <- function(df) {
  if (!is.data.frame(df)) {
    warning("Found a non-dataframe element; skipping.")
    return(NULL)
  }
  
  data.frame(
    time = common_time,
    GEN = approx(df$time, df$GEN, xout = common_time, method = "constant", rule = 2)$y,
    TEM = approx(df$time, df$TEM, xout = common_time, method = "constant", rule = 2)$y,
    STRUCT = approx(df$time, df$STRUCT, xout = common_time, method = "constant", rule = 2)$y
  )
}


interpolated_list <- lapply(all_runs, interpolate_run)


combined_data <- do.call(rbind, interpolated_list)



average_results <- combined_data %>%
  group_by(time) %>%
  summarise(
    mean_GEN = mean(GEN),
    mean_TEM = mean(TEM),
    mean_STRUCT = mean(STRUCT),
    sd_GEN = sd(GEN) # Optional: track variability
  )

# Stochastic Plots --------------------------------------------------------


Plot_mean_TEM<-ggplot(data=average_results,
                      aes(x=time,
                          y=mean_TEM))+
  geom_point(size=.1, colour="darkgreen")+
  labs(title = "Evolution of the ammount of TEM molecules after Infection",
       subtitle = "Using an stochastic algorithm",
       x="Time(days after infection)",
       y="Average ammount of TEM molecules")
Plot_mean_TEM

Plot_mean_GEN<-ggplot(data=average_results,
                      aes(x=time,
                          y=mean_GEN))+
  geom_point(size=.1, colour="purple")+
  labs(title = "Evolution of the ammount of GEN molecules after Infection",
       subtitle = "Using an stochastic algorithm",
       x="Time(days after infection)",
       y="Average ammount of GEN molecules")
Plot_mean_GEN

Plot_mean_STRUCT<-ggplot(data=average_results,
                      aes(x=time,
                          y=mean_STRUCT))+
  geom_point(size=.1, colour="orange")+
  labs(title = "Evolution of the ammount of STRUCT molecules after Infection",
       subtitle = "Using an stochastic algorithm",
       x="Time(days after infection)",
       y="Average ammount of STRUCT molecules")
Plot_mean_STRUCT


# Cuestionario ------------------------------------------------------------

# 4)
# Las simulaciones deterministicas no pueden simular adecuadamente la infeccion viral ya que la cantidad de moleculas involucradas en la infeccion
# es pequeña. En los graficos observamos como las cantidades varian hasta llegar a un punto donde la variacion es nula y la infeccion queda "estancada"
# En las approximaciones estocasticas vemos como la naturaleza aleatoria del algoritmo proporcionan resultados imposibles de replicar con un algoritmo determinista.
#La cantidad de moleculas hace que para aproximar la evolucion del sistema sea necesario considerar la probabilidad de que ocurra cada reaccion y "sortear" cual de ellas 
#Ocurre en una cantidad aleatoria de tiempo.

# 5)
#La no linealidad viene dada por el termino asociado a K4. Como se explica en el apendie del paper asociado a este TP luego de un analisis de stabilidad lineal
#se encuentra que uno de los autovalores para el estado estacionario trivial (GEN=STRUCT=TEM=0) es positivo, esto implica que cualquier perturbación al estado
#hace que el sistema se aleje de este. Lo que a su vez hace que una infeccion sea posible. El modelo estocastico puede predecir infecciones fallidas debido
#a su naturaleza azarosa, el hecho de que en una fraccion aleatoria de tiempo pueda ocurrir una reaccion aleatoria lleva a la posibilidad de que ocurran
#series de reacciones que degraden todo el TEM y GEN de modo que la replicacion de estos no pueda continuar entonces resulte en una infeccion fallida. 