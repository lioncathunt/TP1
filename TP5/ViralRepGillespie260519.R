library(tidyverse)
library(pbapply)
library(dplyr)

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
filename <- paste0("ssa_result_run_test", ".csv")
write.csv(ssa_output, file = filename, row.names = FALSE)

plot(ssa_output$time,ssa_output$STRUCT,type="l")









plot.g <- ggplot(data=ssa_output, aes(x=time, y=TEM))+
  geom_line()+
  theme_bw()

plot.g



ggplotly(plot.g)

#############################################################################
##############################MUCHOS LOOPS ##################################
############################################################################




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

n_runs <- 3
all_runs <- pblapply(500:503, function(i) {
  run_ssa(run_id = i)
})

all_runs <- pblapply(1:401, function(i) {
  filename <- paste0("ssa_result_run_", i, ".csv")
  
  # Check if the file exists before trying to read it
  if (file.exists(filename)) {
    return(read.csv(filename))
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


plot(average_results$time,average_results$mean_TEM,type="l")
