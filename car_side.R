# source code for car-side impact problem

rm(list = ls())
library(suropt)

save_dir <- 'C:/Dropbox/temp/'

## objective/constraint function definition
car <- function(x){
  if(!is.matrix(x))
    x <- t(as.matrix(x))
  if(ncol(x) != 7)
    stop('x must be a vector of length 7 or a matrix with 7 columns')

  x1 <- x[,1]
  x2 <- x[,2]
  x3 <- x[,3]
  x4 <- x[,4]
  x5 <- x[,5]
  x6 <- x[,6]
  x7 <- x[,7]
  
  f <- 4.72 - 0.19*x2*x3 - 0.5*x4
  vmbd <- 10.58 - 0.674*x1*x2 - 0.67275*x2
  vfd <- 16.45 - 0.489*x3*x7 - 0.843*x5*x6
  
  f1 <- 1.98 + 4.9*x1 + 6.67*x2 + 6.98*x3 + 4.01*x4 + 1.78*x5 + 0.00001*x6 + 2.73*x7
  f2 <- f
  f3 <- 0.5*(vmbd + vfd)
  
  g1 <- 1.16 - 0.3717*x2*x4 - 0.0092928*x3 - 1
  g2 <- 0.261 - 0.0159*x1*x2 - 0.06486*x1 - 0.019*x2*x7 + 0.0144*x3*x5 + 0.0154464*x6 - 0.32
  g3 <- 0.214 + 0.00817*x5 - 0.045195*x1 - 0.0135168*x1 + 0.03099*x2*x6 - 0.018*x2*x7 + 0.007176*x3 + 0.023232*x3 - 0.00364*x5*x6 - 0.018*x2^2 - 0.32
  g4 <- 0.74 - 0.61*x2 - 0.031296*x3 - 0.031872*x7 + 0.227*x2^2 - 0.32
  g5 <- 28.98 + 3.818*x3 - 4.2*x1*x2 + 1.27296*x6 - 2.68065*x7 - 32
  g6 <- 33.86 + 2.95*x3 - 5.057*x1*x2 - 3.795*x2 - 3.4431*x7 + 1.45728 - 32
  g7 <- 46.36 - 9.9*x2 - 4.4505*x1 - 32
  g8 <- f - 4
  g9 <- vmbd - 9.9
  g10 <- vfd - 15.7
  
  list(y = c(f1, f2, f3), g = c(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10))
}
## inputs of objective/constraint function are scaled so they fit in a hypercube of size 1
fun <- function(x){

  lower <- c(0.5,0.45,0.5,0.5,0.875,0.4,0.4)
  upper <- c(1.5,1.35,1.5,1.5,2.625,1.2,1.2)
  x <- x * (upper - lower) + lower
  
  car(x)
}


## main loop

n0 = 20 #number of initial designs
n1 = 60 #number of infill designs
d = 7 #dimension of design space

N <- 1:25 #run each proccess indenpendently 25 times and save all models oin tempdir
for (i in N){
  model_doe  <- build_surmodel(fun, n0, d, 'olhs')
  model.SME  <- train_sme( model_doe, n1, optimizer = 'nsga2')
  model.MEGO <- train_mego(model_doe, n1, optimizer = 'sa')
  model.HEGO <- train_hego(model_doe, n1, optimizer = 'sa')
  save(model.SME, model.MEGO, model.HEGO, file = paste0(save_dir,'car_run_',i))
}
