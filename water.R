# source code for water-management problem
rm(list = ls())
library(suropt)

save_dir <- 'C:/Dropbox/temp/'

## objective/constraint function definition
water <- function(x){
  if(!is.matrix(x))
    x <- t(as.matrix(x))
  if(ncol(x) != 3)
    stop('x must be a vector of length 3 or a matrix with 3 columns')
  
  x1 <- x[,1]
  x2 <- x[,2]
  x3 <- x[,3]

  f1 <- 106780.37 * (x2 + x3) + 61704.67
  f2 <- 3000 * x1
  f3 <- 30570 * 0.02289 * x2 / (0.06 * 2289)^0.65
  f4 <- 250 * 2289 * exp(-39.75 * x2 + 9.9 * x3 + 2.74)
  f5 <- 25 * (1.39 / (x1 * x2)) + 4940 * x3 - 80
  
  g1 <- 0.00139 / (x1 * x2) + 4.94 * x3 - 0.08 - 1
  g2 <- 0.000306 / (x1 * x2) + 1.082 * x3 - 0.0986 - 1
  g3 <- 12.307 / (x1 * x2) + 49408.24 * x3 + 4051.02 - 50000
  g4 <- 2.098 / (x1 * x2) + 8046.33 * x3 - 696.71 - 16000
  g5 <- 2.138 / (x1 * x2) + 7883.36 * x3 - 705.04 - 10000
  g6 <- 0.417 * (x1 * x2) + 1721.26 * x3 -136.54 - 2000
  g7 <- 0.164 / (x1 * x2) + 631.13 * x3 - 54.48 - 550

  list(y = c(f1, f2, f3, f4, f5), g = c(g1, g2, g3, g4, g5, g6, g7))
}
## inputs of objective/constraint function are scaled so they fit in a hypercube of size 1
fun <- function(x){
  
  lower <- c(0.01,0.01,0.01)
  upper <- c(0.45,0.10,0.10)
  x <- x * (upper - lower) + lower
  
  water(x)
}

## main loop

n0 = 40  #number of initial designs
n1 = 120 #number of infill designs
d = 3    #dimension of design space

N <- 1:25 #run each proccess indenpendently 25 times and save all models oin tempdir
for (i in N){
  model_doe  <- build_surmodel(fun, n0, d, 'olhs')
  model.SME  <- train_sme( model_doe, n1, optimizer = 'nsga2')
  model.MEGO <- train_mego(model_doe, n1, optimizer = 'sa')
  model.HEGO <- train_hego(model_doe, n1, optimizer = 'sa')
  save(model.SME, model.MEGO, model.HEGO, file = paste0(save_dir,'water_run_',i))
}
