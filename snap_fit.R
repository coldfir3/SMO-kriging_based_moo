# source code for snap-fit problem
rm(list = ls())
library(suropt)
library(tidyverse)

# function that reads the output provided by the Ansys simulation
read.data <- function(file){
  o <- read.csv(file)
  o <- o[,-1]
  names(o) <- c(paste0('x', 1:5), c('eps', 'lambda', 'Fx', 'Fy'))
  o <- as.tibble(o)
  o <- transmute(o,
                 X.1 = (x1 - 30)/15,
                 X.2 = (x2 - 5)/5,
                 X.3 = (x3 - 5)/(x2 - 5),
                 X.4 = (x4 - 1.5)/2,
                 X.5 = (x5 - 1.5)/(x4 - 2),
                 Y.1 = eps,
                 Y.2 = -lambda,
                 G.1 = eps - 0.02,
                 G.2 = 2*abs(Fx) + 0*Fy - 25 
                 ) %>% na.omit()
  o
}

## main loop
# reads the output of the simulation
data <- read.data("C:/Dropbox/Adriano/Artigos/2018 - suropt/snap_fit_SME.csv")
# build and visualize the surrogate model
model <- build_surmodel(data)
model %>% predict() %>% plot
model@data[-(1:16),] %>% .Y %>% text(col = 'green')
model@data$is.feasible[-(1:16)]
# get the next infill point
x_star <- train_sme(model, -1) %>% t() %>% as.tibble() %>% transmute(
  x1 = V1*15 + 30,
  x2 = V2*5 + 5,
  x3 = V3*(x2 - 5) + 5,
  x4 = V4*2 + 1.5,
  x5 = V5*(x4 - 2) + 1.5
)
# writes the new infill point to the clipboard so the user can paste it to Ansys
write.table(x_star, file = 'clipboard', col.names = FALSE, row.names = FALSE, sep = '\t')

###################

model %>% predict() %>% plot(xlim = range(.Y(model@data)[,1]), ylim = range(.Y(model@data)[,2]))
points(.Y(model@data), col = 'green')

