# source code for nowacki-beam problem
rm(list = ls())
library(suropt)

save_dir <- 'C:/Dropbox/temp/'

## objective/constraint function definition
fun <- nowacki_beam


## main loop
n0 = 15 #number of initial designs
n1 = 30 #number of infill designs
d = 2   #dimension of design space

N <- 1:25 #run each proccess indenpendently 25 times and save all models oin tempdir
for (i in N){
  model_doe  <- build_surmodel(fun, n0, d, 'olhs')
  model.SME  <- train_sme( model_doe, n1, optimizer = 'nsga2')
  model.MEGO <- train_mego(model_doe, n1, optimizer = 'sa')
  model.HEGO <- train_hego(model_doe, n1, optimizer = 'sa')
  save(model.SME, model.MEGO, model.HEGO, file = paste0(save_dir,'nb_run_',i))
}
