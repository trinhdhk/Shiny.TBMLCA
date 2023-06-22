misc = new.env()
source('R/misc.R', local=misc)
# misc$imp <- load('fits/imp/imp_model.RDS')
imp_model = readRDS('fits/imp/imp_model.RDS')
# `%*%` = Rfast::mat.mult
