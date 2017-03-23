writeleyte <- function(){
  library(RMySQL)
  leyte <- dbConnect(MySQL(), dbname="Leyte", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
}
