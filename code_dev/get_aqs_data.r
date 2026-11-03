get.aqs.data <- function(sql,dbname,userid,passwd) {
  if (missing(dbname)) { dbname <- "aqsprod.ncceastdb02" }
  if (missing(userid)) { userid <- Sys.getenv("AQS_userid") }
  if (missing(passwd)) { passwd <- Sys.getenv("AQS_passwd") }
  require(ROracle)
  driver <- dbDriver("Oracle")
  con <- dbConnect(driver,username=userid,password=passwd,dbname=dbname)
  aqs.data <- dbGetQuery(con,sql)
  colnames(aqs.data) <- tolower(colnames(aqs.data))
  dbDisconnect(con)
  return(aqs.data)
}