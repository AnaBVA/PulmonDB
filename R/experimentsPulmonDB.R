experimentsPulmonDB <- function(){

  mydb = dbConnect(MySQL(),
                   user="guest",
                   password="pulmonDBguest",
                   dbname="expdata_hsapi_ipf",
                   host="132.248.248.114")


  sql= "select * from experiment
where normdata_import_time IS NOT NULL
;"

  rs = dbSendQuery(mydb,sql)
  data = fetch(rs, n=-1)
  dbDisconnect(mydb)
  return(data)
}
