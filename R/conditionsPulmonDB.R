
conditionsPulmonDB = function(cond = 'all'){
  #message("Connecting to PulmonDB")
  a <- Sys.time()

  mydb = dbConnect(MySQL(),
                   user="guest",
                   password="pulmonDBguest",
                   dbname="expdata_hsapi_ipf",
                   host="132.248.248.114")



  sql = "SELECT condition_node_name,
        cond_property_id,
  condition_node_description
  FROM condition_definition"

  rs = suppressWarnings(dbSendQuery(mydb,sql))
  data = fetch(rs, n=-1)
  #message("Data downloaded...")

  data <- data[!duplicated(data[,1]),]
  suppressWarnings(dbDisconnect(mydb))

  if (cond == 'all'){
    return(data)
    } else {
    data <-  data[grepl(toupper(cond),data[,1]),]
    return(data)
  }

}


