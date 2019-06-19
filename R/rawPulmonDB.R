#library(tidyr)
#library(dbConnect)
#library(SummarizedExperiment)

# This gives you homogenized values

rawPulmonDB = function(id){

  mydb = dbConnect(MySQL(),
                   user="guest",
                   password="",
                   dbname="expdata_hsapi_ipf",
                   host="10.200.0.42")

  sql = 'select p.probe_set_name,r.value, a.array_access_id, gn.gene_name \
  from raw_data as r \
  INNER JOIN hybridization AS h ON r.hybridization_fk= h.hybridization_id \
  INNER JOIN array AS a ON h.array_fk= a.array_id \
  INNER JOIN experiment AS e ON a.experiment_fk = e.experiment_id \
  INNER JOIN probe AS p ON r.probe_fk= p.probe_id\
  INNER JOIN gene_name AS gn ON p.gene_fk=gn.gene_fk
  WHERE e.experiment_access_id IN ("'
  #AND p.probe_name IN ("TSPAN6")'

 # sql= 'select url from experiment WHERE experiment_access_id IN ("GSE1122")'


  finalsql=paste(sql,
                 paste(id,collapse='","'),'") limit 10',
                 sep = ""
  )

  print("Downloading raw data from PulmonDB..... ")
  rs = dbSendQuery(mydb,finalsql)
  data = fetch(rs, n=-1)
  dbDisconnect(mydb)

}
  contrast_name_vs_gene_name = data
  data = contrast_name_vs_gene_name
  #contrast_name_vs_gene_name[1] <- NULL
  #View(data)
  contrast_name_vs_gene_name = contrast_name_vs_gene_name %>% spread(contrast_name,value)
  #tidyr::spread(df, contrast_name, value)
  rownames(contrast_name_vs_gene_name) = contrast_name_vs_gene_name$gene_name
  contrast_name_vs_gene_name = contrast_name_vs_gene_name[,-1]
  data_class <- SummarizedExperiment(assays=list(values=as.matrix(contrast_name_vs_gene_name)))
  return(data_class)
}

