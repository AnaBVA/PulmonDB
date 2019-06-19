#library(tidyr)
#library(dbConnect)
#library(SummarizedExperiment)

# This gives you homogenized values

#' @export
normPulmonDB = function(id){

  mydb = dbConnect(MySQL(),
                   user="guest",
                   password="",
                   dbname="expdata_hsapi_ipf",
                   host="10.200.0.42")

  sql = 'select \
  n.value, gn.gene_name,sc.contrast_name \
  from norm_data AS n \
  INNER JOIN gene_name as gn ON n.gene_fk=gn.gene_fk \
  INNER JOIN value_type AS vt ON n.value_type_fk=vt.value_type_id \
  INNER JOIN sample_contrast AS sc ON n.contrast_fk=sc.contrast_id \
  INNER JOIN sample AS s ON sc.test_sample_fk=s.sample_id \
  INNER JOIN experiment AS e ON s.experiment_fk= e.experiment_id
  WHERE vt.value_type="M" AND (e.experiment_access_id IN ("'


  finalsql=paste(sql,
                 paste(id,collapse='","'),'"))',
                 sep = ""
  )


  rs = dbSendQuery(mydb,finalsql)
  data = fetch(rs, n=-1)
  dbDisconnect(mydb)
  contrast_name_vs_gene_name = data
  contrast_name_vs_gene_name = contrast_name_vs_gene_name %>% spread(contrast_name,value)
  #tidyr::spread(df, contrast_name, value)
  rownames(contrast_name_vs_gene_name) = contrast_name_vs_gene_name$gene_name
  contrast_name_vs_gene_name = contrast_name_vs_gene_name[,-1]
  data_class <- SummarizedExperiment(assays=list(values=as.matrix(contrast_name_vs_gene_name)))
  return(data_class)
}


