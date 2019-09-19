
samplesPulmonDB = function(description,gene){

  a <- Sys.time()

  mydb = dbConnect(MySQL(),
                   user="guest",
                   password="pulmonDBguest",
                   dbname="expdata_hsapi_ipf",
                   host="132.248.248.114")

  message("Connecting to PulmonDB")


  d <- conditionsPulmonDB(description)

  suppressWarnings(if (gene == "all"){

    sql = "Select n.value, gn.gene_name,sc.contrast_name  from
 condition_specification_of_ref_sample as csr
    INNER JOIN sample_contrast as sc on sc.ref_sample_fk = csr.sample_fk
    INNER JOIN norm_data as n on n.contrast_fk = sc.contrast_id
    INNER JOIN gene_name as gn ON n.gene_fk=gn.gene_fk
    INNER JOIN value_type AS vt ON n.value_type_fk=vt.value_type_id
    where vt.value_type='M' AND cond_property_fk = "


    finalsql=paste(sql,
                   d$cond_property_id,
                   sep = "")

  } else {

    sql = "Select n.value, gn.gene_name,sc.contrast_name  from
 condition_specification_of_ref_sample as csr
 INNER JOIN sample_contrast as sc on sc.ref_sample_fk = csr.sample_fk
 INNER JOIN norm_data as n on n.contrast_fk = sc.contrast_id
 INNER JOIN gene_name as gn ON n.gene_fk=gn.gene_fk
 INNER JOIN value_type AS vt ON n.value_type_fk=vt.value_type_id
 where vt.value_type='M' AND cond_property_fk = "


 finalsql=paste(sql,
                d$cond_property_id,
                " AND gn.gene_name IN ('",
                paste(gene,collapse = "','"),
                "')",
                sep = ""
 )

  })

  rs = suppressWarnings(dbSendQuery(mydb,finalsql))
  data = fetch(rs, n=-1)
  suppressWarnings(dbDisconnect(mydb))
  d <- data %>%
  group_by(contrast_name,gene_name) %>%
  summarise(value = last(value)) %>%
  spread(contrast_name,value)

  data = data.frame(d)
  colnames(data) <- gsub(".vs.","-vs-",colnames(data))

  message("Data downloaded...")
  rownames(data) = data$genes_name
  data = data[,-1]
  anno = suppressMessages(gsmAnnoPulmonDB(colnames(data)))
  data_class <- SummarizedExperiment(assays=list(values=as.matrix(data)),
                                     colData = anno)

  genes = paste(gene, collapse = " , ")

  t = Sys.time()-a
  message(paste("Time of processing",t, attr(t,"units")),collapse= " ")
  message(paste(length(gene),"Genes: ",genes))
  message("Conditions: ",paste(d$condition_node_name, collapse = " , "))

  return(data_class)
}




