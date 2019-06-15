
#
# This gives you homogenized values

normPulmonDB = function(gene, id){

  empty='this is gonna be erased'
  if(length(gene)>1){
    geneOR="OR gn.gene_name ="
    for (g in 1:((length(gene)))){
      gene2 = paste(shQuote(gene),geneOR)
    }
    gene=gsub(",", "", toString(gene2))
    gene=paste(gene,empty)
    gene=gsub('OR gn.gene_name = this is gonna be erased', '', toString(gene))
    #gene=substring(toString(gene), first = 1, last = 3)
  } else{
    singledoublequote='"'
    gene=paste(singledoublequote,gene)
    gene=paste(gene,singledoublequote)
    gene=gsub(" ", "", gene, fixed = TRUE)
  }

  if(length(id)>1){
    idOR="OR e.experiment_access_id="
    for (i in 1:length(id)){
      id2 = paste(shQuote(id),idOR)
    }
    #print(id2)
    id=gsub(",", "", toString(id2))
    id=paste(id,empty)
    id=gsub('OR e.experiment_access_id= this is gonna be erased', "", toString(id))
  }else{

    singledoublequote='"'
    id=paste(singledoublequote,id)
    id=paste(id,singledoublequote)
    id=gsub(" ", "", id, fixed = TRUE)

  }


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
  WHERE vt.value_type="M" AND ( gn.gene_name='
  sqlgene =')AND e.experiment_access_id='


  finalsql=paste(sql,gene,sqlgene,id)
  as.character(finalsql)

  rs = dbSendQuery(mydb,finalsql)
  data = fetch(rs, n=-1)
  dbDisconnect(mydb)
  contrast_name_vs_gene_name = data
  #contrast_name_vs_gene_name[1] <- NULL
  #View(data)
  df<-data.frame(data)
  contrast_name_vs_gene_name = contrast_name_vs_gene_name %>% spread(contrast_name,value)
  #tidyr::spread(df, contrast_name, value)
  rownames(contrast_name_vs_gene_name) = contrast_name_vs_gene_name$gene_name
  contrast_name_vs_gene_name = contrast_name_vs_gene_name[,-1]
  data_class <- SummarizedExperiment(assays=list(values=as.matrix(contrast_name_vs_gene_name)))
  return(data_class)
}

