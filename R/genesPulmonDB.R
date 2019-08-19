#' Homogenized values of PulmonDB
#'
#' This gives you a S3 object with homogenized values and annotation
#' per contrast.
#'
#' Homogenized values are contrasts of a sample reference vs a test
#' reference in log2. We have
#' performed normalization using RMA for Affymetrix platform and
#' loess normalization for non-Affymetrix.
#'
#' Rownames are genes (e.i."MMP7","JUND") and colnames are contrasts
#' (GSMXX1.ch1-vs-GSMXX2.ch1) in which the first GSM (GSMXX1.ch1) is the
#' test and the second GSM (GSMXXX2.ch1) is the reference. The
#' annotation follows annotationPulmonDB()  output, and has the same order.
#'
#' @param gene A value or character vector with gene names. It also accepts
#' 'all' for downloading every available gene.
#' @param id A value or character vector with GSEs (GEO id).
#'
#' @export
#' @import tidyr
#' @import RMySQL
#' @import dplyr
#' @import SummarizedExperiment
#' @examples
#'
#' a <- genesPulmonDB(c("MMP1","JUND"), c("GSE1122","GSE101286"))
#' #annotation
#' colData(a)
#' #values
#' assays(a)$values
#' assay(a)
#' #All genes
#' all_genes <- genesPulmonDB("all","GSE1122")
#' #Value of MMP7
#' gse <- c("GSE32537","GSE21369","GSE24206","GSE94060","GSE72073","GSE35145","GSE52453")
#' mmp7 <- genesPulmonDB("MMP7",gse)
#'
#'
#' \dontrun{
#' ## a comment
#' genesPulmonDB("MMP1","JUND", c("GSE1122"))
#' }
#'
#' @family pulmondb
#'
#'
genesPulmonDB = function(gene, id){

  value <- NULL

  a <- Sys.time()
  message("Connecting to PulmonDB")

  mydb = dbConnect(MySQL(),
                   user="guest",
                   password="pulmonDBguest",
                   dbname="expdata_hsapi_ipf",
                   host="132.248.248.114")

  suppressWarnings(if (gene == "all"){
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

  } else {

  sql = 'select \
  n.value, gn.gene_name,sc.contrast_name \
  from norm_data AS n \
  INNER JOIN gene_name as gn ON n.gene_fk=gn.gene_fk \
  INNER JOIN value_type AS vt ON n.value_type_fk=vt.value_type_id \
  INNER JOIN sample_contrast AS sc ON n.contrast_fk=sc.contrast_id \
  INNER JOIN sample AS s ON sc.test_sample_fk=s.sample_id \
  INNER JOIN experiment AS e ON s.experiment_fk= e.experiment_id
  WHERE vt.value_type="M" AND  (gn.gene_name IN ("'
  sqlex ='") ) AND (e.experiment_access_id IN ("'


  finalsql=paste(sql,
                 paste(gene,collapse='","'),
                 sqlex,
                 paste(id,collapse='","'),'"))',
                 sep = ""
  )

  })

  rs = suppressWarnings(dbSendQuery(mydb,finalsql))
  data = fetch(rs, n=-1)
  suppressWarnings(dbDisconnect(mydb))
  data = tidyr::spread(data,contrast_name,value)
  #tidyr::spread(df, contrast_name, value)
  rownames(data) = data$genes_name
  data = data[,-1]
  anno = suppressMessages(annotationPulmonDB(id))
  data_class <- SummarizedExperiment(assays=list(values=as.matrix(data)),
                                     colData = anno)

  message("Time of processing ",Sys.time()-a)
  genes = paste(gene, collapse = " , ")
  ids = paste(id, collapse = " , ")
  message("Data downloaded...")
  message(paste(length(gene),"Genes:",genes))
  message(paste(length(id),"GSE:",ids))

  return(data_class)
}


