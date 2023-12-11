
per_gene_SPARK=function(abs_expr,sample_info,size.factor,gene.name){
  result_spark_temp <- SPARK(abs_expr, sample_info, size.factor = size_factor, gene.name = colnames(count)[gene_index])
  finres <- list(Method = 'SPARK',
                 p_val = result_spark_temp$measures$p.value, 
                 Time.min = result_spark_temp$time)
  return(finres)
}


