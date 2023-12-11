
per_gene_SpatialDE=function(abs_expr,sample_info,size.factor,gene.name){
  result_SpatialDE_temp <- SpatialDE(norm_expr_log_vst, sample_info, gene.name = colnames(count)[gene_index])
  finres <- list(Method = 'SpatialDE',
                 p_val = result_SpatialDE_temp$measures$p.value, 
                 Time.min = result_SpatialDE_temp$time)
  return(finres)
}
