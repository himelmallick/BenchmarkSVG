
per_gene_GAM_MGCV=function(abs_expr,sample_info,size.factor,gene.name){
  
  dat<-cbind.data.frame(abs_expr, sample_info)
  dat$size.factor<-size.factor
  stt <- Sys.time()
  fit<-gam(abs_expr ~ offset(log(size.factor)) + s(x, y), family = 'nb', data = dat)
  endd <- Sys.time()
  run_time <- as.numeric(difftime(endd, stt, units = "secs"))

  finres <- list(Method = 'GAM_MGCV',
                 p_val = summary(fit)$s.table[4], 
                 Time.min = run_time)
  return(finres)
}