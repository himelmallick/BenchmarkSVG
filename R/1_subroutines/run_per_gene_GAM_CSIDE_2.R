#######################
# METHOD: GAM CSIDE_2 #
#######################


per_gene_GAM_CSIDE_2=function(abs_expr,sample_info,size.factor,gene.name){
  
  stt <- Sys.time()
  sm <- mgcv::smoothCon(mgcv::s(x, y, k = 15, fx = T, bs ='tp'), data = as.data.frame(sample_info))[[1]]
  mm <- as.matrix(data.frame(sm$X))
  df<-ncol(mm)
  X2 <- cbind(mm[,(df - 2):df], mm[,1:(df-3)]) # swap intercept, x, and y
  X2[,2:df] <- sweep(X2[,2:df], 2, apply(X2[,2:df],2, mean), '-')
  X2[,2:df] <- sweep(X2[,2:df], 2, apply(X2[,2:df],2, sd), '/') #standardize
  loc_cside<-as.data.frame(X2[, -1])
  dat<-cbind.data.frame(abs_expr, loc_cside)
  
  fit <- tryCatch(glm.nb(abs_expr ~ offset(log(size.factor)) + ., data = dat), error = function(err){NULL})
  fit0 <- glm.nb(abs_expr ~ 1 + offset(log(size.factor)), data = dat)
  
  if(!is.null(fit)){
    anova.res<- anova(fit, fit0)
    
    endd <- Sys.time()
    run_time <- as.numeric(difftime(endd, stt, units = "secs"))
    
    finres <- list(Method = 'GAM_CSIDE_2',
                   p_val = anova.res$`Pr(Chi)`[2], 
                   Time.min = run_time)
  } else{
    endd <- Sys.time()
    run_time <- as.numeric(difftime(endd, stt, units = "secs"))
    
    finres <- list(Method = 'GAM_CSIDE_2',
                   p_val = NA, 
                   Time.min = run_time)
  }
  
  
  return(finres)
}















