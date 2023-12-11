
################################
#SUBROUTINE: SCENARIO x METHOD##
################################

Get_subrtn_SVG_benchmark= function(scenario_id, methods, ref_file=master_list){
  

  ##################################################
  # ONE ROW SUMMARY PLACEHOLDER FOR EACH REPLICATE #
  ################################################## 
  Final_summary=list()
  summary<-matrix(0, nrow = length(methods), ncol = 10)
  colnames(summary)<-c('methods','pattern', 'zero_inflation', 'replicate', 'Sensitivity', 'Specificity', 'F1_score', 'FDR', 'AUC', 'MCC')
  
  pb=txtProgressBar(min = 0, max = 100, style = 3)
  

    
    #######################
    #CREATE REFERENCE ROW##
    #######################
    sub_data=ref_file[which(scenario_id==ref_file$UID),]
    ind_ptrn =  sub_data$pattern_list
    ind_zerprp =  sub_data$zero_infl_list
    ind_rplict =  sub_data$rplictn_list
    
    ############
    #LOAD DATA##
    ############
    file_location<-paste(url_name,sub_data$file_list,sep = "/" )
    load_github_data(file_location)
    
    ######################
    # Add TP Information #
    ######################
    
    colnames(count)<-paste('Feature', 1:ncol(count), sep = '')
    colnames(count)<-ifelse(gamma==TRUE, 
                            paste(colnames(count), '_TP', sep =''), 
                            colnames(count))
    
    #################################
    # Standardize spatial locations #
    #################################
    
    loc<-as.data.frame(loc)
    loc <- sweep(loc, 2, apply(loc,2, mean), '-')
    loc <- loc / sd(as.matrix(loc))
    loc<-as.matrix(loc)
    t_count<-as.matrix(t(count))
    
    #####################
    # Create SPE object #
    #####################
    
    spe <- SpatialExperiment(
      list(counts = t_count), 
      spatialCoords = loc)
    
    ################
    # Sanity check #
    ################
    
    head(spatialCoords(spe))
    spatialCoordsNames(spe)
    spe
    dim(spe)
    
    ##########################
    # Size factor estimation # 
    ##########################
    
    spe <- nnSVG::filter_genes(spe, filter_mito = FALSE)
    dim(spe)
    count<-as.data.frame(t(assay(spe)))
    sce<-t(count)
    size_factor <- scran::calculateSumFactors(sce)
    
    ####################
    # Normalize counts #
    ####################
    normalized_count_tss <- normalize.st(count, scaling.method = "TSS")
    normalized_count_log_vst <- normalize.st(count, scaling.method = "log-VST")
    
    ##############################
    # Store location information #
    ##############################  
    # neighbor_info <- get.neighbors(loc, n.neighbors = 4, method = "distance")
    sample_info<-loc
    
    
    
    ###################################
    # PLACEHOLDER FOR PER-GENE SUMMARY#
    ###################################
    n_gene <- ncol(count)
    
    result <- matrix(0, nrow = n_gene, ncol = length(methods))
    colnames(result) <- methods
    
    result_time <- matrix(0, nrow = n_gene, ncol = length(methods))
    colnames(result_time) <- methods
    
    
   for (method in methods) {
     
     ##############################
     # RUN: 1 METHOD x 1 REPLICATE #
     ##############################
     
     for (gene_index in 1:ncol(count)){
       closeAllConnections()
       ###########################
       # Extract per-gene values #
       ###########################
       
       abs_expr <- count[, gene_index]
       norm_expr_log_vst <- normalized_count_log_vst[, gene_index]
       norm_expr_tss <- normalized_count_tss[, gene_index]
       norm_expr_tss_log <- log(norm_expr_tss + 1)
       
       ###################
       # Print Iteration #
       ###################
       
       cat("& Dataset=", scenario_id, "& Gene No: ",gene_index, "& Method:", method, "\r")
       setTxtProgressBar(pb,gene_index)
       
       ##########################
       # RUN: 1 METHOD x 1 GENE ##
       ##########################
       fun <- paste(paste('per_gene_', method, sep = ''),"(abs_expr,sample_info,size.factor=size_factor,gene.name)", sep = '')
       result_temp <- eval(parse(text = fun))
       result[gene_index, method] <- result_temp$p_val
       result_time[gene_index, method] <- result_temp$Time.min
       
     }

     
   }

    
    


    #################
    # Store results #
    #################
    
    for(i in 1:length(methods)){
      temp<-compute.metrics(p.adjust(result[, methods[i]], 'BH'), truth = gamma, predictor.type = 'p-value', threshold = 0.05)
      summary[i, 'methods']<-methods[i]
      summary[i, 'Sensitivity']<-temp$Sensitivity
      summary[i, 'Specificity']<-temp$Specificity
      summary[i, 'F1_score']<-temp$F1_score
      summary[i, 'FDR']<-temp$FDR
      summary[i, 'AUC']<-temp$AUC
      summary[i, 'MCC']<-temp$MCC
      summary[i, 'pattern']=ind_ptrn
      summary[i, 'replicate']=ind_rplict
      summary[i, 'zero_inflation']=ind_zerprp
      
    }

    summary<-as.data.frame(summary)
    summary$Time<-colMeans(result_time)
    Final_summary=rbind(Final_summary,summary)
    
    
   
  
  
  return(Final_summary)  
}
