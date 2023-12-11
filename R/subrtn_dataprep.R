

Get_subrtn_dataprep=function(url_string,fldr_string){

  
data_list = data.frame(github_ls(repo = url_string, folder = fldr_string, recursive = TRUE, quiet = FALSE))$name
# data_list = data.frame(github_ls(repo = "https://github.com/Xijiang1997/BOOST-MI/blob/master/data/simulated%20data", folder = "./data/simulated data", recursive = TRUE, quiet = FALSE))$name
l_limit=mean(nchar(data_list))-sd(nchar(data_list))*6
length1=length(data_list)
data_list=data_list[nchar(data_list) > l_limit]
cat( "~:NOTE:~", "\n", paste(length1- length(data_list), "data set removed due to very short name", sep = " "),"\n")
##########################################################
#CREATE THE DATA PATTERN, ZERO_PROP, REPLICATES COLUMNS###
##########################################################
modfd_data_list <- strsplit(gsub(c(".RData"),'',data_list), split = "_")

#find max number of mentions 
n = max(sapply(modfd_data_list, length))
#set the length of each response to the max (this will fill in NAs)
for (j in 1:length(modfd_data_list))
  length(modfd_data_list[[j]]) <- n
info_colm = do.call(rbind, modfd_data_list) #combine the list of responses into a matrix

#######################################################
##Special treatment: For Mob data with "_" in pattern##
#######################################################
name_index=which(!(is.na(info_colm[,7])))
info_colm[c(name_index),1]=paste(info_colm[c(name_index),1],info_colm[c(name_index),2],sep = "_")
info_colm[c(name_index),4]=info_colm[c(name_index),5]
info_colm[c(name_index),6]=info_colm[c(name_index),7]

data_list= as.data.frame(cbind(data_list,info_colm[,1],as.numeric(info_colm[,4])*1,as.numeric(info_colm[,6])*1, NA))
colnames(data_list) = c("file_list","pattern_list","zero_infl_list","rplictn_list", "UID")
data_list$zero_infl_list=as.numeric(data_list$zero_infl_list)
data_list$rplictn_list=as.numeric(data_list$rplictn_list)
data_list$UID <- paste(data_list$pattern_list, data_list$zero_infl_list, data_list$rplictn_list, sep = "_")

data_list=data_list[
  with(data_list, order(pattern_list, zero_infl_list, rplictn_list)),
]

string1 <- paste(unique(data_list$pattern_list), sep = "")
string2 <- paste(unique(data_list$zero_infl_list), sep = "")
string3 <- paste(unique(data_list$rplictn_list), sep = "")

cat("\n", "#####UNIQUE PATTERN, ZERO INFLATION and REPLICATION  LIST:####", "\n", string1,"\n",string2,"\n",string3)


###If saving as a variable edit the column results that are saved below
return(data_list)
}
