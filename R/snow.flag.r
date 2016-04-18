#' Flags snowy pictures based upon Citizen Science analysis of the imagery (zooniverse)
#' 
#' @param filename: filename of a downloaded time series
#' @keywords PhenoCam, pre-processing, screening, snow, outliers
#' @export

snow.flag = function(filename){

  # read file if not a data frame
  if (!is.data.frame(filename)){
   df = read.table(filename,header=T,sep=',')
  }
  
  # include this in the data part of the package, good for now
  snow_data = read.table('/data/Dropbox/Research_Projects/working/FOV_crunch/snow_data_final.csv',sep=',',header=T)
  
  # extract the site name from an image filename
  mysite = unlist(lapply(strsplit(as.character(df$midday_filename),"_")[1],"[[",1))

  # subset site data
  ss = subset(snow_data,site == mysite,select=c("date","snow_majority_text"))
  
  # extract date
  ss_dates = as.Date(ss$date,"%Y_%m_%d")
  
  # get image dates
  df_dates = as.Date(df$date,"%Y-%m-%d")
    
  for ( k in ss_dates ){
    value = ss$snow_majority_text[which(ss_dates %in% k)]
    df$snow_flag[which(df_dates %in% k)] = value   
  }
  
  if (is.data.frame(filename)){
    return(df) 
  }else{
    
    # get colnames
    phenocam_colnames = names(df)
    phenocam_colnames = matrix(phenocam_colnames,1,length(phenocam_colnames))
    
    # pluck real header from the phenocam file
    phenocam_header = readLines(filename,n=22)
    
    # create output filename string
    output_file_name = sprintf("%s_v4.csv",unlist(strsplit(filename,"_v4.csv")))
    
    # write everything to file using append
    write.table(phenocam_header,output_file_name,quote=F,row.names=F,col.names=F,sep=",")
    write.table(phenocam_colnames,output_file_name,quote=F,row.names=F,col.names=F,append=TRUE,sep=",")
    write.table(df,output_file_name,quote=F,row.names=F,col.names=F,append=TRUE,sep=",")
  }
}