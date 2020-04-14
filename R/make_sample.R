#' @export
make_sample <- function(file_dir="./data_bdf",
                        files="sub.._seg\\.Rdata",
                        stat_summ="median",
                        location=NULL,
                        overwrite=FALSE,
                        dir_out="./"){
  ######## tool functions:
  ########
  get_summary <- function(file_name_set,stat_summ_name,overwrite){
    temp=strsplit(file_name_set,"\\.")[[1]]
    file_name_summary=paste0(paste0(collapse =".", temp[-length(temp)]),"_",stat_summ_name,".",temp[length(temp)])
    
    
    ####load file done or compute the summary
    if((file.exists(file_name_summary))&&(!overwrite) )  
      load(file_name_summary) else{
      cat("\nLoading file ",file_name_set,"...")
      load(file_name_set)
      
      D=D%>%group_by(.sample,condition)%>%summarize_all(eval(parse(text=stat_summ)), na.rm = TRUE) 
    }
    
    ###### adding the subject name in the .segments
    temp=strsplit(file_name_set,"/")[[1]]
    temp=temp[length(temp)]
    sub=strsplit(temp,"_")[[1]][1]
    D$.segments$subj=sub
    # D$.segments$condition
    D
  }
  
  ##############
  update_ids <- function(newD,refD){
    newD$.signal$.id=newD$.signal$.id+max(refD$.signal$.id)
    newD$.segments$.id=newD$.segments$.id+max(refD$.segments$.id)
    newD
  }
  
  ##########################
  if(is.character(stat_summ)) stat_summ_name=stat_summ else stat_summ_name="myfun"
  
  files=dir(file_dir,full.names = TRUE,pattern =files)
  
  data_seg=get_summary(file_name_set=files[1],stat_summ_name,overwrite=overwrite)
  for(f_name_set in files[-1]){
    
    D=get_summary(file_name_set=f_name_set,stat_summ_name,overwrite=overwrite)
    D=update_ids(newD=D,refD=data_seg)

    data_seg=bind(data_seg,D)
  }
  
  if(!is.null(location))
    channels_tbl(data_seg) <-  select(channels_tbl(data_seg), .channel) %>%  left_join(location)
  
  
  save(data_seg,file=paste0(dir_out,"data_seg_",stat_summ,"_sample.Rdata"))
  # table(data_seg$.signal$.id)
  return(data_seg)
}