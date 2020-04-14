make_sample <- function(file_dir="./data_bdf",
                        files="sub.._seg\\.Rdata",
                        stat_summ="median", #MANCA, IMPLEMENTARE QUESTA
                        LOC){
  files=dir(file_dir,full.names = TRUE,pattern =files)
  
  file_name_set=files[1]
  temp=strsplit(file_name_set,"/")[[1]]
  temp=temp[length(temp)]
  sub=strsplit(temp,"_")[[1]][1]
  cat("\nLoading file ",file_name_set,"...")
  load(file_name_set)
  D=D%>%group_by(.sample,condition)%>%summarize_all(median, na.rm = TRUE) 
  D$.segments$subj=sub
  # D$.segments$condition
  
  data_seg=D
  for(file_name_set in files[-1]){
    
    temp=strsplit(file_name_set,"/")[[1]]
    temp=temp[length(temp)]
    sub=strsplit(temp,"_")[[1]][1]
    cat("\nLoading file ",file_name_set,"...")
    load(file_name_set)
    D=D%>%group_by(.sample,condition)%>%summarize_all(median, na.rm = TRUE) 
    D$.segments$subj=sub
    
    
    D$.signal$.id=D$.signal$.id+max(data_seg$.signal$.id)
    # data_seg$.signal=bind_rows(data_seg$.signal,D$.signal)
    
    D$.segments$.id=D$.segments$.id+max(data_seg$.segments$.id)
    # data_seg$.segments=rbind(data_seg$.segments,D$.segments)
    
    # data_seg$.events=rbind(data_seg$.events,D$.events)
    data_seg=bind(data_seg,D)
  }
  
  channels_tbl(data_seg) <- select(channels_tbl(data_seg), .channel) %>%  left_join(LOC)
  
  save(data_seg,file=paste0("data_seg_",stat_summ,"_sample.Rdata"))
  # table(data_seg$.signal$.id)
  
}
