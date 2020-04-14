#' @export
preproc_subj <- function (file_name,overwrite=FALSE,freq = c(1, 40)){
  # require(dplyr)
  cat("\n Preprocessing: ",file_name,"\n")
  
    
  file_out=strsplit(file_name,"/")[[1]]
  dir_out=paste(collapse ="/",file_out[-length(file_out)])
  file_out=file_out[length(file_out)]
  
  file_out=strsplit(file_out,"\\.")[[1]]
  file_seg= paste(dir_out,paste0(file_out[1],"_seg.Rdata"),sep="/")
  if((!overwrite)&&file.exists(file_seg)){
    cat("\nFile ",file_name, " is already preprocessed. Set parameter overwrite=FALSE to force re-preprocessing.")
    return(NA)
  }
  
  cat("\n loading ",file_name,"\n")
  D=eeguana::read_edf(file_name)
  # pryr::object_size(D0)
  # table(D$.events$.description)
  # save(file="sog1",D0)
  
  names(D$.signal) <- gsub("X1","",names(D$.signal))
  
  D$.signal=D$.signal[,-grep("^Ana",names(D$.signal)),with=FALSE]
  
  mean_ch = chs_mean(D,na.rm = TRUE)  
  D$.signal$mean_ch=mean_ch$.signal$mean
  D <- eeg_rereference(D, ref = c("mean_ch"))
  D$.signal$mean_ch=NULL
  
  ###############
  cat("\n filtering ",file_name,"\n")
  
  D <- eeg_filt_band_pass(D,  freq = freq) 
  # plot(D0)
  
  ##location
  # source('C:/Users/livio/Dropbox (unipd)/dataset/hdeeg_emotion/eeguana_location.R', echo=TRUE)
  # channels_tbl(D0)
  # loc
  # loc$.channel=as.character(loc$.channel)
  # loc$.channel<-paste0("X1",loc$.channel)
  # channels_tbl(D0)<-select(channels_tbl(D0), .channel) %>%
  # left_join(loc) 
  
  
  cat("\n segmenting ",file_name,"\n")
  
  Dsegs <- D %>%
    eeg_segment(.description %in% c(9:23),
                lim = c(-1, 1)
    ) %>%
    eeg_events_to_NA(.type == "Bad Interval") %>%
    eeg_baseline()
  
  # rm(D)
  # check
  # table(Dsegs$.segments$description)

  Dsegs$.segments$condition =
    if_else(Dsegs$.segments$description %in%c(9,11,13,15,17,19,21,22), "Pos", "Neg")  
  
  
  cat("\n removing artifacts ",file_name,"\n")
  
  data_seg_artif <- Dsegs %>%
    eeg_artif_minmax %>%
    eeg_artif_step #%>% eeg_artif_amplitude #%>% eeg_artif_peak
  # Signals with artifacts are turned into NA values
  Dsegs <-  eeg_events_to_NA(data_seg_artif,.type == "artifact")
  
  
  D=Dsegs
  
  cat("\n saving preprocessed ",file_name," in file ",file_seg,"\n")
  
  save(file=file_seg,D)
  # Dsegs$.segments
  
  
  
  # library(ggplot2)
  # Dsegs %>%
  #   select(A2,A3, G6, H24) %>%
  #   ggplot(aes(x = .time, y = .value)) +
  #   geom_line(alpha = .1, aes(group = .id, color = condition)) +
  #   stat_summary(
  #     fun = "mean", geom = "line", alpha = 1, size = 1.5,
  #     aes(color = condition)
  #   ) +
  #   facet_wrap(~.key) +
  #   geom_vline(xintercept = 0, linetype = "dashed") +
  #   geom_vline(xintercept = .17, linetype = "dotted") +
  #   theme(legend.position = "bottom")
  # 
  # 
  ######## mean
  D=Dsegs%>%group_by(.sample,condition)%>%summarize_all(mean, na.rm = TRUE)
  # dim(D$.signal)
  save(file=paste(dir_out,paste0(file_out[1],"_seg_mean.Rdata"),sep="/"),D)



  ######## median
  D=Dsegs%>%group_by(.sample,condition)%>%summarize_all(median, na.rm = TRUE)
  # dim(D$.signal)
  save(file=paste(dir_out,paste0(file_out[1],"_seg_median.Rdata"),sep="/"),D)
  
  return(TRUE)
}
