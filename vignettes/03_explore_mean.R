#################Create eeg_lst object##################################
rm(list=ls())
library(magrittr)
library(eeguana)
library(plyr)
library(dplyr)
library(ggplot2)
source('./functions/t_test_light.R', echo=TRUE)

load("data_seg_median_sample.Rdata")

Y=as.matrix(data_seg$.signal[(1:32)*2,,with=TRUE])-
  as.matrix(data_seg$.signal[(1:32)*2-1,,with=TRUE])
res=t.test.light(Y[,-(1:2)],tail=0)
min(res$p)


dim( data_seg$.signal)
names(data_seg$.signal)
table(data_seg$.signal$.id)
data_seg$.signal$.sample

str( data_seg$.segments)
str( data_seg$.events)

table( data_seg$.segments$condition)
table( data_seg$.segments$subj)

# str( data_seg)

#Some plots

#Plot of all the ERP of the O1 electrode

data_seg %>%
  select(H1) %>%
  ggplot(aes(x = .time, y = .value)) +
  geom_line()

#Plot ERP of each subject (average across condition):

# data_seg %>%
#   select(H1) %>%
#   ggplot(aes(x = .time, y = .value)) +
#   geom_line(aes(group = subj))  +
#   stat_summary(
#     fun = "mean", geom = "line", alpha = 1, size = 1.5,
#     aes(color = "red"),show.legend = FALSE
#   ) 

##### average by condition
Dav= data_seg%>%group_by(.sample,condition)%>%summarize_all(mean, na.rm = TRUE)
dim(Dav$.signal)
Dav$.segments

Dav %>%
  select(H1) %>%
  ggplot(aes(x = .time, y = .value)) +
  geom_line(aes(group = condition,color=condition))

##### average POTENTIAL in range
AvePot= data_seg%>%filter(between(as_time(.sample, unit = "s"), .1, .2)) %>%
  group_by(condition)%>%summarize_all(mean, na.rm = TRUE)

AvePot

#Plot ERP of condition (average across subject)
data_seg %>%
  select(H1, G2, F2,E3) %>%
  ggplot(aes(x = .time, y = .value)) +
  geom_line(alpha = .1, aes(group = .id, color = condition)) +
  stat_summary(
    fun = "mean", geom = "line", alpha = 1, size = 1.5,
    aes(color = condition)
  ) +
  facet_wrap(~.key) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = .17, linetype = "dotted") +
  theme(legend.position = "bottom")


ERP_data <-  data_seg %>%
  group_by(.sample, condition) %>%
  summarize_at(channel_names(.), mean, na.rm = TRUE)



ERP_plot<- ERP_data %>%
  ggplot(aes(x = .time, y = .value)) +
  geom_line(aes(color = condition)) +
  facet_wrap(~.key) +
  theme(legend.position = "bottom") +
  ggtitle("ERPs for disgust vs object") +
  theme_eeguana()
ERP_plot

ERP_plot %>% plot_in_layout()

data_seg %>%
  filter(between(as_time(.sample, unit = "s"), .05, .15)) %>%
  group_by(condition) %>%
  summarize_at(channel_names(.), mean, na.rm = TRUE) %>%
  plot_topo() +
  annotate_head() +
  geom_contour() +
  geom_text(colour = "black") +
  facet_grid(~condition)

# df <-  data_seg %>%
#   select(O1, O2, P7, P8) %>%
#   as_tibble() %>%
#   # We can use regular dplyr functions now
#   group_by(.key, .time) %>%
#   summarize(
#     `t-value` = t.test(
#       .value[condition == "disgust"],
#       .value[condition == "object"]
#     )$statistic
#   )
# 
# 
# ggplot(df, aes(x = .time, y = `t-value`)) + geom_line() +
#   facet_wrap(~.key)
# 
# 
# faces_seg_t <-
#    data_seg %>%
#   select(O1, O2, P7, P8) %>%
#   group_by(.sample) %>%
#   summarize_at(channel_names(.), list(t =  ~t.test(
#     .[condition == "disgust"],
#     .[condition == "object"]
#   )$statistic))
# 
# faces_seg_t %>%
#   ggplot(aes(x = .time, y = .value)) +
#   geom_line(alpha = .1, aes(group = .id)) +
#   stat_summary(fun = "mean", geom = "line", alpha = 1, size = 1) +
#   facet_wrap(~.key) +
#   theme(legend.position = "bottom")
# 
