library(moments)
library(TTR)
library(ggplot2)
library(gridExtra)
library(reshape)
# install.packages("stargazer")
library(stargazer)
setwd("~/Desktop/petition/")
# petitions = read.csv("~/Desktop/petition/data/petitions_merged.txt", header=T, quote="", sep="\t")
# freq = as.matrix(petitions[,grep("^day", names(petitions))])
myFindPeaks = function(v){
#   smoothed = na.omit(SMA(v2, n=smooth_len))
  thres = 0.02*max(v)
  wid = 3
  n = length(v)
  v2 = c(rep(0, wid), v, rep(0, wid))
  peaks = c()
  for(i in (1+wid):(n+wid)){
    around = c(v2[(i-wid):(i-1)], v2[(i+1):(i+wid)])
    if (v2[i] > max(around)){
      if (v2[i] - 0.15*max(v) > mean(around)){
        peaks = c(peaks, i-wid)
      }
    }
  }
  return(peaks)
}
# peaks = apply(freq, 1, myFindPeaks)
# petitions$numpeaks = sapply(peaks, length)
# petitions$total = rowSums(freq)
# petitions_sorted = petitions[order(-petitions$total), ]
# write.table(petitions, file="~/Desktop/petition/data/petitions_counts.txt", quote=F, row.names=F, col.names=T, sep="\t")

petitions = read.csv("~/Desktop/petition/data/petitions_counts.txt", header=T, quote="", sep="\t")
day_idx = which(names(petitions) == 'day1')

### OVERLAY PLOTS

options(scipen=8)
numdays = 60
melted = melt(petitions, measure.vars=day_idx:(day_idx+numdays-1))
melted$day = as.numeric(substring(melted$variable, 4))
melted$group_col = cut(melted$total, breaks=c(150, 1000, 2000, 5000, 10000, 20000, 100000, 500000), dig.lab=10)

melted_suc = melt(petitions[petitions$total>100000,], measure.vars=day_idx:(day_idx+numdays-1))
melted_suc$day = as.numeric(substring(melted_suc$variable, 4))
melted_suc$group_col = cut(melted_suc$total, breaks=c(150, 1000, 2000, 5000, 10000, 20000, 100000, 500000), dig.lab=10)

cumul = petitions
for (i in 1:nrow(petitions)){
  cumul[i, day_idx : (day_idx + 59)] = cumsum(as.numeric(petitions[i, day_idx : (day_idx + 59)])) / petitions[i, 'total']
}
melted_cumul = melt(cumul, measure.vars=day_idx:(day_idx+numdays-1))
melted_cumul$day = as.numeric(substring(melted$variable, 4))
melted_cumul$group_col = cut(melted$total, breaks=c(150, 1000, 2000, 5000, 10000, 20000, 100000, 500000), dig.lab=10)

melted$plot_alpha = ifelse(melted$success, 1, 0.25)
melted$success = factor(melted$success)
melted_cumul$success = melted_cumul$total > 100000
melted_cumul$plot_alpha = ifelse(melted_cumul$success, 1, 0.25)
melted_cumul$success = factor(melted$success)

levels(melted$success) = c('Failed', 'Successful')
ggplot(melted, aes(x=day, y=value, group=title, alpha=plot_alpha)) + 
  geom_line(size=0.05) + scale_y_log10() + 
  ggtitle("Adoption curves of all 3682 petitions") +
  ylab("Number of signatures") +
  facet_wrap(~success) +
  guides(alpha=FALSE) +
  geom_vline(xintercept=30, col='red', linetype = "longdash", size=0.1)
ggsave("plots/adoption_all_bw.png")

melted_cumul_sub = melted_cumul[melted_cumul$group_col %in% c('(150,1000]', '(20000,100000]', '(100000,500000]'), ]
melted_cumul_sub$subgroup = factor(melted_cumul_sub$group_col)
levels(melted_cumul_sub$subgroup) = c('150 to 1000', '20,000 to 100,000', '> 100,000')
ggplot(melted_cumul_sub, aes(x=day, y=value, group=title, alpha=.7)) + 
  geom_line(size=0.1) + 
  ggtitle("Cumulative adoption curves (grouped by total number of signatures)") +
  ylab("Fraction of total signature count") +
  facet_wrap(~subgroup) +
  guides(alpha=FALSE) +
  geom_vline(xintercept=30, col='red', linetype = "longdash", size=0.3)
ggsave("plots/adoption_all_cdf_bw.png", width=30, height=12, units="cm")

ggplot(melted_cumul, aes(x=day, y=value, group=title, color=group_col)) + 
  geom_line(size=0.1, alpha=0.2) + 
  ggtitle("Cumulative adoption curves of all 3682 petitions") +
  ylab("Fraction of total signature count") +
  geom_vline(xintercept=30, col='red', linetype = "longdash", size=0.1) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size=1))) +
  scale_color_hue("Total number of signatures", l=70, c=100, na.value="black")
ggsave("plots/adoption_all_cdf_color.png")

ggplot(melted, aes(x=day, y=value, group=title, color=group_col)) + 
  geom_line(size=0.1, alpha=0.2) + scale_y_log10() + 
  ggtitle("Adoption curves of all 3682 petitions") +
  ylab("Number of signatures") + scale_color_hue("Total signature count") +
  geom_vline(xintercept=30, col='red', linetype = "longdash", size=0.1) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size=1)))
ggsave("plots/adoption_all_color.png")

ggplot(melted_suc, aes(x=day, y=value, group=title, color=group_col)) + 
  geom_line(size=0.3) + scale_y_log10() + 
  ggtitle("Adoption curves of successful petitions") +
  ylab("Number of signatures") + scale_color_hue("Total signature count") +
  geom_vline(xintercept=30, col='red', linetype = "longdash", size=0.1)
ggsave("plots/adoption_successful_color.png")


#### THRESHOLD EFFECTS SUBSECTION

# jan 15 2013
change_date = as.numeric(as.POSIXct("2013-01-15", format="%Y-%m-%d"))
petitions_late = petitions[petitions$created > change_date, ]
petitions_early = petitions[petitions$created <= change_date, ]

p1 = qplot(1:nrow(petitions_early), sort(petitions_early$total, decreasing=TRUE)) + scale_x_log10() + scale_y_log10() + 
  xlab("Petition rank") + ylab("Number of signatures") +
  geom_hline(yintercept=25000, color="red") +
  ggtitle("Petitions started before Jan 2013")

p2 = qplot(1:nrow(petitions_late), sort(petitions_late$total, decreasing=TRUE)) + scale_x_log10() + scale_y_log10() + 
  xlab("Petition rank") + ylab("Number of signatures") +
  geom_hline(yintercept=100000, color="blue") +
  ggtitle("Petitions started after Jan 2013")

grob = arrangeGrob(p1, p2, ncol=2, main=textGrob("Number of signatures for petitions of each rank", gp=gpar(fontsize=20)))
ggsave("plots/threshold_kinks.png", grob, width=30, height=15, units="cm")

successDF = petitions_late[petitions_late$total >= 100000, ] 

# calculate when each successful petition crossed the 100000 threshold
successDF$cross = rep(NA, nrow(successDF))
for (i in 1:nrow(successDF)){
  for (j in 1:60){
    if(sum(successDF[i,day_idx:(day_idx+j-1)]) >= threshold){
      successDF$cross[i] = j
      break
    }
  }
}

# threshold effect: change in rate of change
successDF$before = rep(NA, nrow(successDF))
successDF$after = rep(NA, nrow(successDF))
for (i in 1:nrow(successDF)){
  cross = successDF$cross[i]
  cur = as.numeric(successDF[i,day_idx:(day_idx+60-1)])
      successDF[i, 'before'] = mean(as.numeric(cur[(cross-bw+1):(cross)]))
      successDF[i, 'after'] = mean(as.numeric(cur[(cross+1):(cross+bw)]))
}
ggplot(successDF, aes(x=before, y=after)) + geom_point() + 
  scale_x_log10(limits=c(1e2, 1e5)) + scale_y_log10(limits=c(1e2, 1e5)) + geom_abline(intercept=0, slope=1) +
  xlab("Number of signatures on day before") +
  ylab("Number of signatures on day after") +
  ggtitle("Number of signatures on days before\n and after crossing 100,000 threshold")
ggsave("plots/threshold_scatter.png")

# comparing to fake thresholds
rel_dat = data.frame(threshold=numeric(), rel_change = numeric())
for (threshold in seq(60000, 140000, by=20000)) {
  successDF = petitions_late[petitions_late$total >= threshold, ] 
  
  # calculate when each successful petition crossed the 100000 threshold
  successDF$cross = rep(NA, nrow(successDF))
  for (i in 1:nrow(successDF)){
    for (j in 1:60){
      if(sum(successDF[i,day_idx:(day_idx+j-1)]) >= threshold){
        successDF$cross[i] = j
        break
      }
    }
  }
  
  # threshold effect: change in rate of change
#   bw = 1
#   successDF$ave_before = rep(NA, nrow(successDF))
#   successDF$ave_after = rep(NA, nrow(successDF))
  for (i in 1:nrow(successDF)){
    cross = successDF$cross[i]
    cur = as.numeric(successDF[i,day_idx:(day_idx+60-1)])
#     successDF[i, 'ave_before'] = mean(as.numeric(cur[(cross-bw+1):(cross)]))
#     successDF[i, 'ave_after'] = mean(as.numeric(cur[(cross+1):(cross+bw)]))
    rel_change = (cur[cross+1] / cur[cross]) - 1
    rel_dat = rbind(rel_dat, data.frame(threshold=threshold, rel_change=rel_change))
  }
}
options(scipen=5)
ggplot(rel_dat, aes(x=factor(threshold), y=rel_change)) + geom_boxplot() + 
  ylim(-1.5, 1.5) + ggtitle("Comparison of threshold effects across 5 thresholds") +
  xlab("Threshold") + ylab("Relative change in no. of signatures")
ggsave(filename="plots/threshold_boxplot.png")

thres = rel_dat$threshold
wilcox.test(rel_dat[thres==100000, 'rel_change'], rel_dat[thres!=100000, 'rel_change'], "less")

myplot = ggplot(successDF, aes(x=cross)) + geom_histogram(binwidth=5) + ggtitle("Day when petition crossed 100,000 signatures") + 
  xlab("Day") + ylab("Number of petitions") + xlim(c(0, 60))
ggsave(myplot, filename="plots/threshold_histogram.png")

# PEAKS SUBSECTION

# compute features
for (i in 1:nrow(petitions)) {
  counts = petitions[i,day_idx:(day_idx+59)]
  freq = counts / sum(counts)
  petitions[i, 'global_peak_day'] = which.max(counts)  #which index is maximum?
  petitions[i, 'peak_size'] = max(counts) #what is maximum?
  petitions[i, 'entropy'] = sum(freq * log(1 / freq))
  petitions[i, 'var'] = var(as.numeric(counts))
  petitions[i, 'skewness'] = skewness(as.numeric(counts))
  petitions[i, 'kurtosis'] = kurtosis(as.numeric(counts))
  petitions[i, 'first_day_decay'] = ifelse(counts[1] > counts[2], 1, 0)
}
petitions$num_local_peaks = findLocalPeaks(petitions, 60)
options(scipen=5)
lm1 = lm(total~skewness + kurtosis, data=petitions)
summary(lm1)
lm2 = lm(total~global_peak_day, data=petitions)
summary(lm2)
lm3 = lm(total~global_peak_day + num_local_peaks + skewness + kurtosis, data=petitions)
summary(lm3)
lm4 = lm(log(total)~global_peak_day + num_local_peaks + skewness + kurtosis, data=petitions)
summary(lm4)

stargazer(lm1, lm2, lm3, lm4, report=('vc*p'), font.size="footnotesize")

ggplot(petitions, aes(y=total, x=peak_day)) + 
  geom_jitter(alpha=0.1)  + stat_smooth(method="lm", se=FALSE) +
  scale_y_log10()
ggplot(petitions, aes(x=factor(peak_day), y=total)) + 
  stat_summary(fun.y="mean", geom="bar") + xlab("Day") +
  ylab("Mean no. of signatures") + 
  ggtitle("Mean no. of signatures among petitions peaking on each day")

# correlation between peaks and success

findLocalPeaks = function(dat, num_days) {
  res = rep(NA, nrow(dat))
  for (i in 1:nrow(dat)) {
    cur = dat[i, day_idx:(day_idx + num_days - 1)]
    numpeaks = 0
    for (j in 1:num_days) {
      if ((j==1 || cur[j] > cur[j-1]) && (j==num_days || cur[j] > cur[j+1])) {
        numpeaks = numpeaks + 1
      }
    }
    res[i] = numpeaks
  }
  return(res)
}



#Finds exceed count for each row of data
findExceed = function(dat, num_days) {
  res = rep(NA, nrow(dat))
  for (i in 1:nrow(dat)) {
    cur = dat[i, day_idx:(day_idx + num_days - 1)]
    exceed = 0
    for (j in 1:num_days) {
      if ((j==1 || cur[j] > cur[j-1]) && (j==num_days || cur[j] > cur[j+1])) {
        if (j==1) {
          curr_exceed = cur[j] - cur[j+1]
        }
        else if (j==num_days) {
          curr_exceed = cur[j] - cur[j-1]
        }
        else {
          curr_exceed = cur[j] - min(cur[j-1], cur[j+1])
        }
        exceed = exceed + curr_exceed
      }
    }
    res[i] = exceed
  }
  return(res)
}

day_idx = which(names(petitions_small) == 'day1')

petitions$exceedCounts = findExceed(petitions, 60)
petitions$exceedCounts = unlist(petitions$exceedCounts)
petitions$exceedRatios = petitions$exceedCounts/petitions$signature_count

successDF = petitions[petitions$total >= 100000, ]
failDF = petitions[petitions$total < 100000, ]

exceedratio_succ_mean = mean(successDF$exceedRatios)
exceedratio_fail_mean = mean(failDF$exceedRatios)


subSize = 5
successSub = successDF[sample(1:nrow(successDF), subSize), ]
failSub = failDF[sample(1:nrow(failDF), subSize), ]
plotTimeSeries(successSub, subSize, sprintf("plots/subset_success.png", i), 5, 18, numdays=40, showThreshold=F, title='Successful petitions')
plotTimeSeries(failSub, subSize, sprintf("plots/subset_fail.png", i), 5, 18, numdays=40, showThreshold=F, title='Failed petitions')




petitions$numpeaks1 = findLocalPeaks(petitions, 60)

window_size = 30
petitions$two_zeros = rep(0, nrow(petitions))
for (i in 1:nrow(petitions)) {
  cur = petitions[i, day_idx:(day_idx + window_size - 1)]
  for (j in 1:(window_size - 1)){
    if (cur[j] == 0 && cur[j+1] == 0) {
      petitions[i, 'two_zeros'] = 1
      break
    }
  }
}

window_size = 30
# petitions_nz = petitions[petitions$two_zeros == 0, ]
petitions_nz$numpeaks1 = findLocalPeaks(petitions_nz, window_size)

lm4 = lm(total~numpeaks1, data=petitions_nz)
summary(lm4)

summary(lm(log(total)~numpeaks1, data=petitions_nz))
plot(log(total)~numpeaks1, data=petitions_nz)

petitions$peaks1group = cut(petitions$numpeaks1, breaks=2*(0:10))
ggplot(petitions, aes(x=peaks1group, y=total)) + geom_boxplot() + scale_y_log10()

petitions_nz$peaks1group = cut(petitions_nz$numpeaks1, breaks=2*(0:10))
ggplot(petitions_nz, aes(x=peaks1group, y=total)) + geom_boxplot() + scale_y_log10()

subset_peaks = 4
subgroup = petitions_nz[petitions_nz$numpeaks1 == subset_peaks,]
plotTimeSeries(subgroup[1:12,], ncol=4, sprintf("plots/subgroup_%d_peaks.png", subset_peaks), 10, 12, numdays=window_size, showThreshold=F)

lm1 = lm(total~factor(numpeaks), data=petitions)
summary(lm1)
ggplot(petitions, aes(x=factor(numpeaks), y=total)) + geom_boxplot() + scale_y_log10()

# first day vs second day

tab = table(petitions$success, petitions$decayed)
norm_tab = tab %*% rowSums(tab)
ggplot(petitions, aes(x=success)) + geom_bar(stat="bin")

# successful (hist + cumul) plots

plotTimeSeries = function(df, ncol, filename, height, width, numdays, showThreshold, title) {
  #   plotlist = list()
  #   for (j in 1:30){
  #     dat = data.frame(day=1:60, signatures=as.numeric(psubi[j, day_idx:(day_idx+59)]))
  #     plotlist[[j]] = ggplot(dat, aes(day, signatures)) + geom_bar(stat="identity") + ggtitle(psubi$title[j])  +
  #       theme(axis.title.x = element_blank(),axis.title.y = element_blank())
  #   }
  #   do.call("grid.arrange", c(plotlist, ncol=5))
  melted = melt(df, measure.vars=day_idx:(day_idx+numdays-1))
  melted$day = as.numeric(substring(melted$variable, 4))
  melted$title = sapply(as.character(melted$title), function(x) iconv(enc2utf8(x),sub="byte"))
  melted$shorttitle = paste(substr(melted$title, 1, 30), "...", sep="")
  myplot = ggplot(melted, aes(x=day, y=value)) + geom_bar(stat="identity") + ylab("signatures") + 
    facet_wrap(~shorttitle, ncol=ncol, scales="free") + theme(strip.text.x = element_text(size = 7)) +
    ggtitle(title)
  if (showThreshold) {
    myplot = myplot + geom_vline(aes(xintercept=cross), df, colour="red")
  }
  print(myplot)
  ggsave(myplot, height=height, width=width, filename=filename)
}

# chunks = list(1:16, 17:32, 33:48, 49:57)
chunks = list(1:16)
for (i in 1:length(chunks)){
  psubi = successDF[chunks[[i]], ]
  plotTimeSeries(psubi, 4, sprintf("plots/successful_hist_%d.png", i), showThreshold=F)
}




# CUMULATIVE
pcum = successDF
for (i in 1:nrow(successDF)){
  pcum[i, day_idx : (day_idx + 59)] = cumsum(as.numeric(successDF[i, day_idx : (day_idx + 59)]))
}

for (i in 1:length(chunks)){
  #   plotlist = list()
  #   for (j in 1:30){
  #     dat = data.frame(day=1:60, signatures=as.numeric(psubi[j, day_idx:(day_idx+59)]))
  #     plotlist[[j]] = ggplot(dat, aes(day, signatures)) + geom_bar(stat="identity") + ggtitle(psubi$title[j])  +
  #       theme(axis.title.x = element_blank(),axis.title.y = element_blank())
  #   }
  #   do.call("grid.arrange", c(plotlist, ncol=5))
  
  psubi = pcum[chunks[[i]], ]
  melted = melt(psubi, measure.vars=day_idx:(day_idx+59))
  melted$day = as.numeric(substring(melted$variable, 4))
  melted$title = sapply(as.character(melted$title), function(x) iconv(enc2utf8(x),sub="byte"))
  melted$shorttitle = paste(substr(melted$title, 1, 40), "...", sep="")
  psubi$title = sapply(as.character(psubi$title), function(x) iconv(enc2utf8(x),sub="byte"))
  psubi$shorttitle = paste(substr(psubi$title, 1, 40), "...", sep="")
  myplot = ggplot(melted, aes(x=day, y=value)) + geom_bar(stat="identity") + ylab("signatures") + 
    facet_wrap(~shorttitle, ncol=4, scales="free") + theme(strip.text.x = element_text(size = 7))
    geom_vline(aes(xintercept=cross), psubi, colour="red")
  print(myplot)
  ggsave(myplot, filename=sprintf("plots/successful_cumul_%d.png", i))
}


# petitions$peak = apply(petitions[,day_idx:(day_idx+59)], 1, max)
# petitions$proportion_peak = petitions$peak / petitions$total
# ggplot(petitions, aes(x=total, y=proportion_peak)) + geom_point()+ scale_x_log10()


