library(plyr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)
library(ggmap)
library(zipcode)
library(geosphere)
library(zoo)
library(parallel)
library(doParallel)  

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)

data(zipcode)
require(data.table)
installsetwd("~/Desktop/petition/")

# INITIAL DATA SUBSETTING
 petitions = read.csv("~/Desktop/petition/petitions_counts.txt", header=T, quote="", sep="\t")
 chosen_ids = petitions$id[1:100];
 signatures = read.csv("~/Desktop/petition/signatures_large.txt", header=T, quote="", sep="\t")
# 
 signatures_small = signatures[signatures$petition_id %in% chosen_ids, ]
 petitions_small = petitions[petitions$id %in% chosen_ids, ]
 write.table(signatures_small, file="~/Desktop/petition/signatures_small.txt", quote=F, row.names=F, col.names=T, sep="\t")
 write.table(petitions_small, file="~/Desktop/petition/petitions_small.txt", quote=F, row.names=F, col.names=T, sep="\t")

 signatures_small = read.table("~/Desktop/petition/signatures_small.txt", quote="", sep="\t", comment.char="", header=T)
 petitions_small = read.table("~/Desktop/petition/petitions_small.txt", quote = "", sep="\t", comment.char="", header=T)

 petitions_small$petition_id = petitions_small$id
 petitions_small$petition_created = petitions_small$created
 signatures_small$signature_created = signatures_small$created
 signatures_small$petition_created = join(signatures_small, petitions_small, by = "petition_id")$petition_created
 signatures_small$day = floor((signatures_small$signature_created - signatures_small$petition_created) / (60 * 60))
 signatures_small$created = NULL
 write.table(signatures_small, file="~/Desktop/petition/signatures_small2.txt", quote=F, row.names=F, col.names=T, sep="\t")

# GENERATE SUMMARY TABLE FILES
petitions = read.csv("~/Desktop/petition/petitions_counts.txt", header=T, quote="", sep="\t")
signatures = read.csv("~/Desktop/petition/signatures_large.txt", header=T, quote="", sep="\t")

petitions = petitions[petitions$id != "NULL", ]
petitions = petitions[!duplicated(petitions$id), ]
signatures = signatures[signatures$petition_id %in% petitions$id, ]
petitions = petitions[petitions$id %in% unique(signatures$petition_id), ]
bad_petitions = unique(signatures$petition_id[signatures$day < 0])
signatures = signatures[!signatures$petition_id %in% bad_petitions, ]
petitions = petitions[!petitions$id %in% bad_petitions, ]
petitions$id = droplevels(petitions$id)
signatures$petition_id = droplevels(signatures$petition_id)
lookup_table = unique(petitions[,c("id", "created")])
names(lookup_table) = c("petition_id", "petition_created")
signatures$petition_created = join(signatures, lookup_table, by = "petition_id")$petition_created
signatures$day = floor((signatures$created - signatures$petition_created) / (24*60*60))
signatures$minute = floor((signatures$created - signatures$petition_created) / (60))

# write.table(signatures, file="~/Desktop/petition/data/signatures_large_with_day.txt", quote=F, row.names=F, col.names=T, sep="\t")
id_by_day = table(signatures$petition_id, signatures$day)[,1:86400]
id_by_day = as.data.frame(id_by_day)
df <- id_by_day %>% select(signatures$day, frequency)
spread(df, key = signatures$day , value = frequency)

#names(id_by_day) = sprintf("minute%d", 1:86400)
id_by_day$id = rownames(id_by_day)
petitions_merged = join(petitions, id_by_day, by = "id")
write.table(petitions_merged, file="~/Desktop/petition/petitions_merged_MINUTES.txt", quote=F, row.names=F, col.names=T, sep="\t")

#--------------------------------------------------------------#
#--------------------------------------------------------------#
#--------------------------------------------------------------#
#--------------- ADJACENT SIGNATURE TESTS ---------------------#
#--------------------------------------------------------------#
#--------------------------------------------------------------#
#--------------------------------------------------------------#
#--------------------------------------------------------------#

getAllDistances = function(selected_petition) {
  subset = integer(length(zip_and_petition_big))
  subset <- zip_and_petition_big[petition_id == selected_petition]
  zipcodes = subset$zip
  zipcodes = as.numeric(zipcodes)
  selected_zipcodes = zipcodes[grepl("\\d{5}", zipcodes)]
  return(rollapply(selected_zipcodes,width=2,FUN=getDistance))
}


succ_petition_ids = as.vector(successDF$id)
print(succ_petition_ids)
petition_ids = as.vector(petitions$id)
unsucc_petition_ids = outersect(succ_petition_ids, petition_ids)

#PreProcessing
bigsigDT = setDT(signatures)
zip_and_petition_big = subset(bigsigDT, select=c("petition_id", "zip"))

#Parallel Processing
library(parallel)
no_cores <- detectCores()
cl <- makeCluster(no_cores)
clusterExport(cl=cl, varlist=c("text.var", "ntv", "gc.rate", "pos"), envir=environment())
system.time(parLapply(cl, unsucc_petition_ids_sample, getAllDistances))
stopCluster(cl)

# Successful Average Geographic Distance (stored in successful_avg)
nums <- data.frame(unlist(sapply(succ_distances, as.numeric)))
succ_pairs = nums[!is.na(nums)]
successful_avg = mean(succ_pairs)

# Unsuccessful Average Geographic Distance (stored in successful_avg)
unsucc_nums <- data.frame(unlist(sapply(unsucc_distances, as.numeric)))
unsucc_pairs = unsucc_pairs[!is.na(unsucc_pairs)]
unsuccessful_average = mean(unsucc_pairs_filtered)
