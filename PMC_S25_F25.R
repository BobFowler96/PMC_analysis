
#----Step 1----
#--------------#



# put together metadata with pmc time file
# need to:
#     pull time series licor data
#     find final 5ish measurements before time gap
#     average them
#     save avg as ppm for pause #
#     match pause # with metadata file


library(dplyr)
library(ggplot2)


#Run1
getwd()
dir() #what files are in my wd?
# tst=read.table(file.choose())#lets you pull files from folders

meta1 = read.csv("Run 1.csv", header=TRUE)
#sample data and approx times, sep="," for csv



str(meta1)

licor1 = read.table("Run1.txt", header = TRUE,
                    sep="\t", #tab-delim
                    skip = 1, #skip the first two lines
                    col.names = c("date", "time", "ppm", "temp","press",
                                  "abs", "voltage", "flow", "junk"))


str(licor1)#what kind of data? structure
## get date
licor1$date=as.Date(licor1$date, "%Y-%m-%d")
licor1$time = as.POSIXct(licor1$time, format = "%H:%M:%S")


smple_cnt1 = 1
for (i in 1:nrow(licor1)){
  print(paste("Working on",i)) #print the name of the row
  if (i == nrow(licor1)){ #on the last row, do something different
    print("  Last one...")
    print(paste("pause #", smple_cnt1))
    ppm_avg1 = mean(licor1$ppm[(i-5):i])
    #ppm_avg is the major output of this code
    #it tells you, what did  the licor find the average ppm right beforethe pause
    
    meta1[smple_cnt1, 'time'] = substr(as.character(licor1$time[i]), 12, 19)
    meta1[smple_cnt1, 'ppm'] = ppm_avg1
    next
  }
  no_pause1 = as.numeric(difftime(licor1$time[i+1], licor1$time[i], units="secs"))
  #line above , "no_pause", calculates the difference between each line
  if (no_pause1<=4) { #if () is trure, then the action {} happens
    print(paste("  No pause yet..."))
    next #go to next line
  } 
  print("Found a pause!")
  if (i<5){
    print("But its less than five")
    next #go to next line
  }
  print(paste("Pause #", smple_cnt1))
  
  ppm_avg1 = mean(licor1$ppm[(i-5):i]) # calculates average of previous 5 lines
  
  meta1[smple_cnt1, 'time'] = substr(as.character(licor1$time[i]), 12, 19)
  meta1[smple_cnt1, 'ppm'] = ppm_avg1
  meta1[smple_cnt1, 'line'] = i
  
  smple_cnt1 = smple_cnt1 + 1
}

meta1$ppm.corr = meta1$ppm-mean(meta1$ppm[which(meta1$Sample.ID=="blank")])
# mean(meta$ppm[which(meta$soilmass==0)])#
#correct for blank
#replace "plot' with the column id name that you use if necessary
str(meta1)

#calculate actual time elapsed
meta1$start = as.POSIXct(meta1$start, format = "%H:%M")
meta1$end = as.POSIXct(meta1$end, format = "%H:%M")

meta1$days = (meta1$end-meta1$start)/(24*60*60)+1
meta1$ugC.L <- NA
meta1$ugC.L =as.numeric(meta1$ugC.L)
meta1$days = as.numeric(meta1$days)

meta1$ugC.L = (meta1$ppm.corr*12*1)/(.082058*298.15)
#calculate in ug CO2-C per L using PV=nRT


meta1$ugC.g.day = ((meta1$ugC.L*.932))/as.numeric(meta1$soilmass)
#calculate in ug CO2-C per g soil,
#which = mg C/kg soil
#.932 = L in jar
(meta1$ugC.L*.932)/meta1$soilmass/meta1$days
(meta1$ugC.L*.932)/meta1$soilmass
((meta1$ugC.L*.932))/ meta1$soil.mass
meta1$ugC.g.day = ((meta1$ugC.L*.932))/ meta1$soil.mass


head(meta1)

write.csv(meta1, "Run1_calcsfull.csv",
          row.names = FALSE)

#Run2
getwd()
dir() #what files are in my wd?
# tst=read.table(file.choose())#lets you pull files from folders

meta2 = read.csv("Run 2.csv", header=TRUE)
#sample data and approx times, sep="," for csv

str(meta2)

licor2 = read.table("Run2.txt", header = TRUE,
                    sep="\t", #tab-delim
                    skip = 1, #skip the first two lines
                    col.names = c("date", "time", "ppm", "temp","press",
                                  "abs", "voltage", "flow", "junk"))


str(licor2)#what kind of data? structure
## get date
licor2$date=as.Date(licor2$date, "%Y-%m-%d")
licor2$time = as.POSIXct(licor2$time, format = "%H:%M:%S")


smple_cnt2 = 1
for (i in 1:nrow(licor2)){
  print(paste("Working on",i)) #print the name of the row
  if (i == nrow(licor2)){ #on the last row, do something different
    print("  Last one...")
    print(paste("pause #", smple_cnt2))
    ppm_avg2 = mean(licor2$ppm[(i-5):i])
    #ppm_avg is the major output of this code
    #it tells you, what did  the licor find the average ppm right beforethe pause
    
    meta2[smple_cnt2, 'time'] = substr(as.character(licor2$time[i]), 12, 19)
    meta2[smple_cnt2, 'ppm'] = ppm_avg2
    next
  }
  no_pause2 = as.numeric(difftime(licor2$time[i+1], licor2$time[i], units="secs"))
  #line above , "no_pause", calculates the difference between each line
  if (no_pause2<=4) { #if () is trure, then the action {} happens
    print(paste("  No pause yet..."))
    next #go to next line
  } 
  print("Found a pause!")
  if (i<5){
    print("But its less than five")
    next #go to next line
  }
  print(paste("Pause #", smple_cnt2))
  
  ppm_avg2 = mean(licor2$ppm[(i-5):i]) # calculates average of previous 5 lines
  
  meta2[smple_cnt2, 'time'] = substr(as.character(licor2$time[i]), 12, 19)
  meta2[smple_cnt2, 'ppm'] = ppm_avg2
  meta2[smple_cnt2, 'line'] = i
  
  smple_cnt2 = smple_cnt2 + 1
}

meta2$ppm.corr = meta2$ppm-mean(meta2$ppm[which(meta2$Sample.ID=="blank")])
#meta2$ppm.corr = mean(meta2$ppm[which(meta2$sample.id=="Blank")])
#correct for blank
#replace "plot' with the column id name that you use if necessary
str(meta2)

#calculate actual time elapsed
# this didn't work meta$readdate = as.Date(meta$readdate, format = "%Y%m%d")
meta2$start = as.POSIXct(meta2$start, format = "%H:%M")
meta2$end = as.POSIXct(meta2$end, format = "%H:%M")

meta2$days = (meta2$end-meta2$start)/(24*60*60)+1
meta2$ugC.L <- NA
meta2$ugC.L =as.numeric(meta2$ugC.L)
meta2$soilmass = as.numeric(meta2$soilmass)
meta2$days = as.numeric(meta2$days)

meta2$ugC.L = (meta2$ppm.corr*12*1)/(.082058*298.15)
#calculate in ug CO2-C per L using PV=nRT

meta2$ugC.g.day = (meta2$ugC.L*.932)/meta2$soilmass/meta2$days

(meta2$ugC.L*.932)/meta2$soilmass/meta2$days
(meta2$ugC.L*.932)/meta2$soilmass
((meta2$ugC.L*.932))/ meta2$soil.mass

meta2$ugC.g.day = ((meta2$ugC.L*.932))/ meta2$soil.mass

#calculate in ug CO2-C per g soil,
#which = mg C/kg soil
#.932 = L in jar
head(meta2)

write.csv(meta2, "Run2_calcsfull.csv",
          row.names = FALSE)

#Run3

getwd()
dir() #what files are in my wd?
# tst=read.table(file.choose())#lets you pull files from folders

meta3 = read.csv("Run 3.csv", header=TRUE)
#sample data and approx times, sep="," for csv

str(meta3)

licor3 = read.table("Run3.txt", header = TRUE,
                    sep="\t", #tab-delim
                    skip = 1, #skip the first two lines
                    col.names = c("date", "time", "ppm", "temp","press",
                                  "abs", "voltage", "flow", "junk"))


str(licor3)#what kind of data? structure
## get date
licor3$date=as.Date(licor3$date, "%Y-%m-%d")
licor3$time = as.POSIXct(licor3$time, format = "%H:%M:%S")


smple_cnt3 = 1
for (i in 1:nrow(licor3)){
  print(paste("Working on",i)) #print the name of the row
  if (i == nrow(licor3)){ #on the last row, do something different
    print("  Last one...")
    print(paste("pause #", smple_cnt3))
    ppm_avg3 = mean(licor3$ppm[(i-5):i])
    #ppm_avg is the major output of this code
    #it tells you, what did  the licor find the average ppm right beforethe pause
    
    meta3[smple_cnt3, 'time'] = substr(as.character(licor3$time[i]), 12, 19)
    meta3[smple_cnt3, 'ppm'] = ppm_avg3
    next
  }
  no_pause3 = as.numeric(difftime(licor3$time[i+1], licor3$time[i], units="secs"))
  #line above , "no_pause", calculates the difference between each line
  if (no_pause3<=4) { #if () is trure, then the action {} happens
    print(paste("  No pause yet..."))
    next #go to next line
  } 
  print("Found a pause!")
  if (i<5){
    print("But its less than five")
    next #go to next line
  }
  print(paste("Pause #", smple_cnt3))
  
  ppm_avg3 = mean(licor3$ppm[(i-5):i]) # calculates average of previous 5 lines
  
  meta3[smple_cnt3, 'time'] = substr(as.character(licor3$time[i]), 12, 19)
  meta3[smple_cnt3, 'ppm'] = ppm_avg3
  meta3[smple_cnt3, 'line'] = i
  
  smple_cnt3 = smple_cnt3 + 1
}

meta3$ppm.corr = meta3$ppm-mean(meta3$ppm[which(meta3$Sample.ID=="blank")])
#mean(meta$ppm[which(meta$soilmass==0)])#
#correct for blank
#replace "plot' with the column id name that you use if necessary
str(meta3)

#calculate actual time elapsed
meta3$start = as.POSIXct(meta3$start, format = "%H:%M")
meta3$end = as.POSIXct(meta3$end, format = "%H:%M")

meta3$days = (meta3$end-meta3$start)/(24*60*60)+1
meta3$ugC.L <- NA
meta3$ugC.L =as.numeric(meta3$ugC.L)
meta3$soilmass = as.numeric(meta3$soilmass)


meta3$ugC.L = (meta3$ppm.corr*12*1)/(.082058*298.15)
#calculate in ug CO2-C per L using PV=nRT

meta3$ugC.g.day = (meta3$ugC.L*.932)/meta3$soilmass/as.numeric(meta3$days)
meta3$ugC.g.day = ((meta3$ugC.L*.932))/ meta3$soil.mass
#calculate in ug CO2-C per g soil,
#which = mg C/kg soil
#.932 = L in jar
head(meta3)

write.csv(meta3, "Run3_calcsfull.csv",
          row.names = FALSE)







#### ReRun



#Run4

getwd()
dir() #what files are in my wd?
# tst=read.table(file.choose())#lets you pull files from folders

meta4 = read.csv("PMC4.csv", header=TRUE)
#sample data and approx times, sep="," for csv

str(meta4)

licor4 = read.table("Run4 Edit.txt", header = TRUE,
                    sep="\t", #tab-delim
                    skip = 1, #skip the first two lines
                    col.names = c("date", "time", "ppm", "temp","press",
                                  "abs", "voltage", "flow", "junk"))


str(licor4)#what kind of data? structure
## get date
licor4$date=as.Date(licor4$date, "%Y-%m-%d")
licor4$time = as.POSIXct(licor4$time, format = "%H:%M:%S")


smple_cnt4 = 1
for (i in 1:nrow(licor4)){
  print(paste("Working on",i)) #print the name of the row
  if (i == nrow(licor4)){ #on the last row, do something different
    print("  Last one...")
    print(paste("pause #", smple_cnt4))
    ppm_avg4 = mean(licor4$ppm[(i-5):i])
    #ppm_avg is the major output of this code
    #it tells you, what did  the licor find the average ppm right beforethe pause
    
    meta4[smple_cnt4, 'time'] = substr(as.character(licor4$time[i]), 12, 19)
    meta4[smple_cnt4, 'ppm'] = ppm_avg4
    next
  }
  no_pause4 = as.numeric(difftime(licor4$time[i+1], licor4$time[i], units="secs"))
  #line above , "no_pause", calculates the difference between each line
  if (no_pause4<=4) { #if () is trure, then the action {} happens
    print(paste("  No pause yet..."))
    next #go to next line
  } 
  print("Found a pause!")
  if (i<5){
    print("But its less than five")
    next #go to next line
  }
  print(paste("Pause #", smple_cnt4))
  
  ppm_avg4 = mean(licor4$ppm[(i-5):i]) # calculates average of previous 5 lines
  
  meta4[smple_cnt4, 'time'] = substr(as.character(licor4$time[i]), 12, 19)
  meta4[smple_cnt4, 'ppm'] = ppm_avg4
  meta4[smple_cnt4, 'line'] = i
  
  smple_cnt4 = smple_cnt4 + 1
}

meta4$ppm.corr = meta4$ppm-mean(meta4$ppm[which(meta4$sample.id=="Blank")])
# mean(meta$ppm[which(meta$soilmass==0)])#
#correct for blank
#replace "plot' with the column id name that you use if necessary
str(meta4)

#calculate actual time elapsed
meta4$start = as.POSIXct(meta4$start, format = "%H:%M")
meta4$end = as.POSIXct(meta4$end, format = "%H:%M")

meta4$days = (meta4$end-meta4$start)/(24*60*60)+1
meta4$ugC.L <- NA
meta4$ugC.L =as.numeric(meta4$ugC.L)
meta4$soilmass = as.numeric(meta4$soilmass)

meta4$ugC.L = (meta4$ppm.corr*12*1)/(.082058*298.15)
#calculate in ug CO2-C per L using PV=nRT

meta4$ugC.g.day = (meta4$ugC.L*.932)/meta4$soilmass/as.numeric(meta4$days)

meta4$ugC.g.day = ((meta4$ugC.L*.932))/ meta4$soil.mass
#calculate in ug CO2-C per g soil,
#which = mg C/kg soil
#.932 = L in jar
head(meta4)

write.csv(meta4, "Run4_calcsfull_edit.csv",
          row.names = FALSE)

#----Step 2----
#--------------#

###Combining all complete .csv's into one document with treatment columns

####################Start Here#####################



Run1=read.csv("Run1_calcsfull.csv")
Run2=read.csv("Run2_calcsfull.csv")
Run3=read.csv("Run3_calcsfull.csv")
#Run4=read.csv("Run4_calcsfull.csv")
#Run5=read.csv("Run5_calcsfull.csv")

trtcode = read.csv("PMC_MDACC2025_Treatment_Codes.csv") 
str(trtcode)
str(Run1)    

#Merge all runs when complete 
mergeAll <- rbind(Run1,Run2,Run3)
head(mergeAll)
mergeClean <- select(mergeAll, c(Plot,Sample.ID,ugC.g.day))

#For practice: Clean=select(Run1,plot,sample.id,ugC.g.day)


write.csv(mergeClean, "PMC_calcs_17Feb26.csv",
          row.names = FALSE)


#### Adapting Hava's PMC variablity, and coefficient of variation (CV)

#read in cleaned data from main directory 
all.data<-read.csv("PMC_calcs_17Feb26_edited.csv", stringsAsFactors = FALSE) 

num <- all.data$number 

#Calculate mean ugC.g.day per sample 
#Calculates mean of 3 technical replicates, which now share the same sample.id for easy grouping. 
no.NA = all.data[!is.na(all.data$ugC.g.day),]

full.means <- all.data %>% 
  group_by(Sample.ID) %>%
  summarize(mean.resp = mean(ugC.g.day, na.rm = T))

#Calculate coefficient of variation (CV) 
#CV is standard deviation divided by mean, expressed as a percent. 
add.cv <- no.NA %>% 
  group_by(Sample.ID) %>% 
  summarize(sample.cv = sd(ugC.g.day, na.rm = TRUE)/mean(ugC.g.day, na.rm = TRUE)*100)

#merge sample mean and cv data 
mean.cv <- merge(full.means, add.cv) 

#bring in full sample names from the name master. 
sample.id <- read.csv("PMC_MDACC2025_Treatment_Codes.csv", 
                      stringsAsFactors = FALSE)

#Join the mean and variability dataset with the sample name key 
pmc.var <- full_join(sample.id, mean.cv, by = "Sample.ID") %>%
  distinct(Sample.ID, .keep_all = TRUE)

#keep only samples that have CV > 0. 
pmc.var <- subset(pmc.var, sample.cv > 0) 

write.csv(pmc.var, "PMC_calcsCV_17Feb26.csv")

##now look at the document for samples that need to be looked into further. 
###example
###if CV > 20, maybe one of the samples is an outlire, once deleted no longer on the list 

write.csv(missing, "CCNRD_missing.csv", row.names = FALSE)