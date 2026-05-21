#Make a path file to guide to data files

#set data storage directory
dir <- "/home/elle_nbogen/Schreibtisch/master mikrobiome/soil ecology/myko/data_storage"

#Set raw directory
raw <- paste0(dir, "/raw/")

#Add individual files
ecm.label.string <- paste0(raw, "label.ecm.csv")
obs.ecm.string <- paste0(raw, "obs.ecm.csv")

#Set processed directory
proc <- paste0(dir, "/processed/")
