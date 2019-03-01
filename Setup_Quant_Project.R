##################
#    Created By: #
#     Alex       #
#    Berkout     #
# AND            #
#     Coen       #
#   van der      #
#     Geest      #
##################

library(tidyverse)
library(lubridate)
library(dummies)

# Load csv file into R/ change directory if necessary
QP4 = read.csv('D://Quant_Data/google_play_data.csv', stringsAsFactors = FALSE, sep = ";")

#If needed remove NA values from dataset
#final[complete.cases(final), ]

#gives an overview of the df, type of values in a column 
    # app_version and price need attention
        # both seen as character needs to be changed to numerical (some values currently contain dollarsigns $)
str(QP4)

#Remove + at the end of the intalls column
QP4$intalls = sub('[+]', '', QP4$intalls)

#Remove dollar sign at the beginning of price column
QP4$price = sub('[$]', '', QP4$price)

# remove 'varies with device' from file_size column
## Remove 'k' at the end of file size (400k) to (400)
### subset values in filesize which contain M to later multiply by 1000
QP4[QP4 == 'Varies with device'] <- NA
QP4$file_size = sub('[k]', '', QP4$file_size)
QP4_m = QP4[grepl("M", QP4[["file_size"]]), ]
#remove m add the end of value in the subset
QP4_m$file_size = sub('[M]', '', QP4_m$file_size)
#change column type to numerical before multiplication in subset
QP4_m$file_size = as.numeric(QP4_m$file_size)
#Multiply the values in Filesize in the subset
QP4_m$file_size = QP4_m$file_size * 1000
#Remove M at the end of an value in main df
QP4$file_size = sub('[M]', '', QP4$file_size)
# change the column type in the main df (QP4) to numerical before joining with subset
QP4$file_size = as.numeric(QP4$file_size)
#join df based on ID and overwriting the values in QP4 with the values of QP4_m when not matching
QP4 = anti_join(QP4, QP4_m, by = "id") %>% bind_rows(QP4_m)

# Change NaN in Rating to NA
QP4$rating[QP4$rating == 'NaN'] <- NA

#convert last updated into date variable
QP4$last_updated = as.Date(mdy(QP4$last_updated))

#change category into a factor
## based on the factor create new columns with a 0 or 1
### rename the columns
QP4$category = as.factor(QP4$category)
QP4 = cbind(QP4, dummy(QP4$category, sep = "_"))
colnames(QP4) <- gsub("QP4_", "cat_", fixed = TRUE, colnames(QP4))

#Create new subset with permissions 
Permissions <- c("permissions")
PERM_QP4 <- QP4[Permissions]

#Create new variable representing permission and extract found permission
PERM_QP4$read_sensitive_log_data <- str_extract(PERM_QP4$permissions, "read sensitive log data")

#Transform found permission in '1'
PERM_QP4[PERM_QP4 == "read sensitive log data"] <- 1

#transform 'Na' into '0'
PERM_QP4$read_sensitive_log_data[is.na(PERM_QP4$read_sensitive_log_data)] <- 0 
