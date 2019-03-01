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

#Remove 'M' and 'k' at the end of file size (9.9M) to (9.9)
QP4$file_size = sub('[M]', '', QP4$file_size)
QP4$file_size = sub('[k]', '', QP4$file_size)

#replaces 'varies with device' in the file_size and app_version to NA And change NaN in Rating to NA
QP4[QP4 == 'Varies with device'] <- NA
QP4$rating[QP4$rating == 'NaN'] <- NA

#convert last updated into date variable
QP4$last_updated = as.Date(mdy(QP4$last_updated))
