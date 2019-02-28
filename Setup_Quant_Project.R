##################
#    Created By: #
#     Alex       #
#    Berkout     #
# AND            #
#     Coen       #
#   van der      #
#     Geest      #
##################

# Load csv file into R/ change directory if necessary
QP4 = read.csv('D://Quant_Data/google_play_data.csv', stringsAsFactors = FALSE, sep = ";")

#If needed remove NA values from dataset
final[complete.cases(final), ]

#gives an overview of the df, type of values in a column 
    # app_version and price need attention
        # both seen as character needs to be changed to numerical (some values currently contain dollarsigns $)
str(QP4)
