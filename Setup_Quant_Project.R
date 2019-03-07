#################
#  VU AMSTERDAM #
#               #
#  Created By:  #
#     Alex      #
#    Berkout    #
# AND           #
#     Coen      #
#   van der     #
#     Geest     #
#################

#this code is free the use under the condition to attribute the original authors by refering the following:
#Berkhout, A.T., & van de Geest, C. (2019) R Google Play Store Permissons Extractor, version 1.0, R Script, https://github.com/Snat3r/Quant_P4

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

#Transform found permission in '1'
PERM_QP4[PERM_QP4 == "read sensitive log data"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "retrieve system internal state"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "read your Web bookmarks and history"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "write web bookmarks and history"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "retrieve running apps"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "prevent device from sleeping"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "receive data from Internet"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "run at startup"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "control vibration"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "read battery statistics"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "send sticky broadcast"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "delete all app cache data"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "expand/collapse status bar"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "disable or modify status bar"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "control flashlight"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "close other apps"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "read sync settings"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "draw over other apps"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "modify system settings"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "toggle sync on and off"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "set an alarm"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "uninstall shortcuts"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "install shortcuts"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "read Home settings and shortcuts"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "write Home settings and shortcuts"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "download files without notification"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "use accounts on the device"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "view configured accounts YouTube"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "create accounts and set passwords"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "mock location sources for testing"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "disable your screen lock"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "modify app ops statistics"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "change your audio settings"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "read Google service configuration"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "control Near Field Communication"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "Google Play license check"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "Update component usage statistics"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "change system display settings"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "Broadcast data messages to apps"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "make/receive SIP calls"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "modify battery statistics"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "reorder running apps"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "Access email provider data"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "limit number of running processes"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "act as the AccountManagerService"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "read sync statistics"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "bind to a notification listener service"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "Send download notifications."] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "Access download manager."] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "full license to interact across users  interact across users"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "interact across users"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "measure app storage space"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "bind to NFC service"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "enable app debugging"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "add words to user-defined dictionary"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "manage users"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "Modify Google service configuration"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "power device on or off"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "delete apps"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "delete other apps data"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "directly install apps"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "modify secure system settings"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "set time zone"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "control system backup and restore"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "run the applications scheduled background work"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "read TV channel/program information"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "write TV channel/program information"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "capture video output"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "capture audio output"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "control location update notifications"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "change screen orientation"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "control media playback and metadata access"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "transmit infrared"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "read terms you added to the dictionary"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "modify global animation speed"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "bind to a wallpaper"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "Read email attachments"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "access SurfaceFlinger"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "view configured accounts"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "access all Google services"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "access other Google services"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "Google Docs"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "Google Spreadsheets Google Docs"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "Google mail"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "Google Finance"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "Google Voice"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "bind to an accessibility service"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "prevent app switches"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "delete other apps caches"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "force device reboot"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "retrieve app ops statistics"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "use any media decoder for playback"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "enable or disable app components"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "press keys and control buttons"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "capture secure video output"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "read frame buffer"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "bind to a widget service"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "test hardware"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "display unauthorized windows"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "interact with a device admin"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "Access keyguard secure storage"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "Control displaying and hiding keyguard"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "freeze screen"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "access checkin properties"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "send SMS-received broadcast"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "WAP-PUSH-received broadcast"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "SmartcardServicePermission label"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "get current app info"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "run in factory test mode"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "send respond-via-message events"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "force stop other apps"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "read/write to resources owned by diag record what you type and actions you take"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "reset system to factory defaults"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "access the cache filesystem"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "view network connections"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "full network access"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "change network connectivity"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "change/intercept network settings and traffic"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "read historical network usage"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "find accounts on the device"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "read your own contact card"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "modify your own contact card"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "add or remove accounts"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "read your contacts"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "modify your contacts"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "read calendar events plus confidential information"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "add or modify calendar events and send email to guests without owners knowledge"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "approximate location (network-based)"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "precise location (GPS and network-based)"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "access extra location provider commands"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "receive text messages (SMS)"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "read your text messages (SMS or MMS)"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "receive text messages (MMS)	"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "edit your text messages (SMS or MMS)"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "send SMS messages"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "receive text messages (WAP)"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "MMS Wakeup"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "directly call phone numbers"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "write call log"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "read call log"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "reroute outgoing calls"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "modify phone state"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "directly call any phone numbers"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "read precise phone states"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "read voicemail"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "write voicemail"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "read the contents of your USB storage"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "modify or delete the contents of your USB storage"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "access USB storage filesystem"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "set wallpaper"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "adjust your wallpaper size"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "manage document storage"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "modify/delete internal media storage contents"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "erase USB storage"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "Access all system downloads"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "take pictures and videos"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "record audio"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "view Wi-Fi connections"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "connect and disconnect from Wi-Fi"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "allow Wi-Fi Multicast reception"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "pair with Bluetooth devices"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "allow Bluetooth pairing by Application"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "access Bluetooth settings"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "body sensors (like heart rate monitors)"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "read phone status and identity"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "read your social stream"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "write to your social stream"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "read subscribed feeds"] <- as.numeric(1)
PERM_QP4[PERM_QP4 == "write subscribed feeds"] <- as.numeric(1)

#Transform na in '0'
PERM_QP4[is.na(PERM_QP4)] <- as.numeric(0) 

#Delete string variable permissions
PERM_QP4$permissions <- NULL

#save permissions subset as CSV
write.csv(PERM_QP4, file = "PERM_QP4_FILTERED_NUMERIC.csv")
