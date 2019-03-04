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
PERM_QP4$	read_sensitive_log_data	<- str_extract(PERM_QP4$permissions, "read sensitive log data")
PERM_QP4$	retrieve_system_internal_state	<- str_extract(PERM_QP4$permissions, "retrieve system internal state")
PERM_QP4$	read_your_Web_bookmarks_and_history	<- str_extract(PERM_QP4$permissions, "read your Web bookmarks and history")
PERM_QP4$	write_web_bookmarks_and_history	<- str_extract(PERM_QP4$permissions, "write web bookmarks and history")
PERM_QP4$	retrieve_running_apps	<- str_extract(PERM_QP4$permissions, "retrieve running apps")
PERM_QP4$	prevent_device_from_sleeping	<- str_extract(PERM_QP4$permissions, "prevent device from sleeping")
PERM_QP4$	receive_data_from_Internet	<- str_extract(PERM_QP4$permissions, "receive data from Internet")
PERM_QP4$	run_at_startup	<- str_extract(PERM_QP4$permissions, "run at startup")
PERM_QP4$	control_vibration	<- str_extract(PERM_QP4$permissions, "control vibration")
PERM_QP4$	read_battery_statistics	<- str_extract(PERM_QP4$permissions, "read battery statistics")
PERM_QP4$	send_sticky_broadcast	<- str_extract(PERM_QP4$permissions, "send sticky broadcast")
PERM_QP4$	delete_all_app_cache_data	<- str_extract(PERM_QP4$permissions, "delete all app cache data")
PERM_QP4$	expand_collapse_status_bar	<- str_extract(PERM_QP4$permissions, "expand/collapse status bar")
PERM_QP4$	disable_or_modify_status_bar	<- str_extract(PERM_QP4$permissions, "disable or modify status bar")
PERM_QP4$	control_flashlight	<- str_extract(PERM_QP4$permissions, "control flashlight")
PERM_QP4$	close_other_apps	<- str_extract(PERM_QP4$permissions, "close other apps")
PERM_QP4$	read_sync_settings	<- str_extract(PERM_QP4$permissions, "read sync settings")
PERM_QP4$	draw_over_other_apps	<- str_extract(PERM_QP4$permissions, "draw over other apps")
PERM_QP4$	modify_system_settings	<- str_extract(PERM_QP4$permissions, "modify system settings")
PERM_QP4$	toggle_sync_on_and_off <- str_extract(PERM_QP4$permissions, "toggle sync on and off")
PERM_QP4$	set_an_alarm	<- str_extract(PERM_QP4$permissions, "set an alarm")
PERM_QP4$	uninstall_shortcuts	<- str_extract(PERM_QP4$permissions, "uninstall shortcuts")
PERM_QP4$	install_shortcuts	<- str_extract(PERM_QP4$permissions, "install shortcuts")
PERM_QP4$	read_Home_settings_and_shortcuts	<- str_extract(PERM_QP4$permissions, "read Home settings and shortcuts")
PERM_QP4$	write_Home_settings_and_shortcuts	<- str_extract(PERM_QP4$permissions, "write Home settings and shortcuts")
PERM_QP4$	download_files_without_notification	<- str_extract(PERM_QP4$permissions, "download files without notification")
PERM_QP4$	use_accounts_on_the_device	<- str_extract(PERM_QP4$permissions, "use accounts on the device")
PERM_QP4$	view_configured_accounts_YouTube	<- str_extract(PERM_QP4$permissions, "view configured accounts YouTube")
PERM_QP4$	create_accounts_and_set_passwords	<- str_extract(PERM_QP4$permissions, "create accounts and set passwords")
PERM_QP4$	mock_location_sources_for_testing	<- str_extract(PERM_QP4$permissions, "mock location sources for testing")
PERM_QP4$	disable_your_screen_lock	<- str_extract(PERM_QP4$permissions, "disable your screen lock")
PERM_QP4$	modify_app_ops_statistics	<- str_extract(PERM_QP4$permissions, "modify app ops statistics")
PERM_QP4$	change_your_audio_settings	<- str_extract(PERM_QP4$permissions, "change your audio settings")
PERM_QP4$	read_Google_service_configuration	<- str_extract(PERM_QP4$permissions, "read Google service configuration")
PERM_QP4$	control_Near_Field_Communication	<- str_extract(PERM_QP4$permissions, "control Near Field Communication")
PERM_QP4$	Google_Play_license_check	<- str_extract(PERM_QP4$permissions, "Google Play license check")
PERM_QP4$	Update_component_usage_statistics	<- str_extract(PERM_QP4$permissions, "Update component usage statistics")
PERM_QP4$	change_system_display_settings	<- str_extract(PERM_QP4$permissions, "change system display settings")
PERM_QP4$	Broadcast_data_messages_to_apps	<- str_extract(PERM_QP4$permissions, "Broadcast data messages to apps")
PERM_QP4$	make_receive_SIP_calls	<- str_extract(PERM_QP4$permissions, "make/receive SIP calls")
PERM_QP4$	modify_battery_statistics	<- str_extract(PERM_QP4$permissions, "modify battery statistics")
PERM_QP4$	reorder_running_apps	<- str_extract(PERM_QP4$permissions, "reorder running apps")
PERM_QP4$	Access_email_provider_data	<- str_extract(PERM_QP4$permissions, "Access email provider data")
PERM_QP4$	limit_number_of_running_processes	<- str_extract(PERM_QP4$permissions, "limit number of running processes")
PERM_QP4$	act_as_the_AccountManagerService	<- str_extract(PERM_QP4$permissions, "act as the AccountManagerService")
PERM_QP4$	read_sync_statistics	<- str_extract(PERM_QP4$permissions, "read sync statistics")
PERM_QP4$	bind_to_a_notification_listener_service	<- str_extract(PERM_QP4$permissions, "bind to a notification listener service")
PERM_QP4$	Send_download_notifications.	<- str_extract(PERM_QP4$permissions, "Send download notifications.")
PERM_QP4$	Access_download_manager.	<- str_extract(PERM_QP4$permissions, "Access download manager.")
PERM_QP4$	full_license_to_interact_across_users__interact_across_users	<- str_extract(PERM_QP4$permissions, "full license to interact across users  interact across users")
PERM_QP4$	interact_across_users	<- str_extract(PERM_QP4$permissions, "interact across users")
PERM_QP4$	measure_app_storage_space	<- str_extract(PERM_QP4$permissions, "measure app storage space")
PERM_QP4$	bind_to_NFC_service	<- str_extract(PERM_QP4$permissions, "bind to NFC service")
PERM_QP4$	enable_app_debugging	<- str_extract(PERM_QP4$permissions, "enable app debugging")
PERM_QP4$	add_words_to_user_defined_dictionary	<- str_extract(PERM_QP4$permissions, "add words to user-defined dictionary")
PERM_QP4$	manage_users	<- str_extract(PERM_QP4$permissions, "manage users")
PERM_QP4$	Modify_Google_service_configuration	<- str_extract(PERM_QP4$permissions, "Modify Google service configuration")
PERM_QP4$	power_device_on_or_off	<- str_extract(PERM_QP4$permissions, "power device on or off")
PERM_QP4$	delete_apps	<- str_extract(PERM_QP4$permissions, "delete apps")
PERM_QP4$	delete_other_apps_data	<- str_extract(PERM_QP4$permissions, "delete other apps data")
PERM_QP4$	directly_install_apps	<- str_extract(PERM_QP4$permissions, "directly install apps")
PERM_QP4$	modify_secure_system_settings	<- str_extract(PERM_QP4$permissions, "modify secure system settings")
PERM_QP4$	set_time_zone	<- str_extract(PERM_QP4$permissions, "set time zone")
PERM_QP4$	control_system_backup_and_restore	<- str_extract(PERM_QP4$permissions, "control system backup and restore")
PERM_QP4$	run_the_applications_scheduled_background_work	<- str_extract(PERM_QP4$permissions, "run the applications scheduled background work")
PERM_QP4$	read_TV_channel_program_information	<- str_extract(PERM_QP4$permissions, "read TV channel/program information")
PERM_QP4$	write_TV_channel_program_information <- str_extract(PERM_QP4$permissions, "write TV channel/program information")
PERM_QP4$	capture_video_output	<- str_extract(PERM_QP4$permissions, "capture video output")
PERM_QP4$	capture_audio_output	<- str_extract(PERM_QP4$permissions, "capture audio output")
PERM_QP4$	control_location_update_notifications	<- str_extract(PERM_QP4$permissions, "control location update notifications")
PERM_QP4$	change_screen_orientation	<- str_extract(PERM_QP4$permissions, "change screen orientation	")
PERM_QP4$	control_media_playback_and_metadata_access	<- str_extract(PERM_QP4$permissions, "control media playback and metadata access")
PERM_QP4$	transmit_infrared	<- str_extract(PERM_QP4$permissions, "transmit infrared	")
PERM_QP4$	read_terms_you_added_to_the_dictionary	<- str_extract(PERM_QP4$permissions, "read terms you added to the dictionary")
PERM_QP4$	modify_global_animation_speed	<- str_extract(PERM_QP4$permissions, "modify global animation speed")
PERM_QP4$	bind_to_a_wallpaper	<- str_extract(PERM_QP4$permissions, "bind to a wallpaper")
PERM_QP4$	Read_email_attachments	<- str_extract(PERM_QP4$permissions, "Read email attachments")
PERM_QP4$	access_SurfaceFlinger	<- str_extract(PERM_QP4$permissions, "access SurfaceFlinger")
PERM_QP4$	view_configured_accounts	<- str_extract(PERM_QP4$permissions, "view configured accounts")
PERM_QP4$	access_all_Google_services	<- str_extract(PERM_QP4$permissions, "access all Google services")
PERM_QP4$	access_other_Google_services	<- str_extract(PERM_QP4$permissions, "access other Google services")
PERM_QP4$	Google_Docs	<- str_extract(PERM_QP4$permissions, "Google Docs")
PERM_QP4$	Google_Spreadsheets_Google_Docs	<- str_extract(PERM_QP4$permissions, "Google Spreadsheets Google Docs")
PERM_QP4$	Google_mail	<- str_extract(PERM_QP4$permissions, "Google mail")
PERM_QP4$	Google_Finance	<- str_extract(PERM_QP4$permissions, "Google Finance")
PERM_QP4$	Google_Voice	<- str_extract(PERM_QP4$permissions, "Google Voice")
PERM_QP4$	bind_to_an_accessibility_service	<- str_extract(PERM_QP4$permissions, "bind to an accessibility service")
PERM_QP4$	prevent_app_switches	<- str_extract(PERM_QP4$permissions, "prevent app switches")
PERM_QP4$	delete_other_apps_caches	<- str_extract(PERM_QP4$permissions, "delete other apps caches")
PERM_QP4$	force_device_reboot	<- str_extract(PERM_QP4$permissions, "force device reboot")
PERM_QP4$	retrieve_app_ops_statistics	<- str_extract(PERM_QP4$permissions, "retrieve app ops statistics")
PERM_QP4$	use_any_media_decoder_for_playback	<- str_extract(PERM_QP4$permissions, "use any media decoder for playback")
PERM_QP4$	enable_or_disable_app_components	<- str_extract(PERM_QP4$permissions, "enable or disable app components")
PERM_QP4$	press_keys_and_control_buttons	<- str_extract(PERM_QP4$permissions, "press keys and control buttons")
PERM_QP4$	capture_secure_video_output	<- str_extract(PERM_QP4$permissions, "capture secure video output")
PERM_QP4$	read_frame_buffer	<- str_extract(PERM_QP4$permissions, "read frame buffer")
PERM_QP4$	bind_to_a_widget_service	<- str_extract(PERM_QP4$permissions, "bind to a widget service")
PERM_QP4$	test_hardware	<- str_extract(PERM_QP4$permissions, "test hardware")
PERM_QP4$	display_unauthorized_windows	<- str_extract(PERM_QP4$permissions, "display unauthorized windows")
PERM_QP4$	interact_with_a_device_admin	<- str_extract(PERM_QP4$permissions, "interact with a device admin")
PERM_QP4$	Access_keyguard_secure_storage	<- str_extract(PERM_QP4$permissions, "Access keyguard secure storage")
PERM_QP4$	Control_displaying_and_hiding_keyguard	<- str_extract(PERM_QP4$permissions, "Control displaying and hiding keyguard")
PERM_QP4$	freeze_screen	<- str_extract(PERM_QP4$permissions, "freeze screen")
PERM_QP4$	access_checkin_properties	<- str_extract(PERM_QP4$permissions, "access checkin properties")
PERM_QP4$	send_SMS_received_broadcast	<- str_extract(PERM_QP4$permissions, "send SMS-received broadcast")
PERM_QP4$	WAP_PUSH_received_broadcast	<- str_extract(PERM_QP4$permissions, "WAP-PUSH-received broadcast")
PERM_QP4$	SmartcardServicePermission_label	<- str_extract(PERM_QP4$permissions, "SmartcardServicePermission label")
PERM_QP4$	get_current_app_info	<- str_extract(PERM_QP4$permissions, "get current app info")
PERM_QP4$	run_in_factory_test_mode	<- str_extract(PERM_QP4$permissions, "run in factory test mode")
PERM_QP4$	send_respond_via_message_events	<- str_extract(PERM_QP4$permissions, "send respond-via-message events")
PERM_QP4$	force_stop_other_apps	<- str_extract(PERM_QP4$permissions, "force stop other apps")
PERM_QP4$	read_write_to_resources_owned_by_diag_record_what_you_type_and_actions_you_take	<- str_extract(PERM_QP4$permissions, "read/write to resources owned by diag record what you type and actions you take")
PERM_QP4$	reset_system_to_factory_defaults	<- str_extract(PERM_QP4$permissions, "reset system to factory defaults")
PERM_QP4$	access_the_cache_filesystem	<- str_extract(PERM_QP4$permissions, "access the cache filesystem")
PERM_QP4$	view_network_connections	<- str_extract(PERM_QP4$permissions, "view network connections")
PERM_QP4$	full_network_access	<- str_extract(PERM_QP4$permissions, "full network access")
PERM_QP4$	change_network_connectivity	<- str_extract(PERM_QP4$permissions, "change network connectivity")
PERM_QP4$	change_intercept_network_settings_and_traffic	<- str_extract(PERM_QP4$permissions, "change/intercept network settings and traffic")
PERM_QP4$	read_historical_network_usage	<- str_extract(PERM_QP4$permissions, "read historical network usage")
PERM_QP4$	find_accounts_on_the_device	<- str_extract(PERM_QP4$permissions, "find accounts on the device")
PERM_QP4$	read_your_own_contact_card	<- str_extract(PERM_QP4$permissions, "read your own contact card")
PERM_QP4$	modify_your_own_contact_card	<- str_extract(PERM_QP4$permissions, "modify your own contact card")
PERM_QP4$	add_or_remove_accounts	<- str_extract(PERM_QP4$permissions, "add or remove accounts")
PERM_QP4$	read_your_contacts	<- str_extract(PERM_QP4$permissions, "read your contacts")
PERM_QP4$	modify_your_contacts	<- str_extract(PERM_QP4$permissions, "modify your contacts")
PERM_QP4$	read_calendar_events_plus_confidential_information	<- str_extract(PERM_QP4$permissions, "read calendar events plus confidential information")
PERM_QP4$	add_or_modify_calendar_events_and_send_email_to_guests_without_owners_knowledge	<- str_extract(PERM_QP4$permissions, "add or modify calendar events and send email to guests without owners knowledge")
PERM_QP4$	approximate_location_network_based	<- str_extract(PERM_QP4$permissions, "approximate location (network-based)")
PERM_QP4$	precise_location_GPS_and_network_based	<- str_extract(PERM_QP4$permissions, "precise location (GPS and network-based)")
PERM_QP4$	access_extra_location_provider_commands	<- str_extract(PERM_QP4$permissions, "access extra location provider commands")
PERM_QP4$	receive_text_messages_SMS	<- str_extract(PERM_QP4$permissions, "receive text messages (SMS)")
PERM_QP4$	read_your_text_messages_SMS_or_MMS	<- str_extract(PERM_QP4$permissions, "read your text messages (SMS or MMS)")
PERM_QP4$	receive_text_messages_MMS	<- str_extract(PERM_QP4$permissions, "receive text messages (MMS)	")
PERM_QP4$	edit_your_text_messages_SMS_or_MMS	<- str_extract(PERM_QP4$permissions, "edit your text messages (SMS or MMS)	")
PERM_QP4$	send_SMS_messages	<- str_extract(PERM_QP4$permissions, "send SMS messages")
PERM_QP4$	receive_text_messages_WAP	<- str_extract(PERM_QP4$permissions, "receive text messages (WAP)")
PERM_QP4$	MMS_Wakeup	<- str_extract(PERM_QP4$permissions, "MMS Wakeup")
PERM_QP4$	directly_call_phone_numbers	<- str_extract(PERM_QP4$permissions, "directly call phone numbers")
PERM_QP4$	write_call_log	<- str_extract(PERM_QP4$permissions, "write call log")
PERM_QP4$	read_call_log	<- str_extract(PERM_QP4$permissions, "read call log")
PERM_QP4$	reroute_outgoing_calls	<- str_extract(PERM_QP4$permissions, "reroute outgoing calls")
PERM_QP4$	modify_phone_state	<- str_extract(PERM_QP4$permissions, "modify phone state")
PERM_QP4$	directly_call_any_phone_numbers	<- str_extract(PERM_QP4$permissions, "directly call any phone numbers")
PERM_QP4$	read_precise_phone_states	<- str_extract(PERM_QP4$permissions, "read precise phone states")
PERM_QP4$	read_voicemail	<- str_extract(PERM_QP4$permissions, "read voicemail")
PERM_QP4$	write_voicemail	<- str_extract(PERM_QP4$permissions, "write voicemail")
PERM_QP4$	read_the_contents_of_your_USB_storage	<- str_extract(PERM_QP4$permissions, "read the contents of your USB storage")
PERM_QP4$	modify_or_delete_the_contents_of_your_USB_storage	<- str_extract(PERM_QP4$permissions, "modify or delete the contents of your USB storage")
PERM_QP4$	access_USB_storage_filesystem	<- str_extract(PERM_QP4$permissions, "access USB storage filesystem")
PERM_QP4$	set_wallpaper	<- str_extract(PERM_QP4$permissions, "set wallpaper	")
PERM_QP4$	adjust_your_wallpaper_size	<- str_extract(PERM_QP4$permissions, "adjust your wallpaper size")
PERM_QP4$	manage_document_storage	<- str_extract(PERM_QP4$permissions, "manage document storage")
PERM_QP4$	modify_delete_internal_media_storage_contents	<- str_extract(PERM_QP4$permissions, "modify/delete internal media storage contents")
PERM_QP4$	erase_USB_storage	<- str_extract(PERM_QP4$permissions, "erase USB storage")
PERM_QP4$	Access_all_system_downloads	<- str_extract(PERM_QP4$permissions, "Access all system downloads")
PERM_QP4$	take_pictures_and_videos	<- str_extract(PERM_QP4$permissions, "take pictures and videos")
PERM_QP4$	record_audio	<- str_extract(PERM_QP4$permissions, "record audio")
PERM_QP4$	view_Wi_Fi_connections	<- str_extract(PERM_QP4$permissions, "view Wi-Fi connections")
PERM_QP4$	connect_and_disconnect_from_Wi_Fi	<- str_extract(PERM_QP4$permissions, "connect and disconnect from Wi-Fi")
PERM_QP4$	allow_Wi_Fi_Multicast_reception	<- str_extract(PERM_QP4$permissions, "allow Wi-Fi Multicast reception")
PERM_QP4$	pair_with_Bluetooth_devices	<- str_extract(PERM_QP4$permissions, "pair with Bluetooth devices")
PERM_QP4$	allow_Bluetooth_pairing_by_Application	<- str_extract(PERM_QP4$permissions, "allow Bluetooth pairing by Application")
PERM_QP4$	access_Bluetooth_settings	<- str_extract(PERM_QP4$permissions, "access Bluetooth settings")
PERM_QP4$	body_sensors__like_heart_rate_monitors	<- str_extract(PERM_QP4$permissions, "body sensors (like heart rate monitors)")
PERM_QP4$	read_phone_status_and_identity	<- str_extract(PERM_QP4$permissions, "read phone status and identity")
PERM_QP4$	read_your_social_stream	<- str_extract(PERM_QP4$permissions, "read your social stream")
PERM_QP4$	write_to_your_social_stream	<- str_extract(PERM_QP4$permissions, "write to your social stream")
PERM_QP4$	read_subscribed_feeds	<- str_extract(PERM_QP4$permissions, "read subscribed feeds	")
PERM_QP4$	write_subscribed_feeds	<- str_extract(PERM_QP4$permissions, "write subscribed feeds")

#Transform found permission in '1'
PERM_QP4[PERM_QP4 == "read sensitive log data"] <- 1
PERM_QP4[PERM_QP4 == "retrieve system internal state"] <- 1
PERM_QP4[PERM_QP4 == "read your Web bookmarks and history"] <- 1
PERM_QP4[PERM_QP4 == "write web bookmarks and history"] <- 1
PERM_QP4[PERM_QP4 == "retrieve running apps"] <- 1
PERM_QP4[PERM_QP4 == "prevent device from sleeping"] <- 1
PERM_QP4[PERM_QP4 == "receive data from Internet"] <- 1
PERM_QP4[PERM_QP4 == "run at startup"] <- 1
PERM_QP4[PERM_QP4 == "control vibration"] <- 1
PERM_QP4[PERM_QP4 == "read battery statistics"] <- 1
PERM_QP4[PERM_QP4 == "send sticky broadcast"] <- 1
PERM_QP4[PERM_QP4 == "delete all app cache data"] <- 1
PERM_QP4[PERM_QP4 == "expand/collapse status bar"] <- 1
PERM_QP4[PERM_QP4 == "disable or modify status bar"] <- 1
PERM_QP4[PERM_QP4 == "control flashlight"] <- 1
PERM_QP4[PERM_QP4 == "close other apps"] <- 1
PERM_QP4[PERM_QP4 == "read sync settings"] <- 1
PERM_QP4[PERM_QP4 == "draw over other apps"] <- 1
PERM_QP4[PERM_QP4 == "modify system settings"] <- 1
PERM_QP4[PERM_QP4 == "toggle sync on and off"] <- 1
PERM_QP4[PERM_QP4 == "set an alarm"] <- 1
PERM_QP4[PERM_QP4 == "uninstall shortcuts"] <- 1
PERM_QP4[PERM_QP4 == "install shortcuts"] <- 1
PERM_QP4[PERM_QP4 == "read Home settings and shortcuts"] <- 1
PERM_QP4[PERM_QP4 == "write Home settings and shortcuts"] <- 1
PERM_QP4[PERM_QP4 == "download files without notification"] <- 1
PERM_QP4[PERM_QP4 == "use accounts on the device"] <- 1
PERM_QP4[PERM_QP4 == "view configured accounts YouTube"] <- 1
PERM_QP4[PERM_QP4 == "create accounts and set passwords"] <- 1
PERM_QP4[PERM_QP4 == "mock location sources for testing"] <- 1
PERM_QP4[PERM_QP4 == "disable your screen lock"] <- 1
PERM_QP4[PERM_QP4 == "modify app ops statistics"] <- 1
PERM_QP4[PERM_QP4 == "change your audio settings"] <- 1
PERM_QP4[PERM_QP4 == "read Google service configuration"] <- 1
PERM_QP4[PERM_QP4 == "control Near Field Communication"] <- 1
PERM_QP4[PERM_QP4 == "Google Play license check"] <- 1
PERM_QP4[PERM_QP4 == "Update component usage statistics"] <- 1
PERM_QP4[PERM_QP4 == "change system display settings"] <- 1
PERM_QP4[PERM_QP4 == "Broadcast data messages to apps"] <- 1
PERM_QP4[PERM_QP4 == "make/receive SIP calls"] <- 1
PERM_QP4[PERM_QP4 == "modify battery statistics"] <- 1
PERM_QP4[PERM_QP4 == "reorder running apps"] <- 1
PERM_QP4[PERM_QP4 == "Access email provider data"] <- 1
PERM_QP4[PERM_QP4 == "limit number of running processes"] <- 1
PERM_QP4[PERM_QP4 == "act as the AccountManagerService"] <- 1
PERM_QP4[PERM_QP4 == "read sync statistics"] <- 1
PERM_QP4[PERM_QP4 == "bind to a notification listener service"] <- 1
PERM_QP4[PERM_QP4 == "Send download notifications."] <- 1
PERM_QP4[PERM_QP4 == "Access download manager."] <- 1
PERM_QP4[PERM_QP4 == "full license to interact across users  interact across users"] <- 1
PERM_QP4[PERM_QP4 == "interact across users"] <- 1
PERM_QP4[PERM_QP4 == "measure app storage space"] <- 1
PERM_QP4[PERM_QP4 == "bind to NFC service"] <- 1
PERM_QP4[PERM_QP4 == "enable app debugging"] <- 1
PERM_QP4[PERM_QP4 == "add words to user-defined dictionary"] <- 1
PERM_QP4[PERM_QP4 == "manage users"] <- 1
PERM_QP4[PERM_QP4 == "Modify Google service configuration"] <- 1
PERM_QP4[PERM_QP4 == "power device on or off"] <- 1
PERM_QP4[PERM_QP4 == "delete apps"] <- 1
PERM_QP4[PERM_QP4 == "delete other apps data"] <- 1
PERM_QP4[PERM_QP4 == "directly install apps"] <- 1
PERM_QP4[PERM_QP4 == "modify secure system settings"] <- 1
PERM_QP4[PERM_QP4 == "set time zone"] <- 1
PERM_QP4[PERM_QP4 == "control system backup and restore"] <- 1
PERM_QP4[PERM_QP4 == "run the applications scheduled background work"] <- 1
PERM_QP4[PERM_QP4 == "read TV channel/program information"] <- 1
PERM_QP4[PERM_QP4 == "write TV channel/program information"] <- 1
PERM_QP4[PERM_QP4 == "capture video output"] <- 1
PERM_QP4[PERM_QP4 == "capture audio output"] <- 1
PERM_QP4[PERM_QP4 == "control location update notifications"] <- 1
PERM_QP4[PERM_QP4 == "change screen orientation"] <- 1
PERM_QP4[PERM_QP4 == "control media playback and metadata access"] <- 1
PERM_QP4[PERM_QP4 == "transmit infrared"] <- 1
PERM_QP4[PERM_QP4 == "read terms you added to the dictionary"] <- 1
PERM_QP4[PERM_QP4 == "modify global animation speed"] <- 1
PERM_QP4[PERM_QP4 == "bind to a wallpaper"] <- 1
PERM_QP4[PERM_QP4 == "Read email attachments"] <- 1
PERM_QP4[PERM_QP4 == "access SurfaceFlinger"] <- 1
PERM_QP4[PERM_QP4 == "view configured accounts"] <- 1
PERM_QP4[PERM_QP4 == "access all Google services"] <- 1
PERM_QP4[PERM_QP4 == "access other Google services"] <- 1
PERM_QP4[PERM_QP4 == "Google Docs"] <- 1
PERM_QP4[PERM_QP4 == "Google Spreadsheets Google Docs"] <- 1
PERM_QP4[PERM_QP4 == "Google mail"] <- 1
PERM_QP4[PERM_QP4 == "Google Finance"] <- 1
PERM_QP4[PERM_QP4 == "Google Voice"] <- 1
PERM_QP4[PERM_QP4 == "bind to an accessibility service"] <- 1
PERM_QP4[PERM_QP4 == "prevent app switches"] <- 1
PERM_QP4[PERM_QP4 == "delete other apps caches"] <- 1
PERM_QP4[PERM_QP4 == "force device reboot"] <- 1
PERM_QP4[PERM_QP4 == "retrieve app ops statistics"] <- 1
PERM_QP4[PERM_QP4 == "use any media decoder for playback"] <- 1
PERM_QP4[PERM_QP4 == "enable or disable app components"] <- 1
PERM_QP4[PERM_QP4 == "press keys and control buttons"] <- 1
PERM_QP4[PERM_QP4 == "capture secure video output"] <- 1
PERM_QP4[PERM_QP4 == "read frame buffer"] <- 1
PERM_QP4[PERM_QP4 == "bind to a widget service"] <- 1
PERM_QP4[PERM_QP4 == "test hardware"] <- 1
PERM_QP4[PERM_QP4 == "display unauthorized windows"] <- 1
PERM_QP4[PERM_QP4 == "interact with a device admin"] <- 1
PERM_QP4[PERM_QP4 == "Access keyguard secure storage"] <- 1
PERM_QP4[PERM_QP4 == "Control displaying and hiding keyguard"] <- 1
PERM_QP4[PERM_QP4 == "freeze screen"] <- 1
PERM_QP4[PERM_QP4 == "access checkin properties"] <- 1
PERM_QP4[PERM_QP4 == "send SMS-received broadcast"] <- 1
PERM_QP4[PERM_QP4 == "WAP-PUSH-received broadcast"] <- 1
PERM_QP4[PERM_QP4 == "SmartcardServicePermission label"] <- 1
PERM_QP4[PERM_QP4 == "get current app info"] <- 1
PERM_QP4[PERM_QP4 == "run in factory test mode"] <- 1
PERM_QP4[PERM_QP4 == "send respond-via-message events"] <- 1
PERM_QP4[PERM_QP4 == "force stop other apps"] <- 1
PERM_QP4[PERM_QP4 == "read/write to resources owned by diag record what you type and actions you take"] <- 1
PERM_QP4[PERM_QP4 == "reset system to factory defaults"] <- 1
PERM_QP4[PERM_QP4 == "access the cache filesystem"] <- 1
PERM_QP4[PERM_QP4 == "view network connections"] <- 1
PERM_QP4[PERM_QP4 == "full network access"] <- 1
PERM_QP4[PERM_QP4 == "change network connectivity"] <- 1
PERM_QP4[PERM_QP4 == "change/intercept network settings and traffic"] <- 1
PERM_QP4[PERM_QP4 == "read historical network usage"] <- 1
PERM_QP4[PERM_QP4 == "find accounts on the device"] <- 1
PERM_QP4[PERM_QP4 == "read your own contact card"] <- 1
PERM_QP4[PERM_QP4 == "modify your own contact card"] <- 1
PERM_QP4[PERM_QP4 == "add or remove accounts"] <- 1
PERM_QP4[PERM_QP4 == "read your contacts"] <- 1
PERM_QP4[PERM_QP4 == "modify your contacts"] <- 1
PERM_QP4[PERM_QP4 == "read calendar events plus confidential information"] <- 1
PERM_QP4[PERM_QP4 == "add or modify calendar events and send email to guests without owners knowledge"] <- 1
PERM_QP4[PERM_QP4 == "approximate location (network-based)"] <- 1
PERM_QP4[PERM_QP4 == "precise location (GPS and network-based)"] <- 1
PERM_QP4[PERM_QP4 == "access extra location provider commands"] <- 1
PERM_QP4[PERM_QP4 == "receive text messages (SMS)"] <- 1
PERM_QP4[PERM_QP4 == "read your text messages (SMS or MMS)"] <- 1
PERM_QP4[PERM_QP4 == "receive text messages (MMS)	"] <- 1
PERM_QP4[PERM_QP4 == "edit your text messages (SMS or MMS)"] <- 1
PERM_QP4[PERM_QP4 == "send SMS messages"] <- 1
PERM_QP4[PERM_QP4 == "receive text messages (WAP)"] <- 1
PERM_QP4[PERM_QP4 == "MMS Wakeup"] <- 1
PERM_QP4[PERM_QP4 == "directly call phone numbers"] <- 1
PERM_QP4[PERM_QP4 == "write call log"] <- 1
PERM_QP4[PERM_QP4 == "read call log"] <- 1
PERM_QP4[PERM_QP4 == "reroute outgoing calls"] <- 1
PERM_QP4[PERM_QP4 == "modify phone state"] <- 1
PERM_QP4[PERM_QP4 == "directly call any phone numbers"] <- 1
PERM_QP4[PERM_QP4 == "read precise phone states"] <- 1
PERM_QP4[PERM_QP4 == "read voicemail"] <- 1
PERM_QP4[PERM_QP4 == "write voicemail"] <- 1
PERM_QP4[PERM_QP4 == "read the contents of your USB storage"] <- 1
PERM_QP4[PERM_QP4 == "modify or delete the contents of your USB storage"] <- 1
PERM_QP4[PERM_QP4 == "access USB storage filesystem"] <- 1
PERM_QP4[PERM_QP4 == "set wallpaper"] <- 1
PERM_QP4[PERM_QP4 == "adjust your wallpaper size"] <- 1
PERM_QP4[PERM_QP4 == "manage document storage"] <- 1
PERM_QP4[PERM_QP4 == "modify/delete internal media storage contents"] <- 1
PERM_QP4[PERM_QP4 == "erase USB storage"] <- 1
PERM_QP4[PERM_QP4 == "Access all system downloads"] <- 1
PERM_QP4[PERM_QP4 == "take pictures and videos"] <- 1
PERM_QP4[PERM_QP4 == "record audio"] <- 1
PERM_QP4[PERM_QP4 == "view Wi-Fi connections"] <- 1
PERM_QP4[PERM_QP4 == "connect and disconnect from Wi-Fi"] <- 1
PERM_QP4[PERM_QP4 == "allow Wi-Fi Multicast reception"] <- 1
PERM_QP4[PERM_QP4 == "pair with Bluetooth devices"] <- 1
PERM_QP4[PERM_QP4 == "allow Bluetooth pairing by Application"] <- 1
PERM_QP4[PERM_QP4 == "access Bluetooth settings"] <- 1
PERM_QP4[PERM_QP4 == "body sensors (like heart rate monitors)"] <- 1
PERM_QP4[PERM_QP4 == "read phone status and identity"] <- 1
PERM_QP4[PERM_QP4 == "read your social stream"] <- 1
PERM_QP4[PERM_QP4 == "write to your social stream"] <- 1
PERM_QP4[PERM_QP4 == "read subscribed feeds"] <- 1
PERM_QP4[PERM_QP4 == "write subscribed feeds"] <- 1

#Transform na in '0'
PERM_QP4[is.na(PERM_QP4)] <- 0 

#Delete string variable permissions
PERM_QP4$permissions <- NULL

#save permissions subset as CSV
write.csv(PERM_QP4, file = "PERM_QP4_FILTERED.csv")
