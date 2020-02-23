# raw is the raw ratification data to processed, country_names is the list
# of countries that are used in either test and treaty is either CCPR or
# the optional protocol, which determines what the special cases are.
process_ratification_data = function(raw, country_names, treaty)
{
	# Keep only the columns for country and ratification date
	rd = raw[,c(1,3)]
	colnames(rd) = c("country", "ratification.date")
	rd$ratification.date = as.Date(rd$ratification.date, format='%Y/%m/%d')

	# Not in list of ratifications.
	rd[nrow(rd)+1, "country"] = "Taiwan"
	rd[nrow(rd)+1, "country"] = "Kosovo"
	rd[nrow(rd)+1, "country"] = "Vietnam North"
	rd[nrow(rd)+1, "country"] = "Vietnam South"
	rd[nrow(rd)+1, "country"] = "Yemen North"

	# Change all NA to date past any regime end date in order to not get
	# problems with if-statements. This is okay because NA mens they
	# haven't ratified and will be categorized as such because their made
	# up ratification date is past the regimes end date.
	rd[is.na(rd$ratification.date), "ratification.date"] =
		as.Date("2020-12-31")


	# Ratification dates (or lack thereof) for no longer existing countries
	additional_countries_ccpr = data.frame(
		"country" = c("Germany West", 
					  "Yugoslavia", 
					  "Czechoslovakia", 
					  "Yemen South"),
		"ratification.date" = c(as.Date("1973-12-17"), 
								as.Date("1971-06-02"), 
								as.Date("1975-12-23"), 
								as.Date("1987-02-09")))

	additional_countries_protocol = data.frame(
		"country" = c("Yugoslavia",
					  "Yemen South"),
		"ratification.date" = c(as.Date("1990-03-14"),
								as.Date("2020-12-31")))

	if (treaty == "CCPR")
	{
		rd = rbind(rd, additional_countries_ccpr)
	}
	else if (treaty == "PROTOCOL")
	{
		rd = rbind(rd, additional_countries_protocol)
	}
	else
	{
		stop("Wrong treaty code in argument. Stopping.")
	}

	treaty_entry_into_force = ymd("1976-03-23")
	rd[,"date.of.effect"] = rd[,"ratification.date"] + days(90)
	rd[rd$date.of.effect < treaty_entry_into_force, "date.of.effect"] =
		treaty_entry_into_force

	# Change names so that they correspond to the ones used in the Polity
	# dataset.
	find_replace_un = get_find_replace_list('un')
	rd[,"country"] = country_name_replace(rd[,"country"], country_names,
										  find_replace_un)
	writeLines(paste("UN country names replaced for", treaty))

	return(rd)
}
