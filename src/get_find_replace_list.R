get_find_replace_list = function(target_data)
{

	if (target_data == 'fh') 
	{
		# What it's called in Freedom House dataset.
		find_name = c(
			"The Gambia",
			"Congo (Brazzaville)",
			"Eswatini",
			"Yemen, N.",
			"North Korea",
			"South Korea",
			"Vietnam, N.",
			"Vietnam, S.",
			"Yemen, S.",
			"Slovakia",
			"Bosnia and Herzegovina",
			"Cote d'Ivoire",
			"Congo (Kinshasa)",
			"Myanmar",
			"Timor-Leste",
			"United Arab Emirates"
		)

		# What it's called in Polity dataset.
		replace_name = c(
			"Gambia",
			"Congo Brazzaville",
			"Swaziland",
			"Yemen North",
			"Korea North",
			"Korea South",
			"Vietnam North",
			"Vietnam South",
			"Yemen South",
			"Slovak Republic",
			"Bosnia",
			"Ivory Coast",
			"Congo Kinshasa",
			"Myanmar (Burma)",
			"East Timor",
			"UAE"
		)
	}
	else if (target_data == 'un')
	{
		# What it's called in UN dataset.
		find_name = c(
			"Bolivia (Plurinational State of)",
			"Slovakia",
			"The former Yugoslav Republic of Macedonia",
			"Bosnia and Herzegovina",
			"Republic of Moldova",
			"Russian Federation",
			"Cabo Verde",
			"C\xf4te d'Ivoire",
			"Congo",
			"Democratic Republic of the Congo",
			"United Republic of Tanzania",
			"Eswatini",
			"Iran (Islamic Republic of)",
			"Republic of Korea",
			"Lao People's Democratic Republic",
			"Viet Nam",
			"Timor-Leste",
			"Venezuela (Bolivarian Republic of)",
			"Myanmar",
			"Syrian Arab Republic",
			"United Arab Emirates",
			"United States of America",
			"Democratic People's Republic of Korea"
		)

		# What it's called in Polity dataset.
		replace_name = c(
			"Bolivia",
			"Slovak Republic",
			"Macedonia",
			"Bosnia",
			"Moldova",
			"Russia",
			"Cape Verde",
			"Ivory Coast",
			"Congo Brazzaville",
			"Congo Kinshasa",
			"Tanzania",
			"Swaziland",
			"Iran",
			"Korea South",
			"Laos",
			"Vietnam",
			"East Timor",
			"Venezuela",
			"Myanmar (Burma)",
			"Syria",
			"UAE",
			"United States",
			"Korea North"
		)
	}
	else
	{
		stop("Wrong argument to get_find_replace_list.")
		return(0)
	}

	return(data.frame("find" = find_name,
				"replace" = replace_name,
				stringsAsFactors = FALSE))
}
