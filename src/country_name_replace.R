# country_names is the row or column of countries from a data.frame that
# should be changed (if they are going to be used).
#
# included_countries is the list of countries which have a regime in the
# regime_change variable after filtering for age and such (so only these
# need to have their names changed).
#
# replace_key is the key for knowing which country names in country_names
# to replace and what to replace them with.
country_name_replace = function(country_names, included_countries,
								replace_key) 
{

	print_list = function(the_list) 
	{
		size = length(the_list)
		if (size == 0) 
		{
			writeLines("\tNone")
		} 
		else if (size == 1) 
		{
			cat("\t")
			cat(the_list[1], "\n")
		} 
		else 
		{
			cat("\t")
			cat(the_list[-size], "", sep=", ")
			cat(the_list[size], "\n")
		}
	}

	# Find countries that have different names or are missing in dataset.
	missing_countries = included_countries[!(included_countries %in%
											 country_names)]

	for (i in 1:nrow(replace_key)) 
	{
		find_country = replace_key[i, "find"]
		replace_country = replace_key[i, "replace"]
		
		country_names[country_names==find_country] = replace_country
	}

	# Are there still non-matching countries left after changing the names?
	missing_countries = included_countries[!(included_countries %in%
											 country_names)]

	if (length(missing_countries) > 0)
	{
		warning("Some countries still have the wrong name")
		print_list(missing_countries)
	}

	return (country_names)
}
