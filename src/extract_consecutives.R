extract_consecutives = function(regimes, minimum_age) 
{

	# Empty data frame to be filled with consecutive regimes.
	consecutives = data.frame("country" 		= character(0),
							  "regime1.bdate"	= numeric(0), 
							  "regime1.edate"	= numeric(0), 
							  "regime1.age"		= numeric(0),
							  "regime2.bdate"	= numeric(0), 
							  "regime2.edate"	= numeric(0), 
							  "regime2.age"		= numeric(0),
							  "regtrans" 		= numeric(0))

	# If two consecutive regimes in one country are both over min_age,
	# combine to one row and add to new data.frame.
	for (countryname in unique(regimes$country)) 
	{
		# One loop for each country so that no consecutive regimes are from
		# different countries.
		country = regimes[regimes$country==countryname,]

		if (nrow(country) > 1) 
		{
			for (i in 1:(nrow(country)-1)) 
			{
				# Both regimes need to be at least the minimum age.
				if (country[i,"age"] >= minimum_age & 
					country[i+1, "age"] >= minimum_age) 
				{
					new_row = data.frame(
						"country"		= as.character(countryname),
						"regime1.bdate"	= country[i, "bdate"],
						"regime1.edate"	= country[i, "edate"],
						"regime1.age"	= country[i, "age"],
						"regime2.bdate"	= country[i+1, "bdate"],
						"regime2.edate"	= country[i+1, "edate"],
						"regime2.age"	= country[i+1, "age"],
						"regtrans"		= country[i+1, "regtrans"],
						stringsAsFactors=FALSE)
					consecutives = rbind(consecutives, new_row)
				} 
			}
		}
	}
	return (consecutives)
}
