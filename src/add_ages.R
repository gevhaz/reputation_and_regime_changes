add_ages = function(changes) 
{

	get_end_date = function(a_change) 
	{
		return (as.Date(paste0(a_change[1,"eyear"], "-", 
							   a_change[1,"emonth"], "-", 
							   a_change[1,"eday"])))
	}

	get_begin_date = function(a_change) 
	{
		return (as.Date(paste0(a_change[1,"byear"], "-", 
							   a_change[1,"bmonth"], "-", 
							   a_change[1,"bday"])))
	}


	# add regime age, begin date and end date for regimes
	for (i in 1:(nrow(changes)-1)) 
	{
		same_country = changes[i,"ccode"] == changes[i+1, "ccode"]
		
		# calculate the age of regime 
		if (same_country) 
		{
			bdate = get_begin_date(changes[i,])
			edate = get_end_date(changes[i+1,])
			changes[i,"age"] = dseconds(interval(bdate, edate))
			changes[i, "bdate"] = bdate
			changes[i, "edate"] = edate
		} 
		# special case: last row for a country
		else 
		{
			is_end_of_state = changes[i, "regtrans"] == 98 | 
				changes[i, "regtrans"] == 96

			if (is_end_of_state) 
			{
				# This row does not contain a new regime and is only
				# used for getting the end date of the last regime.
				# Setting it's age to zero or one day removes it when
				# filtering for age later.
				changes[i, "age"] = 0
				changes[i, "bdate"] = ymd("20200101")
				changes[i, "edate"] = ymd("20200102")
			} 
			else 
			{
				# This row is the present regime so count it as ending
				# today.
				bdate = get_begin_date(changes[i,])
				edate = as.Date("2018-12-31")
				changes[i,"age"] = dseconds(interval(bdate, edate))
				changes[i, "bdate"] = bdate
				changes[i, "edate"] = edate
			}
		}
	}

	# the last regime which is ignored in the loop and it is today's Fijian
	# regime so give it edate = December 31, 2018
	bdate = get_begin_date(changes[nrow(changes),])
	edate = as.Date("2018-12-31")
	changes[nrow(changes),"age"] = dseconds(interval(bdate, edate))
	changes[nrow(changes), "bdate"] = bdate
	changes[nrow(changes), "edate"] = edate

	return (changes)
}
