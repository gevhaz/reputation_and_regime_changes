get_average_index = function(start_year, end_year, country, indices)
{
	index_sum = rep(0, length(country))

	for (i in 1:length(index_sum))
	{
		for (year in start_year[i]:end_year[i])
		{
			current_index = indices[country[i], as.character(year)]

			if (is.na(current_index)) 
			{
				stop(paste("No index for", country[i], year))
			}

			index_sum[i] = index_sum[i] + current_index
		}
	}
	return (index_sum / (end_year - start_year + 1))
}


