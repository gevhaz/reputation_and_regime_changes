get_fh_indices = function(freedom_house_data, index_type) 
{
	# Select Civil Liberties or Political Rights
	is_index_type = freedom_house_data["Information type",] == index_type 

	# Remove rows that are not indices
	index_table = freedom_house_data[-c(1,2), is_index_type]

	# South Africa had different ranking for black and white population in
	# 1972 but in order to process the data a single number is required so
	# I use the one for the black population (third character in string)
	index_table["South Africa", 1] = substr(index_table["South Africa", 1],
											3, 3)

	index_table[] = lapply(index_table, as.numeric)
	index_table_1982 = rowMeans(index_table[,10:11])
	index_table = add_column(index_table, index_table_1982, .after = 10)
	colnames(index_table) = 1972:2018

	return (index_table)
}
