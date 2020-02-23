load_data = 0

source("src/add_ages.R")
source("src/get_find_replace_list.R")
source("src/country_name_replace.R")
source("src/get_fh_indices.R")
source("src/process_ratification_data.R")
source("src/get_average_index.R")
source("src/get_ratifier_column.R")
source("src/display_info.R")
source("src/extract_consecutives.R")
source("src/populations.R")

library(gdata)
library(tibble)
library(lubridate)
library(ggplot2)
library(xtable)

if (load_data)
{
raw_freedom_house = read.xls(
	"data/Country_and_Territory_Ratings_and_Statuses_FIW1973-2019.xls",
	sheet=2, stringsAsFactors=FALSE,
	header=FALSE,na.string="-")
writeLines("Freedom House data read")

raw_polity_iv = read.xls("data/p4v2018.xls", sheet=1,
						 stringsAsFactors=FALSE)
writeLines("Polity IV data read")

raw_ratification_data =
	read.xls("data/UnderlyingData_ICCPR_OHCHR_12_02_2020.xls",
			 stringsAsFactors=FALSE, skip=1, nrow=198)
writeLines("CCPR ratification data read")

raw_protocol_ratification_data =
	read.xls("data/UnderlyingData_ICCPR-OP1_OHCHR_07_02_2020.xls",
			 stringsAsFactors=FALSE, skip=1, nrow=198)
writeLines("CCPR Optional Protocol ratification data read\n")
}

##############
## SETTINGS ##
##############

min_age_sinreg = 10
min_age_conreg = 3
n_p1_years_single = 2
n_years_conreg = 2
start_year = 1948
fh_start_year = 1972
max_gap = 3

## Working copy of Polity dataset.
regime_changes = raw_polity_iv

# Keep only rows where there is regime change data (D4=TRUE).
regime_changes = regime_changes[regime_changes$d4 %in% TRUE,]

# Keep only rows with data for years after a start year.
regime_changes = regime_changes[regime_changes$year >= start_year,]

###################
## SPECIAL CASES ##
###################

# Change name of "Sudan-North" in the Polity data because it is just
# called "Sudan" in the other datasets.
regime_changes[regime_changes$country == "Sudan-North","country"] = "Sudan"
SUD2011 = regime_changes$country == "Sudan" & regime_changes$year == 2011
regime_changes = regime_changes[!SUD2011,]

# Removing Kosovo because of too little Freedom House data.
regime_changes = regime_changes[regime_changes$country != "Kosovo",]

# Myanmar seems to have an error in the data, the "byear" of the
# regime is year 6.
MYA2016 = regime_changes$country == "Myanmar (Burma)" &
	regime_changes$year == 2016
regime_changes[MYA2016, "byear"] = 2016
regime_changes[MYA2016, "bmonth"] = 01

# There's no begin-data for Syria in 1961 so removing that entry.
regime_changes = regime_changes[!(regime_changes$country == "Syria" &
								  regime_changes$year == "1961"),]

######################
## REGIME FILTERING ##
######################

# If the regime transition has REGTRANS = 0, exclude it and count it
# as the same regime as before the transition.
regime_changes = regime_changes[regime_changes$regtrans != 0,]

regime_changes = add_ages(regime_changes)

# Create dataset for test 1.
conreg = extract_consecutives(regime_changes, years(min_age_conreg))
conreg = conreg[year(conreg$regime1.edate) >= fh_start_year +
				n_years_conreg,]

too_long_interruption = conreg$regime2.bdate - conreg$regime1.edate >
years(max_gap)
conreg = conreg[!too_long_interruption,]
writeLines(paste(sum(too_long_interruption), "consecutive regimes",
				 "filtered because more than", max_gap, 
				 "years between them"))

# Define periods for analysis.
conreg$p1.bdate = floor_date(conreg$regime1.edate, unit = 'year') -
	years(n_years_conreg)
conreg$p1.edate = floor_date(conreg$regime1.edate, unit = 'year') - days(1)
conreg$p2.bdate = ceiling_date(conreg$regime2.bdate, unit = 'year')
conreg$p2.edate = floor_date(conreg$regime2.bdate, unit = 'year') +
	years(n_years_conreg)

# Create dataset for test 2.
durable_with_data = regime_changes$age >= years(min_age_sinreg) &
	regime_changes$byear >= fh_start_year
columns_to_keep = c("country", "year", "bdate", 
					"edate", "regtrans", "age")
sinreg = regime_changes[durable_with_data, columns_to_keep]

# Define periods for analysis.
sinreg$p1.bdate = ceiling_date(sinreg$bdate, unit = 'years')
sinreg$p1.edate = ceiling_date(sinreg$bdate, unit = 'years') +
	years(n_p1_years_single) - days(1)
sinreg$p2.bdate = floor_date(sinreg$bdate, unit = 'years') +
	years(n_p1_years_single + 1)
sinreg$p2.edate = ceiling_date(sinreg$bdate, unit = 'years') +
	years(min_age_sinreg) - days(1) 

###################
## FREEDOM HOUSE ##
###################

# Replace country names in the Freedom House table so they match names
# in Polity data.
find_replace_fh 	= get_find_replace_list('fh')
included_countries 	= unique(append(conreg$country, sinreg$country))
fh_data 			= raw_freedom_house
fh_data[,1] 		= country_name_replace(fh_data[,1], included_countries,
										   find_replace_fh)
writeLines("Freedom House country names replaced")

# Clean up the Freedom House data.
fh_data[3, "V1"] = "Information type"
rownames(fh_data) = fh_data$V1
fh_data = fh_data[-1,-1]
fh_data["Information type",] = trimws(fh_data["Information type",])

# It's the same country, just a name change.
fh_data["Yugoslavia", 91:99] = fh_data["Serbia and Montenegro", 91:99]

#indices = get_fh_indices(fh_data, "CL")
indices = (get_fh_indices(fh_data, "CL") + 
get_fh_indices(fh_data, "PR")) / 2.0

# Make column for describing human rights trend based in mean index change.
get_trend_column = function(mean_change)
{
	trend = ifelse(mean_change < 0, "IMPROVEMENT", 
				   ifelse(mean_change > 0, "DECLINE", "NO CHANGE"))
	trend = factor(trend, c("DECLINE", "NO CHANGE", "IMPROVEMENT"))
	return (trend)
}

# Get average index for period 1 and period 2 for sinreg.
sinreg$p1.mean = get_average_index(year(sinreg$p1.bdate),
								   year(sinreg$p1.edate), 
								   sinreg$country, indices)
sinreg$p2.mean	 = get_average_index(year(sinreg$p2.bdate), 
									   year(sinreg$p2.edate),
									   sinreg$country, indices)
sinreg$mean.index.change = sinreg$p2.mean - sinreg$p1.mean
sinreg$trend = get_trend_column(sinreg$mean.index.change)

# Get average index for period 1 and period 2 for conreg.
conreg$p1.mean = get_average_index(year(conreg$p1.bdate), 
								   year(conreg$p1.edate),
									 conreg$country, indices)
conreg$p2.mean  = get_average_index(year(conreg$p2.bdate), 
									year(conreg$p2.edate), 
									 conreg$country, indices)
conreg$mean.index.change = conreg$p2.mean - conreg$p1.mean
conreg$trend = get_trend_column(conreg$mean.index.change)

#####################
## RATIFIER COLUMN ##
#####################

# Make columns for CCPR ratification.
ccpr_ratifications = process_ratification_data(raw_ratification_data,
										  included_countries, "CCPR")
conreg$ccpr.ratifier	= get_ratifier_column(ccpr_ratifications, conreg)
sinreg$ccpr.ratifier 	= get_ratifier_column(ccpr_ratifications, sinreg)

# Make columns for Optional Protocol ratification.
protocol_ratifications =
	process_ratification_data(raw_protocol_ratification_data,
							  included_countries, "PROTOCOL")
conreg$protocol.ratifier = get_ratifier_column(protocol_ratifications,
											   conreg)
sinreg$protocol.ratifier = get_ratifier_column(protocol_ratifications,
											   sinreg)

###########
## TESTS ##
###########

# Plot style.
bars_theme   = theme(text=element_text(family="sans,Helvetica", size=20))
bars_palette = scale_fill_brewer(palette = "RdYlGn")

# Plot label names.
bars_labs    = labs(x	 = "Has ratified", 
					y 	 = "Share of sample", 
					fill = "Human rights trend")
histogram_labs = labs(x		= "Mean index change",
					  y		= "Number of cases",
					  fill	= "Has ratified")

# Titles for plots.
init1_bars_title = ggtitle(label = "Test 1: Initial Test")
init2_bars_title = ggtitle(label = "Test 2: Initial Test")
main1_ccpr_bars_title = ggtitle(label = "Test 1: CCPR")
main2_ccpr_bars_title = ggtitle(label = "Test 2: CCPR")
main1_protocol_bars_title = ggtitle(label = "Test 1: Protocol")
main2_protocol_bars_title = ggtitle(label = "Test 2: Protocol")


show_histogram = function(populations, treaty, bars_title)
{
	ggplot(populations,aes_string("mean.index.change", fill=treaty)) + 
		bars_theme + histogram_labs + bars_title + 
		# geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth = 1, 
		# position = "identity") +
		geom_density(alpha = 0.2) + 
		scale_fill_brewer(palette = "Set1")
}

show_bars = function(populations, treaty, bars_title)
{
	return(ggplot(populations,aes_string(treaty, fill="trend")) + 
		   bars_theme + bars_labs + bars_title + bars_palette + 
		   geom_bar(position = "fill"))
}

run_initial_test = function(TEST_NUMBER)
{
	main_dataset = switch(TEST_NUMBER, conreg, sinreg,
					 stop("No test with that number."))
	populations = get_populations_initial_test(main_dataset)

	# ccpr.ratifier just used because an x variable is required.
	treaty = "ccpr.ratifier"
	populations[,treaty] = "MIXED"
	
	# Show statistics for population.
	writeLines(paste("Information for mixed sample of ratifiers,", 
					 "non-ratifiers and those who joined."))
	writeLines(paste("The ratio of democratizations to autocratizations", 
					 "in this group is 1."))
	HR_improvements(populations)

	plot_title = switch(TEST_NUMBER, init1_bars_title, init2_bars_title)
	show_bars(populations, treaty, plot_title)
}

run_main_test = function(TEST_NUMBER, TREATY, GEOMETRY = "BARS")
{
	treaty_column = switch(TREATY, 
						   "CCPR" = "ccpr.ratifier", 
						   "PROTOCOL" = "protocol.ratifier",
						   stop("No treaty with that code."))
	main_dataset = switch(TEST_NUMBER, conreg, sinreg,
					 stop("No test with that number."))
	populations = get_populations_main_test(main_dataset, treaty_column)

	plot_title = switch(paste0(TREATY, TEST_NUMBER), 
				   CCPR1 = main1_ccpr_bars_title,
				   CCPR2 = main2_ccpr_bars_title,
				   PROTOCOL1 = main1_protocol_bars_title,
				   PROTOCOL2 = main2_protocol_bars_title)

	# Show populations statistics.
	HR_improvements_ratification_status(populations, TREATY)
	
	switch(GEOMETRY,
		BARS = show_bars(populations, treaty_column, plot_title),
		HISTOGRAM = show_histogram(populations, treaty_column, plot_title),
		stop("No such plot geometry."))
}
