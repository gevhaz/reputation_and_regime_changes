# Requirements

The [R programming environment](https://www.r-project.org/) is required, 
along with the following packages:

* gdata
* tibble
* lubridate
* ggplot2

The code depends on having the right datasets available. These are:

*	[Polity IV Annual Time-Series, 
	1800-2018](http://www.systemicpeace.org/inscr/p4v2018.xls) from 
	[Center for Systemic 
	Peace](https://www.systemicpeace.org/inscrdata.html) 

*	[Country and Territory Ratings and Statuses, 1973-2019 
	(Excel)](https://freedomhouse.org/sites/default/files/Country_and_Territory_Ratings_and_Statuses_FIW1973-2019.xls) 
	from [Freedom 
	House](https://freedomhouse.org/report-types/freedom-world)

*	Ratification data for the International Covenant on Civil and 
	Political rights and its Optional Protocol from the [Status of 
	Ratification Interactive Dashboard](https://indicators.ohchr.org/) 
	from the United Nations Office of the High Commissioner.

# How to use

Run the main project file in R:

```
R project.R
```

After that, the results of the thesis can be reproduced by calling one 
of the functions `run_initial_test()` or `run_main_test()`. For the 
initial test, only one argument is needed: the test number. It can be 1 
or 2. The main test also takes the test number as the first argument but 
additionally needs to know which treaty to use to divide the regimes 
into populations of ratifiers and non-ratifiers. This argument should be 
"CCPR" or "PROTOCOL". There is also a third, optional argument that 
determines what type of plot will be produced. "BARS" for a bar plot and 
"HISTOGRAM" for a histogram. An example test would be:

```
run_main_test(1, "CCPR", "BARS")
```

which would then show some statistics about the selected sample and 
produce a stacked bar plot with two groups – ratifiers and non-ratifiers 
– and bar heights depending on the fraction of the sample that saw 
improvements, decline or no change in their ranking.
