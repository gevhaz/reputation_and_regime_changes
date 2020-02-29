HR_improvements = function(dataset)
{
    n = nrow(dataset)

    improvements = sum(dataset$mean.index.change < 0)
    nochange     = sum(dataset$mean.index.change == 0)
    decline      = sum(dataset$mean.index.change > 0)

    writeLines(paste0("This dataset has ", n, " members"))
    writeLines(paste0(improvements," (",round(improvements/n*100,digits=1), 
                      "%) saw improvements in HR after regime change."))
    writeLines(paste0(nochange, " (", round(nochange/n*100, digits=1), 
                      "%) saw no change in HR after regime change."))
    writeLines(paste0(decline, " (", round(decline/n*100, digits=1), 
                      "%) saw a decline in HR after regime change."))
    writeLines(paste("The mean of the mean.index.change column is:    ",
                     round(mean(dataset$mean.index.change), digits=4)))
    writeLines(paste("The variance of the mean.index.change column is:",
                     round(var(dataset$mean.index.change ), digits=4)))

    # writeLines("\nFor LaTeX:\n")

	writeLines(paste0(" & ", improvements, " (", round(improvements/n*100,digits=1), "\\%)"))
	writeLines(paste0(" & ", nochange, " (", round(nochange/n*100,digits=1), "\\%)"))
	writeLines(paste0(" & ", decline, " (", round(decline/n*100,digits=1), "\\%)"))
	writeLines(paste0(" & ", round(mean(dataset$mean.index.change), digits=4)))
	writeLines(paste0(" & ", round(var(dataset$mean.index.change), digits=4)))
}

HR_improvements_ratification_status = function(dataset, TREATY)
{
    column = switch(TREATY, 
                    "CCPR" = "ccpr.ratifier", 
                    "PROTOCOL" = "protocol.ratifier")

    ratifiers = dataset[dataset[, column] == "YES",]
    nonratifiers = dataset[dataset[, column] == "NO",]
    
    writeLines("")
    writeLines("Information for ratifiers:")
    HR_improvements(ratifiers)
    writeLines("")
    writeLines("Information for non-ratifiers:")
    HR_improvements(nonratifiers)
}
