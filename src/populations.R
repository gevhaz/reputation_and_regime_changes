get_populations_initial_test = function(dataset)
{
    dem_rows = dataset$regtrans %in%  3
    aut_rows = dataset$regtrans %in% -2

    # Use all members of smaller group and the same number of members
    # sampled from the other group.
    smaller_group = min(sum(dem_rows), sum(aut_rows))
    democratizations = dataset[sample(which(dem_rows), smaller_group),]
    autocratizations = dataset[sample(which(aut_rows), smaller_group),]

    return(rbind(democratizations, autocratizations))
}

# Returns columns saying what rows belong to which population.
get_pop_rows = function(dataset, treaty)
{
    dem   = dataset[,"regtrans"] %in% 3
    aut   = dataset[,"regtrans"] %in% -2
    rat   = dataset[, treaty]    %in% "YES"
    norat = dataset[, treaty]    %in% "NO"
                               
    dem_rat   = dem == TRUE & rat == TRUE
    aut_rat   = aut == TRUE & rat == TRUE
    dem_norat = dem == TRUE & norat == TRUE
    aut_norat = aut == TRUE & norat == TRUE

    rows = data.frame(dem_rat, aut_rat, dem_norat, aut_norat)

    return(rows)
}

# Returns number of members fulfilling the requirements for each population
# used in the tests.
get_pop_sizes = function(dataset, treaty)
{
    n_democratization_ratifier    = nrow(dataset[dataset[,"regtrans"] ==  3
                                         & dataset[,treaty] == "YES",])
    n_autocratization_ratifier    = nrow(dataset[dataset[,"regtrans"] == -2
                                         & dataset[,treaty] == "YES",])
    n_democratization_nonratifier = nrow(dataset[dataset[,"regtrans"] ==  3
                                         & dataset[,treaty] == "NO",])
    n_autocratization_nonratifier = nrow(dataset[dataset[,"regtrans"] == -2
                                         & dataset[,treaty] == "NO",])

    statistics = data.frame("rat" = c(n_democratization_ratifier,
                                      n_autocratization_ratifier),
                            "norat" =
                                c(n_democratization_nonratifier,
                                  n_autocratization_nonratifier))

    rownames(statistics) = c("dem", "aut")

    return(statistics)
}

get_populations_main_test = function(dataset, treaty)
{
    sizes = get_pop_sizes(dataset, treaty)
    rows  = get_pop_rows(dataset, treaty)

    # Use smallest ratio of democratizations to autocratizations in order
    # to have enough of them when sampling. The ratio should be the same
    # for both ratifiers and non-ratifiers.
    ratio = min(sizes["dem",] / sizes["aut",])

    ratifiers = rbind(
             dataset[sample(which(rows$dem_rat), min(sizes$rat) * ratio),],
             dataset[sample(which(rows$aut_rat), min(sizes$rat)),])

    nonratifiers = rbind(
         dataset[sample(which(rows$dem_norat), min(sizes$norat) * ratio),],
         dataset[sample(which(rows$aut_norat), min(sizes$norat)),])

    writeLines(paste("Returning", nrow(ratifiers), "ratifiers and",
                     nrow(nonratifiers), "non-ratifiers with a ratio of",
                     "democratizations to autocratizations of", ratio))
    
    return(rbind(ratifiers, nonratifiers))
}
