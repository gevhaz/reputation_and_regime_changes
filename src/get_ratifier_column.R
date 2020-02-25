# YES if country joined before the start of the first period under
# analysis, NO if it joined after the last period under analysis and JOINED
# if in between.
get_ratifier_column = function(ratifications, regimes)
{
    ratifier_column = vector(mode = "character", length = nrow(regimes))
    for (i in 1:nrow(regimes))
    {
        country = regimes[i, "country"]
        date_of_effect = ratifications[ratifications$country == country,
                                 "date.of.effect"]
        if (date_of_effect < regimes[i, "p1.bdate"])
        {
            ratifier = "YES"
        }
        else if (date_of_effect <= regimes[i, "p2.edate"])
        {
            ratifier = "JOINED"
        }
        else
        {
            ratifier = "NO" 
        }
        ratifier_column[i] = ratifier
    }
    return (ratifier_column)
}
