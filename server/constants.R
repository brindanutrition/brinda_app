find_population_code_name <- function(population) {
    if (population == "Women of Reproductive Age") {
        return("WRA")
    } else if (population == "Preschool-age children") {
        return("PSC")
    } else if (population == "Other (non-WRA, non-PSC population group)") {
        return("Other")
    } else if (population == "User-defined AGP and CRP cutoffs") {
        return("Manual")
    }
}
