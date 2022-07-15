#' LoadBelgium
getDate <- function(x, y) {
        dates <- as.character(Sys.Date() - x)
        if (y == 0) {
            string <- unlist(strsplit(dates, "-"))
            new_date <- sprintf("%s%s%s", string[1], string[2], string[3])
        } else {
            new_date <- dates
        }
        return(new_date)
    }
    
    # cases - loading individual files from historical datasets of cumulative cases at municipality level (581), which we will aggregate to Arrondissements (43).
    # find latest data
    flag <- 0
    aa <- 0
    latest_data <- NULL
    
    while (flag == 0) {
        STRING <- paste0("https://epistat.sciensano.be/Data/", getDate(aa, 0), "/COVID19BE_CASES_MUNI_CUM_", getDate(aa, 0), ".csv")
        tryCatch(
            {
                latest_data <- vroom::vroom(STRING)
            },
            error = function(cond) {
                warning(paste0("No data for ", getDate(aa, 0)))
            }
        )
        if (is.null(dim(latest_data)) == FALSE) {
            flag <- 1
        } else {
            aa <- aa + 1
        }
        if (aa > 5) {
            warning("no recent data")
            flag <- 2
        }
    }
    
