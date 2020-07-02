library(tidyverse)
library(cansim)
library(stringr)
library(plotly)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

#Percentage of workforce teleworking or working remotely as of March 31st, 2020
dta <- get_cansim("33-10-0228-01") %>% normalize_cansim_values()

#Percentage of workforce laid off because of COVID-19, by business characteristics
dta_laidOff <- get_cansim("33-10-0232-01") %>% normalize_cansim_values()

#Business revenue from January 1 to March 31, 2019, compared with January 1 to March 31, 2020, 
#by business characteristics
dta_revenue <- get_cansim("33-10-0234-01") %>% normalize_cansim_values()

#Clean dta_revenue
dta_revenue1 <- (dta_revenue
                 %>% select(REF_DATE,`Business characteristics`,
                            `Revenue change`,VALUE)
                 %>% filter(str_detect(`Business characteristics`,
                                       "hunting|Mining|Utilities|Construction|Manufacturing|Wholesale|Retail|warehousing|cultural|insurance|leasing|scientific|Management|waste|Educational|social|Arts|services|Other|Public"))
                 %>% filter(str_detect(`Revenue change`,"increased|decreased")))

