library(tidyverse)
library(cansim)
library(stringr)
library(plotly)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}


#Percentage of workforce laid off and businesses who laid off 80% or more of their workers because of COVID-19, by business characteristics
dta_laidOff <- read_excel("ExploratoryAnalysis2.xlsx", col_names = FALSE)

#Business revenue from January 1 to March 31, 2019, compared with January 1 to March 31, 2020, 
#by business characteristics
dta_revenue <- get_cansim("33-10-0234-01") %>% normalize_cansim_values()


####  Impact on Revenue ####
#Clean dta_revenue - Percentage of buinsses that reported a decreased in revenue by more than 20%, by sector, Canada, March 2020
dta_revenue1 <- (dta_revenue
                 %>% select(REF_DATE,`Business characteristics`,
                            `Revenue change`,VALUE)
                 %>% filter(str_detect(`Business characteristics`,
                                       "hunting|Mining|Utilities|Construction|Manufacturing|Wholesale|Retail|warehousing|cultural|insurance|leasing|scientific|Management|waste|Educational|social|Arts|services|Other|Public"))
                 %>% filter(str_detect(`Revenue change`,"decreased"))
                 %>% filter(str_detect(`Revenue change`, "Revenue decreased 1% to less than 10%", negate = TRUE))
                 %>% group_by(`Business characteristics`)
                 %>% summarise(total_percentage = sum(VALUE))
                 %>% rename(sectors = `Business characteristics`, total_revenueDecrease = total_percentage))


### Impact on Workers (Percentage of layoffs and businsses that laid off 80% or more of their workforce) ###
dta_laidOff1 <- (dta_laidOff
                 %>% rename(sectors = ...1, totalLayoff = ...2))

### Combine dta_revenue1 and data_laidOff1  ###

new_dta <- (data.frame(dta_laidOff1$sectors, dta_laidOff1$totalLayoff, dta_revenue1$total_revenueDecrease*100)
            %>% rename(totalLayoff=dta_laidOff1.totalLayoff, 
                       sectors = dta_laidOff1.sectors,
                       total_revenueDecrease=dta_revenue1.total_revenueDecrease...100 ))
#Barploy

fig1 <- plot_ly(new_dta,x=~sectors,y=~total_revenueDecrease, hoverinfo='y', type='bar', name='Decline in Revenue',
                marker=list(color='rgb(55,83,109'))
fig1 <- (fig1 %>% add_trace(y=~totalLayoff, name='Layoffs of Staff',hoverinfo='y',marker = list(color = 'rgb(26, 118, 255)'))
        %>% layout(title='Covid-19 Impact on Canadian Businesses- Employee Layoff and Revenue Decline, by Sector, March 2020',
          xaxis=list(title="",tickfont=list(size=12)),
          yaxis=list(title='Percentage Change',barmode='group')))

print(fig1)



### Percentage of businesses that requested credit from financial insitituion ###
dta_credit <- read_excel("FinancialHelp.xlsx")
dta_credit1 <- (dta_credit
                %>% mutate(totalEmploy = as_factor(`Number of Employees`)))
fig2 <- ggplot(dta_credit1, aes(y=Total,x=totalEmploy))
fig2 <- (fig2 + 
  geom_bar(stat="identity", fill="darkblue") +
  geom_text(aes(label=Total),vjust=0.8, hjust=2, color="white", size=3.5) +
  coord_flip() + 
  theme_minimal())

fig2
