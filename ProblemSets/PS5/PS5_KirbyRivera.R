#3:
#Set up workspace.
library(rvest)
library(tidyverse)

#Grab HTML.
h <- read_html("https://en.wikipedia.org/wiki/Pharmaceutical_industry")

#Use HTML parser to grab table.
sales <- h %>%
  html_node("#mw-content-text > div.mw-content-ltr.mw-parser-output > table:nth-child(84)") %>%
  html_table()

#View data.
view(sales)

#4:
#Install recommended repoRter.nih packages for workspace.
library(tibble)
library(repoRter.nih)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(scales)
library(tufte)
library(plyr)

#Advanced search criteria to find funding amount on SSRI related projects.
req <- make_req(criteria =
                  list(advanced_text_search =
                         list(operator = "advanced",
                              search_field = c("terms", "abstract"),
                              search_text = "\"selective serotonin reuptake inhibitor\" OR \"SSRI\"")),
                include_fields = c("ProjectTitle", "Organization", "FiscalYear", "AwardAmount"),
                sort_field = "AwardAmount",
                sort_order = "desc"
)

res <- get_nih_data(req)
view(res)

#Average funding by year:
res <- res %>% group_by(fiscal_year) %>% mutate(cum_award_total = cumsum(award_amount))
ggplot(res, aes(x = fiscal_year, y = award_amount)) +
  stat_summary(fun = mean, geom = "line") +
  scale_x_continuous(breaks = seq(2007, 2024, 1)) +
  labs(title = "Total Funding by Year",
       x = "Fiscal Year",
       y = "Average Funding")
