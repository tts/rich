library(rvest)
library(tidyverse)
library(ggiraph)

#---------
# Forbes
#---------

data <- read.csv("../vscode/selenium/countries.txt", header = FALSE, stringsAsFactors = FALSE)

forbes <- data %>% 
  rename(country = V1) %>% 
  group_by(country) %>% 
  summarise(billionaires = n()) %>% 
  arrange(desc(billionaires))

#----------
# Wikidata
#----------

source("f.R")
sparql_endpoint <- "https://query.wikidata.org/sparql"
ua <- httr::user_agent("https://github.com/tts/rich")

# Population of countries and the like
q <- 'SELECT DISTINCT ?itemLabel ?pop WHERE {
  { ?item wdt:P31 wd:Q3624078 .} # sovereign state
  UNION
  { ?item wdt:P31 wd:Q6256 .} # country
  UNION
  { ?item wdt:P31 wd:Q112099.} # island nation
  UNION
  { ?item wdt:P361 wd:Q42314 .} # Channel Islands
  UNION
  { ?item wdt:P31 wd:Q779415 .} # special administrative region of China
  ?item wdt:P1082 ?pop.
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
}'

res <- process_json(sparql(q))

wiki <- do.call(data.frame, res) %>% 
  select(ends_with("value")) %>% 
  rename(population = pop.value,
         country = itemLabel.value) %>% 
  mutate(population = as.numeric(population)) %>% 
  select(country, population) %>% 
  distinct(country, .keep_all = TRUE)

# Make a few names to match
forbes$country[forbes$country == "China"] <- "People's Republic of China"
forbes$country[forbes$country == "United States"] <- "United States of America"
forbes$country[forbes$country == "Ireland"] <- "Republic of Ireland"
forbes$country[forbes$country == "St. Kitts and Nevis"] <- "Saint Kitts and Nevis"

forbes_countries <- left_join(forbes, wiki)

#--------------------------------------------------------------
# Human Development Index: Life expectancy at birth
# https://hdr.undp.org/data-center/documentation-and-downloads
#--------------------------------------------------------------

hdi <- readxl::read_excel("HDR21-22_Statistical_Annex_HDI_Table.xlsx")
hdi <- hdi[,c(2,5)]
hdi <- hdi[c(8:206),]
names(hdi) <- c("country", "life expectancy")
hdi <- hdi %>% 
  filter(!is.na(`life expectancy`)) %>% 
  mutate(`life expectancy` = as.double(`life expectancy`))

# Make some names to match
hdi$country[hdi$country == "China"] <- "People's Republic of China"
hdi$country[hdi$country == "United States"] <- "United States of America"
hdi$country[hdi$country == "Ireland"] <- "Republic of Ireland"
hdi$country[hdi$country == "Russian Federation"] <- "Russia"
hdi$country[hdi$country == "Hong Kong, China (SAR)"] <- "Hong Kong"
hdi$country[hdi$country == "Korea (Republic of)"] <- "South Korea"
hdi$country[hdi$country == "Czechia"] <- "Czech Republic"
hdi$country[hdi$country == "Viet Nam"] <- "Vietnam"
hdi$country[hdi$country == "Tanzania (United Republic of)"] <- "Tanzania"
hdi$country[hdi$country == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"

# Manually adding few missing ones
hdi <- hdi %>% 
  add_row(country = "Taiwan", `life expectancy` = 81.04) %>% # https://www.macrotrends.net/countries/TWN/taiwan/life-expectancy
  add_row(country = "Turkey", `life expectancy` = 78.45) %>% # https://www.macrotrends.net/countries/TWN/turkey/life-expectancy
  add_row(country = "Guernsey", `life expectancy` = 82.6) %>% # https://guernseypress.com/news/2018/04/12/guernsey-in-top-10-for-life-expectancy/
  add_row(country = "Macau", `life expectancy` = 85) # https://data.worldbank.org/indicator/SP.DYN.LE00.IN?locations=MO

forbes_countries_hdi <- left_join(forbes_countries, hdi)

stats <- forbes_countries_hdi %>% 
  mutate(by_100K = round((billionaires / population) * 100000, digits = 3))

#-------------------
# Interactive graph
#-------------------

# Coords of Finland
arrows <- 
  tibble(
    x1 = c(75),
    x2 = c(82),
    y1 = c(2), 
    y2 = c(0.2)
  )

gg_point <- 
  ggplot(data = stats) +
  geom_point_interactive(aes(x = `life expectancy`, y = by_100K, 
                             color = billionaires, 
                             tooltip = paste0("Country: ", country, 
                                              "\nBillionaires #: ", billionaires,
                                              "\nWithin 100K: ", sprintf("%.3f", by_100K),
                                              "\nLife expectancy (years): ", `life expectancy`)),
                         alpha = 1/2) +
  annotate("text", x = 75, y = 2.2, label = "Finland") +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.2,
    color = "gray20", curvature = -0.3) +
  scale_x_continuous(breaks = seq(from = 45, to = 90, by = 5)) +
  scale_y_continuous(breaks = seq(from = 0, to = 6, by = 0.5)) +
  labs(title = "Forbes 2023 billionaires - the country perspective",
       x = "Life expectancy at birth (years)",
       y = "Number # in population of 100K",
       caption = "Data by Forbes, Wikidata, UN | Graph & code https://github.com/tts/rich") +
  scale_fill_viridis_b() +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 12),
    plot.caption = element_text(size = 4),
    axis.title.x = element_text(size = 6),
    axis.title.y = element_text(size = 6))

p <- girafe(ggobj = gg_point)
p <- girafe_options(p,
                    opts_tooltip(opacity = .8,
                                 offx = 20, offy = -10,
                                 use_fill = TRUE, use_stroke = TRUE,
                                 delay_mouseout = 1000) )

# knitrOptions don't seem to have any effect
htmlwidgets::saveWidget(p, file= "forbes.html", selfcontained = TRUE,
                        knitrOptions = list(out.width = 500, out.height = 250))

