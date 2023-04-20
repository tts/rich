library(rvest)
library(tidyverse)

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
hdi <- hdi[,c(2,11)]
hdi <- hdi[c(8:206),]
names(hdi) <- c("country", "gni")

hdi <- hdi %>% 
  filter(!is.na(gni),
         gni != "..") %>% 
  mutate(gni = as.numeric(gni),
         gni = ceiling(gni))

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
hdi$country[hdi$country == "Türkiye"] <- "Turkey"

# Manually adding few missing ones
hdi <- hdi %>% 
  add_row(country = "Taiwan", gni = 34756) %>% # 2021 https://www.taipeitimes.com/News/biz/archives/2023/03/08/2003795675
  add_row(country = "Guernsey", gni = 66220) %>% # 2007 Channel Islands https://en.wikipedia.org/wiki/List_of_countries_by_GNI_(nominal)_per_capita
  add_row(country = "Macau", gni = 46730) %>% # 2020 https://en.wikipedia.org/wiki/List_of_countries_by_GNI_(nominal)_per_capita
  add_row(country = "Monaco", gni = 186080) # 2008 https://www.destatis.de/EN/Themes/Countries-Regions/International-Statistics/Data-Topic/Tables/BasicData_GNI.html

forbes_countries_hdi <- left_join(forbes_countries, hdi)

stats <- forbes_countries_hdi %>% 
  mutate(by_100K = round((billionaires / population) * 100000, digits = 3)) %>% 
  select(country, population, billionaires, by_100K, gni)

write.csv(stats, "stats.csv", row.names = FALSE)

#-------------------
# Interactive graph
#-------------------

# Arrow to Finland
arrows <- 
  tibble(
    x1 = c(49600),
    x2 = c(49640),
    y1 = c(2), 
    y2 = c(0.5)
  )

gg_point <- 
  ggplot(data = stats) +
  ggiraph::geom_jitter_interactive(aes(x = gni, y = by_100K, 
                              color = billionaires, 
                              tooltip = paste0("Country: ", country, 
                                               "\nGross national income (GNI) per capita: ", gni,
                                              "\nNumber of billionaires: ", billionaires,
                                              "\nWithin 100K: ", sprintf("%.3f", by_100K))),
                          width = 0.6, height = 0.18,
                          alpha = 1/2) +
  # https://stackoverflow.com/a/65077171
  annotate("text", x = 27000, y = 4, size = 8/.pt, label = "The darker the point,\n the more billionaires",
           colour = "#9c9fb0") +
  annotate("text", x = 49600, y = 2.2, size = 8/.pt, label = "Finland", colour = "#003580") +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.2,
  color = "#003580", curvature = -0.1) +
  scale_x_continuous(breaks = seq(from = 2000, to = 200000, by = 25000)) +
  scale_y_continuous(breaks = seq(from = 0, to = 6, by = 1))  +
  labs(title = "Billionaires in 2023 by Forbes - the country perspective",
       x = "Gross national income (GNI) per capita (2017 PPP $)",
       y = "Billionaires in population of 100K",
       caption = "Data by Forbes, Wikidata, UN | Graph & code https://github.com/tts/rich") +
  scale_color_viridis_c(option = "magma", direction = -1, begin = 0.1, end = 0.85) + # 
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 12),
    plot.caption = element_text(size = 4),
    axis.title.x = element_text(size = 6),
    axis.title.y = element_text(size = 6))

# https://stackoverflow.com/a/72552743
p <- ggiraph::girafe(ggobj = cowplot::plot_grid(gg_point, ncol = 1),
                     width_svg = 8, height_svg = 4)

p <- ggiraph::girafe_options(p, ggiraph::opts_tooltip(opacity = .8, offx = 20, offy = -10,
                                                      use_fill = TRUE, use_stroke = TRUE, delay_mouseout = 1000) )

htmlwidgets::saveWidget(p, file= "forbes.html", selfcontained = TRUE, 
                        title = "Billionaires in 2023 by Forbes - the country perspective")
