



#### [0] prep ####
library(tidyverse)
library(tidytext)

## grab data
# global
edge_df_og <- read_csv("output/edge_consumption_countries_global_nutrients_HS96.csv")

#### [1] establish country of interest ####
# can be one or multiple countries
# country_of_interest = "IDN"
country_of_interest = c("IDN","PNG")



#### [2] look at exporters/consumers where consumer country is country of interest ####
## Filter for exports from SourceCountry 
exports_from_countries <- edge_df_og %>%
  ## pick countries
  filter(ConsumerCountry %in% country_of_interest) %>%
  ## exclude domestic production/consumption
  filter(!(SourceCountry == ExporterCountry & ExporterCountry == ConsumerCountry))


## Aggregate by ExporterCountry and Nutrient to find total exports to SourceCountry
exporter_summary_countries <- exports_from_countries %>%
  group_by(ExporterCountry, ConsumerCountry, Nutrient) %>%
  summarise(
    TotalValue = sum(Value, na.rm = TRUE),          # sum of export value
    TotalRNIAmount = sum(rni_amount, na.rm = TRUE)  # sum of RNI contribution
  ) %>%
  ungroup() %>%
  rename(Country=ExporterCountry) %>%
  mutate(Role="Exporter")
consumer_summary_countries <- exports_from_countries %>%
  group_by(SourceCountry,ConsumerCountry, Nutrient) %>%
  summarise(
    TotalValue = sum(Value, na.rm = TRUE),          # sum of export value
    TotalRNIAmount = sum(rni_amount, na.rm = TRUE)  # sum of RNI contribution
  ) %>%
  ungroup() %>%
  rename(Country=SourceCountry) %>%
  mutate(Role="Source")
role_df_countries <- bind_rows(exporter_summary_countries,consumer_summary_countries)

## Rank exporters by total contributions to countries of interest
role_ranked_countries <- role_df_countries %>%
  mutate(Role = factor(Role, levels = c("Source", "Exporter"))) %>%
  group_by(Country,Role) %>%
  mutate(RNIsum = sum(TotalRNIAmount)) %>% ungroup() %>%
  group_by(ConsumerCountry,Role) %>%  # group by Role to apply slice within each group
  arrange(desc(RNIsum)) %>%
  slice_head(n = 50) %>%  # take top 50 per Role
  ungroup()


#### [3] plot ####
ggplot(role_ranked_countries, 
       aes(x = reorder_within(Country, -TotalRNIAmount,interaction(ConsumerCountry,Role)), 
           y = TotalRNIAmount, fill = Nutrient)) +
  facet_wrap(~ConsumerCountry * Role,scales = "free")+
  geom_bar(stat = "identity") +
  scale_x_reordered()+
  labs(x = NULL,
       y = "Total RNIs", 
       title = "Countries Sending Nutrients to Consumer") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=45,hjust=1),
        plot.title = element_text(size = 16, hjust = 0.5),  # center the title
        plot.subtitle = element_text(size = 12, hjust = 0.5),  # subtitle under title
        strip.text = element_text(size = 12, face = "bold"))  # increase facet title size


## save output
folder_name <- if (length(country_of_interest) > 1) "multiple_countries" else country_of_interest
# ggsave(paste0("output/", folder_name, "/top_exportersconsumer.jpg"))




