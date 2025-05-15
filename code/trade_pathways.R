## goal: make two barcharts showing the trade pathways (1) to and (2) from countries of interest 

#### [0] prep ####
library(tidyverse)
library(tidytext)

# load data
edge_df_og <- read_csv("output/edge_consumption_countries_global_nutrients_HS96.csv")


#### [1] establish country of interest ####
country_of_interest <- "IDN"
# country_of_interest <- c("IDN","PNG")


#### [2] make functions for analysis and plotting ####
# summarize nutrient flows by role
summarize_role <- function(df, country_col, role_label) {
  df %>%
    group_by({{ country_col }}, Nutrient) %>%
    summarise(
      TotalValue = sum(Value, na.rm = TRUE),
      TotalRNIAmount = sum(rni_amount, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(Country = {{ country_col }}) %>%
    mutate(Role = role_label)
}

# rank top countries by nutrient contribution
rank_top_roles <- function(df, role_levels, n = 50) {
  df %>%
    mutate(Role = factor(Role, levels = role_levels)) %>%
    group_by(Country, Role) %>%
    mutate(RNIsum = sum(TotalRNIAmount)) %>%
    ungroup() %>%
    group_by(Role) %>%
    arrange(desc(RNIsum)) %>%
    slice_head(n = n) %>%
    ungroup()
}

# classify trade network motifs
classify_trade_motifs <- function(df) {
  df %>%
    mutate(`Trade Network Motif` = case_when(
      SourceCountry == ExporterCountry & ExporterCountry != ConsumerCountry ~ "Domestic Export",
      SourceCountry == ConsumerCountry & SourceCountry != ExporterCountry ~ "Re-Import",
      SourceCountry != ExporterCountry & ExporterCountry != ConsumerCountry ~ "Standard",
      SourceCountry == ExporterCountry & ExporterCountry == ConsumerCountry ~ "Domestic Production/Consumption"
    )) %>%
    mutate(`Trade Network Motif` = factor(`Trade Network Motif`,
           levels = c("Standard", "Domestic Export", "Re-Import", "Domestic Production/Consumption")))
}

# plot trade routes
plot_trade_routes <- function(df, title) {
  ggplot(df, aes(x = reorder(paste(SourceCountry, ExporterCountry, ConsumerCountry, sep = " → "), TotalRNIAmount),
                 y = TotalRNIAmount)) +
    geom_col(aes(fill = `Trade Network Motif`)) +
    coord_flip() +
    labs(x = "Trade Pathway", y = "Total RNIs", title = title) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}


#### [3] analysis: pathways to country of interest ####
exports_to <- edge_df_og %>%
  filter(ConsumerCountry %in% country_of_interest) %>%
  filter(!(SourceCountry == ExporterCountry & ExporterCountry == ConsumerCountry))

exporter_sum <- summarize_role(exports_to, ExporterCountry, "Exporter")
source_sum <- summarize_role(exports_to, SourceCountry, "Source")

role_df_to <- bind_rows(exporter_sum, source_sum)

top_roles_to <- rank_top_roles(role_df_to, role_levels = c("Source", "Exporter"))

top_countries_to <- distinct(top_roles_to[c("Country", "Role")])

filtered_to <- exports_to %>%
  filter(SourceCountry %in% top_countries_to$Country[top_countries_to$Role == "Source"] |
         ExporterCountry %in% top_countries_to$Country[top_countries_to$Role == "Exporter"])

routes_to <- filtered_to %>%
  group_by(SourceCountry, ExporterCountry, ConsumerCountry) %>%
  summarise(TotalRNIAmount = sum(rni_amount, na.rm = TRUE), .groups = "drop") %>%
  classify_trade_motifs()

plot_trade_routes(routes_to %>% slice_max(TotalRNIAmount, n = 50),
                  paste0("Top Seafood Trade Routes for Consuming ", country_of_interest))



#### [4] analysis: pathways from country of interest ####
exports_from <- edge_df_og %>%
  filter(SourceCountry %in% country_of_interest) %>%
  filter(!(SourceCountry == ExporterCountry & ExporterCountry == ConsumerCountry))

exporter_sum <- summarize_role(exports_from, ExporterCountry, "Exporter")
consumer_sum <- summarize_role(exports_from, ConsumerCountry, "Consumer")

role_df_from <- bind_rows(exporter_sum, consumer_sum)

top_roles_from <- rank_top_roles(role_df_from, role_levels = c("Exporter", "Consumer"))

top_countries_from <- distinct(top_roles_from[c("Country", "Role")])

filtered_from <- exports_from %>%
  filter(ExporterCountry %in% top_countries_from$Country[top_countries_from$Role == "Exporter"] |
         ConsumerCountry %in% top_countries_from$Country[top_countries_from$Role == "Consumer"])

routes_from <- filtered_from %>%
  group_by(SourceCountry, ExporterCountry, ConsumerCountry) %>%
  summarise(TotalRNIAmount = sum(rni_amount, na.rm = TRUE), .groups = "drop") %>%
  classify_trade_motifs()

plot_trade_routes(routes_from %>% slice_max(TotalRNIAmount, n = 50),
                  paste0("Top Seafood Trade Routes from Producing ", country_of_interest))
