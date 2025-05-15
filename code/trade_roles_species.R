## goal: make barcharts of trade roles and species

#### [0] prep ####
library(tidyverse)
library(tidytext) # for facetwrap ordering
library(ggtext)
library(rfishbase)
library(RColorBrewer)
library(countrycode)


## load data
nutrients_cons_og <- read_csv("output/consumption_annual_HS96_nutrients.csv")


#### [1] establish country of interest ####
# pick one country
country_of_interest = "IDN"


#### [2] prep data ####
country_trade <- nutrients_cons_og %>%
  filter(source_country_iso3c %in% country_of_interest |
           exporter_iso3c %in% country_of_interest |
           consumer_iso3c %in% country_of_interest)



#### [3] what species are being traded ####
## what are the common names of species?
scientific_names <- sort(unique(country_trade$sciname))
scientific_names <- str_to_sentence(scientific_names)
species_info <- species(scientific_names,fields = c("SpecCode", "Genus", "Species", "FBname")) # use fishbase for common names

## what is the most common species?
# summarize and count the occurrences/tonnes of each species in sciname - overall species counts
species_counts <- country_trade %>%
  group_by(sciname) %>%
  summarize(count = n(), consumption_live_t=sum(consumption_live_t),
            .groups = 'drop') %>%
  arrange(desc(count)) 
# IDN/specific country breakdown and specific role breakdown
species_counts_by_role <- country_trade %>%
  select(Source=source_country_iso3c,Exporter=exporter_iso3c,Consumer=consumer_iso3c,sciname,consumption_live_t) %>%
  pivot_longer(cols=c(Source,Exporter,Consumer),
               names_to = "Role", values_to = "Country") %>%
  filter(Country %in% country_of_interest) %>%
  group_by(sciname,Role) %>%
  summarize(count = n(), consumption_live_t=sum(consumption_live_t),
            .groups = 'drop') %>%
  arrange(desc(count)) 

# prepare species_info with lowercase sciname for joining
species_info <- species(str_to_sentence(unique(country_trade$sciname)), fields = c("Genus", "Species", "FBname")) %>%
  mutate(sciname = str_to_lower(paste(Species))) %>%  # create lowercase sciname
  select(sciname, CommonName = FBname)  # select relevant columns

# join species counts with common names
species_combined <- species_counts %>%
  mutate(sciname = str_to_lower(sciname)) %>%  # ensure sciname is lowercase
  left_join(species_info, by = "sciname") %>%
  slice_max(order_by = consumption_live_t, n = 50) %>%
  arrange(desc(consumption_live_t)) %>%
  mutate(label_name = ifelse(!is.na(CommonName), paste(str_to_sentence(sciname), " / ", CommonName), str_to_sentence(sciname)))

species_combined_by_role <- species_counts_by_role %>%
  mutate(sciname = str_to_lower(sciname)) %>%  # ensure sciname is lowercase
  left_join(species_info, by = "sciname") %>%
  group_by(Role)%>%
  slice_max(order_by = consumption_live_t, n = 25) %>%
  mutate(label_name = ifelse(!is.na(CommonName), paste(str_to_sentence(sciname), " / ", CommonName), str_to_sentence(sciname))) %>%
  mutate(Role = factor(Role, levels = c("Source", "Exporter", "Consumer"))) %>%
  arrange(desc(consumption_live_t)) %>%
  mutate(rank_weight = row_number(),
         sciname_ranked_weight = paste0(rank_weight, ". ", sciname)) %>%
  arrange(desc(count)) %>%
  mutate(rank_count = row_number(),
         sciname_ranked_count = paste0(rank_count, ". ", sciname),.groups='drop') 



#### plot simple species bar chart - tonnes ####
ggplot(species_combined, 
       aes(x =  reorder(sciname, -consumption_live_t), 
           y = consumption_live_t)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(
    expand = c(0.01, 0),
    labels = function(x) {
      ifelse(x >= 1e9, paste0(x / 1e9, "B"),
             ifelse(x >= 1e6, paste0(x / 1e6, "M"),
                    ifelse(x >= 1e3, paste0(x / 1e3, "K"), x)))
    }
  )+
  labs(title = "Most Traded Species (weight)",
       x = NULL,
       y = "Tonnes") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),  
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(size = 12),
    legend.position = "bottom",  # move legend to the bottom for better layout
    legend.title = element_text(face = "bold"),  # make the legend title stand out
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

## save call


#### plot top species bar chart by role - tonnes ####
ggplot(species_combined_by_role, 
       aes(x = reorder_within(sciname_ranked_weight, -consumption_live_t, Role), 
           y = consumption_live_t)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_wrap(~Role, scales = "free") +
  scale_x_reordered() +  # Maintain correct ordering
  scale_y_continuous(
    expand = c(0.01, 0),
    labels = function(x) {
      ifelse(x >= 1e9, paste0(x / 1e9, "B"),
             ifelse(x >= 1e6, paste0(x / 1e6, "M"),
                    ifelse(x >= 1e3, paste0(x / 1e3, "K"), x)))
    }
  )+
  labs(title = "Most Traded Species by Role (weight)",
       x = NULL,
       y = "Tonnes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        plot.title = element_text(hjust=0.5),plot.subtitle = element_text(hjust=0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
# ggsave("output/species_weight.jpg")
# ggsave("output/IDN/species_weight.jpg")






#### [4] what roles are countries playing? ####
# pivot longer to get roles in one column
country_role_counts <- country_trade %>% 
  pivot_longer(cols = c(source_country_iso3c, exporter_iso3c, consumer_iso3c),
               names_to = "role", values_to = "country") %>%
  filter(!is.na(country)) %>%
  group_by(country, role) %>%
  summarize(count = n(), weight=sum(consumption_live_t),.groups = 'drop') %>%
  group_by(role) %>%
  mutate(role = recode(role,
                       "source_country_iso3c" = "Source","source_country_short" = "Source",
                       "exporter_iso3c" = "Exporter","exporter_country_short" = "Exporter",
                       "consumer_iso3c" = "Consumer","consumer_country_short" = "Consumer"),
         role = factor(role, levels = c("Source", "Exporter", "Consumer"))) 


#### plot the facet-wrapped bar plot ####
# first line picks top 30 countries
ggplot(country_role_counts %>% slice_max(order_by = count, n = 30),
       aes(x = tidytext::reorder_within(country, count, role), 
           y = count, fill = role)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ role, scales = "free") +
  tidytext::scale_x_reordered(labels = function(x) {
    sapply(x, function(label) {
      country_name <- sub("___.*", "", label)  #
    })
  }) +
  labs(title = "Country Roles",
       x = "Country",
       y = "Trade Transactions",
       fill = "Role") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_markdown(size = 12),  # markdown needed for bold
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 11, face = "bold"))
# ggsave("output/country_roles.jpg")





