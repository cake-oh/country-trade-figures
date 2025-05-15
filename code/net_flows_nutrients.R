## goal 2: plot trade AND domestic consumption metrics - good for individual countries!
## subgoal 2a: plot net RNI flows from trade
## subgoal 2b: plot population % impacts from trade



#### [0] prep ####
library(tidyverse)
library(tidytext)

## load data
rni_perc_df <- read_csv("output/nutrient_rni_percpop_global_HS96.csv") # trade WITHOUT domestic consumption
rni_perc_df_dom <- read_csv("output/nutrient_rni_percpop_global_domesticconsumption_HS96.csv") # ONLY domestic consumption


#### [1] establish country of interest ####
# can be one or multiple countries
# country_of_interest = "IDN"
country_of_interest = c("IDN","PNG")


#### [2] prep net flows ####
rni_perc_combined <- bind_rows(rni_perc_df,rni_perc_df_dom) %>%
  group_by(Country,category) %>% 
  summarize(net_flow = sum(net_flow),
            rni_amount = sum(rni_amount),
            net_flow_pc = sum(net_flow_pc),
            net_flow_pc_daily = sum(net_flow_pc_daily),
            rni_ratio_perc_pop = sum(rni_ratio_perc_pop),
            rni_ratio_perc_pop_daily = sum(rni_ratio_perc_pop_daily)) %>% ungroup() %>%
  group_by(Country) %>% arrange(-rni_amount)




#### [3] plot ####
ggplot(rni_perc_combined %>%
         filter(Country %in% country_of_interest) %>%
         mutate(category = fct_reorder(category, rni_amount, .desc = TRUE)),  # Reorder categories
       aes(x = category,
           y = rni_amount, fill = category, group = category)) +
  geom_bar(stat = "identity", width = 0.8,show.legend=FALSE) +  # adjusted bar width for cleaner look
  geom_hline(yintercept=0,linewidth=.2)+
  scale_x_reordered() + 
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 5),  # try increasing 'n' for more ticks
    labels = scales::label_number(scale_cut = scales::cut_short_scale())
  )+
  labs(
    x = NULL,
    y = "# of RNIs",
    title = "Net Nutrient Changes from Seafood Trade",
    subtitle = "Recommended Nutrient Intakes Gained or Lost 1996-2020",
    fill = "Country"  # ensure legend is clear for country colors
  ) +
  facet_wrap(~Country,scales = "free") +
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

## save output
folder_name <- if (length(country_of_interest) > 1) "multiple_countries" else country_of_interest
# ggsave(paste0("output/", folder_name, "/netflows_rni_by_nutrient.jpg"))

