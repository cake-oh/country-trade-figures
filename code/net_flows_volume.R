

#### [0] prep ####
library(tidyverse)
library(countrycode)

net_flow <- read_csv("output/national_netflows_foreigndomestic.csv")


#### [1] establish country of interest ####
# can be one or multiple countries
# country_of_interest = "IDN"
country_of_interest = c("IDN","PNG")


#### [2] prep data ####
net_flow_plot <- net_flow %>%
  mutate(Country_full = countrycode(Country,origin = "iso3c",destination="country.name"),
         Country_plot = ifelse(nchar(Country_full) > 10, Country, Country_full),
         Country_full = ifelse(Country_plot=="FSM","Micronesia (Fdr. Sts.)",Country_full))


#### [3] plot ####
ggplot(net_flow_plot %>%
         filter(Country %in% country_of_interest) %>% arrange(weight)) +
  geom_col(aes(flow_metric,weight,
               group=flow_metric,fill=flow_metric),
           position="stack",show.legend = TRUE) + 
  facet_wrap(~Country_full,scales="free_y")+
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 8),  # try increasing 'n' for more ticks
    labels = scales::label_number(scale_cut = scales::cut_short_scale())
  )+
  labs(
    title = "Net Trade Volume Flows by Source",
    x=NULL,y="Weight (tonnes)")+
  scale_fill_discrete(name="Flow Source") +
  geom_hline(yintercept=0,linewidth=.2)+
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

# ggsave(paste0("output/",coucountry_of_interest# ggsave(paste0("output/",country_to_plot,"/netweights_allsources.jpg"))