## goal: plot stacked bar charts of consumption/export ratios


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
country_of_interest <- "IDN"
country_of_interest <- c("IDN","PNG")



######## [goal 1] ########
#### [2] prep data ####
country_trade <- nutrients_cons_og %>%
  filter(source_country_iso3c %in% country_of_interest |
           exporter_iso3c %in% country_of_interest |
           consumer_iso3c %in% country_of_interest)


#### [3] what do production ratios look like? ####
domestic_production <- nutrients_cons_og %>%
  filter(consumption_source == "domestic") %>%
  select(Country = source_country_iso3c,Species = sciname, Quantity_species = consumption_live_t,consumption_source) %>%
  group_by(Country) %>%
  summarise(Country_domestic = sum(Quantity_species))
domestic_perc <- nutrients_cons_og %>%
  select(Country = source_country_iso3c, Species = sciname, Quantity_species = consumption_live_t,consumption_source) %>%
  group_by(Country) %>%
  summarise(Country_total = sum(Quantity_species)) %>% ungroup() %>%
  right_join(domestic_production) %>%
  mutate(Domestic=Country_domestic/Country_total) %>%
  mutate(Exported=1-Domestic)
domestic_perc_long <- domestic_perc %>%
  pivot_longer(
    cols = c("Exported","Domestic"), 
    names_to = "Metric",values_to = "Value") %>%
  mutate(Metric = ifelse(Metric == "Domestic", "Domestically Consumed", Metric),
         Metric = ifelse(Metric == "Exported", "Exported (Foreign)", Metric),
         Metric = factor(Metric, levels = c("Exported (Foreign)","Domestically Consumed"))) %>%
  filter(Country %in% country_of_interest)
# grab ratio of domestically consumed production
pf_ratio <- domestic_perc_long$Value[domestic_perc_long$Metric=="Domestically Consumed"]*100

## stacked consumed vs. exported production
p_production_fate <- ggplot(domestic_perc_long,
                            aes(Country,
                                Value,fill=Metric)) + 
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.2, alpha = 0.8) +
  guides(fill=guide_legend(nrow = 1)) +
  scale_fill_brewer(palette="Set2")+
  labs(x=NULL,y=NULL,
       title="Production Fate Ratio",
       fill=NULL) +
  scale_y_continuous(breaks=seq(0,1,.25), labels=seq(0,1,.25),
                     expand = expansion(mult = c(0, 0.01)))+
  geom_label(data = data.frame(Country = country_of_interest, y = .8),
             aes(x = Country, y = y,
                 label = paste0(round(pf_ratio, 2), "% of production\nis domestically consumed")),
             hjust = 0.5, vjust = -0.5,
             fill = "white",color = "black", show.legend = FALSE)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=12),  
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=12),
        plot.title = element_text(hjust=0.5,size=13),
        legend.text = element_text(size=13),
        legend.position = "bottom")
p_production_fate
# ggsave("output/production_domesticconsumption_countries.jpg")
# ggsave("output/IDN/production_domesticconsumption_countries.jpg")
# ggsave("output/production_domesticconsumption_countries_NSFSiteVisit.jpg")




#### [4] what do consumption ratios look like? ####
## share of consumption produced domestically and share imported
national_consumption <- nutrients_cons_og %>%
  select(Country = consumer_iso3c,Species = sciname, Quantity_species = consumption_live_t,
         consumption_source) %>%
  group_by(Country) %>%
  mutate(Country_total = sum(Quantity_species)) %>% ungroup() %>%
  group_by(Country,consumption_source) %>%
  mutate(Quantity_source=sum(Quantity_species)) %>%
  summarise(Prop=Quantity_source/Country_total) %>% distinct()
national_consumption_domestic <- national_consumption %>%
  filter(Prop!=1) %>%
  mutate(consumption_source = ifelse(consumption_source == "domestic", "Domestically Produced", consumption_source),
         consumption_source = ifelse(consumption_source == "foreign", "Imported (Foreign)", consumption_source),
         consumption_source = factor(consumption_source, levels = c("Imported (Foreign)","Domestically Produced"))) %>%
  filter(Country %in% country_of_interest)
# grab ratio of domestically consumed production
co_ratio <- national_consumption_domestic$Prop[national_consumption_domestic$consumption_source=="Domestically Produced"] * 100


## plot
p_consumption_origin <- ggplot(national_consumption_domestic
                               ,
                               aes(Country,
                                   Prop,fill=consumption_source)) + 
  geom_bar(stat = "identity", position = "stack", color = "black", size = 0.2, alpha = 0.8) +
  scale_fill_brewer(palette = "Set3")+
  guides(fill=guide_legend(nrow = 1)) +
  labs(x=NULL,y=NULL,
       title = "Consumption Origin Ratio", # title="Where does consumed seafood originate from?",
       fill=NULL) +
  scale_y_continuous(breaks=seq(0,1,.25), labels=seq(0,1,.25),
                     expand = expansion(mult = c(0, 0.01)))+
  geom_label(data = data.frame(Country = country_of_interest, y = .8),
             aes(x = Country, y = y,
                 label = paste0(round(co_ratio, 2), "% of consumption\nis domestically produced")),
             hjust = 0.5, vjust = -0.5,
             fill = "white",,color = "black", show.legend = FALSE)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=12),  
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=12),
        plot.title = element_text(hjust=0.5,size=13),
        legend.text = element_text(size=13),
        legend.position = "bottom")
p_consumption_origin
# ggsave("output/consumption_domesticproduction_countries.jpg")
# ggsave("output/IDN/consumption_domesticproduction_countries.jpg")


#### plot stacked bar chart of production fate and consumption origin ratios ####
library(cowplot)
p_ratios_comb <- ggdraw() +
  draw_label("Production and Consumption Ratios", fontface = 'bold', x = 0.5, y=.97, hjust = 0.5, size = 16) +
  draw_plot(
    plot_grid(p_production_fate, p_consumption_origin, ncol = 2),
    y = 0, height = 0.95
  )
p_ratios_comb
# ggsave("output/combined_ratios_barchart.jpg")








######## [goal 2] ########
### works for a single country! ###
#### [5] what species are in domestic consumption? ####
sp_ratio_dom <- nutrients_cons_og %>% 
  filter(source_country_iso3c %in% country_of_interest & consumption_source == "domestic") %>%
  group_by(source_country_iso3c,sciname) %>%
  summarise(sp_total=sum(consumption_live_t),.groups='drop') %>%
  mutate(sp_prop = sp_total/sum(sp_total)*100) %>% 
  arrange(desc(sp_total)) %>%
  slice_head(n=40)


#### plot bar chart of prominent species ####
ggplot(sp_ratio_dom,aes(reorder(sciname,-sp_prop),sp_prop,
                        group=sciname,fill=sciname)) +
  geom_bar(stat = "identity",position="stack",
           color="black",size = 0.2,
           show.legend = FALSE) +
  geom_text(aes(label = paste0(round(sp_prop, 1),"%")), 
            vjust = 0,hjust=0, nudge_y = .05, nudge_x=-.1, size = 3,angle=45) +
  scale_y_continuous(#limits=c(0,20),
    expand = expansion(mult = c(0, 0.1)))+
  labs(title = "Species in Domestic Production/Consumption",
       subtitle = paste0("Top ",length(unique(sp_ratio_dom$sciname))," Species, ~",
                         signif(sum(sp_ratio_dom$sp_prop), 2),
                         "% of total weight"),
       x=NULL,y="Percentage") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5), 
        axis.text.x = element_text(angle=45,vjust=1,hjust=1),
        plot.margin = margin(0.5,.5,0.5,.5, "cm"))
# ggsave("output/IDN/domestic_sp_proportions.jpg")
