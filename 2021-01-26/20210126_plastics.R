library(tidyverse)

tuesdata <- tidytuesdayR::tt_load("2021-01-26")
df <- tuesdata$plastics

df %>%
  view()

df$country <- recode_factor(as.factor(df$country),
                            "United States of America" = "USA",
                            "United Kingdom of Great Britain & Northern Ireland" = "UK",
                            "United Arab Emirates" = "UAE")


p <- df %>%
  filter(year == 2020 & country != "Korea") %>%
  select(country, parent_company, volunteers) %>%
  group_by(country) %>%
  summarize(total_vols = sum(volunteers)) %>%
  arrange(desc(total_vols)) %>%
  top_n(20) %>% 
  mutate(continent = case_when(
    country %in% c("Nigeria", "Tanzania", "Kenya",
                   "Ghana") ~ "Africa",
    country %in% c("Ukraine", "Switzerland", 
                   "Denmark", "UK", "Montenegro",
                   "Spain") ~ "Europe",
    country %in% c("USA", "Colombia") ~ "Americas",
    country == "Australia" ~ "Oceania",
    TRUE ~ "Asia"
  )) %>%
  ggplot(aes(x = reorder(country, total_vols), 
             y = total_vols/1000,
             fill = continent)) +
  geom_col(color = "black") +
  coord_flip() +
  scale_y_continuous(breaks = c(seq(100, 700, by = 100))) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Who Volunteers the Most to Remove Plastics?",
       subtitle = "Top 20 Countries in Number of Volunteers",
       y = "Number of Voluneteers (in thousands)",
       x = "",
       fill = "",
       caption = "Source: Break Free from Plastic\nTidy Tuesday Week 4, 2021") +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = c(.8, .5),
    legend.background = element_rect(fill = "#e4f2da",
                                     color = "black"),
    text = element_text(family = "Georgia"),
    axis.text = element_text(face = "bold"),
    plot.background = element_rect(fill = "#e4f2da"),
    panel.background = element_rect(fill ="#e4f2da"),
    panel.grid.major = element_line(color = "black",
                                    linetype = "dotted"),
    panel.grid.minor = element_blank()
  )

ggsave("plastics.png",
       plot = p,
       width = 5.65,
       height = 4.73,
       units = "in")


options(scipen = 999)

