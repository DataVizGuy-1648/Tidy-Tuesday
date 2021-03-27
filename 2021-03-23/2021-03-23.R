library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2021-03-23')

unvotes <- tuesdata$unvotes
issues <- tuesdata$issues
roll_calls <- tuesdata$roll_calls

unvotes
issues
roll_calls

df <- unvotes %>%
  inner_join(issues, by = "rcid")

df <- roll_calls %>% 
  inner_join(df, by = "rcid")

df$country <- recode(df$country, `United Kingdom` = "UK")
df$country <- recode(df$country, `United States` = "US")

p <- df %>%
  filter(country %in% c("US", "China",
                        "France", "UK", 
                        "Russia")) %>%
  group_by(issue, vote, country) %>%
  count() %>%
  ggplot(aes(x = reorder((issue), n), y = n, fill = vote)) +
  geom_col(color = "black") +
  coord_flip() +
  scale_x_discrete(labels = scales::wrap_format(40)) +
  scale_fill_brewer(labels = c("Abstain",
                                 "No",
                                 "Yes"),
                      guide = guide_legend(reverse = TRUE),
                    palette = "Pastel2") +
  labs(title = "How Permanent Members of the\nUN Security Council vote on\nvarious issues",
       subtitle = "1946-present",
       y = "",
       x = "",
       fill = "",
       caption = "Tidy Tuesday Week 13, 2021") +
  facet_grid(country ~ .) +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "beige",
                                     color = "black"),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    panel.background = element_rect(fill = "beige"),
    plot.background = element_rect(fill = "beige"),
    strip.background = element_rect(fill = "white"),
    text = element_text(family = "Georgia"),
    plot.title = element_text(face = "bold")
  )

p

ggsave("un-votes.png",
       plot = p,
       height = 5.34,
       width = 5.34,
       unit = "in")
