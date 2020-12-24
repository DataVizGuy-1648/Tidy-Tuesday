tuesdata <- tidytuesdayR::tt_load('2020-12-15')

ninja <- tuesdata[[1]]

str(ninja)

# tidying

ninja$location <- as.factor(ninja$location)
ninja$round_stage <- as.factor(ninja$round_stage)
ninja$obstacle_name <- as.factor(ninja$obstacle_name)
ninja$obstacle_order <- as.factor(ninja$obstacle_order)

ninja$name_order <- as.factor(paste(ninja$obstacle_order, ninja$obstacle_name))

library(DataExplorer)

plot_bar(ninja)
plot_histogram(ninja)



library(tidyverse)

ninja %>%
  count(location, round_stage, sort = T) %>%
  ggplot() +
  geom_col(aes(x = location, y = n, fill = round_stage),
           position = "stack") +
  coord_flip()
