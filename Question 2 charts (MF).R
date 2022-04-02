# Get the Data
library(tidyverse)

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')

world_map <- map_data("world")

# separating tectonic settings into 2 columns
tecerup <- eruptions %>%
  left_join(volcano, by = "volcano_number") %>%
  separate(tectonic_settings, into = c("tectonic_plate", "crust"))

# plot of all volcanoes, varying sizes based on vei
ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(long, lat, map_id = region),
           fill = "gray") +
  geom_point(data = tecerup, 
             aes(longitude.y, latitude.y, size = vei, color = tectonic_plate),
             alpha = 0.4)

# calculates mean vei for each tectonic plate
tecerup2 <- tecerup %>%
  group_by(tectonic_plate) %>%
  summarize(vei = mean(vei, na.rm = TRUE))

# bar plot of mean vei for each volcano tectonic plate
tecvei <- ggplot(data = tecerup2, aes(x = tectonic_plate, y = vei, color = "Blue", fill = "Cyan")) +
  geom_bar(stat = "identity")

tecvei

# top 500 volcanoes with highest population density with a 5 km radius
topvol <- volcano %>%
  arrange(desc(volcano$population_within_5_km))
topvol2 <- topvol[1:500,-c(18:22)]

m1 <- as.data.frame(table(topvol[,13]))
m2 <- as.data.frame(table(topvol[,14]))
m3 <- as.data.frame(table(topvol[,15]))
m4 <- as.data.frame(table(topvol[,16]))
m5 <- as.data.frame(table(topvol[,17]))

topvol3 <- m1 %>%
  full_join(m2,by="Var1") %>%
  full_join(m3,by="Var1") %>%
  full_join(m4,by="Var1") %>%
  full_join(m5,by="Var1") %>%
  rename(Rock.Types = Var1)

final <- topvol3 %>%
  mutate(Count=rowSums(topvol3[,2:6],na.rm=TRUE)) %>%
  select(1,7)


favrocks <- ggplot(final[-11,], aes(x = "", y = Count, fill = Rock.Types)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  xlab("") + ylab("Frequency of Major Rock Types") + labs(fill = "Major Rock Types") +
  geom_text(aes(label = Rock.Types), color = "white", size=2, position = position_stack(vjust = 0.5)) +
  geom_text(aes(label = Count), color = "black", size=2, position = position_stack(vjust = 0.3)) +
  ggtitle("Plot of the Most Common Major Rock Types \n for the Top 500 Volcanoes with the Highest Population within 5 km") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))
favrocks
