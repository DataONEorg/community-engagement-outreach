source("./scripts/clean_data.R")

## Load libraries ####
library(ggplot2)
library(ggthemes)

## Clean data ####
names(raw_data)
data_distance_and_how_to_access <- select(raw_data, unique_ID, input_how_to_access, input_data_distance)
data_distance_and_how_to_access$input_data_distance[data_distance_and_how_to_access$input_data_distance=="not accessible"] <- 4
data_distance_and_how_to_access$input_data_distance[data_distance_and_how_to_access$input_data_distance=="not found"] <- 4
data_distance_and_how_to_access$input_data_distance <- as.integer(data_distance_and_how_to_access$input_data_distance)

# Get averages by input_how_to_access
# replace any not accessible with a 4
data_distance_and_how_to_access
unique(data_distance_and_how_to_access$input_data_distance)

data_distance_and_how_to_access %>%
  group_by(input_how_to_access) %>%
  dplyr::summarize(Mean = mean(input_data_distance, na.rm = TRUE))

## Make figure ####
gg <- ggplot(data_distance_and_how_to_access, aes(x = input_how_to_access, y = input_data_distance)) +
  geom_boxplot()
gg <- gg + theme_tufte()
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Study System")
gg <- gg + theme(axis.text.x = element_text(size=15, angle = 90, hjust = 1, vjust = .5),
                   axis.text.y = element_text(size=15),
                   axis.title = element_text(size=20))
gg

gg <- ggplot(data_distance_and_how_to_access, aes(x = input_how_to_access, y = input_data_distance)) +
  geom_jitter(alpha = 0.6, aes(color = input_how_to_access))
gg <- gg + theme_tufte()
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Study System")
gg <- gg + theme(axis.text.x = element_text(size=15, angle = 90, hjust = 1, vjust = .5),
                 axis.text.y = element_text(size=15),
                 axis.title = element_text(size=20),
                 legend.position = "NONE")
gg

