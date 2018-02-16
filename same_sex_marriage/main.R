# Set working directory to clustering.R file path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load libraries
require(ggplot2)
require(data.table)
require(dplyr)

# Load data
data <- read.csv("same_sex_marriage_data.csv",header = T)

# Remove silly "X" from colnames
colnames(data) <- sub("X", "", colnames(data))

# Melt the data by state
molten_data <- melt(data[,-2], id=1)
# Convert legality to factor for plotable numeric representations
molten_data$value <- as.factor(molten_data$value)
# Properly order legal status from None to Legal
molten_data$value <- ordered(molten_data$value, levels = c("No Law", 
                                                           "Statutory Ban", 
                                                           "Constitutional Ban", 
                                                           "Legal"))
# Get first year of Legality by state
custom_order <- molten_data %>%
  group_by(State) %>%
  arrange(variable) %>%
  filter(value == "Legal")
# Apply custom x-axis order
molten_data$State <- factor(as.character(molten_data$State), levels = unique(custom_order$State))

# Plotting magic
almost_lgbt <- c("#e40303", "#ff8c00", "#ffed00", "#008026")

ggplot(molten_data, aes(x = State, y = reorder(variable, -as.numeric(variable)))) +
  geom_tile(aes(fill = value)) +
  theme_minimal() +
  scale_fill_manual(values = almost_lgbt) +
  theme(axis.text.x=element_text(angle=40, hjust = 1)) +
  labs(x = "State",
       y = "Year",
       fill = "Legality",
       title = "Same-sex Marriage Status by US State and Year",
       caption = paste0("States sorted by earliest adoption")
       )

ggsave("same_sex_per_state_per_year.png",
       width = 11,
       height = 6)
