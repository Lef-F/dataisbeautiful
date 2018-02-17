# Set working directory to clustering.R file path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load libraries
require(ggplot2)
require(data.table)
require(reshape2)
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
legality_order <- c("No Law", 
                    "Statutory Ban", 
                    "Constitutional Ban", 
                    "Legal")

molten_data$value <- ordered(molten_data$value, levels = legality_order)

# Get first year of change of legality by state
legal_change <- molten_data %>%
  group_by(State, variable) %>%
  arrange(factor(value, levels = rev(legality_order))) %>%
  group_by(State, value) %>%
  slice(1)

# Convert melted long format back to a wide format
legal_change <- dcast(legal_change, State ~ value, value.var = "variable")

# Sort by year of legality order change
custom_order <-  legal_change %>%
  arrange(Legal, `Constitutional Ban`, `Statutory Ban`, `No Law`)

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
