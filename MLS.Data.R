### Import Excel Files ###

library(readxl)

mls.data <- read_excel("~/Downloads/Single Game Market Case Study_WL.xlsx")
twins.play <- read_excel("~/Downloads/Twins Play.xlsx")


### Clean Data ###

library(dplyr)

mls.data <- as.data.frame(mls.data)

# Negative Values 
# N/A Values 
mls.data <- mls.data %>%
  filter(`Days Out`>= 0) %>%  
  na.omit()                   


twins.play <- as.data.frame(twins.play)

# Format Date Column for Twins Overlap
twins.play$Gamedate <- as.numeric(twins.play$Gamedate)
twins.play$Gamedate <- as.Date(as.POSIXct(as.Date(twins.play$Gamedate,origin="1899-12-30")))

twins.play <- na.omit(twins.play)


### Format Data for Vizualization ###

library(stringr)

# Addition of Opponent Only Column
mls.data$Opponent <- word(mls.data$`Event Name`, 4, -1) 

# Addition of Day of the Week Column
mls.data$Day <- weekdays(as.Date(mls.data$Gamedate)) 

# Sum Tickets and Revenue for Each Game
# Addition of Divison or Non-Division Designation
mls.gamedate <- mls.data %>%
  group_by(`Gamedate`, Opponent, Day) %>%
  summarize(Tickets_Total = sum(Tickets), Revenue_Total = sum(Revenue)) %>%
  mutate(Div_Non = ifelse(Opponent %in% c("Los Angeles FC", "Seattle Sounders", "LA Galaxy", 
                                          "Real Salt Lake", "FC Dallas", "Portland Timbers",
                                          "San Jose Earthquakes"), "Division", "Non")) #Addition of Divison or Non-Division Designation

# Average Tickets and Revenue for Div/Non Each Day of the Week
mls.dv.non.avg <- mls.gamedate %>%
  group_by(Day, Div_Non) %>%
  summarize(Avg_Tickets = mean(Tickets_Total), Avg_Revenue = mean(Revenue_Total))

# Daily Total Average Tickets and Revenue
mls.avg <- mls.gamedate %>%
  group_by(Day) %>%
  summarize(Avg_Tickets = mean(Tickets_Total), Avg_Revenue = mean(Revenue_Total))

# Combine Twins Play and United Play by Gamedate Column
mls.gamedate$Gamedate <- as.Date(mls.gamedate$Gamedate)
twins.merg <- merge(x = twins.play , y = mls.gamedate, by = "Gamedate")

# Average Tickets and Revenue Based on if Twins Play (Y/N)
mls.twins <- twins.merg %>%
  group_by(twins_play) %>%
  summarize(Avg_Tickets = mean(Tickets_Total), Avg_Revenue = mean(Revenue_Total))
  

### Vizualizations ###

library(ggplot2)

p.line = c('#9BCDE4', '#DF2426')
p.fill = c('#9BCDE4', '#D8DAD9')
p.limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
p.labels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")

# Average Tickets Sold Per Day by Div/Non Stacked Bar Chart
# Daily Total Average Tickets Sold Line
p1.tickets = ggplot(mls.dv.non.avg) +
  geom_bar(aes(x = Day, y = Avg_Tickets, fill = Div_Non, color = Div_Non), 
           stat = "identity", width = 0.5, position = position_dodge(width = 0.6)) +
  scale_color_manual(values = p.line) +
  scale_fill_manual(values = p.fill) +
  scale_x_discrete(limits = p.limits, labels = p.labels) +
  scale_y_continuous(name = "Average # Tickets Sold", limits = c(0, 5500), expand = c(0,0)) +
  geom_line(data = mls.avg, aes(x = Day, y = Avg_Tickets, group = 1, linetype = "Daily Total Average"), size = 0.75) +
  scale_linetype_manual(values = 'twodash') +
  theme_minimal() +
  theme(axis.line = element_line(color="black"),
        axis.ticks = element_line(color="black"),
        panel.border = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12))

#p1.tickets

# Average Revenue Per Day by Div/Non Stacked Bar Chart
# Daily Total Average Revenue Line
p2.revenue = ggplot(mls.dv.non.avg) +
  geom_bar(aes(x = Day, y = Avg_Revenue, fill = Div_Non, color = Div_Non), 
           stat = "identity", width = 0.5, position = position_dodge(width = 0.6)) +
  scale_color_manual(values = p.line) +
  scale_fill_manual(values = p.fill) +
  scale_x_discrete(limits = p.limits, labels = p.labels) +
  scale_y_continuous(name = "Average Revenue", limits = c(0, 650000), labels = c("0", "200K", "400K", "600K"), expand = c(0,0)) +
  geom_line(data = mls.avg, aes(x = Day, y = Avg_Revenue, group = 1), linetype = "twodash", size = 0.75) +
  theme_minimal() +
  theme(axis.line = element_line(color="black"),
        axis.ticks = element_line(color="black"),
        panel.border = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12))

#p2.revenue

# Average Tickets Sold based on if Twins Play Bar Chart
p3.twins = ggplot(mls.twins) +
  geom_bar(aes(x = factor(twins_play), y = Avg_Tickets, fill = factor(twins_play)), stat = "identity", color = '#B9975B', size = 1.5) +
  theme_minimal() +
  scale_y_continuous(name = "Average # Tickets Sold", limits = c(0, 3800), expand = c(0,0)) +
  scale_fill_manual(name = "Twins Game", values = c('#002B5C','#D31145'), labels = c("No", "Yes")) +
  theme(axis.line = element_line(color="black"),
        axis.ticks = element_line(color="black"),
        panel.border = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

#p3.twins

# Average Revenue Based on if Twins Play
p4.twins = ggplot(mls.twins) +
  geom_bar(aes(x = factor(twins_play), y = Avg_Revenue, fill = factor(twins_play)), stat = "identity", color = '#B9975B', size = 1.5) +
  theme_minimal() +
  scale_fill_manual(name = "Twins Game", values = c('#002B5C','#D31145'), labels = c("No", "Yes")) +
  scale_y_continuous(name = "Average Revenue", limits = c(0, 265000), 
                     labels = c("0", "100K", "200K", "300K"),
                     breaks = c(0, 100000, 200000, 300000), expand = c(0,0)) +
  theme(axis.line = element_line(color="black"),
        axis.ticks = element_line(color="black"),
        panel.border = element_blank(),
        legend.position = "none",
        legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

#p4.twins

# Combine Daily Div/Non Breakdowns
library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p2.revenue), ggplotGrob(p1.tickets), size = "last"))

# Combine Twins Play Breakdowns
library(ggpubr)
ggarrange(p3.twins, p4.twins, ncol=2, nrow=1, common.legend = TRUE, legend="bottom") 


### Write Dataframes to Excel

write.table(x = mls.avg, file = "mls.avg.csv", row.names = F, sep = ",")
write.table(x = mls.dv.non.avg, file = "mls.dv.non.avg.csv", row.names = F, sep = ",")
write.table(x = mls.twins, file = "mls.twins.csv", row.names = F, sep = ",")

