############## MSDS Orientation slide #####################
library(tidyverse)
library(readxl)

MSDSNew <- MSDS_Survey %>%
  mutate(OSNew = case_when(
    grepl("Windows", Operating_System, ignore.case = TRUE) ~ "Windows",
    grepl("MacOS", Operating_System, ignore.case = TRUE) ~ "MacOS",
    grepl("Linux", Operating_System, ignore.case = TRUE) ~ "Linux",
    TRUE ~ "Other"
  ))
  
hist(MSDS_Survey$CPU_Cores, xlab = "CPU Cores", ylab = "Count", main = "Distribution of CPU Cores",
     col = "seagreen1")
abline()
lines(density(MSDS_Survey$CPU_Cores), col = 'blue', lwd = 3)

ggplot(MSDSNew, aes(x=CPU_Cores, color=OSNew, fill=OSNew)) +
  geom_histogram(position="identity", alpha=0.5)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=sex),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title="Weight histogram plot",x="Weight(kg)", y = "Count")+
  theme_classic()

# Filter out "Other" if not needed
MSDS_filtered <- MSDSNew %>% filter(OSNew != "Other")

# Create the plot
ggplot(MSDS_filtered, aes(x=CPU_Cores, color=OSNew)) +
  geom_histogram(alpha=0.5, position="identity")

ggplot(MSDS_filtered, aes(x=CPU_Cores, color=OSNew)) +
  geom_histogram(fill="white", position="dodge")+
  theme(legend.position="top")

# Create the plot
ggplot(MSDS_Survey, aes(x = CPU_Cores)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, fill = "seagreen1", color = "black", alpha = 0.7) +
  geom_density(color = "blue", size = 1) +
  labs(title = "Distribution of CPU Cores", x = "CPU Cores", y = "Density") +
  theme_minimal()
