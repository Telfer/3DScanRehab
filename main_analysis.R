## Analysis of scan data

# libraries required
library(tidyverse)
library(nlme)

# import data
scan_data <- read_csv(paste0("C:/Users/telfe/Dropbox/My_Projects/3D Scanning", 
                             " rehab/ACLR/analysis/Injured.csv"))

# format
scan_data$timepoint <- factor(scan_data$timepoint, 
                              levels = c("pre", "2wk", "6wk", "12wk", "26wk"))
scan_data$subject <- as.factor(scan_data$subject)
scan_data$region <- as.factor(scan_data$region)

## plot
# boxplots
g <- ggplot(scan_data, aes(x = timepoint, y = absolute_vol, fill = region))
g <- g + geom_boxplot()
g

# line
g1 <- ggplot(scan_data, aes(x = timepoint, y = vol_change, group = region, colour = region))
g1 <- g1 + stat_summary(fun.y = mean, geom = "line", 
                        aes(group = region, colour = region))
g1 <- g1 + stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1)
g1 <- g1 + labs(x = "Timepoint", y = "% Volume Change")
g1 <- g1 + scale_colour_discrete(name = "Regions", labels = c("Patella", "Vastus Lateralis", 
                                                              "Vastus Medialis"))
g1 <- g1 + annotate("text", x = 2, y = 19, label = "*", size = 8, colour = "red")
g1 <- g1 + annotate("text", x = 3, y = 7, label = "*", size = 8, colour = "red")
g1 <- g1 + annotate("text", x = 5, y = 5, label = "*", size = 8, colour = "red")
g1 <- g1 + annotate("text", x = 2, y = 6.5, label = "*", size = 8, colour = "green")
g1 <- g1 + annotate("text", x = 3.93, y = -1.5, label = "*", size = 8, colour = "blue")
g1 <- g1 + annotate("text", x = 4.07, y = -1.5, label = "*", size = 8, colour = "green")
g1 <- g1 + theme_bw()
g1

ggsave("Figure 3.png", g1, dpi = 300, height = 5, width = 6)


# Generalized linear growth models for time on scan data
ctrl <- lmeControl(opt = 'optim', maxIter = 100)

## Vastus Medialis
vm_df <- filter(scan_data, region == "VM")
vm_lme <- lme(absolute_vol ~ timepoint, random = ~ timepoint | subject, vm_df, 
              na.action = na.exclude) 
summary(vm_lme)
vm_ints <- intervals(vm_lme, which = "fixed")
vm_ints_perc <- (vm_ints$fixed / vm_ints$fixed[1, 2]) * 100
vm_ints_perc

## Vastus Lateralis
vl_df <- filter(scan_data, region == "VL")
vl_lme <- lme(absolute_vol ~ timepoint, random = ~ timepoint | subject, control = ctrl, vl_df, 
              na.action = na.exclude)
summary(vl_lme)
vl_ints <- intervals(vl_lme, which = "fixed") 
vl_ints_perc <- (vl_ints$fixed / vl_ints$fixed[1, 2]) * 100
vl_ints_perc

## Patella
pt_df <- filter(scan_data, region == "PT")
pt_lme <- lme(absolute_vol ~ timepoint, random = ~ timepoint | subject, pt_df, 
              na.action = na.exclude) 
summary(pt_lme)
pt_ints <- intervals(pt_lme, which = "fixed") 
pt_ints_perc <- (pt_ints$fixed / pt_ints$fixed[1, 2]) * 100


# Generalized linear growth models for scan data vs ikdc data
## Vastus Medialis
vm_ik_df <- scan_data %>% filter(region == "VM") %>% filter(timepoint != "pre")
vm_ik_lme <- lme(absolute_vol ~ ikdc, random = ~ 1 | subject, vm_ik_df)
summary(vm_ik_lme)
intervals(vm_ik_lme, which = "fixed")

## Vastus Lateralis
vl_ik_df <- scan_data %>% filter(region == "VL") %>% filter(timepoint != "pre")
vl_ik_lme <- lme(absolute_vol ~ ikdc, random = ~ 1 | subject, vl_ik_df)
summary(vl_ik_lme)
intervals(vl_ik_lme)

## Patella
pt_ik_df <- scan_data %>% filter(region == "PT") %>% filter(timepoint != "pre")
pt_ik_lme(absolute_vol ~ ikdc, random = ~ 1 | subject, pt_ik_df)
summary(pt_ik_lme)
intervals(pt_ik_lme)

