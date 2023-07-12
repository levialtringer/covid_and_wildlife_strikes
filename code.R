
########################################################################################################################
#                                   Replication Code for "The Impact of the COVID-19                                   #
#                               Pandemic on Wildlife-Aircraft Collisions at US Airports"                               #
########################################################################################################################



## CHANGE **LINE 44** TO THE USER-SPECIFIC PATH OF THE "covid_and_wildlife_strikes" FOLDER. ##               



# TABLE OF CONTENTS:
#   1. SET WORKING DIRECTORY, LOAD REQUIRED PACKAGES, AND LOAD DATA................................................40-71
#   2. FIGURE 1. Spatial distribution of sample airports and state-level effects of the COVID-19 pandemic on air 
#                traffic volume across the contiguous United States...............................................75-120
#   3. FIGURE 2. Month-specific effects of the pandemic among sample airports....................................124-198
#   4. FIGURE 3. Bivariate relationship between reductions in air traffic and changes in wildlife strikes........202-310
#   5. FIGURE 4. Month-specific model estimated changes in the (a) overall and (b) disruptive wildlife strike rate 
#                during the COVID-19 months of 2020. (ALSO INCLUDES TABLE S2, TABLE S3, & TABLE S4) .............314-456
#   6. CONSTRUCTING COUNTERFACTUAL AIR TRAFFIC VOLUME ACROSS AIRPORTS FOR MARCH 2020-DECEMBER 2020...............460-472
#   7. FIGURE S1. Observed (solid line) and model predicted (dashed line) aircraft movements at three sample airports 
#                 with heterogeneous experiences over the COVID-19 months of 2020................................476-528
#   8. FIGURE S2. Estimated percent change in aircraft movements (relative to model expectation) across sample airports 
#                 during the COVID-19 months of 2020.............................................................532-574
#   9. FIGURE 5. Month-specific model estimated relationship between reductions in air traffic volume and the 
#                (a) overall and (b) disruptive wildlife strike rate during the COVID-19 months of 2020. (ALSO INCLUDES 
#                TABLE S5, TABLE S6, & TABLE S7) ................................................................578-745
#   10. FIGURE S3. Month-specific model estimated changes in size-specific wildlife strike rates during the COVID-19 
#                  months of 2020................................................................................749-832
#   11. FIGURE S4. Estimating changes in the (a) overall and (b) disruptive wildlife strike rate during the COVID-19 
#                  months of 2020 under alternative “treatment” assumptions.....................................836-1012
#   12. FIGURE S5. Estimating the relationship between aircraft movement reductions and changes in the (a) overall and 
#                  (b) disruptive wildlife strike rate during the COVID-19 months of 2020 under alternative “treatment”
#                  assumptions.................................................................................1016-1195
# END



########################################################################################################################
# SET WORKING DIRECTORY, LOAD REQUIRED PACKAGES, AND LOAD DATA.

# Set working directory:
setwd("/Users/laltrin/Desktop/covid_and_wildlife_strikes")

# Install (if necessary) and Load required packages:
  # List of required packages:
  required_packages <- c("doBy", "dplyr", "lubridate", "zoo", # cleaning packages
                         "ggplot2", "ggthemes", "ggpubr", "ggrepel", "maps", "gridExtra", "scales", # plotting packages
                         "fixest", "msm") # modeling packages
  # Install packages that aren't already installed:
  installed_packages <- required_packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(required_packages[!installed_packages])
  }
  rm(installed_packages)
  # Load packages:
  invisible(lapply(required_packages, library, character.only = TRUE))
  # Remove list of required packages:
  rm(required_packages)

# Bringing in data (this data is pre-cleaned and was drawn from the National Wildlife Strike Database (NWSD) and the 
# Federal Aviation Administration's Operations Network Database (OPSNET) or Air Traffic Activity Data System (ATADS)).
df <- read.csv('data/sample_strikes_and_air_traffic__2014_to_2020.csv')

# As an initial step, we remove military operations (movements) from our measure of total operations (movements) since
# all wildlife strike data is drawn from the NWSD, which is inclusive of civil wildlife strike reports and not military 
# wildlife strike reports.
df$total_civil_movements <- df$total_movements - (df$military_itinerant_movements + df$military_local_movements)

########################################################################################################################



########################################################################################################################
# FIGURE 1. Spatial distribution of sample airports and state-level effects of the COVID-19 pandemic on air traffic 
# volume across the contiguous United States. Percent change in aircraft movements (total number of takeoffs and 
# landings) is measured between the pre-pandemic period March 2019–December 2019 and the pandemic period March 2020–
# December 2020. All states experienced declines in air traffic volume beginning in March 2020. The largest changes in 
# air traffic volume were observed in the densely populated District of Columbia (-66.7%), New Jersey (-48.4%), and 
# New York (-48.0%) while the smallest changes were observed in some of the least densely populated states such as 
# Montana (-7.4%), Wyoming (-8.7%), and South Dakota (-10.1%).

# State polygon data:
map_df <- map_data("state")

# Airport code and location data:
airport_df <- df  %>% group_by(airport) %>% slice(n=1) %>% dplyr::select(airport, latitude, longitude)

# State-level changes in air traffic volume:
state_movements_df <- read.csv('data/state_level_air_traffic__2019_and_2020.csv')
state_movements_df$region <- tolower(state.name[match(state_movements_df$state,state.abb)])
state_movements_df$region <- ifelse(state_movements_df$state=="DC", "district of columbia", state_movements_df$region)
state_movements_df <- state_movements_df[,c("region", "mar_to_dec_perc_change")]
state_movements_df$cov_change <- ifelse(state_movements_df$region=="district of columbia", NA, state_movements_df$mar_to_dec_perc_change)

# Merge traffic volume data to state polygon data:
map_df <- merge(map_df, state_movements_df, by="region", all.x = T)

# Generate plot:
png("figure_1.png", width = 5, height = 3.75, units = "in", res = 600)
ggplot() + 
  geom_polygon(data=map_df, aes(x=long, y=lat, group=group, fill = cov_change), color="black", size=0.3) +
  geom_text(data = airport_df, aes(x=longitude, y=latitude), size=5, 
            label = "\u2708", family = "Arial Unicode MS", angle=45) +
  geom_text_repel(data = airport_df, aes(x=longitude, y=latitude, label=airport), size=2.5) +
  scale_fill_distiller(name = expression(Delta*" aircraft movements (%)"), palette = "Reds", 
                       direction = -1, limits = c(-52, -5), breaks = seq(-50,0,10)) +
  theme_void() +
  theme(legend.text = element_text(size = 10), legend.position = "bottom", legend.title.align = 0.5) +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 0.7, title.position = "top",
                                label.position = "bottom", title.theme = element_text(angle = 0, size = 10),
                                frame.colour = "black", ticks.colour = "black",
                                frame.linewidth = 1)) 
dev.off()

# Remove plotting dataframes:
rm(map_df, state_movements_df, airport_df)

########################################################################################################################



########################################################################################################################
# FIGURE 2. Month-specific effects of the pandemic among sample airports. Data is aggregated across all sample airports 
# in this analysis. Percent changes in (a) aircraft movements (total number of takeoffs and landings), (b) wildlife 
# strikes (number of wildlife strikes reported to the NWSD), and (c) the wildlife strike rate (number of reported 
# wildlife strikes per 100,000 movements) are measured between 2019 and 2020 for all months January–December.

# Sum data of interest across sample airports for years 2019 and 2020:
plot_df <- summaryBy(strike + total_civil_movements ~ month_year, 
                     data = subset(df, year>=2019), keep.names = T, FUN = sum, id=~ year + month)

# Generate month factor variable and strike rate variable:
plot_df$month <- factor(format(as.yearmon(plot_df$month_year, "%b %Y"), "%b"), levels = month.abb)
plot_df$strike_rate_per_100k <- ((plot_df$strike)/plot_df$total_civil_movements)*100000

# Generate separate 2019 and 2020 dataframes and merge them:
plot_df_2019 <- subset(plot_df, year==2019)[,c("month", "strike", "total_civil_movements", "strike_rate_per_100k")]
colnames(plot_df_2019) <- c("month", "strike_2019", "total_civil_movements_2019", "strike_rate_per_100k_2019")
plot_df_2020 <- subset(plot_df, year==2020)[,c("month", "strike", "total_civil_movements", "strike_rate_per_100k")]
colnames(plot_df_2020) <- c("month", "strike_2020", "total_civil_movements_2020", "strike_rate_per_100k_2020")
plot_df <- merge(plot_df_2019, plot_df_2020, by="month")
rm(plot_df_2019, plot_df_2020)

# Calculate changes in strike counts, movements, and strike rates:
plot_df$strike_change <- ((plot_df$strike_2020-plot_df$strike_2019)/plot_df$strike_2019)*100
plot_df$movements_change <- ((plot_df$total_civil_movements_2020-plot_df$total_civil_movements_2019)/plot_df$total_civil_movements_2019)*100
plot_df$strike_rate_change <- ((plot_df$strike_rate_per_100k_2020-plot_df$strike_rate_per_100k_2019)/plot_df$strike_rate_per_100k_2019)*100

# Generate air traffic volume plot:
p1 <- ggplot() +
  geom_col(data = plot_df, aes(x=month, y=movements_change), fill = "#FB6A4A", color = "black", size = 0.5) + 
  geom_hline(yintercept = 0, color="black", size = 0.3) +
  theme_clean() +
  ylab("\u0394 aircraft movements (%)\n") + xlab("") +
  scale_y_continuous(limits = c(-75, 20), breaks = seq(-60,20,20)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.65, hjust=0.5, size = 8),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size=9),
        plot.background = element_rect(color = "white")) +
  ggtitle("(a) Aircraft movements") 

# Generate strike count plot:
p2 <- ggplot() +
  geom_col(data = plot_df, aes(x=month, y=strike_change), fill = "#FB6A4A", color = "black", size = 0.5) + 
  geom_hline(yintercept = 0, color="black", size = 0.3) +
  theme_clean() +
  ylab("\u0394 wildlife strikes (%)\n") + xlab("") +
  scale_y_continuous(limits = c(-65, 5), breaks = seq(-60,0,15)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.65, hjust=0.5, size = 8),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size=9),
        plot.background = element_rect(color = "white")) +
  ggtitle("(b) Wildlife strikes") 

# Generate strike rate plot:
p3 <- ggplot() +
  geom_col(data = plot_df, aes(x=month, y=strike_rate_change), fill = "#FB6A4A", color = "black", size = 0.5) + 
  geom_hline(yintercept = 0, color="black", size = 0.3) +
  theme_clean() +
  ylab("\u0394 wildlife strike rate (%)\n") + xlab("") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.65, hjust=0.5, size = 8),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size=9),
        plot.background = element_rect(color = "white")) +
  ggtitle("(c) Wildlife strike rate")

# Combine plots for figure:
png("figure_2.png", width = 3.75, height = 5.75, units = "in", res = 600)
grid.arrange(p1, p2, p3, ncol=1, nrow =3,
             bottom = text_grob("Month", size = 10))
dev.off()

# Remove plot data:
rm(plot_df, p1, p2, p3)

########################################################################################################################



########################################################################################################################
# FIGURE 3: Bivariate relationship between reductions in air traffic volume and changes in wildlife strikes. Each point 
# represents a sample airport, with the size of each point indicating operational size in 2019 as measured by the 
# average monthly number of takeoffs and landings. The percent reduction in aircraft movements (number of takeoffs and 
# landings) at each airport is measured between periods March 2019–December 2019 and March 2020–December 2020. Percent 
# changes () in (a) wildlife strikes (number of wildlife strikes reported to the NWSD) and (b) the wildlife strike 
# rate (number of reported wildlife strikes per 100,000 movements) at each airport are similarly measured between 
# March 2019–December 2019 and March 2020–December 2020. Response curves are bivariate linear regression fits of the 
# data, where the shaded area surrounding each curve is the associated 95% interval of confidence. A Pearson correlation 
# test indicates that reductions in aircraft movements are significantly negatively correlated with changes in the 
# absolute number of wildlife strikes (r = -0.420, p = 0.002). A similar test shows that reductions in aircraft 
# movements are significantly positively correlated with changes in wildlife strike rates (r = 0.380, p = .006).

# Sum data of interest by airport for periods March-December 2019 and March-December 2020:
plot_df <- summaryBy(strike + total_civil_movements ~ airport + year, 
                     data = subset(df, year>=2019 & month>=3), keep.names = T, FUN = sum)

# Generate month factor and strike rate variable:
plot_df$strike_rate_per_100k <- ((plot_df$strike)/plot_df$total_civil_movements)*100000

# Generate separate 2019 and 2020 dataframes and merge them:
plot_df_2019 <- subset(plot_df, year==2019)[,c("airport", "strike", "total_civil_movements", "strike_rate_per_100k")]
colnames(plot_df_2019) <- c("airport", "strike_2019", "total_civil_movements_2019", "strike_rate_per_100k_2019")
plot_df_2020 <- subset(plot_df, year==2020)[,c("airport", "strike", "total_civil_movements", "strike_rate_per_100k")]
colnames(plot_df_2020) <- c("airport", "strike_2020", "total_civil_movements_2020", "strike_rate_per_100k_2020")
plot_df <- merge(plot_df_2019, plot_df_2020, by=c("airport"))
rm(plot_df_2019, plot_df_2020)

# Calculate changes in strike counts, movements, and stike rates:
plot_df$strike_change <- ((plot_df$strike_2020-plot_df$strike_2019)/plot_df$strike_2019)*100
plot_df$movements_change <- -1*(((plot_df$total_civil_movements_2020-plot_df$total_civil_movements_2019)/plot_df$total_civil_movements_2019)*100)
plot_df$strike_rate_change <- ((plot_df$strike_rate_per_100k_2020-plot_df$strike_rate_per_100k_2019)/plot_df$strike_rate_per_100k_2019)*100

# Quick normality check before correlation test:
ggqqplot(plot_df$movements_change)
ggqqplot(plot_df$strike_change)
ggqqplot(plot_df$strike_rate_change)

# Correlations between change in movements and change in strikes/strike rate:
cor.test(plot_df$movements_change, plot_df$strike_change, method = "pearson")
cor.test(plot_df$movements_change, plot_df$strike_rate_change, method = "pearson")

# Generate change in movements vs. change in strikes plot:
p1 <- ggplot() +
  geom_smooth(data=plot_df, aes(x=movements_change, y=strike_change), se = T, method = "lm", 
              size = 0.3, color = "black") + 
  geom_point(data=plot_df, aes(x=movements_change, y=strike_change, size=total_civil_movements_2019/10), 
             shape = 21, fill = "#FB6A4A") +
  scale_size_continuous(breaks = seq(10000, 70000, 20000), labels = comma) +
  theme_clean() +
  guides(fill = "none") + 
  xlab("\nReduction in aircraft movements (%)") + ylab("\u0394 wildlife strikes (%)\n") + 
  scale_y_continuous(breaks = scales::extended_breaks(n=6)) +
  scale_x_continuous(breaks = scales::extended_breaks(n=7)) +
  guides(size = guide_legend("Average monthly aircraft movements (2019)", title.position = "top", 
                             label.position = "bottom", title.theme = element_text(size = 10, hjust = 0.5), 
                             keywidth = 4, keyheight = 1)) +
  theme(legend.position = "none", plot.background = element_rect(color = "white"), 
        legend.background = element_rect(color = NA),
        axis.text = element_text(size = 9), axis.title = element_text(size = 9),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), plot.title = element_text(size = 9, hjust = 0.5)) +
  ggtitle("(a) Wildlife strikes")

# Generate change in movements vs. change in strike rates plot:
p2 <- ggplot() +
  geom_smooth(data=plot_df, aes(x=movements_change, y=strike_rate_change), se = T, method = "lm", size = 0.3, color = "black") + 
  geom_point(data=plot_df, aes(x=movements_change, y=strike_rate_change, size=total_civil_movements_2019/10), shape = 21, fill = "#FB6A4A") +
  scale_size_continuous(breaks = seq(10000, 70000, 20000), labels = comma) +
  theme_clean() +
  xlab("\nReduction in aircraft movements (%)") + ylab("\u0394 wildlife strike rate (%)\n") + 
  scale_y_continuous(breaks = seq(-60,90,30)) +
  scale_x_continuous(breaks = scales::extended_breaks(n=7)) +
  guides(size = guide_legend("Average monthly aircraft movements (2019)", title.position = "top", label.position = "bottom", 
                             title.theme = element_text(size = 10, hjust = 0.5), keywidth = 4, keyheight = 1)) +
  theme(legend.position = "none", plot.background = element_rect(color = "white"), 
        legend.background = element_rect(color = NA),
        axis.text = element_text(size = 9), axis.title = element_text(size = 9),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), plot.title = element_text(size = 9, hjust = 0.5)) +
  ggtitle("(b) Wildlife strike rate") 

# Get legend:
leg <- get_legend(
  ggplot() +
    geom_smooth(data=plot_df, aes(x=movements_change, y=strike_rate_change), se = T, method = "lm", size = 0.3, color = "black") + 
    geom_point(data=plot_df, aes(x=movements_change, y=strike_rate_change, size=total_civil_movements_2019/10), shape = 21, fill = "#FB6A4A") +
    scale_size_continuous(breaks = seq(10000, 70000, 20000), labels = comma) +
    theme_clean() +
    xlab("\nReduction in aircraft movements (%)") + ylab("\u0394 wildlife strike rate (%)\n") + 
    scale_y_continuous(breaks = scales::extended_breaks(n=7)) +
    scale_x_continuous(breaks = scales::extended_breaks(n=7)) +
    guides(size = guide_legend("Monthly aircraft movements", title.position = "top", label.position = "bottom", 
                               title.theme = element_text(size = 8, hjust = 0.5), keywidth = 1.5, keyheight = 0.4)) +
    theme(plot.background = element_rect(color = "white"), legend.direction = "horizontal",
          legend.background = element_rect(color = NA), legend.text = element_text(size = 7),
          axis.text = element_text(size = 8), axis.title = element_text(size = 8),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), 
          plot.title = element_text(size = 9, hjust = 0.5)) +
    ggtitle("(b) Wildlife strike rate") 
)

# Combine plots and legend:
png("figure_3.png", width = 3.75, height = 5.75, units = "in", res = 600)
grid.arrange(arrangeGrob(p1), arrangeGrob(p2), arrangeGrob(leg), heights = c(2,2,0.75))
dev.off()

# Remove plot data:
rm(plot_df, p1, p2, leg)

########################################################################################################################



########################################################################################################################
# FIGURE 4. Month-specific model estimated changes in the (a) overall and (b) disruptive wildlife strike rate during the 
# COVID-19 months of 2020. The connected points in panels (a) and (b) report the model estimated percent change in the 
# wildlife strike rate over the COVID months of 2020, all else equal. The error bars provide the associated 90% and 95% 
# intervals of confidence that surround each estimate. These estimates are derived from the incident rate ratios (IRRs) 
# generated via fixed-effect negative binomial regression models (Tables S3 & S4). Specifically, they are calculated 
# [IRR-1] × 100 to supply a direct interpretation.

## This code chunk also supplies TABLE S2, TABLE S3, & TABLE S4. ##

# Negative binomial regression models for overall strike rate:
model1 <- fenegbin(strike ~ as.factor(covid_month) + offset(log(total_civil_movements)), df)
model2 <- fenegbin(strike ~ as.factor(covid_month) + offset(log(total_civil_movements)) | month, df)
model3 <- fenegbin(strike ~ as.factor(covid_month) + offset(log(total_civil_movements)) | airport + month, df)
model4 <- fenegbin(strike ~ as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month, df)
model5 <- fenegbin(strike ~ as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month^usfws_region, df)
model6 <- fenegbin(strike ~ as.factor(covid_month) + offset(log(total_civil_movements)) + airport:year | airport + month^usfws_region, df)

# Data frame that contains model details, BIC, dBIC, and LL:
results_df <- data.frame(model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
                         model_formula = c("strike ~ as.factor(covid_month) + offset(log(total_civil_movements))",
                                           "strike ~ as.factor(covid_month) + offset(log(total_civil_movements)) | month",
                                           "strike ~ as.factor(covid_month) + offset(log(total_civil_movements)) | airport + month",
                                           "strike ~ as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month",
                                           "strike ~ as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month^usfws_region",
                                           "strike ~ as.factor(covid_month) + offset(log(total_civil_movements)) + airport:year | airport + month^usfws_region"),
                         k = c(model1$nparams, model2$nparams, model3$nparams, model4$nparams, model5$nparams, model6$nparams),
                         bic = c(BIC(model1), BIC(model2), BIC(model3), BIC(model4), BIC(model5), BIC(model6)),
                         delta_bic = c(NA, NA, NA, NA, NA, NA),
                         ll = c(logLik(model1), logLik(model2), logLik(model3), logLik(model4), logLik(model5), logLik(model6)))
results_df <- results_df %>% arrange(bic) %>% mutate(delta_bic = bic - min(bic))

# Show data frame (TABLE S2a):
results_df
rm(model1, model2, model3, model4, model5, model6, results_df)

# Full model output from the best performing model (TABLE S3):
model <- fenegbin(strike ~ as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month^usfws_region, df, vcov = "cluster")
summary(model)

# Collect the coefficients of interest and their SEs from the best performing model:
plot_df <- data.frame(irr = exp(model$coeftable[1:10,1]),
                         se = deltamethod(list(~exp(x1), ~exp(x2), ~exp(x3), ~exp(x4), ~exp(x5), ~exp(x6), ~exp(x7), ~exp(x8), ~exp(x9), ~exp(x10)), coef(model), vcov(model)[-c(12), -c(12)]))
plot_df <- plot_df %>% 
  mutate(month = factor(month.abb[3:12], levels = month.abb[3:12])) %>%
  mutate(var = "(a) All strikes") %>%
  mutate(est = (irr-1)*100) %>%
  mutate(lower_90 = ((irr - se*1.645)-1)*100) %>%
  mutate(upper_90 = ((irr + se*1.645)-1)*100) %>%
  mutate(lower_95 = ((irr - se*1.96)-1)*100) %>%
  mutate(upper_95 = ((irr + se*1.96)-1)*100)

# Calculate IRRs and CIs that are presented in TABLE S3:
temp <- plot_df %>% 
  mutate(irr = round(irr, digits = 3)) %>%
  mutate(ci = paste0("[",round((lower_90/100)+1, digits = 3),", ",round((upper_90/100)+1, digits = 3),"]")) %>%
  select(irr, ci)
est <- exp(model$coeftable[11,1])
se <- deltamethod(list(~exp(x1)), coef(model)[11], vcov(model)[c(11), c(11)])
temp[11,1] <- exp(model$coeftable[11,1])
temp[11,2] <- paste0("[",round(est-1.645*se, digits = 3),", ",round(est+1.645*se, digits = 3),"]")
rownames(temp)[11] <- "year"
temp
rm(model, est, se, temp)

# Negative binomial regression models for disruptive strike rate:
model1 <- fenegbin(disruptive ~ as.factor(covid_month) + offset(log(total_civil_movements)), df)
model2 <- fenegbin(disruptive ~ as.factor(covid_month) + offset(log(total_civil_movements)) | month, df)
model3 <- fenegbin(disruptive ~ as.factor(covid_month) + offset(log(total_civil_movements)) | airport + month, df)
model4 <- fenegbin(disruptive ~ as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month, df)
model5 <- fenegbin(disruptive ~ as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month^usfws_region, df)
model6 <- fenegbin(disruptive ~ as.factor(covid_month) + offset(log(total_civil_movements)) + airport:year | airport + month^usfws_region, df)

# Data frame that contains model details, BIC, dBIC, and LL:
results_df <- data.frame(model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
                         model_formula = c("disruptive ~ as.factor(covid_month) + offset(log(total_civil_movements))",
                                           "disruptive ~ as.factor(covid_month) + offset(log(total_civil_movements)) | month",
                                           "disruptive ~ as.factor(covid_month) + offset(log(total_civil_movements)) | airport + month",
                                           "disruptive ~ as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month",
                                           "disruptive ~ as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month^usfws_region",
                                           "disruptive ~ as.factor(covid_month) + offset(log(total_civil_movements)) + airport:year | airport + month^usfws_region"),
                         k = c(model1$nparams, model2$nparams, model3$nparams, model4$nparams, model5$nparams, model6$nparams),
                         bic = c(BIC(model1), BIC(model2), BIC(model3), BIC(model4), BIC(model5), BIC(model6)),
                         delta_bic = c(NA, NA, NA, NA, NA, NA),
                         ll = c(logLik(model1), logLik(model2), logLik(model3), logLik(model4), logLik(model5), logLik(model6)))
results_df <- results_df %>% arrange(bic) %>% mutate(delta_bic = bic - min(bic))

# Show data frame (TABLE S2b):
results_df
rm(model1, model2, model3, model4, model5, model6, results_df)

# Full model output from the best performing model (TABLE S4):
model <- fenegbin(disruptive ~ as.factor(covid_month) + offset(log(total_civil_movements)) | airport + month, df, vcov = "cluster")
summary(model)

# Collect the coefficients of interest and their SEs from the best performing model:
temp_df <- data.frame(irr = exp(model$coeftable[1:10,1]),
                      se = deltamethod(list(~exp(x1), ~exp(x2), ~exp(x3), ~exp(x4), ~exp(x5), ~exp(x6), ~exp(x7), ~exp(x8), ~exp(x9), ~exp(x10)), coef(model), vcov(model)[-c(11), -c(11)]))
temp_df <- temp_df %>% 
  mutate(month = factor(month.abb[3:12], levels = month.abb[3:12])) %>%
  mutate(var = "(b) Disruptive strikes") %>%
  mutate(est = (irr-1)*100) %>%
  mutate(lower_90 = ((irr - se*1.645)-1)*100) %>%
  mutate(upper_90 = ((irr + se*1.645)-1)*100) %>%
  mutate(lower_95 = ((irr - se*1.96)-1)*100) %>%
  mutate(upper_95 = ((irr + se*1.96)-1)*100)

# Calculate IRRs and CIs that are presented in TABLE S4:
temp <- as.data.frame(model$coeftable)
temp <- temp_df %>% 
  mutate(irr = round(irr, digits = 3)) %>%
  mutate(ci = paste0("[",round((lower_90/100)+1, digits = 3),", ",round((upper_90/100)+1, digits = 3),"]")) %>%
  select(irr, ci)
temp; rm(temp)

# Combine the results for the "All strikes" and "Disruptive strikes" model results for plotting:
plot_df <- rbind(plot_df, temp_df); rm(temp_df, model)

# Generate plot of estimates:
png("figure_4.png", width = 3.75, height = 5.75, units = "in", res = 600)
ggplot() +
  geom_hline(yintercept = 0) +
  geom_linerange(data = plot_df, aes(x = month, ymin = lower_95, ymax = upper_95, color = "95% C.I."), size = 4.5, key_glyph = "path") +
  geom_linerange(data = plot_df, aes(x = month, ymin = lower_90, ymax = upper_90, color = "90% C.I."), size = 4.5, key_glyph = "path") +
  geom_line(data = plot_df, aes(x = month, y = est, group = 1), size =0.8, color = "grey35") +
  geom_point(data = plot_df, aes(x = month, y = est), shape=21, size =3.5, color = "grey35", fill = "white", stroke=1) +
  theme_clean() +
  ylab("Estimated percent \u0394 in wildlife strike\nrate relative to non-COVID conditions\n") + 
  xlab("\nMonth of 2020") +
  scale_color_manual(name = "", values = c("#FB6A4A", "#F4AC9D"), breaks=c("90% C.I.", "95% C.I.")) +
  theme(legend.position = c(0.79,0.955), 
        legend.key.height = unit(0.35, 'cm'), 
        legend.background = element_blank(), legend.text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, vjust = 0.65), 
        strip.text = element_text(face = "bold"),
        plot.background = element_rect(color = "white")) +
  facet_wrap(~ var, nrow = 2, scales = "free", dir = "h") 
dev.off()

# Remove plot dataframe:
rm(plot_df)

########################################################################################################################



########################################################################################################################
# CONSTRUCTING COUNTERFACTUAL AIR TRAFFIC VOLUME ACROSS AIRPORTS FOR MARCH 2020-DECEMBER 2020. Here we employ a simple, 
# but effective, linear regression model to generate counterfactual aircraft movements for the COVID months of 2020.

# Get counterfactual air traffic volume:
df$month_year <- as.yearmon(df$month_year)
model_df <- subset(df, month_year<=as.yearmon("Feb 2020"))
model <- lm(total_civil_movements ~ as.factor(airport) + as.factor(month):as.factor(airport) + year:as.factor(airport), data = model_df)
summary(model)
df$predicted_movements <- predict(model, df)
rm(model_df, model)

########################################################################################################################



########################################################################################################################
# Figure S1. Observed (solid line) and model predicted (dashed line) aircraft movements at three sample airports with 
# heterogeneous experiences over the COVID-19 months of 2020— (a) LaGuardia Airport (LGA), (b) Memphis International 
# Airport (MEM), (c) Southwest Florida International Airport (RSW). Model predicted aircraft movements are obtained via 
# a simple linear regression model, where aircraft movements (AM) at airport i and time (month-year) t are specified to 
# be a linear function of airport fixed-effects, airport-specific month effects, and airport-specific linear time 
# trends.This model is estimated on data from the pre-COVID-19 period—i.e., January 2014 to February 2020—and used to 
# predict monthly aircraft operations for each airport over the entire sample period, including March 2020 to December 
# 2020. The predictive performance of this simple model on pre-COVID-19 data is strong (R2 = 0.997), providing a 
# reliable counterfactual for aircraft movements at each airport had the COVID-19 pandemic not occurred. All sample 
# airports experienced decreases in aircraft movements (relative to model expectation) during the COVID-19 months of 
# 2020 (see Fig. S2). As the figure above indicates, however, some experienced larger, long-lasting reductions in air
# traffic volume while others experienced more mild reductions and/or recovered more quickly.

# Get data for three example airports:
plot_df <- subset(df, airport=="LGA" | airport=="RSW" | airport=="MEM")
plot_df$airport <- factor(plot_df$airport, levels = c("LGA", "MEM", "RSW"))
plot_df$predicted_change <- ((plot_df$total_civil_movements-plot_df$predicted_movements)/plot_df$predicted_movements)*100


# Generate plot:
png("figure_S1.png", width = 4.5, height = 5.75, units = "in", res = 600)
ggplot() +
  geom_segment(data = subset(plot_df, month_year>=as.yearmon("Feb 2020", "%b %Y")), 
               aes(x = month_year, xend = month_year, y = total_civil_movements, yend = predicted_movements,  group = month_year, color = predicted_change), 
               alpha = 0.9, size = 2) +
  geom_line(data = plot_df, aes(x = month_year, y = total_civil_movements, group = 1), size = 0.4) +
  geom_line(data = plot_df, aes(x = month_year, y = predicted_movements,  group = 1), size = 0.4, linetype="dashed") +
  theme_clean() + scale_y_continuous(label = comma) + 
  ylab("Aircraft movements\n") + xlab("\nMonth-year") +
  facet_wrap(~ airport, nrow = 3, scales = "free", 
             labeller = labeller(airport = c("LGA" = "(a) LGA",
                                                  "MEM" = "(b) MEM",
                                                  "RSW" = "(c) RSW"))) + 
  theme(strip.text = element_text(hjust = 0)) + 
  theme(panel.spacing = unit(2, "lines")) +
  scale_color_distiller(name = expression(Delta*" aircraft movements (%)"),
                        palette = "Reds", direction = -1, limits = c(-100, 10), breaks = seq(-90, 0,15)) +
  theme(legend.text = element_text(size = 8), text = element_text(size = 8), legend.position = "bottom",
        axis.title = element_text(size = 10), axis.text = element_text(size = 8)) +
  guides(color = guide_colourbar(barwidth = 10, barheight = 0.7, title.position = "top",
                                 label.position = "bottom", title.theme = element_text(angle = 0, size = 8),
                                 frame.colour = "black", ticks.colour = "black",
                                 frame.linewidth = 1)) +
  theme(legend.title.align = 0.5) + 
  theme(strip.text = element_text(hjust = 0.5, face = "bold", size = 9),
        legend.background = element_rect(color = NA), plot.background = element_rect(color = "white"))
dev.off()

# remove plot data:
rm(plot_df)

########################################################################################################################



########################################################################################################################
# Figure S2. Estimated percent change in aircraft movements (relative to model expectation) across sample airports 
# during the COVID-19 months of 2020. The points, which are jittered for readability, are calculated as airport-by-month 
# differences between predicted and observed aircraft movements—-i.e., the dashed and solid lines shown in Figure S1. 
# The overlaying boxplots present the 25th percentile (bottom of each box), median (bold line within each box), and the 
# 75th percentile (top of each box) of changes in aircraft movements during each month of the COVID-19 pandemic in 2020. 
# It is the measure plotted in this figure, multiplied by -1, that constitutes our continuous measure of “treatment”, 
# the estimated percent reduction (EPR) in aircraft movements, in our fixed-effect negative binomial regression models 
# (see Tables S5-S7).

# Get plot data:
plot_df <- subset(df, month_year>=as.yearmon("Mar 2020", "%b %Y") & month_year<=as.yearmon("Dec 2020", "%b %Y"))
plot_df$month <- factor(format(plot_df$month_year, "%b"), levels = month.abb)
plot_df$predicted_change <- ((plot_df$total_civil_movements-plot_df$predicted_movements)/plot_df$predicted_movements)*100

# plot:
png("figure_s2.png", width = 4.5, height = 4.25, units = "in", res = 600)
ggplot() +
  geom_jitter(data = plot_df, aes(x = as.factor(month), y = predicted_change, fill = predicted_change), 
              shape = 21,size = 1.5,colour = "black", width = 0.1, stroke = 0.3) +
  geom_boxplot(data = plot_df, aes(x = as.factor(month), y = predicted_change), outlier.shape = NA, alpha = 0.001, width = 0.5) +
  theme_clean() +
  ylab("Percent \u0394 in aircraft movements\n(relative to model expectation)\n") + 
  xlab("\nMonth of 2020") +
  scale_fill_distiller(name = expression("Estimated "*Delta*" in aircraft movements (%)"),
                       palette = "Reds", direction = -1, breaks = seq(-90, 0, 15)) +
  theme(legend.text = element_text(size = 9), text = element_text(size = 10), legend.position = "bottom") +
  guides(fill = guide_colourbar(barwidth = 12, barheight = 0.7, title.position = "top",
                                label.position = "bottom", title.theme = element_text(angle = 0, size = 9),
                                frame.colour = "black", ticks.colour = "black",
                                frame.linewidth = 1)) +
  theme(legend.title.align = 0.5) +
  scale_y_continuous(breaks = seq(-90,0,15)) + 
  theme(text = element_text(size = 10)) +
  theme( axis.text.x = element_text(angle = 45, size = 9, hjust = 0.5, vjust = 0.5)) +
  theme( axis.text.y = element_text(size = 9), legend.background = element_rect(color = NA), 
         plot.background = element_rect(color = "white")) 
dev.off()

# remove plot data:
rm(plot_df)

########################################################################################################################



########################################################################################################################
# FIGURE 5. Month-specific model estimated relationship between reductions in air traffic volume and the (a) overall 
# and (b) disruptive wildlife strike rate during the COVID-19 months of 2020. The connected points in panels (a) and 
# (b) report the model estimated percent change in the wildlife strike rate that is associated with a 1% reduction in 
# aircraft movements, all else equal. The error bars provide the associated 90% and 95% intervals of confidence that 
# surround each estimate. These estimates are derived from the incident rate ratios (IRRs) generated via fixed-effect 
# negative binomial regression models (Tables S6 & S7). Specifically, they are calculated: [IRR-1] × 100.

## This code chunk also supplies TABLE S5, TABLE S6, & TABLE S7. ##

# Generate "treatment" measure:
df$epr <- -1*(((df$total_civil_movements-df$predicted_movements)/df$predicted_movements)*100)
df$epr2 <- df$epr^2

# Negative binomial regression models for overall strike rate:
model1 <- fenegbin(strike ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)), df)
model2 <- fenegbin(strike ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) | month, df)
model3 <- fenegbin(strike ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) | airport + month, df)
model4 <- fenegbin(strike ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month, df)
model5 <- fenegbin(strike ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month^usfws_region, df)
model6 <- fenegbin(strike ~ epr:as.factor(covid_month) + epr2:as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month^usfws_region, df)
model7 <- fenegbin(strike ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) + airport:year | airport + month^usfws_region, df)
model8 <- fenegbin(strike ~ epr:as.factor(covid_month) + epr2:as.factor(covid_month) + offset(log(total_civil_movements)) + airport:year | airport + month^usfws_region, df)

# Data frame that contains model details, BIC, dBIC, and LL:
results_df <- data.frame(model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8"),
                         model_formula = c("strike ~ epr:as.factor(covid_month) + offset(log(total_civil_movements))",
                                           "strike ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) | month",
                                           "strike ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) | airport + month",
                                           "strike ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month",
                                           "strike ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month^usfws_region",
                                           "strike ~ epr:as.factor(covid_month) +  epr2:as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month^usfws_region",
                                           "strike ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) + airport:year | airport + month^usfws_region",
                                           "strike ~ epr:as.factor(covid_month) +  epr2:as.factor(covid_month) + offset(log(total_civil_movements)) + airport:year | airport + month^usfws_region"),
                         k = c(model1$nparams, model2$nparams, model3$nparams, model4$nparams, model5$nparams, model6$nparams, model7$nparams, model8$nparams),
                         bic = c(BIC(model1), BIC(model2), BIC(model3), BIC(model4), BIC(model5), BIC(model6), BIC(model7), BIC(model8)),
                         delta_bic = c(NA, NA, NA, NA, NA, NA, NA, NA),
                         ll = c(logLik(model1), logLik(model2), logLik(model3), logLik(model4), logLik(model5), logLik(model6), logLik(model7), logLik(model8)))
results_df <- results_df %>% arrange(bic) %>% mutate(delta_bic = bic - min(bic))

# Show data frame (TABLE S5a):
results_df
rm(model1, model2, model3, model4, model5, model6, model7, model8, results_df)

# Full model output from the best performing model (TABLE S6):
model <- fenegbin(strike ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month^usfws_region, df, vcov = "cluster")
summary(model)

# Collect the coefficients of interest from the best performing model:
plot_df <- data.frame(irr = exp(model$coeftable[3:12,1]),
                      se = deltamethod(list(~exp(x1), ~exp(x2), ~exp(x3), ~exp(x4), ~exp(x5), ~exp(x6), ~exp(x7), ~exp(x8), ~exp(x9), ~exp(x10)), 
                                       coef(model)[3:12], vcov(model)[-c(1:2,13), -c(1:2,13)]))
plot_df <- plot_df %>% 
  mutate(month = factor(month.abb[3:12], levels = month.abb[3:12])) %>%
  mutate(var = "(a) All strikes") %>%
  mutate(est = (irr-1)*100) %>%
  mutate(lower_90 = ((irr - se*1.645)-1)*100) %>%
  mutate(upper_90 = ((irr + se*1.645)-1)*100) %>%
  mutate(lower_95 = ((irr - se*1.96)-1)*100) %>%
  mutate(upper_95 = ((irr + se*1.96)-1)*100)

# Calculate IRRs and CIs that are presented in TABLE S6:
temp <- as.data.frame(model$coeftable)
temp <- plot_df %>% 
  mutate(irr = round(irr, digits = 4)) %>%
  mutate(ci = paste0("[",round((lower_90/100)+1, digits = 4),", ",round((upper_90/100)+1, digits = 4),"]")) %>%
  select(irr, ci)
est <- exp(model$coeftable[1:2,1])
se <- deltamethod(list(~exp(x1), ~exp(x2)), coef(model)[1:2], vcov(model)[c(1,2), c(1,2)])
temp[11,1] <- est[1]
temp[11,2] <- paste0("[",round(est[1]-1.645*se[1], digits = 4),", ",round(est[1]+1.645*se[1], digits = 4),"]") 
temp[12,1] <- est[2]
temp[12,2] <- paste0("[",round(est[2]-1.645*se[2], digits = 4),", ",round(est[2]+1.645*se[2], digits = 4),"]") 
rownames(temp)[11] <- "epr x pre-COVID"
rownames(temp)[12] <- "year"
temp
rm(model, est, se, temp)

# Negative binomial regression models for disruptive strike rate:
model1 <- fenegbin(disruptive ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)), df)
model2 <- fenegbin(disruptive ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) | month, df)
model3 <- fenegbin(disruptive ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) | airport + month, df)
model4 <- fenegbin(disruptive ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month, df)
model5 <- fenegbin(disruptive ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month^usfws_region, df)
model6 <- fenegbin(disruptive ~ epr:as.factor(covid_month) + epr2:as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month^usfws_region, df)
model7 <- fenegbin(disruptive ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) + airport:year | airport + month^usfws_region, df)
model8 <- fenegbin(disruptive ~ epr:as.factor(covid_month) + epr2:as.factor(covid_month) + offset(log(total_civil_movements)) + airport:year | airport + month^usfws_region, df)

# Data frame that contains model details, BIC, dBIC, and LL:
results_df <- data.frame(model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8"),
                         model_formula = c("disruptive ~ epr:as.factor(covid_month) + offset(log(total_civil_movements))",
                                           "disruptive ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) | month",
                                           "disruptive ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) | airport + month",
                                           "disruptive ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month",
                                           "disruptive ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month^usfws_region",
                                           "disruptive ~ epr:as.factor(covid_month) +  epr2:as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month^usfws_region",
                                           "disruptive ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) + airport:year | airport + month^usfws_region",
                                           "disruptive ~ epr:as.factor(covid_month) +  epr2:as.factor(covid_month) + offset(log(total_civil_movements)) + airport:year | airport + month^usfws_region"),
                         k = c(model1$nparams, model2$nparams, model3$nparams, model4$nparams, model5$nparams, model6$nparams, model7$nparams, model8$nparams),
                         bic = c(BIC(model1), BIC(model2), BIC(model3), BIC(model4), BIC(model5), BIC(model6), BIC(model7), BIC(model8)),
                         delta_bic = c(NA, NA, NA, NA, NA, NA, NA, NA),
                         ll = c(logLik(model1), logLik(model2), logLik(model3), logLik(model4), logLik(model5), logLik(model6), logLik(model7), logLik(model8)))
results_df <- results_df %>% arrange(bic) %>% mutate(delta_bic = bic - min(bic))

# Show data frame (TABLE S5b):
results_df
rm(model1, model2, model3, model4, model5, model6, model7, model8, results_df)

# Full model output from the best performing model (TABLE S7):
model <- fenegbin(disruptive ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) | airport + month, df, vcov = "cluster")
summary(model)

# Collect the coefficients of interest from the best performing model:
temp_df <- data.frame(irr = exp(model$coeftable[2:11,1]),
                      se = deltamethod(list(~exp(x1), ~exp(x2), ~exp(x3), ~exp(x4), ~exp(x5), ~exp(x6), ~exp(x7), ~exp(x8), ~exp(x9), ~exp(x10)), 
                                       coef(model)[2:11], vcov(model)[-c(1,12), -c(1,12)]))
temp_df <- temp_df %>% 
  mutate(month = factor(month.abb[3:12], levels = month.abb[3:12])) %>%
  mutate(var = "(b) Disruptive strikes") %>%
  mutate(est = (irr-1)*100) %>%
  mutate(lower_90 = ((irr - se*1.645)-1)*100) %>%
  mutate(upper_90 = ((irr + se*1.645)-1)*100) %>%
  mutate(lower_95 = ((irr - se*1.96)-1)*100) %>%
  mutate(upper_95 = ((irr + se*1.96)-1)*100)

# Calculate IRRs and CIs that are presented in TABLE S7:
temp <- as.data.frame(model$coeftable)
temp <- temp_df %>% 
  mutate(irr = round(irr, digits = 4)) %>%
  mutate(ci = paste0("[",round((lower_90/100)+1, digits = 4),", ",round((upper_90/100)+1, digits = 4),"]")) %>%
  select(irr, ci)
est <- exp(model$coeftable[1,1])
se <- deltamethod(list(~exp(x1)), coef(model)[1], vcov(model)[c(1), c(1)])
temp[11,1] <- est
temp[11,2] <- paste0("[",round(est-1.645*se, digits = 4),", ",round(est+1.645*se, digits = 4),"]") 
rownames(temp)[11] <- "epr x pre-COVID"
temp
rm(model, est, se, temp)

# Combine the results for the "All strikes" and "Disruptive strikes" models:
plot_df <- rbind(plot_df, temp_df); rm(temp_df, model)

# Generate plot of estimates (FIGURE 5):
png("figure_5.png", width = 3.75, height = 5.75, units = "in", res = 600)
ggplot() +
  geom_hline(yintercept = 0) +
  geom_linerange(data = plot_df, aes(x = month, ymin = lower_95, ymax = upper_95, color = "95% C.I."), size = 4.5, key_glyph = "path") +
  geom_linerange(data = plot_df, aes(x = month, ymin = lower_90, ymax = upper_90, color = "90% C.I."), size = 4.5, key_glyph = "path") +
  geom_line(data = plot_df, aes(x = month, y = est, group = 1), size =0.8, color = "grey35") +
  geom_point(data = plot_df, aes(x = month, y = est), shape=21, size =3.5, color = "grey35", fill = "white", stroke=1) +
  theme_clean() +
  ylab("Estimated percent \u0394 in wildlife strike rate in\nresponse to a 1% reduction in aircraft movements\n") + 
  xlab("\nMonth of 2020") +
  scale_color_manual(name = "", values = c("#FB6A4A", "#F4AC9D"), breaks=c("90% C.I.", "95% C.I.")) +
  scale_y_continuous(breaks = scales::extended_breaks(n=7)) +
  theme(legend.position = c(0.79,0.935), 
        legend.key.height = unit(0.35, 'cm'), 
        legend.background = element_blank(), legend.text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, vjust = 0.65), 
        strip.text = element_text(face = "bold"),
        plot.background = element_rect(color = "white")) +
  facet_wrap(~ var, nrow = 2, scales = "free", dir = "h") 
dev.off()

# Remove plot dataframe:
rm(plot_df)

########################################################################################################################



########################################################################################################################
# FIGURE S3.Month-specific model estimated changes in size-specific wildlife strike rates during the COVID-19 months of 
# 2020—(a) small (<= 442g), (b) medium (>= 443g & <= 1500g), and (c) large (> 1500g). The connected points in each panel 
# report the model estimated percent change in the wildlife strike rate during the COVID months of 2020, all else equal. 
# The models used to generate these size-specific strike rate estimates are identical to the preferred model in 
# Supplementary Information Table S2a—i.e., Model 5. The error bars provide the associated 90% and 95% intervals of 
# confidence that surround each estimate.

# Collect the coefficients of interest for small-sized wildlife strike rate:
model <- fenegbin(strike_size_small ~ as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month^usfws_region, df, vcov = "cluster")
plot_df <- data.frame(irr = exp(model$coeftable[1:10,1]),
                      se = deltamethod(list(~exp(x1), ~exp(x2), ~exp(x3), ~exp(x4), ~exp(x5), ~exp(x6), ~exp(x7), ~exp(x8), ~exp(x9), ~exp(x10)), coef(model), vcov(model)[-c(12), -c(12)]))
plot_df <- plot_df %>% 
  mutate(month = factor(month.abb[3:12], levels = month.abb[3:12])) %>%
  mutate(var = "(a) Small-sized strikes") %>%
  mutate(est = (irr-1)*100) %>%
  mutate(lower_90 = ((irr - se*1.645)-1)*100) %>%
  mutate(upper_90 = ((irr + se*1.645)-1)*100) %>%
  mutate(lower_95 = ((irr - se*1.96)-1)*100) %>%
  mutate(upper_95 = ((irr + se*1.96)-1)*100)

# Collect the coefficients of interest medium-sized wildlife strike rate:
model <- fenegbin(strike_size_medium ~ as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month^usfws_region, 
                  df, vcov = "cluster")
temp_df <- data.frame(irr = exp(model$coeftable[1:10,1]),
                      se = deltamethod(list(~exp(x1), ~exp(x2), ~exp(x3), ~exp(x4), ~exp(x5), 
                                            ~exp(x6), ~exp(x7), ~exp(x8), ~exp(x9), ~exp(x10)), 
                                       coef(model), vcov(model)[-c(12), -c(12)]))
temp_df <- temp_df %>% 
  mutate(month = factor(month.abb[3:12], levels = month.abb[3:12])) %>%
  mutate(var = "(b) Medium-sized strikes") %>%
  mutate(est = (irr-1)*100) %>%
  mutate(lower_90 = ((irr - se*1.645)-1)*100) %>%
  mutate(upper_90 = ((irr + se*1.645)-1)*100) %>%
  mutate(lower_95 = ((irr - se*1.96)-1)*100) %>%
  mutate(upper_95 = ((irr + se*1.96)-1)*100)

# Combine the small- and medium-sized wildlife results:
plot_df <- rbind(plot_df, temp_df); rm(temp_df)

# Collect the coefficients of interest large-sized wildlife strike rate:
model <- fenegbin(strike_size_large ~ as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month^usfws_region, 
                  df, vcov = "cluster")
temp_df <- data.frame(irr = exp(model$coeftable[1:10,1]),
                      se = deltamethod(list(~exp(x1), ~exp(x2), ~exp(x3), ~exp(x4), ~exp(x5), 
                                            ~exp(x6), ~exp(x7), ~exp(x8), ~exp(x9), ~exp(x10)), 
                                       coef(model), vcov(model)[-c(12), -c(12)]))
temp_df <- temp_df %>% 
  mutate(month = factor(month.abb[3:12], levels = month.abb[3:12])) %>%
  mutate(var = "(c) Large-sized strikes") %>%
  mutate(est = (irr-1)*100) %>%
  mutate(lower_90 = ((irr - se*1.645)-1)*100) %>%
  mutate(upper_90 = ((irr + se*1.645)-1)*100) %>%
  mutate(lower_95 = ((irr - se*1.96)-1)*100) %>%
  mutate(upper_95 = ((irr + se*1.96)-1)*100)

# Combine the small-, medium-, and large-sized wildlife results:
plot_df <- rbind(plot_df, temp_df); rm(temp_df, model)

# Generate plot of estimates (FIGURE S3):
png("figure_s3.png", width = 3.75, height = 6.75, units = "in", res = 600)
ggplot() +
  geom_hline(yintercept = 0) +
  geom_linerange(data = plot_df, aes(x = month, ymin = lower_95, ymax = upper_95, color = "95% C.I."), size = 4.5, key_glyph = "path") +
  geom_linerange(data = plot_df, aes(x = month, ymin = lower_90, ymax = upper_90, color = "90% C.I."), size = 4.5, key_glyph = "path") +
  geom_line(data = plot_df, aes(x = month, y = est, group = 1), size =0.8, color = "grey35") +
  geom_point(data = plot_df, aes(x = month, y = est), shape=21, size =3.5, color = "grey35", fill = "white", stroke=1) +
  theme_clean() +
  ylab("Estimated percent \u0394 in wildlife strike\nrate relative to non-COVID conditions\n") + 
  xlab("\nMonth of 2020") +
  scale_color_manual(name = "", values = c("#FB6A4A", "#F4AC9D"), breaks=c("90% C.I.", "95% C.I.")) +
  theme(legend.position = c(0.835,0.975), 
        legend.key.height = unit(0.35, 'cm'), 
        legend.background = element_blank(), legend.text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, vjust = 0.65), 
        strip.text = element_text(face = "bold"),
        plot.background = element_rect(color = "white")) +
  facet_wrap(~ var, nrow = 3, scales = "free", dir = "h") 
dev.off()

# Remove plot dataframe:
rm(plot_df)

########################################################################################################################



########################################################################################################################
# Figure S4. Comparing estimated changes in the (a) overall and (b) disruptive wildlife strike rate during the COVID-19 
# months of 2020 under alternative “treatment” assumptions. Specifically, the preferred models from Supplementary Table 
# S2 are re-estimated under two alternative strategies. First, the sample of analysis is subset to include observations
# from January 2014-December 2019 and “treatment” is assigned to the COVID-19 months of 2019 (March-December)—we call 
# this the “Non-COVID” strategy. Second, the sample of analysis is subset to include observations from 
# January 2014-December 2018 and January 2020-December 2020—-i.e., 2019 data is excluded. “Actual treatment” is 
# maintained for the COVID-19 months of 2020 (March-December) since these were, in fact, the months of data that were 
# affected by the COVID-19 pandemic)—we call this the “COVID” strategy. The parameters of interest, estimated under the 
# “Non-COVID” and “COVID” strategies, are then compared. If the regression models sufficiently predict variation in 
# pre-COVID wildlife strike rates, then the expectation is that the “Non-COVID”—i.e., 2019—estimates should not 
# significantly deviate from 0. Further, if pandemic-induced reductions in air traffic volume did, in fact, result in 
# elevated strike rates that are unanticipated by pre-COVID data, the “COVID” estimates should significantly diverge 
# from 0, in addition to significantly deviating from the “Non-COVID” estimates. The estimates presented in panel (a) 
# validate both expectations during the months May-September while the “COVID” and “Non-COVID” estimates in panel (b) 
# only significantly depart from each other in June.

# Generate new COVID indicator that assumes March-December 2019 and March-December 2020 were both subject to a pandemic:
df$new_covid_month <- ifelse(df$month>=3 & df$year %in% c(2019, 2020), df$month-2, 0)

# Collect the 2019 coefficients of interest for the overall strike rate (remove 2020 data):
model <- fenegbin(strike ~ as.factor(new_covid_month) + offset(log(total_civil_movements)) + year + year^2 | airport + month^usfws_region, 
                  subset(df, year<=2019), vcov = "cluster")
plot_df <- data.frame(irr = exp(model$coeftable[1:10,1]),
                      se = deltamethod(list(~exp(x1), ~exp(x2), ~exp(x3), ~exp(x4), ~exp(x5), 
                                            ~exp(x6), ~exp(x7), ~exp(x8), ~exp(x9), ~exp(x10)), 
                                       coef(model)[1:10], vcov(model)[-c(11:13), -c(11:13)]))
plot_df <- plot_df %>% 
  mutate(month = factor(month.abb[3:12], levels = month.abb[3:12])) %>%
  mutate(var = "(a) All strikes") %>%
  mutate(placebo = "Placebo") %>%
  mutate(est = (irr-1)*100) %>%
  mutate(lower_90 = ((irr - se*1.645)-1)*100) %>%
  mutate(upper_90 = ((irr + se*1.645)-1)*100) %>%
  mutate(lower_95 = ((irr - se*1.96)-1)*100) %>%
  mutate(upper_95 = ((irr + se*1.96)-1)*100)

# Collect the 2020 coefficients of interest for the overall strike rate (remove 2019 data):
model <- fenegbin(strike ~ as.factor(new_covid_month) + offset(log(total_civil_movements)) + year + year^2 | airport + month^usfws_region, 
                  subset(df, year!=2019), vcov = "cluster")
temp_df <- data.frame(irr = exp(model$coeftable[1:10,1]),
                      se = deltamethod(list(~exp(x1), ~exp(x2), ~exp(x3), ~exp(x4), ~exp(x5), 
                                            ~exp(x6), ~exp(x7), ~exp(x8), ~exp(x9), ~exp(x10)), 
                                       coef(model)[1:10], vcov(model)[-c(11:13), -c(11:13)]))
temp_df <- temp_df %>% 
  mutate(month = factor(month.abb[3:12], levels = month.abb[3:12])) %>%
  mutate(var = "(a) All strikes") %>%
  mutate(placebo = "Actual") %>%
  mutate(est = (irr-1)*100) %>%
  mutate(lower_90 = ((irr - se*1.645)-1)*100) %>%
  mutate(upper_90 = ((irr + se*1.645)-1)*100) %>%
  mutate(lower_95 = ((irr - se*1.96)-1)*100) %>%
  mutate(upper_95 = ((irr + se*1.96)-1)*100)

# Combine the results for the "2019" and "2020" overall strike rate estimates:
plot_df <- rbind(plot_df, temp_df); rm(temp_df)

# Collect the 2019 coefficients of interest for the disruptive strike rate (remove 2020 data):
model <- fenegbin(disruptive ~ as.factor(new_covid_month) + offset(log(total_civil_movements)) + year | airport + month, 
                  subset(df, year<=2019), vcov = "cluster")
temp_df <- data.frame(irr = exp(model$coeftable[1:10,1]),
                      se = deltamethod(list(~exp(x1), ~exp(x2), ~exp(x3), ~exp(x4), ~exp(x5),
                                            ~exp(x6), ~exp(x7), ~exp(x8), ~exp(x9), ~exp(x10)), 
                                       coef(model), vcov(model)[-c(11), -c(11)]))
temp_df <- temp_df %>% 
  mutate(month = factor(month.abb[3:12], levels = month.abb[3:12])) %>%
  mutate(var = "(b) Disruptive strikes") %>%
  mutate(placebo = "Placebo") %>%
  mutate(est = (irr-1)*100) %>%
  mutate(lower_90 = ((irr - se*1.645)-1)*100) %>%
  mutate(upper_90 = ((irr + se*1.645)-1)*100) %>%
  mutate(lower_95 = ((irr - se*1.96)-1)*100) %>%
  mutate(upper_95 = ((irr + se*1.96)-1)*100)

# Append results to the overall strike rate results:
plot_df <- rbind(plot_df, temp_df); rm(temp_df)

# Collect the 2020 coefficients of interest for the disruptive strike rate (remove 2019 data):
model <- fenegbin(disruptive ~ as.factor(new_covid_month) + offset(log(total_civil_movements)) + year | airport + month, 
                  subset(df, year!=2019), vcov = "cluster")
temp_df <- data.frame(irr = exp(model$coeftable[1:10,1]),
                      se = deltamethod(list(~exp(x1), ~exp(x2), ~exp(x3), ~exp(x4), ~exp(x5), 
                                            ~exp(x6), ~exp(x7), ~exp(x8), ~exp(x9), ~exp(x10)), 
                                       coef(model), vcov(model)[-c(11), -c(11)]))
temp_df <- temp_df %>% 
  mutate(month = factor(month.abb[3:12], levels = month.abb[3:12])) %>%
  mutate(var = "(b) Disruptive strikes") %>%
  mutate(placebo = "Actual") %>%
  mutate(est = (irr-1)*100) %>%
  mutate(lower_90 = ((irr - se*1.645)-1)*100) %>%
  mutate(upper_90 = ((irr + se*1.645)-1)*100) %>%
  mutate(lower_95 = ((irr - se*1.96)-1)*100) %>%
  mutate(upper_95 = ((irr + se*1.96)-1)*100)

# Append results to all other results:
plot_df <- rbind(plot_df, temp_df); rm(temp_df, model)
plot_df$month <- as.numeric(plot_df$month)

# Generate plot of estimates:
p <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_linerange(data = subset(plot_df, placebo=="Actual"), aes(x = month, ymin = lower_95, ymax = upper_95, color = "95% C.I."), 
                 size = 4.5, key_glyph = "path") +
  geom_linerange(data = subset(plot_df, placebo=="Actual"), aes(x = month, ymin = lower_90, ymax = upper_90, color = "90% C.I."), 
                 size = 4.5, key_glyph = "path") +
  geom_line(data = subset(plot_df, placebo=="Actual", color = "COVID Estimate (2020)"), aes(x = month, y = est, group = 1), 
            size =0.8, color = "grey35") +
  geom_point(data = subset(plot_df, placebo=="Actual", color = "COVID Estimate (2020)"), aes(x = month, y = est), 
             shape=21, size =3.5, color = "grey35", fill = "white", stroke=1) +
  geom_ribbon(data = subset(plot_df, placebo=="Placebo"), aes(x = month, ymin = lower_95, ymax = upper_95, 
                                                              fill = "95% C.I."), alpha=0.15) +
  geom_ribbon(data = subset(plot_df, placebo=="Placebo"), aes(x = month, ymin = lower_90, ymax = upper_90, 
                                                              fill = "90% C.I."), alpha=0.25) +
  geom_line(data = subset(plot_df, placebo=="Placebo", color = "Non-COVID Estimate (2019)"), aes(x = month, y = est, group = 1), 
            size =0.8, color = "grey35") +
  geom_point(data = subset(plot_df, placebo=="Placebo", color = "Non-COVID Estimate (2019)"), aes(x = month, y = est), 
             shape=21, size =2.5, color = "grey35", fill = "grey35", stroke=1) +
  theme_clean() +
  ylab("Estimated percent \u0394 in wildlife strike\nrate relative to non-COVID conditions\n") + 
  xlab("\nMonth of 2020") +
  scale_color_manual(name = "", values = c("#FB6A4A", "#F4AC9D"), breaks=c("90% C.I.", "95% C.I.")) +
  scale_fill_manual(name = "", values = c("grey15", "grey60"), breaks=c("90% C.I.", "95% C.I.")) +
  scale_x_continuous(breaks = seq(1,10,1), labels = month.abb[3:12]) +
  scale_y_continuous(breaks = scales::extended_breaks(n=7)) +
  theme(legend.position = c(0.83,0.765), 
        legend.background = element_blank(), legend.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, vjust = 0.65), 
        strip.text = element_text(face = "bold"),
        legend.spacing.y = unit(4.3, "lines"), legend.text.align = 0.5,
        plot.background = element_rect(color = "white")) +
  guides(color = guide_legend(order = 1, keyheight = unit(0.01, 'lines'), keywidth = unit(0.1, 'cm')),
         fill = guide_legend(order = 2, keyheight = unit(0.4, 'cm'), keywidth = unit(0.4, 'cm'))) +
  facet_wrap(~ var, nrow = 2, scales = "free", dir = "h") 

leg <- get_legend(
  ggplot() +
    geom_hline(yintercept = 0) +
    geom_linerange(data = subset(plot_df, placebo=="Actual"), aes(x = month, ymin = lower_95, ymax = upper_95), 
                   color = "#F4AC9D", size = 4.5, key_glyph = "path") +
    geom_linerange(data = subset(plot_df, placebo=="Actual"), aes(x = month, ymin = lower_90, ymax = upper_90), 
                   color = "#FB6A4A", size = 4.5, key_glyph = "path") +
    geom_line(data = subset(plot_df, placebo=="Actual"), aes(x = month, y = est, group = 1, color = "COVID Estimate (2020)"), 
              size =0.8) +
    geom_point(data = subset(plot_df, placebo=="Actual"), aes(x = month, y = est, color = "COVID Estimate (2020)",
                                                              fill = "COVID Estimate (2020)"), shape=21, size =3.5, stroke=1) +
    geom_ribbon(data = subset(plot_df, placebo=="Placebo"), aes(x = month, ymin = lower_95, ymax = upper_95), 
                fill = "grey60", alpha=0.15) +
    geom_ribbon(data = subset(plot_df, placebo=="Placebo"), aes(x = month, ymin = lower_90, ymax = upper_90), 
                fill = "grey15", alpha=0.15) +
    geom_line(data = subset(plot_df, placebo=="Placebo"), aes(x = month, y = est, group = 1, color = "Non-COVID Estimate (2019)"), 
              size =0.8) +
    geom_point(data = subset(plot_df, placebo=="Placebo"), aes(x = month, y = est, color = "Non-COVID Estimate (2019)", 
                                                               fill = "Non-COVID Estimate (2019)"), shape=21, size =2.5, stroke=1) +
    theme_clean() +
    ylab("Estimated percent \u0394 in wildlife strike\nrate relative to model expectation\n") + 
    xlab("\nMonth of 2020") +
    scale_color_manual(name = "", values = c("grey35", "grey35"), 
                       breaks=c("COVID Estimate (2020)", "Non-COVID Estimate (2019)")) +
    scale_fill_manual(name = "", values = c("white", "grey35"), 
                      breaks=c("COVID Estimate (2020)", "Non-COVID Estimate (2019)")) +
    scale_x_continuous(breaks = seq(1,10,1), labels = month.abb[3:12]) +
    theme(legend.position = "bottom", legend.key.width = unit(1, 'cm'), 
          legend.key.height = unit(0.4, 'cm'), legend.direction = "vertical",
          legend.background = element_blank(), legend.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45, vjust = 0.65), 
          strip.text = element_text(face = "bold"), legend.text.align = 0, legend.margin = margin(-15, 0, 0, 0)) +
    facet_wrap(~ var, nrow = 2, scales = "free", dir = "h") 
)

png("figure_s4.png", width = 4.25, height = 5.75, units = "in", res = 600)
grid.arrange(arrangeGrob(p),arrangeGrob(leg), heights = c(4.2,0.65))
dev.off()

# Remove plot dataframe and objects:
rm(plot_df, p, leg)

########################################################################################################################



########################################################################################################################
# Figure S5. Comparing the estimated relationship between aircraft movement reductions and changes in the (a) overall 
# and (b) disruptive wildlife strike rate during the COVID-19 months of 2020 under alternative “treatment” assumptions. 
# Specifically, the preferred models from Supplementary Table S5 are re-estimated under two alternative strategies. 
# First, the sample of analysis is subset to include observations from January 2014-December 2019 and “treatment” is 
# assigned to the COVID-19 months of 2019 (March-December)—i.e., pandemic-induced reductions in air traffic volume (EPR) 
# are assigned to March-December of 2019. We call this the “Non-COVID” strategy. Second, the sample of analysis is 
# subset to include observations from January 2014-December 2018 and January 2020-December 2020—i.e., 2019 data is 
# excluded. “Actual treatment” is maintained for the COVID-19 months of 2020 (March-December) since these were, in fact, 
# the months of data that were affected by the COVID-19 pandemic—we call this the “COVID” strategy. The parameters of 
# interest, estimated under “Non-COVID” and “COVID” strategies, are then compared. If the regression models sufficiently 
# predict variation in pre-COVID wildlife strike rates, then pandemic-induced reductions in air traffic volume during 
# 2020 should not be significantly associated with deviations in wildlife strike rates in 2019. However, if pandemic-
# induced reductions in air traffic volume did have some influence on wildlife strike rates in 2020, the “COVID” 
# estimates should significantly deviate from 0, in addition to significantly diverging from the “Non-COVID” estimates.

# Generate two data sets. The first assigns 2020 "treatment" data to 2019 and the second drops 2019 and retains 2020
# data as its "treatment":
model_df_1 <- subset(df, year!=2019)
model_df_2 <- subset(select(df, -c(covid_month, epr)), year<=2019)
temp <- subset(select(df, c(airport, month, year, covid_month, epr)), year!=2019)
temp$year <- ifelse(temp$year==2020, 2019, temp$year)
model_df_2 <- merge(model_df_2, temp, by = c("airport", "month", "year")); rm(temp)

# Collect the 2019 coefficients of interest for the overall strike rate (2020 data removed):
model <- fenegbin(strike ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month^usfws_region, 
                  model_df_1, vcov = "cluster")
plot_df <- data.frame(irr = exp(model$coeftable[3:12,1]),
                      se = deltamethod(list(~exp(x1), ~exp(x2), ~exp(x3), ~exp(x4), 
                                            ~exp(x5), ~exp(x6), ~exp(x7), ~exp(x8), ~exp(x9), ~exp(x10)), 
                                       coef(model)[3:12], vcov(model)[-c(1:2,13), -c(1:2,13)]))
plot_df <- plot_df %>% 
  mutate(month = factor(month.abb[3:12], levels = month.abb[3:12])) %>%
  mutate(var = "(a) All strikes") %>%
  mutate(placebo = "Actual") %>%
  mutate(est = (irr-1)*100) %>%
  mutate(lower_90 = ((irr - se*1.645)-1)*100) %>%
  mutate(upper_90 = ((irr + se*1.645)-1)*100) %>%
  mutate(lower_95 = ((irr - se*1.96)-1)*100) %>%
  mutate(upper_95 = ((irr + se*1.96)-1)*100)

# Collect the 2020 coefficients of interest for the overall strike rate (2019 data removed):
model <- fenegbin(strike ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) + year | airport + month^usfws_region, 
                  model_df_2, vcov = "cluster")
temp_df <- data.frame(irr = exp(model$coeftable[3:12,1]),
                      se = deltamethod(list(~exp(x1), ~exp(x2), ~exp(x3), ~exp(x4), 
                                            ~exp(x5), ~exp(x6), ~exp(x7), ~exp(x8), ~exp(x9), ~exp(x10)), 
                                       coef(model)[3:12], vcov(model)[-c(1:2,13), -c(1:2,13)]))
temp_df <- temp_df %>% 
  mutate(month = factor(month.abb[3:12], levels = month.abb[3:12])) %>%
  mutate(var = "(a) All strikes") %>%
  mutate(placebo = "Placebo") %>%
  mutate(est = (irr-1)*100) %>%
  mutate(lower_90 = ((irr - se*1.645)-1)*100) %>%
  mutate(upper_90 = ((irr + se*1.645)-1)*100) %>%
  mutate(lower_95 = ((irr - se*1.96)-1)*100) %>%
  mutate(upper_95 = ((irr + se*1.96)-1)*100)

# Combine the results for the "2019" and "2020" overall strike rate estimates:
plot_df <- rbind(plot_df, temp_df); rm(temp_df)

# Collect the 2019 coefficients of interest for the disruptive strike rate (2020 data removed):
model <- fenegbin(disruptive ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) | airport + month, 
                  model_df_1, vcov = "cluster")
temp_df <- data.frame(irr = exp(model$coeftable[2:11,1]),
                      se = deltamethod(list(~exp(x1), ~exp(x2), ~exp(x3), ~exp(x4), 
                                            ~exp(x5), ~exp(x6), ~exp(x7), ~exp(x8), ~exp(x9), ~exp(x10)), 
                                       coef(model)[2:11], vcov(model)[-c(1,12), -c(1,12)]))
temp_df <- temp_df %>% 
  mutate(month = factor(month.abb[3:12], levels = month.abb[3:12])) %>%
  mutate(var = "(b) Disruptive strikes") %>%
  mutate(placebo = "Actual") %>%
  mutate(est = (irr-1)*100) %>%
  mutate(lower_90 = ((irr - se*1.645)-1)*100) %>%
  mutate(upper_90 = ((irr + se*1.645)-1)*100) %>%
  mutate(lower_95 = ((irr - se*1.96)-1)*100) %>%
  mutate(upper_95 = ((irr + se*1.96)-1)*100)

# Append results to the overall strike rate results:
plot_df <- rbind(plot_df, temp_df); rm(temp_df)

# Collect the 2020 coefficients of interest for the disruptive strike rate (2019 data removed):
model <- fenegbin(disruptive ~ epr:as.factor(covid_month) + offset(log(total_civil_movements)) | airport + month, 
                  model_df_2, vcov = "cluster")
temp_df <- data.frame(irr = exp(model$coeftable[2:11,1]),
                      se = deltamethod(list(~exp(x1), ~exp(x2), ~exp(x3), ~exp(x4), ~exp(x5), 
                                            ~exp(x6), ~exp(x7), ~exp(x8), ~exp(x9), ~exp(x10)), 
                                       coef(model)[2:11], vcov(model)[-c(1,12), -c(1,12)]))
temp_df <- temp_df %>% 
  mutate(month = factor(month.abb[3:12], levels = month.abb[3:12])) %>%
  mutate(var = "(b) Disruptive strikes") %>%
  mutate(placebo = "Placebo") %>%
  mutate(est = (irr-1)*100) %>%
  mutate(lower_90 = ((irr - se*1.645)-1)*100) %>%
  mutate(upper_90 = ((irr + se*1.645)-1)*100) %>%
  mutate(lower_95 = ((irr - se*1.96)-1)*100) %>%
  mutate(upper_95 = ((irr + se*1.96)-1)*100)

# Append results to other results:
plot_df <- rbind(plot_df, temp_df); rm(temp_df, model_df_1, model_df_2, model)
plot_df$month <- as.numeric(plot_df$month)

# Generate plot of estimates:

p <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_linerange(data = subset(plot_df, placebo=="Actual"), aes(x = month, ymin = lower_95, ymax = upper_95, color = "95% C.I."), 
                 size = 4.5, key_glyph = "path") +
  geom_linerange(data = subset(plot_df, placebo=="Actual"), aes(x = month, ymin = lower_90, ymax = upper_90, color = "90% C.I."), 
                 size = 4.5, key_glyph = "path") +
  geom_line(data = subset(plot_df, placebo=="Actual", color = "COVID Estimate (2020)"), aes(x = month, y = est, group = 1), 
            size =0.8, color = "grey35") +
  geom_point(data = subset(plot_df, placebo=="Actual", color = "COVID Estimate (2020)"), aes(x = month, y = est), shape=21, 
             size =3.5, color = "grey35", fill = "white", stroke=1) +
  geom_ribbon(data = subset(plot_df, placebo=="Placebo"), aes(x = month, ymin = lower_95, ymax = upper_95, fill = "95% C.I."), 
              alpha=0.15) +
  geom_ribbon(data = subset(plot_df, placebo=="Placebo"), aes(x = month, ymin = lower_90, ymax = upper_90, fill = "90% C.I."), 
              alpha=0.25) +
  geom_line(data = subset(plot_df, placebo=="Placebo", color = "Non-COVID Estimate (2019)"), aes(x = month, y = est, group = 1), 
            size =0.8, color = "grey35") +
  geom_point(data = subset(plot_df, placebo=="Placebo", color = "Non-COVID Estimate (2019)"), aes(x = month, y = est),
             shape=21, size =2.5, color = "grey35", fill = "grey35", stroke=1) +
  theme_clean() +
  ylab("Estimated percent \u0394 in wildlife strike rate in\nresponse to a 1% reduction in aircraft movements\n") + 
  xlab("\nMonth of 2020") +
  scale_color_manual(name = "", values = c("#FB6A4A", "#F4AC9D"), breaks=c("90% C.I.", "95% C.I.")) +
  scale_fill_manual(name = "", values = c("grey15", "grey60"), breaks=c("90% C.I.", "95% C.I.")) +
  scale_x_continuous(breaks = seq(1,10,1), labels = month.abb[3:12]) +
  scale_y_continuous(breaks = scales::extended_breaks(n=7)) +
  theme(legend.position = c(0.83,0.7515), 
        legend.background = element_blank(), legend.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, vjust = 0.65), 
        strip.text = element_text(face = "bold"),
        legend.spacing.y = unit(3.89, "lines"), legend.text.align = 0.5,
        plot.background = element_rect(color = "white")) +
  guides(color = guide_legend(order = 1, keyheight = unit(0.01, 'lines'), keywidth = unit(0.1, 'cm')),
         fill = guide_legend(order = 2, keyheight = unit(0.4, 'cm'), keywidth = unit(0.4, 'cm'))) +
  facet_wrap(~ var, nrow = 2, scales = "free", dir = "h") 

leg <- get_legend(
  ggplot() +
    geom_hline(yintercept = 0) +
    geom_linerange(data = subset(plot_df, placebo=="Actual"), aes(x = month, ymin = lower_95, ymax = upper_95), 
                   color = "#F4AC9D", size = 4.5, key_glyph = "path") +
    geom_linerange(data = subset(plot_df, placebo=="Actual"), aes(x = month, ymin = lower_90, ymax = upper_90), 
                   color = "#FB6A4A", size = 4.5, key_glyph = "path") +
    geom_line(data = subset(plot_df, placebo=="Actual"), aes(x = month, y = est, group = 1, color = "COVID Estimate (2020)"), 
              size =0.8) +
    geom_point(data = subset(plot_df, placebo=="Actual"), aes(x = month, y = est, color = "COVID Estimate (2020)",
                                                              fill = "COVID Estimate (2020)"), shape=21, size =3.5, stroke=1) +
    geom_ribbon(data = subset(plot_df, placebo=="Placebo"), aes(x = month, ymin = lower_95, ymax = upper_95), 
                fill = "grey60", alpha=0.15) +
    geom_ribbon(data = subset(plot_df, placebo=="Placebo"), aes(x = month, ymin = lower_90, ymax = upper_90), 
                fill = "grey15", alpha=0.15) +
    geom_line(data = subset(plot_df, placebo=="Placebo"), aes(x = month, y = est, group = 1, color = "Non-COVID Estimate (2019)"), 
              size =0.8) +
    geom_point(data = subset(plot_df, placebo=="Placebo"), aes(x = month, y = est, color = "Non-COVID Estimate (2019)", 
                                                               fill = "Non-COVID Estimate (2019)"), shape=21, size =2.5, stroke=1) +
    theme_clean() +
    ylab("Estimated percent \u0394 in wildlife strike rate in\nresponse to a 1% reduction in aircraft movements\n") + 
    xlab("\nMonth of 2020") +
    scale_color_manual(name = "", values = c("grey35", "grey35"), breaks=c("COVID Estimate (2020)", "Non-COVID Estimate (2019)")) +
    scale_fill_manual(name = "", values = c("white", "grey35"), breaks=c("COVID Estimate (2020)", "Non-COVID Estimate (2019)")) +
    scale_x_continuous(breaks = seq(1,10,1), labels = month.abb[3:12]) +
    theme(legend.position = "bottom", legend.key.width = unit(1, 'cm'), 
          legend.key.height = unit(0.4, 'cm'), legend.direction = "vertical",
          legend.background = element_blank(), legend.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45, vjust = 0.65), 
          strip.text = element_text(face = "bold"), legend.text.align = 0, legend.margin = margin(-15, 0, 0, 0)) +
    facet_wrap(~ var, nrow = 2, scales = "free", dir = "h") 
)

png("figure_s5.png", width = 4.25, height = 5.75, units = "in", res = 600)
grid.arrange(arrangeGrob(p),arrangeGrob(leg), heights = c(4.2,0.65))
dev.off()

# Remove plot dataframe and objects:
rm(plot_df, p, leg)

########################################################################################################################



########################################################################################################################
#                                                          End                                                         #
########################################################################################################################

