# EMPIRICAL ECONOMICS 2023 - Gabriele Durante
# GRAVITY MODEL - ESSAY FINAL EXAM

#
##
###
####
##### SETTINGS
####
###
##
#

#### libraries
library(tidyverse)
library(ggplot2)
library(corrplot)
library(janitor)
library(lmtest)
library(stargazer)
library(readr)
library(gridExtra)
library(cowplot)
library(modelsummary)

setwd("/Users/gabrieledurante/Desktop/EP - Project")
getwd()

Gravity_V202211 <- read_csv("lecture_4/Gravity_V202211.csv")
View(Gravity_V202211)
names(Gravity_V202211)

gravity_data = read_csv("lecture_4/Gravity_V202211.csv")
colMeans(is.na(gravity_data))

head(gravity_data, n=100) %>% View
nrow(gravity_data)
names(gravity_data)


#
##
###
####
##### DATA CLEANING
####
###
##
#

gravity_data_clean = 
  gravity_data %>% 
  filter(country_exists_d==1,
         country_exists_o==1) 
head(gravity_data_clean, n=100) %>% View
gravity_data_clean$year %>% tabyl

# SELECT SOME VARIABLES
gravity_data_clean = 
  gravity_data_clean %>% 
  select(year,
         iso3_o, iso3_d,
         contig, dist, 
         gmt_offset_2020_o, gmt_offset_2020_d,
         comcol, col45,
         tradeflow_comtrade_o, tradeflow_comtrade_d,
         gatt_o, gatt_d,
         diplo_disagreement, distcap,
         sib_conflict, 
         heg_o, heg_d,
         comlang_off, comlang_ethno,
         pop_o, pop_d,
         gdp_o, gdp_d,
         gdpcap_o, gdpcap_d,
         comrelig,
         eu_o, eu_d, 
         wto_o, wto_d)
head(gravity_data_clean, n=100) %>% View

# FILTER FOR YEAR 2020
gravity_data_clean$year %>% unique %>% sort
gravity_data_clean2020 = gravity_data_clean %>%
  filter(year==2020)
gravity_data_clean2020$year %>% unique %>% sort

head(gravity_data_clean, n=100) %>% View
nrow(gravity_data_clean)
names(gravity_data_clean)

# RENAME VARIABLES
gravity_data_clean2020 = 
  gravity_data_clean2020 %>%
  rename(country_origin = iso3_o, country_dest = iso3_d,
         colonial_relationship = col45, common_colonizer = comcol,
         bilateral_trade_repO = tradeflow_comtrade_o, bilateral_trade_repD = tradeflow_comtrade_d,
         population_origin = pop_o, population_dest = pop_d,
         gdp_origin = gdp_o, gdp_dest = gdp_d,
         gdp_percapita_origin = gdpcap_o, gdp_percapita_dest = gdpcap_d,
         contiguity = contig, distance = dist,
         religion_sim = comrelig,
         common_language_primary = comlang_off, common_language_second = comlang_ethno,
         eu_member_origin = eu_o, eu_member_dest = eu_d,
         wto_member_origin = wto_o, wto_member_dest = wto_d)

names(gravity_data_clean2020)

#EXPORT DATASET
write.csv(gravity_data_clean2020, file = "gravity_data_clean2020")

gravity_data_clean2020 <- read_csv("gravity_data_clean2020")
View(gravity_data_clean2020)
gravity_data_clean2020 <- subset(gravity_data_clean2020, select = -1)
View(gravity_data_clean2020)


# CHECK MISSING DATA
colMeans(is.na(gravity_data_clean2020)) %>% sort

gravity_data_clean2020$gmt_offset_2020_o %>% unique %>% sort
gravity_data_clean2020$gatt_o %>% unique %>% sort
gravity_data_clean2020$common_colonizer %>% unique %>% sort
gravity_data_clean2020$colonial_relationship %>% unique %>% sort
gravity_data_clean2020$heg_o %>% unique %>% sort
gravity_data_clean2020$heg_d %>% unique %>% sort
gravity_data_clean2020$sib_conflict %>% unique %>% sort
gravity_data_clean2020$dist %>% unique %>% sort
gravity_data_clean2020$gdp_percapita_origin %>% unique %>% sort
gravity_data_clean2020$diplo_disagreement %>% unique %>% sort

# SUMMARY
gravity_data_clean2020 %>% str
gravity_data_clean2020 %>% summary()
view(gravity_data_clean2020)
head(gravity_data_clean2020, n=100) %>% View


#
##
###
####
##### STATISTICAL ANALYSIS of DATA
####
###
##
#

library(gridExtra)
library(cowplot)

#REMOVE VALUES FOR TRADES WITH THEMSELVES
gravity_data_clean2020 = 
  gravity_data_clean2020 %>% 
  filter(country_origin != country_dest)

#CORRELATION ANALYSIS
# correlation matrix is very complicated to read, so we use the corrplot()
# function to get an intuitive graphical display.

sapply(gravity_data_clean2020, is.numeric)
correlation <- gravity_data_clean2020 %>%
  select( 
    contiguity,
    distance, 
    gmt_offset_2020_o, 
    gmt_offset_2020_d,
    common_colonizer,
    colonial_relationship,
    bilateral_trade_repO,
    bilateral_trade_repD,
    gatt_o,
    gatt_d,
    diplo_disagreement,
    distcap,
    sib_conflict,
    heg_o,
    heg_d,
    common_language_primary,
    common_language_second,
    population_origin, 
    population_dest,
    gdp_origin,
    gdp_dest,
    gdp_percapita_origin,
    gdp_percapita_dest,
    religion_sim,
    eu_member_origin,
    eu_member_dest,
    wto_member_origin,
    wto_member_dest ) %>% 
  cor(use = "complete.obs")

par(mfrow=c(1,1), mar=c(4,4,2,2))
corrplot(correlation, method="number", type = 'full', diag=FALSE, col=c("black", "grey"),
         col.lab="black", col.main="black", tl.col="black", tl.srt=45,
         cex.main = 2, font.main = 1, main.pos = 4)
title("Matrice di correlazione variabili (gravity_data_clean2020)", xlab = "", ylab = "", line = 0, cex.main = 1.5)
# You can distinguish the sign of the correlation with the colour and 
# you can check the entity of the correlation by looking at the numbers displayed.

#GRAPHICAL ANALYSIS
# we graphically represent the main database variables that will allow us to construct our gravity equation.
# it is very important to compare the graphical representation of the normal variables and their logarithmic version in order to seek the best possible fit of the model.
# This is especially useful when the variable has a skewed distribution or when the data have extreme values or outliers. In addition,
# the logarithmic transformation can make the data easier to interpret, since logarithmic values are closer to the ordinal scale

#MAIN VARIABLES - PLOT
plot1 <- gravity_data_clean2020 %>%
  select(country_origin, gdp_origin) %>% 
  unique() %>% 
  ggplot() +
  aes(x = gdp_origin)  +
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.5, color = 'black', fill = 'red') + 
  theme_bw() +
  geom_density(size = 1, color = 'blue')+
  labs(
    x = "GDP",
    y = "Density",
    title = "Histogram of GDP in 2020"
  )
plot1


plot2 <- gravity_data_clean2020 %>%
  select(country_origin, gdp_origin) %>% 
  unique() %>% 
  ggplot() +
  aes(x = gdp_origin)  +
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.5, color = 'black', fill = 'red') + 
  theme_bw() +
  geom_density(size = 1, color = 'blue')+ scale_x_log10() + 
  labs(
    x = "Log-GDP",
    y = "Density",
    title = "Histogram of Log-GDP in 2020"
  )
plot2
# as we can observe in this case it is preferable to use the logarithmic version of GDP

# check outliers in GDP, comparing variable in levels (non-logs) and in logs
gravity_data_clean2020 %>% 
  ggplot() + 
  aes(x = gdp_origin) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'gdp_origin',
       title = 'gdp_origin')

gravity_data_clean2020 %>% 
  ggplot() + 
  aes(x = log(gdp_origin)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'Log-gdp_origin',
       title = 'Log-gdp_origin')

gravity_data_clean2020 = 
  gravity_data_clean2020 %>% 
  filter(gdp_origin>exp(-1.5))
         

plot3 =
  gravity_data_clean2020 %>%
  ggplot() +
  aes(x = bilateral_trade_repO) + 
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.5, color = 'black', fill = 'red') + 
  theme_bw()+
  labs(
    x = "Bilateral Trade",
    y = "Density",
    title = "Histogram of Bilateral trade in 2020"
  ) + geom_density(size = 1,  color = 'blue')
plot3


plot4 =
  gravity_data_clean2020 %>%
  ggplot() +
  aes(x = bilateral_trade_repO) + 
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.5, color = 'black', fill = 'red') + 
  theme_bw()+ scale_x_log10()+ 
  labs(
    x = "Log-Bilateral Trade",
    y = "Density",
    title = "Histogram of Log-Bilateral trade in 2020"
  ) + geom_density(size = 1,  color = 'blue')
plot4

# as we can observe in this case it is preferable to use the logarithmic version of bilateral trade
# check outliers, comparing variable in levels (non-logs) and in logs
gravity_data_clean2020 %>% 
  ggplot() + 
  aes(x = bilateral_trade_repO) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'gdp_origin',
       title = 'gdp_origin')

gravity_data_clean2020 %>% 
  ggplot() + 
  aes(x = log(bilateral_trade_repO)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'Log-gdp_origin')

gravity_data_clean2020 = 
  gravity_data_clean2020 %>% 
  filter(bilateral_trade_repO>exp(0),
         bilateral_trade_repO<exp(15))


plot5 <- gravity_data_clean2020 %>%
  ggplot() +
  aes(x = distance) + 
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.5, color = 'black', fill = 'red') + 
  theme_bw() +
  geom_density(size = 1, color= 'blue')+
  labs(
    x = "Distance",
    y = "Density",
    title = "Histogram of Distance in 2020"
  )
plot5

plot6 <- gravity_data_clean2020 %>%
  ggplot() +
  aes(x = distance) + 
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.5, color = 'black', fill = 'red') + 
  theme_bw() +
  geom_density(size = 1, color= 'blue')+ scale_x_log10() + 
  labs(
    x = "Log-distance",
    y = "Density",
    title = "Histogram of Log-Distance in 2020"
  )
plot6
# in this case the decision is not a foregone conclusion since the original variable already has a good fit
# check outliers, comparing variable in levels (non-logs) and in logs
gravity_data_clean2020 %>% 
  ggplot() + 
  aes(x = distance) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'distance',
       title = 'distance')

gravity_data_clean2020 %>% 
  ggplot() + 
  aes(x = log(distance)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = 'Log-distance')

gravity_data_clean2020 = 
  gravity_data_clean2020 %>% 
  filter(distance<18000)

#COMPARISON SUMMARY OF MAIN VARIABLES
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 2)
draw((textGrob("Comparison summary of key variables", gp = gpar(fontsize = 16, fontface = "bold"))), x = 0.5, y = 0.95)

#SECONDARY VARIABLES

plot7 <- gravity_data_clean2020 %>%
  select(country_origin, gdp_percapita_origin) %>% 
  unique() %>% 
  ggplot() +
  aes(x = gdp_percapita_origin)  +
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.5, color = 'darkblue', fill = 'lightblue') + 
  theme_bw() +
  geom_density(size = 1, color = 'black')+
  labs(
    x = "GDP",
    y = "Density",
    title = "Histogram of GDP per capita in 2020"
  )
plot7

plot8 <- gravity_data_clean2020 %>%
  select(country_origin, gdp_percapita_origin) %>% 
  unique() %>% 
  ggplot() +
  aes(x = gdp_percapita_origin)  +
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.5, color = 'darkblue', fill = 'lightblue') + 
  theme_bw() +
  geom_density(size = 1, color = 'black')+ scale_x_log10() + 
  labs(
    x = "GDP",
    y = "Density",
    title = "Histogram of log GDP per capita in 2020"
  )
plot8

plot9 <- gravity_data_clean2020 %>%
  ggplot() +
  aes(x = distcap) + 
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.5, color = 'darkblue', fill = 'lightblue') + 
  theme_bw() +
  geom_density(size = 1, color= 'black')+
  labs(
    x = "Distance",
    y = "Density",
    title = "Histogram of Distance from capital in 2020"
  )
plot9

plot10 <- gravity_data_clean2020 %>%
  ggplot() +
  aes(x = distcap) + 
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.5, color = 'darkblue', fill = 'lightblue') + 
  theme_bw() +
  geom_density(size = 1, color= 'black')+ scale_x_log10() + 
  labs(
    x = "Log-distance",
    y = "Density",
    title = "Histogram of Log-Distance from capital in 2020"
  )
plot10

plot11 <- gravity_data_clean2020 %>%
  unique() %>% 
  ggplot() +
  aes(x = religion_sim) + 
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.5, color = 'darkblue', fill = 'lightblue') + 
  theme_bw() +
  geom_density(size = 1, color= 'black') +
  labs(
    x = "Distance",
    y = "Density",
    title = "Histogram of religion similarity index in 2020"
  )
plot11

plot12 <- gravity_data_clean2020 %>%
  unique() %>% 
  ggplot() +
  aes(x = religion_sim) + 
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.5, color = 'darkblue', fill = 'lightblue') + 
  theme_bw() +
  geom_density(size = 1, color= 'black')+ scale_x_log10() + 
  labs(
    x = "Log-distance",
    y = "Density",
    title = "Histogram of Log-Religion similarity index in 2020"
  )
plot12

grid.arrange(plot7, plot8, plot9, plot10, plot11, plot12, ncol = 2)
draw((textGrob("Comparison summary of key variables", gp = gpar(fontsize = 16, fontface = "bold"))), x = 0.5, y = 0.95)


#RELATIONS BETWEEN MOST RELEVANT VARIABLES
scatterplot_gdp_orig_trade_logs = 
  gravity_data_clean2020 %>% 
  ggplot() +
  aes(x = log(gdp_origin), y = log(bilateral_trade_repO)) + 
  geom_point() + 
  labs(
    x = "Log-GDP of origin country",
    y = "Log-Bilateral trade",
    title = "Log-Log Relation between origin country GDP and bilateral trade in 2020"
  ) + theme_bw()
scatterplot_gdp_orig_trade_logs

scatterplot_gdp_dest_trade_logs = 
  gravity_data_clean2020 %>%
  ggplot() +
  aes(x = log(gdp_dest), y = log(bilateral_trade_repO)) + 
  geom_point() + 
  labs(
    x = "Log-GDP of destination country",
    y = "Log-Bilateral trade",
    title = "Log-Log Relation between destination country GDP and bilateral trade in 2020"
  ) + theme_bw()
scatterplot_gdp_dest_trade_logs

scatterplot_gdp_orig_trade_logs + geom_smooth(method = 'lm')
scatterplot_gdp_dest_trade_logs + geom_smooth(method = 'lm')
grid.arrange(scatterplot_gdp_orig_trade_logs + geom_smooth(method = 'lm'),
             scatterplot_gdp_dest_trade_logs + geom_smooth(method = 'lm'))


scatterplot_gdp_percapita_origin = 
  gravity_data_clean2020 %>% 
  ggplot() +
  aes(x = log(gdp_percapita_origin), y = log(bilateral_trade_repO)) + 
  geom_point() + 
  labs(
    x = "Log-GDP per capita of origin country",
    y = "Log-Bilateral trade",
    title = "Log-Log Relation between GDP per capita of origin country and bilateral trade in 2020"
  ) + theme_bw()
scatterplot_gdp_percapita_origin

scatterplot_gdp_percapita_dest = 
  gravity_data_clean2020 %>% 
  ggplot() +
  aes(x = log(gdp_percapita_dest), y = log(bilateral_trade_repO)) + 
  geom_point() + 
  labs(
    x = "Log-GDP per capita of destination country",
    y = "Log-Bilateral trade",
    title = "Log-Log Relation between GDP per capita of destination country and bilateral trade in 2020"
  ) + theme_bw()
scatterplot_gdp_percapita_dest

scatterplot_gdp_percapita_origin + geom_smooth(method = 'lm')
scatterplot_gdp_percapita_dest + geom_smooth(method = 'lm')
grid.arrange(scatterplot_gdp_percapita_origin + geom_smooth(method = 'lm'),
             scatterplot_gdp_percapita_dest + geom_smooth(method = 'lm'))



#
##
###
####
##### ECONOMETRICS
####
###
##
#  

gravity_data_clean2020 %>% str

# EVALUATE THE GRAVITY EQUATION USING REGRESSION

reg_1 = lm(bilateral_trade_repO ~ distance, data = gravity_data_clean2020)
reg_1
reg_2 = lm(bilateral_trade_repO ~ log(distance), data = gravity_data_clean2020)
reg_2
reg_3 = lm(log(bilateral_trade_repO) ~ distance, data = gravity_data_clean2020)
reg_3
reg_4 = lm(log(bilateral_trade_repO) ~ log(distance), data = gravity_data_clean2020)
reg_4

stargazer(reg_1, reg_2, reg_3, reg_4, type = 'text')

# We run several regressions and observe how a change in dependent variable due to change
# in the size of the dependent variable (log), heavily affects the results, in fact regression 1 and 2
# are not comparable to those of 3 and 4. in this case we choose reg 4 as the best empirical model
# as it has a higher (absolute) value of R2, a relatively low SD. in addition, the F test indicates that the model is significant 
# In this model we will therefore use a log-log approach

# MACROECONOMICS INDICATORS

reg_5 = lm(log(bilateral_trade_repO) ~ log(gdp_dest), data = gravity_data_clean2020)
reg_5
reg_6 = lm(log(bilateral_trade_repO) ~ log(gdp_origin), data = gravity_data_clean2020)
reg_6
reg_7 = lm(log(bilateral_trade_repO) ~ log(gdp_origin) + log(gdp_dest), data = gravity_data_clean2020)
reg_7

# check the first gravity equation
reg_gravity = lm(log(bilateral_trade_repO) ~ log(distance) + log(gdp_origin) + log(gdp_dest), data = gravity_data_clean2020)
reg_gravity

plot(reg_gravity)
plot(reg_gravity, which = 1)

coeftest(reg_gravity, vcv = vcovHC(fit1, type = "HC1"))
stargazer(reg_4, reg_5, reg_6, reg_7, reg_gravity, type = 'text')


# add the last 4 macroeconomics indicators
reg_8 = lm(log(bilateral_trade_repO) ~ log(population_origin), data = gravity_data_clean2020)
reg_8
reg_9 = lm(log(bilateral_trade_repO) ~ log(population_dest), data = gravity_data_clean2020)
reg_9
reg_8 = lm(log(bilateral_trade_repO) ~ log(gdp_percapita_origin), data = gravity_data_clean2020)
reg_8
reg_9 = lm(log(bilateral_trade_repO) ~ log(gdp_percapita_dest), data = gravity_data_clean2020)
reg_9

reg_gravity2 = lm(log(bilateral_trade_repO) ~ log(population_origin) + log(population_dest)
                  + log(gdp_percapita_origin) + log(gdp_percapita_dest), data = gravity_data_clean2020)
reg_gravity2
plot(reg_gravity2, which = 1)

coeftest(reg_gravity2, vcv = vcovHC(fit1, type = "HC1"))
stargazer(reg_gravity, reg_gravity2, type = 'text')
# With the following model we can observe that the macroeconomic variables considered are significant

modelsummary(
  list(reg_gravity,
       reg_gravity2),
  stars = T,
  title = "comparison of equations reg_gravity and reg_gravity2",)


# CULTURAL VARIABLES
gravity_data_clean2020 %>% str

reg_10 = lm(log(bilateral_trade_repO) ~ heg_o, data = gravity_data_clean2020)
reg_10
reg_11 = lm(log(bilateral_trade_repO) ~ heg_d, data = gravity_data_clean2020)
reg_11
reg_12 = lm(log(bilateral_trade_repO) ~ diplo_disagreement, data = gravity_data_clean2020)
reg_12
reg_13 = lm(log(bilateral_trade_repO) ~ common_colonizer, data = gravity_data_clean2020)
reg_13
reg_14 = lm(log(bilateral_trade_repO) ~ colonial_relationship, data = gravity_data_clean2020)
reg_14
reg_15 = lm(log(bilateral_trade_repO) ~ sib_conflict, data = gravity_data_clean2020)
reg_15

reg_gravity3 = lm(log(bilateral_trade_repO) ~ log(distance) + log(gdp_origin) + log(gdp_dest) + diplo_disagreement + common_colonizer
                  + colonial_relationship + sib_conflict, data = gravity_data_clean2020)
plot(reg_gravity3, which = 1)

coeftest(reg_gravity3, vcv = vcovHC(fit1, type = "HC1"))

stargazer(reg_gravity, reg_gravity3, type = 'text')
summary(reg_gravity3)

# analyzing the F test we notice how many variables are not significant for the model,
# so we create another model to analyze only those that satisfy the F test and check the goodness of the model.
reg_gravity4 = lm(log(bilateral_trade_repO) ~ log(distance) + log(gdp_origin) + log(gdp_dest) + diplo_disagreement + 
                    sib_conflict, data = gravity_data_clean2020)
plot(reg_gravity4, which = 1)
summary(reg_gravity4)

# we use the anova() function to compare the full model with the model that excludes the variable of interest
# and test whether removing the variable causes a significant reduction in model quality. If the F test is significant,
# then the variable is considered significant
anova(reg_gravity3, reg_gravity4)
stargazer(reg_gravity3, reg_gravity4, type = 'text')
modelsummary(
  list(reg_gravity3,
       reg_gravity4),
  stars = T,
  title = "comparison of equations reg_gravity3 and reg_gravity4",)


# TRADE FACILITATION VARIABLES
gravity_data_clean2020 %>% str

reg_16 = lm(log(bilateral_trade_repO) ~ gatt_o, data = gravity_data_clean2020)
reg_16
reg_17 = lm(log(bilateral_trade_repO) ~ gatt_d, data = gravity_data_clean2020)
reg_17
reg_18 = lm(log(bilateral_trade_repO) ~ eu_member_origin, data = gravity_data_clean2020)
reg_18
reg_19 = lm(log(bilateral_trade_repO) ~ eu_member_dest, data = gravity_data_clean2020)
reg_19
reg_20 = lm(log(bilateral_trade_repO) ~ wto_member_origin, data = gravity_data_clean2020)
reg_20
reg_21 = lm(log(bilateral_trade_repO) ~ wto_member_dest, data = gravity_data_clean2020)
reg_21

reg_gravity5 = lm(log(bilateral_trade_repO) ~ log(distance) + log(gdp_origin) + log(gdp_dest) + gatt_o + gatt_d + eu_member_origin + eu_member_dest
                  + wto_member_origin + wto_member_dest, data = gravity_data_clean2020)
plot(reg_gravity5, which = 1)

coeftest(reg_gravity5, vcv = vcovHC(fit1, type = "HC1"))

stargazer(reg_gravity,reg_gravity3, reg_gravity5, type = 'text')
summary(reg_gravity5)

reg_gravity6 = lm(log(bilateral_trade_repO) ~ log(distance) + log(gdp_origin) + log(gdp_dest) + gatt_o + eu_member_origin + eu_member_dest
                  + wto_member_origin + wto_member_dest, data = gravity_data_clean2020)
plot(reg_gravity6, which = 1)

stargazer(reg_gravity5, reg_gravity6, type = 'text')
anova(reg_gravity5, reg_gravity6)

modelsummary(
  list(reg_gravity5,
       reg_gravity6),
  stars = T,
  title = "comparison of equations reg_gravity5 and reg_gravity6",)

# in this case the variable that is less significant, it is not worth removing because it still positively affects the model.
# so let us consider the model reg_gravity5


#those models can assume more meaning if you add the variable represented by the dummies of both countries moltiplied for each other
#let's start with GATT


reg_22 = lm(log(bilateral_trade_repO) ~ gatt_o + gatt_d, data = gravity_data_clean2020)
reg_22
reg_23 = lm(log(bilateral_trade_repO) ~ gatt_o + gatt_d + gatt_o*gatt_d, data = gravity_data_clean2020)
reg_23
stargazer(reg_16, reg_17, reg_22, reg_23, type = 'text' )
anova(reg_22, reg_23)

#using the reg_23 can allow us to estimate the effect of having one country, the other, or both members of GATT
##since the anova tells us to keep it, we will
#we can do the same for EU

reg_24 = lm(log(bilateral_trade_repO) ~ eu_member_origin + eu_member_dest, data = gravity_data_clean2020)
reg_24
reg_25 = lm(log(bilateral_trade_repO) ~ eu_member_origin + eu_member_dest + eu_member_origin*eu_member_dest, data = gravity_data_clean2020)
reg_25
stargazer(reg_18, reg_19, reg_24, reg_25, type = 'text' )
anova(reg_24, reg_25)

#for the anova test the reg_25 is more significant
#let's now try for WTO members

reg_26 = lm(log(bilateral_trade_repO) ~ wto_member_origin + wto_member_dest, data = gravity_data_clean2020)
reg_26
reg_27 = lm(log(bilateral_trade_repO) ~ wto_member_origin + wto_member_dest + wto_member_origin*wto_member_dest, data = gravity_data_clean2020)
reg_27
stargazer(reg_20, reg_21, reg_26, reg_27, type = 'text' )
anova(reg_26, reg_27)

#then the same anova result happened

#DEEPER DISTANCE-RELATIONED VARIABLES ANALYSIS (CONTIGUITY GMT DISTCAP)

reg_28 = lm(log(bilateral_trade_repO) ~ contiguity, data = gravity_data_clean2020)
reg_28
reg_29 = lm(log(bilateral_trade_repO) ~ gmt_offset_2020_o, data = gravity_data_clean2020)
reg_29
reg_30 = lm(log(bilateral_trade_repO) ~ gmt_offset_2020_d, data = gravity_data_clean2020)
reg_30
reg_31 = lm(log(bilateral_trade_repO) ~ distcap, data = gravity_data_clean2020)
reg_31

reg_gravity7 = lm(log(bilateral_trade_repO) ~ log(distance) + log(gdp_origin) + log(gdp_dest) + contiguity + gmt_offset_2020_o + gmt_offset_2020_d + distcap, data = gravity_data_clean2020)
plot(reg_gravity7, which = 1)

coeftest(reg_gravity7, vcv = vcovHC(fit1, type = "HC1"))

stargazer(reg_28, reg_29, reg_30, reg_31, reg_gravity7, type = 'text')
summary(reg_gravity7)

#we check for same GMT using D1*D2 and compare result with previous regression
reg_gravity8 = lm(log(bilateral_trade_repO) ~ log(distance) + log(gdp_origin) + log(gdp_dest) + contiguity + gmt_offset_2020_o*gmt_offset_2020_d + distcap, data = gravity_data_clean2020)
stargazer(reg_gravity7,reg_gravity8, type = 'text')
anova(reg_gravity7,reg_gravity8)

modelsummary(
  list(reg_gravity7,
       reg_gravity8),
  stars = T,
  title = "comparison of equations reg_gravity7 and reg_gravity8",)

modelsummary(
  list(reg_28, reg_29, reg_30, reg_31, reg_gravity7),
  stars = T,
  title = "comparison of equations reg_28, reg_29, reg_30, reg_31, reg_gravity7",)

# the second model appears to be slightly better than the first model. 
# It has a slightly higher R-squared value, a slightly lower standard error, and a slightly lower but still relatively high F-statistic.

#CULTURAL DISTANCE (RELIGION LANGUAGES) 

reg_32 = lm(log(bilateral_trade_repO) ~ religion_sim, data = gravity_data_clean2020)
reg_32
reg_33 = lm(log(bilateral_trade_repO) ~ common_language_primary, data = gravity_data_clean2020)
reg_33
reg_34 = lm(log(bilateral_trade_repO) ~ common_language_second, data = gravity_data_clean2020)
reg_34

reg_gravity9 = lm(log(bilateral_trade_repO) ~ log(distance) + log(gdp_origin) + log(gdp_dest) + religion_sim + common_language_primary + common_language_second, data = gravity_data_clean2020)
plot(reg_gravity8, which = 1)

coeftest(reg_gravity9, vcv = vcovHC(fit1, type = "HC1"))
stargazer(reg_32, reg_33, reg_34, reg_gravity9, type = 'text')
summary(reg_gravity9)
#both languages in common

reg_gravity10 = lm(log(bilateral_trade_repO) ~ log(distance) + log(gdp_origin) + log(gdp_dest) + religion_sim + common_language_primary * common_language_second, data = gravity_data_clean2020)
stargazer(reg_gravity9,reg_gravity10, type = 'text')
anova(reg_gravity9,reg_gravity10)

modelsummary(
  list(reg_gravity9,
       reg_gravity10),
  stars = T,
  title = "comparison of equations reg_gravity9 and reg_gravity10",)


#UNION OF BEST MODELS
reg_gravity11 = lm(log(bilateral_trade_repO) ~ log(distance) + log(gdp_origin) + log(gdp_dest) + 
                     diplo_disagreement +
                     gatt_o + eu_member_dest+
                     contiguity + gmt_offset_2020_o + common_language_primary*common_language_second +
                     wto_member_dest  , data = gravity_data_clean2020)

reg_gravity12 = lm(log(bilateral_trade_repO) ~ log(distance) + log(gdp_origin) + log(gdp_dest) + 
                     diplo_disagreement + 
                     sib_conflict + gatt_o + gatt_d + gatt_o*gatt_d + eu_member_origin + eu_member_dest +
                     wto_member_origin + wto_member_dest +
                     contiguity + gmt_offset_2020_o + gmt_offset_2020_d + distcap+
                     religion_sim
                   , data = gravity_data_clean2020)

stargazer(reg_gravity11, reg_gravity12, type= 'text')

modelsummary(
  list(reg_gravity11),
  stars = T,
  title = "comparison of equations reg_gravity11 and reg_gravity12",)






# CONCLUSION - gravity equations 
modelsummary(
  list(reg_gravity, reg_gravity5, reg_gravity8, reg_gravity10, reg_gravity11), stars = T, 
  title = "comparison of original gravity equation (1), original gravity equation + trade facilitation variables (2),
  original gravity equation + deeper distance-related variables (3),original gravity equation + cultural distance variables (4),
  complex gravity equation (5)")

modelsummary(list(reg_gravity), stars = T, title = "original gravity equation")
modelsummary(list(reg_gravity5), stars = T, title = "original gravity equation + trade facilitation variables")
modelsummary(list(reg_gravity8), stars = T, title = "original gravity equation + deeper distance-related variables")
modelsummary(list(reg_gravity10), stars = T, title = "original gravity equation + cultural distance variables")
modelsummary(list(reg_gravity11), stars = T, title = "complex gravity equation (structured model)")


stargazer(reg_gravity, type = 'text')    # original gravity equation
stargazer(reg_gravity5, type = 'text')   # original gravity equation + trade facilitation variables
stargazer(reg_gravity8, type = 'text')   # original gravity equation + deeper distance-related variables
stargazer(reg_gravity10, type = 'text')  # original gravity equation + cultural distance variables
stargazer(reg_gravity11, type = 'text')  # complex gravity equation (structured model)

stargazer(reg_gravity, reg_gravity5, reg_gravity8, reg_gravity10, type = 'text')











































