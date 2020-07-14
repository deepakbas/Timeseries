#Load all the required libraries
library(tsibble)
library(xlsx)
#library(tidyquant)
library(sweep)
library(timetk)
library(forecast)
library(tidyverse)
# library("tsibble")
library(feasts)
library(tsibbledata)
library(fpp2)
library(fpp3)
library(dbplyr)
#tsibble

#read csv and convert into tibble using readr::read_csv function
df <- readr::read_csv("file_name.csv",col_types = "ccdd")
#Create new variables, exclude variables, convert tibble into tsibble with key and index identification
df <- df %>%
  mutate(mon = yearmonth(Date)) %>%
  select(-Date) %>%
  as_tsibble(key = Unit, index = mon)
df

#Load the inbuilt data from tsibble
#autoplot to visualize the measurement variables individually keywise w.r.t index
?aus_retail

#Filter tsibble data
df <- aus_retail %>%
  filter(Industry == "Clothing retailing") %>%
  filter(year(Month) >= 2000)%>%
  select(Month, State, Turnover)

df %>%
  autoplot(Turnover) + ggtitle("Monthly turnover of Clothing retailing of various states")

#seasonal plot, seasonal subseries
#visualize seasonal effects of measurements upon time components, such as time of day and day of week, month of the year etc. 

df %>% gg_season(Turnover, labels = "both") +
  ylab("Turnover$ Millions") +
  ggtitle("Seasonal plot: Monthly turnover of Clothing retailing of various states")

#seasonal subseries
df %>%
  gg_subseries(Turnover) +
  ylab("Turnover$ Millions") +
  xlab("Mon") +
  ggtitle("Seasonal subseries plot: Monthly turnover of Clothing retailing of various states")


#Trellis chart
df %>%
  ggplot(aes(x = Month, y = Turnover)) +
  geom_line() +
  facet_grid(vars(State), scales = "free_y") +
  ylab("Monthly turnover of Clothing retailing")

# scatterplot matrix: Correlation between different units/keys
df %>%
  spread(State, Turnover) %>%
  GGally::ggpairs(columns = 2:9)

#filter function to remove units/keys or subset data
remove <- c("Queensland", "Tasmania", "Victoria")
df %>% 
  filter(!State %in% remove)%>%
  gg_season(Turnover)

df %>% 
  filter(!State %in% remove)%>% 
  gg_subseries(Turnover)

#lagplot to get acf
df %>% filter(State=="Tasmania") %>% gg_lag(Turnover, geom="point")
df %>% filter(State=="Tasmania") %>% gg_tsdisplay(Turnover)

#Keywise ACF PACF with user defined lags
acf_Turnover_statewise = df %>% ACF(Turnover, lag_max = 10)
pacf_Turnover_statewise = df %>% ACF(Turnover, lag_max = 5, type="partial")

df %>% ACF(Turnover) %>% autoplot()
#ACF on the 1st differencing data, seems to be decreasing
df %>% ACF(difference(Turnover, 1)) %>% autoplot()
#filtered ACF
df %>% 
  filter(!State %in% remove)%>% 
  ACF(Turnover) %>% autoplot()

#Perform STL decomposition, residual diagnostics
library("tsibbledata")
library("fpp3")

#filter a State, autoplot
turnover_onestate <- df %>%
  filter(State == "Northern Territory") %>%
  select(-State)
turnover_onestate %>%
  autoplot(Turnover) +
  xlab("Month") + ylab("Turnover") +
  ggtitle("Turnover_onestate")

#STL on filtered state
dcmp <- turnover_onestate %>%
  model(STL(Turnover ~ season(window = Inf)))
cmp = components(dcmp)
components(dcmp) %>% autoplot()

#trend plot
turnover_onestate %>%
  autoplot(Turnover, color='gray') +
  autolayer(components(dcmp), trend, color='red') +
  xlab("Month") + ylab("Turnover") +
  ggtitle("turnover_onestate_trend")

#Seasonally adj plot
turnover_onestate %>%
  autoplot(Turnover, color='gray') +
  autolayer(components(dcmp), season_adjust, color='blue') +
  xlab("Year") + ylab("Turnover ($M)") +
  ggtitle("turnover_onestate_season_adj")

#Multiunit STL plot with different trend season window configuration
# control how rapidly the trend-cycle and seasonal components can change. 
# Smaller values allow for more rapid changes
# Setting seasonal window to be infinite is equivalent to forcing the seasonal component to be periodic (identical across years).
df %>%
  model(STL(Turnover ~ trend(window = 10))) %>%
  components()%>%
  autoplot()

dcmp1 <- df %>%
  model(STL(Turnover ~ season(window = Inf)))
cmp1 = components(dcmp1)
components(dcmp1) %>% autoplot()

checkresiduals(cmp1$remainder, 12,plot = TRUE)

df %>%
  model(STL(Turnover ~ season(window = "periodic"))) %>%
  components()%>%
  autoplot()

dcmp2 <- df %>%
  model(STL(Turnover ~ trend(window=21) + season(window = 13), robust = TRUE))
cmp2 = components(dcmp2)
components(dcmp2) %>% autoplot()

#check if error values are uncorrelated
Box.test (cmp2$remainder, lag = 3, type = "Ljung")
Box.test (cmp2$remainder, lag = 5, type = "Ljung")

#Moving average smoothing to estimate trend
#Moving Avg for the filtered unit with sliding window option
MA_one_state <- df %>%
  filter(State == "Queensland") %>%
  mutate(
    `5-MA` = slide_dbl(Turnover, mean, .size = 5, .align = "center")
  )

#Moving Avg all units
MA_all_state <-df %>%
  group_by(State) %>% 
  mutate(
    `5-MA` = slide_dbl(Turnover, mean, .size = 5, .align = "center")
  )

#Plot of MA series
MA_one_state %>%
  autoplot(Turnover) +
  autolayer(MA_one_state, `5-MA`, color='red') +
  xlab("Mon") + ylab("Turnover") +
  ggtitle("Turnover") +
  guides(colour=guide_legend(title="series"))

MA_all_state %>%
  autoplot(Turnover) +
  autolayer(MA_all_state, `5-MA`, color='red') +
  facet_grid(vars(State), scales = "free_y") + 
  xlab("Mon") + ylab("Turnover") +
  ggtitle("Turnover") +
  guides(colour=guide_legend(title="series"))

#Moving averages of moving averages
Turnover_ma_ma <- df %>%
  filter(State == "Queensland") %>%
  select(Month, Turnover)
Turnover_2ma <- Turnover_ma_ma %>%
  mutate(
    `4-MA` = slide_dbl(Turnover, mean, .size = 4, .align = "center-left"),
    `2x4-MA` = slide_dbl(`4-MA`, mean, .size = 2, .align = "center-right")
  )

#MA of MA all units
Turnover_all_ma_ma <- df %>%
  group_by(State) %>%
  mutate(
    `4-MA` = slide_dbl(Turnover, mean, .size = 4, .align = "center-left"),
    `2x4-MA` = slide_dbl(`4-MA`, mean, .size = 2, .align = "center-right")
  )

#2x12-MA
Turnover_ma_ma <- df %>%
  group_by(State) %>%
  select(Month, Turnover)
Turnover_2ma <- Turnover_ma_ma %>%
  mutate(
    `12-MA` = slide_dbl(Turnover, mean, .size = 12, .align = "center-left"),
    `2x12-MA` = slide_dbl(`12-MA`, mean, .size = 2, .align = "center-right")
  )

Turnover_2ma %>%
  autoplot(Turnover, color='gray') +
  autolayer(Turnover_2ma, vars(`2x12-MA`), color='red') +
  facet_grid(vars(State), scales = "free_y") +
  xlab("Mon") + ylab("Turnover") +
  ggtitle("2x12-MA Turnover")

#X11 decomposition require minimum 4 yrs. of data
x11_dcmp <- df %>%
  model(x11 = feasts:::X11(Turnover, type = "additive")) %>%
  components()
autoplot(x11_dcmp) + xlab("Year") +
  ggtitle("Additive X11 decomposition of Turnover")

x11_dcmp %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Turnover, colour = "Data")) +
  geom_line(aes(y = season_adjust, colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  xlab("Year") + ylab("Turnover$M") +
  ggtitle("Turnover-Statewise") +
  scale_colour_manual(values=c("gray","blue","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend"))

#shift in Dec month
x11_dcmp %>%
  gg_subseries(seasonal)

#SEATS
seats_dcmp <- df %>%
  model(seats = feasts:::SEATS(Turnover)) %>%
  components()
autoplot(seats_dcmp) + xlab("Year") +
  ggtitle("SEATS decomposition of Turnover")

###
#https://otexts.com/fpp3/some-simple-statistics.html
#Feature extraction and statistics
df %>% features(Turnover, mean)
#sorted
df %>% features(Turnover, list(mean=mean)) %>% arrange(mean)
#summary statistics
df %>% features(Turnover, quantile, prob=seq(0,1,by=0.25))
#ACF features
df %>% features(Turnover, feat_acf)

#STL features
df %>%
  features(Turnover, feat_stl)

#bivariate plot visualizing the strength of the trend/seasonality
df %>%
  features(Turnover, feat_stl) %>%
  ggplot(aes(x=trend_strength, y=seasonal_strength_year)) +
  geom_point() + facet_wrap(vars(State))

df %>%
  features(Turnover, feat_stl) %>%
  ggplot(aes(x=trend_strength, y=seasonal_strength_year, col=State)) +
  geom_point()

#Plot the series with high seasonality
df %>%
  features(Turnover, feat_stl) %>%
  filter(seasonal_strength_year == max(seasonal_strength_year)) %>%
  left_join(df, by = "State") %>%
  ggplot(aes(x = Month, y = Turnover)) + geom_line() +
  facet_grid(vars(State))

#feasts_stl
feasts_Turnover = df %>% features(Turnover, feature_set(tags="stl"))

#feasts_all
feasts_Turnover_all <- df %>%
  features(Turnover, feature_set(pkgs="feasts"))
feasts_Turnover_all

feasts_Turnover_all$State = as.factor(feasts_Turnover_all$State)

#pcomp
library(broom)

#Get Principal components of the features
pcs <- feasts_Turnover_all %>% select(-State) %>%
  prcomp(scale=FALSE) %>% augment(feasts_Turnover_all)

#Plot Principal components .fittedPC1 .fittedPC2 to check for outliers
pcs %>% ggplot(aes(x=.fittedPC1, y=.fittedPC2, col=State)) +
  geom_point() + theme(aspect.ratio=1)

#identifying outier series
outliers <- pcs %>%
  filter(.fittedPC1 > 1000) %>%
  select(State, .fittedPC1, .fittedPC2)
outliers

df$State = as.factor(df$State)
#outliers$state = as.numeric(outliers$State)
#plotting outlier series based on different thresholds of Principal components
outliers %>%
  left_join(df, by = "State") %>%
  mutate(Series = glue::glue("{State}", .sep = "\n\n")) %>%
  ggplot(aes(x = Month, y = Turnover)) +
  geom_line() +
  facet_grid(Series ~ ., scales='free') +
  ggtitle("Outlying time series in PC space")

pcs %>%
  filter(.fittedPC1 == max(.fittedPC1)) %>%
  left_join(df, by = "State") %>%
  ggplot(aes(x = Month, y = Turnover)) +
  geom_line() +
  facet_grid(vars(State)) +
  ggtitle("Outlying time series in PC space") +
  theme(legend.position = "none")

#outlying series based on combination of Principal components
pcs %>%
  filter(.fittedPC1 > 1000) %>%
  left_join(df, by = "State") %>%
  ggplot(aes(x = Month, y = Turnover)) +
  geom_line() +
  facet_grid(vars(State)) +
  ggtitle("Outlying time series in PC space") +
  theme(legend.position = "none")

##
#Time series forecasting methods

#Linear Trend Model
df$State = as.factor(df$State)
lmfit <- df %>%
  model(trend_model = TSLM(Turnover ~ trend()))
lmfit
lmfore <- lmfit %>% forecast(h = "3 years")
?forecast

lmfit %>% forecast(h = "1 year")%>%
  filter(State=="Victoria") %>%
  autoplot(df) +
  ggtitle("Turnover_forecast") + ylab("Turnover")

#simple forecasting methods
#average
library(dplyr)
df %>% model(avgmd = MEAN(Turnover))

train <- df %>% filter_index("2000 Jan" ~ "2017 Dec")
# Fit the models
df_fit <- train %>%
  model(
    Mean = MEAN(Turnover),
    `Naïve` = NAIVE(Turnover),
    `Seasonal naïve` = SNAIVE(Turnover)
  )

df_fit <- df %>% 
  filter(State %in% c("Victoria", "New South Wales", "Queensland")) %>% 
  model(snaive = SNAIVE(Turnover),
  ets = ETS(log(Turnover) ~ error("A") + trend("A") + season("A")))

#Correlation Analysis
#Long to wide format for calculating correlation among states
# Rolling correlation between X and Y to know change in correlation over time
# changes in correlation can signal events
# To decide whether to trust the relationship for forecasting
# detect shift in trend as and when timeseries becomes more/less correlated

df_pivot = dcast(df, Month ~ State, value.var="Turnover", fun.aggregate=mean)
GGally::ggpairs((df_pivot[,2:9]))

library(tidyquant)  # Loads tidyverse, tidyquant, financial pkgs, xts/zoo
library(cranlogs)   # For inspecting package downloads over time
library(corrr)      # Tidy correlation tables and correlation plotting
library(cowplot)    # Multiple plots with plot_grid()
library(tidyverse)
library("bitops")

#static Correlation between states
rownames(df_pivot) <- df_pivot$Month
df_pivot$Month <- NULL
x <- df_pivot %>%
  correlate() %>%   
  rearrange() %>%  
  shave()
corr_turnover = fashion(x)
rplot(x)

#network_plot between states to check for outlying behaviour
df_pivot %>% 
  correlate() %>% 
  network_plot(min_cor = .2,colours = c(palette_light()[[2]], "white", palette_light()[[4]]), legend = TRUE) +
  labs(
    title = "Correlations",
    subtitle = "Check the outlier state"
  ) +
  expand_limits(x = c(-0.75, 0.25), y = c(-0.4, 0.4)) +
  theme_tq() +
  theme(legend.position = "bottom")

#Tsibble with multiple measurement variables
us_change
#correlation between different predictors
us_change %>%
  GGally::ggpairs(columns = 2:6)

#static correlation between measurement variables of a single unit
us_change_df = as.data.frame(us_change)
rownames(us_change_df) <- us_change_df$Quarter
us_change_df$Quarter <- NULL
x <- us_change_df %>%
  correlate() %>%
  rearrange() %>%
  shave()
corr_stat_us_change = fashion(x)
rplot(x)

#static correlation between measurement variables of multiple keys/units
#Load gefcom2017data data from github
devtools::install_github("camroach87/gefcom2017data")
library(gefcom2017data)
gefcom2017 <- gefcom %>% 
  ungroup() %>%
  as_tsibble(key=zone, index=ts)
gefcom2017

#filter data 2016 Mar onward
gefcom2017 <- gefcom2017 %>%
  filter(yearmonth(date) >= yearmonth("2016 Mar"))

#static correlation between demand and dewpnt-zone wise
require(plyr)
func <- function(df)
{
  return(data.frame(COR = cor(df$demand, df$dewpnt)))
}
#copy the tsibble as it gets converted into dataframe
gefcom2017_df <- gefcom2017
# corr_stat_demand_tem = ddply(gefcom2017_df, .(zone), func)
corr_stat_demand_dew = ddply(gefcom2017_df, .(zone), func)

# Get rolling correlations demand and dewpnt-zone wise
#convert tsibble to tibble as runcor works with tibble format
gefcom2017_df_tb = as.tibble(gefcom2017)%>%
  group_by(zone)
  
corr_roll_demand_dew <- gefcom2017_df_tb %>%
  select(ts, zone, demand, dewpnt) %>%
  tq_mutate_xy(
    x          = dewpnt,
    y          = demand,
    mutate_fun = runCor, 
    n          = 6,
    use        = "pairwise.complete.obs",
    col_rename = "rol_corr"
  )

write.csv(corr_roll_demand_dew, "corr_roll_demand_dew.csv")

#Join static correlations with rolling correlations for insights
corr_roll_demand_dew = data.frame(corr_roll_demand_dew)
corr_roll_demand_dew = merge(corr_roll_demand_dew, corr_stat_demand_dew, by.x="zone", by.y="zone")
names(corr_roll_demand_dew)[6] <- "static_corr"
corr_roll_demand_dew = as.tibble(corr_roll_demand_dew)
corr_roll_demand_dew <- corr_roll_demand_dew %>% group_by(zone)
