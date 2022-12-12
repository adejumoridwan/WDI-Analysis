# Importing Data 
library(tidyverse)
life_exp <- read_csv("~/WDI-Analysis/LE_DATA.csv") |> 
  rename(
    Country = `Country Name`,
    Code = `Country Code`
  ) 

#Data Description
skimr::skim(life_exp)

#Replace missing values, remove predictor variables with missing values more than half of the observations 
life_exp <- life_exp |> 
  select(-c(SI.POV.LMIC, EG.FEC.RNEW.ZS, SE.PRM.CUAT.ZS,
                      SE.TER.CUAT.BA.ZS, SE.ADT.LITR.ZS, SE.COM.DURS,SH.HIV.INCD.14,FR.INR.RINR)) |> 
  mutate_if(is.numeric, 
            function(x) ifelse(is.na(x), median(x, na.rm = T), x))

#Filter only numeric variables
life_exp_num <- life_exp |> 
  select(-c(1,2,3))

#Descriptive Statistics
summary(life_exp)

#---------------------------
#Exploratory Data Analysis"
#-----------------------------

# Distribution of Life Expectancy
life_exp |> 
  ggplot(aes(x = SP.DYN.LE00.IN)) +
  geom_density() +
  xlab("Life Expectancy") +
  ylab("Density") +
  ggtitle("Distribution of Life Expectancy across various countries")

#Average Life Expectancy by Continent
life_exp |> 
  group_by(Continent) |> 
  summarize(Avg_life_exp = mean(SP.DYN.LE00.IN)) |> 
  arrange(desc(Avg_life_exp))

# Life Expectancy by continent
life_exp |> 
  ggplot(aes(x = Continent,
             y = SP.DYN.LE00.IN, 
             color = Continent)) +
  geom_boxplot() +
  geom_point() +
  xlab("Continent") +
  ylab("Average Life Expectancy") +
  ggtitle("Median Life Expectancy By Continents")

# Top 10 countries with the highest life expectancy
life_exp |> 
  arrange(desc(SP.DYN.LE00.IN))|> 
  head(10) |> 
  ggplot(aes(y = reorder(Country, SP.DYN.LE00.IN),x = SP.DYN.LE00.IN, fill = Country)) +
  geom_col() +
  xlab("Average Life Expectancy") +
  ylab("Country") +
  ggtitle("Top 10 Countries with the Highest Life Expectancy")

# Top 10 countries with the lowest life expectancy
life_exp |> 
  arrange(SP.DYN.LE00.IN)|> 
  head(10) |> 
  ggplot(aes(y = reorder(Country, SP.DYN.LE00.IN),x = SP.DYN.LE00.IN, fill = Country)) +
  geom_col() +
  xlab("Life Expectancy") +
  ylab("Country") +
  ggtitle("Top 10 Countries with the Lowest Life Expectancy")


# Calculate correlation matrix
cor_matrix <- cor(life_exp_num)
cor_matrix |> View()

# Check For Multicollinearity and drop predictors with
#VIF less greater than 10
library(car)
vif_model = lm(SP.DYN.LE00.IN ~ ., life_exp_num)

for(i in 1:length(vif(vif_model))){
  if (vif(vif_model)[[i]] <= 10){
    print(vif(vif_model)[i])
  }
}

# Fit a regression model with predictor variables
reg_model <- lm(SP.DYN.LE00.IN ~ EG.ELC.ACCS.ZS + SE.PRM.UNER + 
       SP.DYN.IMRT.IN + SE.PRM.CMPT.ZS + EN.POP.DNST + 
       SP.POP.TOTL + SH.XPD.CHEX.GD.ZS + SH.XPD.CHEX.PC.CD + 
       SL.UEM.TOTL.NE.ZS + NY.GDP.MKTP.KD.ZG + NY.GDP.PCAP.CD + 
       SH.HIV.INCD + SH.H2O.SMDW.ZS + SP.DYN.CBRT.IN + SP.POP.GROW, 
       life_exp_num)
summary(reg_model)

#Analysis of Variance
anova <- aov(SP.DYN.LE00.IN ~ Continent, data = life_exp)
summary(anova)


# Conduct a Tukey HSD post-hoc test
library(stats) # load the stats package
TukeyHSD(anova)
