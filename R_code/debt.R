library(ggplot2)
library(scales)
library(gdata)
library(stringr)

outdir = "../slides/%s"

outfile = function(f){
  ff = sprintf(outdir,f)
  return(ff)
}

dat = read.csv('../data/TreasuryDirectHistoricalDebt.csv')
dat$millions = dat$DEBT/1e6
head(dat)

attach(dat)


png(file = outfile("slide1.png"))
plot(x=YEAR,y=millions,type='l',col='red',ylab="Debt in Millions")
title("Total Debt from 1790 through Present")
dev.off()

scatter = ggplot(data=dat,aes(x=YEAR, y=millions)) +
  geom_line(color="red")+
  scale_y_continuous(labels = comma)+
  ggtitle("Total Debt from 1790 through Present") + 
  ylab("Debt in Millions")

png(file = outfile("slide2.png"))
print(scatter)
dev.off()

### read xlsx file - doesn't work too well
#df = read.xls("../data/hist01z1.xlsx", sheet=1, header=TRUE)
#head(df)

#---- total receipts, outlays, and surplus/deficits ----#

df1_1 = read.csv("../data/hist01z1.csv", header=TRUE)
str(df1_1)
summary(df1_1)
head(df1_1)

df1_2 = read.csv("../data/hist01z2.csv", header=TRUE) 
str(df1_2)
summary(df1_2)
head(df1_2)

## gdp in billions
gdp = df1_2[df1_2$Year>=1940,1:2]
head(gdp)

df1_3 = read.csv("../data/hist01z3.csv", header=TRUE) 
str(df1_3)
summary(df1_3)
head(df1_3)

#deflator values
df10_1 = read.csv("../data/hist10z1.csv", header=TRUE) 
str(df10_1)
summary(df10_1)
head(df10_1)

#check
sum(df10_1$GDP - gdp$GDP)

#totals are all in millions
totals = data.frame(Year=gdp[,1],
                    GDP=gdp[,2]*1000, 
                    GDP_deflator=df10_1$GDP_deflator, 
                    GDP_2009 = gdp[,2]*1000/df10_1$GDP_deflator,
                    df1_1[df1_1$Year>=1940,2:4],
                    df1_3[,5:7]*1000,
                    df1_3[,8:11])
str(totals)
head(totals)


### do returns of 2009 GDP and outlays, to compare grown in GDP with growth in spending?
n = nrow(totals)
#gdp_logreturn = c(0,round(diff(log(totals$GDP_2009))*100,1))
gdp_return = c(0,round(((totals$GDP_2009[-1] - totals$GDP_2009[-n])/totals$GDP_2009[-n])*100,1))

#outlays_logreturn = c(0,round(diff(log(totals$Outlays_2009))*100,1))
outlays_return = c(0,round(((totals$Outlays_2009[-1] - totals$Outlays_2009[-n])/totals$Outlays_2009[-n])*100,1))

totals = data.frame(totals,gdp_return,outlays_return)
str(totals)

#save it
write.csv(x=totals, file="../dataForSlides/receipts_outlays.csv", row.names=FALSE)


#---- receipts by category ----#


df2_1 = read.csv("../data/hist02z1.csv", header=TRUE) 
df2_1 = df2_1[df2_1$Year >= 1940,]
str(df2_1)
summary(df2_1)
head(df2_1)

df2_2 = read.csv("../data/hist02z2.csv", header=TRUE) 
df2_2 = df2_2[df2_2$Year >= 1940,]
str(df2_2)
summary(df2_2)
head(df2_2)

df2_3 = read.csv("../data/hist02z3.csv", header=TRUE) 
df2_3 = df2_3[df2_3$Year >= 1940,]
str(df2_3)
summary(df2_3)
head(df2_3)

receipts_by_category = data.frame(
  
  #these are in current (not inflation adjusted dollars)
  #total tax should match totals$Receipts
  df2_1,
  
  #these are percent of receipts, adds up to 100
  individual_pct_receipts = df2_2$Individual.Income.Tax,
  corproate_pct_receipts = df2_2$Corporate.Income.Tax,
  social_pct_receipts = df2_2$Social.Insurance.and.Retirement,
  excise_pct_receipts = df2_2$Excise.Taxes,
  other_pct_receipts = df2_2$Other,
  total_pct_receipts = df2_2$Total,
  
  #these are percent of GDP, total should match totals$receipt_pct
  individual_pct_gdp = df2_3$Individual.Income.Tax,
  corproate_pct_gdp = df2_3$Corporate.Income.Tax,
  social_pct_gdp = df2_3$Social.Insurance.and.Retirement,
  excise_pct_gdp = df2_3$Excise.Taxes,
  other_pct_gdp = df2_3$Other,
  total_pct_gdp = df2_3$Total
)

str(receipts_by_category)
head(receipts_by_category)
write.csv(x=receipts_by_category, file="../dataForSlides/receipts_by_category.csv", row.names=FALSE)


# ----  outlays by function ---- #


# note : offsetting receipts is income from from business-like activities of the government such as fees,
# medicare premiums, etc.

# this data set is 'wide' rather than 'long'
df3_1_w = read.csv("../data/hist03z1.csv", header=TRUE)
df31_names = as.character(df3_1_w[,1])
df31_names = c("Year",df31_names)
df3_1 = t(df3_1_w[,-1])

#add year column back in
years_c = row.names(df3_1)
#remove 'X'
years_c = str_sub(years_c,2)
years = strtoi(years_c)
df3_1 = data.frame(Years=years,df3_1)
colnames(df3_1) = df31_names
str(df3_1)
head(df3_1)

write.csv(x=df3_1, file="../dataForSlides/outlays_by_category_full.csv", row.names=FALSE)

#check that the 2 sets match
sum(df3_1$Total - totals$Outlays)

#make a data set as % of GDP
df3_1_pct_gdp = df3_1
df3_1_pct_gdp[,-1] = round(df3_1_pct_gdp[,-1]/totals$GDP*100,1)
str(df3_1_pct_gdp)
head(df3_1_pct_gdp)
write.csv(x=df3_1_pct_gdp, file="../dataForSlides/outlays_by_category_full_pct_gdp.csv", row.names=FALSE)

#make a data set as % of outlays
df3_1_pct_outlays = df3_1
df3_1_pct_outlays[,-1] = round(df3_1_pct_outlays[,-1]/totals$Outlays*100,1)
str(df3_1_pct_outlays)
head(df3_1_pct_outlays)
write.csv(x=df3_1_pct_outlays, file="../dataForSlides/outlays_by_category_full_pct_outlays.csv", row.names=FALSE)


#make 2 data sets one with just the categories summarized
df3_1_summary = df3_1[,c(1,2,3,10,16,17,23,24)]
str(df3_1_summary)
write.csv(x=df3_1_summary, file="../dataForSlides/outlays_by_category_summary.csv", row.names=FALSE)

#make a data set as % of GDP
df3_1_summary_pct_gdp = df3_1_summary
df3_1_summary_pct_gdp[,-1] = round(df3_1_summary_pct_gdp[,-1]/totals$GDP*100,1)
head(df3_1_summary_pct_gdp)
write.csv(x=df3_1_summary_pct_gdp, file="../dataForSlides/outlays_by_category_summary_pct_gdp.csv", row.names=FALSE)

#make a data set as % of outlays
df3_1_summary_pct_outlays = df3_1_summary
df3_1_summary_pct_outlays[,-1] = round(df3_1_summary_pct_outlays[,-1]/totals$Outlays*100,1)
head(df3_1_summary_pct_outlays)
write.csv(x=df3_1_summary_pct_outlays, file="../dataForSlides/outlays_by_category_summary_pct_outlays.csv", row.names=FALSE)


## data set with details
df3_1_details = df3_1[,c(1,2,4,5,6,7,8,9,11,12,13,14,15,16,18,19,20,21,22,23,24)]
str(df3_1_details)
write.csv(x=df3_1_details, file="../dataForSlides/outlays_by_category_details.csv", row.names=FALSE)

#make a data set as % of GDP
df3_1_details_pct_gdp = df3_1_details
df3_1_details_pct_gdp[,-1] = round(df3_1_details_pct_gdp[,-1]/totals$GDP*100,1)
head(df3_1_details_pct_gdp)
write.csv(x=df3_1_details_pct_gdp, file="../dataForSlides/outlays_by_category_details_pct_gdp.csv", row.names=FALSE)

#make a data set as % of outlays
df3_1_details_pct_outlays = df3_1_details
df3_1_details_pct_outlays[,-1] = round(df3_1_details_pct_outlays[,-1]/totals$Outlays*100,1)
head(df3_1_details_pct_outlays)
write.csv(x=df3_1_details_pct_outlays, file="../dataForSlides/outlays_by_category_details_pct_outlays.csv", row.names=FALSE)


#---- outlays by department, current dollars  ----#


#- uses table hist04z1.xls

df4_1_w = read.csv("../data/hist04z1.csv", header=TRUE)
df41_names = as.character(df4_1_w[,1])
df41_names = c("Year",df41_names)
df4_1 = t(df4_1_w[,-1])

#add year column back in
years_c = row.names(df4_1)
#remove 'X'
years_c = str_sub(years_c,2)
years = strtoi(years_c)
df4_1 = data.frame(Years=years,df4_1)
colnames(df4_1) = df41_names
str(df4_1)
head(df4_1)

write.csv(x=df4_1, file="../dataForSlides/outlays_by_dept.csv", row.names=FALSE)

#### outlays by department, % GDP - uses table hist04z1.xls
## instead calculate it from numbers we already have

#gdp is in billions, df4_2 is in millions
#only have data 1962-2015
g = gdp[gdp$Year>=1962,2]*1000

#exclude year
df4_1_gdp = df4_1[,-1]

#don't need loop
#for( i in 1:nrow(df4_1_gdp)) {
#  df4_1_gdp[i,] = round(df4_1_gdp[i,]/g[i]*100,1)
#}

#this works
df4_1_gdp = round(df4_1_gdp/g*100,1)
head(df4_1_gdp)

write.csv(x=df4_1_gdp, file="../dataForSlides/outlays_by_dept_pct_gdp.csv", row.names=FALSE)

#### outlays by department, % outlays - uses table hist04z2.xls

df4_2_w = read.csv("../data/hist04z2.csv", header=TRUE)
df42_names = as.character(df4_2_w[,1])
df42_names = c("Year",df42_names)
df4_2 = t(df4_2_w[,-1])

#add year column back in
years_c = row.names(df4_2)
#remove 'X'
years_c = str_sub(years_c,2)
years = strtoi(years_c)
df4_2 = data.frame(Years=years,df4_2)
colnames(df4_2) = df42_names
str(df4_2)
head(df4_2)

write.csv(x=df4_2, file="../dataForSlides/outlays_by_dept_pct_outlays.csv", row.names=FALSE)


#---- gross debt values ----#


df7_1 = read.csv("../data/hist07z1.csv", header=TRUE)

df71_names=c(
  "Year",
  "Gross Federal Debt",
  "Government Accounts",
  "Total Public",
  "Federal Reserve System",
  "Other",
  "Gross Federal Debt %GDP",
  "Government Accounts %GDP",
  "Total Public %GDP",
  "Federal Reserve System %GDP",
  "Other %GDP"
)
names(df7_1) = df71_names
str(df7_1)
summary(df7_1)
head(df7_1)

write.csv(x=df7_1, file="../dataForSlides/gross_debt.csv", row.names=FALSE)

#---- outlays by manditory/discretionary ----#

df8_3 = read.csv("../data/hist08z3.csv", header=TRUE)

df83_names=c(
  "Year",
  "Total",
  "Total Discretionary",
  "Defense",
  "Non-Defense",
  "Total Manditory and Net Interest",
  "Total Manditory",
  "Total Programmatic",
  "Social Security",
  "Deposit Insurance",
  "Means Tested Entitlements",
  "Other",
  "Undistributed Offsetting Receipts",
  "Net Interest"
  )

names(df8_3) = df83_names
str(df8_3)
summary(df8_3)
head(df8_3)

write.csv(x=df8_3, file="../dataForSlides/discretionary_manditory_outlays_pct_outlays.csv", row.names=FALSE)


#---- marginal tax rate ----#
#from https://commons.wikimedia.org/wiki/File:Historical_Marginal_Tax_Rate_for_Highest_and_Lowest_Income_Earners.jpg

marginal_tax = read.csv("../dataForSlides/marginal_tax.csv",header=TRUE)
str(marginal_tax)
head(marginal_tax)
tail(marginal_tax)


# make a data set for slide 9: bar chart of debt as %GDP by decade. use df7_1
year=c()
debt=c()
agg_debt = 0
n = 0

for( i in seq_along(df7_1$Year)) {

  y = df7_1$Year[i]
  d = df7_1$`Gross Federal Debt %GDP`[i]

  agg_debt = agg_debt + d
  n = n+1
  
  if((y %% 10 == 0) && n > 1 ) {
    print(y)
    print(agg_debt/n)

    #use midpoint
    year = c(year,y-5)
    debt = c(debt,(agg_debt/n))
    
    n = 0
    agg_debt = 0
    
  }
}

#final
year = c(year,y)
debt = c(debt,(agg_debt/n))

debt_by_decade = data.frame(year,round(debt,0))
names(debt_by_decade) = c("Year","Average Debt by % GDP for Decade")
write.csv(x=debt_by_decade, file="../dataForSlides/debt_by_decade.csv", row.names=FALSE)


gdp_current_return = round(diff(totals$GDP)/(totals$GDP[-n])*100,1)
debt_current_return = round(diff(df7_1$`Gross Federal Debt`)/df7_1$`Gross Federal Debt`[-n]*100,1)
growth_gdp_debt = data.frame(gdp=gdp_current_return,debt=debt_current_return)
write.csv(x=growth_gdp_debt, file="../dataForSlides/growth_gdp_debt.csv", row.names=FALSE)



