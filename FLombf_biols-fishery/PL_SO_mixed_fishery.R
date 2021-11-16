#this code now contains data from plaice and sole in FLStock format. 
#It was used to create a mixed FLFishery (one gear) with data for both species, 
#and selectivity and prices. Selectivity was computed for each species from the 
#datasets. Prices were used from stecf data. 

rm(list=ls());gc()

install.packages(c("ggplot2"))
install.packages(c("FLa4a","FLash","FLXSA","FLBRP","ggplotFL"), repos="http://flr-project.org/R")
install.packages("FLasher", repos="http://flr-project.org/R")

# Loads all necessary packages
library(FLa4a)
library(FLash)
library(FLXSA)
library(FLBRP)
library(ggplotFL)
library(FLasher)

#data below is retrieved from local hard drive. 
load("C:/Users/marc-/Documents/WUR/Thesis/sol.27.4_FLStock.RData")
SO <- run
load("C:/Users/marc-/Documents/WUR/Thesis/ple.27.420_assessment_result_2021base_run.RData")
PL <- ass.stockOrigDisc
rm(run, ass.stockOrigDisc) #to keep the script and the environment tidy

# Creat an FLIndex object using an existing FLQuant object for PL & SO
# Create a perfect index of abundance from abundance at age
PL.idx <- FLIndex(index=stock.n(PL))
SO.idx <- FLIndex(index=stock.n(SO))
# Add some noise around the signal
index(PL.idx) <- index(PL.idx)*exp(rnorm(1, index(PL.idx)
                                                 -index(PL.idx), 0.1))
index(SO.idx) <- index(SO.idx)*exp(rnorm(1, index(SO.idx)
                                                 -index(SO.idx), 0.1))

it <- 20    # iterations
y0 <- range(PL)["minyear"] # initial data year #same for both PL&SO
dy <- range(PL)["maxyear"] # final data year
iy <- dy+1  # initial year of projection (also intermediate year)
ny <- 12    # number of years to project from intial year
fy <- dy+ny # final year
nsqy <- 3   # number of years to compute status quo metrics

# Set up the catchability submodel with a smoothing spline
# (setting up a 'list' allows for more than one index)
qmod <- list(~s(age, k=6))
# Set up the fishing mortality submodel as a tensor spline,
# which allows age and year to interact
fmod <- ~te(replace(age, age>9,9), year, k=c(6,8))
# Set up the MCMC parameters
mcsave <- 100
mcmc <- it * mcsave

#setup startf & endf ("the period of the year, expressed as proportions of 
#a year, that corresponds to the index (numeric).)" that are needed for sca fit 
#below. These slots were empty in the initial datasets. 
#Defined startf and endf  at 0.5 and 0.6, so around the middle of the year, 
#if that's appropriate (try to find a justification for this later). 
#For ple4.index it was 0.6453376 & 0.6453376. 
PL.idx@range[["startf"]] <- 0.5
PL.idx@range[["endf"]] <- 0.6
SO.idx@range[["startf"]] <- 0.5
SO.idx@range[["endf"]] <- 0.6

# Fit the model
fit_PL <- sca(PL, PL.idx, fmodel = fmod, qmodel = qmod, fit = "MCMC", 
           mcmc = SCAMCMC(mcmc = mcmc, mcsave = mcsave, mcprobe = 0.4)) 
fit_SO <- sca(SO, SO.idx, fmodel = fmod, qmodel = qmod, fit = "MCMC", 
                  mcmc = SCAMCMC(mcmc = mcmc, mcsave = mcsave, mcprobe = 0.4))

# Update the FLStock object
PL <- PL + fit_PL
SO <- SO + fit_SO
# Reduce to keep one iteration only for reference points
PL_0 <- qapply(PL, iterMedians)
SO_0 <- qapply(SO, iterMedians)

# Fit the stock-recruit model for both species
sr_bh_PL <- fmle(as.FLSR(PL, model="bevholt"), method="L-BFGS-B", 
             lower=c(1e-6, 1e-6), upper=c(max(rec(PL)) * 3, Inf))
sr_bh_PL_0 <- fmle(as.FLSR(PL_0, model="bevholt"), method="L-BFGS-B", 
              lower=c(1e-6, 1e-6), upper=c(max(rec(PL_0)) * 3, Inf))

sr_bh_SO <- fmle(as.FLSR(SO, model="bevholt"), method="L-BFGS-B", 
                 lower=c(1e-6, 1e-6), upper=c(max(rec(SO)) * 3, Inf))
sr_bh_SO_0 <- fmle(as.FLSR(SO_0, model="bevholt"), method="L-BFGS-B", 
                   lower=c(1e-6, 1e-6), upper=c(max(rec(SO_0)) * 3, Inf))

# Generate stock-recruit residuals for the projection period
sr_bh_PL_res <- rnorm(it, FLQuant(0, dimnames=list(year=iy:fy)), 
                  mean(c(apply(residuals(sr_bh_PL), 6, sd))))
sr_bh_SO_res <- rnorm(it, FLQuant(0, dimnames=list(year=iy:fy)), 
                      mean(c(apply(residuals(sr_bh_SO), 6, sd))))


# Calculate the reference points
brp_PL <- brp(FLBRP(PL_0, sr_bh_PL_0))
Fmsy_PL <- c(refpts(brp_PL)["msy","harvest"])
msy_PL <- c(refpts(brp_PL)["msy","yield"])
Bmsy_PL <- c(refpts(brp_PL)["msy","ssb"])
Bpa_PL <- 0.5*Bmsy_PL #assumption so check with literature later
Blim_PL <- Bpa_PL/1.4 #assumption
# Prepare the FLStock object for projections
PL <- stf(PL, fy-dy, nsqy, nsqy)

# Calculate the reference points
brp_SO <- brp(FLBRP(SO_0, sr_bh_SO_0))
Fmsy_SO <- c(refpts(brp_SO)["msy","harvest"])
msy_SO <- c(refpts(brp_SO)["msy","yield"])
Bmsy_SO <- c(refpts(brp_SO)["msy","ssb"])
Bpa_SO <- 0.5*Bmsy_SO #assumption so check with literature later
Blim_SO <- Bpa_SO/1.4 #assumption
# Prepare the FLStock object for projections
SO <- stf(SO, fy-dy, nsqy, nsqy)


#CREATE FISHERY
PL_catch <- as(PL, "FLCatch") #as() forces the FLstock PL to be an FLcatch which
                              #is needed for FLFishery to also for example include
                              #prices
                              #this step also computes the selectivity of fisheries.
units(price(PL_catch)) <- "EUR / kg"

#here the values/kg after 2008 are replaced with stecf data.
# PL_catch@price@.Data <- replace(PL_catch@price@.Data, c(521:530, 531:540, 541:550, 
#                                                         551:560, 561:570, 571:580, 
#                                                         581:590, 591:600, 601:610,
#                                                         611:620, 621:630,631:640), 
#                                                       c(1.303, 1.248, 1.337, 1.364, 
#                                                         1.258, 1.208, 1.454, 1.6021, 
#                                                         1.803, 2.426, 2.379, 2.379))
#for some reason this code above with c() didnt work. So I did it 12 times separately

PL_catch@price@.Data <- replace(PL_catch@price@.Data, 1:520, 1.77)#stecf data has value/kg data from 2008-2020. So before
#2008 the 2008 price is used (1.77), after that the data below.
PL_catch@price@.Data <- replace(PL_catch@price@.Data, 521:530, 1.303)
PL_catch@price@.Data <- replace(PL_catch@price@.Data, 531:540, 1.248)
PL_catch@price@.Data <- replace(PL_catch@price@.Data, 541:550, 1.337)
PL_catch@price@.Data <- replace(PL_catch@price@.Data, 551:560, 1.364)
PL_catch@price@.Data <- replace(PL_catch@price@.Data, 561:570, 1.258)
PL_catch@price@.Data <- replace(PL_catch@price@.Data, 571:580, 1.208)
PL_catch@price@.Data <- replace(PL_catch@price@.Data, 581:590, 1.454)
PL_catch@price@.Data <- replace(PL_catch@price@.Data, 591:600, 1.6021)
PL_catch@price@.Data <- replace(PL_catch@price@.Data, 601:610, 1.803)
PL_catch@price@.Data <- replace(PL_catch@price@.Data, 611:620, 2.426)
PL_catch@price@.Data <- replace(PL_catch@price@.Data, 621:630, 2.379)
PL_catch@price@.Data <- replace(PL_catch@price@.Data, 631:640, 2.379)

SO_catch <- as(SO, "FLCatch")
units(price(SO_catch)) <- "EUR / kg"
#again stecf data
SO_catch@price@.Data <- replace(SO_catch@price@.Data, 1:520, 9.46)
SO_catch@price@.Data <- replace(SO_catch@price@.Data, 521:530, 9.666)
SO_catch@price@.Data <- replace(SO_catch@price@.Data, 531:540, 10.94)
SO_catch@price@.Data <- replace(SO_catch@price@.Data, 541:550, 10.73)
SO_catch@price@.Data <- replace(SO_catch@price@.Data, 551:560, 9.285)
SO_catch@price@.Data <- replace(SO_catch@price@.Data, 561:570, 8.353)
SO_catch@price@.Data <- replace(SO_catch@price@.Data, 571:580, 9.28)
SO_catch@price@.Data <- replace(SO_catch@price@.Data, 581:590, 10.27)
SO_catch@price@.Data <- replace(SO_catch@price@.Data, 591:600, 10.424)
SO_catch@price@.Data <- replace(SO_catch@price@.Data, 601:610, 10.46)
SO_catch@price@.Data <- replace(SO_catch@price@.Data, 611:620, 11.15)
SO_catch@price@.Data <- replace(SO_catch@price@.Data, 621:630, 11.33)
SO_catch@price@.Data <- replace(SO_catch@price@.Data, 631:640, 11.33)

#below I tried to make the price/kg vary for ages. I used Batsleer (2015) for this.
#they showed how the prices per 5 age classes differ between 2003 and 2007, so
#I used those variations.

#below method seems a dead end...
#SO_prices <- SO_catch@price@.Data #isolate the prices

#matrix_prices <- apply(SO_prices, 2, c) #make a matrix from the prices

#I couldn't figure out how to manipulate the prices per age within R, so I exported
#the prices to an excel file to be able to manipulate them. There, the stecf prices
#were multiplied with the prices per age adapted from Batsleer et al. (2015)
#write.csv(matrix_prices, "C:\\Users\\marc-\\Documents\\WUR\\Thesis\\SO_prices.csv")

#needed for the read_excel function
#install.packages("dplyr")
#library(dplyr)

#read in the data
#SO_prices_age_year <- read_excel("C:\\Users\\marc-\\Documents\\WUR\\Thesis\\SO_prices_age_year.xlsx")

#below gives an error. I think because the way the prices are formatted in the
#excel file are not supported by FLCatch
#SO_catch@price@.Data <- SO_prices_age_year
#so this method seems like a dead end

#I also tried many other methods (select, subset, [], [[]], "") but nothing worked


#so I'm gonna do it very cumbersome
#these are the 'corrections' that I computed adapted from batsleer 2015. 
#there, prices/kg per age class were given between 2003-2007, I compared them
#with the price from stecf between 2003 and 2007 (9.46) to calculate a 'correction'
#for the different ages, that can later be applied to the entire price dataset.
SO_12_corr <- 7.5/9.46
SO_34_corr <- 10/9.46
SO_56_corr <- 13/9.46
SO_78_corr <- 15/9.46
SO_910_corr <- 16/9.46

list_ages_1 <- seq(1, 631, by = 10) #in the price array, all prices have an index
#so number 1 is age 1 in year 1957, number 2 is age 2 in 1957, number 10 is age 10
#in 1957, and number 11 is age 1 in 1958. So every index ending with a 1 is age 1
#in the next year. If we therefore make this sequence, it will contain all ages 1 indexes. 

SO_catch@price@.Data <- replace(SO_catch@price@.Data, list_ages_1, 
                                SO_catch@price@.Data[list_ages_1] * SO_12_corr) #multiplies all ages 1 with the correction factor

list_ages_2 <- seq(2, 632, by = 10)
SO_catch@price@.Data <- replace(SO_catch@price@.Data, list_ages_2, 
                                SO_catch@price@.Data[list_ages_2] * SO_12_corr)

list_ages_34 <- c(seq(3, 633, by = 10), seq(4, 634, by = 10))#since Batsleer distuingished 5 age classes and my dataset distuingishes 10 age classes, I now took ages 1 and 2 together but I could also interpolate between them (so that e.g. correction age 2 is between age 1 and 3 instead of 1=2, 3=4)
SO_catch@price@.Data <- replace(SO_catch@price@.Data, list_ages_34, 
                                SO_catch@price@.Data[list_ages_34] * SO_34_corr)

list_ages_56 <- c(seq(5, 635, by = 10), seq(6, 636, by = 10))
SO_catch@price@.Data <- replace(SO_catch@price@.Data, list_ages_56, 
                                SO_catch@price@.Data[list_ages_56] * SO_56_corr)

list_ages_78 <- c(seq(7, 637, by = 10), seq(8, 638, by = 10))
SO_catch@price@.Data <- replace(SO_catch@price@.Data, list_ages_78, 
                                SO_catch@price@.Data[list_ages_78] * SO_78_corr)

list_ages_910 <- c(seq(9, 639, by = 10), seq(10, 640, by = 10))
SO_catch@price@.Data <- replace(SO_catch@price@.Data, list_ages_910, 
                                SO_catch@price@.Data[list_ages_910] * SO_910_corr)

#now the prices are varying in age (taken from Bartleer, 2015) and vary in year
#from the year 2008, taken from stecf data

#below the same method for PL
#2.5-0 are estimated from graph from batsleer. 1.77 is from stecf
PL_910_corr <- 2.5/1.77 
PL_78_corr <- 2.1/1.77 
PL_56_corr <- 1.95/1.77 
PL_34_corr <- 1.8/1.77 
PL_12_corr <- 0/1.77 

PL_catch@price@.Data <- replace(PL_catch@price@.Data, list_ages_1, 
                                PL_catch@price@.Data[list_ages_1] * PL_12_corr)

PL_catch@price@.Data <- replace(PL_catch@price@.Data, list_ages_2, 
                                PL_catch@price@.Data[list_ages_2] * PL_12_corr)

PL_catch@price@.Data <- replace(PL_catch@price@.Data, list_ages_34, 
                                PL_catch@price@.Data[list_ages_34] * PL_34_corr)

PL_catch@price@.Data <- replace(PL_catch@price@.Data, list_ages_56, 
                                PL_catch@price@.Data[list_ages_56] * PL_56_corr)

PL_catch@price@.Data <- replace(PL_catch@price@.Data, list_ages_78, 
                                PL_catch@price@.Data[list_ages_78] * PL_78_corr)

PL_catch@price@.Data <- replace(PL_catch@price@.Data, list_ages_910, 
                                PL_catch@price@.Data[list_ages_910] * PL_910_corr)


#now the FLFishery contains the prices for plaice and sole per age per year
flf_PL_SO <- FLFishery(PL = PL_catch, SO = SO_catch)
rm(PL_catch, SO_catch) #(probably) not needed anymore because they're in the
                       #flf now so delete to keep environment tidy

#some plotting of data within the flffisheries dataset
xyplot(data~year|age, flf_PL_SO[["PL"]]@price, xlab="year", ylab="???") #, type="b", cex=0.5, pch=19)

xyplot(data~year|age, flf_PL_SO[["SO"]]@price, xlab="year", ylab="???", xlim=2000:2020) #, type="b", cex=0.5, pch=19)

xyplot(data~year|age, flf_PL_SO[["PL"]]@landings.n, xlab="year", ylab="landing numbers") #, type="b", cex=0.5, pch=19)



