setwd("C:/Users/Joe/Documents/Work/Coronavirus")

require(ggmap)
require(gganimate)
require(gifski)
require(transformr)
require(ggplot2)
require(sf)
require(rnaturalearth)
require(colorspace)

#require(coronavirus)
#data(coronavirus)
# Need to run update_dataset() to get the latest data, and the restart Rate
#update_dataset()
# Move to a smaller data.frame name
#CoV = coronavirus

# Alternatively, read it in from the github, if doing this, need to change the date to date class
CoV <- read.csv("https://raw.githubusercontent.com/RamiKrispin/coronavirus/master/csv/coronavirus.csv",stringsAsFactors = FALSE)
CoV$date <- as.Date(CoV$date)

# Convert type into a factor
CoV$type = factor(CoV$type)
CoV$country = factor(CoV$country)
CoV$province = factor(CoV$province)

# Include a new column for each country / province, being cumulative cases for confirmed, recovered, death
CoV$cumCases = NA
for(iC in levels(CoV$country)){
	for(iP in levels(CoV$province)){
		for(iT in 1:3){
			
			# Extract the appropriate data.frame
			cutDF = CoV[CoV$country == iC & CoV$province == iP & CoV$type == c("confirmed","death","recovered")[iT],]
			
			# If there's no data, then go next
			if(nrow(cutDF) == 0) next
			
			# Otherwise, calculate the cumulative cases
			cumCases = cumsum(cutDF$cases)
			
			CoV$cumCases[CoV$country == iC & CoV$province == iP & CoV$type == c("confirmed","death","recovered")[iT]] = cumCases
			
		}
	}
}

# Loop over countries, and make sure they have an empty province for all types. If not, create one from all provinces
for(iC in levels(CoV$country)){
	for(iT in levels(CoV$type)){
		if(nrow(subset(CoV,country==iC & type==iT & province==""))==0){
			# Get all of the provinces that are in this country
			CoV.cut = subset(CoV,country==iC & type == iT)
			# Make a copy of CoV.cut, but with only the necessary contents
			tempDF = data.frame(date=as.Date(unique(CoV.cut$date)))
			tempDF$province = ""
			tempDF$country = iC
			tempDF$lat = NA
			tempDF$long = NA
			tempDF$type = iT
			tempDF$cases = aggregate(CoV.cut$cases,by=list(Date = CoV.cut$date),FUN=sum)$x
			tempDF$cumCases = aggregate(CoV.cut$cumCases,by=list(Date = CoV.cut$date),FUN=sum)$x
			names(tempDF) = names(CoV.cut)
			CoV = rbind(CoV,tempDF)
		}
}}

# Plot the daily number of confirmed cases in countries
ggplot(subset(CoV,province=="" & type == "death"),aes(x=date,y=cases)) + 
	geom_line(aes(colour=country)) +
	guides(colour=FALSE)

# Select a specific country: United Kingdom for UK, US for USA
iCountry = "China"
ggplot(subset(CoV,country == iCountry & province=="" & type == "confirmed"),aes(x=date,y=cases)) + 
	geom_line(aes(colour=country)) +
	guides(colour=FALSE)

# Loop over each country
par(ask=TRUE)
for(iCountry in levels(CoV$country)){
	ggp = ggplot(subset(CoV,country == iCountry & province=="" & type == "death"),aes(x=date,y=cases)) + 
		geom_line(aes(colour=country)) +
		ggtitle(iCountry) +
		guides(colour=FALSE)
	print(ggp)
};par(ask=FALSE)

## ****** Prepare map shapefile

# Read in country shape file
sf<-ne_countries(scale="medium",returnclass="sf")

# Set the theme for ggplot
theme_set(theme_bw())
# Plot the shape files
ggplot(data = sf) + geom_sf() + xlab("Latitude") + ylab("Longitude")

# Change some of the names of the coronavirus dataset to agree with those names in sf
levels(CoV$country)[levels(CoV$country)=="US"] = "United States"
levels(CoV$country)[levels(CoV$country)=="Taiwan*"] = "Taiwan"
levels(CoV$country)[levels(CoV$country)=="Russia"] = "Russian Federation"
levels(CoV$country)[levels(CoV$country)=="Congo (Brazzaville)"] = "Republic of Congo"
levels(CoV$country)[levels(CoV$country)=="Congo (Kinshasa)"] = "Democratic Republic of the Congo"
levels(CoV$country)[levels(CoV$country)=="Laos"] = "Lao PDR"
levels(CoV$country)[levels(CoV$country)=="Sao Tome and Principe"] = "São Tomé and Principe"
levels(CoV$country)[levels(CoV$country)=="North Macedonia"] = "Macedonia"
levels(CoV$country)[levels(CoV$country)=="Korea, South"] = "Dem. Rep. Korea"
levels(CoV$country)[levels(CoV$country)=="Holy See"] = "Vatican"
levels(CoV$country)[levels(CoV$country)=="Gambia"] = "The Gambia"
levels(CoV$country)[levels(CoV$country)=="Eswatini"] = "Swaziland"
levels(CoV$country)[levels(CoV$country)=="Czechia"] = "Czech Republic"
levels(CoV$country)[levels(CoV$country)=="Cote d'Ivoire"] = "Côte d'Ivoire"
levels(CoV$country)[levels(CoV$country)=="Burma"] = "Myanmar"
levels(CoV$country)[levels(CoV$country)=="Brunei"] = "Brunei Darussalam"

# Find the match between countries in sf and countries in CoV
sfinCoV = CoV$country %in% sf$name_long
CoVinsf = sf$name_long %in% CoV$country
# Print those countries that are in sf but not in CoV
print(unique(CoV$country[!sfinCoV]))
# Print those countries that are in CoV but not in sf
print(sf$name_long[!CoVinsf])

# **************** Reformat data into a simple features data.frame

## Need to reformat the data so that it is a long data.frame for use in gganimate
## A sf is simply a data.frame with one special column that is called geom or geometry
## The geom column is a list column, containing the geometry for that row
## Want to have a data.frame with "Country", "Date", the geometry, and relevant metrics

# Only choose those provinces that have an empty province
CoV.cut = subset(CoV,province=="")
# And only choose countries that are in the sf
CoV.cut = subset(CoV.cut,country %in% sf$name_long)

## Create a long data.frame (CoV.sf) with a single set of sf for each date, including the relevant metrics
# Start the date at the earliest date
iDate = min(CoV.cut$date)
# Get all the country names in the countries shape file
sf.Countries = as.data.frame(sf)$name_long
# Loop over every date from the start of CoV data, to the end of CoV
while(iDate != max(CoV.cut$date)){
	print(iDate)
	# Create a temporary data.frame to store this date
	sf2 = sf
	# Add a column for the date
	sf2 = cbind(sf2,Date = rep(iDate,nrow(sf2)))
	# Add a column for the number of cases
	sf2 = cbind(sf2,NewCases = rep(NA,nrow(sf2)))
	sf2 = cbind(sf2,CumCases = rep(NA,nrow(sf2)))
	sf2 = cbind(sf2,NewDeaths = rep(NA,nrow(sf2)))
	sf2 = cbind(sf2,CumDeaths = rep(NA,nrow(sf2)))
	# Get the CoV data for this date
	CoV_Day = CoV.cut[CoV.cut$date == iDate,]
	# Get the countries for this day
	CoV_Day.ID = as.character(CoV_Day$country)
	# Cut the confirmed cases out
	confirmedCases = CoV_Day[CoV_Day$type == "confirmed",]
	# Cut the dead cases out
	deadCases = CoV_Day[CoV_Day$type == "death",]
	# Store the confirmed new cases
	sf2$NewCases[match(confirmedCases$country,sf.Countries,NA)] = confirmedCases$cases
	# Update the cumulative number of cases
	sf2$CumCases[match(confirmedCases$country,sf.Countries,NA)] = confirmedCases$cumCases
	# Store the confirmed deaths
	sf2$NewDeaths[match(confirmedCases$country,sf.Countries,NA)] = deadCases$cases
	# Update the cumulative deaths
	sf2$CumDeaths[match(confirmedCases$country,sf.Countries,NA)] = deadCases$cumCases
	# Bind onto the data.frame
	if(iDate == min(CoV.cut$date)){
		CoV.sf = sf2
	} else {
		CoV.sf = rbind(CoV.sf,sf2)
	}
	iDate = iDate + 1
}

# **************** Using linear regression to calculate r (pretty rubbish!)

# Formula to calculate rate of change for a given variable ("NewCases","CumCases","NewDeaths","CumDeaths") using loess
calculate_loess<-function(variable,span,PLOT=FALSE){

	if(!(variable %in% c("NewCases","CumCases","NewDeaths","CumDeaths"))) stop(paste(variable,"not known."))
	
	# Create a vector
	loessVector = rep(NA,nrow(CoV.sf))
	
	# Loop over each area
	for(iAC in 1:length(unique(CoV.sf$name_long))){
		
		countryName = unique(CoV.sf$name_long)[iAC]
		
		# Pull out the requested variable
		y.data = CoV.sf[[variable]][CoV.sf$name_long == countryName]
		# Remove negative data (can't be log-transformed)
		y.data[y.data < 0] = NA
		
		if(all(is.na(y.data))) next
		
		# Get dates from inside the CoV.sf
		Dates = CoV.sf$Date[CoV.sf$name_long == countryName]		
		
		# Fit the loess model
		loessFit = loess(y~x,data=data.frame(y=y.data,x=as.numeric(Dates)),span=span)
		
		# Predict the observation at time t
		y.sim = predict(loessFit,data.frame(x=as.numeric(Dates)))
		
		# Calculate the growth rate between day i+1 and i
		y.r = y.sim[-1] - y.sim[-length(y.sim)]
		
		# Plot the stuff
		if(PLOT){
			plot(y.data~Dates,type="l",xlab="Date",ylab=variable)
			lines(predict(loessFit)~Dates)
		}
		
		#if(length(c(NA,y.r)) != length(which(CoV.sf$name_long == countryName))) print(iAC)
		
		# Store the r inside the vector
		loessVector[which(CoV.sf$name_long == countryName)] = c(NA,y.r)
		
	}
	
	# Return the vector
	return(loessVector)
	
}

# Try and estimate a r for each area, rolling average over x days -- this doesn't work very well. In fact, it's terrible.
CoV.sf$loess.NewCases = calculate_loess("NewCases",0.2)
CoV.sf$loess.CumCases = calculate_loess("CumCases",0.2)
CoV.sf$loess.NewDeaths = calculate_loess("NewDeaths",0.2)
CoV.sf$loess.CumDeaths = calculate_loess("CumDeaths",0.2)

# Want to make these into factors if necessary
boop = floor(log10(max(abs(range(CoV.sf$loess.NewCases,na.rm=TRUE))))+1)
breaks = c(-10^seq(boop,0),10^seq(0,boop))
CoV.sf$f.loess.NC = cut(CoV.sf$loess.NewCases,breaks=breaks)
boop = floor(log10(max(abs(range(CoV.sf$loess.NewDeaths,na.rm=TRUE))))+1)
breaks = c(-10^seq(boop,0),10^seq(0,boop))
CoV.sf$f.loess.ND = cut(CoV.sf$loess.NewDeaths,breaks=seq(-boop,boop,length.out=10))

# Select the growth rate for a specific country: United Kingdom for UK, US for USA
iCountry = "Afghanistan"
ggplot(subset(CoV.sf,name_long == iCountry),aes(x=Date,y=loess.NewCases)) + 
	geom_line(aes(colour=name_long)) +
	guides(colour=FALSE)

# **************** Plotting maps

# Plot a single day's cumulative cases on the map
day = as.Date("2020-03-22",format="%Y-%m-%d")
ggplot(data = subset(CoV.sf,Date==day)) + 
	geom_sf(aes(fill = CumCases)) +
	scale_fill_gradient(low="white",high="red",trans="log",na.value="gray95")

# Plot a single day's cumulative cases on the map
day = as.Date("2020-03-22",format="%Y-%m-%d")
ggplot(data = subset(CoV.sf,Date==day)) + 
	geom_sf(aes(fill = CumCases)) +
	scale_fill_gradient(low="white",high="red",trans="log",na.value="gray95")

# Plot the growth rate of deaths at a given time
day = as.Date("2020-03-22",format="%Y-%m-%d")
ggplot(data = subset(CoV.sf,Date==day)) + 
	geom_sf(aes(fill = loess.NewDeaths)) +
	scale_fill_gradient(low="green",high="red",na.value="gray95")

# Plot the growth rate of deaths at a given time, but using a factor
day = as.Date("2020-03-22",format="%Y-%m-%d")
nCols = ceiling(length(levels(CoV.sf$f.loess.ND))/2)
cP = c(sequential_hcl(nCols,"Reds 3"),rev(sequential_hcl(nCols,"Blues 3"))[-1])
names(cP) = levels(CoV.sf$f.loess.ND)
ggplot(data = subset(CoV.sf,Date==day)) + 
	geom_sf(aes(fill = f.loess.ND)) +
	scale_fill_manual(values=cP,na.value="gray")

# **************** Create an animation

# Cut a few dates just to play with
CoV.sf.2 = CoV.sf[CoV.sf$Date > as.Date("21-07-2020",format="%d-%m-%Y"),]

# Or create an animation

# 1. Cumulative cases
anim = ggplot(data = CoV.sf.2) + 
		geom_sf(aes(fill = CumCases)) + 
		scale_fill_gradient(low="white",high="red",trans="log",na.value="gray95") +
		transition_time(Date) +
		ggtitle('Date: {frame_time}',subtitle="Cumulative cases of Covid-19") +
		theme(plot.title=element_text(size=22))
anim
anim2 = animate(anim,duration = 30,end_pause = 2,height=800,width=1200)
anim_save("CoVCumCases.gif",anim2)

# 2. Daily cases
anim3 = ggplot(data = CoV.sf) + 
		geom_sf(aes(fill = NewCases)) + 
		scale_fill_gradient(low="white",high="red",trans="log",na.value="gray") +
		transition_time(Date) +
		ggtitle('Date: {frame_time}',subtitle="Daily cases of Covid-19") +
		theme(plot.title=element_text(size=22))
anim3
anim4 = animate(anim3,duration = 30,end_pause = 2,height=800,width=1200)
anim_save("CoVDailyCases.gif",anim4)

# 3. Cumulative deaths
anim5 = ggplot(data = CoV.sf) + 
		geom_sf(aes(fill = CumDeaths)) + 
		scale_fill_gradient(low="white",high="red",trans="log",na.value="gray") +
		transition_time(Date) +
		ggtitle('Date: {frame_time}',subtitle="Cumulative deaths from Covid-19") +
		theme(plot.title=element_text(size=22))
anim5
anim6 = animate(anim,duration = 30,end_pause = 2,height=800,width=1200)
anim_save("CoVCumDeaths.gif",anim6)

# 4. Daily deaths
anim7 = ggplot(data = CoV.sf.2) + 
		geom_sf(aes(fill = NewDeaths)) + 
		scale_fill_gradient(low="white",high="red",trans="log",na.value="gray") +
		transition_time(Date) +
		ggtitle('Date: {frame_time}',subtitle="Daily deaths from Covid-19") +
		theme(plot.title=element_text(size=22))
anim7
anim8 = animate(anim3,duration = 30,end_pause = 2,height=800,width=1200)
anim_save("CoVDailyDeaths.gif",anim8)

# 6. Loess growth rate of new cases
nCols = ceiling(length(levels(CoV.sf$f.loess.NC))/2)
cP = c(sequential_hcl(nCols,"Blues 3"),rev(sequential_hcl(nCols,"Reds 3"))[-1])
names(cP) = levels(CoV.sf$f.loess.NC)
anim11 = ggplot(data = CoV.sf) +
		geom_sf(aes(fill=f.loess.NC,group=interaction(f.loess.NC,name_long))) +
		scale_fill_manual(values=cP,na.value="gray") +
		transition_time(Date) +
		ggtitle('Date: {frame_time}',subtitle="Change in new cases (loess smoothed)") +
		theme(plot.title=element_text(size=22))
anim11
anim12 = animate(anim11,duration = 60,end_pause = 2,height=800,width=1200)
anim_save("CoVloessNewCases.gif",anim12)

# 7. Loess growth rate of new deaths
nCols = ceiling(length(levels(CoV.sf$f.loess.ND))/2)
cP = c(sequential_hcl(nCols,"Blues 3"),rev(sequential_hcl(nCols,"Reds 3"))[-1])
names(cP) = levels(CoV.sf$f.loess.ND)
anim13 = ggplot(data = CoV.sf) +
		geom_sf(aes(fill=f.loess.ND,group=interaction(f.loess.ND,name_long))) +
		scale_fill_manual(values=cP,na.value="gray") +
		transition_time(Date) +
		ggtitle('Date: {frame_time}',subtitle="Change in new deaths (loess smoothed)") +
		theme(plot.title=element_text(size=22))
anim13
anim14 = animate(anim11,duration = 30,end_pause = 2,height=800,width=1200)
anim_save("CoVloessNewDeaths.gif",anim14)

