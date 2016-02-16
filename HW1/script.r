# Homework 1 Gonzalo Diaz
# For each question I have created a csv file containing the answer, 
# please find them attached.

library(rjson)

# My local path
path<-"C:\\Users\\GONZALO\\json\\"

# I get all the files in the directory
jsonFiles<-list.files(path)

#------------------------------------------------------------------------------
# Part 1 - Distinct cities

# Function that returns the city of a certain data
getLocation<-function(data){
	location <- data$HotelInfo$Address
	if(!is.null(location)){
		search<-"v:locality\">"
		initialSplit<-strsplit(location, search)
		finalSplit<-gsub("<..*","",initialSplit[[1]][2])
	}else{
		finalSplit<-"NA"
	}
	return (finalSplit) 
}

counter<-1
array<-rep(1,length(jsonFiles))
for(i in 1:length(jsonFiles)){
	filePath <- paste(path,jsonFiles[i],sep="")
	raw_data <- fromJSON(file=filePath)
	location <- getLocation(raw_data)
	if(!identical(location,"NA")){
		array[counter]<-location
		counter<-counter+1
	}
}

# Getting rid of the ones used to create the array
finalArray1<-rep(1,counter-1)
j<-1
for(i in 1:length(array)){
	if(!identical(array[i],"1")){
		finalArray1[j]<-array[i]
		j<-j+1
	}
}

finalArray1<-unique(finalArray1)
write.csv(finalArray1, file = "AllCities.csv")

#------------------------------------------------------------------------------
# Part 2 - Distinct American cities

# Function that returns the city if has not country information (then it is American)
getUSALocation<-function(data){
	location<- data$HotelInfo$Address
	if(!is.null(location)){
		searchState<-"v:country-name\">"
		stateSplit<-strsplit(location, searchState)
		if(is.na(stateSplit[[1]][2])){
			search<-"v:locality\">"
			initialSplit<-strsplit(location, search)
			finalSplit<-gsub("<..*","",initialSplit[[1]][2])
		}else{
			finalSplit<-"NA"	
		}
	}else{
		finalSplit<-"NA"
	}
	return (finalSplit)
}


counter<-1
arrayUSA<-rep(1,length(jsonFiles))
for(i in 1:length(jsonFiles)){
	filePath <- paste(path,jsonFiles[i],sep="")
	raw_data <- fromJSON(file=filePath)
	location <- getUSALocation(raw_data)
	if(!identical(location,"NA")){
		arrayUSA[counter]<-location
		counter<-counter+1
	}
}

# Getting rid of the ones used to create the array
finalArray2<-rep(1,counter-1)
j<-1
for(i in 1:length(arrayUSA)){
	if(!identical(arrayUSA[i],"1")){
		finalArray2[j]<-arrayUSA[i]
		j<-j+1
	}
}


finalArray2<-unique(finalArray2)
write.csv(finalArray2, file = "AmericanCities.csv")

#------------------------------------------------------------------------------
# Part 3 - Cities with more than 30 hotels

a<-table(array)
array30H<-rep(1,length(a))
names<-rownames(a)
counter<-1
for(i in 1:length(a)){
	if(a[[i]]>30){
		if(names[i]!="1"){
			array30H[counter]<-names[i]
			counter<-counter+1
		}
	}
}

# Getting rid of the ones used to create the array
finalArray3<-rep(1,counter-1)
j<-1
for(i in 1:length(array30H)){
	if(array30H[i]!="1"){
		finalArray3[j]<-array30H[i]
		j<-j+1
	}
}

write.csv(finalArray3, file = "MoreThan30Hotels.csv")

#------------------------------------------------------------------------------
# Part 4 - Table containing the name, price range and address of each hotel

# Function that returns the name of the hotel
getName<-function(data){
	if(!is.null(data$HotelInfo$Name)){
		return(data$HotelInfo$Name)
	}else{
		sol<-"No name"
		return(sol)
	}
}

# Function that returns the price of the hotel
getPrice<-function(data){
	if(!is.null(data$HotelInfo$Price)){
		return(data$HotelInfo$Price)
	}else{
		sol<-"No price"
		return(sol)
	}
}

# Function that returns the address of the hotel
getAddress<-function(data){
	address <- data$HotelInfo$Address
	if(!is.null(address)){
		search<-"v:street-address\">"
		initialSplit<-strsplit(address, search)
		finalSplit<-gsub("<..*","",initialSplit[[1]][2])
	}else{
		finalSplit<-"No Address"
	}
	return (finalSplit) 
}

df<-data.frame("Name"=character(),"Price"=character(),"Address"=character(),stringsAsFactors=FALSE)

for(i in 1:length(jsonFiles)){
	filePath <- paste(path,jsonFiles[i],sep="")
	raw_data <- fromJSON(file=filePath)
	name <- getName(raw_data)
	price <- getPrice(raw_data)
	address <- getAddress(raw_data)
	df[i,]<-c(name,price,address)
}
write.csv(df, file = "NamePriceAddressTable.csv")

