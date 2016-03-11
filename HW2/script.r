library(rjson)
path<-"path"

#Part 1 Number of incidents reported during 2015 by “Community Area”
raw_data <- fromJSON(file=path)
data<-raw_data$data


parseYear<-function(data){
	str<-substr(data,1,4)
	return(str)
}

counter<-1
array<-rep(1,length(data))
for(i in 1:length(data)){
	if(identical(parseYear(data[[i]][[9]]),"2015")&&(!is.null(data[[i]][[22]]))){
		array[counter]<-data[[i]][[22]]
		counter<-counter+1
	}
}

finalArray<-rep(1,counter-1)
j<-1
for(i in 1:length(array)){
	if(array[i]!="1"){
		finalArray[j]<-array[i]
		j<-j+1
	}
}

finalArray<-table(finalArray)
write.csv(finalArray, file = "Grafitti2015.csv")

#Part 2 Visualization which better conveys knowledge regarding graffiti and community

barplot(finalArray,main="Community Area graffiti distribution", xlab="Community Area", ylab="Graffiti number")
