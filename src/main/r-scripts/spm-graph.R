library(dplyr)
library(pals)
library(stringi)

export_graph <- function (workingPath, picName, bpo, 
		ctMeasure, ctLog, ctShortMeasure, colName, ylim)
{
	
	bpo <- rundata %>% filter (baseLog == ctLog)
	tb <- as.numeric(bpo[[colName]])
	bpo[[colName]] <- tb

	byLog <- bpo %>% group_by (Short.Id)

	sbyLog <- byLog %>% summarise(
			n     = n(),
			max   = max( .data[[colName]] ),
	  		mean 	= mean( .data[[colName]] )
		)

	bnames <- sbyLog$Short.Id
	jpeg( paste(workingPath, picName,".jpg", sep="") )

	barplot( 
		sbyLog$mean, 
		main= paste(ctMeasure, ctLog), 
		names.arg = bnames, 
		ylab=ctShortMeasure,
		horiz=FALSE, cex.names=0.8, las=2,
		ylim = ylim,
		col=alphabet())
	dev.off()
}

em_graph <- function(workingPath, picName, rundata, ctLog){
	export_graph( workingPath, picName = picName, 
		  rundata, 
		  ctMeasure = "Earth Movers", 
		  ctLog = ctLog,
		  ctShortMeasure = "tEMSC 0.8",
		  colName = "EARTH_MOVERS_LIGHT_COVERAGE",
		  ylim=c(0,1))
}

entp_graph <- function(workingPath, picName, rundata, ctLog){
	export_graph( workingPath, picName = picName, 
		  rundata, 
		  ctMeasure = "Entropy Precision", 
		  ctLog = ctLog,
		  ctShortMeasure = "S_Precision",
		  colName = "ENTROPY_PRECISION",
		  ylim=c(0,1))
}

entr_graph <- function(workingPath, picName, rundata, ctLog){
	export_graph( workingPath, picName = picName, 
		  rundata, 
		  ctMeasure = "Entropy Recall", 
		  ctLog = ctLog,
		  ctShortMeasure = "S_Recall",
		  colName = "ENTROPY_RECALL",
		  ylim=c(0,1))
}

entct_graph <- function(workingPath, picName, rundata, ctLog){
	export_graph( workingPath, picName = picName, 
		  rundata, 
		  ctMeasure = "Entity Count", 
		  ctLog = ctLog,
		  ctShortMeasure = "|P| + |T|",
		  colName = "MODEL_ENTITY_COUNT",
		  ylim=c(0,3000))
}

edgect_graph <- function(workingPath, picName, rundata, ctLog){
	export_graph( workingPath, picName = picName, 
		  rundata, 
		  ctMeasure = "Entity Count", 
		  ctLog = ctLog,
		  ctShortMeasure = "|F|",
		  colName = "MODEL_EDGE_COUNT",
		  ylim=c(0,90))
}

duration_graph <- function(workingPath, picName, rundata, ctLog){
	bpo <- rundata %>% filter (Log == ctLog)
	tb <- as.numeric(bpo[[colName]])
	bnames <- bpo$Short.Id
	jpeg( paste(workingPath, picName,".jpg", sep="") )
	barplot(tb, main= paste("Estimator Duration", ctLog), 
		names.arg = bnames, 
		ylab= "t",
		horiz=FALSE, cex.names=0.8, las=2,
		ylim = ylim,
		col=alphabet())
	dev.off()
}

workingPath = "c:/Users/burkeat/bpm/toothpaste/var/"

rundata = read.csv( paste(workingPath,"winserver.psv", sep=""), 
			sep ="|", strip.white=TRUE)

clncreators <- recode(rundata$Short.Id,
			     "bce-fodina" = "fork-fodina",
                       "bce-inductive" = "fork-inductive",
                       "bce-split" = "fork-split",
                       "rssmt" = "rsd")

clncreators <- recode(rundata$Short.Id,
			     "bce-fodina" = "fork-fodina",
                       "bce-inductive" = "fork-inductive",
                       "bce-split" = "fork-split",
                       "rssmt" = "rsd")


rundata$Short.Id <- clncreators

baseLogs <- stri_sub(rundata$Log,0,-4)

rundata$baseLog = baseLogs


logs <- unique(baseLogs)

empicName   <- paste("em", gsub(" ","_", tolower(logs)), sep="") 
entppicName <- paste("entp", gsub(" ","_", tolower(logs)), sep="") 
entrpicName <- paste("entr", gsub(" ","_", tolower(logs)), sep="") 
entcpicName <- paste("entc", gsub(" ","_", tolower(logs)), sep="") 
edgepicName <- paste("edge", gsub(" ","_", tolower(logs)), sep="") 
durpicName  <- paste("dur", gsub(" ","_", tolower(logs)), sep="") 



count <- 1
for (log in logs){
	em_graph( workingPath, picName = empicName[count], 
			  rundata, ctLog = log )
	entp_graph( workingPath, picName = entppicName[count], 
		  rundata, ctLog = log )
	entr_graph( workingPath, picName = entrpicName[count], 
		  rundata, ctLog = log )
#	entct_graph( workingPath, picName = entcpicName[count], 
#		  rundata, ctLog = log )
#	edgect_graph( workingPath, picName = edgepicName[count], 
#		  rundata, ctLog = log )
	#duration_graph( workingPath, picName = durpicName[count], 
	#	  rundata, ctLog = log )
	count= count +1
}



