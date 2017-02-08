# Upload data packages for Sevilleta community dynamics

library(datapack)
library(EML)
library(rmarkdown)

process_dp1 <- function() {
    # Create an EML document describing the data and R scripts


    # Create identifiers for data and metadata objects

    # Create a DataPackage to hold all of the objects
    dp <- new("DataPackage")
    dataDir <- getwd()
    emlFile <- sprintf("%s/dp1-metadata.xml", dataDir)
    eml <- create_eml()

    inputs <- list()
    outputs <- list()
    # Create a DataObject to hold the script file and add it to the package
    progFile <- "Met_gap_fill.R"
    progObj <- new("DataObject", format="application/R",
                   filename=sprintf("%s/%s", dataDir, progFile),
                   mediaType="text/x-rsrc", suggestedFilename=progFile)
    dp <- addData(dp, progObj)

    # Update the distribution URL in the EML object to match the DataONE PID for this object
    # eml <- updateEMLdistURL(eml, entityName="Data merging R script", resolveUrl=sprintf("%s/%s", resolveURI, getIdentifier(progObj)))
    #
    # do <- new("DataObject", format="text/csv", filename=sprintf("%s/%s", dataDir, "df35b.256.1.csv"), suggestedFilename="Non-EVOS SINs.csv")
    # dp <- addData(dp, do)
    # inputs[[length(inputs)+1]] <- do
    # eml <- updateEMLdistURL(eml, entityName="Non-EVOS SINs.csv", resolveUrl=sprintf("%s/%s", resolveURI, getIdentifier(do)))
    #
    # doOut <- new("DataObject", format="text/csv", filename=sprintf("%s/%s", dataDir, "df35b.254.3.csv"), suggestedFilename="Total_Aromatic_Alkanes_PWS.csv")
    # dp <- addData(dp, doOut)
    # outputs[[length(outputs)+1]] <- doOut
    # eml <- updateEMLdistURL(eml, entityName="Total_Aromatic_Alkanes_PWS.csv", resolveUrl=sprintf("%s/%s", resolveURI, getIdentifier(doOut)))


    # Now add the provenance relationships for this script, and it's inputs and outputs
    # dp <- insertDerivation(dp, sources=inputs, program=progObj, derivations=outputs)


    # Associate the metadata object with each package member
    # pids <- getIdentifiers(dp)
    # for(iPid in 1:length(pids)) {
    #     thisPid <- pids[[iPid]]
    #     dp <- insertRelationship(dp, subjectID=getIdentifier(metadataObj), objectIDs=thisPid)
    # }
    # Add each object to the DataPackage

    # Add provenance information about the objects

    # Upload package to the repository
    # cn <- CNode("DEV2")
    # mn <- getMNode(cn, "urn:node:mnDevUCSB1")
    # d1c <- new("D1Client", cn=cn, mn=mn)
    #
    # resourceMapId <- uploadDataPackage(d1c, dp, replicate=TRUE, public=TRUE, quiet=F, resolveURI=resolveURI)


}

create_eml <- function(){
    eml <- new("eml")
    return(eml)

    title <- "Sevilleta LTER Metstation number 49, precipition, daily raw and gap filled from 1992 - 2015"
    pubDate <- "2017"
    abstract <- as(set_TextType("abstract.docx"), "abstract")

    #start the dataset EML
    dataset <- new("dataset",
                   title = title,
                   pubDate = pubDate,
                   abstract = abstract)

    individualName <- new("individualName",
                          givenName = "Jenn",
                          surName = "Rudgers")

    creator <- new("creator",
                   individualName = individualName,
                   organizationName = "University of New Mexico",
                   electronicMailAddress = "jrudgers@unm.edu")

    dataset@creator <- new("ListOfcreator", c(creator))

    dc <- as.person("Scott Collins scollins@sevilleta.unm.edu")
    dataset_contact <- as(dc, "contact")
    dataset@contact <- new("ListOfcontact", c(dataset_contact))

    #add keywords
    keywordSet <- c(new("keywordSet",
                        keyword = c("meteorology", "precipitation")))

    dataset@keywordSet <- new("ListOfkeywordSet", c(keywordSet))

    #intellectual Rights
    dataset@intellectualRights <- as(set_TextType("intellectualRights.txt"), "intellectualRights")


    #add methods
    methods <- set_methods("methods.docx")

    dataset@methods <- methods

    #add coverage
    begindate <- "1992-01-01"
    enddate <- "2016-09-18"
    geographicDescription <- "Five Points Meteorological Station (No. 49) is at the Southern edge of Mckenzie Flats - about 2.5 km west of the actual  Five-Points.  North of the road and just northeast of the intersection where another road takes off going north.  Transition area from creosote to the south into Grasslands to the north"
    coverage <- set_coverage(begin = begindate, end = enddate,
                             geographicDescription = geographicDescription,
                             west = -106.729, east = -106.729,
                             north = 34.335, south = 34.335)

    dataset@coverage <- coverage


    df <- read.csv("Met_all_excel.csv", header=TRUE, sep=",", quote="\"", as.is=TRUE)

    #set up the attribute metadata csv file
    rows <- ncol(df)

    attributes <- data.frame(attributeName = character(rows),
                             formatString = character(rows),
                             unit = character(rows),
                             numberType = character(rows),
                             definition = character(rows),
                             attributeDefinition = character(rows),
                             columnClasses = character(rows),
                             minimum = character(rows),
                             maximum = character(rows),
                             missingValueCode = character(rows),
                             missingValueCodeExplanation = character(rows),
                             stringsAsFactors = FALSE)

    #get some metadata from data frame
    #add the column names to the template file
    attributes$attributeName <- names(df)
    #get the data types for each column
    attributes$columnClasses <- sapply(df, class)
    attributes$minimum <- sapply(df, min)
    attributes$maximum <- sapply(df, max)
    #set what R thinks is integer to numeric
    attributes$columnClasses[attributes$columnClasses == "integer"] <- "numeric"
    #write the prepared template to a csv file
    #write.csv(attributes, file = "met_all_csv_metadata.csv", row.names = FALSE)


    #look at the standard units to get them right
    standardUnits <- get_unitList()
    View(standardUnits$units)

}
