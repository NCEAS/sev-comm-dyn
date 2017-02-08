# Upload data packages for Sevilleta community dynamics

library(datapack)
library(EML)
library(rmarkdown)
library(stringr)
library(dataone)

process_dp1 <- function() {

    # Set up repository access
    cn <- CNode("DEV2")
    mn <- getMNode(cn, "urn:node:mnDevUCSB1")
    d1c <- new("D1Client", cn=cn, mn=mn)

    # Create a DataPackage to hold all of the objects
    dp <- new("DataPackage")
    dataDir <- getwd()
    eml_file <- sprintf("%s/dp1-metadata.xml", dataDir)
    eml <- create_eml()

    inputs <- list()
    outputs <- list()

    # Create a DataObject to hold the script file and add it to the package and EML file
    file_name <- "Met_gap_fill.R"
    file_description <- "R script that fills in data gaps in meteorlogical data."
    file_path <- sprintf("%s/%s", dataDir, file_name)
    progObj <- new("DataObject", format="application/R", filename=file_path,
                   mediaType="text/x-rsrc", suggestedFilename=file_name)
    other_entity <- create_entity_eml(file_name, file_description, file_path, progObj@sysmeta@identifier, cn@endpoint)
    eml@dataset@otherEntity <- c(other_entity)
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

    # Set the package identifier
    eml@packageId <- new("xml_attribute", "foo")
    eml@system <- new("xml_attribute", "knb")

    # Validate the eml
    eml_validate(eml)

    # Write out the eml xml file to disk
    #write_eml(eml, eml_file)

    # Add provenance information about the objects

    # Upload package to the repository
    # resourceMapId <- uploadDataPackage(d1c, dp, replicate=TRUE, public=TRUE, quiet=F, resolveURI=resolveURI)

}

create_eml <- function(){
    eml <- new("eml")

    title <- "Sevilleta LTER Metstation number 49, precipition, daily raw and gap filled from 1992 - 2015"
    pubDate <- "2017"
    abstract <- as(set_TextType("abstractdp1.txt"), "abstract")

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
    # methods <- set_methods("methods.docx")
    #
    # dataset@methods <- methods

    #add coverage
    # begindate <- "1992-01-01"
    # enddate <- "2016-09-18"
    # geographicDescription <- "Five Points Meteorological Station (No. 49) is at the Southern edge of Mckenzie Flats - about 2.5 km west of the actual  Five-Points.  North of the road and just northeast of the intersection where another road takes off going north.  Transition area from creosote to the south into Grasslands to the north"
    # coverage <- set_coverage(begin = begindate, end = enddate,
    #                          geographicDescription = geographicDescription,
    #                          west = -106.729, east = -106.729,
    #                          north = 34.335, south = 34.335)
    #
    # dataset@coverage <- coverage

    eml@dataset <- dataset
    return(eml)
}

create_entity_eml <- function(entity_name, entity_description, file_path, identifier, resolve_uri) {

    if (stringr::str_detect(file_path, '.*.R$')) {
        other_entity <- new("otherEntity")
        other_entity@entityName <- entity_name
        other_entity@entityDescription <- entity_description
        other_entity@entityType <- "text/x-rsrc"
        resolve_url <- paste(resolve_uri, identifier, sep="/")
        online <- new("online")
        online@url <- new("url", resolve_url)
        dist <- new("distribution")
        dist@online <- online
        phys <- new("physical")
        phys@objectName <- basename(file_path)
        phys@dataFormat@externallyDefinedFormat@formatName <- "application/R"
        phys@distribution <- c(dist)
        other_entity@physical <- c(phys)
        return(other_entity)
    } else {
        df <- read.csv(paste(file_path, entity_name, sep = "/"), header=TRUE, sep=",", quote="\"", as.is=TRUE)

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
        metafile_name <- paste(entity_name,"meta.csv", sep = "")
        #write.csv(attributes, file = paste(file_path, metafile_name, sep = "/"), row.names = FALSE)


        #look at the standard units to get them right
        #standardUnits <- get_unitList()
        #View(standardUnits$units)

        #define custom unit for joulePerCentimeterSquared
        unitType <- data.frame(id = c("energyPerArea", "energyPerArea"), dimension = c("energy", "area"), power = c(1, 0.0001) )
        custom_units <- data.frame(id = "joulePerCentimeterSquared", unitType = "energyPerArea", parentSI = "joulesPerSquareKilometer", multiplierToSI = 0.0001, description = "solar radiation")
        unitsList <- set_unitList(custom_units, unitType)

        #read the attributes file back in with all new entries
        attributes <- read.csv(paste(file_path, metafile_name, sep = "/"), header = TRUE, sep = ",", quote = "\"", as.is = TRUE, na.strings = "")

        factor_meta <- paste(file_path, paste(entity_name, "factor.csv", sep = ""), sep = "/")

        if(file.exists(factor_meta)){
            factors <- read.csv(factor_meta, header = TRUE, sep = ",", quote = "\"", as.is = TRUE)
        }

        # get the column classes into a vector as required by the set_attribute function
        col_classes <- attributes[,"columnClasses"]

        #take out that column again
        attributes$columnClasses <- NULL

        #with the attributes data frames in place we can create the attributeList element - no factors need to be defined for this dataset
        attributeList <- set_attributes(attributes, col_classes = col_classes)

        #physical parameter for standard Microsoft csv file
        physical <- set_physical(entity_name,
                                 numHeaderLines = "1",
                                 recordDelimiter = "\\r\\n")


        #pull to gether information for the dataTable
        dataTable1 <- new("dataTable",
                          entityName = entity_name,
                          entityDescription = "daily average or total metstation data from Sevilleta LTER",
                          physical = physical,
                          attributeList = attributeList)

        additionalMetadata = as(unitsList, "additionalMetadata")


        return(dataTable1, additionalMetadata)
    }
}

# Update the distribution url in the EML object with the DataONE URL
updateEMLdistURL <- function(EMLdoc, entityName, resolveUrl) {
    # Search for the entity among the 'otherEntity' elements
    found <- FALSE
    for (iEntity in 1:length(EMLdoc@dataset@otherEntity@.Data)) {
        thisEntityName <- EMLdoc@dataset@otherEntity@.Data[[iEntity]]@entityName
        if(thisEntityName == entityName) {
            message(sprintf("Updating otherEntity %s in EML\n", thisEntityName))
            EMLdoc@dataset@otherEntity@.Data[[iEntity]]@physical[[1]]@distribution[[1]]@online@url@.Data <- resolveUrl
            found <- TRUE
        }
    }
    if(found) return(EMLdoc)
    # If not already found, search for the entity among the 'dataTable' elements
    for (iEntity in 1:length(EMLdoc@dataset@dataTable@.Data)) {
        thisEntityName <- EMLdoc@dataset@dataTable@.Data[[iEntity]]@entityName
        if(thisEntityName == entityName) {
            message(sprintf("Update dataTable %s in EML\n", thisEntityName))
            EMLdoc@dataset@dataTable@.Data[[iEntity]]@physical[[1]]@distribution[[1]]@online@url@.Data <- resolveUrl
        }
    }
    return(EMLdoc)
}

