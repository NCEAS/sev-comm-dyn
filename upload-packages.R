# Upload data packages for Sevilleta community dynamics

library(datapack)
library(EML)
library(rmarkdown)
library(stringr)
library(dataone)
library(uuid)

# Set up repository access
cn <- CNode("DEV2")
mn <- getMNode(cn, "urn:node:mnDevUCSB1")
d1c <- new("D1Client", cn=cn, mn=mn)


process_dp1 <- function() {

    # Create a DataPackage to hold all of the objects
    dp <- new("DataPackage")
    dataDir <- getwd()
    eml_file <- sprintf("%s/dp1-metadata.xml", dataDir)
    title <- "Sevilleta LTER Metstation number 49, precipition, daily raw and gap filled from 1992 - 2015"
    pubDate <- "2017"
    file_ext <- "dp1"
    keywords <- c("meteorology", "precipitation")

    eml <- create_eml(title, pubDate, file_ext, keywords, dataDir)
    eml <- add_people_eml(eml, file_ext)

    # Create a DataObject to hold the script file and add it to the EML file
    file_name <- "Met_gap_fill.R"
    file_description <- "R script that fills in data gaps in meteorlogical data."
    file_path <- sprintf("%s/%s", dataDir, file_name)
    progObj <- new("DataObject", format="application/R", filename=file_path,
                   mediaType="text/x-rsrc", suggestedFilename=file_name)
    eml <- add_entity_eml_new(eml, file_name, file_description, file_path, progObj@sysmeta@identifier, cn@endpoint)

    # Document and add the first data file to the package
    file_name <- "Met_all_excel.csv"
    file_description <- "Sevilleta Meteorological data"
    file_path <- sprintf("%s/%s", dataDir, file_name)
    do1 <- new("DataObject", format="text/csv", filename=file_path,
                   mediaType="text/csv", suggestedFilename=file_name)
    eml <- add_entity_eml(eml, file_name, file_description, file_path, do1@sysmeta@identifier, cn@endpoint)

    # Document and add the derived data file to the package
    file_name <- "Met_all.csv"
    file_description <- "Sevilleta Meteorological data, with gaps filled"
    file_path <- sprintf("%s/%s", dataDir, file_name)
    do2 <- new("DataObject", format="text/csv", filename=file_path,
               mediaType="text/csv", suggestedFilename=file_name)
    eml <- add_entity_eml(eml, file_name, file_description, file_path, do2@sysmeta@identifier, cn@endpoint)

    # Set the package identifier
    eml_id <- paste0("urn:uuid:", uuid::UUIDgenerate())
    eml@packageId <- new("xml_attribute", eml_id)
    eml@system <- new("xml_attribute", "knb")

    # Validate the eml and write it to disk
    eml_validate(eml)
    write_eml(eml, eml_file)

    # add the data objects and EML to the DataPackage
    eml_object <- new("DataObject", format="eml://ecoinformatics.org/eml-2.1.1", filename=eml_file,
                      mediaType="text/xml", suggestedFilename=basename(eml_file))
    dp <- addData(dp, do1, mo=eml_object)
    dp <- addData(dp, progObj, mo=eml_object)
    dp <- addData(dp, do2, mo=eml_object)

    # Add provenance information about the derived objects
    dp <- insertDerivation(dp,
                           sources=list("sev.1.file1", "sev.1.file2", "sev.1.file3", "sev.1.file4", "sev.1.file5"),
                           derivations=list(do1@sysmeta@identifier))
    dp <- insertDerivation(dp, do1, progObj, do2)

    # Upload package to the repository
    resourceMapId <- uploadDataPackage(d1c, dp, replicate=FALSE, public=TRUE, quiet=F, resolveURI=paste0(d1c@cn@endpoint, "/resolve"))
    message("    EML ID: ", eml_object@sysmeta@identifier)
    message("Package ID: ", resourceMapId)
}

process_dp2 <- function(){

    # Create a DataPackage to hold all of the objects
    dp <- new("DataPackage")
    dataDir <- getwd()
    eml_file <- sprintf("%s/dp2-metadata.xml", dataDir)
    title <- "Sevilleta species list with life form designations"
    pubDate <- "2017"
    file_ext <- "dp2"
    keywords <- c("species names", "taxonomy", "life forms", "metabolism")

    eml <- create_eml(title, pubDate, file_ext, keywords, dataDir)

    # Document and add the first data file to the package
    file_name <- "sevilleta_species-list.csv"
    file_description <- "Sevilleta species list with life form designations"
    file_path <- sprintf("%s/%s", dataDir, file_name)
    do1 <- new("DataObject", format="text/csv", filename=file_path,
               mediaType="text/csv", suggestedFilename=file_name)
    eml <- add_entity_eml(eml, file_name, file_description, file_path, do1@sysmeta@identifier, cn@endpoint)


}

process_dp3 <- function(){

    # Create a DataPackage to hold all of the objects
    dp <- new("DataPackage")
    dataDir <- getwd()
    eml_file <- sprintf("%s/dp2-metadata.xml", dataDir)
    title <- "Data analysis and graphs for publication"
    pubDate <- "2017"
    file_ext <- "dp3"
    keywords <- c("species names", "taxonomy", "life forms", "metabolism")

    eml <- create_eml(title, pubDate, file_ext, keywords, dataDir)

    # Document and add the first file to the package
    file_name <- "MSDS.jpg"
    file_description <- "Odination of mice feeding guilds at two sites"
    file_path <- sprintf("%s/%s", dataDir, file_name)
    do1 <- new("DataObject", format="image/jpg", filename=file_path,
               mediaType="image/jpg", suggestedFilename=file_name)
    eml <- add_entity_eml(eml, file_name, file_description, file_path, do1@sysmeta@identifier, cn@endpoint)

    # Document and add the second file to the package
    file_name <- "Noble_BlackGramaCore.png"
    file_description <- "Precipitation, biomass, NPP, and rodent abundance at black grama site"
    file_path <- sprintf("%s/%s", dataDir, file_name)
    do1 <- new("DataObject", format="image/png", filename=file_path,
               mediaType="image/png", suggestedFilename=file_name)
    eml <- add_entity_eml(eml, file_name, file_description, file_path, do1@sysmeta@identifier, cn@endpoint)

    # Document and add the third file to the package
    file_name <- "Noble_CreosoteCore.png"
    file_description <- "Precipitation, biomass, NPP, and rodent abundance at creosote site"
    file_path <- sprintf("%s/%s", dataDir, file_name)
    do1 <- new("DataObject", format="image/png", filename=file_path,
               mediaType="image/png", suggestedFilename=file_name)
    eml <- add_entity_eml(eml, file_name, file_description, file_path, do1@sysmeta@identifier, cn@endpoint)

    # Create a DataObject to hold the script file and add it to the EML file
    file_name <- "veg_analysis.R"
    file_description <- "R script that manipulates input data, runs analysis and generates graphs"
    file_path <- sprintf("%s/%s", dataDir, file_name)
    progObj <- new("DataObject", format="application/R", filename=file_path,
                   mediaType="text/x-rsrc", suggestedFilename=file_name)
    eml <- add_entity_eml(eml, file_name, file_description, file_path, progObj@sysmeta@identifier, cn@endpoint)


}


create_eml <- function(title, pubDate, file_ext, keywords, dataDir){
    eml <- new("eml")

    abstract_file <- paste(dataDir, paste("abstract", file_ext, ".txt", sep = ""), sep = "/")

    abstract <- as(set_TextType(abstract_file), "abstract")

    #start the dataset EML
    dataset <- new("dataset",
                   title = title,
                   pubDate = pubDate,
                   abstract = abstract)

    # individualName <- new("individualName",
    #                       givenName = "Jenn",
    #                       surName = "Rudgers")
    #
    # creator <- new("creator",
    #                individualName = individualName,
    #                organizationName = "University of New Mexico",
    #                electronicMailAddress = "jrudgers@unm.edu")
    #
    # dataset@creator <- new("ListOfcreator", c(creator))
    #
    # dc <- as.person("Scott Collins scollins@sevilleta.unm.edu")
    # dataset_contact <- as(dc, "contact")
    # dataset@contact <- new("ListOfcontact", c(dataset_contact))

    #add keywords
    keywordSet <- c(new("keywordSet",
                        keyword = keywords))

    dataset@keywordSet <- new("ListOfkeywordSet", c(keywordSet))

    #intellectual Rights
    dataset@intellectualRights <- as(set_TextType("intellectualRights.txt"), "intellectualRights")


    #add methods
    method_file <- paste(dataDir, paste("method", file_ext, ".txt", sep = ""), sep = "/")
    methods <- set_methods(method_file)

    dataset@methods <- methods

    #add coverage
    begindate <- "1992-01-01"
    enddate <- "2016-09-18"
    geographicDescription <- "Five Points Meteorological Station (No. 49) is at the Southern edge of Mckenzie Flats - about 2.5 km west of the actual  Five-Points.  North of the road and just northeast of the intersection where another road takes off going north.  Transition area from creosote to the south into Grasslands to the north, Sampling sites are in creosote and black grama areas, respectively"
    coverage <- set_coverage(begin = begindate, end = enddate,
                             geographicDescription = geographicDescription,
                             west = -106.729, east = -106.729,
                             north = 34.335, south = 34.335)

    dataset@coverage <- coverage

    eml@dataset <- dataset
    return(eml)
}

add_people_eml <- function(eml, file_ext){


    people_file <- paste(dataDir, paste("people", file_ext, ".csv", sep = ""), sep = "/")

    #read csv file with person information (givenName, surName, organization,  electronicMailAddress, userId)
    personinfo <- read.csv(people_file, header = TRUE, sep = ",", colClasses = "character")

    #subset personinfo for creators
    creatorinfo <- subset(personinfo, role == "creator")

    #run each row through the helper function to set creators
    eml@dataset@creator <- as(lapply(1:dim(creatorinfo)[1], function(i)
        set_creator(creatorinfo[i,])),
        "ListOfcreator")

    #subset personinfo for contacts
    contactinfo <- subset(personinfo, role == "contact")

    #run each row through the helper function to set creators
    eml@dataset@contact <- as(lapply(1:dim(contactinfo)[1], function(i)
        set_contact(contactinfo[i,])),
        "ListOfcontact")

    return(eml)


}

set_creator <- function(personinforow){

    individualName <- new("individualName",
                          givenName = personinforow[,"givenName"],
                          surName = personinforow[,"surName"])

    creator <- new("creator",
                   individualName = individualName,
                   organizationName = personinforow[,"organizationName"])

    if(nchar(personinforow[,"electronicMailAddress"]) > 0){

        email <- new("electronicMailAddress")
        email@.Data <- personinforow[,"electronicMailAddress"]
        creator@electronicMailAddress <- new("ListOfelectronicMailAddress", c(email))
    }

    if(nchar(personinforow[,"userId"]) > 0){

        userId <- new("userId")
        userId@directory <- new("xml_attribute", "http://orcid.org")
        userId@.Data <- personinforow[,"userId"]
        creator@userId <- new("ListOfuserId", c(userId))
    }
    return(creator)
}

set_contact <- function(personinforow){

    individualName <- new("individualName",
                          givenName = personinforow[,"givenName"],
                          surName = personinforow[,"surName"])

    contact <- new("contact",
                   individualName = individualName,
                   organizationName = personinforow[,"organizationName"])

    if(nchar(personinforow[,"electronicMailAddress"]) > 0){

        email <- new("electronicMailAddress")
        email@.Data <- personinforow[,"electronicMailAddress"]
        contact@electronicMailAddress <- new("ListOfelectronicMailAddress", c(email))
    }

    if(nchar(personinforow[,"userId"]) > 0){

        userId <- new("userId")
        userId@directory <- new("xml_attribute", "http://orcid.org")
        userId@.Data <- personinforow[,"userId"]
        contact@userId <- new("ListOfuserId", c(userId))
    }
    return(contact)
}


add_entity_eml <- function(eml, entity_name, entity_description, file_path, identifier, resolve_uri) {

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
        current_other_entities <- eml@dataset@otherEntity
        current_other_entities[[length(current_other_entities)+1]] <- other_entity
        eml@dataset@otherEntity <- current_other_entities
        return(eml)
    } else if(stringr::str_detect(file_path, '.*.png$')){
        other_entity <- new("otherEntity")
        other_entity@entityName <- entity_name
        other_entity@entityDescription <- entity_description
        other_entity@entityType <- "image/png"
        resolve_url <- paste(resolve_uri, identifier, sep="/")
        online <- new("online")
        online@url <- new("url", resolve_url)
        dist <- new("distribution")
        dist@online <- online
        phys <- new("physical")
        phys@objectName <- basename(file_path)
        phys@dataFormat@externallyDefinedFormat@formatName <- "image/png"
        phys@distribution <- c(dist)
        other_entity@physical <- c(phys)
        current_other_entities <- eml@dataset@otherEntity
        current_other_entities[[length(current_other_entities)+1]] <- other_entity
        eml@dataset@otherEntity <- current_other_entities
        return(eml)
    } else {
        file_name <- basename(file_path)
        path_only <- dirname(file_path)

        #define custom units
        unitTypefile_name <- paste(file_name, "unitType.csv", sep = "")
        if(file.exists(paste(path_only, unitTypefile_name, sep = "/"))){
            unitType <- read.csv(paste(path_only, unitTypefile_name, sep = "/"), header = TRUE, sep = ",", quote = "\"", as.is = TRUE, na.strings = "")
            custom_unitsfile_name <- paste(file_name, "custom_units.csv", sep = "")
            custom_units <- read.csv(paste(path_only, custom_unitsfile_name, sep = "/"), header = TRUE, sep = ",", quote = "\"", as.is = TRUE, na.strings = "")
            unitsList <- set_unitList(custom_units, unitType)
            additionalMetadata <- as(unitsList, "additionalMetadata")
            eml@additionalMetadata <- c(additionalMetadata)
        }

        #read the attributes file back in with all new entries
        attrmetafile_name <- paste(file_name,"meta.csv", sep = "")
        attributes <- read.csv(paste(path_only, attrmetafile_name, sep = "/"), header = TRUE, sep = ",", quote = "\"", as.is = TRUE, na.strings = "")

        factor_meta <- paste(path_only, paste(file_name, "factor.csv", sep = ""), sep = "/")

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
        resolve_url <- paste(resolve_uri, identifier, sep="/")

        physical <- set_physical(file_name,
                                 numHeaderLines = "1",
                                 recordDelimiter = "\\r\\n",
                                 url = resolve_url)


        #pull to gether information for the dataTable

        dataTable <- new("dataTable",
                         entityName = entity_name,
                         entityDescription = entity_description,
                         physical = physical,
                         attributeList = attributeList)

        current_dataTables <- eml@dataset@dataTable
        current_dataTables[[length(current_dataTables)+1]] <- dataTable
        eml@dataset@dataTable <- current_dataTables


        return(eml)
    }


}

add_entity_eml_new <- function(eml, entity_name, entity_description, file_path, identifier, resolve_uri) {

    if (stringr::str_detect(file_path, '.*.csv$')){
        file_name <- basename(file_path)
        path_only <- dirname(file_path)

        #define custom units
        unitTypefile_name <- paste(file_name, "unitType.csv", sep = "")
        if(file.exists(paste(path_only, unitTypefile_name, sep = "/"))){
            unitType <- read.csv(paste(path_only, unitTypefile_name, sep = "/"), header = TRUE, sep = ",", quote = "\"", as.is = TRUE, na.strings = "")
            custom_unitsfile_name <- paste(file_name, "custom_units.csv", sep = "")
            custom_units <- read.csv(paste(path_only, custom_unitsfile_name, sep = "/"), header = TRUE, sep = ",", quote = "\"", as.is = TRUE, na.strings = "")
            unitsList <- set_unitList(custom_units, unitType)
            additionalMetadata <- as(unitsList, "additionalMetadata")
            eml@additionalMetadata <- c(additionalMetadata)
        }

        #read the attributes file back in with all new entries
        attrmetafile_name <- paste(file_name,"meta.csv", sep = "")
        attributes <- read.csv(paste(path_only, attrmetafile_name, sep = "/"), header = TRUE, sep = ",", quote = "\"", as.is = TRUE, na.strings = "")

        factor_meta <- paste(path_only, paste(file_name, "factor.csv", sep = ""), sep = "/")

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
        resolve_url <- paste(resolve_uri, identifier, sep="/")

        physical <- set_physical(file_name,
                                 numHeaderLines = "1",
                                 recordDelimiter = "\\r\\n",
                                 url = resolve_url)


        #pull to gether information for the dataTable

        dataTable <- new("dataTable",
                         entityName = entity_name,
                         entityDescription = entity_description,
                         physical = physical,
                         attributeList = attributeList)

        current_dataTables <- eml@dataset@dataTable
        current_dataTables[[length(current_dataTables)+1]] <- dataTable
        eml@dataset@dataTable <- current_dataTables


        return(eml)

    }else{

        if (stringr::str_detect(file_path, '.*.R$')){
            entityType <- "text/x-rsrc"
            formatName <- "application/R"
        }else if(stringr::str_detect(file_path, '.*.png$')){
            entityType <- "image/png"
            formatName <- "image/png"
        }else if(stringr::str_detect(file_path, '.*.jpg$')){
            entityType <- "image/jpg"
            formatName <- "image/jpg"
        }


        other_entity <- new("otherEntity")
        other_entity@entityName <- entity_name
        other_entity@entityDescription <- entity_description
        other_entity@entityType <- entityType
        resolve_url <- paste(resolve_uri, identifier, sep="/")
        online <- new("online")
        online@url <- new("url", resolve_url)
        dist <- new("distribution")
        dist@online <- online
        phys <- new("physical")
        phys@objectName <- basename(file_path)
        phys@dataFormat@externallyDefinedFormat@formatName <- formatName
        phys@distribution <- c(dist)
        other_entity@physical <- c(phys)
        current_other_entities <- eml@dataset@otherEntity
        current_other_entities[[length(current_other_entities)+1]] <- other_entity
        eml@dataset@otherEntity <- current_other_entities
        return(eml)
    }
}

