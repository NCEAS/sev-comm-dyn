# Upload data packages for Sevilleta community dynamics

library(datapack)
library(EML)
library(rmarkdown)

process_dp1 <- function() {
    # Create an EML document describing the data and R scripts


    # Create identifiers for data and metadata objects

    # Create a DataPackage to hold all of the objects

    # Add each object to the DataPackage

    # Add provenance information about the objects

    # Upload package to the repository

}

createEML <- function(){

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
}
