library(ABwaterwells)
library(dplyr)

# Get all table names from the ABwaterwells package
table_names <- awwid_list_tables()

# Generate a list of data frames containing attributes and their types for each table
# table_obj <- lapply(table_names, function(name) {
#   x <- awwid_tbl(name, top = 1L)
#   x <- tibble(attributes = names(x), type = sapply(x, class))
#   attr(x, "awwid_tbl") <- tolower(name)
#   return(x)
# })
# metadata <- setNames(table_obj, tolower(table_names))

# Function to generate metadata for each table
create_table_metadata <- function(
  title,
  description,
  columns,
  relations = NULL
) {
  metadata = list(
    title = title,
    description = description,
    columns = columns,
    relations = relations
  )
  return(metadata)
}

# Create metadata for all tables
awwid_metadata <- list(
  AnalysisItems = create_table_metadata(
    "Analysis Items",
    "Measured values of chemical analyses performed on water samples",
    list(
      elementid = list(type = "integer", description = "Element ID"),
      chemicalanalysisid = list(
        type = "integer",
        description = "Chemical Analysis ID"
      ),
      value = list(
        type = "double",
        description = "Value of the chemical analysis"
      ),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      )
    ),
    list(
      pk = "elementid_chemicalanalysisid",
      fk = c("elementid", "chemicalanalysisid")
    )
  ),

  Elements = create_table_metadata(
    "Elements",
    "Lookup information for chemical elements measured in water samples",
    list(
      elementid = list(type = "integer", description = "Element ID"),
      elementname = list(type = "character", description = "Element name"),
      elementsymbol = list(
        type = "character",
        description = "Abbreviation of element name"
      ),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      ),
      decimalplaces = list(
        type = "integer",
        description = "Precision of measurement in decimals"
      ),
      unitofmeasure = list(
        type = "character",
        description = "Units of measurement"
      )
    ),
    list(pk = "elementid")
  ),

  ChemicalAnalysis = create_table_metadata(
    "Chemical Analysis",
    "Sample details and analysis dates for chemical analyses performed on water samples",
    list(
      chemicalanalysisid = list(
        type = "integer",
        description = "PK unique identifier"
      ),
      wellid = list(
        type = "integer",
        description = "Foreign key to the Wells Table"
      ),
      samplenumber = list(type = "character", description = "Sample Number"),
      sampledate = list(type = "POSIXct", description = "Sampling Date"),
      analysisdate = list(type = "POXIXct", description = "Analysis Date"),
      laboratory = list(type = "character", description = "Laboratory Code"),
      waterlevel = list(type = "units", description = "Water level in well"),
      aquifer = list(type = "character", description = "Name of aquifer"),
      remarks = list(type = "character", description = "Remarks"),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      ),
      wellreportid = list(
        type = "integer",
        description = "Foreign key to the Well_Reports table"
      )
    ),
    list(pk = "chemicalanalysisid", fk = c("wellid", "wellreportid"))
  ),

  Boreholes = create_table_metadata(
    "Boreholes",
    "Borehole depth and diameter information",
    list(
      wellreportid = list(
        type = "integer",
        description = "Foreign key to the Well_Reports table"
      ),
      boreholeid = list(
        type = "integer",
        description = "Primary key uniquely identifying the record"
      ),
      diameter = list(
        type = "units",
        description = "Diameter	Diameter of the bore hole"
      ),
      boreholedepthfrom = list(
        type = "units",
        description = "From	Top of the borehole as measured from ground level"
      ),
      boreholedepthto = list(
        type = "units",
        description = "To	Bottom of the borehole as measured from ground level"
      ),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      )
    ),
    list(pk = "boreholeid", fk = c("wellreportid"))
  ),

  MaterialOptions = create_table_metadata(
    "Material Options",
    "Drilling material type lookup table",
    list(
      materialoptionid = list(
        type = "integer",
        description = "Primary key uniquely identifying the record"
      ),
      name = list(type = "character", description = "Material option name"),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      )
    ),
    list(pk = "materialoptionid")
  ),

  WellCasingLogs = create_table_metadata(
    "Well Casing Logs",
    "Well casing depth and diameter information",
    list(
      wellcasinglogid = list(
        type = "integer",
        description = "Primary key uniquely identifying the record"
      ),
      welldecommissioningdetailid = list(
        type = "integer",
        description = "Foreign key from the WellDecommissioningDetails table"
      ),
      wellcasingdepthfrom = list(
        type = "units",
        description = "From Top of the casing as measured from ground level"
      ),
      wellcasingdepthto = list(
        type = "units",
        description = "To Bottom of the casing as measured from ground level"
      ),
      wellcasingdiameter = list(
        type = "units",
        description = "Well casing diameter"
      ),
      plugmaterialoptionid = list(
        type = "integer",
        description = "Foreign key to the PlugMaterialOptions table"
      ),
      othermaterials = list(type = "logical", description = "Other materials"),
      casingstatusid = list(
        type = "integer",
        description = "Foreign key to the CasingStatus table"
      ),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      )
    ),
    list(
      pk = "wellcasinglogid",
      fk = c("welldecommissioningdetailid", "plugmaterialoptionid", "casingstatusid")
    )
  ),

  Drillers = create_table_metadata(
    "Drillers",
    "Driller information including name and journeyman number",
    list(
      drillerid = list(
        type = "integer",
        description = "Primary key uniquely identifying the record"
      ),
      lastname = list(
        type = "character",
        description = "Last name of the driller"
      ),
      middleinitial = list(
        type = "character",
        description = "Middle initial of the driller"
      ),
      firstname = list(
        type = "character",
        description = "First name of the driller"
      ),
      journeymannumber = list(
        type = "character",
        description = "Journeyman number of the driller"
      ),
      isactiveflag = list(type = "logical", description = "Is active flag"),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      )
    ),
    list(pk = "drillerid")
  ),

  DrillingCompanies = create_table_metadata(
    "Drilling Companies",
    "Drilling company information including company name and contact details",
    list(
      drillingcompanyid = list(
        type = "integer",
        description = "Primary key uniquely identifying the record"
      ),
      companyname = list(
        type = "character",
        description = "Name of the drilling company"
      ),
      streetaddress = list(
        type = "character",
        description = "Street address of the drilling company"
      ),
      city = list(
        type = "character",
        description = "City of the drilling company"
      ),
      province = list(
        type = "character",
        description = "Province of the drilling company"
      ),
      country = list(
        type = "character",
        description = "Country of the drilling company"
      ),
      postalcode = list(
        type = "character",
        description = "Postal code of the drilling company"
      ),
      email = list(
        type = "character",
        description = "Email of the drilling company"
      ),
      gwrisid = list(
        type = "character",
        description = "GWRIS ID of the drilling company"
      ),
      isactiveflag = list(type = "logical", description = "Is active flag"),
      startingwellid = list(type = "integer", description = "Starting Well_ID"),
      endingwellid = list(type = "integer", description = "Ending Well_ID"),
      lastwellidused = list(
        type = "integer",
        description = "Last Well_ID used"
      ),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      )
    ),
    list(pk = "drillingcompanyid")
  ),

  GeophysicalLogs = create_table_metadata(
    "Geophysical Logs",
    "Geophysical log types and whether they were taken and sent to AENV",
    list(
      geophysicallogid = list(
        type = "integer",
        description = "Primary key uniquely identifying the record"
      ),
      wellreportid = list(
        type = "integer",
        description = "Foreign key to the Well_Reports table"
      ),
      logtype = list(
        type = "character",
        description = "Type of geophysical log"
      ),
      logtakenflag = list(
        type = "logical",
        description = "TRUE is log was taken"
      ),
      senttoaenvflag = list(
        type = "logical",
        description = "TRUE is log was sent to AENV"
      ),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      )
    ),
    list(pk = "geophysicallogid", fk = c("wellreportid"))
  ),

  Lithologies = create_table_metadata(
    "Lithologies",
    "Lithologic interval descriptions",
    list(
      lithologyid = list(
        type = "integer",
        description = "Primary key uniquely identifying the record"
      ),
      wellreportid = list(
        type = "integer",
        description = "Foreign key to the WellReports Table"
      ),
      lithdepthfrom = list(
        type = "units",
        description = "The depth to the top of a discrete lithologic material encountered during drilling"
      ),
      lithdepthto = list(
        type = "units",
        description = "The depth to the bottom of a discrete lithologic material encountered during drilling"
      ),
      waterbearing = list(
        type = "logical",
        description = "TRUE if the material is water bearing"
      ),
      colour = list(
        type = "character",
        description = "The colour of the material in the lithologic section"
      ),
      material = list(
        type = "character",
        description = "The lithologic material encountered during drilling"
      ),
      description = list(
        type = "character",
        description = "Description of the lithologic material"
      ),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      )
    ),
    list(pk = "lithologyid", fk = c("wellreportid"))
  ),

  OtherSeals = create_table_metadata(
    "Other Seals",
    "Seal information for seals other than annular seals",
    list(
      othersealid = list(
        type = "integer",
        description = "Primary key uniquely identifying the record"
      ),
      wellreportid = list(
        type = "integer",
        description = "Foreign key to the Well_Reports table"
      ),
      othersealtype = list(
        type = "character",
        description = "Seal Type (other than annular)"
      ),
      sealdepthfrom = list(
        type = "units",
        description = "Top of the seal as measured from ground level"
      ),
      sealdepthto = list(
        type = "units",
        description = "Bottom of the seal as measured from ground level"
      ),
      at = list(
        type = "units",
        description = "Depth at which the seal is placed"
      ),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      )
    ),
    list(pk = "othersealid", fk = c("wellreportid"))
  ),

  Perforations = create_table_metadata(
    "Perforations",
    "Perforation depths, diameters, and intervals for water wells",
    list(
      perforationid = list(
        type = "integer",
        description = "Primary key uniquely identifying the record"
      ),
      wellreportid = list(
        type = "integer",
        description = "Foreign key to the Well_Reports table"
      ),
      distancebetween = list(
        type = "units",
        description = "Distance between perforations"
      ),
      perfdepthfrom = list(
        type = "units",
        description = "Top of the perforations as measured from ground level"
      ),
      perfdepthto = list(
        type = "units",
        description = "Bottom of the perforations as measured from ground level"
      ),
      perfdiameter = list(
        type = "units",
        description = "Diameter of each perforation hole or length of each slot"
      ),
      perfinterval = list(
        type = "units",
        description = "Distance between performations"
      ),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      )
    ),
    list(pk = "perforationid", fk = c("wellreportid"))
  ),

  PumpTests = create_table_metadata(
    "Pump Tests",
    "Pump test data for water wells",
    list(
      pumptestid = list(
        type = "integer",
        description = "Primary key uniquely identifying the record"
      ),
      wellreportid = list(
        type = "integer",
        description = "Foreign key to the Well_Reports table"
      ),
      testdate = list(
        type = "POSIXct",
        description = "The date the well was tested"
      ),
      starttime = list(
        type = "character",
        description = "The time of day the well was tested. Use a 24 hour clock"
      ),
      takenfromtopofcasing = list(
        type = "units",
        description = "True if the pump test depth measurements were taken from the top of the casing.False if they were taken from ground level."
      ),
      staticwaterlevel = list(
        type = "units",
        description = "The non-pumping or static water level in the well. The measurement is a positive number and is usually measured from the top of the casing. For a flowing well, the S.W.L. is preceded by a negative sign."
      ),
      endwaterlevel = list(
        type = "units",
        description = "The level of the water at the end of the pump test"
      ),
      waterremovaltype = list(
        type = "character",
        description = "The method of testing the well i.e. pump, air"
      ),
      waterremovalrate = list(
        type = "units",
        description = "Water_Removal_Rate	The rate of water removal during a test"
      ),
      removaldepthfrom = list(
        type = "units",
        description = "The depth at which the bottom of the drill stem is placed when testing the well with air (normally the bottom of the hole). If the well is pump tested then it is the depth at which the pump is set"
      ),
      reasonforshorttest = list(
        type = "character",
        description = "Reason why the water removal test was shorter than required by regulation."
      ),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      )
    ),
    list(pk = "pumptestid", fk = c("wellreportid"))
  ),

  PumpTestItems = create_table_metadata(
    "Pump Test Items",
    "Additional pump test data including pumping minutes and recovery measurements",
    list(
      pumptestitemid = list(
        type = "integer",
        description = "Primary key uniquely identifying the record"
      ),
      pumptestid = list(
        type = "integer",
        description = "Foreign key to the PumpTests table"
      ),
      minutes = list(
        type = "integer",
        description = "The minutes from the start of the test of the drawdown or recovery measurements"
      ),
      pumpingdepth = list(
        type = "units",
        description = "The water level at a particular time during the drawdown period (pumping) of the water removal test"
      ),
      recoverydepth = list(
        type = "units",
        description = "The water level at a particular time during the recovery period (after the pump is turned off) of the water removal test"
      ),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      )
    ),
    list(pk = "pumptestitemid", fk = c("pumptestid"))
  ),

  Screens = create_table_metadata(
    "Screens",
    "Screen depth, diameter, and slot size information for water wells",
    list(
      screenid = list(
        type = "integer",
        description = "Primary key uniquely identifying the record"
      ),
      wellreportid = list(
        type = "integer",
        description = "Foreign key to the Well_Reports table"
      ),
      screeninsidediameter = list(
        type = "units",
        description = "Inside diameter of the screen"
      ),
      screendepthfrom = list(
        type = "units",
        description = "Top of the screen as measured from ground level"
      ),
      screendepthto = list(
        type = "units",
        description = "Bottom of the screen as measured from ground level"
      ),
      slotsize = list(type = "units", description = "Slot size of the screen"),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      )
    ),
    list(pk = "screenid", fk = c("wellreportid"))
  ),

  Wells = create_table_metadata(
    "Wells",
    "Well location and identification information",
    list(
      wellid = list(
        type = "integer",
        description = "Primary key uniquely identifying the record"
      ),
      drillingcompanyid = list(
        type = "integer",
        description = "Foreign key to the DrillingCompanies table"
      ),
      gicwellid = list(
        type = "integer",
        description = "Well Number that uniquely identifies a well"
      ),
      goawelltagnumber = list(
        type = "character",
        description = "Not currently in use"
      ),
      longitude = list(
        type = "double",
        description = "Longitude in decimal degrees"
      ),
      latitude = list(
        type = "double",
        description = "Latitude in decimal degrees"
      ),
      elevation = list(
        type = "units",
        description = "Elevation in metres above sea level"
      ),
      gpsobtained = list(
        type = "character",
        description = "How the GPS coordinates were obtained"
      ),
      distance_north = list(
        type = "units",
        description = "Distance_North	Distance from the north boundary"
      ),
      distance_south = list(
        type = "units",
        description = "Distance from the south boundary"
      ),
      distance_east = list(
        type = "units",
        description = "Distance from the east boundary"
      ),
      distance_west = list(
        type = "units",
        description = "Distance from the west boundary"
      ),
      lsd = list(type = "character", description = "Legal Subdivision"),
      section = list(type = "integer", description = "Section"),
      township = list(type = "integer", description = "Township"),
      range = list(type = "integer", description = "Range"),
      meridian = list(type = "integer", description = "Meridian"),
      lot = list(type = "character", description = "Lot"),
      block = list(type = "character", description = "Block"),
      plan = list(type = "character", description = "Plan"),
      additionaldescription = list(
        type = "character",
        description = "Additional description of the well location"
      ),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      )
    ),
    list(pk = "wellid", fk = c("drillingcompanyid"))
  ),

  UnitOptions = create_table_metadata(
    "Unit Options",
    "Lookup table for units of measurement",
    list(
      unitoptionid = list(
        type = "integer",
        description = "Primary key uniquely identifying the record"
      ),
      name = list(type = "character", description = "Unit option name"),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      )
    ),
    list(pk = "unitoptionid")
  ),

  PlugMaterialOptions = create_table_metadata(
    "Plug Material Options",
    "Lookup table for plug material types",
    list(
      plugmaterialoptionid = list(
        type = "integer",
        description = "Primary key uniquely identifying the record"
      ),
      name = list(
        type = "character",
        description = "Plug material option name"
      ),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      )
    ),
    list(pk = "plugmaterialoptionid")
  ),

  WellMaterialsLogs = create_table_metadata(
    "Well Materials Logs",
    "Well construction material depth and type information",
    list(
      wellmateriallogid = list(
        type = "integer",
        description = "Primary key uniquely identifying the record"
      ),
      welldecommissioningdetailid = list(
        type = "integer",
        description = "Foreign key to the WellDecommissioningDetailed table"
      ),
      wellmaterialdepthfrom = list(
        type = "units",
        description = "Top of the material as measured from ground level"
      ),
      wellmaterialdepthto = list(
        type = "units",
        description = "Bottom of the material as measured from ground level"
      ),
      materialoptionid = list(
        type = "integer",
        description = "Foreign key to the MaterialOptions table"
      ),
      placementmethodoptionid = list(
        type = "integer",
        description = "Foreign key to the PlacementMethodOptions table"
      ),
      amount = list(type = "numeric", description = "Amount of material used"),
      unitoptionid = list(
        type = "integer",
        description = "Foreign key to the UnitOptions table"
      ),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      )
    ),
    list(
      pk = "wellmateriallogid",
      fk = c(
        "welldecommissioningdetailid",
        "materialoptionid",
        "placementmethodoptionid",
        "unitoptionid"
      )
    )
  ),

  WellDecommissioningDetails = create_table_metadata(
    "Well Decommissioning Details",
    "This table contains detailed information about well decommissioning activities",
    list(
      welldecommissioningdetailid = list(
        type = "integer",
        description = "Primary key uniquely identifying the record"
      ),
      wellreportid = list(
        type = "integer",
        description = "Foreign key to the Well_Reports table"
      ),
      workcompletedate = list(
        type = "POSIXct",
        description = "The date the well decommissioning work was completed"
      ),
      currentstaticwaterlevel = list(
        type = "units",
        description = "The current static water level in the well"
      ),
      currentwelldepth = list(
        type = "units",
        description = "The current depth of the well"
      ),
      welldecommissioningreasonid = list(
        type = "integer",
        description = "Foreign key to the WellDecommissioningReasons table"
      ),
      otherreasons = list(
        type = "character",
        description = "Other reasons for decommissioning the well"
      ),
      iswelldisinfectedpriortoplugging = list(
        type = "logical",
        description = "TRUE if the well was disinfected prior to plugging"
      ),
      ispumpremoved = list(
        type = "logical",
        description = "TRUE if the pump was removed from the well"
      ),
      pumpnotremovedexplanation = list(
        type = "character",
        description = "Explanation why the pump was not removed from the well"
      ),
      iscasingcutoffbelowgroundlevel = list(
        type = "logical",
        description = "TRUE if the casing was cut off below ground level"
      ),
      casingcutoffbelowgroundlevel = list(
        type = "units",
        description = "The depth below ground level that the casing was cut off"
      ),
      casingnotcutoffexplanation = list(
        type = "character",
        description = "Explanation why the casing was not cut off below ground level"
      ),
      additionalcomments = list(
        type = "character",
        description = "Additional comments"
      ),
      iswellreportcopygiventoowner = list(
        type = "logical",
        description = "TRUE if a copy of the well report was given to the owner"
      ),
      iscertify = list(
        type = "logical",
        description = "TRUE if the driller certifies the information is correct"
      ),
      personresponsible = list(
        type = "character",
        description = "Name of the person responsible for the decommissioning"
      ),
      iscompletedbyowner = list(
        type = "logical",
        description = "TRUE if the decommissioning was completed by the owner"
      ),
      iscompletedbydriller = list(
        type = "logical",
        description = "TRUE if the decommissioning was completed by the driller"
      ),
      certificationnotice = list(
        type = "character",
        description = "Certification notice text"
      ),
      companyname = list(
        type = "character",
        description = "Name of the company"
      ),
      datereportreceived = list(
        type = "POSIXct",
        description = "The date the well report was received"
      ),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      )
    ),
    list(
      pk = "welldecommissioningdetailid",
      fk = c("wellreportid", "welldecommissioningreasonid")
    )
  ),

  WellDecommissioningReasons = create_table_metadata(
    "Well Decommissioning Reasons",
    "Lookup table for well decommissioning reason types",
    list(
      welldecommissioningreasonid = list(
        type = "integer",
        description = "Primary key uniquely identifying the record"
      ),
      name = list(
        type = "character",
        description = "Well decommissioning reason name"
      ),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      )
    ),
    list(pk = "welldecommissioningreasonid")
  ),

  WellOwners = create_table_metadata(
    "Well Owners",
    "This table contains information about well owners at the time of drilling",
    list(
      wellownerid = list(
        type = "integer",
        description = "Primary key uniquely identifying the record"
      ),
      wellid = list(
        type = "integer",
        description = "Foreign key to the Wells table"
      ),
      ownername = list(
        type = "character",
        description = "The owner of the well at the time of drilling"
      ),
      address = list(
        type = "character",
        description = "Address of the well owner"
      ),
      city = list(type = "character", description = "City of the well owner"),
      postalcode = list(
        type = "character",
        description = "Postal code of the well owner"
      ),
      province = list(
        type = "character",
        description = "Province of the well owner"
      ),
      country = list(
        type = "character",
        description = "Country of the well owner"
      ),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      )
    ),
    list(pk = "wellownerid", fk = c("wellid"))
  ),

  WellReports = create_table_metadata(
    "Well Reports",
    "Water well reports submitted by drillers",
    list(
      wellreportid = list(
        type = "integer",
        description = "Primary key uniquely identifying the record"
      ),
      wellid = list(
        type = "integer",
        description = "Foreign key to the Wells table"
      ),
      wellownerid = list(
        type = "integer",
        description = "Foreign key to the WellOwners table"
      ),
      drillerid = list(
        type = "integer",
        description = "Foreign key to the Drillers table"
      ),
      drillingcompanyid = list(
        type = "integer",
        description = "Foreign key to the DrillingCompanies table"
      ),
      drillerinstanceid = list(
        type = "integer",
        description = "Instance of the driller"
      ),
      datereceived = list(
        type = "POSIXct",
        description = "The date the well report was received"
      ),
      drillingmethod = list(
        type = "character",
        description = "The method of drilling such as a boring rig, rotary rig, backhoe"
      ),
      typeofwork = list(
        type = "character",
        description = "The type of work done on the well such as New Well, Deepening, Dry Hole"
      ),
      plugdate = list(
        type = "POSIXct",
        description = "Date the well was plugged"
      ),
      plugmaterialtype = list(
        type = "character",
        description = "Material the well was plugged with"
      ),
      plugmaterialamount = list(
        type = "numeric",
        description = "Amount of material used in plugging the well"
      ),
      plugunits = list(
        type = "character",
        description = "Units of the plug material used to plug the well"
      ),
      existingwellid = list(
        type = "integer",
        description = "If the well is a deepening or re-entry, the existing well ID"
      ),
      welluse = list(
        type = "character",
        description = "The proposed use of the water well such as domestic, injection, standby"
      ),
      otherwelluse = list(type = "character", description = "Other well use"),
      totaldepthdrilled = list(
        type = "units",
        description = "Total depth drilled"
      ),
      finishedwelldepth = list(
        type = "units",
        description = "Depth of the completed well"
      ),
      drillingstartdate = list(
        type = "POSIXct",
        description = "The date the drilling started"
      ),
      drillingenddate = list(
        type = "POSIXct",
        description = "The date the drilling ended"
      ),
      casingmaterial = list(
        type = "character",
        description = "The type of casing material of the first string of casing used in constructing the water well, i.e. steel, cribbing, culvert"
      ),
      casingod = list(type = "units", description = "Casing outside diameter"),
      casingthickness = list(type = "units", description = "Casing thickness"),
      casingbottom = list(
        type = "units",
        description = "The depth from surface to the bottom of the casing"
      ),
      linermaterial = list(
        type = "character",
        description = "The type of liner material"
      ),
      linerod = list(
        type = "units",
        description = "Outside diameter of the liner"
      ),
      linerthickness = list(
        type = "units",
        description = "Thickness of the liner"
      ),
      linertop = list(
        type = "units",
        description = "Top of the liner as measured from ground level. If the liner is above ground then a negative sign is used to indicate the distance above ground"
      ),
      linerbottom = list(
        type = "units",
        description = "Bottom of the liner as measured from ground level"
      ),
      perforationby = list(
        type = "character",
        description = "Object or tool used to perforate the liner"
      ),
      annularsealmaterial = list(
        type = "character",
        description = "Material the first seal is composed of"
      ),
      annularsealfrom = list(
        type = "units",
        description = "Top of the first seal as measured from ground level"
      ),
      annularsealto = list(
        type = "units",
        description = "Bottom of the first seal as measured from ground level"
      ),
      annularsealamount = list(
        type = "numeric",
        description = "Amount of material used in the first seal"
      ),
      annularsealunits = list(
        type = "character",
        description = "Units the amount of material is measured in"
      ),
      screenmaterial = list(
        type = "character",
        description = "The screen material (Stainless Steel or PVC, etc.)"
      ),
      screensizeod = list(
        type = "units",
        description = "The outside diameter of the screen"
      ),
      screenattachment = list(
        type = "character",
        description = "How the screen is attached (Telescoped, Washed Down, etc.)"
      ),
      screentopfittings = list(
        type = "character",
        description = "Material the top fittings of the screen is composed of"
      ),
      screenbottomfittings = list(
        type = "character",
        description = "Material the bottom fittings of the screen is composed of"
      ),
      packtype = list(
        type = "character",
        description = "Material the pack is made of"
      ),
      packgrainsize = list(
        type = "character",
        description = "The grain size of the pack material"
      ),
      packamount = list(
        type = "numeric",
        description = "The amount of packing material used"
      ),
      packunits = list(
        type = "character",
        description = "The units the packing material is measured (Bags, Kg, etc.)"
      ),
      locationverificationmethod = list(
        type = "character",
        description = "The method used to verify the well location"
      ),
      distancecasingground = list(
        type = "units",
        description = "The distance the casing is from the ground level"
      ),
      artesianflowflag = list(
        type = "logical",
        description = "Is the well an artesian well"
      ),
      artesianflowrate = list(
        type = "units",
        description = "The rate of flow from an artesian well"
      ),
      encountersalinewaterflag = list(
        type = "logical",
        description = "TRUE if saline water was encountered during drilling"
      ),
      salinewaterdepth = list(
        type = "units",
        description = "The depth at which saline water was encountered"
      ),
      gasdepth = list(
        type = "units",
        description = "The depth at which gas was encountered"
      ),
      encountergasflag = list(
        type = "logical",
        description = "TRUE if gas was encountered during drilling"
      ),
      remedialaction = list(
        type = "character",
        description = "Remedial action taken during drilling"
      ),
      flowcontrolinstalledflag = list(
        type = "logical",
        description = "True if artesian flow control was installed"
      ),
      flowcontroldescription = list(
        type = "character",
        description = "Description of the artesian flow control installed"
      ),
      recommendedrate = list(
        type = "units",
        description = "Driller recommended rate of water removal"
      ),
      recommendedintakedepth = list(
        type = "units",
        description = "Driller recommended depth of pump intake"
      ),
      pumpinstalledflag = list(
        type = "logical",
        description = "TRUE if a pump was installed in the well"
      ),
      pumpinstalleddepth = list(
        type = "units",
        description = "The depth the pump was installed"
      ),
      pumptypeinstalled = list(
        type = "character",
        description = "The type of pump installed in the well"
      ),
      pumpmodel = list(
        type = "character",
        description = "The model of the pump installed in the well"
      ),
      pumphorsepower = list(
        type = "numeric",
        description = "The horsepower of the pump installed in the well"
      ),
      welldisinfected = list(
        type = "logical",
        description = "TRUE if the well was disinfected"
      ),
      otherlog = list(
        type = "character",
        description = "Type of additional log taken"
      ),
      potabilitysampletakenflag = list(
        type = "logical",
        description = "True if a potability sample was taken for chemical analysis"
      ),
      potabilitysamplesenttoaenvflag = list(
        type = "logical",
        description = "True if the potability sample was sent to AENV"
      ),
      divertedwatersource = list(
        type = "character",
        description = "Source of the water diverted during drilling"
      ),
      divertedwateramount = list(
        type = "numeric",
        description = "Amount of water diverted during drilling"
      ),
      diversiondatetime = list(
        type = "POSIXct",
        description = "Date and time the water was diverted"
      ),
      createdby = list(
        type = "character",
        description = "User who created the record"
      ),
      issubmitted = list(
        type = "logical",
        description = "TRUE if the report has been officially submitted"
      ),
      submittedby = list(
        type = "character",
        description = "User who submitted the report"
      ),
      isvalidated = list(
        type = "logical",
        description = "TRUE if the report has been validated"
      ),
      additionalcomments = list(
        type = "character",
        description = "Additional comments"
      ),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      ),
      approvalholdersignaturedate = list(
        type = "POSIXct",
        description = "The date the approval holder signed the report"
      ),
      drillingreportgiventoowner = list(
        type = "logical",
        description = "TRUE if a copy of the drilling report was given to the owner"
      ),
      modeloutputrating = list(
        type = "character",
        description = "Model output rating"
      ),
      drillingcompanywellid = list(
        type = "character",
        description = "Well ID assigned by the drilling company"
      )
    ),
    list(
      pk = "wellreportid",
      fk = c("wellid", "wellownerid", "drillerid", "drillingcompanyid")
    )
  ),

  PlacementMethodOptions = create_table_metadata(
    "Placement Method Options",
    "Lookup table for placement method types",
    list(
      placementmethodoptionid = list(
        type = "integer",
        description = "Primary key uniquely identifying the record"
      ),
      name = list(
        type = "character",
        description = "Placement method option name"
      ),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      )
    ),
    list(pk = "placementmethodoptionid")
  ),

  CasingStatus = create_table_metadata(
    "Casing Status",
    "Lookup table for casing status types",
    list(
      casingstatusid = list(
        type = "integer",
        description = "Primary key uniquely identifying the record"
      ),
      name = list(type = "character", description = "Casing status name"),
      createtimestamp = list(
        type = "POSIXct",
        description = "Creation timestamp"
      ),
      updatetimestamp = list(
        type = "POSIXct",
        description = "Updated timestamp"
      )
    ),
    list(pk = "casingstatusid")
  )
)

# extract_column_metadata <- function(name, table) {
#   x = table$columns
#   table_description = table$description
#
#   tibble(
#     table_name = gsub("_", "", name),
#     table_description = table_description,
#     column = names(x),
#     type = sapply(x, function(item) item$type),
#     description = sapply(x, function(item) item$description)
#   )
# }
#
# metadata <- mapply(extract_column_metadata, names(awwid_metadata), awwid_metadata, SIMPLIFY = FALSE)
# metadata <- do.call(rbind, metadata)
# metadata <- metadata |>
#   tidyr::nest(attributes = c(column, type, description))

metadata <- awwid_metadata
usethis::use_data(metadata, overwrite = TRUE)
