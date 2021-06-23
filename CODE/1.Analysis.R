# Load data
data <- read.csv("DATA/Data_for_analysis.csv")
head(data)

### VARIABLES OF INTEREST
### Annual increment
# data$ICCapv_ha # Current annual volume increment of living trees
# data$ICWapv_ha # Dry weight correspondent to the current annual volume increment of living trees
# data$ICVapv_ha # Organic carbon stock of current annual volume increment of living trees

### Stock (sum?)
# data$Capv_ha # Organic carbon stock of total above-ground biomass of living trees
# data$Capm_ha # Organic carbon stock of the total above-ground biomass of standing dead trees

### Climate
# data$codcfor # Forest type from INFI data
# data$climate_classification # Climate class (1=Mediterranean, 0=temperate)
# data$vpd # Mean annual vapor pressure deficit
# data$soilmoisture # Mean annual soil moisture

### Functional diversity / composition
# data$FDis # Functional dispersion
# data$SpRich # Species richness in the plot
# data$cwm_SeedMass_log # community-weighted mean of log seed mass
# data$cwm_Height_log # community-weighted mean of log height
# data$cwm_SLA_log # community-weighted mean of log SLA
# data$cwm_StemDensity # community-weighted mean of wood density
# data$cwm_XylemVulnerability # community-weighted mean of p50

### QUESTIONS
# How are functional composition (CWM) and diversity (FDisp) related to climate?
# Do plots with higher *response variable* have higher/lower CWM values?
# Do plots with higher *response variable* have higher functional diversity?
# Does this relationship differ between Mediterranean and temperate climates?
# Do other climate variables mediate the relationship between FD and *response*?


