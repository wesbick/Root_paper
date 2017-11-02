
leco16 <- read.csv("site_nutrients/LECO/Bickford_Dec16.csv", header = F, sep = ",", stringsAsFactors = F)
colnames(leco16) <- c("Study", "SiteID", "mass", "PerN", "PerC", "Date")

leco_soils_15 <- leco16[leco16$Study == "BIckford_15_Soils", ]
leco_soils_16 <- leco16[leco16$Study == "BIckford_16_Soils", ]
leco_plant_16 <- leco16[leco16$Study == "BIckford_16_Plant", ]

