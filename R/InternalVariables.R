# ==============================================================================
# store the URL to the ENTSO-E user guide and the component tables
cEntsoeUserGuideUrl <-
  "https://transparency.entsoe.eu/content/static_content/Static%20content/web%20api/Guide.html#_complete_parameter_list"
lMappingOfTablesToXpaths <- list(
  "available_parameters" = "/html/body/div[2]/section/div[1]/div[2]/div[4]/div/div[7]/div/div/div[2]/div/div[1]/table",
  "available_parameters" = "/html/body/div[2]/section/div[1]/div[2]/div[4]/div/div[7]/div/div/div[2]/div/div[1]/table/tbody",
  "Contract_MarketAgreement.Type" = "/html/body/div[2]/section/div[1]/div[2]/div[4]/div/div[7]/div/div/div[2]/div/div[2]/table",
  "Type_MarketAgreement.Type" = "/html/body/div[2]/section/div[1]/div[2]/div[4]/div/div[7]/div/div/div[2]/div/div[2]/table",
  "Auction.Type" = "/html/body/div[2]/section/div[1]/div[2]/div[4]/div/div[7]/div/div/div[2]/div/div[3]/table",
  "Auction.Category" = "/html/body/div[2]/section/div[1]/div[2]/div[4]/div/div[7]/div/div/div[2]/div/div[4]/table",
  "PsrType" = "/html/body/div[2]/section/div[1]/div[2]/div[4]/div/div[7]/div/div/div[2]/div/div[5]/table",
  "BusinessType" = "/html/body/div[2]/section/div[1]/div[2]/div[4]/div/div[7]/div/div/div[2]/div/div[6]/table",
  "ProcessType" = "/html/body/div[2]/section/div[1]/div[2]/div[4]/div/div[7]/div/div/div[2]/div/div[7]/table",
  "DocStatus" = "/html/body/div[2]/section/div[1]/div[2]/div[4]/div/div[7]/div/div/div[2]/div/div[9]/table",
  "DocumentType" = "/html/body/div[2]/section/div[1]/div[2]/div[4]/div/div[7]/div/div/div[2]/div/div[9]/table",
  "Areas" = "/html/body/div[2]/section/div[1]/div[2]/div[4]/div/div[7]/div/div/div[2]/div/div[10]/table"
)
charSecurityToken <- "f5264b37-ebd8-462c-97d5-73de3d4af7a3"
# ==============================================================================



# ==============================================================================
lDictionaryBiddingZones <- list(
  "Poland" = "10YPL-AREA-----S",
  "Lithuania" = "10YLT-1001A0008Q",
  "Slovakia" = "10YSK-SEPS-----K",
  "Belarus" = "10Y1001A1001A51S",
  "CzechRepublic" = "10YCZ-CEPS-----N",
  "Kaliningrad" = "10Y1001A1001A50U",
  "Ukraine" = "10Y1001C--00003F",
  "Ukraine_DobTPP" = "10Y1001A1001A869",
  "Ukraine_BEI" = "10YUA-WEPS-----0",
  "Ukraine_IPS" = "10Y1001C--000182",
  "Amprion" = "10YDE-RWENET---I",
  "50Hertz" = "10YDE-VE-------2",
  "TenneT" = "10YDE-EON------1",
  "TransnetBW" = "10YDE-ENBW-----N",
  "Luxembourg" = "10YLU-CEGEDEL-NQ"
)
# ==============================================================================



# ==============================================================================
lDictionaryContractMarketAgreementType <- list(
  "daily" = "A01",
  "weekly" = "A02",
  "monthly" = "A03",
  "yearly" = "A04"
)
# ==============================================================================



# ==============================================================================
lDictionaryAuctionCategory <- list(
  "base" = "A01",
  "peak" = "A02",
  "offpeak" = "A03",
  "hourly" = "A04"
)
# ==============================================================================



# ==============================================================================
lDictionaryPsrType <- list(
  "mixed" = "A03",
  "generation" = "A04",
  "load" = "A05",
  "biomass" = "B01",
  "fossilBrownCoalOrLignite" = "B02",
  "fossilCoalDerivedGas" = "B03",
  "fossilGas" = "B04",
  "fossilHardCoal" = "B05",
  "fossilOil" = "B06",
  "fossilOilShale" = "B07",
  "fossilPeat" = "B08",
  "geothermal" = "B09",
  "hydroPumpedStorage" = "B10",
  "hydroRunOfRiverAndPoundage" = "B11",
  "hydroWaterReservoir" = "B12",
  "marine" = "B13",
  "nuclear" = "B14",
  "otherRenewable" = "B15",
  "solar" = "B16",
  "waste" = "B17",
  "windOffshore" = "B18",
  "windOnshore" = "B19",
  "other" = "B20",
  "AC_link" = "B21",
  "DC_link" = "B22",
  "substation" = "B23",
  "transformer" = "B24"
)
# ==============================================================================



# ==============================================================================
lDictionaryDocumentType <- list(
  "systemTotalLoad" = "A65",
  "generationForecast" = "A71",
  "actualGeneration" = "A73",
  "loadUnavailability" = "A76",
  "windAndSolarForecast" = "A69",
  "installedGenerationPerType" = "A68",
  "imbalanceVolume" = "A86"
)
# ==============================================================================

