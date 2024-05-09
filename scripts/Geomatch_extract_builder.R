setwd("C:/Users/AhyoungLim/Dropbox/WORK/Dengue Project 2022/11_DENData")
# setwd("/Users/eideobra/Dropbox/11_DENData")
rm(list= ls())
require(stringi)
require(stringdist)
require(sf)
require(lubridate)
require(rnaturalearth)
require(dplyr)
# remotes::install_github("epicentre-msf/hmatch")
require(hmatch)
require(countrycode)
require(mgcv)
require(lubridate)
require(readxl)

# load in master openDengue file
# od <- read.csv("/Users/eideobra/Documents/GitHub/OpenDengue/master-repo/data/raw_data/masterDB_V1.1.csv")
od <- read.csv("01_Dengue_data/OD_master/OD_V1.2/masterDB_V1.2.csv")


summary(is.na(od))
### Location information checking

# 01 character names

# basic standardisation first

# remove accents
od$adm_1_name = stri_trans_general(str = od$adm_1_name, id = "Latin-ASCII")
od$adm_2_name = stri_trans_general(str = od$adm_2_name, id = "Latin-ASCII")

# for some reason some latin accents need to be changed manually (colombia, guatemala)
unique(od$adm_1_name[grepl("�", od$adm_1_name)])
unique(od$adm_2_name[grepl("�", od$adm_2_name)])

pattern_list <- list(
N = c("NARI�O" ,"SALDA�A" , "EL PE�OL" ,"CA�A" ,  "MO�ITOS",
       "COVE�AS", "PIJI�O DEL CARMEN (PIJI�O)" , "PUERTO NARI�O", 
       "CA�ASGORDAS","LA PE�A", "PUERTO CARRE�O","EL PE�ON" , "PE�OL" , "EL PI�ON" ) ,           
E = c("GUACHEN�", "BEL�N DE BAJIRA","Pet�n Norte", "Pet�n Sur Occidental","Pet�n Suroccidental",
       "Pet�n Sur Oriental"  ,"El Quich�", "Sacatep�quez", "Pet�n Suroriental"), 
I = c("NOROS�" ) , 
A = c("Ixc�n" ,"Totonicap�n","Solol�") 
)

clean_strings <- function(od, pattern_list) {
  for (char in names(pattern_list)) {
    strings <- paste(pattern_list[[char]], collapse = "|")
    od$adm_1_name[grepl(strings, od$adm_1_name)] <-
      gsub("�", char, od$adm_1_name[grepl(strings, od$adm_1_name)])
    od$adm_2_name[grepl(strings, od$adm_2_name)] <-
      gsub("�", char, od$adm_2_name[grepl(strings, od$adm_2_name)])
  }
  return(od)
}

od <- clean_strings(od, pattern_list)

od$adm_2_name[grepl("DEL CARMEN", od$adm_2_name)] <- "PIJINO DEL CARMEN (PIJINO)"



# convert all to upper case
od$adm_0_name = toupper(od$adm_0_name)
od$adm_1_name = toupper(od$adm_1_name)
od$adm_2_name = toupper(od$adm_2_name)

# remove any double spacing
od$adm_0_name = gsub("  ", " ", od$adm_0_name)
od$adm_1_name = gsub("  ", " ", od$adm_1_name)
od$adm_2_name = gsub("  ", " ", od$adm_2_name)

# trim trailing space
od$adm_0_name = trimws(od$adm_0_name)
od$adm_1_name = trimws(od$adm_1_name)
od$adm_2_name = trimws(od$adm_2_name)

od$adm_2_name[od$adm_2_name == ""] = NA


# function that returns a table of most similar links within a list of county
# or admin 1/2 names
find_similar <- function(a1c){
  # unique names
  ua1n <- sort(unique(a1c))
  # similar matches
  distmatrix <- stringdist::stringdistmatrix(ua1n, ua1n, method='jw')
  diag(distmatrix) = NA # excludes direct matches
  best_fit <- apply(distmatrix, 1, which.min) # best match
  similarity <- apply(distmatrix,1,min, na.rm = T) # match value
  
  # build table of most similar non identical matches
  match_tab <- data.frame(original = ua1n,
                          match = ua1n[best_fit],
                          distance = similarity)
  # sort table most to least similar
  match_tab = match_tab[order(match_tab$distance, decreasing = F), ]
  return(match_tab)
}


# A) national level
# unique names
ucn <- sort(unique(od$adm_0_name))
# similar matches
c_match <- find_similar(ucn)
# manually inspect for duplication
head(c_match, n = 20)
od$adm_0_name[od$adm_0_name == "VIETNAM"] = "VIET NAM"
od$adm_0_name[od$adm_0_name == "WALLIS & FUTUNA"] = "WALLIS AND FUTUNA"
od$adm_0_name[od$adm_0_name == "PITCAIRN ISLANDS"] = "PITCAIRN"
od$adm_0_name[od$adm_0_name == "PNG"] = "PAPUA NEW GUINEA"
od$adm_0_name[od$adm_0_name == "FSM"] = "MICRONESIA (FEDERATED STATES OF)"
od$adm_0_name[od$adm_0_name == "N MARIANA IS"] = "NORTHERN MARIANA ISLANDS"
od$adm_0_name[od$adm_0_name == "ECUARDOR"] = "ECUADOR"


# B) admin 1 level
ad_a1 <- od[!is.na(od$adm_1_code), ]
# combine with country name
a1c <- apply(ad_a1[, c("adm_0_name", "adm_1_name")],
             1,
             paste, collapse = "/")
# similar matches
a1_match <- find_similar(a1c)
# manually inspect for duplication
head(a1_match, n = 20)
#! "PANAMA/VERGAUAS" -> "PANAMA/VERAGUAS"
od$adm_1_name[od$adm_1_name == "VERGAUAS"] = "VERAGUAS"
# od <- od %>% filter(!od$adm_1_name == "AL-SHAMAL") # probably adm1 name of QATAR

# C) admin 2 level
ad_a2 <- od[!is.na(od$adm_2_code), ]
# combine with admin 1 name
a2c <- apply(ad_a2[, c("adm_1_name", "adm_2_name")],
             1,
             paste, collapse = "/")
# similar matches
a2_match <- find_similar(a2c)
# manually inspect for duplication
head(a2_match, n = 20)
#! no obvious mismatches
od$adm_2_name[od$adm_1_name == "BUENOS AIRES" & od$adm_2_name == "PILA"] = "PILAR"
od$adm_2_name[od$adm_1_name == "ENTRE RIOS" & od$adm_2_name == "FEDERAL"] = "FEDERACION"
od$adm_2_name[od$adm_0_name == "COLOMBIA"] <- gsub(" \\(CD\\)", "", od$adm_2_name[od$adm_0_name == "COLOMBIA"])



# identify spatial and temporal resolution
od$S_res <- apply(od[, c("adm_0_name", "adm_1_name", "adm_2_name")],
                  1,
                  function(x) c("Admin2", "Admin1", "Admin0")[sum(is.na(x)) + 1])
od$S_res = factor(od$S_res, levels = c("Admin2", "Admin1", "Admin0"))

od$T_res <- as.numeric(as.Date(od$calendar_end_date) - as.Date(od$calendar_start_date))
od$T_res[od$T_res < 8] = 1
od$T_res[(od$T_res > 1) & (od$T_res < 300)] = 2
od$T_res[od$T_res >= 300] = 3
od$T_res = c("Week", "Month", "Year")[od$T_res]
od$T_res = factor(od$T_res, levels = c("Week", "Month", "Year"))

# add year column
od$Year = year(as.Date(od$calendar_start_date))

# add single location identifier
od$full_name <- apply(od[, c("adm_0_name", "adm_1_name", "adm_2_name")],
                      1,
                      function(x) paste(x[!is.na(x)], collapse = ", "))




# geomatching

# quick ISO country code addition
od$ISO_A0 = countrycode::countrycode(od$adm_0_name, 
                                     "country.name",
                                     "iso3c")
od$ISO_A0[od$adm_0_name == "SAINT MARTIN"] = "MAF"

# load in FAO GAUL shapefiles
# GAUL_adm0 <- as.data.frame(read_sf("/Users/eideobra/Dropbox/C1_Reference/Country_shapefiles/Admin0(2011)/admin0.shp"))
# GAUL_adm1 <- as.data.frame(read_sf("/Users/eideobra/Dropbox/C1_Reference/Country_shapefiles/Admin1(2011)/admin1.shp"))
# GAUL_adm2 <- as.data.frame(read_sf("/Users/eideobra/Dropbox/C1_Reference/Country_shapefiles/Admin2(2011)/admin2.shp"))

GAUL_adm0 <- as.data.frame(read_sf("./04_Reference_data/Country_shapefiles/Admin0(2011)/admin0.shp"))
GAUL_adm1 <- as.data.frame(read_sf("./04_Reference_data/Country_shapefiles/Admin1(2011)/admin1.shp"))
GAUL_adm2 <- as.data.frame(read_sf("./04_Reference_data/Country_shapefiles/Admin2(2011)/admin2.shp"))
#
# gaul_adm1 <- read_sf("./04_Reference_data/Country_shapefiles/Admin1(2011)/admin1.shp")
# gaul_adm2 <- read_sf("./04_Reference_data/Country_shapefiles/Admin2(2011)/admin2.shp")


# A) processing GAUL codes ===========================================================================
GAUL_adm0$NAME_A0 = GAUL_adm0$COUNTRY_ID
GAUL_adm0$NAME_A1 = NA
GAUL_adm0$NAME_A2 = NA
GAUL_adm1$NAME_A0 = GAUL_adm0$COUNTRY_ID[match(GAUL_adm1$PARENT_ID, GAUL_adm0$GAUL_CODE)]
GAUL_adm1$NAME_A1 = GAUL_adm1$NAME
GAUL_adm1$NAME_A2 = NA
GAUL_adm2$NAME_A0 = GAUL_adm1$NAME_A0[match(GAUL_adm2$PARENT_ID, GAUL_adm1$GAUL_CODE)]
GAUL_adm2$NAME_A1 = GAUL_adm1$NAME[match(GAUL_adm2$PARENT_ID, GAUL_adm1$GAUL_CODE)]
GAUL_adm2$NAME_A2 = GAUL_adm2$NAME


CN <- c("NAME_A0", "NAME_A1", "NAME_A2", "GAUL_CODE")
GAUL_all = rbind(GAUL_adm0[, CN], GAUL_adm1[, CN], GAUL_adm2[, CN])
# standardise to upper case

GAUL_all$NAME_A1 = iconv(GAUL_all$NAME_A1, "UTF-8", "ISO-8859-1")
GAUL_all$NAME_A2 = iconv(GAUL_all$NAME_A2, "UTF-8", "ISO-8859-1")
GAUL_all$NAME_A1= stri_trans_general(str = GAUL_all$NAME_A1, id = "Latin-ASCII")
GAUL_all$NAME_A2= stri_trans_general(str = GAUL_all$NAME_A2, id = "Latin-ASCII")
GAUL_all[, 1:3] = apply(GAUL_all[, 1:3], 2, toupper)

od_match = od[, c("ISO_A0", "adm_1_name", "adm_2_name", "full_name")]
colnames(od_match) = c("NAME_A0", "NAME_A1", "NAME_A2", "description")


# manually edited exceptions

# Argentina
od_match$NAME_A1[od_match$description == "ARGENTINA, FORMOSA, GRL. JOSE DE SAN MARTIN"] = "SALTA"
od_match$NAME_A2[od_match$description == "ARGENTINA, FORMOSA, GRL. JOSE DE SAN MARTIN"] = "GENERAL JOSE DE SAN MARTI"
od_match$NAME_A2[od_match$NAME_A2 == "GRL. JOSE DE SAN MARTIN"] = "GENERAL JOSE DE SAN MARTI"

# Australia
od_match$NAME_A1[od_match$description == "AUSTRALIA, TAS"] = "TASMANIA"
od_match$NAME_A1[od_match$description == "AUSTRALIA, NT"] = "NORTHERN TERRITORY"
od_match$NAME_A1[od_match$description == "AUSTRALIA, SA"] = "SOUTH AUSTRALIA"
od_match$NAME_A1[od_match$description == "AUSTRALIA, NSW"] = "NEW SOUTH WALES"
od_match$NAME_A1[od_match$description == "AUSTRALIA, ACT"] = "AUSTRALIAN CAPITAL TERRITORY"
od_match$NAME_A1[od_match$description == "AUSTRALIA, VIC"] = "VICTORIA"
od_match$NAME_A1[od_match$description == "AUSTRALIA, QLD"] = "QUEENSLAND"
od_match$NAME_A1[od_match$description == "AUSTRALIA, WA"] = "WESTERN AUSTRALIA"

# Bhutan
od_match$NAME_A1[od_match$description == "BHUTAN, SAMDRUPJONGKHAR"] = "SAMDRUP-JONKHA"

# Brunei
od_match$NAME_A1[od_match$description == "BRUNEI DARUSSALAM, BRUNEI MUARA DISTRICT"] = "BRUNEI AND MUARA"

od_match$NAME_A1[od_match$NAME_A0 == "BRN" & !is.na(od_match$NAME_A1)] = gsub(" DISTRICT", "", od_match$NAME_A1[od_match$NAME_A0 == "BRN" & !is.na(od_match$NAME_A1)])

# Cambodia
od_match$NAME_A1[od_match$description == "CAMBODIA, PREAH SIHANOUK"] = "SIHANOUKVILLE"

# China adm1
# unique(od_match$NAME_A1[od_match$NAME_A0 == "CHN" & !grepl("SHENG|SHI", od_match$NAME_A1, ignore.case=TRUE)]) 
od_match$NAME_A1[od_match$description == "CHINA, SHANGHAI"] = "SHANGHAI SHI"
od_match$NAME_A1[od_match$description == "CHINA, BEIJING"] = "BEIJING SHI"
od_match$NAME_A1[od_match$description == "CHINA, CHONGQING"] = "CHONGQING SHI"

od_match$NAME_A1[od_match$description == "CHINA, ANHUI"] = "ANHUI SHENG"
od_match$NAME_A1[od_match$description == "CHINA, FUJIAN"] = "FUJIAN SHENG"
od_match$NAME_A1[od_match$description == "CHINA, GUANGDONG"] = "GUANGDONG SHENG"
od_match$NAME_A1[od_match$description == "CHINA, HAINAN"] = "HAINAN SHENG"
od_match$NAME_A1[od_match$description == "CHINA, HUNAN"] = "HUNAN SHENG"
od_match$NAME_A1[od_match$description == "CHINA, HENAN"] = "HENEN SHENG"
od_match$NAME_A1[od_match$description == "CHINA, HUBEI"] = "HUBEI SHENG"
od_match$NAME_A1[od_match$description == "CHINA, JIANGSU"] = "JIANGSU SHENG"
od_match$NAME_A1[od_match$description == "CHINA, JIANGXI"] = "JIANGXI SHENG"
od_match$NAME_A1[od_match$description == "CHINA, SICHUAN"] = "SICHUAN SHENG"
od_match$NAME_A1[od_match$description == "CHINA, SHANDONG"] = "SHANDONG SHENG"

od_match$NAME_A1[od_match$description == "CHINA, YUNNAN"] = "YUNNAN SHENG"
od_match$NAME_A1[od_match$description == "CHINA, ZHEJIANG"] = "ZHEJIANG SHENG"

od_match$NAME_A1[od_match$description %in% c("CHINA, GUNAGXI","CHINA, GUANGXI")] = "GUANGXI ZHUANGZU ZIZHIQU"


# Colombia
col_adm2 <- GAUL_all[GAUL_all$NAME_A0 == "COL" & !is.na(GAUL_all$NAME_A2), ]

od_match$NAME_A1[od_match$NAME_A1 == "NORTE SANTANDER" & od_match$NAME_A0 == "COL"] = "NORTE DE SANTANDER"
od_match$NAME_A1[od_match$NAME_A1 == "VALLE" & od_match$NAME_A0 == "COL"] = "VALLE DEL CAUCA"
od_match$NAME_A1[od_match$NAME_A1 == "SAN ANDRES"& od_match$NAME_A0 == "COL"] = "SAN ANDRES Y PROVIDENCIA"
od_match$NAME_A2[od_match$description == "COLOMBIA, SAN ANDRES, SAN ANDRES"] = "SAN ANDRES Y PROVIDENCIA"

od_match$NAME_A2[od_match$description == "COLOMBIA, ATLANTICO, BARRANQUILLA"] = "BARRANQUILLA  (DIST.PORT."
od_match$NAME_A2[od_match$description == "COLOMBIA, MAGDALENA, SANTA MARTA (DIST. ESP)"] = "SANTA MARTA (DIST. ESP.)" # !!!!!!!


# colombia_admin <- read.csv("/Users/eideobra/Dropbox/11_DENData/04_Reference_data/Colombia_adm2_GAUL_lookup.csv")%>% filter(!GAUL_admin2 == "")
colombia_admin <- read.csv("./04_Reference_data/Colombia_adm2_GAUL_lookup.csv")%>% filter(!GAUL_admin2 == "")


for(i in 1:nrow(colombia_admin)){
  ind = (od_match$NAME_A0 == "COL") & (od_match$NAME_A1 == colombia_admin$OD_admin1[i]) & (od_match$NAME_A2 == colombia_admin$OD_admin2[i])
  if(any(ind, na.rm = T)){
    od_match$NAME_A2[ind] = colombia_admin$GAUL_admin2[i]
  }
}

# Costa Rica
od_match$NAME_A2[od_match$description == "COSTA RICA, SAN JOSE, CORONADO"] = "VASQUEZ DE CORONADO"

# Ecuador
od_match$NAME_A1[od_match$description == "ECUADOR, SANTA ELENA"] = "GUAYAS"
od_match$NAME_A1[od_match$description == "ECUADOR, SANTO DOMINGO DE LOS TSACHILAS"] = "PICHINCHA"


# Guatemala
od_match$NAME_A1[od_match$description == "GUATEMALA, EL QUICHE"] = "QUICHE"
od_match$NAME_A1[od_match$description == "GUATEMALA, GUATEMALA CENTRAL"] = "GUATEMALA"
od_match$NAME_A1[od_match$description == "GUATEMALA, GUATEMALA NOR OCCIDENTE"] = "GUATEMALA"
od_match$NAME_A1[od_match$description == "GUATEMALA, GUATEMALA NOROCCIDENTE"] = "GUATEMALA"
od_match$NAME_A1[od_match$description == "GUATEMALA, GUATEMALA NORORIENTE"] = "GUATEMALA"
od_match$NAME_A1[od_match$description == "GUATEMALA, GUATEMALA NOR ORIENTE"] = "GUATEMALA"

od_match$NAME_A1[od_match$description == "GUATEMALA, GUATEMALA SUR"] = "GUATEMALA"
od_match$NAME_A1[od_match$description == "GUATEMALA, IXCAN"] = "QUICHE"
od_match$NAME_A1[od_match$description == "GUATEMALA, IXIL"] = "QUICHE"
od_match$NAME_A1[od_match$description == "GUATEMALA, PETEN NORTE"] = "PETEN"
od_match$NAME_A1[od_match$description == "GUATEMALA, PETEN SUR OCCIDENTAL"] = "PETEN"
od_match$NAME_A1[od_match$description == "GUATEMALA, PETEN SUR ORIENTAL"] = "PETEN"
od_match$NAME_A1[od_match$description == "GUATEMALA, PETEN SUROCCIDENTAL"] = "PETEN"
od_match$NAME_A1[od_match$description == "GUATEMALA, PETEN SURORIENTAL"] = "PETEN"


# India
od_match$NAME_A1[od_match$description == "INDIA, J & K"] = "ADMINISTRATIVE UNIT NOT AVAILABLE" # !!!!- will match multiple GAUL codes
od_match$NAME_A1[od_match$description == "INDIA, PONDICHERRY"] = "PUDUCHERRY"
od_match$NAME_A1[od_match$description == "INDIA, TELANGANA"] = "ANDHRA PRADESH"
od_match$NAME_A1[od_match$description == "INDIA, D&N HAVELI"] = "DADRA AND NAGAR HAVELI"
od_match$NAME_A1[od_match$description == "INDIA, D & N HAVELI"] = "DADRA AND NAGAR HAVELI"
od_match$NAME_A1[od_match$description == "INDIA, A&N ISLAND"] = "ANDAMAN AND NICOBAR"
od_match$NAME_A1[od_match$description == "INDIA, A & N ISLAND"] = "ANDAMAN AND NICOBAR"
od_match$NAME_A1[od_match$description == "INDIA, ODISHA"] = "ORISSA"
od_match$NAME_A1[od_match$description == "INDIA, DAMAN & DIU"] = "DAMAN AND DIU"

# Indonesia
od_match$NAME_A1[od_match$description == "INDONESIA, ACEH"] = "NANGGROE ACEH DARUSALAM"
od_match$NAME_A1[od_match$description == "INDONESIA, DAERAH ISTIMEWA YOGYAKARTA"] = "DI YOGYAKARTA"
od_match$NAME_A1[od_match$description == "INDONESIA, KALIMANTAN UTARA"] = "KALIMANTAN TIMUR"
od_match$NAME_A1[od_match$description == "INDONESIA, NANGROE ACEH DARUSSALAM"] = "NANGGROE ACEH DARUSALAM" # !!!!
od_match$NAME_A1[od_match$description == "INDONESIA, KEPULAUAN BANGKA BELITUNG"] =  "BANGKA BELITUNG" 

# Lao
od_match$NAME_A1[od_match$description == "LAO PEOPLE'S DEMOCRATIC REPUBLIC, VIENTIANE CAPITAL [PREFECTURE]"] = "VIENTIANE CAPITAL"
od_match$NAME_A1[od_match$description == "LAO PEOPLE'S DEMOCRATIC REPUBLIC, XAISOMBOUN"] = "VIENTIANE"

# JAPAN
od_match$NAME_A1[od_match$description == "JAPAN, KOCHI"] = "KOOTI"
od_match$NAME_A1[od_match$description == "JAPAN, YAMAGUCHI"] = "YAMAGUTI"
od_match$NAME_A1[od_match$description == "JAPAN, TOKYO"] = "TOOKYOO"

od_match$NAME_A1[od_match$description == "JAPAN, AICHI"] = "AITI"
od_match$NAME_A1[od_match$description == "JAPAN, CHIBA"] = "TIBA"
od_match$NAME_A1[od_match$description == "JAPAN, TOCHIGI"] = "TOTIGI"
od_match$NAME_A1[od_match$description == "JAPAN, FUKUSHIMA"] = "HUKUSIMA"

# Malaysia
od_match$NAME_A1[od_match$description == "MALAYSIA, W.P KUALA LUMPUR"] = "KUALA LUMPUR"
od_match$NAME_A1[od_match$description == "MALAYSIA, W.P. KUALA LUMPUR"] = "KUALA LUMPUR"
od_match$NAME_A1[od_match$description == "MALAYSIA, W.P. LABUAN"] = "LABUAN"
od_match$NAME_A1[od_match$description == "MALAYSIA, WP LABUAN"] = "LABUAN"
od_match$NAME_A1[od_match$description == "MALAYSIA, SAN=BAH"] = "SABAH"


# Mexico 
od_match$NAME_A1[od_match$description == "MEXICO, COAHUILA"] = "COAHUILA DE ZARAGOZA"
od_match$NAME_A1[od_match$description == "MEXICO, MICHOACAN"] = "MICHOACAN DE OCAMPO"
od_match$NAME_A1[od_match$description == "MEXICO, VERACRUZ"] = "VERACRUZ DE IGNACIO DE LA LLAVE"


# Myanmar
od_match$NAME_A1[od_match$description == "MYANMAR, BAGO (E)"] = "BAGO EAST DIVISION"
od_match$NAME_A1[od_match$description == "MYANMAR, BAGO (W"] = "BAGO WEST DIVISION"
od_match$NAME_A1[od_match$description == "MYANMAR, CHIN"] = "CHIN STATE"
od_match$NAME_A1[od_match$description == "MYANMAR, KACHIN"] = "KACHIN STATE"
od_match$NAME_A1[od_match$description == "MYANMAR, KAYAH"] = "KAYAH STATE"
od_match$NAME_A1[od_match$description == "MYANMAR, KAYIN"] = "KAYIN STATE"
od_match$NAME_A1[od_match$description == "MYANMAR, MAGWAY"] = "MAGWAY DIVISION"
od_match$NAME_A1[od_match$description == "MYANMAR, MANDALAY"] = "MANDALAY DIVISION"
od_match$NAME_A1[od_match$description == "MYANMAR, MONGAR"] = "MON STATE"
od_match$NAME_A1[od_match$description == "MYANMAR, NAYPYITAW"] = "BAGO WEST DIVISION"
od_match$NAME_A1[od_match$description == "MYANMAR, RAKHINE"] = "RAKHINE STATE"
od_match$NAME_A1[od_match$description == "MYANMAR, SAGAING"] = "SAGAING DIVISION"
od_match$NAME_A1[od_match$description == "MYANMAR, SHAN (N)"] = "SHAN STATE (NORTH)"
od_match$NAME_A1[od_match$description == "MYANMAR, SHAN (S)"] = "SHAN STATE (SOUTH)"
od_match$NAME_A1[od_match$description == "MYANMAR, TANINTHARYI"] = "TANINTHARYI DIVISION"
od_match$NAME_A1[od_match$description == "MYANMAR, YANGON"] = "YANGON DIVISION"
od_match$NAME_A1[od_match$description == "MYANMAR, AYAYARWADDY"] = "AYEYARWADY DIVISION"

# Nepal
# load in lookup table
# nepal_admin <- read.csv("/Users/eideobra/Dropbox/11_DENData/04_Reference_data/Nepal_district_GAUL_lookup.csv")
nepal_admin <- read.csv("./04_Reference_data/Nepal_district_GAUL_lookup.csv")
nepal_admin$OD_admin2 = toupper(nepal_admin$OD_admin2)
nepal_admin$GAUL_admin2 = toupper(nepal_admin$GAUL_admin2)

# some manual edits
nepal_admin$OD_admin2[nepal_admin$OD_admin2 == "MAKWANPUR"] = "MAKAWANPUR"
nepal_admin$OD_admin2[nepal_admin$OD_admin2 == "NAWALPARASI(W)"] = "NAWALPARASI"
nepal_admin$OD_admin2[nepal_admin$OD_admin2 == "KAPILBASTU"] = "KAPILVASTU"
nepal_admin$OD_admin2[nepal_admin$OD_admin2 == "KAVREPALANCHOK"] = "KAVRE"
nepal_admin$OD_admin2[nepal_admin$OD_admin2 == "RUKUM(W)"] = "RUKUM"


for(i in 1:nrow(nepal_admin)){
  ind = (od_match$NAME_A0 == "NPL") & (od_match$NAME_A2 == nepal_admin$OD_admin2[i])
  if(any(ind, na.rm = T)){
    od_match$NAME_A2[ind] = nepal_admin$GAUL_admin2[i]
  }
}

od_match$NAME_A2[od_match$description == "NEPAL, KATHMANDU"] = "BAGMATI"
od_match$NAME_A2[od_match$description == "NEPAL, LALITPUR"] = "BAGMATI"
od_match$NAME_A1[od_match$description == "NEPAL, PROVINCE 1"] = "EASTERN"
od_match$NAME_A2[od_match$description == "NEPAL, PROVINCE 1"] = "KOSHI"
od_match$NAME_A1[od_match$description == "NEPAL, BAGMATI"] = "CENTRAL"
od_match$NAME_A2[od_match$description == "NEPAL, BAGMATI"] = "BAGMATI"
od_match$NAME_A1[od_match$description == "NEPAL, KARNALI"] = "MID WESTERN"
od_match$NAME_A2[od_match$description == "NEPAL, KARNALI"] = "KARNALI"
od_match$NAME_A1[od_match$description == "NEPAL, GANDAKI"] = "WESTERN"
od_match$NAME_A2[od_match$description == "NEPAL, GANDAKI"] = "GANDAKI"
od_match$NAME_A1[od_match$description == "NEPAL, LUMBINI"] = "WESTERN"
od_match$NAME_A2[od_match$description == "NEPAL, LUMBINI"] = "LUMBINI"
od_match$NAME_A1[od_match$description == "NEPAL, MADHESH"] = "CENTRAL" # capital city of madhesh province
od_match$NAME_A2[od_match$description == "NEPAL, MADHESH"] = "JANAKPUR" # capital city of madhesh province
od_match$NAME_A1[od_match$description == "NEPAL, SUDURPASCHIM"] = "FAR WESTERN"
od_match$NAME_A2[od_match$description == "NEPAL, SUDURPASCHIM"] = "MAHAKALI"


# Nicaragua
od_match$NAME_A1[od_match$NAME_A1 == "BILWI"] = "ATLANTICO NORTE"
od_match$NAME_A1[od_match$NAME_A1 == "REGION AUTONOMA DEL ATLANTICO SUR"] = "ATLANTICO SUR"
od_match$NAME_A1[od_match$NAME_A1 == "ZELAYA CENTRAL"] = "ATLANTICO SUR"

# Panama
od_match$NAME_A1[od_match$NAME_A1 == "KUNA YALA"] = "COMARCA DE SAN BLAS"
od_match$NAME_A1[od_match$NAME_A1 == "NGOBE BUGLE"] = "BOCAS DEL TORO"

# Pakistan
od_match$NAME_A1[od_match$description == "PAKISTAN, KPK"] = "NWFP"
od_match$NAME_A1[od_match$description == "PAKISTAN, TDS-KP"] = "FATA" #Tribal Districts KP
od_match$NAME_A1[od_match$description == "PAKISTAN, BALUCH"] = "BALOCHISTAN"
od_match$NAME_A1[od_match$description == "PAKISTAN, ICT"] = "ISLAMABAD"
od_match$NAME_A1[od_match$description == "PAKISTAN, KHYBER PAKTHUNK"] = "NWFP"

od_match$NAME_A2[od_match$description == "PAKISTAN, SINDH, KARACHI MALIR"] = "MALIR"
od_match$NAME_A2[od_match$description == "PAKISTAN, SINDH, KARWEST"] = "KARACHI WEST"
od_match$NAME_A2[od_match$description == "PAKISTAN, SINDH, QAMBER"] = "LARKANA"
od_match$NAME_A2[od_match$description == "PAKISTAN, SINDH, KAMBER"] = "LARKANA"
od_match$NAME_A2[od_match$description == "PAKISTAN, BALOCHISTAN, NASEERABAD"] = "NASIRABAD"

od_match$NAME_A1[grepl("PAKISTAN, KHYBER PAKHTUNKHWA,", od_match$description)] <- "NWFP"
od_match$NAME_A2[od_match$description == "PAKISTAN, KHYBER PAKHTUNKHWA, LAKKIMARWAT"] = "LAKKI MARAWAT"
od_match$NAME_A1[od_match$description == "PAKISTAN, KHYBER PAKHTUNKHWA, LAKKIMARWAT"] = "NWFP"

od_match$NAME_A2[od_match$description == "PAKISTAN, KHYBER PAKHTUNKHWA, KHYBER"] = "KHYBER AGENCY"
od_match$NAME_A1[od_match$description == "PAKISTAN, KHYBER PAKHTUNKHWA, KHYBER"] = "FATA"

od_match$NAME_A2[od_match$description == "PAKISTAN, KHYBER PAKHTUNKHWA, MALAKAND"] = "MALAKAND PA"


# Palau
od_match$NAME_A1[od_match$NAME_A0 == "PLW" & !is.na(od_match$NAME_A1)] <- "ADMINISTRATIVE UNIT NOT AVAILABLE"

# Peru
od_match$NAME_A1[od_match$description == "PERU, CHANKA"] = "LIMA"
od_match$NAME_A1[od_match$description == "PERU, CHOTA"] = "CAJAMARACA"
od_match$NAME_A1[od_match$description == "PERU, CUTERVO"] = "CAJAMARCA"
od_match$NAME_A1[od_match$description == "PERU, JAEN"] = "CAJAMARCA"
od_match$NAME_A1[od_match$description == "PERU, LIMA CIUDAD"] = "LIMA"
od_match$NAME_A1[od_match$description == "PERU, LIMA ESTE"] = "LIMA"
od_match$NAME_A1[od_match$description == "PERU, LIMA NORTE"] = "LIMA"
od_match$NAME_A1[od_match$description == "PERU, LIMA SUR"] = "LIMA"
od_match$NAME_A1[od_match$description == "PERU, PIURA I"] = "PIURA"
od_match$NAME_A1[od_match$description == "PERU, PIURA II"] = "PIURA"


# Philippines
# remove "PROVINCE OF" in Admin2 name
od_match$NAME_A2 = gsub("PROVINCE OF ", "", od_match$NAME_A2)
# reverse admin1 region number and name
P_region_reverse = data.frame(num_first = c("Region I (Ilocos region)",
                                            "Region II (Cagayan Valley)",
                                            "Region III (Central Luzon)",
                                            "Region IV-A (Calabarzon)",
                                            "Region IV-B (Mimaropa)",
                                            "Region IX (Zamboanga Peninsula)",
                                            "Region V (Bicol region)",
                                            "Region VI (Western Visayas)",
                                            "Region VII (Central Visayas)",
                                            "Region VIII (Eastern Visayas)",
                                            "Region X (Northern Mindanao)",
                                            "Region XI (Davao region)",
                                            "Region XII (Soccsksargen)",
                                            "Region XIII (Caraga)"),
                              name_first = c("Ilocos (Region I)",
                                             "Cagayan Valley (Region II)",
                                             "Central Luzon (Region III)",
                                             "Calabarzon (Region IV-A)",
                                             "Mimaropa (Region IV-B)",
                                             "Zamboanga Peninsula (Region IX)",
                                             "Bicol (Region V)",
                                             "Western Visayas (Region VI)",
                                             "Central Visayas (Region VII)",
                                             "Eastern Visayas (Region VIII)",
                                             "Northern Mindanao (Region X)",
                                             "Davao (Region XI)",
                                             "Soccsksargen (Region XII)",
                                             "Caraga (Region XIII)"))
P_region_reverse$num_first = toupper(P_region_reverse$num_first)
P_region_reverse$name_first = toupper(P_region_reverse$name_first)

for(i in 1:nrow(P_region_reverse)){
  if(P_region_reverse$name_first[i] %in% od_match$NAME_A1){
    od_match$NAME_A1[od_match$NAME_A1 == P_region_reverse$name_first[i]] = P_region_reverse$num_first[i]
  }
}

od_match$NAME_A2[od_match$description == "PHILIPPINES, DAVAO (REGION XI), COMPOSTELA VALLEY"] = "COMPOSTELA"
od_match$NAME_A2[od_match$description == "PHILIPPINES, SOCCSKSARGEN (REGION XII), PROVINCE OF COTABATO"] = "NORTH COTABATO"

od_match$NAME_A1[od_match$description == "PHILIPPINES, BANGSAMORO AUTONOMOUS REGION IN MUSLIM MINDANAO (BARMM)"] = "AUTONOMOUS REGION IN MUSLIM MINDANAO (ARMM)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, A.R.M.M"] = "AUTONOMOUS REGION IN MUSLIM MINDANAO (ARMM)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, C.A.R" ] = "CORDILLERA ADMINISTRATIVE REGION (CAR)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, NAT. CAP. REGION" ] = "NATIONAL CAPITAL REGION (NCR)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION 1" ] = "REGION I (ILOCOS REGION)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION 2" ] = "REGION II (CAGAYAN VALLEY)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION 3" ] = "REGION III (CENTRAL LUZON)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION 4"] = "REGION IV-A (CALABARZON)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION 5" ] = "REGION V (BICOL REGION)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION 6" ] = "REGION VI (WESTERN VISAYAS)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION 7" ] = "REGION VII (CENTRAL VISAYAS)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION 8" ] = "REGION VIII (EASTERN VISAYAS)"

od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION 9" ] = "REGION IX (ZAMBOANGA PENINSULA)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION 10" ] = "REGION X (NORTHERN MINDANAO)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION 11" ] = "REGION XI (DAVAO REGION)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION 12" ] = "REGION XII (SOCCSKSARGEN)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION CARAGA (CARAGA)"] = "REGION XIII (CARAGA)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, CARAGA"] = "REGION XIII (CARAGA)"


# Saudi Arabia
# SAU_admin <- read.csv("/Users/eideobra/Dropbox/11_DENData/04_Reference_data/SAU_adm1_RNE_lookup.csv")
SAU_admin <- read.csv("./04_Reference_data/SAU_adm1_RNE_lookup.csv")

for(i in 1:nrow(SAU_admin)){
  ind = (od_match$NAME_A0 == "SAU") & (od_match$NAME_A1 == SAU_admin$OD_adm2[i])
  if(any(ind, na.rm = T)){
    od_match$NAME_A1[ind] = SAU_admin$GAUL_adm1[i]
  }
}
od_match$NAME_A1[od_match$description == "SAUDI ARABIA, AL-BAHAH"] = "BAHA"

od_match$NAME_A2[od_match$NAME_A0 == "SAU" & !is.na(od_match$NAME_A1)] <- "ADMINISTRATIVE UNIT NOT AVAILABLE"


# Solomon islands
od_match$NAME_A1[od_match$description == "SOLOMON ISLANDS, CENTRAL ISLANDS"] = "CENTRAL"
od_match$NAME_A1[od_match$description == "SOLOMON ISLANDS, MAKIRA"] = "MAKIRA AND ULAWA"

# Taiwan
od_match$NAME_A0[grepl("TAIWAN", od_match$description, ignore.case=TRUE)] = "CHN"
od_match$NAME_A1[grepl("TAIWAN", od_match$description, ignore.case=TRUE)] = "TAIWAN SHENG"

# Thailand
od_match$NAME_A2[od_match$description == "THAILAND, BUNGKAN"] = "BUNG KAN"
od_match$NAME_A1[od_match$description == "THAILAND, BUNGKAN"] = "NONG KHAI"
od_match$NAME_A1[od_match$description == "THAILAND, P.NAKHON S.AYUTTHAYA"] = "PHRA NAKHON SI AYUDHYA"
od_match$NAME_A1[od_match$description == "THAILAND, PRACHIN BURI"] = "PHACHINBURI"
od_match$NAME_A1[od_match$description == "THAILAND, PRACHUAP KHIRI KHAN"] = "PRACHUAP KHILIKHAN"

# Vietnam
od_match$NAME_A1[od_match$description == "VIET NAM, CAN THO CITY"] = "CAN THO"
od_match$NAME_A1[od_match$description == "VIET NAM, DA NANG CITY"] = "DA NANG"
od_match$NAME_A1[od_match$description == "VIET NAM, DIEN BIEN"] = "DIEN BIEN PHU"
od_match$NAME_A1[od_match$description == "VIET NAM, HA NOI CITY"] = "HA NOI"
od_match$NAME_A1[od_match$description == "VIET NAM, HAI PHONG CITY"] = "HAI PHONG"
od_match$NAME_A1[od_match$description == "VIET NAM, HO CHI MINH CITY"] = "HO CHI MINH"

# national level
od_match$NAME_A0[od_match$description == "BONAIRE, SAINT EUSTATIUS AND SABA"] = "ANT"
od_match$NAME_A0[od_match$description == "SAINT BARTHELEMY"] = "FRA"
od_match$NAME_A0[od_match$description == "SAINT MARTIN"] = "FRA"
od_match$NAME_A0[od_match$description == "SINT MAARTEN"] = "NLD"


GAUL_match <- hmatch_composite(raw = od_match,
                               ref = GAUL_all,
                               pattern = "^NAME",
                               allow_gaps = T,
                               fuzzy = T)

table(GAUL_match$match_type) # good matches - types "gaps" is fine (admin1s as admin2s)
unique(GAUL_match$description[is.na(GAUL_match$match_type)])

unique(GAUL_match$description[GAUL_match$match_type=="settle"])


## manually assign GAUL code ==================================

# Taiwan adm2 code manual matching 
twn_adm1 <- unique(od$adm_1_name[od$adm_0_name == "TAIWAN" & !is.na(od$adm_1_name)])
twn_adm1_inc <- "TAINAN|KAOHSIUNG|TAICHUNG|TAIPEI|KEELUNG" 
twn_adm1_exc <- paste(c(twn_adm1[!grepl(twn_adm1_inc, twn_adm1, ignore.case=TRUE)], "NEW TAIPEI CITY"), collapse = "|")

GAUL_match$GAUL_CODE[grepl(twn_adm1_inc, GAUL_match$description, ignore.case=TRUE) & !grepl("NEW TAIPEI", GAUL_match$description, ignore.case=TRUE)]= 13276

GAUL_match$GAUL_CODE[grepl(twn_adm1_exc, GAUL_match$description, ignore.case=TRUE) ]= 13277

# China adm2 
china_admin_ext <- read.csv("./04_Reference_data/China_adm2_GAUL_extract.csv")

china_admin_ext$ADM2_EN = toupper(china_admin_ext$ADM2_EN)
china_admin_ext$ADM2_EN[china_admin_ext$ADM2_EN == toupper("Xishuangbanna Dai Autonomous Prefecture")] = "JINGHONG"

GAUL_match$GAUL_CODE[GAUL_match$NAME_A1 %in% c("GUANGDONG", "ZHEJIANG", "YUNNAN")] <- china_admin_ext$GAUL_CODE[match(GAUL_match$NAME_A2[GAUL_match$NAME_A1 %in% c("GUANGDONG", "ZHEJIANG", "YUNNAN")], china_admin_ext$ADM2_EN)]

# Colombia adm2 
# col_admin_ext <- read.csv("/Users/eideobra/Dropbox/11_DENData/04_Reference_data/Colombia_adm2_GAUL_extract.csv")%>% rename(NAME_A1 = ADM1_EN, NAME_A2 = ADM2_EN) %>% mutate(NAME_A0 = "COL") %>% dplyr::select(NAME_A0, NAME_A1, NAME_A2, GAUL_CODE)

col_admin_ext <- read.csv("./04_Reference_data/Colombia_adm2_GAUL_extract.csv")%>% rename(NAME_A1 = ADM1_EN, NAME_A2 = ADM2_EN) %>% mutate(NAME_A0 = "COL") %>% dplyr::select(NAME_A0, NAME_A1, NAME_A2, GAUL_CODE)

col_admin_ext$NAME_A1= stri_trans_general(str = col_admin_ext$NAME_A1, id = "Latin-ASCII")
col_admin_ext$NAME_A2= stri_trans_general(str = col_admin_ext$NAME_A2, id = "Latin-ASCII")
col_admin_ext$NAME_A1 = toupper(col_admin_ext$NAME_A1)
col_admin_ext$NAME_A2 = toupper(col_admin_ext$NAME_A2)

col_admin_ext$description <- apply(col_admin_ext[, c("NAME_A0", "NAME_A1", "NAME_A2")],
                      1,
                      function(x) paste(x[!is.na(x)], collapse = ", "))

od_col <- GAUL_match[is.na(GAUL_match$GAUL_CODE) & GAUL_match$NAME_A0 == "COL", c("NAME_A0", "NAME_A1", "NAME_A2", "description")]
od_col$NAME_A2 <- gsub(" )", "", gsub("\\([^)]+", "", od_col$NAME_A2))

col_admin_ext$NAME_A2[col_admin_ext$NAME_A2 == "BOGOTA, D.C."] = "BOGOTA"
col_admin_ext$NAME_A2[col_admin_ext$NAME_A2 == "VILLA RICA"] = "VILLARICA"
col_admin_ext$NAME_A1[col_admin_ext$NAME_A1 == "LA GUAJIRA"] = "GUAJIRA"
col_admin_ext$NAME_A1[col_admin_ext$NAME_A1 == "ARCHIPIELAGO DE SAN ANDRES, PROVIDENCIA Y SANTA CATALINA"] = "SAN ANDRES Y PROVIDENCIA"
col_admin_ext$NAME_A1[col_admin_ext$NAME_A1 == "VALLE"] = "VALLE DEL CAUCA"
col_admin_ext$NAME_A1[col_admin_ext$NAME_A1 == "BOGOTA, D.C."] = "BOGOTA"

GAUL_match_col <- hmatch_composite(raw = od_col,
                               ref = col_admin_ext,
                               pattern = "^NAME",
                               allow_gaps = T,
                               fuzzy = T)

GAUL_match$GAUL_CODE[is.na(GAUL_match$GAUL_CODE) & GAUL_match$NAME_A0 == "COL"] <- GAUL_match_col$GAUL_CODE[match(GAUL_match$description[is.na(GAUL_match$GAUL_CODE)& GAUL_match$NAME_A0 == "COL"], GAUL_match_col$description)]


# for some reason just won't match
GAUL_match$GAUL_CODE[GAUL_match$description == "COLOMBIA, MAGDALENA, SANTA MARTA (DIST. ESP)"] = 14028

GAUL_match$GAUL_CODE[GAUL_match$description == "COLOMBIA, CHOCO, BELEN DE BAJIRA"] = 13803 # area with  territorial dispute
GAUL_match$GAUL_CODE[GAUL_match$description == "COLOMBIA, VALLE, DARIEN"] = 14375

GAUL_match$GAUL_CODE[GAUL_match$description == "TONGA, HAAPAI"] = 2976
GAUL_match$GAUL_CODE[GAUL_match$description == "TONGA, TONGATAPU"] = 2976
GAUL_match$GAUL_CODE[GAUL_match$description == "TONGA, VAVA'U"] = 2976

GAUL_match$GAUL_CODE[GAUL_match$description == "PAKISTAN, AJK"] = 40408
GAUL_match$GAUL_CODE[grepl("PAKISTAN, GILGIT BALTISTAN,", GAUL_match$description)] = 40409
GAUL_match$GAUL_CODE[GAUL_match$description == "PAKISTAN, KHYBER PAKHTUNKHWA, LAKIMARWAT"] = 40341



# check before assign if any NAs in GAUL_CODE
sort(unique(GAUL_match$description[is.na(GAUL_match$GAUL_CODE)]))


# assign GAUL codes
od$FAO_GAUL_code <- GAUL_match$GAUL_CODE[match(od$full_name, GAUL_match$description)]


summary(is.na(od))

# B) processing RNaturalEarth =============================================================

# load in Rnatural earth shapefiles
RNE_country <- as.data.frame(ne_countries(scale = 10))
RNE_states <- as.data.frame(ne_states())


RNE_country$NAME_A0 <- RNE_country$adm0_a3
RNE_country$NAME_A1 <- NA
RNE_country$RNE_CODE <- RNE_country$adm0_a3
RNE_states$NAME_A0 <- RNE_states$adm0_a3
RNE_states$NAME_A1 <- RNE_states$name
RNE_states$RNE_CODE <- RNE_states$iso_3166_2 # using ISO 3166-2 codes to match

CN <- c("NAME_A0", "NAME_A1", "RNE_CODE")
RNE_all = rbind(RNE_country[, CN], RNE_states[, CN])

RNE_all$NAME_A1= stri_trans_general(str = RNE_all$NAME_A1, id = "Latin-ASCII")
# standardise to upper case
RNE_all[, 1:3] = apply(RNE_all[, 1:3], 2, toupper)

od_match = od[, c("ISO_A0", "adm_1_name", "full_name")]
colnames(od_match) = c("NAME_A0", "NAME_A1", "description")

# rne_states <- ne_states()
# rne_states[rne_states$adm0_a3 == "PAK", c("name")]%>% mapview()

# manually edited exceptions

# Australia
od_match$NAME_A1[od_match$description == "AUSTRALIA, TAS"] = "TASMANIA"
od_match$NAME_A1[od_match$description == "AUSTRALIA, NT"] = "NORTHERN TERRITORY"
od_match$NAME_A1[od_match$description == "AUSTRALIA, SA"] = "SOUTH AUSTRALIA"
od_match$NAME_A1[od_match$description == "AUSTRALIA, NSW"] = "NEW SOUTH WALES"
od_match$NAME_A1[od_match$description == "AUSTRALIA, ACT"] = "AUSTRALIAN CAPITAL TERRITORY"
od_match$NAME_A1[od_match$description == "AUSTRALIA, VIC"] = "VICTORIA"
od_match$NAME_A1[od_match$description == "AUSTRALIA, QLD"] = "QUEENSLAND"
od_match$NAME_A1[od_match$description == "AUSTRALIA, WA"] = "WESTERN AUSTRALIA"

# Bhutan
od_match$NAME_A1[od_match$description == "BHUTAN, DAGANA"] = "DAGA"
od_match$NAME_A1[od_match$description == "BHUTAN, LHUNTSE"] = "LHUNTSHI"
od_match$NAME_A1[od_match$description == "BHUTAN, WANGDUEPHODRANG"] = "WANGDI PHODRANG"
od_match$NAME_A1[od_match$description == "BHUTAN, SARPANG"] = "GEYLEGPHUG"
od_match$NAME_A1[od_match$description == "BHUTAN, TRASHIYANGTSE"] = "TASHI YANGTSE"
od_match$NAME_A1[od_match$description == "BHUTAN, TSIRANG"] = "CHIRANG"
od_match$NAME_A1[od_match$description == "BHUTAN, SAMTSE"] = "SAMCHI"

# Brunei
od_match$NAME_A1 = gsub(" DISTRICT", "", od_match$NAME_A1)
od_match$NAME_A1[od_match$description == "BRUNEI DARUSSALAM, BRUNEI MUARA DISTRICT"] = "BRUNEI AND MUARA"


# Bolivia
od_match$NAME_A1[od_match$NAME_A1 == "BENI"] = "EL BENI"

# China
# remove "SHENG" and "SHI" from admin1
od_match$NAME_A1 = gsub(" SHENG", "", od_match$NAME_A1)
od_match$NAME_A1 = gsub(" SHI", "", od_match$NAME_A1)
od_match$NAME_A1[od_match$description == "CHINA, GUANGXI ZHUANGZU ZIZHIQU"] = "GUANGXI"
od_match$NAME_A1[od_match$description == "CHINA, NEI MONGOL ZIZHIQU"] = "INNER MONGOL"
od_match$NAME_A1[od_match$description == "CHINA, NINGXIA HUIZU ZIZHIQU"] = "NINGXIA"
od_match$NAME_A1[od_match$description == "CHINA, XINJIANG UYGUR ZIZHIQU"] = "XINJIANG"
od_match$NAME_A1[od_match$description == "CHINA, XIZANG ZIZHIQU"] = "XIZANG"

# Dominican Republic
od_match$NAME_A1[od_match$NAME_A1 == "ELIAS PINA"] = "LA ESTRELLETA"
od_match$NAME_A1[od_match$NAME_A1 == "SALCEDO"] = "HERMANAS"

# Cambodia
od_match$NAME_A1[od_match$description == "CAMBODIA, KAMPONG SPEU"] = "KÂMPÓNG SPŒ"
od_match$NAME_A1[od_match$description == "CAMBODIA, KRATIE"] = "KRÂCHÉH"
od_match$NAME_A1[od_match$description == "CAMBODIA, PAILIN"] = "KRONG PAILIN"
od_match$NAME_A1[od_match$description == "CAMBODIA, PREAH SIHANOUK"] = "KRONG PREAH SIHANOUK"
od_match$NAME_A1[od_match$description == "CAMBODIA, PURSAT"] = "POUTHISAT"
od_match$NAME_A1[od_match$description == "CAMBODIA, RATANAK KIRI"] = "RÔTÂNÔKIRI"
od_match$NAME_A1[od_match$description == "CAMBODIA, SIEM REAP"] = "SIEMRÉAB"
od_match$NAME_A1[od_match$description == "CAMBODIA, STUNG TRENG"] = "STŒNG TRÊNG"

# Colombia
od_match$NAME_A1[od_match$NAME_A1 == "GUAJIRA"] = "LA GUAJIRA"
od_match$NAME_A1[od_match$NAME_A1 == "NORTE SANTANDER" & od_match$NAME_A0 == "COL"] = "NORTE DE SANTANDER"
od_match$NAME_A1[od_match$NAME_A1 == "VALLE" & od_match$NAME_A0 == "COL"] = "VALLE DEL CAUCA"
od_match$NAME_A1[od_match$NAME_A1 == "SAN ANDRES"& od_match$NAME_A0 == "COL"] = "SAN ANDRES Y PROVIDENCIA"


#Cuba
od_match$NAME_A1[od_match$NAME_A1 == "LA HABANA"] = "CIUDAD DE LA HABANA"

# Guatemala
od_match$NAME_A1[od_match$description == "GUATEMALA, EL QUICHE"] = "QUICHE"
od_match$NAME_A1[od_match$description == "GUATEMALA, GUATEMALA CENTRAL"] = "GUATEMALA"
od_match$NAME_A1[od_match$description == "GUATEMALA, GUATEMALA NOR OCCIDENTE"] = "GUATEMALA"
od_match$NAME_A1[od_match$description == "GUATEMALA, GUATEMALA NOROCCIDENTE"] = "GUATEMALA"
od_match$NAME_A1[od_match$description == "GUATEMALA, GUATEMALA NORORIENTE"] = "GUATEMALA"
od_match$NAME_A1[od_match$description == "GUATEMALA, GUATEMALA NOR ORIENTE"] = "GUATEMALA"

od_match$NAME_A1[od_match$description == "GUATEMALA, GUATEMALA SUR"] = "GUATEMALA"
od_match$NAME_A1[od_match$description == "GUATEMALA, IXCAN"] = "QUICHE"
od_match$NAME_A1[od_match$description == "GUATEMALA, IXIL"] = "QUICHE"
od_match$NAME_A1[od_match$description == "GUATEMALA, PETEN NORTE"] = "PETEN"
od_match$NAME_A1[od_match$description == "GUATEMALA, PETEN SUR OCCIDENTAL"] = "PETEN"
od_match$NAME_A1[od_match$description == "GUATEMALA, PETEN SUR ORIENTAL"] = "PETEN"
od_match$NAME_A1[od_match$description == "GUATEMALA, PETEN SUROCCIDENTAL"] = "PETEN"
od_match$NAME_A1[od_match$description == "GUATEMALA, PETEN SURORIENTAL"] = "PETEN"

# India
od_match$NAME_A1[od_match$description == "INDIA, DADRA AND NAGAR HAVELI"] = "DADRA AND NAGAR HAVELI AND DAMAN AND DIU"
od_match$NAME_A1[od_match$description == "INDIA, DAMAN AND DIU"] = "DADRA AND NAGAR HAVELI AND DAMAN AND DIU"
od_match$NAME_A1[od_match$description == "INDIA, J & K"] = "JAMMU AND KASHMIR"
od_match$NAME_A1[od_match$description == "INDIA, ORISSA"] = "ODISHA"

od_match$NAME_A1[od_match$description == "INDIA, PONDICHERRY"] = "PUDUCHERRY"
od_match$NAME_A1[od_match$description == "INDIA, TELANGANA"] = "ANDHRA PRADESH"
od_match$NAME_A1[od_match$description == "INDIA, D&N HAVELI"] = "DADRA AND NAGAR HAVELI AND DAMAN AND DIU"
od_match$NAME_A1[od_match$description == "INDIA, A&N ISLAND"] = "ANDAMAN AND NICOBAR"
od_match$NAME_A1[od_match$description == "INDIA, D & N HAVELI"] = "DADRA AND NAGAR HAVELI AND DAMAN AND DIU"
od_match$NAME_A1[od_match$description == "INDIA, A & N ISLAND"] = "ANDAMAN AND NICOBAR"
od_match$NAME_A1[od_match$description == "INDIA, DAMAN & DIU"] = "DADRA AND NAGAR HAVELI AND DAMAN AND DIU"

# Indonesia
od_match$NAME_A1[od_match$description == "INDONESIA, DAERAH ISTIMEWA YOGYAKARTA"] = "YOGYAKARTA"
od_match$NAME_A1[od_match$description == "INDONESIA, DKI JAKARTA"] = "JAKARTA RAYA"
od_match$NAME_A1[od_match$description == "INDONESIA, KALIMANTAN UTARA"] = "KALIMANTAN TIMUR"
od_match$NAME_A1[od_match$description == "INDONESIA, NANGROE ACEH DARUSSALAM"] = "ACEH"
od_match$NAME_A1[od_match$description == "INDONESIA, NANGGROE ACEH DARUSSALAM"] = "ACEH"
od_match$NAME_A1[od_match$description == "INDONESIA, DI YOGYAKARTA"] = "YOGYAKARTA"
od_match$NAME_A1[od_match$description == "INDONESIA, KEPULAUAN BANGKA BELITUNG"] =  "BANGKA-BELITUNG" 

# Lao
od_match$NAME_A1[od_match$description == "LAO PEOPLE'S DEMOCRATIC REPUBLIC, VIENTIANE CAPITAL"] = "VIENTIANE [PREFECTURE]"
od_match$NAME_A1[od_match$description == "LAO PEOPLE'S DEMOCRATIC REPUBLIC, VIENTIANE CAPITAL [PREFECTURE]"] = "VIENTIANE [PREFECTURE]"
od_match$NAME_A1[od_match$description == "LAO PEOPLE'S DEMOCRATIC REPUBLIC, XAISOMBOUN"] = "VIENTIANE"

# Malaysia
od_match$NAME_A1[od_match$description == "MALAYSIA, W.P KUALA LUMPUR"] = "KUALA LUMPUR"
od_match$NAME_A1[od_match$description == "MALAYSIA, W.P. KUALA LUMPUR"] = "KUALA LUMPUR"
od_match$NAME_A1[od_match$description == "MALAYSIA, W.P. LABUAN"] = "LABUAN"
od_match$NAME_A1[od_match$description == "MALAYSIA, WP LABUAN"] = "LABUAN"
od_match$NAME_A1[od_match$description == "MALAYSIA, SAN=BAH"] = "SABAH"

#Myanmar
od_match$NAME_A1[od_match$description %in% c("MYANMAR, SHAN (N)", "MYANMAR, SHAN (S)")] = "SHAN"
od_match$NAME_A1[od_match$description %in% c("MYANMAR, BAGO (W", "MYANMAR, BAGO (E)")] = "BAGO"
od_match$NAME_A1[od_match$description == "MYANMAR, MONGAR"] = "MON"
od_match$NAME_A1[od_match$description == "MYANMAR, AYAYARWADDY"] = "AYEYARWADY"
od_match$NAME_A1[od_match$description == "MYANMAR, NAYPYITAW"] = "MANDALAY"

# Nepal - new constitution since 2015 but RNE shapefiles do not match with the current divisions. Largest cities/capital cities matched to the former adm1 division
od_match$NAME_A1[od_match$description == "NEPAL, PROVINCE 1"] = "BHOJPUR"
od_match$NAME_A1[od_match$description == "NEPAL, MADHESH"] = "JANAKPUR"
od_match$NAME_A1[od_match$description == "NEPAL, SUDURPASCHIM"] = "MAHAKALI"


# Nicaragua
od_match$NAME_A1[od_match$NAME_A1 == "BILWI"] = "ATLANTICO NORTE"
od_match$NAME_A1[od_match$NAME_A1 == "REGION AUTONOMA DEL ATLANTICO SUR"] = "ATLANTICO SUR"
od_match$NAME_A1[od_match$NAME_A1 == "ZELAYA CENTRAL"] = "ATLANTICO SUR"

# Pakistan
od_match$NAME_A1[od_match$description == "PAKISTAN, KPK"] = "K.P."
od_match$NAME_A1[od_match$description == "PAKISTAN, TDS-KP"] = "F.A.T.A."
od_match$NAME_A1[od_match$description == "PAKISTAN, BALUCH"] = "BALUCHISTAN"
od_match$NAME_A1[od_match$description == "PAKISTAN, AJK"] = "AZAD KASHMIR"
od_match$NAME_A1[od_match$description == "PAKISTAN, ICT"] = "F.C.T."
od_match$NAME_A1[od_match$description == "PAKISTAN, KHYBER PAKTHUNK"] = "K.P."

od_match$NAME_A1[grepl("PAKISTAN, KHYBER PAKTHUNK", od_match$description)] = "K.P."
od_match$NAME_A1[grepl("PAKISTAN, KHYBER PAKHTUNKHWA", od_match$description)] = "K.P."
od_match$NAME_A1[grepl("PAKISTAN, GILGIT BALTISTAN", od_match$description)] = "NORTHERN AREAS"

# Peru
od_match$NAME_A1[od_match$description == "PERU, CHANKA"] = "LIMA"
od_match$NAME_A1[od_match$description == "PERU, CHOTA"] = "CAJAMARACA"
od_match$NAME_A1[od_match$description == "PERU, CUTERVO"] = "CAJAMARCA"
od_match$NAME_A1[od_match$description == "PERU, JAEN"] = "CAJAMARCA"
od_match$NAME_A1[od_match$description == "PERU, LIMA CIUDAD"] = "LIMA"
od_match$NAME_A1[od_match$description == "PERU, LIMA ESTE"] = "LIMA"
od_match$NAME_A1[od_match$description == "PERU, LIMA NORTE"] = "LIMA"
od_match$NAME_A1[od_match$description == "PERU, LIMA SUR"] = "LIMA"
od_match$NAME_A1[od_match$description == "PERU, PIURA I"] = "PIURA"
od_match$NAME_A1[od_match$description == "PERU, PIURA II"] = "PIURA"

# Philippines
# OpenDengue admin1s map to "region" in RNE states shapefiles
RNE_PHL_regions <- data.frame(State = RNE_states$name[RNE_states$adm0_a3 == "PHL"],
                              Region = RNE_states$region[RNE_states$adm0_a3 == "PHL"])
# replace Philippines states with regions in RNE admin file
RNE_all$NAME_A1[RNE_all$NAME_A0 == "PHL"] = toupper(RNE_PHL_regions$Region[match(RNE_all$NAME_A1[RNE_all$NAME_A0 == "PHL"],                                                  toupper(RNE_PHL_regions$State))])

# manual alignments
od_match$NAME_A1[od_match$NAME_A1 == "REGION XIII (CARAGA)"] = "DINAGAT ISLANDS (REGION XIII)"
od_match$NAME_A1[od_match$NAME_A1 == "CARAGA (REGION XIII)"] = "DINAGAT ISLANDS (REGION XIII)"

od_match$NAME_A1[od_match$description == "PHILIPPINES, NATIONAL CAPITAL REGION (NCR)"] = "NATIONAL CAPITAL REGION"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION I (ILOCOS REGION)"] = "ILOCOS (REGION I)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION X (NORTHERN MINDANAO)"] = "NORTHERN MINDANAO (REGION X)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION XI (DAVAO REGION)"] = "DAVAO (REGION XI)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION XII (SOCCSKSARGEN)"] = "SOCCSKSARGEN (REGION XII)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION II (CAGAYAN VALLEY)"] = "CAGAYAN VALLEY (REGION II)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION III (CENTRAL LUZON)"] = "CENTRAL LUZON (REGION III)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION 4"] = "CALABARZON (REGION IV-A)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION V (BICOL REGION)"] = "BICOL (REGION V)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION VI (WESTERN VISAYAS)"] = "WESTERN VISAYAS (REGION VI)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION VII (CENTRAL VISAYAS)"] = "CENTRAL VISAYAS (REGION VII)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION VIII (EASTERN VISAYAS)"] = "EASTERN VISAYAS (REGION VIII)"

od_match$NAME_A1[od_match$description == "PHILIPPINES, BANGSAMORO AUTONOMOUS REGION IN MUSLIM MINDANAO (BARMM)"] = "AUTONOMOUS REGION IN MUSLIM MINDANAO (ARMM)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION IV-A (CALABARZON)"] = "CALABARZON (REGION IV-A)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION CARAGA (CARAGA)"] = "DINAGAT ISLANDS (REGION XIII)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION IV-B (MIMAROPA)"] = "MIMAROPA (REGION IV-B)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION IX (ZAMBOANGA PENINSULA)"] = "ZAMBOANGA PENINSULA (REGION IX)"

od_match$NAME_A1[od_match$description == "PHILIPPINES, A.R.M.M"] = "AUTONOMOUS REGION IN MUSLIM MINDANAO (ARMM)"

od_match$NAME_A1[od_match$description == "PHILIPPINES, C.A.R" ] = "CORDILLERA ADMINISTRATIVE REGION (CAR)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, NAT. CAP. REGION" ] = "NATIONAL CAPITAL REGION"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION 1" ] = "ILOCOS (REGION I)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION 2" ] = "CAGAYAN VALLEY (REGION II)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION 3" ] = "CENTRAL LUZON (REGION III)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION 5" ] = "BICOL (REGION V)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION 6" ] = "WESTERN VISAYAS (REGION VI)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION 7" ] = "CENTRAL VISAYAS (REGION VII)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION 8" ] = "EASTERN VISAYAS (REGION VIII)"

od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION 9" ] = "ZAMBOANGA PENINSULA (REGION IX)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION 10" ] = "NORTHERN MINDANAO (REGION X)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION 11" ] = "DAVAO (REGION XI)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, REGION 12" ] = "SOCCSKSARGEN (REGION XII)"
od_match$NAME_A1[od_match$description == "PHILIPPINES, CARAGA" ] = "DINAGAT ISLANDS (REGION XIII)"


# Saudi Arabia

# SAU_admin <- read.csv("/Users/eideobra/Dropbox/11_DENData/04_Reference_data/SAU_adm1_RNE_lookup.csv")%>% filter(!RNE_adm1 == "")
SAU_admin <- read.csv("./04_Reference_data/SAU_adm1_RNE_lookup.csv")%>% filter(!RNE_adm1 == "")


for(i in 1:nrow(SAU_admin)){
  ind = (od_match$NAME_A0 == "SAU") & (od_match$NAME_A1 == SAU_admin$OD_adm2[i])
  if(any(ind, na.rm = T)){
    od_match$NAME_A1[ind] = SAU_admin$RNE_adm1[i]
  }
}

od_match$NAME_A1[od_match$description == "SAUDI ARABIA, BAHA"] = "AL BAHAH"
od_match$NAME_A1[od_match$description == "SAUDI ARABIA, AL-SHAMAL"] = "AL HUDUD ASH SHAMALIYAH"

# Solomon islands
od_match$NAME_A1[od_match$description == "SOLOMON ISLANDS, CENTRAL ISLANDS"] = "CENTRAL"
od_match$NAME_A1[od_match$description == "SOLOMON ISLANDS, HONIARA"] = "CAPITAL TERRITORY (HONIARA)"



# Sri Lanka
# od data is at province levle (n = 9), RNE shapefiles are at district level (n = 25)
# adminsitrative divisions from here: https://en.wikipedia.org/wiki/Districts_of_Sri_Lanka
RNE_LKA_regions = data.frame(District = toupper(sort(unique(RNE_all$NAME_A1[RNE_all$NAME_A0 == "LKA"]))))
RNE_LKA_regions$Province = c("EASTERN", "NORTH CENTRAL", "UVA", "EASTERN", "WESTERN", "SOUTHERN",
                             "WESTERN", "SOUTHERN", "NORTHERN", "WESTERN", "CENTRAL", "SABARAGAMUWA",
                             "NORTHERN", "NORTH WESTERN", "NORTHERN", "CENTRAL", "SOUTHERN", "UVA",
                             "NORTHERN", "CENTRAL", "NORTH CENTRAL", "NORTH WESTERN", "SABARAGAMUWA",
                             "EASTERN", "NORTHERN")

# replace SL districts with provinces in RNE admin file
RNE_all$NAME_A1[RNE_all$NAME_A0 == "LKA"] = RNE_LKA_regions$Province[match(RNE_all$NAME_A1[RNE_all$NAME_A0 == "LKA"], toupper(RNE_LKA_regions$District))]


# Thailand
od_match$NAME_A1[od_match$description == "THAILAND, BANGKOK"] = "BANGKOK METROPOLIS"
od_match$NAME_A1[od_match$description == "THAILAND, BUNGKAN"] = "BUENG KAN"
od_match$NAME_A1[od_match$description == "THAILAND, PHRA NAKHON SI AYUDHYA"] = "PHRA NAKHON SI AYUTTHAYA"
od_match$NAME_A1[od_match$description == "THAILAND, PHACHINBURI"] = "PRACHIN BURI"
od_match$NAME_A1[od_match$description == "THAILAND, PRACHUAP KHILIKHAN"] = "PRACHUAP KHIRI KHAN"

# Timor-Leste
od_match$NAME_A1[od_match$description == "TIMOR-LESTE, OECUSSI"] = "AMBENO"


# Vietnam
od_match$NAME_A1[od_match$description == "VIET NAM, BAC KAN"] = "DONG BAC"
od_match$NAME_A1[od_match$description == "VIET NAM, CAN THO CITY"] = "CAN THO"
od_match$NAME_A1[od_match$description == "VIET NAM, DA NANG CITY"] = "ĐÀ NẴNG"
od_match$NAME_A1[od_match$description == "VIET NAM, DONG NAI"] = "ĐÔNG NAM BỘ"
od_match$NAME_A1[od_match$description == "VIET NAM, HA NOI CITY"] = "HA NOI"
od_match$NAME_A1[od_match$description == "VIET NAM, HA TAY"] = "GIA LAI"
od_match$NAME_A1[od_match$description == "VIET NAM, HAI PHONG CITY"] = "HẢI PHÒNG"
od_match$NAME_A1[od_match$description == "VIET NAM, HUNG YEN"] = "ĐỒNG BẰNG SÔNG HỒNG"

# Taiwan 
od_match$NAME_A1[od_match$NAME_A0 == "TWN"] = gsub(" COUNTY", "", od_match$NAME_A1[od_match$NAME_A0 == "TWN"])

od_match$NAME_A1[grepl("TAIWAN, TAOYUAN", od_match$description)] = "TAOYUAN"



# national level
od_match$NAME_A0[od_match$description == "BONAIRE, SAINT EUSTATIUS AND SABA"] = "NLD"
od_match$NAME_A0[od_match$description == "FRENCH GUIANA"] = "FRA"
od_match$NAME_A0[od_match$description == "MARTINIQUE"] = "FRA"

# matching algorithm
RNE_match <- hmatch_composite(raw = od_match,
                               ref = RNE_all,
                               pattern = "^NAME",
                               allow_gaps = T,
                               fuzzy = T)

table(RNE_match$match_type) # all good matches
unique(RNE_match$description[is.na(RNE_match$match_type)])

# No RNE CODE available 
RNE_match$RNE_CODE[RNE_match$description == "TAIWAN, LIENCHIANG COUNTY, NANGAN TOWNSHIP"] = "TWN"
unique(RNE_match$description[is.na(RNE_match$RNE_CODE)])

# check before assign if any NAs in RNE_CODE
sort(unique(RNE_match$description[is.na(RNE_match$RNE_CODE)]))

# assign RNE codes
od$RNE_iso_code <- RNE_match$RNE_CODE[match(od$full_name, RNE_match$description)]

# IBGE code
od$IBGE_code <- NA

# remove any other administrative codes from od
od = od[, c("adm_0_name",
            "adm_1_name",
            "adm_2_name",
            "full_name",
            "ISO_A0",
            "FAO_GAUL_code",
            "RNE_iso_code",
            "IBGE_code",
            "calendar_start_date",
            "calendar_end_date",
            "Year",
            "dengue_total",
            "case_definition_standardised",
            "S_res", 
            "T_res", 
            "UUID")]

summary(is.na(od)) #391429 obs

# write.csv(od, "01_Dengue_data/OD_master/od.csv", row.names=F)


# DOUBLE COUNT PROTOCOL  =============================================================
bra <- read.csv("01_Dengue_data/open_dengue_1.2/Brazil_adm2_2001_2021.csv") 
od <- read.csv("01_Dengue_data/OD_master/od.csv") %>%
  rbind(., bra)%>%
  mutate(rowid = row_number())


# For original data source names of TYCHO
tycho <- read.csv("01_Dengue_data/source_files/original_name/tycho_dengue_allcountries.csv")

tycho$PeriodStartDate <- dmy(tycho$PeriodStartDate)
tycho$Year <- year(tycho$PeriodStartDate)

tycho_source <- tycho %>% 
  group_by(CountryISO, Year, SourceName)%>% tally() %>% select(-n)

# Get source categories (MOH, WHO, TYCHO, etc.)
od <- od %>%
  rowwise()%>%
  mutate(source_cat = strsplit(UUID, "-")[[1]][1])


# leap year corrections
od <- od %>%
  mutate(leap = lubridate::leap_year(calendar_end_date) & month(calendar_end_date) == 2 & day(calendar_end_date) == 28) %>%
  mutate(calendar_end_date = ifelse(leap == TRUE, paste0(Year, "-02-29"), calendar_end_date))

od <- od %>%
  mutate(leap_start = lubridate::leap_year(calendar_start_date) & month(calendar_start_date) == 2 & day(calendar_start_date) == 28) %>%
  mutate(calendar_start_date = ifelse(leap_start == TRUE, paste0(Year, "-02-29"), calendar_start_date))

od <- od %>% select(-leap, -leap_start)

# 1) check double count cases 
od <- od %>%
  group_by(full_name, calendar_start_date, calendar_end_date) %>%
  mutate(double = ifelse(n()>1, TRUE, FALSE)) 

# 2) see any records that have multiple sources including TYCHO but not MOH 
dup_tycho <- od %>%
  group_by(full_name, calendar_start_date, calendar_end_date) %>%
  filter(any(source_cat %in% c("TYCHO")) & !any(source_cat %in% c("MOH"))) %>% 
  mutate(dup_tycho = ifelse(n()>1, TRUE, FALSE))
 
dup_tycho %>% group_by(dup_tycho)%>% tally() # 385 records to be inspected

### get list of countries where tycho source names need to be checked
tycho_check <- dup_tycho %>% 
  filter(double==TRUE & dup_tycho == TRUE) %>%
  group_by(adm_0_name, ISO_A0, Year)%>%
  tally() %>% select(-n)%>%
  mutate(iso2 = countrycode(ISO_A0, origin = "iso3c", destination = "iso2c"))%>%
  merge(., tycho_source, by.x = c("iso2", "Year"), by.y= c("CountryISO", "Year"), all.x=T)

unique(tycho_check$SourceName)
unique(tycho_check$SourceName[!grepl("World Health Organization", tycho_check$SourceName)]) # only Thailand data are from MOH

# quick look at thailand data
dup_tycho %>% filter(double==TRUE & dup_tycho == TRUE & adm_0_name == "THAILAND") %>%
  group_by(full_name, calendar_start_date, calendar_end_date, source_cat, dengue_total)%>%
  tally()%>% print(n = 50)
  
# for now, ok to ignore TYCHO original source names (cos there is no difference in dengue counts between different sources in Thailand)
od <- od %>%
  mutate(source_cat2 = ifelse(source_cat == "MOH", 4, 
                        ifelse(grepl("WHO", source_cat), 3, 
                          ifelse(source_cat == "LITERATURE", 1, 2)))) 


# find and remove duplicated rows 
# 3-1) exact same dengue count from the same data source
dup1 <- od %>%
  group_by(full_name, calendar_start_date, calendar_end_date, dengue_total, UUID) %>%
  mutate(dup1 = ifelse(n()>1, TRUE, FALSE)) %>% 
  filter(dup1 == TRUE & rowid == max(rowid))%>%
  ungroup() # 0 obs to be removed

# 3-2) if multiple data sources, remove the lower data hierarchy
# Dengue cases are taken from the higher data hierarchy regardless of whether they are higher in the lower hierarchy
dup2 <- od %>%
  group_by(full_name, calendar_start_date, calendar_end_date)%>%
  filter(double == TRUE & source_cat2 < max(source_cat2))%>%
  ungroup() # 7425 obs to be removed

# remove duplicated rows
od <- od %>%
  filter(!rowid %in% dup1$rowid)%>%
  filter(!rowid %in% dup2$rowid)

# 3-3) different dengue counts from the same source category (e.g. MOH)
od <- od %>% 
  filter(!UUID == "MOH-PHL-20102019-Y03-00") # case counts from this source are abnormally high

od <- od %>%
  group_by(full_name, calendar_start_date, calendar_end_date)%>%
  mutate(diff = ifelse(n()>1, TRUE, FALSE)) %>% 
  ungroup()

unique(od$adm_0_name[od$diff==TRUE]) # check any large differences of dengue counts between different MOH source files

dup3 <- od %>% # remove the smaller counts
  group_by(full_name, calendar_start_date, calendar_end_date)%>%
  filter(diff == TRUE & dengue_total == min(dengue_total))%>% ungroup()

od <- od %>%
  filter(!rowid %in% dup3$rowid)

# 4) checking errors - different dengue count but from the exact same data source

od <- od %>%
  group_by(full_name, calendar_start_date, calendar_end_date, UUID) %>%
  mutate(error = ifelse(n()>1, TRUE, FALSE))%>% ungroup()

x <- od %>%
  filter(error == TRUE)%>%
  select(rowid, full_name, calendar_start_date, calendar_end_date, dengue_total, UUID)

# write.csv(x, "01_Dengue_data/OD_master/error_inspection.csv", row.names=F)

# final check
od[duplicated(od),]%>% arrange(full_name, calendar_start_date)


# update filingDB and metadata ===============================================
f <- read_excel("01_Dengue_data/OD_master/filingDB_allV.xlsx", sheet=1) 

f <- f %>%
  mutate(released = ifelse(released == "Y" & !UUID %in% od$UUID, "N (dup)", released))

meta_public <- f %>%
  filter(released == "Y")%>%
  select("UUID", "source_cat","country","period", "case_definition_original","metadata_description", "metadata_url", "metadata_steps")%>%
 
  arrange(desc(source_cat), country, period)

writexl::write_xlsx(f, "01_Dengue_data/OD_master/filingDB_allV_V1.2.1.xlsx")
write.csv(meta_public, "01_Dengue_data/OD_master/OD_V1.2/metadata_V1.2.1.xlsx")


# 383548 obs
# 2484414 obs (including brazil adm2)
od <- od %>% select(-(rowid:error))


# write.csv(od, "01_Dengue_data/OD_master/od2.csv", row.names=F)
# 
# od <- read.csv("01_Dengue_data/OD_master/od2.csv")

# EXTRACT builders ===========================================



##### 01 national extract builder  ########

# split by admin levels
od_adm0 <- od[od$S_res == "Admin0", ]
od_adm1 <- od[od$S_res == "Admin1", ]
od_adm2 <- od[od$S_res == "Admin2", ]

# check if any countries have more dengue case counts in admin1 data for any given years
a0_sum <- aggregate(dengue_total ~ adm_0_name + Year, data = od_adm0, FUN = sum)
a1_sum <- aggregate(dengue_total ~ adm_0_name + Year, data = od_adm1, FUN = sum)
a2_sum <- aggregate(dengue_total ~ adm_0_name + Year, data = od_adm2, FUN = sum)

# identify conflicts
# conflicts = admin1 data higher counts than admin0
# or admin1 data for years and countries not in admin 0

# loop through admin1 summaries to build conflicts list
conflicts = data.frame(adm_0_name = NA,
                       Year = NA,
                       Type = "NA")
conflicts = conflicts[-1, ]

for(i in 1:nrow(a1_sum)){
  # find any matching records in a0
  link = a0_sum[(a0_sum$adm_0_name == a1_sum$adm_0_name[i]) & 
                  (a0_sum$Year == a1_sum$Year[i]), ]
  if(nrow(link) == 0){
    conflicts = rbind(conflicts,
                      data.frame(adm_0_name = a1_sum$adm_0_name[i],
                                 Year = a1_sum$Year[i],
                                 Type = "Data_ad1_noData_ad0"))
  }else{
    if(a1_sum$dengue_total[i] > link$dengue_total){
      conflicts = rbind(conflicts,
                        data.frame(adm_0_name = link$adm_0_name,
                                   Year = link$Year,
                                   Type = "HigherData_ad1_than_ad0"))}
  }
  
}

# now go through the conflicts one by one, aggregate up the ad1 data then replace or add to ad0 as necessary
for(i in 1:nrow(conflicts)){
  # if replacing the record, remove original record first
  if(conflicts$Type[i] == "HigherData_ad1_than_ad0"){
    od_adm0 = od_adm0[!((od_adm0$adm_0_name == conflicts$adm_0_name[i]) &
                          (od_adm0$Year == conflicts$Year[i])), ]
  }
  
  # find the admin1 records and spatially aggregate
  ad1_recs = od_adm1[(od_adm1$adm_0_name == conflicts$adm_0_name[i]) &
                       (od_adm1$Year == conflicts$Year[i]), ]
  # identify unique start and end date combinations
  u_dates = unique(ad1_recs[, c("calendar_start_date", "calendar_end_date")])
  # now loop through dates aggregating records
  for(k in 1:nrow(u_dates)){
    ad1_recs_t <- ad1_recs[(ad1_recs$calendar_start_date %in% u_dates[k, 1]) &
                             (ad1_recs$calendar_end_date %in% u_dates[k, 2]), ]
    # compose the new record to be added
    new_rec = ad1_recs_t[1, ]
    new_rec$adm_1_name = NA
    new_rec$S_res = "Admin0"
    new_rec$full_name = new_rec$adm_0_name
    new_rec$dengue_total = sum(ad1_recs_t$dengue_total)
    
    # now replace with new record
    od_adm0 = rbind(od_adm0, new_rec)
  }
}

# now just re-sort by country name and date
od_adm0 = od_adm0[order(od_adm0$adm_0_name, od_adm0$calendar_start_date), ]



#### same again but for admin 2 data
# identify conflicts
# conflicts = admin2 data higher counts than admin0
# or admin2 data for years and countries not in admin 0

# loop through admin1 summaries to build conflicts list
conflicts = data.frame(adm_0_name = NA,
                       Year = NA,
                       Type = "NA")
conflicts = conflicts[-1, ]

for(i in 1:nrow(a2_sum)){
  # find any matching records in a0
  link = a0_sum[(a0_sum$adm_0_name == a2_sum$adm_0_name[i]) & 
                  (a0_sum$Year == a2_sum$Year[i]), ]
  if(nrow(link) == 0){
    conflicts = rbind(conflicts,
                      data.frame(adm_0_name = a2_sum$adm_0_name[i],
                                 Year = a2_sum$Year[i],
                                 Type = "Data_ad2_noData_ad0"))
  }else{
    if(a2_sum$dengue_total[i] > link$dengue_total){
      conflicts = rbind(conflicts,
                        data.frame(adm_0_name = link$adm_0_name,
                                   Year = link$Year,
                                   Type = "HigherData_ad2_than_ad0"))}
  }
}

# now go through the conflicts one by one, aggregate up the ad2 data then replace or add to ad0 as necessary
for(i in 1:nrow(conflicts)){
  # if replacing the record, remove original record first
  if(conflicts$Type[i] == "HigherData_ad2_than_ad0"){
    od_adm0 = od_adm0[!((od_adm0$adm_0_name == conflicts$adm_0_name[i]) &
                          (od_adm0$Year == conflicts$Year[i])), ]
  }
  
  # find the admin2 records and spatially aggregate
  ad2_recs = od_adm2[(od_adm2$adm_0_name == conflicts$adm_0_name[i]) &
                       (od_adm2$Year == conflicts$Year[i]), ]
  # identify unique start and end date combinations
  u_dates = unique(ad2_recs[, c("calendar_start_date", "calendar_end_date")])
  # now loop through dates aggregating records
  for(k in 1:nrow(u_dates)){
    ad2_recs_t <- ad2_recs[(ad2_recs$calendar_start_date %in% u_dates[k, 1]) &
                             (ad2_recs$calendar_end_date %in% u_dates[k, 2]), ]
    # compose the new record to be added
    new_rec = ad2_recs_t[1, ]
    new_rec$adm_1_name = NA
    new_rec$adm_2_name = NA
    new_rec$S_res = "Admin0"
    new_rec$full_name = new_rec$adm_0_name
    new_rec$dengue_total = sum(ad2_recs_t$dengue_total)
    
    # now replace with new record
    od_adm0 = rbind(od_adm0, new_rec)
  }
}

# now just re-sort by country name and date againa nd rename
od_adm0 = od_adm0[order(od_adm0$adm_0_name, od_adm0$calendar_start_date), ]
national_extract <- od_adm0

# save national extract
# write.csv(national_extract, file = "/Users/eideobra/Documents/GitHub/OpenDengue/master-repo/data/releases/V1.1/National_extract_V1_1.csv", row.names=F)
write.csv(national_extract, file = "01_Dengue_data/OD_master/OD_V1.2.1/releases/National_extract_V1_2_1.csv", row.names=F)






##############################################
##### 02 spatially preserved extract  ########
##############################################

# split into admin 0 and admin1
od_adm0 <- od[od$S_res == "Admin0", ]
od_adm1 <- od[od$S_res == "Admin1", ]
od_adm2 <- od[od$S_res == "Admin2", ]

# starting with admin2 look for records in admin1 that do NOT overlap then add to adm2.

# quick add for all the countries that are in admin1 but not in Admin2 at all
adm1_only = !(od_adm1$adm_0_name %in% od_adm2$adm_0_name)
toadd_adm1_only = od_adm1[adm1_only, ]
# delete to be added records for more efficient processing in next step
od_adm1 = od_adm1[!adm1_only, ]

# template for collection
toadd_mix = od_adm1[1, ]
toadd_mix = toadd_mix[-1, ]

# # loop through admin1 records and see which ones need to be added
# for(i in 1:nrow(od_adm1)){
#   # assemble records we already have for this country in the admin2 data
#   f_od_adm2 = od_adm2[od_adm2$adm_0_name == od_adm1$adm_0_name[i], ]
#   
#   # if this record is from a time when we don't have data then add it to the list
#   if(!(od_adm1$calendar_start_date[i] %in% f_od_adm2$calendar_start_date)){
#     toadd_mix = rbind(toadd_mix, od_adm1[i, ])
#   }
# }

# f_od_adm2 = 9060
# toadd_mix = 10610

ctr = unique(od_adm2$adm_0_name[od_adm2$adm_0_name %in% od_adm1$adm_0_name])

 
for(c in 1:length(ctr)){ 

  s_od_adm1 = od_adm1[od_adm1$adm_0_name %in% ctr[c], ]
  f_od_adm2 = od_adm2[od_adm2$adm_0_name %in% ctr[c], ]

  for(i in 1:nrow(s_od_adm1)){

    # if this record is from a time when we don't have data then add it to the list
    if(!(s_od_adm1$calendar_start_date[i] %in% unique(f_od_adm2$calendar_start_date))){
      toadd_mix = rbind(toadd_mix, s_od_adm1[i, ])
  }
  }
}



# combine and sort
od_adm2 <- rbind(od_adm2, toadd_adm1_only, toadd_mix)
od_adm2 = od_adm2[order(od_adm2$adm_0_name, od_adm2$calendar_start_date), ]



### now repeat at adm0 level

# quick add for all the countries that are not in Admin1 at all
national_only = !(od_adm0$adm_0_name %in% od_adm2$adm_0_name)
toadd_national_only = od_adm0[national_only, ]
# delete to be added records for more efficient processing in next step
od_adm0 = od_adm0[!national_only, ]

# template for collection
toadd_mix = od_adm0[1, ]
toadd_mix = toadd_mix[-1, ]

# # loop through admin0 records and see which ones need to be added
# for(i in 1:nrow(od_adm0)){
#   # assemble records we already have for this country in the admin2/1 data
#   f_od_adm2 = od_adm2[od_adm2$adm_0_name == od_adm0$adm_0_name[i], ]
#   
#   # if this record is from a time when we don't have data then add it to the list
#   if(!(od_adm0$calendar_start_date[i] %in% f_od_adm2$calendar_start_date)){
#     toadd_mix = rbind(toadd_mix, od_adm0[i, ])
#   }
# }

ctr = unique(od_adm2$adm_0_name[od_adm2$adm_0_name %in% od_adm0$adm_0_name])

 
for(c in 1:length(ctr)){ 

  s_od_adm0 = od_adm0[od_adm0$adm_0_name %in% ctr[c], ]
  f_od_adm2 = od_adm2[od_adm2$adm_0_name %in% ctr[c], ]

  for(i in 1:nrow(s_od_adm0)){

    # if this record is from a time when we don't have data then add it to the list
    if(!(s_od_adm0$calendar_start_date[i] %in% unique(f_od_adm2$calendar_start_date))){
      toadd_mix = rbind(toadd_mix, s_od_adm0[i, ])
  }
  }
}



# combine and sort
spatial_extract <- rbind(od_adm2, toadd_national_only, toadd_mix)
spatial_extract = spatial_extract[order(spatial_extract$adm_0_name, spatial_extract$calendar_start_date), ]

# write.csv(spatial_extract, file = "/Users/eideobra/Documents/GitHub/OpenDengue/master-repo/data/releases/V1.1/Spatial_extract_V1_1.csv", row.names=F)
write.csv(spatial_extract, file = "01_Dengue_data/OD_master/OD_V1.2/releases/Spatial_extract_V1_2_1.csv", row.names=F)









###############################################
##### 03 temporally preserved extract  ########
###############################################
plyr::count(od$T_res)

brazil <- od %>% filter(adm_0_name == "BRAZIL")
od2 <- od %>% filter(!adm_0_name == "BRAZIL")

# split into Weekly, Monthly and Annual datasets
od_Week <- od2[od2$T_res == "Week", ]
od_Month <- od2[od2$T_res == "Month", ]
od_Year <- od2[od2$T_res == "Year", ]

# starting with Week look for records in Month that do NOT overlap spatially or temporally then add to Week

# quick add for all the areas in Month or Year that are not in Week
ind_Month <- !(od_Month$full_name %in% od_Week$full_name)
ind_Year <- !(od_Year$full_name %in% od_Week$full_name)

toadd1 <- od_Month[ind_Month, ]
od_Month = od_Month[!ind_Month, ]
toadd2 <- od_Year[ind_Year, ]
od_Year = od_Year[!ind_Year, ]

# template for collection
toadd_mix1 = od_Month[1, ]
toadd_mix1 = toadd_mix1[-1, ]

# loop through Monthly records and see which ones need to be added
# for(i in 1:nrow(od_Month)){
#   # assemble records from the corresponding area in Weekly data
#   f_od_Week = od_Week[od_Week$full_name == od_Month$full_name[i], ]
#   
#   # if this record is from a time when we don't have data then add it to the list
#   if(!(od_Month$calendar_start_date[i] %in% f_od_Week$calendar_start_date)){
#     toadd_mix1 = rbind(toadd_mix1, od_Month[i, ])
#   }
# }

f_name = unique(od_Week$full_name[od_Week$full_name %in% od_Month$full_name])

c_progress = 0
 
for(c in 1:length(f_name)){ 
  s_od_Month = od_Month[od_Month$full_name %in% f_name[c], ]
  f_od_Week = od_Week[od_Week$full_name %in% f_name[c], ]

  for(i in 1:nrow(s_od_Month)){

    # if this record is from a time when we don't have data then add it to the list
    if(!(s_od_Month$calendar_start_date[i] %in% unique(f_od_Week$calendar_start_date))){
      toadd_mix1 = rbind(toadd_mix1, s_od_Month[i, ])
    }

  }
  c_progress <- c_progress + 1
  cat("Outer Loop Progress: ", c_progress, "/", length(f_name), "\r")

}

# add monthly datasets to weekly records then repeat with annual data
od_Week = rbind(od_Week, toadd_mix1)

# template for collection
toadd_mix2 = od_Year[1, ]
toadd_mix2 = toadd_mix2[-1, ]

# loop through Yearly records and see which ones need to be added
# for(i in 1:nrow(od_Year)){
#   # assemble records from the corresponding area in Weekly data
#   f_od_Week = od_Week[od_Week$full_name == od_Year$full_name[i], ]
#   
#   # if this record is from a time when we don't have data then add it to the list
#   if(!(od_Year$calendar_start_date[i] %in% f_od_Week$calendar_start_date)){
#     toadd_mix2 = rbind(toadd_mix2, od_Year[i, ])
#   }
# }

f_name = unique(od_Week$full_name[od_Week$full_name %in% od_Year$full_name])

c_progress = 0
 
for(c in 1:length(f_name)){ 
  s_od_Year = od_Year[od_Year$full_name %in% f_name[c], ]
  f_od_Week = od_Week[od_Week$full_name %in% f_name[c], ]

  for(i in 1:nrow(s_od_Year)){

    # if this record is from a time when we don't have data then add it to the list
    if(!(s_od_Year$calendar_start_date[i] %in% unique(f_od_Week$calendar_start_date))){
      toadd_mix2 = rbind(toadd_mix2, s_od_Year[i, ])
  }
  }
  c_progress <- c_progress + 1
  cat("Outer Loop Progress: ", c_progress, "/", length(f_name), "\r")
}

temporal_extract <- rbind(od_Week, toadd1, toadd2, toadd_mix2)


#########################################
# Extract BRAZIL data separately ======
plyr::count(brazil$T_res)

# split into Weekly, Monthly and Annual datasets
bra_Week <- brazil[brazil$T_res == "Week", ]
bra_Month <- brazil[brazil$T_res == "Month", ]
bra_Year <- brazil[brazil$T_res == "Year", ]


# starting with Week look for records in Month that do NOT overlap spatially or temporally then add to Week

# quick add for all the areas in Month or Year that are not in Week
ind_Month <- !(bra_Month$full_name %in% bra_Week$full_name)
ind_Year <- !(bra_Year$full_name %in% bra_Week$full_name)

btoadd1 <- bra_Month[ind_Month, ]
bra_Month = bra_Month[!ind_Month, ]
btoadd2 <- bra_Year[ind_Year, ]
bra_Year = bra_Year[!ind_Year, ]

# find records that both spatially and temporally overlap
num_w <- bra_Week %>%
  filter(full_name %in% bra_Month$full_name)%>%
  group_by(full_name, Year)%>%
  tally() %>% rename(week_n = n)

num_m <- bra_Month %>%
 filter(full_name %in% num_w$full_name)%>%
 group_by(full_name, Year)%>%
 tally() %>% rename(month_n = n)

num <- merge(num_w, num_m, by=c("full_name", "Year"), all = T) %>%
  filter(!is.na(week_n) & !is.na(month_n))

# no overlaps found for now !!!!!

# #subset records to loop through (only the records for 2013 overlap in Week and Month)
# btoadd3 = bra_Month[bra_Month$full_name %in% num$full_name & !bra_Month$Year == 2013 , ]
# btoadd4 = bra_Week[bra_Week$full_name %in% num$full_name & !bra_Week$Year == 2013, ]
# 
# # subset records to loop through (only the records for 2013 overlap in Week and Month)
# s_bra_Month = bra_Month[bra_Month$full_name %in% num$full_name & bra_Month$Year == 2013 , ]
# f_bra_Week = bra_Week[bra_Week$full_name %in% num$full_name & bra_Week$Year == 2013, ]
# 
# # template for collection
# btoadd_mix1 = bra_Month[1, ]
# btoadd_mix1 = btoadd_mix1[-1, ]
# 
# for(i in 1:nrow(s_bra_Month)){
# 
#     # if this record is from a time when we don't have data then add it to the list
#     if(!(s_bra_Month$calendar_start_date[i] %in% unique(f_bra_Week$calendar_start_date))){
#       btoadd_mix1 = rbind(btoadd_mix1, s_bra_Month[i, ])
#     }
# 
#  cat("Loop Progress: ", i, "/", nrow(s_bra_Month), "\r")
# 
# }
# 
bra_Week = rbind(bra_Week, bra_Month)
 


# repeat with annual data
# find records that both spatially and temporally overlap
num_w <- bra_Week %>%
  filter(full_name %in% bra_Year$full_name)%>%
  group_by(full_name, Year)%>%
  tally() %>% rename(week_n = n)

num_y <- bra_Year %>%
 filter(full_name %in% num_w$full_name)%>%
 group_by(full_name, Year)%>%
  tally() %>% rename(year_n = n)

num2 <- merge(num_w, num_y, by=c("full_name", "Year"), all = T) %>%
  filter(!is.na(week_n) & !is.na(year_n))

# no overlaps found for now !!!!!


# subset records to loop through (only the records for 2013 overlap in Week and Month)
# s_bra_Year = bra_Year[bra_Year$full_name %in% num2$full_name & bra_Year$Year == 2013, ]
# f_bra_Week = bra_Week[bra_Week$full_name %in% num2$full_name & bra_Week$Year == 2013, ]
# 
# # template for collection
# btoadd_mix1 = bra_Month[1, ]
# btoadd_mix1 = btoadd_mix1[-1, ]
# 
# for(i in 1:nrow(s_bra_Month)){
# 
#     # if this record is from a time when we don't have data then add it to the list
#     if(!(s_bra_Month$calendar_start_date[i] %in% unique(f_bra_Week$calendar_start_date))){
#       btoadd_mix1 = rbind(btoadd_mix1, s_bra_Month[i, ])
#     }
# 
#  cat("Loop Progress: ", i, "/", nrow(s_bra_Month), "\r")
# 
# }

bra_extract <- rbind(bra_Week, btoadd1, btoadd2, bra_Year)


# combine and sort
temporal_extract <- rbind(temporal_extract, bra_extract)
temporal_extract = temporal_extract[order(temporal_extract$adm_0_name, temporal_extract$calendar_start_date), ]

# write.csv(temporal_extract, file = "/Users/eideobra/Documents/GitHub/OpenDengue/master-repo/data/releases/V1.1/Temporal_extract_V1_1.csv", row.names=F)
write.csv(temporal_extract, file = "01_Dengue_data/OD_master/OD_V1.2/releases/Temporal_extract_V1_2_1.csv", row.names=F)

### extract comparison metrics

# basic dimensions
nrow(national_extract)
nrow(spatial_extract)
nrow(temporal_extract)

# number of countries
length(unique(national_extract$adm_0_name))
length(unique(spatial_extract$adm_0_name))
length(unique(temporal_extract$adm_0_name))

# distribution of records over spatial scale
100 * table(national_extract$S_res) / nrow(national_extract)
100 * table(spatial_extract$S_res) / nrow(spatial_extract)
100 * table(temporal_extract$S_res) / nrow(temporal_extract)

# number of countries with subnational data
length(unique(spatial_extract$adm_0_name[spatial_extract$S_res != "Admin0"]))
length(unique(temporal_extract$adm_0_name[temporal_extract$S_res != "Admin0"]))

# distribution of records over temporal scale
100 * table(national_extract$T_res) / nrow(national_extract)
100 * table(spatial_extract$T_res) / nrow(spatial_extract)
100 * table(temporal_extract$T_res) / nrow(temporal_extract)

# distribution of records over case defintion
100 * table(national_extract$case_definition_standardised) / nrow(national_extract)
100 * table(spatial_extract$case_definition_standardised) / nrow(spatial_extract)
100 * table(temporal_extract$case_definition_standardised) / nrow(temporal_extract)


# comparison between previous versions
od_old <- read.csv("/Users/eideobra/Documents/GitHub/OpenDengue/master-repo/data/releases/V1.1/National_extract_V1_1.csv")
od_new <- read.csv("/Users/eideobra/Documents/GitHub/OpenDengue/master-repo/data/releases/V1.2/National_extract_V1_2.csv")


# total number of countries
length(unique(od_old$ISO_A0))
length(unique(od_new$ISO_A0))
# new countries added:
unique(od_new$ISO_A0)[!(unique(od_new$ISO_A0) %in% unique(od_old$ISO_A0))]

# new total cases
sum(od_old$dengue_total, na.rm = T)
sum(od_new$dengue_total, na.rm = T)


