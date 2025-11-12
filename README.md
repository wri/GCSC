# sLUC emission factors related to deforestation
This repository contains the code and tables from land use change and land occupation emissions


LUC_Emission_factors: Emission factor tables in CSV

There are four subfolders in here: deforestation_emission_factors_XXX . The final portion of the name (i.e, XXX) represents the traceability level: global, admin0 (administrative level 0: country), admin1 (administrative level 1: state, province), and admin2 (administrative level 2: municipality, county, etc.)

Each subfolder has more folders inside plus four lose csv files labeled "emission_factors_GAS_ADMX_master.csv" These are the tables that compile all commodities within a single file. I recommend to work with these if all commodities will be incorporated at once. 
If you seek individual commodities, the folder "individual_commodities_GAS" will have the independent CSV tables for each commodity in the following name structure "EF_ADMX_CROP_GAS.csv"  

If needed, there is a key_gadm.csv table in this LUC_Emission_Factors core folder that helps link the GADM codes with the names of the countries (GID_0), states (GID_1), municipalities (GID_2).
####################

Each emission factors CSV table contains the following variables:

GID_0: Country (Administrative level 0) code from GADM 4.1
GID_1: Administrative level 1 code from GADM 4.1
GID_2: Administrative level 2 code from GADM 4.1         
crop_type: SPAM_CODE from below
LD_2020 through LD_2024:  Linearly discounted emissions for reporting year 20XX in tonnes (t)
production_2020 through production_2024: Annual crop production in tonnes
EF_2020 through EF_2024: Emission factor for reporting year 20XX in tonnes CO2e/tonne crop
Data for many gases: CO2e, CO2, CH4, and N2O. All are expressed in CO2e
CO2e is the sum of the others (CO2 + CH4 + N2O).

####################
# COMMODITY CODES:

Individual commodities:

SPAM_CODE	COMMODITY_NAME 
BANA	Banana
BARL	Barley
BEAN	Bean
CASS	Cassava
CHIC	Chickpea
CNUT	Coconut
COCO	Cocoa
ACOF	Arabica_Coffee
RCOF	Robusta_Coffee
COTT	Cotton
COWP	Cowpea
GROU	Groundnut
LENT	Lentil
MAIZ	Maize
PMIL	Pearl_Millet
SMIL	Small_Millet
OILP	Oil_Palm
PIGE	Pigeon_Pea
PLNT	Plantain
POTA	Potato
RAPE	Rapeseed
RICE	Rice
SESA	Sesame_Seed
SORG	Sorghum
SOYB	Soybean
SUGB	Sugarbeet
SUGC	Sugarcane
SUNF	Sunflower
SWPO	Sweet_Potato
TEAS	Tea
TOBA	Tobacco
WHEA	Wheat
YAMS	Yams

Grouped commodities:

SPAM_CODE	COMMODITY_NAME 
OCER	Other_Cereals
OFIB	Other_Fibre_Crops
OOIL	Other_Oil_Crops
OPUL	Other_Pulses
ORTS	Other_Roots
REST	Rest_of_Crops
TEMF	Temperate_Fruit
TROF	Tropical_Fruit
VEGE	Vegetables


