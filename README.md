# sLUC emission factors related to deforestation and land occupation

This repository contains the code and tables from land use change and land occupation emissions



\###################################

# sLUC emission factors data structure:

\###################################



The sLUC emission tables (CSV format) are located in the "data" folder, under the "sLUC\_emission\_factors".

There are four subfolders in here: deforestation\_emission\_factors\_XXX . The final portion of the name (i.e, XXX) represents the traceability level: global, admin0 (administrative level 0: country), admin1 (administrative level 1: state, province), and admin2 (administrative level 2: municipality, county, etc.)

Each subfolder has more folders inside containing the data per each crop category. The folder structure "individual\_commodities\_GAS" will have the independent CSV tables for each commodity in the following name structure "EF\_ADMX\_CROP\_GAS.csv"





\####################################################

# sLUC TABLE VARIABLES:

\####################################################



Each emission factors CSV table contains the following variables:



GID\_0: Country (Administrative level 0) code from GADM 4.1
GID\_1: Administrative level 1 code from GADM 4.1 (e.g., provinces, states)
GID\_2: Administrative level 2 code from GADM 4.1 (e.g., municipalities, counties) 
crop\_type: SPAM\_CODE from below
LD\_2020 through LD\_2024:  Linearly discounted emissions for reporting year 20XX in tonnes (t)
production\_2020 through production\_2024: Annual crop production in tonnes
EF\_2020 through EF\_2024: Emission factor for reporting year 20XX in tonnes CO2e/tonne crop
Data for many gases: CO2e, CO2, CH4, and N2O. All are expressed in CO2e
CO2e is the sum of the others (CO2 + CH4 + N2O).



\######################################################

# jdLUC TABLE VARIABLES:

\######################################################



Each emission factors CSV table contains the following variables:



GID\_0: Country (Administrative level 0) code from GADM 4.1
GID\_1: Administrative level 1 code from GADM 4.1
GID\_2: Administrative level 2 code from GADM 4.1  
commodity: SPAM\_CODE from below

area\_\_ha: Total area of crop X in the jurisdiction. This is used to calculate production (area\_\_ha \* yield\_mt\_ha = production\_mt)

yield\_mt\_ha: Yield in metric tons for crop X based of harvested area. Based on SPAM 2020V2 data

production\_mt: Annual crop production in tonnes
LD\_2020 through LD\_2024:  Linearly discounted emissions for reporting year 20XX in tonnes (t)
EF\_2020 through EF\_2024: Emission factor for reporting year 20XX in tonnes CO2e/tonne crop
Data for many gases: CO2e, CO2, CH4, and N2O. All are expressed in CO2e
CO2e is the sum of the others (CO2 + CH4 + N2O).





\##########################################################################

# YIELD FACTORS TABLE VARIABLES:

\##########################################################################



Each emission factors CSV table contains the following variables:



GID\_0: Country (Administrative level 0) code from GADM 4.1
GID\_1: Administrative level 1 code from GADM 4.1
GID\_2: Administrative level 2 code from GADM 4.1  
crop: SPAM\_CODE from below
yield\_mt: Yield in metric tons for crop X based of physical area (already corrected for multiple cropping based on SPAM crop intensity factors). Based on SPAM 2020V2 data

yield\_kg: Yield in kilograms for crop X based of physical area (already corrected for multiple cropping based on SPAM crop intensity factors). Based on SPAM 2020V2 data

yield\_factor\_kg: The inverse of the yield (1/yield) in kg-1







\###################################

# DATASET DESCRIPTION:

\###################################



The dataset developed by Fitts et al. (2025) provides globally consistent, spatially explicit estimates of statistical land use change (sLUC) emission factors for 42 agricultural crops across multiple spatial scales—global, national (ADM0), subnational (ADM1), and local administrative units (ADM2). Emission factors quantify greenhouse gas emissions from deforestation linked to agricultural expansion over a 20-year assessment period (2001–2020), aligning with the first full time span of Global Forest Watch’s annual tree cover loss and forest carbon flux data. Emission factors are available for reporting years 2020 through 2024 and we expect to update the dataset annually as new tree cover loss and emissions data become available. The dataset integrates several harmonized, open-source geospatial inputs: the SPAM 2020 v2.0 cropland maps (10 km resolution) for crop distribution and production; Global Forest Watch’s tree cover loss (Hansen et al. 2013, annual updates) and forest carbon flux model (Harris et al. 2021; Gibbs et al. 2025) for estimating deforestation-related emissions; Global Pasture Watch (Parente et al. 2024) for pasture extent; and drivers of forest loss (Sims et al. 2025) to isolate agriculture-driven deforestation.



This dataset also includes standardized yields and yield factors (i.e., 1/yield) to be used in land occupation calculations based on data from SPAM 2020v2.



\###################################

# DATASET CITATION:

\###################################



Fitts, L.A., O. James, D. Gibbs, E. Goldman, N.Harris, M. Ramlow, L. Sloat, B. Wielgosz, C. Wilson, C. Winchester, A. Ernstoff, T. Hohmann, V. Rossi, and A. Stern. 2025. “Statistical land use change emissions from deforestation and land occupation for crops.” Technical Note. Washington, DC: World Resources Institute. Available online at doi.org/10.46830/writn.25.00085.



\###################################

# COMMODITY CODES:

\###################################



Individual commodities:

SPAM\_CODE	COMMODITY\_NAME
BANA	Banana
BARL	Barley
BEAN	Bean
CASS	Cassava
CHIC	Chickpea
CNUT	Coconut
COCO	Cocoa
ACOF	Arabica\_Coffee
RCOF	Robusta\_Coffee
COTT	Cotton
COWP	Cowpea
GROU	Groundnut
LENT	Lentil
MAIZ	Maize
PMIL	Pearl\_Millet
SMIL	Small\_Millet
OILP	Oil\_Palm
PIGE	Pigeon\_Pea
PLNT	Plantain
POTA	Potato
RAPE	Rapeseed
RICE	Rice
SESA	Sesame\_Seed
SORG	Sorghum
SOYB	Soybean
SUGB	Sugarbeet
SUGC	Sugarcane
SUNF	Sunflower
SWPO	Sweet\_Potato
TEAS	Tea
TOBA	Tobacco
WHEA	Wheat
YAMS	Yams

Grouped commodities:

SPAM\_CODE	COMMODITY\_NAME
OCER	Other\_Cereals
OFIB	Other\_Fibre\_Crops
OOIL	Other\_Oil\_Crops
OPUL	Other\_Pulses
ORTS	Other\_Roots
REST	Rest\_of\_Crops
TEMF	Temperate\_Fruit
TROF	Tropical\_Fruit
VEGE	Vegetables



\###################################

# CAUTIONS/LIMITATIONS:

\###################################



\#############################

Scope:

\#############################



This dataset focuses exclusively on metrics relevant for tracking GHG emissions from deforestation, defined as land use change from forests (or other areas with at least 10 percent tree canopy density and woody vegetation greater than 5 m in height) and associated soil GHG emissions to cropland (e.g., agricultural commodity-linked deforestation). LUC emissions due to other land use conversions (e.g., conversion of natural grasslands to croplands, conversion of natural grassland to managed grassland, etc.) are not covered. Despite this limitation, deforestation is a dominant contributor to land use change emissions, thus the methods described here should cover a major part of scope 1 and scope 3 land sector emissions. See Fitts et al. 2025 “Beyond deforestation” section for an interim solution on how to calculate the LUC emission factors beyond deforestation.



This database is intended only for crop commodities. Non-crop commodities (animal products, pulp and paper, etc.) are out of scope.

The current methodology is agnostic to companies’ traceability approaches for their supply chains. It assumes that companies have followed traceability requirements and recommendations in the Greenhouse gas protocol and are applying the correct sLUC method given their circumstances.



\#############################

Traceability level to choose:

\#############################



We recommend using sLUC at the finest level of traceability available (i.e., municipalities or counties are preferred over states and regions, and the latter are preferred over country-level traceability) to more accurately represent emissions in the sourcing area. When using a national-level emission factor, emissions are averaged across the country and areas of high or low emissions would not be identified or appropriately represented when reporting.



Some considerations to have when choosing the municipality-level traceability:



-When a municipality has very low commercial production volumes (e.g., <10 tonnes of a crop for a whole municipality), we recommend scaling up a traceability level (i.e., admin 1) and use the state/provincial-level sLUC emission factor instead. Companies can choose their own production threshold that is meaningful in their supply chain and justify their choice when reporting.



This methodology excludes subsistence crop production from the cropland footprint in our analysis because it is unlikely to be linked to commercial commodity value chains. Therefore, some locations might see elevated emissions that get allocated to very little commercial crop production. Another scenario when these elevated emissions might be assigned to very little amount of crop is when a municipality is a city where very little crop production occurs, but the nature of the statistical data allocates a small amount of the crop to that area. In both scenarios, we recommend scaling up to administrative level 1 sLUC emission factor.



-For very small municipalities smaller than or of similar size to the best-available spatial unit for gridded sLUC (e.g., <10km2) it is recommended to scale up to administrative level 1 (e.g., state/province).



\#############################

Uncertainty:

\#############################



Our ability to calculate uncertainty on our results depends on the uncertainty of our input datasets, including SPAM, which does not provide pixel-level uncertainty and the carbon flux model (CFM). The CFM also does not quantify uncertainty in emissions from the CFM at spatial scales below climate domain (boreal, temperate, subtropical, and tropical forests) due to limitations in the uncertainty of the input datasets. However, the GHG protocol’s LSRS does not require quantitative uncertainty assessments.

