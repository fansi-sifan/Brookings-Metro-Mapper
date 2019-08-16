# Brookings-Metro-Mapper

 
### Product:
* generate & download a *customizable*, *interactive* U.S. chloropleth map from a metro-level or county-level dataset you upload (or select form data warehouse). Download publication-ready map and source code to customize further in Rmarkdown!

#### Customization features:
* **custom scale:** 
    * enter custom cutpoints 
    * custom RGB color selector
    * toggle categorical or continuous scale 
* **custom geography filter:** (dropdown menu or search)
    * specify the counties/metros to map by name 
    * map all counties/metros within a chosen state or multiple chosen states
    * filter for all counties in a chosen metro area or multiple chosen metros
    * Use the above geography filters in combination with each other!
* toggle to bubble map showing 2 variables simultaneously 

#### Interactivity features:
* 2 types of tooltips (hover & click)
* zoom and click
* toggle terrain layers

#### Download your customized map:
* download a static image of your customized map (.pdf or .png)
* download an ineractive version of your customized map (.html)
* download publication-ready version of your customized map with custom title, subtitle, legend title, notes source, etc.
* download data warehouse dataset (or your dataset) (.csv)

##### Download R code that produces customized publication-ready map & customize further



### INSTRUCTIONS


The first three "Step 1" subparts are mandatory in order to proceed (Steps 1A, 1B, 1C). Mark those three subparts **in order** from top to bottom and then press "Show Data" to display map.

### Avoid Common Error Triggers:

* **If your dataset contains *county-level* data, you MUST mark *"county"* for (Step 1C) "The dataset you chose is on what geography level?" (Likewise, mark "metro" for metro-level datasets)**
  * All data warehouse datasets beginning with **"co_"** are **county** level (Likewise, those with **"cbsa_"** are **metro** level). Be sure to mark Step 1C accordingly! 

* Your uploaded dataset MUST meet the criteria (below)

* The variable in your dataset selected in the dropdown menu in **step 2** is unsuitable to be mapped. Usually this is because it contains FIPS codes or place names. Select a different variable from the step 2 dropdown menu.

 * Pressing "Show data" when the (Step 1D) "Filter Dataset by Jurisdiction" checkbox is selected AND all filter dropdowns (Filter by metro area / state / county) are left blank.
 
 * Pressing "Show Data" when Step 1A or Step 1B is left blank

**Phantom Error Note:** Some errors may pop up for a couple seconds and then disappear when the map finishes redrawing. *Do not be alarmed.* On some machines, the map may take a few seconds to appear.


### Criteria for Uploaded Datasets

1) **FIPS code column:** dataset must contain a column of county OR cbsa 5-digit FIPS codes *named "cbsa_code","geoid", or "stco_code"*
2) **Tidy:** the unit of observation must be cbsa OR county *(1 row = 1 county OR 1 cbsa)*
3) **Single geography level:** dataset must be entirely county-level or entirely cbsa-level, not a mixture (NO datasets where some rows as CBSAs and other rows as Counties)



### Step 4 Detail

Note: Custom label options (title, subtitle, notes, source) apply only to "Download Map (Publication Ready!)" and "Download the Code" products (except "legend title" which applies to all map downloads).

**Download the map!(Publication Ready!)** 
**"Download the code"** generates code that you can use a to reproduce on your PC the customized publication-ready map. The "Download csv!" and "Download Shapefiles!" buttons equip you with all the dependencies you need to run the code in an Rmarkdown file on your machine. You can then edit the code, customizing the map further. Be sure to follow the instructions below. Especially if you are working outside of Brookings, filepaths in the code MUST be edited in order for the dependencies to be loaded. 


## Instructions to download code to reproduce publication-ready map

1. Click "Download the Code!". An HTML window will open containing code that produces the publication ready map; copy all content (below the dependency table) into an empty Rmarkdown file with the YAML header set to "output: html_document"
2. *Install Dependencies* Click "Download csv!" & "Download Shapefiles!" A csv file containing the dataset will download as well as a zipfile containing shapefiles and geography crosswalk. Ensure that the csv file containing the dataset is saved under the name "fulldata.csv" in the same folder as the Rmarkdown file. Ensure that the zipfile is saved under "shape_files.zip" (downloading zipfile is not necessary if on a Brookings computer with access to V drive). Ensure paths to downloaded files and working directory are properly set in code (see warning)
3. Ensure that all necessary packages are installed on your machine. Then, click knit!

**WARNING: If not on a Brookings computer with access to the V drive, you MUST download the zipfile containing the three shapefiles and geography crosswalk accordingly adjust the four filepaths in the Rmarkdown code before knitting. Moreover, you must delete the code at the very end of the document that loads the logo, "logo2.jpeg".** 

### Dependencies for Downloaded Code

Here are the dependencies for running downloaded code on Brookings and non-Brookings machines:

Dependency&nbsp;&nbsp;&nbsp;	| File name	| Access inside Brookings	  | Access outside Brookings
-----------|-----------|----------------|----------
Your Dataset |	 full_data.csv	| "Download csv!" button	| "Download csv!" button 
CBSA Shapefile	| cbsas51_inset_ld.shp	| V drive |	 "Download shapefiles!" button 
CBSA Shapefile	| counties51_inset_ld.shp	| V drive	| "Download shapefiles!" button 
CBSA Shapefile	| states51_inset_ld.shp	| V drive	| "Download shapefiles!" button 
geography crosswalk	| county_cbsa_state.csv	| V drive	| "Download shapefiles!" button
logo	| logo2.jpeg | V drive | NA


### Index of Data Warehouse Datasets



#### County-Level Datasets

County&nbsp;&nbsp;&nbsp; | file name | descriptions
-------|-----------|-------------
1 | co_acs | Selected indicators calculated from ACS summary tables
2 | co_acs_raw | Original columns fetched from ACS summary tables
3 | co_export | Export volume and intensity from Export Monitor
4 | co_jobdensity | Weighted actual job density, (jobs per sq mile)
5 | co_oic | Indicators from old industrial cities report
6 | co_oow | Seven major Out-of-work groups
7 | co_univ_licensing | University licensing activity and income
8 | co_univ_rd | University R&D investment, 2017
9 | co_uspto | Utility patent grants, 2015

#### CBSA-Level Datasets

CBSA&nbsp; &nbsp;&nbsp;| file name | descriptions
-----|-----------|---------------------------------------------------
1 | cbsa_acs | Selected indicators calculated from ACS summary tables
2 | cbsa_acs_raw | Original columns fetched from ACS summary tables
3 | cbsa_export | Export volume and intensity from Export Monitor
4 | cbsa_jobdensity | Weighted actual job density, (jobs per sq mile)
5 | cbsa_digitalization | Digitalization scores and share of jobs by digital level
6 | cbsa_housing_price | Housing-income ratio
7 | cbsa_univ_licensing | University licensing activity and income
8 | cbsa_univ_rd | University R&D investment, 2017
9 | cbsa_uspto | Utility patent grants, 2015
10| cbsa_regpat | OECD regpat patent counts, 2012
11| cbsa_patentcomplex | Patent complexity index, 2015
12| cbsa_vc | VC investment, 2017
13| cbsa_biz_rd | Domestic R&D performed by companies for selected metros, 2015
14| cbsa_i5hgc | Inc 5000 high growth firms by metro, 2017
15| cbsa_metromonitor | Rankings and values from metro monitor report, 2019
16| cbsa_low_wage_worker | Nine major low-wage-worker groups

### Known Issues

* When resolution is set to low for a non-warehouse dataset, hover tooltips become innacurate (click tooltips remain accurate)

* **'Clicking a Zillion Times' Advisory:** Clicking on an input a zillion times is not advised. The server redraws the map with each user's click on an input, even if that click is made obselete by a future click. (E.g., if a user toggles an option from "off" to "on" and then a second later from "on" to "off", the map will be redrawn once reflecting "on" and again to reflect "off"). On some machines, loading may take a few seconds, and a chain of many clicks may create a backlog


