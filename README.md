# Japanese Interment and Immigration 

I don't know how to make a proper MAKEFILE yet,
so here's how I build the datasets for this project:

### Set up directories for downloads
```bash
mkdir ./data/
mkdir ./data/census/
mkdir ./data/fullcount/
mkdir ./data/maps/
```

### Download some data from sources which I can't script

Go to the [Behind Barbed Wire](https://www.arcgis.com/home/item.html?id=787f1fabd34c49308df8fe0d4dc7470f) 
arcgis page and hit the *Download* button.

Extract the zip file to inside the `data/` subdirectory:
```bash
mkdir ./data/BehindBarbedWire_StoryMap/
unzip "~/Downloads/BehindBarbedWire_StoryMap_Data.zip" -d data/BehindBarbedWire_StoryMap
```

### Download IPUMS and NHGIS data
```bash
R CMD BATCH R/download-data.R
```

This should download to the appropriate locations in the `data/` subdir.
```
.
├── data
│   ├── BehindBarbedWire_StoryMap
│   │   ├── BehindBarbedWire_StoryMap_AssemblyCentersMap_Data_1_1.csv
│   │   ├── BehindBarbedWire_StoryMap_Data.zip
│   │   ├── BehindBarbedWire_StoryMap_EvacuationMap_Data.csv
│   │   ├── BehindBarbedWire_StoryMap_InternmentCampLocationsMap_Data.csv
│   │   └── BehindBarbedWire_StoryMap_JAICNewspapersMap_Data.csv
│   ├── census
│   │   ├── usa_*****.dat.gz
│   │   └── usa_*****.xml
│   ├── fullcount
│   │   ├── usa_*****.dat.gz
│   │   └── usa_*****.xml
│   └── maps
│       ├── 1940
│       │   └── nhgis****_shape.zip
│       ├── nhgis****_shape.zip
.
```
where the ipums and nhgis files will have a number in thier file name 
corresponding to the extract number on your account (e.g., `usa_00093.dat.gz`).

### Clean data into csv form
```bash
R CMD BATCH R/data.R
```

This should create the files:
  - `./data/demographics.csv` which contains county-year level summary statistics on race, population, migration, etc.
  - `./data/distances.csv` which contains the distance matrix from all ten camp locations to all mainland 1990 counties
  - `./data/data.csv` which is the final dataset with demographics and distance data for county-year observations crosswalked onto their 1990 areas

### Summarize and visualize the data
```bash
Rscript -e "outline.rmd"
```

Should create a sum stats table 
  - `tables/ctysumstats.tex`
and some figures
  - `figures/countymap.png`
  - `figures/migrationmap.png`

### Run and summarize regressions
```bash
R CMD BATCH R/results.R
```

Should create the regression tables:
  - `tables/distreg.tex`
  - `tables/ezdistreg.tex`
