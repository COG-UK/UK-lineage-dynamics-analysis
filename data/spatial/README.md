# Spatial data

Spatial input data used in the analyses. 


- Three geojsons containing map shapes one each for the UK, Northern Ireland
  counties and the channel islands:
  * [`UK.json`](map_files/UK.json)
  * [`NI_counties.geojson`](map_files/NI_counties.geojson)
  * [`channel_islands.json`](map_files/channel_islands.json)

- Mapping files for the England, Wales and Scotland in the spatial data were
  taken from the [Global Adminstrative Database](https://gadm.org/).

- Mapping files for Northern Irish counties were taken from [Open Data
  NI](https://www.opendatani.gov.uk/dataset?tags=Counties)

- Mapping files for the channel islands were taken from
  https://gist.github.com/markmarkoh
  
- A `.csv` file containing cleaning information converting data found in sequence
  metadata to the true admin2 region: [`adm2_cleaning.csv`](adm2_cleaning.csv) This involves
  correcting spelling mistakes, identifying the correct adm2 region for a more
  precise region (eg Solihull --> Birmingham) and merging some real adm2 regions
  together to form what is in the sequence metadata (eg West Midlands).

- A list of all the final admin 2 regions used in the spatial analysles can be found in [`final_list_adm2_used.tsv`](final_list_adm2_used.tsv).