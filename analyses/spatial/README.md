# Spatial Analysis

## Preparing_adm2.ipynb

This jupyter notebook cleans Administrative level two (adm2) data provided in
COG metadata so that they match as closely as possible to adm2 regions found in
the Global Administrative Database (GADM).

### Inputs

- Three geojsons containing map shapes one each for the UK, Northern Ireland
  counties and the channel islands:
  * `"UK.json"`
  * `"NI_counties.geojson"`
  * `"channel_islands.json"`
  
- A csv file containing cleaning information converting data found in sequence
  metadata to the true admin2 region: `adm2_cleaning.csv` This involves
  correcting spelling mistakes, identifying the correct adm2 region for a more
  precise region (eg Solihull --> Birmingham) and merging some real adm2 regions
  together to form what is in the sequence metadata (eg West Midlands).
  
- Sequence metadata (not provided here as it contains non-shareable information)

- Mapping files for the England, Wales and Scotland in the spatial data are
  taken from the [Global Adminstrative Database](https://gadm.org/).

- Mapping files for Northern Irish counties were taken from [Open Data
  NI](https://www.opendatani.gov.uk/dataset?tags=Counties)

- Mapping files for the channel islands were taken from
  https://gist.github.com/markmarkoh


### How it runs

1. Map files are read in, and UK and NI county files are combined

2. The clean locations file (`adm2_cleaning.csv`) is read in. For each adm2
   entry in the sequence metadata that is not present in the GADM list of adm2
   regions:
   *  If only one option is available for the sequence adm2 in the file, the
      clean location is given as that option. This is the case where spelling
      mistakes are present, or where sequence metadata was too high resolution.
    * If multiple locations are given, this means that GADM locations must be
      merged in order to map the sequences.
    * If the location is in the Welsh county Rhondda Cynon Taff, this is
      specifically altered as it contains commas and therefore isn't properly
      recorded in the csv.
      
3. Some cities are merged into wider counties if the cities had no sequence
   metadata associated with them, but cases reported from them. We assumed in
   these cases that some sequences with the surrounding adm2 as their metadata
   may be from these cities, and so they added into the list to be merged into
   the larger adm2 region in the map.
   
4. For each location in the GADM metadata, if it is present in those locations
   which must be merged together, a new column is added to the dataframe
   containing the larger adm2. This dataframe is then dissolved so that shape
   polygons for the smaller locations are combined to give the larger one. This
   dataframe is then used
   
5. Finally, the each sequence in the metadata is assigned to one of these clean
   locations. These can then be added to the merged_locs dataframe and used in
   maps.


### Outputs

- A dictionary containing the sequence name as a key and the new, cleaned adm2
  as the value
  
- A dataframe containing polygons of locations which correspond to cleaned
  locations

