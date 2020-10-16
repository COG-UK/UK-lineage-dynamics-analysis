wget "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
mv time_series_covid19_deaths_global.csv raw-data/jhu-deaths.csv

wget "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv"
mv WPP2019_TotalPopulationBySex.csv raw-data/un-population.csv
