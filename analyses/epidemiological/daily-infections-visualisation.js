const incidenceUrl = "http://localhost:8000/results/estimated-daily-infections.csv";

const incidenceChart = vl
      .markLine({point: true})
      .data(incidenceUrl)
      .encode(
          vl.x().fieldT("date").title("Date"),
          vl.y().fieldQ("num_infs").title("Estimated daily infections"),
          vl.color().fieldN("location").title("Source Location"),
          vl.tooltip(["location"])
      )
      .width(700)
      .height(500)
      .toJSON();

const deathsUrl = "http://localhost:8000/results/clean-jhu-deaths.csv";

const deathsChart = vl.markLine({point: true})
      .data(deathsUrl)
      .encode(
          vl.x().fieldT("date").title("Date"),
          vl.y().fieldQ("daily_deaths").title("Daily difference in cumulative deaths"),
          vl.color().fieldN("location").title("Source Location"),
          vl.tooltip(["location"])
      )
      .width(700)
      .height(500)
      .toJSON();


const combinedChart = vl.vconcat(incidenceChart,deathsChart).toJSON();

vegaEmbed('#daily-infections', combinedChart);
