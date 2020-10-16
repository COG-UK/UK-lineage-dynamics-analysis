const dataCsv = "http://localhost:8000/results/estimated-introduction-index.csv"

const chartSpec = vl
      .markLine({point: true})
      .data(dataCsv)
      .encode(
          vl.x().fieldT("date").title("Date"),
          vl.y().fieldQ("num_intros").title("Estimated Introduction Index"),
          vl.color().fieldN("primary_location").title("Source Location"),
          vl.tooltip(["primary_location"])
      )
      .width(700)
      .height(500)
      .toJSON();

vegaEmbed('#eii', chartSpec);
