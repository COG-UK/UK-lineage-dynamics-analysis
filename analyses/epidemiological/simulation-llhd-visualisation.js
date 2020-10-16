const simLlhdCsv = "http://localhost:8000/results/sim-study-2-llhds.csv";

const llhdRectangles = vl
    .markRect()
    .data(simLlhdCsv)
    .encode(
        vl.x().fieldO("alpha").title("Alpha"),
        vl.y().fieldO("beta").scale({
            "reverse": true
        }).title("Beta"),
        vl.color().fieldQ("llhd").scale({
            "range": ["#416D9D", "#DEAC58"]
        }).legend({
            "title": "Log-likelihood"
        })
    );

const paramsPoint = vl
    .markCircle()
    .data({
        "values": [ {
            "alpha": "07",
            "beta": "24"
        }, {
            "alpha": "08",
            "beta": "20"
        }]
    })
    .encode(
        vl.x().fieldO("alpha"),
        vl.y().fieldO("beta"),
        vl.size({
            "value": 100
        })
    );

vegaEmbed("#simulation-llhd-heatmap",
          vl
          .layer(llhdRectangles, paramsPoint)
          .width(700)
          .toJSON());
