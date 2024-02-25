tue <- tibble::tibble(
  Day = "Tue",
  Type = c(rep("ECR Workshop",4), "ECR Social"),
  When = c(
    "1300-1330",
    "1330-1430",
    "1430-1500",
    "1500-1700",
    "1800-"
    ),
  What = c(
    "Welcome",
    "Workshop 1",
    "Coffee Break",
    "Workshop 2",
    "Dinner+Social"
    ),
  Track = "",
  Paper = 1,
  Detail = rep("...",5),
  Where = c(html(denim_fa), html(denim_fa), html(merchant_fa), html(denim_fa), html(tbc_fa))
  ) 

wed <- tibble::tibble(
  Day = "Wed",
  Type = c(
    rep("ECR Workshop",3), 
    rep("GISRUK Opens",3), 
    rep("GISRUK Session 1",3),
    rep("GISRUK Session 2",2),
    "GISRUK Social"),
  When = c(
    "0930-1045",
    "1045-1115",
    "1115-1230",
    "1100-1300",
    "1300-1330",
    "1330-1430",
    "1430-1530",
    "",
    "1530-1600",
    "1600-1700",
    "",
    "1800-"
    ),
  What = c(
    "Workshop 3",
    "Coffee Break",
    "Workshop 4",
    "Registration+Lunch",
    "Welcome",
    "Keynote 1",
    "Track A",
    "Track B",
    "Coffee Break",
    "Track A",
    "Track B",
    "Conference reception"
    ),
    Track="",
    Paper = 1,
    Detail = rep("...",12),
    Where = c(
      html(denim_fa), 
      html(merchant_fa),
      html(denim_fa), 
      html(merchant_fa),
      html(herringbone_fa), 
      html(herringbone_fa), 
      html(herringbone_fa), 
      html(tweed_fa),
      html(merchant_fa),
      html(herringbone_fa), 
      html(tweed_fa),
      html(merchant_fa))
)

thu <- tibble::tibble(
  Day = "Thu",
  Type = c(
    rep("GISRUK Session 3",3), 
    rep("GISRUK Session 4",3), 
    rep("GISRUK Lunch | Posters | Keynote",2),
    rep("GISRUK Session 5",4),
    rep("GISRUK Session 6",2),
    "GISRUK Social"),
  When = c(
    "0930-1030",
    "",
    "1030-1045",
    "1045-1145",
    "",
    "",
    "1145-1330",
    "1330-1430",
    "1430-1530",
    "",
    "",
    "1530-1600",
    "1600-1700",
    "",
    "1800-"
    ),
  What = c(
    "Track A",
    "Track B",
    "Coffee Break",
    "Track A",
    "Track B",
    "Track C",
    "Lunch + Posters",
    "Keynote 2",
    "Track A",
    "Track B",
    "Track C",
    "Coffee Break",
    "Track A",
    "Track B",
    "Conference reception"
    ),
    Track = "",
    Paper = 1,
    Detail = rep("...",15),
    Where = c(
      html(herringbone_fa), 
      html(tweed_fa), 
      html(merchant_fa),
      html(herringbone_fa), 
      html(tweed_fa),
      html(denim_fa), 
      html(merchant_fa),
      html(herringbone_fa), 
      html(herringbone_fa), 
      html(tweed_fa),
      html(denim_fa), 
      html(merchant_fa),
      html(herringbone_fa), 
      html(tweed_fa),
      html(tbc_fa))
)

fri <- tibble::tibble(
  Day = "Fri",
  Type = c(
    rep("GISRUK Session 7",3), 
    rep("GISRUK Session 8",3), 
    rep("Close",2)),
  When = c(
    "0930-1030",
    "",
    "1030-1045",
    "1045-1145",
    "",
    "1145-1200",
    "1200-1300",
    "1300-1330"
    ),
  What = c(
    "Track A",
    "Track B",
    "Coffee Break",
    "Track A",
    "Track B",
    "Coffee Break",
    "Keynote 3",
    "Awards and Close"
    ),
    Track = "",
    Paper = 1,
    Detail = rep("...",8),
    Where = c(
      html(herringbone_fa), 
      html(tweed_fa), 
      html(merchant_fa),
      html(herringbone_fa), 
      html(tweed_fa), 
      html(merchant_fa),
      html(herringbone_fa), 
      html(herringbone_fa))
)
days <- dplyr::bind_rows(tue, wed, thu, fri)

days |> group_by(Day, )

readr::write_csv(days, "./program/program.csv")
