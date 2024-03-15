
library(gtExtras)
library(fontawesome)
library(dplyr)
library(readr)
library(glue)
library(purrr)
library(magick)
library(ggplot2)
library(ggtext)
library(showtext)
library(stringr)
sysfonts::font_add_google("Jost", "jost", regular.wt=500)
showtext_auto() 
## turn off if no longer needed
showtext_auto(FALSE)


denim <- "#4d7ab0"
herringbone <- "#d06c4b"
merchant <- "#8c4772"
tweed <- "#589157"
tbc <- "#9a9a9a"

fn_text <- glue(
      '<<fontawesome::fa("info-circle", fill="#4d7ab0")>> Denim |
       <<fontawesome::fa("info-circle", fill="#d06c4b")>> Herringbone | 
      <<fontawesome::fa("info-circle", fill="#8c4772")>> Merchant Hall |
      <<fontawesome::fa("info-circle", fill="#589157")>> Tweed |
       <<fontawesome::fa("info-circle", fill="#9a9a9a")>> Other ',
      .open = "<<", .close = ">>"
    )

denim_fa <- glue(
      '<<fontawesome::fa("info-circle", fill={denim})>>',
      .open = "<<", .close = ">>"
    )

herringbone_fa <- glue(
      '<<fontawesome::fa("info-circle", fill={herringbone})>>',
      .open = "<<", .close = ">>"
    )

merchant_fa <- glue(
      '<<fontawesome::fa("info-circle", fill={merchant})>>',
      .open = "<<", .close = ">>"
    )

tweed_fa <- glue(
      '<<fontawesome::fa("info-circle", fill={tweed})>>',
      .open = "<<", .close = ">>"
    )

tbc_fa <-  glue(
      '<<fontawesome::fa("info-circle", fill={tbc})>>',
      .open = "<<", .close = ">>"
    )

link_fa <- glue(
      '<<fontawesome::fa("link")>>',
      .open = "<<", .close = ">>"
    )

days <- read_csv("program/program_sandpit.csv") |> 
  mutate(
    Author=if_else(is.na(IsSolo) & !is.na(Author), paste0(Author, ", ", CoAuthor), Author),
    Title=str_to_title(Title), 
    across(c(When), ~if_else(is.na(.x), "", .x)),
    Paper=pmap_chr(list(Title, Author, Link),
     ~html(glue("<span style='font-style:italic; font-size:14pt'> {..1}</span><br><span style='font-style:italic; font-size:13.5pt'>--&nbsp;{..2}</span>"))
    ),
     Paper=case_when(
      !is.na(Track) ~ Track,
      !is.na(Author) ~ Paper,
      TRUE ~ What),
     Room=case_when(
      !is.na(Author) ~ "",
      TRUE ~ Room
     )) |>
     select(-What) |> rename(What=Paper)

# From https://rpubs.com/kaustav/table_contest_2020
gt_theme_gisruk <- function(data,...) {
  data |>
  opt_all_caps()  |>
  opt_table_font(
    font = list(
      google_font("Jost"),
      default_fonts()
    )
  ) |>
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(1)
      ),
      locations = cells_body(
        columns = everything(),
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  |>
  tab_options(
    column_labels.background.color = "white",
    table.border.top.width = px(1),
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    row_group.border.top.width = px(1),
    table.border.bottom.width = px(1),
    column_labels.border.top.width = px(1),
    column_labels.border.top.color = "transparent",
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = "black",
    data_row.padding = px(1),
    source_notes.font.size = 12,
    table.font.size = 16,
    heading.align = "left",
    heading.border.bottom.width = px(1),
    ...
  ) |>
  tab_style(
      style=list(
        cell_borders(
          sides = c("bottom"),  
          weight = px(2),
          color ="white"),
      cell_fill(color="#eeeeee")),
     locations = cells_row_groups()
  ) #|>
  # tab_header(title="",subtitle = html(fn_text))
}

days |> filter(Day=="Tue") |> select(When, What, Room, Type) |>
rename(R=Room) |>
gt(groupname_col = "Type") |> 
  cols_align("left", Type) |>
    fmt_markdown(c(R, What)) |>
    gt_theme_gisruk() |>
    cols_align(align = "right", columns = R) |>
    cols_width(matches("When") ~ pct(13)) |>
    cols_width(matches("What") ~ pct(80)) |>
    tab_options(data_row.padding = px(.2),
              table.font.size = px(22),
              column_labels.font.size = px(22), table.align="left", row_group.font.size = px(22))  |>
              tab_header(title=html("<span style='font-weight:bold; #font-size:19pt'>Tue 9th April</span>")) |>
    tab_style(
      style = cell_borders(
          sides = c("bottom"),  
          weight = px(1), 
          color ="white"
          ),
          locations = cells_body(rows = c(2,5))) |>
             gtsave("program/tue.png", expand = 10)



days |> filter(Day=="Wed", Type=="ECR Workshop") |> select(When, What, Room, Type) |> 
rename(R=Room) |>
gt(groupname_col = "Type") |> 
  cols_align("left", Type) |>
    fmt_markdown(columns=c(R, What)) |>
    gt_theme_gisruk() |>
    cols_align(align = "right", columns = R) |>
    tab_options(data_row.padding = px(.2),
              table.font.size = px(22),
              column_labels.font.size = px(22), table.align="left", row_group.font.size = px(22)) |>
    cols_width(matches("When") ~ pct(13)) |>
    cols_width(matches("What") ~ pct(80)) |>
    tab_style(
      style = cell_borders(
          sides = c("bottom"),  
          weight = px(1), 
          color ="white"
          ),
          #locations = cells_body(rows = c(1, 4, 9,10,11,13,14,15, 18,19,20,22,23,24))) |>
          locations = cells_body(rows = c(1))) |>
       tab_header(title=html("<span style='font-weight:bold; font-size:19pt'>Wed 10th April</span>")) |>
     gtsave("program/wed_workshop.png", expand = 10)


days |> filter(Day=="Wed", Type!="ECR Workshop") |> select(When, What, Room, Type) |> 
rename(R=Room) |>
gt(groupname_col = "Type") |> 
  cols_align("left", Type) |>
    fmt_markdown(columns=c(R, What)) |>
    gt_theme_gisruk() |>
    cols_align(align = "right", columns = R) |>
    tab_options(data_row.padding = px(.2),
              table.font.size = px(22),
              column_labels.font.size = px(22), table.align="left", row_group.font.size = px(22)) |>
    cols_width(matches("When") ~ pct(13)) |>
    cols_width(matches("What") ~ pct(80)) |>
    tab_style(
      style = cell_borders(
          sides = c("bottom"),  
          weight = px(1), 
          color ="white"
          ),
          #locations = cells_body(rows = c(1, 4, 9,10,11,13,14,15, 18,19,20,22,23,24))) |>
          locations = cells_body(rows = c(3, 5,6,7,9,10,11, 14,15, 16, 18,19, 20))) |>
       tab_header(title=html("<span style='font-weight:bold; font-size:19pt'>Wed 10th April</span>")) |>
     gtsave("program/wed_conf.png", expand = 10)

days |> filter(Day=="Wed") |> select(When, What, Room, Type) |> 
rename(R=Room) |>
gt(groupname_col = "Type") |> 
  cols_align("left", Type) |>
    fmt_markdown(columns=c(R, What)) |>
    gt_theme_gisruk() |>
    cols_align(align = "right", columns = R) |>
    tab_options(data_row.padding = px(.2),
              table.font.size = px(22),
              column_labels.font.size = px(22), table.align="left", row_group.font.size = px(22)) |>
    cols_width(matches("When") ~ pct(15)) |>
    tab_style(
      style = cell_borders(
          sides = c("bottom"),  
          weight = px(1), 
          color ="white"
          ),
          locations = cells_body(rows = c(1, 4, 9,10,11,13,14,15, 18,19,20,22,23,24))) |>
       tab_header(title=html("<span style='font-weight:bold; font-size:19pt'>Wed 10th April</span>")) |>
     gtsave("program/wed.png", expand = 10)



days |> filter(Day=="Thu") |> select(When, What, Room, Type) |> 
rename(R=Room) |>
gt(groupname_col = "Type") |> 
  cols_align("left", Type) |>
    fmt_markdown(columns=c(R, What)) |>
    gt_theme_gisruk() |>
    cols_align(align = "right", columns = R) |>
    tab_options(data_row.padding = px(.2),
              table.font.size = px(22),
              column_labels.font.size = px(22), table.align="left", row_group.font.size = px(22)) |>
    cols_width(matches("When") ~ pct(13)) |>
    cols_width(matches("What") ~ pct(80)) |>
    tab_style(
      style = cell_borders(
          sides = c("bottom"),  
          weight = px(1), 
          color ="white"
          ),
          locations = cells_body(rows = c(1,2,3,5,6,7,10,11,12,14,15,16,18,19,20,23,24,25,26,27,29,30,31,33,34,35,38,39,40,42,43,44))) |>
      tab_header(title=html("<span style='font-weight:bold; #font-size:19pt'>Thu 11th April</span>"))  |>
     gtsave("program/thu.png", expand = 10)



days |> filter(Day=="Fri") |> select(When, What, Room, Type) |> 
rename(R=Room) |>
gt(groupname_col = "Type") |> 
  cols_align("left", Type) |>
    fmt_markdown(columns=c(R, What)) |>
    gt_theme_gisruk() |>
    cols_align(align = "right", columns = R) |>
   cols_width(matches("When") ~ pct(13)) |>
   cols_width(matches("What") ~ pct(80)) |>
   tab_options(data_row.padding = px(.5),
              table.font.size = px(23),
              column_labels.font.size = px(22), table.align="left", row_group.font.size = px(22)) |>
  tab_style(
      style = cell_borders(
          sides = c("bottom"),  
          weight = px(1), 
          color ="white"
          ),
          locations = cells_body(rows = c(1,2,3,5,6,7,10,11,12,14,15,16)) 
    ) |>
     tab_header(title=html("<span style='font-weight:bold; font-size:19pt'>Fri 12th April</span>")) |>
    gtsave("program/fri.png", expand = 10)


days |> filter(Day=="Poster") |> select(When, What, Type) |> 
gt(groupname_col = "Type") |> 
  cols_align("left", Type) |>
    fmt_markdown(columns=c(What)) |>
    gt_theme_gisruk() |>
    #cols_align(align = "right", columns = R) |>
   cols_width(matches("When") ~ pct(13)) |>
   cols_width(matches("What") ~ pct(87)) |>
   tab_options(data_row.padding = px(.5),
              table.font.size = px(23),
              column_labels.font.size = px(22), table.align="left", row_group.font.size = px(22)) |>
     tab_header(title=html("<span style='font-weight:bold; font-size:19pt'>Posters: Thu 11th April, 1145--1330</span>")) |>
    gtsave("program/posters.png", expand = 10)



tue <- image_read("program/tue.png") |> 
  image_fill('none') |> 
  as.raster()
wed <- image_read("program/wed.png") |> 
  image_fill('none') |> 
   as.raster()
wed_works <- image_read("program/wed_workshop.png") |> 
  image_fill('none') |> 
   as.raster()
wed_conf <- image_read("program/wed_conf.png") |> 
  image_fill('none') |> 
   as.raster()
thu <- image_read("program/thu.png") |> 
  image_fill('none') |>
  as.raster()
fri <- image_read("program/fri.png") |> 
  image_fill('none') |> 
  as.raster()
sponsors <- image_read("program/sponsors.png") |> 
  image_fill('none') |> 
  as.raster() 

room <- image_read("program/tue_room.png") |> 
  image_fill('none') |> 
  as.raster() 

os_logo <- image_read("img/logos/OS_logo.jpg") |> 
  image_fill('none') |> 
  as.raster() 

esri_logo <- image_read("img/logos/Esri_UK_Emblem_tag_sRGB.png") |> 
  image_fill('none') |> 
  as.raster() 

ai_logo <- image_read("img/logos/logo_ati.png") |> 
  image_fill('none') |> 
  as.raster() 

osgeo_logo <- image_read("img/logos/OSGeoUK2.png") |> 
  image_fill('none') |> 
  as.raster() 

cdrc_logo <- image_read("img/logos/logo_cdrc.webp") |> 
  image_fill('none') |> 
  as.raster() 

rgs_logo <- image_read("img/logos/logo_rgs.webp") |> 
  image_fill('none') |> 
  as.raster() 


sysfonts::font_add(
  family = "Iosevka", 
  regular = "/Users/rogerbeecham/Library/Fonts/iosevka-semibold.ttc")
showtext_auto() 
## turn off if no longer needed
showtext_auto(FALSE)

conf <- glue::glue("<span style='font-size:110.0pt; font-family:\"jost\"'>GISRUK 2024 | University of Leeds</span>")
conf_web <- glue::glue("<span style='font-size:105.0pt; font-family:\"Iosevka\"'>2024.gisruk.org</span>")
web <- glue::glue("<span style='font-size:105.0pt; font-family:\"Iosevka\"'>2024.gisruk.org</span>")

p <- ggplot() +
  annotation_raster(tue, .02, .45, 1.14, 1.41) +
  annotation_raster(wed, .02, .475, .59, 1.235) +
  annotation_raster(fri, 0.03, .458, 0, .58) +
  annotation_raster(thu, .515, .98, .07, 1.32) +
  annotation_raster(room, .75, .98, 1.315, 1.33) +
  scale_x_continuous(limits=c(0, 1), expand=c(0,0)) +
  scale_y_continuous(limits=c(0, 1.414), expand=c(0,0)) +
  annotate("richtext", x=.98, y=1.41, label=conf, fill = NA, label.color = NA, hjust="right", vjust="top") +
  #annotate("richtext", x=.98, y=1.385, label=conf_theme, fill = NA, label.color = NA, hjust="right", vjust="top", size=36) +
  #annotation_raster(sponsors, .75, .98, 0, .07) +
  theme_classic()+
    theme(
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    axis.text = element_blank(), axis.line = element_blank()
  ) 
ggsave("program/printout.png", p, width=10, height=14.1, dpi=700)

p <- ggplot() +
  annotation_raster(tue, .02, .42, 1.03, 1.258) +
  annotation_raster(wed_works, .02, .44, .85, 1.03) +
  annotation_raster(wed_conf, .02, .47, .095, .775) +
  #annotation_raster(fri, 0.03, .458, 0, .58) +
  annotation_raster(thu, .53, .95, .02, 1.4) +
  annotation_raster(room, .02, .3, 1.305, 1.32) +
  scale_x_continuous(limits=c(0, 1), expand=c(0,0)) +
  scale_y_continuous(limits=c(0, 1.414), expand=c(0,0)) +
  annotate("richtext", x=.02, y=1.285, label="**ECR Workshops**", fill = NA, label.color = NA, hjust="left", vjust="top", family="Jost", size=30) +
  annotate("richtext", x=.02, y=.805, label="**GISRUK Conference**", fill = NA, label.color = NA, hjust="left", vjust="top", family="Jost", size=30) +
  annotate("richtext", x=.02, y=1.39, label=conf, fill = NA, label.color = NA, hjust="left", vjust="top") +
  annotate("richtext", x=.02, y=1.36, label=web, fill = NA, label.color = NA, hjust="left", vjust="top") +
  #annotate("richtext", x=.98, y=1.385, label=conf_theme, fill = NA, label.color = NA, hjust="right", vjust="top", size=36) +
  #annotation_raster(sponsors, .75, .98, 0, .07) +
  theme_classic()+
    theme(
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    axis.text = element_blank(), axis.line = element_blank()
  ) 
ggsave("program/printout.png", p, width=10, height=14.1, dpi=700)

poster_img <- image_read("program/posters.png") |> 
  image_fill('none') |> 
  as.raster()

dinner <- "-- The Faversham, 1-5 Springfield Mount, Woodhouse, Leeds LS2 9NG"
venue <- "-- Cloth Court Hall, Cloth Hall Court, Quebec Street, LS1 2HA"
gisruk_bp <- "-- GISRUK Best Paper, see 2024.gisruk.org for voting" 
os_open <- "-- GISRUK & OSGeo:UK GoFundGeo Award, see 2024.gisruk.org/osgeo/"
casa <- "-- CASA Best Spatial Analysis Paper, in mem. Sinesio Alves Junior (1967-2010)"
epb <- "-- Environment & Planning B Urban Data:Code, see 2024.gisruk.org/si/"

p <- ggplot() +
  annotation_raster(fri, .02, .45, .59, 1.258) +
  annotation_raster(room, .02, .3, 1.305, 1.32) +
  annotation_raster(poster_img, .53, .98, .36, 1.34) +
  annotation_raster(os_logo, .02, .35, .15, .25) +
  annotation_raster(esri_logo, .36, .72, .13, .27) +
  annotation_raster(ai_logo, .02, .15, .04, .1) +
  annotation_raster(osgeo_logo, .16, .35, .03, .11) +
  annotation_raster(rgs_logo, .38, .52, .03, .12) +
  annotation_raster(cdrc_logo, .58, .82, .04, .12) +
  annotate("richtext", x=.02, y=1.285, label="**GISRUK Conference**", fill = NA, label.color = NA, hjust="left", vjust="top", family="Jost", size=30) +
  annotate("richtext", x=.52, y=1.38, label="**GISRUK Posters: Thu 11th April**", fill = NA, label.color = NA, hjust="left", vjust="top", family="Jost", size=30) +
  annotate("richtext", x=.02, y=1.39, label=conf, fill = NA, label.color = NA, hjust="left", vjust="top") +
  annotate("richtext", x=.02, y=1.36, label=web, fill = NA, label.color = NA, hjust="left", vjust="top") +
  annotate("richtext", x=.02, y=.56, label="**GISRUK SI**", fill = NA, label.color = NA, hjust="left", vjust="top", family="Jost", size=26, text.color="#525252") +
  annotate("richtext", x=.02, y=.54, label=epb, fill = NA, label.color = NA, hjust="left", vjust="top", family="Jost", size=26, text.color="#525252") +
  annotate("richtext", x=.02, y=.51, label="**Awards**", fill = NA, label.color = NA, hjust="left", vjust="top", family="Jost", size=26, text.color="#525252") +
  annotate("richtext", x=.02, y=.49, label=gisruk_bp, fill = NA, label.color = NA, hjust="left", vjust="top", family="Jost", size=26, text.color="#525252") +
  annotate("richtext", x=.02, y=.47, label=os_open, fill = NA, label.color = NA, hjust="left", vjust="top", family="Jost", size=26, text.color="#525252") +
  annotate("richtext", x=.02, y=.45, label=casa, fill = NA, label.color = NA, hjust="left", vjust="top", family="Jost", size=26, text.color="#525252") +
  annotate("richtext", x=.02, y=.415, label="**Main venue**", fill = NA, label.color = NA, hjust="left", vjust="top", family="Jost", size=26, text.color="#525252") +
  annotate("richtext", x=.02, y=.395, label=venue, fill = NA, label.color = NA, hjust="left", vjust="top", family="Jost", size=26, text.color="#525252") +
  annotate("richtext", x=.02, y=.365, label="**Dinner venue**", fill = NA, label.color = NA, hjust="left", vjust="top", family="Jost", size=26, text.color="#525252") +
  annotate("richtext", x=.02, y=.345, label=dinner, fill = NA, label.color = NA, hjust="left", vjust="top", family="Jost", size=26, text.color="#525252") +
  #annotate("richtext", x=.98, y=1.385, label=conf_theme, fill = NA, label.color = NA, hjust="right", vjust="top", size=36) +
  #annotation_raster(sponsors, .75, .98, 0, .07) +
  scale_x_continuous(limits=c(0, 1), expand=c(0,0)) +
  scale_y_continuous(limits=c(0, 1.414), expand=c(0,0)) +
  theme_classic()+
    theme(
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    axis.text = element_blank(), axis.line = element_blank()
  ) 
ggsave("program/printout2.png", p, width=10, height=14.1, dpi=700)


