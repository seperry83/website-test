
# Import Relevant Functions -----------------------------------------------

`%>%` <- magrittr::`%>%`

read_csv <- readr::read_csv
locale <- readr::locale

read_html <- rvest::read_html
html_element <- rvest::html_element
html_text2 <- rvest::html_text2

include_graphics <- knitr::include_graphics
kable <- knitr::kable
include_graphics <- knitr::include_graphics
is_html_output <- knitr::is_html_output
is_latex_output <- knitr::is_latex_output
combine_words <- knitr::combine_words

R6Class <- R6::R6Class

`%/%` <- patchwork:::`/.ggplot`
`%|%` <- patchwork:::`|.ggplot`
plot_annotation <- patchwork::plot_annotation
wrap_plots <- patchwork::wrap_plots
plot_layout <- patchwork::plot_layout
area <- patchwork::area

`%m+%` <- lubridate::`%m+%`
`%m-%` <- lubridate::`%m-%`
parse_date_time <- lubridate::parse_date_time
year <- lubridate::year
month <- lubridate::month
ymd <- lubridate::ymd
`%within%` <- lubridate::`%within%`
interval <- lubridate::interval

sym <- rlang::sym
syms <- rlang::syms

str_match <- stringr::str_match
str_detect <- stringr::str_detect
str_remove <- stringr::str_remove
str_extract_all <- stringr::str_extract_all

glue <- glue::glue

fct_rev <- forcats::fct_rev
fct_shift <- forcats::fct_shift

left_join <- dplyr::left_join
filter <- dplyr::filter
pull <- dplyr::pull
mutate <- dplyr::mutate
group_by <- dplyr::group_by
ungroup <- dplyr::ungroup
rename <- dplyr::rename
summarize <- dplyr::summarize
select <- dplyr::select
arrange <- dplyr::arrange
n <- dplyr::n
relocate <- dplyr::relocate
desc <- dplyr::desc
slice <- dplyr::slice
if_else <- dplyr::if_else
distinct <- dplyr::distinct
count <- dplyr::count
row_number <- dplyr::row_number
across <- dplyr::across
pick <- dplyr::pick
reframe <- dplyr::reframe

ggplot <- ggplot2::ggplot
aes <- ggplot2::aes
geom_line <- ggplot2::geom_line
geom_point <- ggplot2::geom_point
geom_boxplot <- ggplot2::geom_boxplot
geom_segment <- ggplot2::geom_segment
annotate <- ggplot2::annotate
theme_bw <- ggplot2::theme_bw
ggtitle <- ggplot2::ggtitle
labs <- ggplot2::labs
theme <- ggplot2::theme
scale_color_manual <- ggplot2::scale_color_manual
scale_x_continuous <- ggplot2::scale_x_continuous
scale_x_discrete <- ggplot2::scale_x_discrete
scale_y_continuous <- ggplot2::scale_y_continuous
scale_x_date <- ggplot2::scale_x_date
scale_fill_manual <- ggplot2::scale_fill_manual
ggsave <- ggplot2::ggsave
element_blank <- ggplot2::element_blank
element_text <- ggplot2::element_text
guide_legend <- ggplot2::guide_legend
vars <- ggplot2::vars
geom_text <- ggplot2::geom_text
geom_col <- ggplot2::geom_col
theme_void <- ggplot2::theme_void
guides <- ggplot2::guides
facet_wrap <- ggplot2::facet_wrap
coord_cartesian <- ggplot2::coord_cartesian
xlab <- ggplot2::xlab
margin <- ggplot2::margin
unit <- ggplot2::unit

createWorkbook <- openxlsx::createWorkbook
addWorksheet <- openxlsx::addWorksheet
writeData <- openxlsx::writeData
saveWorkbook <- openxlsx::saveWorkbook

kable_styling <- kableExtra::kable_styling
add_header_above <- kableExtra::add_header_above
footnote <- kableExtra::footnote
column_spec <- kableExtra::column_spec

darken <- colorspace::darken
lighten <- colorspace::lighten

brewer.pal <- RColorBrewer::brewer.pal

complete <- tidyr::complete
nest <- tidyr::nest

here <- here::here

map <- purrr::map
pmap <- purrr::pmap
map2_chr <- purrr::map2_chr

geom_borderline <- ggborderline::geom_borderline

label_comma <- scales::label_comma

geom_treemap <- treemapify::geom_treemap
geom_treemap_text <- treemapify::geom_treemap_text
geom_treemap_subgroup_border <- treemapify::geom_treemap_subgroup_border

