## ---- include = FALSE, setup--------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cols.print = 3
)
multiqc_data_path = system.file("extdata", "wgs/multiqc_data.json", package = "TidyMultiqc")

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("TidyMultiqc")

## ---- eval=FALSE--------------------------------------------------------------
#  library(TidyMultiqc)

## ----paged.print=TRUE---------------------------------------------------------
df = TidyMultiqc::load_multiqc(multiqc_data_path)
df

## -----------------------------------------------------------------------------
TidyMultiqc::load_multiqc(multiqc_data_path, sections = 'raw')

## -----------------------------------------------------------------------------
df_both = TidyMultiqc::load_multiqc(multiqc_data_path, sections = c('raw', 'general'))
ncol(df_both)

## -----------------------------------------------------------------------------
library(magrittr)

df %>%
  ggplot2::ggplot(ggplot2::aes(x=metadata.sample_id, y=general.percent_duplication)) +
  ggplot2::geom_col()

## -----------------------------------------------------------------------------
t.test(df$general.percent_gc, mu=41)

## -----------------------------------------------------------------------------
TidyMultiqc::load_multiqc(
  multiqc_data_path, 
  find_metadata = function(sample, parsed) {
    # Split the sample ID to obtain some metadata
    segments <- stringr::str_split(sample, "_")[[1]]
    c(
      batch = segments[[1]],
      sample = segments[[2]]
    )
  }
)

## -----------------------------------------------------------------------------
TidyMultiqc::load_multiqc(
  multiqc_data_path, 
  find_metadata = function(sample, parsed) {
    # This gives us the path to the fastqc output file
    filepath = parsed$report_data_sources$FastQC$all_sections[[sample]]
    # Split into path segments
    path_segments = stringr::str_split(filepath, "/")[[1]]
    # The filename is the last path segment
    filename = dplyr::last(path_segments)
    # Split the filename using dots and underscores
    name_segments = stringr::str_split(filename, "[_\\.]")[[1]]
    # Arbitrarily assign names for the outputs
    name_segments %>% purrr::set_names(LETTERS[1:length(name_segments)])
  }
)

## -----------------------------------------------------------------------------
TidyMultiqc::load_multiqc(
  multiqc_data_path, 
  find_metadata = function(sample, parsed) {
    parsed[c(
      "config_creation_date",
      "config_version"
    )]
  }
)

## ----message=FALSE, warning=FALSE---------------------------------------------
df_both %>% dplyr::select(dplyr::contains('quality'))

## ---- eval = FALSE------------------------------------------------------------
#  TidyMultiqc::list_plots(multiqc_data_path)

## ---- echo = FALSE------------------------------------------------------------
TidyMultiqc::list_plots(multiqc_data_path) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), ~stringr::str_trunc(., 50)))

## -----------------------------------------------------------------------------
df = TidyMultiqc::load_multiqc(
  multiqc_data_path, 
  sections = 'plot',
  plots = "fastqc_per_sequence_quality_scores_plot"
)
df

## -----------------------------------------------------------------------------
df$plot.fastqc_per_sequence_quality_scores_plot[[1]]

## -----------------------------------------------------------------------------
df %>%
  tidyr::unnest(cols = plot.fastqc_per_sequence_quality_scores_plot)

## -----------------------------------------------------------------------------
df %>%
  tidyr::unnest(cols = plot.fastqc_per_sequence_quality_scores_plot) %>%
  dplyr::group_by(metadata.sample_id) %>%
  dplyr::summarise(total_reads = sum(y))

## -----------------------------------------------------------------------------
df %>%
  dplyr::mutate(
    total_reads = purrr::map_dbl(plot.fastqc_per_sequence_quality_scores_plot, ~sum(.$y)),
    plot.fastqc_per_sequence_quality_scores_plot = NULL
  )

## -----------------------------------------------------------------------------
df %>%
  tidyr::unnest(cols = plot.fastqc_per_sequence_quality_scores_plot) %>%
  dplyr::group_by(metadata.sample_id) %>%
  dplyr::mutate(hist = list(HistDat::HistDat(vals = x, counts = y)), .keep = "unused") %>%
  dplyr::mutate(
    mean_coverage = hist %>% dplyr::first() %>% mean(),
    median_coverage = hist %>% dplyr::first() %>% median(),
    max_coverage = hist %>% dplyr::first() %>% max(),
    hist= NULL
  ) %>%
  dplyr::slice(1)

## -----------------------------------------------------------------------------
df %>%
  dplyr::mutate(
    purrr::map_dfr(plot.fastqc_per_sequence_quality_scores_plot, function(plot_df){
      hist = HistDat::HistDat(vals=plot_df$x, counts = plot_df$y)
      list(
        mean_coverage = mean(hist),
        median_coverage = median(hist),
        max_coverage = max(hist)
      )
    }),
    plot.fastqc_per_sequence_quality_scores_plot = NULL
  )


## -----------------------------------------------------------------------------
TidyMultiqc::load_multiqc(
  multiqc_data_path, 
  sections = 'plot',
  plots = "fastqc_per_sequence_quality_scores_plot",
  plot_parsers = list(
    # This fake parser function takes a plot and just returns the iris dataset
    xy_line = function(plot_data, name){
      list(
        sample_1 = list(
          plot_name = list(iris)
        )
      )
    }
  )
)

