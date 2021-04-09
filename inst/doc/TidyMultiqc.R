## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
multiqc_data_path = system.file("extdata", "wgs/multiqc_data.json", package = "TidyMultiqc")

## ----setup--------------------------------------------------------------------
library(TidyMultiqc)

## ----paged.print=TRUE---------------------------------------------------------
df = load_multiqc(multiqc_data_path)
df

## -----------------------------------------------------------------------------
load_multiqc(multiqc_data_path, sections = 'raw')

## -----------------------------------------------------------------------------
df_both = load_multiqc(multiqc_data_path, sections = c('raw', 'general'))
ncol(df_both)

## -----------------------------------------------------------------------------
library(ggplot2)

ggplot(df, aes(x=metadata.sample_id, y=general.percent_duplication)) + geom_col()

## -----------------------------------------------------------------------------
t.test(df$general.percent_gc, mu=41)

## -----------------------------------------------------------------------------
library(dplyr)

df_both %>% select(contains('quality'))

## -----------------------------------------------------------------------------
load_multiqc(
  multiqc_data_path, 
  sections = 'plots',
  plot_opts = list(
    `fastqc_per_sequence_quality_scores_plot` = list(
      extractor = extract_histogram,
      summary = list(median=median),
      prefix = "quality"
    )
  )
)

