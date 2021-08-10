
set_theme_ggplot <- function(ggplot_obj,
                             axis_text_size = 10,
                             axis_title_size = 17,
                             strip_text_x_size = 17,
                             legend_text_size = 12,
                             legend_title_size = 12,
                             legend_position = "right") {
  out <- ggplot_obj +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = axis_text_size),
      axis.title = ggplot2::element_text(size = axis_title_size),
      strip.text.x = ggplot2::element_text(size = strip_text_x_size),
      legend.text = ggplot2::element_text(size = legend_text_size),
      legend.title = ggplot2::element_text(size = legend_title_size),
      legend.position = legend_position
    )

  return(out)
}

fix_lines_metrics <- function(x) {
  if (x == "Fraction of predicted positives") x <- "Fraction of predicted\n positives"
  if (x == "False positive rate (FPR)") x <- "False positive rate\n (FPR)"
  if (x == "False negative rate (FNR)") x <- "False negative rate\n (FNR)"
  if (x == "Positive predicted values (PPV)") x <- "Positive predicted values\n (PPV)"
  if (x == "Area under the curve (AUC)") x <- "Area under the curve\n (AUC)"
  x
}
