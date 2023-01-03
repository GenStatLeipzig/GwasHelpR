#' Create a quantile-quantile plot of p-values with ggplot2.
#'
#' Assumptions:
#'   - Expected P values are uniformly distributed.
#'   - Confidence intervals assume independence between tests.
#'     We expect deviations past the confidence intervals if the tests are
#'     not independent.
#'     For example, in a genome-wide association study, the genotype at any
#'     position is correlated to nearby positions. Tests of nearby genotypes
#'     will result in similar test statistics.
#'
#' @param pvalues Vector of p-values.
#' @param ci Size of the confidence interval, 95% by default.
#' @param maf Vector of maf-values, NULL by default.
#' @param info Vector of info-values, NULL by default.
#' @param mafcutoff cutoff for coloring low maf snps, if maf is not NULL, 0.05 by default.
#' @param infocutoff cutoff for different shape of low info snps, if info is not NULL, 0.8 by default.
#' @return A ggplot2 plot.
#' @examples
#' library(ggplot2)
#' gg_qqplot(runif(1e2)) 
#' @author adapted from https://slowkow.com/notes/ggplot2-qqplot/ by Holger
#' @rdname gg_qqplot
#' @export

#' @importFrom data.table data.table
#' @import ggplot2
#' @import scales

gg_qqplot <- function(pvalues, ci = 0.95, maf=NULL, info=NULL, mafcutoff=0.05, infocutoff=0.8) {
  
  df = data.table::data.table(pvalues, maf, info)
  data.table::setorder(df, pvalues)
  n  <- nrow(df)
  df[, observed := -log10(pvalues)]
  df[, expected := -log10(ppoints(n))]
  df[, clower   := -log10(qbeta(p = (1 - ci) / 2, shape1 = 1:n, shape2 = n:1))]
  df[, cupper   := -log10(qbeta(p = (1 + ci) / 2, shape1 = 1:n, shape2 = n:1))]
  
  log10Pe <- expression(paste("Expected -log"[10], plain(P)))
  log10Po <- expression(paste("Observed -log"[10], plain(P)))
  
  p1pre = ggplot2::ggplot(df) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(x = expected, ymin = clower, ymax = cupper),
      alpha = 0.3
    )
  
  if(is.null(maf) ==F & is.null(info)==F) p2pre = p1pre + ggplot2::geom_point(ggplot2::aes(expected, observed, col = maf<0.05, pch=info<0.8),size = 1)
  
  if(is.null(maf) ==F & is.null(info)==T) p2pre = p1pre + ggplot2::geom_point(ggplot2::aes(expected, observed, col = maf<0.05),size = 1)
  
  if(is.null(maf) ==T & is.null(info)==F) p2pre = p1pre + ggplot2::geom_point(ggplot2::aes(expected, observed , pch=info<0.8),size = 1)
  
  if(is.null(maf) ==T & is.null(info)==T) p2pre = p1pre + ggplot2::geom_point(ggplot2::aes(expected, observed),size = 1)
  
  p3 = p2pre +
    ggplot2::geom_abline(intercept = 0, slope = 1, alpha = 0.5) +

    ggplot2::xlab(log10Pe) +
    ggplot2::ylab(log10Po)+
    ggplot2::theme_classic(base_size = 14) +
    ggplot2::theme(
      axis.ticks = ggplot2::element_line(size = 0.5),
      panel.grid = ggplot2::element_blank(),
      legend.position = c(0.2, 0.7)
      # panel.grid = element_line(size = 0.5, color = "grey80")
    )+
    ggplot2::scale_color_manual(values = c("black", "red"))+
    ggplot2::scale_shape_manual(values = c(1,2)) + 
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(8)) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(6))
  
  
  
  p3 
  
  
  
}
