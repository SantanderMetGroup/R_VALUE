#' @title Interannual trend
#' @description Computes slope and significance of the interannual linear trend
#' @author J Bedia
#' @template templateIndexParams
#' @template templateDates
#' @param output Desired output. The options are \code{"coef"} for the slope
#' coefficient (default) or \code{"sig"}, which is a binary value indicating whether 
#' the slope coefficient is significant (1) or not (0) at the given \code{sig.level}.
#' @param sig.level Significance level. Default to 0.95. 
#' @return A numeric value with either the slope coefficient (for \code{output = "coef"})
#' or whether it is significant or not (\code{output = "sig"}) at the specified \code{sig.level}
#' @export
#' @importFrom stats lm coef confint.lm

index.trend <- function(ts, dates, output = c("coef","sig"), sig.level = 0.95) {
      output <- match.arg(output, choices = c("coef","sig"))
      INDEX <- getYearsAsINDEX.VALUE(dates)
      y <- tapply(ts, INDEX = INDEX, FUN = mean, na.rm = TRUE)
      mod <- lm(y ~ unique(INDEX))
      if (output == "coef") {
            unname(coef(mod)[2])
      } else {
            ifelse(sign(prod(confint.lm(mod, parm = 2, level = sig.level))) == 1, 1, 0)
      }
}




