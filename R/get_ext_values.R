#' @title Get the extinction coefficients of HbO and HbR at the two wavelengths.
#' @description This function returns the extinction coefficients of HbO and HbR at the two wavelengths. The extinction coefficients are taken from the literature.
#' @param lambda1 The first wavelength.
#' @param lambda2 The second wavelength.
#' @param table Which table of extinction coefficients to use. Options are "wray", "takatani", "moaveni", "gratzer" and "cope".
#'
#' @return a matrix with the extinction coefficients of HbO and HbR at the two wavelengths, which can be used with the `applyMBLL` function.
#' @export

get_ext_values<- function(lambda1 = 730, lambda2 = 850, table = "wray"){

library(dplyr)

if(table == "wray"){
  data <- as.data.frame(cobifnirs:::wray)
} else if(table == "takatani"){
  data <- cobifnirs:::takatani
} else if(table == "moaveni"){
  data <- cobifnirs:::moaveni
} else if(table == "gratzer"){
  data <- cobifnirs:::gratzer
} else if(table == "cope"){
  data <- cobifnirs:::cope
} else {
  stop("table must be one of 'wray', 'takatani', 'moaveni', 'gratzer' or 'cope'")
}

 print(paste0("calculating extinction coefficients based on ", lambda1, "nm and ", lambda2, "nm"))
 print(paste0("using the ", table, " table"))

  model_hbo <- lm(hbo ~ lambda, data = data)
  model_hbr <- lm(hbr ~ lambda, data = data)


  e_hbo_l1 <- approx(data$lambda, data$hbo, xout = lambda1)
  e_hbr_l1 <-  approx(data$lambda, data$hbr, xout = lambda1)
  e_hbo_l2 <- approx(data$lambda, data$hbo, xout = lambda2)
  e_hbr_l2 <- approx(data$lambda, data$hbr, xout = lambda2)

  print(paste0("HbO at ", lambda1, "nm: ", e_hbo_l1$y))
  print(paste0("HbR at ", lambda1, "nm: ", e_hbr_l1$y))
  print(paste0("HbO at ", lambda2, "nm: ", e_hbo_l2$y))
  print(paste0("HbR at ", lambda2, "nm: ", e_hbr_l2$y))

  print("extinction coefficients calculated. Returning extinction matrix")
 return(matrix(c(e_hbo_l1$y,e_hbr_l1$y,e_hbo_l2$y, e_hbr_l2$y), nrow = 2, byrow = T) )
}
