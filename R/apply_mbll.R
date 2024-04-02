#' @title Apply the Modified Beer Lambert Law to the change in optical density data
#' @description Applies the Modified Beer Lambert Law to the change in optical density data to calculate the change in concentration of oxygenated and deoxygenated hemoglobin.
#' @param deltaOdData (DATAFRAME) NIRS data containing the delta od values for each channel. This should be in the format of the output from the function \code{\link{create_delta_ods}}.
#' @param extMatrix (MATRIX) The extinction coefficients for each wavelength. This should be in the format of the output from the function \code{\link{create_ext_matrix}}.
#' @param pathLength (NUMERIC) a value specifying the pathlength of the light through the tissue. This should be in cm. The default value is 2.5.
#' @param dpf (NUMERIC) a value specifying the differential path length factor. The default value is 6.
#' @return a data frame which adds the HbO and HbR values for each channel to new columns.
#'
#' @export
#' @examples
#' \dontrun{
#'
#' # Import the NIRS data
#'
#' nirsData <- import_nirs("path/to/nirs/data.nir")
#' # Create the delta ODs
#' nirsData <- create_delta_ods(nirsData, reference = "baseline")
#'
#' # Create the extinction matrix
#'
#' extMatrix <- create_ext_matrix(lambda1 = 730, lambda2 = 850, table= "wray")
#'
#' # Apply the Modified Beer Lambert Law
#'
#' nirsData <- apply_mbll(data, extMatrix)
#' }
#' @seealso \code{\link{create_delta_ods}}, \code{\link{create_ext_matrix}}, \code{\link{import_nirs}}



apply_mbll <- function(deltaOdData, extMatrix, pathLength = 2.5, dpf = 6){


  # Equivalent to:
  # [[Dc_HbO]  = [[e_HbO_1, e_HbR_1] -1 . [[Dod_1/(l*DPF_1)]
  # [DC_HbR]]    [e_HbO_2, e_HbR_2]]      [Dod_2/(l*DPF_2)]]


  # molar coefficient for 730:	Hb02 390	Hbr 1102.2
  # molar coefficient for 850:	Hb02 1058	Hbr 691.32

  #  e_HbO_730 <- 390
  # e_HbR_730 <- 1102.2
  # e_HbO_850 <- 1058
  #  e_HbR_850 <- 691.32

  # from wray tables

  l <- pathLength * 0.1
  e_matrix <- extMatrix


  e_matrix_inv <- solve(e_matrix)

  delta_od_matrix <- deltaOds %>%
    group_by(optode, t) %>%
    summarise(hbo_hbr = e_matrix_inv %*% matrix(c(delta_od_850/(l*dpf), delta_od_730/(l*dpf))),t,data_row, delta_od_850, delta_od_730, startTime,fileName  ) %>%
    mutate(hbo = hbo_hbr[1,], hbr = hbo_hbr[2,]) %>%
    ungroup()

  # filter our the original hbo_hbr matrix
  delta_od_matrix <- delta_od_matrix %>% dplyr::select(-hbo_hbr) %>% distinct()


  oxyData <- delta_od_matrix



 return( oxyData )


}

