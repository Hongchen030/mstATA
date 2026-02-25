#' @keywords internal
#' @noRd

expected_nrCat <- function(model, params) {

  model <- toupper(model)

  params_vec <- unlist(params, use.names = FALSE)
  if (!is.numeric(params_vec)) {
    stop("Item parameters must be numeric.", call. = FALSE)
  }

  npar <- sum(!is.na(params_vec))

  out<-switch(model,
              # Dichotomous models (after fill_dichotomous_defaults)
              "1PL" = 2L,
              "RASCH" = 2L,
              "2PL" = 2L,
              "3PL" = 2L,
              "4PL" = 2L,

              ## ---- GRM: a + (K-1) thresholds ----
              "GRM" = {
                if (npar < 2L) {
                  stop("GRM requires at least 2 parameters (a + thresholds).",
                       call. = FALSE)
                }
                (npar - 1L) + 1L
              },

              ## ---- MGRM: (aj, bj) + (K-1) thresholds ----
              "MGRM" = {
                if (npar < 3L) {
                  stop("MGRM requires at least 3 parameters.", call. = FALSE)
                }
                (npar - 2L) + 1L
              },

              ## ---- PCM: K-1 step parameters ----
              "PCM" = {
                if (npar < 1L) {
                  stop("PCM requires at least 1 step parameter.", call. = FALSE)
                }
                npar + 1L
              },

              ## ---- GPCM: a + (K-1) step parameters ----
              "GPCM" = {
                if (npar < 2L) {
                  stop("GPCM requires at least 2 parameters (a + steps).",
                       call. = FALSE)
                }
                (npar - 1L) + 1L
              },

              ## ---- RSM: b + (K-1) common step parameters ----
              "RSM" = {
                if (npar < 2L) {
                  stop("RSM requires at least 2 parameters (b + steps).",
                       call. = FALSE)
                }
                (npar - 1L) + 1L
              },


              ## ---- NRM: (a1,b1)...(aK,bK) ----
              "NRM" = {
                if (npar %% 2L != 0L || npar < 2L) {
                  stop("NRM requires an even number of parameters (a,b pairs).",
                       call. = FALSE)
                }
                (npar %/% 2L) + 1L
              },

              stop("Unsupported model: ", model, call. = FALSE)
  )
  as.integer(out)
}
