#' @keywords internal
#' @noRd


validate_enemy_set_index <- function(enemy_set,itempool) {
  pool_size <- nrow(itempool)
  if (!"ItemIndex" %in% names(enemy_set)) {
    stop("`enemy_set` must contain `ItemIndex` for index validation.",
         call. = FALSE)
  }
  confirm_index(
    enemy_set$ItemIndex,
    pool_size,
    what = "enemy indices",
    allow_list = TRUE
  )

  invisible(TRUE)
}
