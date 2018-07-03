COMID <- COMID.y <- Divergence <- DnHydroseq <- DnMinorHyd <- FTYPE <- FromNode <-
  Hydroseq <- ID <- LENGTHKM <- LevelPathI <- Pathlength <- StartFlag <- StreamCalc <-
  StreamOrde <- TerminalFl <- TerminalPa <- ToNode <- TotDASqKM <- becomes <- dsLENGTHKM <-
  ds_joined_fromCOMID <- ds_num_upstream <- fID <- fromCOMID <- fromLENGTHKM <-
  fromTotDASqKM <- geom_len <- geometry <- join_category <- joined_fromCOMID <-
  joined_fromCOMID_new <- joined_toCOMID <- member_COMID <- new_joined_fromCOMID <-
  new_joined_toCOMID <- new_toCOMID <- num_upstream <- part <- piece <- pieces <-
  removed_COMID <- split_fID <- toCOMID <- toID <- usLENGTHKM <- usTotDASqKM <-
  . <- L1 <- X <- Y <- breaks <- dist_ratio <- ideal_len <- len <- nID <- new_index <-
  piece_len <- setNames <- start <- FromMeas <- REACHCODE <- REACH_meas <- ToMeas <-
  index <- measure <- nn.idx <- precision_index <- NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(strwrap(
    "USGS Research Package:
    https://owi.usgs.gov/R/packages.html#research"),
    collapse = "\n"))
}

get_dsLENGTHKM <- function(flines) {
  # This gets all the next-downstream flowlines and finds the length of the next downstream
  flines$dsLENGTHKM <- flines[["LENGTHKM"]][match(flines$toCOMID, flines$COMID)]
  # already removed comids get NA dsLength -- ok to set them to 0.
  flines[["dsLENGTHKM"]][is.na(flines$dsLENGTHKM)] <- 0
  flines[["dsLENGTHKM"]]
}

get_upstream <- function(flines) {
  left_join(select(flines, COMID), select(flines, COMID, toCOMID),
            by = c("COMID" = "toCOMID")) %>%
    rename(fromCOMID = COMID.y)
}

get_upstream_length <- function(flines) {
  left_join(select(flines, COMID), select(flines, toCOMID, LENGTHKM),
            by = c("COMID" = "toCOMID")) %>%
    rename(usLENGTHKM = LENGTHKM)
}

get_num_upstream <- function(flines) {
  left_join(select(flines, COMID, toCOMID),
            get_upstream(flines) %>%
              group_by(COMID) %>%
              summarise(num_upstream = n()),
            by = "COMID")[["num_upstream"]]
}

get_ds_num_upstream <- function(flines) {
  flines <- mutate(flines, num_upstream = get_num_upstream(flines))
  flines[["num_upstream"]][match(flines$toCOMID, flines$COMID)]
}

get_ds_joined_fromCOMID <- function(flines) {
  flines <- mutate(flines, ds_joined_fromCOMID = joined_fromCOMID)
  flines[["ds_joined_fromCOMID"]][match(flines$toCOMID, flines$COMID)]
}
