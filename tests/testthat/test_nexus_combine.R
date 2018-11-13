context("nexus combine")
test_that("walker combine runs", {
source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

outlets <- data.frame(ID = c(31, 3, 5, 1),
                      type = c("outlet", "outlet", "outlet", "terminal"),
                      stringsAsFactors = FALSE)

collapsed <- collapse_catchments(walker_fline_rec, walker_catchment_rec, outlets)

expect_equal(collapsed$ID, c(5, 31, 3, 1))
expect(collapsed$ID[1] %in% collapsed$set[[1]], "outlet ids should be in the result")
expect(length(collapsed$set[[2]]) == 5, "got the wrong number in catchment set")
expect(!5 %in% collapsed$set[[2]], "an upstream outlet should not be in another set")

outlets <- data.frame(ID = c(31, 3, 5, 1, 12),
                      type = c("outlet", "outlet", "outlet", "terminal", "inlet"),
                      stringsAsFactors = FALSE)

collapsed <- collapse_catchments(walker_fline_rec, walker_catchment_rec, outlets)

expect_equal(collapsed$ID, c(12, 5, 31, 3, 1))
expect(length(collapsed$set[[1]]) == 7, "got the wrong number in catchment set")
expect(!(any(collapsed$set[[1]] == 12)), "shouldn't have inlet catchment in set")

outlets <- data.frame(ID = c(14, 1),
                      type = c("outlet", "terminal"),
                      stringsAsFactors = FALSE)

collapsed <- collapse_catchments(walker_fline_rec, walker_catchment_rec, outlets)

expect(!any(collapsed$set[[1]] == 4), "shouldn't have a parallel stem in the set")

outlets <- data.frame(ID = c(2, 1),
                      type = c("inlet", "terminal"),
                      stringsAsFactors = FALSE)

collapsed <- collapse_catchments(walker_fline_rec, walker_catchment_rec, outlets)

expect(length(collapsed$set[[1]]) == 100, "got the wrong number in catchment set")
})

lp_ids <- unique(walker_flowline$LevelPathI)

member_COMID_mapper <- function(df) {
df$member_COMID <- lapply(df$member_COMID, function(x)
  strsplit(x, ",")[[1]])

mapper <- tidyr::unnest(select(st_set_geometry(df, NULL),
                               ID, COMID = member_COMID))
}

walker_lp_mapper <- member_COMID_mapper(walker_fline_rec) %>%
  mutate(orig_COMID = as.integer(floor(as.numeric(COMID)))) %>%
  left_join(select(st_set_geometry(walker_flowline, NULL),
                   COMID, LevelPathI, Hydroseq),
            by = c("orig_COMID" = "COMID")) %>%
  group_by(LevelPathI)

walker_headwaters <- filter(walker_lp_mapper, Hydroseq == max(Hydroseq) &
                              (COMID == as.character(orig_COMID) |
                                 COMID == as.character(orig_COMID + 0.1)))

walker_outlets <- filter(walker_lp_mapper, Hydroseq == min(Hydroseq)) %>%
  group_by(orig_COMID) %>%
  filter(as.numeric(COMID) == max(as.numeric(COMID))) %>%
  ungroup()

walker_lps <- data.frame(LevelPathID = unique(walker_outlets$LevelPathI)) %>%
  left_join(select(walker_headwaters, head_ID = ID, LevelPathID = LevelPathI),
            by = "LevelPathID") %>%
  left_join(select(walker_outlets, tail_ID = ID, LevelPathID = LevelPathI),
            by = "LevelPathID")



