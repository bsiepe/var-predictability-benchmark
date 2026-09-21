library(here)
source(here::here("scripts", "00_read_modify_coding_sheet.R"))

# --- helpers ----------------------------------------------------------------

make_df <- function(cols, vals) {
  setNames(as.data.frame(matrix(vals, nrow = length(vals) / length(cols),
                                 byrow = TRUE)),
           cols)
}

check_modifier <- function(id, raw_cols, ref_features, expected_vars) {
  mod <- dataset_modifiers[[id]]
  stopifnot(!is.null(mod))
  cat(id, "modifier: defined: PASS\n")

  n <- length(raw_cols)
  df <- make_df(raw_cols, c(rep(1, n), rep(5, n)))

  result <- mod(df, ref_features)
  stopifnot(is.list(result), all(c("df", "features") %in% names(result)))
  cat(id, "modifier: returns list(df, features): PASS\n")

  stopifnot(all(expected_vars %in% names(result$df)))
  cat(id, "modifier: composite columns created: PASS\n")

  for (v in expected_vars) {
    stopifnot(result$df[[v]][1] == 1)
    stopifnot(result$df[[v]][2] == 5)
  }
  cat(id, "modifier: composite values correct for uniform rows: PASS\n")

  stopifnot(all(expected_vars %in% result$features$name))
  cat(id, "modifier: composite variables added to features: PASS\n")

  stopifnot(all(raw_cols %in% names(result$df)))
  cat(id, "modifier: input columns preserved: PASS\n")

  invisible(result)
}

# --- 0028 (Contreras) -------------------------------------------------------
# paranoia = mean(no_trust, harm, criticism); self_esteem = mean(useless, manage_well)
# 'useless' stored reverse-coded in openESM (higher = more useful); no extra reverse needed

raw_0028 <- c("no_trust", "harm", "criticism", "useless", "manage_well",
              "sad", "others", "avoid")
features_0028 <- data.frame(name = c("no_trust", "useless"),
                             scale_min = 1, scale_max = 9,
                             stringsAsFactors = FALSE)
check_modifier("0028", raw_0028, features_0028,
               expected_vars = c("paranoia", "self_esteem"))

# --- 0036 (Bosley) ----------------------------------------------------------
# dampening = mean(8 items); positive_affect = mean(happy, excited, content)
# worry_frequent is direct (in variables_original, not a composite)

raw_0036 <- c("worry_frequent",
              "bragging_thought", "too_good_to_be_true", "ruminate_negatives",
              "feelings_transient", "hard_to_concentrate", "think_of_risks",
              "undeserving", "luck_will_end",
              "happy", "excited", "content")
features_0036 <- data.frame(name = c("worry_frequent", "bragging_thought", "happy"),
                             scale_min = 1, scale_max = 7,
                             stringsAsFactors = FALSE)
check_modifier("0036", raw_0036, features_0036,
               expected_vars = c("dampening", "positive_affect"))

# --- 0061 (Merolla) ---------------------------------------------------------
# responsiveness = mean(cared_for, respected, supported)
# connected is direct (in variables_original, not a composite)

raw_0061 <- c("cared_for", "respected", "supported", "connected")
features_0061 <- data.frame(name = c("cared_for", "connected"),
                             scale_min = 1, scale_max = 7,
                             stringsAsFactors = FALSE)
check_modifier("0061", raw_0061, features_0061,
               expected_vars = "responsiveness")

# --- 0072 (Neubauer) --------------------------------------------------------
# autonomy_support, need_sat_d, need_dis_d, child_pa, child_na

raw_0072 <- c(
  "parenting_child_decide", "parenting_child_liked",
  "contact_with_people", "connected", "intimacy",
  "own_way", "true_self", "did_interesting",
  "completed_difficult_project", "mastered_challenges", "even_hard_things",
  "excluded_ostracized", "unappreciated", "disagreements_conflicts",
  "pressure", "told_what_to_do", "against_own_will",
  "failure", "did_stupid", "struggled",
  "child_happy", "child_balanced", "child_cheerful", "child_relaxed",
  "child_afraid", "child_sad", "child_worried", "child_angry"
)
ref_cols_0072 <- c("parenting_child_decide", "contact_with_people",
                   "excluded_ostracized", "child_happy", "child_afraid")
features_0072 <- data.frame(name = ref_cols_0072,
                             scale_min = 1, scale_max = 7,
                             stringsAsFactors = FALSE)
check_modifier("0072", raw_0072, features_0072,
               expected_vars = c("autonomy_support", "need_sat_d", "need_dis_d",
                                 "child_pa", "child_na"))

cat("all modifier tests passed\n")
