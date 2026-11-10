# per-dataset modifiers for include = "modify" datasets.
# each modifier: function(df, features) -> list(df = ..., features = ...)
# use dplyr::bind_rows() when adding feature rows (fills missing columns with NA)
# to activate: code modifier, verify variable names, flip TSV include to "yes"/"modify"
# keys are zero-padded 4-digit dataset IDs

library(dplyr)
library(readr)
library(here)

coding_sheet_path <- here::here("data", "meta", "datasets.tsv")

dataset_modifiers <- list()

# unanswered rows sharing a beep slot are dropped; of answered rows sharing a slot, the later
# one in row order is kept
resolve_shared_slots <- function(df, features) {
  items <- features$name[features$answer_categories != ""]
  answered <- rowSums(!is.na(df[items])) > 0
  slot <- paste(df$id, df$day, df$beep)
  shared <- slot %in% slot[duplicated(slot)]
  df <- df[!(shared & !answered), ]
  slot <- paste(df$id, df$day, df$beep)
  df[!duplicated(slot, fromLast = TRUE), ]
}

# canceled and expired prompts share beep slots with answered prompts; slots left unanswered
# return as NA rows through schedule completion. two slots hold double submissions
dataset_modifiers[["0001"]] <- function(df, features) {
  answered <- !(df$duration %in% c("Canceled", "Expired"))
  list(df = resolve_shared_slots(df[answered, ], features), features = features)
}

# day restarts at 1 in study_period 1, which continues the day count of study_period 0.
# each day ends with a row without a beep number
dataset_modifiers[["0010"]] <- function(df, features) {
  df$day <- df$day + df$study_period * max(df$day)
  list(df = df[!is.na(df$beep), ], features = features)
}

# items are person-mean centred in openESM, so only the width of the original scale is known.
# RMSE depends only on that width, so the bounds span the original width around 0 and a
# person at their own mean maps to 0.5. centred values can exceed these bounds, and positions
# on the scale (floor/ceiling) are not meaningful for these items
dataset_modifiers[["0002"]] <- function(df, features) {
  centred <- c("sociable", "creative", "friendly", "organised", "self_esteem")
  width <- as.integer(features$answer_categories[match(centred, features$name)]) - 1
  bounds <- data.frame(name = centred, scale_min = -width / 2, scale_max = width / 2,
                       allow_outside_bounds = TRUE, stringsAsFactors = FALSE)
  list(df = df, features = dplyr::left_join(features, bounds, by = "name"))
}

dataset_modifiers[["0028"]] <- function(df, features) {
  # beep 11 exceeds the 10-beep design; it only occurs after beep 10
  df <- resolve_shared_slots(df[df$beep <= 10, ], features)

  composites <- list(
    paranoia   = list(cols = c("no_trust", "harm", "criticism"), ref = "no_trust"),
    self_esteem = list(cols = c("useless", "manage_well"), ref = "useless")
    # 'useless' is stored reverse-coded in openESM (higher = more useful),
    # so no additional reverse-coding is needed before averaging
  )

  for (nm in names(composites)) {
    df[[nm]] <- rowMeans(df[composites[[nm]]$cols], na.rm = TRUE)
  }

  new_rows <- do.call(rbind, lapply(names(composites), function(nm) {
    ref <- composites[[nm]]$ref
    data.frame(name = nm,
               answer_categories = features$answer_categories[features$name == ref],
               stringsAsFactors = FALSE)
  }))

  list(df = df, features = dplyr::bind_rows(features, new_rows))
}

# one person-day has 12 answered prompts numbered up to 18 in a 10-beep design, so its beep
# numbers cannot be placed on the schedule; the whole person-day is removed
dataset_modifiers[["0029"]] <- function(df, features) {
  person_day <- paste(df$id, df$day)
  off_schedule <- person_day %in% person_day[df$beep > 10]
  list(df = df[!off_schedule, ], features = features)
}

dataset_modifiers[["0034"]] <- function(df, features) {
  composites <- list(
    blame        = list(cols = c("self_blame",          "others_blame"),         ref = "self_blame"),
    neg_thoughts = list(cols = c("neg_thoughts_others", "neg_thoughts_world"),   ref = "neg_thoughts_world")
  )

  for (nm in names(composites)) {
    df[[nm]] <- rowMeans(df[composites[[nm]]$cols], na.rm = TRUE)
  }

  new_rows <- do.call(rbind, lapply(names(composites), function(nm) {
    ref <- composites[[nm]]$ref
    data.frame(name = nm,
               answer_categories = features$answer_categories[features$name == ref],
               stringsAsFactors = FALSE)
  }))

  list(df = df, features = dplyr::bind_rows(features, new_rows))
}

dataset_modifiers[["0036"]] <- function(df, features) {
  composites <- list(
    dampening = list(
      cols = c("bragging_thought", "too_good_to_be_true", "ruminate_negatives",
               "feelings_transient", "hard_to_concentrate", "think_of_risks",
               "undeserving", "luck_will_end"),
      ref  = "bragging_thought"
    ),
    pa = list(
      cols = c("happy", "excited", "content"),
      ref  = "happy"
    )
  )

  for (nm in names(composites)) {
    df[[nm]] <- rowMeans(df[composites[[nm]]$cols], na.rm = TRUE)
  }

  new_rows <- do.call(rbind, lapply(names(composites), function(nm) {
    ref <- composites[[nm]]$ref
    data.frame(name = nm,
               answer_categories = features$answer_categories[features$name == ref],
               stringsAsFactors = FALSE)
  }))

  list(df = df, features = dplyr::bind_rows(features, new_rows))
}

# variables are pre-aggregated in openESM, and answer_categories describes the original items.
# bounds follow from the codebook: https://openesmdata.org/datasets/0041_wright/
dataset_modifiers[["0041"]] <- function(df, features) {
  # circumplex scores weight octant ratings (1-8) by cos/sin of 45-degree steps. the weights
  # sum to zero, so controlling for overall endorsement leaves the extremes at +-7 * (1 + sqrt(2)).
  # the data are rounded to 2 decimals (extremes stored as +-16.9), so the bound is rounded up to match
  circ_max <- ceiling(7 * (1 + sqrt(2)) * 100) / 100
  bounds <- data.frame(
    name = c("dominance", "affiliation", "stressed"),
    # stressed sums 7 events, each scored with four response labels
    scale_min = c(-circ_max, -circ_max, 0),
    scale_max = c(circ_max, circ_max, 21),
    stringsAsFactors = FALSE
  )
  list(df = df, features = dplyr::left_join(features, bounds, by = "name"))
}

dataset_modifiers[["0061"]] <- function(df, features) {
  composites <- list(
    responsiveness = list(
      cols = c("cared_for", "respected", "supported"),
      ref  = "cared_for"
    )
  )

  for (nm in names(composites)) {
    df[[nm]] <- rowMeans(df[composites[[nm]]$cols], na.rm = TRUE)
  }

  new_rows <- do.call(rbind, lapply(names(composites), function(nm) {
    ref <- composites[[nm]]$ref
    data.frame(name = nm,
               answer_categories = features$answer_categories[features$name == ref],
               stringsAsFactors = FALSE)
  }))

  list(df = df, features = dplyr::bind_rows(features, new_rows))
}

dataset_modifiers[["0063"]] <- function(df, features) {
  list(df = resolve_shared_slots(df, features), features = features)
}

dataset_modifiers[["0072"]] <- function(df, features) {
  composites <- list(
    autonomy_support = list(
      cols = c("parenting_child_decide", "parenting_child_liked"),
      ref  = "parenting_child_decide"
    ),
    need_satisfaction = list(
      cols = c("contact_with_people", "connected", "intimacy",
               "own_way", "true_self", "did_interesting",
               "completed_difficult_project", "mastered_challenges", "even_hard_things"),
      ref  = "contact_with_people"
    ),
    need_frustration = list(
      cols = c("excluded_ostracized", "unappreciated", "disagreements_conflicts",
               "pressure", "told_what_to_do", "against_own_will",
               "failure", "did_stupid", "struggled"),
      ref  = "excluded_ostracized"
    ),
    child_pa = list(
      cols = c("child_happy", "child_balanced", "child_cheerful", "child_relaxed"),
      ref  = "child_happy"
    ),
    child_na = list(
      cols = c("child_afraid", "child_sad", "child_worried", "child_angry"),
      ref  = "child_afraid"
    )
  )

  for (nm in names(composites)) {
    df[[nm]] <- rowMeans(df[composites[[nm]]$cols], na.rm = TRUE)
  }

  new_rows <- do.call(rbind, lapply(names(composites), function(nm) {
    ref <- composites[[nm]]$ref
    data.frame(name = nm,
               answer_categories = features$answer_categories[features$name == ref],
               stringsAsFactors = FALSE)
  }))

  list(df = df, features = dplyr::bind_rows(features, new_rows))
}