library(readxl)
library(tidyr)
library(dplyr)
library(stringr)

filename <- "Data/Suffolk wide Neighbourhood Plan Progress.xlsx"

nps_data <- readxl::read_excel(filename, sheet = "Progress")

nps_data <- janitor::clean_names(nps_data)

get_nps_progress <- function(nps) {
nps <- nps |> 
  dplyr::select(
    qualifing_body,
    designating_the_neighbourhood_area_reg_7_and_if_appropriate_the_neighbourhood_forum_reg_10,
    preparing_a_draft_neighbourhood_plan,
    pre_submission_publicity_and_consultation_reg_14,
    submission_of_a_neighbourhood_plan_to_the_local_planning_authority_reg_15_and_publication_of_plan_proposal_by_the_lpa_reg_16,
    independent_examination_reg_17_and_18,
    referendum_reg_19_and_20
  )

nps_all <- pivot_longer(
  nps,
  cols = c(2:7),
  names_to = "status",
  values_to = "value"
)

nps_long <- nps_all |> 
  dplyr::filter(!is.na(value))

# Remove no votes
nps_long <- nps_long |> 
  filter(
    !str_detect(value, "NO VOTE")
    )

checklist <- lapply(nps_long$qualifing_body, function(x) {
  
  parish <- nps_long |> 
    filter(qualifing_body == x)
  
  if ("referendum_reg_19_and_20" %in% parish$status) {
    parish$progress <- "adopted"
  } else {
    parish$progress <- "in progress"
  }
  
})

nps_long$progress <- unlist(checklist)

nps_progress <- nps_long |> 
  select(qualifing_body, progress) |> 
  unique()

nps_not_started <- nps_all |> 
  filter(!qualifing_body %in% nps_progress$qualifing_body) |> 
  select(qualifing_body) |> 
  unique()

return(nps_progress)
} 

