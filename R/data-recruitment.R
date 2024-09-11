# Copyright 2022 Environment and Climate Change Canada
# Copyright 2023 Province of Alberta
# Copyright 2024 Province of Alberta
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

data_clean_recruitment <- function(data, quiet = FALSE) {
  data <- remove_missing(data,
    quiet = quiet,
    cols = c(
      "Yearlings", "Cows", "UnknownAdults",
      "Year", "Month", "Day"
    )
  )
  data
}

data_prep_recruitment <- function(data, year_start = 4L) {
  data$CowsBulls <- data$Cows + data$Bulls
  data$Year <- caribou_year(data$Year, data$Month, year_start = year_start)
  data <-
    data %>%
    dplyr::group_by(Year,PopulationName) %>%
    dplyr::summarize(
      Cows = sum(.data$Cows),
      CowsBulls = sum(.data$CowsBulls),
      UnknownAdults = sum(.data$UnknownAdults),
      Yearlings = sum(.data$Yearlings),
      Calves = sum(.data$Calves)
    ) %>%
    dplyr::ungroup()
  data$Annual <- factor(data$Year)
  data$PopulationID = factor(data$PopulationName)

  data
}

data_list_recruitment <- function(data, model) {
  data <- rescale(data, scale = "Year")
  x <- list(
    nAnnual = length(unique(data$Annual)),
    nPops = length(unique(data$PopulationName)),
    nObs = nrow(data),
    Cows = data$Cows,
    CowsBulls = data$CowsBulls,
    UnknownAdults = data$UnknownAdults,
    Yearlings = data$Yearlings,
    Calves = data$Calves,
    #Cows = pivot_wider(subset(data,select=c(PopulationID,Year,Cows)),names_from=PopulationID,values_from=Cows)[,-1],
    #CowsBulls = pivot_wider(subset(data,select=c(PopulationID,Year,CowsBulls)),names_from=PopulationID,values_from=CowsBulls)[,-1],
    #UnknownAdults = pivot_wider(subset(data,select=c(PopulationID,Year,UnknownAdults)),names_from=PopulationID,values_from=UnknownAdults)[,-1],
    #Yearlings = pivot_wider(subset(data,select=c(PopulationID,Year,Yearlings)),names_from=PopulationID,values_from=Yearlings)[,-1],
    #Calves = pivot_wider(subset(data,select=c(PopulationID,Year,Calves)),names_from=PopulationID,values_from=Calves)[,-1],
    Year = data$Year,
    Annual = as.integer(data$Annual),
    PopulationID = as.integer(data$PopulationID),
    PopulationNames = levels(data$PopulationName)
  )
  x
}
