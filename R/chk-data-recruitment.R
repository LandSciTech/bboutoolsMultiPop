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

#' Check recruitment data structure
#'
#' The data must follow all requirements to not error. This format is required
#' for usage of the bbou suite of tools.
#'
#' @format The data must follow the requirements:
#' \describe{
#' \item{PopulationName}{Name of the herd or population}
#' \item{Year}{The calendar year the observation occurred. Must be a positive
#' integer.}
#' \item{Month}{The calendar month the observation occurred. Must be an integer
#' between 1 and 12.}
#' \item{Day}{The day the observation occurred. Must be an integer between 1 and
#' 31.}
#' \item{Cows}{The total number of cows counted in each group in a survey/year.
#' Must be a positive integer.}
#' \item{Bulls}{The total number of bulls counted in each group in a
#' survey/year. Must be a positive integer}
#' \item{UnknownAdults}{The total number of adults counted that the sex could
#' not be identified in each group in a survey/year. Must be a positive
#' integer.}
#' \item{Yearlings}{The total number of yearlings that did not have the sex
#' identified in each group in a survey/year. Must be a positive integer. }
#' \item{Calves}{The total number of calves counted in each group in a
#' survey/year. Must be a positive integer.}
#' }
#'
#' @param data The data frame check.
#' @param x_name Name of data frame.
#'
#' @return An invisible copy of the original data frame.
#' @export
#'
#' @examples
#' bbd_chk_data_recruitment(bbourecruit_a)
#' bbd_chk_data_recruitment(bbourecruit_b)
#' bbd_chk_data_recruitment(bbourecruit_c)

#' # this example will error as it doesn't follow the requirements
#' x <- bbourecruit_a
#' x[1, 4] <- 32L
#' try(bbd_chk_data_recruitment(x))
bbd_chk_data_recruitment <- function(data, x_name = deparse(substitute(data)),multi_pops=F) {
  nms <- c(
    "PopulationName", "Year", "Month", "Day", "Cows",
    "Bulls", "UnknownAdults", "Yearlings", "Calves"
  )
  chk::chk_superset(names(data), nms, x_name = x_name)

  chk::chk_character_or_factor(data$PopulationName, x_name = xname(x_name, "PopulationName"))
  chk::chk_not_any_na(data$PopulationName, x_name = "PopulationName")
  if(!multi_pops){.chk_population(data)}
  
  chk::chk_whole_numeric(data$Year, x_name = xname(x_name, "Year"))
  chk::chk_gte(data$Year, 0, x_name = xname(x_name, "Year"))

  chk::chk_whole_numeric(data$Month, x_name = xname(x_name, "Month"))
  chk::chk_range(data$Month, range = c(1, 12), x_name = xname(x_name, "Month"))

  chk::chk_whole_numeric(data$Day, x_name = xname(x_name, "Day"))
  chk::chk_range(data$Day, range = c(1, 31), x_name = xname(x_name, "Day"))

  chk::chk_not_any_na(data$Year, x_name = "Year")
  chk::chk_not_any_na(data$Month, x_name = "Month")
  chk::chk_not_any_na(data$Day, x_name = "Day")

  chk::chk_whole_numeric(data$Cows, x_name = xname(x_name, "Cows"))
  chk::chk_gte(data$Cows, 0, x_name = xname(x_name, "Cows"))

  chk::chk_whole_numeric(data$Bulls, x_name = xname(x_name, "Bulls"))
  chk::chk_gte(data$Bulls, 0, x_name = xname(x_name, "Bulls"))

  chk::chk_whole_numeric(data$UnknownAdults, x_name = xname(x_name, "UnknownAdults"))
  chk::chk_gte(data$UnknownAdults, 0, x_name = xname(x_name, "UnknownAdults"))

  chk::chk_whole_numeric(data$Yearlings, x_name = xname(x_name, "Yearlings"))
  chk::chk_gte(data$Yearlings, 0, x_name = xname(x_name, "Yearlings"))

  chk::chk_whole_numeric(data$Calves, x_name = xname(x_name, "Calves"))
  chk::chk_gte(data$Calves, 0, x_name = xname(x_name, "Calves"))

  invisible(data)
}
