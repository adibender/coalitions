#' Initial dowload and data base creation
#' 
#' @import dplyr
#' @importFrom magrittr "%<>%"
#' @importFrom tidyr nest unnest
initialize <- function(path="./") {

	assert_directory(path)

	current <- get_surveys()

	surveys_meta <- current %>% 
		unnest() %>% 
		select(institute, datum:befragte)

	saveRDS(surveys_meta, paste0(path, "surveys_meta.Rds"))

	surveys <- current %>% 
		select(institute, surveys) %>% 
		unnest() %>% 
		select(institute, datum, survey)

	saveRDS(surveys, paste0(path, "surveys_raw.Rds"))

}


#' Extract "meta" information from survey data base
#' 
#' @importFrom dplyr select
#' @importFrom tidyr unnest
#' @export
get_meta <- function(surveys_df) {

	surveys_df %>% 
		unnest() %>% 
		select(institute, datum:befragte)

}



update_surveys <- function(
	path="./", 
	name_meta="surveys_meta.Rds", 
	name_raw ="surveys_raw.Rds") {

	meta_old <- readRDS(paste0(path, name_meta))
	new_df   <- get_surveys()
	meta_new    <- get_meta(new_df)

	meta_update <- anti_join(meta_new, meta_old)

	new_df %>% unnest() %>% semi_join(meta_update) %>% 
		mutate(probs = get_probs(., nsim=10))



}

#' Scrape surveys from all survey institutes
#' 
#' @import dplyr
#' @importFrom purrr map
#' @export
get_surveys <- function() {

	institutes_df %>% 
		mutate(
			surveys = map(address, scrape_wahlrecht), 
			surveys = map(surveys, collapse_parties)) %>%
		select(-address)

}