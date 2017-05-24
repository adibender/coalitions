institutes_df <- tibble::tibble(
	institute = c(
		"allensbach", 
		"emnid", 
		"forsa", 
		"fgw", 
		"gms", 
		"infratest", 
		"insa"), 
	address = c(
		"http://www.wahlrecht.de/umfragen/allensbach.htm",
		"http://www.wahlrecht.de/umfragen/emnid.htm",
		"http://www.wahlrecht.de/umfragen/forsa.htm",
		"http://www.wahlrecht.de/umfragen/politbarometer.htm",
		"http://www.wahlrecht.de/umfragen/gms.htm",
		"http://www.wahlrecht.de/umfragen/dimap.htm", 
		"http://www.wahlrecht.de/umfragen/insa.htm"))

div_vec <- 0.5:(598+0.5)
div_mat <- t(matrix(1/div_vec, nrow=length(div_vec), ncol=20))

devtools::use_data(div_mat, institutes_df, internal = TRUE, overwrite=TRUE)