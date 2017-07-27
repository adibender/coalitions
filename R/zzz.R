.onLoad <- function(libname=find.package("pam"), pkgname="pam") {
	
	if(getRversion() >= "2.5.1") {
		utils::globalVariables(c(
			"befragte", "datum", "party", "percent", 
			"probabilities", "seats", "sim", "survey", "total", "votes", "zeitraum", 
			".", "majority", "draws", "institute", "address"))
	}

	invisible()

}