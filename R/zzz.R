.onLoad <- function(libname=find.package("coalitions"), pkgname="coalitions") {

	if(getRversion() >= "2.5.1") {
		utils::globalVariables(c(
			"datum", "respondents", "party", "percent", "cdu", "others", "surveys",
			"probabilities", "seats", "sim", "survey", "total", "votes",
			".", "majority", "draws", "pollster", "address", "zeitraum",
			"befragte"))
	}

	invisible()

}