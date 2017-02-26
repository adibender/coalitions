.onLoad <- function(libname=find.package("pam"), pkgname="pam") {
	
	if(getRversion() >= "2.5.1") {
		utils::globalVariables(
			c("Datum", "Kommentar", ",", ".", "Veroeffentlichung", "BEFRAGTE", "DATUM", 
				"PARTY", "PERCENT", "V1", "V11", "V12", "ZEITRAUM", "total"))
	}

	invisible()

}