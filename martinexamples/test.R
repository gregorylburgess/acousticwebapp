# Test function illustrating how to pass named parameters as a list.
test = function(params=...){
	if (!is.null(params["derp"])){
		print(params["derp"])
	}
	
	return("Hello WOrld")
	}

	
# Pass a list of named parameters to our test function
params=c(derp="Hi", derp2="HELLO")
print(test(params))