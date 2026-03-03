document.getElementById("btnPrec").addEventListener("click", () => {
	const params = new URLSearchParams(window.location.search);
	const adminCategory = params.get("admin_category");
	const adminSubcategory = params.get("admin_subcategory");
	const adminCountry = params.get("admin_country");
	const adminLang = params.get("admin_lang");
	
	if (adminCategory != ""){
		window.location.href = "./Home?admin_category="+adminCategory+"&admin_subcategory="+adminSubcategory+"&admin_country="+adminCountry+"&admin_lang="+adminLang;
	} else {
		window.location.href = "./Home";
	}
});