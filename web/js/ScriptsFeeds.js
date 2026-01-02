// Récupérer le JWT depuis l'URL
const urlParams = new URLSearchParams(window.location.search);
const token = urlParams.get('token');

if (token) {
	alert(token);
	
	// Stocker le JWT dans localStorage
	localStorage.setItem('jwt', token);

	// Supprimer le token de l'URL pour éviter qu'il ne reste visible
	//window.history.replaceState({}, document.title, window.location.pathname);
}

new TomSelect("#categories",   { plugins: ['remove_button'], maxItems: null });
new TomSelect("#sousCategories",   { plugins: ['remove_button'], maxItems: null });
new TomSelect("#pays",   { plugins: ['remove_button'], maxItems: null });
new TomSelect("#langues",   { plugins: ['remove_button'], maxItems: null });
	
// Ouverture du popup de contexte
async function openContextPopup(id) {
  const btn = document.getElementById("btnSaveContent");
  btn.dataset.id = id;

  // Affichage du popup
  document.getElementById("overlay").style.display = "block";
  document.getElementById("popup").style.display = "block";
  document.body.style.overflow = "hidden";

  // Récupération du contenu serveur
  try {
	const token = localStorage.getItem('jwt');

    const response = await fetch('./GetFeed?id=' + id, {
	  method: "GET",
	  headers: {
		"jwt": "Bearer " + token
	  }
	});
    const data = await response.json();
	const tsBU = document.getElementById("categories").tomselect;
	const tsSousCategorie = document.getElementById("sousCategories").tomselect;
	const tsPays = document.getElementById("pays").tomselect;
	const tsLang = document.getElementById("langues").tomselect;
	
	if (data.AllContext == "O") {
		document.getElementById("AllContext").checked = true;
		
		tsBU.disable();
		tsSousCategorie.disable();
		tsPays.disable();
		tsLang.disable();
	} else {
		document.getElementById("AllContext").checked = false;
		
		tsBU.enable();
		tsSousCategorie.enable();
		tsPays.enable();
		tsLang.enable();
		
		console.log(data.BU);
		
		const selectedBU = data.BU.map(String);
		tsBU.setValue(selectedBU);
		
		const selectedTypePartner = data.TypePartner.map(String);
		tsSousCategorie.setValue(selectedTypePartner);
		
		const selectedPays = data.Country.map(String);
		tsPays.setValue(selectedPays);
		
		const selectedLang = data.Lang.map(String);
		tsLang.setValue(selectedLang);
	}
	//document.getElementById("select2").value = data.TypePartner;
	//document.getElementById("select3").value = data.Country;
	//document.getElementById("select4").value = data.Lang;
  } catch (err) {
    console.error('Erreur chargement fil d’info:', err);
    quill.setText("");
  }
}

// Gestion du checkbox
document.getElementById('AllContext').addEventListener('change', function () {
	const tsBU = document.getElementById("categories").tomselect;
	const tsSousCategorie = document.getElementById("sousCategories").tomselect;
	const tsPays = document.getElementById("pays").tomselect;
	const tsLang = document.getElementById("langues").tomselect;
	
    if (this.checked) {
        tsBU.disable();   
		tsSousCategorie.disable(); 
		tsPays.disable(); 
		tsLang.disable(); 
		
		tsBU.clear();   
		tsSousCategorie.clear(); 
		tsPays.clear(); 
		tsLang.clear(); 
    } else {
        tsBU.enable(); 
		tsSousCategorie.enable();
		tsPays.enable();
		tsLang.enable();		
    }
});

function confirmDelete(aForm){
	if (confirm("Etes-vous certains de vouloir supprimer ce fil d'informations ?")){
		aForm.dispatchEvent(new Event("submit", { cancelable: true, bubbles: true }));
	}
}

async function SendContext() {
  const btn = document.getElementById("btnSaveContent");
  idFeed = btn.dataset.id;

  console.log('Feed Id : '+idFeed);
  
  try {
	const token = localStorage.getItem('jwt');
	
	const CheckAll = document.getElementById("AllContext");
	if (CheckAll.checked == true){
		ALL = "O";
	} else {
		ALL = "N";
	}
	// Récupération des valeurs des listes
	
	// Récupère le <select multiple>
	const categoriesSelect = document.getElementById('categories');
	console.log("categoriesSelect "+categoriesSelect);
	const BU = Array.from(categoriesSelect.selectedOptions).map(opt => opt.value);
	
	const sousCategoriesSelect = document.getElementById("sousCategories");
	console.log("sousCategoriesSelect "+sousCategoriesSelect);
	const TypePartner = Array.from(sousCategoriesSelect.selectedOptions).map(opt => opt.value);
	
	const CountrySelect = document.getElementById("pays");
	console.log("CountrySelect "+CountrySelect);
	const Country = Array.from(CountrySelect.selectedOptions).map(opt => opt.value);
	
	const LangSelect = document.getElementById("langues");
	console.log("langselect "+LangSelect);
	const Lang = Array.from(LangSelect.selectedOptions).map(opt => opt.value);
	
	// Construction du payload
	const payload = {
	  AllContext: ALL,
	  BU: BU,  // valeur du select1
	  TypePartner: TypePartner,          // valeur du select2
	  Country: Country,      // valeur du select3
	  Lang: Lang   // valeur du select4
	};

    const res = await fetch('./saveContext?idFeed='+idFeed, {
      method: 'POST',
      headers: { 
		"Content-Type": "application/json",
		"jwt": "Bearer " + token},
      body: JSON.stringify(payload)
    });

    if (!res.ok) throw new Error(res.statusText || 'Erreur HTTP');

    // ✅ Met à jour le lien pour refléter le nouveau contenu
    //const btn = document.getElementById("btnViewNews");
	const btn = document.querySelector('.btnViewNews[data-idfeed="' + idFeed + '"]');
    btn.dataset.content = html;
	
	closePopup();
  } catch (err) {
    alert('Erreur en sauvegarde: ' + (err.message || 'Erreur inconnue') );
  }
}