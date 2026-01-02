window.QuillService = {
  quill: null,

  set(quillInstance) {
    this.quill = quillInstance;
  },

  get() {
    return this.quill;
  }
};

function showToast(type = 'info', message = 'Message par défaut') {
  const container = document.getElementById('toast-container');
  const toast = document.createElement('div');
  toast.className = `my-toast ${type}`;
  toast.textContent = message;

  container.appendChild(toast);

  setTimeout(() => {
    toast.remove();
  }, 4000); // durée d'affichage
}

function confirmDelete(aForm){
	if (confirm("Etes-vous certains de vouloir supprimer ce fil d'informations ?")){
		aForm.dispatchEvent(new Event("submit", { cancelable: true, bubbles: true }));
	}
}

// Intercepter toutes les requêtes HTMX pour ajouter le JWT
document.body.addEventListener('htmx:configRequest', function(evt) {
  console.log("enregistrement token");
  const token = localStorage.getItem('jwt');
  if (token) {
	console.log("enregistrement ok");
	evt.detail.headers['jwt'] = 'Bearer ${'+token+'}';
  }
});
	
	// Fonction pour traduire un élément ou un conteneur
function translateElement(element) {
  // Si l'élément a un attribut data-i18n, le traduire
  if (element.hasAttribute('data-i18n')) {
    const key = element.getAttribute('data-i18n');
    element.textContent = i18next.t(key);
  }

  // Parcourir les enfants pour traduire les éléments imbriqués
  element.querySelectorAll('[data-i18n]').forEach(el => {
    const key = el.getAttribute('data-i18n');
    el.textContent = i18next.t(key);
  });
}

document.body.addEventListener('htmx:beforeSwap', function (evt) {
	const xhr = evt.detail.xhr;

	const text = event.detail.xhr.responseText;
	
	if (text.startsWith('ERR:')) {
		evt.preventDefault(); // annule la suite de l'évennement
		showToast('error', text.substr(4));
	} else if (text.startsWith('INFO:')) {
		evt.preventDefault(); // annule la suite de l'évennement
		showToast('success', text.substr(5));
	}
});

// Écouter l'événement HTMX après insertion de contenu
document.addEventListener('htmx:afterSwap', function(evt) {
  // Traduire le nouvel élément inséré par HTMX
  translateElement(evt.detail.elt);
});

function updateHxPost(elementId, newUrl) {
  const el = document.getElementById(elementId);
  if (!el) {
    console.warn(`Élément avec ID "${elementId}" introuvable.`);
    return;
  }

  el.setAttribute("hx-post", newUrl);
  htmx.process(el); // Re-scanne l’élément pour que HTMX prenne en compte le changement
}

let quill;

async function openPopup(id) {
  const btn = document.getElementById("btnSaveContent");
  btn.dataset.id = id;

  // Affichage du popup
  document.getElementById("overlay").style.display = "block";
  document.getElementById("popup").style.display = "block";
  document.body.style.overflow = "hidden";

  // Initialisation Quill si nécessaire
  if (!quill) {
    const Font = Quill.import('formats/font');
    Font.whitelist = ['sans-serif', 'serif', 'monospace', 'arial', 'times', 'comic'];
    Quill.register(Font, true);

    quill = new Quill('#editor-container', {
      theme: 'snow',
      modules: {
        toolbar: [
          [{ font: Font.whitelist }],
          ['bold', 'italic', 'underline'],
          [{ 'align': [] }],
          [{ list: 'ordered' }, { list: 'bullet' }],
          [{ 'header': [1, 2, 3, 4, 5, 6, false] }],
          [{ 'color': [] }, { 'background': [] }],
          ['link', 'image', 'video'],
          ['clean']
        ]
      }
    });
	
	window.QuillService.set( quill );
	console.log('quill affecté');
  }

  // Récupération du contenu serveur
  try {
	const token = localStorage.getItem('jwt');

    const response = await fetch('./GetNews?id=' + id, {
	  method: "GET",
	  headers: {
		"jwt": "Bearer " + token
	  }
	});
    const data = await response.json();

    if (data.content && data.content.trim() !== "") {
      quill.clipboard.dangerouslyPasteHTML(data.content);
    } else {
      quill.setText("");
    }
	
	console.log(data.Category);
	
	const tsBU = document.getElementById("categories").tomselect;
	const selectedBU = data.Category.map(String);
	tsBU.setValue(selectedBU);
	
	const tsSousCategorie = document.getElementById("sousCategories").tomselect;
	const selectedTypePartner = data.Subcategory.map(String);
	tsSousCategorie.setValue(selectedTypePartner);
	
	const tsPays = document.getElementById("pays").tomselect;
	const selectedPays = data.Country.map(String);
	tsPays.setValue(selectedPays);
	
	const tsLang = document.getElementById("langues").tomselect;
	const selectedLang = data.Lang.map(String);
	tsLang.setValue(selectedLang);
	
	//document.getElementById("select2").value = data.TypePartner;
	//document.getElementById("select3").value = data.Country;
	//document.getElementById("select4").value = data.Lang;
  } catch (err) {
    console.error('Erreur chargement fil d’info:', err);
    quill.setText("");
  }
}

function closePopup() {
  document.getElementById("popup").style.display = "none";
  document.getElementById("overlay").style.display = "none";
  document.body.style.overflow = ""; // réactive le scroll
}

async function SendContent() {
  const btn = document.getElementById("btnSaveContent");
  idNews = btn.dataset.id;

  html= quill.root.innerHTML;
  
  try {
	const token = localStorage.getItem('jwt');
	
	// Récupération des valeurs des listes
	
	// Récupère le <select multiple>
	const categoriesSelect = document.getElementById('categories');
	console.log("categoriesSelect "+categoriesSelect);
	const Category = Array.from(categoriesSelect.selectedOptions).map(opt => opt.value);
	
	const sousCategoriesSelect = document.getElementById("sousCategories");
	console.log("sousCategoriesSelect "+sousCategoriesSelect);
	const Subcategory = Array.from(sousCategoriesSelect.selectedOptions).map(opt => opt.value);
	
	const CountrySelect = document.getElementById("pays");
	console.log("CountrySelect "+CountrySelect);
	const Country = Array.from(CountrySelect.selectedOptions).map(opt => opt.value);
	
	const LangSelect = document.getElementById("langues");
	console.log("langselect "+LangSelect);
	const Lang = Array.from(LangSelect.selectedOptions).map(opt => opt.value);
	
	// Construction du payload
	const payload = {
	  content: html,       // texte Quill
	  Category: Category,  // valeur du select1
	  Subcategory: Subcategory,          // valeur du select2
	  Country: Country,      // valeur du select3
	  Lang: Lang   // valeur du select4
	};

    const res = await fetch('./saveContent?idNews='+idNews, {
      method: 'POST',
      headers: { 
		"Content-Type": "application/json",
		"jwt": "Bearer " + token},
      body: JSON.stringify(payload)
    });

    if (!res.ok) throw new Error(res.statusText || 'Erreur HTTP');

    // ✅ Met à jour le lien pour refléter le nouveau contenu
    //const btn = document.getElementById("btnViewNews");
	const btn = document.querySelector('.btnViewNews[data-idnews="' + idNews + '"]');
    btn.dataset.content = html;
	
	closePopup();
	
	console.log('lastEditedFeedId ' + lastEditedFeedId);
	refreshNewsPanel(lastEditedFeedId);
	lastEditedFeedId = null; // reset
  } catch (err) {
    alert('Erreur en sauvegarde: ' + (err.message || 'Erreur inconnue') );
  }
}

let lastEditedFeedId = null;

function setLastEditedFeed(el) {
	lastEditedFeedId = el.getAttribute('data-Feedid');
}
  
document.body.addEventListener('htmx:afterRequest', function(evt) {
    if (lastEditedFeedId) {
		console.log('refresh');
      refreshNewsPanel(lastEditedFeedId);
    }
	
	lastEditedFeedId = null; // reset
  });
  
function refreshNewsPanel(idFeed) {	
	const token = localStorage.getItem('jwt');
	
	fetch('./Show?idFeed='+idFeed+'&Template=ShowNews.html', {
	  method: "GET",
	  headers: {
		"jwt": "Bearer " + token
	  }
	})
    .then(response => response.text())
    .then(html => {
      document.getElementById('sidePanel').innerHTML = html;
    })
    .catch(err => console.error('Erreur chargement fil d’info:', err));
} 

function toggleDir(th) {
  let vals = JSON.parse(th.getAttribute("hx-vals"));
  vals.dir = vals.dir === "asc" ? "desc" : "asc";
  th.setAttribute("hx-vals", JSON.stringify(vals));

  // enlever les indicateurs des autres colonnes
  document.querySelectorAll("th").forEach(h => h.classList.remove("asc","desc"));

  // ajouter l’indicateur sur la colonne cliquée
  th.classList.add(vals.dir);
}
