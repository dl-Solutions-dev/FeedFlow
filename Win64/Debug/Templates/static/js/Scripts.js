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

function openPopup_anc(texte1, id) {
  const btn = document.getElementById("btnSaveContent");
  btn.dataset.id = id;
  
  fetch('./GetNews?id='+id)
    .then(response => response.text())
    .then(html => {
      texte = html;
    })
    .catch(err => console.error('Erreur chargement fil d’info:', err));
	
  document.getElementById("overlay").style.display = "block";
  document.getElementById("popup").style.display = "block";
  document.body.style.overflow = "hidden";

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
  }
  
  if (typeof texte != 'undefined' & texte != null) {
	quill.clipboard.dangerouslyPasteHTML(texte);
  } else {
	quill.setText("");
  }
}

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
	
	console.log(data.BU);
	// Positionner les valeurs dans les listes déroulantes
	document.getElementById("select1").value = data.BU;
	document.getElementById("select2").value = data.TypePartner;
	document.getElementById("select3").value = data.Country;
	document.getElementById("select4").value = data.Lang;
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
	const BU = document.getElementById("select1").value;
	const TypePartner = document.getElementById("select2").value;
	const Country = document.getElementById("select3").value;
	const Lang = document.getElementById("select4").value;
	
	// Construction du payload
	const payload = {
	  content: html,       // ton texte Quill
	  BU: BU,  // valeur du select1
	  TypePartner: TypePartner,          // valeur du select2
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


