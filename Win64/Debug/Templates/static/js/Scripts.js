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

document.body.addEventListener('htmx:beforeSwap', function (evt) {
	const xhr = evt.detail.xhr;

	const text = event.detail.xhr.responseText;
	console.log("retour : " + text);
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

function openPopup(texte, id) {
  const btn = document.getElementById("btnSaveContent");
  btn.dataset.id = id;
  
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
		  [{ list: 'ordered' }, { list: 'bullet' }],
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
    const res = await fetch('/saveContent?idNews='+idNews, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ content: html })
    });

    if (!res.ok) throw new Error(res.statusText || 'Erreur HTTP');

    // ✅ Met à jour le lien pour refléter le nouveau contenu
    //const btn = document.getElementById("btnViewNews");
	const btn = document.querySelector('.btnViewNews[data-id="' + idNews + '"]');
    btn.dataset.content = html;
	
	closePopup();
  } catch (err) {
    alert('Erreur en sauvegarde: ' + (err.message || 'Erreur inconnue') );
  }
}

