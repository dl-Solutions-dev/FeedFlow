const uploadZoneDocument = document.getElementById('uploadDocumentZone');
const documentInput = document.getElementById('documentInput');
const documentPreviewv = document.getElementById('documentPreview');
const progressDocumentBar = document.getElementById('progressDocumentBar');

// Initialisation de Quill
let quill;

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

///window.QuillService.set( quill );

// Clique sur la zone => ouvre le sélecteur
uploadZoneDocument.addEventListener('click', () => documentInput.click());

// Drag & drop
uploadZoneDocument.addEventListener('dragover', (e) => {
  e.preventDefault();
  uploadZoneDocument.classList.add('dragover');
});
uploadZoneDocument.addEventListener('dragleave', () => {
  uploadZoneDocument.classList.remove('dragover');
});
uploadZoneDocument.addEventListener('drop', (e) => {
  e.preventDefault();
  uploadZoneDocument.classList.remove('dragover');
  handleDocuments(e.dataTransfer.files);
});

// Input classique
fileInput.addEventListener('change', () => {
  handleDocuments(documentInput.files);
});

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

function handleDocuments(files) {
  [...files].forEach(file => {
	uploadDocument(file);
  });
}

const renameInput = document.getElementById('file-rename-input');
let activeFileLink = null;

// double-clic sur un lien
quill.root.addEventListener('mousedown', (e) => {
  if (e.detail !== 2) return; // double clic

  const link = e.target.closest('a');
  if (!link || !quill.root.contains(link)) return;

  e.preventDefault();
  e.stopPropagation();

  const blot = Quill.find(link);
  if (!blot) return;

  activeFileLink = {
    index: blot.offset(quill.scroll),
    length: blot.length(),
    url: link.getAttribute('href'),
    text: link.innerText
  };

  showRenameInput(activeFileLink);
});



function showRenameInput(fileLink) {
  const bounds = quill.getBounds(
    fileLink.index,
    fileLink.length
  );

  const editorRect = quill.root.getBoundingClientRect();

  renameInput.style.top =
    `${editorRect.top + bounds.top - 2}px`;

  renameInput.style.left =
    `${editorRect.left + bounds.left}px`;

  renameInput.style.width =
    `${Math.max(bounds.width + 16, 120)}px`;

  renameInput.value = fileLink.text;
  renameInput.style.display = 'block';
  renameInput.focus();
  renameInput.select();
}


renameInput.addEventListener('keydown', (e) => {
  if (e.key === 'Enter') {
    e.preventDefault();
    applyRename();
  }

  if (e.key === 'Escape') {
    hideRenameInput();
  }
});

renameInput.addEventListener('blur', hideRenameInput);

function applyRename() {
  const newName = renameInput.value.trim();
  if (!newName || !activeFileLink) {
    hideRenameInput();
    return;
  }

  quill.deleteText(
    activeFileLink.index,
    activeFileLink.length,
    'silent'
  );

  quill.insertText(
    activeFileLink.index,
    newName,
    { link: activeFileLink.url },
    'silent'
  );

  quill.setSelection(
    activeFileLink.index + newName.length,
    0,
    'silent'
  );

  hideRenameInput();
}

function hideRenameInput() {
  renameInput.style.display = 'none';
  activeFileLink = null;
}

function uploadDocument(file) { 
  const formData = new FormData();
  formData.append('file', file);

  const token = localStorage.getItem('jwt');
  const xhr = new XMLHttpRequest();
  xhr.open('POST', '/UploadDocument'); 

  xhr.setRequestHeader('jwt', 'Bearer ' + token);
  
  xhr.upload.addEventListener('progress', (e) => {
	if (e.lengthComputable) {
	  const percent = (e.loaded / e.total) * 100;
	  progressDocumentBar.style.width = percent + '%';
	}
  });

  xhr.onload = () => {
	if (xhr.status === 200) {
		try {
			const response = xhr.responseText;
			const data = JSON.parse(response);
			const fileName = data.url.split('/').pop();
			
			//const quill = window.QuillService.get();

			const range = quill.getSelection();
			if (range) {
				quill.insertText(range.index, fileName, { link: data.url });
			} else {
				const pos = quill.getLength();

				quill.insertText(pos, fileName, { link: data.url });
				quill.insertText(pos + data.url.length, "\n");
				}
		} catch (e) {
			
			console.error("La réponse n'est pas du JSON valide", e);
		}
	} else {
	  alert('Erreur lors de l’upload ❌');
	}
	lastEditedFeedId = null; // reset
  };

  xhr.send(formData);
}