const uploadZoneDocument = document.getElementById('uploadDocumentZone');
const documentInput = document.getElementById('documentInput');
const documentPreviewv = document.getElementById('documentPreview');
const progressDocumentBar = document.getElementById('progressDocumentBar');

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

function handleDocuments(files) {
  [...files].forEach(file => {
	uploadDocument(file);
  });
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
			
			const quill = window.QuillService.get();

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