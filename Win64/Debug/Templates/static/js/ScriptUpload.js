const uploadZone = document.getElementById('uploadZone');
const fileInput = document.getElementById('fileInput');
const preview = document.getElementById('TemplateName');
const progressBar = document.getElementById('progressBar');

// Clique sur la zone => ouvre le sélecteur
uploadZone.addEventListener('click', () => fileInput.click());

// Drag & drop
uploadZone.addEventListener('dragover', (e) => {
  e.preventDefault();
  uploadZone.classList.add('dragover');
});
uploadZone.addEventListener('dragleave', () => {
  uploadZone.classList.remove('dragover');
});
uploadZone.addEventListener('drop', (e) => {
  e.preventDefault();
  uploadZone.classList.remove('dragover');
  handleFiles(e.dataTransfer.files);
});

// Input classique
fileInput.addEventListener('change', () => {
  handleFiles(fileInput.files);
});

function handleFiles(files) {
  TemplateName.innerHTML = '';
  [...files].forEach(file => {
	if (file.type.startsWith('image/')) {
	  const img = document.createElement('img');
	  img.src = URL.createObjectURL(file);
	  preview.appendChild(img);
	} else {
	  //const p = document.createElement('p');
	  //p.textContent = file.name;
	  //preview.appendChild(p);
	  TemplateName.innerHTML = file.name;
	}
	uploadFile(file);
  });
}

function uploadFile(file) {
  const FeedId = document.getElementById('FeedId');
  lastEditedFeedId = FeedId.innerHTML;
  
  const formData = new FormData();
  formData.append('file', file);

  const xhr = new XMLHttpRequest();
  xhr.open('POST', '/uploadTemplate?FeedId='+FeedId.innerHTML); // 🔧 adapte ton endpoint backend

  xhr.upload.addEventListener('progress', (e) => {
	if (e.lengthComputable) {
	  const percent = (e.loaded / e.total) * 100;
	  progressBar.style.width = percent + '%';
	}
  });

  xhr.onload = () => {
	if (xhr.status === 200) {
	  refreshNewsPanel(lastEditedFeedId);
	} else {
	  alert('Erreur lors de l’upload ❌');
	}
	lastEditedFeedId = null; // reset
  };

  xhr.send(formData);
}