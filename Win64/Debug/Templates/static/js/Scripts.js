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