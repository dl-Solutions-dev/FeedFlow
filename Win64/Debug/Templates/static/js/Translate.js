// Fonction pour charger un fichier JSON
async function loadTranslationFile(lang) {
  console.log("fichier : "+`locales/${lang}.json`);
  
  const response = await fetch(`./static/locales/${lang}.json`);
  console.log(response);
  if (!response.ok) throw console.log("fichier non trouvé");
  return await response.json();
}

// Fonction pour traduire la page
function translatePage() {
  document.querySelectorAll('[data-i18n]').forEach(element => {
    const key = element.getAttribute('data-i18n');
    element.textContent = i18next.t(key);
  });
}

// Récupérer la langue de l'utilisateur
function getUserLanguage() {
  const token = localStorage.getItem('jwt');
  if (!token) return "en";

  try {
    const payload = JSON.parse(atob(token.split('.')[1]));
	console.log("payload : "+payload.Lang);
    return payload.Lang || "en";
  } catch (e) {
    console.error('Erreur lors du décodage du JWT :', e);
    return "en";
  }
}

// Initialiser i18next
async function initializeI18next() {
  const userLang = getUserLanguage();
  console.log("langue user : "+userLang);
  try {
    const translation = await loadTranslationFile(userLang);

    await i18next.init({
      lng: userLang.split('-')[0] || 'fr',
      fallbackLng: 'en',
      resources: {
        [userLang.split('-')[0]]: { translation }
      }
    });
    translatePage();
  } catch (error) {
    console.error("Erreur:", error);
    // En cas d'erreur, utiliser une langue par défaut
    await i18next.init({
      lng: 'fr',
      fallbackLng: 'en',
      resources: {
        fr: { translation: { greeting: "Bonjour" } }, // Ressources minimales
        en: { translation: { greeting: "Hello" } }
      }
    });
    translatePage();
  }
}

// Appeler l'initialisation
initializeI18next();
