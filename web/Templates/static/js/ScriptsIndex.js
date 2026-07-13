document.getElementById("btnLogin").addEventListener("click", async () => {
	const user = document.getElementById("username").value;
	const pass = document.getElementById("passwd").value;
	const errorDiv = document.getElementById("errorMsg");

	// Nettoyage du message précédent
	errorDiv.textContent = "";

	try {
	const response = await fetch("./login", {
	  method: "POST",
	  headers: { "Content-Type": "application/x-www-form-urlencoded" },
	  body: `username=${encodeURIComponent(user)}&password=${encodeURIComponent(pass)}`
	});

	if (response.ok) {
	  // ✅ Succès : on suppose que le serveur renvoie { token: "..." }
	  const data = await response.json();
	  localStorage.setItem("jwt", data.token);

	  // Redirection vers la page principale
	  window.location.href = "./Home";
	} else if (response.status === 401) {
	  // ❌ Identifiants invalides
	  errorDiv.textContent = "Nom d'utilisateur ou mot de passe incorrect !";
	} else {
	  // ❌ Autre erreur serveur
	  const errorText = await response.text();
	  errorDiv.textContent = "Erreur serveur : " + errorText;
	}
	} catch (err) {
	// ❌ Erreur réseau ou autre
	console.error("Erreur de connexion :", err);
	errorDiv.textContent = "Impossible de contacter le serveur. Vérifiez votre connexion.";
	}
});