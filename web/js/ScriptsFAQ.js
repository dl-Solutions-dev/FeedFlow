const carousel = document.querySelector("#popularFaqCarousel");

if (carousel) {
	console.log("carousel");
	const bsCarousel = new bootstrap.Carousel(carousel, {
		interval: 5000, // temps entre chaque slide (ms)
		pause: "hover", // pause quand la souris est dessus
		ride: "carousel" // démarre automatiquement
	});

	const carouselEl = document.querySelector("#popularFaqCarousel");
	const items = carouselEl.querySelectorAll(".carousel-item");

	// Ne pas appliquer la hauteur fixe en mobile
	if (window.innerWidth > 768) {
		let maxHeight = 0;

		items.forEach((item) => {
			const h = item.scrollHeight;
			if (h > maxHeight) maxHeight = h;
		});

		carouselEl.querySelector(".carousel-inner").style.height = maxHeight + "px";
	}

	function adaptCarouselForMobile() {
		console.log("ok");
		const isMobile = window.innerWidth <= 768;
		const carousel = document.querySelector("#popularFaqCarousel");
		const inner = carousel.querySelector(".carousel-inner");

		// Si mobile → reconstruire le carrousel en 1 card/slide
		if (isMobile) {
			const allCards = [...inner.querySelectorAll(".mini-card")];

			// On vide le carrousel
			inner.innerHTML = "";

			allCards.forEach((card, index) => {
				const item = document.createElement("div");
				item.classList.add("carousel-item");
				if (index === 0) item.classList.add("active");

				const row = document.createElement("div");
				row.classList.add("d-flex", "justify-content-center");

				// On clone la card pour éviter de la retirer du desktop
				row.appendChild(card.cloneNode(true));

				item.appendChild(row);
				inner.appendChild(item);
			});
		}
	}

	adaptCarouselForMobile();
	window.addEventListener("resize", adaptCarouselForMobile);
}