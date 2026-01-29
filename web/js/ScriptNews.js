new TomSelect("#categories",   { plugins: ['remove_button'], maxItems: null });
new TomSelect("#sousCategories",   { plugins: ['remove_button'], maxItems: null });
new TomSelect("#pays",   { plugins: ['remove_button'], maxItems: null });
new TomSelect("#langues",   { plugins: ['remove_button'], maxItems: null });

document.getElementById("btnPrec").addEventListener("click", () => {
  window.location.href = "./FeedsList?scope=Page";
});
