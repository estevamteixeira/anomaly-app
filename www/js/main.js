window.onload = function() {
  const footerHeading = document.querySelector('.footer-heading');
  const grid = document.querySelector('.main-content-grid')
  const addChange = () => grid.style.display = "flex";
  if (!footerHeading || !grid) {
    addChange();
  } else if (!footerHeading.textContent.toLowerCase().includes("marketplace")) {
    addChange();
  } else {
    grid.style.display = "grid";
  }
}
