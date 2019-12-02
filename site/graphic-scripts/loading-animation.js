//with some help from https://css-tricks.com/making-animations-wait/
//although I don't love that so, tinker with this instead? http://spin.js.org/
document.documentElement.className += " js-loading";

window.addEventListener("load", showPage, false);

function showPage() {
  document.documentElement.className = document.documentElement.className.replace("js-loading","");
}
