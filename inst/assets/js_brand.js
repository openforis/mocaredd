// FAO branding: footer links and alt texts follow the active language; favicon follows
// the browser light/dark theme.
// Elements carry data-src-<lang>, data-alt-<lang> or data-href-<lang> attributes
// (en, fr, es); see R/shiny_run_mocaredd.R. The FAO logo is English in all languages.

// Language: <html lang>, footer links, Global Goals alt text
Shiny.addCustomMessageHandler('app-locale', function(msg) {
  var lang = msg.lang;
  document.documentElement.setAttribute('lang', lang);
  ['src', 'alt', 'href'].forEach(function(attr) {
    document.querySelectorAll('[data-' + attr + '-' + lang + ']').forEach(function(el) {
      el.setAttribute(attr, el.getAttribute('data-' + attr + '-' + lang));
    });
  });
});

// !!! FAO BRANDING ON HOLD, pending authorisation: the FAO favicon switch below is
// commented out (see R/shiny_run_mocaredd.R). To revert, uncomment it.
// !!!
// // Favicon: white FAO emblem when the browser or OS uses a dark theme.
// // Chrome ignores a changed href on the existing <link>, so the element is replaced.
// (function() {
//   var dark = window.matchMedia('(prefers-color-scheme: dark)');
//   var setFavicon = function() {
//     var old = document.getElementById('favicon');
//     var link = document.createElement('link');
//     link.id = 'favicon';
//     link.rel = 'icon';
//     link.type = 'image/x-icon';
//     link.href = dark.matches ? 'assets/branding/favicon-white.ico' : 'assets/branding/favicon.ico';
//     if (old) old.remove();
//     document.head.appendChild(link);
//   };
//   document.addEventListener('DOMContentLoaded', setFavicon);
//   dark.addEventListener('change', setFavicon);
// })();
