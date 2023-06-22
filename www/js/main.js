$(document).on('shiny:sessioninitialized', (event) => {
  if (window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches) {
    $("html").addClass("theme-dark");
    Shiny.setInputValue('color_scheme', 'dark');
  } else {
    $("html").removeClass("theme-dark");
    Shiny.setInputValue('color_scheme', 'light');
  }
  window.matchMedia("(prefers-color-scheme: dark)")
    .addEventListener("change", e => {
      const newColorScheme = e.matches ? "dark" : "light";
      Shiny.setInputValue('color_scheme', newColorScheme);
      if (newColorScheme == 'dark')
        $("html").addClass("theme-dark");
      else
        $("html").removeClass("theme-dark");
  });
});

$(document).ready(()=> {
  
  $('.tab-link').click(function(){
    tab = $(this)[0]
    if (tab.classList.contains('tab-link-active') && tab.dataset.tab.includes('Diagnosis')){
      $('button#submit').click();
    }
  });
})
              