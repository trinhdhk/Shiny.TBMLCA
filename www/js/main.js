$(document).on('shiny:sessioninitialized', (event) => {
  console.log('Session Initialized. Color scheme adjustment.')
  if (window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches) {
    $("html").addClass("dark");
    $("body").addClass("dark");
    Shiny.setInputValue('color_scheme', 'dark');
  } else {
    $("html").removeClass("dark");
    $("body").removeClass("dark");
    Shiny.setInputValue('color_scheme', 'light');
  }
  
  window.matchMedia("(prefers-color-scheme: dark)")
    .addEventListener("change", e => {
      const newColorScheme = e.matches ? "dark" : "light";
      Shiny.setInputValue('color_scheme', newColorScheme);
      if (newColorScheme == 'dark') {
        $("html").addClass("dark");
        $("body").addClass("dark");
      } else {
        $("html").removeClass("dark");
        $("body").removeClass("dark");
      }
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
              