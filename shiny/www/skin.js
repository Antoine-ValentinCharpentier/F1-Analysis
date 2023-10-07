Shiny.addCustomMessageHandler('change_skin', function(colors) {
    /*Skin*/
    document.body.className = `skin-${colors['skin']}`;
    
    /*Infos box (Stats)*/
    document.querySelectorAll('.info-box-icon').forEach(function(element) {
      element.className = "info-box-icon";
      element.style.backgroundColor = colors['first'];
      element.style.color = 'white';
    });

    /*Boxes*/
    document.querySelectorAll('.box').forEach(function(element) {
      element.style.borderTopColor = colors['first'];
    });

    /*Slider*/
    document.querySelectorAll('div:not(.slider-color-remover) > .shiny-input-container .irs-bar').forEach(function(element) {
      element.style.backgroundColor = colors['first'];
      element.style.borderTopColor = colors['first'];
      element.style.borderBottomColor = colors['first'];
    });
    document.querySelectorAll('div.slider-color-remover > .shiny-input-container .irs-bar').forEach(function(element) {
      element.style.display = 'none';
    });
    document.querySelectorAll('.irs-single').forEach(function(element) {
      element.style.backgroundColor = colors['first'];
    });
    document.querySelectorAll('.irs-from').forEach(function(element) {
      element.style.backgroundColor = colors['first'];
    });
    document.querySelectorAll('.irs-to').forEach(function(element) {
      element.style.backgroundColor = colors['first'];
    });

    /*f1*/
    document.querySelectorAll('.f1-color1').forEach(function(element) {
      element.style.backgroundColor = colors['first'];
    });
    document.querySelectorAll('.f1-color3').forEach(function(element) {
      element.style.backgroundColor = colors['fourth'];
    });
    document.querySelectorAll('.f1-color-mix').forEach(function(element) {
      element.style.backgroundImage = `linear-gradient(0deg, ${colors['first']} 63%, ${colors['second']} 63%)`;
    });
    document.querySelectorAll('.back-wing::before').forEach(function(element) {
      element.style.backgroundColor = colors['second'];
    });
    document.querySelectorAll('.wheel-rayons::before').forEach(function(element) {
      element.style.backgroundColor = colors['second'];
    });
    document.querySelectorAll('.wheel-rayons::after').forEach(function(element) {
      element.style.backgroundColor = colors['second'];
    });
});
