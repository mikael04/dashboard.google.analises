Shiny.addCustomMessageHandler('background-color', function(color) {
    document.body.style.backgroundColor = color;
  });
$(document).ready(function() {
console.log('pronto');
    $('#tabs_dash li a').on('click', function() {
        aba_anterior = $(this).text()
        console.log('Aba - Clickando Aba (anterior) 1', aba_anterior);
        console.log('Aba - Clickando  6', $(this).text());
        if(aba_anterior == 'Table'){ // Tabela
            // Aba de navegação topo
            $('.navbar-default .navbar-nav .active a').css({'color': '#64C1C7'})
            
            // Aba de navegação (Gráfico/Tabela)
            //$('').css({'background-color': '#660909'})

            // Footer
            $('.footer').css({'background-color': '#660909'})
            
            //$('.navbar-default .navbar-nav .active a').css({'background-color': '#660909'})
            
            //$('.navbar-default .navbar-nav .active a').css({'background-color': '#660909'})
            $('.well').css({'height': '850px'})
        }else{ // Plots
            // Aba de navegação topo
            $('.navbar-default .navbar-nav a').css({'background-color': '#DEEBF0'})
            $('.navbar-default .navbar-nav .active a').css({'background-color': '#64C1C7'})
            
            // Aba de navegação (Gráfico/Tabela)
            //$('.tabbable ul li a').css({'background-color': '#64C1C7'})

            // Footer
            $('.footer').css({'background-color': '#64C1C7'})
            
        }     
    })
    /* Repetindo, mas agora para o switch*/
    Shiny.addCustomMessageHandler('switch_change', function(state) {
        console.log("Começando switch change")
        if(state == true){ // Tabela
            console.log("Mudando para tabela")
            console.log("Switch = true")
            // Aba de navegação topo
            $('.navbar-default .navbar-nav li a').css({'background-color': '#EADDDD'})
            $('.navbar-default .navbar-nav .active a').css({'background-color': '#660909'})
            
            $('#tabs_dash li a').css({'color': '#660909'})
            $('#tabs_dash li a').css({'font-weight': '700'})
            $('#tabs_dash li.active a').css({'color': 'black'})
            $('#tabs_dash li.active a').css({'font-weight': '400'})
            // Aba de navegação (Gráfico/Tabela)
            //$('').css({'background-color': '#660909'})

            // Footer
            $('.footer').css({'background-color': '#660909'})
            
            //$('.navbar-default .navbar-nav .active a').css({'background-color': '#660909'})
            
            //$('.navbar-default .navbar-nav .active a').css({'background-color': '#660909'})
            $('.well').css({'height': '850px'})
        }else{ // Plots
            console.log("Mudando para plots")
            console.log("Switch = false")
            // Aba de navegação topo
            $('.navbar-default .navbar-nav .active a').css({'background-color': '#DEEBF0'})
            $('.navbar-default .navbar-nav a').css({'background-color': '#64C1C7'})
            
            // Aba de navegação (Gráfico/Tabela)
            $('#tabs_dash li a').css({'color': '#64C1C7'})
            $('#tabs_dash li a').css({'font-weight': '700'})
            $('#tabs_dash li.active a').css({'color': 'black'})
            $('#tabs_dash li.active a').css({'font-weight': '400'})

            // Footer
            $('.footer').css({'background-color': '#64C1C7'})
            
        }     
      });
    })