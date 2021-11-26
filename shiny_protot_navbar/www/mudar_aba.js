debug = true
Shiny.addCustomMessageHandler('background-color', function(color) {
    document.body.style.backgroundColor = color;
  });
$(document).ready(function() {
console.log('pronto');
console.log('debug = ', debug)
aba_anterior = "Dashboard"

    /* Fazendo a alteração de cores ao clicar na mudança de tabpanel */
    $('#tabs_dash li a').on('click', function() {
        aba_anterior = $(this).text()
        if(debug){
            console.log('Aba - Clickando Aba (anterior) 1', aba_anterior);
            console.log('Aba - Clickando  6', $(this).text());
        }
        if(aba_anterior == 'Table'){ // Tabela
            // // Aba de navegação topo
            // $('.navbar-default .navbar-nav .active a').css({'color': '#64C1C7'})
            
            // Aba de navegação (Gráfico/Tabela)
            //$('').css({'background-color': '#660909'})

            // Footer
            // $('.footer').css({'background-color': '#660909'})
            
            //$('.navbar-default .navbar-nav .active a').css({'background-color': '#660909'})
            
            //$('.navbar-default .navbar-nav .active a').css({'background-color': '#660909'})
            // $('.well').css({'height': '850px'})
        }else{ // Plots
            // // Aba de navegação topo
            // $('.navbar-default .navbar-nav a').css({'background-color': '#DEEBF0'})
            // $('.navbar-default .navbar-nav .active a').css({'background-color': '#64C1C7'})
            
            // // Aba de navegação (Gráfico/Tabela)
            // $('.tabbable ul li a').css({'background-color': '#64C1C7'})

            // // Footer
            // $('.footer').css({'background-color': '#64C1C7'})
            
        }     
    })
    /* Repetindo, mas agora para o switch*/
    Shiny.addCustomMessageHandler('switch_change', function(state) {
        if(debug){
            console.log("Começando switch change")            
        }
        if(state == true){ // Tabela
            if(debug){
                console.log("Mudando para tabela")
                console.log("Switch = true")
                console.log("$('.navbar-default .navbar-nav a') = ", $('.navbar-default .navbar-nav li.active a').text())           
            }
            // Aba de navegação topo
            $('.navbar-default .navbar-nav a').css({'background-color': '#EADDDD'})
            $('.navbar-default .navbar-nav>.active>a').css({'background-color': '#660909'})
            
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
            if(debug){
                console.log("Mudando para plots")
                console.log("Switch = false")            
            }
            // Aba de navegação topo
            $('.navbar-default .navbar-nav li a').css({'background-color': '#DEEBF0'})
            $('.navbar-default .navbar-nav li.active a').css({'background-color': '#64C1C7'})
            
            // Aba de navegação (Gráfico/Tabela)
            $('#tabs_dash li a').css({'color': '#64C1C7'})
            $('#tabs_dash li a').css({'font-weight': '700'})
            $('#tabs_dash li.active a').css({'color': 'black'})
            $('#tabs_dash li.active a').css({'font-weight': '400'})

            // Footer
            $('.footer').css({'background-color': '#64C1C7'})
            
        }     
    });
    // /* Voltando a cor padrão ao sair de "Dashboard */
    // $('#navbar li a').on('click', function(state) {
    //     navbar_sel = $('#navbar li.active a').text();
    //     if(debug){
    //         console.log("Aba selecionada = ", navbar_sel)         
    //     }
    //     if(navbar_sel == "Dashboard"){
    //         if(debug){
    //             console.log("Se Dashboard")    
    //         }     
    //         dash_sel = $('#tabs_dash li.active a').text()
    //         if(dash_sel == 'Tabela'){
    //             if(debug){
    //                 console.log("Se estava em tabela")    
    //             }   
    //             Footer
    //             $('.footer').css({'background-color': '#660909 !important'})

    //         }else{

    //         }
    //     }else{
    //         if(debug){
    //             console.log("Se Sobre")        
    //         }
    //         Footer
    //         $('.footer').css({'background-color': '#64C1C7'})

    //     }
    // })
    Shiny.addCustomMessageHandler('navbar_change', function(state) {
        if(debug){
            console.log("Começando navbar change")
            console.log("State = ", state)           
        }
        if(state == true){
            if(debug){
                console.log("State = T")
                console.log("Tab selecionada =  ", $('#tabs_dash li.active a').text())           
            }
            if($('#tabs_dash li.active a').text() == "Tabela"){
                // Aba de navegação topo
                $('.navbar-default .navbar-nav a').css({'background-color': '#EADDDD'})
                $('.navbar-default .navbar-nav>.active>a').css({'background-color': '#660909'})
                // Footer
                $('.footer').css({'background-color': '#660909'})
            }else{
                // Aba de navegação topo
                $('.navbar-default .navbar-nav a').css({'background-color': '#DEEBF0'})
                $('.navbar-default .navbar-nav>.active>a').css({'background-color': '#64C1C7'})
            }         
        }else{
            if($('#tabs_dash li.active a').text() == "Tabela"){
                if(debug){
                    console.log("State = F")
                    console.log("Tab selecionada =  ", $('#tabs_dash li.active a').text())           
                }
                // Aba de navegação topo
                $('.navbar-default .navbar-nav li a').css({'background-color': '#DEEBF0'})
                $('.navbar-default .navbar-nav li.active a').css({'background-color': '#64C1C7'})
                // Footer
                $('.footer').css({'background-color': '#64C1C7'})
            }else{
                // Aba de navegação topo
                $('.navbar-default .navbar-nav li a').css({'background-color': '#DEEBF0'})
                $('.navbar-default .navbar-nav li.active a').css({'background-color': '#64C1C7'})

            }
        }
    })
})