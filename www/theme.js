Shiny.addCustomMessageHandler("switch_theme", function(dark) {

    const theme = document.getElementById("theme-css");

    if (dark) {
        theme.href = "dark.css";
    } else {
        theme.href = "light.css";
    }

});