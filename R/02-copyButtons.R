copyButton <- function(inputId, tableId) {
  tagList(
    singleton(tags$head(
      tags$script(src = "https://cdn.jsdelivr.net/npm/clipboard@2.0.8/dist/clipboard.min.js")
    )),
    actionButton(inputId, class = "btn btn-default", "Copy")
  )
}

pasteButton <- function(inputId, outputId, containerId) {
  tagList(
    tags$button(id = inputId, class = "btn btn-default", "Paste"),
    tags$script(HTML(
      paste0(
        "$('#",
        inputId,
        "').click(function (e) {
          navigator.permissions.query({name: 'clipboard-read'}).then(result => {
            if (result.state == 'granted' || result.state == 'prompt') {
              navigator.clipboard.readText().then(clipText => {
                Shiny.setInputValue('",
        outputId,
        "', {content: clipText, random: Math.random()})
              })
            }
          }).catch(() => {
             alert('Pasting with a click is not supported in your browser. Please click on the top-left of the table and use Ctrl + V on your keyboard.')
          });
        });

        $('#",
        containerId,
        "').on('paste', function (e) {
          if(e.target.nodeName == 'INPUT') return;

          let clipText = (event.clipboardData || window.clipboardData).getData('text');
          Shiny.setInputValue('",
        outputId,
        "', {content: clipText, random: Math.random()});
          e.preventDefault();
          return false;
        });
        "
      )
    ))
  )
}