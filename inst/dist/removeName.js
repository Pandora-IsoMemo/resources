$(function(){
    $(".matrix-input").click(function(e){
        var parent = $(e.target).closest(".matrix-input");
        var rowHeader = $(e.target).parent().is(".matrix-input-row-header-cell");
        var colHeader = $(e.target).parent().is("div.matrix-input:not(.fixed-colnames) .matrix-input-col-header-cell");
        var empty = $(e.target).next().html() === "";

        if (!rowHeader & !colHeader) return;

        if (empty) return;

        if (!$(e.target).is("span.remove")) return;

        /* last name */
        if (colHeader) {
            var notEmpty = $(".matrix-input-col-header-1 .matrix-input-col-header-cell div", parent)
                .toArray()
                .map(v => $(v).html())
                .filter(v => v.trim() != "");

            if (notEmpty.length == 1) {
                alert("You cannot delete the last column;");
                return;
            }

        }
        if (rowHeader) {
            var notEmpty = $(".matrix-input-row-header-cell div", parent)
                .toArray()
                .map(v => $(v).html())
                .filter(v => v.trim() != "");

            if (notEmpty.length == 1) {
                alert("You cannot delete the last row;");
                return;
            }

        }


        var valueField = $(e.target).next();
        var value = $("input", valueField).val() ? $("input", valueField).val() : $(valueField).html();
        var dim = rowHeader ? "row" : "column";

        var which = Shiny.shinyapp.$inputValues["fruits-adaptiveNames"] ? "ALL tables" : "this table";
        var doIt = confirm("Do you really want to remove " + dim + " '" + value +
                           "' with values from " + which + "?");

        if (doIt == true) {
            var id = parent.attr("id");
            var obj = {id: id, value: value, dim: dim};

            console.log(obj);

            Shiny.setInputValue("fruits-removeName", obj);
        }
    });
});
