$(function(){
    $(".calc-button").click(function(){
        if ($(this).text() == "D")
            priorDeleteChar();
        else if ($(this).text() == "E")
            priorEmpty();
        else priorAppend($(this).text());
    });

    $(".calculatorInputContainer select").dblclick(function(e){
        var char = "[" + $(e.target).val() + "]";

        priorAppend(char);
    });
});

function priorAppend(char){
    $("#fruits-newPrior").val($("#fruits-newPrior").val() + char);
    $("#fruits-newPrior").trigger("change");
}

function priorDeleteChar(){
    var current = $("#fruits-newPrior").val();

    if (current.length == 0) return;

    if (current.slice(-1) == "]"){
        var value = current.split("[").slice(0, -1).join("[");
    } else {
        var value = current.slice(0, -1);
    }

    $("#fruits-newPrior").val(value);
    $("#fruits-newPrior").trigger("change");
}

function priorEmpty(){
    $("#fruits-newPrior").val("");

    $("#fruits-newPrior").trigger("change");
}
