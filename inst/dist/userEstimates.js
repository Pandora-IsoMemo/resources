$(function(){
    $(".calcUserEstimate-button").click(function(){
        if ($(this).text() == "D")
            userEstimateDeleteChar();
        else if ($(this).text() == "E")
            userEstimateEmpty();
        else userEstimateAppend($(this).text());
    });

    $(".calculatorInputContainerUserEstimate select").dblclick(function(e){
        var char = "[" + $(e.target).val() + "]";

        userEstimateAppend(char);
    });

    $("#addUserEstimateConstant").click(function(){
        var char = "~C(" + $("#userEstimateConstant").val() + ")~";

        userEstimateAppend(char);

        $("#userEstimateConstant").val("");
    });

    $("#addUserEstimateND").click(function(){
        var char = "~N(" + $("#userEstimateNDMean").val() + "," + $("#userEstimateNDSd").val() + ")~";

        userEstimateAppend(char);

        $("#userEstimateNDMean").val("");
        $("#userEstimateNDSd").val("");
    });
});

function userEstimateAppend(char){
    $("#fruits-newUserEstimate").val($("#fruits-newUserEstimate").val() + char);
    $("#fruits-newUserEstimate").trigger("change");
}

function userEstimateDeleteChar(){
    var current = $("#fruits-newUserEstimate").val();

    if (current.length == 0) return;

    if (current.slice(-1) == "]"){
        var value = current.split("[").slice(0, -1).join("[");
    } else {
        var value = current.slice(0, -1);
    }

    $("#fruits-newUserEstimate").val(value);
    $("#fruits-newUserEstimate").trigger("change");
}

function userEstimateEmpty(){
    $("#fruits-newUserEstimate").val("");

    $("#fruits-newUserEstimate").trigger("change");
}
