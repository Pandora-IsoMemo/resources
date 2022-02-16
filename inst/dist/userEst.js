$(function(){
  $(".calcUserEst-button").click(function(){
    if ($(this).text() == "D")
      userEstDeleteChar();
    else if ($(this).text() == "E")
      userEstEmpty();
    else userEstAppend($(this).text());
  });
  
  $(".calculatorInputContainerUserEst select").dblclick(function(e){
    var char = "[" + $(e.target).val() + "]";
    
    userEstAppend(char);
  });
});

function userEstAppend(char){
  $("#fruits-newUserEst").val($("#fruits-newUserEst").val() + char);
  $("#fruits-newUserEst").trigger("change");
}

function userEstDeleteChar(){
  var current = $("#fruits-newUserEst").val();
  
  if (current.length == 0) return;
  
  if (current.slice(-1) == "]"){
    var value = current.split("[").slice(0, -1).join("[");
  } else {
    var value = current.slice(0, -1);
  }
  
  $("#fruits-newUserEst").val(value);
  $("#fruits-newUserEst").trigger("change");
}

function userEstEmpty(){
  $("#fruits-newUserEst").val("");
  
  $("#fruits-newUserEst").trigger("change");
}
