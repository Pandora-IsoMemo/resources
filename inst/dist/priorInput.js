var priorBinding = new Shiny.InputBinding();

function createPriorField(parentEl){
    var newPrior = $("<div><input class=\"form-control prior\" disabled = \"disabled\"><button class=\"btn btn-default\">X</button></div>");

    $("button", newPrior).click(function(){
        $(this).closest("div").remove();

        $(parentEl).trigger("change");
    });

    return(newPrior);
}

$.extend(priorBinding, {
    find: function(scope) {
        return $(scope).find(".prior");
    },
    getValue: function(el) {
        return $("input", el).map(function(){
            return $(this).val();
        }).toArray();
    },
    setValue: function(el, value) {
        if (!Array.isArray(value))
            value = [value];

        $(el).empty();

        for (i = 0; i < value.length; i ++){
            if ($(el).children().length < i + 1){
                // add element
                $(el).append(createPriorField(el));
            }

            $("input", $(el).children().eq(i)).val(value[i]);
        }
    },
    subscribe: function(el, callback) {
        $(el).on("change.priorBinding", function(e) {
            callback();
        });

        $("input", el).on("blur", function(e) {
            callback();
        });
    },
    unsubscribe: function(el) {
        $(el).off(".priorBinding");
    },
    receiveMessage: function(el, data) {
        if (data.hasOwnProperty('value'))
            this.setValue(el, data.value);

        $(el).trigger("change");
    }
});

Shiny.inputBindings.register(priorBinding);
