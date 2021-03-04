$(document).on('shiny:busy', function() {
  var $inputs = $('button,input,slider,btn,selectize-input,a');
console.log($inputs);
$inputs.prop('disabled', true);
});

$(document).on('shiny:idle', function() {
var $inputs = $('button,input,slider,btn,selectize-input,a');
console.log($inputs);
$inputs.prop('disabled', false);
})