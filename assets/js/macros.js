
remark.macros.scale = function (percentage) {
  var url = this;
  return '<img src="' + url + '" style="width: ' + percentage + '" />';
};



/* put an object on an exact spot on the slide */
/* ![:abs width, left, top](url) */
/* ![:abs 30%, 50%, 0%](url) */
remark.macros['abs'] = function(width="30%", left="85%", top="15%", cl="") {
var url = this;
return '<img src="' + url + '" style="position:absolute;left:' + left + ';top:' + top + ';width:' + width + '" class="' + cl + '" />';
};



/* roteren van een object, geef het aantal graden dat het met de klok mee moet worden gedraaid */
remark.macros.flip = function (degrees) {
  var url = this;
  return '<img src="' + url + '" style="transform: rotate(' + degrees + 'deg)" />';
};


/* alles in 1 */
remark.macros['abs2'] = function(width="30%", left="85%", top="15%", deg="0", cl="") {
var url = this;
return '<img src="' + url + '" style="position:absolute;left:' + left + ';top:' + top + ';width:' + width + '; transform: rotate(' + deg + 'deg)" class="' + cl + '" />';
};

