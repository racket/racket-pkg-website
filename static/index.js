"use strict";

$(document).ready(function () {
  // "Cool URLs Don't Break" - catch uses of fragment-based links to
  // specific packages, and effectively redirect to the new-style
  // specific package URL.
  //
  var oldstyle_link = document.location.hash.match(/#\[(.*)\]$/);
  if (oldstyle_link) {
    var linked_package = oldstyle_link[1];
    document.location = document.location.pathname + 'package/' + linked_package;
  }
});
