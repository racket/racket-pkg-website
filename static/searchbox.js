$(document).ready(function () {
  $("#q").focus();
  $.getJSON("/json/search-completions", function (searchCompletions) {
    searchCompletions.sort();
    PkgSite.multiTermComplete(PkgSite.preventTabMovingDuringSelection($("#q")), searchCompletions);
  });
});
