$(document).ready(function () {
  $("#q").focus();
  PkgSite.getJSON("search-completions", function (searchCompletions) {
    searchCompletions.sort();
    PkgSite.multiTermComplete(PkgSite.preventTabMovingDuringSelection($("#q")), searchCompletions);
  });
});
