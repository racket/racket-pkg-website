$(document).ready(function () {
  PkgSite.staticJSON("search-completions", function (searchCompletions) {
    searchCompletions.sort();
    PkgSite.multiTermComplete(PkgSite.preventTabMovingDuringSelection($("#q")), searchCompletions);
  });
});
