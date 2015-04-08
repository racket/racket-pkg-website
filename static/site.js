PkgSite = (function () {
  function preventTabMovingDuringSelection(x) {
      return x.bind("keydown", function (e) {
	if (e.which === $.ui.keyCode.TAB && $(this).autocomplete("instance").menu.active) {
	  e.preventDefault();
	}
      });
  }

  function multiTermComplete(x, completions) {
    return x.autocomplete({
      source: function (req, resp) {
	resp($.ui.autocomplete.filter(completions, req.term.split(/\s+/).pop()));
      },
      focus: function () {
	return false;
      },
      select: function (event, ui) {
	var terms = this.value.split(/\s+/);
	terms.pop();
	terms.push(ui.item.value);
	this.value = terms.join(" ") + " ";
	return false;
      }
    });
  }

  function getJSON(relative_url, k) {
    return $.getJSON(PkgSiteDynamicBaseUrl + '/json/' + relative_url, k);
  }

  return {
    multiTermComplete: multiTermComplete,
    preventTabMovingDuringSelection: preventTabMovingDuringSelection,
    getJSON: getJSON
  };
})();

$(document).ready(function () {
  $("table.sortable").tablesorter();

  if ($("#tags").length) {
    PkgSite.getJSON((document.body.className === "package-form")
	            ? "formal-tags"
	            : "tag-search-completions",
		    function (completions) {
		      completions.sort();
		      PkgSite.multiTermComplete(PkgSite.preventTabMovingDuringSelection($("#tags")),
						completions);
		    });
  }
});
