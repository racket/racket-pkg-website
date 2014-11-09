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

  return {
    multiTermComplete: multiTermComplete,
    preventTabMovingDuringSelection: preventTabMovingDuringSelection
  };
})();

$(document).ready(function () {
  $("table.sortable").tablesorter();

  if ($("#tags").length) {
    $.getJSON("/json/tag-completions", function (tagCompletions) {
      tagCompletions.sort();
      PkgSite.multiTermComplete(PkgSite.preventTabMovingDuringSelection($("#tags")),
				tagCompletions);
    });
  }
});
