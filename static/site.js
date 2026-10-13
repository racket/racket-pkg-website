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

  function dynamicJSON(relative_url, k) {
    return $.getJSON(PkgSiteDynamicBaseUrl + '/json/' + relative_url, k);
  }

  function staticJSON(relative_url, k) {
    return $.getJSON((IsStaticPage ? PkgSiteStaticBaseUrl : PkgSiteDynamicBaseUrl)
		     + '/json/' + relative_url, k);
  }

  return {
    multiTermComplete: multiTermComplete,
    preventTabMovingDuringSelection: preventTabMovingDuringSelection,
    dynamicJSON: dynamicJSON,
    staticJSON: staticJSON
  };
})();

$(document).ready(function () {
  $("table.sortable").tablesorter({
    widgets: ['children'],
    widgetOptions: {
      children_attach: true   // keeps child rows with their parent during sort
    }
  })

  if ($("#tags").length) {
    PkgSite.dynamicJSON((document.body.className === "package-form")
			? "formal-tags"
			: "tag-search-completions",
			function (completions) {
			  completions.sort();
			  PkgSite.multiTermComplete(
			    PkgSite.preventTabMovingDuringSelection($("#tags")),
			    completions);
			});
  }

});

// The server renders timestamps in UTC. <time datetime="..."> elements
// carry the exact ISO 8601 timestamp, so convert the displayed text to
// the viewer's local time zone (issue #43).
$(function () {
  "use strict";
  document.querySelectorAll("time[datetime]").forEach(function (el) {
    var d = new Date(el.getAttribute("datetime"));
    if (isNaN(d.getTime())) return;
    try {
      el.textContent = d.toLocaleString(undefined, { dateStyle: "medium", timeStyle: "short" });
    } catch (e) {
      // Very old browsers without option support: keep the UTC text.
    }
  });
});
