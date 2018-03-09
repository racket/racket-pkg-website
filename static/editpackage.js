function control(e, name) {
  // Use getElementById here because there are dots (!) in the ID
  // strings, and if we were to use jquery, it would interpret those
  // as class separators.
  return $(document.getElementById("version__" + e.dataset.packageversion + "__" + name));
}

// Update control visibility for a particular package version source
// control group when its type selector changes value.
//
function preenSourceType(e) {
  function showhide1(n, v) {
    var c = control(e, n + "__group");
    if (v) {
      c.show();
    } else {
      c.hide();
    }
    return control(e, n).val();
  }
  function showhide(s, gt, gh, gr, gc, gp) {
    return [showhide1("simple_url", s),
	    showhide1("g_transport", gt),
	    showhide1("g_host_port", gh),
	    showhide1("g_repo", gr),
	    showhide1("g_commit", gc),
	    showhide1("g_path", gp)];
  }
  var pieces;
  var previewUrl;
  var previewGroup = control(e, "urlpreview__group");
  var previewInput = control(e, "urlpreview");
  switch (e.value) {
    case "git":
      previewGroup.show();
      pieces = showhide(false, true, true, true, true, true);
      previewUrl = "https" + "://" + pieces[2] + "/" + pieces[3] +
        (pieces[5] ? "?path=" + pieces[5] : "") +
        (pieces[4] && (pieces[4] !== 'master') ? '#' + pieces[4] : "");
      break;
    case "simple":
    default:
      previewGroup.hide();
      pieces = showhide(true, false, false, false, false, false);
      previewUrl = pieces[0];
      break;
  }
  previewInput.html("").append(document.createTextNode(previewUrl));
}

$(document).ready(function () {
  // Stop the enter key from submitting the form using a random submit
  // button (there is no sensible default to choose; or rather, the
  // default varies with location).
  //
  // We could come back to this later and make enter do the
  // contextually-appropriate thing, perhaps, but I think it's not
  // much of a win at the moment.
  //
  $('#edit-package-form').bind("keyup keypress", function(e) {
    if (e.which == 13 && document.activeElement.tagName !== "TEXTAREA") {
      e.preventDefault();
      return false;
    }
  });

  // Start monitoring package version source type selectors for
  // changes, and do the initial cleanup of the form fields.
  //
  $(".package-version-source-type").each(function (index, e) {
    var preenE = function () { preenSourceType(e); };
    $(e).change(preenE);
    var names = ['simple_url', 'g_transport', 'g_host_port', 'g_repo', 'g_commit', 'g_path'];
    for (var i = 0; i < names.length; i++) {
      control(e, names[i]).change(preenE).keyup(preenE);
    }
    preenSourceType(e);
  });
});
