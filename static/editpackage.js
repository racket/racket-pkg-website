function preenSourceTypes() {
  $(".package-version-source-type").each(function (index, e) {
    preenSourceType(e);
  });
}

function preenSourceType(e) {
  function controlId(name) {
    return "#version__" + e.dataset.packageversion + "__" + name;
  }
  function showhide1(n, v) {
    var c = $(controlId(n));
    if (v) {
      c.show();
    } else {
      c.hide();
    }
  }
  function showhide(s, gh, gu, gp, gb) {
    showhide1("simple_url", s);
    showhide1("g_host", gh);
    showhide1("g_user", gu);
    showhide1("g_project", gp);
    showhide1("g_branch", gb);
  }
  console.log(e.dataset.packageversion);
  switch (e.value) {
    case "github":
      showhide(false, false, true, true, true);
      break;
    case "git":
      showhide(false, true, true, true, true);
      break;
    case "simple":
    default:
      showhide(true, false, false, false, false);
      break;
  }
}

$(document).ready(function () {
  $(".package-version-source-type").each(function (index, e) {
    $(e).change(function () {
      preenSourceType(e);
    });
  });
  preenSourceTypes();
});
