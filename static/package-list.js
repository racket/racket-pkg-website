// Functions related to package listings as produced by `package-summary-table` in site.rkt

function toggleBulkOperationSelections() {
  var checkboxes = Array.from(document.querySelectorAll("input.selected-packages"));
  var anySelected = checkboxes.some(function (n) { return n.checked; });
  var newState = anySelected ? false : true;
  checkboxes.forEach(function (n) { n.checked = newState; });
}

var language_family = "Racket"
var language_families = false
var language_family_elems = false

function on_language_family() {
  if (!language_families) {
    language_families = []
    language_family_elems = []
    document.querySelectorAll('#language-family-list .family-select').forEach(elem => {
      language_families.push(elem.dataset.family)
      language_family_elems.push(elem)
    })
  }
  const i = language_families.indexOf(language_family);
  if (i > 0) {
    language_families.unshift(language_families.splice(i, 1)[0]);
    language_family_elems.unshift(language_family_elems.splice(i, 1)[0]);
    const dest = document.querySelector("#language-family-list");
    const elems = []
    for (const fam of language_family_elems) {
      if (elems.length > 0) {
        const span = document.createElement("span");
        span.textContent = ", ";
        elems.push(span);
      }
      elems.push(fam);
    }
    dest.replaceChildren(...elems);
  }
  
  document.querySelectorAll('[data-families]').forEach(row => {
    const families = row.dataset.families.split(",");
    if (families.includes(language_family)) {
      row.style.backgroundColor = "";
    } else {
      row.style.backgroundColor = "whitesmoke";
    }
  })

  $("#package-table").trigger("sortReset");
  const tbody = document.querySelector("#package-table tbody");
  const rows = [...tbody.querySelectorAll("tr")];

  rows.sort((a, b) => {
    const a_fams = a.dataset.families.split(",");
    const b_fams = b.dataset.families.split(",");

    function best_match(fams) {
      best = language_families.length
      for (const fam of fams) {
        const i = language_families.indexOf(fam)
        if (i > -1 && i < best) {
          best = i;
        }        
      }
      return best;
    }

    const a_pos = best_match(a_fams);
    const b_pos = best_match(b_fams);

    if (a_pos != b_pos)
      return a_pos - b_pos;
    return parseInt(a.dataset.sortpos) - parseInt(b.dataset.sortpos);
  });
  
  rows.forEach(row => tbody.appendChild(row));

  document.querySelectorAll('.family-select').forEach(elem => {
    if (elem.dataset.family == language_family) {
      const span = document.createElement("span");
      span.textContent = language_family;
      elem.replaceChildren(span);
    } else {
      const link = document.createElement("a");
      link.href = "#"; // makes it look/behave like a link
      link.textContent = elem.dataset.family;
      link.addEventListener("click", (e) => {
        e.preventDefault(); // stop the # from scrolling/navigating
        language_family = elem.dataset.family;
        on_language_family();
      });
      elem.replaceChildren(link);
    }
  })
}

$(function() {
  "use strict";

  var query_family = (new URLSearchParams(window.location.search)).get("langfam");
  if (query_family) {
    language_family = query_family;
  }

  function applyFilter() {
    $("table.packages > tbody > tr").each(function() {
      var row = this;
      if (Number.parseInt($(row).data("todokey"), 10) === 0) {
        row.style.display = "none";
      }
    });
    $("table.packages").trigger("sorton", [[[4, 1]]]);
  }

  function removeFilter() {
    $("table.packages > tbody > tr").each(function() {
      var row = this;
      if (Number.parseInt($(row).data("todokey"), 10) === 0) {
        row.style.display = "";
      }
    });
    $("table.packages").trigger("sorton", [[[1, 0]]]);
  }

  var todoTotal = $("table.packages").data("todokey");

  if (todoTotal > 0) {
    $("#todo-msg").show();
    $("#todo-msg").html(
      todoTotal + " todos. " +
      "<a style='cursor:pointer' id='filter-pkgs'> See package with todos.</a>"
    );

    var filterIsApplied = false;

    $("#filter-pkgs").click(function() {
      var filterLink = $(this);
      if (!filterIsApplied) {
        applyFilter();
        filterLink.text("See all packages.");
        filterIsApplied = true;
      } else {
        removeFilter();
        filterLink.text("See packages with todos.");
        filterIsApplied = false;
      }
    });
  } else {
    $("#todo-msg").hide();
  }

  document.querySelectorAll('.umbrella-arrow').forEach(arrow => {
    arrow.addEventListener('click', () => {
      const isOpening = arrow.classList.contains("umbrella-closed");

      if (isOpening) {
        arrow.classList.remove('umbrella-closed');
        arrow.classList.add('umbrella-open');
        document.querySelectorAll('.umbrella-content').forEach(
          function (c) {
            if (c.dataset.umbrella == arrow.dataset.umbrella) {
              c.classList.remove('in-closed-umbrella')
              c.classList.add('in-open-umbrella')
            }
          }
        );
      } else {
        arrow.classList.remove('umbrella-open');
        arrow.classList.add('umbrella-closed');
        document.querySelectorAll('.umbrella-content').forEach(
          function (c) {
            if (c.dataset.umbrella == arrow.dataset.umbrella) {
              c.classList.remove('in-open-umbrella')
              c.classList.add('in-closed-umbrella')
            }
          }
        );
      }
    });
  });

  on_language_family();
  
}); /* document.ready */
