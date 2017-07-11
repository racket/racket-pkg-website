$(function() {
  "use strict";

  function applyFilter() {
    $("table.packages > tbody > tr").each(function() {
      var row = this;
      if (Number.parseInt($(row).data("todokey"), 10) === 0) {
        row.style.display = "none";
      }
    });
    $("table.packages > thead > tr > :nth-child(5)").trigger("click");
  }

  function removeFilter() {
    $("table.packages > tbody > tr").each(function() {
      var row = this;
      if (Number.parseInt($(row).data("todokey"), 10) === 0) {
        row.style.display = "";
      }
    });
    $("table.packages > thead > tr > :nth-child(2)").trigger("click");
  }

  var todoTotal = $("table.packages").data("todokey");

  if (todoTotal > 0) {
    $("#todo-msg").html(
      todoTotal + " todos. " + 
      "<a style='cursor:pointer' id='filter-pkgs'> Click here to see them.</a>"
    );

    var filterIsApplied = false;

    $("#filter-pkgs").click(function() {
      var filterLink = $(this);
      if (!filterIsApplied) {
        applyFilter();
        filterLink.text("Click to see all packages.");
        filterIsApplied = true;
      } else {
        removeFilter();
        filterLink.text("Click here to see them.");
        filterIsApplied = false;
      }
    });
  }

}); /* document.ready */
