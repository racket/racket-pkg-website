$(document).ready(function () {
  // Get the search input element
  const searchInput = $("#q");
  
  // Function to filter packages based on search input
  function filterPackages() {
    const searchText = searchInput.val().toLowerCase();
    
    // Filter each row in the package table
    $("table.packages > tbody > tr").each(function() {
      const row = $(this);
      const packageName = row.find("td:nth-child(2) h2 a").text().toLowerCase();
      const description = row.find("td:nth-child(3) p:first-child").text().toLowerCase();
      const tags = row.find(".taglinks").text().toLowerCase();
      
      // Show row if the search text is found in the package name, description or tags
      if (searchText === "" || 
          packageName.includes(searchText) || 
          description.includes(searchText) || 
          tags.includes(searchText)) {
        row.show();
      } else {
        row.hide();
      }
    });
    
    // Update the package count
    const visibleRows = $("table.packages > tbody > tr:visible").length;
    $(".package-count:first").text(visibleRows + " packages" + (searchText ? " found" : ""));
  }
  
  // Add input event listener to search box for real-time filtering
  searchInput.on('input', filterPackages);
});
