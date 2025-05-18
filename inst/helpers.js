// Execute when the document is fully loaded
document.addEventListener('DOMContentLoaded', function() {

  // Hide the sidebar-toggle element
  const sidebarToggle = document.getElementsByClassName('sidebar-toggle')[0];
  sidebarToggle.style.visibility = 'hidden';
});

document.addEventListener("keydown", function(e) {
  Shiny.setInputValue("key_pressed", e.key, {priority: "event"});
});

document.body.classList.add('fixed');


// JavaScript function for rendering long text with a tooltip
function renderLongText(data, type, row, meta) {
  return type === 'display' && data.length > 30
    ? '<span title="' + data + '">' + data.substr(0, 30) + '...</span>'
    : data;
}

function datatableCallback() {
  return function(table) {
    table.rows().every(function(i, tab, row) {
      var $this = $(this.node());
      $this.attr('id', this.data()[0]);
      $this.addClass('shiny-input-container');
    });
    Shiny.unbindAll(table.table().node());
    Shiny.bindAll(table.table().node());
  };
}
