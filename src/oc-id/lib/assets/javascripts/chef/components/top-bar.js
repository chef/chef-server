//= require ../chef

(function($, window) {
  'use strict';

  Chef.Web.Core.components.topBar = {
    init: function() {
      // Hide/show the nav items when the nav icon is clicked
      $(".top-bar .nav-icon").click(function (event) {
        $(event.target).siblings("ul").slideToggle()
      });

      // Ensure the menu items get put back to inline if the window gets bigger
      $(window).resize(function (event) {
        var display = "inline";
        if ($(".top-bar .nav-icon").is(":visible")) { display = "block"; }
        $(".top-bar ul").css("display", display);
      });
    }
  };
})(jQuery, window);
