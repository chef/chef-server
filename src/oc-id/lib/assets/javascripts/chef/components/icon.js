//= require ../chef

(function($, window) {
  'use strict';

  Chef.Web.Core.components.icon = {
    init: function() {

      var $icons = $('<div id="chef-icons"></div>');
      $('body').prepend($icons);

      $icons.load(Chef.Web.Core.imageUrl('icons/all.svg'), function() {
        $('i[class^=icon-]').each(function() {
          var $el = $(this);
          var className = $el.attr('class').split(' ')[0];

          $el.append($('<svg class="' + className + '"><use xlink:href="#' + className + '"></use></svg>'));

          if ($el.data('caption')) {
            $el.append($('<span></span>').html($el.data('caption'))).addClass('caption');
          }
        });
      });
    },
    refresh: function() {
      $('i[class^=icon-] svg, i[class^=icon-] span').remove();
      this.init();
    }
  };

})(jQuery, window);