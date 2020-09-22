//= require ../chef

(function($, window) {
  'use strict';

  $.fn.logo = function(action) {

    function toggleClasses(el) {
      el.toggleClass('animate', action === 'animate');
      el.toggleClass('deanimate', action === 'deanimate');
    }

    if (action === 'deanimate') {
      this.find('.mark g').one('webkitAnimationIteration MSAnimationIteration animationiteration', $.proxy(function() {
        toggleClasses(this);
      }, this));
    }
    else {
      toggleClasses(this);
    }
  };

  Chef.Web.Core.components.logo = {
    init: function() {

      $('.logo').each(function() {
        var $el = $(this);
        
        if ($el.find('> svg').length === 0) {
          $el.load(Chef.Web.Core.imageUrl('chef-logo.svg'), function() {
            var tag = $el.data('tag-line');
            if (tag) {
              var tag = this.querySelector('svg .tag-line text');
              if (tag) {
                tag.textContent = tag;
              }
            }
          });  
        }
      });
    },

    refresh: function() {
      $('.logo svg').remove();
      this.init();
    }
  };

})(jQuery, window);