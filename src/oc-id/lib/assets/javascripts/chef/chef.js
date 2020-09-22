//= require foundation/foundation
//= require foundation

(function($, window, document) {

  window.Chef = window.Chef || {};
  window.Chef.Web = window.Chef.Web || {};

  (function() {
    var self;

    Chef.Web.Core = {
      
      init: function(doc, opts) {
        self = this;

        self.options = opts || {
          assets: {
            images: '/assets'
          }
        };
        
        for (var name in self.components) {
          self.components[name].init();
        }
      },

      components: {},
      
      imageUrl: function(asset) {
        return [self.options.assets.images, asset].join('/');
      }
    };
  })();

  $.fn.chef = function() {
    $(document).foundation.apply(this, arguments);

    var args = Array.prototype.slice.call(arguments, 0);

    return this.each(function() {
      Chef.Web.Core.init.apply(Chef.Web.Core, [this].concat(args));
      return this;
    });
  };

})(jQuery, window, window.document);
