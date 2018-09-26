function px(x) {
  if (typeof(x) === 'number')
    return x + 'px';
  else
    return x;
}

HTMLWidgets.widget({
  name: 'geovis_widget',
  type: 'output',
  factory: function(el, width, height) {
    // TODO: define shared variables for this instance
    return {
      renderValue: function(x) {
        var dv = document.createElement('div');
        dv.id = x.id;
        el.appendChild(dv);
        if (x.in_knitr) {
          el.style.marginTop = '30px';
          el.style.marginBottom = '30px';
        }

        geovisApp(x.id, width, height);

        // var scrpt = document.createElement('script');
        // scrpt.text= "(function() { geovisApp('" + x.id + "', " + width + ", " + height + "); })();";
        // el.appendChild(scrpt);
      },

      resize: function(width, height) {
        // TODO: code to re-render the widget with a new size
      }
    };
  }
});