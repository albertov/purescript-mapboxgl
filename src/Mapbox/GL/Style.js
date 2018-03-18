// Copied from https://github.com/mapbox/mapbox-gl-js/blob/master/src/style-spec/deref.js
// to avoid incurring in a dependency
var refProperties = ['type', 'source', 'source-layer', 'minzoom', 'maxzoom', 'filter', 'layout'];

function deref(layer, parent) {
  var result = {};

  for (var k in layer) {
      if (k !== 'ref') {
          result[k] = layer[k];
      }
  }

  refProperties.forEach(function (k)  {
      if (k in parent) {
          result[k] = parent[k];
      }
  });

  return result;
}

exports.derefLayers = function (layers) {
  layers = layers.slice();

  var map = Object.create(null);
  for (var i = 0; i < layers.length; i++) {
      map[layers[i].id] = layers[i];
  }

  for (var i = 0; i < layers.length; i++) {
      if ('ref' in layers[i]) {
          layers[i] = deref(layers[i], map[layers[i].ref]);
      }
  }

  return layers;
}