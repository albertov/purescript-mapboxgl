exports.pairsImpl = function (Just) {
  return function (Nothing) {
    return function (Tuple) {
      return function (input) {
        if (input.length % 2 == 0) {
          var ret = [];
          for (var i=0; i<input.length; i=i+2) {
            ret.push(Tuple(input[i])(input[i+1]));
          }
          return Just(ret)
        } else {
          return Nothing;
        }
      }
    }
  }
}