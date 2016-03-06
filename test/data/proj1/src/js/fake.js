'use strict';

function doSomeStuff() {
    alert('we did it!');
}

$scope.resized = function(path, w, h) {
  path = path.replace(':8081', ':9090');
  if (path.indexOf('.gif') !== -1) {
    return path;
  }
  return $scope.resizeHost + encodeURIComponent(path) + '?w=' + w + '&h=' + h;
};

someFunc();



//
console.log(backgroundColor);
