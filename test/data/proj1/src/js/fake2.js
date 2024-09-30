console.log(doSomeStuff());
// nothing
var someVar = 13,
    someVar2 = 14;

function workWithSomeVar(someVar) {
    var someVar2 = {a: 1, b: 2};
    console.log('someVar is', someVar);
    console.log('someVar is', someVar2);
}

function workWithSomeVar(blarg, someVar) {
    var someVar2 = 13;
    console.log('someVar is', someVar);
    console.log('someVar is', someVar2);
}


function workWithSomeVar(blarg, someVar, blah3) {
    var someVar2 = 13;
    console.log('someVar is', someVar);
    console.log('someVar is', someVar2);
}


var backgroundColor = '#222';
    if (isActive) {
      backgroundColor = 'darkgreen';
    } else if (canDrop) {
      backgroundColor = 'darkkhaki';
    }

console.log(backgroundColor);
