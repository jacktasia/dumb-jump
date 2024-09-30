let funcName1 = (foo) => "bar";

const funcName2 = (foo) => "bar";

const funcName3 = (foo) => {
  return "bar";
}

const obj = {
  funcName4 : (foo) => {
    return "bar";
  }
}

const obj = {
  funcName5(foo) {
    return "bar";
 }
}

funcName1();
funcName2();
funcName3();
funcName4();
funcName5();


class MyClass {

    constructor(a, b) {
        this.a = a;
        this.b = b;
    }
}


var b = new MyClass();
