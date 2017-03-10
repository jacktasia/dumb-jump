class Foo {
public:
  int bar(int val) {
    return val;
  }
};

int main() {
  return Foo::bar(42);
}
