#include "external.h"

int ExternalThing() {
  return 0;
}

class ExternalThing {
};

int main(int argc, char **argv) {
  ExternalThing et;
}
