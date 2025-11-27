#include "agc/driver.hpp"

int main(int argc, char **argv) {
  agc::CompilerDriver driver;
  return driver.run(argc, argv);
}
