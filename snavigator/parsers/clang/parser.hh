#include <cstdio>

namespace cppbrowser {
  class parser
  {
  public:
    FILE *in;

    int parse();
    void reset();
  };
}
