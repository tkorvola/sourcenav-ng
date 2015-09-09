#ifndef PARSER_HH_
#define PARSER_HH_

#include <cstdio>
#include <memory>

namespace cppbrowser {
  class Parser_impl;

  class Parser
  {
  public:
    Parser();
    virtual ~Parser();

    int parse(const char *filename);
    void reset();

  private:
    std::unique_ptr<Parser_impl> impl;
  };
}

#endif
