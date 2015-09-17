#ifndef PARSER_HH_
#define PARSER_HH_

#include <memory>
#include <vector>
#include <string>

namespace cppbrowser {
  class Parser_impl;

  class Parser
  {
  public:
    Parser();
    virtual ~Parser();

    void add_file(std::string &&file);
    void add_incdir(std::string &&dir);

    int parse_all();

  private:
    std::unique_ptr<Parser_impl> impl;
  };
}

#endif
