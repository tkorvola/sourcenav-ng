extern "C" {
#include "snptools.h"
}

#include "parser.hh"

using namespace cppbrowser;

static parser pr;

static int
parse()
{
  return pr.parse();
}

static void
reset()
{
  pr.reset();
}

int
main(int argc, char **argv)
{
  return sn_main(argc, argv, "c++", &pr.in, parse, reset);
}
