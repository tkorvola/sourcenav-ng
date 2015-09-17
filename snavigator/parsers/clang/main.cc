extern "C" {
#include "snptools.h"
}

#include <utility>
#include <vector>
#include <string>
#include <cstdio>
#include <cstring>

#include "parser.hh"

using namespace std;
using namespace cppbrowser;

// This mimics sn_main as much as reasonable.
int
main(int argc, char **argv)
{
  sn_set_group("c++");
  sn_process_options(argc, argv);
  Parser parser;
  // How annoying.  I'd rather use ifstream.
  FILE *listfile = (FILE *)sn_getopt(SN_OPT_LISTFILE);
  if (listfile) {
    char fname[512];
    while (fgets(fname, sizeof(fname), listfile)) {
      char *lf = strchr(fname, '\n');
      if (lf)
	*lf = '\0';
      if (*fname && *fname != '#')
	parser.add_file(fname);
    }
  } else if (optind < argc)
    parser.add_file(argv[optind]);
  else {
    sn_parser_help();
    return 1;
  }
  for (const char *inc = sn_includepath_first();
       inc; inc = sn_includepath_next())
    parser.add_incdir(inc);
  sn_init();
  int res = parser.parse_all();
  sn_close();
  return res;
}
