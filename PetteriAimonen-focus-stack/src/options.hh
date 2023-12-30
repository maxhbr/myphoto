// Command line option parsing
// This is quite basic, just included to avoid adding extra dependencies.
// Doesn't expect flag arguments to begin with --, use e.g. ./--weird-filename to escape them.

#pragma once
#include <string>
#include <vector>

namespace focusstack {

class Options
{
public:
  Options(int argc, const char *argv[]);

  // Example: has_flag("--verbose") returns true if that flag exists
  bool has_flag(std::string name);

  // Example: with "--output=file.jpg", then get_arg("--output") returns "file.jpg"
  std::string get_arg(std::string name, std::string defval = "");

  // Returns all arguments that don't begin with --
  std::vector<std::string> get_filenames();

  // Returns all arguments that were not parsed
  std::vector<std::string> get_unparsed();

private:
  std::vector<std::string> m_options;
  std::vector<bool> m_parsed;
};

}
