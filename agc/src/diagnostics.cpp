#include "agc/diagnostics.hpp"
#include <fstream>

namespace agc {

// ANSI colors
static const char *RESET = "\033[0m";
static const char *RED = "\033[31m";
static const char *YELLOW = "\033[33m";
static const char *CYAN = "\033[36m";
static const char *WHITE = "\033[37m";
static const char *BOLD = "\033[1m";

void DiagnosticEngine::report(DiagLevel level, const DiagLoc &loc,
                              const std::string &msg) {
  if (level == DiagLevel::Note && !verbose_)
    return;

  std::ostream &out = (level == DiagLevel::Error || level == DiagLevel::Fatal)
                          ? std::cerr
                          : std::cout;

  if (!loc.file.empty()) {
    out << BOLD << loc.file;
    if (loc.line > 0)
      out << ":" << loc.line;
    if (loc.col > 0)
      out << ":" << loc.col;
    out << ": " << RESET;
  }

  switch (level) {
  case DiagLevel::Note:
    out << CYAN << "note: " << RESET;
    break;
  case DiagLevel::Warning:
    out << YELLOW << "warning: " << RESET;
    break;
  case DiagLevel::Error:
    out << RED << "error: " << RESET;
    errorCount_++;
    break;
  case DiagLevel::Fatal:
    out << RED << "fatal error: " << RESET;
    errorCount_++;
    break;
  case DiagLevel::Debug:
    if (!verbose_)
      return;
    out << WHITE << "[DEBUG] " << RESET;
    break;
  }

  out << BOLD << msg << RESET << "\n";

  if (!loc.file.empty() && loc.line > 0) {
    printSnippet(out, loc);
  }
}

void DiagnosticEngine::printSnippet(std::ostream &out, const DiagLoc &loc) {
  std::ifstream file(loc.file);
  if (!file.is_open())
    return;

  std::string line;
  int currentLine = 1;
  while (std::getline(file, line)) {
    if (currentLine == loc.line) {
      // Print line number and content
      out << "  " << loc.line << " | " << line << "\n";

      // Print caret
      out << "  " << std::string(std::to_string(loc.line).length(), ' ')
          << " | ";
      if (loc.col > 0) {
        for (int i = 1; i < loc.col; ++i) {
          if (i < line.length() && line[i - 1] == '\t')
            out << '\t';
          else
            out << ' ';
        }
        out << BOLD << RED << "^" << RESET;
      }
      out << "\n";
      break;
    }
    currentLine++;
  }
}

void DiagnosticEngine::report(DiagLevel level, const std::string &msg) {
  report(level, DiagLoc{}, msg);
}

} // namespace agc
