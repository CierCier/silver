#include "agc/diagnostics.hpp"

namespace agc {

// ANSI colors
static const char* RESET = "\033[0m";
static const char* RED = "\033[31m";
static const char* YELLOW = "\033[33m";
static const char* CYAN = "\033[36m";
static const char* WHITE = "\033[37m";
static const char* BOLD = "\033[1m";

void DiagnosticEngine::report(DiagLevel level, const DiagLoc& loc, const std::string& msg) {
    if (level == DiagLevel::Note && !verbose_) return;

    std::ostream& out = (level == DiagLevel::Error || level == DiagLevel::Fatal) ? std::cerr : std::cout;

    if (!loc.file.empty()) {
        out << BOLD << loc.file;
        if (loc.line > 0) out << ":" << loc.line;
        if (loc.col > 0) out << ":" << loc.col;
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
    }

    out << BOLD << msg << RESET << "\n";
}

void DiagnosticEngine::report(DiagLevel level, const std::string& msg) {
    report(level, DiagLoc{}, msg);
}

} // namespace agc
