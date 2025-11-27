#pragma once

#include <string>
#include <vector>
#include <iostream>

namespace agc {

enum class DiagLevel {
    Note,
    Warning,
    Error,
    Fatal
};

struct DiagLoc {
    std::string file;
    int line{0};
    int col{0};
};

class DiagnosticEngine {
public:
    DiagnosticEngine() = default;

    void setVerbose(bool v) { verbose_ = v; }
    bool isVerbose() const { return verbose_; }

    void report(DiagLevel level, const DiagLoc& loc, const std::string& msg);
    void report(DiagLevel level, const std::string& msg); // No loc

    bool hasErrors() const { return errorCount_ > 0; }
    int errorCount() const { return errorCount_; }

private:
    bool verbose_{false};
    int errorCount_{0};

    void printSnippet(std::ostream& out, const DiagLoc& loc);
};

} // namespace agc
