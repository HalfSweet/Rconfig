#pragma once

#include <string>

namespace demo::foo {

bool enabled();
std::string mode();
unsigned timeout_ms();
unsigned retries();
std::string lane();
std::string format();
bool supports_fast_path();
std::string summary();

} // namespace demo::foo
