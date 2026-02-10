#pragma once

#include <string>

namespace demo::bar {

bool enabled();
std::string flavor();
unsigned queue_depth();
std::string endpoint();
bool ready_for_prod();
std::string summary();

} // namespace demo::bar
