#include "bar/bar.h"

#include "config.h"

namespace demo::bar {

bool enabled() {
#if defined(CONFIG_BAR_ENABLED)
  return CONFIG_BAR_ENABLED != 0;
#else
  return false;
#endif
}

std::string flavor() {
#if defined(CONFIG_BAR_FLAVOR_TINY) && CONFIG_BAR_FLAVOR_TINY
  return "tiny";
#elif defined(CONFIG_BAR_FLAVOR_FULL) && CONFIG_BAR_FLAVOR_FULL
  return "full";
#else
  return "unknown";
#endif
}

unsigned queue_depth() {
#if defined(CONFIG_BAR_QUEUE_DEPTH)
  return static_cast<unsigned>(CONFIG_BAR_QUEUE_DEPTH);
#else
  return 0;
#endif
}

std::string endpoint() {
#ifdef CONFIG_BAR_ENDPOINT
  return CONFIG_BAR_ENDPOINT;
#else
  return "unset";
#endif
}

bool ready_for_prod() {
  return enabled() && flavor() == "full" && queue_depth() >= 64;
}

std::string summary() {
  return "enabled=" + std::string(enabled() ? "1" : "0") +
         ",flavor=" + flavor() + ",queue_depth=" +
         std::to_string(queue_depth()) + ",endpoint=" + endpoint();
}

} // namespace demo::bar
