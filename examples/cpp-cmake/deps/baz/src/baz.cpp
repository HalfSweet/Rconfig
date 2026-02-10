#include "baz/baz.h"

#include "config.h"

namespace demo::baz {

std::string default_lane() {
#if defined(CONFIG_BAZ_DEFAULT_LANE_ALPHA) && CONFIG_BAZ_DEFAULT_LANE_ALPHA
  return "alpha";
#elif defined(CONFIG_BAZ_DEFAULT_LANE_BETA) && CONFIG_BAZ_DEFAULT_LANE_BETA
  return "beta";
#elif defined(CONFIG_BAZ_DEFAULT_LANE_GAMMA) && CONFIG_BAZ_DEFAULT_LANE_GAMMA
  return "gamma";
#else
  return "unknown";
#endif
}

bool strict_lane() {
#if defined(CONFIG_BAZ_STRICT_LANE)
  return CONFIG_BAZ_STRICT_LANE != 0;
#else
  return false;
#endif
}

unsigned max_batch() {
#if defined(CONFIG_BAZ_MAX_BATCH)
  return static_cast<unsigned>(CONFIG_BAZ_MAX_BATCH);
#else
  return 0;
#endif
}

std::string summary() {
  return "lane=" + default_lane() + ",strict=" + (strict_lane() ? "on" : "off") +
         ",max_batch=" + std::to_string(max_batch());
}

} // namespace demo::baz
