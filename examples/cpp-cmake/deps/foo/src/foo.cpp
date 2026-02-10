#include "foo/foo.h"

#include "baz/baz.h"
#include "qux/qux.h"
#include "config.h"

namespace demo::foo {

bool enabled() {
#if defined(CONFIG_FOO_ENABLED)
  return CONFIG_FOO_ENABLED != 0;
#else
  return false;
#endif
}

std::string mode() {
#if defined(CONFIG_FOO_MODE_OFF) && CONFIG_FOO_MODE_OFF
  return "off";
#elif defined(CONFIG_FOO_MODE_BASIC) && CONFIG_FOO_MODE_BASIC
  return "basic";
#elif defined(CONFIG_FOO_MODE_PRO) && CONFIG_FOO_MODE_PRO
  return "pro";
#else
  return "unknown";
#endif
}

unsigned timeout_ms() {
#if defined(CONFIG_FOO_TIMEOUT_MS)
  return static_cast<unsigned>(CONFIG_FOO_TIMEOUT_MS);
#else
  return 0;
#endif
}

unsigned retries() {
#if defined(CONFIG_FOO_RETRIES)
  return static_cast<unsigned>(CONFIG_FOO_RETRIES);
#else
  return 0;
#endif
}

std::string lane() {
#if defined(CONFIG_FOO_LANE_ALPHA) && CONFIG_FOO_LANE_ALPHA
  return "alpha";
#elif defined(CONFIG_FOO_LANE_BETA) && CONFIG_FOO_LANE_BETA
  return "beta";
#elif defined(CONFIG_FOO_LANE_GAMMA) && CONFIG_FOO_LANE_GAMMA
  return "gamma";
#else
  return "unknown";
#endif
}

std::string format() {
#if defined(CONFIG_FOO_FORMAT_JSON) && CONFIG_FOO_FORMAT_JSON
  return "json";
#elif defined(CONFIG_FOO_FORMAT_TEXT) && CONFIG_FOO_FORMAT_TEXT
  return "text";
#elif defined(CONFIG_FOO_FORMAT_BINARY) && CONFIG_FOO_FORMAT_BINARY
  return "binary";
#else
  return "unknown";
#endif
}

bool supports_fast_path() {
  return enabled() && mode() == "pro" && timeout_ms() >= 1500 &&
         format() == "binary" && demo::baz::strict_lane();
}

std::string summary() {
  return "enabled=" + std::string(enabled() ? "1" : "0") +
         ",mode=" + mode() + ",timeout_ms=" + std::to_string(timeout_ms()) +
         ",retries=" + std::to_string(retries()) + ",lane=" + lane() +
         ",format=" + format() + ",qux_env=" + demo::qux::env_label() +
         ",baz_batch_cap=" + std::to_string(demo::baz::max_batch());
}

} // namespace demo::foo
