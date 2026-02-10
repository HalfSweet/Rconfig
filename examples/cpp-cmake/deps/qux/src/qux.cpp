#include "qux/qux.h"

#include "config.h"

namespace demo::qux {

std::string format() {
#if defined(CONFIG_QUX_FORMAT_JSON) && CONFIG_QUX_FORMAT_JSON
  return "json";
#elif defined(CONFIG_QUX_FORMAT_TEXT) && CONFIG_QUX_FORMAT_TEXT
  return "text";
#elif defined(CONFIG_QUX_FORMAT_BINARY) && CONFIG_QUX_FORMAT_BINARY
  return "binary";
#else
  return "unknown";
#endif
}

std::string env_label() {
#ifdef CONFIG_QUX_ENV_LABEL
  return CONFIG_QUX_ENV_LABEL;
#else
  return "unset";
#endif
}

unsigned shard_count() {
#if defined(CONFIG_QUX_SHARD_COUNT)
  return static_cast<unsigned>(CONFIG_QUX_SHARD_COUNT);
#else
  return 0;
#endif
}

std::string summary() {
  return "format=" + format() + ",env=" + env_label() +
         ",shards=" + std::to_string(shard_count());
}

} // namespace demo::qux
