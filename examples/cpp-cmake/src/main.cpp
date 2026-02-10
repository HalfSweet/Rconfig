#include <iostream>

#include "bar/bar.h"
#include "baz/baz.h"
#include "config.h"
#include "foo/foo.h"
#include "qux/qux.h"

namespace {

void print_app_view() {
#if defined(CONFIG_APP_ENABLED) && CONFIG_APP_ENABLED
  std::cout << "app.enabled=1\n";
#else
  std::cout << "app.enabled=0\n";
#endif

#if defined(CONFIG_APP_CHANNEL_DEV) && CONFIG_APP_CHANNEL_DEV
  std::cout << "app.channel=dev\n";
#elif defined(CONFIG_APP_CHANNEL_STAGE) && CONFIG_APP_CHANNEL_STAGE
  std::cout << "app.channel=stage\n";
#elif defined(CONFIG_APP_CHANNEL_PROD) && CONFIG_APP_CHANNEL_PROD
  std::cout << "app.channel=prod\n";
#else
  std::cout << "app.channel=unknown\n";
#endif

#ifdef CONFIG_APP_TARGET
  std::cout << "app.target=" << CONFIG_APP_TARGET << "\n";
#endif

#ifdef CONFIG_APP_RELEASE_NAME
  std::cout << "app.release_name=" << CONFIG_APP_RELEASE_NAME << "\n";
#endif

#ifdef CONFIG_APP_WORKERS
  std::cout << "app.workers=" << CONFIG_APP_WORKERS << "\n";
#endif

#ifdef CONFIG_APP_LANE
  std::cout << "app.lane=" << CONFIG_APP_LANE << "\n";
#endif

#ifdef CONFIG_APP_OUT_FORMAT
  std::cout << "app.out_format=" << CONFIG_APP_OUT_FORMAT << "\n";
#endif

#if defined(CONFIG_APP_WIN_CONSOLE) && CONFIG_APP_WIN_CONSOLE
  std::cout << "platform=windows-path\n";
#endif

#if defined(CONFIG_APP_POSIX_SIGNALS) && CONFIG_APP_POSIX_SIGNALS
  std::cout << "platform=posix-path\n";
#endif
}

} // namespace

int main() {
  std::cout << "=== rcfg cpp/cmake demo ===\n";
  std::cout << "profile=" << RCFG_PROFILE << "\n";

  print_app_view();

  std::cout << "baz.summary=" << demo::baz::summary() << "\n";
  std::cout << "qux.summary=" << demo::qux::summary() << "\n";
  std::cout << "foo.summary=" << demo::foo::summary() << "\n";
  std::cout << "bar.summary=" << demo::bar::summary() << "\n";

  const bool fast_path_ready = demo::foo::supports_fast_path() && demo::bar::ready_for_prod();
  std::cout << "pipeline.fast_path=" << (fast_path_ready ? "enabled" : "disabled") << "\n";

  return 0;
}
