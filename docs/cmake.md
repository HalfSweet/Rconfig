# CMake 集成示例

下面示例使用 `add_custom_command` 在构建前调用 `rcfg export`，并把产物接入编译系统。

## 目录约定

```text
project/
  CMakeLists.txt
  config/
    schema.rcfg
    profile.rcfgv
```

## `add_custom_command` 示例

```cmake
cmake_minimum_required(VERSION 3.20)
project(app C)

find_program(RCFG_BIN rcfg REQUIRED)

set(RCFG_SCHEMA "${CMAKE_SOURCE_DIR}/config/schema.rcfg")
set(RCFG_VALUES "${CMAKE_SOURCE_DIR}/config/profile.rcfgv")
set(RCFG_OUT_H "${CMAKE_BINARY_DIR}/generated/config.h")
set(RCFG_OUT_CMAKE "${CMAKE_BINARY_DIR}/generated/config.cmake")

add_custom_command(
  OUTPUT "${RCFG_OUT_H}" "${RCFG_OUT_CMAKE}"
  COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_BINARY_DIR}/generated"
  COMMAND ${RCFG_BIN}
          export
          --schema "${RCFG_SCHEMA}"
          --values "${RCFG_VALUES}"
          --out-h "${RCFG_OUT_H}"
          --out-cmake "${RCFG_OUT_CMAKE}"
  DEPENDS "${RCFG_SCHEMA}" "${RCFG_VALUES}"
  COMMENT "Generating config.h/config.cmake from Rconfig"
  VERBATIM
)

add_custom_target(rcfg_generate ALL
  DEPENDS "${RCFG_OUT_H}" "${RCFG_OUT_CMAKE}"
)

add_executable(app src/main.c)
add_dependencies(app rcfg_generate)
target_include_directories(app PRIVATE "${CMAKE_BINARY_DIR}/generated")

include("${RCFG_OUT_CMAKE}")
message(STATUS "CFG_APP_ENABLED=${CFG_APP_ENABLED}")
```

## 使用建议

- 将 `schema.rcfg` 和 `profile.rcfgv` 都放进 `DEPENDS`，保证输入变更后自动重新导出。
- 使用构建目录下的 `generated/` 存放导出文件，避免污染源码目录。
- 通过 `include(config.cmake)` 把导出值传入 CMake 选项或 `target_compile_definitions`。
