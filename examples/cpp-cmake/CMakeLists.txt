cmake_minimum_required(VERSION 3.20)
project(rcfg_cpp_cmake_demo LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(RCFG_PROFILE "dev" CACHE STRING "Rconfig profile (dev|prod)")
set_property(CACHE RCFG_PROFILE PROPERTY STRINGS dev prod)

if(NOT RCFG_BIN)
  find_program(RCFG_BIN rcfg REQUIRED)
endif()

set(RCFG_ROOT "${CMAKE_CURRENT_SOURCE_DIR}")
set(RCFG_MANIFEST "${RCFG_ROOT}/Config.toml")
set(RCFG_VALUES "${RCFG_ROOT}/profiles/${RCFG_PROFILE}.rcfgv")
set(RCFG_CONTEXT "${RCFG_ROOT}/context/${RCFG_PROFILE}.json")
set(RCFG_GEN_DIR "${CMAKE_BINARY_DIR}/generated")
set(RCFG_OUT_H "${RCFG_GEN_DIR}/config.h")
set(RCFG_OUT_CMAKE "${RCFG_GEN_DIR}/config.cmake")

if(NOT EXISTS "${RCFG_VALUES}")
  message(FATAL_ERROR "Unknown RCFG_PROFILE=${RCFG_PROFILE}; expected dev or prod")
endif()

if(NOT EXISTS "${RCFG_CONTEXT}")
  message(FATAL_ERROR "Missing context file: ${RCFG_CONTEXT}")
endif()

set(RCFG_SCHEMA_FILES
  "${RCFG_ROOT}/src/schema.rcfg"
  "${RCFG_ROOT}/deps/foo/src/schema.rcfg"
  "${RCFG_ROOT}/deps/bar/src/schema.rcfg"
  "${RCFG_ROOT}/deps/baz/src/schema.rcfg"
  "${RCFG_ROOT}/deps/qux/src/schema.rcfg"
)

set(RCFG_MANIFEST_FILES
  "${RCFG_ROOT}/Config.toml"
  "${RCFG_ROOT}/deps/foo/Config.toml"
  "${RCFG_ROOT}/deps/bar/Config.toml"
  "${RCFG_ROOT}/deps/baz/Config.toml"
  "${RCFG_ROOT}/deps/qux/Config.toml"
)

add_custom_command(
  OUTPUT "${RCFG_OUT_H}" "${RCFG_OUT_CMAKE}"
  COMMAND ${CMAKE_COMMAND} -E make_directory "${RCFG_GEN_DIR}"
  COMMAND ${RCFG_BIN}
          export
          --manifest "${RCFG_MANIFEST}"
          --values "${RCFG_VALUES}"
          --context "${RCFG_CONTEXT}"
          --out-h "${RCFG_OUT_H}"
          --out-cmake "${RCFG_OUT_CMAKE}"
          --export-name-rule path-only
  DEPENDS
          "${RCFG_VALUES}"
          "${RCFG_CONTEXT}"
          ${RCFG_SCHEMA_FILES}
          ${RCFG_MANIFEST_FILES}
  COMMENT "Generating config.h/config.cmake from Rconfig (${RCFG_PROFILE})"
  VERBATIM
)

add_custom_target(rcfg_generate ALL
  DEPENDS "${RCFG_OUT_H}" "${RCFG_OUT_CMAKE}"
)

add_subdirectory(deps/baz)
add_subdirectory(deps/qux)
add_subdirectory(deps/foo)
add_subdirectory(deps/bar)

add_executable(demo_app src/main.cpp)
add_dependencies(demo_app rcfg_generate)
target_include_directories(demo_app PRIVATE "${RCFG_GEN_DIR}")
target_compile_definitions(demo_app PRIVATE "RCFG_PROFILE=\"${RCFG_PROFILE}\"")
target_link_libraries(demo_app PRIVATE foo_cfg bar_cfg baz_cfg qux_cfg)

include("${RCFG_OUT_CMAKE}" OPTIONAL)

message(STATUS "RCFG_BIN=${RCFG_BIN}")
message(STATUS "RCFG_PROFILE=${RCFG_PROFILE}")
if(EXISTS "${RCFG_OUT_CMAKE}")
  message(STATUS "CFG_APP_CHANNEL_PROD=${CFG_APP_CHANNEL_PROD}")
  message(STATUS "CFG_FOO_TIMEOUT_MS=${CFG_FOO_TIMEOUT_MS}")
  message(STATUS "CFG_BAR_QUEUE_DEPTH=${CFG_BAR_QUEUE_DEPTH}")
else()
  message(STATUS "CFG_* variables will be available after first build")
endif()
