add_library(qux_cfg STATIC src/qux.cpp)
add_dependencies(qux_cfg rcfg_generate)

target_include_directories(qux_cfg
  PUBLIC
    "${CMAKE_CURRENT_SOURCE_DIR}/include"
  PRIVATE
    "${RCFG_GEN_DIR}"
)
