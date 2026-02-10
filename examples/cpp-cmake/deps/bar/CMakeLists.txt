add_library(bar_cfg STATIC src/bar.cpp)
add_dependencies(bar_cfg rcfg_generate)

target_include_directories(bar_cfg
  PUBLIC
    "${CMAKE_CURRENT_SOURCE_DIR}/include"
  PRIVATE
    "${RCFG_GEN_DIR}"
)
