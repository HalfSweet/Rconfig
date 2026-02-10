add_library(baz_cfg STATIC src/baz.cpp)
add_dependencies(baz_cfg rcfg_generate)

target_include_directories(baz_cfg
  PUBLIC
    "${CMAKE_CURRENT_SOURCE_DIR}/include"
  PRIVATE
    "${RCFG_GEN_DIR}"
)
