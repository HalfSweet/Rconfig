add_library(foo_cfg STATIC src/foo.cpp)
add_dependencies(foo_cfg rcfg_generate)

target_include_directories(foo_cfg
  PUBLIC
    "${CMAKE_CURRENT_SOURCE_DIR}/include"
  PRIVATE
    "${RCFG_GEN_DIR}"
)

target_link_libraries(foo_cfg
  PUBLIC
    baz_cfg
    qux_cfg
)
