set(SOURCES foo.f90)

add_library(farray ${SOURCES})
target_include_directories(hello PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
    )

add_executable(farray.exe main.f90)
target_link_libraries(farray.exe farray)
set_target_properties(farray.exe PROPERTIES OUTPUT_NAME farray)
