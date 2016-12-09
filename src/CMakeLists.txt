set(SOURCES 
    farray.f90
    portable.F90
    )

add_library(farray ${SOURCES})
target_include_directories(farray PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
    )
target_link_libraries(farray PUBLIC
    ${NETCDFF_LIBRARY} ${NETCDF_LIBRARY}
    )

add_executable(farray.exe main.f90)
target_link_libraries(farray.exe farray)
set_target_properties(farray.exe PROPERTIES OUTPUT_NAME farray)
