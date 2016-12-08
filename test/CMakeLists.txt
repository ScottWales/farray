set(SOURCES test_foo.pfunit)

include($ENV{PFUNIT}/include/PreprocessMacro.cmake)
find_library(PFUNIT_LIB libpfunit.a HINTS $ENV{PFUNIT}/lib)
find_file(PFUNIT_DRIVER driver.F90 HINTS $ENV{PFUNIT}/include)
find_path(PFUNIT_MOD pfunit.mod HINTS $ENV{PFUNIT}/mod)

add_pfunit_sources(pfsources ${SOURCES})
add_executable(unittests ${pfsources} ${PFUNIT_DRIVER})
target_link_libraries(unittests ${PFUNIT_LIB} farray)
target_include_directories(unittests PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories(unittests PUBLIC ${PFUNIT_MOD})
target_compile_definitions(unittests PUBLIC USE_MPI)
add_test(NAME unittests COMMAND mpirun -np 4 $<TARGET_FILE:unittests>)

