set(SOURCES test_farray.pfunit)

if (${CMAKE_Fortran_COMPILER_ID} MATCHES GNU)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wno-unused-parameter -Wno-unused-variable -Wno-maybe-uninitialized")

elseif(${CMAKE_Fortran_COMPILER_ID} MATCHES Intel)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -traceback -warn all")

endif(${CMAKE_Fortran_COMPILER_ID} MATCHES GNU)

#if ($ENV{PFUNIT})

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
    add_test(NAME unittests COMMAND mpirun -np 4 $<TARGET_FILE:unittests> -name farray -xml unittests.xml)

    #endif ($ENV{PFUNIT})
