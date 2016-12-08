cmake_minimum_required(VERSION 3.4)
enable_language(Fortran)

project(farray)

include(CTest)

set(CMAKE_C_COMPILER       mpicc)
set(CMAKE_Fortran_COMPILER mpif90)

if (${CMAKE_Fortran_COMPILER_ID} MATCHES GNU)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -Wall -Wextra -fimplicit-none -fbacktrace")
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -fbounds-check --coverage")

elseif(${CMAKE_Fortran_COMPILER_ID} MATCHES Intel)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -traceback -warn all")
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -check all")

endif(${CMAKE_Fortran_COMPILER_ID} MATCHES GNU)

add_subdirectory(src)
add_subdirectory(test)

