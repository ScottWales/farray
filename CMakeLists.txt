cmake_minimum_required(VERSION 2.8)
enable_language(Fortran)

project(farray)

include(CTest)

set(CMAKE_C_COMPILER       mpicc)
set(CMAKE_Fortran_COMPILER mpif90)

if (${CMAKE_Fortran_COMPILER_ID} MATCHES GNU)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -Wall -Wextra -Werror -fimplicit-none -fbacktrace")
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -fbounds-check --coverage")

elseif(${CMAKE_Fortran_COMPILER_ID} MATCHES Intel)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -traceback -warn all")
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -check all")

endif(${CMAKE_Fortran_COMPILER_ID} MATCHES GNU)


find_package(PkgConfig)
pkg_check_modules(PC_NETCDFF QUIET netcdf-fortran)
pkg_check_modules(PC_NETCDF  QUIET netcdf)

find_path(NETCDFF_INCLUDE_DIR netcdf4_f03.mod
    HINTS ${PC_NETCDFF_INCLUDEDIR})
find_library(NETCDFF_LIBRARY libnetcdff.so
    HINTS ${PC_NETCDFF_LIBDIR})
find_library(NETCDF_LIBRARY libnetcdf.so
    HINTS ${PC_NETCDFF_LIBDIR})

include_directories(${NETCDFF_INCLUDE_DIR})

add_subdirectory(src)
add_subdirectory(test)

