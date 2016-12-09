!> 
!! Copyright 2016 ARC Centre of Excellence for Climate Systems Science
!!
!! \author  Scott Wales <scott.wales@unimelb.edu.au>
!!
!! Licensed under the Apache License, Version 2.0 (the "License");
!! you may not use this file except in compliance with the License.
!! You may obtain a copy of the License at
!!
!!     http://www.apache.org/licenses/LICENSE-2.0
!!
!! Unless required by applicable law or agreed to in writing, software
!! distributed under the License is distributed on an "AS IS" BASIS,
!! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!! See the License for the specific language governing permissions and
!! limitations under the License.

module farray_mod
    private

    public open_dataset

    ! Represents a netCDF/OPENDAP file
    type, public :: dataset_t
        integer :: ncid
    contains
        procedure :: variable
        procedure :: close => close_dataset
    end type

    ! Represents a variable within the file
    type, public :: variable_t
        integer :: ncid
        integer :: varid
        integer, allocatable :: dimid(:)
    contains
        procedure :: dimension_index => var_dimension_index
        generic :: dimension => dimension_index

        procedure :: shape => var_shape

        procedure :: read_int32
        procedure :: read_int64
        procedure :: read_real32
        procedure :: read_real64
        generic :: read => read_int32, read_int64, read_real32, read_real64

!        procedure :: read_int32_alloc
!        procedure :: read_int64_alloc
!        procedure :: read_real32_alloc
!        procedure :: read_real64_alloc
!        generic :: read_alloc => read_int32_alloc, read_int64_alloc, read_real32_alloc, read_real64_alloc
    end type

    ! Represents a dimension
    type, public :: dimension_t
        integer :: ncid
        integer :: dimid
    contains
        procedure :: size => dimension_size
    end type

    interface open_dataset
        procedure :: open_dataset_f08
        procedure :: open_dataset_f90
    end interface

contains

    function open_dataset_f08(path, comm) result(r)
        use mpi_f08
        use netcdf4_f03
        character(len=*), intent(in) :: path
        type(mpi_comm), intent(in) :: comm 
        type(dataset_t) :: r

        r = open_dataset_f90(path, comm%mpi_val)
    end function

    function open_dataset_f90(path, comm) result(r)
        use mpi_f08
        use netcdf4_f03
        character(len=*), intent(in) :: path
        integer, intent(in) :: comm 
        type(dataset_t) :: r
        integer :: mode
        integer :: err

        mode = NF_NOWRITE
        err = nf_open_par(path, mode, comm=comm, info=MPI_INFO_NULL%mpi_val, ncid=r%ncid)

        if (err == NF_ENOPAR) then
            ! No parallel netcdf support
            err = nf_open(path, mode, ncid=r%ncid)
        end if
        call check(err)
    end function

    subroutine close_dataset(this)
        use netcdf4_f03
        class(dataset_t), intent(inout) :: this
        integer :: err

        err = nf_close(this%ncid)
        call check(err)
    end subroutine

    function variable(this, name) result(r)
        use netcdf4_f03
        class(dataset_t), intent(in) :: this
        character(len=*), intent(in) :: name
        type(variable_t) :: r
        integer :: ndims
        integer :: err

        r%ncid = this%ncid
        err = nf_inq_varid(r%ncid, name, r%varid)
        call check(err)

        err = nf_inq_varndims(r%ncid, r%varid, ndims)
        call check(err)

        allocate(r%dimid(ndims))

        err = nf_inq_vardimid(r%ncid, r%varid, r%dimid)
        call check(err)
    end function

    function var_shape(this) result(r)
        class(variable_t), intent(in) :: this
        integer, allocatable :: r(:)
        type(dimension_t) :: dd
        integer :: ii

        allocate(r(size(this%dimid)))
        do ii=1,size(this%dimid)
            dd = this%dimension(ii)
            r(ii) = dd%size()
        end do
    end function

    function var_dimension_index(this, idx) result(r)
        use netcdf4_f03
        class(variable_t), intent(in) :: this
        integer, intent(in) :: idx
        type(dimension_t) :: r

        r%ncid = this%ncid
        r%dimid = this%dimid(idx)
    end function

    function dimension_size(this) result(r)
        use netcdf4_f03
        class(dimension_t) :: this
        integer :: r
        integer :: err
    
        err = nf_inq_dimlen(this%ncid, this%dimid, r)
        call check(err)
    end function

    subroutine read_int32(this, array)
        use netcdf4_f03
        class(variable_t), intent(in) :: this
        integer(kind=4), dimension(..) :: array
        integer :: err

        err = nf_get_vara_int(this%ncid, this%varid, lbound(array), shape(array), array)
        call check(err)
    end subroutine

    subroutine read_int64(this, array)
        use netcdf4_f03
        class(variable_t), intent(in) :: this
        integer(kind=8), dimension(..) :: array
        integer :: err

        err = nf_get_vara_int64(this%ncid, this%varid, lbound(array), shape(array), array)
        call check(err)
    end subroutine

    subroutine read_real32(this, array)
        use netcdf4_f03
        class(variable_t), intent(in) :: this
        real(kind=4), dimension(..) :: array
        integer :: err

        err = nf_get_vara_real(this%ncid, this%varid, lbound(array), shape(array), array)
        call check(err)
    end subroutine

    subroutine read_real64(this, array)
        use netcdf4_f03
        class(variable_t), intent(in) :: this
        real(kind=8), dimension(..) :: array
        integer :: err

        err = nf_get_vara_double(this%ncid, this%varid, lbound(array), shape(array), array)
        call check(err)
    end subroutine

!    subroutine read_int32_alloc(this, array)
!        use netcdf4_f03
!        class(variable_t), intent(in) :: this
!        integer(kind=4), allocatable, dimension(..) :: array
!        integer :: err
!
!        allocate(array(this%shape()))
!        err = nf_get_vara_int(this%ncid, this%varid, lbound(array), shape(array), array)
!        call check(err)
!    end subroutine
!
!    subroutine read_int64_alloc(this, array)
!        use netcdf4_f03
!        class(variable_t), intent(in) :: this
!        integer(kind=8), allocatable, dimension(..) :: array
!        integer :: err
!
!        allocate(array(this%shape()))
!        err = nf_get_vara_int64(this%ncid, this%varid, lbound(array), shape(array), array)
!        call check(err)
!    end subroutine
!
!    subroutine read_real32_alloc(this, array)
!        use netcdf4_f03
!        class(variable_t), intent(in) :: this
!        real(kind=4), allocatable, dimension(..) :: array
!        integer :: err
!
!        allocate(array(this%shape()))
!        err = nf_get_vara_real(this%ncid, this%varid, lbound(array), shape(array), array)
!        call check(err)
!    end subroutine
!
!    subroutine read_real64_alloc(this, array)
!        use netcdf4_f03
!        class(variable_t), intent(in) :: this
!        real(kind=8), allocatable, dimension(..) :: array
!        integer :: err
!
!        allocate(array(this%shape()))
!        err = nf_get_vara_double(this%ncid, this%varid, lbound(array), shape(array), array)
!        call check(err)
!    end subroutine

    subroutine check(err)
        use portable_mod
        use netcdf4_f03
        use mpi_f08
        integer, intent(in) :: err
        integer :: merr

        if (err /= 0) THEN
            write(*,*) nf_strerror(err)
            call traceback()
            call MPI_Abort(MPI_COMM_WORLD,err,merr)
        end if
    end subroutine
end module
