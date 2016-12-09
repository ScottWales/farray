! Misc portability functions
module portable_mod
contains

    ! Print a traceback and continue
#if defined(__GFORTRAN__)
    subroutine traceback()
        CALL backtrace()
    end subroutine

#elif defined(__INTEL__COMPILER)
    subroutine traceback()
        USE ifcore
        CALL tracebackqq(user_exit_code=-1)
    end subroutine

#else
#error Unknown compiler
#endif

end module
