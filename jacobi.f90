module functions
    implicit none
contains

subroutine show_arr(arr, name)
    real, DIMENSION(:,:), ALLOCATABLE, intent(in) :: arr  
    character (*), intent(in) :: name 

    print *, name
    if (allocated(arr)) then
        write(*,"(3f20.7)") arr
    end if
end subroutine show_arr

SUBROUTINE FillMatrix(matrix)
    real, DIMENSION(:,:), ALLOCATABLE :: matrix  
    matrix(1,1) = 1.0
    matrix(1,2) = sqrt(2.0)
    matrix(1,3) = 2.0
    matrix(2,1) = sqrt(2.0)
    matrix(2,2) = 3.0
    matrix(2,3) = sqrt(2.0)
    matrix(3,1) = 2.0
    matrix(3,2) = sqrt(2.0)
    matrix(3,3) = 1.0
    return
END


SUBROUTINE fill_jacobi_matrix(matrix, p, q, phi)
    real, DIMENSION(:,:), ALLOCATABLE :: matrix 
    real, intent(in) :: phi
    Integer i,j,p,q,N
    N = size(matrix,2)
    ForAll(i = 1:N, j = 1:N) matrix(i,j) = (i/j)*(j/i)
    matrix(p,p) = cos(phi)
    matrix(q,q) = matrix(p,p)
    matrix(p,q) = sin(phi)
    matrix(q,p) = -1.0*matrix(p,q)
    return
END
    
end module functions


program Jacobi
    use functions
    implicit none
    integer :: i,j
    REAL A,SUM 
    real, DIMENSION(:,:), ALLOCATABLE :: matrix  
    ALLOCATE(matrix(3,3))
    call fill_jacobi_matrix(matrix,1,2, 3.14159/4) 
    call show_arr(matrix, "Matrix")
    call show_arr(matrix, "Jacobi Translation Matrix")

end program Jacobi
