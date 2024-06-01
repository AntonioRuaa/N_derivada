! Programa que calcula la enesima derivada en un punto dado.

Program derivadas 

! Se declaran todas las variables del programa

integer :: i, j
integer :: factorial, p, N !Función factorial, orden de derivada y número de puntos
real :: h, a, z !punto sobre el que se evaluará la derivada y espacio entre intervalos 
real, dimension(10, 11) :: coef_matrix !Matriz para guardar coeficientes 
real, dimension(10) :: Indexes, coef ! Vector de indices y coeficientes


! Se piden los valores para calcular la derivada

write(*,*) '¿Cual es el orden de la derivada a calcular?'
Read(*,*) p

write(*,*) '¿Sobre que punto se calculara la derivada?'
Read(*,*) a 

z=0
N = 10 ! N-1 es la derivada máxima de este programa
h = 0.1 
Indexes = (/-5, -4, -3, -2, -1, 1, 2, 3, 4, 5/)

! Se llena la matriz de coeficientes 

Do i=1, N 
    Do j=1, N
        If (i == 1) then
            coef_matrix(i,j) = 1.0
        Else if (i == p+1) then 
            coef_matrix(i,j) = (Indexes(j)*h)**p 
        Else 
            coef_matrix(i,j) = (Indexes(j))**(i-1)
        End if
    End do
End do

Do i=1, N 
    If (i == p+1) then
        coef_matrix(i, 11) = 1.0*factorial(p)
    Else 
        coef_matrix(i, 11) = 0
    End if
End do

! Se resuelve el sistema de ecuaciones y se guardan los coeficientes en un arreglo 
Do i=1, N 
    write(*,*) (coef_matrix(i,j), j=1,11)
end do 

call Gauss(coef_matrix)


! Se calcula la derivada sumando el producto de los coeficientes con la función evaluada en cada punto

Do i=1, N 
    d = a+Indexes(i)*h
    z=z+coef(i)*f(d)
End do 

z = z/(h**p)
write(*,*) 'La derivada a la ', p, 'en el punto', a, 'es ', z

Do i=1, N 
        write(*,*) (coef_matrix(i,j), j=1,11)
end do 

End program 

!Función sobre la que se derivará
FUNCTION F(X)
    pi = 4 * atan(1.0)
    F = x**4
    !F = tanh(x)
    !F = tan(x)
  RETURN
END FUNCTION

!Función que calcula el factorial
FUNCTION Factorial(X) 
    implicit none
    integer :: X, factorial, i
    factorial = 1
    Do i = 1, X 
        factorial = factorial*i
    End do 

  RETURN
END FUNCTION 

! Subrutina para resolver sistema de ecuaciones lineales
SUBROUTINE Gauss(A)

    IMPLICIT NONE
    INTEGER :: N,k,i,j
    REAL :: factor
    REAL, INTENT(in out) :: A(10,11)
    Real, dimension(10) :: v
    
    N = 10

    DO k=1,n+1
        DO i=k+1,n
            factor = A(i,k) / A(k,k)
            A(i,k:N+1) = A(i,k:N+1) - factor*A(k,k:N+1)
        END DO
    END DO
      
    write(*,*) 'UPPER TRIANGULAR MATRIX - '
    DO i=1,n
        write(*,*)(A(i,j),j=1,n+1)
    END DO

    !LAST ELEMENT
    v(n)=A(n,n+1)/A(n,n)

    !REST OF THE ELEMENTS

    DO i=n-1,1,-1
        v(i)=(A(i,n+1)- DOT_PRODUCT(A(i,i+1:N), v(i+1:N)))/ A(i,i)
    END DO

    write(*,*)'SOLUTIONS ARE - '
    DO i=1,n
        write(*,*) v(i)
    END DO

    
END SUBROUTINE 
