! Programa que calcula la enesima derivada en un punto dado.

Program derivadas 

    ! Se declaran todas las variables del programa
    
    integer :: i, j
    integer :: factorial, n !Función factorial, orden de derivada y número de puntos
    real :: h, a, z, n_derivada !punto sobre el que se evaluará la derivada y espacio entre intervalos 
    
    
    ! Se piden los valores para calcular la derivada
    
    write(*,*) '¿Cual es el orden de la derivada a calcular?'
    Read(*,*) n
    
    write(*,*) '¿Sobre que punto se calculara la derivada?'
    Read(*,*) a 
    
    n_derivada=0.0
    h = 0.01 
    j = n 

    Do i=0,n 
        z = (1/((2*h)**n))*((-1)**i)*(factorial(n)/(factorial(i)*factorial(n-i)))*F(a+j*h)
        n_derivada = n_derivada + z 
        j = j-2 
        write(*,*) n_derivada
    end do

    write(*,*) 'La derivada a la ', n, 'en el punto', a, 'es ', n_derivada
    
    End program 
    
    !Función sobre la que se derivará
    FUNCTION F(X)
        pi = 4 * atan(1.0)
        !F = 4*x**8
        !F = tanh(x)
        F = exp(x)
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
    