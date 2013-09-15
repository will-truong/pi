      program montecarlo
            type point
                  real(16) :: x, y
            end type point
            integer(8) inside, total, i
            real(16) :: pi                  
            type(point) :: random
            call srand(23489)
            pi = 0
            inside = 0
            total = 0
            i = 0
            
		do i = 1, 100000000
                  random = point(rand(), rand())
                  if(random % y ** 2 < 1 - random % x ** 2) then
                        inside = inside + 1
                  end if
                  total=total + 1
            end do
		pi = 4.0 * inside / total
            print *, pi
      end program montecarlo