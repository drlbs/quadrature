      program gleg

****  Gauss-Legendre Qaudrature

****  Program will integrate functon exp(sin((x+1)**2+2*cos(4*x+1)))


      double precision X(8), W(8), sum, fofx, A, B
      external fofx

      X(1) = -9.602898564975363E-001  
      X(2) = -7.966664774136267E-001  
      X(3) = -5.255324099163290E-001  
      X(4) = -1.834346424956498E-001  
      X(5) = 1.834346424956498E-001  
      X(6) =  5.255324099163290E-001  
      X(7) =  7.966664774136267E-001  
      X(8) =  9.602898564975363E-001  
      W(1) =  1.012285362903706E-001
      W(2) =  2.223810344533744E-001          
      W(3) =  3.137066458778874E-001         
      W(4) =  3.626837833783621E-001       
      W(5) =  3.626837833783621E-001      
      W(6) =  3.137066458778874E-001
      W(7) =  2.223810344533744E-001     
      W(8) =  1.012285362903706E-001      

      A = 0.0
      B = acos(-1.0D0) 
      sum = 0.0
      do 10 i = 1, 8
         sum = sum + W(i) * fofx(((B-A)*X(i)+(A+B))/2.0)*(B-A)/2.0
10    continue

      print *, 'The value of the integral is ', sum

      end 
      

      double precision function fofx( x )
      double precision x
      fofx = exp(sin((x+1)**2+2*cos(4*x+1)))
      return
      end
