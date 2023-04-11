    

  DO I =1, iquotient1*BS1, BS1
        DO J = 1, iquotient2*BS2, BS2
            DO II = I, I-1+BS1
                DO JJ = J, J-1+BS2
                    A(II,JJ) = A(II,JJ) + B(II,JJ)
                    BS_sum=BS_sum+1
                ENDDO   
            ENDDO
        ENDDO 
    ENDDO

    if(iremainder1 > 0 .and. iremainder2>0) then 
    DO I =1, iMAX1 
        DO J = iquotient2*BS2+1,iMAX2 
                    A(I,J) = A(I,J) + B(I,J)
                    BS_sum=BS_sum+1
        ENDDO 
    ENDDO
    DO I =iquotient1*BS1+1, iMAX1 
        DO J =1, iquotient2*BS2 
                    A(I,J) = A(I,J) + B(I,J)
                    BS_sum=BS_sum+1
        ENDDO 
    ENDDO
    endif

    if(iremainder1 > 0 .and. iremainder2==0) then 
    DO I =iquotient1*BS1+1, iMAX1 
        DO J =1, iquotient2*BS2 
                    A(I,J) = A(I,J) + B(I,J)
                    BS_sum=BS_sum+1
        ENDDO 
    ENDDO
    endif

    if(iremainder1 == 0 .and. iremainder2>0) then 
    DO I =1, iMAX1 
        DO J = iquotient2*BS2+1,iMAX2 
                    A(I,J) = A(I,J) + B(I,J)
                    BS_sum=BS_sum+1
        ENDDO 
    ENDDO
    endif


   DO I =1, iMAX1
        DO J = 1, iMAX2
            A(I,J) = A(I,J) + B(I,J)
            NOBS_sum=NOBS_sum+1
        ENDDO 
    ENDDO 



