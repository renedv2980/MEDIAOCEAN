*          DATA SET SDEDSUB    AT LEVEL 015 AS OF 05/01/02                      
*PHASE DEISSUBA                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE DATVAL                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE DTCNV                                                                  
*INCLUDE MONVAL                                                                 
*INCLUDE PERVERT                                                                
         TITLE 'DATESUB - TEST OF LOADABLE DATE MODULES'                        
***********************************************************************         
*LOADED BY DEISDATE SUBROUTINE LINKED INTO CALLER'S PROGRAM           *         
*CALLED BY DEISDATE SUBROUTINE WITH PARAMETERS IN R1,R2,R3            *         
*                                                                     *         
*R1 AL4(CALLER'S PARAMETER LIST)                                      *         
*R2 AL4(EXTRACTED VALUES FROM CTRY)                                   *         
*R3 AL1 SUBROUTINE NUMBER                                             *         
***********************************************************************         
         SPACE                                                                  
         PRINT NOGEN                                                            
DATESUB  CSECT                                                                  
         REQUS                                                                  
*                                                                               
         ENTRY MASTC               V(MASTC) FOR CTRY MACRO                      
*                                                                               
         USING *,RF                                                             
         ST    RE,SAVERE           SAVE CALLER'S RETURN ADDRESS                 
         B     SETCTRY                                                          
         DC    C'*DATESUB'                                                      
*                                                                               
SETCTRY  LA    RE,MASTC            SET UP DUMMY MASTC FOR CTRY MACRO            
         USING MASTD,RE                                                         
         STCM  R2,2,MCCTRY         ACTUAL COUNTRY CODE                          
         STCM  R2,1,MCLANG         LANGUAGE CODE                                
         STCM  R2,8,MCAGCOPT       AGENCY COUNTRY OPTIONS                       
         STCM  R2,4,MCAGCTRY       AGENCY COUNTRY CODE                          
         DROP  RE                                                               
*                                                                               
         SRL   R3,22               R3 <== DISPLACEMENT TO A(SUBROUTINE)         
         L     RF,VDATEMOD(R3)     RF <== A(SUBROUTINE)                         
         BASR  RE,RF                                                            
         DROP  RF                                                               
*                                                                               
         USING *,RE                                                             
         L     RE,SAVERE           RETURN TO CALLER                             
         BR    RE                                                               
         DROP  RE                                                               
         EJECT                                                                  
VDATEMOD DS    0A                                                               
         DC    V(DATCON)           ROUTINE NUM 00                               
         DC    V(PERVAL)           ROUTINE NUM 01                               
*                                                                               
SAVERE   DS    A                                                                
*                                                                               
MASTC    DC    XL256'00'           FOR CTRY MACRO                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015SDEDSUB   05/01/02'                                      
         END                                                                    
