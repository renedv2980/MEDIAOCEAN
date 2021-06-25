*          DATA SET CTXSFM     AT LEVEL 004 AS OF 08/17/00                      
*PHASE CTXSFMA                                                                  
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CHANGE ZENITH REP KEYS                          
*                                                                     *         
***********************************************************************         
         TITLE 'CTXSFM - CHANGE ZENITH REC'                                     
CTXSFM   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*CTXSFM*                                                       
*                                                                               
         LR    RC,R1               GET W/S POINTER                              
         USING CONWORKD,RC                                                      
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
*                                                                               
MAIN     DS    0H                                                               
         L     R2,AIOAREA          POINT TO RECORD                              
         LA    R2,4(R2)            POINT TO FIRST BYTE OF RECORD                
*                                                                               
         CLI   0(R2),X'FF'         TEST FILE TRAILER                            
         BE    EXIT                                                             
*                                                                               
M10      DS    0H                                                               
*                                                                               
         CLI   0(R2),X'05'         ID REC?                                      
         BNE   EXIT                                                             
         CLI   1(R2),X'0A'         ID REC?                                      
         BNE   EXIT                                                             
         MVC   19(5,R2),20(R2)                                                  
         MVI   24(R2),0                                                         
*                                                                               
* PRINT KEYS OF ALTERED RECS                                                    
         MVC   P(25),0(R2)                                                      
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
OUTTAB   DS    0CL10                                                            
         DC    C'S1APS     '                                                    
         DC    C'S1AS      '                                                    
         DC    C'S1CANS    '                                                    
         DC    C'S1CAPS    '                                                    
         DC    C'S1CAPSD   '                                                    
         DC    C'S1CAPSL   '                                                    
         DC    C'S1CAPSLD  '                                                    
         DC    C'S1CAPSM   '                                                    
         DC    C'S1CAPSMD  '                                                    
         DC    C'S1CAS     '                                                    
         DC    C'S1CPS     '                                                    
         DC    C'S1DAPS    '                                                    
         DC    C'S1LAPS    '                                                    
         DC    C'S1LAPSN   '                                                    
         DC    C'S1LAS     '                                                    
         DC    C'S1PALS    '                                                    
         DC    C'S1PS      '                                                    
         DC    C'S1PSD     '                                                    
         DC    C'S1RS      '                                                    
         DC    C'S1S       '                                                    
         DC    C'S1SAP     '                                                    
         DC    C'S1TAPS    '                                                    
         DC    C'S1TS      '                                                    
         DC    X'FF'                                                            
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004CTXSFM    08/17/00'                                      
         END                                                                    
