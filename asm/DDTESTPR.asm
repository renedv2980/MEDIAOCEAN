*          DATA SET DDTESTPR   AT LEVEL 001 AS OF 04/27/16                      
*PHASE TESTPRA                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'A TEST PROGRAM TO TEST PRINTER CODES'                           
         PRINT  GEN                                                             
TEST     CSECT                                                                  
         NBASE 0,**TEST**,WORK                                                  
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE(20),=CL20'PRINTER CODES'                                   
*                                                                               
         MVI   Q,C' '                                                           
         MVC   Q+1(L'Q-1),Q                                                     
         GOTO1 =V(PRINT),PARAMS,QCC,=C'BC01'                                    
         SR    R5,R5                                                            
*                                                                               
LOOP     STC   R5,Q                                                             
         GOTO1 =V(HEXOUT),PARAMS,Q,Q+2,1,=C'TOG'                                
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  Q+5(3),DUB                                                       
         STC   R5,Q+9                                                           
         GOTO1 =V(PRINT),PARAMS,QCC,=C'BL01'                                    
         LA    R5,1(R5)                                                         
         CHI   R5,255                                                           
         BNH   LOOP                                                             
*                                                                               
EXIT     XBASE                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
DUB      DS    D                                                                
PARAMS   DS    6F                                                               
C        DS    CL80                                                             
QCC      DS    X                                                                
Q        DS    CL132                                                            
WORK     DS    1000D                                                            
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DDTESTPR  04/27/16'                                      
         END                                                                    
