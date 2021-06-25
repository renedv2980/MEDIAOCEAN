*          DATA SET YYUNTSK    AT LEVEL 035 AS OF 05/01/02                      
*PHASE YYUNTSKA                                                                 
YYUNTSK  CSECT                                                                  
*                                                                               
         STM   RE,RC,12(RD)                                                     
         BASR  RB,0                                                             
         LA    RB,0(RB)                                                         
         SH    RB,12(RB)                                                        
         B     32(RB)                                                           
         DC    AL2(6)                                                           
         DC    AL2(0)                                                           
         DC    CL8' '                                                           
         DC    AL2(4096)                                                        
         USING *-32,RB                                                          
         L     RC,=A(R13CHAIN)                                                  
         CNOP  0,4                                                              
         B     *+8                                                              
         DC    A(*)                                                             
         LA    RF,*-4                                                           
         S     RF,*-8                                                           
         AR    RC,RF                                                            
         SR    RF,RF                                                            
         ICM   RF,3,20(RB)                                                      
         LA    RF,16(RC,RF)                                                     
         ST    RF,8(RD)                                                         
         ST    RD,4(RF)                                                         
         LR    RD,RF                                                            
         MVC   0(4,RC),24(RB)                                                   
         XC    4(4,RC),4(RC)                                                    
         ST    RD,8(RC)                                                         
         ST    R1,12(RC)                                                        
         LA    RC,16(RC)                                                        
         L     RE,4(RD)                                                         
         LM    RE,R1,12(RE)                                                     
*                                                                               
*                                                                               
         XC    TSKECB,TSKECB                                                    
         ATTACH EPLOC=SUBTASK,ECB=TSKECB                                        
         ST    R1,TSKTCB                                                        
         OC    TSKTCB,TSKTCB                                                    
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
*                                                                               
         WAIT  ECB=TSKECB                                                       
*                                                                               
         DETACH TSKTCB             DETACH THE TASK                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
*                                                                               
         SR    RF,RF                                                            
         L     RD,4(RD)                                                         
         L     RE,12(RD)                                                        
         LM    R0,RC,20(RD)                                                     
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
TSKTCB   DS    F                   SUBTASK TCB                                  
TSKECB   DS    F                   SUBTASK ECB                                  
SUBTASK  DC    C'YYUNTSK2'                                                      
R13CHAIN DS    500D                REGISTER SAVE AREA                           
*                                                                               
R0       EQU   0                                                                
R1       EQU   1                                                                
R2       EQU   2                                                                
R3       EQU   3                                                                
R4       EQU   4                                                                
R5       EQU   5                                                                
R6       EQU   6                                                                
R7       EQU   7                                                                
R8       EQU   8                                                                
R9       EQU   9                                                                
RA       EQU   10                                                               
RB       EQU   11                                                               
RC       EQU   12                                                               
RD       EQU   13                                                               
RE       EQU   14                                                               
RF       EQU   15                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035YYUNTSK   05/01/02'                                      
         END                                                                    
