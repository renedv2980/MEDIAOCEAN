*          DATA SET YYUNTSKS   AT LEVEL 030 AS OF 08/16/00                      
*PHASE YYUNTSKA                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
YYUNTSK  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*YYUNTSK,=A(R13CHAIN),R9                                       
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         TITLE 'IDF TEST FOR PROGRAM WITH ATTACHED SUBTASK'                     
*                                                                               
         MVC   P(20),=CL20'ATTACHING '                                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XC    TSKPARMS,TSKPARMS                                                
         XC    TSKECB,TSKECB                                                    
         ATTACH EPLOC=SUBTASK,ECB=TSKECB,PARAM=TSKPARMS,SZERO=NO                
         ST    R1,TSKTCB                                                        
         OC    TSKTCB,TSKTCB                                                    
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
*                                                                               
         WAIT  ECB=TSKECB                                                       
*                                                                               
*********STIMER WAIT,DINTVL=WAITTIME                                            
*                                                                               
         MVC   P(20),=CL20'DETACHING '                                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         DETACH TSKTCB             DETACH THE TASK                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
*                                                                               
         XBASE                                                                  
*                                                                               
         DS    0D                                                               
WAITTIME DC    C'00001000'         30 SEC                                       
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
DMWORK   DS    12D                                                              
DMCB     DS    10F                                                              
DUB      DS    D                                                                
RECORD   DS    CL150                                                            
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
WORK     DS    CL256                                                            
TSKTCB   DS    F                                                                
TSKECB   DS    F                   ECB FOR E-MAIL SUBTASK                       
SUBTASK  DC    C'YYUNTSK2'                                                      
TSKPARMS DS    XL16                                                             
         EJECT                                                                  
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
*                                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030YYUNTSKS  08/16/00'                                      
         END                                                                    
