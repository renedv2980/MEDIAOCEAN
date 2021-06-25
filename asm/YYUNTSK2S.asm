*          DATA SET YYUNTSK2S  AT LEVEL 026 AS OF 08/16/00                      
*PHASE YYUNTSK2A                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
YYUNTSK2 CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,YYUNTSK2,=A(R13CHAIN),R9                                       
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         TITLE 'IDF TEST FOR PROGRAM WITH ATTACHED SUBTASK XXXX'                
*                                                                               
         OPEN  FILEIN                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETREC   GET   FILEIN,P                                                         
         GOTO1 =V(PRINTER)                                                      
         B     GETREC                                                           
*                                                                               
         EJECT                                                                  
GOODBYE  CLOSE FILEIN                                                           
*                                                                               
         XBASE                                                                  
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
FILEIN   DCB   DDNAME=FILEIN,MACRF=GM,DSORG=PS,EODAD=GOODBYE                    
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
TSKECB   DS    F                                                                
SUBPARMS DS    XL16                                                             
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
**PAN#1  DC    CL21'026YYUNTSK2S 08/16/00'                                      
         END                                                                    
