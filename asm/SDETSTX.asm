*          DATA SET SDETSTX    AT LEVEL 015 AS OF 08/13/00                      
*PHASE SDETESTA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE DEISDATE                                                               
*INCLUDE KHDUMMY                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'SDETEST -- A SILLY LITTLE PROGRAM'                              
SDETEST  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*SDETEST,=V(REGSAVE),R9                                        
*                                                                               
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         MVC   P,=CL132'CALL DATCON:'                                           
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(5,P)                                      
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
WORK     DS    CL17                                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015SDETSTX   08/13/00'                                      
         END                                                                    
