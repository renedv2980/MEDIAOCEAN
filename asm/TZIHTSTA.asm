*          DATA SET TZIHTSTA   AT LEVEL 073 AS OF 10/23/00                      
*PHASE TZIHTSTA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
         TITLE 'TZIHTSTA - TIM''S TEST PROGRAM'                                 
                                                                                
*                                                                               
TZIHTSTA CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*TZIHTSTA,=V(REGSAVE)                                          
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN10                                                           
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(TZIHTSTA),V(DUMMY)                                             
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
MAIN10   DS    0H                                                               
         EJECT                                                                  
**********************                                                          
* * YOUR CODE HERE *                                                            
READIN   DS    0H                  INPUT LIST MUST END WITH /*                  
         GOTO1 =V(CARDS),DMCB,IO,=C'RE00'                                       
********* ***** ******                                                          
         XBASE                                                                  
         EJECT                                                                  
DMCB     DS    6F                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
WORK     DS    CL64                                                             
IO       DS    CL80                                                             
*                                                                               
******************************************************                          
* IF YOU WISH TO DEFINE ANY STORAGE OR CONSTANTS,                               
* JUST APPEND THEM TO THE LIST ABOVE (PUT THEM HERE)                            
******************************************************                          
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073TZIHTSTA  10/23/00'                                      
         END                                                                    
