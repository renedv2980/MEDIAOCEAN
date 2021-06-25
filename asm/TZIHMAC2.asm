*          DATA SET TZIHMAC2   AT LEVEL 075 AS OF 10/17/00                      
*PHASE TZIHMAC2                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
         TITLE 'TZIHMAC -- TESTING MACROS'                                      
* LEAVE THIS CODE ALONE                                                         
*                                                                               
TZIHMAC  CSECT                                                                  
                                                                                
***MY MACRO IS HERE***                                                          
                                                                                
         MACRO                                                                  
         MAC   &P1,&P2                                                          
         LCLC  &V1,&V2,&V3                                                      
&V1      SETC  '&P1'(1,1)                                                       
&V2      SETC  '&P1'(1,2)                                                       
&V3      SETC  '&P1'(1,3)                                                       
         X&V1                                                                   
         X&V2                                                                   
         X&V3                                                                   
         MEND                                                                   
                                                                                
***END OF MACRO***                                                              
                                                                                
         PRINT NOGEN                                                            
         NBASE 0,*TZIHMAC,=V(REGSAVE)                                           
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN10                                                           
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(TZIHMAC),V(DUMMY)                                              
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
MAIN10   DS    0H                                                               
         EJECT                                                                  
**********************                                                          
* * YOUR CODE HERE *                                                            
MAIN     DS    0H                                                               
                                                                                
         MAC   (ONE,TWO,THREE),R0                                               
                                                                                
**********************                                                          
*                                                                               
*                                                                               
* THE XBASE MACRO ENDS PROGRAM EXECUTION                                        
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
                                                                                
DMCB     DS    6F                                                               
DUB1     DS    D                                                                
DUB2     DS    D                                                                
FULL1    DS    F                                                                
FULL2    DS    F                                                                
HALF1    DS    H                                                                
HALF2    DS    H                                                                
BYTE1    DS    X                                                                
BYTE2    DS    X                                                                
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
**PAN#1  DC    CL21'075TZIHMAC2  10/17/00'                                      
         END                                                                    
