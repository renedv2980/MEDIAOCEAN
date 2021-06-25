*          DATA SET DDTRFIBR   AT LEVEL 040 AS OF 08/21/98                      
*PHASE GCHEFIBR                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
         TITLE 'TESTFIB -- FIBONACCI SEQUENCE'                                  
***********************************************************************         
*                                                                               
* THE ASSIGNMENT IS TO PRINT THE FIRST 25 FIBONACCI NUMBERS, ONE PER            
* PRINT LINE.  DON'T WORRY ABOUT SUPPRESSING LEADING ZEROES.                    
*                                                                               
* 1.  BE SURE TO CHANGE THE *PHASE CARD ABOVE SO YOU DON'T WIPE                 
*     OUT EACH OTHER'S LOAD MODULES.  REPLACE THE 'XXXX' WITH YOUR              
*     USERID.                                                                   
*                                                                               
* 2.  USE THE CVD AND UNPK INSTRUCTIONS TO PUT EACH BINARY NUMBER               
*     INTO EACH PRINT FIELD.                                                    
*                                                                               
* 3.  THERE IS JCL TO RUN THE PROGRAM IN 'DEIS.DDS.JCL(XXXXFIB)' --             
*     COPY THIS TO YOUR OWN JCL LIBRARY.                                        
*                                                                               
***********************************************************************         
         EJECT                                                                  
* LEAVE THIS CODE ALONE                                                         
*                                                                               
TESTFIB  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*TESTFIB,=V(REGSAVE)                                           
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN10                                                           
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(TESTFIB),V(DUMMY)                                              
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
MAIN10   DS    0H                                                               
         EJECT                                                                  
**********************                                                          
* PUT YOUR CODE HERE *                                                          
**********************                                                          
* INITIALIZE ONE REGISTER TO 0 AND ANOTHER TO 0.                                
* INITIALIZE SUM TO 0. ADD REGISTERS AND SWAP THEM.                             
         SR    R2,R2                                                            
         LA    R3,1                                                             
         SR    R4,R4                                                            
         AR    R2,R3                                                            
         LR    R4,R2                                                            
         LA    R5,2                                                             
LOOP     AR    R2,R3                                                            
         LA    R5,1(R5)                                                         
         LR    R4,R2                                                            
         LR    R2,R3                                                            
         LR    R3,R4                                                            
         CHI   R5,24                                                            
         BNE   LOOP                                                             
*                                                                               
* PRINT LAST TWO NUMBERS. SUBTRACT PRIVIOUS NUMBER                              
* NUMBER FROM LAST ONE. PRINT                                                   
* THE RESULT AND SWAP ALL NUMBERS.                                              
*                                                                               
         CVD   R3,DUB                                                           
         UNPK  P(15),DUB                                                        
         OI    P+14,X'F0'                                                       
         GOTO1 =V(PRINTER)                                                      
         CVD   R2,DUB                                                           
         UNPK  P(15),DUB                                                        
         OI    P+14,X'F0'                                                       
         GOTO1 =V(PRINTER)                                                      
         LA    R5,23                                                            
LOOP10   SR    R3,R2                                                            
         CVD   R3,DUB                                                           
         UNPK  P(15),DUB                                                        
         OI    P+14,X'F0'                                                       
         GOTO1 =V(PRINTER)                                                      
         LR    R4,R3                                                            
         LR    R3,R2                                                            
         LR    R2,R4                                                            
         BCT   R5,LOOP10                                                        
* THE XBASE MACRO ENDS PROGRAM EXECUTION                                        
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
DMCB     DS    6F                                                               
******************************************************                          
* IF YOU WISH TO DEFINE ANY STORAGE OR CONSTANTS,                               
* JUST APPEND THEM TO THE LIST ABOVE (PUT THEM HERE)                            
******************************************************                          
DUB      DS    D                                                                
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040DDTRFIBR  08/21/98'                                      
         END                                                                    
