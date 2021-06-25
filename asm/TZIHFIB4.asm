*          DATA SET TZIHFIB4   AT LEVEL 058 AS OF 09/11/00                      
*PHASE TZIHFIB4                                                                 
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
* 3.  THERE IS JCL TO RUN THE PROGRAM IN 'DEIS.DDS.JCL(FIB)' --                 
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
* * YOUR CODE HERE *                                                            
MAIN     DS    0H                                                               
FIB1     EQU   1                                                                
FIB2     EQU   1                                                                
         LHI   R2,FIB1             INITIALIZE FIRST TWO NUMBERS                 
         LHI   R3,FIB2                                                          
         LHI   R5,23               R5 SERVES AS A COUNTER                       
         LA    R6,FIBS             R6 POINTS TO FIBS                            
         ST    R2,0(R6)            *STORE FIB1 IN MEMORY                        
         ST    R3,4(R6)            *STORE FIB2 IN MEMORY                        
         AHI   R6,8                *INC COUNTER                                 
*                                                                               
LOOP     DS    0H                                                               
         LA    R4,0(R2,R3)         R4 IS SUM OF R2,R3                           
*                                                                               
         ST    R4,0(R6)            *STORE BINARY NUM IN MEMORY                  
         AHI   R6,4                                                             
         LR    R2,R3               SHIFT NO'S SO R2,R3 HOLD LAST TWO            
         LR    R3,R4                                                            
         BCT   R5,LOOP                                                          
*                                                                               
         L     RF,=V(PRINTER)                                                   
         LHI   R5,25               R5 SERVES AS A COUNTER                       
LOOP2    DS    0H                                                               
         AHI   R6,-4                                                            
         L     R4,0(R6)                                                         
         CVD   R4,PACD             NUMBER-PRINTING SEQUENCE                     
         UNPK  P(16),PACD                                                       
         OC    P+15(1),=X'F0'                                                   
         BASR  RE,RF                                                            
         BCT   R5,LOOP2                                                         
*                                                                               
**********************                                                          
*                                                                               
*                                                                               
* THE XBASE MACRO ENDS PROGRAM EXECUTION                                        
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
PACD     DS    D                                                                
FIBS     DS    25F                                                              
QTY      DC    F'24'                                                            
DMCB     DS    6F                                                               
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
**PAN#1  DC    CL21'058TZIHFIB4  09/11/00'                                      
         END                                                                    
