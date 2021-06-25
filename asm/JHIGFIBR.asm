*          DATA SET JHIGFIBR   AT LEVEL 049 AS OF 09/11/00                      
*PHASE JHIGFIBB                                                                 
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
* PUT YOUR CODE HERE *                                                          
**********************                                                          
*                                                                               
LIMIT    EQU   25               NUMBER OF FIBONACCI NUMBERS DESIRED             
         LA    R1,1             FIRST NUMBER IS 1                               
         LA    R2,1             SECOND NUMBER IS 1                              
         LA    R4,2             COUNTER IS INITIALIZED TO 2                     
         LA    R5,OUTP          COUNTER TO TRACK STORAGE LOC OF OUTP            
*                                                                               
         CVD   R1,DB            CONVERT NUMBER TO EBCDIC                        
         UNPK  0(8,R5),DB                                                       
         OI    7(R5),X'F0'                                                      
         AHI   R5,8             INCREMENT COUNTER                               
         CVD   R2,DB            CONVERT NUMBER TO EBCDIC                        
         UNPK  0(8,R5),DB                                                       
         OI    7(R5),X'F0'                                                      
         AHI   R5,8             INCREMENT COUNTER                               
*                                                                               
LOOP     CHI   R4,LIMIT         COMPARE COUNTER TO DESIRED NUMBER               
         BE    DONE             EXIT LOOP IF NUMBER IS REACHED                  
         AR    R1,R2            ADD LAST TWO NUMBERS, STORE NEXT IN R1          
         CVD   R1,DB            CONVERT NUMBER TO EBCDIC                        
         UNPK  0(8,R5),DB                                                       
         OI    7(R5),X'F0'                                                      
         AHI   R5,8             INCREMENT COUNTER                               
         AHI   R4,1             INCREMENT COUNTER                               
         CHI   R4,LIMIT         COMPARE COUNTER TO DESIRED NUMBER               
         BE    DONE             EXIT LOOP IF NUMBER IS REACHED                  
         AR    R2,R1            ADD LAST TWO NUMBERS, STORE NEXT IN R2          
         CVD   R2,DB            CONVERT NUMBER TO EBCDIC                        
         UNPK  0(8,R5),DB                                                       
         OI    7(R5),X'F0'                                                      
         AHI   R5,8             INCREMENT COUNTER                               
         AHI   R4,1             INCREMENT COUNTER                               
         B     LOOP             GO BACK TO TOP OF LOOP                          
DONE     AHI   R5,-8                                                            
*                                                                               
         L     RF,=V(PRINTER)                                                   
LOOP2    MVC   P(8),0(R5)       PUT NUMBER ON PRINT LINE                        
         BASR  RE,RF            PRINT NUMBER                                    
         AHI   R5,-8            DECREMENT LOCATION                              
         BCT   R4,LOOP2         R4 SHOULD BE AT LIMIT VALUE                     
*                                                                               
* DONE2    MVC   P(8),0(R5)                                                     
*          BASR  RE,RF                                                          
*                                                                               
*                                                                               
*                                                                               
* THE XBASE MACRO ENDS PROGRAM EXECUTION                                        
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
DMCB     DS    6F                                                               
DB       DS    D                                                                
OUTP     DS    25D                                                              
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
**PAN#1  DC    CL21'049JHIGFIBR  09/11/00'                                      
         END                                                                    
