*          DATA SET JHIGFIB    AT LEVEL 035 AS OF 09/11/00                      
*PHASE JHIGFIBA                                                                 
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
LIMIT    EQU   25               NUMBER OF FIBONACCI NUMBERS DESIRED             
         LA    R1,1             FIRST NUMBER IS 1                               
         LA    R2,1             SECOND NUMBER IS 1                              
         LA    R4,2             COUNTER IS INITIALIZED TO 2                     
         CVD   R1,DB            CONVERT NUMBER TO EBCDIC                        
         UNPK  OUTP,DB                                                          
         OI    OUTP+7,X'F0'                                                     
         MVC   P(8),OUTP        PUT FIRST NUMBER ON PRINT LINE                  
         L     RF,=V(PRINTER)   PRINT A LINE                                    
         BASR  RE,RF                                                            
         CVD   R2,DB            CONVERT NUMBER TO EBCDIC                        
         UNPK  OUTP,DB                                                          
         OI    OUTP+7,X'F0'                                                     
         MVC   P(8),OUTP        PUT SECOND NUMBER ON PRINT LINE                 
         BASR  RE,RF                                                            
LOOP     CHI   R4,LIMIT         COMPARE COUNTER TO DESIRED NUMBER               
         BE    DONE             EXIT LOOP IF NUMBER IS REACHED                  
         AR    R1,R2            ADD LAST TWO NUMBERS, STORE NEXT IN R1          
         CVD   R1,DB            CONVERT NUMBER TO EBCDIC                        
         UNPK  OUTP,DB                                                          
         OI    OUTP+7,X'F0'                                                     
         MVC   P(8),OUTP        PUT NEXT NUMBER ON PRINT LINE                   
         BASR  RE,RF                                                            
         AHI   R4,1             INCREMENT COUNTER                               
         CHI   R4,LIMIT         COMPARE COUNTER TO DESIRED NUMBER               
         BE    DONE             EXIT LOOP IF NUMBER IS REACHED                  
         AR    R2,R1            ADD LAST TWO NUMBERS, STORE NEXT IN R2          
         CVD   R2,DB            CONVERT NUMBER TO EBCDIC                        
         UNPK  OUTP,DB                                                          
         OI    OUTP+7,X'F0'                                                     
         MVC   P(8),OUTP        PUT NEXT NUMBER ON PRINT LINE                   
         BASR  RE,RF                                                            
         AHI   R4,1             INCREMENT COUNTER                               
         B     LOOP             GO BACK TO TOP OF LOOP                          
DONE     DS    0H                                                               
*                                                                               
*                                                                               
*                                                                               
* THE XBASE MACRO ENDS PROGRAM EXECUTION                                        
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
DMCB     DS    6F                                                               
DB       DS    D                                                                
OUTP     DS    D                                                                
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
**PAN#1  DC    CL21'035JHIGFIB   09/11/00'                                      
         END                                                                    
