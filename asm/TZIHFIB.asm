*          DATA SET TZIHFIB    AT LEVEL 042 AS OF 09/08/00                      
*PHASE TZIHFIB                                                                  
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
         LHI   R5,2                R5 SERVES AS A COUNTER                       
*        MVC   P,=132C' '                                                       
*        MVC   P,=C'START'                                                      
*        L     RF,=V(PRINTER)                                                   
*        BASR  RE,RF                                                            
*                                                                               
         CVD   R2,PACD             CONVERT FIB1 TO PACKED DECIMAL               
         UNPK  P(16),PACD          UNPACK IT, SO IT LOOKS LIKE EBCDIC           
         OC    P+15(1),=X'F0'      CONVERT SIGN NIBBLE TO F                     
         L     RF,=V(PRINTER)      PRINT                                        
         BASR  RE,RF                                                            
*                                                                               
         CVD   R3,PACD             SAME PROCEDURE FOR FIB2                      
         UNPK  P(16),PACD                                                       
         OC    P+15(1),=X'F0'                                                   
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
*                                                                               
LOOP     DS    0H                                                               
         LHI   R4,0                INITIALIZE NEXT FIB NO. TO ZERO              
         AR    R4,R3               NEXT FIB. NO IS SUM OF PREVIOUS TWO          
         AR    R4,R2                                                            
*                                                                               
         CVD   R4,PACD             NUMBER-PRINTING SEQUENCE                     
         UNPK  P(16),PACD                                                       
         OC    P+15(1),=X'F0'                                                   
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
         LR    R2,R3               SHIFT NO'S SO R2,R3 HOLD LAST TWO            
         LR    R3,R4                                                            
         AHI   R5,1                INCREMENT COUNTER                            
         CL    R5,QTY              IF COUNTER IS NOT 25 - ONCE MORE             
         BNE   LOOP                                                             
**********************                                                          
*                                                                               
*                                                                               
* THE XBASE MACRO ENDS PROGRAM EXECUTION                                        
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
PACD     DS    D                                                                
QTY      DC    F'25'                                                            
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
**PAN#1  DC    CL21'042TZIHFIB   09/08/00'                                      
         END                                                                    
