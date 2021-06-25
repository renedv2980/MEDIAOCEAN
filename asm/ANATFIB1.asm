*          DATA SET ANATFIB1   AT LEVEL 058 AS OF 09/11/00                      
*PHASE ANATFIBA                                                                 
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
                                                                                
MAIN10   DS    0H                                                               
         EJECT                                                                  
**********************                                                          
* PUT YOUR CODE HERE *                                                          
         LA    R2,1                 FIRST FIBONACCI IN R2                       
         LA    R6,FULL                                                          
         ST    R2,0(R6)                                                         
         LA    R3,1                 SECOND FIBONACCI IN R3                      
         ST    R3,4(R6)                                                         
         AHI   R6,8                                                             
         LA    R5,23                    COUNTER IN R5                           
*                                                                               
LOOP1    DS    0H                                                               
         LA    R4,0                                                             
         AR    R4,R2                                                            
         AR    R4,R3                    THE CURRENT FIBONACCI IN R4             
         ST    R4,0(R6)                                                         
         AHI   R6,4                                                             
*                                                                               
         LR    R2,R3          MOVE THE (CURRENT-2)ND FIBONACCI IN R2            
         LR    R3,R4          MOVE THE (CURRENT-1)ST FIBONACCI IN R3            
         BCT   R5,LOOP1                                                         
*                                                                               
         LA    R5,25                                                            
*                                                                               
*                                   THE FOLLOWING LOOP WILL PRINT FIB           
*                                   NUMBERS.                                    
*                                                                               
LOOP2    DS    0H                                                               
         AHI   R6,-4                                                            
         L     R4,0(R6)                                                         
         CVD   R4,PACKD                                                         
         UNPK  UNPACKD,PACKD                                                    
         OC    UNPACKD+15(1),=X'F0'                                             
         MVC   P(16),UNPACKD                                                    
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
         BCT   R5,LOOP2                                                         
**********************                                                          
*                                                                               
*                                                                               
*                                                                               
* THE XBASE MACRO ENDS PROGRAM EXECUTION                                        
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
DMCB     DS    6F                                                               
******************************************************                          
* IF YOU WISH TO DEFINE ANY STORAGE OR CONSTANTS,                               
* JUST APPEND THEM TO THE LIST ABOVE (PUT THEM HERE)                            
*                                                                               
PACKD    DS    D       CREATE DOUBLE WORD STORAGE FOR PACKED DECIMAL            
UNPACKD  DS    CL16    CREATE STORAGE FOR UNPACKED DECIMAL                      
FULL     DS    25F     CREATE       STORAGE FOR FIB NUMBERS                     
******************************************************                          
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'058ANATFIB1  09/11/00'                                      
         END                                                                    
