*          DATA SET ANATHEX    AT LEVEL 043 AS OF 09/14/00                      
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
         USING HEXTABD,R3                                                       
         LA    R3,HEXTAB                                                        
         LA    R4,INTAB                                                         
LOOP1    DS    0H                                                               
         CLI   0(R4),X'FF'                                                      
INTAB    DS    0CL6                                                             
         DC    C'127CB2'                                                        
         DC    C'347EQ8'                                                        
         DC    C'FC356A'                                                        
         DC    X'FF'                                                            
*                                                                               
*                                   THE LOOKUP TABLE IS AS FOLLOWING            
*                                                                               
HEXTAB   DC    C'F',X'F'                                                        
         DC    C'E',X'E'                                                        
         DC    C'D',X'D'                                                        
         DC    C'C',X'C'                                                        
         DC    C'B',X'B'                                                        
         DC    C'A',X'A'                                                        
         DC    C'9',X'9'                                                        
         DC    C'8',X'8'                                                        
         DC    C'7',X'7'                                                        
         DC    C'6',X'6'                                                        
         DC    C'5',X'5'                                                        
         DC    C'4',X'4'                                                        
         DC    C'3',X'3'                                                        
         DC    C'2',X'2'                                                        
         DC    C'1',X'1'                                                        
         DC    C'0',X'0'                                                        
         DC    X'FF'                                                            
**********************                                                          
*                                                                               
*                                                                               
*                                                                               
* THE XBASE MACRO ENDS PROGRAM EXECUTION                                        
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
DMCB     DS    6F                                                               
PACKD    DS    D       CREATE DOUBLE WORD STORAGE FOR PACKED DECIMAL            
UNPACKD  DS    CL16    CREATE STORAGE FOR UNPACKED DECIMAL                      
******************************************************                          
* IF YOU WISH TO DEFINE ANY STORAGE OR CONSTANTS,                               
* JUST APPEND THEM TO THE LIST ABOVE (PUT THEM HERE)                            
*                                                                               
******************************************************                          
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
HEXTABD  DSECT                                                                  
CHAR     DS    C                                                                
HEX      DS    X                                                                
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043ANATHEX   09/14/00'                                      
         END                                                                    
