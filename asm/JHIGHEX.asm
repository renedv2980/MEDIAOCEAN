*          DATA SET JHIGHEX    AT LEVEL 132 AS OF 09/19/00                      
*PHASE JHIGHEX                                                                  
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
LIMIT    EQU   6                                                                
         LA    R2,17            TO BE USED AS COUNTER FOR MYTABLE               
         LA    R3,0             COUNTER KEEPS TRACK OF LENGTH OF INPUT          
         LA    R4,INTAB         USED TO STEP THROUGH INTAB                      
         LA    R5,MYTABLE       USED TO STEP THOUGH MYTABLE                     
         SR    R6,R6            CLEAR R6 TO USE TO STORE NUMBER                 
         SR    R7,R7                                                            
         L     RF,=V(PRINTER)                                                   
         USING HEXDEC,R5                                                        
*                                                                               
TERM     CLI   0(R4),X'FF'     CHECK FOR TERMINAL VALUE                         
         BE    DONE                                                             
*                                                                               
VERIFY   CLC   0(1,R4),CHAR     COMPARE INPUT CHAR TO TABLE CHAR                
         BNE   NEXT                                                             
         CLI   0(R4),C' '       CHECK IF CHARACTER IS SPACE                     
         BE    PRINTDEC                                                         
         MHI   R6,16            MULTIPLY R6 BY 16                               
         ZIC   R9,HEX           PUT VALUE IN R9                                 
         AR    R6,R9            ADD TO R6                                       
         AHI   R3,1             INCREASE LENGTH VALUE OF INPUT                  
         AHI   R4,1             GO TO NEXT CHARACTER                            
         LA    R5,MYTABLE       GO BACK TO TOP OF MYTABLE                       
         LA    R2,17            REINIT COUNTER                                  
         CHI   R3,LIMIT         CHECK TO SEE IF END OF NUMBER REACHED           
         BE    PRINTDEC         BRANCH TO SPACE IF IT IS                        
         B     VERIFY                                                           
*                                                                               
NEXT     AHI   R5,2             COMPARE TO NEXT CHAR IN TABLE                   
         BCT   R2,VERIFY        INVALID CHARACTER ENTERED                       
*                                                                               
INVALID  MVC   P(25),=C'INVALID CHARACTER ENTERED'                              
         BASR  RE,RF                                                            
         SR    R4,R3                                                            
         AHI   R4,6                                                             
         SR    R6,R6                                                            
         SR    R3,R3                                                            
         LA    R2,17                                                            
         LA    R5,MYTABLE                                                       
         B     TERM                                                             
*                                                                               
PRINTDEC CVD   R6,DUB                                                           
         UNPK  P(8),DUB                                                         
         OI    P+7,X'F0'                                                        
         BASR  RE,RF                                                            
         LA    R5,MYTABLE                                                       
         SR    R4,R3                                                            
         AHI   R4,6                                                             
         SR    R6,R6                                                            
         SR    R3,R3                                                            
         LA    R2,17                                                            
         B     TERM             GO BACK TO TOP OF LOOP                          
*                                                                               
DONE     DS    0H                                                               
*                                                                               
*                                                                               
* THE XBASE MACRO ENDS PROGRAM EXECUTION                                        
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
DMCB     DS    6F                                                               
DUB      DS    D                                                                
OUTP     DS    D                                                                
MYTABLE  DC    C'F',X'F'                                                        
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
BLANK    DC    C' '                                                             
*                                                                               
INTAB    DS    0CL6                                                             
         DC    C'127CB2'                                                        
         DC    C'F14BE3'                                                        
         DC    C'4B3XAC'                                                        
         DC    C'9BDE12'                                                        
         DC    C'A274C8'                                                        
         DC    X'FF'                                                            
******************************************************                          
* IF YOU WISH TO DEFINE ANY STORAGE OR CONSTANTS,                               
* JUST APPEND THEM TO THE LIST ABOVE (PUT THEM HERE)                            
******************************************************                          
         SPACE 3                                                                
         LTORG                                                                  
*                                                                               
       ++INCLUDE JHIGDSEC                                                       
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'132JHIGHEX   09/19/00'                                      
         END                                                                    
