*          DATA SET TZIHHTDA   AT LEVEL 130 AS OF 09/14/00                      
*PHASE TZIHHTDA                                                                 
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
MAIN      DS    0H                                                              
*                                                                               
          LA    R4,NTAB             R4 POINTS TO NEXT NUM TO CONVERT            
*                                                                               
MLOOP     DS    0H                  OUTER LOOP FOR TABLE ENTRIES                
          CLI   0(R4),X'FF'         IF CHAR=FF - END OF INPUT TABLE             
          BE    FIN                                                             
          LA    R2,0                INITIALIZE RESULT REGISTER                  
          LR    R6,R4               R6 IS USED TO WALK CHARACTERS               
*                                                                               
          LA    R5,8                CHARACTER COUNTER FOR LOOP                  
LOOP      DS    0H                  INNER LOOP, FOR INDIVIDUAL CHARS            
          CLI   0(R6),X'40'         IF CHARACTER IS A SPACE = DONE              
          BE    DONE                                                            
*                                                                               
          USING LKTABD,R3           NOW CHAR CAN BE USED                        
          LA    R3,LKTAB            R3 POINTS TO LOOKUP TABLE                   
TLOOP     DS    0H                  LOOP TO WALK LOOKUP TABLE                   
          CLC   CHAR,X'FF'                                                      
          BE    INVALID                                                         
          CLC   CHAR,0(R6)                                                      
          BE    INSERT                                                          
          AHI   R3,2                ADVANCE TO NEXT ENTRY IN LKTABLE            
          B     TLOOP                                                           
*                                                                               
INSERT    DS    0H                                                              
          XC    FULL,FULL           INITIALIZE FULL TO ZEROS                    
          MVC   FULL+3(1),HEX       STORE HEX VALUE IN FULLWORD                 
          SLL   R2,4                SHIFT REGISTER LEFT                         
          O     R2,FULL             INSERT VALUE INTO LOWEST 4 BITS             
          AHI   R6,1                ADVANCE TO NEXT CHARACTER                   
          BCT   R5,LOOP             UPDATE CHARACTER COUNT                      
*                                                                               
DONE      DS    0H                                                              
          BCTR  R5,0                                                            
          C     R5,=F'0'            IF R5 IS 0, END OF THE NUMBER               
          BE    PRNT                PRINT IT                                    
*                                                                               
CHKCP     DS    0H                                                              
          AHI   R6,1                ADVANCE TO NEXT CHARACTER                   
          CLI   0(R6),X'40'         IF CHARACTER IS NOT A SPACE, INVAL          
          BNE   INVALID                                                         
          BCT   R5,CHKCP                                                        
*                                                                               
PRNT      DS    0H                                                              
*         EDIT  (R2),(32,P)                                                     
          CVD   R2,DUB                                                          
          UNPK  P(16),DUB                                                       
          OC    P+15(1),=X'F0'                                                  
          L     RF,=V(PRINTER)                                                  
          BASR  RE,RF                                                           
          B     OVER                                                            
*                                                                               
INVALID   DS    0H                                                              
          MVC   P(13),=C'INVALID INPUT'                                         
          L     RF,=V(PRINTER)                                                  
          BASR  RE,RF                                                           
*                                                                               
OVER      DS    0H                                                              
          AHI   R4,L'NTAB           ADVANCE TO NEXT ENTRY IN THE TABLE          
          B     MLOOP                                                           
*                                                                               
FIN       DS    0H                                                              
**********************                                                          
*                                                                               
*                                                                               
* THE XBASE MACRO ENDS PROGRAM EXECUTION                                        
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
DUB       DS    D                                                               
DMCB     DS    6F                                                               
TMP       DS    F                                                               
FULL      DS    F                                                               
NTAB      DC    0CL8                                                            
          DC    CL8'1A2B3C'                                                     
          DC    CL8'BLAH'                                                       
          DC    CL8'ABCDEF'                                                     
          DC    CL8'123 ABC'                                                    
          DC    X'FF'                                                           
LKTAB     DC    0H                                                              
          DC    C'0',X'0'                                                       
          DC    C'1',X'1'                                                       
          DC    C'2',X'2'                                                       
          DC    C'3',X'3'                                                       
          DC    C'4',X'4'                                                       
          DC    C'5',X'5'                                                       
          DC    C'6',X'6'                                                       
          DC    C'7',X'7'                                                       
          DC    C'8',X'8'                                                       
          DC    C'9',X'9'                                                       
          DC    C'A',X'A'                                                       
          DC    C'B',X'B'                                                       
          DC    C'C',X'C'                                                       
          DC    C'D',X'D'                                                       
          DC    C'E',X'E'                                                       
          DC    C'F',X'F'                                                       
          DC    X'FF'                                                           
*                                                                               
HALF      DS    H                                                               
WORK      DS    CL64                                                            
******************************************************                          
* IF YOU WISH TO DEFINE ANY STORAGE OR CONSTANTS,                               
* JUST APPEND THEM TO THE LIST ABOVE (PUT THEM HERE)                            
******************************************************                          
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
LKTABD    DSECT                                                                 
CHAR      DS    C                                                               
HEX       DS    X                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'130TZIHHTDA  09/14/00'                                      
         END                                                                    
