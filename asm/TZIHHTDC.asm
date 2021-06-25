*          DATA SET TZIHHTDC   AT LEVEL 160 AS OF 09/15/00                      
*PHASE TZIHHTDC                                                                 
*INCLUDE  CARDS                                                                 
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
          USING OUTFRM,R4           PRINT OUT HEADER LINE                       
          LA    R4,P                                                            
          MVC   INPUT(7),=C'DECIMAL'                                            
          MVC   OUTPUT(11),=C'HEXADECIMAL'                                      
          L     RF,=V(PRINTER)                                                  
          BASR  RE,RF                                                           
          MVC   INPUT(7),=C'-------'                                            
          MVC   OUTPUT(11),=C'-----------'                                      
          L     RF,=V(PRINTER)                                                  
          BASR  RE,RF                                                           
          DROP  R4                                                              
*                                                                               
MLOOP     DS    0H                  OUTER LOOP FOR INPUT ENTRIES                
          GOTO1 =V(CARDS),DMCB,IO,=C'RE00'                                      
*         READ  NEXT ENTRY FROM      CARDS                                      
          CLC   IO,=C'/*'           CHECK FOR TERMINATOR                        
          BE    FIN                 IF /* - NO MORE INPUT - FINISH              
          LA    R2,0                INITIALIZE RESULT REGISTER                  
          LA    R6,IO               R6 POINTS TO INDIVIDUAL CHARACHERS          
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
          CLI   0(R6),X'40'         IF CHARACTER IS NOT A SPACE, INVAL          
          BNE   TOOLONG                                                         
*                                                                               
PRNT      DS    0H                                                              
          USING OUTFRM,R4                                                       
          LA    R4,P                                                            
          EDIT  (R2),(10,OUTPUT),ALIGN=LEFT                                     
          MVC   INPUT(L'INPUT),IO                                               
          L     RF,=V(PRINTER)                                                  
          BASR  RE,RF                                                           
          DROP  R4                                                              
          B     OVER                                                            
*                                                                               
TOOLONG   DS    0H                                                              
          USING OUTFRM,R4                                                       
          MVC   OUTPUT(14),=C'INPUT TOO LONG'                                   
          MVC   INPUT(L'INPUT),IO                                               
          L     RF,=V(PRINTER)                                                  
          BASR  RE,RF                                                           
          DROP  R4                                                              
          B     OVER                                                            
*                                                                               
INVALID   DS    0H                                                              
          USING OUTFRM,R4                                                       
          LA    R4,P                                                            
          MVC   OUTPUT(13),=C'INVALID INPUT'                                    
          MVC   INPUT(L'INPUT),IO                                               
          L     RF,=V(PRINTER)                                                  
          BASR  RE,RF                                                           
          DROP  R4                                                              
*                                                                               
OVER      DS    0H                                                              
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
DMCB      DS    6F                                                              
TMP       DS    F                                                               
FULL      DS    F                                                               
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
IO        DS    80C                                                             
******************************************************                          
* IF YOU WISH TO DEFINE ANY STORAGE OR CONSTANTS,                               
* JUST APPEND THEM TO THE LIST ABOVE (PUT THEM HERE)                            
******************************************************                          
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE TZIHDSCT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'160TZIHHTDC  09/15/00'                                      
         END                                                                    
