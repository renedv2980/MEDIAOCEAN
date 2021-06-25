*          DATA SET TZIHHTOD   AT LEVEL 102 AS OF 09/12/00                      
*PHASE TZIHHTOD                                                                 
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
          LA    R3,NTAB             R3 POINTS TO BEGINNING OF NUM TABLE         
          XC    P,P                 CLEAR THE PRINT LINE                        
*                                                                               
MLOOP     DS    0H                  OUTER LOOP FOR TABLE ENTRIES                
          CLI   0(R3),X'FF'         IF CHARACTER IS FF - END OF TABLE           
          BE    FIN                                                             
          LR    R4,R3               MAKE R4 ADDRESS NEXT BYTE TO EXAM           
          LA    R2,0                INITIALIZE RESULT REGISTER                  
          LA    R5,8                CHARACTER COUNTER                           
LOOP      DS    0H                  INNER LOOP, FOR INDIVIDUAL CHARS            
*                                                                               
          CLI   0(R4),X'40'         IF CHARACTER IS A SPACE = DONE              
          JE    DONE                                                            
          CLI   0(R4),X'C1'                                                     
          BL    INVALID             IF EBCDIC CODE < C1, THEN INVALID           
          CLI   0(R4),X'C6'         IF CODE >C6 COULD BI DIGIT                  
          BNH   CHAR                IF <C6, THEN DEFINITELY CHAR                
          CLI   0(R4),X'F0'                                                     
          BL    INVALID             IF < F0, THEN INVALID                       
          CLI   0(R4),X'F9'                                                     
          BH    INVALID             IF > F9, THEN INVALID                       
*                                                                               
*                                   AT THIS POINT DEFINITELY A DIGIT            
          XC    TMP,TMP             INITIALIZE TMP TO 0'S                       
          MVC   TMP+3(1),0(R4)      STORE FIRST BIT IN TMP                      
          NI    TMP+3,X'0F'         SET FIRST NIBBLE TO 0'S                     
          SLL   R2,4                SHIFT LEFT CONTENTS OF REGISTER             
          O     R2,TMP              INSERT THE NIBBLE INTO REGISTER             
          B     CONT                SKIP CHARACTER CONVERSION PART              
*                                                                               
CHAR      DS    0H                                                              
          XC    TMP,TMP             INITIALIZE TMP TO 0'S                       
          SR    R6,R6               INITIALIZE REGISTER TO 0                    
          IC    R6,0(R4)            INSERT EBCDIC CHARACTER                     
          AHI   R6,9                ADD NINE TO CONVERT TO BINARY               
          STC   R6,TMP+3            PUT THE OBTAINED NO TO TMP                  
          NI    TMP+3,X'0F'         SET FIRST NIBBLE TO 0'S                     
          SLL   R2,4                SHIFT LEFT CONTENTS OF REGISTER             
          O     R2,TMP              INSERT THE NIBBLE INTO REGISTER             
*                                                                               
CONT      DS    0H                                                              
          AHI   R4,1                ADVANCE TO NEXT CHARACTER                   
          BCT   R5,LOOP                                                         
*                                                                               
DONE      DS    0H                                                              
          C     R5,=F'0'            IF R5 IS 0, END OF THE NUMBER               
          BE    PRNT                                                            
          BCTR  R5,0                                                            
CHKCP     DS    0H                                                              
          AHI   R4,1                ADVANCE TO NEXT CHARACTER                   
          CLI   0(R4),X'40'         IF CHARACTER IS NOT A SPACE, INVAL          
          BNE   INVALID                                                         
          BCT   R5,CHKCP                                                        
                                                                                
PRNT      DS    0H                                                              
          CVD   R2,PACD             CONVERT TO DECIMAL AND PRINT                
          UNPK  P(16),PACD                                                      
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
          AHI   R3,L'NTAB           ADVANCE TO NEXT ENTRY IN THE TABLE          
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
PACD      DS    D                                                               
DMCB     DS    6F                                                               
TMP       DS    F                                                               
NTAB      DC    0CL8                                                            
          DC    CL8'1F2E3D4C'                                                   
          DC    CL8'123'                                                        
          DC    CL8'ABCD 12'                                                    
          DC    CL8'1A2B3C4D'                                                   
          DC    CL8'BLAH'                                                       
          DC    CL8'1F2E3D4C'                                                   
          DC    CL8'2+2'                                                        
          DC    X'FF'                                                           
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
**PAN#1  DC    CL21'102TZIHHTOD  09/12/00'                                      
         END                                                                    
