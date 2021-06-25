*          DATA SET TZIHDATF   AT LEVEL 177 AS OF 09/25/00                      
*PHASE TZIHDATF                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE  CARDS                                                                 
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
*                                                                               
MAIN50    DS    0H                  ONE ITERATION PER INPUT CARD                
          GOTO1 =V(CARDS),DMCB,IO,=C'RE00'                                      
          CLC   IO(2),=C'/*'        SEE IF END OF INPUT                         
          BE    EMAIN50                                                         
*                                                                               
          XC    VALUES(16),VALUES   INITIALIZE INPUT TABLE TO 0S                
          LA    R2,IO               POINT R2 TO FIRST INPUT CHAR                
MAIN20    DS    0H                  ONE ITERATION PER KEYWORD=VALUE             
          CLI   0(R2),C' '                                                      
          BE    EMAIN20             SPACE INDICATES END OF INPUT                
          MVC   WORK,MYSPACES       INITIALIZE WORK TO SPACES                   
          BAS   RE,READPAIR         CALL PAIR-READING SUB                       
          BAS   RE,GETVAL           PUT VALUE INTO WORK2                        
          BAS   RE,ETOB             CONVERT EBCDIC TO BINARY                    
          BAS   RE,INSERT           STORE BINARY VALUE                          
          B     MAIN20                                                          
EMAIN20   DS    0H                                                              
*                                                                               
          CLC   YEAR1,=F'0'         SEE IF ALL VALUES HAVE BEEN                 
          BE    ERROR               READ IN                                     
          CLC   DAY1,=F'0'                                                      
          BE    ERROR                                                           
          CLC   YEAR2,=F'0'                                                     
          BE    ERROR                                                           
          CLC   DAY2,=F'0'                                                      
          BE    ERROR                                                           
*                                                                               
          MVC   FULL,YEAR1          GET NUMBER OF DAYS IN YEAR1                 
          BAS   RE,NDAYS                                                        
          CLC   FULL,DAY1                                                       
          BL    ERROR                                                           
          MVC   FULL,YEAR2                                                      
          BAS   RE,NDAYS                                                        
          CLC   FULL,DAY2                                                       
          BL    ERROR                                                           
*                                                                               
          LA    R3,P                                                            
          USING OUTD,R3             OUTD - TO FORMAT OUTPUT                     
*                                                                               
          EDIT  DAY1,(4,P)                                                      
          MVI   P+5,C'/'            INSERT SLASH                                
          EDIT  YEAR1,(4,P+6),ALIGN=LEFT                                        
          EDIT  DAY2,(4,P+13)                                                   
          MVI   P+17,C'/'           INSERT SLASH                                
          EDIT  YEAR2,(4,P+18),ALIGN=LEFT                                       
          L     RF,=V(PRINTER)                                                  
          BASR  RE,RF                                                           
*                                                                               
          MVC   FULL,YEAR1          MOVE YEAR1,DAY1 INTO FULL,FULL2             
          MVC   FULL2,DAY1                                                      
          BAS   RE,CONVDAT          CONVERT DATE                                
          EDIT  FULL2,(2,OD1)                                                   
          EDIT  YEAR1,(4,OY1),ALIGN=LEFT                                        
          MVI   OSL1,C'/'                                                       
          MVC   OM1,WORK2                                                       
*                                                                               
          MVC   FULL,YEAR2          MOVE YEAR2,DAY2 INTO FULL,FULL2             
          MVC   FULL2,DAY2                                                      
          BAS   RE,CONVDAT          CONVERT DATE                                
          EDIT  FULL2,(2,OD2)                                                   
          EDIT  YEAR2,(4,OY2),ALIGN=LEFT                                        
          MVI   OSL2,C'/'                                                       
          MVC   OM2,WORK2                                                       
*                                                                               
          MVC   FULL,YEAR2                                                      
          MVC   FULL2,DAY2                                                      
          BAS   RE,ABSDAT           CALCULATE END ABSOLUTE DATE                 
          L     R9,FULL             LOAD ABSOLUTE DATE INTO R9                  
          MVC   FULL,YEAR1          MOVE YEAR1,DAY1 INTO FULL,FULL2             
          MVC   FULL2,DAY1                                                      
          BAS   RE,ABSDAT           CALCULATE START ABSOLUTE DATE               
          S     R9,FULL             FIND DIFFERENCE                             
*                                                                               
          EDIT  (R9),(8,ODIFF),ALIGN=LEFT,MINUS=YES                             
*                                                                               
          L     RF,=V(PRINTER)                                                  
          BASR  RE,RF                                                           
          MVC   P(20),=20C'-'                                                   
          BASR  RE,RF                                                           
EMAIN     DS    0H                                                              
          B     MAIN50                                                          
EMAIN50   DS    0H                                                              
**********************                                                          
*                                                                               
*                                                                               
* THE XBASE MACRO ENDS PROGRAM EXECUTION                                        
*                                                                               
         XBASE                                                                  
*                                                                               
READPAIR  DS    0H                  ASSUMING R2 POINTS TO IO                    
*         PRE:  R2 POINTS TO FIRST  CHARACTER IN KEYWORD=VALUE                  
*               STATEMENT TO BE REA D IN                                        
*         POST: KEYWORD-VALUE PAIR  IS COPIED INTO WORK                         
*               R2 POINTS TO FIRST  CHARACTER IN NEXT KEYYWORD=VALUE            
*               PAIR                                                            
*               R4,R5 ARE USED                                                  
          LA    R5,WORK                                                         
          LA    R4,11               CHARACTER COUNTER                           
READP10   DS    0H                  ASSUMING R2 POINTS TO IO                    
          CLI   0(R2),C','                                                      
          BE    EREADP10                                                        
          CLI   0(R2),C' '                                                      
          BE    EREADP10                                                        
          MVC   0(1,R5),0(R2)       COPY CHARACTER TO TEMP LOC.                 
          AHI   R2,1                                                            
          AHI   R5,1                                                            
          BCT   R4,READP10                                                      
          B     ERROR               IF COUNTER EXCEEDS 10, INVALID INPT         
EREADP10  DS    0H                                                              
          AHI   R2,1                POINT R2 TO BEGINNING OF NEXT PAIR          
          BR    RE                                                              
*                                                                               
GETVAL    DS    0H                                                              
*         PRE:  KEYWORD=VALUE PAIR  IS IN MEMORY LOCATION WORK                  
*         POST: WORK HAS THE KEYWOR D=VALUE PAIR                                
*               WORK2 HAS VALUE IN  EBCDIC                                      
*               R4,R5,R6 ARE USED                                               
          MVC   WORK2,MYSPACES      INITIALIZE WORK2 TO SPACES                  
          LA    R4,WORK2            R4 POINTS TO OUTPUT                         
          LA    R5,WORK             R5 POINTS TO WORK                           
          LA    R6,7                CHARACTER COUNTER                           
GETVAL10  DS    0H                  GET TO EQUAL SIGN                           
          CLI   0(R5),C'='                                                      
          BE    GETVAL20                                                        
          AHI   R5,1                                                            
          BCT   R6,GETVAL10         IF MORE THAN 6 CHARACTERS READ, BUT         
          B     ERROR               NO '=' SIGN - THAT'S AN ERROR               
GETVAL20  DS    0H                                                              
          AHI   R5,1                NOW R5 POINTS TO FIRST VALUE BYTE           
GETVAL30  CLI   0(R5),C' '          WORK IS INITIALIZED TO SPACES, SO           
          BE    EGETVAL             TERMINATION IS GUARANTEED                   
          CLI   0(R5),C'0'          CHECKING FOR INVALID CHARACTERS             
          BL    ERROR               NON-DIGITS ARE UNACCEPTABLE                 
          CLI   0(R5),C'9'                                                      
          BH    ERROR                                                           
          MVC   0(1,R4),0(R5)       MOVE CHARACTER TO WORK2                     
          AHI   R5,1                                                            
          AHI   R4,1                                                            
          B     GETVAL30                                                        
EGETVAL   DS    0H                  WORK2 NOW HAS VALUE FOR THE KEY             
          BR    RE                                                              
*                                                                               
ETOB      DS    0H                                                              
*         PRE:  WORK2 HAS A NUMBER  IN EBCDIC                                   
*         POST: FULL HAS BINARY VAL UE FOR THAT NUMBER                          
*         R4-R6  ARE USED FOR CALCU LATIONS                                     
          XC    FULL,FULL                                                       
          LA    R5,WORK2                                                        
          SR    R4,R4               R4 HAS THE RESULT                           
          SR    R6,R6                                                           
ETOB10    DS    0H                                                              
          CLI   0(R5),C' '                                                      
          BE    EGETS                                                           
          NI    0(R5),X'0F'                                                     
          MHI   R4,10                                                           
          IC    R6,0(R5)                                                        
          AR    R4,R6                                                           
          AHI   R5,1                                                            
          B     ETOB10                                                          
EGETS     DS    0H                                                              
          ST    R4,FULL                                                         
          BR    RE                                                              
*                                                                               
INSERT    DS    0H                                                              
*         PRE:  FULL HAS BINARY VAL UE WHICH IS TO INSERT                       
*               WORK STARTS WITH TH E KEYWORD                                   
*         POST: BINARY VALUE IS INS ERTED INTO ONE OF THE                       
*               FULL WORDS IN VALUE S,DEPENDING ON THE KEYWORD                  
*               R4 IS USED                                                      
          ST    RE,SAVERE                                                       
          LA    R4,YEAR1                                                        
                                                                                
          CLC   =C'YEAR1',WORK      CHECK IF KEYWORD IS YEAR1                   
          BNE   INS10               IF NOT, GO TO NEXT CHECK                    
          BAS   RE,VALYR            IF YES, VALIDATE INPUT                      
          CLC   YEAR1,=F'0'         IF YEAR1 IS NOT ZERO -                      
          BNE   ERROR                    IT'S BEING READ IN SECOND TIME         
          MVC   YEAR1,FULL          IF OK, PLACE IT INTO STORAGE AREA           
          B     EINSERT             GO TO END OF THE ROUTINE                    
                                                                                
INS10     CLC   =C'YEAR2',WORK                                                  
          BNE   INS20                                                           
          BAS   RE,VALYR                                                        
          CLC   YEAR2,=F'0'         IF YEAR2 IS NOT ZERO -                      
          BNE   ERROR                    IT'S BEING READ IN SECOND TIME         
          MVC   YEAR2,FULL                                                      
          B     EINSERT                                                         
                                                                                
INS20     CLC   =C'DAY1',WORK                                                   
          BNE   INS30                                                           
          BAS   RE,VALDY                                                        
          CLC   DAY1,=F'0'          IF DAY1 IS NOT ZERO -                       
          BNE   ERROR                    IT'S BEING READ IN SECOND TIME         
          MVC   DAY1,FULL                                                       
          B     EINSERT                                                         
                                                                                
INS30     CLC   =C'DAY2',WORK                                                   
          BNE   ERROR                                                           
          BAS   RE,VALDY                                                        
          CLC   DAY2,=F'0'          IF DAY2 IS NOT ZERO -                       
          BNE   ERROR                    IT'S BEING READ IN SECOND TIME         
          MVC   DAY2,FULL                                                       
          B     EINSERT                                                         
                                                                                
EINSERT   DS    0H                                                              
          L     RE,SAVERE                                                       
          BR    RE                                                              
*                                                                               
NDAYS     DS    0H                                                              
*         PRE:  FULL CONTAINS BINAR Y VALUE OF THE YEAR                         
*         POST: FULL CONTAINS BINAR Y VALUE OF # OF DAYS IN THAT YEAR           
*               R4-R6 ARE USED FOR  CALCULATIONS                                
*                                                                               
          SR    R4,R4               LOAD YEAR NUMBER INTO R4,R5 -               
          L     R5,FULL             EVEN-ODD REGISTER PAIR                      
          LA    R6,400                                                          
          DR    R4,R6               DIVIDE BY 400                               
          LTR   R4,R4               IF DIVISIBLE BY 400 - LEAP                  
          BZ    NDLEAP                                                          
          SR    R4,R4               LOAD YEAR NUMBER INTO R4,R5                 
          L     R5,FULL                                                         
          LA    R6,4                                                            
          DR    R4,R6               DIVIDE BY 4                                 
          LTR   R4,R4               IF NOT DIVISIBLE BY 4 - NOT LEAP            
          BNZ   NDNLEAP             IF DIVISIBLE, HAVE TO CHECK FOR 100         
          SR    R4,R4               LOAD YEAR NUMBER INTO R4,R5 AGAIN           
          L     R5,FULL                                                         
          LA    R6,100              DIVIDE BY 100                               
          DR    R4,R6                                                           
          LTR   R4,R4                                                           
          BZ    NDNLEAP             DIVISIBLE BY 100 - NOT LEAP                 
NDLEAP    DS    0H                                                              
          MVC   FULL,=F'366'                                                    
          B     ENDAYS                                                          
NDNLEAP   DS    0H                                                              
          MVC   FULL,=F'365'                                                    
ENDAYS    DS    0H                                                              
          BR    RE                                                              
*                                                                               
ABSDAT    DS    0H                                                              
*         PRE:  FULL, FULL2 CONTAIN YEAR,DAY RESPECTIVELY                       
*         POST: FULL CONTAINS ABSOL UTE NUMBER OF DAYS SINCE MOMENT 0           
*         WORK  WORK2 R7,R8ARE USED                                             
          MVC   WORK(4),FULL        STORE YEAR IN WORK                          
          LA    R7,1                R7 COUNTS YEARS STARTING FROM 1             
          SR    R8,R8               R8 HAS TOTAL NUMBER OF DAYS                 
ABSLOOP   DS    0H                                                              
          C     R7,WORK             SEE IF COUNTER IS EQUAL TO THE YEAR         
          BNL   EABSLOOP                                                        
          ST    R7,FULL             STORE COUNTER IN FULL                       
*                                                                               
          ST    RE,SAVERE                                                       
          BAS   RE,NDAYS            NOW FULL HAS # DAYS IN THAT YEAR            
          L     RE,SAVERE                                                       
*                                                                               
          A     R8,FULL                                                         
          AHI   R7,1                                                            
          B     ABSLOOP                                                         
EABSLOOP  DS    0H                                                              
          A     R8,FULL2            ADD DAYS                                    
          ST    R8,FULL             FULL HAS THE ABSOLUTE DATE                  
          BR    RE                                                              
*                                                                               
CONVDAT   DS    0H                                                              
*         PRE:  FULL,FULL2 HAVE YEA R AND DAY                                   
*         POST: WORK2 HAS MONTH NAM E                                           
*               FULL2 HAS DAY                                                   
*         R4,R5 ,R6 ARE USED                                                    
          MVI   MVALS+11,28         MAKE SURE VALUE FOR FEB IS 28               
          ST    RE,SAVERE                                                       
          BAS   RE,NDAYS            NOW FULL HAS # DAYS IN THAT YEAR            
          L     RE,SAVERE                                                       
          CLC   FULL,=F'365'        IF NOT LEAP YEAR, SKIP NEXT LINE            
          BE    *+8                                                             
          MVI   MVALS+11,29         IF LEAP - MAKE FEB=29                       
*                                                                               
          LA    R4,MVALS            POINTS TO LOOKUP TABLE                      
          L     R5,FULL2            R5 HAS # OF DAYS                            
          SR    R6,R6               # OF DAYS FROM JAN TO CURNT MONTH           
CONLOOP   DS    0H                                                              
          A     R6,0(R4)                                                        
          CR    R5,R6                                                           
          BNH   ECONVDAT                                                        
          AHI   R4,8                POINTS TO NEXT # OF DAYS IN TABLE           
          B     CONLOOP                                                         
*                                                                               
ECONVDAT  DS    0H                                                              
          MVC   WORK2(4),4(R4)                                                  
          S     R6,0(R4)                                                        
          SR    R5,R6                                                           
          ST    R5,FULL2                                                        
          BR    RE                                                              
*                                                                               
ERROR     DS    0H                                                              
          MVC   P(20),=CL20'INVALID INPUT'                                      
          L     RF,=V(PRINTER)                                                  
          BASR  RE,RF                                                           
          LA    RE,EMAIN                                                        
          BR    RE                                                              
*                                                                               
VALYR     DS    0H                                                              
*         PRE:  FULL HAS THE BINARY  VALUE TO BE CHECKED                        
*         POST: IF IT IS OUT OF RAN GE, ERROR IS CALLED                         
*         R5 IS  USED                                                           
          CLC   FULL,=F'1'                                                      
          BL    ERROR                                                           
          CLC   FULL,=F'2500'                                                   
          BH    ERROR                                                           
          BR    RE                                                              
*                                                                               
VALDY     DS    0H                                                              
*         PRE:  FULL HAS THE BINARY  VALUE TO BE CHECKED                        
*         POST: IF IT IS OUT OF RAN GE, ERROR IS CALLED                         
*         R5 IS  USED                                                           
          CLC   FULL,=F'1'                                                      
          BL    ERROR                                                           
          CLC   FULL,=F'366'                                                    
          BH    ERROR                                                           
          BR    RE                                                              
*                                                                               
DUB       DS    D                                                               
SAVERE    DS    F                                                               
FULL      DS    F                                                               
FULL2     DS    F                                                               
FEB       DC    F'28'                                                           
DMCB     DS    6F                                                               
VALUES    DS    0C                                                              
YEAR1     DS    F                                                               
DAY1      DS    F                                                               
YEAR2     DS    F                                                               
DAY2      DS    F                                                               
                                                                                
HALF      DS    H                                                               
MVALS     DC    F'31',C'JAN '                                                   
          DC    F'28',C'FEB '                                                   
          DC    F'31',C'MAR '                                                   
          DC    F'30',C'APR '                                                   
          DC    F'31',C'MAY '                                                   
          DC    F'30',C'JUN '                                                   
          DC    F'31',C'JUL '                                                   
          DC    F'31',C'AUG '                                                   
          DC    F'30',C'SEP '                                                   
          DC    F'31',C'OCT '                                                   
          DC    F'30',C'NOV '                                                   
          DC    F'31',C'DEC '                                                   
WORK      DS    CL64                                                            
WORK2     DS    CL64                                                            
MYSPACES  DC    80C' '                                                          
IO        DS    CL80                                                            
******************************************************                          
* IF YOU WISH TO DEFINE ANY STORAGE OR CONSTANTS,                               
* JUST APPEND THEM TO THE LIST ABOVE (PUT THEM HERE)                            
******************************************************                          
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
*                                                                               
OUTD      DSECT                                                                 
OD1       DS    CL2                                                             
OM1       DS    CL3                                                             
OSL1      DS    CL1                                                             
OY1       DS    CL6                                                             
OD2       DS    CL2                                                             
OM2       DS    CL3                                                             
OSL2      DS    CL1                                                             
OY2       DS    CL9                                                             
ODIFF     DS    CL3                                                             
*                                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'177TZIHDATF  09/25/00'                                      
         END                                                                    
