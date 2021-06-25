*          DATA SET JHIGDATE   AT LEVEL 235 AS OF 10/02/00                      
*PHASE JHIGDATE                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
         TITLE 'JHIGDATE -- DATE CONVERSION'                                    
***********************************************************************         
         EJECT                                                                  
* LEAVE THIS CODE ALONE                                                         
*                                                                               
DATECONV CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*DATECONV,=V(REGSAVE)                                          
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN10                                                           
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(DATECONV),V(DUMMY)                                             
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
YRLENG   EQU   4                                                                
DAYLENG  EQU   3                                                                
*                                                                               
GET      GOTO1 =V(CARDS),DMCB,IOAREA,=C'RE00'                                   
         CLC   IOAREA(2),=C'/*'                                                 
         BE    DONE                                                             
         LA    R5,0                      USED TO HOLD YEAR1                     
         LA    R6,0                      USED TO HOLD YEAR2                     
         LA    R7,0                      USED TO HOLD DAY1                      
         LA    R8,0                      USED TO HOLD DAY2                      
         STC   R8,SIGNFLAG               INIT SIGNFLAG TO 0                     
         STC   R8,LEAPFLAG                                                      
         STC   R8,SAMEYR                                                        
         LA    R4,IOAREA                                                        
         MVC   P,0(R4)                                                          
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
         CLC   0(1,R4),=C'Y'                                                    
         BE    LOOK                                                             
         CLC   0(1,R4),=C'D'                                                    
         BNE   INVALID                                                          
*                                                                               
LOOK     DS    0H                                                               
         LA    R9,2                      ONLY WANT TO MAKE 2 PASSES             
*                                                                               
LOOK2    DS    0H                                                               
         SR    R2,R2                                                            
         CLC   0(6,R4),=C'YEAR1='                                               
         BE    YR1                                                              
         CLC   0(6,R4),=C'YEAR2='                                               
         BE    YR2                                                              
         CLC   0(5,R4),=C'DAY1='                                                
         BE    DY1                                                              
         CLC   0(5,R4),=C'DAY2='                                                
         BE    DY2                                                              
         CLC   0(1,R4),=C','                                                    
         BNE   INVALID                                                          
         AHI   R4,1                                                             
         BCT   R9,LOOK2                                                         
         B     INVALID                                                          
*                                                                               
YR1      DS    0H                                                               
         CLC   6(1,R4),=X'EF'            CHECK 1ST CHAR FOR VALIDITY            
         BNH   INVALID                                                          
         CLC   6(1,R4),=X'FA'                                                   
         BNL   INVALID                                                          
         AHI   R2,1                      INCREMENT COUNTER                      
         AHI   R4,1                      GO TO NEXT CHARACTER                   
*                                                                               
Y1       DS    0H                                                               
         CLC   6(1,R4),=X'EF'            CHECK FOR VALID CHARACTERS             
         BNH   GETYR1                                                           
         CLC   6(1,R4),=X'FA'                                                   
         BNL   GETYR1                                                           
         AHI   R2,1                      INCREMENT COUNTER                      
         AHI   R4,1                      GO TO NEXT CHARACTER                   
         B     Y1                                                               
*                                                                               
YR2      DS    0H                                                               
         CLC   6(1,R4),=X'EF'            CHECK 1ST CHARACTER                    
         BNH   INVALID                                                          
         CLC   6(1,R4),=X'FA'                                                   
         BNL   INVALID                                                          
         AHI   R2,1                      INCREMENT COUNTER                      
         AHI   R4,1                      GO TO NEXT CHARACTER                   
*                                                                               
Y2       DS    0H                                                               
         CLC   6(1,R4),=X'EF'            CHECK FOR VALID CHARACTERS             
         BNH   GETYR2                                                           
         CLC   6(1,R4),=X'FA'                                                   
         BNL   GETYR2                                                           
         AHI   R2,1                      INCREMENT COUNTER                      
         AHI   R4,1                      GO TO NEXT CHARACTER                   
         B     Y2                                                               
*                                                                               
DY1      DS    0H                                                               
         CLC   5(1,R4),=X'EF'            CHECK 1ST CHARACTER                    
         BNH   INVALID                                                          
         CLC   5(1,R4),=X'FA'                                                   
         BNL   INVALID                                                          
         AHI   R2,1                      INCREMENT COUNTER                      
         AHI   R4,1                      GO TO NEXT CHARACTER                   
*                                                                               
D1       DS    0H                                                               
         CLC   5(1,R4),=X'EF'            CHECK FOR VALID CHARACTERS             
         BNH   GETDAY1                                                          
         CLC   5(1,R4),=X'FA'                                                   
         BNL   GETDAY1                                                          
         AHI   R2,1                      INCREMENT COUNTER                      
         AHI   R4,1                      GO TO NEXT CHARACTER                   
         B     D1                                                               
*                                                                               
DY2      DS    0H                                                               
         CLC   5(1,R4),=X'EF'            CHECK 1ST CHARACTER                    
         BNH   INVALID                                                          
         CLC   5(1,R4),=X'FA'                                                   
         BNL   INVALID                                                          
         AHI   R2,1                      INCREMENT COUNTER                      
         AHI   R4,1                      GO TO NEXT CHARACTER                   
*                                                                               
D2       DS    0H                                                               
         CLC   5(1,R4),=X'EF'            CHECK FOR VALID CHARACTERS             
         BNH   GETDAY2                                                          
         CLC   5(1,R4),=X'FA'                                                   
         BNL   GETDAY2                                                          
         AHI   R2,1                      INCREMENT COUNTER                      
         AHI   R4,1                      GO TO NEXT CHARACTER                   
         B     D2                                                               
*                                                                               
GETYR1   DS    0H                                                               
         LA    R9,6                                                             
         SR    R9,R2                                                            
         AR    R4,R9                                                            
*                                                                               
GTY1     DS    0H                                                               
         ZIC   R9,0(R4)                  PUT NUMBER IN REGISTER                 
         SHI   R9,X'F0'                  CONVERT TO DECIMAL                     
         MHI   R5,10                                                            
         AR    R5,R9                                                            
         AHI   R4,1                                                             
         BCT   R2,GTY1                                                          
         B     NEXT                                                             
*                                                                               
GETYR2   DS    0H                                                               
         LA    R9,6                                                             
         SR    R9,R2                                                            
         AR    R4,R9                                                            
*                                                                               
GTY2     DS    0H                                                               
         ZIC   R9,0(R4)                  PUT NUMBER IN REGISTER                 
         SHI   R9,X'F0'                  CONVERT TO DECIMAL                     
         MHI   R6,10                                                            
         AR    R6,R9                                                            
         AHI   R4,1                                                             
         BCT   R2,GTY2                                                          
         B     NEXT                                                             
*                                                                               
GETDAY1  DS    0H                                                               
         LA    R9,5                                                             
         SR    R9,R2                                                            
         AR    R4,R9                                                            
*                                                                               
GTD1     DS    0H                                                               
         ZIC   R9,0(R4)                  PUT NUMBER IN REGISTER                 
         SHI   R9,X'F0'                  CONVERT TO DECIMAL                     
         MHI   R7,10                                                            
         AR    R7,R9                                                            
         AHI   R4,1                                                             
         BCT   R2,GTD1                                                          
         B     NEXT                                                             
*                                                                               
GETDAY2  DS    0H                                                               
         LA    R9,5                                                             
         SR    R9,R2                                                            
         AR    R4,R9                                                            
*                                                                               
GTD2     DS    0H                                                               
         ZIC   R9,0(R4)                  OUT NUMBER IN REGISTER                 
         SHI   R9,X'F0'                  CONVERT TO DECIMAL                     
         MHI   R8,10                                                            
         AR    R8,R9                                                            
         AHI   R4,1                                                             
         BCT   R2,GTD2                                                          
         B     NEXT                                                             
*                                                                               
NEXT     DS    0H                        CHECK IF ALL FIELDS ARE FILLED         
         LTR   R5,R5                                                            
         BZ    LOOK                                                             
         LTR   R6,R6                                                            
         BZ    LOOK                                                             
         LTR   R7,R7                                                            
         BZ    LOOK                                                             
         LTR   R8,R8                                                            
         BZ    LOOK                                                             
         CLC   0(1,R4),=C' '             LOOK FOR SPACE AT END                  
         BNE   INVALID                                                          
         CHI   R5,0                      CHECK FOR VALID YEARS                  
         BL    INVALID                                                          
         CHI   R5,2500                                                          
         BH    INVALID                                                          
         CHI   R6,0                                                             
         BL    INVALID                                                          
         CHI   R6,2500                                                          
         BH    INVALID                                                          
*                                                                               
DAY1TEST ST    R5,TESTLEAP                                                      
         BAS   RE,ISLEAP                 CHECK FOR VALID DAYS                   
         ZIC   R2,LEAPFLAG                                                      
         LTR   R2,R2                                                            
         BNZ   LEAPD1                                                           
*                                                                               
NOLEAPD1 DS    0H                                                               
         CHI   R7,365                                                           
         BH    INVALID                                                          
         B     DAY2TEST                                                         
*                                                                               
LEAPD1   DS    0H                                                               
         CHI   R7,366                                                           
         BH    INVALID                                                          
*                                                                               
DAY2TEST ST    R6,TESTLEAP                                                      
         BAS   RE,ISLEAP                                                        
         ZIC   R2,LEAPFLAG                                                      
         LTR   R2,R2                                                            
         BNZ   LEAPD2                                                           
*                                                                               
NOLEAPD2 DS    0H                                                               
         CHI   R8,365                                                           
         BH    INVALID                                                          
         B     CONT                                                             
*                                                                               
LEAPD2   DS    0H                                                               
         CHI   R8,366                                                           
         BH    INVALID                                                          
*                                                                               
CONT     DS    0H                                                               
         ST    R5,Y1STORE                                                       
         ST    R6,Y2STORE                                                       
         BAS   RE,CALC1                  CALCS DAYS IN ALL FULL YEARS           
         L     R3,DAYTOT                 LOAD DAY TOTAL                         
         ZIC   R9,SAMEYR                 SKIP STEPS IF SAME YEAR                
         LTR   R9,R9                                                            
         BNZ   SKIP                                                             
         BAS   RE,CALC2                  ADDS FIRST AND LAST YEAR               
         L     R3,DAYTOT                 LOAD DAY TOTAL                         
*                                                                               
SKIP     ST    R5,TESTLEAP                                                      
         ST    R7,D1STORE                                                       
         BAS   RE,CALC3                  CONVERTS DATE1 TO ROMAN                
*                                                                               
* PRINT ROMAN DATES                                                             
*                                                                               
         MVC   P(17),=C'CONVERTED OUTPUT:'                                      
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
         L     R9,DAY1ANS                                                       
         EDIT  (R9),(3,P)                                                       
         MVC   P+3(3),MONTH1                                                    
         MVC   P+6(1),=C'/'                                                     
         EDIT  (R5),(4,P+7),ALIGN=LEFT                                          
         ST    R6,TESTLEAP                                                      
         ST    R8,D1STORE                                                       
         BAS   RE,CALC3                  CONVERTS DATE2 TO ROMAN                
         L     R9,DAY1ANS                                                       
         EDIT  (R9),(3,P+13)                                                    
         MVC   P+16(3),MONTH1                                                   
         MVC   P+19(1),=C'/'                                                    
         EDIT  (R6),(4,P+20),ALIGN=LEFT                                         
         BASR  RE,RF                                                            
         L     R3,DAYTOT                                                        
         MVC   P(35),=C'NUMBER OF DAYS BETWEEN THESE DATES:'                    
         ZIC   R9,SAMEYR                                                        
         LTR   R9,R9                                                            
         BZ    SIGN                                                             
         CR    R7,R8                                                            
         BL    POS                                                              
         B     NEG                                                              
SIGN     ZIC   R9,SIGNFLAG                                                      
         LTR   R9,R9                                                            
         BZ    POS                                                              
NEG      MVC   P+36(1),=C'-'                                                    
POS      EDIT  (R3),(6,P+37),ALIGN=LEFT                                         
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
         BASR  RE,RF                                                            
         B     GET                                                              
*                                                                               
INVALID  MVC   P(21),=C'INVALID INPUT ENTERED'                                  
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                     PRINT ERROR MESSAGE                    
         BASR  RE,RF                                                            
         B     GET                       BRANCH TO BEGINNING                    
*                                                                               
DONE     DS    0H                                                               
*        DC    H'0'                                                             
*                                                                               
*                                                                               
*                                                                               
* THE XBASE MACRO ENDS PROGRAM EXECUTION                                        
*                                                                               
         XBASE                                                                  
*                                                                               
CALC1    DS    0H                                                               
         NTR1                                                                   
         SR    R3,R3                     CLEAR TOTAL                            
         L     R5,Y1STORE                                                       
         L     R6,Y2STORE                                                       
         CR    R5,R6                     CHECK FOR SAME YEAR                    
         BE    SAME                                                             
         CR    R5,R6                     CHECK FOR YEAR2 HIGH                   
         BL    C1                                                               
         L     R5,Y2STORE                                                       
         L     R6,Y1STORE                SWITCH YEARS IF YEAR1 HIGH             
         LA    R9,1                                                             
         STC   R9,SIGNFLAG                                                      
*                                                                               
C1       AHI   R5,1                      INCREASE YEAR1 BY 1                    
*                                                                               
YRLOOP   DS    0H                                                               
         LR    R9,R5                     MAKE COPY OF CURRENT YEAR              
         CR    R5,R6                     COMPARE TO YEAR2                       
         BE    REACHED                                                          
         ST    R5,TESTLEAP                                                      
         BAS   RE,ISLEAP                 CHECK FOR LEAP YEAR                    
         ZIC   R2,LEAPFLAG                                                      
         LTR   R2,R2                                                            
         BNZ   LEAP                                                             
         B     NOTLEAP                                                          
*                                                                               
LEAP     DS    0H                                                               
         AHI   R3,366                    ADD 366 TO TOTAL                       
         LR    R5,R9                                                            
         AHI   R5,1                      INCREMENT YEAR                         
         B     YRLOOP                                                           
*                                                                               
NOTLEAP  DS    0H                                                               
         AHI   R3,365                    ADD 365 TO TOTAL                       
         LR    R5,R9                                                            
         AHI   R5,1                      INCREMENT YEAR                         
         B     YRLOOP                                                           
*                                                                               
SAME     DS    0H                                                               
         SR    R8,R7                                                            
         LR    R3,R8                                                            
         LA    R2,1                                                             
         STC   R2,SAMEYR                                                        
*                                                                               
REACHED  DS    0H                                                               
         ST    R3,DAYTOT                 STORE TOTAL                            
         XIT1                                                                   
*                                                                               
CALC2    DS    0H                                                               
         NTR1                                                                   
         ZIC   R9,SIGNFLAG               CHECK FOR NEG YEARS                    
         LTR   R9,R9                                                            
         BZ    NORM                                                             
         L     R5,Y2STORE                SWITCH TO YEAR2                        
         LR    R9,R8                     SWITCH DAY1 AND DAY2                   
         LR    R8,R7                                                            
         LR    R7,R9                                                            
*                                                                               
NORM     ST    R5,TESTLEAP                                                      
         BAS   RE,ISLEAP                                                        
         ZIC   R2,LEAPFLAG                                                      
         LTR   R2,R2                                                            
         BNZ   YESLEAP                                                          
*                                                                               
NOLEAP   DS    0H                                                               
         LA    R9,365                                                           
         SR    R9,R7                                                            
         AR    R3,R9                                                            
         B     DONE1                                                            
*                                                                               
YESLEAP  DS    0H                                                               
         LA    R9,366                                                           
         SR    R9,R7                                                            
         AR    R3,R9                                                            
*                                                                               
DONE1    DS    0H                                                               
         AR    R3,R8                                                            
         ST    R3,DAYTOT                 ADDS LAST YEAR'S DAYS TO TOT           
         XIT1                                                                   
*                                                                               
CALC3    DS    0H                                                               
         NTR1                                                                   
         ST    R5,TESTLEAP                                                      
         BAS   RE,ISLEAP                                                        
         ZIC   R2,LEAPFLAG                                                      
         LTR   R2,R2                                                            
         BZ    NLEAP                                                            
*                                                                               
YLEAP    DS    0H                                                               
         LA    R9,LEAPTAB                                                       
         USING DAYMON,R9                                                        
         L     R8,D1STORE                                                       
         B     D1CALC                                                           
*                                                                               
NLEAP    DS    0H                                                               
         LA    R9,NORMTAB                                                       
         USING DAYMON,R9                                                        
         L     R8,D1STORE                                                       
*                                                                               
D1CALC   LH    R7,DAYD                                                          
         SR    R6,R6                                                            
         DR    R6,R8                                                            
         LTR   R7,R7                                                            
         BNZ   GETDATE                                                          
         LA    R9,8(R9)                                                         
         B     D1CALC                                                           
*                                                                               
GETDATE  DS    0H                                                               
         CHI   R8,32                                                            
         BL    JANDATE                                                          
         LH    R8,DAYSD                  LOAD DAYS IN MONTH INTO R8             
         SR    R8,R6                     SUBTRACT REMAINDER                     
         ST    R8,DAY1ANS                                                       
         B     ALLDATES                                                         
*                                                                               
JANDATE  ST    R8,DAY1ANS                SPECIAL CASE                           
*                                                                               
ALLDATES DS    0H                                                               
         L     R4,MONTHD                                                        
         ST    R4,MONTH1                                                        
         XIT1                                                                   
*                                                                               
ISLEAP   DS    0H                                                               
         NTR1                                                                   
         L     R5,TESTLEAP                                                      
         LR    R9,R5                     MAKE COPY OF YEAR                      
         SR    R4,R4                                                            
         LA    R2,4                                                             
         DR    R4,R2                     CHECK IF DIVISIBLE BY 4                
         LTR   R4,R4                                                            
         BNZ   NOLEAP1                   IF REMAINDER, NOT LEAP YEAR            
         LR    R5,R9                                                            
         SR    R4,R4                                                            
         LA    R2,100                                                           
         DR    R4,R2                     CHECK IF DIVISIBLE BY 100              
         LTR   R4,R4                                                            
         BNZ   YESLEAP1                  IF REMAINDER, IS LEAP YEAR             
         LR    R5,R9                                                            
         SR    R4,R4                                                            
         LA    R2,400                                                           
         DR    R4,R2                     CHECK IF DIVISIBLE BY 400              
         LTR   R4,R4                                                            
         BNZ   NOLEAP1                   IF REMAINDER, NOT LEAP YEAR            
*                                                                               
YESLEAP1 DS    0H                                                               
         LA    R2,1                                                             
         STC   R2,LEAPFLAG                                                      
         B     OUT                                                              
*                                                                               
NOLEAP1  DS    0H                                                               
         LA    R2,0                                                             
         STC   R2,LEAPFLAG                                                      
*                                                                               
OUT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
DUB      DS    D                                                                
OUTP     DS    D                                                                
DMCB     DS    6F                                                               
FULL     DS    F                                                                
Y1STORE  DS    F                                                                
Y2STORE  DS    F                                                                
D1STORE  DS    F                                                                
D2STORE  DS    F                                                                
DAY1ANS  DS    F                                                                
DAY2ANS  DS    F                                                                
DAYTOT   DS    F                                                                
TESTLEAP DS    F                                                                
LEAPFLAG DS    X                                                                
SIGNFLAG DS    X                                                                
SAMEYR   DS    X                                                                
BYTE     DS    X                                                                
MONTH1   DS    CL3                                                              
MONTH2   DS    CL3                                                              
IOAREA   DS    CL80                                                             
WORK     DS    CL64                                                             
NORMTAB  DS    0H                                                               
         DC    H'31',C'JAN',H'31'                                               
         DC    H'59',C'FEB',H'28'                                               
         DC    H'90',C'MAR',H'31'                                               
         DC    H'120',C'APR',H'30'                                              
         DC    H'151',C'MAY',H'31'                                              
         DC    H'181',C'JUN',H'30'                                              
         DC    H'212',C'JUL',H'31'                                              
         DC    H'243',C'AUG',H'31'                                              
         DC    H'273',C'SEP',H'30'                                              
         DC    H'304',C'OCT',H'31'                                              
         DC    H'334',C'NOV',H'30'                                              
         DC    H'365',C'DEC',H'31'                                              
*                                                                               
LEAPTAB  DS    0H                                                               
         DC    H'31',C'JAN',H'31'                                               
         DC    H'60',C'FEB',H'29'                                               
         DC    H'91',C'MAR',H'31'                                               
         DC    H'121',C'APR',H'30'                                              
         DC    H'152',C'MAY',H'31'                                              
         DC    H'182',C'JUN',H'30'                                              
         DC    H'213',C'JUL',H'31'                                              
         DC    H'244',C'AUG',H'31'                                              
         DC    H'274',C'SEP',H'30'                                              
         DC    H'305',C'OCT',H'31'                                              
         DC    H'335',C'NOV',H'30'                                              
         DC    H'366',C'DEC',H'31'                                              
******************************************************                          
* IF YOU WISH TO DEFINE ANY STORAGE OR CONSTANTS,                               
* JUST APPEND THEM TO THE LIST ABOVE (PUT THEM HERE)                            
******************************************************                          
         SPACE 3                                                                
         LTORG                                                                  
*                                                                               
DAYMON   DSECT                                                                  
DAYD     DS    H                                                                
MONTHD   DS    CL3                                                              
DAYSD    DS    H                                                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'235JHIGDATE  10/02/00'                                      
         END                                                                    
