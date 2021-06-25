*          DATA SET DDBOOKVAL  AT LEVEL 048 AS OF 04/12/18                      
*PHASE T00A00C                                                                  
         SPACE 1                                                                
*                                                                               
* 23MAR18 REMOVE 4 DIGIT YEAR SUPPORT                                           
* 11DEC06 ALLOW 2 CHAR BOOK TYPES                                               
* 07JUN00 ALLOW 4 DIGIT YEAR                                                    
* 30OCT98 WINTER/SPRING/SUMMER/W1/W2 SUPPORT                                    
* 15JAN98 YEAR 2000 SUPPORT (ONLY VALID YEARS WILL BE 1928-2027)                
* 13JUN96 EQUATE MEDIAFAX AND ARB                                               
* 11JUN96 ALLOW UP TO 21 BOOKS TO BE VALIDATED                                  
* 18FEB88 ADD SPANISH VALIDATATION                                              
* 13NOV87 FIX SUSAN'S EGREGIOUS SEPTEMBER BUG FROM 23MAY85 (FINALLY)            
         TITLE 'BOOK STRING VALIDATION MODULE'                                  
*                                                                               
*                                  PARA1 BYTE1    PRESET SOURCE A/N             
*                                            2-4  A(SCREEN HEADER)              
*                                  PARA2 BYTE1    MAX ALLOWED FIELDS            
*                                            2-4  A(STRING OF 3 BYTE            
*                                                   BOOK FIELDS)                
*                                  PARA3 BYTE1    N=NETWORK                     
*                                                 L=LABEL FEATURE               
*                                                 B=BOOK TYPE                   
*                                            2-4  A(SCANNER)                    
*                                                                               
*                                  PARA2 BYTE1    SET TO ACTUAL NUMBER          
*                                                 OF VALID FLDS OR ZERO         
*                                                                               
*                                  PARA4          A(LABEL AREA) OR              
*                                                 A(BOOK TYPE AREA)             
*                                                                               
*                                  PARA5 BYTE1    C'C'        (OPT)             
*                                            2-4  A(COMFACS)  (OPT)             
         SPACE 1                                                                
*                                  BOOK FIELDS                                  
*                                        BYTE1   X'80' 1=SUPPRESS CPM           
*                                                X'40' 0=ARB 1=NSI              
*         SPANISH SOURCES                              0=    1=SRC              
*                                                X'20' 1=ESTIMATED              
*                                                X'10' 1=NETWORK                
*                                                X'08' 1=TIME PERIOD            
*                                                X'04' 1=PROJECTED              
*                                                X'02' 1=SPECIAL SURVEY         
*                                                X'01' 1=SPANISH DEMOS          
*                                        BYTE2    YEAR                          
*                                        BYTE3    MONTH OR WEEK (NET)           
*                                                                               
         EJECT                                                                  
         SPACE 2                                                                
         PRINT NOGEN                                                            
BOOKVAL  CSECT                                                                  
         DS    1100C                                                            
         ORG   BOOKVAL                                                          
         NMOD1 BOOKDL,**BKVL**                                                  
         USING BOOKD,RC                                                         
         MVI   SOURCE,X'FF'                                                     
*                                                                               
         XC    ACOMFACS,ACOMFACS                                                
         CLI   16(R1),C'C'         TEST A(COMFACS) PASSED                       
         BNE   *+10                                                             
         MVC   ACOMFACS,16(R1)                                                  
*                                                                               
         CLI   0(R1),C'M'          EQUATE MEDIAFAX AND ARB                      
         BE    *+8                                                              
         CLI   0(R1),C'A'                                                       
         BNE   *+8                                                              
         MVI   SOURCE,0                                                         
*                                                                               
         CLI   0(R1),C'N'          NSI                                          
         BNE   *+8                                                              
         MVI   SOURCE,X'40'                                                     
*                                                                               
         CLI   0(R1),C'S'          SRC                                          
         BNE   *+8                                                              
         MVI   SOURCE,X'41'                                                     
*                                                                               
         MVI   VALEXP,0                                                         
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,4(R1)                                                         
         MVI   4(R1),0             PRESET TO INVALID                            
         LR    R9,R1                                                            
         MVC   SCANNER,8(R1)                                                    
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         STC   R4,MAXEXP                                                        
         MVI   SORN,0                                                           
         CLI   8(R1),C'N'                                                       
         BNE   *+12                                                             
         MVI   SOURCE,X'40'                                                     
         MVI   SORN,X'10'          NETWORK INDICATOR                            
         EJECT                                                                  
*              SCAN AND VALIDATE                                                
         SPACE 3                                                                
         MVC   PARAS+8(4),=C',=,('                                              
*                                                                               
         GOTO1 SCANNER,PARAS,(0,(R2)),(20,BLOCK)                                
         ZIC   R5,PARAS+4                                                       
         LTR   R5,R5                                                            
         BZ    XIT                                                              
         LA    R6,BLOCK                                                         
         SR    R2,R2                                                            
         CLI   8(R9),C'B'          BOOK TYPE OPTION                             
         BE    *+12                                                             
         CLI   8(R9),C'L'          LABEL OPTION                                 
         BNE   *+8                                                              
         L     R2,12(R9)           USER PASSED A(LABEL AREA) OR                 
*                                              A(BOOK TYPE AREA)                
         SPACE 2                                                                
VAL      MVI   NOCPM,0                                                          
         MVI   EST,0                                                            
         MVI   TP,0                                                             
         MVI   PROJ,0                                                           
         MVI   SPEC,0                                                           
         SPACE 1                                                                
         CLC   12(2,R6),=C'M '     CHECK KEYWORDS                               
         BE    VAL1                                                             
         CLC   12(2,R6),=C'A '     CHECK KEYWORDS                               
         BNE   VAL2                                                             
VAL1     MVI   SOURCE,0                                                         
         B     ADD2                                                             
         SPACE 1                                                                
VAL2     CLC   12(2,R6),=C'N '                                                  
         BNE   VAL3                                                             
VAL2N    MVI   SOURCE,X'40'                                                     
         B     ADD2                                                             
         SPACE 1                                                                
VAL3     CLC   12(2,R6),=C'S '                                                  
         BNE   VAL4                                                             
VAL3S    MVI   SOURCE,X'41'                                                     
         B     ADD2                                                             
         SPACE 2                                                                
VAL4     CLC   12(4,R6),=C'ARB '   ARBITRON                                     
         BE    VAL1                                                             
         CLC   12(4,R6),=C'MFX '   MEDIAFAX                                     
         BE    VAL1                                                             
         CLC   12(4,R6),=C'NSI '   NIELSEN                                      
         BE    VAL2N                                                            
         CLC   12(4,R6),=C'SRC '   STRATEGY RESEARCH                            
         BE    VAL3S                                                            
         SPACE 1                                                                
         MVI   SAMESW,C'N'                                                      
         ZIC   R1,0(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),=C'SAME'                                                
         BNE   VAL5                                                             
         MVI   SAMESW,C'Y'                                                      
         B     VAL14                                                            
         SPACE 2                                                                
VAL5     CLI   SOURCE,X'FF'        SHOULD HAVE SOURCE BY NOW                    
         BE    XIT                                                              
         LA    R7,12(R6)                                                        
         SPACE 1                                                                
*                                                                               
*  MONTH CAN HAVE PREFIX OF - AND/OR T OR P OR E OR S                           
*                                                                               
VAL5B    CLI   0(R7),C'-'          CHECK FOR PREFIX OF - (NO CPM)               
         BNE   VAL5C                                                            
         MVI   NOCPM,X'80'                                                      
         LA    R7,1(R7)                                                         
         SPACE 1                                                                
VAL5C    CLI   0(R7),C'T'          T (TIME PERIOD)                              
         BNE   VAL5E                                                            
         CLI   SOURCE,X'41'        T IS INVALID FOR SRC                         
         BE    XIT                                                              
         MVI   TP,X'08'                                                         
         LA    R7,1(R7)                                                         
         B     VAL5O                                                            
         SPACE 1                                                                
VAL5E    CLI   0(R7),C'P'          P (PROJECTED)                                
         BNE   VAL5G                                                            
         MVI   PROJ,X'04'                                                       
         LA    R7,1(R7)                                                         
         B     VAL5O                                                            
         SPACE 1                                                                
VAL5G    CLI   0(R7),C'S'          S (SPECIAL SURVEY)                           
         BNE   VAL5H                                                            
         CLI   1(R7),C'E'          E PROBABLY MEANS SEPTEMBER                   
         BE    VAL5H                                                            
         CLI   1(R7),C'U'          U PROBABLY MEANS SUMMER                      
         BE    VAL5H                                                            
         CLI   1(R7),C'P'          P PROBABLY MEANS SPRING                      
         BE    VAL5H                                                            
         CLI   1(R7),C'/'          IF YEAR SEP, TREAT AS SEPT                   
         BE    VAL5H                                                            
         CLI   1(R7),C'0'          IF NUMERIC, TREAT AS SEPT                    
         BNL   VAL5H                                                            
         CLI   SOURCE,X'41'        S IS INVALID FOR SRC                         
         BE    XIT                                                              
         MVI   SPEC,X'02'                                                       
         LA    R7,1(R7)                                                         
         B     VAL5O                                                            
         SPACE 1                                                                
VAL5H    CLI   0(R7),C'E'          E (ESTIMATED)                                
         BNE   VAL6                                                             
         MVI   EST,X'20'                                                        
         LA    R7,1(R7)                                                         
         CLC   0(2,R7),=C'ST'      EST MEANS UNSPECIFIED MONTH                  
         BNE   VAL5O                                                            
         MVI   MONTH,0                                                          
         LA    R7,2(R7)                                                         
         B     VAL12                                                            
         SPACE 1                                                                
VAL5O    CLI   0(R7),C'-'          - (NO CPM)                                   
         BNE   VAL6                                                             
         MVI   NOCPM,X'80'                                                      
         LA    R7,1(R7)                                                         
         SPACE 2                                                                
VAL6     LA    R1,9(R6)            FIND LENGTH OF MONTH (DAY) EXPRESS.          
         ZIC   R0,0(R6)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),C'/'          EXPRESSION MUST END WITH YY                  
         BE    *+8                         AND MAY END WITH /YY                 
         LA    R1,1(R1)                                                         
         SR    R1,R7                                                            
         BNP   XIT                                                              
         CHI   R1,3                                                             
         BH    XIT                                                              
         BCTR  R1,0                                                             
         LA    R8,MONTABLE                                                      
         MVC   DUB,=CL8' '                                                      
         EX    R1,MONMOVE                                                       
         CLI   SORN,0                                                           
         BE    VAL8                                                             
         CLI   0(R7),X'F0'         NETWORK TESTS                                
         BL    XIT                 MUST BE NUMERIC, 1-53                        
         CLI   0(R7),X'F9'                                                      
         BH    XIT                                                              
         PACK  DUB,0(1,R7)                                                      
         CLI   1(R7),C' '                                                       
         BE    VAL7                                                             
         CLI   1(R7),X'F0'                                                      
         BL    XIT                                                              
         CLI   1(R7),X'F9'                                                      
         BH    XIT                                                              
         PACK  DUB,0(2,R7)                                                      
         SPACE 2                                                                
VAL7     CVB   R0,DUB                                                           
         STC   R0,MONTH                                                         
         CLI   MONTH,0                                                          
         BE    XIT                                                              
         CLI   MONTH,53                                                         
         BH    XIT                                                              
         LA    R7,1(R7,R1)                                                      
         B     VAL12                                                            
         SPACE 2                                                                
VAL8     CLC   DUB(3),0(R8)                                                     
         BE    VAL10                                                            
         LA    R8,4(R8)                                                         
         CLI   0(R8),X'FF'                                                      
         BNE   VAL8                                                             
         B     XIT                                                              
         SPACE 2                                                                
VAL10    MVC   MONTH(1),3(R8)                                                   
         LA    R7,1(R7,R1)                                                      
         SPACE 2                                                                
VAL12    CLI   0(R7),C'/'          OPTIONAL SLASH                               
         BNE   *+8                                                              
         LA    R7,1(R7)                                                         
*                                                                               
* REMOVE 4 DIGIT YEAR SUPPORT                                                   
* REMOVE CENTURY CHECK FOR ADJUSTMENT AND ASSUME YEAR 2000                      
*                                                                               
*&&DO                                                                           
         CLC   0(2,R7),=C'19'      ALLOW FOR 4 DIGIT YEAR                       
         BE    *+14                                                             
         CLC   0(2,R7),=C'20'                                                   
         BNE   *+8                                                              
         LA    R7,2(R7)                                                         
*&&                                                                             
         CLI   0(R7),X'F0'         CHECK YEAR                                   
         BL    XIT                                                              
         CLI   1(R7),X'F0'                                                      
         BL    XIT                                                              
         PACK  DUB,0(2,R7)                                                      
         CVB   R1,DUB                                                           
                                                                                
*&&DO                                                                           
* NOTE: LOWEST YEAR SUPPORTED IN YEAR 2K CONVERSION IS 2027                     
* *** Y2K !!!                                                                   
         DS    0H                  YEAR 2000                                    
         OR    R1,R1                IF YEAR INPUTTED IS C'00'                   
         BZ    *+12                                                             
         CHI   R1,27                 AND C'27', INCLUSIVE,                      
         BH    *+8                                                              
*&&                                                                             
*                                                                               
* UNCONDITIONALLY ADJUST THE YEAR TO ASSUME 21ST CENTURY                        
* FOR YEAR 00 TO 59 ONLY                                                        
*                                                                               
         CHI   R1,59                                                            
         BH    XIT                                                              
         AHI   R1,100                MAKE IT A 2000 YEAR                        
*                                                                               
                                                                                
         STC   R1,YEAR                                                          
         CLI   2(R7),C' '          SHOULD END EXPRESSION                        
         BNE   XIT                                                              
         EJECT                                                                  
*              ADD AN ENTRY TO USER LIST                                        
         SPACE 3                                                                
VAL14    ZIC   R1,VALEXP                                                        
         LA    R1,1(R1)                                                         
         STC   R1,VALEXP                                                        
         CLC   VALEXP,MAXEXP                                                    
         BH    XIT                                                              
         MVC   0(1,R3),SOURCE                                                   
         OC    0(1,R3),NOCPM                                                    
         OC    0(1,R3),EST                                                      
         OC    0(1,R3),SORN                                                     
         OC    0(1,R3),TP                                                       
         OC    0(1,R3),PROJ                                                     
         OC    0(1,R3),SPEC                                                     
         MVC   1(1,R3),YEAR                                                     
         MVC   2(1,R3),MONTH                                                    
         CLI   SAMESW,C'Y'                                                      
         BNE   *+10                                                             
         MVC   0(3,R3),=C'SAM'                                                  
         SPACE 1                                                                
         LTR   R2,R2               POSSIBLE LABEL OR BOOK TYPE OPERAND          
         BZ    VAL16                                                            
         CLI   8(R9),C'L'          LABEL OPTION                                 
         BE    VAL14A                                                           
         SPACE 1                                                                
         ZIC   R1,1(R6)            BOOK TYPE OPTION                             
         CHI   R1,2                                                             
         BH    VAL2BKT             CHECK FOR 2 CHAR BOOK TYPE                   
         LTR   R1,R1                                                            
         BZ    *+10                                                             
         MVC   0(1,R2),22(R6)                                                   
         LA    R2,1(R2)                                                         
         B     VAL16                                                            
         SPACE 1                                                                
*                                                                               
VAL2BKT  CHI   R1,3                STOP MORE THAN 2 CHARS INPUT                 
         BH    XIT                 THIS IS RIGHT!  "AA)" IN BLOCK               
         ICM   RF,15,ACOMFACS                                                   
         BZ    XIT                                                              
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB  GET A(BOOK TABLE)                            
         ICM   RF,15,0(R1)         A(TABLE)RETURNED IN P1                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RE,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RF                                                      
*                                                                               
VAL2BKT2 CLI   0(RF),X'FF'                                                      
         BE    XIT                 INVALID BOOK TYPE                            
         CLC   SPBKTYPA,22(R6)                                                  
         BE    *+10                                                             
         AR    RF,RE                                                            
         B     VAL2BKT2                                                         
         MVC   0(1,R2),SPBKTYPN                                                 
         LA    R2,1(R2)                                                         
         B     VAL16                                                            
         DROP  RF                                                               
*                                                                               
         SPACE 1                                                                
VAL14A   ZIC   R1,1(R6)            GET RID OF RIGHT HAND BRACKET                
         LTR   R1,R1                                                            
         BZ    VAL15                                                            
         LA    R1,21(R1,R6)                                                     
         MVI   0(R1),C' '                                                       
         MVC   0(5,R2),22(R6)                                                   
         SPACE 1                                                                
VAL15    LA    R2,5(R2)                                                         
         SPACE 1                                                                
VAL16    LA    R3,3(R3)                                                         
         LA    R6,32(R6)                                                        
         BCT   R5,VAL                                                           
         MVC   4(1,R9),VALEXP                                                   
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
ADD2     LA    R6,32(R6)                                                        
         BCT   R5,VAL                                                           
         B     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
*              MONTH TABLES ETC                                                 
         SPACE 3                                                                
MONMOVE  MVC   DUB(0),0(R7)                                                     
MONTABLE DS    0H                                                               
         DC    C'JAN',AL1(1)                                                    
         DC    C'FEB',AL1(2)                                                    
         DC    C'MAR',AL1(3)                                                    
         DC    C'APR',AL1(4)                                                    
         DC    C'MAY',AL1(5)                                                    
         DC    C'JUN',AL1(6)                                                    
         DC    C'JUL',AL1(7)                                                    
         DC    C'AUG',AL1(8)                                                    
         DC    C'SEP',AL1(9)                                                    
         DC    C'OCT',AL1(10)                                                   
         DC    C'NOV',AL1(11)                                                   
         DC    C'DEC',AL1(12)                                                   
         DC    C'1  ',AL1(1)                                                    
         DC    C'2  ',AL1(2)                                                    
         DC    C'3  ',AL1(3)                                                    
         DC    C'4  ',AL1(4)                                                    
         DC    C'5  ',AL1(5)                                                    
         DC    C'6  ',AL1(6)                                                    
         DC    C'7  ',AL1(7)                                                    
         DC    C'8  ',AL1(8)                                                    
         DC    C'9  ',AL1(9)                                                    
         DC    C'10 ',AL1(10)                                                   
         DC    C'11 ',AL1(11)                                                   
         DC    C'12 ',AL1(12)                                                   
         DC    C'01 ',AL1(1)                                                    
         DC    C'02 ',AL1(2)                                                    
         DC    C'03 ',AL1(3)                                                    
         DC    C'04 ',AL1(4)                                                    
         DC    C'05 ',AL1(5)                                                    
         DC    C'06 ',AL1(6)                                                    
         DC    C'07 ',AL1(7)                                                    
         DC    C'08 ',AL1(8)                                                    
         DC    C'09 ',AL1(9)                                                    
         DC    C'JA ',AL1(1)                                                    
         DC    C'F  ',AL1(2)                                                    
         DC    C'FE ',AL1(2)                                                    
         DC    C'AP ',AL1(4)                                                    
         DC    C'M  ',AL1(5)                                                    
         DC    C'MA ',AL1(5)                                                    
         DC    C'J  ',AL1(7)                                                    
         DC    C'JU ',AL1(7)                                                    
         DC    C'AU ',AL1(8)                                                    
         DC    C'S  ',AL1(9)                                                    
         DC    C'SE ',AL1(9)                                                    
         DC    C'O  ',AL1(10)                                                   
         DC    C'OC ',AL1(10)                                                   
         DC    C'N  ',AL1(11)                                                   
         DC    C'NO ',AL1(11)                                                   
         DC    C'D  ',AL1(12)                                                   
         DC    C'DE ',AL1(12)                                                   
* SPECIALS                                                                      
         DC    C'W1 ',AL1(01)      WINTER #1                                    
         DC    C'WIN',AL1(02)      WINTER                                       
         DC    C'W2 ',AL1(02)      WINTER#2                                     
         DC    C'SP ',AL1(05)      SPRING                                       
         DC    C'SPR',AL1(05)      SPRING                                       
         DC    C'SU ',AL1(07)      SUMMER                                       
         DC    C'SUM',AL1(07)      SUMMER                                       
         DC    X'FF'                                                            
         SPACE 2                                                                
*              DSECT FOR MODULE                                                 
         SPACE 2                                                                
BOOKD    DSECT                                                                  
DUB      DS    D                                                                
PARAS    DS    6F                                                               
DMCB     DS    6F                                                               
BLOCK    DS    666C                                                             
YEAR     DS    CL1                                                              
MONTH    DS    CL1                                                              
EST      DS    CL1                                                              
TP       DS    CL1                                                              
SOURCE   DS    CL1                                                              
SCANNER  DS    V                                                                
NOCPM    DS    CL1                                                              
VALEXP   DS    CL1                                                              
MAXEXP   DS    CL1                                                              
SAMESW   DS    CL1                                                              
SORN     DS    CL1                                                              
PROJ     DS    CL1                                                              
SPEC     DS    CL1                                                              
ACOMFACS DS    A                                                                
BOOKDL   EQU   *-BOOKD                                                          
*                                                                               
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048DDBOOKVAL 04/12/18'                                      
         END                                                                    
