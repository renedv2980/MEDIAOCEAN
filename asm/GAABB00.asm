*          DATA SET GAABB00    AT LEVEL 128 AS OF 08/22/00                      
*PHASE TB2200A                                                                  
*INCLUDE NUMVAL                                                                 
         SPACE 5                                                                
GRADES   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 GRADEX-GRADE,GAABB00,RR=R2     REGSAVE AREA DURING TASK          
         USING GRADE,RC                                                         
         L     RA,4(R1)                                                         
         USING TB22FFD,RA          TWA BASE ADDRESS                             
         ST    R2,RELO             RELOCATION FACTOR JUST IN CASE               
         L     RE,16(R1)                                                        
         USING COMFACSD,RE                                                      
         MVC   VSCANNER,CSCANNER                                                
         DROP  RE                                                               
**********************************************************************          
* THIS PROGRAM ALLOWS A TEACHER TO INPUT TEST SCORES FOR A MAX. OF   *          
* STUDENTS.  THESE SCORES WILL BE TRANSLATED TO LETTER GRADES.  IN   *          
* ADDITION, THE AVERAGE FOR THE PREVIOUS TEST AND THIS TEST WILL BE  *          
* CALCULATED AS WELL AS THE PERCENTAGE CHANGE IN THIS AVERAGE.       *          
**********************************************************************          
         XC    SCRHEAD,SCRHEAD                                                  
         MVC   SCRHEAD(34),=C'PLEASE INPUT GRADES, ONE AT A TIME'               
         OI    SCRHEADH+6,X'80'                                                 
         LA    R8,SCRGRD1H                                                      
         SR    R4,R4                                                            
         SR    R6,R6                                                            
         SR    R7,R7               TOTAL OF ALL GRADES                          
*                                                                               
         BAS   RE,SPECIAL                                                       
         B     LOOP                                                             
*                                                                               
SPECIAL  NTR1                                                                   
         TM    SCROPTH+4,X'80'                                                  
         BZ    EXIT                                                             
         GOTO1 VSCANNER,DMCB,SCROPTH,BLOCK                                      
         LA    R3,BLOCK                                                         
         ZIC   R4,DMCB+4                                                        
COMP     CLC   OPTS1,12(R3)        IF NOT = TO NOCURVE                          
         BNE   *+12                SKIP NEXT 2 LINES                            
         OI    FLAGS,X'10'          ELSE PUT FLAG BIT ON                        
         B     NEXT                                                             
         CLC   OPTS2,12(R3)        IF NOT = TO TOSSHI/LO                        
         BNE   *+20                SKIP NEXT 4 LINES                            
         CLI   22(R3),C'Y'         ELSE, IF NOT YES                             
         BNE   NEXT                SKIP  NEXT 2 LINES                           
         OI    FLAGS,X'20'          BUT IF = & YES, PUT BIT ON                  
         B     NEXT                                                             
         CLC   OPTS3,12(R3)                                                     
         BNE   NEXT                      GOTO NEXT                              
         OI    FLAGS,X'40'          BUT IF = PUT FLAG BIT ON                    
         ZICM  R6,22(R3),2                                                      
         STCM  R6,3,MINPASS                                                     
NEXT     LA    R3,32(R3)                                                        
         BCT   R4,COMP                                                          
EXIT     XIT1                                                                   
*                                                                               
LOOP     TM    1(R8),X'20'         INPUT FIELD SHOULDN'T BE                     
         BZ    *+6                 PROTECTED...                                 
         DC    H'0'                DIE IF PROTECTED (INPUT FIELD)               
*                                                                               
         ZICM  R1,5(R8),1          USE LENGTH                                   
         BNZ   TEST                                                             
         BAS   RE,CLEAR                                                         
         B     AVERAGE                                                          
*                                                                               
TEST     TM    FLAGS,X'10'                                                      
         BO    NOCURVE                                                          
         B     CURVE                                                            
*                                                                               
CLEAR    NTR1                                                                   
         LR    R5,R8               IF NO INPUT, NEED TO CLEAR                   
         LA    R3,SCRLSCRH         FOLLOWING GRADE FIELDS                       
CLEAR1   ZIC   R1,0(R5)                                                         
         AR    R5,R1               NOW R5 PTS TO LETTER GRD HDR                 
         XC    8(1,R5),8(R5)       CLEAR OUT THE GRADE FLD                      
         OI    6(R5),X'80'         AND TRANSMIT                                 
         ZIC   R1,0(R5)                                                         
         AR    R5,R1               NOW R5 PTS TO NEXT SCORE FLDHDER             
         CR    R5,R3               IS R5 PTING TO LAST SCORE HDR                
         BH    *+8                                                              
         B     CLEAR1                                                           
         XIT1                                                                   
*                                                                               
NOCURVE  TM    FLAGS,X'20'                                                      
         BNZ   OPTERR                                                           
         LR    R5,R8               PT R5 AT 1ST GRADE HDER                      
         GOTO1 =V(NUMVAL),DMCB,(0,8(R5)),(01,0),RR=RELO                         
         CLI   DMCB,X'FF'          IF INPUT IS INVALID                          
         BNE   *+12                                                             
         OI    6(R8),X'40'+X'80'   TRANSMIT AND GOTO ERROR                      
         B     ERROR                                                            
         L     R3,DMCB+4           R3 HOLDS THE GRADE IN BINARY                 
*                                                                               
                                                                                
         AR    R7,R3               R7 HOLDS TOTAL OF ALL GRADES                 
                                                                                
         ZIC   R9,0(R8)            R9 HOLDS FLD LEN                             
         LR    R5,R8               R5 PTS TO SCORE HDER                         
         AR    R5,R9               R5 PTS TO GRADE HDER                         
         ZIC   R9,5(R8)            R9 HOLDS INPUT LENGH                         
         CH    R9,=H'3'            IF 3 DIGITS OF INPUT                         
         BL    CHKA                                                             
         CLC   =C'100',8(R8)       AND = 100                                    
         BE    AAA                 GET AN 'A'                                   
         OI    6(R8),X'40'+X'80'                                                
         B     ERROR                                                            
*                                                                               
CHKA     TM    FLAGS,X'40'                                                      
         BZ    *+24                IF MINPASS FLAG OFF,SKIP 4 LINES             
         CLC   =C'90',MINPASS      IF MINPASS <90                               
         BH    *+14                SKIP NEXT 2 LINES                            
         CLC   MINPASS,8(R8)       IF GRADE IS < MINPASS                        
         B     *+10                                                             
         CLC   =C'90',8(R8)                                                     
         BH    *+12                IF < 90,SKIP 2 LINES                         
AAA      MVI   8(R5),C'A'         ELSE PRINT 'A'                                
         B     INC                 AND GOTO INC                                 
*                                                                               
         TM    FLAGS,X'40'          IF MINPASS FLAG BIT IS ON                   
         BZ    *+24                                                             
         CLC   =C'80',MINPASS                                                   
         BH    *+14                                                             
         CLC   MINPASS,8(R8)       IF < MINPASS                                 
         B     *+10                                                             
         CLC   =C'80',8(R8)                                                     
         BH    *+12                IF <80, SKIP 2 LINES                         
         MVI   8(R5),C'B'         ELSE PRINT 'B'                                
         B     INC                 AND GOTO INC                                 
*                                                                               
         TM    FLAGS,X'40'          IF MINPASS FLAG BIT IS ON                   
         BZ    *+24                                                             
         CLC   =C'70',MINPASS                                                   
         BH    *+14                                                             
         CLC   MINPASS,8(R8)       IF < MINPASS                                 
         B     *+10                                                             
         CLC   =C'70',8(R8)                                                     
         BH    *+12                IF < 70,SKIP 2 LINES                         
         MVI   8(R5),C'C'         ELSE PRINT C                                  
         B     INC                 AND GOTO INC                                 
*                                                                               
         TM    FLAGS,X'40'          IF MINPASS FLAG BIT IS ON                   
         BZ    *+14                                                             
         CLC   MINPASS,8(R8)       IF < MINPASS                                 
         B     *+10                                                             
         CLC   =C'65',8(R8)                                                     
         BH    *+12                IF <65,SKIP 2 LINES                          
         MVI   8(R5),C'D'         ELSE PRINT D                                  
         B     INC                 AND GOTO INC                                 
*                                                                               
         MVI   8(R5),C'F'         PRINT F                                       
*                                                                               
INC      OI    6(R5),X'80'         TRANSMIT FIELD                               
         LA    R3,SCRLSCRH                                                      
         CR    R3,R8                                                            
         BNH   AVERAGE                                                          
*                                                                               
         ZIC   R9,0(R8)                                                         
         AR    R8,R9               POINT RA TO NEXT INPUT FIELD                 
         ZIC   R9,0(R8)                                                         
         AR    R8,R9                                                            
         LA    R4,1(R4)            R4 HOLDS THE # OF SCORES                     
         B     LOOP                AND GOTO LOOP                                
*                                                                               
CURVE    OC    DIFF,DIFF                                                        
         BNZ   CONVERT                                                          
         LA    R8,SCRGRD1H                                                      
         LR    R5,R8                                                            
         LA    R2,100              HOLDS LOW SCORE                              
         ST    R2,SECLOW                                                        
         LA    R3,0                HOLDS HIGH SCORE                             
         ST    R3,SECHI                                                         
*                                                                               
CURVE1   GOTO1 =V(NUMVAL),DMCB,(0,8(R5)),(01,0),RR=RELO                         
         CLI   DMCB,X'FF'          IF INVALID INPUT                             
         BNE   *+12                                                             
         OI    6(R8),X'40'+X'80'   PLACE CURSOR                                 
         B     ERROR               AND GOTO ERROR                               
         C     R2,DMCB+4           IF R2 < INPUT                                
         BL    *+12                SKIP NEXT 2 LINES                            
         ST    R2,SECLOW           ELSE HOLD R2 AT 2ND LOW                      
         L     R2,DMCB+4           REPLACE R2 WITH NEW                          
         B     *+20                SKIP NEXT 3 LINES                            
         CLC   SECLOW,DMCB+4       ELSE,IF 2ND LOW IS >NEW                      
         BL    *+10                                                             
         MVC   SECLOW,DMCB+4       REPLACE 2ND LOW WITH NEW                     
*                                                                               
         C     R3,DMCB+4                                                        
         BH    *+16                IF R3> INPUT SKIP NEXT 3 LINES               
         ST    R3,SECHI            ELSE KEEP R3 AT 2ND HIGH                     
         L     R3,DMCB+4           REPLACE R3 WITH NEW                          
         B     *+20                AND SKIP NEXT 3 LINESS                       
         CLC   SECHI,DMCB+4        IF 2ND HI > NEW                              
         BH    *+10                SKIP A LINE                                  
         MVC   SECHI,DMCB+4        ELSE REPLACE 2ND HI WITH NEW                 
*                                                                               
AROUND   LA    R4,1(R4)            INC # OF SCORES                              
*                                                                               
         ZIC   R9,0(R5)                                                         
         AR    R5,R9               PT R5 TO NEXT GRADE FIELD                    
         ZIC   R9,0(R5)                                                         
         AR    R5,R9               PT R5 TO NEXT SCORE FIELD                    
*                                                                               
         ZICM  R1,5(R5),1          IF NO INPUT                                  
         BNZ   CURVE1              ELSE GOTO CURVE1                             
         TM    FLAGS,X'20'                                                      
         BZ    DIFFER                                                           
         B     DIFFER2                                                          
*                                                                               
DIFFER   SR    R3,R2               GET DIFF BET HI AND LOW SCORE                
         SR    R2,R2                                                            
         DR    R2,R4                                                            
         ST    R3,DIFF             AND STORE AT DIFF                            
         B     *+22                                                             
*                                                                               
DIFFER2  L     R3,SECHI                                                         
         L     R2,SECLOW                                                        
         SR    R3,R2                                                            
         SR    R2,R2                                                            
         SH    R4,=H'2'                                                         
         DR    R2,R4                                                            
         ST    R3,DIFF                                                          
*                                                                               
         LA    R8,SCRGRD1H                                                      
         CH    R4,=H'2'                                                         
         BNH   CRVERR                                                           
         SR    R4,R4                                                            
         SR    R7,R7                                                            
*                                                                               
CONVERT  LR    R5,R8                                                            
         GOTO1 =V(NUMVAL),DMCB,(0,8(R5)),(01,0),RR=RELO                         
         CLI   DMCB,X'FF'          IF INPUT IS INVALID                          
         BNE   *+12                                                             
         OI    6(R8),X'40'+X'80'   TRANSMIT AND GOTO ERROR                      
         B     ERROR                                                            
         L     R3,DMCB+4           R3 HOLDS THE GRADE IN BINARY                 
*                                                                               
         AR    R7,R3               R7 HOLDS TOTAL OF ALL GRADES                 
*                                                                               
         TM    FLAGS,X'20'                                                      
         BZ    *+14                IF NOT TOSSHILO SKIP NEXT 2 LINES            
         CLC   8(R8),SECHI         ELSE IF INPUT > SECHI                        
         BH    AAAA                GOTO AAAA                                    
*                                                                               
         TM    FLAGS,X'40'                                                      
         BZ    *+14                IF NOT MINPASS,SKIP NEXT 2 LINES             
         PACK  PKMINPAS,MINPASS    ELSE,PACK AND CONVERT MINPASS                
         CVB   R1,PKMINPAS                                                      
*                                                                               
         ZIC   R6,0(R8)            R6 HOLDS FLD LEN                             
         LR    R5,R8               R5 PTS TO GRADE HDER                         
         AR    R5,R6                                                            
         ZIC   R6,5(R8)            R6 HOLDS INPUT LENGH                         
         CH    R6,=H'3'            IF 3 DIGITS OF INPUT                         
         BL    CHCKA                                                            
         CLC   =C'100',8(R8)       AND = 100                                    
         BE    AAAA                GET AN 'A'                                   
         OI    6(R8),X'40'+X'80'                                                
         B     ERROR                                                            
*                                                                               
CHCKA    LA    R2,100                                                           
         S     R2,DIFF                                                          
         TM    FLAGS,X'40'                                                      
         BZ    *+20                IF MINPASS FLAG OFF,SKIP 4 LINES             
         CR    R2,R1               IF MINPASS < 'A'                             
         BH    *+14                SKIP NEXT 2 LINES                            
         CLC   MINPASS,8(R8)       IF GRADE IS < MINPASS                        
         B     *+6                                                              
         CR    R2,R3                                                            
         BH    *+12                IF < 90,SKIP 2 LINES                         
AAAA     MVI   8(R5),C'A'          ELSE PRINT 'A'                               
         B     INC                 AND GOTO INC                                 
*                                                                               
         S     R2,DIFF                                                          
         TM    FLAGS,X'40'          IF MINPASS FLAG BIT IS ON                   
         BZ    *+20                                                             
         CR    R2,R1                                                            
         BH    *+14                                                             
         CLC   MINPASS,8(R8)  IF < MINPASS                                      
         B     *+6                                                              
         CR    R2,R3                                                            
         BH    *+12                IF <80, SKIP 2 LINES                         
         MVI   8(R5),C'B'          ELSE PRINT 'B'                               
         B     INC                 AND GOTO INC                                 
*                                                                               
         S     R2,DIFF                                                          
         TM    FLAGS,X'40'          IF MINPASS FLAG BIT IS ON                   
         BZ    *+20                                                             
         CR    R2,R1                                                            
         BH    *+14                                                             
         CLC   MINPASS,8(R8)       IF < MINPASS                                 
         B     *+6                                                              
         CR    R2,R3                                                            
         BH    *+12                IF < 70,SKIP 2 LINES                         
         MVI   8(R5),C'C'         ELSE PRINT C                                  
         B     INC                 AND GOTO INC                                 
*                                                                               
         S     R2,DIFF                                                          
         TM    FLAGS,X'40'          IF MINPASS FLAG BIT IS ON                   
         BZ    *+14                                                             
         CLC   MINPASS,8(R8)       IF < MINPASS                                 
         B     *+6                                                              
         CR    R2,R3                                                            
         BH    *+12                IF <65,SKIP 2 LINES                          
         MVI   8(R5),C'D'         ELSE PRINT D                                  
         B     INC                 AND GOTO INC                                 
*                                                                               
         MVI   8(R5),C'F'         PRINT F                                       
         B     INC                                                              
*                                                                               
AVERAGE  LTR   R4,R4                                                            
         BZ    ERROR                                                            
         SR    R6,R6                                                            
         DR    R6,R4               FINDS AVG GRADE FOR TEST                     
         EDIT  (R7),(3,SCRAVG1)    AND MOVES IT TO SCREEN                       
         OI    SCRAVG1H+6,X'80'                                                 
         CLI   16(RA),0                                                         
         BE    OVEREDIT            SKIP EDIT &% CHANGE 1ST TIME THRU            
*                                                                               
         EDIT  (4,LSTAVG),(3,SCRAVG2)                                           
         OI    SCRAVG2H+6,X'80'                                                 
*                                                                               
         L     R3,LSTAVG           R3 HOLDS PREV AVG                            
         LR    R5,R7               R5 HOLDS NEW AVG                             
         SR    R4,R4               USED IN DIVISION                             
         SR    R5,R3               CALCULATE DIFFERENCE                         
         BNM   *+8                                                              
         L     R4,=F'-1'                                                        
         MH    R5,=H'100'          IN PERCENTAGE                                
         LTR   R3,R3                                                            
         BZ    OVEREDIT                                                         
         DR    R4,R3               BETWEEEN THE TWO GRADES                      
         EDIT  (R5),(3,SCRPRCT),MINUS=YES,ZERO=NOBLANK                          
         OI    SCRPRCTH+6,X'80'                                                 
*                                                                               
OVEREDIT ST    R7,LSTAVG           AND SAVES IT                                 
         MVI   16(RA),1                                                         
         B     THEEND1                                                          
*                                                                               
CRVERR   XC    SCRHEAD,SCRHEAD                                                  
         MVC   SCRHEAD(39),=C'3 GRADES MIN. WITH CURVE(5 IF TOSSHILO)'          
         OI    SCRHEADH+6,X'80'                                                 
         OI    SCRGRD1H+6,X'80'+X'40'                                           
         B     THEEND                                                           
*                                                                               
OPTERR   XC    SCRHEAD,SCRHEAD                                                  
         MVC   SCRHEAD(37),=C'INVALID OPTION COMBINATION. TRY AGAIN'            
         OI    SCRHEADH+6,X'80'                                                 
         OI    SCROPTH+6,X'80'+X'40'                                            
         B     THEEND                                                           
*                                                                               
ERROR    XC    SCRHEAD,SCRHEAD                                                  
         MVC   SCRHEAD(33),=C'THIS INPUT IS INVALID. TRY AGAIN.'                
         OI    SCRHEADH+6,X'80'   TRANSMIT FLD                                  
*                                                                               
THEEND1  OI    SCROPTH+6,X'40'+X'80'                                            
THEEND   XMOD1                                                                  
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
OPTS1    DC    CL7'NOCURVE'                                                     
OPTS2    DC    CL8'TOSSHILO'                                                    
OPTS3    DC    CL7'MINPASS'                                                     
*******************STORAGE WITHIN A TRANSACTION *********************           
GRADE    DSECT                                                                  
FLAGS    DS    X                   OPTION FLAG/TEST BITS                        
DIFF     DS    F                   DIFF BET. GRADES FOR TOSSHILO                
SECHI    DS    F                   2ND HIGHEST GRADE FOR TOSSHILO               
SECLOW   DS    F                   2ND LOWEST GRADE FOR TOSSHILO                
DUB      DS    D                   FOR PACKING & EDIT                           
WORK     DS    XL17                FOR EDIT                                     
RELO     DS    F                                                                
VSCANNER DS    V                   FOR SUBROUTINE                               
DMCB     DS    6F                                                               
MINPASS  DS    CL2                 MINPASS GRADE                                
PKMINPAS DS    D                   PKED MINPASS FOR COMPARES                    
BLOCK    DS    10CL32              OUTPUT FROM SCANNER                          
GRADEX   EQU   *                                                                
****************************TWA***************************************          
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE GAABBFFD                                                       
LSTAVG   DS    F                   HOLDS LAST AVRGE BET. BEING CALLED           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'128GAABB00   08/22/00'                                      
         END                                                                    
