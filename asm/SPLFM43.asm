*          DATA SET SPLFM43    AT LEVEL 080 AS OF 05/01/02                      
*PHASE T21943A,+0                                                               
SPLFM43  TITLE '-  SPTFILE MAINT - AGENCY HUT REC - WEEKLY'                     
T21943   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21943                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING NAHRECD,R8                                                       
         LA    R1,FIVEWEEK                                                      
         ST    R1,A5WEEK                                                        
         MVC   0(12,R1),WEEKVALS                                                
         CLI   SVFMTSW,0           TEST FORMAT OR EDIT                          
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
WK95VALS DC    X'310000320000330000340000'                                      
WEEKVALS DC    X'310000320000330000340000'                                      
*               5APR/2CLHUT/5JUN/2CLHUT/5AUG/2CLHUT/5DEC/2CLHUT                 
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
FMT      DS    0H                                                               
         CLI   SVACT,C'S'          SPECIAL DDS DISPLAY                          
         BNE   FMT0                                                             
         B     FMTDDS                                                           
*                                                                               
FMT0     CLI   HUTSPECH+5,0        FIRST CHK FOR INPUT IN SPECIAL               
         BE    FMT1                                                             
         B     FMTPCT                                                           
FMT1     MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
         LA    R7,REC+24                                                        
FMTHUTS  CLI   0(R7),X'90'         COULD GET HERE FROM FMTDDS                   
*                                  OR FMTPCT                                    
         BE    FMT2                                                             
         MVI   ELCODE,X'90'                                                     
         BAS   RE,NEXTEL                                                        
         BE    FMT2                                                             
         DC    H'0'                MUST FIND 90 ELEM                            
FMT2     DS    0H                                                               
         USING NAHUTEL,R7                                                       
         BAS   RE,CLRSCRN                                                       
         LA    R2,HUTJAN1H                                                      
         LA    R5,MAXHUTS                                                       
         LA    R6,NAHUTS                                                        
*                                                                               
FMT4     ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
*        XC    8(L'HUTJAN1,R2),8(R2)                                            
         OC    0(2,R6),0(R6)                                                    
         BZ    FMT4B                                                            
         EDIT  (B2,0(R6)),(6,8(R2)),1,ALIGN=LEFT                                
FMT4B    FOUT  (R2)                                                             
         BAS   RE,NEXTUN                                                        
         LA    R6,2(R6)            NEXT HUT                                     
         BCT   R5,FMT4                                                          
*                                                                               
*        LA    R2,HUTJAN1H         CLEAR 5WEEK FIELDS                           
*        LA    R5,4                                                             
* FMT4C  BAS   RE,NEXT5WK                                                       
*        ZIC   R1,0(R2)                                                         
*        SH    R1,=H'9'                                                         
*        EX    R1,*+8                                                           
*        B     *+10                                                             
*        XC    8(0,R2),8(R2)                                                    
*        FOUT  (R2)                                                             
*        BCT   R5,FMT4C                                                         
*                                                                               
*        CLI   NAHUTLEN,X'64'      IS IT 48 WEEK ELEM                           
*        BE    FMT5                                                             
*        LA    R2,HUTJAN1H         NO/52 WEEK, DO EXTRA WEEKS                   
*        LA    R6,NAHUTS49                                                      
*        LA    R5,4                                                             
* FMT4E  BAS   RE,NEXT5WK                                                       
*        EDIT  (B2,0(R6)),(6,8(R2)),1,ALIGN=LEFT,ZERO=BLANK                     
*        FOUT  (R2)                                                             
*        LA    R6,2(R6)                                                         
*        BCT   R5,FMT4E                                                         
*                                                                               
FMT5     DS    0H                  DISPLAY MONTH TOTALS                         
         LA    R2,HUTMTH1H                                                      
         LA    R5,12               FOR 12 MTHS                                  
         LA    R6,NAHUTS                                                        
         B     *+8                                                              
FMT5B    BAS   RE,NEXTMNTH                                                      
*  CHECK FOR 5 WEEK MONTH                                                       
         MVI   WEEK5SW,C'N'                                                     
         C     R5,=F'1'             DECEMBER                                    
         BE    FMT5C                                                            
         C     R5,=F'5'             AUGUST                                      
         BE    FMT5C                                                            
         C     R5,=F'7'             JUNE                                        
         BE    FMT5C                                                            
         C     R5,=F'9'             APRIL                                       
         BNE   *+8                                                              
FMT5C    MVI   WEEK5SW,C'Y'                                                     
         XC    8(L'HUTMTH1,R2),8(R2)                                            
         LH    R1,0(R6)            WEEK 1                                       
         AH    R1,2(R6)            WEEK 2                                       
         AH    R1,4(R6)            WEEK 3                                       
         AH    R1,6(R6)            WEEK 4                                       
         CLI   WEEK5SW,C'Y'                                                     
         BNE   *+8                                                              
         AH    R1,8(R6)            WEEK 5                                       
         LTR   R1,R1                                                            
         BZ    FMT5M                                                            
         SR    RF,RF                                                            
         OC    0(2,R6),0(R6)       WEEK 1                                       
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         OC    2(2,R6),2(R6)       WEEK 2                                       
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         OC    4(2,R6),4(R6)       WEEK 3                                       
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         OC    6(2,R6),6(R6)       WEEK 4                                       
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         CLI   WEEK5SW,C'Y'                                                     
         BNE   *+18                                                             
         OC    6(2,R6),8(R6)       WEEK 5                                       
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         CH    RF,=H'1'            ONLY 1 WEEK                                  
         BE    FMT5K                                                            
         SR    R0,R0                                                            
         SLL   R1,1                                                             
         DR    R0,RF               DIVIDE BY 2, 3, 4 OR 5 WEEKS                 
         AH    R1,=H'1'            ROUND                                        
         SRA   R1,1                                                             
FMT5K    CVD   R1,DUB                                                           
         EDIT  (P8,DUB),(6,8(R2)),1,ALIGN=LEFT                                  
FMT5M    FOUT  (R2)                                                             
         LA    R6,8(R6)                                                         
         CLI   WEEK5SW,C'Y'                                                     
         BNE   *+8                                                              
         LA    R6,2(R6)             BUMP PAST 5TH WEEK                          
         BCT   R5,FMT5B                                                         
         B     FMT5X                                                            
*                                                                               
         CLI   NAHUTLEN,X'64'      IS IT 48 WEEK HUTS                           
         BE    FMT5X                                                            
         LA    R2,HUTJAN1H         NO/52 WEEKS/RECALCULATE MNTH AV              
         BAS   RE,NEXT5WK                                                       
         LA    R5,4                                                             
         LA    R3,4                                                             
         XC    FULL,FULL           USE FULL AS TEMP STORE                       
         LA    R6,NAHUTS49                                                      
         OC    0(2,R6),0(R6)       IF 5WK=0,LEAVE AV AS IS                      
         BZ    FMT5A8                                                           
FMT5A5   ZIC   R1,0(R2)            5WK FIELD                                    
         BCTR  R1,0                5WK=7,4WK=6                                  
         SLA   R1,2                MULT BY 4                                    
         SR    R2,R1               GET START OF 4 WEEK FIELD                    
         SR    R0,R0               R0 - COUNT OF WEEKS                          
         B     *+8                                                              
FMT5A6   BAS   RE,NEXTUN             ADD UP 4 4WEEK FIELDS INTO FULL            
         CLC   8(7,R2),SPACES                                                   
         BE    FMT5A6A                                                          
         CLI   8(R2),0             OR NOTHING THERE                             
         BE    FMT5A6A                                                          
*        ZIC   RE,7(R2)              L'OF FIELD                                 
         LA    RE,6                                                             
         SR    R4,R4                 GET LENGTH OF INPUT                        
         LA    R1,8(R2)                                                         
FMT5LP   CLI   0(R1),X'40'                                                      
         BNH   GOTIT                                                            
         LA    R4,1(R4)                                                         
         LA    R1,1(R1)                                                         
         BCT   RE,FMT5LP                                                        
GOTIT    GOTO1 VCASHVAL,DMCB,(1,8(R2)),(R4)                                     
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,4(R1)                                                         
         LTR   R4,R4               WEEKS 1 THRU 4                               
         BZ    FMT5A6A                                                          
         LR    R1,R0                                                            
         LA    R1,1(R1)                                                         
         LR    R0,R1               INCREMENT WEEK COUNT                         
         A     R4,FULL                                                          
         ST    R4,FULL                                                          
FMT5A6A  BCT   R5,FMT5A6                                                        
*                              ADD 5TH WEEK VALU TO 4WEEK TOT IN FULL           
FMT5A7   MVC   HALF,0(R6)                                                       
         L     R4,FULL                                                          
         XC    FULL,FULL                                                        
         LTR   R4,R4               WEEK 5                                       
         BZ    *+12                                                             
         LR    R1,R0                                                            
         LA    R1,1(R1)                                                         
         LR    R0,R1               INCREMENT WEEK COUNT                         
         AH    R4,HALF                                                          
         SPACE                                                                  
         CH    R0,=H'1'            BYPASS IF NOT 2 WEEKS OR MORE                
         BNH   FMT5A8                                                           
         SR    R5,R5               NOW ROUND                                    
         SRDA  R4,31                                                            
         DR    R4,R0               DIVIDE BY 2, 3, 4 OR 5 WEEKS                 
         LTR   R5,R5                                                            
         BM    *+8                                                              
         AH    R5,=H'1'                                                         
         SRA   R5,1                ROUNDED ANSWER IN R5                         
         BAS   RE,NEXTMNTH                                                      
         EDIT  (R5),(6,8(R2)),1,ALIGN=LEFT,ZERO=BLANK                           
         FOUT  (R2)                                                             
FMT5A8   BAS   RE,NEXT5WK                                                       
         LTR   R1,R1         NO MORE 5WK FIELDS/END OF 5 WK AV RTN              
         BNZ   FMT5X                                                            
         LA    R5,4                RESET BCT LIMIT OF 4 WEEK FIELDS             
         LA    R6,2(R6)            BUMP TO NXT HUT IN ELEM                      
         OC    0(2,R6),0(R6)                                                    
         BZ    FMT5A8                                                           
         BCT   R3,FMT5A5                                                        
*                                                                               
FMT5X    DS    0H                                                               
         CLI   SVACT,C'S'          DDS DISPLAY                                  
         BNE   FMT5X4                                                           
         XC    HUTACT,HUTACT                                                    
         MVC   WORK(30),=CL30'DDS STANDARD HUTS (DIARY)'                        
         CLI   SVKEY+3,C'Z'                                                     
         BNE   *+14                                                             
         MVC   WORK+18(10),=C'(ASCRIBED)'                                       
         B     FMT5X2                                                           
         CLI   SVKEY+3,C'I'                                                     
         BNE   FMT5X2                                                           
         MVC   WORK+18(12),=C'(INTEGRATED)'                                     
FMT5X2   FOUT  HUTACTH,WORK,30                                                  
         B     FMT5X10                                                          
*                                                                               
FMT5X4   CLI   HUTSPECH+5,0        CHK FOR INPUT IN SPECIAL                     
         BE    FMT5XX                                                           
         XC    HUTACT,HUTACT                                                    
FMT5X5   FOUT  HUTACTH,HUTSPEC,22                                               
FMT5X10  FOUT  HUTSPECH,SPACES,22                                               
         MVI   SVFMTSW,0           SET TO FORMAT MODE                           
         OI    HUTJAN1H+1,X'01'    SET TO MODIFIED                              
         B     EXXMOD                                                           
*                                                                               
FMT5XX   XC    HUTACT,HUTACT                                                    
         LA    R7,REC+24                                                        
         CLI   0(R7),X'01'                                                      
         BE    FMT6                                                             
         MVI   ELCODE,X'01'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   FMT6X                                                            
*                                                                               
FMT6     MVC   HUTACT(13),=C'LAST ACTIVITY'                                     
         USING NAHEL01,R7                                                       
         GOTO1 VDATCON,DMCB,(3,NAHACTD),(5,HUTACT+14)                           
         MVC   HUTACT+24(3),=C'ADD'                                             
         CLI   NAHACT,C'A'                                                      
         BE    *+10                                                             
         MVC   HUTACT+24(6),=C'CHANGE'                                          
*                                                                               
FMT6X    FOUT  HUTACTH                                                          
         B     EXXMOD                                                           
         DROP  R7                                                               
         EJECT                                                                  
FMTDDS   DS    0H                  DISPLAY HUTS FROM DEMO FILE                  
         XC    NAHREC(150),NAHREC                                               
         LA    R7,NAHUTEL                                                       
         MVI   0(R7),X'90'                                                      
         MVI   1(R7),X'64'                                                      
         CLI   SVKEY+3,C'A'        IS IT 48 WEEK HUT                            
         BE    *+8                                                              
         MVI   1(R7),X'6C'         NO-52 WEEKS                                  
         LA    R7,2(R7)                                                         
         ST    R7,HUTHOOKA         SET HUTHOOKA TO NAHUTS                       
         SH    R7,=H'2'            RESET R7 TO NAHUTEL                          
         USING NAHUTEL,R7                                                       
         SPACE                                                                  
         LA    R6,REC+500          SET UP GETHUT BLOCK                          
         XC    0(100,R6),0(R6)                                                  
         USING GHBLOCK,R6                                                       
         SPACE                                                                  
         MVI   GHSCHEME,X'FE'      READ FROM DEMO FILE                          
         MVI   GHAVE,C'W'          RETURN WEEKLY AVERAGE                        
         CLI   SVKEY+3,C'A'        TEST 52 WEEK OPTION                          
         BE    FMTDDS3                                                          
         MVI   GH52,C'Y'           YES-52 WEEKS                                 
         CLI   SVKEY+3,C'Z'        TEST ASCRIBED OPTION                         
         BNE   *+12                                                             
         MVI   GHBKTYPE,C'A'       YES-ASCRIBED                                 
         B     FMTDDS3                                                          
         CLI   SVKEY+3,C'I'        TEST INTEGRATED OPTION                       
         BNE   FMTDDS3                                                          
         MVI   GHBKTYPE,C'I'       YES-INTEGRATED                               
FMTDDS3  MVC   GHREPDAY,SVKEY+4          SET DAY                                
         MVC   GHMILTIM(2),SVKEY+5       SET START                              
         MVC   GHMILTIM+2(2),SVKEY+5     END TIME                               
         MVC   GHYEAR,SVKEY+7            SET YEAR                               
         MVI   GHNYEARS,1                SET NUMB OF YEARS BACK                 
         MVC   GHBOOKS(1),GHYEAR                                                
         MVI   GHBOOKS+1,1                                                      
         MVC   GHBOOKS+2(1),GHYEAR                                              
         MVI   GHBOOKS+3,52                                                     
         CLI   GHYEAR,95                                                        
         BNE   FMTDD95X                                                         
         MVI   GHBOOKS+1,2                                                      
         MVI   GHBOOKS+3,53                                                     
         MVC   FIVEWEEK,WK95VALS                                                
FMTDD95X DS    0C                                                               
         MVC   GHAGY,SVAALPHA      ALPHA AGENCY CODE                            
         MVI   GHZERO,X'FF'        RETURN PARTIAL DATA                          
         SPACE                                                                  
         MVC   GHCOMFCS,VCOMFACS                                                
         GOTO1 VCALLOV,DMCB,0,X'D9000A33'    GET GETHUT                         
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                ERROR. DIE.                                  
         L     RF,DMCB                                                          
         XC    DMCB,DMCB                                                        
         LA    R1,HUTHOOK                                                       
         ST    R1,GHHOOK                                                        
         GOTO1 (RF),DMCB,GHBLOCK                                                
         CLI   GH52,C'Y'                                                        
         BNE   FMTDDSX                                                          
         LA    R2,NAHUTS49                                                      
         LA    R3,FIVEWEEK                                                      
         LA    R4,4                                                             
FMTDDS5  MVC   0(2,R2),1(R3)                                                    
         LA    R2,2(R2)                                                         
         LA    R3,3(R3)                                                         
         BCT   R4,FMTDDS5                                                       
FMTDDSX  B     FMTHUTS                                                          
         SPACE 2                                                                
HH94WK   DS    0C                                                               
         DC    AL1(0,1,2,3,4,5,6,7,8,9,10,11,12)                                
         DC    AL1(13,14,15,16,49,17,18,19,20,21,22,23,24,50)                   
         DC    AL1(25,26,27,28,29,30,31,32,51,33,34,35,36)                      
         DC    AL1(37,38,39,40,41,42,43,44,45,46,47,48,52)                      
         DS    0F                                                               
HUTHOOK  NTR1                                                                   
         ZIC   R1,GHHOOKWK         USE WEEK FOR DISPLACEMENT                    
         CLI   GHYEAR,95                                                        
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         CLI   GHYEAR,94           CONVERT FLAT TO OLD WEEKS                    
         BNE   HH93                                                             
         LA    RF,HH94WK(R1)        94 00 HUT RECORD IS MESSED UP               
         ZIC   R1,0(RF)                                                         
*                                                                               
HH93     CLI   GH52,C'Y'           IS IT 52 WEEKS                               
         BNE   HUTHK5                                                           
         L     R2,A5WEEK           YES/FIND 5TH WEEKS                           
HUTHK1   ZIC   R0,0(R2)                                                         
         CR    R0,R1               IS IT A 5TH WEEK                             
         BH    HUTHK5                 - NO                                      
         BE    HUTHK3                 - YES                                     
         LA    R2,3(R2)               - HOPEFULLY                               
         B     HUTHK1                                                           
         SPACE                                                                  
HUTHK3   MVC   1(2,R2),GHHOOKHT    SET 5TH WEEK IN SAVE AREA                    
         ST    R2,A5WEEK           INCREMENT A5WEEK                             
         B     HUTHKX                                                           
         SPACE                                                                  
HUTHK5   L     R2,HUTHOOKA                                                      
         BCTR  R1,0                                                             
         SLL   R1,1                                                             
         AR    R2,R1                                                            
         MVC   0(2,R2),GHHOOKHT    MOVE HUT TO REC ELEM                         
         SPACE                                                                  
HUTHKX   XIT1                                                                   
         DROP  R7                                                               
         EJECT                                                                  
FMTPCT   DS    0H                  PERCENTAGE ADJUSTMENT                        
         LA    R2,HUTSPECH                                                      
         CLI   SVACT,C'A'                                                       
         BE    EDTERR              NO PCT ADJUSTMENT ON ADDS                    
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC              REREAD REC                                   
FMTPC    DS    0H                                                               
         LA    R7,REC+24                                                        
         MVI   ELCODE,X'90'                                                     
         CLI   0(R7),X'90'                                                      
         BE    FMTPC1                                                           
         BAS   RE,NEXTEL                                                        
         BE    FMTPC1                                                           
         DC    H'0'                MUST FIND 90 ELEM                            
         USING NAHUTEL,R7                                                       
*                                                                               
FMTPC1   LA    R6,NAHUTS                                                        
         LA    R5,MAXHUTS                                                       
*                                  BUILD AND EXPRESSION FOR CASHVAL             
*                                  TO EDIT                                      
FMTPC1A  XC    WORK(20),WORK                                                    
         ZIC   R4,5(R2)                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   WORK+7(0),8(R2)        MOVE ADJUSTMENT                           
         LA    R3,WORK+7                                                        
         AR    R3,R4               POINT TO LAST CHAR                           
         CLI   0(R3),C'%'                                                       
         BE    FMTPC2                                                           
         MVI   1(R3),C'%'          % NOT INPUT - ADD IT                         
         LA    R4,1(R4)                                                         
*                                                                               
FMTPC2   DS    0H                                                               
         AH    R4,=H'8'            SET TOTAL LENGHT OF EXPRESSION               
FMTPD    LH    R0,0(R6)                                                         
         LTR   R0,R0                                                            
         BZ    FMTPH                                                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+1(6),DUB                                                    
         MVC   WORK(4),WORK+1                                                   
         MVI   WORK+4,C'.'         ALTER TO N.NN FOR CASH VAL                   
         GOTO1 VCASHVAL,DMCB,WORK,(R4)                                          
         CLI   DMCB,0                                                           
         BE    FMTPE                                                            
FMTPERR  MVI   ERRCD,INVERR                                                     
         B     LFMERR                                                           
*                                                                               
FMTPE    L     R0,DMCB+4                                                        
         C     R0,=F'0'                                                         
         BNH   FMTPERR             CAN'T GO NEGATIVE                            
         C     R0,=F'1000'                                                      
         BNH   FMTPH               IF EXCEEDS 100.0 SET TO 100.0                
         LH    R0,=H'1000'                                                      
FMTPH    STH   R0,0(R6)                                                         
         LA    R6,2(R6)                                                         
         BCT   R5,FMTPD                                                         
*                                                                               
         CLI   NAHUTLEN,X'64'      TEST IF 52 WEEK HUTS                         
         BE    FMTPX                                                            
         LA    R1,NAHUTS52         CHK IF EXTRA WEEKS ALREADY DONE              
         CR    R6,R1                                                            
         BE    FMTPX                                                            
         LA    R6,NAHUTS49         NO/DO EXTRA WEEKS                            
         LA    R5,4                RESET BCT FOR 4 EXTRA WEEKS                  
         BCT   R5,FMTPD                                                         
*                                                                               
FMTPX    DS    0H                                                               
         B     FMTHUTS                                                          
         EJECT                                                                  
EDT      DS    0H                                                               
         CLI   HUTSPECH+5,0        CHK FOR INPUT IN SPECIAL                     
         BE    EDT0                NO                                           
         B     FMTPCT              MUST BE PCT ADJUSTMENT                       
*                                                                               
EDT0     CLI   SVACT,C'A'          SEE IF ADD                                   
         BE    EDT1C                                                            
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC              REREAD REC                                   
*                                                                               
         B     EDT1E                                                            
*                                                                               
EDT1C    LA    R7,REC+24                                                        
         MVC   NAHLEN,=H'24'      INITIALIZE REC LEN                            
*                                                                               
EDT1E    DS    0H                                                               
*                                                                               
EDT2     LA    R2,HUTJAN1H         CURSOR TO FIRST FIELD                        
         GOTO1 ANY                 REQUIRED                                     
         CLI   SVACT,C'A'          NO DELETE FOR ADD                            
         BE    EDT6                                                             
         CLC   8(6,R2),=C'DELETE'                                               
         BNE   EDT3                                                             
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,SVKEY                                                        
         MVI   KEY+13,X'80'                                                     
         MVC   COMMAND,=CL8'DMWRT'                                              
         GOTO1 DIR                                                              
*                                                                               
         MVI   REC+15,X'C0'                                                     
         GOTO1 PUTREC                                                           
         FOUT  LFMMSGH,=C'** HUTREC DELETED **',20                              
         MVI   ERRAREA,01                                                       
         LA    R2,LFMRECH          CURSOR TO RECORD                             
         B     EXXMOD                                                           
*                                                                               
EDT3     LA    R7,REC+24                                                        
         MVI   ELCODE,X'90'                                                     
         CLI   0(R7),X'90'                                                      
         BE    EDT6                                                             
         BAS   RE,NEXTEL                                                        
         BE    EDT6                                                             
         DC    H'0'                MUST FIND 02 ELEM                            
*                                                                               
EDT6     XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'906C'       SET CODE AND LENGHT                       
EDT6A    LA    R7,ELEM                                                          
         USING NAHUTEL,R7                                                       
         LA    R2,HUTJAN1H                                                      
         LA    R5,MAXHUTS                                                       
         LA    R6,NAHUTS                                                        
*                                                                               
EDT6B    BAS   RE,EDTFLD                                                        
         LA    R6,2(R6)            NEXT WEEK                                    
         BAS   RE,NEXTUN           SET R2 TO NEXT 4WK FIELD                     
         BCT   R5,EDT6B                                                         
*                                                                               
*        LA    R2,HUTJAN1H         SEE IF 52 WEEK HUTS                          
*        BAS   RE,NEXT5WK                                                       
*        CLI   5(R2),0                                                          
*        BE    EDT7                                                             
*        MVC   ELEM(2),=X'906C'     SET LENGTH TO 108                           
*        LA    R5,4                RESET BCT FOR NXT 4 WEEKS                    
* EDT6G  BAS   RE,EDTFLD                                                        
*        LA    R6,2(R6)            NEXT WEEK                                    
*        BAS   RE,NEXT5WK          SET R2 TO NEXT 5WK FIELD                     
*        BCT   R5,EDT6G                                                         
*                                                                               
EDT7     DS    0H                                                               
         EJECT                                                                  
EDT8     DS    0H                  CHECK FOR PERCENT ADJ                        
*                                  NOW IN FMT LOGIC                             
EDT20    DS    0H                                                               
*                                                                               
EDT20B   LA    R7,REC+24                                                        
         MVI   ELCODE,X'90'                                                     
         CLI   0(R7),X'90'                                                      
         BE    EDT20D                                                           
         BAS   RE,NEXTEL                                                        
         BE    EDT20D                                                           
         CLI   SVACT,C'A'          SEE IF ADD                                   
         BE    EDT20F                                                           
         DC    H'0'                MUST FIND 90 ELEM                            
*                                                                               
EDT20D   DS    0H                                                               
*        CLC   ELEM(2),0(R7)       CAN NOT CHA FROM 48-52 WEEKS                 
*        MVI   ERRCD,NOFNDERR                                                   
*        BNE   LFMERR                                                           
         GOTO1 VRECUP,DMCB,(0,REC),0(R7),0      DELETE OLD 90                   
*                                                                               
EDT20F   GOTO1 VRECUP,DMCB,(0,REC),ELEM,0(R7)       ADD NEW ONE                 
WRITE    DS    0H                                                               
         BAS   RE,ACTIVITY         ADD OR UPDATE ACTIVITY ELEM                  
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         MVC   KEY,SVKEY                                                        
         MVC   REC(13),SVKEY                                                    
         CLI   SVACT,C'A'          SEE IF ADD                                   
         BE    ADDNHUT                                                          
         LA    R0,REC2                                                          
         ST    R0,AREC                                                          
         GOTO1 GETREC              REREAD REC                                   
         ST    R8,AREC                                                          
         GOTO1 PUTREC                                                           
         B     FMT                                                              
*                                                                               
ADDNHUT  GOTO1 ADDREC                                                           
         MVC   SVKEY,KEY                                                        
         B     FMT                 GO REFORMAT RECORD                           
         EJECT                                                                  
ACTIVITY NTR1                                                                   
         XC    ELEM+200(10),ELEM+200                                            
         MVC   ELEM+200(2),=X'0108'                                             
         LA    R6,ELEM+200                                                      
         USING NAHEL01,R6                                                       
         GOTO1 VDATCON,DMCB,(5,0),(3,NAHACTD)                                   
*                                  SET ACTIVITY DATE - TODAY                    
         MVC   NAHACT,SVACT        SAVE ACTION                                  
         LA    R7,REC+24                                                        
         CLI   0(R7),X'01'                                                      
         BE    ACT2                                                             
         MVI   ELCODE,X'01'                                                     
         BAS   RE,NEXTEL                                                        
         BE    ACT2                                                             
         LA    R7,REC+24           ADD AS FIRST ELEM+200                        
         GOTO1 VRECUP,DMCB,(0,REC),ELEM+200,0(R7)                               
         B     ACTX                                                             
*                                                                               
ACT2     MVC   0(8,R7),ELEM+200    SWITCH OLD AND NEW ELEMS                     
*                                                                               
ACTX     XIT1                                                                   
         EJECT                                                                  
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R7)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  LTR   R7,R7                                                            
         BR    RE                                                               
         SPACE 2                                                                
NEXTUN   DS    0H                  48 WEEK FIELD L'=6                           
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),0             END OF SCREEN                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    NEXTUN                                                           
*        CLI   0(R2),X'0F'         TEST 5 WEEK FIELD                            
*        BE    NEXTUN                                                           
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
NEXT5WK  DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             END OF SCREEN                                
         BE    N5X                                                              
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    NEXT5WK                                                          
         CLI   0(R2),X'0F'         TEST 5 WEEK FIELD L'=7                       
         BNE   NEXT5WK                                                          
         SR    R1,R1               R1=0, FOUND FIELD                            
         B     *+8                                                              
N5X      LA    R1,1                                                             
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
CLRSCRN  NTR1                                                                   
         LA    R2,HUTJAN1H                                                      
CLR3     ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         FOUT  (R2)                                                             
CLR5     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             END OF SCREEN                                
         BE    CLRX                                                             
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    CLR5                                                             
         B     CLR3                                                             
CLRX     XIT1                                                                   
         SPACE 2                                                                
*                                                                               
NEXTMNTH DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             END OF SCREEN                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    1(R2),X'20'         TEST PROTECTED                               
         BNO   NEXTMNTH                                                         
         CLI   0(R2),X'10'         TEST MONTH FIELD L'=8                        
         BNE   NEXTMNTH                                                         
         BR    RE                                                               
         EJECT                                                                  
*********************************************                                   
* CALLED FROM EDT MODE TO EDIT WEEK FIELDS  *                                   
*                                           *                                   
*********************************************                                   
         DS    F                                                                
EDTFLD   DS    0H                                                               
         ST    RE,EDTFLD-4                                                      
         CLI   5(R2),0                                                          
         BNE   EF5                                                              
         SR    R0,R0                                                            
         B     EF7                                                              
*                                                                               
EF5      ZIC   R3,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,8(R2),(R3)                                         
         CLI   DMCB,0                                                           
         BNE   EDTERR                                                           
         L     R0,DMCB+4                                                        
         C     R0,=F'0'                                                         
         BNH   EDTERR              CAN'T BE NEGATIVE                            
         CVD   R0,DUB                                                           
         DP    DUB,=P'10'                                                       
         CP    DUB+6(2),=P'0'      MUST GET ZERO REMAINDER                      
         BNE   EDTERR              ONE DECIMAL                                  
         CP    DUB(6),=P'1000'     CAN'T EXCEED 100.0                           
         BH    EDTERR                                                           
         MVC   WORK(6),DUB                                                      
         ZAP   DUB,WORK(6)                                                      
         CVB   R0,DUB                                                           
EF7      STH   R0,0(R6)                                                         
         L     RE,EDTFLD-4                                                      
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
EDTERR   MVI   ERRCD,INVERR                                                     
*                                                                               
LFMERR   GOTO1 ERROR                                                            
*                                                                               
MAXHUTS  EQU   52    USED TO BE 48                                              
*                                                                               
*        LFM TO CONTROL FILE DAY CONVERSION                                     
*                                                                               
TRDAY    DC    X'007C'             M-F                                          
         DC    X'0140'             MON                                          
         DC    X'0220'             TUE                                          
         DC    X'0310'             WED                                          
         DC    X'0408'             THR                                          
         DC    X'0504'             FRI                                          
         DC    X'0602'             SAT                                          
         DC    X'0701'             SUN                                          
         DC    X'087F'             M-S                                          
         DC    X'FFFF'                                                          
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPLFMWRK                                                       
         ORG   LFMTABH                                                          
*SPLFMC3                                                                        
*      ++INCLUDE SPLFMC3AD                                                      
       ++INCLUDE SPLFMC3D                                                       
         ORG   HUTWORK                                                          
HUTHOOKA DS    F                                                                
A5WEEK   DS    F                                                                
FIVEWEEK DS    0CL12         SAVE EXTRA 4 WEEKS HERE                            
WEEKNUMB DS    CL1                                                              
WEEKHUT  DS    CL2                                                              
         DS    CL9                                                              
WEEK5SW  DS    CL1                                                              
         EJECT                                                                  
*                                                                               
       ++INCLUDE NEGETHUTD                                                      
         EJECT                                                                  
       ++INCLUDE SPGENHUT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'080SPLFM43   05/01/02'                                      
         END                                                                    
