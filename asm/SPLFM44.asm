*          DATA SET SPLFM44    AT LEVEL 031 AS OF 05/01/02                      
*PHASE T21944A,+0                                                               
SPLFM44  TITLE '-  SPTFILE MAINT - AGENCY HUT REC - MONTHLY'                    
T21944   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21944                                                         
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
WEEKVALS DC    X'310000320000330000340000'                                      
*               5APR/2CLHUT/5JUN/2CLHUT/5AUG/2CLHUT/5DEC/2CLHUT                 
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
FMT      DS    0H                                                               
         CLI   SVACT,C'S'          DDS DISPLAY                                  
         BNE   FMT0                                                             
         B     FMTDDS                                                           
*                                                                               
FMT0     CLI   HUTSPECH+5,0                                                     
         BE    FMT1                                                             
         B     FMTPCT              MUST BE PERCENTAGE                           
*                                                                               
FMT1     MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
         LA    R7,REC+24                                                        
FMTHUTS  CLI   0(R7),X'90'         CAN GET HERE FROM FMTDDS OR FMTPCT           
         BE    FMT2                                                             
         MVI   ELCODE,X'90'                                                     
         BAS   RE,NEXTEL                                                        
         BE    FMT2                                                             
         DC    H'0'                MUST FIND 02 ELEM                            
FMT2     DS    0H                                                               
         USING NAHUTEL,R7                                                       
         LA    R2,HUTJAN1H                                                      
         LA    R5,MAXMTHS                                                       
         LA    R6,NAHUTS                                                        
*                                                                               
*  CHECK FOR 5 WEEK MONTH                                                       
FMT4     MVI   WEEK5SW,C'N'                                                     
         C     R5,=F'1'             DECEMBER                                    
         BE    FMT4A                                                            
         C     R5,=F'5'             AUGUST                                      
         BE    FMT4A                                                            
         C     R5,=F'7'             JUNE                                        
         BE    FMT4A                                                            
         C     R5,=F'9'             APRIL                                       
         BNE   *+8                                                              
FMT4A    MVI   WEEK5SW,C'Y'                                                     
*                                                                               
         XC    8(L'HUTJAN1,R2),8(R2)                                            
         LH    R1,0(R6)            WEEK 1                                       
         AH    R1,2(R6)            WEEK 2                                       
         AH    R1,4(R6)            WEEK 3                                       
         AH    R1,6(R6)            WEEK 4                                       
         CLI   WEEK5SW,C'Y'                                                     
         BNE   *+8                                                              
         AH    R1,8(R6)             WEEK5                                       
         LTR   R1,R1                                                            
         BZ    FMT4F                                                            
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
         OC    8(2,R6),8(R6)       WEEK 5                                       
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         CH    RF,=H'1'            ONLY 1 WEEK                                  
         BE    FMT4D                                                            
         SR    R0,R0                                                            
         SLL   R1,1                                                             
         DR    R0,RF               DIVIDE BY 2, 3, 4 OR 5 WEEKS                 
         AH    R1,=H'1'            ROUND                                        
         SRA   R1,1                                                             
FMT4D    CVD   R1,DUB                                                           
         EDIT  (P8,DUB),(6,8(R2)),1,ALIGN=LEFT                                  
FMT4F    FOUT  (R2)                                                             
         BAS   RE,NEXTUN                                                        
         LA    R6,8(R6)            NEXT MONTH                                   
         CLI   WEEK5SW,C'Y'                                                     
         BNE   *+8                                                              
         LA    R6,2(R6)                                                         
         BCT   R5,FMT4                                                          
         B     FMT5                                                             
*                                                                               
*  SINCE WE WENT TO A 52 WEEK HUT                                               
*  THE FOLLOWING CODE IS NEVER USED                                             
*  BUT HAS NOT BEEN DELETED JUST IN CASE                                        
*                                                                               
         CLI   NAHUTLEN,X'64'      IS IT 48 WEEKS                               
         BE    FMT5                                                             
         LA    R2,HUTAPR1H     RECALCULATE 5 WEEK MONTHS                        
         LA    R3,NAHUTS49                                                      
         LA    R4,NAHUTS+24         BEG OF APR HUTS                             
         BAS   RE,CALCDISP                                                      
         LA    R2,HUTJUN1H                                                      
         LA    R3,2(R3)                                                         
         LA    R4,NAHUTS+40         BEG OF JUN HUTS                             
         BAS   RE,CALCDISP                                                      
         LA    R2,HUTAUG1H                                                      
         LA    R3,2(R3)                                                         
         LA    R4,NAHUTS+56         BEG OF AUG HUTS                             
         BAS   RE,CALCDISP                                                      
         LA    R2,HUTDEC1H                                                      
         LA    R3,2(R3)                                                         
         LA    R4,NAHUTS+88         BEG OF DEC HUTS                             
         BAS   RE,CALCDISP                                                      
*                                                                               
FMT5     DS    0H                                                               
         CLI   SVACT,C'S'          DDS DISPLAY                                  
         BNE   FMT5B                                                            
         XC    HUTACT,HUTACT                                                    
         MVC   WORK(30),=CL30'DDS STANDARD HUTS (DIARY)'                        
         CLI   SVKEY+3,C'Z'                                                     
         BNE   *+14                                                             
         MVC   WORK+18(10),=C'(ASCRIBED)'                                       
         B     FMT5A                                                            
         CLI   SVKEY+3,C'I'                                                     
         BNE   FMT5A                                                            
         MVC   WORK+18(12),=C'(INTEGRATED)'                                     
FMT5A    FOUT  HUTACTH,WORK,30                                                  
         B     FMT5E                                                            
FMT5B    CLI   HUTSPECH+5,0        CHK FOR INPUT IN SPECIAL                     
         BE    FMT5X                                                            
         XC    HUTACT,HUTACT                                                    
FMT5C    FOUT  HUTACTH,HUTSPEC,22                                               
FMT5E    FOUT  HUTSPECH,SPACES,22                                               
         MVI   SVFMTSW,0           SET TO FORMAT MODE                           
         OI    HUTJAN1H+1,X'01'    SET TO MODIFIED                              
         B     EXXMOD                                                           
*                                                                               
FMT5X    DS    0H                                                               
         FOUT  HUTSPECH,SPACES,22                                               
*                                                                               
         XC    HUTACT,HUTACT                                                    
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
FMTDDS   DS    0H                                                               
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
FMTDDS3  MVC   GHREPDAY,SVKEY+4             SET DAY                             
         MVC   GHMILTIM(2),SVKEY+5       SET START-                             
         MVC   GHMILTIM+2(2),SVKEY+5     END TIME                               
         MVC   GHYEAR,SVKEY+7            SET YEAR                               
         MVI   GHNYEARS,1                SET NUMB OF YEARS BACK                 
         MVC   GHBOOKS(1),GHYEAR                                                
         MVI   GHBOOKS+1,1                                                      
         MVC   GHBOOKS+2(1),GHYEAR                                              
         MVI   GHBOOKS+3,52                                                     
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
         CLI   GHYEAR,94           CONVERT FLAT TO OLD WEEKS                    
         BNE   HH93                                                             
         LA    RF,HH94WK(R1)       94 00 HUT RECORD IS OUT OF SYNC              
         ZIC   R1,0(RF)                                                         
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
         BE    EDTERR                                                           
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC              REREAD REC                                   
         LA    R7,REC+24                                                        
         MVI   ELCODE,X'90'                                                     
         CLI   0(R7),X'90'                                                      
         BE    FMTP4                                                            
         BAS   RE,NEXTEL                                                        
         BE    FMTP4                                                            
         DC    H'0'                MUST FIND 02 ELEM                            
*                                                                               
         USING NAHUTEL,R7                                                       
*                                                                               
FMTP4    LA    R6,NAHUTS                                                        
         LA    R5,MAXHUTS                                                       
*                                  BUILD AND EXPRESSION FOR CASHVAL             
*                                  TO EDIT                                      
         XC    WORK(20),WORK                                                    
         ZIC   R4,5(R2)                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   WORK+7(0),8(R2)        MOVE ADJUSTMENT                           
         LA    R3,WORK+7                                                        
         AR    R3,R4               POINT TO LAST CHAR                           
         CLI   0(R3),C'%'                                                       
         BE    FMTP8C2                                                          
         MVI   1(R3),C'%'          % NOT INPUT - ADD IT                         
         LA    R4,1(R4)                                                         
*                                                                               
FMTP8C2  DS    0H                                                               
         AH    R4,=H'8'            SET TOTAL LENGHT OF EXPRESSION               
FMTP8D   LH    R0,0(R6)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+1(6),DUB                                                    
         MVC   WORK(4),WORK+1                                                   
         MVI   WORK+4,C'.'         ALTER TO N.NN FOR CASH VAL                   
         GOTO1 VCASHVAL,DMCB,WORK,(R4)                                          
         CLI   DMCB,0                                                           
         BE    FMTP8E                                                           
FMTP8ERR MVI   ERRCD,INVERR                                                     
         B     LFMERR                                                           
*                                                                               
FMTP8E   L     R0,DMCB+4                                                        
         C     R0,=F'0'                                                         
         BL    FMTP8ERR            CAN'T GO NEGATIVE                            
         C     R0,=F'1000'         IF EXCEEDS 100.0                             
         BNH   FMTP8H                                                           
         LH    R0,=H'1000'         YES SET TO 100.0                             
FMTP8H   STH   R0,0(R6)                                                         
         LA    R6,2(R6)                                                         
         BCT   R5,FMTP8D                                                        
*                                                                               
         CLI   NAHUTLEN,X'64'      TEST IF 52 WEEK HUTS                         
         BE    FMTP8X                                                           
         LA    R1,NAHUTS52         CHK IF EXTRA WEEKS ALREADY DONE              
         CR    R6,R1                                                            
         BE    FMTP8X                                                           
         LA    R6,NAHUTS49         NO/DO EXTRA WEEKS                            
         LA    R5,4                RESET BCT FOR 4 EXTRA WEEKS                  
         BCT   R5,FMTP8D                                                        
*                                                                               
*                                                                               
FMTP8X   DS    0H                                                               
         B     FMTHUTS                                                          
         EJECT                                                                  
EDT      DS    0H                                                               
         CLI   HUTSPECH+5,0        CHK FOR INPUT IN SPECIAL FIELD               
         BE    EDT1                                                             
         B     FMTPCT              MUST BE PCT ADJUSTMENT                       
*                                                                               
EDT1     CLI   SVACT,C'A'          SEE IF ADD                                   
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
         LA    R2,LFMRECH                                                       
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
EDT6     XC    ELEM(150),ELEM                                                   
         MVC   ELEM(2),=X'906C'       SET CODE AND LENGHT                       
EDT6A    LA    R7,ELEM                                                          
         USING NAHUTEL,R7                                                       
         LA    R2,HUTJAN1H                                                      
         LA    R5,MAXMTHS                                                       
         LA    R6,NAHUTS                                                        
*                                                                               
EDT6B    CLI   5(R2),0                                                          
         BNE   EDT6C                                                            
         SR    R0,R0                                                            
         B     EDT6D                                                            
*                                                                               
EDT6C    ZIC   R3,5(R2)                                                         
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
EDT6D    STH   R0,0(R6)                                                         
         STH   R0,2(R6)                                                         
         STH   R0,4(R6)                                                         
         STH   R0,6(R6)                                                         
         LA    R6,8(R6)                                                         
*  IF R5 = 1,5,7,9 IT MEANS IT'S A FIVE WEEK MONTH                              
         C     R5,=F'1'             DECEMBER                                    
         BE    EDT6F                                                            
         C     R5,=F'5'             AUGUST                                      
         BE    EDT6F                                                            
         C     R5,=F'7'             JUNE                                        
         BE    EDT6F                                                            
         C     R5,=F'9'             APRIL                                       
         BNE   EDT6H                                                            
EDT6F    STH   R0,0(R6)                                                         
         LA    R6,2(R6)                                                         
*                                                                               
EDT6H    BAS   RE,NEXTUN           SET R2 TO NEXT UNPROTECTED FLD               
         BCT   R5,EDT6B                                                         
         EJECT                                                                  
EDT20    DS    0H                                                               
*        CLI   NAHUTLEN,X'64'      IT IT 48 WEEKS                               
*        BE    EDT20B                                                           
*        LA    R5,NAHUTS+24        NO/ 52 WEEKS/ SET EXTRA WEEKS                
*        MVC   NAHUTS49,0(R5)                                                   
*        LA    R5,NAHUTS+40                                                     
*        MVC   NAHUTS50,0(R5)                                                   
*        LA    R5,NAHUTS+56                                                     
*        MVC   NAHUTS51,0(R5)                                                   
*        LA    R5,NAHUTS+88                                                     
*        MVC   NAHUTS52,0(R5)                                                   
*        MVI   ELEM+1,X'6C'        SET NEW LENGTH                               
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
*                                  SET ACITVITY DATE - TODAY                    
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
***********************************                                             
* R2 - MONTH FIELD HEADER                                                       
* R3 - 5TH WEEK HUT VALUE                                                       
* R4 - BEG OF HUT VALUES IN NAHUTS                                              
*                                                                               
************************************                                            
CALCDISP NTR1                                                                   
         LH    R1,0(R4)            WEEK 1                                       
         AH    R1,2(R4)            WEEK 2                                       
         AH    R1,4(R4)            WEEK 3                                       
         AH    R1,6(R4)            WEEK 4                                       
         AH    R1,0(R3)            ADD 5TH WEEK HUT                             
         LTR   R1,R1                                                            
         BZ    CDX                                                              
         SR    RF,RF                                                            
         OC    0(2,R4),0(R4)       WEEK 1                                       
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         OC    2(2,R4),2(R4)       WEEK 2                                       
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         OC    4(2,R4),4(R4)       WEEK 3                                       
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         OC    6(2,R4),6(R4)       WEEK 4                                       
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         OC    0(2,R3),0(R3)       WEEK 5                                       
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         CH    RF,=H'1'            ONLY 1 WEEK                                  
         BE    CD80                                                             
         SR    R0,R0                                                            
         SLL   R1,1                                                             
         DR    R0,RF               DIVIDE BY 2, 3 OR 4 WEEKS                    
         AH    R1,=H'1'            ROUND                                        
         SRA   R1,1                                                             
CD80     LR    R6,R1               ROUNDED ANDWER IN R6                         
         EDIT  (R6),(6,8(R2)),1,ALIGN=LEFT,ZERO=BLANK                           
         FOUT  (R2)                                                             
CDX      XIT1                                                                   
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
NEXTUN   DS    0H                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),0             END OF SCREEN                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    NEXTUN                                                           
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
EDTERR   MVI   ERRCD,INVERR                                                     
*                                                                               
LFMERR   GOTO1 ERROR                                                            
*                                                                               
         EJECT                                                                  
MAXHUTS  EQU   52                                                               
MAXMTHS  EQU   12                                                               
*                                                                               
TRDAY    DC    X'007C'             M-F                                          
         DC    X'0140'             MON                                          
         DC    X'0220'             TUE                                          
         DC    X'0310'             WED                                          
         DC    X'0408'             THU                                          
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
*SPLFMC4                                                                        
       ++INCLUDE SPLFMC4D                                                       
         ORG   HUTWORK                                                          
HUTHOOKA DS    F                                                                
A5WEEK   DS    F                                                                
FIVEWEEK DS    0CL12                                                            
WEEKNUMB DS    CL1                                                              
WEEKHUT  DS    CL2                                                              
         DS    CL9                                                              
WEEK5SW  DS    CL1                                                              
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPGENHUT                                                       
         EJECT                                                                  
       ++INCLUDE NEGETHUTD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031SPLFM44   05/01/02'                                      
         END                                                                    
