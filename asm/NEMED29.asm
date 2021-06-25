*          DATA SET NEMED29    AT LEVEL 057 AS OF 01/31/05                      
*PHASE T31E29A                                                                  
*INCLUDE BINSRCH2                                                               
         TITLE 'T31E29 - WEEKLY GRP FLOW '                                      
T31E29   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NE29**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         LA    RA,4095(RB)                                                      
         LA    RA,1(RA)                                                         
         USING T31E29,RB,RA                                                     
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS3          R7-WORKING STORAG (ANETWS3+500)              
         LA    R7,500(R7)                                                       
         USING WORKD,R7                                                         
         L     R6,ANETWS4                                                       
         USING NDDEMBLK,R6         R6-NDDEMBLK,DEDBLOCK (ANETWS4)               
         ST    R6,NBADEM                                                        
         ST    R2,RELO                                                          
         L     R2,ANETWS2          ANETWS2+500 GETS CLISTSV(880)                
         LA    R2,500(R2)                                                       
         ST    R2,ACLISTSV                                                      
         EJECT                                                                  
*HIPO******************************************************************         
*  TITLE: NEMED86 (T31E29) PRE-BUY SCHEDULE                           *         
*                                                                     *         
*  COMMENTS: WRITES A REPORT THAT BREAKS OUT GOAL AND EST GRPS        *         
*            I.E. GRPS FROM GOAL REC VS FROM UNIT REC                 *         
*                                                                     *         
*  CALLS TO: NETIO,NETGOAL                                            *         
*                                                                     *         
*  GLOBAL: R7-MYWORKD (ANETWS3+500)                                   *         
*          R6-NDDEMBLK,DEDBLOCK (ANETWS4)                             *         
*                                                                     *         
***********************                                               *         
*  LOGIC:  FIRST, PRGM GETS WEEKLIST THROUGH NVWKLST                  *         
*          AND STORES IN PERLIST TO USE IN PRINTING HEADING           *         
*                                                                     *         
*          SECOND, READS GOAL RECS THROUGH NETGOAL                    *         
*                  ADD DATA TO BINREC USING BINSRCH                   *         
*                                                                     *         
*          THIRD, READS UNIT RECS THROUGH NETIO                       *         
*                 ADDS UNIT DATA TO BINREC USING BINSRCH              *         
*                                                                     *         
*ENDHIPO***************************************************************         
         SPACE 3                                                                
         B     LR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
         SPACE                                                                  
********************************************************                        
* GETS WEEKLIST/READS GOAL RECS/READS UNIT RECS/PRINTS                          
*                                                                               
********************************************************                        
         SPACE                                                                  
LR       DS    0H                                                               
         L     R1,=A(DUMMYTBL)                                                  
         A     R1,RELO                                                          
         ST    R1,ADUMMY                                                        
         A     R1,=F'2000'                                                      
         MVI   0(R1),X'FF'                                                      
*                                                                               
         L     R1,=A(WKTOTBL)                                                   
         A     R1,RELO                                                          
         ST    R1,AWKTOTBL                                                      
         L     RE,AWKTOTBL         CLEAR WKTOTBL                                
         L     RF,=F'1280'                                                      
         XCEF                                                                   
         LA    R1,1280(R1)      320 FULL WORDS                                  
         MVI   0(R1),X'FF'      SET WKTOTEND TO FF                              
*                                                                               
*                               * SET UP BINSRCH PARAMETERS                     
         SR    R0,R0               A OF REC TO BE ADDED                         
         LA    R1,BINTABLE         A OF BINTBL                                  
         SR    R2,R2               NUM OF REC IN TBL,UPDATED BY BINSRCH         
         LA    R3,BRECLENE         LENGTH OF REC                                
         LA    R4,BKEYLENE         DISP OF KEY INTO REC                         
         L     R5,=F'4000'         MAX RECS IN BINTBL                           
         STM   R0,R5,BINDMCB                                                    
*                                                                               
         LA    RE,BINTABLE      CLEAR BINTABLE                                  
         L     RF,=F'100000'                                                    
         XCEF                                                                   
         SPACE 2                                                                
*                               *****  GET WEEKLIST  *****                      
*                                                                               
         MVI   NBDATA,C'U'                                                      
         MVI   NBSEQ,C'D'                                                       
         MVI   NBRESUME,NBPROCPK                                                
LR02     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST                                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBVALDAT                                                  
         BNE   LR02                                                             
         MVI   PERTYPE,C'W'        WEEKS ONLY                                   
         LA    R3,15               FOR 15 WEEKS MAX                             
         ST    R3,NUMPER                                                        
         LA    R3,PERLIST                                                       
         NETGO NVWKLST,DMCB,NUMPER,(R3),PERTYPE                                 
         SPACE                                                                  
         BAS   RE,CHKDUPS          CHK FOR DUP DATES                            
         BAS   RE,GETDISP          GET P LINE DIPLACEMENT                       
         SPACE 2                                                                
*                               ****  READ GOAL RECS ****                       
*                                                                               
         L     R5,ANETWS1                                                       
         USING NETGOALD,R5                                                      
         XC    0(150,R5),0(R5)     (AT3/27/85 NETGOAL BLOCK=CL100)              
         LA    R2,NETBLOCK                                                      
         ST    R2,NGANTBLK                                                      
         L     R2,ANETWS1          LIST=160 MAX                                 
         LA    R2,150(R2)                                                       
         ST    R2,NGAPLIST                                                      
         MVI   NGMAXPRD,160                                                     
         LA    R2,NETGOLHK                                                      
         ST    R2,NGAHOOK                                                       
         L     R2,ANETWS2                                                       
         LA    R2,450(R2)                                                       
         USING BINRECD,R2                                                       
         XC    0(BRECLENE,R2),0(R2)                                             
         GOTO1 ANETGOAL,DMCB,NGBLOCK                                            
         B     LR05                                                             
         SPACE                                                                  
*                                                                               
NETGOLHK NTR1                                                                   
         SPACE                                                                  
         MVC   BPRD,NGOALPRD       PRD CODE CL3                                 
         MVC   BPNO,NGOALPNO       PRD NO CL1                                   
         MVC   BSTRT,NGOALWK       WEEK START DATE                              
         MVC   BSLN,NGOALSL        SPOT LENGTH                                  
         MVC   BGGRP,NGOALGRP      GOAL GRP                                     
         MVC   BGOAL,NGOALTRG      GOAL TARGET DEMO                             
         DROP  R2                                                               
*                                                                               
         GOTO1 =V(BINSRCH),BINDMCB,(1,(R2)),RR=RELO                             
         CLI   0(R1),X'01'         TEST IF REC FOUND(X'01'=NOT FOUND)           
         BE    NGX                                                              
         L     R3,0(R1)            REC FOUND/ADD TO GOALS                       
         USING BINRECD,R3                                                       
         L     R4,BGGRP                                                         
         A     R4,NGOALGRP                                                      
         ST    R4,BGGRP                                                         
NGX      B     EXIT                                                             
         DROP  R5,R3                                                            
         EJECT                                                                  
*                             **** READ UNIT RECS ****                          
LR05     DS    0H                                                               
         BAS   RE,DUMMY            SETS UP TABLE OF DUMMY PRDS                  
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         MVI   NBSELUOP,C'A'       ACTUAL SCHDEDULE                             
         NETGO NVDEMOPT,DMCB      EST DEMOS FOR MAKE GOOD/NOT FOR PFB           
         MVI   NBSPLOPT,X'C0'                                                   
         CLI   DEMOPT,C'A'                                                      
         BNE   LR06                                                             
         MVI   NBACTOPT,C'Y'                                                    
         SPACE                                                                  
LR06     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST                                                  
         BE    PRINTIT                                                          
         CLI   NBMODE,NBPROCUN                                                  
         BNE   LR06                                                             
*                                                                               
         L     R2,ANETWS2                                                       
         USING BINRECD,R2                                                       
         XC    0(BRECLENE,R2),0(R2)                                             
         MVC   BPNO,NBSPLPRN       SET PRD NO FROM NETBLOCK                     
         MVC   WORK(1),NBSPLPRN                                                 
         BAS   RE,GETPRD           GET PRD CODE                                 
         MVC   BPRD,PRDSV          SET PRD CODE                                 
         MVC   BSLN,NBLEN          SET SPOT LENGTH                              
         BAS   RE,GETDAT           GET WEEK START DATE                          
         CLI   WORK,X'FF'          CHK DATE OUT OF RANGE                        
         BE    SKIPIT            YES/SO SKIP AND GET NXT UNIT                   
         MVC   BSTRT,WORK        NO/SET STRT DATE                               
         SPACE                                                                  
         BAS   RE,CHKDUMMY         CHK IF UNIT BELONGS TO DUMMY PRD             
         BE    BINIT                                                            
         USING DUMMYD,R5                                                        
         CLC   BPNO,NBPRD2      IS THIS 2ND PIGGY PROD                          
         BE    LR06             YES/SO SKIP ELSE GRPS WILL BE DOUBLED           
*                                   FOR SPLIT RETURNS SAME GRPS TWICE           
         MVC   BPRD,PRDXX        NO/SET DUMMY PRD                               
         MVC   BPNO,PRDXNO                                                      
         MVC   BSLN,PRDXL                                                       
         DROP  R5,R2                                                            
BINIT    GOTO1 =V(BINSRCH),BINDMCB,(1,(R2)),RR=RELO                             
         L     R3,0(R1)                                                         
         USING BINRECD,R3                                                       
         LTR   R3,R3               TEST BINTABLE FULL                           
         BNZ   *+6                                                              
         DC    H'0'                IF YES/BOMB                                  
         SPACE                                                                  
ADDIT    DS    0H                  ADD EST GRP TO BINREC                        
         LA    R4,NDDEMOS              MATCH GOAL DEM TO                        
         LA    R5,NDESTDEM+2           NDDEMO LIST                              
         CLI   DEMOPT,C'A'                                                      
         BNE   *+8                                                              
         LA    R5,NDACTDEM+2                                                    
         ZIC   R1,NDNDEMOS                                                      
*        LA    R1,20                                                            
*ADD3     CLI   1(R4),C'R'                                                      
**        BNE   *+14                                                            
ADD3     CLC   2(1,R4),BGOAL                                                    
         BE    ADD5                                                             
         LA    R4,3(R4)                                                         
         LA    R5,8(R5)                                                         
         BCT   R1,ADD3                                                          
         CLI   BGOAL,0             IS GOAL ZERO                                 
         BE    ADD3A                                                            
*                                  NO/ERROR                                     
         MVI   BEGRP,C'*'          SET IN * AND CATCH AT PRINT TIME             
         B     SKIPIT                                                           
         SPACE                                                                  
*                      NO MATCH/GET DEMO FROM SAME BINREC PRD GROUP             
ADD3A    L     R4,NUMPER           BCT LIMIT = BINRECS IN THAT PRD              
         LA    R5,BINTABLE                                                      
ADD3B    CLC   0(5,R3),0(R5)                                                    
         BE    ADD4                                                             
         LA    R5,BRECLENE(R5)     BUMP BINTBL                                  
         BCT   R4,ADD3B                                                         
         B     ADD4B               NO MATCH                                     
ADD4     MVC   BGOAL,16(R5)     SET DEMO FROM OTHER BINREC                      
         CLI   BGOAL,X'0'          CHK IF PERCHANCE SELFSAME REC                
         BNE   ADDIT               NOW TRY ADDIT AGAIN                          
         LA    R5,BRECLENE(R5)     ELSE/BUMP BINTBL AGAIN                       
         CLC   0(5,R3),0(R5)            TEST NEXT REC                           
         BE    ADD4                     IF SAME TRY AGAIN                       
ADD4B    MVC   WORK(3),BPRD     CANT FIND/SO READ BRAND EST HEADER              
         BAS   RE,GTTRGDEM         GET TARGET DEMO                              
         MVC   BGOAL,WORK          SET IN BGOAL                                 
         B     ADDIT               NOW DO IT AGAIN                              
*                                                                               
ADD5     SR    R1,R1                                                            
         ICM   R1,3,0(R5)                                                       
         A     R1,BEGRP                                                         
         ST    R1,BEGRP                                                         
         B     SKIPIT                                                           
         SPACE                                                                  
*                                                                               
SKIPIT   B     LR06                GET NXT UNT REC                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************                                 
* STEPS THROUGH BINTABLE PRINTING DATA        *                                 
* BREAKS ON PRODUCT CHANGE                    *                                 
*                                             *                                 
* LOCALS: R2- BINTABLE                        *                                 
*         R3- PRINT LINE                      *                                 
***********************************************                                 
PRINTIT  DS    0H                                                               
         LA    R2,BINTABLE                                                      
         USING BINRECD,R2                                                       
PT02     L     R3,AP1                                                           
         USING PLINED,R3                                                        
         MVC   PRDSV,BPRD          SAVE PROD CODE                               
         MVC   PRDNOSV,BPNO        SAVE PROD NUMBER                             
         MVC   SPTLNSV,BSLN        SAVE SPOT LENGTH                             
*                                                                               
         CLI   BPNO,0                         CHK IF ZERO PRD NO                
         BNE   PTUNA                                                            
PTUNB    MVC   PBRAND(11),=C'UNALLOCATED'     YES/SET TO UNALLOC                
         MVI   132(R3),0           TO PRINT MULT P LINES                        
         B     PT02A                                                            
PTUNA    CLI   BPRD,C'*'         CHK PRDCD SET BY GETPRD TO UNALLOC             
         BE    PTUNB                 IF YES/SET TO UNALLOC                      
         MVC   WORK(3),BPRD                                                     
         BAS   RE,GETPRDNM         RETURNS NAME IN WORK                         
         MVC   PBRAND(3),BPRD       SET PRD CODE                                
         MVC   PBRAND+4(15),WORK    SET PRDNAME  (P1 LINE)                      
PT02A    LA    R3,132(R3)                                                       
         CLI   BGOAL,0             IF FIRST REC HAS NO GOAL                     
         BNE   PT02A2                                                           
         LR    R4,R2               GET IT FROM NEXT REC                         
PT02A1   LA    R4,BRECLENE(R4)                                                  
         CLC   0(2,R4),=2X'00'                                                  
         BE    PT02B                                                            
         USING BINRECD,R4                                                       
         CLC   PRDSV,BPRD                                                       
         BNE   PT02B                                                            
         CLC   PRDNOSV,BPNO                                                     
         BNE   PT02B                                                            
         CLC   SPTLNSV,BSLN                                                     
         BNE   PT02B                                                            
         CLI   BGOAL,0             POSSIBLE NEXT REC ALSO HAS NO GOAL           
         BE    PT02A1              THEN LOOK AT ONE AFTER THAT                  
         MVC   WORK+2(1),BGOAL                                                  
         B     *+10                                                             
         DROP  R4                                                               
PT02A2   MVC   WORK+2(1),BGOAL                                                  
         BAS   RE,GETDEMNM         RETURNS DEMO IN WORK                         
         MVC   0(7,R3),WORK           (P2 LINE)                                 
         MVI   7(R3),C'-'                                                       
         EDIT  BSLN,(2,9(R3))      SET SPOT LEN                                 
PT02B    LA    R3,132(R3)                                                       
         MVC   PBRAND(4),=C'GOAL'     (P3 LINE)                                 
         MVC   132(6,R3),=C'EST   '                                             
         CLI   DEMOPT,C'A'                                                      
         BNE   *+10                                                             
         MVC   132(6,R3),=C'ACT   '                                             
         LA    R3,PWK1                    SET P3 LINE TO WEEK START             
         B     PT03                                                             
         EJECT                                                                  
* MATCH START DATE OF BINREC WITH PERLIST                                       
* TO FIND P LINE PRINT DISP                                                     
*                                                                               
PT03     LA    R4,PERLIST                                                       
         L     R5,NUMPER           R5 - NUMBER OF WEEKS                         
PT05     CLC   0(2,R4),BSTRT                                                    
         BE    GOTIT                                                            
         LA    R4,4(R4)                    BUMP PERLIST                         
         LA    R3,PWK2-PWK1(R3)            BUMP PRINT LINE                      
         BCT   R5,PT05                                                          
         B     NXTBIN       *****  IF NO MATCH/SKIP IT FOR NOW                  
*                                                                               
GOTIT    DS    0H                                                               
         EDIT  BGGRP,(5,0(R3)),1                                                
         CLI   BEGRP,C'*'                                                       
         BNE   *+14                                                             
         MVC   132(5,R3),=C'NODEM'                                              
         B     GOTIT5                                                           
         EDIT  BEGRP,(5,132(R3)),1                                              
GOTIT5   L     R1,GTOTL                                                         
         A     R1,BGGRP                                                         
         ST    R1,GTOTL                                                         
         CLI   BEGRP,C'*'          IS IT NODEM ERROR                            
         BE    GOTIT7              YES/SKIP TOTAL ADDD                          
         L     R1,ETOTL                                                         
         A     R1,BEGRP                                                         
         ST    R1,ETOTL                                                         
GOTIT7   BAS   RE,WKTOT          ADD TO APPROPRIATE WEEKLY TOTAL                
         CLI   PRDTFLG,C'Y'                                                     
         BNE   NXTBIN                                                           
         BAS   RE,ROLLPRD          ADD TO PROD SUBTOTALS                        
*                                                                               
NXTBIN   DS    0H                                                               
         LA    R2,BRECLENE(R2)     BUMP BINTABLE                                
         CLC   BSTRT,=2X'00'       CHK END OF BINTABLE                          
         BNE   NXT5                                                             
         OC    P(10),P             IF END OF TABLE/P LINE CLEAR                 
         BZ    PRINTX                                                           
         BAS   RE,PRTOTS           PRINT TOTALS                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         CLI   PRDTFLG,C'Y'                                                     
         BNE   *+8                                                              
         BAS   RE,PRODTOT                                                       
         B     PRINTX                                                           
         SPACE                                                                  
NXT5     CLC   PRDNOSV,BPNO        CHK SAME PRD                                 
         BNE   NXT7                                                             
         CLC   SPTLNSV,BSLN        CHK SAME SPOT LENGTH                         
         BNE   NXT7                                                             
         L     R3,AP1              YES/RESET P3 LINE DISP                       
         LA    R3,264(R3)                                                       
         LA    R3,PWK1                                                          
         B     PT03                                                             
NXT7     BAS   RE,PRTOTS           PRINT TOTALS                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         CLI   PRDTFLG,C'Y'                                                     
         BNE   NXT10                                                            
         CLC   PRDNOSV,BPNO         IF SAME PRODUCT/NO PRD-SUBTOTS              
         BE    NXT10                                                            
         BAS   RE,PRODTOT                                                       
NXT10    GOTO1 SPOOL,DMCB,(R8)        SKIP LINE                                 
         B     PT02                AND DO NEXT PROD AND P LINE                  
*                                                                               
PRINTX   DS    0H                                                               
         BAS   RE,PRTWKTOT                                                      
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
****************************                                                    
* ROLL PRODTOTS / USE GRANDTOT AREA                                             
*                                                                               
*  INPUT R5=WEEK NUMBER                                                         
*        R2=USING FOR BINREC                                                    
*                                                                               
ROLLPRD  NTR1                                                                   
         LA    R3,GRANDTOT                                                      
         L     R4,NUMPER                                                        
ROL2     CR    R4,R5                                                            
         BE    ROL5                                                             
         BCTR  R4,0                                                             
         LA    R3,8(R3)                                                         
         B     ROL2                                                             
ROL5     L     R1,0(R3)                                                         
         A     R1,BGGRP                                                         
         ST    R1,0(R3)                                                         
         L     R1,4(R3)                                                         
         A     R1,BEGRP                                                         
         ST    R1,4(R3)                                                         
         B     EXIT                                                             
         SPACE 2                                                                
***************************                                                     
* PROD SUB-TOTALS                                                               
PRODTOT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         L     R2,AP1                                                           
         USING PLINED,R2                                                        
         MVC   1(9,R2),=C'SUB-TOTAL'                                            
         LA    R2,PWK1-2                                                        
         LA    R3,GRANDTOT                                                      
         L     R6,NUMPER                                                        
PDT5     EDIT  (B4,0(R3)),(6,1(R2)),1                                           
         L     R1,0(R3)                                                         
         A     R1,GDGOAL                                                        
         ST    R1,GDGOAL                                                        
         EDIT  (B4,4(R3)),(6,133(R2)),1                                         
         L     R1,4(R3)                                                         
         A     R1,GDACT                                                         
         ST    R1,GDACT                                                         
         LA    R3,8(R3)                                                         
         LA    R2,6(R2)                                                         
         BCTR  R6,0                                                             
         LTR   R6,R6                                                            
         BNZ   PDT5                                                             
         LA    R2,2(R2)                                                         
         EDIT  GDGOAL,(7,0(R2)),1                                               
         EDIT  GDACT,(7,132(R2)),1                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    GDGOAL,GDGOAL                                                    
         XC    GDACT,GDACT                                                      
         XC    GRANDTOT(120),GRANDTOT                                           
         B     EXIT                                                             
         SPACE 2                                                                
***************************                                                     
* PRINT TOTALS TO P LINE  *                                                     
* AT END OF ROWS          *                                                     
***************************                                                     
PRTOTS   DS    0H                                                               
         MH    R5,=H'6'   R5 = NUMBER OF FIELDS LEFT TO TOTAL'S FIELD           
         AR    R3,R5      R3 = CURRENT DEMO FIELD ON P LINE                     
         EDIT  GTOTL,(7,0(R3)),1                                                
         EDIT  ETOTL,(7,132(R3)),1                                              
         XC    GTOTL,GTOTL                                                      
         XC    ETOTL,ETOTL                                                      
         BR    RE                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
*******************************************                                     
* THIS ROUTINE ADDS TO WEEKLY TOTAL COLUMN                                      
* TOTS ARE BY SPOTLEN AND WEEK NUMBER                                           
*      TABLE CONSISTS OF 20 SIXTEEN FULL WORD ENTRIES                           
*      DIVIDED INTO GROUPS OF 2                                                 
*      1ST 16 FULL WORDS ARE FOR GOAL TOTS                                      
*      2ND 16 FULL WORDS ARE FOR ACTUAL TOTS                                    
*      AND SO ON TO A TOTAL OF 10 SETS (OR 20 TOTS IN ALL)                      
*                                                                               
* INPUT R2-BINREC                                                               
*       R5-WEEK NUMBER(PRINT FIELD)                                             
*******************************************                                     
         SPACE                                                                  
WKTOT    NTR1                                                                   
         USING BINRECD,R2                                                       
         L     R3,AWKTOTBL                                                      
         LA    R3,64(R3)          BUMP TO START OF ACTUAL TOT                   
*                                  R3 POINTS TO ACTUAL TOT ENTRY                
WKT5     CLI   0(R3),X'FF'           CHK TBL FULL                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),0                                                          
         BNE   *+10                                                             
         MVC   0(1,R3),SPTLNSV     SET INTO ACT TOTS                            
         LR    R4,R3                                                            
         SH    R4,=H'64'                                                        
         CLI   0(R4),0      ******                                              
         BNE   *+10         ******                                              
         MVC   0(1,R4),SPTLNSV     SET INTO GOAL TOTS                           
         CLC   0(1,R3),SPTLNSV                                                  
         BE    WKT10                                                            
         LA    R3,128(R3)     BUMP TO NXT TABLE ENTRY                           
         B     WKT5                                                             
WKT10    LA    R3,4(R3)            BUMP TO TOT BUCKETS                          
         L     R4,NUMPER                                                        
         LTR   R4,R4                                                            
         BZ    WKTX                                                             
WKT12    CR    R4,R5                                                            
         BE    WKT15                                                            
         LA    R3,4(R3)            BUMP TO NXT TOT BUCKET                       
         BCT   R4,WKT12                                                         
         DC    H'0'                                                             
WKT15    CLI   BEGRP,C'*'                                                       
         BE    WKT15A                                                           
         L     R1,BEGRP            ADD TO APPROPRIATE TOT BUCKET                
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
WKT15A   L     R1,BGGRP                                                         
         LR    R4,R3                                                            
         SH    R4,=H'64'                                                        
         A     R1,0(R4)             REMEMBER GOAL TOT BEFORE ACT TOT            
         ST    R1,0(R4)                                                         
WKTX     B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*******************************************                                     
* PRINT WEEKLY TOT BUCKETS AT BOTTOM OF COLUMNS                                 
*  NOTE ALL 4 P LINES ARE USED                                                  
* WEEK1 P,P2 WEEK2 P3,P4 WEEK3 P,P2 WEEK4 P3,P4 ETC                             
* R2-PRINT LINE                                                                 
* R3-WKTOTBL                                                                    
******************************************                                      
         SPACE                                                                  
PRTWKTOT NTR1                                                                   
         ZIC   R1,LINE                                                          
         AH    R1,=H'9'                                                         
         ZIC   R2,MAXLINES                                                      
         CR    R1,R2                                                            
         BH    *+8                                                              
         BAS   RE,PRINTML          PRINT A BOX LINE                             
*                                                                               
         L     R2,AP1                                                           
         MVC   0(14,R2),=C'*** TOTALS ***'                                      
         MVI   ALLOWLIN,9                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    GTOTL,GTOTL                                                      
         XC    ETOTL,ETOTL                                                      
*                                                                               
         L     R2,AP1                                                           
         USING PLINED,R2                                                        
         L     R6,NUMPER          R2-PRINT LINE                                 
         LTR   R6,R6              R6-NUMBER OF PRINT FIELDS(WEEKS)              
         BZ    PWKX                                                             
         L     R3,AWKTOTBL                                                      
         ST    R3,WORKAD                                                        
PWK10    ZIC   R4,0(R3)            GET SPTLN                                    
         LA    R5,PBRAND+10                                                     
         EDIT  (R4),(2,(R5))                                                    
         EDIT  (R4),(2,132(R5))                                                 
         MVC   PBRAND(9),=C'GOAL TOT-'                                          
         MVC   PBRAND+132(9),=C'EST TOT -'                                      
         CLI   DEMOPT,C'A'                                                      
         BNE   *+10                                                             
         MVC   PBRAND+132(9),=C'ACT TOT -'                                      
         LA    R2,PWK1-2                                                        
PWK12    LA    R3,4(R3)            POINT TO BUCKETS                             
         L     R4,0(R3)                                                         
         EDIT  (R4),(7,0(R2)),1    SET GOAL                                     
         A     R4,GTOTL            ADD TO GOAL TOTAL                            
         ST    R4,GTOTL                                                         
         L     R4,64(R3)                                                        
         EDIT  (R4),(7,132(R2)),1  SET ACTUAL                                   
         A     R4,ETOTL            ADD TO EST TOTAL                             
         ST    R4,ETOTL                                                         
         BCTR  R6,0                                                             
         LTR   R6,R6                                                            
         BZ    PWK14                                                            
         LA    R2,6(R2)            BUMP PRINT LINE                              
         LA    R3,4(R3)            BUMP TBL TO NEXT ENTRY                       
         L     R4,0(R3)                                                         
         EDIT  (R4),(7,264(R2)),1  SET GOAL                                     
         A     R4,GTOTL            ADD TO GOAL TOTAL                            
         ST    R4,GTOTL                                                         
         L     R4,64(R3)                                                        
         EDIT  (R4),(7,396(R2)),1  SET ACT                                      
         A     R4,ETOTL            ADD TO EST TOTAL                             
         ST    R4,ETOTL                                                         
         BCTR  R6,0                                                             
         LTR   R6,R6                                                            
         BZ    PWK14                                                            
         LA    R2,6(R2)            BUMP PRINT LINE                              
         B     PWK12                                                            
PWK14    LA    R2,7(R2)            BUMP PRINT LINE FOR TOTS                     
         EDIT  GTOTL,(7,0(R2)),1                                                
         EDIT  ETOTL,(7,132(R2)),1                                              
         XC    GTOTL,GTOTL                                                      
         XC    ETOTL,ETOTL                                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         L     R3,WORKAD                                                        
         LA    R3,128(R3)          GET NEXT ENTRY IN TBL                        
         CLI   0(R3),0             IS IT EMPTY OR EOF                           
         BE    PWKX                                                             
         CLI   0(R3),X'FF'                                                      
         BE    PWKX                                                             
         BAS   RE,PRINTML          PRINT BOX LINE                               
         ST    R3,WORKAD                                                        
         L     R6,NUMPER           RESET NUM OF PRINT FIELDS                    
         L     R2,AP1                                                           
         B     PWK10                                                            
*                                                                               
PWKX     L     R3,AWKTOTBL                                                      
         XC    GRANDTOT,GRANDTOT                                                
GD00     LA    R5,GRANDTOT                                                      
         CLI   0(R3),0                                                          
         BE    GD20                                                             
         CLI   0(R3),X'FF'                                                      
         BE    GD20                                                             
         LA    R3,4(R3)            BUMP PAST SPOT LEN                           
         LA    R2,15                                                            
GD10     L     R4,0(R3)                                                         
         A     R4,0(R5)                                                         
         ST    R4,0(R5)                                                         
         L     R4,64(R3)                                                        
         A     R4,60(R5)                                                        
         ST    R4,60(R5)                                                        
         LA    R5,4(R5)                                                         
         LA    R3,4(R3)                                                         
         BCT   R2,GD10                                                          
         LA    R3,64(R3)                                                        
         B     GD00                                                             
GD20     BAS   RE,PRINTML          PRINT BOX LINE                               
         L     R2,AP1                                                           
         MVC   PBRAND(11),=C'GRAND TOTAL'                                       
         LA    R2,PWK1-2                                                        
         L     R6,NUMPER                                                        
         LA    R3,GRANDTOT                                                      
GD25     EDIT  (B4,0(R3)),(7,0(R2)),1    SET GOAL                               
         L     R1,0(R3)                                                         
         A     R1,GDGOAL           ADD TO GOAL TOTAL                            
         ST    R1,GDGOAL                                                        
         L     R4,60(R3)                                                        
         EDIT  (R4),(7,132(R2)),1  SET ACTUAL                                   
         A     R4,GDACT               ADD TO EST TOTAL                          
         ST    R4,GDACT                                                         
         LA    R2,6(R2)            BUMP PRINT LINE                              
         LA    R3,4(R3)            BUMP TBL TO NEXT ENTRY                       
         BCTR  R6,0                                                             
         LTR   R6,R6                                                            
         BNZ   GD27                                                             
         LA    R2,264(R2)                                                       
         B     GD30                                                             
GD27     EDIT  (B4,0(R3)),(7,264(R2)),1    SET GOAL                             
         L     R1,0(R3)                                                         
         A     R1,GDGOAL           ADD TO GOAL TOTAL                            
         ST    R1,GDGOAL                                                        
         L     R4,60(R3)                                                        
         EDIT  (R4),(7,396(R2)),1  SET ACTUAL                                   
         A     R4,GDACT               ADD TO EST TOTAL                          
         ST    R4,GDACT                                                         
         LA    R2,6(R2)            BUMP PRINT LINE                              
         LA    R3,4(R3)            BUMP TBL TO NEXT ENTRY                       
         BCT   R6,GD25                                                          
GD30     LA    R2,1(R2)                                                         
         EDIT  GDGOAL,(7,0(R2)),1                                               
         EDIT  GDACT,(7,132(R2)),1                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
*****************************************                                       
* PRINTS A BOX LINE                                                             
*                                                                               
*****************************************                                       
         SPACE                                                                  
PRINTML  NTR1                                                                   
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         LA    R2,BOXROWS                                                       
         ZIC   R3,LINE                                                          
         AR    R2,R3                                                            
         MVI   0(R2),C'M'                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
*******************************************                                     
* READS THROUGH BINTABLE TO GET PRDHDRS TO FIND                                 
*                DUMMY PRDS                                                     
* DUMMYTBL=DUMMYPRD/PRDNO/LEN*AAPRD/PRDNO/LEN*BBPRD/PRDNO                       
*                                                                               
DUMMY    NTR1                                                                   
         LA    R2,BINTABLE                                                      
         USING BINRECD,R2                                                       
DUM1     LA    R3,KEY                                                           
         USING PKEY,R3                                                          
         XC    KEY,KEY                                                          
         MVC   PKEYAM,NBACTAM                                                   
         MVC   PKEYCLT,NBACTCLI                                                 
         MVC   PKEYPRD,BPRD                                                     
         NETGO NVSETSPT,DMCB                                                    
         MVC   KEYSAVE,KEY                                                      
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R3,NBAIO                                                         
         USING PRDHDR,R3                                                        
         CLC   =C'PB=',PNAME                                                    
         BNE   DUM10                                                            
*                                                                               
         L     R1,ADUMMY           ADDRESS OF DUMMYTBL                          
         USING DUMMYD,R1                                                        
DUM7     CLI   0(R1),0                                                          
         BE    DUM9                                                             
         CLI   0(R1),X'FF'         MAX TABLE REACHED                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R1,DUMLENE(R1)                                                   
         B     DUM7                                                             
DUM9     MVC   PRDXX,BPRD          SET DUMMY PRD                                
         MVC   PRDXNO,BPNO         SET DUMMY PRDNO                              
         MVC   PRDXL,BSLN          SET DUMMY SPOT LEN                           
         XC    PRODNAME,PRODNAME                                                
         MVC   PRODNAME(17),PNAME+3          DROP PB=                           
         BAS   RE,GETPIGS                    RETURNS PIGGYS IN DUB              
         MVC   PRDAA,DUB         SET AAPRD                                      
         MVC   PRDAL,DUB+3       SET AALEN                                      
         BAS   RE,GETPRDNO                                                      
         MVC   PRDANO,WORK       SET AAPRDNO                                    
*                                                                               
         MVC   DUB(4),DUB+4        MOVE UP BB PRD                               
         MVC   PRDBB,DUB         SET BBPRD                                      
         MVC   PRDBL,DUB+3       SET BBLEN                                      
         BAS   RE,GETPRDNO                                                      
         MVC   PRDBNO,WORK       SET BBPRDNO                                    
*                                                                               
DUM10    LA    R2,BRECLENE(R2)                                                  
         CLI   BPRD,0              END OF BINTABLE                              
         BNE   DUM1                NO/GET NEXT PRD IN BINTABLE                  
*                                                                               
DUMMYX   NETGO NVSETUNT,DMCB       RESET TO UNIT FILE                           
         XC    FILENAME,FILENAME   CLEAR FILENAME                               
         B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
******************************************                                      
*        CHECK FOR PIGGYBACK BRAND PAIRS                                        
*        EXPECTS PIGGYS IN PRODNAME                                             
*                                                                               
GETPIGS  NTR1                                                                   
         OC    PRODNAME,SPACES                                                  
         XC    SCANWRK,SCANWRK                                                  
         XC    DMCB,DMCB                                                        
         MVC   DMCB+8(4),=C',=,-'                                               
         GOTO1 SCANNER,DMCB,(C'C',PRODNAME),SCANWRK                             
*                                                                               
         LA    R4,SCANWRK                                                       
         CLI   0(R4),0             MUST HAVE PRD CODE                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   DUB(3),12(R4)       SET PROD                                     
         MVC   DUB+3(1),11(R4)     SET SPOT LEN                                 
         LA    R4,32(R4)           NEXT PRD/SHR                                 
         OC    8(4,R4),8(R4)       SHARE IN 2ND HALF                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   DUB+4(3),12(R4)     SET 2ND PROD                                 
         MVC   DUB+7(1),11(R4)     SET 2ND SPOT LEN                             
         XIT1                                                                   
         SPACE 2                                                                
****************************************************                            
* CHECK UNIT BELONGS TO DUMMY BRAND                                             
*                                                                               
CHKDUMMY NTR1                                                                   
         L     R5,ADUMMY                                                        
         USING DUMMYD,R5                                                        
CD1      CLI   0(R5),0             EOF                                          
         BE    CDX                                                              
         CLI   0(R5),X'FF'         EOF                                          
         BE    CDX                                                              
         CLC   PRDANO,NBPRD        1ST PIGGY                                    
         BNE   CD5                                                              
         CLC   PRDAL,NBLEN                                                      
         BNE   CD5                                                              
         CLC   PRDBNO,NBPRD2       2ND PIGGY                                    
         BNE   CD5                                                              
         CLC   PRDBL,NBLEN1                                                     
         BE    YESDUM                                                           
CD5      LA    R5,DUMLENE(R5)                                                   
         B     CD1                                                              
YESDUM   LTR   R1,R1               UNEQUAL = GOT DUMMY                          
*                                  EQUAL = NO DUMMY MATCH                       
CDX      XIT1  REGS=(R5)                                                        
         DROP  R5                                                               
         EJECT                                                                  
********************************************                                    
* READ PRODUCT HEADER                      *                                    
*      INPUT : WORK CONTAINS 3CL PRD CODE  *                                    
*      OUTPUT: PRDNAME IN WORK             *                                    
*                                          *                                    
********************************************                                    
         SPACE                                                                  
GETPRDNM NTR1                                                                   
         LA    R3,KEY                                                           
         USING PKEY,R3                                                          
         XC    KEY,KEY                                                          
         MVC   PKEYAM,NBACTAM                                                   
         MVC   PKEYCLT,NBACTCLI                                                 
         MVC   PKEYPRD,WORK                                                     
         NETGO NVSETSPT,DMCB                                                    
         MVC   KEYSAVE,KEY                                                      
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R3,NBAIO                                                         
         USING PRDHDR,R3                                                        
         MVC   WORK(20),PNAME                                                   
         SPACE                                                                  
GPNX     NETGO NVSETUNT,DMCB       RESET TO UNIT FILE                           
         XC    FILENAME,FILENAME   CLEAR FILENAME                               
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
**********************************                                              
*  TO GET PRD CODE FROM C LIST   *                                              
*  INPUT WORK HAS PRDNO          *                                              
*  OUTPUT PRDCODE IN PRDSV       *                                              
**********************************                                              
GETPRD   NTR1                                                                   
         L     R2,ACLISTSV                                                      
GP10     CLI   3(R2),0             IF E-O-F CLIST                               
         BNE   GP12                SET TO UNDEFINED                             
         XC    PRDSV,PRDSV                                                      
         MVC   PRDSV,=C'***'                                                    
         B     GPX                                                              
GP12     CLC   3(1,R2),WORK                                                     
         BE    GP14                                                             
         LA    R2,4(R2)            INCREMENT CLIST                              
         B     GP10                RETURN TO LOOP                               
GP14     MVC   PRDSV,0(R2)      SET 3 CHAR PRINTABLE PRD CODE                   
*                                                                               
GPX      B     EXIT                                                             
         SPACE 2                                                                
**********************************                                              
*  TO GET PRD NO   FROM C LIST   *                                              
*  INPUT:   DUB HAS PRDCODE      *                                              
*  OUTPUT:  PRDNO IN WORK        *                                              
**********************************                                              
GETPRDNO NTR1                                                                   
         L     R2,ACLISTSV                                                      
GPN10    CLI   3(R2),0             IF E-O-F CLIST                               
         BNE   GPN12               SET TO UNDEFINED                             
         MVI   WORK,X'FF'                                                       
         B     GPNOX                                                            
GPN12    CLC   0(3,R2),DUB                                                      
         BE    GPN14                                                            
         LA    R2,4(R2)            INCREMENT CLIST                              
         B     GPN10               RETURN TO LOOP                               
GPN14    MVC   WORK(1),3(R2)      SET 1 CHAR PRD NO                             
*                                                                               
GPNOX    B     EXIT                                                             
         EJECT                                                                  
***************************************                                         
* CHECK PERLIST FOR START/END DUP DATE                                          
* THIS CAN HAPPEN WHEN START/END INPUT                                          
* DATES DO NOT COINCIDE WITH START/                                             
* END OF WEEK                                                                   
*                                                                               
*************************************                                           
         SPACE                                                                  
CHKDUPS  NTR1                                                                   
         LA    R3,PERLIST       * DOES PERLIST BEGIN WITH WEEK DATES            
         CLC   0(2,R3),2(R3)       CHK START/END EQUAL                          
         BNE   DUP2                                                             
         MVC   0(L'PERLIST-4,R3),4(R3)    IF EQUAL/MEANS NOT WEEK               
         L     R3,NUMPER                  SO GET RID OF THIS DATE               
         BCTR  R3,0                       BEFORE START/END WEEK DATES           
         ST    R3,NUMPER                                                        
         LA    R3,PERLIST                                                       
         LA    R3,L'PERLIST-4(R3)         CLEAR LAST 4 BYTES                    
         XC    0(4,R3),0(R3)                                                    
         SPACE                                                                  
DUP2     L     R3,NUMPER                                                        
         BCTR  R3,0                                                             
         MH    R3,=H'4'                                                         
         LA    R2,PERLIST                                                       
         AR    R2,R3               R2 POINTS TO LAST WEEK                       
         CLC   0(2,R2),2(R2)       ARE LAST TWO DATES EQUAL                     
         BNE   DUPX                                                             
         XC    0(4,R2),0(R2)       YES ZERO OUT                                 
         L     R3,NUMPER                                                        
         BCTR  R3,0                                                             
         ST    R3,NUMPER           SET NUMPER TO ONE LESS                       
DUPX     B     EXIT                                                             
         EJECT                                                                  
****************************************                                        
* GET DEMO NAME/ RETURN IN WORK                                                 
*     INPUT: WORK+2 HAS 1 BYTE DEMCODE                                          
****************************************                                        
GETDEMNM NTR1                                                                   
         LA    R4,DBLOCK                                                        
         XC    0(256,R4),0(R4)                                                  
         MVC   DBCOMFCS,ACOMFACS   *SET FOR DEMOCON                             
         MVC   DBFILE,=C'NTI'      *                                            
         MVI   DBSELMED,C'N'       *                                            
         SPACE                                                                  
         MVI   WORK,0                                                           
         MVI   WORK+1,C'R'                                                      
         LA    R2,WORK                                                          
         LA    R3,WORK+3                                                        
         SPACE                                                                  
         GOTO1 ADEMOCON,DMCB,(1,(R2)),(2,(R3)),(0,(R4))                         
         SPACE                                                                  
         MVC   WORK(7),WORK+3                                                   
         B     EXIT                                                             
         EJECT                                                                  
*********************************************                                   
* ROUTINE READS BRAND EST HEADER OF UNIT REC                                    
* ONLY CALLED IF NO GOALS FOR A BRAND                                           
* IF NO BRAND EST HEADER/ DEFAULTS TO 1ST DEMO                                  
*    ON POL EST HEADER (IN NDDEMOS)                                             
* INPUT   WORK = 3CL PRD CODE                                                   
* OUTPUT WORK=1CL DEMO CODE                                                     
***********************************************                                 
GTTRGDEM NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),NBACTAM                                                 
         MVC   KEY+2(2),NBACTCLI                                                
         MVC   KEY+4(3),WORK                                                    
         MVC   KEY+7(1),NBACTEST                                                
         SPACE                                                                  
         MVC   COMMAND,=CL8'DMRDHI'          HANDLE COMMANDS                    
         MVC   SYSFIL,=C'SPTDIR  '         DIRECTORIES                          
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,COMMAND,SYSFIL,KEY,KEY,0                            
         CLC   KEYSAVE(13),KEY                                                  
         BNE   TRG10                                                            
         SPACE 1                                                                
         LA    R3,KEY+14                                                        
         MVC   SYSFIL,=C'SPTFILE '     FILE                                     
         MVC   DATADISP,=H'24'                                                  
         L     R2,ANETWS4                                                       
         A     R2,=F'700'              NEED MORE SPACE FOR NETDEMOT             
         GOTO1 DATAMGR,DMCB,(X'00',=C'GETREC'),SYSFIL,(R3),(R2),DMWORK          
         CLI   8(R1),0                                                          
         BNE   TRG10                                                            
         USING ESTHDR,R2                                                        
         MVC   WORK(1),EDEMLST+2   SET TARGET DEMO CODE                         
         DROP  R2                                                               
         B     TRG12                                                            
         SPACE                                                                  
TRG10    MVC   WORK(1),NDDEMOS+2   USE POL EST DEFAULT                          
         SPACE                                                                  
TRG12    DS    0H                    RESET FOR UNT READS                        
         NETGO NVSETUNT,DMCB                                                    
         MVI   NBFUNCT,NBFRDHI                                                  
         MVC   KEY,NBKEY                                                        
         XC    FILENAME,FILENAME                                                
         B     EXIT                                                             
         EJECT                                                                  
********************************************************************            
* SEARCH PERLIST TO FIND WEEK STRT/END PARAMS OF NBACTDAT                       
* OUTPUT: WEEK START IN WORK                                                    
*         WORK=FF IF NBACTDAT(AIR DATE) LESS THAN START OF PERLIST              
********************************************************************            
         SPACE                                                                  
GETDAT   NTR1                                                                   
         LA    R2,PERLIST                                                       
         CLC   NBACTDAT,0(R2)                                                   
         BE    GDIT             IF NBACTDAT LESS THAN START OF PERLIST          
         BNL   GD7                 SET WORK TO FF AND EXIT                      
OUTRANGE MVI   WORK,X'FF'                                                       
         B     GDX                                                              
         SPACE                                                                  
GD7      CLC   NBACTDAT,2(R2)                                                   
         BNH   GDIT                                                             
         LA    R2,4(R2)            BUMP PERLIST                                 
         CLI   0(R2),0                                                          
         BNE   *+8                 IF END OF PERLIST                            
         B     OUTRANGE               THEN OUT OF DATE RANGE                    
         CLC   NBACTDAT,0(R2)                                                   
         BNE   GD7                                                              
GDIT     MVC   WORK(2),0(R2)       SET STRT TO WORK                             
GDX      B     EXIT                                                             
         EJECT                                                                  
*****************************************                                       
* GETS DISPLACEMENT OF PRINT LINE                                               
* TO CENTER OUTPUT                                                              
*                                                                               
* OUTPUT AP1,AH10,ABOXCOLS                                                      
*****************************************                                       
         SPACE                                                                  
GETDISP  NTR1                                                                   
         L     R1,NUMPER           NUMPER CONTAINS NO OF WEEKS                  
         MH    R1,=H'6'            6=LENGTH OF GRP PRINT FIELDS                 
         AH    R1,=H'29'           29=PRD NAME + TOTAL FIELD                    
         LA    R2,132                                                           
         SR    R2,R1                                                            
         LTR   R2,R2                                                            
         BZ    GTD5                                                             
         BP    *+6                                                              
         DC    H'0'                MUST NOT  BE NEGATIVE                        
         SRA   R2,1                DIV BY 2(SRA IGNORES REMAINDER)              
         SPACE                                                                  
* R2 NOW CONTAINS NUMBER TO BE ADDED TO P LINES                                 
GTD5     LTR   R2,R2               IF R2=0,ADD 1 FOR BOXES                      
         BNZ   *+8                                                              
         LA    R2,1(R2)                                                         
         LA    R1,P1                                                            
         AR    R1,R2                                                            
         ST    R1,AP1              SET START OF P LINE                          
         LA    R1,H10                                                           
         AR    R1,R2                                                            
         ST    R1,AH10             SET START OF HEADLINE                        
         L     R3,ABOX                                                          
         USING BOXD,R3                                                          
         LA    R1,BOXCOLS                                                       
         BCTR  R2,0                SUBTRACT 1 FOR BOXES START                   
         AR    R1,R2                                                            
         ST    R1,ABOXCOLS         SET START OF BOXES                           
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         SPACE                                                                  
*                                                                               
HDRTN    NTR1                                                                   
         L     R5,ATWA                                                          
         USING T31EFFD,R5                                                       
         MVC   H1(12),=C'NETWORK T.V.'                                          
         MVC   H3(6),=C'CLIENT'                                                 
         MVC   H3+10(3),SPLCLI                                                  
         MVC   H3+17(20),SPLCLIN                                                
         MVC   H4(7),=C'PRODUCT'                                                
         MVC   H4+10(3),SPLPRO                                                  
         MVC   H4+17(20),SPLPRON                                                
         MVC   H5(8),=C'ESTIMATE'                                               
         MVC   H5+10(6),SPLEST                                                  
         MVC   H5+17(20),SPLESTN                                                
         DROP  R5                                                               
         SPACE                                                                  
         L     R2,AH10                                                          
         USING PLINED,R2                                                        
         MVC   PBRAND(7),=C'PRODUCT'                                            
         LA    R2,PWK1                                                          
         LA    R3,PERLIST          PRINT DATE HEADINGS                          
HDHK1    CLI   0(R3),0                                                          
         BE    HDHK1A                                                           
         GOTO1 DATCON,DMCB,(2,0(R3)),(4,0(R2))                                  
         LA    R2,PWK2-PWK1(R2)                                                 
         LA    R3,4(R3)                                                         
         B     HDHK1                                                            
HDHK1A   MVC   1(5,R2),=C'TOTAL'                                                
         SPACE                                                                  
         CLI   BOXSET,C'Y'          SET PARAMS FOR BOXES                        
         BE    HDX                                                              
         MVI   BOXSET,C'Y'                                                      
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         LTR   R1,R1               IS ABOX ZEROS                                
         BZ    HDX                 YES/ ON-LINE SKIP BOXES                      
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         SPACE                                                                  
         L     R5,ABOXCOLS                                                      
         MVI   0(R5),C'L'                                                       
         LA    R5,22(R5)                                                        
         MVI   0(R5),C'C'                                                       
         L     R4,NUMPER                                                        
BOX5     LA    R5,6(R5)                                                         
         MVI   0(R5),C'C'                                                       
         BCT   R4,BOX5                                                          
         LA    R5,8(R5)                                                         
         MVI   0(R5),C'R'                                                       
         SPACE                                                                  
         LA    R5,BOXROWS                                                       
         LA    R5,8(R5)                                                         
         MVI   0(R5),C'T'                                                       
         LA    R5,2(R5)                                                         
         MVI   0(R5),C'M'                                                       
         LA    R5,46(R5)                                                        
         MVI   0(R5),C'B'                                                       
         SPACE                                                                  
HDX      B     EXIT                 (XIT1)                                      
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,52,C'WEEKLY GRP FLOWCHART'                                    
         SSPEC H2,52,C'--------------------'                                    
         SSPEC H3,50,PERIOD                                                     
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
         SPACE 2                                                                
         LTORG                                                                  
*                                                                               
BINTABLE DS    CL20000        ***  GRAB 100,000 FOR BINTABLE  ***               
         DS    CL20000                                                          
         DS    CL20000                                                          
         DS    CL20000                                                          
         DS    CL20000                                                          
*                                                                               
         DS    0F                                                               
WKTOTBL  CSECT                     WEEKLY TOTALS TABLE                          
         DS    320F                1ST CHAR=SPT LEN/15 FULL WORD TOTS           
*                          GIVES A MAX OF 10 TOT TABLES FOR GOAL AND            
*                          ACTUALS 16X10X2=320 FULL WORDS                       
WKTOTEND DS    CL1                 WEEK TOTALS TABLE END/SET TO X'FF'           
*                                                                               
DUMMYTBL CSECT                                                                  
         DS    CL2000                                                           
DUMMYEND DS    CL1                                                              
       EJECT                                                                    
*                      **BE CAREFUL THIS SHOULD BE SAME AS                      
*                      **EDIT MODULE                                            
WORKD    DSECT                     MYWORK AREA  ANETWS3+500                     
BINDMCB  DS    6F                                                               
ANETGOAL DS    V                                                                
ADEMOCON DS    V                                                                
ACLISTSV DS    A                                                                
WORKAD   DS    A                   TEMP WORKING ADDRESS                         
AWKTOTBL DS    A                                                                
*                                                                               
RELO     DS    F                                                                
NUMPER   DS    F                                                                
AP1      DS    F                                                                
AH10     DS    F                                                                
ABOXCOLS DS    F                                                                
GTOTL    DS    F                                                                
ETOTL    DS    F                                                                
TBL      DS    F                                                                
TBL2     DS    F                                                                
PERTYPE  DS    CL3                                                              
DEMSV    DS    CL3                                                              
PRDSV    DS    CL3                                                              
PRDNOSV  DS    CL1                                                              
SPTLNSV  DS    XL1                                                              
PERLIST  DS    CL60                                                             
         DS    CL1                                                              
FRST     DS    CL1                                                              
BOXSET   DS    CL1                                                              
DEMOPT   DS    CL1                                                              
PRDTFLG  DS    CL1                                                              
*                        **** END OF SIMILAR WORK AREA                          
PRODNAME DS    CL80                                                             
SCANWRK  DS    CL100                                                            
ADUMMY   DS    F                                                                
GRANDTOT DS    30F                                                              
GDGOAL   DS    F                                                                
GDACT    DS    F                                                                
         SPACE 2                                                                
*                                                                               
BINRECD  DSECT                                                                  
BREC     DS    0CL1                                                             
BPRD     DS    CL3                                                              
BPNO     DS    CL1                                                              
BSLN     DS    CL1                                                              
BSTRT    DS    CL2                                                              
BKEYLENE EQU   *-BREC                                                           
BGGRP    DS    F                                                                
BEGRP    DS    F                                                                
BGOAL    DS    CL1                                                              
         DS    CL4                                                              
BRECLENE EQU   *-BREC                                                           
         SPACE 2                                                                
*                                                                               
DUMMYD   DSECT                   *** DSECT FOR DUMMY TABLE                      
PRDXX    DS    CL3                                                              
PRDXNO   DS    CL1                                                              
PRDXL    DS    CL1                                                              
PRDAA    DS    CL3                                                              
PRDANO   DS    CL1                                                              
PRDAL    DS    CL1                                                              
PRDBB    DS    CL3                                                              
PRDBNO   DS    CL1                                                              
PRDBL    DS    CL1                                                              
DUMLENE  EQU   *-PRDXX                                                          
         EJECT                                                                  
*                                                                               
PLINED   DSECT                                                                  
PBRAND   DS    CL20            **** SINCE VARIABLE NUMBER OF                    
         DS    CL2             **** PWK FIELDS DSECT IS ONLY                    
PWK1     DS    CL5             **** AN INDICATION                               
         DS    CL1                                                              
PWK2     DS    CL5                                                              
         DS    CL1                                                              
         DS    CL5                                                              
         DS    CL1                                                              
         DS    CL5                                                              
         DS    CL1                                                              
         DS    CL5                                                              
         DS    CL1                                                              
         DS    CL5                                                              
         DS    CL1                                                              
         DS    CL5                                                              
         DS    CL1                                                              
         DS    CL5                                                              
         DS    CL1                                                              
         DS    CL5                                                              
         DS    CL1                                                              
         DS    CL5                                                              
         DS    CL1                                                              
         DS    CL5                                                              
         DS    CL1                                                              
         DS    CL5                                                              
         DS    CL1                                                              
         DS    CL5                                                              
         DS    CL1                                                              
         DS    CL5                                                              
         DS    CL1                                                              
         DS    CL5                                                              
         DS    CL1                                                              
PTOTAL   DS    CL7                                                              
         EJECT                                                                  
         SPACE                                                                  
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NENETGOALD                                                     
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDF9D                                                       
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*                                                                               
NDBLK    DSECT                                                                  
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
PRDHD    DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057NEMED29   01/31/05'                                      
         END                                                                    
