*          DATA SET SPRES03    AT LEVEL 120 AS OF 12/09/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE T20F03A                                                                  
         TITLE 'T20F03 - RADIO RANKER'                                          
*---------------------------------------------------------------------          
* 08/8/00 - SUPPORT RADAR FILE.  USA GEOGGRAPHICS ONLY.                         
*                                                                               
*---------------------------------------------------------------------          
* 12/4/92 - FIX PROBLEM WITH MARKET SCREEN, IF MARKETS IN USER                  
*           GO PAST END OF FIELD BUMP TO NEXT USER LIST.         (GH)           
*---------------------------------------------------------------------          
* 7/6/92  - FIX PROBLEM WITH MARKET SCREEN, MARKET SHOWING UP IN                
*           THE ACTION FIELD.  MOVED 'MVC  RNKSTATH+5(1),STATLEN'               
*           AFTER SEEING IF WE USED MARKET SCREEN IN MYEND       (GH)           
*---------------------------------------------------------------------          
* 6/2/92  - FIX PROBLEM WITH CUSTOM STATIONS, 11 STATIONS AAAA-F                
*           EXCEEDED MINIPAR'S LENGTH OF X'50' CHANGED TO X'70'                 
*           IN AFTPARSE'S 2ND VMINIPAR CALL                      (GH)           
*---------------------------------------------------------------------          
* 5/26/92 - FIX THE FORMLINE STATION ERROR                                      
*           STATIONS COMING OUT WITH # ERRORS WHEN THEY                         
*           SHOULDN'T.  HAD TO CLEAR REST OF BUFFER AFTER                       
*           AVECOMP FINISHED THE # OF DEMOS. (ESP. BUFFSAN)      (GH)           
*---------------------------------------------------------------------          
* 5/5/92  - FIX THE SORTING ERROR                                               
*           CAN'T LET MAXRANK EXCEED 40 WHEN YOU HAVE COMBOS.    (GH)           
*---------------------------------------------------------------------          
* 4/22/92 - TSL CALCULATION FOR COMBOS:                                         
*               (IMP * NO OF DAYPTS)/ CUME                                      
*           CHANGED COMBO TSL DEMO TO CUME ('X' -> 'C')   /FILLCMB4/            
*           USED A TEMPORARY BUFFER TO GET IMPRESSIONS                          
*           FOR TSL CALCULATION, THIS WAY THE USER CAN                          
*           REQUEST 4 TSL'S.                                     (GH)           
*---------------------------------------------------------------------          
* 4/10/92 - 3 OR MORE STATIONS, APPLY THE FORMULA ON THE FIRST 2                
*           THEN USE FORMULA WITH THAT RESULT AND THE 3RD STATION               
*           AND THE SAME FOR THE OTHER STATIONS IF ANY.                         
*                                                                               
*             RESULT=FORM(FORM(FORM(A,B),C),...Z)                (GH)           
*---------------------------------------------------------------------          
* 4/2/92  - PROGRAM FORMULA FOR RANDOM DUPLICATION (2 STATIONS)                 
*                                                                               
*               (STA1 CUME + STA2 CUME)                                         
*             - (STA1 CUME RTG * STA2 CUME RTG * UNIV)                          
*             ========================================                          
*               COMBO (STA1 AND STA2) CUME                                      
*                                                                               
*         - IF COUNTRY IS CANADA, DEFAULT CALCOPT=N              (GH)           
*---------------------------------------------------------------------          
T20F03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 1000,T20F03**,RA,R6,RR=R2,CLEAR=YES                              
         LR    R3,RC               SAVE A(MY STORAGE FOR THE MOMENT)            
         USING MYD,R3                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE                                                        
         ST    R3,AMYD                                                          
         LA    RE,THISBUFF                                                      
         ST    RE,ATHISBUF                                                      
*                                                                               
         CLI   =X'2',LENDEM        SET UP MASK FOR WORST OPTION                 
         BNE   *+10                                                             
         MVC   ANDMASK,=F'65535'                                                
         CLI   =X'3',LENDEM                                                     
         BNE   *+10                                                             
         MVC   ANDMASK,=F'16777215'                                             
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD KEY                          
         BE    VREC                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*              VALIDATE REQUEST AT MODE=VALREC                                  
*                                                                               
VREC     DS    0H                                                               
         OI    RNKMKTNH+6,X'80'    CLEAR OUT HIGHLIGHTS                         
         MVI   STALSTD,C'N'                                                     
         OI    RNKSTATH+6,X'80'    IN CASE OF ERROR, KEEP PROPERTIES            
*                                                                               
         L     R2,ATIOB            CHECK FOR PF KEYS                            
         LTR   R2,R2                                                            
         BZ    VREC1                                                            
         USING TIOBD,R2                                                         
         CLI   TIOBAID,0                                                        
         BE    VREC1                                                            
*                                                                               
         CLI   ACTNUM,ACTREP       ONLY FOR ACTION REPORT                       
         BNE   PFKERR                                                           
*                                                                               
         MVC   PFKEY,TIOBAID                                                    
         CLI   TIOBAID,12          EQUATE 13-24 TO 1-12                         
         BNH   VREC0001                                                         
         ZIC   R1,TIOBAID                                                       
         SH    R1,=H'12'                                                        
         STC   R1,TIOBAID                                                       
         STC   R1,PFKEY                                                         
*                                                                               
VREC0001 OI    CONHEADH+6,X'80'                                                 
         OI    CONSERVH+6,X'81'                                                 
*                                                                               
         CLI   TIOBAID,2                                                        
         BNE   VRECT1                                                           
         CLC   RMKMKID,=C'MID1'    ALREADY HAVE TREAT AS ENTER                  
         BE    VRECT0A                                                          
         LA    R2,RNKSRCEH         VALIDATE SOURCE                              
         GOTO1 VVALSRC                                                          
         MVI   DBSELMED,C'R'       (ADJUST FOR RADIO)                           
         NI    BAGYMD,X'F0'                                                     
         OI    BAGYMD,X'02'                                                     
         SPACE 1                                                                
         LA    R2,RNKBOOKH         VALIDATE BOOK                                
         MVI   MAX,1                                                            
         GOTO1 VRADBOOK                                                         
         GOTOR SUBR07,DMCB,('M1INITE',(RC))                                     
         USING SVTD,RE                                                          
         BE    VRECT0B                                                          
         DC    H'0'                                                             
*        B     *+8                                                              
VRECT0A  BAS   RE,SELMKT                                                        
VRECT0B  LA    RE,RMKWORK                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELAGY,AGENCY                                                  
         MVC   DBAREC,AIO1                                                      
         MVC   DBSELSRC,SVTSRC                                                  
         MVC   DBSELBK,SVTBOOK                                                  
         MVC   DBBTYPE,SVTBTYP                                                  
         MVC   DBFILE,SVTFILE                                                   
         MVC   DBSELMED,SVTMED                                                  
         BAS   RE,BLDMKT                                                        
         B     MYEND                                                            
*                                                                               
VRECT1   CLI   TIOBAID,12          PF12 - RETURN TO RANKER SCREEN               
         BNE   VRECT2                                                           
         CLC   RMKMKID,=C'MID1'    PROPER SCREEN                                
         BNE   MYEND                                                            
         BAS   RE,SELMKT                                                        
         MVC   BUFF(200),RMKWORK                                                
*                                                                               
         LA    RE,RMKWORK                                                       
         LA    RE,SVTMKTS          CHECK FOR ANY COMBO MARKETS                  
VRECT1A  CLI   0(RE),0             END OF MARKET LIST                           
         BE    VRECT1B                                                          
*        CLI   0(RE),C'+'                                                       
*        BC    0,BLDCUS                                                         
         LA    RE,3(RE)                                                         
         B     VRECT1A                                                          
*                                                                               
*RECT1B  LA    R2,2                RESTORE RANKER SCREEN                        
*        SLL   R2,32-8                                                          
*        ICM   R2,3,TERM                                                        
*        GOTO1 DATAMGR,DMCB,(0,=C'DMREAD '),=C'TEMPSTR',(R2),ATWA,0             
*        CLI   8(R1),0                                                          
VRECT1B  MVI   TWANUM,2            SET THE TWA NUMBER                           
         GOTOR SUBR07,DMCB,('RESTWAE',(RC))                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,CONRECH                                                       
         BAS   RE,BUMP                                                          
         CLI   0(R2),0                                                          
         BNE   *-8                                                              
         MVC   1(2,R2),=X'0101'                                                 
         LA    R2,RNKSRCEH                                                      
         OI    6(R2),X'01'                                                      
         MVC   CONHEAD(32),=C'MAKE CHANGES OR ENTER TO PROCESS'                 
         BAS   RE,BLDUL            BUILD THE USER LISTS                         
         B     SPERR                                                            
*                                                                               
VRECT2   MVC   CONHEAD(16),=C'PLEASE HIT ENTER'                                 
         CLI   TIOBAID,3                                                        
         BNE   VREC1                                                            
*                                                                               
VREC1    LA    R2,RMKMK1H          SET TO FIRST MARKET LIST FLD                 
         MVC   SVMKTH5,RNKMKTH+5                                                
         CLC   RMKMKID,=C'MID1'    ENSURE THE PROPER SCREEN                     
         BE    VRECT0A                                                          
         LA    R2,RNKSRCEH         VALIDATE SOURCE                              
         GOTO1 VVALSRC                                                          
         MVI   DBSELMED,C'R'       (ADJUST FOR RADIO)                           
         MVC   MEDTYPE,DBSELMED                                                 
         MVC   SRCTYPE,DBSELSRC                                                 
         XC    BOOKTYPE,BOOKTYPE                                                
         CLC   RNKBOOK(4),=C'LATEST'                                            
         BNE   NLATEST                                                          
         MVC   RNKBOOK(5),=C'1BOOK'                                             
         MVC   RNKBOOK+5(3),RNKBOOK+6                                           
         MVI   RNKBOOK+8,C' '                                                   
NLATEST  LA    R2,RNKBOOK                                                       
         ZIC   R1,RNKBOOKH+5                                                    
         LTR   R1,R1                                                            
         BZ    VREC01A                                                          
         BCTR  R1,0                                                             
         AR    R1,R2               ADDRESS OF LAST CHAR IN BOOK                 
SRCHLP   CR    R2,R1               CHECKED ALL CHARS?                           
         BH    VREC01A                                                          
         CLI   0(R2),C'('                                                       
         BE    FOUNDCH                                                          
         LA    R2,1(R2)                                                         
         B     SRCHLP                                                           
*                                                                               
FOUNDCH  CLI   2(R2),C')'          MAKE SURE ENCLOSED BY ()                     
         BNE   VREC01A                                                          
         MVC   BOOKTYPE,1(R2)                                                   
VREC01A  NI    BAGYMD,X'F0'                                                     
         OI    BAGYMD,X'02'                                                     
         MVC   WSTATS,=F'5'                                                     
         SPACE 1                                                                
         XC    MKTNUM,MKTNUM                                                    
* AFTPARSE GOES HERE                                                            
         MVI   STASTYLE,C'M'                                                    
         USING ELEMDS,R2                                                        
         GOTO1 =A(AFTPARSE),DMCB,(R9),(RC),RR=RELO                              
         BZ    MKTXST                                                           
*                                                                               
         MVI   RNKSTATH+5,0                                                     
OUTMRKT  L     R2,OUTAREA                                                       
         CLI   0(R2),C'='                                                       
         BE    OUTMRKT1                                                         
         CLI   0(R2),C','                                                       
         BE    OUTMRKT1                                                         
         MVC   2(3,R2),=X'02E400'                                               
OUTMRKT1 CLC   2(2,R2),=X'02E4'                                                 
         BNH   OUTMRKT2                                                         
         MVC   CONHEAD(L'MKTERR2),MKTERR2                                       
         B     CPERR                                                            
OUTMRKT2 MVC   CONHEAD(L'MKTERR),MKTERR                                         
         B     CPERR                                                            
MKTXST   L     R2,OUTAREA                                                       
         GOTOR SUBR07,DMCB,('TESTMKE',(RC))                                     
         BNZ   OUTMRKT1                                                         
         STH   R1,MKTNUM                                                        
         GOTO1 =A(ALPHMRKT),DMCB,(R9),(RC),RR=RELO                              
         MVC   DBAREC,AIO1                                                      
         MVI   DBFUNCT,DBGETMK                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'R'                                                    
         MVC   DBSELRMK,MKTNUM                                                  
         SPACE 1                                                                
         LA    R2,RNKBOOKH         VALIDATE BOOK                                
         MVI   MAX,4                                                            
         SPACE 1                                                                
         TM    RNKBOOK,X'F0'                                                    
         BNO   *+12                                                             
         BAS   RE,VMULTBK                                                       
         BE    VREC02                                                           
         CLI   BYTE,2              NOT ENOUGH BOOKS FOR MULTI                   
         BNE   *+14                                                             
         MVC   CONHEAD(L'BADMUL),BADMUL                                         
         B     MYEND                                                            
         SPACE 1                                                                
         GOTO1 VRADBOOK                                                         
         SPACE 1                                                                
VREC02   LA    R2,RNKMAXH          MAXIMUM STATIONS TO RANK                     
         MVI   MAXRANK,10          DEFAULT IS 10                                
         CLI   5(R2),0                                                          
         BE    VREC10                                                           
         GOTO1 VALINUM                                                          
         MVC   MAXRANK,ACTUAL                                                   
         CLI   MAXRANK,40                                                       
         BNH   VREC10                                                           
         L     RE,=A(TOOMNY)                                                    
         A     RE,RELO                                                          
         MVC   CONHEAD(L'TOOMNY),0(RE)                                          
         B     MYEND                                                            
         SPACE 1                                                                
VREC10   MVC   SVMAXRNK,MAXRANK    SAVE IN CASE OF LIST                         
         LA    RE,COMBLIST                                                      
         LA    RF,CUMESW-COMBLIST                                               
         XCEF                                                                   
         BAS   RE,EDITMKT          EDIT MARKET OR STATIONS                      
*                                                                               
         LA    R2,RNKOPTH          OPTIONS                                      
         GOTOR SUBR08,DMCB,('EDITOPTE',(RC))                                    
         BNE   SPERR                                                            
*                                                                               
         MVI   NMARKET,0                                                        
         MVC   TMPBOOKL,NBOOKS     COPY BOOKS AND NBOOKS                        
         L     R2,OUTAREA                                                       
         GOTOR SUBR07,DMCB,('EDITMLE',(RC))                                     
         BNE   CPERR                                                            
         MVC   NBOOKS(16),TMPBOOKL                 RESTORE BOOKS                
*                                                                               
         OC    MKTNUM,MKTNUM                                                    
         BZ    VREC100                                                          
*                                                                               
         CLC   =C'RADA',RNKSRCE            RADA DONT HAVE THESE RECS            
         BE    VREC100                                                          
*                                                                               
         GOTOR SUBR07,DMCB,('EDITMBE',(RC))                                     
         BNE   BADBOOKM                                                         
VREC100  LA    R2,RNKDEMOH         VALIDATE DEMOGRAPHICS                        
         CLI   5(R2),0                                                          
         BZ    VREC100A                                                         
         XC    BLOCK(256),BLOCK                                                 
         MVC   PARAS+8(3),=C',=,'                                               
         MVI   PARAS+11,X'FF'                                                   
         GOTO1 SCANNER,PARAS,(R2),(1,BLOCK)                                     
         CLI   BLOCK+1,0           NEED COMPLETE EXPRESSION                     
         BE    VREC10A                                                          
VREC100A MVC   CONHEAD(L'BADDEM),BADDEM                                         
         B     MYEND                                                            
*                                                                               
VREC10A  GOTO1 VVALDEM                                                          
         LA    R2,RNKDPTH          DAYPART MENU                                 
*---->   BAS   RE,EDITDPT                                                       
         GOTOR SUBR08,DMCB,('SETPHASE',(RC))                                    
         GOTOR SUBR08,DMCB,('EDITDPTE',(RC))                                    
         MVI   NFLDS,1                                                          
         MVC   PRPNDEMS,NDEMOS                                                  
         MVC   TMPSEPBF,NDEMOS                                                  
         MVI   DEMSTYLE,C'D'                                                    
         CLI   NDEMOS,1                                                         
         BH    VRC10_1                                                          
         MVI   DEMSTYLE,C'C'                                                    
VRC10_1  MVI   MAX,10                                                           
         LA    R2,RNKCATSH         DEMO CATEGORIES                              
         GOTO1 VVALCATS                                                         
*------------------------------------------------------                         
         ZIC   RE,NBOOKS                                                        
         ZIC   RF,NMARKET                                                       
         MR    RE,RE                                                            
         CLI   SEPAOPT,C'N'                                                     
         BE    VREC10A2                                                         
         ZIC   RF,NMARKET                                                       
         CLI   SEPAOPT,C'D'                                                     
         BNE   VREC10AC                                                         
         ZIC   RE,PRPNDEMS         IF BY DEMOS                                  
         B     VREC10AD                                                         
VREC10AC ZIC   RE,NCATS            IF BY CATEGORIES                             
VREC10AD MR    RE,RE                                                            
         STC   RF,PRDRSLT                                                       
         ZIC   RE,NBOOKS                                                        
         MR    RE,RE                                                            
*                                                                               
VREC10A2 CLI   NBOOKS,4                                                         
         BL    VREC10AA                                                         
         CLC   MKTNUM,=H'1'                                                     
         BE    VREC10AX                                                         
         CLC   MKTNUM,=H'3'                                                     
         BNE   VREC10AA                                                         
VREC10AX TM    WHEN,X'40'                                                       
         BNO   VREC10AA                                                         
         LA    R2,RNKMKTH                                                       
         MVC   CONHEAD(L'SOONITN-2),SOONITN+2                                   
         B     MYEND                                                            
VREC10AA C     RF,=F'6'                                                         
         BNL   VRC10A2A                                                         
         CLI   SEPAOPT,C'N'                                                     
         BE    VREC10X                                                          
         TM    WHEN,X'40'                                                       
         BZ    VREC10B                                                          
         CLI   PRDRSLT,4                                                        
         BL    VREC10X                                                          
         MVC   CONHEAD(L'SOONITSP-6),SOONITSP+6                                 
         MVC   CONHEAD+L'SOONITSP-6(L'SOONITN),SOONITN                          
         MVI   CONHEAD+L'SOONITSP-6,C'3'                                        
         CLI   SEPAOPT,C'C'                                                     
         BNE   MYEND                                                            
         MVC   CONHEAD+7(4),=C'CATS'                                            
         B     MYEND                                                            
VRC10A2A TM    WHEN,X'40'                                                       
         BZ    VREC10B                                                          
         CLI   SEPAOPT,C'N'                                                     
         BE    VREC10A3                                                         
         MVC   CONHEAD(L'SOONITSP),SOONITSP                                     
         MVC   CONHEAD+L'SOONITSP(L'SOONITN),SOONITN                            
         B     MYEND                                                            
VREC10A3 MVC   CONHEAD(L'SOONIT),SOONIT                                         
         MVC   CONHEAD+L'SOONIT(L'SOONITN),SOONITN                              
         B     MYEND                                                            
         SPACE 1                                                                
VREC10B  C     RF,=F'41'                                                        
         BL    VREC10X                                                          
         TM    WHEN,X'20'                                                       
         BZ    VREC10C                                                          
         CLI   SEPAOPT,C'N'                                                     
         BE    VREC10B1                                                         
         MVC   CONHEAD(L'SOONITSP),SOONITSP                                     
         MVC   CONHEAD+L'SOONITSP(L'SOONITS),SOONITS                            
         B     MYEND                                                            
VREC10B1 MVC   CONHEAD(L'SOONIT),SOONIT                                         
         MVC   CONHEAD+L'SOONIT(L'SOONITS),SOONITS                              
         B     MYEND                                                            
         SPACE 1                                                                
*REC10C  C     RF,=F'33'                                                        
*        BL    VREC10X                                                          
*        CLI   SEPAOPT,C'N'                                                     
*        BE    VREC10C1                                                         
*        MVC   CONHEAD(L'SOONITSP),SOONITSP                                     
*        MVC   CONHEAD+L'SOONITSP(L'SOONITO),SOONITO                            
*        B     MYEND                                                            
*REC10C1 MVC   CONHEAD(L'SOONIT),SOONIT                                         
*        MVC   CONHEAD+L'SOONIT(L'SOONITO),SOONITO                              
*        B     MYEND                                                            
         SPACE 1                                                                
VREC10C  DS    0H                                                               
VREC10X  CLI   PRPNDEMS,4          NO MORE THAN 4                               
         BNH   VREC11                                                           
         CLI   SEPAOPT,C'D'        IF SEP=D THAN UP TO 6 ALLOWED                
         BNE   VREC10X1                                                         
         CLI   PRPNDEMS,6                                                       
         BNH   VREC11                                                           
VREC10X1 L     RE,=A(TOOMNYDM)                                                  
         A     RE,RELO                                                          
         MVC   CONHEAD(L'TOOMNYDM),0(RE)                                        
         LA    R2,RNKDEMOH                                                      
         B     MYEND                                                            
         SPACE 1                                                                
VREC11   MVI   DEMSTYLE,C'D'       DEMO STYLE REPORT IF MORE THAN 1             
         CLI   PRPNDEMS,1                                                       
         BH    VREC12                                                           
         MVI   DEMSTYLE,C'C'       ELSE ITS A CATEGORY STYLE                    
         SPACE 1                                                                
VREC12   CLI   NCATS,4             LIMIT OF 4 CATEGORIES                        
         BNH   VREC12A                                                          
         CLI   SEPAOPT,C'C'                                                     
         BNE   VREC14                                                           
         CLI   NCATS,6                                                          
         BH    VREC14                                                           
VREC12A  CLI   DEMSTYLE,C'D'       IF ITS A MULTI-DEMO REPORT                   
         BNE   VREC16                                                           
         CLI   NCATS,1             LIMIT IS 1 CATEGORY                          
         BE    VREC16                                                           
         SPACE 1                                                                
         CLI   SEPAOPT,C'N'                                                     
         BE    VREC14                                                           
         B     VREC16                                                           
         SPACE 1                                                                
VREC14   L     RE,=A(TOOMNYCT)                                                  
         A     RE,RELO                                                          
         MVC   CONHEAD(L'TOOMNYCT),0(RE)                                        
         B     MYEND                                                            
         SPACE 1                                                                
VREC16   CLI   SEPAOPT,C'C'        SEPARATE BY CATEGORIES                       
         BNE   VREC16A                                                          
         MVC   TMPDEMOS(20),TMPSEPBF                                            
         MVC   TMPSEPBF,NDEMOS     TEMP BUFF FOR SEPARATING                     
         MVC   TMPSEPBF(1),NCATS   MOVE IN # OF CATEGORIES                      
         ZIC   RE,TMPDEMOS                                                      
         STC   RE,NDEMOS                                                        
         LA    R2,TMPDEMOS+1       WANT TO MOVE ONLY THE DEMO PART              
         LA    RF,DEMOS                                                         
VREC16_1 MVC   2(1,RF),2(R2)                                                    
         LA    R2,3(R2)                                                         
         LA    RF,3(RF)                                                         
         BCT   RE,VREC16_1                                                      
         B     VREC16B                                                          
         SPACE 1                                                                
VREC16A  CLI   SEPAOPT,C'N'                                                     
         BE    VREC16B                                                          
         MVC   NDEMOS,NCATS        FILTERS                                      
VREC16B  ZIC   R0,NDEMOS                                                        
         MH    R0,=H'3'                                                         
         LA    R2,DEMOS                                                         
         AR    R2,R0                                                            
         MVI   0(R2),X'FF'                                                      
         LA    R2,RNKFILTH         FILTERS                                      
         BAS   RE,EDITFILT                                                      
*                                                                               
         L     R2,AIO2                                                          
         LA    R2,1000(R2)                                                      
         MVC   SVMKTNUM,MKTNUM                                                  
VREC17   CLI   0(R2),0             EOL                                          
         BE    VREC17W                                                          
         CLI   0(R2),1             MARKET NUMBER                                
         BE    VREC17C                                                          
VREC17B  ZIC   RE,1(R2)                                                         
         AR    R2,RE                                                            
         B     VREC17                                                           
         SPACE 1                                                                
VREC17C  TM    RNKBOOK,X'F0'       CHECK FOR MULTI BOOK                         
         BNO   VREC17G             NO - CHECK REQUESTED BOOKS                   
         MVC   MKTNUM,2(R2)        YES - SEED THE MARKET NUMBER                 
         MVI   RNKMKTH+5,0                                                      
         BAS   RE,VMULTBK          CHECK BOOKS FOR THIS MARKET                  
         CLI   BYTE,1                                                           
         BE    BADBOOKM                                                         
         CLI   BYTE,2                                                           
         BNE   VREC17B                                                          
         MVC   CONHEAD(L'BADMUL),BADMUL                                         
         LH    R0,MKTNUM                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CONHEAD+L'BADMUL+1(4),DUB+5(3)                                   
         B     MYEND                                                            
         SPACE 1                                                                
VREC17G  MVC   MKTNUM,2(R2)                                                     
         CLC   =C'RADA',RNKSRCE             RADAR DONT HAVE THESE RECS          
         BE    VREC17B                                                          
         GOTOR SUBR07,DMCB,('EDITMBE',(RC))                                     
         BE    VREC17B                                                          
         B     BADBOOKS                                                         
         SPACE 1                                                                
VREC17W  MVC   RNKMKTH+5(1),SVMKTH5   RESTORE FIELDS                            
         MVC   MKTNUM,SVMKTNUM                                                  
VREC17X  DS    0H                                                               
         LA    R2,RNKTTLH          CUSTOM TITLE                                 
         MVC   RESTITLE,=CL40'RADIO RANKER'                                     
         CLC   =C'RADA',RNKSRCE             RADAR TITLE                         
         BNE   *+10                                                             
         MVC   RESTITLE,=CL40'RADAR RANKER'                                     
         CLI   5(R2),0                                                          
         BE    VREC20                                                           
         GOTO1 ANY                                                              
         MVC   RESTITLE,WORK                                                    
         SPACE 1                                                                
VREC20   GOTO1 CENTER,DMCB,RESTITLE,40                                          
         MVC   THISBUFF(256),COMBLIST                                           
         MVC   THISBUFF+256(CUMESW-COMBLIST-256),COMBLIST+256                   
         SPACE 1                                                                
VRECX    MVC   RNKSTATH+5(1),STATLEN                                            
         B     XIT                                                              
         SPACE 1                                                                
PFKERR   MVC   CONHEAD(L'PFKERRM),PFKERRM                                       
         B     MYEND                                                            
         DROP  R3                                                               
         EJECT                                                                  
VMULTBK  NTR1                                                                   
         MVI   NBOOKS,0                                                         
         MVC   BADMULP(1),RNKBOOK                                               
         XC    BOOKS,BOOKS                                                      
         CLI   RNKMKTH+5,0                                                      
         BE    *+8                                                              
         BAS   RE,EDITMKT          DO THIS TO GET MARKET #                      
         LH    RE,MKTNUM           NOW SET MARKET IN STATION FIELD              
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DBSELSTA(4),DUB+5(3)                                             
         MVI   DBSELSTA+4,C'A'     BY CONVENTION                                
         MVC   DBSELRMK,MKTNUM                                                  
*                                                                               
         LA    R1,1                                                             
         CLC   RNKBOOK+1(4),=C'BOOK'                                            
         BNE   XIT                                                              
         MVC   BYTE(1),RNKBOOK     SET NUMBER OF BOOKS                          
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,4                                                           
         BH    VMULTBKX                                                         
         CLI   RNKBOOK+5,C'('                                                   
         BNE   *+10                                                             
         MVC   DBBTYPE,RNKBOOK+6                                                
*                                                                               
         MVI   DBFUNCT,DBGETMB     GET BOOKS FOR MARKET                         
         GOTO1 DEMAND,DMCB,DBLOCK,BKHOOK                                        
         LA    R1,1                                                             
         CLI   NBOOKS,0            NOT FOUND                                    
         BE    VMULTBKX                                                         
         SPACE 1                                                                
         XC    WORK,WORK                                                        
         CLC   NBOOKS,BYTE         NOT ENOUGH BOOKS                             
         BNE   VMULTBKX                                                         
*                                                                               
         ZIC   R0,BYTE             DON'T ALLOW PREV. YEAR                       
         LA    RF,BOOKS                                                         
VMULTBK2 ZIC   R1,2(RF)            GET THE BOOK                                 
         LA    R1,WORK(R1)                                                      
         CLI   0(R1),0                                                          
         BE    *+12                                                             
         LA    R1,2                SET ERROR NUMBER                             
         B     VMULTBKX                                                         
*                                                                               
         MVI   0(R1),C'0'                                                       
         LA    RF,4(RF)                                                         
         BCT   R0,VMULTBK2                                                      
*                                                                               
         SR    R1,R1                                                            
VMULTBKX LTR   R1,R1                                                            
         STC   R1,BYTE                                                          
         B     XIT                                                              
*                                                                               
BKHOOK   NTR1                                                                   
         L     R4,DBAREC                                                        
         USING SBKEY,R4                                                         
         TM    SBBOOK,X'80'        BYPASS REVERSE SEQ BOOKS                     
         BO    XIT                                                              
**       CLC   SBBTYP,DBBTYPE                                                   
**       BNE   XIT                                                              
         CLC   SBBTYP,DBBTYPE                                                   
         BE    BKHOOK1                                                          
* IF BOOKTYPE IS NOT EQUAL THEN CHECK FOR PPM EQUIVALENCE                       
         CLI   DBBTYPE,0          IF ASKED FOR LIVE BOOKTYPE                    
         BNE   *+12               PRELIMINARY KEYS ALLOWED                      
         CLI   SBBTYP,C'P'                                                      
         BE    BKHOOK1                                                          
         CLI   DBBTYPE,C'P'       IF ASKED FOR PRELIM BOOKTYPES                 
         BNE   *+12               THEN LIVE KEYS ARE NOT ALLOWED                
         CLI   SBBTYP,0                                                         
         BE    XIT                                                              
         CLI   DBBTYPE,C'H'                                                     
         BNE   *+12                                                             
         CLI   SBBTYP,C'I'                                                      
         BE    BKHOOK1                                                          
         CLI   DBBTYPE,C'I'                                                     
         BNE   *+12                                                             
         CLI   SBBTYP,C'H'                                                      
         BE    XIT                                                              
         CLI   DBBTYPE,C'B'                                                     
         BNE   *+12                                                             
         CLI   SBBTYP,C'E'                                                      
         BE    BKHOOK1                                                          
         CLI   DBBTYPE,C'E'                                                     
         BNE   *+12                                                             
         CLI   SBBTYP,C'B'                                                      
         BE    XIT                                                              
         B     XIT                                                              
* STOP DUPLICATE ENTRIES                                                        
BKHOOK1  ZIC   R1,NBOOKS                                                        
         CHI   R1,0                                                             
         BE    *+10                                                             
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LA    R1,BOOKS(R1)                                                     
         CLC   1(2,R1),SBBOOK                                                   
         BE    XIT                                                              
*                                                                               
         ZIC   R1,NBOOKS                                                        
         LA    R1,1(R1)                                                         
         CLC   NBOOKS,BYTE         BOOK TABLE FULL - NEED A SLIDE               
         BNE   BKHOOK2                                                          
         ZIC   RE,NBOOKS                                                        
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         LA    RE,BOOKS(RE)                                                     
         CLC   1(2,RE),SBBOOK      IF LATEST BOOK IN TABLE IS ALREADY           
         BNL   XIT                 EQUAL OR HIGHER THAN KEEP EXIT               
*                                                                               
         MVC   BOOKS(L'BOOKS-4),BOOKS+4                                         
         IC    R1,NBOOKS                                                        
*                                                                               
BKHOOK2  STC   R1,NBOOKS                                                        
         BCTR  R1,0                                                             
         SLL   R1,2                *4                                           
         LA    R1,BOOKS(R1)                                                     
         MVC   1(2,R1),SBBOOK                                                   
         MVC   3(1,R1),SBBTYP                                                   
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              EDIT MARKET (OPTIONALLY STATIONS BELOW)                          
         SPACE 3                                                                
EDITMKT  NTR1                                                                   
         OC    MKTNUM,MKTNUM       DO WE HAVE MARKET #?                         
         BNZ   HAVEMKTN            GOT IT ALREADY                               
         XC    WSTATS,WSTATS                                                    
         MVI   NCOMBOS,0                                                        
         MVI   NSTATS,0                                                         
         MVI   WSTATS+3,5          STATION TABLE WIDTH SET TO 5                 
         LA    R2,RNKMKTH          EITHER MARKET NUMBER OR STALIST              
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 ANY                                                              
         CLI   8(R2),C'='                                                       
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(1,BLOCK)                                      
         L     R1,BLOCK+4                                                       
         LTR   R1,R1                                                            
         BZ    EDITLIST                                                         
         STH   R1,MKTNUM                                                        
         MVI   STASTYLE,C'M'                                                    
HAVEMKTN MVI   RNKMKTN,C'*'                                                     
         BAS   RE,GETMKT                                                        
*                                                                               
         CLC   =C'RADA',RNKSRCE                                                 
         BE    XIT                                                              
*                                                                               
         CLI   RNKMKTN,C'*'                                                     
         BE    BADMKT                                                           
         L     R4,AMYD                                                          
         USING MYD,R4                                                           
         ZIC   RE,NMARKET                                                       
         LA    RE,1(RE)                                                         
         STC   RE,NMARKET                                                       
         MVC   STASTYLE,STALIST                                                 
         DROP  R4                                                               
         CLI   RNKSTATH+5,0        MAY ALSO HAVE INPUT STATIONS                 
         BNE   EDITSTAT            IF SO, GO AND USE THESE                      
         B     XIT                                                              
         SPACE 1                                                                
GETMKT   NTR1                                                                   
         MVC   DBAREC,AIO1                                                      
         MVI   DBFUNCT,DBGETMK                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'R'                                                    
         MVC   DBSELRMK,MKTNUM                                                  
         GOTO1 DEMAND,DMCB,DBLOCK,MKTHOOK                                       
         B     XIT                                                              
         SPACE 1                                                                
MKTHOOK  NTR1                      HOOK FOR MARKET NAME                         
         OI    RNKMKTNH+6,X'80'                                                 
         MVC   RNKMKTN,SPACES                                                   
         L     R4,DBAREC                                                        
         USING DMKEY,R4                                                         
         LA    R4,DMFRSTEL                                                      
         USING DMELEM,R4                                                        
         ZIC   R1,DMLEN                                                         
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   RNKMKTN(0),DMMNAME                                               
         DROP  R4                                                               
         EJECT                                                                  
*              EDIT A LIST OF STATIONS                                          
         SPACE 3                                                                
*                                  IF MARKET WAS NOT NUMERIC                    
*                                  IT MIGHT BE A STATION LIST                   
         SPACE 1                                                                
EDITLIST CLI   BLOCK+12,C'+'       CAN HAVE COMBO LIST                          
         BE    *+20                                                             
         CLI   RNKSTATH+5,0        CAN'T HAVE STATION LIST                      
         BNE   BADMKT              AS WELL AS CUSTOM STATIONS                   
         MVI   STASTYLE,C'L'                                                    
         MVI   WSTATS+3,9                                                       
         LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         USING SLKEY,R5                                                         
         MVC   SLKTYPE(2),=X'0D5B'                                              
         MVC   SLKAM,BAGYMD                                                     
         MVC   SLKNAME,BLOCK+12                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BADMKT                                                           
         GOTO1 GETREC                                                           
         XC    MKTNUM,MKTNUM                                                    
         L     R5,AIO1                                                          
         LA    R5,SLDELEM                                                       
         SR    R3,R3               COUNT NUMBER OF VALID STATION                
         LA    R4,BUFF             STATIONS ARE IN BUFF                         
         SPACE 1                                                                
LIST2    CLI   0(R5),X'01'         DESCRIPTION ELEMENT                          
         BNE   LIST4                                                            
         USING SLDELEM,R5                                                       
         MVC   MKTNUM,SLDMKTA      PICK UP ARB MARKET NUMBER                    
         CLI   CTRY,C'C'                                                        
         BE    LIST3                                                            
         CLI   RNKSRCE,C'B'                                                     
         BNE   *+10                                                             
         MVC   MKTNUM,SLDMKTB      OR BIRCH                                     
*                                                                               
LIST3    CLI   BLOCK+12,C'+'       COMBO LIST                                   
         BE    LISTNEXT            DON'T USE NAME                               
         MVC   RNKMKTN,SLDDESC                                                  
         OI    RNKMKTNH+6,X'80'                                                 
         B     LISTNEXT                                                         
         SPACE 1                                                                
LIST4    CLI   0(R5),X'05'         STATION ELEMENT                              
         BNE   LISTNEXT                                                         
         USING SLSELEM,R5                                                       
         MVC   WORK(4),SLSFORM     SAVE FORMAT                                  
         LA    RF,SLSTALST                                                      
         ZIC   R1,1(R5)            N'STATION = (L-6)/5                          
         SH    R1,=H'6'                                                         
         SR    R0,R0                                                            
         D     R0,=F'5'                                                         
         SPACE 1                                                                
         CLI   BLOCK+12,C'+'       IS IT A COMBO                                
         BE    LIST7               YES - CREATE COMBO LIST                      
         SPACE 1                                                                
         AR    R3,R1                                                            
         SPACE 1                                                                
LIST6    MVC   0(5,R4),0(RF)       THESE ENTRIES ARE STATION                    
         MVC   5(4,R4),WORK        PLUS FORMAT                                  
         A     R4,WSTATS           WIDTH OF STATIONS                            
         LA    RF,5(RF)                                                         
         BCT   R1,LIST6                                                         
         B     LISTNEXT                                                         
         SPACE 1                                                                
LIST7    BAS   RE,LIST10                                                        
         SPACE 1                                                                
LISTNEXT ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         CLI   0(R5),0                                                          
         BNE   LIST2                                                            
         STC   R3,NSTATS                                                        
         LTR   R3,R3                                                            
         BZ    LIST8                                                            
         ZIC   RE,NCOMBOS          ADJUST FOR COMBO LISTS                       
         AR    RE,R3                                                            
         STC   RE,HALF                                                          
         CLC   MAXRANK(1),HALF                                                  
         BL    *+10                                                             
         MVC   MAXRANK(1),HALF                                                  
LIST8    OC    MKTNUM,MKTNUM       SHOULD NOW HAVE MARKET NUMBER                
         BZ    BADLIST                                                          
         B     XIT                                                              
         SPACE 1                                                                
LIST10   NTR1                                                                   
         L     R4,AMYD             ADDRESS START OF LIST                        
         USING MYD,R4                                                           
         LA    R4,COMBLIST         POINT TO SAVE AREA                           
         DROP  R4                  DON'T NEED ADDRESSABLITY ANYMORE             
*                                                                               
LIST12   CLI   0(R4),0             EMPTY SLOT IS OK                             
         BE    LIST14                                                           
         CLC   WORK(4),0(R4)       AND EQUAL ID IS ALSO                         
         BE    LIST14                                                           
         LA    R4,LNCOMB(R4)       OTHERWISE TRY NEXT SLOT                      
         B     LIST12                                                           
LIST14   MVC   0(4,R4),WORK        FOUND A SLOT                                 
         ZIC   RE,NCOMBOS          KEEP TRACK OF NUMBER OF COMBOS               
         LA    RE,1(RE)                                                         
         STC   RE,NCOMBOS                                                       
         LA    RE,9(R4)                                                         
         CLI   0(RE),0             SO FIND AN EMPTY SLOT HERE                   
         BE    *+12                                                             
         LA    RE,5(RE)                                                         
         B     *-12                                                             
LIST16   MVC   0(5,RE),0(RF)       ADD COMPLETE LIST OF STATIONS                
         LA    RE,5(RE)                                                         
         LA    RF,5(RF)                                                         
         BCT   R1,LIST16           HAS NUMBER OF STATIONS                       
         XIT1                                                                   
         SPACE 1                                                                
BADMKT   MVC   CONHEAD(L'MKTERR),MKTERR                                         
         B     MYEND                                                            
         SPACE 1                                                                
BADLIST  MVC   CONHEAD(L'LISTERR),LISTERR                                       
         B     MYEND                                                            
         EJECT                                                                  
*              EDIT CUSTOM STATIONS                                             
         SPACE 3                                                                
EDITSTAT LA    R2,RNKSTATH         OR A LIST OF STATIONS                        
         LA    R3,1                COUNT NUMBER OF VALID STATIONS               
         LA    R4,BUFF             STATIONS ARE IN BUFF                         
         LA    R0,12               MAXIMUM OF 12 FROM SCREEN                    
         GOTO1 ANY                 MUST BE AT LEAST 1                           
STAT4    CLI   8(R2),C'+'                                                       
         BE    STAT7                                                            
         SPACE 1                                                                
         MVI   STASTYLE,C'C'                                                    
         GOTOR SUBR08,DMCB,('VALDSTAE',(RC))                                    
         MVC   0(5,R4),ACTSTAT                                                  
         GOTOR SUBR07,DMCB,('EDITSBE',(RC))                                     
         BNE   BADBOOKS                                                         
         STC   R3,NSTATS                                                        
         BAS   RE,BUMP                                                          
         CLI   5(R2),0                                                          
         BE    STAT6                                                            
         LA    R3,1(R3)                                                         
         LA    R4,5(R4)                                                         
         BCT   R0,STAT4                                                         
         SPACE 1                                                                
STAT6    OC    NSTATS,NSTATS                                                    
         BZ    XIT                                                              
         ZIC   RE,NCOMBOS          INCLUDE THE COMBOS ALSO                      
         ZIC   RF,NSTATS                                                        
         AR    RE,RF                                                            
         STC   RE,HALF                                                          
         CLC   MAXRANK(1),HALF                                                  
         BL    *+10                                                             
         MVC   MAXRANK(1),HALF                                                  
         B     XIT                                                              
         SPACE 1                                                                
STAT7    BAS   RE,EDCOMB           EDIT A COMBO LIST                            
         BAS   RE,BUMP                                                          
         CLI   5(R2),0             ANY INPUT                                    
         BE    STAT6                                                            
         BCT   R0,STAT4            ANY MORE STATIONS LEFT                       
         B     STAT6                                                            
         SPACE 1                                                                
EDCOMB   NTR1                                                                   
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,PARAS,(R2),(1,BLOCK),0                                   
         B     EDITLIST            EDIT FOR A COMBO LIST                        
         EJECT                                                                  
*              EDIT OPTIONS                                                     
         EJECT                                                                  
*              EDIT FILTERS                                                     
         SPACE 3                                                                
EDITFILT NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    BADFILT                                                          
         SPACE 1                                                                
FILT2    DS    0H                                                               
         SPACE 1                                                                
BADFILT  MVC   CONHEAD(L'FILTERR),FILTERR                                       
         B     SPERR                                                            
         SPACE 1                                                                
FILTEND  LA    R4,32(R4)                                                        
         BCT   R0,FILT2                                                         
         B     XIT                                                              
         EJECT                                                                  
         EJECT                                                                  
BLDMKT   NTR                                                                    
         MVI   DBFUNCT,DBGETMKB                                                 
         LA    RE,BUFF                                                          
         LR    R2,RE                                                            
         L     RF,=F'7000'                                                      
         XCEF                                                                   
         ST    R2,FULL                                                          
         XC    DUB,DUB                                                          
         GOTO1 DEMAND,DMCB,DBLOCK,MKBHOOK                                       
         OC    DUB(4),DUB                                                       
         BZ    XIT                                                              
         BAS   RE,MKBFILL                                                       
         B     XIT                                                              
         SPACE 2                                                                
MKBHOOK  NTR1                                                                   
         L     R2,FULL                                                          
         L     R4,DBAREC                                                        
         USING DMKEY,R4                                                         
         MVC   0(2,R2),DMRMKT                                                   
         LA    R4,DMFRSTEL                                                      
         USING DMELEM,R4                                                        
         ZIC   R1,DMLEN                                                         
         SH    R1,=H'5'                                                         
         CH    R1,=H'12'                                                        
         BL    *+8                                                              
         LA    R1,11                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R2),DMMNAME                                                  
         L     R1,DUB                                                           
         LA    R1,1(R1)                                                         
         ST    R1,DUB                                                           
         LA    R2,14(R2)                                                        
         ST    R2,FULL                                                          
         B     XIT                                                              
         DROP  R4                                                               
MKBFILL  NTR1                                                                   
         L     R2,DUB                                                           
         GOTO1 XSORT,DMCB,(0,BUFF),(R2),14,12,2                                 
         LA    R2,RMKMKLH+(RMKMKA1H-RMKMK1H)                                    
         TWAXC RMKMK1H,(R2),PROT=Y                                              
         LA    R1,RMKMKLH+(RMKMKA1H-RMKMK1H)                                    
         LA    R8,BUFF                                                          
         LA    RE,RMKWORK                                                       
         LA    R2,RMKMKA1H                                                      
MKBFILL2 CR    R2,R1                                                            
         BH    XIT                                                              
         CLC   SVTKMKT(12),2(R8)                                                
         BNL   MKBFILL3                                                         
*                                                                               
         CLI   SVTFLTR,C'='        FILTER ACTIVE                                
         BNE   *+14                                                             
         CLC   SVTFLTR+1(1),2(R8)                                               
         BH    MKBFILL3                                                         
*                                                                               
         OI    6(R2),X'80'         TRANSMIT                                     
         MVC   SVTKMKT(12),2(R8)                                                
         EDIT  (B2,0(R8)),(4,DUB)                                               
         MVC   RMKMKA1-RMKMKA1H(4,R2),DUB                                       
         MVC   RMKMKA1-RMKMKA1H+5(L'RMKMKA1-5,R2),2(R8)                         
         LA    R2,RMKMK2H-RMKMK1H(R2)                                           
MKBFILL3 LA    R8,14(R8)                                                        
         OC    0(2,R8),0(R8)                                                    
         BNZ   MKBFILL2                                                         
         XC    RMKWORK(12),RMKWORK                                              
         B     XIT                                                              
         EJECT                                                                  
SELMKT   NTR1                                                                   
         LA    RE,RMKWORK                                                       
         LA    R4,SVTMKTS          POINT TO SAVED MARKET LIST                   
         OC    0(3,R4),0(R4)       FIND AN OPEN SLOT                            
         BZ    *+12                                                             
         LA    R4,3(R4)                                                         
         B     *-14                                                             
         SPACE 1                                                                
         LA    RF,RMKMKLH+(RMKMKA1H-RMKMK1H)   SET FOR END                      
         LA    R2,RMKMK1H          SET TO START                                 
SELMKT2  CR    R2,RF               END OF INPUT                                 
         BH    XIT                                                              
         LA    RE,RMKWORK                                                       
         LA    RE,SVTMKTS+39       STOP IF MAX.                                 
         CR    R4,RE                                                            
         BNL   XIT                                                              
*                                                                               
         CLI   5(R2),0             ANY INPUT                                    
         BE    SELMKT4             NO - ON TO NEXT                              
         MVC   0(1,R4),8(R2)                                                    
         BAS   RE,BUMP                                                          
         MVC   FULL,8(R2)          SET MARKET NUMBER                            
         OC    FULL,=C'0000'       MAKE IT NUMBERS                              
         PACK  DUB,FULL                                                         
         CVB   R1,DUB                                                           
         LTR   R1,R1               IGNORE IF NO MARKET                          
         BZ    *+12                                                             
         STCM  R1,3,1(R4)          SAVE THE MARKET                              
         LA    R4,3(R4)                                                         
         B     *+8                 ALREADY PAST FIRST FIELD                     
         SPACE 1                                                                
SELMKT4  BAS   RE,BUMP             BYPASS FIRST                                 
         BAS   RE,BUMP             BYPASS SECOND                                
         B     SELMKT2                                                          
         EJECT                                                                  
BLDUL    NTR1                                                                   
         LA    RE,RNKUL1H          POINT TO DATA FIELD                          
         LA    RF,68(RE)           LAST CHAR IN FIELD                           
         LA    R2,8(RE)                                                         
         XC    0(60,R2),0(R2)                                                   
         LA    R4,BUFF+(SVTMKTS-SVTD) AND SAVED INPUT                           
         XC    48(3,R4),48(R4)     LIMIT TO 5 MAX                               
         CLI   0(R4),0                                                          
         BE    XIT                                                              
         MVC   0(2,R2),=C'M='                                                   
         LA    R2,2(R2)                                                         
BLDUL2   CLI   0(R4),0             END OF LIST                                  
         BE    BLDUL4              ADJUST THE SCREEN HEADER                     
         LR    R5,R2               SAVE AS A REFERENCE                          
         EDIT  (B2,1(R4)),(5,MSASAVE),ALIGN=LEFT                                
         AR    R5,R0               R0 HOLDS CHARACTERS OUTPUTTED                
         CR    R5,RF                                                            
         BH    BLDUL4                                                           
         LR    R1,R0               WE CAN MOVE CHARACTERS                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),MSASAVE                                                  
         LR    R2,R5               EQUAL OR LESS THAN LAST ONE                  
         CR    R2,RF                                                            
         BE    BLDUL4                                                           
*                                                                               
*        CLI   0(R2),C' '          ELIMINATE TRAILING SPACES                    
*        BE    *+12                                                             
*        LA    R2,1(R2)                                                         
*        B     *-12                                                             
         MVI   0(R2),C','          SET UP FOR NEXT                              
         LA    R2,1(R2)                                                         
         LA    R4,3(R4)                                                         
         B     BLDUL2                                                           
*                                                                               
BLDUL4   BCTR  R2,0                GET RID OF TRAILING COMMA                    
         XC    0(1,R2),0(R2)                                                    
         LA    R1,8(RE)            GET LENGTH                                   
         SR    R2,R1                                                            
         STC   R2,5(RE)                                                         
         CLI   0(R4),0             HAVE TO END HERE                             
         BZ    XIT                                                              
         C     RE,RNKUL3H          STOP AFTER THIS                              
         BE    XIT                                                              
         ZIC   RF,0(RE)            BUMP TO NEXT USER LIST                       
         AR    RE,RF                                                            
         LA    RF,68(RE)           LAST CHAR IN FIELD                           
         LA    R2,8(RE)                                                         
         XC    0(60,R2),0(R2)                                                   
         B     BLDUL2                                                           
         EJECT                                                                  
*              CONTROL REPORT PRINTING                                          
         SPACE 3                                                                
TMPSEPBF DC    XL21'00'            1 FOR LEN, 20 FOR 6 DEM/CAT                  
         DC    X'FF'               END OF DEMO LIST                             
TMPDEMOS DC    XL60'00'                                                         
PRPNDEMS DC    X'00'                                                            
PRPDEMOS DC    XL24'00'                                                         
PTMPR3   DC    F'0'                                                             
*                                                                               
         USING MYD,R3                                                           
PREP     L     R1,=A(HEDSPECS)     INTIALIZATION                                
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         ST    R3,PTMPR3                                                        
*                                                                               
*        MVI   NOSHARE,C'Y'                                                     
P_SEP0   CLI   SEPAOPT,C'N'                                                     
         BE    PSEPX                                                            
         CLI   SEPAOPT,C'D'                                                     
         BNE   P_SEP0A                                                          
         MVI   DEMSTYLE,C'C'                                                    
         B     P_SEP0B                                                          
P_SEP0A  MVI   DEMSTYLE,C'D'                                                    
P_SEP0B  MVC   PRPNDEMS(22),TMPSEPBF                                            
         MVC   TMPDEMOS,DEMOS                                                   
P_SEP1   MVI   RUNIFLG,C'N'                                                     
         XC    RUNIVS,RUNIVS                                                    
         MVC   DEMOS,TMPDEMOS      GET THE CORRECT DEMOS                        
         L     R3,PTMPR3                                                        
         USING MYD,R3                                                           
         ZIC   R5,NDEMOS                                                        
         LA    R2,DEMOS                                                         
P_SEP2   CLI   SEPAOPT,C'D'                                                     
         BNE   P_SEP2A                                                          
         MVC   2(1,R2),PRPDEMOS+2  INSERT THE DEMO                              
         B     P_SEP3                                                           
P_SEP2A  MVC   1(1,R2),PRPDEMOS+1  INSERT THE CATEGORY                          
P_SEP3   LA    R2,3(R2)                                                         
         LTR   R5,R5                                                            
         BZ    P_SEP9                                                           
         BCT   R5,P_SEP2                                                        
*                                                                               
P_SEP9   ZIC   R5,PRPNDEMS                                                      
         BCTR  R5,0                                                             
         STC   R5,PRPNDEMS                                                      
         MH    R5,=H'3'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   PRPDEMOS(0),PRPDEMOS+3                                           
         LA    R2,PRPDEMOS                                                      
         AR    R2,R5                                                            
         LA    R2,1(R2)                                                         
         MVI   0(R2),X'FF'         MAKE SURE TO END DEMOS                       
PSEPX    MVC   COMBLIST(256),THISBUFF                                           
         MVC   COMBLIST+256(CUMESW-COMBLIST-256),THISBUFF+256                   
         MVI   USEBUFF,C'N'                                                     
         MVI   CMB_B4,C'Y'                                                      
         MVI   STASTYLE,C'M'                                                    
         MVI   FIRSTTM,C'Y'                                                     
         MVI   PRNTD,C'N'                                                       
         MVI   RUNIFLG,C'N'                                                     
         XC    RUNIVS,RUNIVS                                                    
         MVC   TMPDEMOS,DEMOS                                                   
*                                                                               
         LA    RF,TMPDEMOS                                                      
         MVI   USESHARE,C'N'                                                    
         ZIC   RE,NDEMOS                                                        
CKDEM10  CLI   1(RF),C'S'          ARE WE CALCULATING SHARES?                   
         BNE   CKDEM20                                                          
         MVI   USESHARE,C'Y'                                                    
         B     FINCKDEM                                                         
CKDEM20  LA    RF,3(RF)                                                         
         BCT   RE,CKDEM10                                                       
*                                                                               
FINCKDEM CLI   CALCOPT,C'Y'        DO IMPRESSIONS?                              
         BNE   NOTIMPS1                                                         
*--------------------------------------------------------------                 
*  EQUATES FOR IMPRESSIONS                                                      
*    1)  F = C              6) S = I, IFF SOURCE <> CANADIAN                    
*    2)  G = D                                                                  
*    3)  H = E                                                                  
*    4)  P = Q                                                                  
*    5)  R = I                                                                  
*--------------------------------------------------------------                 
         LA    R2,DEMOS                                                         
         ZIC   R0,NDEMOS                                                        
IMPAGN0  LA    R1,IMPTABLE                                                      
IMPAGN1  CLI   0(R1),X'FF'         END OF IMPTABLE?                             
         BE    IMPNXT1                                                          
         CLC   1(1,R2),0(R1)       DOES DEMO NEED CONVERSION?                   
         BNE   NOEQU1                                                           
         CLI   1(R2),C'S'          SHARE BECOMES IMPRESSIONS                    
         BNE   NOEQU2                                                           
*        MVI   NOSHARE,C'N'                                                     
         CLI   DBSELSRC,C'M'       CANADIAN MARKETS DON'T                       
         BE    IMPNXT1                                                          
         CLI   0(R2),2             TSA MARKETS DON'T                            
         BE    IMPNXT1                                                          
NOEQU2   MVC   1(1,R2),1(R1)       USED CONVERTED IMPRESSION                    
         B     IMPNXT1                                                          
NOEQU1   LA    R1,2(R1)            GET NEXT POSSIBLE IMPRESSION                 
         B     IMPAGN1               FOR CONVERSION                             
IMPNXT1  LA    R2,3(R2)            NEXT DEMO                                    
         BCT   R0,IMPAGN0          UNTIL ALL DEMOS CHECKED                      
         B     NOTIMPS1                                                         
*--------------------------------------------------------------                 
IMPTABLE DC    C'FCGDHEPQRISI',X'FF'                                            
*--------------------------------------------------------------                 
NOTIMPS1 OC    MKTNUM,MKTNUM       RESET BOOKS FOR SOFT LIST                    
         BZ    PREPSM                                                           
         TM    RNKBOOK,X'F0'                                                    
         BNO   PREPSM                                                           
         BAS   RE,VMULTBK                                                       
*                                                                               
PREPSM   L     RE,AIO2                                                          
         LA    RE,1000(RE)                                                      
         ST    RE,MKTPNTR                                                       
*        MVI   NCMBS,0                                                          
*                                                                               
         BAS   RE,CHKFORM                                                       
*                                                                               
         CLI   0(RE),1             MARKET ELEMENT                               
         BNE   PREP1                                                            
         CLC   MKTNUM,2(RE)        CHECK FOR DUP                                
         BNE   PREP1                                                            
         ZIC   R1,1(RE)                                                         
         AR    RE,R1                                                            
         ST    RE,MKTPNTR                                                       
*                                                                               
PREP1    MVI   ANYUNIV,C'N'                                                     
         MVI   USEBUFF,C'N'                                                     
         MVI   STACTSW,0                                                        
         XC    UNIVBK,UNIVBK                                                    
         GOTO1 =A(SETDPTS),DMCB,(R9),(RC),RR=RELO                               
         LA    RE,BUFF             INITIALIZE MAIN BUFFER                       
         LA    RE,500(RE)                                                       
         L     RF,=AL4(L'BUFFREC*40*15+100)                                     
         XCEF                                                                   
         OC    MKTNUM,MKTNUM       IF LIST ONLY INPUT                           
         BZ    PREP4                                                            
*                                                                               
         GOTO1 =A(COMPDISP),DMCB,(R8),(R9),(RC),RR=RELO                         
*                                                                               
         L     R3,AMYD                                                          
         CLI   USESHARE,C'Y'                                                    
         BNE   NOSHRS                                                           
         XC    MKTBUFF,MKTBUFF                  CLEAR OUT OLD STUFF             
         XC    MKTBUF2,MKTBUF2                  CLEAR OUT OLD STUFF             
         GOTO1 =A(MKTSTAT),DMCB,MKTSTN,(R9),(RC),RR=RELO                        
         GOTO1 =A(FILLBUFF),DMCB,MKTSTN,(R7),(R9),(RC),RR=RELO                  
         BAS   RE,TOTDIV                                                        
NOSHRS   MVC   MKTSTN,=C'     '                                                 
         DROP  R3                                                               
*                                                                               
         GOTO1 =A(EXPSTATS),DMCB,(R9),(RC),RR=RELO                              
*                                                                               
         L     R2,ASTATS                                                        
         LA    R3,1                                                             
         ZIC   R0,NSTATS                                                        
         LTR   R0,R0               IF NO VALID STATIONS                         
         BZ    XIT                 THE STORY'S OVER                             
         CLI   FIRSTTM,C'Y'        FIRST TIME PRINTING?                         
         BE    PREP4               YES, SKIP THE FIRST REPORT                   
*                                                                               
*        ZIC   R1,NCMBS                                                         
*        SR    R0,R1                                                            
*        BZ    PREP2A                                                           
*                                                                               
PREP2    STC   R3,STATNUM          NOTE STATION NUMBER                          
         L     R4,AMYD                                                          
         USING MYD,R4                                                           
         MVI   NEWSTAT,C'Y'        SET TO CLEAR BUFFER                          
*                                                                               
         MVC   HALF,=H'1'                                                       
         GOTO1 =A(FILLBUFF),DMCB,(R2),(R7),(R9),(RC),RR=RELO                    
         GOTO1 =A(RANKBUFF),DMCB,(R9),(RC),RR=RELO                              
         A     R2,WSTATS                                                        
         LA    R3,1(R3)                                                         
         BCT   R0,PREP2                                                         
*                                                                               
PREP2A   BAS   RE,FILLCMB          COMBINE THE COMBO                            
*                                  WE'RE DONE WITH THE STATIONS                 
         BAS   RE,PRNTBUFF         SO PRINT IT ALL OUT                          
         MVI   RUNIFLG,C'N'                                                     
         XC    RUNIVS(16),RUNIVS   CLEAR ALL 4 WORDS                            
         GOTO1 =A(FOOT),DMCB,(R8),(R9),(RC),RR=RELO                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   CMB_B4,C'N'                                                      
         CLI   PRNTD,C'N'                                                       
         BNE   PREP3                                                            
         MVI   PRNTD,C'Y'                                                       
PREP3    L     RE,MKTPNTR                                                       
         CLI   0(RE),0                                                          
         BNE   PREP4                                                            
         CLI   SEPAOPT,C'N'                                                     
         BE    PREPXIT                                                          
         CLI   PRPNDEMS,0                                                       
         BZ    PREPXIT                                                          
         L     RE,AIO2                                                          
         LA    RE,1000(RE)                                                      
         MVC   MKTNUM,2(RE)                                                     
         BAS   RE,GETMKT                                                        
         B     P_SEP1                                                           
PREPXIT  MVC   RNKSTATH+5(1),STATLEN                                            
         B     XIT                                                              
*                                                                               
PREP4    L     R4,AMYD             MAKE SURE TO POINT TO NMOD AREA              
         MVI   FIRSTTM,C'N'                                                     
         MVI   CMB_B4,C'Y'                                                      
         LA    RE,COMBLIST                                                      
         LA    RF,CUMESW-COMBLIST                                               
         XCEF                                                                   
         MVC   SVMKTH5,RNKMKTH+5                                                
         MVI   RNKMKTH+5,0                                                      
*                                                                               
         LH    RE,MKTNUM           NOW SET MARKET IN STATION FIELD              
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DBSELSTA(4),DUB+5(3)                                             
         MVI   DBSELSTA+4,C'A'     BY CONVENTION                                
         MVC   DBSELRMK,MKTNUM                                                  
*                                                                               
         MVC   RNKMKTH+5(1),SVMKTH5                                             
         ZIC   R0,NSTATS                                                        
         MVI   NSTATS,0                                                         
         GOTO1 =A(EXPSTATS),DMCB,(R9),(RC),RR=RELO                              
         STC   R0,NSTATS                                                        
*                                                                               
         L     R2,ASTATS                                                        
         LA    R3,1                                                             
         ZIC   R0,NSTATS                                                        
         LTR   R0,R0               IF NO VALID STATIONS                         
         BZ    XIT                 THE STORY'S OVER                             
PREP4A   L     RE,MKTPNTR          CHEK FOR MULTI-MARKET REQ                    
         CLI   0(RE),0             END OF LIST                                  
         BE    PREP1               FINISH REPORT                                
         CLI   0(RE),1                                                          
         BE    PREP5                                                            
         B     PREP6                                                            
*                                                                               
PREP5    CLC   MKTNUM,2(RE)                                                     
         BE    PREP5A                                                           
         CLI   PRNTD,C'N'                                                       
         BE    PREP1                                                            
         MVI   FIRSTTM,C'Y'                                                     
         MVC   MKTNUM,2(RE)                                                     
         MVI   STASTYLE,C'M'                                                    
         MVC   MAXRANK,SVMAXRNK    RESTORE MAX RANK                             
         BAS   RE,CHKFORM                                                       
         B     PREP1                                                            
*                                                                               
PREP5A   ZIC   RF,1(RE)            AND BUMP PAST IT                             
         AR    RE,RF                                                            
         ST    RE,MKTPNTR                                                       
*                                                                               
         BAS   RE,GETMKT           GET MARKET NAME                              
         TM    RNKBOOK,X'F0'                                                    
         BNO   PREP6                                                            
         MVC   SVMKTH5,RNKMKTH+5                                                
         MVI   RNKMKTH+5,0                                                      
         BAS   RE,VMULTBK          RESET FOR SOFT BOOKS                         
         MVC   RNKMKTH+5(1),SVMKTH5                                             
         BNE   MBKERR              NOT ENOUGH BOOKS                             
*                                                                               
PREP6    L     RE,MKTPNTR                                                       
         MVI   PRNTD,C'N'                                                       
PREP6A   CLI   0(RE),2                                                          
         BE    PREP7                                                            
         BL    PREP4A                                                           
         CLI   0(RE),3                                                          
         BE    PREP6B                                                           
         CLI   0(RE),4             STATION LIST                                 
         BE    PREP7                                                            
         ZIC   RF,1(RE)            UNDEFINED                                    
         AR    RE,RF               SKIP IT                                      
         ST    RE,MKTPNTR                                                       
         B     PREP4A                                                           
*------------------------------------------------------------                   
* THIS PART SHOULD TAKE CARE OF THE BUFFER                                      
* PUT THE STATIONS IN THE STATION LIST, COUNT # OF STATIONS                     
* AND DO FILLBUFF AND GO BACK TO CHECK NEXT ELEMENT                             
*------------------------------------------------------------                   
PREP6B   LR    RF,RE                                                            
         CLI   USEBUFF,C'N'                                                     
         BNE   PREP6C                                                           
*------------------------------------------------------------                   
* CHECK WHETHER OR NOT TO CLEAR BUFFER                                          
* BY CHECKING IF THERE IS A STATION LIST AFTER THIS BUT BEFORE                  
* A NEW MARKET OR BEFORE THE END OF USER LIST                                   
*------------------------------------------------------------                   
PREP6B1  ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BE    PREP6OK                                                          
         CLI   0(RE),1                                                          
         BE    PREP6OK                                                          
         CLI   0(RE),2             STATION BEFORE 1 AND 0                       
         BE    PREP6BC             CLEAR THE BUFFER                             
         B     PREP6B1                                                          
*                                                                               
PREP6BC  LR    R0,RF                                                            
         LA    RE,BUFF                                                          
         LA    RE,500(RE)                                                       
         L     RF,=AL4(L'BUFFREC*40*15+100)                                     
         XCEF                                                                   
         LA    RE,PREP6BC1                                                      
         B     PREP7A                                                           
PREP6BC1 LR    RF,R0                                                            
         B     PREP6BX                                                          
*                                                                               
PREP6OK  MVI   CMB_B4,C'Y'                                                      
PREP6BX  MVI   USEBUFF,C'Y'                                                     
PREP6C   BAS   RE,SEEDCMB                                                       
         LR    RE,RF                                                            
         B     PREP4A                                                           
PREP7    CLI   USEBUFF,C'N'                                                     
         BNE   PREP9                                                            
*                                                                               
         MVI   USEBUFF,C'Y'                                                     
         LA    RE,BUFF                                                          
         LA    RE,500(RE)                                                       
         L     RF,=AL4(L'BUFFREC*40*15+100)                                     
         XCEF                                                                   
         LA    RE,PREP9                                                         
PREP7A   LA    R3,1                                                             
         L     R4,AMYD                                                          
         XC    NSTATS,NSTATS                                                    
         LA    R2,BUFF                                                          
         ST    R2,ASTATS                                                        
*                                                                               
*        CLI   DBSELSRC,C'M'                                                    
*        BER   RE                                                               
*        CLI   DEMOS,2                                                          
*        BER   RE                                                               
*        CLI   NOSHARE,C'Y'                                                     
*        BER   RE                                                               
*                                                                               
*        MVI   NSTATS,1                                                         
*        STC   R3,STATNUM                                                       
*        LR    R0,RE               SAVE RETURN ADDRESS                          
*        GOTO1 =A(MKTSTAT),DMCB,(R2),(R9),(RC),RR=RELO                          
*        ZIC   RF,MAXRANK                                                       
*        LA    RF,1(RF)                                                         
*        STC   RF,MAXRANK                                                       
*        CLI   MAXRANK,40                                                       
*        BL    *+8                                                              
*        MVI   MAXRANK,40                                                       
*        GOTO1 =A(FILLBUFF),DMCB,(R2),(R7),(R9),(RC),RR=RELO                    
*        A     R2,WSTATS           PREPARE FOR NEXT STATION                     
*        LA    R5,5(R5)            NEXT STATION                                 
*        LA    R3,1(R3)            INCREMENT STATION NUMBER                     
*        LR    RE,R0               RESTORE RETURN ADDRESS                       
*                                                                               
         BR    RE                                                               
*                                                                               
PREP9    L     RE,MKTPNTR          LENGTH OF ELEMENT                            
         MVI   CMB_B4,C'N'                                                      
         ZIC   RF,1(RE)            LENGTH OF ELEMENT                            
         LA    R5,2(RE)                                                         
         SH    RF,=H'2'                                                         
         CLI   0(RE),4                                                          
         BNE   PREP9A                                                           
         LA    R5,4(R5)                                                         
         SH    RF,=H'4'                                                         
PREP9A   SR    RE,RE                                                            
         D     RE,=F'5'                                                         
         ZIC   RE,NSTATS                                                        
         AR    RE,RF                                                            
         STC   RE,NSTATS                                                        
         CLC   MAXRANK,NSTATS                                                   
         BH    PREP9B                                                           
         ZIC   RE,MAXRANK                                                       
         AR    RE,RF                                                            
         STC   RE,MAXRANK                                                       
         CLI   MAXRANK,40                                                       
         BL    *+8                                                              
         MVI   MAXRANK,40                                                       
PREP9B   LR    R0,RF                                                            
         L     RE,MKTPNTR                                                       
*                                                                               
PREP10   MVC   0(5,R2),0(R5)                                                    
         CLI   0(RE),4                                                          
         BNE   *+10                                                             
         MVC   5(4,R2),2(RE)                                                    
         STC   R3,STATNUM                                                       
         GOTO1 =A(FILLBUFF),DMCB,(R2),(R7),(R9),(RC),RR=RELO                    
         A     R2,WSTATS           PREPARE FOR NEXT STATION                     
         LA    R5,5(R5)            NEXT STATION                                 
         LA    R3,1(R3)            INCREMENT STATION NUMBER                     
         L     RE,MKTPNTR                                                       
         BCT   R0,PREP10                                                        
*                                                                               
         L     RE,MKTPNTR                                                       
         ZIC   RF,1(RE)            NEXT ELEMENT                                 
         AR    RE,RF                                                            
         ST    RE,MKTPNTR                                                       
         B     PREP4A              PROCESS NEXT ELEMENT                         
*--------------------------------------------------------------------           
MBKERR   MVI   FORCEHED,C'Y'                                                    
         MVC   P(040),=C'****************************************'              
         MVC   P2(L'BADMUL),BADMUL                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PREP4                                                            
         SPACE 1                                                                
         B     XIT                                                              
*****    DROP  R3                                                               
         EJECT                                                                  
CHKFORM  NTR1                                                                   
         MVI   WSTATS+3,5          DEFAULT, WIDTH OF 5                          
         CLI   FORMOPT,C'Y'                                                     
         BNE   XIT                                                              
         L     R4,AMYD             MAKE SURE WE GET MKTPNTR                     
         USING MYD,R4                                                           
         L     RE,MKTPNTR                                                       
CKFORM1  CLI   0(RE),0                                                          
         BE    XIT                                                              
         CLI   0(RE),1             NEW MARKET?                                  
         BNE   CKFORM10                                                         
         CLC   MKTNUM,2(RE)                                                     
         BNE   XIT                 NEW MARKET, LEAVE                            
         B     CKFORM90            NEXT ONE                                     
CKFORM10 CLI   0(RE),4                                                          
         BNE   CKFORM90                                                         
         MVI   WSTATS+3,9                                                       
         B     XIT                                                              
CKFORM90 ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     CKFORM1                                                          
         EJECT                                                                  
TOTDIV   NTR1                                                                   
         LR    R4,R3                                                            
         LA    R2,MKTBUFF                                                       
         LR    R3,R2               SAVE FOR LATER                               
         ZIC   R5,NDPTS                                                         
TOTDIV0  LR    R2,R3               GET PRESENT TOTAL                            
         ZIC   R0,NDEMOS                                                        
         L     R1,0(R2)                                                         
         C     R1,=F'2'                                                         
         BL    TOTDIV2             0 OR 1 SKIP DIVISION                         
         LA    R2,4(R2)                                                         
TOTDIV1  SR    RE,RE                                                            
         L     RF,0(R2)                                                         
         M     RE,=F'10'                                                        
         DR    RE,R1                                                            
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         ST    RF,0(R2)                                                         
         LA    R2,4(R2)                                                         
         BCT   R0,TOTDIV1                                                       
TOTDIV2  LA    R3,20(R3)                                                        
         BCT   R5,TOTDIV0                                                       
         B     XIT                                                              
         EJECT                                                                  
SEEDCMB  NTR1                                                                   
         ST    RF,FULL                                                          
         LA    R1,COMBLIST                                                      
         LR    RE,R1                                                            
         LA    RF,CUMESW-COMBLIST                                               
         XCEF                                                                   
         L     RF,FULL                                                          
SEEDCMB2 MVC   0(9,R1),2(RF)                                                    
         ZIC   RE,1(RF)                                                         
         SH    RE,=H'12'                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   9(0,R1),11(RF)                                                   
         LA    R1,LNCOMB(R1)                                                    
         ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         ST    RF,MKTPNTR                                                       
*        ZIC   RE,NCMBS            INCLUDE THE COMBOS IN THE REPORT             
*        LA    RE,1(RE)                                                         
*        STC   RE,NCMBS                                                         
*        ZIC   RE,NSTATS           INCLUDE THE COMBOS IN THE REPORT             
*        LA    RE,1(RE)                                                         
*        STC   RE,NSTATS                                                        
         CLI   0(RF),3                                                          
         BE    SEEDCMB2                                                         
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------------             
FILLCMB  NTR1  WORK=(R7,50)                                                     
         GOTOR SUBR08,DMCB,('SETUNIVE',(RC))                                    
*                                                                               
         LA    R1,4                                                             
         LA    RE,DEMOS                                                         
         LA    RF,COPDEMOS                                                      
FILLCMB0 MVC   0(1,RF),1(RE)                                                    
         CLI   1(RE),C'X'          CONVERT TSL TO CUME                          
         BNE   *+8                                                              
         MVI   1(RE),C'C'                                                       
         LA    RE,3(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,FILLCMB0                                                      
*                                                                               
         ST    R7,ATHISB2                                                       
         LR    RE,R7                                                            
         LA    RF,15*L'THISREC                                                  
         XCEF                                                                   
         L     R4,AMYD                                                          
         USING MYD,R4                                                           
         LA    R7,COMBLIST                                                      
         LA    R1,0                                                             
FILLCMB1 CLI   0(R7),0             END - JUST GET OUT                           
         BE    XIT                                                              
         MVI   CNOSTA,0                                                         
         LA    RE,CUMERAT                                                       
         LA    RF,15*L'THISREC                                                  
         XCEF                                                                   
         LA    RF,CUMERAT          FILL CUME RTGS WITH 1'S                      
         LA    RE,90               15 DAYPARTS/4 IN EACH DAYPART                
FILLCMB2 MVI   3(RF),1                                                          
         LA    RF,4(RF)                                                         
         BCT   RE,FILLCMB2                                                      
*                                                                               
         L     RE,=A(TEMPBUF)                                                   
         A     RE,RELO                                                          
         LA    RF,15*L'THISREC                                                  
         XCEF                                                                   
*                                                                               
         MVI   NEWSTAT,C'Y'                                                     
         L     RF,WSTATS           PUT COMBO IN REG STATION LIST                
         LR    RE,R3                                                            
         BCTR  RE,0                                                             
         MR    RE,RE                                                            
         A     RF,ASTATS                                                        
         MVI   4(RF),0                                                          
         MVC   0(4,RF),=C'CMB '    SET DUMMY IN STATION LIST                    
         STC   R1,3(RF)            FOR COMBOS                                   
         OI    3(RF),X'F0'                                                      
         LA    R1,1(R1)                                                         
         STC   R1,CMBCNTR          SAVE FOR LATER                               
         LA    R2,9(R7)            BUMP PAST TITLE                              
         SPACE 1                                                                
         SR    RE,RE               FIND NUMBER OF STATS. FOR WEIGHT             
         LA    RE,1(RE)            MUST BE ONE                                  
         LA    R2,5(R2)            NEXT ENTRY                                   
         CLI   0(R2),0             NOT THE END OF LIST                          
         BNE   *-12                                                             
*                                                                               
         LA    R2,9(R7)            RESET POINTER                                
         STH   RE,HALF             AND SAVE THE WEIGHT                          
         GOTO1 XSORT,DMCB,(R2),(RE),5,5,0                                       
*                                                                               
FILLCMB3 STC   R3,STATNUM                                                       
         GOTO1 =A(FILLBUFF),DMCB,(R2),(R7),(R9),(RC),RR=RELO                    
         L     R1,ATHISB2                                                       
         BAS   RE,AVECOMPB         COMP AVG FOR THIS STATION AND SUM            
         MVI   NEWSTAT,C'Y'        FORCE A CLEAR                                
*                                                                               
         ZIC   R1,CNOSTA           INCREMENT NUMBER OF                          
         LA    R1,1(R1)            STATIONS IN COMBO                            
         STC   R1,CNOSTA                                                        
         CLI   CNOSTA,2                                                         
         BL    FILLCMB4                                                         
         GOTO1 =A(NEWCUMES),DMCB,(R9),(RC),RR=RELO                              
*                                                                               
FILLCMB4 DS    0H                  GET ALL IMPRESSIONS                          
         BAS   RE,REPLDEMS         REPLACE DEMOS FOR IMPRESSIONS                
         GOTO1 =A(FILLBUFF),DMCB,(R2),(R7),(R9),(RC),RR=RELO                    
         L     R1,=A(TEMPBUF)                                                   
         A     R1,RELO                                                          
         BAS   RE,AVECOMPB                                                      
         BAS   RE,RESTDEMS         RESTORE DEMOS                                
*                                                                               
FILLCMB7 LA    R2,5(R2)            GET NEXT STATION                             
         CLI   0(R2),0                                                          
         BNE   FILLCMB3                                                         
*                                                                               
         ZIC   RE,NSTATS           INCLUDE THE COMBOS IN THE REPORT             
         LA    RE,1(RE)                                                         
         STC   RE,NSTATS                                                        
         ZIC   RE,MAXRANK          INCLUDE THE COMBOS IN THE REPORT             
         LA    RE,1(RE)                                                         
         STC   RE,MAXRANK                                                       
         CLI   MAXRANK,40                                                       
         BNH   *+8                                                              
         MVI   MAXRANK,40                                                       
*------------------------------------------------------------------             
* BEFORE WE SAVE THE CURRENT BUFFER                                             
* CHECK IF WE REQUESTED TSL, IF WE DID                                          
* PERFORM THE CALCULATION   TSL=(IMP * NO. QTR HOURS)/CUME                      
*------------------------------------------------------------------             
         BAS   RE,TSLCALC                                                       
         L     RE,ATHISB2                                                       
         MVC   THISBUFF(250),0(RE)                                              
         MVC   THISBUFF+250(15*L'THISREC-250),250(RE)                           
         GOTO1 =A(RANKBUFF),DMCB,(R9),(RC),RR=RELO                              
         L     RE,ATHISB2                                                       
         LA    RF,15*L'THISREC                                                  
         XCEF                                                                   
         LA    R3,1(R3)            SET TO NEXT SLOT                             
         ZIC   R1,CMBCNTR                                                       
         LA    R7,LNCOMB(R7)       AND NEXT COMBO                               
         B     FILLCMB1                                                         
         DROP  R4                                                               
         EJECT                                                                  
*------------------------------------------------------------------             
TSLCALC  NTR1                                                                   
         L     R7,AMYD                                                          
         L     RE,ATHISB2                                                       
         L     RF,=A(TEMPBUF)                                                   
         A     RF,RELO                                                          
*                                                                               
         ZIC   R1,NDPTS                                                         
TSLCLC02 ZIC   R2,NDEMOS                                                        
         LA    R3,COPDEMOS                                                      
         STM   RE,RF,DUB                                                        
         BAS   RE,QTRHRSCT         COUNT # OF QTR HOURS IN DAYPART              
         LM    RE,RF,DUB                                                        
         LA    RE,4(RE)            FIRST ONE IS NOT DEMO                        
         LA    RF,4(RF)                                                         
TSLCLC03 CLI   0(R3),C'X'          TSL?                                         
         BNE   TSLCLC05                                                         
         SR    R4,R4                                                            
         L     R5,0(RF)            GET IMPRESSIONS                              
         M     R4,=F'100'                                                       
         M     R4,NOQTRHRS         MULT BY # OF QTR HOURS                       
         OC    0(4,RE),0(RE)       CAN'T DIVIDE BY ZERO                         
         BZ    TSLCLC05            KEEP IT AS ZERO                              
         D     R4,0(RE)            DIVIDE BY CUME                               
         A     R5,=F'5'                                                         
         SR    R4,R4                                                            
         D     R4,=F'10'                                                        
         ST    R5,0(RE)            REPLACE IT WITH TSL                          
TSLCLC05 LA    R3,1(R3)            NEXT DEMO                                    
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R2,TSLCLC03                                                      
*                                                                               
         LM    RE,RF,DUB                                                        
         LA    RE,L'THISREC(RE)                                                 
         LA    RF,L'THISREC(RF)                                                 
         LA    R7,L'STANDENT(R7)                                                
         BCT   R1,TSLCLC02                                                      
         B     XIT                                                              
*                                                                               
NOQTRHRS DC    F'80'                                                            
*------------------------------------------------------------------             
QTRHRSCT NTR1                                                                   
         LR    R4,R7                                                            
         XC    NOQTRHRS,NOQTRHRS                                                
         CLI   0(R4),C'S'          STANDARD DAYPART                             
         BNE   XIT                 CUSTOM DAYPART, TSL = 0                      
         L     RF,ADDPHS60                                                      
         USING PHSE60D,RF                                                       
         CLI   DBSELSRC,C'R'       RADAR STANDARD DAYPART                       
         BNE   *+12                                                             
         L     RE,ARSTNDPT         LOAD STANDARD DPT TABLE                      
         B     *+8                                                              
         L     RE,ASTNDPTS         LOAD STANDARD DPT TABLE                      
         S     RE,APHASE60                                                      
         AR    RE,RF                                                            
         USING STAND,RE                                                         
QHDAYSCH CLI   STANDDTS,X'FF'      END OF TABLE?                                
         BE    XIT                                                              
         CLC   1(1,R4),STANDDTS    CHECKING DAY RANGE                           
         BNE   QHDAYS2                                                          
         CLC   2(1,R4),STANDPRG                                                 
         BE    QHFNDDPT                                                         
QHDAYS2  LA    RE,L'STANDENT(RE)                                                
         B     QHDAYSCH                                                         
QHFNDDPT LR    R4,RE                                                            
*                                                                               
QHCUST1  CLI   0(R4),0             FINISHED?                                    
         BZ    XIT                                                              
         ZIC   R1,2(R4)            END HOUR                                     
         ZIC   R2,1(R4)            START HOUR                                   
         SR    R1,R2                                                            
         MH    R1,=H'4'            4 QTR HOURS PER HOUR                         
         SR    RF,RF                                                            
         ZIC   R2,0(R4)                                                         
QHCUST2  LTR   R2,R2                                                            
         BZ    QHCUST9                                                          
         BP    *+8                                                              
         LA    RF,1(RF)                                                         
         SLL   R2,1                                                             
         B     QHCUST2                                                          
*                                                                               
QHCUST9  SR    R0,R0                                                            
         MR    R0,RF                                                            
         A     R1,NOQTRHRS                                                      
         ST    R1,NOQTRHRS                                                      
         LA    R4,3(R4)                                                         
         B     QHCUST1                                                          
*                                                                               
         B     XIT                                                              
         DROP  RE                                                               
         DROP  RF                                                               
*------------------------------------------------------------------             
REPLDEMS NTR1                                                                   
         ZIC   R1,NDEMOS                                                        
         LA    R2,DEMOS                                                         
         LA    R3,TSLDEMOS                                                      
REPLD1   MVC   0(1,R3),1(R2)       SAVE DEMO CATEGORY                           
         MVI   1(R2),C'I'          REPLACE IT WITH IMPRESSIONS                  
         LA    R2,3(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R1,REPLD1                                                        
         B     XIT                                                              
*------------------------------------------------------------------             
RESTDEMS NTR1                                                                   
         ZIC   R1,NDEMOS                                                        
         LA    R2,DEMOS                                                         
         LA    R3,TSLDEMOS                                                      
RESTD1   MVC   1(1,R2),0(R3)       SAVE DEMO CATEGORY                           
         LA    R2,3(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R1,RESTD1                                                        
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------                 
AVECOMPB NTR1                                                                   
         L     R2,ATHISBUF                                                      
         USING THISD,R2                                                         
         LR    R3,R1               ACCUMULATE AREA FOR COMBOS                   
         ZIC   R5,NDPTS                                                         
         LA    R4,CUMERAT                                                       
AVECMPB1 STM   R2,R4,DMCB          SAVE THE POINTERS                            
         L     RF,THISWT           PICK UP WEIGHT FOR DAYPART                   
         LA    RF,0(RF)            KILL HOB                                     
         CLI   0(R2),200                                                        
         BL    *+10                                                             
         XC    THISWT(L'THISREC),THISWT                                         
         LA    R1,1                                                             
         ST    R1,0(R3)                                                         
*                                                                               
         LA    R2,THISDEMS                                                      
         LA    R3,THISDEMS-THISD(R3)                                            
         ZIC   R8,NDEMOS                                                        
         LA    R7,DEMOS                                                         
         MVI   CUMEFLG,C'N'                                                     
*                                                                               
         BAS   RE,AVECMPB2                                                      
*                                                                               
AVECMPB6 LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         LA    R7,3(R7)            NEXT DEMO                                    
         BCT   R8,AVECMPB2                                                      
         LM    R2,R4,DMCB          RESET TO DAYPART START                       
         LA    R2,L'THISREC(R2)    THEN ON TO NEXT DAYPART                      
         LA    R3,L'THISREC(R3)                                                 
         LA    R4,16(R4)           NEXT CUME RATING                             
         BCT   R5,AVECMPB1                                                      
         B     XIT                                                              
CNOSTA   DC    X'0'                                                             
CUMEFLG  DC    C'N'                                                             
TSLDEMOS DC    CL4' '                                                           
COPDEMOS DC    CL4' '                                                           
         EJECT                                                                  
AVECMPB2 NTR1                                                                   
         SR    R1,R1                                                            
         LTR   RF,RF                                                            
         BZ    AVECMPB4                                                         
         L     R1,0(R2)            AVERAGE                                      
         SR    R0,R0                                                            
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
*                                                                               
AVECMPB4 DS    0H                                                               
         LR    RF,R1                                                            
         A     R1,0(R3)            SUM UP THE COMPONENTS                        
         ST    R1,0(R3)                                                         
*                                                                               
         CLI   1(R7),C'C'          CUME?                                        
         BNE   XIT                                                              
*                                                                               
         LR    R1,RF               CALCULATE CUME RTG FOR                       
         SR    R0,R0               CURRENT STATION                              
         M     R0,=F'10000'                                                     
         BAS   RE,FINDUNIV         FINDS RIGHT UNIV AND DIVIDES                 
*                                                                               
         AH    R1,=H'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         SR    R0,R0                                                            
         M     R0,0(R4)            SAVE IT WITH OTHER CUME RTGS                 
         ST    R1,0(R4)                                                         
         MVI   CUMEFLG,C'Y'                                                     
         B     XIT                                                              
*------------------------------------------------------------------             
FINDUNIV NTR1                                                                   
         LA    RF,RUNIVS                                                        
         CLI   DEMSTYLE,C'C'       BY CATEGORIES                                
         BNE   FUNIVD              NO, BY DEMOS                                 
*---------------------------------------------------------------------          
* IF BY CATEGORIES, THE UNIVERSES ARE ORGANIZED 1.MSA, 2.TSA, 3.ADI             
*---------------------------------------------------------------------          
         CLI   0(R7),2             TSA?                                         
         BL    FUNIVXIT            LESS THAN, MSA                               
         BH    FUNIVC2             GREATER THAN, ADI                            
         LA    RF,RUNIVS+4                                                      
         B     FUNIVXIT                                                         
FUNIVC2  LA    RF,RUNIVS+8                                                      
         B     FUNIVXIT                                                         
*---------------------------------------------------------------------          
* IF BY DEMOS, THE UNIVERSES ARE ORGANIZED WITH THE DEMOS                       
* USE THE NDEMOS COUNTER R8 AS AN INDEX TO UNIVERSE                             
*---------------------------------------------------------------------          
FUNIVD   ZIC   R2,NDEMOS           # OF DEMOS USED                              
         SR    R2,R8               SUBTRACTING CURRENT COUNTER                  
         LTR   R2,R2               GIVES INDEX, 0 IS FIRST                      
         BZ    FUNIVXIT                                                         
FUNIVD2  LA    RF,4(RF)                                                         
         BCT   R2,FUNIVD2                                                       
FUNIVXIT L     R8,0(RF)                                                         
         DR    R0,R8                                                            
         XIT1  REGS=(R1)                                                        
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO CONTROL PRINTING OF BUFFER                            
         SPACE 3                                                                
*              INPUTS              NDPTS   DPTLIST                              
*                                  NDEMOS  DEMOS                                
*                                  UP      DPTGAP   DPTWIDTH DISP               
*              OUTPUT              PRINT LINES                                  
         SPACE 1                                                                
PRNTBUFF NTR1                                                                   
         L     RE,AMYD                                                          
         USING MYD,RE                                                           
         LA    RF,MKTBUFF                                                       
         ST    RF,TOTDENOM                                                      
         ST    RF,TMPDENOM                                                      
         DROP  RE                                                               
*                                                                               
         MVC   MAXRANK,SVMAXRNK                                                 
*                                                                               
PB0      LA    R2,BOOKS                                                         
         ZIC   R0,NBOOKS                                                        
         XC    UNIVBK,UNIVBK                                                    
PB1      MVC   DBSELBK,1(R2)                                                    
         MVC   DBBTYPE,3(R2)                                                    
         CLI   DBSELBK,87                                                       
         BL    *+10                                                             
         MVC   DBSELMK,MKTNUM                                                   
         CLI   DBSELSRC,C'M'                                                    
         BE    PB1A                                                             
         GOTOR SUBR08,DMCB,('SETUNIVE',(RC))                                    
*                                                                               
PB1A     CLI   LONGOPT,C'Y'                                                     
         BNE   PC0                                                              
         LA    RF,103              MAX OF 86 LINES                              
         B     PC0A                                                             
PC0      LA    RF,60                                                            
PC0A     STC   RF,MAXLINES                                                      
         CLI   CALCOPT,C'Y'                                                     
         BNE   PC1                                                              
         GOTOR SUBR08,DMCB,('SETNEWUE',(RC))                                    
PC1      BAS   RE,COMPRANK         WORK OUT RANK NUMBERS IF NEEDED              
         GOTO1 =A(COMPCUSH),DMCB,(R9),(RC),RR=RELO                              
         MVI   FIRSTDP,C'Y'                                                     
         XC    DPTSLEFT,DPTSLEFT                                                
         MVC   DPTSLEFT+3(1),NDPTS                                              
         MVC   ADPTBEST,ABEST                                                   
         L     R2,AMYD                                                          
         USING MYD,R2                                                           
*                                                                               
         MVI   BYTE,C'N'           SET FOR NO FOOTS                             
         CLI   COMBLIST,0                                                       
         BE    *+8                                                              
         MVI   BYTE,C'Y'           NEED IF THERE ARE COMBOS                     
*                                                                               
         LA    R2,DPTLIST                                                       
         USING DPENTRY,R2                                                       
         MVI   DPTNUM,1                                                         
         SPACE 1                                                                
PC2      L     R0,UP               NUMBER OF DAYPARTS WE CAN FIT                
         C     R0,DPTSLEFT         MAY BE MORE THAN WHAT WE HAVE LEFT           
         BNH   *+8                                                              
         L     R0,DPTSLEFT                                                      
         SPACE 1                                                                
         L     RF,DPTSLEFT         UPDATE NUMBER LEFT FOR NEXT TIME             
         SR    RF,R0                                                            
         ST    RF,DPTSLEFT                                                      
         SPACE 1                                                                
         CLI   FIRSTDP,C'Y'        FIRST TIME                                   
         BNE   PC4                                                              
         MVI   FIRSTDP,C'N'                                                     
         MVI   FORCEHED,C'Y'                                                    
         L     R4,ABOX             NEED TO GET BOXES WARMED UP                  
         LTR   R4,R4                                                            
         BZ    PC3                                                              
         USING BOXD,R4                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,SPACES                                                   
         SPACE 1                                                                
PC3      GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PC6                                                              
         SPACE 1                                                                
PC4      ZIC   R1,LINE             DO WE HAVE ENOUGH SPACE FOR THIS SET         
         ZIC   RF,MAXRANK          NEED LINE FOR EACH RANK                      
         CLC   MAXRANK,NSTATS      SET FOR REAL MAX                             
         BL    *+8                                                              
         IC    RF,NSTATS                                                        
         LA    R1,5(R1,RF)         +5                                           
         CLI   BYTE,C'Y'           ADJUST IF FOOTLINES                          
         BNE   *+8                                                              
         LA    R1,2(R1)                                                         
         CLI   STACTSW,1                                                        
         BNE   *+8                                                              
         LA    R1,2(R1)                                                         
         ZIC   RF,MAXLINES                                                      
         CR    R1,RF                                                            
         BL    PC5A                                                             
         GOTO1 =A(FOOT),DMCB,(R8),(R9),(RC),RR=RELO                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   FORCEHED,C'Y'                                                    
PC5A     GOTO1 SPOOL,DMCB,(R8)     SPACE BETWEEN EACH SET                       
         SPACE 1                                                                
PC6      GOTO1 =A(SETBOX),DMCB,(R8),(R9),(RC),RR=RELO                           
         GOTO1 =A(DPTITLES),DMCB,(R2),(R8),(R9),(RC),RR=RELO                    
         L     R3,ADPTBEST         SET UP TO PRINT RANKED LINE                  
         MVC   MAXRANK,SVMAXRNK    RESTORE ORIGINAL MAXRANK                     
         ZIC   R4,MAXRANK                                                       
         CLC   MAXRANK,NSTATS      SET FOR REAL MAX                             
         BL    PC7                                                              
         IC    R4,NSTATS           DON'T SHOW MARKET STATIONS                   
         B     PC7A                                                             
PC7      CLI   MAXRANK,40                                                       
         BNE   PC7A                                                             
         LA    R4,40                                                            
PC7A     DS    0H                                                               
         MVI   STATNUM,1                                                        
         MVC   SAVDPTN,DPTNUM                                                   
*        CLI   DBSELSRC,C'M'       NO MARKET STATION TO SKIP                    
*        BE    PC8                                                              
*        CLI   DEMOS,2             NO MARKET STATION FOR TSA                    
*        BE    PC8                                                              
*        CLI   NOSHARE,C'Y'        NO MARKET STATION FOR TSA                    
*        BE    PC8                                                              
*        LA    R3,L'BUFFREC(R3)                                                 
*        MVI   STATNUM,2           THIS IS TO GET RANK #S RIGHT                 
         SPACE 1                                                                
PC8      MVC   DPTNUM,SAVDPTN                                                   
         GOTO1 =A(FORMLINE),DMCB,(R8),(R9),(RC),RR=RELO                         
         MVC   TMPDENOM,TOTDENOM   RESTORE TOTAL DENOMINATOR                    
         LA    R3,L'BUFFREC(R3)                                                 
         AI    STATNUM,1                                                        
         BCT   R4,PC8                                                           
PC9      GOTO1 SPOOL,DMCB,(R8)     ONE MORE LINE FOR BOX BOTTOM                 
         SPACE 1                                                                
         OC    DPTSLEFT,DPTSLEFT   ANY MORE DAYPARTS TO DO?                     
         BZ    XIT                                                              
         LA    R1,L'DPENTRY        MOVE ON TO NEXT SET OF DAYPARTS              
         MH    R1,UP+2             (WE'VE JUST DONE 'UP' DAYPARTS)              
         AR    R2,R1                                                            
*                                                                               
         L     R3,TOTDENOM                                                      
         LA    R1,20                                                            
         MH    R1,UP+2                                                          
         AR    R3,R1                                                            
         ST    R3,TOTDENOM                                                      
         ST    R3,TMPDENOM                                                      
*                                                                               
         L     R3,ADPTBEST                                                      
         LA    R1,L'BUFFREC*40                                                  
         MH    R1,UP+2                                                          
         AR    R3,R1                                                            
         ST    R3,ADPTBEST                                                      
         B     PC2                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              COMPUTE RANK NUMBERS IF REQUESTED                                
         SPACE 3                                                                
*              OUTPUTS             RANKNUMS FOR EACH DAYPART                    
         SPACE 1                                                                
COMPRANK NTR1                                                                   
         CLI   RANKOPT,C'Y'                                                     
         BNE   XIT                                                              
         L     R2,AMYD                                                          
         USING MYD,R2                                                           
         LA    R2,RANKNUMS                                                      
         DROP  R2                                                               
         L     R3,ABEST                                                         
         ZIC   R0,NDPTS                                                         
         LA    R5,1                R5 HAS TABLE NUMER                           
*        CLI   DBSELSRC,C'M'       NO MARKET STATION TO SKIP                    
*        BE    COMPR2                                                           
*        CLI   DEMOS,2             NO MARKET STATION FOR TSA                    
*        BE    COMPR2                                                           
*        CLI   NOSHARE,C'Y'        NO MARKET STATION FOR TSA                    
*        BE    COMPR2                                                           
*        SR    R5,R5               MARKET STATION IS RANKED 0                   
         SPACE 1                                                                
COMPR2   BAS   RE,COMPR4           COMPUTE FOR EACH DAYPART                     
         LA    R2,40(R2)                                                        
         LA    R3,L'BUFFREC*40(R3)                                              
         BCT   R0,COMPR2                                                        
         B     XIT                                                              
         SPACE 1                                                                
COMPR4   NTR1                                                                   
         XC    0(40,R2),0(R2)      CLEAR RANK NUMBERS FOR THIS DPT              
         USING BUFFD,R3                                                         
         LA    R4,1                R4 WILL CONTAIN RANK NUMBER                  
         XC    LASTVAL,LASTVAL                                                  
         ZIC   R0,MAXRANK                                                       
         SPACE 1                                                                
COMPR6   CLC   LASTVAL,BUFFDEMS                                                 
         BE    COMPR8                                                           
         MVC   LASTVAL,BUFFDEMS                                                 
         LR    R4,R5               THIS NOT EQUAL TO LAST SO RESET RANK         
         STC   R4,0(R2)                                                         
         CLC   BUFFDEMS(LENDEM),BUFFDEMS+L'BUFFREC                              
         BNE   COMPR10                                                          
         OI    0(R2),X'80'         THIS EQUALS NEXT SO MARK X'80' (=)           
         B     COMPR10                                                          
         SPACE 1                                                                
COMPR8   STC   R4,0(R2)            THIS EQUAL TO LAST                           
         OI    0(R2),X'80'         SO MARK EQUAL                                
         SPACE 1                                                                
COMPR10  LA    R2,1(R2)            ON TO NEXT STATION                           
         LA    R3,L'BUFFREC(R3)                                                 
         LA    R5,1(R5)                                                         
         BCT   R0,COMPR6                                                        
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              HEADLINE HOOK                                                    
         SPACE 3                                                                
HOOK     NTR1                                                                   
         L     R4,AMYD                                                          
         USING MYD,R4                                                           
         MVC   BLOCK(80),SPACES    DEMOS AND CATEGORIES                         
         CLI   SEPAOPT,C'D'                                                     
         BNE   HOOKAX                                                           
         LA    R2,DEMOS                                                         
         GOTO1 DEMOCON,DMCB,(R2),(5,WORK),DBLOCK,0                              
         MVC   BLOCK(5),WORK                                                    
         B     HOOKAY                                                           
HOOKAX   MVC   BLOCK(40),RNKDEMO                                                
         CLI   SEPAOPT,C'C'                                                     
         BNE   HOOKAY                                                           
         ZIC   R2,PRPNDEMS                                                      
         ZIC   R3,NCATS                                                         
         BCTR  R3,0                                                             
         SR    R3,R2                                                            
         MH    R3,=H'5'                                                         
         LA    R2,CATTITS                                                       
         AR    R2,R3                                                            
         MVC   BLOCK+40(5),0(R2)                                                
         B     HOOKAZ                                                           
HOOKAY   MVC   BLOCK+40(40),RNKCATS                                             
HOOKAZ   GOTO1 VSQUASH,DMCB,BLOCK,80                                            
         MVC   H4(44),BLOCK                                                     
         CLI   HOUROPT,C'Y'                                                     
         BNE   *+10                                                             
         MVC   H3(35),=C'*****BASED ON HOURLY DATA ONLY*****'                   
         SPACE 1                                                                
         MVC   BLOCK(42),SPACES    MARKET NUMBER AND NAME                       
         EDIT  (2,MKTNUM),(4,BLOCK)                                             
         MVC   BLOCK+5(30),RNKMKTN                                              
         CLI   STALSTD,C'Y'                                                     
         BNE   HOOKAZ1                                                          
         MVC   BLOCK+36(6),=C'*LIST*'                                           
         OI    RNKMKTNH+6,X'88'                                                 
HOOKAZ1  GOTO1 VSQUASH,DMCB,BLOCK,42                                            
         MVC   H5+7(42),BLOCK                                                   
         SPACE 1                                                                
         MVC   BLOCK(60),SPACES    SOURCE AND BOOK                              
         MVC   BLOCK(8),RNKSRCE                                                 
         LA    R1,BLOCK+10                                                      
         ZIC   R0,NBOOKS                                                        
         OR    R0,R0               IF THERE ARE NO BOOKS -SKIP LOOP             
         BZ    HOOKAZ2                                                          
         LA    R2,BOOKS+1                                                       
         BAS   RE,CONVBOOK                                                      
         MVC   0(9,R1),WORK                                                     
         LA    R1,10(R1)                                                        
         LA    R2,4(R2)                                                         
         BCT   R0,*-18                                                          
*        MVC   BLOCK+10(40),RNKBOOK                                             
HOOKAZ2  GOTO1 VSQUASH,DMCB,BLOCK,60                                            
         MVC   H6(44),BLOCK                                                     
         L     RE,DBEXTEND         CHECK FOR CONDENSED MARKETS/DEMOS            
         USING DBEXTRAD,RE                                                      
HOOK0    LTR   RE,RE                                                            
         BZ    HOOK1                                                            
         CLC   0(4,RE),=C'RADI'                                                 
         BE    *+12                                                             
         L     RE,4(RE)                                                         
         B     HOOK0                                                            
         CLI   DBRADEMT,C'R'                                                    
         BNE   HOOK1                                                            
         L     RF,=A(CNDHDR)                                                    
         A     RF,RELO                                                          
         MVC   H8+12(L'CNDHDR),0(RF)                                            
         L     RF,=A(CNDHDR2)                                                   
         A     RF,RELO                                                          
         MVC   H9+12(L'CNDHDR2),0(RF)                                           
HOOK1    LA    R2,H1+30            TITLE                                        
         CLI   ADDROPT,C'N'                                                     
         BNE   HOOK1B                                                           
         CLI   RCSUBPRG,1                                                       
         BE    HOOK1A                                                           
         MVC   H2+99(32),SPACES                                                 
         B     HOOK1B                                                           
HOOK1A   MVC   H2+75(32),SPACES                                                 
HOOK1B   CLI   RCSUBPRG,1                                                       
         BE    *+8                                                              
         LA    R2,11(R2)                                                        
         MVC   0(40,R2),RESTITLE   ALREADY CENTERED                             
         GOTO1 UNDERLIN,DMCB,(40,(R2)),(X'BF',132(R2))                          
         SPACE 1                                                                
         CLI   RNKSRCE,C'A'                                                     
         BNE   HOOK1C                                                           
         MVC   H3+27(55),=C'AUDIENCE ESTIMATES: COPYRIGHT XXXX THE ARBIX        
               TRON COMPANY'                                                    
         GOTO1 DATCON,DMCB,(3,BTODAY),(21,WORK)                                 
         MVC   H3+31+26(4),WORK+6                                               
HOOK1C   LA    R2,H4+44            UNIVERSES FOR MULTIPLE CATS                  
         CLI   RCSUBPRG,1                                                       
         BE    *+8                                                              
         LA    R2,11(R2)                                                        
         CLI   DEMSTYLE,C'D'                                                    
         BE    HOOK2                                                            
         CLI   ANYMSA,C'Y'             MSA                                      
         BNE   *+18                                                             
         MVC   0(10,R2),MSASAVE                                                 
         BAS   RE,IUNV                                                          
         LA    R2,132(R2)                                                       
         CLI   ANYADI,C'Y'             ADI                                      
         BNE   *+18                                                             
         MVC   0(10,R2),ADISAVE                                                 
         BAS   RE,IUNV                                                          
         LA    R2,132(R2)                                                       
         CLI   ANYUSA,C'Y'             USA     8/7/00                           
         BNE   *+18                                                             
         MVC   0(10,R2),USASAVE                                                 
         BAS   RE,IUNV                                                          
         LA    R2,132(R2)                                                       
         CLI   ANYTSA,C'Y'             TSA                                      
         BNE   HOOKX                                                            
         MVC   0(10,R2),TSASAVE                                                 
         BAS   RE,IUNV                                                          
         B     HOOKX                                                            
         SPACE 1                                                                
HOOK2    BCTR  R2,0                UNIVERSES FOR MULTIPLE DEMOS                 
         MVC   0(12,R2),DUNBLOCK                                                
         BAS   RE,IUNV                                                          
         LA    R2,132(R2)                                                       
         MVC   0(12,R2),DUNBLOCK+12                                             
         BAS   RE,IUNV                                                          
         LA    R2,132(R2)                                                       
         MVC   0(12,R2),DUNBLOCK+24                                             
         BAS   RE,IUNV                                                          
         LA    R2,132(R2)                                                       
         MVC   0(12,R2),DUNBLOCK+36                                             
         BAS   RE,IUNV                                                          
HOOKX    B     XIT                                                              
         SPACE 2                                                                
IUNV     NTR1                      INSERT POPULATION CAPTION                    
         LA    R0,12                                                            
         CLI   0(R2),C'='          IN FRONT OF '='                              
         BE    *+16                                                             
         LA    R2,1(R2)                                                         
         BCT   R0,*-12                                                          
         B     XIT                                                              
         MVC   DUB,0(R2)                                                        
         MVC   9(8,R2),DUB                                                      
         MVC   0(9,R2),=C' UNIV(00)'                                            
         CLI   DBSELSRC,C'R'       FOR RADAR                                    
         BNE   *+16                                                             
         MVC   0(9,R2),=C' UNV(000)'                                            
         MVC   9(8,R2),DUB         JUST DROP THE LAST                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONVERT BOOK FOR HEADINGS                             
         SPACE 3                                                                
CONVBOOK NTR1                                                                   
*                                                                               
         L     RF,DBCOMFCS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,PPMTAB                                                 
         ICM   RE,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,4(R1)            R0 HAS LENGTH OF TABLE ENTRY                 
         LR    R1,RE               R1=A(TABLE)                                  
         USING PPMTABD,R1                                                       
CNVBK02  CLC   =X'FFFF',0(R1)                                                   
         BE    CNVBK04                                                          
         OC    CITYCODE,CITYCODE                                                
         BZ    CNVBK02A                                                         
         CLC   PPMAMKT,CITYCODE                                                 
         BNE   CNVBK03                                                          
         B     CNVBK02C                                                         
CNVBK02A DS    0C                                                               
         CLC   PPMNMKT,MKTNUM                                                   
         BNE   CNVBK03                                                          
CNVBK02C CLC   0(L'PPMSTRTP,R2),PPMSTRTP   PRELIM OR LATER?                     
         BNL   CNVBK05                                                          
CNVBK03  AR    R1,R0                                                            
         B     CNVBK02                                                          
* NON PPM SECTION                                                               
CNVBK04  CLI   BOOKTYPE,C'B'       BLACK                                        
         BE    CNVEBTY                                                          
         CLI   BOOKTYPE,C'H'       AND HISPANIC - FUNNY BOOKS                   
         BE    CNVEBTY                                                          
         CLI   BOOKTYPE,C'P'                                                    
         BNE   CNVBK20                                                          
* PPM SECTION                                                                   
CNVBK05  MVC   WORK(6),=C'MON/NN'   DEFAULT NO BOOKTYPE                         
         CLI   BOOKTYPE,0                                                       
         BE    CNVBK06                                                          
         MVI   WORK+6,C'('                                                      
         MVC   WORK+7(1),BOOKTYPE                                               
         MVI   WORK+8,C')'                                                      
CNVBK06  LA    R3,WORK                                                          
         L     R1,=A(ARBMON)                                                    
         A     R1,RELO                                                          
*                                                                               
CNVBK08  CLC   1(1,R2),3(R1)                                                    
         BNE   CNVBK10                                                          
         MVC   WORK(3),0(R1)                                                    
         LA    R3,WORK+4                                                        
         B     CONVB2                                                           
CNVBK10  LA    R1,4(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   CNVBK08                                                          
         MVC   WORK(6),=C'??????'                                               
         B     CONVB2                                                           
*                                                                               
CNVBK20  MVC   WORK(9),=C' FALL/NN '                                            
         LA    R3,WORK+6                                                        
         CLI   1(R2),11            NOVEMBER=FALL                                
         BE    CONVB2                                                           
         MVC   WORK(9),=C'WINTER/NN'                                            
         LA    R3,WORK+7                                                        
         CLI   1(R2),1             JAN=WINTER                                   
         BE    CONVB2               FOR BBM                                     
         CLI   1(R2),2             FEBRUARY=WINTER                              
         BE    CONVB2                                                           
         MVC   WORK(6),=C'SPRING'                                               
         CLI   1(R2),5             MAY=SPRING                                   
         BE    CONVB2                                                           
         MVC   WORK(6),=C'SUMMER'                                               
         CLI   1(R2),7                                                          
         BE    CONVB2                                                           
         MVC   WORK(6),=C'??????'                                               
         B     CONVB2                                                           
         SPACE 1                                                                
CNVEBTY  L     R3,E1BOOK           CHECK FOR 1 BOOK ETHNICS                     
CNVEBTY1 CLI   0(R3),X'FF'                                                      
         BE    CNVEBTY3                                                         
         CLC   0(1,R3),BOOKTYPE                                                 
         BNE   CNVEBTY2                                                         
         CLC   1(2,R3),MKTNUM                                                   
         BNE   CNVEBTY2                                                         
         CLC   0(2,R2),3(R3)                                                    
         BL    CNVEBTY2                                                         
         CLC   0(2,R2),5(R3)                                                    
         BH    CNVEBTY2                                                         
         B     CNVEBTY6                                                         
CNVEBTY2 LA    R3,7(R3)                                                         
         B     CNVEBTY1                                                         
*                                                                               
CNVEBTY3 MVC   WORK(9),=C'WINSPR/NN '   BOOK NAMES FOR 2 BOOK MARKETS           
         LA    R3,WORK+7                                                        
         CLI   1(R2),5                                                          
         BE    CONVB2                                                           
         MVC   WORK(9),=C'SUMFAL/NN '                                           
         LA    R3,WORK+7                                                        
         CLI   1(R2),11                                                         
         BE    CONVB2                                                           
         MVC   WORK(6),=C'??????'                                               
         B     CONVB2                                                           
         SPACE 1                                                                
CNVEBTY6 MVC   WORK(7),=C'FWS/NN ' BOOK NAMES FOR 1 BOOK MARKETS                
         LA    R3,WORK+4                                                        
         CLI   1(R2),5                                                          
         BE    CONVB2                                                           
         MVC   WORK(6),=C'??????'                                               
         B     CONVB2                                                           
         SPACE 1                                                                
CONVB2   EDIT  (1,0(R2)),(2,(R3)),WRK=DMCB                                      
         B     XIT                                                              
         EJECT                                                                  
*              COMMON ROUTINES                                                  
         SPACE 3                                                                
NEQXIT   LA    R1,1                                                             
         J     *+6                                                              
EQXIT    SR    R1,R1                                                            
         LTR   R1,R1                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
BADBOOKS L     RE,=A(ERRBOOK)                                                   
         A     RE,RELO                                                          
         MVC   L'ERRBOOK-7(7,RE),=C'STATION'                                    
         B     BADBOOK                                                          
         SPACE 1                                                                
BADBOOKM L     RE,=A(ERRBOOK)                                                   
         A     RE,RELO                                                          
         MVC   L'ERRBOOK-7(7,RE),=C' MARKET'                                    
BADBOOK  MVC   CONHEAD(L'ERRBOOK),0(RE)                                         
         MVC   CONHEAD+L'ERRBOOK+1(4),DBSELSTA                                  
*------------------------------------------------------------------             
MYEND    MVI   ERROR,SUPPLIED      USING MY OWN ERROR MSG                       
         CLC   RMKMKID,=C'MID1'                                                 
         BE    MYEND2                                                           
         MVC   RNKSTATH+5(1),STATLEN                                            
         L     RE,ATIOB                                                         
         USING TIOBD,RE                                                         
         CLI   TIOBAID,0                                                        
         BE    MYEND1                                                           
MYEND1   MVC   RNKSTATH+5(1),STATLEN                                            
MYEND2   MVC   WORK,CONHEAD                                                     
ERREND   GOTO1 VGETERR                                                          
         SPACE 2                                                                
SPERR    MVI   ERROR,SUPPLIED                                                   
         MVC   RNKSTATH+5(1),STATLEN                                            
         MVC   WORK,CONHEAD                                                     
         GOTO1 VCURSERR,PARAS,0                                                 
         B     XIT                                                              
         SPACE 2                                                                
CPERR    MVI   ERROR,SUPPLIED                                                   
         MVC   RNKSTATH+5(1),STATLEN                                            
         MVC   WORK,CONHEAD                                                     
         GOTO1 VCPRSERR,PARAS,0                                                 
         B     XIT                                                              
         SPACE 2                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
         SPACE 2                                                                
PATCH    DS    0H                  PATCH AREA                                   
*        DC    XL32'00'                                                         
         EJECT                                                                  
*              LTORG                                                            
         SPACE 3                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ERRORS SPECS AND CONSTANTS                                       
         SPACE 3                                                                
*                                  MY ERROR MESSAGES                            
         SPACE 1                                                                
HEDSPECS DS    0H                  HEADLINE SPECS                               
         SPACE 1                                                                
         SPROG 1,2                                                              
         SSPEC H1,1,C'DDS RADIO RESEARCH'                                       
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H5,1,C'MARKET'                                                   
         SPACE 1                                                                
         SPROG 1                                                                
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,77,AGYADD                                                     
         SSPEC H4,77,RUN                                                        
         SSPEC H5,77,REPORT                                                     
         SSPEC H5,93,PAGE                                                       
         SPACE 1                                                                
         SPROG 2                                                                
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,99,RUN                                                        
         SSPEC H5,99,REPORT                                                     
         SSPEC H5,115,PAGE                                                      
         DC    X'00'                                                            
*                                  DISPLACEMENT TABLE                           
         SPACE 1                                                                
DISPTAB  DC    AL1(1,1,3,3,4)      EACH ENTRY IS                                
         DC    AL1(1,2,3,3,4)      WIDTH (1-3)                                  
         DC    AL1(1,3,2,6,10)     NUMBER OF DEMO CATEGORIES (1-4)              
         DC    AL1(1,4,2,4,4)      UP                                           
         DC    AL1(2,1,5,1,0)      GAP BETWEEN DAYPARTS                         
         DC    AL1(2,2,5,1,0)      DISPLACEMENT FROM LEFT HAND EDGE             
         DC    AL1(2,3,4,0,1)                                                   
         DC    AL1(2,4,3,5,0)                                                   
         DC    AL1(3,1,6,1,0)                                                   
         DC    AL1(3,2,6,1,0)                                                   
         DC    AL1(3,3,4,4,6)                                                   
         DC    AL1(3,4,4,0,0)                                                   
         DC    X'FF'                                                            
*        SPACE 2                                                                
MSG      DS    0C                                                               
FILTERR  DC    C'** ERROR ** INVALID FILTER'                                    
MKTERR   DC    C'** ERROR ** MARKET NOT FOUND'                                  
MKTERR2  DC    C'** MARKET MISSING OR MISSING M= IN USER LIST'                  
LISTERR  DC    C'** ERROR ** NEED GOOD MARKET NUMBER IN LIST'                   
PFKERRM  DC    C'** PFKEYS FOR ACTION "REPORT" ONLY**'                          
BADDEM   DC    C'** ERROR ** INVALID DEMO EXPRESSION'                           
SOONIT   DC    C'BOOKS X MARKETS > '                                            
SOONITSP DC    C'BKS X MKTS X DEMS > '                                          
SOONITN  DC    C'5 REQUEST SOON OR OVERNITE'                                    
SOONITS  DC    C'40 REQUEST OVERNITE'                                           
*SOONITO DC    C'32 SPLIT THE REQUEST'                                          
         SPACE 1                                                                
*AXERR   DC    C'TOO MANY ITEMS IN LIST - SPLIT REQUEST'                        
BADMUL   DC    C'MARKET IS NOT SWEPT (N) TIMES A YEAR'                          
BADMULP  EQU   BADMUL+21                                                        
ERRBOOK  DC    C'REQUESTED BOOK(S) NOT FOUND- MARKET'                           
CUMERAT  DC    90F'1'                                                           
TEMPBUF  DC    90F'0'                                                           
TOOMNY   DC    C'** ERROR ** LIMITED TO 40 STATIONS'                            
TOOMNYDM DC    C'** ERROR ** TOO MANY DEMOGRAPHICS'                             
TOOMNYCT DC    C'** ERROR ** TOO MANY CATEGORIES'                               
MAXCMBA  DC    C'MAX OF 8 COMBOS FOR ONE REQUEST'                               
MAXCMB   DC    C'COMBO LIMITED TO 7 STATIONS '                                  
VIO1     DC    C'#=VIOLATION: SPECIAL STATION ACTIVITIES. DETAILS: P.13+        
                ARB MKT. REPORT'                                                
VIO2     DC    C'A = STATION CITED  Z = STATION LISTED BELOW THE LINE'          
CNDHDR   DC    C'DUE TO SMALLER SAMPLE SIZES IN CONDENSED MARKETS FOR TX        
               HIS DEMOGRAPHIC'                                                 
CNDHDR2  DC    C'AND/OR DAYPART THE USER SHOULD USE DISCRETION WHEN USIX        
               NG THESE ESTIMATES'                                              
         EJECT                                                                  
PROT     EQU   X'20'                                                            
TRAILB   EQU   X'80'                                                            
LOWCASE  EQU   X'40'                                                            
NUMBR    EQU   X'10'                                                            
INTHI    EQU   X'08'                                                            
INTLO    EQU   X'0C'                                                            
INTNOR   EQU   X'00'                                                            
MODIFY   EQU   X'01'                                                            
         SPACE 1                                                                
T1T      DC    AL1(3,7+4,0,1,4,PROT+INTLO,0)                                    
         DC    CL4'CMB1'                                                        
T1T2     DC    AL1(3,7+4,2,0,4,PROT,0)                                          
         DC    CL4'1234'                                                        
         DC    AL1(3,7+24,0,9,24,PROT,0)                                        
         DC    CL24'MARKET NAME'                                                
T1T3     DC    AL1(3,7+11,1,0,11,PROT,0)                                        
         DC    CL11'COMBO NAME'                                                 
         DC    AL1(3,7,0,15,9,INTNOR,0)                                         
         DC    AL1(3,7+7,1,0,7,PROT,0)                                          
         DC    CL7'STATION'                                                     
         DC    AL1(3,7,0,10,7,INTNOR,0)                                         
         DC    AL1(3,7,0,20,7,INTNOR,0)                                         
         DC    AL1(3,7,0,30,7,INTNOR,0)                                         
         DC    AL1(3,7,0,40,7,INTNOR,0)                                         
         DC    AL1(3,7,0,50,7,INTNOR,0)                                         
         DC    AL1(3,7,0,60,7,INTNOR,0)                                         
         DC    AL1(3,7,0,70,7,INTNOR,0)                                         
         DC    X'00'                                                            
*              STANDARD DDS DAYPART MENUS ARB                                   
         SPACE 3                                                                
DDSDPLST DS    0H                                                               
DDSDPENT DS    0CL15                                                            
         SPACE 1                                                                
*              MENU NO/ENTRY LENGTH/NUMBER OF DAYPARTS                          
*              UP TO 15 DAY/START-END/(OPTIONAL START-END)                      
         SPACE 1                                                                
         DC    C'1',AL1(228,15)    DAYPART 1                                    
         DC    X'7C',AL1(06,10),12X'00'      M-F 10A-3P                         
         DC    X'7C',AL1(10,15),12X'00'      M-F 10A-3P                         
         DC    X'7C',AL1(15,19),12X'00'      M-F 3-7P                           
         DC    X'7C',AL1(19,24),12X'00'      M-F 7P-MID                         
         DC    X'7C',AL1(06,24),12X'00'      M-F 6-MID                          
         DC    X'02',AL1(06,10),12X'00'      SAT 6-10A                          
         DC    X'02',AL1(10,15),12X'00'      SAT 10A-3P                         
         DC    X'02',AL1(15,19),12X'00'      SAT 3-7P                           
         DC    X'02',AL1(19,24),12X'00'      SAT 7P-MID                         
         DC    X'7C',AL1(06,10)              M-F DRIVE                          
         DC    X'7C',AL1(15,19),09X'00'                                         
         DC    X'01',AL1(06,10),12X'00'      SUN 6-10A                          
         DC    X'01',AL1(10,15),12X'00'      SUN 10A-3P                         
         DC    X'01',AL1(15,19),12X'00'      SUN 3-7P                           
         DC    X'01',AL1(19,24),12X'00'      SUN 7P-MID                         
         DC    X'7F',AL1(06,24),12X'00'      M-S 6-MID                          
         SPACE 1                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
*              STANDARD DDS DAYPART MENUS RADAR                                 
         SPACE 3                                                                
RDSDPLST DS    0H                                                               
RDSDPENT DS    0CL15                                                            
RDSDPM1S DS    00C                                                              
         SPACE 1                                                                
*              MENU NO/ENTRY LENGTH/NUMBER OF DAYPARTS                          
*              UP TO 15 DAY/START-END/(OPTIONAL START-END)                      
         SPACE 1                                                                
         DC    C'1',AL1(RDSDPM1E-RDSDPM1S,15)    DAYPART 1                      
         DC    X'7C',AL1(06,10),12X'00'      M-F 10A-3P                         
         DC    X'7C',AL1(10,15),12X'00'      M-F 10A-3P                         
         DC    X'7C',AL1(15,19),12X'00'      M-F 3-7P                           
         DC    X'7C',AL1(19,24),12X'00'      M-F 7P-MID                         
         DC    X'7C',AL1(06,24),12X'00'      M-F 6-MID                          
         DC    X'02',AL1(06,10),12X'00'      SAT 6-10A                          
         DC    X'02',AL1(10,15),12X'00'      SAT 10A-3P                         
         DC    X'02',AL1(15,19),12X'00'      SAT 3-7P                           
         DC    X'02',AL1(19,24),12X'00'      SAT 7P-MID                         
         DC    X'7C',AL1(06,10)              M-F DRIVE                          
         DC    X'7C',AL1(15,19),09X'00'                                         
         DC    X'01',AL1(06,10),12X'00'      SUN 6-10A                          
         DC    X'01',AL1(10,15),12X'00'      SUN 10A-3P                         
         DC    X'01',AL1(15,19),12X'00'      SUN 3-7P                           
         DC    X'01',AL1(19,24),12X'00'      SUN 7P-MID                         
         DC    X'92',AL1(06,24),12X'00'      MS* 6-MID                          
RDSDPM1E DS   0C                                                                
         SPACE 1                                                                
RDSDPM2S DS    00C                                                              
         SPACE 1                                                                
*              MENU NO/ENTRY LENGTH/NUMBER OF DAYPARTS                          
*              UP TO 15 DAY/START-END/(OPTIONAL START-END)                      
         SPACE 1                                                                
         DC    C'2',AL1(RDSDPM2E-RDSDPM2S,10)    DAYPART 2                      
         DC    X'97',AL1(06,24),12X'00'      M-S 6A-12M                         
         DC    X'97',AL1(06,19),12X'00'      M-S 6A-7P                          
         DC    X'7C',AL1(06,24),12X'00'      M-F 6A-12M                         
         DC    X'7C',AL1(06,19),12X'00'      M-F 6A-7P                          
         DC    X'02',AL1(06,24),12X'00'      SAT 6-12M                          
         DC    X'02',AL1(06,19),12X'00'      SAT 6A-7P                          
         DC    X'01',AL1(06,24),12X'00'      SUN 6A-12M                         
         DC    X'01',AL1(06,19),12X'00'      SUN 6A-12M                         
         DC    X'92',AL1(24,24),12X'00'      MS* 12M-12M                        
         DC    X'92',AL1(06,24),12X'00'      MS* 6A-MID                         
RDSDPM2E DS   0C                                                                
         SPACE 1                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
HDSDPLST DS    0H                  CANADIAN DEFAULT DAYPARTS                    
HDSDPENT DS    0CL15                                                            
         SPACE 1                                                                
*              MENU NO/ENTRY LENGTH/NUMBER OF DAYPARTS                          
*              UP TO 15 DAY/START-END/(OPTIONAL START-END)                      
         SPACE 1                                                                
HDSM1ST  DC    C'1',AL1(HDSM1EN-HDSM1ST,10)    DAYPART 1                        
         DC    X'7C',AL1(01,01),12X'00'      BREAKFAST                          
         DC    X'7C',AL1(02,02),12X'00'      DAY                                
         DC    X'7C',AL1(03,03),12X'00'      DRIVE                              
         DC    X'7C',AL1(04,04),12X'00'      EVENING                            
         DC    X'7C',AL1(19,19),12X'00'      M-F ALL DAYPARTS                   
         DC    X'02',AL1(05,05),12X'00'      SAT                                
         DC    X'01',AL1(06,06),12X'00'      SUN                                
         DC    X'7F',AL1(33,33),12X'00'      ALL DAYPARTS                       
         DC    X'7F',AL1(07,07),12X'00'      REACH                              
         DC    X'7F',AL1(08,08),12X'00'      M-S 5A-1A                          
HDSM1EN  DS    0C                                                               
         SPACE 1                                                                
*                                                                               
         DC    X'FF'                                                            
*MONTHLY TABLE                                                                  
ARBMON   DC    C'JAN',AL1(01)                                                   
         DC    C'FEB',AL1(02)                                                   
         DC    C'MAR',AL1(03)                                                   
         DC    C'APR',AL1(04)                                                   
         DC    C'MAY',AL1(05)                                                   
         DC    C'JUN',AL1(06)                                                   
         DC    C'JUL',AL1(07)                                                   
         DC    C'AUG',AL1(08)                                                   
         DC    C'SEP',AL1(09)                                                   
         DC    C'OCT',AL1(10)                                                   
         DC    C'NOV',AL1(11)                                                   
         DC    C'DEC',AL1(12)                                                   
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
*                                  STANDARD DAYPART TABLE                       
*      + INCLUDE RADSDPL                                                        
*        EJECT                                                                  
*                                  CANADIAN DAYPART TABLE                       
*      + INCLUDE RADSDPLC                                                       
*                                                                               
*                                                                               
*        EJECT                                                                  
         DS    0H                                                               
AFTPARSE NMOD1 0,**AFTPARSE**                                                   
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         MVI   STALIST,C'M'                                                     
         L     RE,AIO2             CLEAR A BIG AREA                             
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
* COMBINE MARKET AND USER LISTS                                                 
*                                                                               
         LA    R2,RNKMKTH          COMBINE MARKET                               
         ST    R2,LTABLE                                                        
         MVI   LTABLE,X'1'         1 ENTRY                                      
         LA    R2,RNKUL1H          AND INCLUDE USER LISTS                       
         ST    R2,LTABLE+4                                                      
         MVI   LTABLE+4,X'3'       3 ENTRIES                                    
         LA    R2,OUTAREA1                                                      
         CLI   RNKMKTH+5,0         CHECK IF EMPTY                               
         BZ    HHHH                                                             
         SPACE 1                                                                
         USING ELEMDS,R2                                                        
         SPACE 1                                                                
* NOT EMPTY, PUT IN M=                                                          
         SPACE 1                                                                
         MVI   RNKMKTH+4,X'C0'                                                  
         MVC   ELEMSTOP(ELEMBDAT+1),=X'7E0102E400D4'                            
         LA    R2,ELEMBDAT+1(R2)                                                
HHHH     GOTO1 VMINIPAR,DMCB,LTABLE,SCTABL,(X'FF',(R2))                         
*                                                                               
         LA    R2,OUTAREA1         MUST HAVE A MARKET FIRST                     
         CLI   ELEMSTOP,C'='                                                    
         BNE   AFTERROR                                                         
         CLI   ELEMDATA,C'M'                                                    
         BNE   AFTERROR                                                         
*                                                                               
*                                                                               
* COMBINE STATIONS                                                              
*                                                                               
         XC    LTABLEDS,LTABLEDS                                                
         LA    R2,RNKSTATH         COMBINE MARKET                               
         ST    R2,LTABLE                                                        
         MVI   LTABLE,X'0C'        12 ENTRIES                                   
         LA    R2,OUTAREA2                                                      
         XC    STATLEN,STATLEN                                                  
         CLI   RNKSTATH+5,0                                                     
         BZ    AFTPARX2                                                         
         SPACE 1                                                                
* NOT EMPTY, PUT IN S=                                                          
         SPACE 1                                                                
         MVC   STATLEN,RNKSTATH+5                                               
         MVI   RNKSTATH+4,X'C0'                                                 
         MVC   ELEMSTOP(ELEMBDAT+1),=X'7E01033900E2'                            
         LA    R2,ELEMBDAT+1(R2)                                                
         GOTO1 VMINIPAR,DMCB,LTABLE,SCTABL,(X'70',(R2))                         
*-----------------------------------------------------------------              
* INSERTION OF STATION INTO MARKET AND USER LIST                                
*-----------------------------------------------------------------              
*                                                                               
         MVI   RNKSTATH+5,0                                                     
         MVC   OUTAREA3,OUTAREA1                                                
         MVC   OUTAREA1,=CL128' '                                               
         LA    R1,OUTAREA1                                                      
         LA    R2,OUTAREA2                                                      
         LA    R3,OUTAREA3                                                      
* COPY FIRST ELEM TO AREA3                                                      
         MVC   0(ELEMBDAT,R1),0(R3)    FIRST ONE HAS NO ERROR                   
         ZIC   R4,1(R3)            LENGTH OF ELEM                               
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   ELEMBDAT(0,R1),ELEMBDAT(R3)                                      
         LA    R4,ELEMBDAT+1(R4)       1 FOR EX, 4 FOR HEADER                   
         AR    R3,R4                                                            
         AR    R1,R4                                                            
*                                                                               
         MVC   0(ELEMBDAT,R1),0(R3)    FIRST ONE HAS NO ERROR                   
         ZIC   R4,1(R3)            LENGTH OF ELEM                               
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   ELEMBDAT(0,R1),ELEMBDAT(R3)                                      
         LA    R4,ELEMBDAT+1(R4)       1 FOR EX, 4 FOR HEADER                   
         AR    R3,R4                                                            
         AR    R1,R4                                                            
*                                                                               
TWO_3    CLI   0(R2),X'FF'                                                      
         BE    ONE_3                                                            
         CLI   0(R2),X'0'                                                       
         BE    ONE_3                                                            
         MVC   0(ELEMBDAT,R1),0(R2)                                             
         ZIC   R4,1(R2)            LENGTH OF ELEM                               
         LTR   R4,R4                                                            
         BNZ   TWO_3A                                                           
         BCTR  R4,0                                                             
         B     TWO_3B                                                           
TWO_3A   BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   ELEMBDAT(0,R1),ELEMBDAT(R2)                                      
TWO_3B   LA    R4,ELEMBDAT+1(R4)   1 FOR EX, 4 FOR HEADER                       
         AR    R2,R4                                                            
         AR    R1,R4                                                            
         B     TWO_3                                                            
*                                                                               
ONE_3    CLI   0(R3),X'FF'                                                      
         BE    AFTPARXT                                                         
         CLI   0(R3),X'0'                                                       
         BE    AFTPARXT                                                         
         MVC   0(ELEMBDAT,R1),0(R3)                                             
         ZIC   R4,1(R3)            LENGTH OF ELEM                               
         LTR   R4,R4                                                            
         BNZ   ONE_3A                                                           
         BCTR  R4,0                                                             
         B     ONE_3B                                                           
ONE_3A   BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   ELEMBDAT(0,R1),ELEMBDAT(R3)                                      
ONE_3B   LA    R4,ELEMBDAT+1(R4)   1 FOR EX, 4 FOR HEADER                       
         AR    R3,R4                                                            
         AR    R1,R4                                                            
         B     ONE_3                                                            
*                                                                               
AFTPARXT MVC   0(4,R1),0(R3)                                                    
AFTPARX2 LA    R1,OUTAREA1                                                      
         ST    R1,OUTAREA                                                       
         C     R1,OUTAREA          NO ERROR                                     
         XMOD1                                                                  
AFTERROR LA    R1,OUTAREA1                                                      
         ST    R1,OUTAREA                                                       
*        CLI   RNKMKTH+5,0                                                      
*        BNE   AFTERR1                                                          
*FTERR1  MVC   OUTAREA1+3(3),=X'02E400'                                         
         LTR   R1,R1               ERROR                                        
         XMOD1                                                                  
         SPACE 2                                                                
         SPACE 2                                                                
OUTAREA1 DC    2CL256' '                                                        
OUTAREA2 DC    2CL256' '                                                        
OUTAREA3 DC    2CL256' '                                                        
LTABLEDS DS    0FL8                                                             
LTABLE   DC    8F'0'                                                            
SCTABL   DC    C'=,',X'FF'                                                      
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------                  
* THIS PROCEDURE IS TO FIND THE ALPHA MARKET FOR A GIVEN                        
* MARKET NUMBER.                                                                
* THE INPUTS NEEDED ARE: MARKET NUMBER    (MKTNUM)                              
*                        MEDIA TYPE       (MEDTYPE)                             
*                        RATING SOURCE    (SRCTYPE)                             
*                        BOOK TYPE        (BOOKTYPE)                            
* THE OUTPUT WILL BE AN ALPHA MARKET THAT GOES IN CITYCODE                      
*-------------------------------------------------------------                  
ALPHMRKT NMOD1 0,**ALPHMRKT**                                                   
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         LA    R5,CODETABL                                                      
         MVC   0(1,R5),MEDTYPE                                                  
         MVC   1(1,R5),SRCTYPE                                                  
         MVC   2(1,R5),BOOKTYPE                                                 
         MVC   3(2,R5),MKTNUM                                                   
         GOTO1 VNUM2CDE                                                         
         MVC   CITYCODE,5(R5)                                                   
         CLC   =CL3' ',CITYCODE                                                 
         BNE   XITCALPH                                                         
XITCALFE XC    CITYCODE,CITYCODE                                                
XITCALPH XMOD1                                                                  
         SPACE 1                                                                
CODETABL DC    CL8' '                                                           
         LTORG                                                                  
         EJECT                                                                  
         DS    0H                                                               
FOOT     NMOD1 0,**FOOT                                                         
         L     R8,0(R1)                                                         
         L     R9,4(R1)                                                         
         L     RC,8(R1)                                                         
*                                                                               
         LA    RF,P                                                             
         ST    RF,FOOTP                                                         
         CLI   STACTSW,1                                                        
         BNE   FOOT1                                                            
         L     RE,=A(VIO1)                                                      
         A     RE,RELO                                                          
         MVC   P(L'VIO1),0(RE)                                                  
         L     RE,=A(VIO2)                                                      
         A     RE,RELO                                                          
         MVC   P2(L'VIO2),0(RE)                                                 
         LA    RF,P3                                                            
FOOT1    MVI   0(RF),C' '                                                       
         MVC   1(131,RF),0(RF)                                                  
FOOT2    L     R4,AMYD                                                          
         ST    RF,DUB              SAVE PL ADDR                                 
         CLI   COMBLIST,0                                                       
         BE    *+8                                                              
         BAS   RE,FOOTC                                                         
         BAS   RE,DAYFOOT                                                       
         XMOD1                                                                  
*-------------------------------------------------------------------            
FOOTC    NTR1                                                                   
         LA    RE,COMBLIST         POINT TO LIST OF COMBOS                      
FOOTC2   ST    RE,FULL             SAVE FOR NEXT LOOP                           
         CLI   0(RE),0                                                          
         BE    FOOTCX                                                           
         MVC   0(9,RF),0(RE)       MOVE IN THE COMBO ID                         
         MVI   9(RF),C'='                                                       
         LA    RF,10(RF)                                                        
         LA    RE,9(RE)                                                         
FOOTC4   MVC   4(3,RF),=C'- M'     SET UP FOR STATION                           
         MVC   0(4,RF),0(RE)       SET CALL LETTERS                             
         MVC   5(1,RF),4(RE)       AND BAND                                     
         LA    RF,7(RF)                                                         
         LA    RE,5(RE)                                                         
         CLI   0(RE),0             ON TO NEXT IF END                            
         BE    FOOTC6                                                           
         MVI   0(RF),C','          SET A '+' BETWEEN STATIONS                   
         LA    RF,1(RF)                                                         
         B     FOOTC4                                                           
         SPACE 1                                                                
FOOTC6   L     R5,DUB                                                           
         GOTO1 VSQUASH,DMCB,(R5),(0,180)                                        
         L     RF,DUB                                                           
         A     RF,DMCB+4                                                        
         LA    RF,2(RF)            SET FOR NEXT PRINT POS.                      
         L     RE,FULL             RESET TO PREV. COMBO                         
         LA    RE,LNCOMB(RE)       NOW TRY FOR ANOTHER COMBO                    
         B     FOOTC2                                                           
FOOTCX   ST    RF,FOOTP                                                         
         B     XIT                                                              
         SPACE 1                                                                
         DROP  R4                                                               
*-------------------------------------------------------------------            
DAYFOOT  NTR1                                                                   
         MVI   DAYPCNT,C'1'                                                     
         CLI   NCMBDPT,0          NO CUSTOM DAYPARTS                            
         BZ    XIT                                                              
         L     R3,FOOTP                                                         
         LA    R3,1(R3)                                                         
         L     R2,DPTTABL                                                       
DAYFTLP  CLI   0(R2),0                                                          
         BZ    XIT                                                              
         MVC   0(19,R3),=C'CUSTOM DAYPART #  ='                                 
         MVC   16(1,R3),DAYPCNT                                                 
         LA    R3,20(R3)                                                        
         ZIC   R1,0(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),1(R2)                                                    
         LA    R1,2(R1)                                                         
         AR    R2,R1                                                            
         AR    R3,R1                                                            
         MVC   0(2,R3),=C'  '                                                   
         LA    R3,2(R3)                                                         
         ZIC   R5,DAYPCNT                                                       
         LA    R5,1(R5)                                                         
         STC   R5,DAYPCNT                                                       
         B     DAYFTLP                                                          
FOOTP    DC    F'0'                                                             
DAYPCNT  DC    C'1'                                                             
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO AVERAGE, COMPRESS AND RANK BUFFERS                    
         SPACE 3                                                                
*              INPUTS              THISBUFF NDEMOS NDPTS    MAXRANK             
*                                  NUMSTAT                                      
*              OUTPUT              BUFF                                         
         SPACE 1                                                                
         DS    0H                                                               
RANKBUFF NMOD1 0,**RANKBUFF**                                                   
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         L     R2,ATHISBUF                                                      
         L     R3,ABEST                                                         
         L     R4,AWORST                                                        
         ZIC   R0,NDPTS                                                         
         SPACE 1                                                                
RANK2    CLI   0(R2),200           CO STATION MISSING DATA                      
         BH    *+12                JUST BYPASS IT                               
****     BH    *+8                 JUST BYPASS IT                               
**       BH    *+4                 JUST BYPASS IT                               
         BAS   RE,AVECOMP                                                       
         BAS   RE,SHUFFLE                                                       
RANK3    LA    R2,L'THISREC(R2)       BUMP TO NEXT DAYPART                      
         LA    R3,L'BUFFREC*40(R3)                                              
         LA    R4,L'BUFFREC*40(R4)                                              
         BCT   R0,RANK2                                                         
         XMOD1                                                                  
*--------------------------------------------------------------                 
AVECOMP  NTR1                                                                   
         USING THISD,R2                                                         
         USING BUFFD,R3                                                         
         LR    R7,R2               SAVE BLOCK ADDRESS                           
         LR    R3,R2               OVERWRITING WITH SMALLER VERSION             
         L     RF,THISWT           PICK UP WEIGHT FOR DAYPART                   
         LA    RF,0(RF)            KILL HOB                                     
         CLI   0(R2),200                                                        
         BL    *+10                                                             
         XC    THISWT(L'THISREC),THISWT                                         
*                                                                               
         MVC   BUFFSTAT,STATNUM                                                 
         LA    R2,THISDEMS                                                      
         LA    R3,BUFFDEMS                                                      
*        LA    R4,4                                                             
         ZIC   R4,NDEMOS                                                        
         SPACE 1                                                                
AVECOMP2 SR    R1,R1                                                            
         LTR   RF,RF                                                            
         BZ    AVECOMP4                                                         
         L     R1,0(R2)            AVERAGE                                      
         SR    R0,R0                                                            
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         SPACE 1                                                                
AVECOMP4 CLI   WORSTOPT,C'Y'       (CARRY COMPLEMENT FOR WORST)                 
         BNE   *+6                                                              
         LCR   R1,R1                                                            
         STCM  R1,MSKDEM,0(R3)          SAVE IT                                 
         LA    R2,4(R2)                                                         
         LA    R3,LENDEM(R3)                                                    
         BCT   R4,AVECOMP2                                                      
*                                                                               
         LA    R4,4                                                             
         ZIC   R2,NDEMOS                                                        
         SR    R4,R2                                                            
         MH    R4,=AL2(LENDEM)                                                  
*        SLA   R4,1                                                             
         EX    R4,*+8              DIDN'T BCTR, WANT TO CLEAR BUFFSAN           
         B     *+10                                                             
         XC    0(0,R3),0(R3)                                                    
*                                                                               
         LR    R2,R7               RESTORE BLOCK ADDRESS                        
         LR    R3,R7                                                            
         CLI   THISANN,C' '                                                     
         BNH   XIT                                                              
         MVC   BUFFSAN,THISANN                                                  
         MVI   STACTSW,1                                                        
         B     XIT                                                              
*-----------------------------------------------------------------              
SHUFFLE  NTR1                                                                   
         OC    1(L'BUFFREC-1,R2),1(R2)  NO DEMS AT ALL                          
**       BZ    XIT                      JUST GET OUT                            
         OC    1(LENDEM,R4),1(R4)       IF NO WORST JUST ACCEPT IT              
         BZ    *+14                                                             
         CLC   1(LENDEM,R2),1(R4)       COMPARE AGAINST THE WORST               
         BNH   XIT                 IF BETTER,                                   
         LR    RE,R3               LOOK FOR AN OPEN SLOT                        
         ZIC   R5,MAXRANK                                                       
         BCTR  R5,0                                                             
         SPACE 1                                                                
SHUFFLE2 OC    0(L'BUFFREC,RE),0(RE)                                            
         BZ    SHUFFLE4                                                         
         LA    RE,L'BUFFREC(RE)                                                 
         BCT   R5,SHUFFLE2                                                      
*                                  (NO ROOM - SO OVERWRITE LAST)                
SHUFFLE4 DS    0H                                                               
         MVC   0(L'BUFFREC,RE),0(R2)       MOVE IN A NEW LINE                   
         ZIC   R5,MAXRANK          AND RERANK THIS DAYPART                      
SHUFFLE5 DS    0H                                                               
*                              (DESCENDING)                                     
         GOTO1 XSORT,DMCB,(1,(R3)),(R5),L'BUFFREC,LENDEM,1                      
         B     XIT                                                              
         DROP  R2                                                               
         DROP  R3                                                               
SHUF1ST  DC    C'Y'                                                             
         LTORG                                                                  
         EJECT                                                                  
*------------------------------------------------------------------             
* NEWCUMES IS TO CALCULATE RANDOM DUPLICATION FORMULA                           
*------------------------------------------------------------------             
NEWCUMES NMOD1 0,**NEWCUME**                                                    
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         LA    R6,CUMERAT                                                       
         ZIC   R8,NDPTS                                                         
         L     R7,ATHISB2                                                       
NEWCUME0 STM   R6,R7,DUB                                                        
         LR    RF,R7                                                            
         LA    RF,4(RF)            FIRST DEMO                                   
         ZIC   R5,NDEMOS                                                        
         LA    R4,DEMOS                                                         
*                                                                               
NEWCUME1 CLI   1(R4),C'C'          CUMES ONLY                                   
         BNE   NEWCUME9                                                         
         BAS   RE,FNDUNIVS         R3 RETURNS WITH UNIVERSE                     
         ST    R3,DMCB                                                          
         SR    R2,R2                                                            
         M     R2,0(R6)            MULTIPLY BY CUME RATINGS                     
*                                                                               
*        ZIC   R1,CNOSTA           # OF STATIONS IN THIS COMBO                  
*        BCTR  R1,0                                                             
NEWCUME3 D     R2,=F'1000'         DIVIDE BY 1000 FOR STATIONS-1                
         SR    R2,R2               DON'T WANT REMAINDER                         
*        BCT   R1,NEWCUME3                                                      
         D     R2,=F'100'                                                       
         A     R3,=F'5'                                                         
         SR    R2,R2                                                            
         D     R2,=F'10'                                                        
*                                                                               
         L     R1,0(RF)            SUBTRACT SUM BY CUME RTGS * UNIV             
         SR    R1,R3                                                            
         ST    R1,0(RF)                                                         
*                                                                               
         SR    R0,R0                                                            
         M     R0,=F'10000'                                                     
         D     R0,DMCB             DIVIDE BY UNIVERSE WE FOUND BEFORE           
         AH    R1,=H'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         ST    R1,0(R6)                                                         
*                                                                               
NEWCUME9 LA    R4,3(R4)            NEXT DEMO                                    
         LA    RF,4(RF)                                                         
         LA    R6,4(R6)            NEXT CUME RTG                                
         BCT   R5,NEWCUME1                                                      
*                                                                               
         LM    R6,R7,DUB                                                        
         LA    R7,24(R7)           NEXT DAYPART                                 
         LA    R6,16(R6)           NEXT CUME RTG FOR DAYPART                    
         BCT   R8,NEWCUME0                                                      
*                                                                               
         XMOD1                                                                  
*---------------------------------------------------------------                
FNDUNIVS NTR1                                                                   
         LA    RF,RUNIVS                                                        
         CLI   DEMSTYLE,C'C'       BY CATEGORIES                                
         BNE   FNDUVSD             NO, BY DEMOS                                 
*                                                                               
         CLI   0(R4),2             TSA?                                         
         BL    FNDUVXIT            LESS THAN, MSA                               
         BH    FNDUVSC2            GREATER THAN, ADI                            
         LA    RF,RUNIVS+4                                                      
         B     FNDUVXIT                                                         
FNDUVSC2 LA    RF,RUNIVS+8                                                      
         B     FNDUVXIT                                                         
*                                                                               
FNDUVSD  ZIC   R2,NDEMOS           # OF DEMOS USED                              
         SR    R2,R5               SUBTRACTING CURRENT COUNTER                  
         LTR   R2,R2               GIVES INDEX, 0 IS FIRST                      
         BZ    FNDUVXIT                                                         
FNDUVSD2 LA    RF,4(RF)                                                         
         BCT   R2,FNDUVSD2                                                      
FNDUVXIT L     R3,0(RF)                                                         
         XIT1  REGS=(R3)                                                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO FILL BUFFER FOR THIS STATION                          
         SPACE 3                                                                
*              INPUTS              R2=A(5-BYTE STATION)                         
*                                  NDPTS DPTLIST                                
*              OUTPUT              THISBUFF (R2)                                
         SPACE 1                                                                
         DS    0H                                                               
FILLBUFF NMOD1 0,**FILLBUFF**                                                   
         L     R2,0(R1)                                                         
         L     R7,4(R1)                                                         
         L     R9,8(R1)                                                         
         L     RC,12(R1)                                                        
*                                                                               
         L     R4,AMYD                                                          
         USING MYD,R4                                                           
         CLI   NEWSTAT,C'Y'                                                     
         BNE   FILLB0                                                           
         LA    RE,THISBUFF         CLEAR BUFFER FOR EACH STATION                
         LA    RF,L'THISREC*15                                                  
         XCEF                                                                   
FILLB0   MVI   DBSELMED,C'R'                                                    
         XC    DBSELMK,DBSELMK                                                  
         LA    RE,DBRADIC                                                       
         ST    RE,DBEXTEND                                                      
         XC    0(128,RE),0(RE)                                                  
         USING DBRID,RE                                                         
         MVC   DBRID,=C'RADI'                                                   
         MVI   DBRCOPT,C'Y'                                                     
         LA    RF,128(RE)                                                       
         ST    RF,DBRNEXT                                                       
         LR    RE,RF                                                            
         LA    RF,900                                                           
         STM   RE,RF,DMCB                                                       
         XCEF                                                                   
         LM    RE,RF,DMCB                                                       
         USING DBXFORMD,RE                                                      
         MVC   DBXFID,=C'FORM'                                                  
         STCM  RF,3,DBXFLEN                                                     
         SPACE 1                                                                
*                                                                               
         CLI   RNKSRCE,C'A'        THIS COMES OUT SOON                          
         BE    FILLB1              *                                            
         TM    4(R2),X'80'         *                                            
         BO    *+10                *                                            
         SPACE 1                                                                
FILLB1   MVC   DBSELMK,MKTNUM                                                   
         OI    4(R2),X'80'                                                      
         MVC   DBSELSTA,0(R2)                                                   
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBAREC,AIO1                                                      
         CLI   RUNIFLG,C'X'                                                     
         BNE   FILLB1A                                                          
         MVI   RUNIFLG,C'N'                                                     
         DROP  R4                                                               
FILLB1A  LA    R2,BOOKS            SET UP FOR MULTIPLE BOOKS                    
         ZIC   R3,NBOOKS                                                        
         SPACE 1                                                                
FILLB2   MVI   ANYUNIV,C'N'                                                     
         MVC   DBSELBK,1(R2)                                                    
         MVC   DBBTYPE,3(R2)                                                    
         CLI   DBSELBK,87          ALWAYS NEED MARKET AFTER 86                  
         BL    *+10                                                             
         MVC   DBSELMK,MKTNUM                                                   
         MVC   DBFILE,=C'RDP'      READ RDP FOR STANDARD                        
         MVI   THISTYPE,C'S'       DO ALL STANDARD FIRST                        
         BAS   RE,FILL2                                                         
         MVC   DBFILE,=C'TP '      READ TP FOR CUSTOM DAYPARTS                  
         MVI   THISTYPE,C'C'       THEN DO THE CUSTOM                           
         BAS   RE,FILL2                                                         
         LA    R2,4(R2)                                                         
         CLI   DBSELSRC,C'M'                                                    
         BNE   FILLB2A                                                          
         CLI   RUNIFLG,C'Y'                                                     
         BE    FILLB2A                                                          
         CLI   CALCOPT,C'Y'                                                     
         BNE   FILLB2A                                                          
         BAS   RE,FRELUNIV                                                      
FILLB2A  BCT   R3,FILLB2                                                        
         CLI   DBSELSRC,C'M'                                                    
         BNE   FILLB3                                                           
         OC    RUNIVS,RUNIVS                                                    
         BZ    FILLB3                                                           
         CLI   RUNIFLG,C'Y'                                                     
         BE    FILLB3                                                           
         BAS   RE,FAVEUNIV                                                      
         MVI   RUNIFLG,C'Y'                                                     
FILLB3   XMOD1                                                                  
         SPACE 3                                                                
FRELUNIV NTR1                                                                   
         L     R2,UNIVS                                                         
         A     R2,RUNIVS                                                        
         ST    R2,RUNIVS                                                        
         L     R2,UNIVS+4                                                       
         A     R2,RUNIVS+4                                                      
         ST    R2,RUNIVS+4                                                      
         L     R2,UNIVS+8                                                       
         A     R2,RUNIVS+8                                                      
         ST    R2,RUNIVS+8                                                      
         L     R2,UNIVS+12                                                      
         A     R2,RUNIVS+12                                                     
         ST    R2,RUNIVS+12                                                     
         B     XIT                                                              
         SPACE 3                                                                
FAVEUNIV NTR1                                                                   
         LA    R0,4                                                             
         ZIC   R5,NBOOKS                                                        
         LA    R7,RUNIVS                                                        
FAVUNILP SR    R2,R2                                                            
         L     R3,0(R7)                                                         
         DR    R2,R5                                                            
         ST    R3,0(R7)                                                         
         LR    R3,R2                                                            
         SR    R2,R2                                                            
         LA    R5,10                                                            
         MR    R2,R5                                                            
         ZIC   R5,NBOOKS                                                        
         DR    R2,R5                                                            
         CH    R3,=H'5'                                                         
         BL    FAVNXTLP                                                         
         L     R3,0(R7)                                                         
         LA    R3,1(R3)                                                         
         ST    R3,0(R7)                                                         
FAVNXTLP LA    R7,4(R7)                                                         
         BCT   R0,FAVUNILP                                                      
         B     XIT                                                              
         SPACE 3                                                                
FILL2    NTR1                                                                   
         L     R2,ATHISBUF                                                      
         USING THISD,R2                                                         
         L     R4,AMYD                                                          
         USING MYD,R4                                                           
*                                                                               
         CLC   =C'     ',MKTSTN                                                 
         BE    FILL3                                                            
         LA    R2,MKTBUFF                                                       
*                                                                               
FILL3    LA    R4,DPTLIST                                                       
         USING DPENTRY,R4                                                       
         ZIC   R0,NDPTS                                                         
         MVI   BYTE,1                                                           
         SPACE 1                                                                
FILL4    CLC   DPSTYPE,THISTYPE    MATCH ON TYPE (C/S)                          
         BNE   FILLNXT1                                                         
         CLI   DPSTYPE,C'S'        STANDARD SETTING                             
         BNE   FILL6                                                            
         CLI   DBSELSRC,C'R'                                                    
         BNE   *+10                                                             
         MVC   DBFILE,=C'RTP'                                                   
         MVC   DBSELDAY,DPSDAY     NEEDS DAY                                    
         XC    DBSELTIM,DBSELTIM   DOES NOT NEED TIME                           
*****    MVI   DBDQSQH,5                                                        
*                                  BUT DOES NEED PROGRAM                        
         MVC   DBSELPRG+1(1),DPSPROG                                            
         GOTO1 DEMAND,DMCB,DBLOCK,DEMHOOK                                       
         B     FILLNEXT                                                         
         SPACE 1                                                                
FILL6    LA    R3,DPCDAY           CUSTOM HAS UP TO 5 DAY/TIMES                 
         XC    DBSELPRG,DBSELPRG                                                
         LA    R5,5                                                             
         SPACE 1                                                                
FILL8    CLI   0(R3),0                                                          
         BE    FILLNEXT                                                         
         MVC   DBSELDAY,0(R3)      MOVE IN DAY                                  
         ZIC   R1,1(R3)            WORK OUT MILITARY TIMES                      
         MH    R1,=H'100'                                                       
         STCM  R1,3,DUB                                                         
         ZIC   R1,2(R3)                                                         
         MH    R1,=H'100'                                                       
         STCM  R1,3,DUB+2                                                       
         MVC   DBSELTIM,DUB        START AND END                                
         GOTO1 DEMAND,DMCB,DBLOCK,DEMHOOK                                       
         LA    R3,3(R3)                                                         
         BCT   R5,FILL8                                                         
         SPACE 1                                                                
FILLNEXT CLI   BYTE,1              DATA NOT FOUND                               
         BNE   FILLNXT1                                                         
         CLI   DBSELSTA+4,C'C'     SIMULCASTS CAUSE PROBLEMS                    
         BNE   *+8                 BECAUSE A+F MAY NOT ADD UP TO CO             
         MVI   0(R2),201           IF CO HASE MISSING DATA                      
*                                                                               
FILLNXT1 L     RE,AMYD                                                          
         USING MYD,RE                                                           
         CLC   =C'     ',MKTSTN                                                 
         BE    FILLNXT2                                                         
         LA    R2,20(R2)                                                        
         B     FILLNXT3                                                         
FILLNXT2 LA    R2,L'THISREC(R2)                                                 
FILLNXT3 LA    R4,L'DPENTRY(R4)    ON TO NEXT DAYPART                           
         MVI   BYTE,1                                                           
         BCT   R0,FILL4                                                         
         B     XIT                                                              
         EJECT                                                                  
*              HOOK FROM DEMAND                                                 
         SPACE 3                                                                
*              INPUTS              R2=A(THISBUFF FOR DAYPART)                   
*                                  DEMOS NDEMOS                                 
*              OUTPUT              ADD WEIGHTED DEMOS TO THIS BUFF              
         SPACE 1                                                                
*                                  DEMOS NDEMOS                                 
DEMHOOK  NTR1                                                                   
*                                  FORMAT UNIVERSES FOR HEADLINES               
*                                                                               
         CLI   RUNIFLG,C'Y'                                                     
         BE    DEMHOOK0                                                         
         CLI   DBSELSRC,C'M'                                                    
         BNE   DEMHOOK0                                                         
         GOTOR SUBR08,DMCB,('SETUNIBE',(RC))                                    
*                                  FIRST GET DEMOS TO THISLINE                  
         SPACE 1                                                                
DEMHOOK0 XC    THISLINE,THISLINE                                                
         OC    DBFACTOR,DBFACTOR   PROTECT AGAINST THIS                         
         BNZ   *+10                                                             
         MVC   DBFACTOR,=H'1'                                                   
         MVC   THISLINE+2(2),DBFACTOR                                           
         L     RE,DBAREC                                                        
         LA    RE,23(RE)                                                        
         USING MARELEM,RE                                                       
         CLI   MARCODE,MARCODEQ    '01' ELEMENT?                                
         BNE   DEMHK1                                                           
         CLC   MARNO,MKTNUM                                                     
         BNE   DEMHK1                                                           
*        B     DMHKT               ****TEST****                                 
         CLI   MARELN,MARLNEQ2     EXTENDED '01' ELEMENT?                       
         BL    DMHKT               NO                                           
*        MVC   THISNOA(2),MARDATE+2                                             
         CLI   MARACTCD,C' '                                                    
         BNH   DMHKT                                                            
         MVC   20(2,R2),MARAIRTM   INCLUDES FIELD MARACTCD                      
         DROP  RE                                                               
DMHKT    DS    0H                                                               
****     MVI   DEMOS+0,0                                                        
         GOTO1 DEMOUT,DMCB,(C'L',DEMOS),DBLOCK,THISLINE+4                       
         SPACE 1                                                                
DEMHK1   LA    R3,THISLINE+4       NOW WEIGHT BY DBFACTOR                       
*                                                                               
         ZIC   R0,NDEMOS                                                        
         LA    R5,TMPDEMOS                                                      
         SPACE 1                                                                
DEMHK2   L     R1,0(R3)                                                         
*                                                                               
         L     RE,AMYD                                                          
         USING MYD,RE                                                           
         CLC   =C'     ',MKTSTN                                                 
         BE    DEMHK3                                                           
         L     R1,0(R2)            INCREMENT COUNTER                            
         LA    R1,1(R1)                                                         
         ST    R1,0(R2)                                                         
*                                                                               
         LA    R2,4(R2)            SKIP PASS COUNTER                            
         B     DEMHK4                                                           
*                                                                               
DEMHK3   MH    R1,DBFACTOR                                                      
         ST    R1,0(R3)                                                         
         LA    R3,4(R3)                                                         
DEMHK3A  BCT   R0,DEMHK2                                                        
         SPACE 1                                                                
         LA    R3,THISLINE         THEN ADD INTO BUFFER FOR STATION             
         ZIC   R0,NDEMOS                                                        
         ZIC   RE,0(R2)            COUNT STA. FOR COMBOS                        
         ZIC   R1,BYTE                                                          
         AR    RE,R1                                                            
         MVI   BYTE,0                                                           
         L     R1,0(R2)                                                         
         LA    R1,0(R1)            KILL HOB                                     
         A     R1,0(R3)            SUM WEIGHT                                   
         ST    R1,0(R2)            AND SAVE                                     
         STC   RE,0(R2)            SAVE STATION COPUNT                          
         LA    R2,4(R2)            BUMP TO DEMOS                                
         LA    R3,4(R3)                                                         
         SPACE 1                                                                
DEMHK4   L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,DEMHK4                                                        
         SPACE 1                                                                
         B     XIT                                                              
         SPACE 1                                                                
*        CLI   TRACEOPT,C'Y'       OPTION TO TRACE                              
*        BNE   XIT                                                              
*        MVC   P(5),DBSELSTA                                                    
*        LA    R2,P+6                                                           
*        GOTO1 DEFINE,DMCB,=C'DAY',DBLOCK,WORK                                  
*        MVC   0(4,R2),WORK+2                                                   
*        LA    R2,5(R2)                                                         
*        GOTO1 DEFINE,DMCB,=C'TIME',DBLOCK,WORK                                 
*        GOTO1 UNTIME,DMCB,WORK+2,(R2)                                          
*        LA    R2,8(R2)                                                         
*        LA    R3,THISLINE                                                      
*        LA    R4,5                                                             
*        SPACE 1                                                                
*RACE2   EDIT  (4,0(R3)),(6,(R2))                                               
*        LA    R2,7(R2)                                                         
*        LA    R3,4(R3)                                                         
*        BCT   R4,TRACE2                                                        
*        GOTO1 SPOOL,DMCB,(R8)                                                  
*        B     XIT                                                              
         EJECT                                                                  
         DROP  R4                                                               
         DROP  RE                                                               
         DROP  R2                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              FORMAT AND PRINT A REPORT LINE                                   
         SPACE 3                                                                
*              INPUTS              R0=NUMBER OF DAYPARTS TO PRINT               
*                                  R3=A(FIRST BUFFER LINE)                      
*                                  ASTATS=A(5 CHARACTER STATION CODES)          
*                                  NDEMOS DEMOS                                 
*                                  DISP  DPTWIDTH  DPTGAP                       
         SPACE 1                                                                
         USING BUFFD,R3                                                         
         DS    0H                                                               
FORMLINE NMOD1 0,**FORMLINE**                                                   
         L     R8,0(R1)                                                         
         L     R9,4(R1)                                                         
         L     RC,8(R1)                                                         
*                                                                               
         LA    R2,P+1                                                           
         A     R2,DISP                                                          
         SPACE 1                                                                
FORM2    BAS   RE,FORM4            PRINT STATION AND DEMOS FOR DAYPART          
         A     R2,DPTWIDTH                                                      
         A     R2,DPTGAP           BUMP TO NEXT DAYPART                         
         LA    R3,L'BUFFREC*40(R3)                                              
         L     RE,TMPDENOM                                                      
         LA    RE,20(RE)                                                        
         ST    RE,TMPDENOM                                                      
         AI    DPTNUM,1                                                         
         BCT   R0,FORM2                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XMOD1                                                                  
*-------------------------------------------------------------------            
FORM4    NTR1                                                                   
         CLI   BUFFSTAT,0          INDEX INTO STATION LIST                      
***      BE    XIT                                                              
         BNE   *+8                                                              
         MVI   BUFFSTAT,1                                                       
         ZIC   R1,BUFFSTAT                                                      
         BCTR  R1,0                                                             
         MH    R1,WSTATS+2                                                      
         A     R1,ASTATS                                                        
         MVC   0(4,R2),0(R1)       OUTPUT WABC                                  
         CLI   4(R1),0             COMBOS - LEFT ALONE                          
         BE    FORM6                                                            
         MVI   USECMB,C'N'                                                      
         MVI   4(R2),C'-'                 WABC-                                 
         MVC   5(1,R2),4(R1)              WABC-A                                
         MVI   6(R2),C'M'                 WABC-AM                               
         CLI   5(R2),C'C'              OR                                       
         BNE   *+8                                                              
         MVI   6(R2),C'O'                 WABC-CO                               
         CLI   5(R2),C'B'              OR                                       
         BNE   *+10                                                             
         MVC   5(2,R2),=C'AA'             WABC-AA                               
         CLI   5(R2),C'D'              OR                                       
         BNE   *+10                                                             
         MVC   5(2,R2),=C'FF'             WABC-FF                               
         CLI   DBSELSRC,C'M'       BBM SPECIAL SUFFIXES                         
         BNE   *+8                                                              
         BAS   RE,CNVBBMS                                                       
         CLI   3(R2),C' '          CHANGE WOR -AM                               
         BNE   *+10                                                             
         MVC   3(4,R2),4(R2)           TO WOR-AM                                
         CLI   BUFFSAN,C' '                                                     
         BNH   FORM6A                                                           
         MVC   6(1,R2),BUFFSAN                                                  
         MVC   4(1,R2),5(R2)                                                    
         MVI   5(R2),C'#'                                                       
         MVI   STACTSW,1                                                        
         B     FORM6A                                                           
         SPACE 1                                                                
FORM6    L     R4,AMYD                                                          
         USING MYD,R4                                                           
         TM    3(R1),X'F0'         DEFINES THIS AS A COMBO                      
         BO    *+8                                                              
         B     FORM6A                                                           
         NI    3(R1),X'0F'         SET THE INDEX                                
         ZIC   RE,3(R1)                                                         
         OI    3(R1),X'F0'         RESTORE FOR NEXT TIME                        
         MVI   USECMB,C'Y'                                                      
         LA    RF,LNCOMB                                                        
         MR    RE,RE                                                            
         LA    RF,COMBLIST(RF)     AND THE BASE                                 
         MVC   0(9,R2),0(RF)       MOVE IN EXPANDED COMBO NAME                  
         SPACE 1                                                                
FORM6A   L     R4,AMYD                                                          
         CLI   COMBLIST,0                                                       
         BE    *+8                                                              
         LA    R2,2(R2)                                                         
         DROP  R4                                                               
         LA    R2,7(R2)            BUMP PAST THE CALL LETTERS                   
         SPACE 1                                                                
         CLI   FORMOPT,C'Y'        OPTION TO SHOW STATION FORMAT                
         BNE   FORM8                                                            
         CLI   WSTATS+3,9                                                       
         BL    FORM8                                                            
         MVC   1(4,R2),5(R1)                                                    
         LA    R2,5(R2)                                                         
         SPACE 1                                                                
FORM8    CLI   RANKOPT,C'Y'        OPTION TO SHOW THE RANK NO.                  
         BNE   FORM18                                                           
         ZIC   R1,DPTNUM           FIND RANK NUMBER IN RANKNUMS                 
         BCTR  R1,0                                                             
         MH    R1,=H'40'                                                        
         ZIC   R0,STATNUM                                                       
         BCTR  R0,0                                                             
         AR    R1,R0                                                            
         L     RF,AMYD                                                          
         USING MYD,RF                                                           
         LA    RF,RANKNUMS(R1)                                                  
         DROP  RF                                                               
         TM    0(RF),X'80'         CHECK FOR EQUALITY                           
         BNO   *+8                                                              
         MVI   4(R2),C'='                                                       
         IC    R1,0(RF)                                                         
         SLL   R1,25                                                            
         SRL   R1,25                                                            
         EDIT  (R1),(4,0(R2))                                                   
         LA    R2,5(R2)                                                         
         SPACE 1                                                                
FORM18   LA    R3,BUFFDEMS         AND SET UP FOR THE DEMOS                     
         LA    R4,TMPDEMOS                                                      
         ZIC   R5,NDEMOS                                                        
         BAS   RE,IMPRESS                                                       
         SPACE 1                                                                
FORM20   SR    R1,R1                                                            
         ICM   R1,MSKDEM,0(R3)   USE AS AN UNSIGNED NUMBER                      
         CLI   WORSTOPT,C'Y'                                                    
         BNE   *+6                                                              
         LCR   R1,R1                                                            
         N     R1,ANDMASK                                                       
         EDIT  (R1),(6,0(R2)),1                                                 
         CLI   1(R4),C'R'          1 DEC. FOR RATINGS ETC.                      
         BE    FORM22                                                           
         CLI   1(R4),C'F'                                                       
         BE    FORM22                                                           
         CLI   1(R4),C'P'                                                       
         BE    FORM22                                                           
         CLI   1(R4),C'G'                                                       
         BE    FORM22                                                           
         CLI   1(R4),C'H'                                                       
         BE    FORM22                                                           
         CLI   1(R4),C'L'                                                       
         BE    FORM22                                                           
         CLI   1(R4),C'S'                                                       
         BE    FORM22                                                           
         CLI   1(R4),C'X'                                                       
         BE    FORM30                                                           
         CLI   DBSELSRC,C'R'       ADJUST RADAR PRECISION                       
         BNE   *+10                                                             
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(6,0(R2))      ELSE NO DECIMAL PLACES                       
*                                                                               
         CLI   USECMB,C'Y'                                                      
         BNE   FORM22                                                           
FORM21   CLI   1(R4),C'C'          SUPPRES CUMES FOR COMBOS                     
         BNE   FORM22                                                           
FORM21E  DS    0H                                                               
*        MVC   0(6,R2),=C'   N/A'                                               
         SPACE 1                                                                
FORM22   LA    R2,6(R2)                                                         
         LA    R3,LENDEM(R3)                                                    
         LA    R4,3(R4)                                                         
         BCT   R5,FORM20                                                        
         B     XIT                                                              
*                                                                               
FORM30   CLI   USECMB,C'Y'         SUPPRESS TSL FOR COMBOS                      
*        BE    FORM21E                                                          
*                                                                               
         XC    0(6,R2),0(R2)                                                    
         MVI   3(R2),C':'                                                       
         SR    R0,R0                                                            
         M     R0,=F'15'                                                        
         D     R0,=F'600'          QUARTER HOURS W/O DECIMAL                    
         LR    RF,R0               EVEN REGISTER IS REMAINDER                   
         EDIT  (R1),(2,1(R2)),ZERO=NOBLANK      # OF HOURS                      
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         EDIT  (RF),(2,4(R2)),FILL=0                                            
*        SR    R1,R1                                                            
*        ICM   R1,3,0(R3)                                                       
         B     FORM22                                                           
         SPACE 3                                                                
USECMB   DC    C'N'                                                             
         DROP  R3                                                               
         EJECT                                                                  
CNVBBMS  NTR1                                                                   
         LA    RE,BBMSUF           CONVERT BBM SUFFIXES                         
CNVBBMS2 CLI   0(RE),X'FF'         PROTECT FOR GARBAGE                          
         BE    CNVBBMSX                                                         
         CLC   5(1,R2),2(RE)       FIND EXPANSION                               
         BE    *+12                                                             
         LA    RE,L'BBMSUF(RE)                                                  
         B     CNVBBMS2                                                         
CNVBBMS4 MVC   5(2,R2),0(RE)       SEND IT TO OUTPUT                            
CNVBBMSX B     XIT                                                              
BBMSUF   DS    0CL3                                                             
       ++INCLUDE DEBBMSUFX                                                      
         EJECT                                                                  
*------------------------------------------------------------------             
* IMPRESS IS LAST MINUTE CALCULATIONS ON DATA                                   
*------------------------------------------------------------------             
IMPRESS  NTR1                                                                   
         CLI   CALCOPT,C'Y'                                                     
         BNE   XIT                                                              
         L     R2,=A(TMPDEMOS)                                                  
         A     R2,RELO                                                          
         L     R8,TMPDENOM         SHOULD BE THE MARKET BUFFER                  
         LA    R8,4(R8)                                                         
         LA    R7,RUNIVS                                                        
*        LA    R8,1(R8)                                                         
         ZIC   R0,NDEMOS                                                        
DIVLOOP  CLI   DEMSTYLE,C'D'                                                    
         BE    DIVLOOP1                                                         
         LA    R7,RUNIVS                                                        
DIVLOOP1 CLI   1(R2),C'F'                                                       
         BE    DIVUNIV                                                          
         CLI   1(R2),C'G'                                                       
         BE    DIVUNIV                                                          
         CLI   1(R2),C'H'                                                       
         BE    DIVUNIV                                                          
         CLI   1(R2),C'P'                                                       
         BE    DIVUNIV                                                          
         CLI   1(R2),C'R'                                                       
         BE    DIVUNIV                                                          
         CLI   1(R2),C'S'          SHARE NEEDS DIFF FORMULA                     
         BNE   FINIDIV                                                          
         CLI   0(R2),2             DON'T HAVE TOTALS FOR TSA                    
         BE    FINIDIV                                                          
         CLI   DBSELSRC,C'M'       DON'T HAVE TOTALS FOR CANADA                 
         BE    FINIDIV                                                          
         B     DIVTOTS                                                          
DIVUNIV  SR    RE,RE                                                            
         SR    RF,RF                                                            
         CLI   DEMSTYLE,C'D'                                                    
         BE    DIVUNIV0                                                         
         CLI   0(R2),2             CHECK FOR TSA/ADI IF BY CATEGORY             
         BL    DIVUNIV0                                                         
         LA    R7,4(R7)                                                         
         CLI   0(R2),3                                                          
         BL    DIVUNIV0                                                         
         LA    R7,4(R7)                                                         
DIVUNIV0 ICM   RF,MSKDEM,0(R3)                                                  
         CLI   WORSTOPT,C'Y'                                                    
         BNE   DIVUNV0A                                                         
         LCR   RF,RF                                                            
         N     RF,ANDMASK                                                       
DIVUNV0A L     R5,=F'10000'                                                     
         MR    RE,R5               (10000*DEMO)                                 
         L     R5,0(R7)            UNIV ALREADY POSITIVE                        
         LTR   R5,R5                                                            
         BNZ   DIVUNIV1                                                         
******   SR    RF,RF                 TEST BPOO  7/20                            
         B     DIVUNIV2                                                         
DIVUNIV1 DR    RE,R5               (1000*DEMO)/UNIVERSE                         
         AH    RF,=H'5'            ROUNDING                                     
         SR    RE,RE                                                            
         LA    R5,10                                                            
         DR    RE,R5                                                            
DIVUNIV2 CLI   WORSTOPT,C'Y'                                                    
         BNE   DIVUNV2A                                                         
         LCR   RF,RF                                                            
DIVUNV2A STCM  RF,MSKDEM,0(R3)                                                  
FINIDIV  LA    R2,3(R2)                                                         
         LA    R3,LENDEM(R3)                                                    
         CLI   DEMSTYLE,C'D'       MORE THAN ONE UNIVERSE?                      
         BNE   FINIDIV1                                                         
         LA    R7,4(R7)                                                         
FINIDIV1 BCT   R0,DIVLOOP                                                       
         B     XIT                                                              
*                                                                               
DIVTOTS  SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,MSKDEM,0(R3)                                                  
         CLI   WORSTOPT,C'Y'                                                    
         BNE   DIVTOTS0                                                         
         LCR   RF,RF                                                            
         N     RF,ANDMASK                                                       
DIVTOTS0 L     R5,=F'10000'                                                     
         MR    RE,R5               (10000*DEMO)                                 
*        SR    R5,R5                                                            
         ICM   R5,15,0(R8)                                                      
DIVTOT0A LTR   R5,R5                                                            
         BNZ   DIVTOTS1                                                         
         SR    RF,RF                                                            
         B     DIVTOTS2                                                         
DIVTOTS1 DR    RE,R5               (1000*DEMO)/UNIVERSE                         
         AH    RF,=H'5'            ROUNDING                                     
         SR    RE,RE                                                            
         LA    R5,10                                                            
         DR    RE,R5                                                            
DIVTOTS2 CLI   WORSTOPT,C'Y'                                                    
         BNE   *+6                                                              
         LCR   RF,RF                                                            
         STCM  RF,MSKDEM,0(R3)          SAVE SHARE RESULT                       
         LA    R8,4(R8)            GET NEXT TOTAL                               
         B     FINIDIV                                                          
         EJECT                                                                  
*------------------------------------------                                     
         LTORG                                                                  
         EJECT                                                                  
*              COMPUTE CUMULATIVE SHARES IF NEEDED                              
         SPACE 3                                                                
*              OUTPUTS             BUFFER ADJUSTED                              
         SPACE 1                                                                
COMPCUSH NMOD1 0,**COMPCUSH**                                                   
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         L     R3,ABEST                                                         
         ZIC   R0,NDPTS                                                         
         SPACE 1                                                                
COMPC2   BAS   RE,COMPC4           ADJUST FOR EACH DAYPART                      
         LA    R3,L'BUFFREC*40(R3)                                              
         BCT   R0,COMPC2                                                        
         XMOD1                                                                  
         SPACE 1                                                                
COMPC4   NTR1                                                                   
         LA    R2,CATTITS                                                       
         LA    R3,1(R3)            (PAST STATION NUMBER)                        
         ZIC   R0,NDEMOS                                                        
         SPACE 1                                                                
COMPC6   CLC   1(4,R2),=C'CUSH'    FOR CUMULATIVE SHARE                         
         BNE   *+8                                                              
         BAS   RE,COMPC8           GO AND ADJUST THE COLUMN                     
         LA    R2,5(R2)                                                         
         LA    R3,LENDEM(R3)                                                    
         BCT   R0,COMPC6                                                        
         B     XIT                                                              
         SPACE 1                                                                
COMPC8   NTR1                      ADJUST COLUMN FOR THIS DEMO                  
         ZIC   R0,MAXRANK                                                       
         SR    R1,R1                                                            
         SPACE 1                                                                
COMPC10  AH    R1,LENDEM-2(R3)       PICK UP SHARE FOR THIS STATION             
         STH   R1,LENDEM-2(R3)       ADD TO PREVIOUS AND RESTORE                
         LA    R3,L'BUFFREC(R3)            ON TO NEXT STATION                   
         BCT   R0,COMPC10                                                       
         B     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO COMPUTE PRINT AND BUFFER DISPLACEMENTS                
         SPACE 3                                                                
*              INPUTS              NDPTS    NSTATS   NDEMOS                     
*                                  WIDEOPT  MAXRANK  BOXOPT                     
*              OUTPUTS             DISP     DPTGAP   UP      DPTWIDTH           
*                                  ASTATS   ABEST    AWORST                     
         SPACE 1                                                                
         DS    0H                                                               
COMPDISP NMOD1 0,**COMPDISP**                                                   
         L     R8,0(R1)                                                         
         L     R9,4(R1)                                                         
         L     RC,8(R1)                                                         
*                                                                               
         LA    R2,BUFF             STATION LIST IN BUFF                         
         ST    R2,ASTATS                                                        
         LA    R2,500(R2)          BUMP PAST THIS TO DEMO BUFFER                
         ST    R2,ABEST            FIRST LINE IS BEST                           
         ZIC   R1,MAXRANK                                                       
         BCTR  R1,0                                                             
         MH    R1,=AL2(L'BUFFREC)  (EACH LINE IS 9 BYTES)                       
         AR    R2,R1                                                            
         ST    R2,AWORST                                                        
*                                                                               
         MVI   RCSUBPRG,1                                                       
         CLI   WIDEOPT,3                                                        
         BNE   *+8                                                              
         MVI   RCSUBPRG,2                                                       
         ZIC   R1,NDEMOS           COMPUTE WIDTH OF DAYPART CHUNK               
*        SR    R1,R5                                                            
         MH    R1,=H'6'            (N'DEMOS X 6) + 9                            
         LA    R1,9(R1)                                                         
         CLI   RANKOPT,C'Y'        OPTION TO SHOW RANK NUMBER?                  
         BNE   *+8                                                              
         LA    R1,5(R1)            INCREASES WIDTH                              
         CLI   FORMOPT,C'Y'        OPTION TO SHOW STATION FORMAT                
         BNE   *+8                                                              
         LA    R1,5(R1)            INCREASES WIDTH                              
*                                                                               
         L     R4,AMYD             STION WIDTH = 9 FOR COMBO                    
         USING MYD,R4                                                           
         CLI   COMBLIST,0                                                       
         BE    *+8                                                              
         LA    R1,2(R1)                                                         
         DROP  R4                                                               
*                                                                               
         CH    R1,=H'21'           MINIMUM WIDTH OF A CHUNK IS 21               
         BH    *+8                                                              
         LA    R1,21                                                            
         ST    R1,DPTWIDTH                                                      
         SPACE 1                                                                
         LA    R1,80               GET MAX WIDTH INTO R1                        
         CLI   WIDEOPT,2                                                        
         BL    COMPD2                                                           
         LA    R1,110                                                           
         BE    COMPD2                                                           
         LA    R1,132                                                           
         SPACE 1                                                                
COMPD2   SR    R0,R0                                                            
         D     R0,DPTWIDTH         FIGURE HOW MANY WILL FIT                     
         ST    R1,UP                                                            
         BCTR  R1,0                NUMBER OF GAPS IS UP-1                       
         LR    R2,R1               (NOW IN R2)                                  
         SRDL  R0,32               REMAINING BYTES IS NOW IN R1                 
         DR    R0,R2                                                            
         ST    R1,DPTGAP           R1 NOW HAS WIDTH OF EACH GAP                 
         SRL   R0,1                                                             
         ST    R0,DISP             AND HALF THE REMAINS IS DISPLACEMENT         
         XMOD1                                                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE FIGURES OUT WHETHER DAYPART IS STANDARD                  
         SPACE 3                                                                
*              INPUTS              DPTLIST   NDPTS                              
*              OUTPUT              CHANGES DAYPART LIST TO STANDARD             
*                                  ENTRIES IF A MATCH IS FOUND                  
*                                  PICKS OUT DAYPART NAME IF NEEDED             
         SPACE 1                                                                
SETDPTS  NMOD1 0,**SETDPTS**                                                    
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         GOTOR SUBR08,DMCB,('EDITDPTE',(RC))     EDIT DAYPARTS AGAIN            
*                                  THESE ARE IN THE NMOD AREA                   
         L     R4,AMYD                                                          
         USING MYD,R4                                                           
* CHECK FOR CUMES ACTIVE - USE CUME DAYPARTS                                    
         MVI   CUMESW,0                                                         
         LA    R2,DEMOS            CHECK ALL DEMOS FOR CUME DATA                
         ZIC   R0,NDEMOS                                                        
TSTCUM   LA    RE,CUMMOD                                                        
TSTCUM2  CLC   1(1,R2),0(RE)       FOUND A CUME DEMO                            
         BE    TSTCUMY             INDICATE IT                                  
         LA    RE,1(RE)            NO KEEP SEARCHING                            
         CLI   0(RE),X'FF'         EOT                                          
         BNE   TSTCUM2                                                          
         LA    R2,3(R2)            TEST NEXT DEMO                               
         BCT   R0,TSTCUM           UNTIL EXHAUSTED                              
         B     *+8                                                              
TSTCUMY  MVI   CUMESW,C'C'                                                      
         B     SETDPTS1                                                         
CUMMOD   DC    C'CEFHJT',X'FF'                                                  
         DS    0H                                                               
*                                                                               
SETDPTS1 LA    R5,DPTLIST                                                       
         USING DPENTRY,R5                                                       
         ZIC   R0,NDPTS                                                         
         SPACE 1                                                                
SETDPTS2 BAS   RE,LUKDPT                                                        
         LA    R5,L'DPENTRY(R5)                                                 
         BCT   R0,SETDPTS2                                                      
SETDPTXT XMOD1                                                                  
         SPACE 1                                                                
LUKDPT   NTR1                      FOR EACH DAYPART IN LIST                     
         L     RF,ADDPHS60                                                      
         USING PHSE60D,RF                                                       
*                                                                               
*        L     R2,=A(STANDPTS)     LOOK UP STANDARD DAYPARTS                    
*        A     R2,RELO                                                          
*                                                                               
         CLI   DBSELSRC,C'R'       RADAR STANDARD DAYPART                       
         BNE   *+12                                                             
         L     R2,ARSTNDPT         LOAD STANDARD DPT TABLE                      
         B     *+8                                                              
         L     R2,ASTNDPTS                                                      
         CLI   DBSELSRC,C'M'       BBM HAS DIFFERENT DAYPARTS                   
         BNE   LUKDPT1                                                          
*                                                                               
*        L     R2,=A(BBMDPTS)      BBM HAS DIFFERENT DAYPARTS                   
*        A     R2,RELO                                                          
*                                                                               
         L     R2,ABBMDPTS                                                      
LUKDPT1  S     R2,APHASE60         MINUS START OF PHASE                         
         AR    R2,RF               THIS SHOULD GIVE ADDRESS                     
         USING STANDENT,R2                                                      
         MVI   DPCTYPE,C'C'        PRESET TYPE TO CUSTOM                        
         CLI   HOUROPT,C'Y'        HOURLY DATA ONLY                             
         BE    XIT                                                              
         SPACE 1                                                                
LUKDPT2  CLI   0(R2),X'FF'         IF NOT MATCH IS FOUND                        
         BE    XIT                    IT IS A CUSTOM ONE                        
         CLC   STANDDTS(15),DPCDAY LOOK FOR A MATCH ON DAY/TIMES                
         BNE   LUKDPT4                                                          
         CLI   CUMESW,0           INCLUDE CUME DAYPARTS IF CUME ACTIVE          
         BNE   *+12                                                             
         CLI   STANDESC+19,C'C'    BYPASS CUME ONLY DAYPARTS                    
         BE    LUKDPT4                                                          
         MVI   DPSTYPE,C'S'        MATCHED - SO THIS ONE IS STANDARD            
         MVC   DPSPROG,STANDPRG    ALL WE NEED IS THE PROGRAM CODE              
         XC    DPSSPARE,DPSSPARE                                                
         CLI   DPDESC,C' '         IF THERE IS NOT YET A DESCRIPTION            
         BH    XIT                                                              
         MVC   DPDESC,STANDESC     TAKE THE STANDARD DESCRIPTION                
         B     XIT                                                              
         SPACE 1                                                                
LUKDPT4  LA    R2,L'STANDENT(R2)                                                
         B     LUKDPT2                                                          
         DROP  R2                                                               
         DROP  R4                                                               
         DROP  R5                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SET UP BOXES FOR THIS SET OF DAYPARTS                 
         SPACE 3                                                                
*              INPUTS              R0=NUMBER OF DAYPARTS IN THIS CHUNK          
*                                  DISP DPTWIDTH DPTGAP                         
*                                  LINE MAXRANK                                 
*              OUTPUT              BOXCOLS BOXROWS                              
         SPACE 1                                                                
SETBOX   NMOD1 0,**SETBOX**                                                     
         L     R8,0(R1)                                                         
         L     R9,4(R1)                                                         
         L     RC,8(R1)                                                         
*                                                                               
         CLI   BOXOPT,C'N'         OPTION TO SUPPRESS                           
         BE    SETBOX_X                                                         
         L     R4,ABOX             HANDLE BOXES IF WE'RE OFF LINE               
         USING BOXD,R4                                                          
         LTR   R4,R4                                                            
         BZ    SETBOX_X                                                         
         SPACE 1                                                                
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         SPACE 1                                                                
         MVC   BOXROWS,SPACES                                                   
         ZIC   R1,LINE             PICK UP PRESENT LINE NUMBER                  
         BCTR  R1,0                                                             
         LA    R1,BOXROWS(R1)      AND SET THE ROWS                             
         MVI   0(R1),C'T'                                                       
         MVI   3(R1),C'M'                                                       
         ZIC   RE,MAXRANK                                                       
         LA    R1,4(R1,RE)                                                      
         MVI   0(R1),C'B'                                                       
         SPACE 1                                                                
         MVC   BOXCOLS,SPACES      NOW DO THE COLUMNS                           
         LA    R1,BOXCOLS                                                       
         A     R1,DISP                                                          
         SPACE 1                                                                
BOXSET2  MVI   0(R1),C'L'          LEFT                                         
         A     R1,DPTWIDTH                                                      
         BCTR  R1,0                                                             
         MVI   0(R1),C'R'          AND RIGHT FOR EACH DAYPART                   
         LA    R1,1(R1)                                                         
         A     R1,DPTGAP                                                        
         BCT   R0,BOXSET2                                                       
SETBOX_X XMOD1                                                                  
         EJECT                                                                  
*              SET UP TITLES FOR DAYPARTS                                       
         SPACE 3                                                                
*              INPUTS              R0=NUMBER OF DAYPARTS IN THIS SET            
*                                  R2=A(DPENTRIES)                              
*              OUTPUTS             TITLES WITH SURROUNDING BOXES                
         SPACE 1                                                                
DPTITLES NMOD1 0,**DPTITLES**                                                   
         L     R2,0(R1)                                                         
         L     R8,4(R1)                                                         
         L     R9,8(R1)                                                         
         L     RC,12(R1)                                                        
*                                                                               
         USING DPENTRY,R2                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R3,P+1                                                           
         A     R3,DISP                                                          
         SPACE 1                                                                
DPTIT2   CLC   DPDESC,SPACES                                                    
         BE    DPTITNXT                                                         
         OC    DPDESC,SPACES                                                    
         MVC   0(19,R3),DPDESC                                                  
         L     R4,DPTWIDTH                                                      
         SH    R4,=H'2'                                                         
         GOTO1 CENTER,DMCB,(R3),(R4)                                            
         LR    RE,R3                                                            
         OC    ABOX,ABOX                                                        
         BZ    DPTIT4                                                           
         CLI   BOXOPT,C'N'         IF NO BOXES                                  
         BNE   DPTITNXT                                                         
         SPACE 1                                                                
DPTIT4   CLI   0(RE),C' '          NEST DAYPART NAME IN DASHES                  
         BNE   DPTIT6                                                           
         MVI   0(RE),C'-'                                                       
         LA    RE,1(RE)                                                         
         B     DPTIT4                                                           
         SPACE 1                                                                
DPTIT6   LR    RE,R3                                                            
         AR    RE,R4                                                            
         BCTR  RE,0                                                             
         SPACE 1                                                                
DPTIT8   CLI   0(RE),C' '                                                       
         BNE   DPTITNXT                                                         
         MVI   0(RE),C'-'                                                       
         BCT   RE,DPTIT8                                                        
         SPACE 1                                                                
DPTITNXT BAS   RE,DPSUBTIT                                                      
         LA    R2,L'DPENTRY(R2)                                                 
         A     R3,DPTWIDTH                                                      
         A     R3,DPTGAP                                                        
         BCT   R0,DPTIT2                                                        
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XMOD1                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*              SUBTITLES (DEMO CATEGORIES ETC.) FOR DAYPART                     
         SPACE 3                                                                
*              INPUT               R3=A(TITLE IN PRINT LINE)                    
*                                  CATTITS LIST OF 5 BYTE CAT TITLES            
*                                  DEMOS  NDEMOS                                
         SPACE 1                                                                
DPSUBTIT NTR1                                                                   
         LA    R3,132(R3)          SPACE TO P2                                  
         MVC   0(7,R3),=C'STATION'                                              
         LA    R3,8(R3)                                                         
         L     R4,AMYD                                                          
         USING MYD,R4                                                           
         CLI   COMBLIST,0                                                       
         BE    *+8                                                              
         LA    R3,2(R3)                                                         
         DROP  R4                                                               
         SPACE 1                                                                
         CLI   FORMOPT,C'Y'        OPTION TO SHOW FORMAT                        
         BNE   DPST2                                                            
         CLI   WSTATS+3,9                                                       
         BL    DPST2                                                            
         MVC   0(4,R3),=C'FORM'                                                 
         LA    R3,5(R3)                                                         
         SPACE 1                                                                
DPST2    CLI   RANKOPT,C'Y'        OPTION TO SHOW RANK                          
         BNE   DPST4                                                            
         MVC   0(4,R3),=C'RANK'                                                 
         LA    R3,5(R3)                                                         
         SPACE 1                                                                
DPST4    LA    R2,CATTITS                                                       
         LA    R4,DEMOS                                                         
         ZIC   R0,NDEMOS                                                        
         SPACE 1                                                                
DPST6    MVC   0(5,R3),0(R2)       NOW POP IN N CATEGORIES                      
         CLI   DEMSTYLE,C'C'                                                    
         BE    DPST8               OR AGE/SEX TITLES                            
         GOTO1 DEMOCON,DMCB,(R4),(5,WORK),DBLOCK,0                              
         MVC   0(5,R3),WORK                                                     
         SPACE 1                                                                
DPST8    LA    R2,5(R2)                                                         
         LA    R3,6(R3)                                                         
         LA    R4,3(R4)                                                         
         BCT   R0,DPST6                                                         
         B     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
MKTSTAT  NMOD1 0,**MKTSTAT**                                                    
         L     R2,0(R1)                                                         
         L     R9,4(R1)                                                         
         L     RC,8(R1)                                                         
*                                                                               
         MVI   4(R2),C'A'                                                       
         LH    RF,MKTNUM                                                        
         CVD   RF,DUB                                                           
         UNPK  0(4,R2),DUB                                                      
         OI    3(R2),X'F0'                                                      
*                                                                               
MKTXIT   XMOD1                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO BUILD STATIONS IN MARKET                              
         SPACE 3                                                                
*              INPUT               MKTNUM                                       
*              OUTPUTS             ASTATS = A(100 5 BYTE STATIONS)              
*                                  NSTATS   ACTUAL NUMBER OF STATIONS           
         SPACE 1                                                                
         DS    0H                                                               
EXPSTATS NMOD1 0,**EXPSTATS**                                                   
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         CLI   STASTYLE,C'M'       WHEN MARKET HAS BEEN SELECTED                
         BNE   EXPSTX                                                           
         CLI   CMB_B4,C'Y'                                                      
         BE    EXPSTA1                                                          
         CLI   NSTATS,0                                                         
         BNE   EXPSTX                                                           
EXPSTA1  MVC   ATHISTAT,ASTATS                                                  
         MVI   NSTATS,0                                                         
*        CLI   DBSELSRC,C'M'       CANADA DOESN'T HAVE MARKET RECORDS           
*        BE    EXPSTA1C                                                         
*        CLI   DEMOS,2             TSA DOESN'T HAVE MARKET RECORDS              
*        BE    EXPSTA1C                                                         
*        CLI   NOSHARE,C'Y'        TSA DOESN'T HAVE MARKET RECORDS              
*        BE    EXPSTA1C                                                         
*                                                                               
*        L     RF,ATHISTAT                                                      
*        MVI   4(RF),C'A'          CREATE MARKET STATION                        
*        LH    RE,MKTNUM                                                        
*        CVD   RE,DUB                                                           
*        UNPK  0(4,RF),DUB                                                      
*        OI    3(RF),X'F0'                                                      
*                                                                               
*        LA    RF,5(RF)                                                         
*        ST    RF,ATHISTAT                                                      
*        MVI   NSTATS,1                                                         
*        ZIC   RF,MAXRANK                                                       
*        LA    RF,1(RF)                                                         
*        CH    RF,=H'40'                                                        
*        BL    *+8                                                              
*        LA    RF,40                                                            
*        STC   RF,MAXRANK                                                       
*                                                                               
EXPSTA1C MVC   DBAREC,AIO1                                                      
         MVI   DBFUNCT,DBGETMS                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'R'                                                    
*                                                                               
         ZIC   RF,NBOOKS         BASE BOOK LIST ON LATEST BOOK IN LIST          
         OR    RF,RF                                                            
         BZ    EXPSTX                                                           
         LA    RE,BOOKS                                                         
         XC    DBSELBK,DBSELBK                                                  
         XC    PREVSTAT,PREVSTAT                                                
EXPSTAT2 CLC   DBSELBK,1(RE)                                                    
         BH    EXPSTAT4                                                         
         MVC   DBSELBK,1(RE)                                                    
         MVC   DBBTYPE,3(RE)                                                    
EXPSTAT4 LA    RE,4(RE)                                                         
         BCT   RF,EXPSTAT2                                                      
*                                                                               
         L     R4,DBAREC                                                        
         USING MLKEY,R4                                                         
         GOTO1 DEMAND,DMCB,DBLOCK,EXPHOOK                                       
EXPSTX   XMOD1                                                                  
         SPACE 1                                                                
EXPHOOK  NTR1                                                                   
         CLI   EXPANOPT,C'Y'       OPTION TO REPORT STATS INCLUDED IN           
         BE    *+12                COMBOS                                       
         BAS   RE,FLTCMB                                                        
         BE    XIT                                                              
*                                                                               
         CLI   SIMULOPT,C'N'       EXCLUDE SIMULCAST STATIONS?                  
         BNE   EXPHOOK1                                                         
         CLI   MLSTAT+4,C'B'       BYE,BYE IF SIMULCAST                         
         BE    XIT                                                              
         CLI   MLSTAT+4,C'C'       BYE,BYE IF SIMULCAST                         
         BE    XIT                                                              
         CLI   MLSTAT+4,C'D'       BYE,BYE IF SIMULCAST                         
         BE    XIT                                                              
*                                                                               
EXPHOOK1 CLI   MLSTAT,C'Z'         CUT OUT MARKET TOTALS                        
         BH    XIT                                                              
         CLI   SPILLOPT,C'Y'       OPTION TO SHOW SPILL ONLY                    
         BNE   EXPHOOK2                                                         
         CLI   MLHOME,C'S'                                                      
         BNE   XIT                                                              
         SPACE 1                                                                
EXPHOOK2 CLI   SPILLOPT,C'N'       OPTION TO OMIT SPILL                         
         BNE   EXPHOOK4                                                         
         CLI   MLHOME,C'S'                                                      
         BE    XIT                                                              
         SPACE 1                                                                
EXPHOOK4 L     R2,ATHISTAT                                                      
         CLC   MLSTAT,PREVSTAT                                                  
         BE    XIT                                                              
         MVC   0(5,R2),MLSTAT                                                   
         MVC   PREVSTAT,MLSTAT                                                  
         OC    MLKMKT,MLKMKT       NOTE SPILL                                   
         BZ    *+8                                                              
         NI    4(R2),X'7F'         BY TURNING OFF X'80' BIT                     
         LA    R2,5(R2)                                                         
         ST    R2,ATHISTAT                                                      
         AI    NSTATS,1                                                         
         CLI   NSTATS,100                                                       
         BNH   XIT                                                              
         DC    H'0'                OUT OF SPACE IN STATION LIST                 
         SPACE 2                                                                
FLTCMB   NTR1                                                                   
         L     RE,AMYD                                                          
         USING MYD,RE                                                           
         LA    R1,1                                                             
         LA    R7,COMBLIST                                                      
FLTCMB1  CLI   0(R7),0             END OF COMBOS                                
         BE    FLTCMBX                                                          
         LA    R2,9(R7)            POINT TO STATIONS                            
FLTCMB2  CLI   0(R2),0             END OF STATIONS                              
         BE    FLTCMB3                                                          
         CLC   MLSTAT,0(R2)                                                     
         BNE   *+10                                                             
         SR    R1,R1                                                            
         B     FLTCMBX                                                          
         LA    R2,5(R2)                                                         
         B     FLTCMB2                                                          
*                                                                               
FLTCMB3  LA    R7,LNCOMB(R7)       NEXT COMBO LIST                              
         B     FLTCMB1                                                          
*                                                                               
FLTCMBX  LTR   R1,R1                                                            
         B     XIT                                                              
         DROP  RE                                                               
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
         PRINT NOGEN                                                            
SUBR07   NMOD1 0,**SR07**,RA,RR=R7                                              
         USING MYD,R3                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R3,AMYD                                                          
         USING MYD,R3                                                           
         ST    R7,RELO2            SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE2                                                       
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         MVC   CONHEAD(L'CONHEAD),SPACES                                        
         L     R1,0(R1)                                                         
         SRL   R1,24               GET THE ROUTINE NUMBER                       
         SLL   R1,2                AND BRANCH ADDRESS                           
         LA    RF,*+2(R1)                                                       
         BR    RF                                                               
EDITMLE  EQU   (EDITML#-*)/4+1                                                  
WRTWAE   EQU   (WRTWA#-*)/4+1                                                   
RESTWAE  EQU   (RESTWA#-*)/4+1                                                  
M1INITE  EQU   (M1INIT#-*)/4+1                                                  
*ENDPTE  EQU   (GENDPT#-*)/4+1                                                  
EDDYTME  EQU   (EDDYTM#-*)/4+1                                                  
EDITMBE  EQU   (EDITMB#-*)/4+1                                                  
EDITSBE  EQU   (EDITSB#-*)/4+1                                                  
TESTMKE  EQU   (TESTMK#-*)/4+1                                                  
         SPACE 1                                                                
EDITML#  B     EDITML         2    EDIT A USER LIS                              
WRTWA#   B     WRTWA          3    WRITE A TWA (TWANUM=WHICH ONE)               
RESTWA#  B     RESTWA         4    RESTORE A TWA                                
M1INIT#  B     M1INIT         5    INITIALIZE A MKT LIST SCREEN                 
*ENDPT#  B     GENDPT         6    GENERATE DAYPART SCREEN                      
EDDYTM#  B     EDDYTM         7    EDIT A DAY TIME LIST                         
EDITMB#  B     EDITMB         8    VALIDATE A LIST OF BOOKS FOR MKT             
EDITSB#  B     EDITSB         9    VALIDATE A LIST OF BOOKS FOR STATION         
TESTMK#  B     TESTMRKT      13    TEST MARKET                                  
         EJECT                                                                  
*                                                                               
HSTAT    DC    X'0'                                                             
CMBCOUNT DC    X'0'                                                             
TEMPMRKT DC    H'0'                                                             
         SPACE 2                                                                
* EDIT USER LISTS                                                               
         USING ELEMDS,R2                                                        
EDITML   DS    0H                                                               
         L     R4,AIO2                                                          
         ST    R4,SCANADDR                                                      
         LA    R4,1000(R4)                                                      
         XC    PARAS(16),PARAS                                                  
         ST    R4,PARAS+12                                                      
*                                                                               
PARDOK   CLI   ELEMSTOP,X'FF'         END OF LIST, ERROR                        
         BE    NEEDMRKT               NEED A MARKET                             
         CLI   ELEMSTOP,X'0'          DITTO                                     
         BE    NEEDMRKT                                                         
EDITML2  CLI   ELEMDATA,C'+'                                                    
         BE    EDMLC                                                            
         CLI   ELEMDATA,C'M'                                                    
         BE    EDMLM                                                            
         CLI   ELEMDATA,C'S'                                                    
         BE    EDMLS                                                            
         B     EDITML1                                                          
         SPACE 1                                                                
EDITML1  MVC   CONHEAD(L'NEEDKW),NEEDKW                                         
         LA    RE,CONHEAD+L'NEEDKW+1  GIVE SOME IDEA OF WHERE                   
         B     REGERR                                                           
         SPACE 1                                                                
EDITML1A MVC   CONHEAD(L'ERRSTA),ERRSTA                                         
         LA    RE,CONHEAD+L'ERRSTA+1  GIVE SOME IDEA OF WHERE                   
         B     REGERR                                                           
         SPACE 1                                                                
EDITML1C MVC   CONHEAD(L'ERRCBL),ERRCBL                                         
         OI    RNKUL1H+6,X'81'                                                  
         B     REGERR                                                           
         SPACE 1                                                                
NEEDMRKT MVC   CONHEAD(L'NEEDMKT),NEEDMKT                                       
         LA    RE,CONHEAD+L'NEEDMKT+1                                           
         B     REGERR                                                           
         SPACE 1                                                                
NEEDSTAL MVC   CONHEAD(L'NEEDSL),NEEDSL                                         
         LA    RE,CONHEAD+L'NEEDSL+1                                            
         MVC   0(9,RE),0(R4)       SEND THE KEYWORD                             
         B     ULXIT                                                            
NEEDSCBN MVC   CONHEAD(21),=C'FIELD LENGTH TOO LONG'                            
         B     ULXIT                                                            
NEEDCMBN MVC   CONHEAD(54),=C'NEED COMBO NAME FOR COMBOS WITH MORE THANX        
                TWO STATIONS'                                                   
         B     ULXIT                                                            
         SPACE 2                                                                
REGERR   ZIC   RF,ELEMLEN                                                       
         LTR   RF,RF                                                            
         BZ    ULXIT                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     ULXIT                                                            
         MVC   0(0,RE),ELEMDATA                                                 
         EJECT                                                                  
EDMLM    BAS   RE,NEXTELEM         NEXT ELEMENT                                 
         CLI   ELEMSTOP,X'FF'      END OF LIST?                                 
         BE    NEEDMRKT            YES, NEED MARKET ERROR                       
         CLI   ELEMSTOP,X'0'       SHORTENED LIST?                              
         BE    NEEDMRKT            YES, NEED MARKET ERROR                       
         CLI   ELEMSTOP,C'='                                                    
         BE    NEEDMRKT                                                         
MOREMRKT CLI   ELEMLEN,0           NOTHING IN DATA                              
         BZ    NEEDMRKT                                                         
*---------------------------------------------------------------                
* CHECK IF THE MARKET GIVEN IS A STATION LIST                                   
*---------------------------------------------------------------                
         LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         USING SLKEY,R5                                                         
         MVC   SLKTYPE(2),=X'0D5B'                                              
         MVC   SLKAM,BAGYMD                                                     
         MVC   SLKNAME,=CL8' '                                                  
         ZIC   RF,ELEMLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SLKNAME(0),ELEMDATA                                              
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   MKTNSTL             MARKET NOT A STATION LIST                    
         GOTO1 GETREC                                                           
         MVI   STALSTD,C'Y'                                                     
         SPACE 1                                                                
         L     R5,AIO1                                                          
         LA    R5,SLDELEM                                                       
EDMLM1   CLI   0(R5),X'01'         DESCRIPTION?                                 
         BNE   EDMLM2              NO, COULD BE THE STATIONS                    
         USING SLDELEM,R5                                                       
         SR    RF,RF                                                            
         MVC   TEMPMRKT,SLDMKTA                                                 
         CLI   CTRY,C'C'                                                        
         BE    EDMLM3                                                           
         CLI   RNKSRCE,C'B'                                                     
         BNE   EDMLM1A                                                          
         MVC   TEMPMRKT,SLDMKTB                                                 
EDMLM1A  BAS   RE,GOTTAMKT                                                      
EDMLM2   CLI   0(R5),X'05'                                                      
         BNE   EDMLM3                                                           
         USING SLSELEM,R5                                                       
         L     R4,PARAS+12                                                      
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         ST    R4,PARAS+4                                                       
         ST    R4,PARAS+12                                                      
*                                                                               
         ZIC   R1,1(R5)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),SLSELEM     MOVE ALL THE STATIONS                        
*                                                                               
         MVI   0(R4),X'04'         STATION(S)                                   
         CLI   FORMOPT,C'Y'                                                     
         BNE   *+8                                                              
         MVI   WSTATS+3,9                                                       
EDMLM3   ZIC   R1,1(R5)                                                         
         AR    R5,R1               GET NEXT ELEMENT                             
         CLI   0(R5),X'00'                                                      
         BNZ   EDMLM1                                                           
         B     EDMLM4                                                           
         SPACE 1                                                                
MKTNSTL  ZIC   RF,ELEMLEN                                                       
         LA    R3,ELEMDATA                                                      
         GOTO1 VNUMVAL,DMCB,(R3),(RF)                                           
         ZIC   RF,DMCB                                                          
         LTR   RF,RF               SEE IF INVALID                               
         BNZ   NOTYET1             ALPHA                                        
         MVC   TEMPMRKT(2),DMCB+6                                               
         LH    R1,TEMPMRKT                                                      
         B     GOTMRKT                                                          
NOTYET1  ZIC   RF,ELEMLEN          COULD BE ALPHA MARKET                        
         CH    RF,=H'3'            DATA LONGER THAN 3 CHAR                      
         BH    NEEDMRKT            YES, ALPHA MARKET AT MOST 3                  
         BCTR  RF,0                                                             
         MVC   CITYCODE,=CL3' '                                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CITYCODE(0),ELEMDATA                                             
         LA    R5,MEDTYPE                                                       
         GOTO1 VCTYMRKT                                                         
         BZ    NEEDMRKT                                                         
         STH   R1,TEMPMRKT                                                      
GOTMRKT  BAS   RE,GOTTAMKT                                                      
EDMLM4   BAS   RE,NEXTELEM                                                      
         ZIC   RE,NMARKET                                                       
         LA    RE,1(RE)                                                         
         STC   RE,NMARKET                                                       
         CLI   ELEMSTOP,C','       STILL MARKETS?                               
         BNE   EDMLM5                                                           
         CLI   ELEMDATA,C'+'                                                    
         BE    EDITML2                                                          
         B     MOREMRKT            YES GET MORE                                 
EDMLM5   CLI   ELEMSTOP,X'FF'      FINISHED, LEAVE PROGRAM                      
         JE    EQXIT                                                            
         CLI   ELEMSTOP,X'00'                                                   
         JE    EQXIT                                                            
         B     EDITML2                                                          
         EJECT                                                                  
EDMLS    XC    HSTAT,HSTAT         STATION HERE BEFORE                          
         BAS   RE,NEXTELEM         GET NEXT ELEM                                
         CLI   ELEMSTOP,X'FF'      END OF LIST?                                 
         BE    EDITML1A                                                         
         CLI   ELEMSTOP,X'00'      SHORTENED LIST?                              
         BE    EDITML1A                                                         
         CLI   ELEMSTOP,C'='                                                    
         BE    EDITML1A                                                         
EDMLS2   CLI   ELEMLEN,0                                                        
         BZ    EDITML1A                                                         
         OC    MKTNUM,MKTNUM                                                    
         BZ    NEEDMRKT                                                         
* DON'T NEED THIS NOW WITH THE PARSER                                           
*        OC    PARAS+4(4),PARAS+4  CAN'T HAVE STATIONS                          
*        BNZ   NEEDMRKT                                                         
         L     R4,PARAS+12                                                      
EDSTAT1  ZIC   RF,ELEMLEN                                                       
         LA    R3,ELEMDATA                                                      
         GOTO1 VNUMVAL,DMCB,(R3),(RF)                                           
         OC    HSTAT,HSTAT                                                      
         BZ    EDSTAT2                                                          
         CLI   DMCB,0                                                           
         BZ    NEEDSTAL                                                         
         L     R4,PARAS+4                                                       
         ZIC   R1,1(R4)                                                         
         LA    R0,5(R1)                                                         
         STC   R0,1(R4)                                                         
         AR    R4,R1                                                            
         B     EDSTAT3                                                          
EDSTAT2  CLI   DMCB,0                                                           
         BZ    NEEDSTAL                                                         
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         ST    R4,PARAS+4                                                       
         ST    R4,PARAS+12                                                      
         MVC   0(2,R4),=X'0207'                                                 
*DSTAT3  BAS   RE,NEXTELEM                                                      
*        BNE   NEEDSTAL                                                         
EDSTAT3  BAS   RE,EDMVSTA                                                       
         BNE   EDITML1A                                                         
         OC    HSTAT,HSTAT                                                      
         BZ    EDSTAT4                                                          
         MVC   0(5,R4),ACTSTAT                                                  
         B     EDSTAT5                                                          
EDSTAT4  MVC   2(5,R4),ACTSTAT                                                  
EDSTAT5  BAS   RE,NEXTELEM                                                      
         CLI   ELEMSTOP,X'FF'                                                   
         JE    EQXIT                                                            
         CLI   ELEMSTOP,X'0'                                                    
         JE    EQXIT                                                            
         CLI   ELEMSTOP,C'='                                                    
         BE    EDITML2                                                          
         CLI   ELEMDATA,C'+'                                                    
         BE    EDITML2                                                          
         OI    HSTAT,X'1'                                                       
         B     EDSTAT1                                                          
         EJECT                                                                  
EDMLC    XC    CMBCOUNT,CMBCOUNT   INIT COMBO COUNTER                           
         CLI   ELEMLEN,0           COMBO NAME AT LEAST 1                        
         BZ    EDITML1                                                          
         CLI   ELEMLEN,10          COMBO NAMES < 10 CHARS                       
         BH    NEEDSCBN                                                         
         OC    MKTNUM,MKTNUM       NEED A MARKET                                
         BNZ   *+14                                                             
         OC    PARAS(4),PARAS      FROM SOMEPLACE                               
         BZ    EDITML1                                                          
         CLI   ELEMSTOP,C'='       IS IT A STANDARD COMBO?                      
         BE    EDMLC1              YES, TREAT IT AS SUCH                        
*--------------------------------------------------------------------           
* HERE'S WHERE I HAVE TO CHECK FOR A COMBOLIST.                                 
* IF IT IS THEN ADD IN ALL THE COMBO'S IN THAT RECORD                           
* IF NOT, THEN IT IS A REGULAR COMBO                                            
*--------------------------------------------------------------------           
         LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         USING CLKEY,R5                                                         
         MVC   CLKTYPE(2),=X'0D5E'                                              
         MVC   CLKAM,BAGYMD                                                     
         MVC   CLKBTYP,BOOKTYPE                                                 
         MVC   CLKRSRV,SRCTYPE                                                  
         MVC   CLKNAME,=CL5' '                                                  
         ZIC   RF,ELEMLEN                                                       
         BCTR  RF,0                FOR '+'                                      
         LTR   RF,RF                                                            
         BZ    EDMLC1              STANDARD COMBO                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CLKNAME(0),ELEMDATA+1   COMBO NAME                               
         MVC   TEMPNAME,CLKNAME                                                 
         MVC   CLKMKAL,CITYCODE    ALPHA CODE                                   
         GOTO1 HIGH                SEARCH FOR IT                                
         CLC   KEY(13),KEYSAVE     DO WE HAVE IT?                               
         BNE   EDMLC0              NO, STANDARD COMBO                           
         GOTO1 GETREC              YES, GET THE RECORD                          
         B     EDCMB                                                            
*                                                                               
EDMLC0   XC    KEY,KEY                                                          
         MVI   SUBSID,C'N'                                                      
         GOTO1 =A(CKSUBSID),DMCB,(R9),(RC),RR=RELO                              
         BNZ   EDMLC01             NOT A SUBSID                                 
         MVI   SUBSID,C'Y'                                                      
*                                                                               
EDMLC01  XC    KEY,KEY             CLEAR KEY USED FOR REP                       
         MVI   GOTMATCH,C'N'                                                    
         USING CLKEY,R5                                                         
         MVC   CLKTYPE(2),=X'0D5E'                                              
         MVC   CLKAM,BAGYMD                                                     
         CLI   SUBSID,C'Y'         IS CURRENT AGENCY A SUBSIDARY                
*                                    OF INTEREP?                                
         BNE   AGY00002                                                         
         MVI   CLKAM,X'B2'         MAKE SURE WE USE INTEREP                     
AGY00002 MVC   CLKMKAL,CITYCODE    ALPHA CODE                                   
         GOTO1 HIGH                SEARCH FOR IT                                
*                                                                               
EDMLC0A  CLC   KEY(6),KEYSAVE      FIRST PART MATCHING?                         
         BE    EDMLC0B                                                          
         CLI   GOTMATCH,C'Y'       DID WE GET SOMETHING CLOSE?                  
         BE    EDCMB               YES, LEAVE LOOP AND CONTINUE                 
         B     EDMLCERR                                                         
*                                                                               
EDMLC0B  CLC   CLKNAME,TEMPNAME    ARE THE COMBO NAMES EQUAL?                   
         BNE   EDMLC0X                                                          
         MVI   GOTMATCH,C'Y'                                                    
         GOTO1 GETREC                                                           
         CLC   CLKBTYP,BOOKTYPE                                                 
         BNE   EDMLC0X                                                          
         B     EDCMB                                                            
EDMLC0X  GOTO1 SEQ                                                              
         B     EDMLC0A                                                          
*-----------------------------------------------------------------              
EDCMB    L     R5,AIO1                                                          
         SR    R1,R1                                                            
         BAS   RE,CKCMBLST                                                      
         BE    EDCMB_1                                                          
         LTR   R1,R1                                                            
         BZ    EDITML1C                                                         
         B     EDMLCERR                                                         
EDCMB_1  LA    R5,CLDELEM                                                       
EDCMB0   CLI   0(R5),X'00'         END OF LIST?                                 
         BE    EDCMBOK                                                          
EDCMB1   CLI   0(R5),X'01'         DESCRIPTION?                                 
         BNE   EDCMB2              NO, MAYBE AN ELEMENT                         
         USING CLDELEM,R5                                                       
EDCMB1A  ZIC   R0,CLDLEN                                                        
         AR    R5,R0               SKIP DESCRIPTIONS                            
         B     EDCMB0                                                           
EDCMB2   CLI   0(R5),X'05'         ELEMENT?                                     
         BNE   EDCMB1A             UNDEFINED                                    
         USING CLSELEM,R5                                                       
         L     R4,PARAS+12                                                      
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         ST    R4,PARAS+4                                                       
         ST    R4,PARAS+12                                                      
         ZIC   RF,CLSLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),CLSELEM                                                  
         MVI   0(R4),X'03'         STATION CODE                                 
         ZIC   R0,CLSLEN                                                        
         AR    R5,R0                                                            
         B     EDCMB0                                                           
EDCMB3   ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     EDCMB0                                                           
EDCMBOK  BAS   RE,NEXTELEM                                                      
         CLI   ELEMSTOP,C','       MORE COMBOS?                                 
         BE    EDMLC                                                            
         CLI   ELEMSTOP,X'FF'                                                   
         JE    EQXIT                                                            
         CLI   ELEMSTOP,X'00'                                                   
         JE    EQXIT                                                            
         B     EDITML2                                                          
*                                                                               
EDMLCERR MVC   CONHEAD(L'CMBELMER),CMBELMER                                     
         LA    RE,CONHEAD+L'CMBELMER+1                                          
         MVC   0(5,RE),TEMPNAME                                                 
         B     ULXIT                                                            
*---------------------------------------------------------------                
EDMLC1   L     R4,PARAS+12         GET LAST ELEMENT                             
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         MVC   0(2,R4),=X'0310'    SET COMBO ELEMENT                            
* NEED EX STATMENT ON NEXT ONE                                                  
         MVC   2(9,R4),=CL9' '      SET COMBO CAPTION                           
         ZIC   RF,ELEMLEN                                                       
         LTR   RF,RF                                                            
         BZ    EDCOMB0                                                          
         BCTR  RF,0                ONE FOR '+'                                  
         LTR   RF,RF                                                            
         BNZ   EDMLC1A                                                          
         ST    R2,DISP                                                          
         B     EDCOMB0                                                          
EDMLC1A  BCTR  RF,0                ANOTHER FOR EX INSTRUCTION                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R4),ELEMDATA+1  SET COMBO CAPTION                            
EDCOMB0  BAS   RE,NEXTELEM                                                      
         CLI   ELEMSTOP,X'FF'                                                   
         BE    EDITML1A                                                         
         CLI   ELEMSTOP,X'0'                                                    
         BZ    EDITML1A                                                         
         CLI   ELEMSTOP,C'='                                                    
         BE    EDITML1A                                                         
EDCOMB1  BAS   RE,EDMVSTA                                                       
         BNE   EDITML1A                                                         
*        OC    CMBCOUNT,CMBCOUNT                                                
*        BZ    EDCOMB2                                                          
*        MVC   0(5,R4),ACTSTAT                                                  
*        B     EDCOMB3                                                          
EDCOMB2  MVC   11(5,R4),ACTSTAT                                                 
EDCOMB3  ST    R4,PARAS+12                                                      
         ST    R4,PARAS+8                                                       
EDCOMB4  ZIC   RF,CMBCOUNT                                                      
         LA    RF,1(RF)                                                         
         STC   RF,CMBCOUNT                                                      
         BAS   RE,NEXTELEM                                                      
         CLI   ELEMSTOP,X'FF'                                                   
         BE    TESTEND                                                          
         CLI   ELEMSTOP,X'0'                                                    
         BE    TESTEND                                                          
         CLI   ELEMDATA,C'+'       A NEW COMBO?                                 
         BE    TESTEND             YES, PREPARE FOR A NEW COMBO                 
         CLI   ELEMSTOP,C'='                                                    
         BNE   EDMLC2                                                           
TESTEND  CLI   CMBCOUNT,2                                                       
         BL    NEEDSTAL                                                         
         L     R4,PARAS+12                                                      
         CLC   2(9,R4),=CL9' '                                                  
         BNE   TESTENDX                                                         
         CLI   CMBCOUNT,2                                                       
         BE    TESTEND1                                                         
         L     R2,DISP                                                          
         B     NEEDCMBN                                                         
TESTEND1 CLC   11(4,R4),16(R4)     STATION CALL LETTERS SAME?                   
         BNE   TESTEND5                                                         
         MVC   2(4,R4),11(R4)      YES, PUT '/AF' BEHIND IT                     
         MVC   6(3,R4),=C'/AF'                                                  
         B     TESTENDX                                                         
TESTEND5 MVC   2(4,R4),11(R4)      NO, SEPARATE WITH '/'                        
         MVI   6(R4),C'/'                                                       
         MVC   7(4,R4),16(R4)                                                   
TESTENDX CLI   ELEMSTOP,C'='                                                    
         BE    EDITML2                                                          
         CLI   ELEMDATA,C'+'                                                    
         BE    EDITML2                                                          
         J     EQXIT                                                            
EDMLC2   ZIC   RF,ELEMLEN                                                       
         LA    R3,ELEMDATA                                                      
         GOTO1 VNUMVAL,DMCB,(R3),(RF)                                           
         CLI   DMCB,0                                                           
         BZ    NEEDMRKT                                                         
         L     R4,PARAS+12                                                      
         ZIC   R1,1(R4)                                                         
         CLI   1(R4),46                                                         
         BL    EDMLC3                                                           
         B     NEEDMRKT                                                         
EDMLC3   LR    R0,R1                                                            
         LA    R1,5(R1)                                                         
         STC   R1,1(R4)                                                         
         AR    R4,R0                                                            
         BAS   RE,EDMVSTA                                                       
         BNE   EDITML1A                                                         
         MVC   0(5,R4),ACTSTAT                                                  
*        DC    H'0'                                                             
         B     EDCOMB4                                                          
         SPACE 3                                                                
TESTMRKT CLI   ELEMSTOP,C'='       SEE IF M= IS FIRST PARAM                     
         BNE   NEEDMRKT                                                         
         CLI   ELEMDATA,C'M'                                                    
         BNE   NEEDMRKT                                                         
         BAS   RE,NEXTELEM         NEXT ELEMENT                                 
         CLI   ELEMSTOP,X'FF'      END OF LIST?                                 
         BE    NEEDMRKT            YES, NEED MARKET ERROR                       
         CLI   ELEMSTOP,X'0'       SHORTENED LIST?                              
         BE    NEEDMRKT            YES, NEED MARKET ERROR                       
         CLI   ELEMSTOP,C'='                                                    
         BE    NEEDMRKT                                                         
         CLI   ELEMLEN,0           NOTHING IN DATA                              
         BZ    NEEDMRKT                                                         
         ZIC   RF,ELEMLEN                                                       
         LA    R3,ELEMDATA                                                      
         GOTO1 VNUMVAL,DMCB,(R3),(RF)                                           
         ZIC   RF,DMCB                                                          
         LTR   RF,RF               SEE IF INVALID                               
         BNZ   NOTYET2             ALPHA                                        
         MVC   TEMPMRKT(2),DMCB+6                                               
         LH    R1,TEMPMRKT                                                      
         B     TESTMXIT                                                         
NOTYET2  ZIC   RF,ELEMLEN          COULD BE ALPHA MARKET                        
         CH    RF,=H'3'            DATA LONGER THAN 3 CHAR                      
         BH    NOTYET3             YES, ALPHA MARKET AT MOST 3                  
         BCTR  RF,0                                                             
         MVC   CITYCODE,=CL3' '                                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CITYCODE(0),ELEMDATA                                             
         LA    R5,MEDTYPE                                                       
         GOTO1 VCTYMRKT                                                         
         BZ    NEEDMRKT                                                         
         STH   R1,TEMPMRKT                                                      
         B     TESTMXIT                                                         
*---------------------------------------------------------------                
NOTYET3  LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         USING SLKEY,R5                                                         
         MVC   SLKTYPE(2),=X'0D5B'                                              
         MVC   SLKAM,BAGYMD                                                     
         MVC   SLKNAME,=CL8' '                                                  
         ZIC   RF,ELEMLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SLKNAME(0),ELEMDATA                                              
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NEEDMRKT            MARKET NOT A STATION LIST                    
         GOTO1 GETREC                                                           
         L     R5,AIO1                                                          
         LA    R5,SLDELEM                                                       
         USING SLDELEM,R5                                                       
         MVC   TEMPMRKT,SLDMKTA                                                 
         LH    R1,TEMPMRKT                                                      
         CLI   CTRY,C'C'                                                        
         BE    TESTMXIT                                                         
         CLI   RNKSRCE,C'A'                                                     
         BE    TESTMXIT                                                         
         MVC   TEMPMRKT,SLDMKTB                                                 
         LH    R1,TEMPMRKT                                                      
         SPACE 1                                                                
TESTMXIT SR    R0,R0                                                            
         LTR   R0,R0                                                            
         XIT1  REGS=(R1)                                                        
         SPACE 3                                                                
OLDADDSP DC    XL3'0'                                                           
NEXTELEM NTR1                      GETS NEXT ELEMENT OF PARSED LIST             
         SPACE 1                                                                
         MVC   OLDADDSP(3),2(R2)                                                
         CLI   0(R2),X'FF'         DON'T GET NEXT ELEMENT                       
         BE    NXTELMER            IF END OF LIST OR SHORTENED LIST             
         CLI   0(R2),X'0'                                                       
         BZ    NXTELMER                                                         
         ZIC   RF,1(R2)                                                         
         LA    RF,ELEMBDAT(RF)                                                  
         AR    R2,RF                                                            
         CR    R2,R2                                                            
         B     NXTELMXT                                                         
NXTELMER LA    R1,1                                                             
         LTR   R1,R1                                                            
NXTELMXT XIT1  REGS=(R2)                                                        
         SPACE 3                                                                
EDMVSTA  NTR1                                                                   
         MVC   THISLINE(8),RNKUL1H                                              
         MVC   THISLINE+8(7),=CL7' '                                            
         ZIC   RF,1(R2)                                                         
         LTR   RF,RF                                                            
         BNZ   EDMVSTA1                                                         
         CLI   1(R2),1             CAUSE AN ERROR CODE                          
         J     XIT                                                              
EDMVSTA1 BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   THISLINE+8(0),ELEMBDAT(R2)                                       
         MVI   THISLINE+4,X'C0'                                                 
         MVC   THISLINE+5(1),1(R2)                                              
*                                                                               
         LA    R2,THISLINE                                                      
         GOTOR SUBR08,DMCB,('VALDSTAE',(RC))                                    
         SR    R1,R1                                                            
         CLI   DBERROR,X'10'                                                    
         BNE   *+8                                                              
         LA    R1,1                                                             
         LTR   R1,R1                                                            
         J     XIT                                                              
         SPACE 1                                                                
ULXIT    CLI   0(R2),X'FF'                                                      
         BE    ULXIT1                                                           
         CLI   0(R2),X'0'                                                       
         BNZ   ULXIT2                                                           
ULXIT1   MVC   2(3,R2),OLDADDSP    INSURE CURSOR POINTS SOMEWHERE               
ULXIT2   LA    R1,1                                                             
         LTR   R1,R1                                                            
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
M1INIT   MVC   CONHEAD(31),=C'SELECT DESIRED MARKETS (5 MAX.)'                  
         LA    RE,BUFF                                                          
         USING SVTD,RE                                                          
         XC    BUFF(200),BUFF                                                   
         MVC   SVTSRC,DBSELSRC                                                  
         MVC   SVTBOOK,BOOKS+1                                                  
         MVC   SVTBTYP,BOOKS+3                                                  
         MVC   SVTFILE,DBFILE                                                   
         MVC   SVTMED(1),DBSELMED                                               
         MVC   SVTFLTR,RNKMKT                                                   
         SPACE 1                                                                
         MVI   TWANUM,2            SAVE IN TWA 2                                
         MVC   COMAND2,=CL8'DMWRT'                                              
         GOTOR SUBR07,DMCB,('WRTWAE',(RC))                                      
         JNE   NEQXIT                                                           
*                                  GET MARKET LIST SCREEN                       
         GOTO1 CALLOV,DMCB,(X'A3',CONTAGH),0,0                                  
         LA    R2,RMKMK1H                                                       
         MVC   RMKWORK(200),BUFF                                                
         J     EQXIT                                                            
         EJECT                                                                  
*   TWA HANDLING STUFF                                                          
WRTWA    MVC   COMAND2,=C'DMWRT   '                                             
         B     TWAIO                                                            
         SPACE 1                                                                
RESTWA   MVC   COMAND2,=C'DMREAD  '                                             
         SPACE 1                                                                
TWAIO    XC    DMCB(8),DMCB                                                     
         ZIC   R2,TWANUM                                                        
         SLL   R2,32-8                                                          
         ICM   R2,3,TERM                                                        
         GOTO1 DATAMGR,DMCB,COMAND2,=C'TEMPSTR',(R2),ATWA,0                     
         CLI   8(R1),0                                                          
         JE    EQXIT                                                            
         J     NEQXIT                                                           
         EJECT                                                                  
*   SCREEN GENERATORS                                                           
*ENDPT   SR    RF,RF               COUNT THE ACTIVE DAYPARTS                    
*        L     RE,=A(STANDPTS)                                                  
*        A     RE,RELO                                                          
*        CLI   0(RE),X'FF'         END OF TABLE                                 
*        BE    *+16                                                             
*        LA    RE,L'STANDENT(RE)                                                
*        LA    RF,1(RF)                                                         
*        B     *-16                                                             
*        STC   RF,BYTE             SAVE NUMBER OF DAYPARTS                      
*        L     RE,AIO2             AREA FOR TWABLD LIST                         
*        LA    RF,1000                                                          
*        XCEF                                                                   
*        ZIC   R0,BYTE                                                          
*        SR    R5,R5                                                            
*        SR    R3,R3                                                            
*ENDPT3  CH    R3,=H'3'            3 GROUPS PER LINE                            
*        BNE   *+6                                                              
*        SR    R3,R3                                                            
*        LR    R4,R3               SET INDEX                                    
*        SLL   R4,1                *2                                           
*        LA    R4,DPDISP(R4)                                                    
*        MVC   DPL1+3(1),0(R4)     POS FIELD 1                                  
*        MVC   DPL2+3(1),1(R4)     POS FIELD 2                                  
*        LR    R4,R5               RESET INDEX FOR NAME                         
*        MH    R4,=AL2(L'STANDENT)                                              
*        A     R4,=A(STANDPTS)     POINT TO NAME                                
*        A     R4,RELO                                                          
*        USING STAND,R4                                                         
*        MVC   DPL2C,STANDESC      AND MOVE IT IN                               
*        DROP  R4                                                               
*        LA    R4,DPL1             SCREEN LINE 1                                
*        PRINT GEN                                                              
*ENDPT4  GOTO1 HELLO,DMCB,(C'P',=C'CORETAB'),AIO2,(R4),=C'ADD=END'              
*        PRINT NOGEN                                                            
*        CLI   16(R1),0                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        ZIC   RE,1(R4)            NEXT FIELD                                   
*        AR    R4,RE                                                            
*        CLI   0(R4),X'FF'         END OF PAIRS                                 
*        BNE   GENDPT4                                                          
*        LA    R5,1(R5)                                                         
*        LA    R3,1(R3)                                                         
*        BCT   R0,GENDPT3          DO IT FOR NUMBER OF ENTRIES                  
*        L     R4,AIO2                                                          
*        LA    R4,2(R4)                                                         
*        GOTO1 VTWABLD,DMCB,ATWA,(R4),CONTAGH,2304                              
*        J     EQXIT                                                            
         EJECT                                                                  
EDITMB   LA    R4,BOOKS            CHECK MARKET FOR ALL BOOKS                   
         CLI   DBSELSRC,C'M'       BBM RADIO DOESN'T HAVE MKT LEVELS            
         JE    EQXIT                                                            
         SPACE 1                                                                
EDITMB2  CLI   1(R4),0             EOL                                          
         JE    EQXIT                                                            
         MVC   DBAREC,AIO1                                                      
         MVI   DBFUNCT,DBVLSTBK                                                 
         MVC   DBSELBK,1(R4)                                                    
         MVC   DBBTYPE,3(R4)                                                    
         LH    R0,MKTNUM           SET STATION = MARKET                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DBSELSTA(4),DUB+5(3)                                             
         MVI   DBSELSTA+4,C'A'                                                  
         MVC   DBSELMK,MKTNUM      AND MARKET IN KEY                            
         MVC   DBFILE,=C'TP '                                                   
         GOTO1 DEMAND,DMCB,DBLOCK,0                                             
         CLI   DBERROR,X'10'       NOT FOUND --ERROR                            
         JE    NEQXIT                                                           
         LA    R4,4(R4)            NEXT BOOK                                    
         B     EDITMB2                                                          
         SPACE 2                                                                
EDITSB   LA    R4,BOOKS            CHECK STATION OR ALL BOOKS                   
         CLI   DBSELSRC,C'M'       BBM RADIO DOESN'T HAVE MKT LEVELS            
         JE    EQXIT                                                            
         SPACE 1                                                                
EDITSB2  CLI   1(R4),0             EOL                                          
         JE    EQXIT                                                            
         MVC   DBAREC,AIO1                                                      
         MVI   DBFUNCT,DBVLSTBK                                                 
         MVC   DBSELSTA,ACTSTAT    SET STATION                                  
         MVC   DBSELBK,1(R4)       BOOK                                         
         MVC   DBBTYPE,3(R4)       BOOK TYPE                                    
         MVC   DBSELMK,MKTNUM      AND MARKET IN KEY                            
         MVC   DBFILE,=C'TP '                                                   
         GOTO1 DEMAND,DMCB,DBLOCK,0                                             
         CLI   DBERROR,X'10'       NOT FOUND --ERROR                            
         JE    NEQXIT                                                           
         LA    R4,4(R4)            NEXT BOOK                                    
         B     EDITSB2                                                          
         EJECT                                                                  
*              EDIT DAYPART                                                     
         SPACE 3                                                                
*              AGENCY SPECIFIED    DAY/TIMES INPUT                              
         SPACE 1                                                                
EDDYTM   LA    R2,CMBDESCR                                                      
         ST    R2,DPTTABL                                                       
         MVI   NCMBDPT,C'1'                                                     
         ST    R2,CMBDESC                                                       
         LA    R2,1(R2)                                                         
         ST    R2,CMBDESCX                                                      
         LA    R2,RNKDPTH                                                       
         MVI   NDPTS,0                                                          
         MVI   CMBDPT,C'N'                                                      
         GOTO1 ANY                                                              
         ZIC   R0,5(R2)                                                         
         LTR   R0,R0                                                            
         BZ    EDDT2ZZ                                                          
         LA    R8,DPTLIST                                                       
         USING DPENTRY,R8                                                       
         LR    R5,R8                                                            
         SPACE 1                                                                
EDDT2    LA    R4,8(R2)                                                         
         ZIC   R0,5(R2)                                                         
EDDT2A   CLI   0(R4),C'/'                                                       
         BE    EDDT2D                                                           
         CLI   0(R4),C' '                                                       
         BE    EDDT2D                                                           
         B     EDDT2Z                                                           
EDDT2D   MVI   0(R4),C','                                                       
EDDT2Z   LA    R4,1(R4)                                                         
         BCT   R0,EDDT2A                                                        
*                                  TRY TO FIX BAD CHARS                         
EDDT2ZZ  LA    RE,RNKDPT                                                        
         LA    R4,RNKDPT+1                                                      
         ZIC   R1,RNKDPTH+5                                                     
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    EDDT7                                                            
EDDT3    CLI   0(R4),C'+'                                                       
         BE    EDDT5                                                            
         CLI   0(R4),C'-'                                                       
         BNL   EDDT5                                                            
         CLI   0(RE),C','                                                       
         BE    EDDT5                                                            
         CLI   1(R4),C','                                                       
         BE    EDDT5                                                            
         CLI   1(R4),0                                                          
         BE    EDDT5                                                            
         MVI   0(R4),C','                                                       
EDDT5    LA    R4,1(R4)                                                         
         LA    RE,1(RE)                                                         
         BCT   R1,EDDT3                                                         
*                                                                               
EDDT7    MVI   SCANLEN,32                                                       
         XC    BLOCK(250),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(15,BLOCK)                                     
         LA    R4,BLOCK                                                         
         SPACE 1                                                                
EDDT10   ZIC   R0,0(R4)                                                         
         MVI   DPTNUM,0                                                         
         LR    R8,R5                                                            
         LA    R8,DPCDAY                                                        
         CLI   1(R4),0                                                          
         BE    EDDT11                                                           
         BAS   RE,EDDT50           SAVE THE CAPTION                             
         MVC   0(1,R4),1(R4)       SLIDE THE FIELDS                             
         MVC   12(10,R4),12+10(R4)                                              
         MVI   DPTNUM,X'FF'                                                     
         MVI   1(R4),0                                                          
         SPACE 1                                                                
EDDT11   CLI   12(R4),C'('         IS THIS A COMBO DAYPART?                     
         BNE   EDDT13                                                           
         CLI   CMBDPT,C'Y'         PROCESSING A COMBO DAYPART?                  
         JE    NEQXIT              ERROR, NO NESTED COMBOS                      
         MVI   CMBDPT,C'Y'                                                      
         ST    R8,CMBTR8           SAVE R8 FOR LATER                            
         XC    CMBNDPT,CMBNDPT                                                  
         MVI   CMBDPTND,C'N'                                                    
         L     R7,CMBDESC                                                       
         STC   R0,0(R7)                                                         
         L     R7,CMBDESCX                                                      
         LR    R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),12(R4)                                                   
         AR    R7,R0                                                            
         MVI   0(R7),C'/'                                                       
         LA    R7,1(R7)                                                         
         ST    R7,CMBDESCX                                                      
*                                                                               
         SH    R0,=H'1'            LESS ONE FOR C'('                            
         STC   R0,0(R4)                                                         
         LR    R7,R0                                                            
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R4),13(R4)                                                  
EDDT13   GOTO1 DAYVAL,DMCB,((R0),12(R4)),BYTE,WORK                              
EDDT13A  CLI   BYTE,0                                                           
         BNE   EDDT20              NON-ZERO MEANS VALID INPUT                   
         SPACE 1                                                                
         BAS   RE,EDDT50                                                        
         BAS   RE,FRIENDLY         CHECK FOR EASY EXPRESSIONS                   
         BE    EDDT40                                                           
         LA    R1,SPCLDAY          CHECK FOR SPECIAL DAYS                       
         LA    R0,SPCLDAYL                                                      
         SPACE 1                                                                
EDDT14   CLC   12(4,R4),1(R1)                                                   
         BNE   EDDT16                                                           
         MVC   BYTE,0(R1)          SET VALID INPUT                              
         B     EDDT20                                                           
         SPACE 1                                                                
EDDT16   LA    R1,1(R1)                                                         
         BCT   R0,EDDT14                                                        
         J     NEQXIT                                                           
         SPACE 1                                                                
SPCLDAY  DS    0CL6                                                             
         DC    X'03',CL5'S-S  '    DAY VALUE/INPUT STRING                       
         DC    X'03',CL5'SA-S '                                                 
         DC    X'03',CL5'S-SU '                                                 
         DC    X'7F',CL5'M-S  '                                                 
         DC    X'81',CL5'ALL  '                                                 
SPCLDAYL EQU   *-SPCLDAY                                                        
         SPACE 1                                                                
EDDT20   BAS   RE,EDDT50                                                        
         MVC   COMAND2,12(R4)                                                   
         LA    R1,DAYLIST                                                       
         LA    R0,DAYLISTL                                                      
         SPACE 1                                                                
EDDT22   CLC   BYTE,0(R1)                                                       
         BE    EDDT24                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,EDDT22                                                        
         J     NEQXIT                                                           
         SPACE 1                                                                
DAYLIST  DC    X'7C'               MO-FR                                        
         DC    X'7E'               MO-SA                                        
         DC    X'7F'               MO-SU                                        
         DC    X'03'               SA-SU                                        
         DC    X'02'               SAT                                          
         DC    X'01'               SUN                                          
         DC    X'81'               ALL                                          
DAYLISTL EQU   *-DAYLIST                                                        
CMBDPT   DC    C'N'                ANY COMBO DAYPARTS?                          
CMBNDPT  DC    X'00'               # OF DAYPARTS IN COMBO DAYPART               
CMBDPTND DC    C'N'                COMBO DAYPART ENDS?                          
CMBTR8   DC    F'0'                COMBO TEMP R8                                
CMBDESC  DC    F'0'                                                             
CMBDESCX DC    F'0'                                                             
CMBDESCR DC    XL70'00'            DESCRIPTION                                  
         SPACE 1                                                                
EDDT24   MVC   0(1,R8),BYTE        MOVE DAY TO LIST                             
         LA    R4,32(R4)           NEXT SCANNER ENTRY                           
         CLI   0(R4),0                                                          
         BNE   EDDT26                                                           
         J     NEQXIT              CAN'T HAVE JUST DAYS, ERROR                  
         SPACE 1                                                                
EDDT26   BAS   RE,EDDT60           GET LENGTH OF NEW ELEMENT                    
         BAS   RE,VALTIME          CHECKS IF THE HOURS ARE VALID                
         JNE   NEQXIT                                                           
         SPACE 1                                                                
EDDT28   CLI   CMBDPT,C'Y'                                                      
         BNE   EDDT28A                                                          
         L     R7,CMBTR8                                                        
         MVC   1(2,R7),HALF                                                     
         MVC   0(1,R7),BYTE                                                     
         L     R7,CMBDESCX                                                      
         ZIC   R3,0(R4)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),12(R4)                                                   
         AR    R7,R3                                                            
         LA    R7,1(R7)                                                         
         MVI   0(R7),C','                                                       
         LA    R7,1(R7)                                                         
         ST    R7,CMBDESCX                                                      
         B     EDDT28B                                                          
EDDT28A  MVC   1(2,R8),HALF        MOVE TIME TO ELEMENT                         
EDDT28B  BAS   RE,EDDT50                                                        
         SPACE 1                                                                
         CLI   CMBDPT,C'Y'                                                      
         BNE   EDDT28E                                                          
         CLI   CMBDPTND,C'Y'                                                    
         BE    EDDT28E                                                          
         AI    CMBNDPT,1                                                        
         CLI   CMBNDPT,4                                                        
         BL    EDDT28C                                                          
         J     NEQXIT                                                           
EDDT28C  L     R7,CMBTR8                                                        
         A     R7,=F'3'                                                         
         ST    R7,CMBTR8                                                        
         B     EDDT28X                                                          
EDDT28E  AI    NDPTS,1                                                          
         CLI   CMBDPT,C'Y'                                                      
         BNE   EDDT28H                                                          
         L     R7,CMBDESCX                                                      
         BCTR  R7,0                                                             
         MVI   0(R7),C')'                                                       
         S     R7,CMBDESC                                                       
         L     R3,CMBDESC                                                       
         STC   R7,0(R3)                                                         
         XC    16(20,R5),16(R5)    CLEAR THE DESCRIPTION                        
         CLI   0(R3),19                                                         
         BH    EDDT28G                                                          
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   16(0,R5),1(R3)                                                   
         EX    R7,*+8                                                           
         B     *+10                                                             
         XC    0(0,R3),0(R3)                                                    
         B     EDDT28H                                                          
EDDT28G  MVC   16(16,R5),=C'CUSTOM DAYPART #'                                   
         MVC   32(1,R5),NCMBDPT                                                 
         ZIC   R3,NCMBDPT                                                       
         LA    R3,1(R3)                                                         
         STC   R3,NCMBDPT                                                       
         L     R7,CMBDESC                                                       
         ZIC   R3,0(R7)                                                         
         AR    R7,R3                                                            
         LA    R7,1(R7)                                                         
         ST    R7,CMBDESC                                                       
EDDT28H  MVI   CMBDPT,C'N'                                                      
         MVI   CMBNDPT,X'00'                                                    
         MVI   CMBDPTND,C'N'                                                    
         L     R7,CMBDESC                                                       
         LA    R7,1(R7)                                                         
         ST    R7,CMBDESCX                                                      
         LA    R8,L'DPENTRY(R5)                                                 
         LR    R5,R8                                                            
         LA    R8,DPCDAY                                                        
EDDT28X  LA    R4,32(R4)                                                        
         CLI   1(R4),0             KEYWORD                                      
         BNE   EDDT10               YES - PROCESS IT                            
         CLI   0(R4),0             TEST ANY MORE INPUT THIS FIELD               
         BNE   EDDT29                                                           
         J     EQXIT                                                            
         SPACE 1                                                                
EDDT29   CLI   NDPTS,15                                                         
         BE    EDDT30                                                           
         SPACE 1                                                                
         BAS   RE,EDDT60                                                        
         BAS   RE,VALTIME          VALIDATE FOR ANOTHER TIME                    
         BNE   EDDT10              IF INVALID, TRY FOR ANOTHER DAY              
         SPACE 1                                                                
         MVC   DPDESC-DPCDAY(5,R8),COMAND2                                      
         MVI   DPTNUM,5                                                         
*        LR    RE,R8                                                            
*        SH    RE,=AL2(L'DPENTRY)                                               
         MVC   0(1,R8),BYTE        COPY DAY FROM PREVIOUS                       
         B     EDDT28               AND CONTINUE                                
         SPACE 1                                                                
EDDT30   MVI   ERROR,TOOMANY                                                    
         J     NEQXIT                                                           
         SPACE 1                                                                
EDDT40   CLI   CMBDPT,C'Y'                                                      
         BNE   EDDT41                                                           
         CLI   CMBDPTND,C'Y'                                                    
         BE    EDDT41                                                           
         AI    CMBNDPT,1                                                        
         CLI   CMBNDPT,4                                                        
         BL    EDDT41                                                           
         J     NEQXIT                                                           
EDDT41   AI    NDPTS,1                                                          
         LA    R4,32(R4)                                                        
EDDT42   CLI   0(R4),0                                                          
         JE    EQXIT                                                            
         LA    R8,L'DPENTRY(R5)                                                 
         LR    R5,R8                                                            
         B     EDDT10                                                           
         SPACE 1                                                                
EDDT50   ZIC   R1,DPTNUM           LENGTH SO FAR                                
         LTR   R1,R1                                                            
         BZ    *+8                                                              
         LA    R1,1(R1)                                                         
         ZIC   RF,0(R4)            INPUT LENGTH                                 
         LR    R0,RF                                                            
         AR    R0,R1               NEW LENGTH                                   
         CH    R0,=H'19'                                                        
         BHR   RE                                                               
         STC   R0,DPTNUM                                                        
         AR    R1,R8               DEST ADDR                                    
         BCTR  RF,R0               FOR EXECUTE                                  
         EX    RF,*+6                                                           
         BR    RE                                                               
         MVC   DPDESC-DPCDAY(0,R1),12(R4)                                       
         SPACE 3                                                                
EDDT60   NTR1                                                                   
         ZIC   R0,0(R4)            GET LENGTH OF NEW ELEMENT                    
         SH    R0,=H'1'                                                         
         LR    R7,R4                                                            
         AR    R7,R0                                                            
         CLC   =C')',12(R7)                                                     
         BNE   EDDT60A                                                          
         MVI   CMBDPTND,C'Y'       DAYPART IS GOING TO END                      
         STC   R0,0(R4)                                                         
         MVI   12(R7),C' '                                                      
EDDT60A  J     XIT                                                              
         EJECT                                                                  
*              VALIDATE FOR FRIENDLY DAYPART EXPRESSION                         
         SPACE 3                                                                
FRIENDLY NTR1                                                                   
         L     RF,ADDPHS60         GET ADDRESS OF OVERLAY 60                    
         USING PHSE60D,RF                                                       
*                                                                               
         CLI   DBSELSRC,C'M'       CANADIAN DAYPARTS?                           
         BE    CFRENDLY                                                         
         L     R1,AFRIENDS         GET FRIENDLY DAYPART EXPRESSIONS             
         S     R1,APHASE60                                                      
         AR    R1,RF                                                            
         SPACE 1                                                                
FRIEND2  CLC   0(8,R1),12(R4)                                                   
         BE    FRIEND4                                                          
         LA    R1,23(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         JE    NEQXIT                                                           
         B     FRIEND2                                                          
         SPACE 1                                                                
FRIEND4  MVC   0(15,R8),8(R1)                                                   
         J     EQXIT                                                            
         SPACE 1                                                                
*FRIENDS  DC    C'AMDRIVE ',X'7C',AL1(06,10),12X'00'                            
*        DC    C'PMDRIVE ',X'7C',AL1(15,19),12X'00'                             
*        DC    C'DRIVE   ',X'7C',AL1(06,10),X'7C',AL1(15,19),9X'00'             
*        DC    X'FF'                                                            
         EJECT                                                                  
CFRENDLY L     R1,ACFRENDS         USE CANADIAN DAYPART EXPRESSIONS             
         S     R1,APHASE60                                                      
         AR    R1,RF                                                            
*CFRENDLY L     R1,=A(CFRENDS)                                                  
*        A     R1,RELO                                                          
         SPACE 1                                                                
CFREND2  CLC   0(8+7,R1),12(R4)                                                 
         BE    CFREND4                                                          
         LA    R1,23+7(R1)                                                      
         CLI   0(R1),X'FF'                                                      
         JE    NEQXIT                                                           
         B     CFREND2                                                          
         SPACE 1                                                                
CFREND4  MVC   0(15,R8),8+7(R1)                                                 
         J     EQXIT                                                            
         SPACE 1                                                                
         EJECT                                                                  
*CFRENDS  DC    C'BREAKFAST      ',X'7C',AL1(01,01),12X'00'                     
*        DC    C'DAY            ',X'7C',AL1(02,02),12X'00'                      
*        DC    C'DRIVE          ',X'7C',AL1(03,03),12X'00'                      
*        DC    C'EVENING        ',X'7C',AL1(04,04),12X'00'                      
*        DC    C'BR+DA          ',X'7C',AL1(09,09),12X'00'                      
*        DC    C'BR+DR          ',X'7C',AL1(10,10),12X'00'                      
*        DC    C'BR+EV          ',X'7C',AL1(11,11),12X'00'                      
*        DC    C'DA+DR          ',X'7C',AL1(12,12),12X'00'                      
*        DC    C'DA+EV          ',X'7C',AL1(13,13),12X'00'                      
*        DC    C'DR+EV          ',X'7C',AL1(14,14),12X'00'                      
*        DC    C'BR+DA+DR       ',X'7C',AL1(15,15),12X'00'                      
*        DC    C'BR+DA+EV       ',X'7C',AL1(16,16),12X'00'                      
*        DC    C'BR+DR+EV       ',X'7C',AL1(17,17),12X'00'                      
*        DC    C'DA+DR+EV       ',X'7C',AL1(18,18),12X'00'                      
*        DC    C'BR+DA+DR+EV    ',X'7C',AL1(19,19),12X'00'                      
*        DC    C'SATURDAY       ',X'02',AL1(05,05),12X'00'                      
*        DC    C'SUNDAY         ',X'01',AL1(06,06),12X'00'                      
*        DC    C'SA+SU          ',X'03',AL1(20,20),12X'00'                      
*        DC    C'BR+SA          ',X'7E',AL1(21,21),12X'00'                      
*        DC    C'DA+SA          ',X'7E',AL1(22,22),12X'00'                      
*        DC    C'DR+SA          ',X'7E',AL1(23,23),12X'00'                      
*        DC    C'BR+DR+SA       ',X'7E',AL1(24,24),12X'00'                      
*        DC    C'BR+SA+SU       ',X'7F',AL1(25,25),12X'00'                      
*        DC    C'DA+SA+SU       ',X'7F',AL1(26,26),12X'00'                      
*        DC    C'DR+SA+SU       ',X'7F',AL1(27,27),12X'00'                      
*        DC    C'EV+SA+SU       ',X'7F',AL1(28,28),12X'00'                      
*        DC    C'BR+DA+DR+SA    ',X'7E',AL1(29,29),12X'00'                      
*        DC    C'BR+DA+SA+SU    ',X'7F',AL1(30,30),12X'00'                      
*        DC    C'BR+DA+DR+SA+SU ',X'7F',AL1(31,31),12X'00'                      
*        DC    C'BR+DA+DR+EV+SA ',X'7E',AL1(32,32),12X'00'                      
*        DC    C'ALL            ',X'7F',AL1(33,33),12X'00'                      
*        DC    C'REACH          ',X'7F',AL1(07,07),12X'00'                      
*        DC    C'M-S 5A-1A      ',X'7F',AL1(07,07),12X'00'                      
*        DC    X'FF'                                                            
         DS    0D                                                               
         DROP  R8                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE A TIME EXPRESSION                            
         SPACE 3                                                                
VALTIME  NTR1                      VALIDATE TIME EXPRESSION                     
         ZIC   R0,0(R4)                                                         
         CLI   1(R4),0             TIME RANGE, NO 2ND FIELD ALLOWED             
         BZ    *+8                                                              
         J     NEQXIT                                                           
         GOTO1 TIMVAL,DMCB,((R0),12(R4)),FULL                                   
         CLI   0(R1),X'FF'                                                      
         JE    NEQXIT                                                           
         SPACE 1                                                                
         MVI   ERROR,BADSTTIM                                                   
         LH    R0,FULL                                                          
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LTR   R0,R0               TIME MUST BE HOUR ONLY                       
         JNZ   NEQXIT                                                           
         STC   R1,HALF             SET START HOUR                               
         STC   R1,HALF+1           PRESET END TIME                              
         SPACE 1                                                                
         MVI   ERROR,BADNDTIM                                                   
         SR    R0,R0                                                            
         ICM   R0,3,FULL+2                                                      
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LTR   R0,R0                                                            
         JNZ   NEQXIT                                                           
         LTR   R1,R1               LEAVE PRESET IF NO END TIME                  
         JZ    EQXIT                                                            
         STC   R1,HALF+1           SET END HOUR                                 
         J     EQXIT                                                            
         EJECT                                                                  
*------------------------------------------------------------------             
* GOTTAMKT, SAVE MARKET NUMBER FOR EDMLM                                        
*------------------------------------------------------------------             
GOTTAMKT NTR1                                                                   
         L     R4,PARAS+12                                                      
         ZIC   R3,1(R4)                                                         
         AR    R4,R3                                                            
         SR    R3,R3                                                            
         XC    PARAS(16),PARAS                                                  
         ST    R4,PARAS                                                         
         ST    R4,PARAS+12                                                      
         MVC   0(2,R4),=X'0104'                                                 
         LH    R1,TEMPMRKT                                                      
         STCM  R1,3,2(R4)                                                       
         MVC   DBSELMK,TEMPMRKT                                                 
         OC    MKTNUM,MKTNUM                                                    
         BNZ   GOTTAMXT                                                         
         MVC   MKTNUM,TEMPMRKT                                                  
GOTTAMXT CLC   RNKBOOK+1(4),=C'BOOK'                                            
         JNE   XIT                                                              
         GOTO1 =A(FMULTBK),DMCB,(R9),(RC),TEMPMRKT,RR=RELO                      
         J     XIT                                                              
         EJECT                                                                  
CKCMBLST NTR1                                                                   
         USING CLKEY,R5                                                         
         MVI   ACTELOPT,C'N'                                                    
CKCMB0   LA    R5,CLDELEM                                                       
         MVI   NUMCMBS,0           CLEAR # OF COMBOS                            
CKCMB1   CLI   0(R5),0             END OF LIST?                                 
         BE    CKCMB6                                                           
CKCMB2   CLI   0(R5),1             DESCRIPTION ELEMENT?                         
         BNE   CKCMB3                                                           
         USING CLDELEM,R5                                                       
CKCMB2A  ZIC   R0,CLDLEN                                                        
         AR    R5,R0                                                            
         B     CKCMB1                                                           
CKCMB3   CLI   0(R5),X'05'                                                      
         BNE   CKCMB2A                                                          
*                                                                               
         AI    NUMCMBS,1           INCREMENT # OF COMBOS                        
         MVI   NUMSTAT,0           CLEAR # OF STATIONS                          
         MVI   CHNGLST,C'N'        COMBO LIST DOESN'T NEED CHANGES              
         MVI   CHNGLSTC,11         MINIMUM W/O STATIONS                         
         MVC   NWCMBLST,0(R5)      SAVE OLD ELEM KEY                            
         XC    NEWCLIST,NEWCLIST   CLEAR NEW COMBO LIST                         
         LA    R8,NEWCLIST                                                      
*                                                                               
         USING CLSELEM,R5                                                       
         ZIC   R4,CLSLEN                                                        
         AR    R4,R5               LIMIT OF ELEMENT                             
         LA    R3,CLSTALST         FIRST STATION IN CMBLIST                     
CKCMB4   MVC   THISLINE(8),RNKUL1H    MAKE UP A FAKE FIELD HEADER               
         MVC   THISLINE+8(5),0(R3)                                              
         MVI   THISLINE+5,5        LENGTH OF STATION                            
         MVI   THISLINE+4,X'C0'                                                 
         LA    R2,THISLINE                                                      
         GOTOR SUBR08,DMCB,('VALDSTAE',(RC))                                    
         BNE   CKCMB5              A STATION IS INVALID                         
         AI    NUMSTAT,1           INCREMENT # OF STATIONS                      
         MVC   0(5,R8),0(R3)       SAVE VALID STATION                           
         LA    R8,5(R8)            WIDTH OF STATION                             
         ZIC   R0,CHNGLSTC                                                      
         AH    R0,=H'5'                                                         
         STC   R0,CHNGLSTC                                                      
CKCMB4A  LA    R3,L'CLSTALST(R3)   GET NEXT STATION                             
         CR    R3,R4                                                            
         BL    CKCMB4                                                           
         CLI   CHNGLST,C'Y'        WAS COMBO LIST CHANGED?                      
         BNE   CKCMB4B             NO, CONTINUE                                 
         GOTO1 GETREC                                                           
         GOTO1 HELLO,DMCB,(C'D',=C'SPTFIL'),(X'05',AIO1),(9,NWCMBLST+2)C        
               ,0                                                               
         CLI   NUMSTAT,1           ONE STATION IS NOT A COMBO                   
         BNH   CKCMB4A5                                                         
         MVC   NWCMBLST+1(1),CHNGLSTC   COPY NEW LENGTH                         
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL'),AIO1,NWCMBLST,0,0,0                 
CKCMB4A5 GOTO1 PUTREC                                                           
         L     R5,AIO1                                                          
         B     CKCMB0                                                           
CKCMB4B  LR    R5,R4                                                            
         B     CKCMB1              KEEP DOING UNTIL LIST FINISHED               
*                                                                               
CKCMB5   CLI   PFKEY,4                                                          
         BE    CKCMB5A                                                          
         CLI   AUTOOPT,C'Y'                                                     
         JNE   NEQXIT                                                           
* PUSH THE REST OF THE LIST TO THE LEFT                                         
CKCMB5A  MVI   CHNGLST,C'Y'        LIST WAS CHANGED                             
         B     CKCMB4A                                                          
*                                                                               
CKCMB6   DS    0H                                                               
* HAVE TO CHECK IF THERE WERE ANY COMBOS                                        
         CLI   NUMCMBS,0                                                        
         JNZ   EQXIT                                                            
         MVI   RDUPDATE,C'Y'       MAKE SURE IT'S DELETED                       
         GOTO1 GETREC              NO COMBOS, HAVE TO DELETE IT                 
         L     R5,AIO1                                                          
         USING CLKEY,R5                                                         
         OI    CLRCNTL,X'80'                                                    
         GOTO1 PUTREC                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R5)                                                    
         DROP  R5                                                               
         MVI   RDUPDATE,C'Y'       MAKE SURE KEY IS DELETED                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+13,X'80'                                                     
         GOTO1 WRITE                                                            
         LA    R1,1                THIS IS WHEN CMBLIST DELETED                 
         LTR   R1,R1                                                            
         XIT1  REGS=(R1)           GIVE BACK ERROR CODE                         
         SPACE 3                                                                
CHNGLST  DC    C'N'                                                             
CHNGLSTC DC    X'00'                                                            
NWCMBLST DC    XL11'00'                                                         
NEWCLIST DC    XL240'00'                                                        
NUMSTAT  DC    X'00'                                                            
NUMCMBS  DC    X'00'                                                            
         EJECT                                                                  
TEMPNAME DC    CL5' '                                                           
CMBELMER DC    C'COULD NOT FIND COMBO ELEMENT'                                  
ERRCBL   DC    C'COMBO LIST STATION(S) INVALID, PRESS PF4 FOR AUTO-DELEX        
               TION'                                                            
GOTMATCH DC    C' '                                                             
ERRSTA   DC    C'INVALID STATION FOR BOOK OR MARKET'                            
NEEDSL   DC    C'STATION LIST REQUIRED'                                         
NEEDKW   DC    C'KEYWORD MISSING'                                               
NEEDMKT  DC    C'MARKET MISSING'                                                
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   DAYPART SCREEN GENERATOR STUFF                                              
         SPACE 1                                                                
DPDISP   DC    AL1(1,3)            FIRST                                        
         DC    AL1(25,27)          SECOND                                       
         DC    AL1(52,54)          THIRD                                        
         SPACE 1                                                                
DPL1     DC    AL1(3,7,0,1,1,INTNOR,0)     SELECT FIELD                         
DPL2     DC    AL1(3,7+20,0,3,20,PROT,0)   NAME FIELD                           
DPL2C    DC    CL20' '                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
         DROP  RA                                                               
         EJECT                                                                  
         DS    0H                                                               
FMULTBK  NMOD1 0,**FMULTBK**                                                    
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
         L     RE,8(R1)                                                         
*                                                                               
         MVI   NBOOKS,0                                                         
         MVC   BADMULP(1),RNKBOOK                                               
         XC    BOOKS,BOOKS                                                      
         LH    RE,0(RE)                                                         
         STH   RE,TMPMKT                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DBSELSTA(4),DUB+5(3)                                             
         MVI   DBSELSTA+4,C'A'     BY CONVENTION                                
         MVC   DBSELRMK,TMPMKT                                                  
*                                                                               
         LA    R1,1                                                             
         CLC   RNKBOOK+1(4),=C'BOOK'                                            
         JNE   XIT                                                              
         MVC   BYTE(1),RNKBOOK     SET NUMBER OF BOOKS                          
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,4                                                           
         BH    FMULTBKX                                                         
         CLI   RNKBOOK+5,C'('                                                   
         BNE   *+10                                                             
         MVC   DBBTYPE,RNKBOOK+6                                                
*                                                                               
         MVI   DBFUNCT,DBGETMB     GET BOOKS FOR MARKET                         
         GOTO1 DEMAND,DMCB,DBLOCK,BKHOOKF                                       
         LA    R1,1                                                             
         CLI   NBOOKS,0            NOT FOUND                                    
         BE    FMULTBKX                                                         
         SPACE 1                                                                
         XC    WORK,WORK                                                        
         CLC   NBOOKS,BYTE         NOT ENOUGH BOOKS                             
         BNE   FMULTBKX                                                         
*                                                                               
         ZIC   R0,BYTE             DON'T ALLOW PREV. YEAR                       
         LA    RF,BOOKS                                                         
FMULTBK2 ZIC   R1,2(RF)            GET THE BOOK                                 
         LA    R1,WORK(R1)                                                      
         CLI   0(R1),0                                                          
         BE    *+12                                                             
         LA    R1,2                SET ERROR NUMBER                             
         B     FMULTBKX                                                         
*                                                                               
         MVI   0(R1),C'0'                                                       
         LA    RF,4(RF)                                                         
         BCT   R0,FMULTBK2                                                      
*                                                                               
         SR    R1,R1                                                            
FMULTBKX LTR   R1,R1                                                            
         STC   R1,BYTE                                                          
         XMOD1                                                                  
*                                                                               
BKHOOKF  NTR1                                                                   
         L     R4,DBAREC                                                        
         USING SBKEY,R4                                                         
         TM    SBBOOK,X'80'        BYPASS REVERSE SEQ BOOKS                     
         JO    XIT                                                              
         CLC   SBBTYP,DBBTYPE                                                   
         JNE   XIT                                                              
         ZIC   R1,NBOOKS                                                        
         LA    R1,1(R1)                                                         
         CLC   NBOOKS,BYTE         BOOK TABLE FULL - NEED A SLIDE               
         BNE   BKHOOK2F                                                         
         MVC   BOOKS(L'BOOKS-4),BOOKS+4                                         
         IC    R1,NBOOKS                                                        
*                                                                               
BKHOOK2F STC   R1,NBOOKS                                                        
         BCTR  R1,0                                                             
         SLL   R1,2                *4                                           
         LA    R1,BOOKS(R1)                                                     
         MVC   1(2,R1),SBBOOK                                                   
         MVC   3(1,R1),SBBTYP                                                   
         J     XIT                                                              
         SPACE 3                                                                
TMPMKT   DC    H'0'                                                             
         LTORG                                                                  
         SPACE 2                                                                
         DROP  R7                                                               
         EJECT                                                                  
*--------------------------------------------------------------*                
* CKSUBSID: CHECK IF CURRENT AGENCY IS SUBSIDARY OF INTEREP    *                
*--------------------------------------------------------------*                
CKSUBSID NMOD1 0,**CKSUBSID**                                                   
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         BAS   RE,SWTCHREP                                                      
         BNZ   CKSBSDER                                                         
         LA    RE,KEY                                                           
         USING RREPKEY,RE                                                       
         MVI   RREPKTYP,1                                                       
         MVC   RREPKREP,=C'IR'     INTEREP                                      
         BAS   RE,REPHIGH                                                       
         BNZ   CKSBSDER                                                         
         CLC   KEY(27),KEYSAVE                                                  
         BNE   CKSBSDER                                                         
         L     R6,AIO1                                                          
         BAS   RE,REPGETRC                                                      
         MVC   DATADISP,=H'34'                                                  
         MVI   ELCODE,2            JUST NEED SUBSIDS                            
         BRAS  RE,GETEL                                                         
         BNE   CKSBSDER            THERE HAS TO BE A 2 ELEMENT                  
C        USING RREPSUB,R6                                                       
         LLC   R0,C.RREPSCNT       # OF SUBSIDS                                 
         LA    R6,C.RREPSCOD                                                    
CKSUBS   CLC   AGENCY,0(R6)        IS AGENCY IN SUBSID LIST?                    
         BE    CKSUBXST            YES, LEAVE                                   
         LA    R6,2(R6)                                                         
         BCT   R0,CKSUBS                                                        
         DROP  C                                                                
CKSBSDER BAS   RE,SWTCHSPT                                                      
         LA    R1,1                                                             
         LTR   R1,R1                                                            
         XMOD1                                                                  
CKSUBXST BAS   RE,SWTCHSPT                                                      
         SR    R1,R1                                                            
         XMOD1                                                                  
         EJECT                                                                  
*----------------------------------------------------------                     
* SWITCH INTO REP SYSTEM TO CHECK SUBSIDARIES OF INTEREP                        
*----------------------------------------------------------                     
SWTCHREP NTR1                                                                   
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CSWITCH                                                       
         DROP  RE                                                               
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVI   DMCB,X'08'          REP SYSTEM #                                 
         GOTO1 (RF),DMCB           JUMP INTO REP                                
*                                                                               
         CLI   4(R1),0             CHECK IF SYSTEM IS NOT OPEN                  
         JE    EQXIT                                                            
         J     NEQXIT              THERE WAS AN ERROR                           
         EJECT                                                                  
*----------------------------------------------------------                     
* SWITCH BACK TO SPOT SYSTEM                                                    
*----------------------------------------------------------                     
SWTCHSPT NTR1                                                                   
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CSWITCH                                                       
         DROP  RE                                                               
*                                                                               
         GOTO1 (RF),DMCB,=C'SPOT',0                                             
*                                                                               
         CLI   4(R1),0             CHECK IF SYSTEM IS NOT OPEN                  
         BE    *+6                                                              
         DC    H'0'                                                             
         J     EQXIT               EVERYTHING OK                                
         EJECT                                                                  
*----------------------------------------------------------                     
* DATA MANAGER CALLS TO USE IN REP SYSTEM                                       
*----------------------------------------------------------                     
REPHIGH  MVC   COMMAND,=C'DMRDHI'                                               
         B     REPDIR                                                           
*                                                                               
REPDIR   NTR1                                                                   
         IC    R4,DMINBTS                                                       
         IC    R3,TERM                                                          
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPDIR  ',KEYSAVE,KEY,   X        
               ((R3),0),0                                                       
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         JNZ   NEQXIT                                                           
         J     EQXIT                                                            
*                                                                               
REPGETRC MVC   COMMAND,=C'GETREC'                                               
         NTR1                                                                   
         LA    R2,KEY+28           GET DISK ADDRESS                             
         IC    R3,TERM                                                          
         IC    R4,DMINBTS                                                       
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPFIL  ',               X        
               (R2),AIO1,((R3),DMWORK),0                                        
         J     EQXIT                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         PRINT NOGEN                                                            
SUBR08   NMOD1 0,**SR08**,RA,RR=R7                                              
         USING MYD,R3                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R3,AMYD                                                          
         USING MYD,R3                                                           
         ST    R7,RELO2            SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE2                                                       
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         MVC   CONHEAD(L'CONHEAD),SPACES                                        
         L     R1,0(R1)                                                         
         SRL   R1,24               GET THE ROUTINE NUMBER                       
         SLL   R1,2                AND BRANCH ADDRESS                           
         LA    RF,*+2(R1)                                                       
         BR    RF                                                               
VALDSTAE EQU   (VALDSTA#-*)/4+1                                                 
EDITOPTE EQU   (EDITOPT#-*)/4+1                                                 
EDITDPTE EQU   (EDITDPT#-*)/4+1                                                 
SETUNIVE EQU   (SETUNIV#-*)/4+1                                                 
SETUNIBE EQU   (SETUNIB#-*)/4+1                                                 
SETNEWUE EQU   (SETNEWU#-*)/4+1                                                 
SETPHASE EQU   (SETPHAS#-*)/4+1                                                 
         SPACE 1                                                                
VALDSTA# B     VALDSTA        1    VALIDATE A STAT. ON DEM FILE                 
EDITOPT# B     EDITOPT        2    VALIDATE THE OPTIONS                         
EDITDPT# B     EDITDPT        3    VALIDATE THE OPTIONS                         
SETUNIV# B     SETUNIVA       4    EDIT THE UNIVERSES                           
SETUNIB# B     SETUNIVB       5    EDIT THE UNIVERSES                           
SETNEWU# B     SETNEWUN       6    EDIT THE UNIVERSES                           
SETPHAS# B     SETPHAS6       7    LOAD OVERLAY 60, DATA                        
         EJECT                                                                  
* VALIDATE STATION EXPRESSIONS (WTAE,WTAE/103,WABC-A)                           
*  USING DEMO FILE STATIONS AND MARKETS                                         
*                                                                               
VALDSTA  XC    ACTSTAT,ACTSTAT                                                  
         XC    ACTMKT,ACTMKT                                                    
*                                                                               
         CLI   5(R2),0                                                          
         JE    XIT                                                              
*                                                                               
         LA    R4,BLOCK                                                         
         XC    0(64,R4),0(R4)      CLEAR SCANNER TABLE                          
         GOTO1 SCANNER,DMCB,(R2),(2,(R4)),C',=/-'                               
*                                                                               
         TM    2(R4),X'80'         TEST VALID NUMERIC                           
         BO    BADSTA              YES - INPUT IS NOT VALID                     
*                                                                               
         CLI   0(R4),5             MUST BE STAT+BAND                            
         BNE   *+14                                                             
         MVC   ACTSTAT(5),12(R4)                                                
         B     DSTA10                                                           
*                                                                               
         CLI   0(R4),3                                                          
         BL    BADSTA                                                           
         CLI   0(R4),4                                                          
         BH    BADSTA                                                           
         TM    2(R4),X'40'         TEST ALPHA (MSPACK REQUIREMENT)              
         BNZ   DSTA1                                                            
         MVC   ACTSTAT(4),12(R4)   COULD BE TOTALING STATION                    
         MVC   ACTSTAT+4(1),22(R4)                                              
         B     DSTA10                                                           
DSTA1    MVC   ACTSTAT(4),12(R4)                                                
*                                                                               
         CLI   DBSELMED,C'R'       TEST RADIO                                   
         BE    DSTA6                                                            
         CLI   1(R4),0             TEST MEDIA ENTERED                           
         BNE   DSTA4                                                            
         MVI   ACTSTAT+4,C'T'                                                   
         B     DSTA10                                                           
*                                                                               
DSTA4    MVC   ACTSTAT+4(1),22(R4)                                              
         SR    R5,R5                                                            
         IC    R5,1(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R4),=C'TV' *EXECUTED*                                       
         BE    DSTA10                                                           
         B     BADSTA                                                           
         SPACE 2                                                                
DSTA6    CLI   1(R4),2                                                          
         BH    BADSTA                                                           
         MVC   ACTSTAT+4(1),22(R4)                                              
         ZIC   R5,1(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,DSTAAM                                                        
         BE    DSTA10                                                           
         EX    R5,DSTAFM                                                        
         BE    DSTA10                                                           
         EX    R5,DSTACO                                                        
         BE    DSTA10                                                           
         B     BADSTA                                                           
DSTAAM   CLC   22(0,R4),=C'AM'                                                  
DSTAFM   CLC   22(0,R4),=C'FM'                                                  
DSTACO   CLC   22(0,R4),=C'CO'                                                  
         SPACE 1                                                                
* READ STATION RECORD                                                           
*                                                                               
DSTA10   MVC   DBAREC,AIO1                                                      
         MVI   DBFUNCT,DBVLSTBK                                                 
         CLI   NBOOKS,1            IS THERE ONLY 1 BOOK?                        
         BH    DSTA13              NO, USE THE HIGHEST BOOK FOR VAL.            
         MVC   DBSELBK,BOOKS+1                                                  
         MVC   DBBTYPE,BOOKS+3                                                  
         B     DSTA15                                                           
DSTA13   ZIC   RE,NBOOKS           GET THE HIGHEST BOOK                         
         BCTR  RE,0                                                             
         LA    R1,BOOKS                                                         
         LA    RF,BOOKS                                                         
DSTA14   LA    RF,4(RF)                                                         
         CLC   1(2,R1),1(RF)                                                    
         BH    DSTA14A                                                          
         LR    R1,RF                                                            
DSTA14A  BCT   RE,DSTA14                                                        
         MVC   DBSELBK,1(R1)                                                    
         MVC   DBBTYPE,3(R1)                                                    
DSTA15   MVC   DBSELSTA,ACTSTAT                                                 
         MVC   DBFILE,=C'TP '                                                   
         GOTO1 DEMAND,DMCB,DBLOCK,0                                             
         CLI   DBERROR,X'10'                                                    
         BE    BADSTA                                                           
         SPACE 2                                                                
* GET MARKET NAME                                                               
         SPACE 1                                                                
DSTA20   MVC   ACTMKT,DBACTRMK     MOVE MARKET NUMBER                           
         J     EQXIT                                                            
*                                                                               
BADSTA   MVI   DBERROR,X'10'                                                    
         J     NEQXIT                                                           
         SPACE 3                                                                
         EJECT                                                                  
EDITOPT  MVI   BOXOPT,C'Y'                                                      
         MVI   WIDEOPT,2                                                        
         MVI   TRACEOPT,C'N'                                                    
         MVI   RANKOPT,C'N'                                                     
         MVI   FORMOPT,C'N'                                                     
         MVI   WORSTOPT,C'N'                                                    
         MVI   SPILLOPT,0                                                       
         MVI   EXPANOPT,C'N'                                                    
         MVI   SIMULOPT,C'Y'                                                    
         MVI   HOUROPT,C'N'                                                     
         MVI   CALCOPT,C'Y'                                                     
         CLI   CTRY,C'C'                                                        
         BNE   *+8                                                              
         MVI   CALCOPT,C'N'                                                     
         MVI   SEPAOPT,C'N'                                                     
         MVI   ADDROPT,C'Y'                                                     
         MVI   LONGOPT,C'N'                                                     
         MVI   AUTOOPT,C'N'                                                     
         CLI   5(R2),0                                                          
         JE    EQXIT                                                            
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    BADOPT                                                           
         SPACE 1                                                                
OPT2     CLC   12(3,R4),=C'BOX'    BOX OPTION                                   
         BNE   OPT4                                                             
         MVC   BOXOPT,22(R4)                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT4     CLC   12(4,R4),=C'WIDE'   WIDE OPTION                                  
         BNE   OPT6                                                             
         MVI   WIDEOPT,3                                                        
         B     OPTEND                                                           
         SPACE 1                                                                
OPT6     CLC   12(4,R4),=C'NARROW' NARROW REPORT OPTION                         
         BNE   OPT8                                                             
         MVI   WIDEOPT,1                                                        
         B     OPTEND                                                           
         SPACE 1                                                                
OPT8     CLC   12(5,R4),=C'TRACE'  OPTION TO TRACE DEMAND HOOKS                 
         BNE   OPT10                                                            
         MVI   TRACEOPT,C'Y'                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT10    CLC   12(4,R4),=C'RANK'   OPTION TO PRINT RANK NUMBERS                 
         BNE   OPT12                                                            
         MVI   RANKOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT12    CLC   12(4,R4),=C'FORMAT' OPTION TO PRINT STATION FORMAT               
         BNE   OPT14                                                            
         MVI   FORMOPT,C'Y'                                                     
*        CLI   WSTATS+3,9          (ONLY VALID FOR STATION LIST)                
*        BNE   BADOPT                                                           
         B     OPTEND                                                           
         SPACE 1                                                                
OPT14    CLC   12(4,R4),=C'WORST'  OPTION TO SHOW WORST STATIONS!               
         BNE   OPT16                                                            
         MVI   WORSTOPT,C'Y'                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT16    CLC   12(4,R4),=C'SPILL'  OPTION TO SHOW SPILL ONLY                    
         BNE   OPT18                                                            
         MVI   SPILLOPT,C'Y'                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT18    CLC   12(4,R4),=C'NOSPILL'          AND NO SPILL                       
         BNE   OPT20                                                            
         MVI   SPILLOPT,C'N'                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT20    CLC   12(4,R4),=C'EXPA'   OPTION TO EXPAND COMBOS                      
         BNE   OPT22                (SHOW INDIV STATIONS + COMBO)               
         MVC   EXPANOPT,22(R4)                                                  
         B     OPTEND                                                           
         SPACE 1                                                                
OPT22    CLC   12(4,R4),=C'SIMU'  OPTION TO SUPRESS SIMULCAST STATIONS          
         BNE   OPT24                                                            
         MVC   SIMULOPT,22(R4)                                                  
         B     OPTEND                                                           
OPT24    CLC   12(4,R4),=C'HOUR'   HOURLY DATA ONLY OPTION                      
         BNE   OPT25                                                            
         MVI   HOUROPT,C'Y'                                                     
         B     OPTEND                                                           
OPT25    CLC   12(4,R4),=C'CALC'    IMPRESSION DATA                             
         BNE   OPT26                                                            
         CLC   22(3,R4),=C'BOOK'                                                
         BE    OPT25A                                                           
         B     OPTEND                                                           
OPT25A   MVI   CALCOPT,C'N'                                                     
         B     OPTEND                                                           
OPT26    CLC   12(3,R4),=C'SEP'    USE MULTIPLE DEMO REPORTS                    
         BNE   OPT27                                                            
         CLI   22(R4),C'D'                                                      
         BNE   OPT26A                                                           
         B     OPT26B                                                           
OPT26A   CLI   22(R4),C'C'                                                      
         BNE   OPTEND                                                           
OPT26B   MVC   SEPAOPT,22(R4)      C, MULTIPLE CATEGORIES                       
         B     OPTEND              D, MULTIPLE DEMOS                            
OPT27    CLC   12(4,R4),=C'ADDR'   USE AGENCY ADDRESS?                          
         BNE   OPT28                                                            
         CLI   22(R4),C'N'                                                      
         BNE   OPTEND                                                           
         MVI   ADDROPT,C'N'                                                     
         B     OPTEND                                                           
OPT28    CLC   12(4,R4),=C'LONG'    USE 25 MORE LINES?                          
         BNE   OPT29                                                            
         CLI   22(R4),C'Y'                                                      
         BNE   OPTEND                                                           
         MVI   LONGOPT,C'Y'                                                     
         B     OPTEND                                                           
OPT29    CLC   12(4,R4),=C'AUTO'    USE AUTO DELETE ON CMBLIST?                 
         BNE   OPT50                                                            
         CLC   =C'DELETE',22(R4)                                                
         BNE   OPTEND                                                           
         MVI   AUTOOPT,C'Y'                                                     
         B     OPTEND                                                           
OPT50    DS    0H                                                               
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
         J     NEQXIT                                                           
         SPACE 1                                                                
OPTEND   LA    R4,32(R4)                                                        
         BCT   R0,OPT2                                                          
         J     EQXIT                                                            
OPTERR   DC    C'** ERROR ** INVALID OPTION'                                    
         DS    0F                                                               
         EJECT                                                                  
         DROP  R3                                                               
*              EDIT DAYPART                                                     
         SPACE 3                                                                
*              DDS STANDARD        NUMERIC 1-9                                  
*              AGENCY SPECIFIED    ALPHA FIELD                                  
         SPACE 1                                                                
EDITDPT  L     R4,AMYD                                                          
         USING MYD,R4                                                           
         LA    R4,DPTLIST                                                       
         USING DPENTRY,R4                                                       
         MVI   NCMBDPT,X'00'                                                    
         LR    RE,R4               CLEAR DAYPART TABLE                          
         LA    RF,L'DPENTRY                                                     
         MH    RF,=H'15'                                                        
         XCEF                                                                   
         LA    R2,RNKDPTH                                                       
         MVI   WORK,C'1'           DEFAULT IS DDS1                              
         CLI   5(R2),0                                                          
         BE    DDSDPT                                                           
         GOTO1 ANY                                                              
         CLI   WORK,C'0'                                                        
         BL    AGYDPT                                                           
         CLI   WORK,C'9'                                                        
         BH    AGYDPT                                                           
         CLI   WORK+1,C' '                                                      
         BNE   BADDPT                                                           
         SPACE 1                                                                
DDSDPT   L     R3,=A(DDSDPLST)                                                  
         CLI   DBSELSRC,C'R'       RADAR LIST IS DIFFERENT                      
         BNE   *+8                                                              
         L     R3,=A(RDSDPLST)                                                  
         CLI   DBSELSRC,C'M'       BBM LIST IS DIFFERENT                        
         BNE   *+8                                                              
         L     R3,=A(HDSDPLST)                                                  
         A     R3,RELO                                                          
         SPACE 1                                                                
DDSDPT2  CLC   0(1,R3),WORK                                                     
         BE    DDSDPT4                                                          
         CLI   0(R3),X'FF'                                                      
         BE    BADDPT                                                           
         ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     DDSDPT2                                                          
         SPACE 1                                                                
DDSDPT4  MVC   NDPTS,2(R3)         PICK UP NUMBER OF DAYPARTS                   
         ZIC   R0,NDPTS                                                         
         LA    R3,3(R3)            (GET TO DAY/TIMES IN TABLE)                  
         SPACE 1                                                                
DDSDPT6  MVC   DPSDAY(15),0(R3)    STANDARD DAY/TIMES TO DPTLIST                
         LA    R3,L'DDSDPENT(R3)                                                
         LA    R4,L'DPENTRY(R4)                                                 
         BCT   R0,DDSDPT6                                                       
         J     EQXIT                                                            
         DROP  R4                                                               
         EJECT                                                                  
*              AGENCY DAYPART CODE                                              
         SPACE 3                                                                
*              INPUT               CODE IS IN WORK                              
*                                  R4=A(DPTLIST)                                
*              OUTPUT              DPTLIST                                      
AGYDPT   DS    0H                  AGENCY CUSTOM DAYPART                        
         USING DPENTRY,R4                                                       
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING CDPKEY,R3                                                        
         MVI   CDPKTYPE,X'0D'                                                   
         MVI   CDPKSUB,X'5A'                                                    
         MVC   CDPKAM,BAGYMD                                                    
         MVC   CDPKNAME,WORK                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BNE   DPTDYT                                                           
         GOTO1 GETREC                                                           
         L     R3,AIO1                                                          
         LA    R3,DSCELEM                                                       
         MVI   NDPTS,0                                                          
         SPACE 1                                                                
AGYDPT2  ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         CLI   0(R3),0                                                          
         JE    XIT                                                              
         CLI   0(R3),X'05'                                                      
         BNE   AGYDPT2                                                          
         USING DPTELEM,R3                                                       
         AI    NDPTS,1                                                          
         CLI   NDPTS,15                                                         
         BH    BADDPT                                                           
         MVC   DPCDAY(15),DPTDAY1  UP TO 5 DAY/TIMES                            
         MVC   DPDESC(18),DPTNAME                                               
         CLI   DPDESC,C' '         IF THIS IS STILL SPACES                      
         BH    *+10                                                             
         MVC   DPDESC(18),DPTINPUT USE THE ORIGINAL INPUT                       
         MVI   DPDESC+18,C' '                                                   
         OC    DPDESC,SPACES                                                    
         LA    R4,L'DPENTRY(R4)                                                 
         B     AGYDPT2                                                          
         SPACE 1                                                                
BADDPT   MVC   CONHEAD(L'INVDAYPT),INVDAYPT                                     
         B     MYEND_3                                                          
         DROP  R4                                                               
         SPACE 1                                                                
DPTDYT   GOTOR SUBR07,DMCB,('EDDYTME',(RC))                                     
         BNE   BADDPT                                                           
         J     EQXIT                                                            
         SPACE 2                                                                
INVDAYPT DC    C'** ERROR ** INVALID DAYPART'                                   
         EJECT                                                                  
SETNEWUN L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         CLI   DEMSTYLE,C'D'                                                    
         BE    SETNEWA                                                          
         LM    R2,R4,RUNIVS                                                     
         LA    R5,MSASAVE+4                                                     
         EDIT  (R2),(6,(R5)),ALIGN=LEFT                                         
         CLI   DBSELMED,C'M'       BBM FIXES                                    
         BE    SETNEWU3                                                         
         LA    R5,TSASAVE+4                                                     
         EDIT  (R3),(6,(R5)),ALIGN=LEFT                                         
         LA    R5,ADISAVE+4                                                     
         EDIT  (R4),(6,(R5)),ALIGN=LEFT                                         
         CLI   DBSELSRC,C'R'                                                    
         BNE   SETNEWU3                                                         
*        LM    R2,R5,RUNIVS                                                     
         L     R5,UNIVS                                                         
         SR    R4,R4               ADJUST TO FIT                                
         D     R4,=F'10'                                                        
         LA    R2,USASAVE+4                                                     
         EDIT  (R5),(6,(R2)),ALIGN=LEFT                                         
*        LA    R5,USASAVE+4                    8/7/00                           
*        EDIT  (R4),(6,(R5)),ALIGN=LEFT                                         
SETNEWU3 J     EQXIT                                                            
SETNEWA  LA    R2,DEMOS                                                         
         LA    R3,RUNIVS                                                        
         LA    R4,DUNBLOCK                                                      
         ZIC   R5,NDEMOS                                                        
         MVC   DUNBLOCK,SPACES                                                  
         SPACE 1                                                                
SETNEWU2 GOTO1 DEMOCON,DMCB,(R2),(5,WORK),DBLOCK                                
         MVC   0(5,R4),WORK        W1834                                        
         MVI   5(R4),C'='          W1834=                                       
         L     R1,0(R3)                                                         
         CLI   DBSELSRC,C'R'       RADAR ADJUST PRECISION                       
         BNE   *+10                                                             
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(6,6(R4)),ALIGN=LEFT                                        
         LA    R2,3(R2)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,12(R4)                                                        
         BCT   R5,SETNEWU2                                                      
         J     EQXIT                                                            
         EJECT                                                                  
*              ROUTINE TO FORMAT UNIVERSES FOR HEADLINES                        
         SPACE 3                                                                
*              INPUT               FIRST DEMO IN DEMOS                          
*              OUTPUTS             ADISAVE MSASAVE TSASAVE                      
         SPACE 1                                                                
         DS    0H                                                               
SETUNIVA XC    UNIVS(32),UNIVS                                                  
         LA    R6,BOOKS                                                         
         ZIC   R7,NBOOKS                                                        
SETUNV1  MVC   DBSELBK,1(R6)                                                    
         MVC   DBBTYPE,3(R6)                                                    
         MVI   DBSELMED,C'R'                                                    
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBSELMK,MKTNUM                                                   
         MVI   DBSELSTA+4,C'A'                                                  
*                                                                               
         CLC   =C'R',DBSELSRC     RADAR DONT HAVE THIS REC                      
         BE    SETUNV1A                                                         
*                                                                               
         EDIT  (2,MKTNUM),(4,DBSELSTA)                                          
*                                                                               
         LA    R2,4                                                             
         LA    R3,DBSELSTA                                                      
*                                                                               
SETUNV2  CLI   0(R3),C' '                                                       
         BNE   SETUNV3                                                          
         MVI   0(R3),C'0'                                                       
SETUNV3  LA    R3,1(R3)                                                         
         BCT   R2,SETUNV2                                                       
*                                                                               
SETUNV1A MVC   DBFILE,=C'RDP'                                                   
*                                                                               
         MVI   DBSELDAY,X'7C'                                                   
         XC    DBSELTIM,DBSELTIM                                                
         MVI   DBSELPRG+1,1                                                     
         MVC   DBAREC,AIO1                                                      
*                                                                               
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   DEMSTYLE,C'D'                                                    
         BE    SETDUNS                                                          
         MVC   MSASAVE(4),=C'MSA='                                              
         CLI   DBSELSRC,C'M'       FIX UP FOR BBM                               
         BNE   *+14                                                             
         MVC   MSASAVE(4),=C'CMA='                                              
         B     *+16                                                             
         MVC   TSASAVE(4),=C'TSA='                                              
         MVC   ADISAVE(4),=C'ADI='                                              
         MVC   USASAVE(4),=C'USA='             8/7/00                           
*                                                                               
         CLI   DBSELSRC,C'R'                  RADAR ONLY HAS USA                
         BNE   SETUNV1C                                                         
***      B     SETDUNS                                                          
         MVC   UNIVLIST,DEMOS                                                   
         MVI   UNIVLIST+1,C'U'                                                  
         B     SETUNV1D                                                         
*                                                                               
         SPACE 1                                                                
SETUNV1C MVC   UNIVLIST,UNIVCATS                                                
         MVC   UNIVLIST+2(1),DEMOS+2                                            
         MVC   UNIVLIST+5(1),DEMOS+2                                            
         MVC   UNIVLIST+8(1),DEMOS+2                                            
         MVC   UNIVLIST+11(1),DEMOS+2           USA                             
SETUNV1D GOTO1 DEMAND,DMCB,DBLOCK,UNVHOOK                                       
*                                                                               
SETUNIV2 LM    R2,R4,UNIVS                                                      
         LA    R5,MSASAVE+4                                                     
         EDIT  (R2),(6,(R5)),ALIGN=LEFT                                         
         CLI   DBSELMED,C'M'       BBM FIXES                                    
         BE    SETUNIVX                                                         
         LA    R5,TSASAVE+4                                                     
         EDIT  (R3),(6,(R5)),ALIGN=LEFT                                         
         LA    R5,ADISAVE+4                                                     
         EDIT  (R4),(6,(R5)),ALIGN=LEFT                                         
*                                                                               
         CLI   DBSELSRC,C'R'                                                    
         BNE   SETUNIVX                                                         
         L     R5,UNIVS                                                         
         SR    R4,R4               ADJUST TO FIT                                
         D     R4,=F'10'                                                        
         LA    R2,USASAVE+4                                                     
         EDIT  (R5),(6,(R2)),ALIGN=LEFT                                         
*                                                                               
         SPACE 1                                                                
SETUNIVX BAS   RE,REALUNIV                                                      
         LA    R6,4(R6)            NEXT BOOK                                    
         BCT   R7,SETUNV1                                                       
         BAS   RE,AVEUNIV                                                       
         CLI   DEMSTYLE,C'D'                                                    
         BE    SETUXIT                                                          
*                                                                               
SETUNIV3 LA    R2,DEMOS                                                         
         ZIC   R0,NDEMOS                                                        
         SPACE 1                                                                
SETANYS  CLI   0(R2),1             CHECK TO SEE WHICH AREAS WERE                
         BNE   NOMSAU              REQUESTED. THIS AFFECTS UNIV DISPLAY         
         MVI   ANYMSA,C'Y'                                                      
         OC    UNIVS(4),UNIVS                                                   
         BNZ   *+8                                                              
         MVI   ANYUNIV,C'N'                                                     
NOMSAU   CLI   0(R2),2                                                          
         BNE   NOTSAU                                                           
         MVI   ANYTSA,C'Y'                                                      
         OC    UNIVS+4(4),UNIVS+4                                               
         BNZ   *+8                                                              
         MVI   ANYUNIV,C'N'                                                     
NOTSAU   CLI   0(R2),3                                                          
*        BNE   NOADIU                                                           
         BNE   NOUSAU                                                           
         MVI   ANYADI,C'Y'                                                      
         OC    UNIVS+8(4),UNIVS+8                                               
         BNZ   *+8                                                              
         MVI   ANYUNIV,C'N'                                                     
NOUSAU   CLI   0(R2),0                        USA STUFF BPOO NEW                
         BNE   NOADIU                                                           
         MVI   ANYUSA,C'Y'                                                      
         OC    UNIVS+12(4),UNIVS+12           8/7/00                            
         BNZ   *+8                                                              
         MVI   ANYUNIV,C'N'                                                     
NOADIU   LA    R2,3(R2)                                                         
         BCT   R0,SETANYS                                                       
SETUXIT  J     XIT                                                              
         SPACE 1                                                                
UNIVCATS DC    X'01',C'U',X'00'    MSA                                          
         DC    X'02',C'U',X'00'    TSA                                          
         DC    X'03',C'U',X'00'    ADI                                          
         DC    X'00',C'U',X'00'    USA                                          
         DC    X'FF'                                                            
         EJECT                                                                  
*              ROUTINE TO FORMAT UNIVERSES FOR MULTIPLE DEMOS                   
         SPACE 3                                                                
*              INPUT               DEMOS NDEMOS                                 
*              OUTPUTS             DUNBLOCK                                     
         SPACE 1                                                                
SETDUNS  MVC   UNIVLIST,DEMOS                                                   
         MVI   UNIVLIST+1,C'U'                                                  
         MVI   UNIVLIST+4,C'U'                                                  
         MVI   UNIVLIST+7,C'U'                                                  
         MVI   UNIVLIST+10,C'U'                                                 
         GOTO1 DEMAND,DMCB,DBLOCK,UNVHOOK                                       
         OC    UNIVS(12),UNIVS     SET TO RETRY IF NO UNIV FOR THIS REC         
         BNZ   *+8                                                              
         MVI   ANYUNIV,C'N'                                                     
         SPACE 1                                                                
         LA    R2,DEMOS                                                         
         LA    R3,UNIVS                                                         
         LA    R4,DUNBLOCK                                                      
         ZIC   R5,NDEMOS                                                        
         MVC   DUNBLOCK,SPACES                                                  
         SPACE 1                                                                
SETDUNS2 GOTO1 DEMOCON,DMCB,(R2),(5,WORK),DBLOCK                                
         MVC   0(5,R4),WORK        W1834                                        
         MVI   5(R4),C'='          W1834=                                       
         EDIT  (4,(R3)),(6,6(R4)),ALIGN=LEFT                                    
         LA    R2,3(R2)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,12(R4)                                                        
         BCT   R5,SETDUNS2                                                      
         B     SETUNIVX                                                         
         SPACE 3                                                                
UNVHOOK  NTR1                                                                   
         GOTO1 DEMOUT,DMCB,(C'L',UNIVLIST),DBLOCK,UNIVS                         
         J     XIT                                                              
*                                                                               
REALUNIV NTR1                                                                   
*        LA    R0,4                                                             
         LA    R0,4                                                             
         LA    R2,UNIVS                                                         
         LA    R3,RUNIVS                                                        
REALUNV1 L     R5,0(R2)                                                         
         A     R5,0(R3)                                                         
         ST    R5,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,REALUNV1                                                      
         J     XIT                                                              
         SPACE 3                                                                
AVEUNIV  NTR1                                                                   
         LA    R0,4                                                             
         ZIC   R5,NBOOKS                                                        
         LA    R7,RUNIVS                                                        
AVEUNILP SR    R2,R2                                                            
         L     R3,0(R7)                                                         
         DR    R2,R5                                                            
         ST    R3,0(R7)                                                         
         LR    R3,R2                                                            
         SR    R2,R2                                                            
         LA    R5,10                                                            
         MR    R2,R5                                                            
         ZIC   R5,NBOOKS                                                        
         DR    R2,R5                                                            
         CH    R3,=H'5'                                                         
         BL    AVENXTLP                                                         
         L     R3,0(R7)                                                         
         LA    R3,1(R3)                                                         
         ST    R3,0(R7)                                                         
AVENXTLP LA    R7,4(R7)                                                         
         BCT   R0,AVEUNILP                                                      
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO FORMAT UNIVERSES FOR HEADLINES   (OLD)                
         SPACE 3                                                                
*              INPUT               FIRST DEMO IN DEMOS                          
*              OUTPUTS             ADISAVE MSASAVE TSASAVE                      
         SPACE 1                                                                
         DS    0H                                                               
SETUNIVB L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         CLC   DBSELBK,UNIVBK                                                   
         BNH   *+8                                                              
         MVI   ANYUNIV,C'N'                                                     
         CLI   ANYUNIV,C'Y'                                                     
         BE    SETUBX                                                           
         MVI   ANYUNIV,C'Y'                                                     
         MVC   UNIVBK,DBSELBK                                                   
*                                                                               
         CLI   DEMSTYLE,C'D'                                                    
         BE    SETDUNSB                                                         
         MVC   MSASAVE(4),=C'MSA='                                              
         CLI   DBSELSRC,C'M'       FIX UP FOR BBM                               
         BNE   *+14                                                             
         MVC   MSASAVE(4),=C'CMA='                                              
         B     *+16                                                             
         MVC   TSASAVE(4),=C'TSA='                                              
         MVC   ADISAVE(4),=C'ADI='                                              
         SPACE 1                                                                
         MVC   UNIVLIST,UNIVCATB                                                
         MVC   UNIVLIST+2(1),DEMOS+2                                            
         MVC   UNIVLIST+5(1),DEMOS+2                                            
         MVC   UNIVLIST+8(1),DEMOS+2                                            
         GOTO1 DEMOUT,DMCB,(C'L',UNIVLIST),DBLOCK,UNIVS                         
SETUB2   LM    R2,R4,UNIVS                                                      
         LA    R5,MSASAVE+4                                                     
         EDIT  (R2),(6,(R5)),ALIGN=LEFT                                         
         CLI   DBSELMED,C'M'       BBM FIXES                                    
         BE    SETUB3                                                           
         LA    R5,TSASAVE+4                                                     
         EDIT  (R3),(6,(R5)),ALIGN=LEFT                                         
         LA    R5,ADISAVE+4                                                     
         EDIT  (R4),(6,(R5)),ALIGN=LEFT                                         
SETUB3   DS    0H                                                               
         SPACE 1                                                                
         LA    R2,DEMOS                                                         
         ZIC   R0,NDEMOS                                                        
         SPACE 1                                                                
SETANYSB CLI   0(R2),1             CHECK TO SEE WHICH AREAS WERE                
         BNE   NOMSAUB             REQUESTED. THIS AFFECTS UNIV DISPLAY         
         MVI   ANYMSA,C'Y'                                                      
         OC    UNIVS(4),UNIVS                                                   
         BNZ   *+8                                                              
         MVI   ANYUNIV,C'N'                                                     
NOMSAUB  CLI   0(R2),2                                                          
         BNE   NOTSAUB                                                          
         MVI   ANYTSA,C'Y'                                                      
         OC    UNIVS+4(4),UNIVS+4                                               
         BNZ   *+8                                                              
         MVI   ANYUNIV,C'N'                                                     
NOTSAUB  CLI   0(R2),3                                                          
         BNE   NOADIUB                                                          
         MVI   ANYADI,C'Y'                                                      
         OC    UNIVS+8(4),UNIVS+8                                               
         BNZ   *+8                                                              
         MVI   ANYUNIV,C'N'                                                     
NOADIUB  LA    R2,3(R2)                                                         
         BCT   R0,SETANYSB                                                      
SETUBX   SR    R1,R1                                                            
         LTR   R1,R1                                                            
         J     XIT                                                              
         SPACE 1                                                                
****CATB DC    X'00',C'U',X'00'    USA                                          
UNIVCATB DC    X'01',C'U',X'00'    MSA                                          
         DC    X'02',C'U',X'00'    TSA                                          
         DC    X'03',C'U',X'00'    ADI                                          
         DC    X'FF'                                                            
         EJECT                                                                  
*              ROUTINE TO FORMAT UNIVERSES FOR MULTIPLE DEMOS                   
         SPACE 3                                                                
*              INPUT               DEMOS NDEMOS                                 
*              OUTPUTS             DUNBLOCK                                     
         SPACE 1                                                                
SETDUNSB MVC   UNIVLIST,DEMOS                                                   
         MVI   UNIVLIST+1,C'U'                                                  
         MVI   UNIVLIST+4,C'U'                                                  
         MVI   UNIVLIST+7,C'U'                                                  
         MVI   UNIVLIST+10,C'U'                                                 
         GOTO1 DEMOUT,DMCB,(C'L',UNIVLIST),DBLOCK,UNIVS                         
         OC    UNIVS(12),UNIVS     SET TO RETRY IF NO UNIV FOR THIS REC         
         BNZ   *+8                                                              
         MVI   ANYUNIV,C'N'                                                     
         SPACE 1                                                                
         LA    R2,DEMOS                                                         
         LA    R3,UNIVS                                                         
         LA    R4,DUNBLOCK                                                      
         ZIC   R5,NDEMOS                                                        
         MVC   DUNBLOCK,SPACES                                                  
         SPACE 1                                                                
SETDUBS2 GOTO1 DEMOCON,DMCB,(R2),(5,WORK),DBLOCK                                
         MVC   0(5,R4),WORK        W1834                                        
         MVI   5(R4),C'='          W1834=                                       
         EDIT  (4,(R3)),(6,6(R4)),ALIGN=LEFT                                    
         LA    R2,3(R2)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,12(R4)                                                        
         BCT   R5,SETDUBS2                                                      
         B     SETUBX                                                           
         EJECT                                                                  
SETPHAS6 DS    0H                                                               
         PRINT GEN                                                              
*        GOTO1 CALLOV,DMCB,(X'60',0),0                                          
         GOTO1 CALLOV,DMCB,(X'61',0),0                                          
         PRINT NOGEN                                                            
         L     RF,DMCB                                                          
         LA    RF,0(RF)                                                         
         ST    RF,ADDPHS60                                                      
         J     XIT                                                              
         EJECT                                                                  
MYEND_3  MVI   ERROR,SUPPLIED      USING MY OWN ERROR MSG                       
         MVC   RNKSTATH+5(1),STATLEN                                            
         MVC   WORK,CONHEAD                                                     
         GOTO1 VGETERR                                                          
         LTORG                                                                  
         EJECT                                                                  
PHSE60D  DSECT                                                                  
         ORG   *+88                                                             
APHASE60 DS    A                                                                
AFRIENDS DS    A                                                                
ACFRENDS DS    A                                                                
ABBMDPTS DS    A                                                                
ARSTNDPT DS    A                                                                
ASTNDPTS DS    A                                                                
         DS    30A                                                              
         SPACE 3                                                                
STATENTD DSECT                                                                  
STATENT  DS    0CL10                                                            
STATCALL DS    CL5                                                              
STATFORM DS    CL4                                                              
         DS    C                                                                
         SPACE 2                                                                
ELEMDS   DSECT                                                                  
ELEMSTOP DS    C                                                                
ELEMLEN  DS    X                                                                
ELEMADDR DS    XL2                                                              
ELEMDISP DS    X                                                                
ELEMBDAT EQU   *-ELEMSTOP                                                       
ELEMDATA DS    0C                                                               
         SPACE 3                                                                
*              DSECT TO COVER ENTRY IN STANDARD DAYPART TABLE                   
         SPACE 1                                                                
STAND    DSECT                                                                  
STANDENT DS    0CL36                                                            
STANDDTS DS    XL15                UP TO 15 DAY/HOUR/HOUR                       
STANDPRG DS    XL1                 'PROGRAM' CODE                               
STANDESC DS    CL20                DESCRIPTION                                  
         SPACE 1                                                                
SVTD     DSECT TWA SAVE DESCRIPTION                                             
SVTKMKT  DS    CL12                PREV MARKET KEY                              
SVTSRC   DS    CL1                 SOURCE                                       
SVTBOOK  DS    CL2                 BOOK                                         
SVTBTYP  DS    C                   BOOK TYPE                                    
SVTFILE  DS    CL3                 DBFILE                                       
SVTMED   DS    C                   MEDIA                                        
SVTFLTR  DS    CL8                 MARKET FILTER                                
SVTMKTS  DS    13CL3               ROOM FOR 13 MARKETS                          
         DS    C                   TERMINATOR FOR MARKET LIST                   
         EJECT                                                                  
*              MY LITTLE NMOD AREA                                              
         SPACE 1                                                                
MYD      DSECT                                                                  
DPTLIST  DS    15CL36                                                           
RANKNUMS DS    15XL40                                                           
COMBLIST DS    10XL49                                                           
LNCOMB   EQU   49                                                               
CUMESW   DS    C                                                                
THISBUFF DS    22XL(L'THISREC)                                                  
*MARKET  DS    C                                                                
SVMKTH5  DS    C                                                                
SVMKTNUM DS    H                                                                
MKTPNTR  DS    F                                                                
NEWSTAT  DS    C                                                                
CMBCNTR  DS    C                                                                
         SPACE 2                                                                
MKTSTN   DS    CL5                                                              
MKTBUFF  DS    CL255                                                            
MKTBUF2  DS    CL128                                                            
DBRADIC  DS    CL128                                                            
DBFORMC  DS    900C                                                             
TOTTABL  DS    80F                                                              
         SPACE 2                                                                
*              DSECT TO COVER A DAYPART ENTRY                                   
         SPACE 1                                                                
DPED     DSECT                                                                  
DPENTRY  DS    0CL36                                                            
DPSTYPE  DS    CL1                 FOR STANDARD, TYPE = S                       
DPSDAY   DS    XL1                          AND  DAY CODE                       
DPSPROG  DS    CL1                          AND  'PROGRAM' CODE 1-18            
DPSSPARE DS    XL13                                                             
         ORG   DPSTYPE                                                          
DPCTYPE  DS    CL1                 FOR CUSTOM,   TYPE = C                       
DPCDAY   DS    XL1                          AND  DAY CODE,                      
DPCSTART DS    XL1                               START HOUR NUMBER              
DPCEND   DS    XL1                               END HOUR NUMBER                
DPCOTHER DS    XL12                         OR   UP TO 4 MORE DAY/TIMES         
         SPACE 1                                                                
DPDESC   DS    CL19                FOR BOTH, DESCRIPTION                        
         DS    CL1                                                              
         EJECT                                                                  
*              BUFFER ARRANGEMENT                                               
         SPACE 3                                                                
*              THERE ARE 15 DAYPART BUFFERS                                     
*                                                                               
*              EACH OF THESE IS COMPOSED OF UP TO 40 STATION LINES              
*              AFTER EACH STATION IS PROCESSED, EACH DAYPART                    
*              BUFFER WILL BE SORTED WITH THE BEST UP FRONT                     
         SPACE 2                                                                
*              DSECT TO COVER RECORDS IN BUFF                                   
         SPACE 1                                                                
MSKDEM   EQU   7                   OLD 3                                        
LENDEM   EQU   3                   OLD 2                                        
BUFFD    DSECT                                                                  
BUFFREC  DS    0CL(2+LENDEM*4)                                                  
BUFFSTAT DS    XL1                 STATION NUMBER                               
BUFFDEMS DS    XL(LENDEM*4)        UP TO 4 DEMOS                                
BUFFSAN  DS    C                   SPECIAL ANNOUNCEMENT                         
         SPACE 3                                                                
*              DSECT TO COVER RECORDS IN THISBUFF                               
         SPACE 3                                                                
THISD    DSECT                                                                  
THISREC  DS    0CL24                                                            
THISWT   DS    F                   WEIGHT                                       
THISDEMS DS    4F                  UP TO 4 DEMOS                                
THISNOA  DS    C                   NOT ON AIR IND                               
THISANN  DS    C                   SPECIAL ANNOUNCEMENT IND                     
         DS    CL2                 SPARE                                        
*                                                                               
       ++INCLUDE DEALPHAMKD                                                     
       ++INCLUDE DEDBEXTRAD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENCUSDP                                                     
         EJECT                                                                  
       ++INCLUDE SPGENSTLST                                                     
       ++INCLUDE SPGENCOMBO                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE DDTWABLDD                                                      
MARKETD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE SPRESWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REGENREPA                                                      
         PRINT ON                                                               
         EJECT                                                                  
*              DSECT TO COVER SCREEN                                            
         SPACE 3                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE SPRESD3D                                                       
         EJECT                                                                  
*              DSECT TO COVER SCREEN MARKET SELECT                              
         SPACE 3                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE SPRESA3D                                                       
         EJECT                                                                  
*              STORAGE FOR RANKER                                               
         SPACE 3                                                                
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
MAXRANK  DS    XL1                 MAX STATIONS TO RANK                         
SVMAXRNK DS    XL1                 SAVE MAX RANK IN CASE OF LIST                
WIDEOPT  DS    XL1                 WIDTH 1-3 = 80/110/132                       
TRACEOPT DS    CL1                 Y=TRACE DEMAND HOOKS                         
RANKOPT  DS    CL1                 Y=PRINT RANK NUMBERS                         
FORMOPT  DS    CL1                 Y=PRINT STATION FORMAT                       
WORSTOPT DS    CL1                 Y=SHOW WORST STATIONS                        
SPILLOPT DS    CL1                 Y=SPILL ONLY N=NO SPILL                      
EXPANOPT DS    CL1                 Y=SHOW STATIONS INCLUDED IN COMBOS           
SIMULOPT DS    CL1                 N=SUPPRESS SIMULCAST STATIONS                
HOUROPT  DS    CL1                 Y=USE HOURLY DATA ONLY                       
CALCOPT  DS    CL1                 Y=USE IMPRESSION DATA                        
SEPAOPT  DS    C                   Y=PRINT MULTIPLE DEMOS                       
ADDROPT  DS    C                   Y=PRINT AGENCY ADDRESS                       
LONGOPT  DS    C                   Y=PRINT REPORT 25 MORE LINES                 
AUTOOPT  DS    C                   Y=USE AUTO DELETE ON CMBLISTS                
TWANUM   DS    C                                                                
STACTSW  DS    C                                                                
COMAND2  DS    CL8                                                              
MYBASE   DS    A                                                                
RELO2    DS    F                                                                
MYBASE2  DS    F                                                                
ATHISBUF DS    F                                                                
ANDMASK  DS    F                                                                
         SPACE 1                                                                
MSASAVE  DS    CL10                ROOM FOR MSA=NNNNNN                          
TSASAVE  DS    CL10                ROOM FOR TSA=NNNNNN                          
ADISAVE  DS    CL10                ROOM FOR ADI=NNNNNN                          
USASAVE  DS    CL10                ROOM FOR USA=NNNNNN                          
ANYMSA   DS    CL1                 SET TO Y IF MSA SPECIFIED                    
ANYTSA   DS    CL1                 SET TO Y IF TSA SPECIFIED                    
ANYADI   DS    CL1                 SET TO Y IF ADI SPECIFIED                    
ANYUSA   DS    CL1                 SET TO Y IF USA SPECIFIED                    
         ORG   MSASAVE                                                          
DUNBLOCK DS    CL48                OR UP TO 4 W1834=123456                      
ANYUNIV  DS    CL1                 Y=UNIVERSES DONE                             
NDPTS    DS    XL1                 NUMBER OF DAYPARTS                           
THISTYPE DS    CL1                 C(USTOM) OF S(TANDARD)                       
FIRSTDP  DS    CL1                 SWITCH FOR PRINTING                          
DISP     DS    F                   DISPLACEMENT INTO PRINT LINE                 
DPTGAP   DS    F                   GAP BETWEEN DAYPARTS                         
DPTWIDTH DS    F                   WIDTH OF EACH DAYPART CHUNK                  
DPTSLEFT DS    F                   NUMBER OF DAYPARTS STILL TO PRINT            
UP       DS    F                   NUMBER OF DAYPARTS                           
MKTNUM   DS    H                   MARKET NUMBER SELECTED                       
AMYD     DS    A                   ADDRESS OF MY STORAGE                        
ATHISB2  DS    A                   ADDRESS OF EXTRA BUFFER                      
ASTATS   DS    A                   ADDRESS OF STATION LIST                      
WSTATS   DS    F                   WIDTH OF STATION LIST                        
ABEST    DS    A                   ADDRESS OF FIRST DAYPART BEST                
AWORST   DS    A                   ADDRESS OF FIRST DAYPART WORST               
ADPTBEST DS    A                   ADDRESS OR FIRST FOR THIS DP SET             
ATHISTAT DS    A                   ADDRESS OF STATION BEING PROCESSED           
STATNUM  DS    XL1                 NUMBER OF STATION BEING PROCESSED            
DPTNUM   DS    XL1                 NUMBER OF DAYPART BEING PROCESSED            
SAVDPTN  DS    XL1                 FIRST DAYPART BEING PROCESSED                
*HISBUFF DS    15XL(L'THISREC)     BUFFER FOR STATION IN PROGRESS               
THISLINE DS    XL(L'THISREC)       TEMPORARY BUFFER FOR WEIGHTING               
UNIVLIST DS    XL16                DEMOUT LIST FOR UNIVS                        
UNIVS    DS    4F                  UNIVS FOR MSA TSA ADI                        
RUNIVS   DS    4F                  REAL UNIVS FOR MULTIPLE BOOKS                
LASTVAL  DS    XL(LENDEM)          VALUE DURING RANKING                         
DEMSTYLE DS    CL1                 D=DEMO C=CATEGORY                            
STASTYLE DS    CL1                 M=MARKET L=LIST C=CUSTOM                     
UNIVBK   DS    CL2                 UNIVERSE BOOK                                
NCOMBOS  DS    CL1                 NUMBER OF COMBO STATIONS                     
PREVSTAT DS    CL5                                                              
MEDTYPE  DS    C                                                                
SRCTYPE  DS    C                                                                
BOOKTYPE DS    C                                                                
CITYCODE DS    CL3                                                              
MISCOMMA DS    C                                                                
OUTAREA  DS    A                                                                
RELO     DS    A                                                                
STALIST  DS    C                                                                
USEBUFF  DS    C                   USING BUFFER?                                
CMB_B4   DS    C                   COMBO BEFORE STATION?                        
FIRSTTM  DS    C                   FIRST TIME PRINTING?                         
PRNTD    DS    C                   PRINTED NEW MARKET NUMBER                    
STATLEN  DS    C                   STATIONS LEN                                 
NMARKET  DS    X                                                                
RUNIFLG  DS    C                   REAL UNIVERSE FLAG                           
NCMBDPT  DS    C                                                                
DPTTABL  DS    A                   TABLE OF DAYPART NAMES                       
PRDRSLT  DS    X                   PRODUCT RESULT OF MARKET X DEMOS             
USESHARE DS    C                                                                
*NCMBS    DS    X                   NO OF COMBOS                                
TOTDENOM DS    A                                                                
TMPDENOM DS    A                                                                
ADDPHS60 DS    A                   ADDRESS OF OVERLAY 60                        
PFKEY    DS    C                                                                
SUBSID   DS    C                                                                
*NOSHARE  DS    C                                                               
STALSTD  DS    C                   USED STATION LIST?                           
TMPBOOKL DS    CL16                TMP BOOK LIST, INCLUDES NBOOK                
         SPACE 1                                                                
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'120SPRES03   12/09/20'                                      
         END                                                                    
