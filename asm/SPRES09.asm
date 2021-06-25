*          DATA SET SPRES09    AT LEVEL 065 AS OF 12/09/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE T20F09A                                                                  
*                                                                               
*--------------------------------------------------------------------           
* 2/24/93:- COPY STATION VALIDATION METHOD FROM RANKER           (GH)           
*--------------------------------------------------------------------           
* 1/8/92: - NOT ALLOWING -CO STATIONS FOR INPUT.                                
*           THEY DON'T HAVE HOURLY DATA.                         (GH)           
*--------------------------------------------------------------------           
* 9/11/92:- FIXED A PROBLEM WITH THE USER LISTS.  MULTI MRKTS, 2ND              
*           ONE WASN'T VALIDATING WHEN USING NBOOK INPUT.                       
*           ADDED 'MVC  DBSELRMK,MKTNUM IN VMULTBK               (GH)           
*--------------------------------------------------------------------           
* 6/15/92:- APPLIED RANDOM DUPLICATION FORMULA TO CALCULATE CUMES               
*           AND TSL FOR COMBOS. THE FILLCMB ROUTINE HANDLES COMBO               
*           PROCESSING FOR THE 4 DAYPARTS.                      (MTA)           
*--------------------------------------------------------------------           
* 4/8/92: - PROBLEM WITH TSA RATINGS.  MADE SURE TO TEST FOR TSA                
*           DEMOS IN IMPAGN1 & IMPBUF3A/ CLI 0(R2),2                            
*         - IF COUNTRY IS CANADA, DEFAULT CALCOPT=N              (GH)           
*--------------------------------------------------------------------           
         TITLE 'T20F09 - RADIO HOURLY'                                          
T20F09   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20F09**,RA,RR=R2                                              
         LR    RE,RC                                                            
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    RE,MYWORK                                                        
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE                                                        
         L     RE,=V(SUBR07)                                                    
         A     RE,RELO                                                          
         ST    RE,VSUBR07                                                       
*                                                                               
         L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         SR    R0,R0                                                            
         IC    R0,TIOBAID          PICK UP PFKEY VALUE                          
         DROP  RF                                                               
         CH    R0,=H'12'           PF1 - PF12 IS OKAY                           
         BNH   *+8                                                              
         SH    R0,=H'12'           PF13 - PF24 = PF1 - PF12                     
         STC   R0,PFKEY            SAVE ADJUSTED PFKEY VALUE                    
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD KEY                          
         BE    VREC                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*              VALIDATE REQUEST AT MODE=VALREC                                  
         SPACE 3                                                                
VREC     LA    R2,HRSSRCEH         VALIDATE SOURCE                              
         MVI   STAMISS,C'Y'                                                     
         MVI   STALSTD,C'N'                                                     
         XC    NOA(2),NOA          ALSO CLEAR FIELD 'SPANN'                     
         GOTO1 VVALSRC                                                          
         CLI   DBSELMED,C'T'       IF TV                                        
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'       (ADJUST FOR RADIO)                           
         MVC   MEDTYPE,DBSELMED                                                 
         MVC   SRCTYPE,DBSELSRC                                                 
         XC    BOOKTYPE,BOOKTYPE                                                
* ------------------------------->     LATEST BOOK = 1BOOK                      
         CLC   HRSBOOK(4),=C'LATEST'                                            
         BNE   NLATEST                                                          
         MVC   HRSBOOK(5),=C'1BOOK'                                             
         MVC   HRSBOOK+5(3),HRSBOOK+6                                           
         MVI   HRSBOOK+8,C' '                                                   
NLATEST  LA    R2,HRSBOOK                                                       
         ZIC   R1,HRSBOOKH+5                                                    
         LTR   R1,R1                                                            
         BZ    VREC01A                                                          
         BCTR  R1,0                                                             
         AR    R1,R2                                                            
SRCHLP   CR    R2,R1                                                            
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
         SPACE 1                                                                
         MVC   SVMKTH5,HRSMKTH+5                                                
         XC    MKTNUM,MKTNUM                                                    
*                                                                               
         USING ELEMDS,R2                                                        
         GOTO1 =A(AFTPARSE),DMCB,(R9),(RC),RR=RELO                              
         BZ    MKTXST                                                           
*                                                                               
OUTMRKT  L     R2,OUTAREA                                                       
         CLI   0(R2),C'='                                                       
         BE    OUTMRKT1                                                         
         CLI   0(R2),C','                                                       
         BE    OUTMRKT1                                                         
         MVC   2(3,R2),=X'028800'                                               
OUTMRKT1 CLC   2(2,R2),=X'0288'                                                 
         BNH   OUTMRKT2                                                         
         MVC   CONHEAD(L'MKTERR2),MKTERR2                                       
         B     CPERR                                                            
OUTMRKT2 MVC   CONHEAD(L'MKTERR),MKTERR                                         
         B     CPERR                                                            
MKTXST   L     R2,OUTAREA                                                       
         GOTO1 VSUBR07,DMCB,('TESTMKE',(RC))                                    
         BNZ   OUTMRKT1                                                         
         STH   R1,MKTNUM                                                        
         GOTO1 =A(ALPHMRKT),DMCB,(R9),(RC),RR=RELO                              
         MVC   DBAREC,AIO1                                                      
         MVI   STASTYLE,C'M'                                                    
         MVI   DBFUNCT,DBGETMK                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'R'                                                    
         MVC   DBSELRMK,MKTNUM                                                  
*                                                                               
         LA    R2,HRSBOOKH         VALIDATE BOOK                                
         MVI   MAX,4                                                            
*                                                                               
         TM    HRSBOOK,X'F0'       SOFT BOOK REQUEST                            
         BNO   *+12                                                             
         BAS   RE,VMULTBK                                                       
         BE    VREC10              HAVE IT - BYPASS REGULAR EDIT                
*                                                                               
         CLI   MAXRANK,2           NOT ENOUGH BOOKS FOR MARKET                  
         BNE   *+14                                                             
         MVC   CONHEAD(L'BADMUL),BADMUL                                         
         B     CPERR                                                            
*                                                                               
         GOTO1 VRADBOOK                                                         
         SPACE 1                                                                
VREC10   LA    R2,HRSMKTH                                                       
         CLI   5(R2),0             ANY MARKET NETERED                           
         BNE   VREC11              YES VALIDATE IT FROM HERE                    
         LA    R2,OUTAREA          NO - CHECK USER LIST                         
VREC11   XC    ACTSTAT,ACTSTAT                                                  
         BAS   RE,EDITMKT          EDIT MARKET                                  
*                                                                               
         OC    MKTNUM,MKTNUM                                                    
         BZ    VRC11                                                            
         GOTO1 VSUBR07,DMCB,('EDITMBE',(RC))                                    
         BNE   BADBOOK                                                          
*                                                                               
VRC11    LA    R2,HRSOPTH          OPTIONS                                      
         BAS   RE,EDITOPT                                                       
         LA    R2,HRSDEMOH         VALIDATE DEMOGRAPHIC                         
         GOTO1 ANY                                                              
         MVI   MAX,8                                                            
         MVI   NFLDS,1                                                          
         GOTO1 VVALDEM                                                          
         MVC   SVNDEM,NDEMOS       SAVE THE NUMBER OF AGE/SEX CATS              
         CLI   NDEMOS,3                                                         
         BNH   VREC11A                                                          
         MVC   CONHEAD(L'TOOMNYDM),TOOMNYDM                                     
         B     MYEND                                                            
         SPACE 1                                                                
VREC11A  MVI   DEMSTYLE,C'D'                                                    
         CLI   NDEMOS,1                                                         
         BH    VREC12                                                           
         MVI   DEMSTYLE,C'C'                                                    
VREC12   MVC   PRPDEMOS,DEMOS                                                   
         MVC   PRPNDEMS,NDEMOS                                                  
         SPACE 1                                                                
         MVI   MAX,8                                                            
         LA    R2,HRSCATSH         DEMO CATEGORY                                
         GOTO1 ANY                                                              
         GOTO1 VVALCATS                                                         
*                                                                               
         CLI   SEPAOPT,C'D'                                                     
         BNE   VREC12_0                                                         
         MVC   NDEMOS,NCATS                                                     
         B     VREC12A                                                          
VREC12_0 CLI   SEPAOPT,C'C'                                                     
         BNE   VREC12A                                                          
         MVC   TMPDEMOS(16),PRPDEMOS                                            
         MVC   PRPDEMOS,DEMOS                                                   
         MVC   PRPNDEMS,NCATS                                                   
*                                                                               
         ZIC   RE,SVNDEM                                                        
         STC   RE,NDEMOS                                                        
         LA    R2,TMPDEMOS         WANT TO MOVE ONLY THE DEMO PART              
         LA    RF,DEMOS                                                         
VREC12_1 MVC   2(1,RF),2(R2)                                                    
         LA    R2,3(R2)                                                         
         LA    RF,3(RF)                                                         
         BCT   RE,VREC12_1                                                      
         MVI   0(RF),X'FF'                                                      
*                                                                               
VREC12A  CLI   NCATS,3              YES - UP TO 3 CATS                          
         BNH   VREC13                                                           
VREC12E  MVC   CONHEAD(L'TOOMNYCT),TOOMNYCT                                     
         B     MYEND                                                            
         SPACE 1                                                                
VREC13   CLI   SVNDEM,1            ONLY ONE DEMO                                
         BNH   VREC14                                                           
         CLI   NCATS,1              NO - CAN ONLY HAVE ONE CAT                  
         BNH   VREC14                                                           
         CLI   SEPAOPT,C'N'                                                     
         BE    VREC12E                                                          
VREC14   MVC   MSASAVE(5),ACTSTAT                                               
         L     R2,OUTAREA                                                       
         GOTO1 VSUBR07,DMCB,('EDITMLE',(RC))                                    
         BNE   CPERR                                                            
         TM    HRSBOOK,X'F0'                                                    
         BNO   VREC14A                                                          
*-----------------------------------------------------------------              
         MVC   DBAREC,AIO1                                                      
         MVI   STASTYLE,C'M'                                                    
         MVI   DBFUNCT,DBGETMK                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'R'                                                    
         MVC   DBSELRMK,MKTNUM                                                  
         BAS   RE,VMULTBK                                                       
*-----------------------------------------------------------------              
VREC14A  MVC   ACTSTAT(5),MSASAVE  STATION REQUIRED BY NOW                      
         OC    ACTSTAT,ACTSTAT                                                  
         BZ    VREC15                                                           
         MVC   ACTSTAT(5),MSASAVE                                               
         OC    MKTNUM,MKTNUM       NEED A MARKET BY NOW                         
         BNZ   VREC16                                                           
         MVC   CONHEAD(L'LISTERR),LISTERR                                       
         B     MYEND                                                            
         SPACE 1                                                                
VREC15   CLI   STAMISS,C'Y'        MISSING STATION?                             
         BNE   VREC16              NO!                                          
         LA    R2,HRSSTATH                                                      
         B     BADSTA                                                           
         SPACE 1                                                                
VREC16   LA    R2,HRSFILTH         FILTERS                                      
         BAS   RE,EDITFILT                                                      
*                                                                               
         L     R2,AIO2                                                          
         LA    R2,1000(R2)                                                      
         MVC   SVMKTNUM,MKTNUM                                                  
*                                                                               
VREC17   CLI   0(R2),0             EOL                                          
         BE    VREC17W                                                          
         CLI   0(R2),1                                                          
         BE    VREC17C                                                          
VREC17B  ZIC   RE,1(R2)                                                         
         AR    R2,RE                                                            
         B     VREC17                                                           
         SPACE 1                                                                
VREC17C  TM    HRSBOOK,X'F0'                                                    
         BNO   VREC17G                                                          
         MVC   MKTNUM,2(R2)                                                     
         MVI   HRSMKTH+5,0                                                      
         BAS   RE,VMULTBK                                                       
         MVC   HRSMKTH+5(1),SVMKTH5                                             
         CLI   NBOOKS,0                                                         
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
         GOTO1 VSUBR07,DMCB,('EDITMBE',(RC))                                    
         BE    VREC17B                                                          
BADBOOKM MVC   ERRBOOK+L'ERRBOOK-7(7),=C' MARKET'                               
BADBOOK  MVC   CONHEAD(L'ERRBOOK),ERRBOOK                                       
         MVC   CONHEAD+L'ERRBOOK+1(4),DBSELSTA                                  
         B     MYEND                                                            
VREC17W  MVC   HRSMKTH+5(1),SVMKTH5                                             
         MVC   MKTNUM,SVMKTNUM                                                  
*                                                                               
         LA    R2,HRSTTLH          CUSTOM TITLE                                 
         MVC   RESTITLE,=CL40'        AVERAGE HOUR REPORT'                      
         CLI   5(R2),0                                                          
         BE    VREC20                                                           
         GOTO1 ANY                                                              
         MVC   RESTITLE,WORK                                                    
         SPACE 1                                                                
VREC20   GOTO1 CENTER,DMCB,RESTITLE,40                                          
         SPACE 1                                                                
VRECX    B     XIT                                                              
         EJECT                                                                  
VMULTBK  NTR1                                                                   
         CLC   HRSBOOK+1(2),=C'BOOK'                                            
         BNE   XIT                                                              
         MVI   NBOOKS,0                                                         
*        MVC   BADMULP(1),HRSBOOK                                               
         XC    BOOKS,BOOKS                                                      
*        CLI   HRSMKTH+5,0                                                      
*        BE    *+8                                                              
*        BAS   RE,EDITMKT          DO THIS TO GET MARKET #                      
         LH    RE,MKTNUM           NOW SET MARKET IN STATION FIELD              
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DBSELSTA(4),DUB+5(3)                                             
         MVI   DBSELSTA+4,C'A'     BY CONVENTION                                
         MVC   DBSELRMK,MKTNUM                                                  
*                                                                               
         LA    R1,1                                                             
         MVC   MAXRANK(1),HRSBOOK  SET NUMBER OF BOOKS                          
         NI    MAXRANK,X'0F'                                                    
         CLI   MAXRANK,4                                                        
         BH    VMULTBKX                                                         
         CLI   HRSBOOK+5,C'('                                                   
         BNE   *+10                                                             
         MVC   DBBTYPE,HRSBOOK+6                                                
*                                                                               
         MVI   DBFUNCT,DBGETMB     GET BOOKS FOR MARKET                         
         GOTO1 DEMAND,DMCB,DBLOCK,BKHOOK                                        
         CLI   NBOOKS,0            NOT FOUND                                    
         BE    VMULTBKX                                                         
         XC    WORK,WORK                                                        
         CLC   NBOOKS,MAXRANK      NOT ENOUGH BOOKS                             
         BNE   VMULTBKX                                                         
*                                                                               
         ZIC   R0,MAXRANK          DON'T ALLOW PREV YEAR                        
         LA    RF,BOOKS                                                         
VMULTBK2 ZIC   R1,2(RF)                                                         
         LA    R1,WORK(R1)                                                      
         CLI   0(R1),0                                                          
         BE    *+12                                                             
         LA    R1,2                SET ERROR MESSAGE                            
         B     VMULTBKX                                                         
*                                                                               
         MVI   0(R1),C'0'                                                       
         LA    RF,4(RF)                                                         
         BCT   R0,VMULTBK2                                                      
*                                                                               
         SR    R1,R1                                                            
VMULTBKX LTR   R1,R1                                                            
         STC   R1,MAXRANK                                                       
         B     XIT                                                              
*                                                                               
BKHOOK   NTR1                                                                   
         L     R4,DBAREC                                                        
         USING SBKEY,R4                                                         
         TM    SBBOOK,X'80'        BYPASS REVERSE SEQ BOOKS                     
         BO    XIT                                                              
         CLC   SBBTYP,DBBTYPE                                                   
         BNE   XIT                                                              
* CHECK FOR DUPLICATE BOOKS                                                     
         ZIC   R1,NBOOKS                                                        
         BCTR  R1,0                                                             
         SLL   R1,2                *4                                           
         LA    R1,BOOKS(R1)                                                     
         CLC   1(2,R1),SBBOOK                                                   
         BE    XIT                                                              
*                                                                               
         ZIC   R1,NBOOKS                                                        
         LA    R1,1(R1)                                                         
         CLC   NBOOKS,MAXRANK      BOOK TABLE FULL - NEED A SLIDE               
         BNE   BKHOOK2                                                          
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
         EJECT                                                                  
*              EDIT MARKET (OPTIONALLY STATIONS BELOW)                          
         SPACE 3                                                                
EDITMKT  NTR1                                                                   
         OC    MKTNUM,MKTNUM                                                    
         BNZ   HAVEMKTN                                                         
         LA    R2,HRSMKTH          EITHER INPUT MARKET NUMBER                   
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 ANY                              OR STATION LIST                 
         GOTO1 SCANNER,DMCB,(R2),(1,BLOCK)                                      
         L     R1,BLOCK+4                                                       
         STH   R1,MKTNUM                                                        
HAVEMKTN MVI   STASTYLE,C'M'                                                    
         MVI   HRSMKTN,C'*'                                                     
         MVC   DBAREC,AIO1                                                      
         MVI   DBFUNCT,DBGETMK                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'R'                                                    
         MVC   DBSELRMK,MKTNUM                                                  
         GOTO1 DEMAND,DMCB,DBLOCK,MKTHOOK                                       
         CLI   HRSMKTN,C'*'                                                     
         BE    BADMKT                                                           
         B     EDITSTAT                                                         
         SPACE 1                                                                
MKTHOOK  NTR1                      HOOK FOR MARKET NAME                         
         OI    HRSMKTNH+6,X'80'                                                 
         MVC   HRSMKTN,SPACES                                                   
         L     R6,DBAREC                                                        
         USING DMKEY,R6                                                         
         LA    R6,DMFRSTEL                                                      
         USING DMELEM,R6                                                        
         ZIC   R1,DMLEN                                                         
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   HRSMKTN(0),DMMNAME                                               
         DROP  R6                                                               
         EJECT                                                                  
         SPACE 1                                                                
BADMKT   MVC   CONHEAD(L'MKTERR),MKTERR                                         
         B     MYEND                                                            
         SPACE 1                                                                
BADLIST  MVC   CONHEAD(L'LISTERR),LISTERR                                       
         B     MYEND                                                            
         SPACE 1                                                                
BADSTA   MVC   CONHEAD(L'STAERR),STAERR                                         
         LA    R1,CONHEAD          SEND THE STATION IN ERROR                    
         LA    R1,L'STAERR+1(R1)                                                
         MVC   0(5,R1),ACTSTAT                                                  
         B     MYEND                                                            
         EJECT                                                                  
*              EDIT CUSTOM STATIONS                                             
         SPACE 3                                                                
EDITSTAT LA    R2,HRSSTATH         OR A LIST OF STATIONS                        
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         LA    R0,1                MAXIMUM OF 12 FROM SCREEN                    
         GOTO1 ANY                 MUST BE AT LEAST 1                           
         SPACE 1                                                                
         L     RE,AIO2             SET A BIG SCANNER BLOCK                      
         LR    R4,RE                                                            
         LA    RF,2000                                                          
         XCEF                                                                   
         XC    STATSV,STATSV                                                    
         LA    R5,STATSV                                                        
         GOTO1 SCANNER,DMCB,(R2),(10,AIO2),C',=,-'                              
         MVC   DBSELMK,MKTNUM                                                   
STAT2    GOTO1 VSUBR07,DMCB,('VALDST2E',(RC))                                   
         BNE   BADSTA                                                           
*  SAVE A LIST OF ACTSTAT                                                       
         MVC   0(5,R5),ACTSTAT                                                  
         LA    R5,5(R5)                                                         
         LA    R4,L'SCLINE(R4)                                                  
         CLI   0(R4),0             EOL                                          
         BE    XIT                                                              
         B     STAT2                                                            
         DROP  R4                                                               
         EJECT                                                                  
*              EDIT OPTIONS                                                     
         SPACE 3                                                                
EDITOPT  NTR1                                                                   
         MVI   BOXOPT,C'Y'                                                      
         MVI   TRACEOPT,C'N'                                                    
         MVI   LEFTOPT,C'N'                                                     
         MVI   SPACOPT,1                                                        
         MVI   CALCOPT,C'Y'                                                     
         CLI   CTRY,C'C'           CANADA?                                      
         BNE   *+8                                                              
         MVI   CALCOPT,C'N'        YES, NO ADDITIONAL CALCULATIONS              
         MVI   SEPAOPT,C'N'                                                     
         MVI   ADDROPT,C'Y'                                                     
         MVI   AUTOOPT,C'N'                                                     
         CLI   5(R2),0                                                          
         BE    XIT                                                              
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
OPT4     CLC   12(4,R4),=C'LEFT'   LEFT OPTION                                  
         BNE   OPT6                                                             
         MVI   LEFTOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT6     CLC   12(2,R4),=C'S   '   SPACING OPTION                               
         BNE   OPT8                                                             
         MVC   SPACOPT,11(R4)                                                   
         CLI   SPACOPT,1                                                        
         BL    BADOPT                                                           
         CLI   SPACOPT,3                                                        
         BH    BADOPT                                                           
         B     OPTEND                                                           
         SPACE 1                                                                
OPT8     CLC   12(5,R4),=C'TRACE'  OPTION TO TRACE DEMAND HOOKS                 
         BNE   OPT10                                                            
         MVC   TRACEOPT,22(R4)                                                  
         B     OPTEND                                                           
         SPACE 1                                                                
OPT10    CLC   12(4,R4),=C'CALC'   OPTION TO USE IMPRESSIONS                    
         BNE   OPT12                                                            
         CLC   22(3,R4),=C'BOOK'                                                
         BE    OPT10A                                                           
         B     OPTEND                                                           
OPT10A   MVI   CALCOPT,C'N'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT12    CLC   12(3,R4),=C'SEP'    OPTION TO USE MULTIPLE DEMOS                 
         BNE   OPT13                                                            
         CLI   22(R4),C'D'                                                      
         BE    OPT12B                                                           
         CLI   22(R4),C'C'                                                      
         BE    OPT12B                                                           
         B     OPTEND                                                           
OPT12B   MVC   SEPAOPT,22(R4)                                                   
         B     OPTEND                                                           
         SPACE 1                                                                
OPT13    CLC   12(4,R4),=C'ADDR'   OPTION TO USE ADDRESS IN REPORT              
         BNE   OPT14                                                            
         CLI   22(R4),C'N'                                                      
         BNE   OPTEND                                                           
         MVI   ADDROPT,C'N'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT14    CLC   12(4,R4),=C'AUTO'   OPTION TO USE AUTO DELETION                  
         BNE   OPT30                                                            
         CLC   =C'DELETE',22(R4)                                                
         BNE   OPTEND                                                           
         MVI   AUTOOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT30    DS    0H                                                               
         SPACE 1                                                                
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
         B     SPERR                                                            
         SPACE 1                                                                
OPTEND   LA    R4,32(R4)                                                        
         BCT   R0,OPT2                                                          
         B     XIT                                                              
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
*              CONTROL REPORT PRINTING                                          
         SPACE 3                                                                
TMPDEMOS DC    XL60'00'                                                         
PRPNDEMS DC    X'00'                                                            
PRPDEMOS DC    XL16'00'                                                         
*                                                                               
PREP     L     R1,=A(HEDSPECS)     INTIALIZATION                                
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R2,MKTSTAT                                                       
         MVI   4(R2),C'A'                                                       
         EDIT  (2,MKTNUM),(4,(R2))                                              
*                                                                               
         LA    RE,4                                                             
         LR    R3,R2                                                            
MKTSTA1A CLI   0(R3),C' '                                                       
         BNE   MKTSTA1B                                                         
         MVI   0(R3),C'0'                                                       
MKTSTA1B LA    R3,1(R3)                                                         
         BCT   RE,MKTSTA1A                                                      
*                                                                               
         MVC   TMPDEMOS,DEMOS                                                   
         CLI   SEPAOPT,C'N'        ONLY ONE DEMO                                
         BE    PSEPX                                                            
         CLI   SEPAOPT,C'D'                                                     
         BNE   P_SEP00                                                          
         MVI   SVNDEM,1                                                         
         MVI   DEMSTYLE,C'C'                                                    
         B     P_SEP0                                                           
P_SEP00  MVI   DEMSTYLE,C'D'                                                    
P_SEP0   MVC   DEMOS,TMPDEMOS                                                   
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
         MVI   0(R2),X'FF'                                                      
PSEPX    MVI   COMBSW,C'N'                                                      
         MVI   RUNIFLG,C'N'                                                     
         MVC   TMPDEMOS,DEMOS                                                   
         CLI   CALCOPT,C'Y'                                                     
         BNE   NOTIMPS1                                                         
*--------------------------------------------------------------                 
*  EQUATES FOR IMPRESSIONS                                                      
*    1)  F = C                                                                  
*    2)  G = D                                                                  
*    3)  H = E                                                                  
*    4)  P = Q                                                                  
*    5)  R = I                                                                  
*--------------------------------------------------------------                 
         LA    R2,DEMOS                                                         
         ZIC   R0,NDEMOS                                                        
IMPAGN1  CLI   0(R2),2             TEST FOR TSA                                 
         BE    IMPNXT1             DON'T CONVERT RATINGS TO IMPRESSIONS         
*                                                                               
         CLI   1(R2),C'F'                                                       
         BNE   NOEQU1A                                                          
         MVI   1(R2),C'C'                                                       
         B     IMPNXT1                                                          
NOEQU1A  CLI   1(R2),C'G'                                                       
         BNE   NOEQU2A                                                          
         MVI   1(R2),C'D'                                                       
         B     IMPNXT1                                                          
NOEQU2A  CLI   1(R2),C'H'                                                       
         BNE   NOEQU3A                                                          
         MVI   1(R2),C'E'                                                       
         B     IMPNXT1                                                          
NOEQU3A  CLI   1(R2),C'P'                                                       
         BNE   NOEQU4A                                                          
         MVI   1(R2),C'Q'                                                       
         B     IMPNXT1                                                          
NOEQU4A  CLI   1(R2),C'R'                                                       
         BNE   NOEQU5                                                           
         MVI   1(R2),C'I'                                                       
         B     IMPNXT1                                                          
NOEQU5   CLI   1(R2),C'S'                                                       
         BNE   IMPNXT1                                                          
         CLI   DBSELSRC,C'M'                                                    
         BE    IMPNXT1                                                          
         MVI   1(R2),C'I'                                                       
         B     IMPNXT1                                                          
IMPNXT1  LA    R2,3(R2)            NEXT DEMO                                    
         BCT   R0,IMPAGN1                                                       
*                                                                               
NOTIMPS1 OI    HRSMKTH+6,X'80'     INSURE INPUT                                 
         XC    DPENTRY,DPENTRY                                                  
         XC    RUNIVS,RUNIVS                                                    
         L     RE,AIO2             SET THE USER LIST POINTER                    
         LA    RE,1000(RE)                                                      
         ST    RE,ULPNTR                                                        
         B     PREP4                                                            
         SPACE 1                                                                
PREP1    MVI   ANYUNIV,C'N'                                                     
         XC    UNIVBK,UNIVBK                                                    
         MVI   COMBNUM,0           SET FOR THIS MARKETS COMBOS                  
         XC    COMBSV,COMBSV                                                    
         LA    RE,BUFF             INITIALIZE MAIN BUFFER                       
         L     RF,=F'2000'                                                      
         XCEF                                                                   
         BAS   RE,COMPDISP         COMPUTE PRINT DISPLACEMENTS ETC              
         OC    MKTNUM,MKTNUM       JIC A LIST ONLY INPUT                        
         BZ    PREP4                                                            
         LA    R5,STATSV                                                        
         CLI   0(R5),0                                                          
         BNZ   PREP1A                                                           
         OC    ACTSTAT,ACTSTAT     JIC A LIST ONLY INPUT                        
         BNZ   PREP2                                                            
PREP1A   CLI   0(R5),0             MAYBE A LIST OF STATIONS                     
         BE    PREP4                                                            
         MVC   ACTSTAT(5),0(R5)                                                 
         LA    R5,5(R5)                                                         
         SPACE 1                                                                
PREP2    GOTO1 =A(FILLBUFF),DMCB,(R2),(R3),(R4),(R9),(RC),RR=RELO               
         MVI   SUMDPTFL,C'N'                                                    
         CLI   DBSELSRC,C'M'                                                    
         BE    PREP3                                                            
*        CLI   DEMOS,2                                                          
*        BE    PREP3                                                            
         GOTO1 VSUBR07,DMCB,('SETUNIAE',(RC))                                   
PREP3    BAS   RE,RNKDPT                                                        
         BAS   RE,SUMDPT                                                        
         GOTO1 =A(PRNTBUFF),DMCB,(R8),(R9),(RC),TMPDEMOS,RR=RELO                
         MVI   RUNIFLG,C'N'                                                     
         XC    RUNIVS(16),RUNIVS                                                
         LA    RE,BUFF                                                          
         L     RF,=F'2000'                                                      
         XCEF                                                                   
         B     PREP1A                                                           
         SPACE 1                                                                
PREP4    L     R1,ULPNTR           POINT TO CURR. USER LIST                     
         XC    STATSV,STATSV                                                    
         CLI   0(R1),0             NOTHING - JUST FINISH THE REPORT             
         BE    PREPXIT                                                          
         CLI   0(R1),1             MARKET ELEMENT                               
         BNE   PREP5                                                            
         MVC   MKTNUM,2(R1)        NO - DO THIS ONE NOW                         
         SPACE 1                                                                
         ZIC   RE,1(R1)            YES - TAKE A LOOK AT IT                      
         AR    R1,RE                                                            
         ST    R1,ULPNTR                                                        
         SPACE 1                                                                
         MVC   DBSELRMK,MKTNUM                                                  
         TM    HRSBOOK,X'F0'       RESET BOOK LIST IF NEEDED                    
         BNO   *+12                                                             
         MVI   HRSMKTH+5,0                                                      
         BAS   RE,VMULTBK                                                       
         SPACE 1                                                                
         MVC   DBAREC,AIO1                                                      
         MVI   DBFUNCT,DBGETMK                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'R'                                                    
         MVC   DBSELRMK,MKTNUM                                                  
         GOTO1 DEMAND,DMCB,DBLOCK,MKTHOOK                                       
         L     R1,ULPNTR                                                        
         SPACE 1                                                                
PREP5    CLI   0(R1),2             STATION LIST                                 
         BNE   PREP6                                                            
         ZIC   RE,1(R1)            SET UP FOR EXECUTE                           
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   STATSV(0),2(R1)       * EXECUTED * MOVE STAT LIST                
         ZIC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         ST    R1,ULPNTR                                                        
         LA    R5,STATSV           POINT TO NEW STATION LIST                    
         B     PREP1A              AND PROCESS IT                               
         SPACE 1                                                                
PREP6    CLI   0(R1),3             UL - COMBO ELEMENT                           
         BE    PREP8                                                            
         CLI   0(R1),0                                                          
         BE    PREPXIT                                                          
         CLI   0(R1),1                                                          
         BE    PREP4                                                            
         ZIC   RE,1(R1)            UNDEFINED - JUST BYPASS                      
         AR    R1,RE                                                            
         ST    R1,ULPNTR                                                        
         B     PREP4                                                            
         SPACE 1                                                                
PREP8    XC    DPENTRY,DPENTRY                                                  
         LA    RE,BUFF                                                          
         L     RF,=F'4000'                                                      
         XCEF                                                                   
         MVI   COMBSW,C'Y'                                                      
         LA    R2,DPENTRY                                                       
         ST    R1,CMBENTRY                                                      
         ZIC   RF,1(R1)                                                         
         AR    RF,R1               POINT TO NEXT ELEMENT                        
         LA    R1,11(R1)           POINT TO STATION                             
         LR    R5,R1                                                            
         LR    R6,RF                                                            
         ZIC   RE,NSTATS           ADD ENTRY TO STAT. LIST                      
         LA    RE,1(RE)                                                         
         STC   RE,NSTATS                                                        
PREP9    CR    R5,R6                                                            
         BNL   PREP10              END OF THIS ONE                              
         MVI   COMBNUM,1                                                        
         MVC   ACTSTAT,0(R5)      SET THE CALL LETTERS                          
         MVC   0(5,R2),0(R5)                                                    
         CLI   5(R5),0                                                          
         BE    *+8                                                              
         MVI   5(R2),C'+'                                                       
         LA    R2,6(R2)                                                         
         LA    R5,5(R5)                                                         
         GOTO1 =A(FILLBUFF),DMCB,(R2),(R3),(R4),(R9),(RC),RR=RELO               
         CLI   DBSELSRC,C'M'                                                    
         BE    PREP9                                                            
*        CLI   DEMOS,2                                                          
*        BE    PREP9                                                            
         GOTO1 VSUBR07,DMCB,('SETUNIAE',(RC))                                   
         B     PREP9                                                            
PREP10   MVI   SUMDPTFL,C'N'                                                    
         BAS   RE,AVEBUF                                                        
         BAS   RE,ADDBUF                                                        
         SPACE 1                                                                
         BCTR  R2,0                                                             
         MVI   0(R2),C' '                                                       
         SPACE 1                                                                
         LA    R1,10               SLIDE BUFFER TO NORMAL LOCATION              
         LA    RE,BUFF                                                          
         LA    RF,2000(RE)                                                      
         MVC   0(200,RE),0(RF)                                                  
         LA    RE,200(RE)                                                       
         LA    RF,200(RF)                                                       
         BCT   R1,*-14                                                          
         SPACE 1                                                                
         BAS   RE,RNKDPT                                                        
         BAS   RE,SUMDPT                                                        
         GOTO1 =A(PRNTBUFF),DMCB,(R8),(R9),(RC),TMPDEMOS,RR=RELO                
         MVI   COMBSW,C'N'         RESET COMBO INDICATOR                        
         XC    DPENTRY,DPENTRY                                                  
         L     R1,ULPNTR                                                        
         ZIC   RF,1(R1)                                                         
         AR    RF,R1                                                            
         ST    RF,ULPNTR                                                        
         MVI   RUNIFLG,C'N'                                                     
         XC    RUNIVS(16),RUNIVS                                                
         LA    RE,BUFF                                                          
         L     RF,=F'2000'                                                      
         XCEF                                                                   
         XC    ACTSTAT,ACTSTAT                                                  
         B     PREP1                                                            
PREPXIT  CLI   SEPAOPT,C'N'                                                     
         BE    PREPXIT1                                                         
         CLI   PRPNDEMS,0                                                       
         BZ    PREPXIT1                                                         
         CLI   SEPAOPT,C'C'                                                     
         BNE   P_SEP0                                                           
         MVC   CATTITS(10),CATTITS+5                                            
         B     P_SEP0                                                           
PREPXIT1 B     XIT                                                              
*                                                                               
         EJECT                                                                  
*              ROUTINE TO COMPUTE PRINT AND BUFFER DISPLACEMENTS                
         SPACE 3                                                                
*              OUTPUTS             DISP                                         
*                                  ABOOKBF                                      
         SPACE 1                                                                
COMPDISP NTR1                                                                   
         SPACE 1                                                                
         XC    DISP,DISP                                                        
         CLI   LEFTOPT,C'Y'        NO CENTERING IF LEFT OPTION                  
*        BE    XIT                 DON'T NEED THIS STATEMENT                    
         B     XIT                                                              
         EJECT                                                                  
*              COMPUTE DAYPART RANK NUMBERS                                     
         SPACE 3                                                                
*              OUTPUTS             RANKNUMS WITHIN EACH DAYPART                 
         SPACE 1                                                                
RNKDPT   NTR1                                                                   
         BAS   RE,AVEBUF                                                        
         GOTO1 =A(IMPBUF),DMCB,TMPDEMOS,(R9),(RC),RR=RELO                       
         LA    R3,RNKDPTTB                                                      
RNKDPTA  LA    R5,TDVLMF-TDSQH     DATA DISPLACEMENT                            
         LA    R0,9                3 DEMOS X 3 DAYS                             
RNKDPT1  ZIC   RE,0(R3)            FIRST HOUR                                   
         LR    R1,RE                                                            
         ZIC   R4,1(R3)            LAST HOUR                                    
         MH    RE,=AL2(L'TDENT)                                                 
         LA    RE,BUFF(RE)         INDEX TO START                               
         AR    RE,R5                                                            
         LA    RF,WORK             SORT AREA                                    
RNKDPT2  STC   R1,3(RF)            SAVE RANK                                    
         MVC   0(3,RF),0(RE)       SAVE 3 BYTE DEMO VALUE                       
         LA    R1,1(R1)                                                         
         CR    R1,R4                                                            
         BH    RNKDPT3                                                          
         LA    RE,L'TDENT(RE)                                                   
         LA    RF,4(RF)            NEXT BUCKET                                  
         B     RNKDPT2                                                          
         SPACE 1                                                                
RNKDPT3  ZIC   R1,0(R3)                                                         
         LA    R4,1(R4)                                                         
         SR    R4,R1               GET THE NUMBER OF ENTRIES                    
         GOTO1 XSORT,DMCB,(1,WORK),(R4),4,4,0                                   
         LA    RF,WORK                                                          
         LA    R1,1                FOR RANK COUNT                               
RNKDPT4  ZIC   RE,3(RF)            SEED THE RANK IN THE BUFFER                  
         MH    RE,=AL2(L'TDENT)                                                 
         LA    RE,BUFF(RE)                                                      
         AR    RE,R5                                                            
         MVC   0(4,RE),0(RF)       REPLACE DEMO AND RANK                        
         STC   R1,3(RE)            SAVE RANK                                    
         LA    R1,1(R1)                                                         
         LA    RF,4(RF)            NEXT BUCKET                                  
         BCT   R4,RNKDPT4                                                       
         SPACE 2                                                                
         LA    R5,4(R5)            BUMP THE INDEX                               
         BCT   R0,RNKDPT1                                                       
*                                                                               
         LA    R3,2(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   RNKDPTA                                                          
         B     XIT                                                              
         SPACE 1                                                                
RNKDPTTB DC    AL1(0,4)            5A-10A                                       
         DC    AL1(5,9)            10A-3P                                       
         DC    AL1(10,13)          3-7P                                         
         DC    AL1(14,19)          7P-1A                                        
         DC    X'FF',X'00'         END,ALIGN                                    
         EJECT                                                                  
* ********************************************************************          
* AVEBUF -     AVERAGE THE BUFFER                                               
* ********************************************************************          
AVEBUF   NTR1                                                                   
         LA    R4,BUFF             AVERAGE THE BUFFER                           
         USING TDD,R4                                                           
         CLI   SUMDPTFL,C'Y'                                                    
         BNE   AVEBUF1                                                          
         LA    RE,24                                                            
         MH    RE,=AL2(L'TDENT)                                                 
         AR    R4,RE               PT TO SUM-DPT ENTRY                          
         LA    R0,4                4 DAYPART GROUPS (AM,PM,DRV, ? )             
         B     AVEBUF2                                                          
AVEBUF1  LA    R0,24                                                            
AVEBUF2  CLI   TDCNT,2             DON'T NEED DIVIDE                            
         BL    AVEBUF6                                                          
         SR    R1,R1                                                            
         IC    R1,TDCNT                                                         
         LR    R5,R4               SAVE TAB START                               
         LA    R8,9                LOOP FOR THREE DAYS, 3 DEMS                  
AVEBUF4  SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,7,TDVLMF                                                      
         MH    RF,=H'100'                                                       
         DR    RE,R1                                                            
         AH    RF,=H'50'                                                        
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         STCM  RF,7,TDVLMF                                                      
         LA    R4,4(R4)            NEXT BUCKET                                  
         BCT   R8,AVEBUF4                                                       
         LR    R4,R5                                                            
AVEBUF6  LA    R4,L'TDENT(R4)      NEXT (DAYPART) ENTRY                         
         BCT   R0,AVEBUF2                                                       
         B     XIT                                                              
         EJECT                                                                  
* ********************************************************************          
* ADDBUF       ADD 2 BUFFERS TOGETHER                                           
* ********************************************************************          
ADDBUF   NTR1                                                                   
         LA    R4,BUFF             ADD TWO BUFFERS                              
         USING TDD,R4                                                           
         LA    R0,24                                                            
ADDBUF2  MVI   TDCNT,1             DON'T NEED DIVIDE                            
         LR    R5,R4               SAVE TAB START                               
         LA    R8,9                LOOP FOR THREE DAYS, 3 DEMS                  
         LA    R1,2000(R4)                                                      
         MVC   0(TDCNT-TDENT,R1),0(R4)                                          
ADDBUF4  SR    RE,RE                                                            
         ICM   RE,7,TDVLMF                                                      
         ICM   RF,7,TDVLMF-TDENT(R1)                                            
         AR    RE,RF                                                            
         STCM  RE,7,TDVLMF-TDENT(R1)                                            
         LA    R4,4(R4)            NEXT BUCKET                                  
         LA    R1,4(R1)                                                         
         BCT   R8,ADDBUF4                                                       
         LR    R4,R5                                                            
ADDBUF6  LA    R4,L'TDENT(R4)                                                   
         BCT   R0,ADDBUF2                                                       
         LA    RE,BUFF                                                          
         L     RF,=F'2000'                                                      
         XCEF                                                                   
         B     XIT                                                              
         EJECT                                                                  
* ********************************************************************          
* SUBDPT-      SUM DAYPARTS                                                     
* ********************************************************************          
*                                                                               
SUMDPT   NTR1                                                                   
         LA    RF,RNKDPTTB                                                      
         LA    R3,SUMDPTB                                                       
SUMDPT0  ZIC   RE,1(RF)                                                         
         MH    RE,=AL2(L'TDENT)                                                 
         LA    RE,BUFF(RE)                                                      
         MVC   0(1,RE),2(R3)       SET TOTAL INDEX                              
         OI    0(RE),X'80'         FLAG AS TOTAL                                
         LA    R3,7(R3)            ONWARD TO END                                
         LA    RF,2(RF)                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   SUMDPT0                                                          
*                                                                               
         CLI   COMBSW,C'Y'                                                      
         BNE   SUMDPT5                                                          
         GOTO1 =A(FILLCMB),DMCB,(R2),(R3),(R4),(R9),(RC),RR=RELO                
         MVI   SUMDPTFL,C'Y'                                                    
         BAS   RE,AVEBUF                                                        
         B     SUMDPT9                                                          
*                                                                               
SUMDPT5  GOTO1 =A(FILLDPTS),DMCB,(R2),(R3),(R4),(R9),(RC),RR=RELO               
         MVI   SUMDPTFL,C'Y'                                                    
         BAS   RE,AVEBUF                                                        
*                                                                               
SUMDPT9  GOTO1 =A(IMPBUF),DMCB,TMPDEMOS,(R9),(RC),RR=RELO                       
         MVI   SUMDPTFL,C'N'                                                    
         B     XIT                                                              
         SPACE 1                                                                
SUMDPTB  DC    AL1(1,4,24),AL2(0600,1000)                                       
         DC    AL1(5,9,25),AL2(1000,1500)                                       
         DC    AL1(10,13,26),AL2(1500,1900)                                     
         DC    AL1(14,18,27),AL2(1900,2400)                                     
         DC    X'FF'                                                            
*                                                                               
SUMDPTFL DC    C'N'                                                             
         EJECT                                                                  
*              HEADLINE HOOK                                                    
         SPACE 3                                                                
HOOK     NTR1                                                                   
         BAS   RE,SETBOX                                                        
         BAS   RE,SETTITLE                                                      
         MVC   BLOCK(42),SPACES    MARKET NUMBER AND NAME                       
         EDIT  (2,MKTNUM),(4,BLOCK)                                             
         MVC   BLOCK+5(30),HRSMKTN                                              
         CLI   STALSTD,C'Y'                                                     
         BNE   HOOK1                                                            
         MVC   BLOCK+36(6),=C'*LIST*'                                           
         OI    HRSMKTNH+6,X'88'                                                 
HOOK1    GOTO1 SQUASHER,DMCB,BLOCK,42                                           
         MVC   H4+7(42),BLOCK                                                   
         SPACE 1                                                                
         MVC   BLOCK(60),SPACES                                                 
         MVC   BLOCK(8),HRSSRCE    SOURCE, BOOK                                 
         LA    R1,BLOCK+10                                                      
         ZIC   R0,NBOOKS                                                        
         LA    R2,BOOKS+1                                                       
         BAS   RE,CONVBOOK                                                      
         MVC   0(9,R1),WORK                                                     
         LA    R1,10(R1)                                                        
         LA    R2,4(R2)                                                         
         BCT   R0,*-18                                                          
         GOTO1 SQUASHER,DMCB,BLOCK,50                                           
         MVC   H5(50),BLOCK                                                     
         SPACE 1                                                                
         MVC   H6(7),=C'STATION'                                                
         MVC   H6+8(36),DPENTRY                                                 
         CLI   COMBSW,C'Y'                                                      
         BE    HKTITLE                                                          
         MVC   H6+8(5),ACTSTAT                                                  
         CLI   ACTSTAT+4,C'C'                                                   
         BNE   *+10                                                             
         MVC   H6+12(3),=C'-CO'                                                 
         CLI   ACTSTAT+4,C'A'                                                   
         BNE   *+10                                                             
         MVC   H6+12(3),=C'-AM'                                                 
         CLI   ACTSTAT+4,C'F'                                                   
         BNE   *+10                                                             
         MVC   H6+12(3),=C'-FM'                                                 
         CLI   ACTSTAT+4,C'B'                                                   
         BNE   *+10                                                             
         MVC   H6+12(3),=C'-AA'                                                 
         CLI   ACTSTAT+4,C'D'                                                   
         BNE   *+10                                                             
         MVC   H6+12(3),=C'-FF'                                                 
         CLI   SPANN,C' '                                                       
         BNH   HKTITLE                                                          
         MVC   H6+14(1),SPANN                                                   
         MVC   H6+12(1),H6+13                                                   
         MVI   H6+13,C'#'                                                       
         SPACE 1                                                                
HKTITLE  LA    R2,H1+30            TITLE                                        
         MVC   0(40,R2),RESTITLE   ALREADY CENTERED                             
         GOTO1 UNDERLIN,DMCB,(40,(R2)),(X'BF',132(R2))                          
*                                                                               
         LA    R1,ARBTBL                                                        
ARBCPYRT CLI   0(R1),X'FF'                                                      
         BE    CNDMKT                                                           
         CLC   0(6,R1),HRSSRCE                                                  
         BE    *+12                                                             
         LA    R1,L'ARBTBL(R1)                                                  
         B     ARBCPYRT                                                         
         MVC   H3+27(55),=C'AUDIENCE ESTIMATES: COPYRIGHT XXXX THE ARBIX        
               TRON COMPANY'                                                    
         GOTO1 DATCON,DMCB,(3,BTODAY),(21,WORK)                                 
         MVC   H3+31+26(4),WORK+6                                               
*                                                                               
CNDMKT   L     RE,DBEXTEND         CHECK FOR CONDENSED MARKET                   
         USING DBEXTRAD,RE                                                      
CONDSLP  LTR   RE,RE                                                            
         BZ    NOCONDS                                                          
         CLC   =C'RADI',0(RE)                                                   
         BE    *+12                                                             
         L     RE,4(RE)                                                         
         B     CONDSLP                                                          
*--->    CLI   DBRADEMT,C'R'       RESTRICTED DEMO                              
         CLI   DBRAMTYP,C'C'       RESTRICTED MARKET FOR HOURLY                 
         BNE   NOCONDS                                                          
         MVC   H7+12(L'CNDHDR),CNDHDR                                           
         MVC   H8+12(L'CNDHDR2),CNDHDR2                                         
         DROP  RE                                                               
         SPACE 1                                                                
NOCONDS  LA    R2,H4+44            UNIVERSES                                    
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
         CLI   ANYTSA,C'Y'             TSA                                      
         BNE   XIT                                                              
         MVC   0(10,R2),TSASAVE                                                 
         BAS   RE,IUNV                                                          
         LA    R2,132(R2)                                                       
         B     XIT                                                              
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
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET UP BOXES                                          
         SPACE 3                                                                
*              OUTPUT              BOXCOLS BOXROWS                              
         SPACE 1                                                                
SETBOX   NTR1                                                                   
         CLI   BOXOPT,C'N'         OPTION TO SUPPRESS                           
         BE    XIT                                                              
         L     R4,ABOX             HANDLE BOXES IF WE'RE OFF LINE               
         USING BOXD,R4                                                          
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         SPACE 1                                                                
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         SPACE 1                                                                
         MVC   BOXROWS,SPACES      SET ROWS                                     
         MVI   BOXROWS+07,C'T'                                                  
         MVI   BOXROWS+10,C'M'                                                  
         MVI   BOXROWS+58,C'B'                                                  
         SPACE 1                                                                
         MVC   BOXCOLS,SPACES      NOW DO THE COLUMNS                           
         LA    R1,BOXCOLS                                                       
         A     R1,DISP                                                          
         SPACE 1                                                                
BOXSET2  MVI   0(R1),C'L'          LEFT                                         
         LA    R1,08(R1)                                                        
         MVI   0(R1),C'C'                                                       
         LA    R1,24(R1)                                                        
         MVI   0(R1),C'R'                                                       
         LA    R1,2(R1)                                                         
         MVI   0(R1),C'L'                                                       
         LA    R1,25(R1)                                                        
         MVI   0(R1),C'R'                                                       
         LA    R1,2(R1)                                                         
         MVI   0(R1),C'L'                                                       
         LA    R1,25(R1)                                                        
         MVI   0(R1),C'R'                                                       
         B     XIT                                                              
         EJECT                                                                  
*              SET UP TITLES                                                    
         SPACE 3                                                                
*              OUTPUTS             TITLES                                       
         SPACE 1                                                                
SETTITLE NTR1                                                                   
         CLI   ADDROPT,C'N'                                                     
         BNE   SETT1                                                            
         MVC   H2+76(32),SPACES                                                 
SETT1    LA    R3,H8+1                                                          
         A     R3,DISP                                                          
         LA    R3,15(R3)                                                        
         MVC   1(7,R3),=C'MON-FRI'                                              
         LA    R3,27(R3)                                                        
         MVC   0(8,R3),=C'SATURDAY'                                             
         LA    R3,27(R3)                                                        
         MVC   1(6,R3),=C'SUNDAY'                                               
         SPACE 1                                                                
         CLI   SVNDEM,1            MORE THAN ON DEMO EXCHANGE LOGIC             
         BH    SETTIT1                                                          
         LA    R3,H9+17                                                         
         A     R3,DISP                                                          
         GOTO1 DEMOCON,DMCB,DEMOS,(5,WORK),DBLOCK,0                             
         MVC   0(5,R3),WORK                                                     
         MVC   27(5,R3),WORK                                                    
         MVC   54(5,R3),WORK                                                    
         SPACE 1                                                                
         LA    R3,H10+1                                                         
         A     R3,DISP                                                          
         MVC   0(7,R3),=C' TIME  '                                              
         LA    R3,8(R3)                                                         
         LA    R3,2(R3)                                                         
         MVC   0(5,R3),CATTITS                                                  
         CLI   CATTITS+6,0                                                      
         BE    *+10                                                             
         MVC   8(5,R3),CATTITS+5                                                
         CLI   CATTITS+11,0                                                     
         BE    *+10                                                             
         MVC   16(5,R3),CATTITS+10                                              
         MVC   27(21,R3),0(R3)                                                  
         MVC   54(21,R3),0(R3)                                                  
         B     XIT                                                              
         SPACE 2                                                                
SETTIT1  DS    0C                                                               
         LA    R3,H9+17                                                         
         A     R3,DISP                                                          
         MVC   0(5,R3),CATTITS                                                  
         MVC   27(5,R3),CATTITS                                                 
         MVC   54(5,R3),CATTITS                                                 
         LA    R3,H10+1                                                         
         A     R3,DISP                                                          
         MVC   0(7,R3),=C' TIME  '                                              
         LA    R3,8(R3)                                                         
         LA    R3,2(R3)                                                         
         GOTO1 DEMOCON,DMCB,DEMOS,(5,WORK),DBLOCK,0                             
         MVC   0(5,R3),WORK                                                     
         CLI   NDEMOS,1                                                         
         BE    SETTIT2                                                          
         GOTO1 (RF),DMCB,DEMOS+3                                                
         MVC   8(5,R3),WORK                                                     
         CLI   NDEMOS,2                                                         
         BE    SETTIT2                                                          
         GOTO1 (RF),DMCB,DEMOS+6                                                
         MVC   16(5,R3),WORK                                                    
SETTIT2  MVC   27(21,R3),0(R3)                                                  
         MVC   54(21,R3),0(R3)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONVERT BOOK FOR HEADINGS                             
         SPACE 3                                                                
CONVBOOK NTR1                                                                   
         CLI   BOOKTYPE,C'B'       BLACK                                        
         BE    CNVEBTY                                                          
         CLI   BOOKTYPE,C'H'       AND HISPANIC - FUNNY BOOKS                   
         BE    CNVEBTY                                                          
         CLI   BOOKTYPE,C'P'                                                    
         BNE   CNVBK20                                                          
*                                                                               
         MVC   WORK(9),=C'MON/NN(P)'                                            
         LA    R3,WORK                                                          
         LA    R1,ARBMON                                                        
*                                                                               
CNVBK05  CLC   1(1,R2),3(R1)                                                    
         BNE   CNVBK10                                                          
         MVC   WORK(3),0(R1)                                                    
         LA    R3,WORK+4                                                        
         B     CONVB2                                                           
CNVBK10  LA    R1,4(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   CNVBK05                                                          
         MVC   WORK(6),=C'??????'                                               
         B     CONVB2                                                           
*                                                                               
CNVBK20  MVC   WORK(9),=C' FALL/NN '                                            
         LA    R3,WORK+6                                                        
         CLI   1(R2),11            NOVEMBER=FALL                                
         BE    CONVB2                                                           
         MVC   WORK(9),=C'WINTER/NN'                                            
         LA    R3,WORK+7                                                        
         CLI   1(R2),1             JAN=WINTER FOR BBM                           
         BE    CONVB2                                                           
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
         B     *+6                                                              
         SPACE 1                                                                
EQXIT    SR    R1,R1                                                            
         LTR   R1,R1                                                            
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
MYEND    MVI   ERROR,SUPPLIED      USING MY OWN ERROR MSG                       
         MVC   WORK,CONHEAD                                                     
ERREND   GOTO1 VGETERR                                                          
         SPACE 2                                                                
SPERR    MVI   ERROR,SUPPLIED                                                   
         MVC   WORK,CONHEAD                                                     
         GOTO1 VCURSERR,PARAS,0                                                 
         B     XIT                                                              
CPERR    MVI   ERROR,SUPPLIED                                                   
         MVC   WORK,CONHEAD                                                     
         GOTO1 VCPRSERR,PARAS,0                                                 
         B     XIT                                                              
         SPACE 2                                                                
GETELM   LR    R0,RE                                                            
         GOTO1 HELLO,PARAS,(C'G',SYSFIL),(ELCODE,(R4)),0,0                      
         L     R6,12(R1)                                                        
         LR    RE,R0                                                            
         CLI   12(R1),0                                                         
         BR    RE                                                               
         SPACE 2                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
ARBTBL   DS    0CL6                                                             
         DC    C'ARB   '                                                        
         DC    C'RARB  '                                                        
         DC    C'DRARB '                                                        
         DC    X'FF'                                                            
*                                                                               
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
         SPACE 2                                                                
PATCH    DS    0H                  PATCH AREA                                   
         DC    XL32'00'                                                         
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
OPTERR   DC    C'** ERROR ** INVALID OPTION'                                    
FILTERR  DC    C'** ERROR ** INVALID FILTER'                                    
TOOMNY   DC    C'** ERROR ** LIMITED TO 100 STATIONS'                           
TOOMNYDM DC    C'** ERROR ** ONLY 3 DEMOS ALLOWED'                              
TOOMNYCT DC    C'** ERROR ** ONLY 3 CATEGORIES ALLOWED'                         
INVDAYPT DC    C'** ERROR ** INVALID DAYPART'                                   
MKTERR   DC    C'** ERROR ** MARKET NOT FOUND'                                  
MKTERR2  DC    C'** MARKET MISSING OR MISSING M= IN USER LIST'                  
STAERR   DC    C'** ERROR ** STATION NOT FOUND'                                 
LISTERR  DC    C'** ERROR ** NEED GOOD MARKET IN LIST'                          
ERRBOOK  DC    C'REQUESTED BOOK(S) NOT FOUND- MARKET'                           
BADMUL   DC    C'MARKET IS NOT SWEPT (N) TIMES A YEAR '                         
BADMULP  EQU   BADMUL+21                                                        
VIO1     DC    C'#=VIOLATION: SPECIAL STATION ACTIVITIES. DETAILS: P.13+        
                ARB MKT. REPORT'                                                
VIO2     DC    C'A = STATION CITED  Z = STATION LISTED BELOW THE LINE'          
*NDHDR   DC    C'**CONDENSED MARKET - EXERCISE DISCRETION WHEN USING TH         
*              ESE ESTIMATES**'                                                 
*          DATA SET SPRES03    AT LEVEL 112 AS OF 10/30/96                      
CNDHDR   DC    C'DUE TO SMALLER SAMPLE SIZES IN CONDENSED MARKETS FOR TX        
               HIS DEMOGRAPHIC'                                                 
CNDHDR2  DC    C'AND/OR DAYPART THE USER SHOULD USE DISCRETION WHEN USIX        
               NG THESE ESTIMATES'                                              
         EJECT                                                                  
         SPACE 1                                                                
HEDSPECS DS    0H                  HEADLINE SPECS                               
         SPACE 1                                                                
         SSPEC H1,1,C'DDS RADIO RESEARCH'                                       
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H4,1,C'MARKET'                                                   
         SSPEC H6,1,C'DAYPART'                                                  
         SPACE 1                                                                
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,77,AGYADD                                                     
         SSPEC H4,77,RUN                                                        
         SSPEC H5,77,REPORT                                                     
         SSPEC H5,93,PAGE                                                       
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE RADSDPL                                                        
         EJECT                                                                  
*----------------------------------------------------------------               
AFTPARSE NMOD1 0,**AFTPARSE**                                                   
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         L     RE,AIO2             CLEAR A BIG AREA                             
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
* COMBINE MARKET AND USER LISTS                                                 
*                                                                               
         LA    R2,HRSMKTH          COMBINE MARKET                               
         ST    R2,LTABLE                                                        
         MVI   LTABLE,X'1'         1 ENTRY                                      
         LA    R2,HRSUL1H          AND INCLUDE USER LISTS                       
         ST    R2,LTABLE+4                                                      
         MVI   LTABLE+4,X'3'       3 ENTRIES                                    
         LA    R2,OUTAREA1                                                      
         CLI   HRSMKTH+5,0         CHECK IF EMPTY                               
         BZ    HHHH                                                             
         SPACE 1                                                                
         USING ELEMDS,R2                                                        
         SPACE 1                                                                
* NOT EMPTY, PUT IN M=                                                          
         SPACE 1                                                                
         MVI   HRSMKTH+4,X'C0'                                                  
         MVC   ELEMSTOP(ELEMBDAT+1),=X'7E01028800D4'                            
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
         LA    R2,HRSSTATH         COMBINE MARKET                               
         ST    R2,LTABLE                                                        
         MVI   LTABLE,X'1'         1 ENTRY                                      
         LA    R2,OUTAREA2                                                      
         CLI   HRSSTATH+5,0                                                     
         BZ    AFTPARX2                                                         
         SPACE 1                                                                
* NOT EMPTY, PUT IN S=                                                          
         MVI   HRSSTATH+4,X'C0'                                                 
         MVI   STAMISS,C'N'                                                     
         MVC   ELEMSTOP(ELEMBDAT+1),=X'7E0102D500E2'                            
         LA    R2,ELEMBDAT+1(R2)                                                
         GOTO1 VMINIPAR,DMCB,LTABLE,SCTABL,(X'50',(R2))                         
*-----------------------------------------------------------------              
* INSERTION OF STATION INTO MARKET AND USER LIST                                
*-----------------------------------------------------------------              
*                                                                               
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
*        MVC   OUTAREA1+3(3),=X'028800'                                         
         LTR   R1,R1               ERROR                                        
         XMOD1                                                                  
         SPACE 2                                                                
OUTAREA1 DC    CL255' '                                                         
OUTAREA2 DC    CL255' '                                                         
OUTAREA3 DC    CL255' '                                                         
TEMPR1   DC    F'0'                                                             
TEMPR3   DC    F'0'                                                             
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
XITCALFE MVC   CITYCODE,=X'000000'                                              
XITCALPH XMOD1                                                                  
         SPACE 1                                                                
CODETABL DC    CL8' '                                                           
         LTORG                                                                  
         EJECT                                                                  
IMPBUF   NMOD1 0,**IMPBUF**                                                     
         L     R6,0(R1)            TMPDEMOS                                     
         L     R9,4(R1)                                                         
         L     RC,8(R1)                                                         
*                                                                               
         CLI   CALCOPT,C'Y'                                                     
         BNE   IMPBUFX                                                          
         LA    R4,BUFF             AVERAGE THE BUFFER                           
         USING TDD,R4                                                           
         CLI   SUMDPTFL,C'Y'                                                    
         BNE   IMPBUF1                                                          
         LA    RE,24                                                            
         MH    RE,=AL2(L'TDENT)                                                 
         AR    R4,RE                                                            
         LA    R0,4                                                             
         B     IMPBUF2                                                          
IMPBUF1  LA    R0,24                                                            
IMPBUF2  LR    R5,R4               SAVE TAB START                               
         LA    R8,3                LOOP FOR THREE DAYS                          
IMPBUF3  LR    R2,R6                                                            
         LA    R7,3                AND THREE DEMOS                              
         LA    R1,RUNIVS                                                        
IMPBUF3A CLI   0(R2),2             TSA?                                         
         BE    IMPBUF5             DON'T DIVIDE BY UNIVERSE                     
*                                                                               
         CLI   1(R2),C'F'                                                       
         BE    IMPBUF4                                                          
         CLI   1(R2),C'G'                                                       
         BE    IMPBUF4                                                          
         CLI   1(R2),C'H'                                                       
         BE    IMPBUF4                                                          
         CLI   1(R2),C'P'                                                       
         BE    IMPBUF4                                                          
         CLI   1(R2),C'R'                                                       
         BE    IMPBUF4                                                          
         B     IMPBUF5                                                          
IMPBUF4  SR    RE,RE                                                            
         SR    RF,RF                                                            
         CLI   0(R2),2                                                          
         BL    IMPBUF40                                                         
         LA    R1,4(R1)                                                         
         CLI   0(R2),3                                                          
         BL    IMPBUF40                                                         
         LA    R1,4(R1)                                                         
IMPBUF40 ICM   RF,7,TDVLMF                                                      
         M     RE,=F'10000'                                                     
         OC    0(4,R1),0(R1)                                                    
         BNZ   IMPBUF4A                                                         
         SR    RF,RF                                                            
         B     IMPBUF4B                                                         
IMPBUF4A D     RE,0(R1)                                                         
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
IMPBUF4B STCM  RF,7,TDVLMF                                                      
IMPBUF5  LA    R4,4(R4)            NEXT BUCKET                                  
         LA    R2,3(R2)            NEXT DEMO                                    
         CLI   DEMSTYLE,C'D'                                                    
         BNE   IMPBUF5A                                                         
         LA    R1,4(R1)                                                         
IMPBUF5A BCT   R7,IMPBUF3A                                                      
         BCT   R8,IMPBUF3                                                       
         LR    R4,R5                                                            
IMPBUF6  LA    R4,L'TDENT(R4)                                                   
         BCT   R0,IMPBUF2                                                       
IMPBUFX  XMOD1                                                                  
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
SHRBUFF  NMOD1 0,**SHRBUFF**                                                    
         L     R4,0(R1)                                                         
         L     R6,4(R1)            HOLDS BUFFER OF TOTALS                       
         L     R2,8(R1)                                                         
         ST    R2,TDEMOS                                                        
         L     R9,12(R1)                                                        
         L     RC,16(R1)                                                        
*                                                                               
         LA    R3,3                MON-FRI,SAT AND SUN                          
         LA    R4,6(R4)            POINT TO FIRST ONE                           
         LR    R5,R4               SAVE IT FOR LATER                            
         LR    R7,R6                                                            
SHRB1    L     R2,TDEMOS                                                        
         ZIC   R1,NDEMOS                                                        
SHRB2    CLI   1(R2),C'S'          IS IT A SHR?                                 
         BNE   SHRB10                                                           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,7,0(R4)          GET IMPRESSION                               
         M     RE,=F'10000'                                                     
         SR    R8,R8                                                            
         L     R8,0(R6)                                                         
         LTR   R8,R8                                                            
         BNZ   SHRB5                                                            
         SR    RF,RF                                                            
         B     SHRB8                                                            
SHRB5    DR    RE,R8                                                            
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
SHRB8    STCM  RF,7,0(R4)                                                       
SHRB10   LA    R2,3(R2)            NEXT DEMO                                    
         LA    R4,4(R4)            NEXT BUCKET                                  
         LA    R6,4(R6)                                                         
         BCT   R1,SHRB2                                                         
SHRB15   LR    R6,R7                                                            
         LA    R6,12(R6)                                                        
         LR    R7,R6                                                            
         LR    R4,R5                                                            
         LA    R4,L'TDVLMF(R4)     NEXT DAYGROUP                                
         LR    R5,R4                                                            
         BCT   R3,SHRB1                                                         
SHRBUFFX XMOD1                                                                  
         SPACE 3                                                                
TDEMOS   DC    F'0'                                                             
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO FILL BUFFER FOR THIS STATION                          
         SPACE 3                                                                
*              INPUTS              R2=A(5-BYTE STATION)                         
*                                  R3=STATION NUMBER                            
*                                  R4=A(BUFFER FOR STATION)                     
*              OUTPUT              BUFFDEM(S)                                   
         SPACE 1                                                                
FILLBUFF NMOD1 0,**FILLBUFF**                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
         L     R9,12(R1)                                                        
         L     RC,16(R1)                                                        
*                                                                               
         USING BUFFD,R4                                                         
         STC   R3,BUFFSTAT                                                      
*                                                                               
         LA    RE,DBRADIC          SET UP FOR CONDENSED MARKETS                 
         ST    RE,DBEXTEND                                                      
         XC    0(128,RE),0(RE)                                                  
         USING DBRID,RE                                                         
         MVC   DBRID,=C'RADI'                                                   
         MVI   DBRCOPT,C'Y'                                                     
         DROP  RE                                                               
*                                                                               
         LA    R5,ACTSTAT                                                       
         MVI   DBSELMED,C'R'                                                    
         USING STATENTD,R2                                                      
         XC    DBSELMK,DBSELMK                                                  
FILLB1   MVC   DBSELMK,MKTNUM                                                   
         MVC   DBSELSTA,ACTSTAT                                                 
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBAREC,AIO1                                                      
         SPACE 1                                                                
         LA    R2,BOOKS            SET UP FOR MULTIPLE BOOKS                    
         ZIC   R0,NBOOKS                                                        
         SPACE 1                                                                
FILL2    MVC   DBSELBK,1(R2)                                                    
         MVC   DBBTYPE,3(R2)                                                    
         MVC   DBSELMK,MKTNUM                                                   
         XC    THISDEM,THISDEM     CLEAR BUFFER FOR STATION/BOOK                
         XC    THISWT,THISWT                                                    
         SPACE 1                                                                
         MVI   DBSELDAY,X'04'      FRI WILL GET M-F(ALL WE NEED)                
FILL6    MVC   DBFILE,=C'TP '      READ TP FOR CUSTOM DAYPARTS                  
         XC    DBSELPRG,DBSELPRG                                                
         MVC   DBSELTIM(2),=H'0500'                                             
         MVC   DBSELTIM+2(2),=H'0445'                                           
         SPACE 1                                                                
FILL7    CLC   =C'CMB',0(R5)                                                    
         BNE   FILL8                                                            
         ZIC   R7,3(R5)                                                         
         N     R7,=X'0000000F'                                                  
         L     R8,ULPNTR                                                        
MORECMBS CLI   0(R8),3                                                          
         BNE   MRECMBS1                                                         
         BCTR  R7,0                                                             
         LTR   R7,R7                                                            
         BZ    FIRSTCMB                                                         
         ZIC   R6,1(R8)                                                         
         AR    R8,R6                                                            
         B     MORECMBS                                                         
*                                                                               
MRECMBS1 ZIC   R6,1(R8)                                                         
         AR    R8,R6                                                            
         B     MORECMBS                                                         
FIRSTCMB ZIC   R6,1(R8)                                                         
         AR    R6,R8                                                            
         ST    R6,NEXTCOMB                                                      
         LA    R6,11(R6)                                                        
         MVC   DBSELSTA,0(R6)                                                   
*                                                                               
FILL8    GOTO1 DEMAND,DMCB,DBLOCK,DEMHOOK                                       
         CLC   =C'CMB',0(R5)                                                    
         BNE   FILL8D                                                           
         LR    RE,R5               GET BACK COMBOS BUFF PLACE                   
         S     RE,MYWORK                                                        
         SRDL  RE,32                                                            
         D     RE,=F'9'                                                         
         LR    RE,RF                                                            
         LA    R4,BUFF                                                          
         LA    RF,L'TDENT                                                       
         MR    RE,RE                                                            
         AR    R4,RF                                                            
* IF BUFFER HAD SOMETHING IN IT, DEMAND WILL ADD TO IT                          
         ZIC   RF,5(R4)            INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         STC   RF,5(R4)                                                         
         LA    R6,5(R6)            NEXT STATION IN COMBO                        
         C     R6,NEXTCOMB         FINISHED COMBO?                              
         BNL   CHNGBUFF                                                         
         MVC   DBSELSTA,0(R6)                                                   
         B     FILL8                                                            
CHNGBUFF MVC   1(4,R4),0(R5)                                                    
         ZIC   RF,NBOOKS                                                        
         STC   RF,5(R4)                                                         
FILL8D   CLI   DBSELDAY,X'01'      SUNDAY                                       
         BE    FILL9                                                            
         CLI   DBSELDAY,X'04'      FRI - DO SAT NEXT                            
         BNE   *+12                 NO - JUST DO SUNDAY                         
         MVI   DBSELDAY,X'02'                                                   
         B     FILL8                                                            
         MVI   DBSELDAY,X'01'                                                   
         B     FILL8                                                            
         SPACE 1                                                                
FILL9    CLI   RUNIFLG,C'Y'                                                     
         BE    FILL9A                                                           
         CLI   CALCOPT,C'Y'                                                     
         BNE   FILL9A                                                           
         BAS   RE,FRELUNIV                                                      
FILL9A   LA    R2,4(R2)                                                         
         BCT   R0,FILL2                                                         
         OC    RUNIVS,RUNIVS                                                    
         BZ    FILL9B                                                           
         CLI   RUNIFLG,C'Y'                                                     
         BE    FILL9B                                                           
*        BAS   RE,FAVEUNIV                                                      
         MVI   RUNIFLG,C'Y'                                                     
FILL9B   XMOD1                                                                  
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
         EJECT                                                                  
*              HOOK FROM DEMAND                                                 
         SPACE 2                                                                
*              INPUTS              R2=A(THISBUFF FOR DAYPART)                   
*                                  DEMOS NDEMOS                                 
*              OUTPUT              ADD WEIGHTED DEMOS TO THIS BUFF              
         SPACE 1                                                                
DEMHOOK  NTR1                                                                   
         CLI   RUNIFLG,C'Y'        DID WE GET IT?                               
         BE    DMHK0               YEAH, DON'T GET IT AGAIN                     
         CLI   DBSELSRC,C'M'                                                    
         BNE   DMHK0                                                            
         GOTO1 VSUBR07,DMCB,('SETUNIBE',(RC))                                   
*                                  ADD DEMOS TO THISBUFF                        
DMHK0    L     RE,DBAREC                                                        
         LA    RE,23(RE)                                                        
         USING MARELEM,RE                                                       
         CLI   MARELN,MARLNEQ2     EXTENDED '01' ELEMENT?                       
         BL    DMHK1               NO                                           
         CLI   MARACTCD,C' '                                                    
         BNH   DMHK1                                                            
         MVC   NOA(2),MARAIRTM     ALSO "MVC SPANN,MARACTCD"                    
         DROP  RE                                                               
         SPACE 1                                                                
DMHK1    XC    THISLINE,THISLINE                                                
         GOTO1 DEMOUT,DMCB,(C'L',DEMOS),DBLOCK,THISLINE                         
         SPACE 1                                                                
         GOTO1 DEFINE,DMCB,=C'TIME',DBLOCK,WORK                                 
         LA    R4,BUFF                                                          
         USING TDD,R4                                                           
         ZIC   RE,WORK             START QH                                     
         SRL   RE,2                                                             
         LA    RF,L'TDENT                                                       
         MR    RE,RE                                                            
         AR    R4,RF                                                            
         MVC   TDSHR(4),WORK+2                                                  
         LA    R3,TDVLMF                                                        
         CLI   DBSELDAY,1                                                       
         BNE   *+8                                                              
         LA    R3,TDVLSUN          SET TO DO SUNDAY                             
         CLI   DBSELDAY,2                                                       
         BNE   *+8                                                              
         LA    R3,TDVLSAT          SET TO SAT                                   
         CLI   DBSELDAY,X'04'      ONLY FOR FRI                                 
         BNE   DEMHOOK0                                                         
         ZIC   R1,NBOOKS           COUNT SO I CAN AVERAGE LATER                 
*        LA    R1,1(R1)                                                         
         STC   R1,TDCNT                                                         
DEMHOOK0 ZIC   R1,NDEMOS                                                        
         LA    RE,THISLINE                                                      
DEMHOOK1 SR    R2,R2                                                            
         ICM   R2,7,0(R3)                                                       
         A     R2,0(RE)                                                         
         STCM  R2,7,0(R3)                                                       
         LA    RE,4(RE)                                                         
         LA    R3,4(R3)            NEXT BUCKET                                  
         BCT   R1,DEMHOOK1                                                      
         SPACE 1                                                                
         CLI   TRACEOPT,C'Y'       OPTION TO TRACE                              
         BNE   XIT                                                              
         MVC   P(5),DBSELSTA                                                    
         LA    R2,P+6                                                           
         GOTO1 DEFINE,DMCB,=C'DAY',DBLOCK,WORK                                  
         MVC   0(4,R2),WORK+2                                                   
         LA    R2,5(R2)                                                         
         GOTO1 DEFINE,DMCB,=C'TIME',DBLOCK,WORK                                 
         GOTO1 UNTIME,DMCB,WORK+2,(R2)                                          
         LA    R2,12(R2)                                                        
         EDIT  (2,DBFACTOR),(5,(R2))                                            
         LA    R2,6(R2)                                                         
         EDIT  (4,THISLINE),(5,(R2))                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 3                                                                
NXTDPTS  DC    C'N'                NEXT TIME FILL DAYPARTS                      
NEXTCOMB DC    F'0'                                                             
         LTORG                                                                  
         EJECT                                                                  
* ********************************************************************          
* FILLCMB -    APPLY THE RANDOM DUPLICATION FORMULA TO CUME/TSL COMBO           
*              FOR IMPRESSIONS, JUST GET THE AVERAGE OVER BOOKS                 
* ********************************************************************          
*                                                                               
         DS    0H                                                               
FILLCMB  NMOD1 0,**FLCMB**                                                      
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
         L     R9,12(R1)                                                        
         L     RC,16(R1)                                                        
         ST    R3,ASUMDPTB         SAVE ADDRESS OF DAYPART TABLE                
*                                                                               
         LA    R2,DPENTRY          PT TO LIST OF STATNS IN COMBO                
         L     R5,CMBENTRY                                                      
         ZIC   R6,1(R5)                                                         
         AR    R6,R5                                                            
         ST    R6,NXTCMB           END OF LIST                                  
         LA    R5,11(R5)           PT TO STATION LIST FOR COMBO                 
*                                                                               
         BAS   RE,INITBUF          INIT BUFFS AND VARIABLES                     
*                                                                               
*--SORT STATIONS FOR CONSISTENCY IN RDF/TSL CALCULATIONS                        
         SR    R3,R3               R3= # STATIONS IN COMBO                      
         LR    R8,R5               PT TO 1ST STATN IN LIST                      
FLCMB10  LA    R3,1(R3)            BUMP STATN COUNTER                           
         LA    R8,5(R8)            NEXT STATION                                 
         C     R8,NXTCMB           FINISHED W/STATN LIST?                       
         BL    FLCMB10                                                          
         GOTO1 XSORT,DMCB,(R5),(R3),5,5,0                                       
         SPACE 2                                                                
*--LOOP THRU STATIONS IN COMBO LIST.  READ IN DATA AND PROCESS                  
         SPACE 1                                                                
FLCMB15  C     R5,NXTCMB           HAVE WE DONE ALL THE STATNS?                 
         BNL   FLCMB50                                                          
         MVI   COMBNUM,1                                                        
         MVC   ACTSTAT,0(R5)       SAVE 1ST STATION FOR FILLDPTS                
         CLI   5(R5),0                                                          
         BE    *+8                                                              
         LA    R2,6(R2)            NEXT STATION-LABEL FOR COMBO                 
         LA    R5,5(R5)            NEXT STATION IN LIST                         
*                                                                               
         L     R3,ASUMDPTB         PASS A(SUM-DAYPARTS TABLE)                   
         GOTO1 =A(FILLDPTS),DMCB,(R2),(R3),(R4),(R9),(RC),RR=RELO               
*                                                                               
         ZIC   RE,CNOSTA           BUMP COUNTER ON # STATIONS                   
         LA    RE,1(RE)                                                         
         STC   RE,CNOSTA                                                        
*                                                                               
         LA    R1,DPTLST           CALC CUME RTG,NEWCUME, AND CLR BUFF          
         MVI   DAYPT,0             INIT DAYPART COUNTER-LOOP THRU ALL 4         
*                                                                               
FLCMB25  ST    R1,LISTPTR          SAVE WHICH DAYPART WE'RE WORKING ON          
         BAS   RE,BUFFPLC          PT R4  TO RT POSN IN SOURCE BUFFER           
         BAS   RE,AVECMB           AVG DEMS JUST GOTTEN ON TDCNT/BOOKS          
         ZIC   RF,DAYPT            COMPUTE OFFSET INTO SAVE SUM BUFF            
         LA    RE,L'TDENT                                                       
         MR    RE,RE               DAYPART * L(DAYPART ENTRT BUFF)              
         LA    R3,CMBACCUM         R3: SAVE SUM DEMOS BUFFER                    
         AR    R3,RF               ADD ON (DAYPART) OFFSET                      
         CLI   CNOSTA,1            COPY IN HEADER STUFF ON 1ST STATN            
         BNE   *+10                INTO COMBO ACCUM                             
         MVC   0(5,R3),0(R4)                                                    
         LA    R3,6(R3)                                                         
         BAS   RE,ADDCMB2          ADD DEMOS TO PREV STAT DEMOS                 
*                                                                               
         LA    R6,CUMERAT          PT TO CUME RATING BUFFER                     
         ZIC   R0,DAYPT            COMPUTE OFFSET INTO BUFF                     
         MH    R0,=Y(CUMDPTQ)      DISP TO CUMERAT SAVE FOR DAYPART             
         AR    R6,R0               R6 PTS TO CUMERTG FOR THIS DAYPART           
         BAS   RE,CUMERTG          CALC CUME-RATINGS FOR CUMES                  
         CLI   CNOSTA,2            2 OR MORE STATIONS, APPLY RDF                
         BL    *+8                                                              
         BAS   RE,NEWCUME          AND CALC NEW CUME AND CUME SUM               
*                                                                               
         XC    5(L'TDENT-5,R4),5(R4)  CLR BUFFER FOR NEXT STATN DEMOS           
         LA    R1,5(R1)            NEXT DAYPART ENTRY IN LIST                   
         ZIC   RE,DAYPT            BUMP DAYPART COUNTER                         
         LA    RE,1(RE)                                                         
         STC   RE,DAYPT                                                         
         CLI   DAYPT,4             HAVE WE DONE ALL 4 DAYPARTS?                 
         BL    FLCMB25             NO, DO NEXT ONE                              
*                                                                               
         CLI   TSLFLG,C'Y'         ONLY NEED TO READ IMPS IF TSL REQSTD         
         BNE   *+8                                                              
         BAS   RE,GETIMP           READ IN IMPS FOR ALL DAYPARTS                
         B     FLCMB15             PROCESS NEXT STATION IN COMBO                
         SPACE 2                                                                
*                                                                               
FLCMB50  DS    0H                  DONE GETTING DATA FOR ALL STATIONS           
         LA    RE,DEMOS            RESTORE DEMO LIST                            
         LA    RF,SVDEMOS                                                       
FLCMB55  MVC   1(1,RE),0(RF)                                                    
         LA    RE,3(RE)            NEXT DEMO                                    
         LA    RF,1(RF)            NEXT SAVE-DEMO BUCKET                        
         CLI   0(RE),X'FF'                                                      
         BNE   FLCMB55                                                          
*                                                                               
FLCMB60  DS    0H                                                               
         LA    R0,4                DO 4 DAYPARTS                                
         LA    R1,DPTLIST                                                       
         LA    R2,CMBACCUM                                                      
         LA    R3,IMPACCUM         FOR TSL CALC                                 
*                                                                               
FLCMB65  ST    R1,LISTPTR          SAVE WHICH DAYPART WE'RE WORKING ON          
         CLI   TSLFLG,C'Y'         IF THERE'S A TSL DEMO, DO LAST CALC          
         BNE   *+8                 NO                                           
         BAS   RE,TSLCALC          DO FINAL TSL CALCULATION                     
         BAS   RE,BUFFPLC          PT R4  TO RT POSN IN SOURCE BUFFER           
         MVC   0(L'TDENT,R4),0(R2)  COPY FINAL CALC TO REAL BUFF                
         LA    R1,5(R1)            NEXT DAYPART ENTRY IN LIST                   
         LA    R2,L'TDENT(R2)      NEXT CMBACCUM  "                             
         LA    R3,IMPDPTQ(R3)      NEXT IMPACCUM  "                             
         BCT   R0,FLCMB65                                                       
*                                                                               
FLCMBX   XMOD1                                                                  
*                                                                               
XIT3     XIT1                                                                   
         EJECT                                                                  
* ------------------------------------------------------------------            
* VARIABLES  USED IN FLCMB AND IT'S RELATED PROCEDURES                          
* ------------------------------------------------------------------            
*                                                                               
NXTCMB   DC    F'00'                                                            
SVDEMOS  DC    XL6'00'                                                          
ASUMDPTB DC    F'00'               ADDRESS OF SUM-DPTS-TABLE                    
TSLFLG   DC    C'N'                SET TO C'Y' IF THERE'S A TSL DEMO            
DAYPT    DC    X'00'               DAYPART COUNTER FOR LOOP                     
CNOSTA   DC    X'00'               NUMBER STATIONS DONE SO FAR FOR CMBO         
IMPACCUM DC    XL(4*4*3*3)'00'     IMPRESS ACCUM FOR TSL (3BYTE BCKTS)          
IMPDPTQ  EQU   L'IMPACCUM/4        DISPL TO NEXT DAYPART ENTRY                  
CMBACCUM DC    XL(4*L'TDENT)'00'   DEMO SUM                                     
CUMERAT  DC    XL(4*4*3*3)'00'     CUME RATINGS (4BYTE BUCKETS)                 
CUMDPTQ  EQU   L'CUMERAT/4         DISPL TO NEXT DAYPART ENTRY                  
AVGUNIV  DC    F'00'                                                            
AUNIV    DC    F'00'               ADDRESS OF DPT IN BUFFER                     
ADPTBUF  DC    F'00'               ADDRESS OF DPT IN BUFFER                     
*                                                                               
DPTLST   DC    X'7C',AL1(01,01,1,24)  M-F/6-10A                                 
         DC    X'7C',AL1(02,02,1,25)  M-F/10A-3P                                
         DC    X'7C',AL1(03,03,1,26)  M-F/3-7P                                  
         DC    X'7C',AL1(04,04,1,27)  M-F/7P-12M                                
         DC    X'FF'                                                            
         SPACE 2                                                                
QTRTBL   DS    0XL2                                                             
         DC    AL1(06,10)             M-F/6-10A                                 
         DC    AL1(10,15)             M-F/10A-3P                                
         DC    AL1(15,19)             M-F/3-7P                                  
         DC    AL1(19,24)             M-F/7P-12M                                
         DC    X'FF'                                                            
         SPACE 2                                                                
LISTPTR  DC    A(DPTLST)                                                        
*                                                                               
         EJECT                                                                  
* -------------------------------------------------------------------           
* INITBUF -    INITIALIZE/CLEAR BUFFERS FOR COMBO PROCESSING                    
* -------------------------------------------------------------------           
INITBUF  NTR1                                                                   
         LA    R0,4                DO 4 DAYPARTS                                
         LA    R1,DPTLIST          CALC CUME RTG,NEWCUME, AND CLR BUFF          
INIT10   ST    R1,LISTPTR          SAVE WHICH DAYPART WE'RE WORKING ON          
         BAS   RE,BUFFPLC          PT R4  TO RT POSN IN SOURCE BUFFER           
         XC    5(L'TDENT-5,R4),5(R4)                                            
         LA    R1,5(R1)            NEXT DAYPART ENTRY IN LIST                   
         BCT   R0,INIT10                                                        
*                                                                               
         XC    CMBACCUM,CMBACCUM                                                
         XC    IMPACCUM,IMPACCUM                                                
         XC    CUMERAT,CUMERAT                                                  
         MVI   CNOSTA,0                                                         
         MVI   TSLFLG,C'N'         FLAG IF TSL WAS ONE OF DEMO REQSTD           
*                                                                               
         LA    R2,L'CUMERAT/4      INIT CUME RATING=1 FOR MULTIPLY              
         LA    R1,CUMERAT                                                       
         MVI   3(R1),1                                                          
         LA    R1,4(R1)                                                         
         BCT   R2,*-8                                                           
*                                                                               
         LA    RE,DEMOS            ORIG DEMO LIST                               
         LA    RF,SVDEMOS          CREATE SAVED LIST                            
INIT15   MVC   0(1,RF),1(RE)                                                    
         CLI   1(RE),C'X'          REPLACE TSL WITH CUME                        
         BNE   *+12                                                             
         MVI   1(RE),C'C'                                                       
         MVI   TSLFLG,C'Y'                                                      
         LA    RE,3(RE)            NEXT DEMO                                    
         LA    RF,1(RF)            NEXT SAVE-DEMO BUCKET                        
         CLI   0(RE),X'FF'                                                      
         BNE   INIT15                                                           
*                                                                               
*                                                                               
INITX    B     XIT3                                                             
*                                                                               
* -------------------------------------------------------------------           
* BUFFPLC -    SET (R4) TO PT TO BEGINING OF BUFFER FOR DAYPART ENTRY           
*              SPECIFIED IN LISTPTR                                             
* -------------------------------------------------------------------           
BUFFPLC  NTR1                                                                   
         LA    R4,BUFF                                                          
         USING TDD,R4                                                           
         L     RE,LISTPTR                                                       
         ZIC   RF,4(RE)                                                         
         LA    RE,L'TDENT                                                       
         MR    RE,RE                                                            
         AR    R4,RF                                                            
         XIT1  REGS=(R4)                                                        
*                                                                               
         EJECT                                                                  
* -------------------------------------------------------------------           
* AVECMB-      CALCULATE AVERAGE OF DEMO OVER MULTI BOOKS-FOR 1 DAYPT           
*              (R4) - DEMO SOURCE                                               
* -------------------------------------------------------------------           
AVECMB   NTR1                                                                   
         CLI   5(R4),2             TDCNT                                        
         BL    AVECMBX                                                          
         ZIC   R2,5(R4)            NUMBER BOOKS IN BUFFER                       
         MVI   5(R4),1             CLEAR OUT TDCNT AFTER AVGING                 
         LA    R4,6(R4)            1ST DEMO BUCKET                              
         LA    R0,3                LOOP THRU 3 DAY GROUPS:M-F,SAT,SUN           
AVECMB5  ZIC   R1,NDEMOS           WITHIN EACH GROUP, UP TO 3 DEMS              
         LR    R3,R4                                                            
AVECMB10 SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,7,0(R3)                                                       
         SLDA  RE,1                                                             
         DR    RE,R2                                                            
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         STCM  RF,7,0(R3)                                                       
         LA    R3,4(R3)            NEXT DEMO CATAG IN DAY GROUP                 
         BCT   R1,AVECMB10                                                      
*                                                                               
         LA    R4,L'TDVLMF(R4)     NEXT DAY GROUP                               
         BCT   R0,AVECMB5                                                       
*                                                                               
AVECMBX  B     XIT3                                                             
*                                                                               
* --------------------------------------------------------------------          
* GETIMP  -    READ IN IMPRESSIONS FOR TSL CALCULATION                          
* --------------------------------------------------------------------          
         DS    0H                                                               
GETIMP   NTR1                                                                   
*                                                                               
* --SET DEMO LIST TO READ IMPRESSIONS                                           
         LA    RE,DEMOS            SUBSITUTE TSL FOR IMPS                       
GETIMP2  MVI   1(RE),C'I'          SET TO READ IMPRESSIONS                      
         LA    RE,3(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   GETIMP2                                                          
*                                                                               
         L     R3,ASUMDPTB         PASS A(SUM-DAYPARTS TABLE)                   
         GOTO1 =A(FILLDPTS),DMCB,(R2),(R3),(R4),(R9),(RC),RR=RELO               
*                                                                               
         LA    R0,4                4 DAYPARTS                                   
         LA    R1,DPTLST           GET AVERAGE FOR ALL DAYPARTS                 
         LA    R3,IMPACCUM         SAVE SUM IMPS BUFFER                         
GETIMP4  ST    R1,LISTPTR          SAVE WHICH DAYPART WE'RE WORKING ON          
         BAS   RE,BUFFPLC          PT R4  TO RT POSN IN SOURCE BUFFER           
         BAS   RE,AVECMB           AVG DEMS JUST GOTTEN ON TDCNT/BOOKS          
         BAS   RE,ADDCMB2          ADD IMPR TO PREV STATN                       
         LA    R3,IMPDPTQ(R3)      NEXT DAYPART FOR IMPS                        
         XC    5(L'TDENT-5,R4),5(R4)  CLR BUFFER FOR NEXT STATN DEMOS           
         LA    R1,5(R1)            NEXT DAYPART                                 
         BCT   R0,GETIMP4                                                       
*                                                                               
* --RESTORE DEMO LIST FROM SVDEMOS--REPLACE 'X' (TSL) WITH 'C' CUME             
         LA    RE,DEMOS            READ IN IMP                                  
         LA    RF,SVDEMOS                                                       
GETIMP8  MVC   1(1,RE),0(RF)       REPLACE ORIG DEMO LIST                       
         CLI   1(RE),C'X'          TSL?                                         
         BNE   *+8                                                              
         MVI   1(RE),C'C'          SET TO READ CUMES INSTEAD                    
         LA    RE,3(RE)            NEXT DEMO                                    
         LA    RF,1(RF)            NEXT SAVE-DEMO BUCKET                        
         CLI   0(RE),X'FF'                                                      
         BNE   GETIMP8                                                          
*                                                                               
GETIMPX  B     XIT3                XIT                                          
         EJECT                                                                  
* --------------------------------------------------------------------          
* CUMERTG -    CALCULATE THE CUME RATING FOR EACH DEMO                          
*              (R4) -  CUMES JUST READ IN                                       
*              (R6) -  CUME RATINGS FOR DAYPART BEING PROCESSED                 
* --------------------------------------------------------------------          
*                                                                               
         DS    0H                                                               
CUMERTG  NTR1                                                                   
         LA    R4,6(R4)            CUME FOR THIS DEMO                           
         LA    RF,2*L'TDVLMF(R4)   ADDR OF BEG OF LAST DAYGROUP BUCKET          
         ST    RF,SVREG+8          SAVE ADDR OF BEG OF 1ST DAYGRP               
*                                                                               
CUMERG0  ZIC   R5,NDEMOS           NUMBER DEMOS IN LIST                         
         LA    R7,DEMOS            CALC CUMERTGS FOR DEMOS                      
         ST    R4,SVREG            SAVE FOR BUMPING  TO NEXT DAYGROUP           
         ST    R6,SVREG+4                                                       
         LA    RF,RUNIVS           SAVE ADDR OF BEG OF UNIV LIST                
         CLI   NCATS,1             ONLY ONE CATEGORY?                           
         BH    CUMRG0A             MORE THAN ONE: UNIV- MSA,TSA,ADI             
         CLI   NDEMOS,1            ONLY ONE DEMO?-UNIV:MSA,TSA,ADI              
         BH    CUMRG01             MORE THAN ONE: UNIV- DEM,DEM,DEM             
CUMRG0A  CLI   0(R7),2             TSA?   (#CAT > 1)                            
         BNE   *+8                                                              
         LA    RF,RUNIVS+4                                                      
         CLI   0(R7),3             ADI?                                         
         BNE   *+8                                                              
         LA    RF,RUNIVS+8                                                      
CUMRG01  ST    RF,AUNIV            SAVE ADDR OF UNIV FOR EA DEMO                
*                                                                               
CUMERG1  CLI   1(R7),C'C'          ONLY CALC CUME RATNGS FOR CUME DEMOS         
         BNE   CUMERG8             LOOP TO NEXT DEMO CATEGORY                   
         L     RF,AUNIV                                                         
         MVC   AVGUNIV,0(RF)                                                    
*                                                                               
         ZICM  R1,0(R4),(7)        GET CUME FOR THIS DEMO                       
         SR    R0,R0                                                            
         M     R0,=F'10000'                                                     
         STM   R0,R1,DUB                                                        
         L     R8,AVGUNIV          PUT (AVG) UNIVERSE IN R8                     
         LTR   R8,R8                                                            
         BNZ   *+12                                                             
         ST    R8,0(R6)            CUME RATING=0                                
         B     CUMERG8                                                          
*                                                                               
CUMERG5  LM    R0,R1,DUB           RESTORE NUMERATOR                            
         DR    R0,R8               DIVIDE CUME BY UNIVERSE                      
         AH    R1,=H'5'            ROUND                                        
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         SR    R0,R0                                                            
         M     R0,0(R6)            MULTIPLY BY PREV STATN CUME RATING           
         ST    R1,0(R6)            SAVE NEW (MULTIPLIED) CUME RATING            
*                                                                               
CUMERG8  LA    R4,4(R4)            NEXT DEMO IN BUFFER                          
         LA    R6,4(R6)            NEXT CUME RTG SAVE AREA                      
         LA    R7,3(R7)            NEXT DEMO CATEGORY                           
         L     RE,AUNIV                                                         
         CLI   NCATS,1                                                          
         BH    CUMERG8A                                                         
         CLI   NDEMOS,1            ONE CAT,ONE DEMO:UNIV-MSA,TSA,ADI            
         BE    CUMERG8A                                                         
         LA    RE,4(RE)            NEXT DEMO UNIVERS                            
         B     CUMERG9                                                          
*                                                                               
CUMERG8A LA    RE,RUNIVS           MSA                                          
         CLI   0(R7),2             TSA?                                         
         BNE   *+8                                                              
         LA    RE,RUNIVS+4                                                      
         CLI   0(R7),3             ADI?                                         
         BNE   *+8                                                              
         LA    RE,RUNIVS+8                                                      
*                                                                               
CUMERG9  ST    RE,AUNIV                                                         
         BCT   R5,CUMERG1          DO ALL DEMOS                                 
*                                                                               
         L     R4,SVREG            PT TO BEG OF THIS DAYGROUP                   
         LA    R4,L'TDVLMF(R4)            NEXT DAY GROUP                        
         L     R6,SVREG+4          PT TO BEG OF THIS DAYGROUP                   
         LA    R6,12(R6)           NEXT DAY GROUP                               
         L     RF,SVREG+8          HAVE WE DONE ALL DAYPROUPS?                  
         CR    R4,RF                                                            
         BNH   CUMERG0             DO ALL DAY GROUPS                            
*                                                                               
CUMERTGX B     XIT3                                                             
*                                                                               
SVREG    DC    5F'00'                                                           
*                                                                               
* ---------------------------------------------------------------------         
* IMPCMB -     CALCULATE AVG AND SUM OF IMPRESSIONS OVER STATNS IN CMBO         
* ---------------------------------------------------------------------         
         DS    0H                                                               
IMPCMB   NTR1                                                                   
         ZIC   R1,NDEMOS           NUMBER DEMOS IN BUFFER                       
         ZIC   R2,NBOOKS           NUMBER OF BOOKS                              
         LA    R4,6(R4)            FIRST DEMO                                   
         LA    R3,IMPACCUM+6       SUM OF IMPS OVER STATIONS IN COMBO           
*                                                                               
IMPCMB2  SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,7,0(R4)          PICK UP THIS STATN'S IMPRESSION              
         SLDA  RE,1                                                             
         DR    RE,R2                                                            
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         ZICM  RE,0(R3),(7)        PREV SUM OF IMPS                             
         AR    RE,RF               ADD THIS IMP TO PREV                         
*                                                                               
         STCM  RE,7,0(R3)          SAVE NEW SUM                                 
         LA    R3,4(R3)            NEXT SUM OF IMPS BUCKET                      
         LA    R4,4(R4)            NEXT IMP DEMO                                
         BCT   R1,IMPCMB2                                                       
*                                                                               
IMPCMBX  B     XIT3                                                             
         EJECT                                                                  
*                                                                               
* ---------------------------------------------------------------------         
* ADDCMB-      ADD THIS STATION'S DEMO VALUES TO THE OTHERS IN COMBO            
*              FOR A SINGLE DAYPART                                             
*              R3 - SAVE SUM BUFFER                                             
*              R4 - CURRENT DEMOS BUFFER                                        
* ---------------------------------------------------------------------         
         DS    0H                                                               
ADDCMB2  NTR1                                                                   
         LA    R4,6(R4)            1ST DEMO IN DAYPART BUFFER                   
         LA    R0,3                3 DAYGROUPS                                  
ADDCMB20 ZIC   R1,NDEMOS                                                        
         LR    R2,R4                                                            
         LR    R5,R3                                                            
ADCMB210 SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,7,0(R5)          SAVE BUCKET                                  
         ICM   RE,7,0(R2)          SOURCE BUCKET                                
         AR    RF,RE                                                            
         STCM  RF,7,0(R5)                                                       
         LA    R5,4(R5)            3BYTE SAVE SUM BCKTS                         
         LA    R2,4(R2)                                                         
         BCT   R1,ADCMB210                                                      
         LA    R4,L'TDVLMF(R4)     NEXT DAY GROUP                               
         LA    R3,L'TDVLMF(R3)                                                  
         BCT   R0,ADDCMB20         DO ALL DAYGROUPS:M-F,SAT,SUN                 
         B     XIT3                                                             
         EJECT                                                                  
* ---------------------------------------------------------------------         
* NEWCUME -  USE RANDOM DUPLICATION FORMULA TO CALCULATE NEW CUME               
*            RDF: CUME =(CUME1+CUME2) - (CUME1/UNIV * CUME2/UNIV *UNIV)         
*                      =CUME1+CUME2   - CUME1RATING * CUME2RATIG *UNIV)         
*            INPUTS:                                                            
*            (R3) - SUM OF CUMES IN CMBACCUM                                    
*            (R4) - CUMES FOR THIS DAYPART                                      
*            (R6) - CUME RATINGS FOR THIS DAYPART                               
* ---------------------------------------------------------------------         
NEWCUME  NTR1                                                                   
         LR    RF,R3               SUM OF CUMES                                 
         LA    R4,6(R4)            THIS DEMO'S CUMES                            
         LA    R1,2*L'TDVLMF(R4)   LAST DAYGRP'S CUMES                          
         ST    R1,SVREG+12                                                      
*                                                                               
NEWCUM0  LA    R5,DEMOS                                                         
         ZIC   R7,NDEMOS           NUMBER DEMOS IN BUFFER                       
         LA    RE,RUNIVS                                                        
         CLI   NCATS,1                                                          
         BH    *+12                                                             
         CLI   NDEMOS,1                                                         
         BH    NEWCUM01                                                         
         CLI   0(R5),2             TSA?                                         
         BNE   *+8                                                              
         LA    RE,RUNIVS+4                                                      
         CLI   0(R5),3             ADI?                                         
         BNE   *+8                                                              
         LA    RE,RUNIVS+8                                                      
NEWCUM01 ST    RE,AUNIV                                                         
         MVC   AVGUNIV,0(RE)                                                    
         ST    R4,SVREG            SAVE ADDR OF BEG OF THIS DAYGRP              
         ST    R6,SVREG+4          OF CUMES,CUMERTGS,SUM OF CUMES               
         ST    RF,SVREG+8                                                       
*                                                                               
NEWCUM1  CLI   1(R5),C'C'          DO CUMES ONLY                                
         BNE   NEWCUM8                                                          
*                                                                               
         L     R3,AVGUNIV          PUT  UNIV IN R3                              
         LTR   R3,R3               SEE IF IT'S ZERO                             
         BNZ   NEWCUM5                                                          
         ST    R3,0(R6)            CUMERTG=0                                    
         STCM  R3,7,0(RF)          CUME SUM=0 (IN CMBACCUM)                     
         B     NEWCUM8             GO TO NEXT DEMO                              
*                                                                               
NEWCUM5  SR    R2,R2                                                            
         M     R2,0(R6)            UNIVERSE*CUME RATING                         
*                                                                               
         D     R2,=F'1000'         DIVIDE FOR STATIONS-1                        
         SR    R2,R2                                                            
         D     R2,=F'100'                                                       
         A     R3,=F'5'                                                         
         SR    R2,R2                                                            
         D     R2,=F'10'                                                        
         ZICM  R1,0(RF),(7)        SUM OF CUMES                                 
         SR    R1,R3               SUM OF CUMES-(CUME RTGS * UNIVERSE)          
         STCM  R1,7,0(RF)          SAVE NEW CUME IN CMBACCUM BUFFER             
*                                                                               
         SR    R0,R0               CALCULATE NEW CUME RATING                    
         M     R0,=F'10000'                                                     
         D     R0,AVGUNIV          NEW CUME RATING=NEW CUME/UNIVERSE            
         AH    R1,=H'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         ST    R1,0(R6)            SAVE NEW CUME RATING                         
*                                                                               
NEWCUM8  LA    RF,4(RF)            NXT CMBACCUM (SUM OF CUMES FOR DEMO)         
         LA    R4,4(R4)            NEXT CUME FOR THE NEXT DEMO                  
         LA    R5,3(R5)            NEXT DEMO CATEGORY                           
         LA    R6,4(R6)            NEXT DEMO'S CUME RATINGS                     
         L     RE,AUNIV            BUMP UNIVERSE PTR                            
*                                                                               
         LA    RE,4(RE)            UNIV FOR NEXT DEMO                           
         CLI   NCATS,1                                                          
         BH    *+12                                                             
         CLI   NDEMOS,1                                                         
         BH    NEWCUM9                                                          
         LA    RE,RUNIVS                                                        
         CLI   0(R5),2             TSA?                                         
         BNE   *+8                                                              
         LA    RE,RUNIVS+4         ADI?                                         
         CLI   0(R5),3                                                          
         BNE   *+8                                                              
         LA    RE,RUNIVS+8                                                      
NEWCUM9  ST    RE,AUNIV                                                         
         MVC   AVGUNIV,0(RE)                                                    
         BCT   R7,NEWCUM1          GO THRU ALL DEMOS IN BUFFER                  
*                                                                               
         L     R4,SVREG            PT TO BEG OF THIS DAYGROUP- CUMES            
         LA    R4,L'TDVLMF(R4)     NEXT DAY GROUP                               
         L     R6,SVREG+4          CUME RATING                                  
         LA    R6,12(R6)                                                        
         L     RF,SVREG+8          CUMES SUM                                    
         LA    RF,L'TDVLMF(RF)                                                  
         L     RE,SVREG+12         HAVE WE DONE ALL DAYPROUPS?                  
         CR    R4,RE                                                            
         BNH   NEWCUM0             DO ALL DAY GROUPS                            
*                                                                               
NEWCUMEX B     XIT3                                                             
         EJECT                                                                  
* --------------------------------------------------------------------          
* TSLCALC -    READ IN IMPRESSIONS FOR TSL CALCULATION                          
*              INPUT:    (R1) - LISTPTR      -DAYPART                           
*                        (R2) - CMBACCUM     -CUMES                             
*                        (R3) - IMPACCUM     -IMPRESSIONS                       
* --------------------------------------------------------------------          
         SPACE 1                                                                
TSLCALC  NTR1                                                                   
*                                                                               
         LA    R2,6(R2)            PT TO 1ST CUME DEMO VALUE                    
         STM   R1,R3,SVREG                                                      
         LA    R6,3                3 DAYGROUPS (M-F,SAT,SUN)                    
*                                                                               
TSLCLC5  STC   R6,DYGRP            WHICH DAY GROUP THIS IS                      
         LA    R7,DEMOS            ONLY PROCESS IF DEMO CATEGORY=X=TSL          
         ZIC   R0,NDEMOS                                                        
*                                                                               
TSLCLC8  CLI   1(R7),C'X'          IS THIS DEMO TSL?                            
         BNE   TSLCLC10                                                         
         BAS   RE,QTRHRSCT         GET # QTRHRS FOR THIS DAYPT/DAY-GRP          
         SR    R4,R4               GO THRU EACH DEMO                            
         ZICM  R5,0(R3),(7)        GET IMPRESSIONS                              
         M     R4,=F'100'                                                       
         M     R4,NOQTRHRS         MULT BY # OF QTR HOURS                       
         OC    0(3,R2),0(R2)       CAN'T DIVIDE BY ZERO CUME                    
         BZ    TSLCLC10            KEEP IT AS ZERO                              
         ZICM  R1,0(R2),(7)                                                     
         DR    R4,R1               DIVIDE BY CUME                               
         A     R5,=F'5'                                                         
         SR    R4,R4                                                            
         D     R4,=F'10'                                                        
         STCM  R5,7,0(R2)          REPLACE IT WITH TSL                          
*                                                                               
TSLCLC10 LA    R7,3(R7)            NEXT DEMO CATEGORY                           
         LA    R2,4(R2)            NEXT CMBACCUM BUCKET FOR DAYGROUP            
         LA    R3,4(R3)            NEXT IMPACCUM BUCKET FOR DAYGROUP            
         BCT   R0,TSLCLC8          DO ALL DEMOS FOR THIS DAYGRP                 
*                                                                               
TSLCLC15 LM    R2,R3,SVREG+4       RESTORE PTRS TO BEG THIS DAYGRP              
         LA    R2,L'TDVLMF(R2)     NEXT DAYGRP'S CMBACCUM                       
         LA    R3,L'TDVLMF(R3)     NEXT DAYGRP'S IMPACCUM                       
         STM   R2,R3,SVREG+4                                                    
         BCT   R6,TSLCLC5          DO ALL DAYGROUPS                             
*                                                                               
         XIT1                                                                   
*                                                                               
DYGRP    DC    X'00'                                                            
TMPWRD   DC    F'01'                                                            
NOQTRHRS DC    F'80'                                                            
         EJECT                                                                  
*------------------------------------------------------------------             
* QTRHRSCT-    FIND THE NUMBER QUARTER HOURS FOR THIS DAYPART                   
*------------------------------------------------------------------             
QTRHRSCT NTR1                                                                   
*                                                                               
         L     R4,SVREG            GET LISTPTR FOR THIS DAYPART                 
         ZIC   R3,1(R4)            WHICH DAYPART                                
         BCTR  R3,0                                                             
         MH    R3,=Y(L'QTRTBL)                                                  
         LA    R4,QTRTBL                                                        
         AR    R4,R3                                                            
         XC    NOQTRHRS,NOQTRHRS                                                
*                                                                               
         ZIC   R1,1(R4)            END HOUR                                     
         ZIC   R2,0(R4)            START HOUR                                   
         SR    R1,R2                                                            
         MH    R1,=H'4'            4 QTR HOURS PER HOUR                         
         SR    RF,RF                                                            
*                                                                               
         LA    R2,X'7C'            SET DAYS TO M-F                              
         CLI   DYGRP,3             M-F?                                         
         BE    QHCUST2                                                          
         LA    R2,X'02'            SET DAYS TO SAT                              
         CLI   DYGRP,2             SAT?                                         
         BE    QHCUST2                                                          
         LA    R2,X'01'            SET DAYS TO SUN                              
*                                                                               
QHCUST2  LTR   R2,R2               LOOP THRU # DAYS                             
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
*                                                                               
QTRX     B     XIT3                                                             
         EJECT                                                                  
* *******************************************************************           
* FILLDPT -    ROUTINE TO FILL BUFFER WITH STATION'S DAYPARTS                   
*              INPUTS              R2=A(5-BYTE STATION)                         
*                                  R3=STATION NUMBER                            
*                                  R4=A(BUFFER FOR STATION)                     
*              OUTPUT              BUFFDEM(S)                                   
* *******************************************************************           
         SPACE 1                                                                
         DS    0H                                                               
FILLDPTS NMOD1 0,**FILLDPTS**                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
         L     R9,12(R1)                                                        
         L     RC,16(R1)                                                        
*                                                                               
         USING BUFFD,R4                                                         
         STC   R3,BUFFSTAT                                                      
*                                                                               
         LA    RE,DBRADIC          SET UP FOR CONDENSED MARKETS                 
         ST    RE,DBEXTEND                                                      
         XC    0(128,RE),0(RE)                                                  
         USING DBRID,RE                                                         
         MVC   DBRID,=C'RADI'                                                   
         MVI   DBRCOPT,C'Y'                                                     
         DROP  RE                                                               
*                                                                               
         LA    R5,ACTSTAT                                                       
         MVI   DBSELMED,C'R'                                                    
         USING STATENTD,R2                                                      
         XC    DBSELMK,DBSELMK                                                  
FILLD1   MVC   DBSELMK,MKTNUM                                                   
         MVC   DBSELSTA,ACTSTAT                                                 
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBAREC,AIO1                                                      
         SPACE 1                                                                
         LA    R2,BOOKS            SET UP FOR MULTIPLE BOOKS                    
         ZIC   R0,NBOOKS                                                        
         SPACE 1                                                                
FILLD2   MVC   DBSELBK,1(R2)                                                    
         MVC   DBBTYPE,3(R2)                                                    
         MVC   DBSELMK,MKTNUM                                                   
         XC    THISDEM,THISDEM     CLEAR BUFFER FOR STATION/BOOK                
         XC    THISWT,THISWT                                                    
         XC    DBSELTIM,DBSELTIM                                                
         LA    RE,DPTLIST                                                       
FILLD3   ST    RE,LISTPNTR                                                      
         SPACE 1                                                                
         MVI   DBSELDAY,X'7C'      FRI WILL GET M-F(ALL WE NEED)                
FILLD6   MVC   DBFILE,=C'RDP'      READ RDP FOR DAYPARTS                        
         XC    DBSELPRG,DBSELPRG                                                
         MVC   DBSELPRG+1(2),1(RE) DEIS: EXPLICIT LENGTH ADDED DEC/2018         
         SPACE 1                                                                
FILLD7   CLC   =C'CMB',0(R5)                                                    
         BNE   FILLD8                                                           
         ZIC   R7,3(R5)                                                         
         N     R7,=X'0000000F'                                                  
         L     R8,ULPNTR                                                        
MORECMBD CLI   0(R8),3                                                          
         BNE   MRECMBD1                                                         
         BCTR  R7,0                                                             
         LTR   R7,R7                                                            
         BZ    FRSTCMBD                                                         
         ZIC   R6,1(R8)                                                         
         AR    R8,R6                                                            
         B     MORECMBD                                                         
*                                                                               
MRECMBD1 ZIC   R6,1(R8)                                                         
         AR    R8,R6                                                            
         B     MORECMBD                                                         
FRSTCMBD ZIC   R6,1(R8)                                                         
         AR    R6,R8                                                            
         ST    R6,NXTCOMB                                                       
         LA    R6,11(R6)                                                        
         MVC   DBSELSTA,0(R6)                                                   
*                                                                               
FILLD8   GOTO1 DEMAND,DMCB,DBLOCK,DEMHOOKD                                      
         CLC   =C'CMB',0(R5)                                                    
         BNE   FILLD8D                                                          
         LR    RE,R5               GET BACK COMBOS BUFF PLACE                   
         S     RE,MYWORK                                                        
         SRDL  RE,32                                                            
         D     RE,=F'9'                                                         
         LR    RE,RF                                                            
         LA    R4,BUFF                                                          
         LA    RF,L'TDENT                                                       
         MR    RE,RE                                                            
         AR    R4,RF                                                            
* IF BUFFER HAD SOMETHING IN IT, DEMAND WILL ADD TO IT                          
         ZIC   RF,5(R4)            INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         STC   RF,5(R4)                                                         
         LA    R6,5(R6)            NEXT STATION IN COMBO                        
         C     R6,NXTCOMB          FINISHED COMBO?                              
         BNL   CHNGBUFD                                                         
         MVC   DBSELSTA,0(R6)                                                   
         B     FILLD8                                                           
CHNGBUFD MVC   1(4,R4),0(R5)                                                    
         ZIC   RF,NBOOKS                                                        
         STC   RF,5(R4)                                                         
FILLD8D  CLI   DBSELDAY,X'01'      SUNDAY                                       
         BE    FILLD9                                                           
         CLI   DBSELDAY,X'7C'      FRI - DO SAT NEXT                            
         BNE   *+12                 NO - JUST DO SUNDAY                         
         MVI   DBSELDAY,X'02'                                                   
         B     FILLD8                                                           
         MVI   DBSELDAY,X'01'                                                   
         B     FILLD8                                                           
         SPACE 1                                                                
FILLD9   L     RE,LISTPNTR                                                      
         LA    RE,5(RE)                                                         
         ST    RE,LISTPNTR                                                      
         CLI   0(RE),X'FF'                                                      
         BE    FILLD15                                                          
         B     FILLD3                                                           
FILLD15  LA    R2,4(R2)                                                         
         BCT   R0,FILLD2                                                        
FILLDXT  XMOD1                                                                  
*                                                                               
* USE DAYPART LIST INSTEAD OF SUMMING THE HOURS                                 
* HAVE TO READ FROM THE RDP FILE                                                
*                                                                               
DPTLIST  DC    X'7C',AL1(01,01,1,24)  M-F/6-10A                                 
         DC    X'7C',AL1(02,02,1,25)  M-F/10A-3P                                
         DC    X'7C',AL1(03,03,1,26)  M-F/3-7P                                  
         DC    X'7C',AL1(04,04,1,27)  M-F/7P-12M                                
         DC    X'FF'                                                            
         SPACE 2                                                                
LISTPNTR DC    A(DPTLIST)                                                       
NXTCOMB  DC    F'0'                                                             
*                                                                               
* HOOK FOR DEMAND FOR DAYPARTS                                                  
*                                                                               
DEMHOOKD NTR1                                                                   
*                                  ADD DEMOS TO THISBUFF                        
DMHKD0   L     RE,DBAREC                                                        
         LA    RE,23(RE)                                                        
         USING MARELEM,RE                                                       
         CLI   MARELN,MARLNEQ2     EXTENDED '01' ELEMENT?                       
         BL    DMHKD1              NO                                           
         CLI   MARACTCD,C' '                                                    
         BNH   DMHKD1                                                           
         MVC   NOA(2),MARAIRTM     ALSO "MVC SPANN,MARACTCD"                    
         DROP  RE                                                               
         SPACE 1                                                                
DMHKD1   XC    THISLINE,THISLINE                                                
         GOTO1 DEMOUT,DMCB,(C'L',DEMOS),DBLOCK,THISLINE                         
         SPACE 1                                                                
         GOTO1 DEFINE,DMCB,=C'TIME',DBLOCK,WORK                                 
         LA    R4,BUFF                                                          
         USING TDD,R4                                                           
         L     RE,LISTPNTR                                                      
         ZIC   RF,4(RE)                                                         
         LA    RE,L'TDENT                                                       
         MR    RE,RE                                                            
         AR    R4,RF                                                            
*                                                                               
         MVC   TDSHR(4),WORK+2                                                  
         LA    R3,TDVLMF                                                        
         CLI   DBSELDAY,1                                                       
         BNE   *+8                                                              
         LA    R3,TDVLSUN          SET TO DO SUNDAY                             
         CLI   DBSELDAY,2                                                       
         BNE   *+8                                                              
         LA    R3,TDVLSAT          SET TO SAT                                   
         CLI   DBSELDAY,X'7C'      ONLY FOR FRI                                 
         BNE   DMHOOKD0                                                         
         ZIC   R1,NBOOKS           COUNT SO I CAN AVERAGE LATER                 
*        LA    R1,1(R1)                                                         
         STC   R1,TDCNT                                                         
DMHOOKD0 ZIC   R1,NDEMOS                                                        
         LA    RE,THISLINE                                                      
DMHOOKD1 SR    R2,R2                                                            
         ICM   R2,7,0(R3)                                                       
         A     R2,0(RE)                                                         
         STCM  R2,7,0(R3)                                                       
         LA    RE,4(RE)                                                         
         LA    R3,4(R3)                                                         
         BCT   R1,DMHOOKD1                                                      
         SPACE 1                                                                
         CLI   TRACEOPT,C'Y'       OPTION TO TRACE                              
         BNE   XIT                                                              
         MVC   P(5),DBSELSTA                                                    
         LA    R2,P+6                                                           
         GOTO1 DEFINE,DMCB,=C'DAY',DBLOCK,WORK                                  
         MVC   0(4,R2),WORK+2                                                   
         LA    R2,5(R2)                                                         
         GOTO1 DEFINE,DMCB,=C'TIME',DBLOCK,WORK                                 
         GOTO1 UNTIME,DMCB,WORK+2,(R2)                                          
         LA    R2,12(R2)                                                        
         EDIT  (2,DBFACTOR),(5,(R2))                                            
         LA    R2,6(R2)                                                         
         EDIT  (4,THISLINE),(5,(R2))                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO FILL BUFFER WITH STATION'S TOTALS                     
         SPACE 3                                                                
*              INPUTS              R2=A(5-BYTE STATION)                         
*                                  R3=STATION NUMBER                            
*                                  R4=A(BUFFER FOR STATION)                     
*              OUTPUT              BUFFDEM(S)                                   
         SPACE 1                                                                
         DS    0H                                                               
FILLTOTS NMOD1 0,**FILLTOTS**                                                   
         L     R3,0(R1)            REAL BUFFER                                  
         L     R4,4(R1)            TOTALS BUFFER                                
         L     R9,8(R1)                                                         
         L     RC,12(R1)                                                        
*                                                                               
         XC    0(36,R4),0(R4)      CLEAR TOTALS BUFFER                          
         USING BUFFD,R4                                                         
*                                                                               
         LA    RE,DBRADIC          SET UP FOR CONDENSED MARKETS                 
         ST    RE,DBEXTEND                                                      
         XC    0(128,RE),0(RE)                                                  
         USING DBRID,RE                                                         
         MVC   DBRID,=C'RADI'                                                   
         MVI   DBRCOPT,C'Y'                                                     
         DROP  RE                                                               
*                                                                               
         LA    R5,MKTSTAT                                                       
         MVI   DBSELMED,C'R'                                                    
         USING STATENTD,R2                                                      
         XC    DBSELMK,DBSELMK                                                  
FILLT1   MVC   DBSELMK,MKTNUM                                                   
         MVC   DBSELSTA,MKTSTAT                                                 
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBAREC,AIO1                                                      
         SPACE 1                                                                
         LA    R2,BOOKS            SET UP FOR MULTIPLE BOOKS                    
         ZIC   R0,NBOOKS                                                        
         SPACE 1                                                                
FILLT2   MVC   DBSELBK,1(R2)                                                    
         MVC   DBBTYPE,3(R2)                                                    
         MVC   DBSELMK,MKTNUM                                                   
         XC    THISDEM,THISDEM     CLEAR BUFFER FOR STATION/BOOK                
         XC    THISWT,THISWT                                                    
         MVI   DBSELDAY,X'04'      FRI WILL GET M-F(ALL WE NEED)                
         XC    DBSELPRG,DBSELPRG                                                
         LA    RE,DPTLISTT                                                      
FILLT2A  CLI   0(RE),X'FF'                                                      
         BE    FILLT2C                                                          
         CLC   1(4,R3),0(RE)                                                    
         BE    FILLT5                                                           
         LA    RE,6(RE)                                                         
         B     FILLT2A                                                          
*                                                                               
FILLT2C  MVC   DBSELTIM(4),1(R3)   MOVE REAL BUFFER'S TIME OVER                 
         CLC   =X'0000',DBSELTIM+2        HAVE TO DO THIS                       
         BNE   FILLT2D                    FOR 11P-12M                           
         MVC   DBSELTIM+2(2),=H'2400'                                           
FILLT2D  SR    RE,RE                                                            
         ICM   RE,3,3(R3)          REAL BUFFER'S TIME                           
         SH    RE,1(R3)            REAL BUFFER'S END TIME                       
         CH    RE,=H'100'          CUSTOM DAYPART?                              
         BNH   FILLT7              YEP, USE TP                                  
* STANDARD DAYPART                                                              
FILLT5   MVC   DBFILE,=C'RDP'      READ RDP FOR STANDARD DAYPARTS               
         XC    DBSELTIM,DBSELTIM                                                
         MVC   DBSELPRG+1(2),4(RE) DEIS: EXPLICIT LENGTH ADDED DEC/2018         
         MVI   CUSTDAYP,C'N'                                                    
         B     FILLT8                                                           
* CUSTOM DAYPART                                                                
FILLT7   MVC   DBFILE,=C'TP '      READ TP FOR CUSTOM DAYPARTS                  
         MVI   CUSTDAYP,C'Y'                                                    
         SPACE 1                                                                
FILLT8   GOTO1 DEMAND,DMCB,DBLOCK,DEMHOOKT                                      
FILLT8D  CLI   DBSELDAY,X'01'      SUNDAY                                       
         BE    FILLT15                                                          
         CLI   DBSELDAY,X'04'      FRI - DO SAT NEXT                            
         BNE   *+12                 NO - JUST DO SUNDAY                         
         MVI   DBSELDAY,X'02'                                                   
         B     FILLT8                                                           
         MVI   DBSELDAY,X'01'                                                   
         B     FILLT8                                                           
         SPACE 1                                                                
FILLT15  LA    R2,4(R2)            NEXT BOOK                                    
         BCT   R0,FILLT2           UNTIL ALL BOOKS DONE                         
*                                                                               
         LA    R5,P+6                                                           
         LA    R2,9                NINE TOTALS                                  
FILLT20  L     RF,0(R4)                                                         
         SR    RE,RE                                                            
         CLI   NBOOKS,1                                                         
         BE    FILLT25                                                          
         M     RE,=F'10'                                                        
         ZIC   R3,NBOOKS           AVERAGE FOR # OF BOOKS                       
         DR    RE,R3                                                            
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         ST    RF,0(R4)                                                         
         CLI   TRACEOPT,C'T'                                                    
         BNE   FILLT25                                                          
         EDIT  (RF),(6,(R5))                                                    
         LA    R5,8(R5)                                                         
FILLT25  LA    R4,4(R4)                                                         
         BCT   R2,FILLT20                                                       
         CLI   TRACEOPT,C'T'                                                    
         BNE   FILLTXT                                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
FILLTXT  XMOD1                                                                  
*                                                                               
* USE DAYPART LIST INSTEAD OF SUMMING THE HOURS                                 
* HAVE TO READ FROM THE RDP FILE                                                
*                                                                               
DPTLISTT DC    AL2(0600,1000),AL1(1,1)     6-10A                                
         DC    AL2(1000,1500),AL1(2,2)     10A-3P                               
         DC    AL2(1500,1900),AL1(3,3)     3-7P                                 
         DC    AL2(1900,0000),AL1(4,4)     7P-12M                               
         DC    X'FF'                                                            
         SPACE 2                                                                
LISTPNTT DC    A(DPTLISTT)                                                      
NXTCOMBT DC    F'0'                                                             
CUSTDAYP DC    C'Y'                                                             
*                                                                               
* HOOK FOR DEMAND FOR DAYPARTS                                                  
*                                                                               
DEMHOOKT NTR1                                                                   
         CLI   TRACEOPT,C'T'                                                    
         BNE   DMHKT0                                                           
DMHKT0   L     RE,DBAREC                                                        
         LA    RE,23(RE)                                                        
         USING MARELEM,RE                                                       
         CLI   MARELN,MARLNEQ2     EXTENDED '01' ELEMENT?                       
         BL    DMHKT1              NO                                           
         CLI   MARACTCD,C' '                                                    
         BNH   DMHKT1                                                           
         MVC   NOA(2),MARAIRTM     ALSO "MVC SPANN,MARACTCD"                    
         DROP  RE                                                               
         SPACE 1                                                                
DMHKT1   XC    THISLINE,THISLINE                                                
         GOTO1 DEMOUT,DMCB,(C'L',DEMOS),DBLOCK,THISLINE                         
         SPACE 1                                                                
DMHKT1G  LA    R3,0(R4)                                                         
         CLI   DBSELDAY,1                                                       
         BNE   *+8                                                              
         LA    R3,24(R4)           SET TO DO SUNDAY                             
         CLI   DBSELDAY,2                                                       
         BNE   *+8                                                              
         LA    R3,12(R4)           SET TO DO SATURDAY                           
DMHOOKT0 ZIC   R1,NDEMOS                                                        
         LA    RE,THISLINE                                                      
DMHOOKT1 SR    R2,R2                                                            
         L     R2,0(R3)                                                         
         A     R2,0(RE)                                                         
         ST    R2,0(R3)                                                         
         LA    RE,4(RE)                                                         
         LA    R3,4(R3)                                                         
         BCT   R1,DMHOOKT1                                                      
         SPACE 1                                                                
         CLI   TRACEOPT,C'T'       OPTION TO TRACE                              
         BNE   XIT                                                              
         MVC   P(5),DBSELSTA                                                    
         LA    R2,P+6                                                           
         GOTO1 DEFINE,DMCB,=C'DAY',DBLOCK,WORK                                  
         MVC   0(4,R2),WORK+2                                                   
         LA    R2,5(R2)                                                         
         GOTO1 DEFINE,DMCB,=C'TIME',DBLOCK,WORK                                 
         GOTO1 UNTIME,DMCB,WORK+2,(R2)                                          
         LA    R2,12(R2)                                                        
         EDIT  (2,DBFACTOR),(5,(R2))                                            
         LA    R2,6(R2)                                                         
         EDIT  (4,THISLINE),(5,(R2))                                            
         LA    R2,6(R2)                                                         
         L     R3,THISLINE+4                                                    
         EDIT  (R3),(5,(R2))                                                    
         LA    R2,6(R2)                                                         
         L     R3,THISLINE+8                                                    
         EDIT  (R3),(5,(R2))                                                    
         LA    R2,10(R2)                                                        
         EDIT  (2,DBSELTIM),(4,(R2))                                            
         LA    R2,5(R2)                                                         
         SR    R3,R3                                                            
         ICM   R3,3,DBSELTIM+2                                                  
         EDIT  (R3),(4,(R2))                                                    
         LA    R2,5(R2)                                                         
         SR    R3,R3                                                            
         ICM   R3,3,DBSELPRG+1                                                  
         EDIT  (R3),(4,(R2))                                                    
         MVC   10(1,R2),CUSTDAYP                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CONTROL PRINTING OF BUFFER                            
         SPACE 3                                                                
*              OUTPUT              PRINT LINES                                  
         SPACE 1                                                                
         DS    0H                                                               
PRNTBUFF NMOD1 0,**PRNTBUFF**                                                   
         L     R8,0(R1)                                                         
         L     R9,4(R1)                                                         
         L     RC,8(R1)                                                         
         L     R2,12(R1)           TMPDEMOS                                     
*                                                                               
         CLI   CALCOPT,C'Y'                                                     
         BNE   PB1                                                              
         GOTO1 VSUBR07,DMCB,('SETNEWUE',(RC))                                   
PB1      LA    R4,BUFF                                                          
         USING TDD,R4                                                           
         LA    R0,24                                                            
         SPACE 1                                                                
PB2      OC    0(5,R4),0(R4)                                                    
         BZ    PB3                                                              
         MVC   SPACING,SPACOPT                                                  
         CLI   DBSELSRC,C'M'                                                    
         BE    PB2A                                                             
*----------------------------------------------------------------------         
* HERE, I HAVE TO READ IN THE TOTALS IF DBSELSRC <> 'M' (CANADIAN)              
* PUT THEM IN TOTBUFF                                                           
*----------------------------------------------------------------------         
         GOTO1 =A(FILLTOTS),DMCB,(R4),TOTBUFF,(R9),(RC),RR=RELO                 
         DS    0H                                                               
         GOTO1 =A(SHRBUFF),DMCB,(R4),TOTBUFF,(R2),(R9),(RC),RR=RELO             
PB2A     BAS   RE,FORMLINE                                                      
         GOTO1 SPOOL,DMCB,(R8)     ONE MORE LINE FOR BOX BOTTOM                 
PB3      TM    0(R4),X'80'         TOTAL LINE REQUIRED                          
         BZ    PB3A                                                             
         LR    R3,R4               SAVE MY INDEX                                
         NI    0(R4),X'7F'         POINT TO TOTAL BUFFER                        
         ZIC   RE,0(R4)                                                         
         MH    RE,=AL2(L'TDENT)                                                 
         LA    R4,BUFF(RE)                                                      
         CLI   DBSELSRC,C'M'                                                    
         BE    PB3_1                                                            
         GOTO1 =A(FILLTOTS),DMCB,(R4),TOTBUFF,(R9),(RC),RR=RELO                 
         DS    0H                                                               
         GOTO1 =A(SHRBUFF),DMCB,(R4),TOTBUFF,(R2),(R9),(RC),RR=RELO             
PB3_1    BAS   RE,FORMLINE         PRINT THE TOTAL                              
         MVI   SPACING,2           WITH EXTRA SPACING                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LR    R4,R3               RESTORE MY INDEX                             
         SPACE 1                                                                
PB3A     LA    R4,L'TDENT(R4)                                                   
         BCT   R0,PB2                                                           
         CLI   SPANN,C' '                                                       
         BNH   PB3B                                                             
         MVC   P(L'VIO1),VIO1                                                   
         MVC   P2(L'VIO2),VIO2                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
PB3B     MVI   FORCEHED,C'Y'       FORCE HEADS ON NEXT                          
         B     PBXIT                                                            
         SPACE 1                                                                
PB4      CLI   COMBNUM,0                                                        
         BE    PBXIT                                                            
         ZIC   R0,COMBNUM                                                       
         LA    R2,COMBSV           POINT TO A LIST OF PNTRS                     
         LA    R3,P+1                                                           
PB5      L     RF,0(R2)            GET A COMBO ELEMENT                          
         ZIC   RE,1(RF)                                                         
         AR    RE,RF               SET END                                      
         MVC   0(9,R3),2(RF)       AND MOVE IN THE COMBO NAM                    
         MVI   10(R3),C'='                                                      
         LA    R3,11(R3)                                                        
         LA    RF,11(RF)                                                        
PB6      CR    RF,RE                                                            
         BNL   PB7                                                              
         MVC   0(4,R3),0(RF)       MOVE IN AND FORMAT THE STATIONS              
         MVC   4(3,R3),=C'- M'                                                  
         MVC   5(1,R3),4(RF)                                                    
         LA    R3,7(R3)                                                         
         LA    RF,5(RF)                                                         
         CR    RF,RE                                                            
         BNL   PB7                                                              
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         B     PB6                                                              
         SPACE 1                                                                
PB7      LA    R2,4(R2)            CONTINUE TILL END                            
         LA    R3,3(R3)                                                         
         BCT   R0,PB5                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
PBXIT    XMOD1                                                                  
         EJECT                                                                  
*              FORMAT AND PRINT A REPORT LINE                                   
         SPACE 3                                                                
*              INPUTS              R2=A(STATION BUFFER)                         
*                                  ASTATS=A(5 CHARACTER STATION CODES)          
*                                  NBOOKS                                       
*                                  DISP                                         
         SPACE 1                                                                
         USING TDD,R4                                                           
FORMLINE NTR1                                                                   
         LA    R3,P+1                                                           
         A     R3,DISP                                                          
         OC    TDEHR,TDEHR                                                      
         BNZ   FRMLN0                                                           
* LINE UP THE '-'                                                               
         MVC   TEMP,=CL7' '                                                     
         MVC   TDEHR,=H'2400'                                                   
FRMLN0   GOTO1 UNTIME,DMCB,TDSHR,TEMP                                           
         LA    R5,TEMP+1           FIRST CHARACTER IS A #                       
         LA    R6,P+3                                                           
FRMLN1   CLI   0(R5),C'-'          HOUR SEPARATOR?                              
         BE    FRMLN2                                                           
         LA    R5,1(R5)                                                         
         BCTR  R6,0                                                             
         B     FRMLN1                                                           
FRMLN2   MVC   0(7,R6),TEMP                                                     
         CLC   TEMP(5),=C'9-10A'                                                
         BNE   *+8                                                              
         MVI   SPACING,2                                                        
         CLC   TEMP(4),=C'2-3P'                                                 
         BNE   *+8                                                              
         MVI   SPACING,2                                                        
         CLC   TEMP(4),=C'6-7P'                                                 
         BNE   *+8                                                              
         MVI   SPACING,2                                                        
         CLC   TEMP(4),=C'12A-1A'                                               
         BNE   *+8                                                              
         MVI   SPACING,2                                                        
         LA    R3,08(R3)                                                        
         BAS   RE,EDTDEML          M-F                                          
         LA    R4,L'TDVLMF(R4)                                                  
         BAS   RE,EDTDEML          SAT                                          
         LA    R4,L'TDVLMF(R4)                                                  
         BAS   RE,EDTDEML          SUN                                          
         B     XIT                                                              
         EJECT                                                                  
EDTDEML  NTR1                                                                   
         SR    R1,R1                                                            
         ICM   R1,7,TDVLMF                                                      
         MVC   BYTE,1(R2)                                                       
         BAS   RE,FORMDEM                                                       
         LA    R3,6(R3)                                                         
         EDIT  (B1,TDVLMF+3),(1,0(R3))                                          
         LA    R3,2(R3)                                                         
         CLI   NDEMOS,1                                                         
         BE    EDTDEML1                                                         
         SR    R1,R1                                                            
         ICM   R1,7,TDVLMF+4                                                    
         MVC   BYTE,4(R2)                                                       
         BAS   RE,FORMDEM                                                       
         LA    R3,6(R3)                                                         
         EDIT  (B1,TDVLMF+7),(1,0(R3))                                          
         LA    R3,2(R3)                                                         
         CLI   NDEMOS,2                                                         
         BE    EDTDEML2                                                         
         SR    R1,R1                                                            
         ICM   R1,7,TDVLMF+8                                                    
         MVC   BYTE,7(R2)                                                       
         BAS   RE,FORMDEM                                                       
         LA    R3,6(R3)                                                         
         LA    RE,TDVLMF+11                                                     
         EDIT  (B1,(RE)),(1,0(R3))                                              
         LA    R3,2(R3)                                                         
         LA    R3,3(R3)            SKIP PAST BOXES                              
         B     EDTDEMLX                                                         
         SPACE 1                                                                
*        ADJUST FOR MISSING DEMOS                                               
EDTDEML1 LA    R3,8(R3)            ONLY ONE DEMO                                
EDTDEML2 LA    R3,11(R3)           ONLY 2 DEMOS                                 
         SPACE 1                                                                
EDTDEMLX XIT1  REGS=(R3)                                                        
         EJECT                                                                  
*              FORMAT FOR A BOOK                                                
         SPACE 3                                                                
*              INPUT               BYTE = DEMO MODIFIER                         
*                                  R1 = DEMO VALUE                              
*                                  R3=A(OUTPUT AREA)                            
         SPACE 1                                                                
FORMDEM  NTR1                                                                   
         EDIT  (R1),(5,0(R3)),1    DISPLAY DEMOS.                               
         CLI   BYTE,C'R'           1 DEC. FOR RATINGS ETC.                      
         BE    FORMDEM2                                                         
         CLI   BYTE,C'F'                                                        
         BE    FORMDEM2                                                         
         CLI   BYTE,C'G'                                                        
         BE    FORMDEM2                                                         
         CLI   BYTE,C'H'                                                        
         BE    FORMDEM2                                                         
         CLI   BYTE,C'L'                                                        
         BE    FORMDEM2                                                         
         CLI   BYTE,C'S'                                                        
         BE    FORMDEM2                                                         
         CLI   BYTE,C'X'                                                        
         BE    FORMDEM5                                                         
         EDIT  (R1),(5,0(R3))      ELSE NO DECIMAL PLACES                       
         SPACE 1                                                                
FORMDEM2 B     XIT                                                              
*                                                                               
FORMDEM5 XC    0(5,R3),0(R3)                                                    
         MVI   2(R3),C':'                                                       
         SR    R0,R0                                                            
         M     R0,=F'15'                                                        
         D     R0,=F'600'          QUARTER HOURS W/O DECIMAL                    
         LR    RF,R0               EVEN REGISTER IS REMAINDER                   
         EDIT  (R1),(2,0(R3)),ZERO=NOBLANK      # OF HOURS                      
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         EDIT  (RF),(2,3(R3)),FILL=0                                            
         B     XIT                                                              
         DROP  R2                                                               
         SPACE 3                                                                
TOTBUFF  DC    9F'0'                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
SUBR07   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SR07**,RA,RR=R7,END=SUBR07X                                  
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R7,RELO2            SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE2                                                       
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R1,0(R1)                                                         
         SRL   R1,24               GET THE ROUTINE NUMBER                       
         SLL   R1,2                AND BRANCH ADDRESS                           
         LA    RF,*+2(R1)                                                       
         BR    RF                                                               
         SPACE 2                                                                
VALDSTAE EQU   (VALDSTA#-*)/4+1                                                 
VALDST2E EQU   (VALDST2#-*)/4+1                                                 
EDITMLE  EQU   (EDITML#-*)/4+1                                                  
WRTWAE   EQU   (WRTWA#-*)/4+1                                                   
RESTWAE  EQU   (RESTWA#-*)/4+1                                                  
M1INITE  EQU   (M1INIT#-*)/4+1                                                  
GENDPTE  EQU   (GENDPT#-*)/4+1                                                  
EDITMBE  EQU   (EDITMB#-*)/4+1                                                  
SETUNIAE EQU   (SETUNIA#-*)/4+1                                                 
SETUNIBE EQU   (SETUNIB#-*)/4+1                                                 
SETNEWUE EQU   (SETNEWU#-*)/4+1                                                 
TESTMKE  EQU   (TESTMK#-*)/4+1                                                  
         SPACE 2                                                                
VALDSTA# B     VALDSTA        1    VALIDATE A STAT. ON DEM FILE                 
*                                   (FROM SCREEN INPUT)                         
EDITML#  B     EDITML         2    EDIT A USER LIS                              
WRTWA#   B     WRTWA          3    WRITE A TWA (TWANUM=WHICH ONE)               
RESTWA#  B     RESTWA         4    RESTORE A TWA                                
M1INIT#  B     M1INIT         5    INITIALIZE A MKT LIST SCREEN                 
GENDPT#  B     GENDPT         6    GENERATE DAYPART SCREEN                      
VALDST2# B     VALDST2        7    VALIDATE A STAT. ON DEM FILE                 
*                                   (FROM SCANNER BLOCK)                        
EDITMB#  B     EDITMB         8    VALIDATE LIST OF BOOKS FOR MKT               
SETUNIA# B     SETUNIVA       9    EDIT THE UNIVERSES                           
SETUNIB# B     SETUNIVB       9    EDIT THE UNIVERSES                           
SETNEWU# B     SETNEWUN      10    EDIT THE UNIVERSES                           
TESTMK#  B     TESTMRKT      11    TEST THE MARKET                              
         EJECT                                                                  
HSTAT    DC    X'0'                                                             
CMBCOUNT DC    X'0'                                                             
TEMPMRKT DC    H'0'                                                             
TMPMARKT DC    H'0'                ANOTHER TEMPORARY MARKET                     
         SPACE 2                                                                
* EDIT USER LISTS                                                               
         USING ELEMDS,R2                                                        
EDITML   DS    0H                                                               
         L     R4,AIO2                                                          
         ST    R4,SCANADDR                                                      
         LA    R4,1000(R4)                                                      
         XC    PARAS(16),PARAS                                                  
         MVI   STATCHK,C'Y'                                                     
         ST    R4,PARAS+12                                                      
         MVC   TMPMARKT,TEMPMRKT                                                
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
         B     REGERR                                                           
         SPACE 1                                                                
NEEDMRKT MVC   CONHEAD(L'NEEDMKT),NEEDMKT                                       
         LA    RE,CONHEAD+L'NEEDMKT+1                                           
         B     REGERR                                                           
         SPACE 1                                                                
NEEDSTAL MVC   CONHEAD(L'NEEDSL),NEEDSL                                         
         LA    RE,CONHEAD+L'NEEDSL+1                                            
         B     REGERR                                                           
         SPACE 1                                                                
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
*        CLI   STATCHK,C'Y'        WAS THERE A STATION BEFORE?                  
*        BNE   NEEDSTAL            NO, NEED STATION LIST                        
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
*                                                                               
         CLI   STATCHK,C'Y'        SEE IF STATION LIST NEEDED                   
         BNE   NEEDSTAL            YES, GIVE ERROR                              
*                                                                               
         GOTO1 GETREC                                                           
         MVI   STALSTD,C'Y'                                                     
         SPACE 1                                                                
         MVI   STATCHK,C'Y'                                                     
         L     R5,AIO1                                                          
         LA    R5,SLDELEM                                                       
EDMLM1   CLI   0(R5),X'01'         DESCRIPTION?                                 
         BNE   EDMLM2              NO, COULD BE THE STATIONS                    
         USING SLDELEM,R5                                                       
         SR    RF,RF                                                            
         MVC   TEMPMRKT,SLDMKTA                                                 
         CLI   CTRY,C'C'                                                        
         BE    EDMLM3                                                           
         CLI   HRSSRCE,C'B'                                                     
         BNE   EDMLM1A                                                          
         MVC   TEMPMRKT,SLDMKTB                                                 
EDMLM1A  BAS   RE,GOTTAMKT                                                      
EDMLM2   CLI   0(R5),X'05'                                                      
         BNE   EDMLM3                                                           
         USING SLSELEM,R5                                                       
         MVI   STAMISS,C'N'                                                     
         L     R4,PARAS+12                                                      
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         ST    R4,PARAS+4                                                       
         ST    R4,PARAS+12                                                      
         MVI   0(R4),X'02'         STATION(S)                                   
         LA    R6,SLSTALST                                                      
         ZIC   R1,1(R5)                                                         
         SH    R1,=H'6'                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R4),0(R6)       MOVE ALL THE STATIONS                        
         AH    R1,=H'3'            2 FOR HEADER + LENGTH OF STATIONS            
*                                  1 FOR BCTR                                   
         STC   R1,1(R4)            SAVE IT                                      
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
GOTMRKT  CLI   STATCHK,C'Y'                                                     
         BE    GOTMRKT1                                                         
         CLC   TEMPMRKT,TMPMARKT                                                
         BNE   NEEDSTAL                                                         
         B     EDMLM4                                                           
GOTMRKT1 BAS   RE,GOTTAMKT                                                      
         MVI   STATCHK,0                                                        
         MVC   TMPMARKT,TEMPMRKT                                                
EDMLM4   BAS   RE,NEXTELEM                                                      
         CLI   ELEMSTOP,C','       STILL MARKETS?                               
         BNE   EDMLM5                                                           
         CLI   ELEMDATA,C'+'                                                    
         BE    EDITML2                                                          
         B     MOREMRKT            YES GET MORE                                 
EDMLM5   CLI   ELEMSTOP,X'FF'      FINISHED, LEAVE PROGRAM                      
         BE    EQXIT2                                                           
         CLI   ELEMSTOP,X'00'                                                   
         BE    EQXIT2                                                           
         B     EDITML2                                                          
         EJECT                                                                  
EDMLS    XC    HSTAT,HSTAT         STATION HERE BEFORE                          
         MVI   STATCHK,C'Y'                                                     
         MVI   STAMISS,C'N'                                                     
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
         BE    EQXIT2                                                           
         CLI   ELEMSTOP,X'0'                                                    
         BE    EQXIT2                                                           
         CLI   ELEMSTOP,C'='                                                    
         BE    EDITML2                                                          
         CLI   ELEMDATA,C'+'                                                    
         BE    EDITML2                                                          
         OI    HSTAT,X'1'                                                       
         B     EDSTAT1                                                          
         EJECT                                                                  
EDMLC    XC    CMBCOUNT,CMBCOUNT   INIT COMBO COUNTER                           
         MVI   STAMISS,C'N'                                                     
         MVI   STATCHK,C'Y'                                                     
         CLI   ELEMLEN,0           COMBO NAME AT LEAST 1                        
         BZ    EDITML1                                                          
         OC    MKTNUM,MKTNUM       NEED A MARKET                                
         BNZ   *+14                                                             
         OC    PARAS(4),PARAS      FROM SOMEPLACE                               
         BZ    EDITML1                                                          
         CLI   ELEMSTOP,C'='                                                    
         BE    EDMLC1                                                           
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
         BNZ   EDMLC01                                                          
         MVI   SUBSID,C'Y'                                                      
EDMLC01  XC    KEY,KEY                                                          
         MVI   GOTMATCH,C'N'                                                    
         USING CLKEY,R5                                                         
         MVC   CLKTYPE(2),=X'0D5E'                                              
         MVC   CLKAM,BAGYMD                                                     
         CLI   SUBSID,C'Y'                                                      
         BNE   AGY00002                                                         
         MVI   CLKAM,X'B2'                                                      
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
*--------------------------------------------------------------------           
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
         BE    EDMLC               YES, GO BACK                                 
         CLI   ELEMSTOP,X'FF'                                                   
         BE    EQXIT2                                                           
         CLI   ELEMSTOP,X'00'                                                   
         BE    EQXIT2                                                           
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
         BZ    EDCOMB0                                                          
         BCTR  RF,0                ANOTHER FOR EX INSTRUCTION                   
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
         CLI   ELEMDATA,C'+'                                                    
         BE    TESTEND                                                          
         CLI   ELEMSTOP,C'='                                                    
         BNE   EDMLC2                                                           
TESTEND  CLI   CMBCOUNT,2                                                       
         BL    NEEDSTAL                                                         
         CLI   ELEMSTOP,C'='                                                    
         BE    EDITML2                                                          
         CLI   ELEMDATA,C'+'                                                    
         BE    EDITML2                                                          
         B     EQXIT2                                                           
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
TESTMRKT BAS   RE,NEXTELEM         NEXT ELEMENT                                 
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
         BH    NOTYET3             YES, BUT COULD BE STATION LIST               
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
         CLI   HRSSRCE,C'A'                                                     
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
         MVC   THISLINE(8),HRSUL1H                                              
         MVC   THISLINE+8(7),=CL7' '                                            
         ZIC   RF,1(R2)                                                         
         LTR   RF,RF                                                            
         BNZ   EDMVSTA1                                                         
         CLI   1(R2),1             CAUSE AN ERROR CODE                          
         B     XIT2                                                             
EDMVSTA1 BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   THISLINE+8(0),ELEMBDAT(R2)                                       
         MVI   THISLINE+4,X'C0'                                                 
         MVC   THISLINE+5(1),1(R2)                                              
*                                                                               
         LA    R2,THISLINE                                                      
         GOTO1 VSUBR07,DMCB,('VALDSTAE',(RC))                                   
         B     XIT2                                                             
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
* VALIDATE STATION EXPRESSIONS (WTAE,WTAE/103,WABC-A)                           
*  USING DEMO FILE STATIONS AND MARKETS                                         
         SPACE 1                                                                
VALDSTA  XC    ACTSTAT,ACTSTAT                                                  
         XC    ACTMKT,ACTMKT                                                    
*                                                                               
         CLI   5(R2),0                                                          
         BE    EQXIT2                                                           
*                                                                               
         LA    R4,BLOCK                                                         
         XC    0(64,R4),0(R4)      CLEAR SCANNER TABLE                          
         GOTO1 SCANNER,DMCB,(R2),(2,(R4)),C',=/-'                               
*                                                                               
VALDST2  TM    2(R4),X'80'         TEST VALID NUMERIC                           
         BO    S7BADSTA            YES - INPUT IS NOT VALID                     
         CLI   1(R4),0                                                          
         BNE   VALDSTA3                                                         
*                                                                               
         CLI   0(R4),5             MUST BE STAT+BAND                            
         BNE   *+14                                                             
         MVC   ACTSTAT(5),12(R4)                                                
         B     DSTA10                                                           
*                                                                               
         CLI   0(R4),4             MUST BE STAT+BAND                            
         BNE   VALDSTA3                                                         
         MVC   ACTSTAT(4),12(R4)                                                
         MVC   ACTSTAT+4(1),ACTSTAT+3                                           
         MVI   ACTSTAT+3,C' '                                                   
         B     DSTA10                                                           
*                                                                               
VALDSTA3 CLI   0(R4),3                                                          
         BL    S7BADSTA                                                         
         CLI   0(R4),4                                                          
         BH    S7BADSTA                                                         
         TM    2(R4),X'40'         TEST ALPHA (MSPACK REQUIREMENT)              
         BZ    S7BADSTA                                                         
         MVC   ACTSTAT(4),12(R4)                                                
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
         B     S7BADSTA                                                         
         SPACE 2                                                                
DSTA6    CLI   1(R4),2                                                          
         BH    S7BADSTA                                                         
         MVC   ACTSTAT+4(1),22(R4)                                              
         ZIC   R5,1(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,DSTAAM                                                        
         BE    DSTA10                                                           
         EX    R5,DSTAFM                                                        
         BE    DSTA10                                                           
         EX    R5,DSTACO                                                        
         BE    DSTA10                                                           
         EX    R5,DSTAAA                                                        
         BE    DSTA10                                                           
         EX    R5,DSTAFF                                                        
         BE    DSTA10                                                           
         B     S7BADSTA                                                         
DSTAAM   CLC   22(0,R4),=C'AM'                                                  
DSTAFM   CLC   22(0,R4),=C'FM'                                                  
DSTACO   CLC   22(0,R4),=C'CO'                                                  
DSTAAA   CLC   22(0,R4),=C'AA'                                                  
DSTAFF   CLC   22(0,R4),=C'FF'                                                  
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
DSTA13   BAS   RE,CHKALBKS         GET THE HIGHEST BOOK                         
         BNZ   S7BADSTA                                                         
DSTA15   MVC   DBSELSTA,ACTSTAT                                                 
         CLI   DBSELSTA+4,C'C'     -CO STATION?                                 
         BE    S7BADSTA            DON'T ALLOW                                  
         MVC   DBFILE,=C'TP '                                                   
         GOTO1 DEMAND,DMCB,DBLOCK,0                                             
         CLI   DBERROR,X'10'                                                    
         BE    S7BADSTA                                                         
         SPACE 2                                                                
* GET MARKET NAME                                                               
         SPACE 1                                                                
DSTA20   MVC   ACTMKT,DBACTRMK     MOVE MARKET NUMBER                           
         B     EQXIT2                                                           
*                                                                               
S7BADSTA MVI   DBERROR,X'10'                                                    
         B     NEQXIT2                                                          
*                                                                               
CHKALBKS NTR1                                                                   
         ZIC   R2,NBOOKS                                                        
         BCTR  R2,0                                                             
         LA    R3,BOOKS            FIND HIGHEST BOOK                            
         LA    RF,BOOKS                                                         
CHKBKS05 LA    RF,4(RF)                                                         
         CLC   1(2,R3),1(RF)                                                    
         BH    *+6                                                              
         LR    R3,RF                                                            
         BCT   R2,CHKBKS05                                                      
*                                                                               
CHKBKS10 MVC   DBSELBK,1(R3)                                                    
         MVC   DBBTYPE,3(R3)                                                    
         MVC   DBSELSTA,ACTSTAT                                                 
         MVC   DBFILE,=C'TP '                                                   
         GOTO1 DEMAND,DMCB,DBLOCK,0                                             
         CLI   DBERROR,X'10'                                                    
         BE    NEQXIT2                                                          
*        LA    R3,4(R3)                                                         
*        BCT   R2,CHKBKS10                                                      
         B     EQXIT2                                                           
         SPACE 2                                                                
         EJECT                                                                  
EDITMB   LA    R4,BOOKS            CHECK MARKET FOR ALL BOOKS                   
         CLI   DBSELSRC,C'M'       BBM RADIO DOESN'T HAVE MKT LEVELS            
         BE    EQXIT2                                                           
         SPACE 1                                                                
EDITMB2  CLI   1(R4),0             EOL                                          
         BE    EQXIT2                                                           
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
         BE    NEQXIT2                                                          
         LA    R4,4(R4)            NEXT BOOK                                    
         B     EDITMB2                                                          
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
         MVC   SVTFLTR,HRSMKT                                                   
         SPACE 1                                                                
         MVI   TWANUM,2            SAVE IN TWA 2                                
         MVC   COMAND2,=CL8'DMWRT'                                              
         GOTO1 VSUBR07,DMCB,('WRTWAE',(RC))                                     
         BNE   NEQXIT2                                                          
*                                  GET MARKET LIST SCREEN                       
         GOTO1 CALLOV,DMCB,(X'A3',CONTAGH),0,0                                  
         LA    R2,RMKMK1H                                                       
         MVC   RMKWORK(200),BUFF                                                
         B     EQXIT2                                                           
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
         BE    EQXIT2                                                           
         B     NEQXIT2                                                          
         EJECT                                                                  
*   SCREEN GENERATORS                                                           
GENDPT   SR    RF,RF               COUNT THE ACTIVE DAYPARTS                    
         L     RE,=A(STANDPTS)                                                  
         A     RE,RELO                                                          
         CLI   0(RE),X'FF'         END OF TABLE                                 
         BE    *+16                                                             
         LA    RE,L'STANDENT(RE)                                                
         LA    RF,1(RF)                                                         
         B     *-16                                                             
         STC   RF,BYTE             SAVE NUMBER OF DAYPARTS                      
         L     RE,AIO2             AREA FOR TWABLD LIST                         
         LA    RF,1000                                                          
         XCEF                                                                   
         ZIC   R0,BYTE                                                          
         SR    R5,R5                                                            
         SR    R3,R3                                                            
GENDPT3  CH    R3,=H'3'            3 GROUPS PER LINE                            
         BNE   *+6                                                              
         SR    R3,R3                                                            
         LR    R4,R3               SET INDEX                                    
         SLL   R4,1                *2                                           
         LA    R4,DPDISP(R4)                                                    
         MVC   DPL1+3(1),0(R4)     POS FIELD 1                                  
         MVC   DPL2+3(1),1(R4)     POS FIELD 2                                  
         LR    R4,R5               RESET INDEX FOR NAME                         
         MH    R4,=AL2(L'STANDENT)                                              
         A     R4,=A(STANDPTS)     POINT TO NAME                                
         A     R4,RELO                                                          
         USING STAND,R4                                                         
         MVC   DPL2C,STANDESC      AND MOVE IT IN                               
         DROP  R4                                                               
         LA    R4,DPL1             SCREEN LINE 1                                
         PRINT GEN                                                              
GENDPT4  GOTO1 HELLO,DMCB,(C'P',=C'CORETAB'),AIO2,(R4),=C'ADD=END'              
         PRINT NOGEN                                                            
         CLI   16(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,1(R4)            NEXT FIELD                                   
         AR    R4,RE                                                            
         CLI   0(R4),X'FF'         END OF PAIRS                                 
         BNE   GENDPT4                                                          
         LA    R5,1(R5)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,GENDPT3          DO IT FOR NUMBER OF ENTRIES                  
         L     R4,AIO2                                                          
         LA    R4,2(R4)                                                         
         GOTO1 VTWABLD,DMCB,ATWA,(R4),CONTAGH,2304                              
         B     EQXIT2                                                           
         EJECT                                                                  
*              ROUTINE TO FORMAT UNIVERSES FOR HEADLINES                        
         SPACE 3                                                                
*              INPUT               FIRST DEMO IN DEMOS                          
*              OUTPUTS             ADISAVE MSASAVE TSASAVE                      
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
         EDIT  (2,MKTNUM),(4,DBSELSTA)                                          
*                                                                               
         LA    R2,4                                                             
         LA    R3,DBSELSTA                                                      
SETUNV2  CLI   0(R3),C' '                                                       
         BNE   SETUNV3                                                          
         MVI   0(R3),C'0'                                                       
SETUNV3  LA    R3,1(R3)                                                         
         BCT   R2,SETUNV2                                                       
*                                                                               
         MVC   DBFILE,=C'RDP'                                                   
         MVI   DBSELDAY,X'04'                                                   
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
         SPACE 1                                                                
         MVC   UNIVLIST,UNIVCATS                                                
         MVC   UNIVLIST+2(1),DEMOS+2                                            
         MVC   UNIVLIST+5(1),DEMOS+2                                            
         MVC   UNIVLIST+8(1),DEMOS+2                                            
         GOTO1 DEMAND,DMCB,DBLOCK,UNVHOOK                                       
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
         BNE   NOADIU                                                           
         MVI   ANYADI,C'Y'                                                      
         OC    UNIVS+8(4),UNIVS+8                                               
         BNZ   *+8                                                              
         MVI   ANYUNIV,C'N'                                                     
NOADIU   LA    R2,3(R2)                                                         
         BCT   R0,SETANYS                                                       
SETUXIT  B     XIT2                                                             
         SPACE 1                                                                
UNIVCATS DC    X'01',C'U',X'00'    MSA                                          
         DC    X'02',C'U',X'00'    TSA                                          
         DC    X'03',C'U',X'00'    ADI                                          
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
         B     XIT2                                                             
*                                                                               
REALUNIV NTR1                                                                   
         LA    R0,4                                                             
         LA    R2,UNIVS                                                         
         LA    R3,RUNIVS                                                        
REALUNV1 L     R5,0(R2)                                                         
         A     R5,0(R3)                                                         
         ST    R5,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,REALUNV1                                                      
         B     XIT2                                                             
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
         B     XIT2                                                             
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
         B     XIT2                                                             
         SPACE 1                                                                
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
SETNEWU3 B     EQXIT2                                                           
SETNEWA  LA    R2,DEMOS                                                         
         LA    R3,RUNIVS                                                        
         LA    R4,DUNBLOCK                                                      
         ZIC   R5,NDEMOS                                                        
         MVC   DUNBLOCK,SPACES                                                  
         SPACE 1                                                                
SETNEWU2 GOTO1 DEMOCON,DMCB,(R2),(5,WORK),DBLOCK                                
         MVC   0(5,R4),WORK        W1834                                        
         MVI   5(R4),C'='          W1834=                                       
         EDIT  (4,(R3)),(6,6(R4)),ALIGN=LEFT                                    
         LA    R2,3(R2)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,12(R4)                                                        
         BCT   R5,SETNEWU2                                                      
         B     EQXIT2                                                           
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
GOTTAMXT CLC   HRSBOOK+1(4),=C'BOOK'                                            
         BNE   XIT2                                                             
         BAS   RE,FMULTBK                                                       
         B     XIT2                                                             
         EJECT                                                                  
FMULTBK  NTR1                                                                   
         CLC   HRSBOOK+1(4),=C'BOOK'                                            
         BNE   XIT2                                                             
         MVI   NBOOKS,0                                                         
*        MVC   BADMULP(1),HRSBOOK                                               
         XC    BOOKS,BOOKS                                                      
         LH    RE,TEMPMRKT         NOW SET MARKET IN STATION FIELD              
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DBSELSTA(4),DUB+5(3)                                             
         MVI   DBSELSTA+4,C'A'     BY CONVENTION                                
         MVC   DBSELRMK,TEMPMRKT                                                
*                                                                               
         LA    R1,1                                                             
         MVC   MAXRANK(1),HRSBOOK  SET NUMBER OF BOOKS                          
         NI    MAXRANK,X'0F'                                                    
         CLI   MAXRANK,4                                                        
         BH    FMULTBKX                                                         
         CLI   HRSBOOK+5,C'('                                                   
         BNE   *+10                                                             
         MVC   DBBTYPE,HRSBOOK+6                                                
*                                                                               
         MVI   DBFUNCT,DBGETMB     GET BOOKS FOR MARKET                         
         GOTO1 DEMAND,DMCB,DBLOCK,BKHOOKF                                       
         LA    R1,1                                                             
         CLI   NBOOKS,0            NOT FOUND                                    
         BNE   *+8                                                              
         B     FMULTBKX                                                         
         SPACE 1                                                                
         XC    WORK,WORK                                                        
         CLC   NBOOKS,MAXRANK      NOT ENOUGH BOOKS                             
         BNE   FMULTBKX                                                         
*                                                                               
         ZIC   R0,MAXRANK          DON'T ALLOW PREV. YEAR                       
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
         STC   R1,MAXRANK                                                       
         B     XIT2                                                             
*                                                                               
BKHOOKF  NTR1                                                                   
         L     R4,DBAREC                                                        
         USING SBKEY,R4                                                         
         TM    SBBOOK,X'80'        BYPASS REVERSE SEQ BOOKS                     
         BO    XIT2                                                             
         CLC   SBBTYP,DBBTYPE                                                   
         BNE   XIT2                                                             
         ZIC   R1,NBOOKS                                                        
         LA    R1,1(R1)                                                         
         CLC   NBOOKS,MAXRANK      BOOK TABLE FULL - NEED A SLIDE               
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
         B     XIT2                                                             
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
         LA    R6,NEWCLIST                                                      
*                                                                               
         USING CLSELEM,R5                                                       
         ZIC   R4,CLSLEN                                                        
         AR    R4,R5               LIMIT OF ELEMENT                             
         LA    R3,CLSTALST         FIRST STATION IN CMBLIST                     
CKCMB4   MVC   THISLINE(8),HRSUL1H    MAKE UP A FAKE FIELD HEADER               
         MVC   THISLINE+8(5),0(R3)                                              
         MVI   THISLINE+5,5        LENGTH OF STATION                            
         MVI   THISLINE+4,X'C0'                                                 
         LA    R2,THISLINE                                                      
         GOTO1 VSUBR07,DMCB,('VALDSTAE',(RC))                                   
         BNE   CKCMB5              A STATION IS INVALID                         
         AI    NUMSTAT,1           INCREMENT # OF STATIONS                      
         MVC   0(5,R6),0(R3)       SAVE VALID STATION                           
         LA    R6,5(R6)            WIDTH OF STATION                             
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
         CLI   NUMSTAT,1                                                        
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
         BNE   NEQXIT2                                                          
* PUSH THE REST OF THE LIST TO THE LEFT                                         
CKCMB5A  MVI   CHNGLST,C'Y'        LIST WAS CHANGED                             
         B     CKCMB4A                                                          
*                                                                               
CKCMB6   DS    0H                                                               
* HAVE TO CHECK IF THERE WERE ANY COMBOS                                        
         CLI   NUMCMBS,0                                                        
         BNZ   EQXIT2                                                           
         MVI   RDUPDATE,C'Y'       MAKE SURE IT'S DELETED                       
         GOTO1 GETREC              NO COMBOS, HAVE TO DELETE IT                 
         L     R5,AIO1                                                          
         USING CLKEY,R5                                                         
         OI    CLRCNTL,X'80'                                                    
         GOTO1 PUTREC                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R5)                                                    
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
NEQXIT2  LA    R1,1                                                             
         B     *+6                                                              
         SPACE 1                                                                
EQXIT2   SR    R1,R1                                                            
         LTR   R1,R1                                                            
         SPACE 1                                                                
XIT2     XIT1                                                                   
         SPACE 2                                                                
MAXCMB   DC    C'COMBO LIMITED TO 7 STATIONS '                                  
ERRSTA   DC    C'INVALID STATION FOR BOOK OR MARKET'                            
ERRCBL   DC    C'COMBO LIST STATION(S) INVALID, PRESS PF4 FOR AUTO-DELEX        
               TION'                                                            
NEEDKW   DC    C'         KEYWORD MISSING'                                      
NEEDMKT  DC    C'MARKET MISSING'                                                
NEEDSL   DC    C'STATION LIST REQUIRED'                                         
TEMPNAME DC    CL5' '                                                           
CMBELMER DC    C'COULD NOT FIND COMBO ELEMENT'                                  
GOTMATCH DC    C' '                                                             
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
         LTORG                                                                  
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
* COMBO SCREEN GENERATOR STUFF                                                  
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
         SPACE 2                                                                
*   DAYPART SCREEN GENERATOR STUFF                                              
DPDISP   DC    AL1(1,3)            FIRST                                        
         DC    AL1(25,27)          SECOND                                       
         DC    AL1(52,54)          THIRD                                        
         SPACE 1                                                                
DPL1     DC    AL1(3,7,0,1,1,INTNOR,0)     SELECT FIELD                         
DPL2     DC    AL1(3,7+20,0,3,20,PROT,0)   NAME FIELD                           
DPL2C    DC    CL20' '                                                          
         DC    X'FF'                                                            
*                                                                               
SUBR07X  EQU   *                                                                
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
         DROP  RE                                                               
         BAS   RE,REPHIGH                                                       
         BNZ   CKSBSDER                                                         
         CLC   KEY(27),KEYSAVE                                                  
         BNE   CKSBSDER                                                         
         L     R6,AIO1                                                          
         BAS   RE,REPGETRC                                                      
         MVC   DATADISP,=H'34'                                                  
         MVI   ELCODE,2            JUST NEED SUBSIDS                            
         BAS   RE,GETEL                                                         
         BNE   CKSBSDER            THERE HAS TO BE A 2 ELEMENT                  
         USING RREPSUB,R6                                                       
         ZIC   R0,RREPSCNT         # OF SUBSIDS                                 
         LA    R6,RREPSCOD                                                      
CKSUBS   CLC   AGENCY,0(R6)        IS AGENCY IN SUBSID LIST?                    
         BE    CKSUBXST            YES, LEAVE                                   
         LA    R6,2(R6)                                                         
         BCT   R0,CKSUBS                                                        
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
         BE    EQXIT2                                                           
         B     NEQXIT2             THERE WAS AN ERROR                           
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
         B     EQXIT2              EVERYTHING OK                                
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
         BNZ   NEQXIT2                                                          
         B     EQXIT2                                                           
*                                                                               
REPGETRC MVC   COMMAND,=C'GETREC'                                               
         NTR1                                                                   
         LA    R2,KEY+28           GET DISK ADDRESS                             
         IC    R3,TERM                                                          
         IC    R4,DMINBTS                                                       
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPFIL  ',               X        
               (R2),AIO1,((R3),DMWORK),0                                        
         B     EQXIT2                                                           
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER PARSER ELEMENTS                                   
ELEMDS   DSECT                                                                  
ELEMSTOP DS    C                                                                
ELEMLEN  DS    X                                                                
ELEMADDR DS    XL2                                                              
ELEMDISP DS    X                                                                
ELEMBDAT EQU   *-ELEMSTOP                                                       
ELEMDATA DS    0C                                                               
         SPACE 3                                                                
*              DSECT TO COVER ENTRY IN STANDARD DAYPART TABLE                   
STAND    DSECT                                                                  
STANDENT DS    0CL36                                                            
STANDDTS DS    XL15                                                             
STANDPRG DS    XL1                                                              
STANDESC DS    CL20                                                             
         SPACE 3                                                                
*              DSECT TO COVER ENTRY IN STANDARD DAYPART TABLE                   
         SPACE 1                                                                
TDD      DSECT                                                                  
TDENT    DS    0CL42                                                            
TDSQH    DS    CL1                                                              
TDSHR    DS    XL2                 START HOUR                                   
TDEHR    DS    XL2                 END HOUR                                     
TDCNT    DS    XL1                 STATION/BOOK COUNT                           
TDVLMF   DS    CL(4*3)             M-F DEMO VALUES                              
TDVLSAT  DS    CL(4*3)             SAT DEMO VALUES                              
TDVLSUN  DS    CL(4*3)             SUN DEMO VALUES                              
TDEND    DS    0C                                                               
         SPACE 2                                                                
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
*              BUFFER ARRANGEMENT                                               
         SPACE 3                                                                
*                                  THIS IS CONTENTS OF BUFF                     
         SPACE 1                                                                
*                                  UP TO 100 10 BYTE STATION ENTRIES            
         SPACE 1                                                                
*              DSECT TO COVER STATION ENTRY                                     
         SPACE 1                                                                
STATENTD DSECT                                                                  
STATENT  DS    0CL10                                                            
STATCALL DS    CL5                 CALL LETTERS                                 
STATFORM DS    CL4                 FORMAT                                       
         DS    CL1                                                              
         SPACE 1                                                                
*                                  THERE ARE 100 STATION ENTRIES                
*                                  EACH IS 20 BYTES                             
         SPACE 1                                                                
*                                  BUFFERS ARE FOR 1-5 BOOKS                    
*                                  PLUS A 6TH FOR AVERAGE OF BOOKS              
         SPACE 2                                                                
*              DSECT TO COVER RECORDS IN BUFF                                   
         SPACE 1                                                                
BUFFD    DSECT                                                                  
BUFFREC  DS    0CL20                                                            
BUFFSTAT DS    XL1                 STATION NUMBER                               
         DS    CL1                 SPARE                                        
*                                  FOR EACH BOOK THERE WILL BE                  
BUFFDEM  DS    XL2                 DEMO                                         
BUFFRANK DS    XL1                 RANK NUMBER (X'80'= EQUAL)                   
         DS    XL15                                                             
         SPACE 3                                                                
         EJECT                                                                  
       ++INCLUDE REGENREPA                                                      
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DEALPHAMKD                                                     
       ++INCLUDE SPGENCUSDP                                                     
         EJECT                                                                  
       ++INCLUDE SPGENSTLST                                                     
       ++INCLUDE SPGENCOMBO                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DEDBEXTRAD                                                     
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE DDTWABLDD                                                      
MARKETD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPRESWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
*              DSECT TO COVER SCREEN                                            
         SPACE 3                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE SPRESD9D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPRESA3D                                                       
         EJECT                                                                  
*              STORAGE FOR RANKER                                               
         SPACE 3                                                                
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
MAXRANK  DS    XL1                 MAX STATIONS TO RANK                         
WIDEOPT  DS    XL1                 WIDTH 1-3 = 80/110/132                       
TRACEOPT DS    CL1                 Y=TRACE DEMAND HOOKS                         
LEFTOPT  DS    CL1                 Y=LEFT ALIGN                                 
SPACOPT  DS    XL1                 SPACING                                      
CALCOPT  DS    CL1                 Y=USE IMPRESSION METHOD                      
SEPAOPT  DS    CL1                 Y=USE MULTIPLE DEMOS                         
ADDROPT  DS    CL1                 N=DONT USE ADDRESS IN REPORT                 
AUTOOPT  DS    CL1                 N=DONT USE AUTO DELETION ON CMBLIST          
RUNIFLG  DS    C                                                                
NOA      DS    CL1                 NOT ON AIR FULL TIME IND.                    
SPANN    DS    CL1                 SPECIAL ANNOUNCMENT IND.                     
RELO     DS    A                                                                
RELO2    DS    A                                                                
MYBASE   DS    A                                                                
MYBASE2  DS    A                                                                
VSUBR07  DS    A                                                                
ULPNTR   DS    F                                                                
         SPACE 1                                                                
STATSV   DS    CL70                                                             
COMBSV   DS    5F                                                               
COMBNUM  DS    C                                                                
COMBSW   DS    C                                                                
TWANUM   DS    C                                                                
COMAND2  DS    CL8                                                              
         SPACE 1                                                                
LASTDEM  DS    H                   USED FOR BOOK/BOOK COMP                      
SVNDEM   DS    C                                                                
DIFFSW   DS    CL1                 FORMAT SWITCH                                
SIGN     DS    CL1                 SET TO = OR - FOR DIFF                       
MSASAVE  DS    CL10                ROOM FOR MSA=NNNNNN                          
TSASAVE  DS    CL10                ROOM FOR TSA=NNNNNN                          
ADISAVE  DS    CL10                ROOM FOR ADI=NNNNNN                          
ANYMSA   DS    CL1                 SET TO Y IF MSA SPECIFIED                    
ANYTSA   DS    CL1                 SET TO Y IF TSA SPECIFIED                    
ANYADI   DS    CL1                 SET TO Y IF ADI SPECIFIED                    
         ORG   MSASAVE                                                          
DUNBLOCK DS    CL48                OR UP TO 4 W1834=123456                      
ANYUNIV  DS    CL1                 Y=UNIVERSES DONE                             
THISTYPE DS    CL1                 C(USTOM) OF S(TANDARD)                       
DISP     DS    F                   DISPLACEMENT INTO PRINT LINE                 
MKTNUM   DS    H                   MARKET NUMBER SELECTED                       
ASTATS   DS    A                   ADDRESS OF STATION LIST                      
WSTATS   DS    F                   WIDTH OF STATION LIST                        
ABOOKBF  DS    A                   ADDRESS OF FIRST DAYPART BEST                
ATHISTAT DS    A                   ADDRESS OF STATION BEING PROCESSED           
STATNUM  DS    XL1                 NUMBER OF STATION BEING PROCESSED            
THISDEM  DS    F                   BUFFER FOR STATION IN PROGRESS               
THISWT   DS    F                   BUFFER FOR STATION IN PROGRESS               
THISLINE DS    XL12                                                             
UNIVLIST DS    XL10                DEMOUT LIST FOR UNIVS                        
UNIVS    DS    4F                  UNIVS FOR MSA TSA ADI                        
RUNIVS   DS    4F                  UNIVS FOR IMPRESSIONS                        
LASTVAL  DS    XL2                 VALUE DURING RANKING                         
DEMSTYLE DS    C                   D=DEMO  C=CATEGORY                           
         SPACE 1                                                                
DPENTRY  DS    0CL36               DAYPART ENTRY                                
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
STASTYLE DS    CL1                 C=CUSTOM L=LIST M=MARKET                     
SVMKTH5  DS    C                                                                
SVMKTNUM DS    CL2                                                              
CUMESW   DS    CL1                                                              
MYWORK   DS    A                                                                
UNIVBK   DS    XL2                 BOOK FOR UNIVERSE                            
MEDTYPE  DS    C                                                                
SRCTYPE  DS    C                                                                
BOOKTYPE DS    C                                                                
CITYCODE DS    CL3                                                              
MISCOMMA DS    C                                                                
DBRADIC  DS    CL128                                                            
PREVSTA  DS    CL5                                                              
OUTAREA  DS    A                                                                
STAMISS  DS    C                                                                
MKTSTAT  DS    CL5                                                              
TEMP     DS    CL7                                                              
STATCHK  DS    C                                                                
PFKEY    DS    C                                                                
SUBSID   DS    C                                                                
STALSTD  DS    C                   USED STATION LISTS?                          
CMBENTRY DS    F                                                                
         SPACE 1                                                                
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'065SPRES09   12/09/20'                                      
         END                                                                    
