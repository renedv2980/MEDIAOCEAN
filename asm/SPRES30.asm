*          DATA SET SPRES30    AT LEVEL 019 AS OF 12/09/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE T20F30A                                                                  
*                                                                               
*---------------------------------------------------------------------          
* 6/01/92 - ADDED RANDOM DUPLICATION FORMULA FOR COMBO CUMES.                   
*           TSL CALCULATION USES CALCULATED RDF CUME FOR COMBO.  (MTA)          
*---------------------------------------------------------------------          
* 5/19/92 - FIX UP MARKET CHECKING IN VMULTBK.  NEEDED THE CODE                 
*             "MVC  DBSELRMK,MKTNUM"  BEFORE GOING TO DEMAND.    (GH)           
*---------------------------------------------------------------------          
* 4/8/92: - IF COUNTRY IS CANADA, DEFAULT CALCOPT=N              (GH)           
*---------------------------------------------------------------------          
         TITLE 'T20F30 - RADIO MULTI DEMO REPORT'                               
T20F30   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 150,T20F30**,RA,RR=R2                                            
         LR    RE,RC               SAVE A(WORKING STORAGE)                      
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
         L     RE,=V(SUBR08)                                                    
         A     RE,RELO                                                          
         ST    RE,VSUBR08                                                       
*                                                                               
         L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         SR    R0,R0                                                            
         IC    R0,TIOBAID          PICK UP PFKEY VALUE                          
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
VREC     MVI   DAYPOPT,C'Y'        SET HERE - NOT VALID SOMETIMES               
         MVI   SHAREOPT,C'N'                                                    
         MVI   DRIVESTA,C'N'                                                    
         MVI   STAMISS,C'Y'                                                     
         MVI   STALSTD,C'N'                                                     
*                                                                               
         LA    R2,HRSMAXH                                                       
         MVI   MAXRANK,15                                                       
         CLI   5(R2),0                                                          
         BE    VREC01                                                           
         GOTO1 VALINUM                                                          
         MVC   MAXRANK,ACTUAL                                                   
         CLI   MAXRANK,30                                                       
         BNH   VREC01                                                           
*********MVC   CONHEAD(L'TOOMANY),TOOMANY   DEIS REMOVED 2/24/2015              
*********B     MYEND                                                            
         MVI   ERROR,TOOMANY                                                    
         B     ERREND                                                           
*                                                                               
VREC01   LA    R2,HRSSRCEH         VALIDATE SOURCE                              
         XC    NOA(2),NOA          ALSO CLEAR FIELD 'SPANN'                     
         GOTO1 VVALSRC                                                          
         CLI   DBSELMED,C'T'       IF TV                                        
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'       (ADJUST FOR RADIO)                           
         MVC   MEDTYPE,DBSELMED    SAVE MEDIA TYPE                              
         MVC   SRCTYPE,DBSELSRC    SAVE SOURCE TYPE                             
         XC    BOOKTYPE,BOOKTYPE   INITIALIZE BOOK TYPE (REG)                   
* -------------------------->            LATEST BOOK = 1BOOK                    
         CLC   HRSBOOK(4),=C'LATEST'                                            
         BNE   NLATEST                                                          
         MVC   HRSBOOK(5),=C'1BOOK'                                             
         MVC   HRSBOOK+5(3),HRSBOOK+6                                           
         MVI   HRSBOOK+8,C' '                                                   
*                                                                               
* SEARCH FOR C'(' IN THE BOOK FIELD                                             
*                                                                               
NLATEST  LA    R2,HRSBOOK                                                       
         ZIC   R1,HRSBOOKH+5                                                    
         LTR   R1,R1                                                            
         BZ    VREC01A                                                          
         BCTR  R1,0                                                             
         AR    R1,R2               GET ADDRESS OF LAST CHAR IN BOOK             
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
         SPACE 1                                                                
         MVC   SVMKTH5,HRSMKTH+5                                                
         XC    MKTNUM,MKTNUM                                                    
*                                                                               
         USING ELEMDS,R2                                                        
* AFTPARSE:  MARKET, STATION AND USER LISTS CONCATER                            
         GOTO1 =A(AFTPARSE),DMCB,(R9),(RC),RR=RELO                              
         BZ    MKTXST             NO ERROR, MARKET EXISTS                       
*                                                                               
OUTMRKT  L     R2,OUTAREA                                                       
         CLI   0(R2),C'='                                                       
         BE    OUTMRKT1                                                         
         CLI   0(R2),C','                                                       
         BE    OUTMRKT1                                                         
         MVC   2(3,R2),=X'02D300'    FIRST TIME, MARKET                         
OUTMRKT1 CLC   2(2,R2),=X'02D3'                                                 
         BNH   OUTMRKT2                                                         
         MVC   CONHEAD(L'MKTERR2),MKTERR2                                       
         B     CPERR                                                            
OUTMRKT2 MVC   CONHEAD(L'MKTERR),MKTERR                                         
         B     CPERR                                                            
BMKTXST  CLI   HRSUL1H+5,0         CHECK IF USER LISTS                          
         BNE   MKTXST                AND STATION/DAYPART FIELD EMPTY            
         CLI   HRSUL2H+5,0                                                      
         BNE   MKTXST                                                           
         CLI   HRSUL3H+5,0                                                      
         BNE   MKTXST                                                           
         CLI   HRSSTATH+5,0                                                     
         BNE   MKTXST                                                           
DAYPERR  L     R2,OUTAREA            THEY ARE, ERROR!                           
         MVC   2(3,R2),=X'032800'    FIRST TIME, STATION/DAYPT FIELD            
         MVC   CONHEAD(L'STAERR),STAERR                                         
         B     CPERR                                                            
MKTXST   L     R2,OUTAREA                                                       
         GOTO1 VSUBR07,DMCB,('TESTMKE',(RC))                                    
         BNZ   OUTMRKT1            RETURNS NON-ZERO IF ERROR                    
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
         CLI   MAXBOOK,2           NOT ENOUGH BOOKS FOR MARKET                  
         BNE   *+14                                                             
         MVC   CONHEAD(L'BADMUL),BADMUL                                         
         B     CPERR                                                            
         GOTO1 VRADBOOK                                                         
         SPACE 1                                                                
VREC10   LA    R2,HRSMKTH                                                       
         CLI   5(R2),0             ANY MARKET NETERED                           
         BNE   VREC11              YES VALIDATE IT FROM HERE                    
         L     R2,OUTAREA          NO - CHECK USER LIST                         
*                                                                               
VREC11   TM    HRSBOOK,X'F0'                                                    
         BNO   VREC11A                                                          
         BAS   RE,VMULTBK                                                       
VREC11A  XC    ACTSTAT,ACTSTAT                                                  
         MVI   NMARKET,0                                                        
         BAS   RE,EDITMKT          EDIT MARKET                                  
*                                                                               
         OC    MKTNUM,MKTNUM                                                    
         BZ    VRC11                                                            
         GOTO1 VSUBR07,DMCB,('EDITMBE',(RC))                                    
         BNE   BADBOOK                                                          
*                                                                               
VRC11    XC    DEMOS,DEMOS                                                      
         XC    MISRDEMO,MISRDEMO                                                
         LA    R2,HRSRDEMH         VALIDATE DEMOGRAPHIC                         
         CLI   5(R2),0             EMPTY RANK DEMO?                             
         BNE   NOTEMPTR                                                         
         MVC   8(4,R2),=C'L12+'                                                 
         CLI   DBSELSRC,C'M'                                                    
         BNE   *+10                                                             
         MVC   8(4,R2),=C'L18+'    CANADA DOESN'T HAVE L12+                     
         MVI   5(R2),4                                                          
         MVI   MISRDEMO,1                                                       
NOTEMPTR MVI   MAX,1                                                            
         GOTO1 VVALDEM                                                          
         MVC   RDEMO,DEMOS         SET THE RANK DEMO                            
         OC    MISRDEMO,MISRDEMO                                                
         BZ    GETIDEMO                                                         
         XC    8(9,R2),8(R2)                                                    
         MVI   5(R2),0                                                          
*                                                                               
GETIDEMO XC    DEMOS,DEMOS                                                      
         LA    R2,HRSIDEMH         VALIDATE DEMOGRAPHIC                         
         MVI   MAX,1                                                            
         GOTO1 VVALDEM                                                          
         MVC   IDEMO,DEMOS         SET THE INDEX DEMO                           
*                                                                               
         LA    R2,HRSDEMOH         VALIDATE DEMOGRAPHIC                         
         GOTO1 ANY                                                              
         MVI   MAX,MAXDEMOS                                                     
         MVI   NFLDS,1                                                          
*                                                                               
         CLC   8(4,R2),=C'MENU'   DEFAULT IS MENU FOR MAXDEMOS                  
         BE    VREC11B            IT IS A MENU, DO MENU STUFF                   
*                                                                               
* CALCULATE POSSIBLE SHORT DEMOS                                                
*                                                                               
*                                                                               
         MVI   NDEMOS,15           DON'T WRITE OVER SAVE STORAGE                
         GOTO1 VVALDEM                                                          
         MVC   SVNDEM,NDEMOS       SAVE THE NUMBER OF AGE/SEX CATS              
         CLI   NDEMOS,MAXDEMOS                                                  
         BNH   VREC12                                                           
         MVC   CONHEAD(L'TOOMNYDM),TOOMNYDM                                     
         B     MYEND                                                            
AGEGROUP DC    AL1(75,90,96,212,213,105,108,110,83,25,40,46,202,203,55,X        
               58,60,33)                                                        
AGEGRP1L EQU   *-AGEGROUP                                                       
AGGRP1C  DC    AL1(75,90,96,212,105,108,110,25,40,46,202,55,58,60)              
AGGRP1CL EQU   *-AGGRP1C                                                        
AGEGRP2  DC    AL1(90,96,101,106,110,95,40,46,51,56,60,45,125)                  
AGEGRP2L EQU   *-AGEGRP2                                                        
AGEGRP3  DC    AL1(133,125,140,146,222,224,158,160)                             
AGEGRP3L EQU   *-AGEGRP3                                                        
VREC11B  DS    0H                                                               
         SPACE 2                                                                
         CLI   DBSELSRC,C'M'                                                    
         BNE   US_AGE                                                           
         LA    R0,AGGRP1CL                                                      
         LA    RE,AGGRP1C          USE CANADA'S MENU1                           
         B     AGEGRPS                                                          
US_AGE   LA    R0,AGEGRP1L                                                      
         LA    RE,AGEGROUP         USE AGEGROUP 1                               
AGEGRPS  STC   R0,NDEMOS                                                        
         MVC   SVNDEM,NDEMOS                                                    
         XC    DEMOS,DEMOS                                                      
         LA    RF,DEMOS                                                         
         CLI   8+4(R2),C'2'        MENU2?                                       
         BNE   VREC11C             NO, DON'T CHANGE SETTINGS                    
         LA    RE,AGEGRP2          USE AGEGRP2                                  
         LA    R0,AGEGRP2L         R0 HOLDS LENGTH                              
         STC   R0,NDEMOS                                                        
         B     VREC11L                                                          
VREC11C  CLI   8+4(R2),C'3'                                                     
         BNE   VREC11L                                                          
         LA    RE,AGEGRP3          USE AGEGRP3                                  
         LA    R0,AGEGRP3L         R0 HOLDS LENGTH                              
         STC   R0,NDEMOS                                                        
VREC11L  MVC   2(1,RF),0(RE)       MOVE A DEMO TO EVERY 3RD POSITION            
         LA    RF,3(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,VREC11L                                                       
         MVI   0(RF),X'FF'         END OF DEMOS                                 
         SPACE 1                                                                
VREC12   LA    R2,HRSOPTH          OPTIONS                                      
         GOTO1 VSUBR07,DMCB,('EDITOPTE',(RC))                                   
         BNE   MYEND                                                            
TESTUNIV CLI   UNIVOPT,C'Y'                                                     
         BNE   NOCHNGE                                                          
         CLI   HRSIDEMH+5,0                                                     
         BNE   NOCHNGE                                                          
         XC    IDEMO,IDEMO                                                      
         MVI   IDEMO+2,133                                                      
NOCHNGE  OC    IDEMO,IDEMO         INSERT IDEMO INTO LIST                       
         BZ    VREC12A                                                          
         MVC   BUFF(L'DEMOS),DEMOS                                              
         MVC   DEMOS(3),IDEMO                                                   
         MVC   DEMOS+3(L'DEMOS-3),BUFF                                          
         ZIC   RE,NDEMOS                                                        
         LA    RE,1(RE)                                                         
         STC   RE,NDEMOS                                                        
         CLI   NDEMOS,MAXDEMOS+1   CUT OFF AT 20 DEMOS                          
         BL    *+8                                                              
         MVI   NDEMOS,MAXDEMOS+1                                                
         MVI   SHAREOPT,C'Y'                                                    
*                                                                               
VREC12A  MVI   MAX,MAXDEMOS+1                                                   
         LA    R2,HRSCATSH         DEMO CATEGORY                                
         GOTO1 ANY                                                              
         GOTO1 VVALCATS                                                         
         OC    RDEMO,RDEMO         FORCE IN CATAGORY FOR RANK                   
         BZ    *+10                                                             
         MVC   RDEMO(2),DEMOS                                                   
         CLI   NCATS,1              ALLOW ONLY ONE CAT                          
         BNH   VREC14                                                           
         MVC   CONHEAD(L'TOOMNYCT),TOOMNYCT                                     
         B     MYEND                                                            
         SPACE 1                                                                
VREC14   LA    R2,HRSUL1H                                                       
         MVC   STASAVE(5),ACTSTAT                                               
         L     R2,OUTAREA                                                       
         GOTO1 VSUBR07,DMCB,('EDITMLE',(RC))                                    
         BNE   CPERR                                                            
         XC    DISP,DISP                                                        
         CLI   STAMISS,C'Y'                                                     
         BE    DAYPERR                                                          
         GOTO1 VSUBR07,DMCB,('EDITMBE',(RC))                                    
         BNE   BADBOOK                                                          
         TM    HRSBOOK,X'F0'                                                    
         BNO   VREC14P                                                          
*----------------------------------------------------------------               
         MVC   DBAREC,AIO1                                                      
         MVI   STASTYLE,C'M'                                                    
         MVI   DBFUNCT,DBGETMK                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'R'                                                    
         MVC   DBSELRMK,MKTNUM                                                  
         BAS   RE,VMULTBK                                                       
*----------------------------------------------------------------               
*                                                                               
VREC14P  ZIC   RE,NBOOKS                                                        
         ZIC   RF,NMARKET                                                       
         MR    RE,RE                                                            
         CLI   SEPAOPT,C'D'                                                     
         BNE   VREC14P1                                                         
         ZIC   RE,NDEMOS                                                        
         MR    RE,RE                                                            
VREC14P1 C     RF,=F'6'                                                         
         BL    REQ_OK                                                           
         TM    WHEN,X'40'                                                       
         BZ    VREC14A                                                          
         CLI   SEPAOPT,C'D'                                                     
         BNE   VREC14P2                                                         
         MVC   CONHEAD(L'SOONITSP),SOONITSP                                     
         MVC   CONHEAD+L'SOONITSP(L'SOONITN),SOONITN                            
         B     MYEND                                                            
VREC14P2 MVC   CONHEAD(L'SOONIT),SOONIT                                         
         MVC   CONHEAD+L'SOONIT(L'SOONITN),SOONITN                              
         B     MYEND                                                            
*                                                                               
VREC14A  C     RF,=F'13'                                                        
         BL    REQ_OK                                                           
         TM    WHEN,X'20'                                                       
         BZ    VREC14B                                                          
         CLI   SEPAOPT,C'D'                                                     
         BNE   VREC14A1                                                         
         MVC   CONHEAD(L'SOONITSP),SOONITSP                                     
         MVC   CONHEAD+L'SOONITSP(L'SOONITS),SOONITS                            
         B     MYEND                                                            
VREC14A1 MVC   CONHEAD(L'SOONIT),SOONIT                                         
         MVC   CONHEAD+L'SOONIT(L'SOONITS),SOONITS                              
         B     MYEND                                                            
*                                                                               
VREC14B  C     RF,=F'21'                                                        
         BL    REQ_OK                                                           
         CLI   SEPAOPT,C'D'                                                     
         BNE   VREC14B1                                                         
         MVC   CONHEAD(L'SOONITSP),SOONITSP                                     
         MVC   CONHEAD+L'SOONITSP(L'SOONITO),SOONITO                            
         B     MYEND                                                            
VREC14B1 MVC   CONHEAD(L'SOONIT),SOONIT                                         
         MVC   CONHEAD+L'SOONIT(L'SOONITO),SOONITO                              
         B     MYEND                                                            
*                                                                               
REQ_OK   MVC   ACTSTAT(5),STASAVE  STATION REQUIRED BY NOW                      
         OC    ACTSTAT,ACTSTAT                                                  
         BZ    VREC15                                                           
*                                                                               
NODRIVES MVC   ACTSTAT(5),STASAVE                                               
         OC    MKTNUM,MKTNUM       NEED A MARKET BY NOW                         
         BNZ   VREC16                                                           
         MVC   CONHEAD(L'LISTERR),LISTERR                                       
         B     MYEND                                                            
         SPACE 1                                                                
VREC15   LA    R2,HRSSTATH                                                      
         CLI   5(R2),0             EMPTY STATION FIELD?                         
         BZ    VREC16                                                           
         GOTO1 ANY                                                              
         MVC   ONEDPT,=X'7C01'     REALLY NEEDS DAYPART EDIT                    
         GOTO1 VSUBR07,DMCB,('EDITDPTE',(RC))                                   
         BNE   MYEND                                                            
         MVI   DAYPOPT,C'N'        CANNOT HAVE DAYPART REPORT                   
         SPACE 1                                                                
*                                                                               
VREC16   L     R2,AIO2                                                          
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
         MVC   RESTITLE,=CL40'    DEMO COMPOSITION REPORT'                      
         CLI   5(R2),0                                                          
         BE    VREC20                                                           
         GOTO1 ANY                                                              
         MVC   RESTITLE,WORK                                                    
         SPACE 1                                                                
VREC20   CLI   ONEDPT,0                                                         
         BNZ   VREC20A                                                          
         CLI   HRSRDEMH+5,0                                                     
         BZ    VREC20A                                                          
         LA    R2,HRSRDEMH                                                      
         MVC   CONHEAD(L'RANKERR),RANKERR                                       
         B     MYEND                                                            
         SPACE 1                                                                
VREC20A  GOTO1 CENTER,DMCB,RESTITLE,40                                          
         SPACE 1                                                                
VRECX    B     XIT                                                              
SVMKTH5  DC    C' '                                                             
SVMKTNUM DC    CL2' '                                                           
         EJECT                                                                  
VMULTBK  NTR1                                                                   
         CLC   HRSBOOK+1(2),=C'BOOK'                                            
         BNE   XIT                                                              
         MVI   NBOOKS,0                                                         
         XC    BOOKS,BOOKS                                                      
         LH    RE,MKTNUM           NOW SET MARKET IN STATION FIELD              
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DBSELSTA(4),DUB+5(3)                                             
         MVI   DBSELSTA+4,C'A'     BY CONVENTION                                
         MVC   DBSELRMK,MKTNUM     NEED THIS TO VERIFY CURRENT MKT              
*                                                                               
         LA    R1,1                                                             
         MVC   MAXBOOK(1),HRSBOOK  SET NUMBER OF BOOKS                          
         NI    MAXBOOK,X'0F'                                                    
         CLI   MAXBOOK,4                                                        
         BH    VMULTBKX                                                         
         CLI   HRSBOOK+5,C'('                                                   
         BNE   *+10                                                             
         MVC   DBBTYPE,HRSBOOK+6                                                
*                                                                               
         MVI   DBFUNCT,DBGETMB     GET BOOKS FOR MARKET                         
         GOTO1 DEMAND,DMCB,DBLOCK,BKHOOK                                        
         CLI   NBOOKS,0            NOT FOUND                                    
         B     *+8                                                              
         B     VMULTBKX                                                         
         XC    WORK,WORK                                                        
         CLC   NBOOKS,MAXBOOK      NOT ENOUGH BOOKS                             
         BNE   VMULTBKX                                                         
*                                                                               
         ZIC   R0,MAXBOOK          DON'T ALLOW PREV YEAR                        
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
         STC   R1,MAXBOOK                                                       
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
         CLC   NBOOKS,MAXBOOK      BOOK TABLE FULL - NEED A SLIDE               
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
         OC    MKTNUM,MKTNUM       DO WE HAVE MARKET #?                         
         BNZ   HAVEMKTN            GOT IT ALREADY                               
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
*                                                                               
         ZIC   R3,5(R2)                                                         
         LA    R1,8(R2)                                                         
STAT1    CLI   0(R1),C','                                                       
         BNE   *+8                                                              
         MVI   0(R1),C'/'                                                       
         AHI   R1,1                                                             
         BCT   R3,STAT1                                                         
*                                                                               
         GOTO1 SCANNER,DMCB,(L'SCLINE,(R2)),(X'80',AIO2),C',=/-'                
         MVC   DBSELMK,MKTNUM                                                   
STAT2    GOTO1 VSUBR07,DMCB,('VALDST2E',(RC))                                   
         BNE   STAT3                                                            
*  SAVE A LIST OF ACTSTAT                                                       
         MVC   0(5,R5),ACTSTAT                                                  
         LA    R5,5(R5)                                                         
         LA    R4,L'SCLINE+22(R4)                                               
         CLI   0(R4),0             EOL                                          
         BE    XIT                                                              
         CLI   0(R4),X'40'         EOL                                          
         BE    XIT                                                              
         B     STAT2                                                            
*                                                                               
STAT3    XC    ACTSTAT,ACTSTAT                                                  
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              CONTROL REPORT PRINTING                                          
         SPACE 3                                                                
TMPDEMOS DC    XL60'00'                                                         
PRPNDEMS DC    X'00'                                                            
PRPDEMOS DC    XL60'00'                                                         
*                                                                               
PREP     L     R1,=A(HEDSPECS)     INTIALIZATION, (HEADLINE SPECS)              
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK         HEADLINE HOOK PROCEDURE                      
         CLI   NCATS,1                                                          
         BNH   P_SEP0                                                           
         MVI   SEPAOPT,C'N'                                                     
         B     PSEPX                                                            
P_SEP0   CLI   SEPAOPT,C'D'                                                     
         BNE   PSEPX                                                            
         MVC   PRPDEMOS,DEMOS                                                   
         MVC   PRPNDEMS,NDEMOS                                                  
         CLI   UNIVOPT,C'Y'        IS UNIV=Y?????                               
         BNE   P_SEP1              NAH, CONTINUE WITH PRINTING                  
         MVC   PRPDEMOS,PRPDEMOS+3   YES, SKIP THE L12+ IN FRONT                
         ZIC   R5,PRPNDEMS         DECREMENT # OF DEMOS                         
         BCTR  R5,0                                                             
         STC   R5,PRPNDEMS                                                      
P_SEP1   MVC   RDEMO,PRPDEMOS      RANK ON SEPARATE DEMO                        
         CLI   UNIVOPT,C'Y'                                                     
         BE    P_SEP2                                                           
         MVI   NDEMOS,1                                                         
         XC    DEMOS,DEMOS                                                      
         MVC   DEMOS(3),PRPDEMOS                                                
         MVI   DEMOS+3,X'FF'                                                    
         B     P_SEP3                                                           
P_SEP2   MVI   NDEMOS,2            IF UNIV=Y, THEN ONE MORE FOR INDEX           
         XC    DEMOS,DEMOS                                                      
         MVC   IDEMO(2),PRPDEMOS                                                
P_SEP2A  MVC   DEMOS(3),IDEMO      INDEX DEMO                                   
         MVC   DEMOS+3(3),PRPDEMOS                                              
         MVI   DEMOS+6,X'FF'                                                    
P_SEP3   ZIC   R5,PRPNDEMS                                                      
         BCTR  R5,0                                                             
         STC   R5,PRPNDEMS                                                      
         MH    R5,=H'3'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   PRPDEMOS(0),PRPDEMOS+3                                           
*                                                                               
PSEPX    MVC   TMPDEMOS,DEMOS                                                   
*                                                                               
         CLI   CALCOPT,C'Y'        DO IMPRESSIONS?                              
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
         LA    RE,IMPADDR1                                                      
IMPAGN1  CLI   1(R2),C'F'                                                       
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
         CLI   DBSELSRC,C'M'       NO CONVERSION FOR CANADA                     
         BE    IMPNXT1             THEY DON'T HAVE MARKET RECORDS               
         MVI   1(R2),C'I'                                                       
IMPNXT1  LA    R2,3(R2)            NEXT DEMO                                    
         BCT   R0,IMPAGN1                                                       
         BR    RE                                                               
*                                                                               
* MAKE SURE THE RANK DEMO IS CHANGED TOO.                                       
*                                                                               
IMPADDR1 LA    R2,RDEMO                                                         
         LA    R0,1                                                             
         LA    RE,IMPADDR2                                                      
         B     IMPAGN1                                                          
*                                                                               
* MAKE SURE THE INDEX DEMO IS CHANGED TOO.                                      
*                                                                               
IMPADDR2 LA    R2,IDEMO                                                         
         LA    R0,1                                                             
         LA    RE,NOTIMPS1                                                      
         B     IMPAGN1                                                          
*                                                                               
NOTIMPS1 MVI   COMBSW,C'N'                                                      
*        XC    DPENTRY,DPENTRY                                                  
         L     RE,AIO2             SET THE USER LIST POINTER                    
         LA    RE,1000(RE)                                                      
         ST    RE,ULPNTR                                                        
         MVI   DRIVESTA,C'Y'                                                    
         CLI   ONEDPT,0                                                         
         BNE   PREP4                                                            
         MVI   DRIVESTA,C'N'                                                    
*        B     PREP4                                                            
         SPACE 1                                                                
PREP1    XC    UNIVBK,UNIVBK                                                    
         XC    UNIVS,UNIVS         FOR UNIV INDEX REPORT                        
         MVI   COMBNUM,0           SET FOR THIS MARKETS COMBOS                  
         XC    COMBSV,COMBSV                                                    
         XC    COMBCNTR,COMBCNTR                                                
         LA    RE,BUFF             INITIALIZE MAIN BUFFER                       
         L     RF,=F'2000'         CLEAR RF BYTES FROM A(RE)                    
         XCEF                                                                   
         L     RE,MYWORK                                                        
         L     RF,=F'800'                                                       
         XCEF                                                                   
         CLI   HRSSTATH+5,0                                                     
         BZ    PREP4                                                            
PREP1NC  XC    DISP,DISP           COMPUTE PRINT DISPLACEMENTS ETC              
         L     R5,MYWORK                                                        
         OC    ONEDPT,ONEDPT                                                    
         BNZ   *+8                                                              
         LA    R5,STATSV           ADDRESS OF STATION LIST                      
         ST    R5,ASTATS                                                        
         CLI   ONEDPT,0                                                         
         BE    NORANK                                                           
         GOTO1 =A(EXPSTATS),DMCB,(R9),(RC),RR=RELO                              
         GOTO1 =A(RANKSTA),DMCB,(R9),(RC),RR=RELO                               
         B     PREP2                                                            
NORANK   OC    MKTNUM,MKTNUM       JIC A LIST ONLY INPUT                        
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
*                                                                               
PREP2    LA    RE,BUFF             INITIALIZE MAIN BUFFER                       
         L     RF,=F'2000'         CLEAR RF BYTES FROM A(RE)                    
         XCEF                                                                   
         GOTO1 =A(FILLBUFF),DMCB,(R2),(R3),(R4),(R5),(R9),(RC),RR=RELO          
         DS    0H                                                               
*                                                                               
PREP2A   GOTO1 =A(AVEBUF),DMCB,(R9),RR=RELO                                     
         MVC   MAINDEM,TMPDEMOS+1                                               
         GOTO1 =A(IMPRESS),DMCB,(R9),(RC),RR=RELO                               
         CLI   DAYPOPT,C'Y'                                                     
         BE    PREP2S                                                           
         CLI   ONEDPT,0                                                         
         BNE   PREP2S                                                           
         GOTO1 VSUBR07,DMCB,('SUMDPTE',(RC))                                    
PREP2S   BAS   RE,PRNTBUFF         PRINT LINES                                  
         XC    UNIVBK,UNIVBK                                                    
         XC    UNIVS,UNIVS                                                      
         LA    RE,BUFF                                                          
         L     RF,=F'2000'                                                      
         XCEF                                                                   
         L     RE,MYWORK                                                        
         L     RF,=F'800'                                                       
         XCEF                                                                   
         MVI   COMBSW,C'N'                                                      
         L     RE,ULPNTR                                                        
         CLI   ONEDPT,0                                                         
         BNE   FIXULLP2                                                         
         LH    R1,COMBCNTR                                                      
         LTR   R1,R1                                                            
         BZ    PREP1A                                                           
*-----------------------------------------------------------                    
* NEXT ELEMENT IN USER LIST                                                     
*-----------------------------------------------------------                    
FIXULLP  ZIC   RF,1(RE)                                                         
         AR    RF,RE                                                            
         LR    RE,RF                                                            
         BCT   R1,FIXULLP                                                       
         ST    RF,ULPNTR                                                        
         B     PREP1A                                                           
         SPACE 1                                                                
FIXULLP2 CLI   0(RE),0                                                          
         BZ    FIXULXIT                                                         
         CLI   0(RE),1                                                          
         BNE   FIXULNXT                                                         
         CLC   MKTNUM,2(RE)                                                     
         BNE   FIXULXIT                                                         
FIXULNXT ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         ST    RE,ULPNTR                                                        
         B     FIXULLP2                                                         
FIXULXIT B     PREP1A                                                           
*-------------------------------------------------------------                  
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
         BNO   PREP4A                                                           
*        MVI   HRSMKTH+5,0                                                      
         BAS   RE,VMULTBK                                                       
         SPACE 1                                                                
PREP4A   MVC   DBAREC,AIO1                                                      
         MVI   DBFUNCT,DBGETMK                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'R'                                                    
         MVC   DBSELRMK,MKTNUM                                                  
         GOTO1 DEMAND,DMCB,DBLOCK,MKTHOOK                                       
         L     R1,ULPNTR                                                        
         CLI   0(R1),0             NOTHING AFTER THAT                           
         BNE   PREP5               NO, USE THE DATA                             
         B     PREP1               YES, JUST SHOW FOR THAT MARKET               
         SPACE 1                                                                
PREP5    CLI   0(R1),2             STATION LIST                                 
         BNE   PREP6                                                            
         CLI   DRIVESTA,C'Y'       USING A DRIVE?                               
         BNE   PREP5A              YES, SKIP INDIVIDUAL STATIONS                
         B     PREP1                                                            
*                                                                               
         ZIC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         ST    R1,ULPNTR                                                        
         CLI   0(R1),0             END OF LIST                                  
         BE    PREP1                 -OR-                                       
         CLI   0(R1),1             NEW MARKET                                   
         BE    PREP1               HAVE TO PRINT REPORT                         
         B     PREP6                                                            
PREP5A   ZIC   RE,1(R1)            SET UP FOR EXECUTE                           
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
         CLI   0(R1),1                                                          
         BE    PREP1                                                            
         ZIC   RE,1(R1)            UNDEFINED - JUST BYPASS                      
         AR    R1,RE                                                            
         ST    R1,ULPNTR                                                        
         B     PREP4                                                            
         SPACE 1                                                                
*PREP8   XC    DPENTRY,DPENTRY                                                  
PREP8    LA    RF,STATSV                                                        
         MVC   0(5,RF),2(R1)                                                    
         L     RF,MYWORK                                                        
         MVC   0(5,RF),=C'CMB1 '   PUT IT IN, RANKSTA MAY                       
         LH    RF,=H'1'            NOT BE EXECUTED                              
         STH   RF,COMBCNTR                                                      
         MVI   COMBSW,C'Y'                                                      
         B     PREP1NC                                                          
         SPACE 3                                                                
PREPXIT  CLI   SEPAOPT,C'D'                                                     
         BNE   PREPXIT1                                                         
         CLI   PRPNDEMS,0                                                       
         BZ    PREPXIT1                                                         
         B     P_SEP1                                                           
PREPXIT1 B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL PRINTING OF BUFFER                            
         SPACE 3                                                                
*              OUTPUT              PRINT LINES                                  
         SPACE 1                                                                
INTRCPT  DC    C' '                                                             
TOTBUFF  DC    20F'0'                                                           
PRNTBUFF NTR1                                                                   
         MVI   INTRCPT,C'N'                                                     
         GOTO1 =A(SETTITLE),DMCB,(R8),(R9),(RC),RR=RELO                         
         CLI   UNIVOPT,C'Y'                                                     
         BNE   PB1                                                              
         GOTO1 =A(PRUNIV),DMCB,(R8),(R9),(RC),RR=RELO                           
*                                                                               
PB1      LA    R4,BUFF                                                          
         USING TDD,R4                                                           
         CLI   DBSELSRC,C'M'       CANADA HAS NO MARKET STATION                 
         BE    PB1_0               TO SKIP, SO DON'T                            
         CLI   DAYPOPT,C'Y'                                                     
         BE    PB1_0                                                            
         LA    R4,L'TDENT(R4)      SKIP MARKET STATION                          
         SPACE 1                                                                
PB1_0    LA    RE,30               DEFUALT LOOP CONTROL                         
         CLI   ONEDPT,0            DO STATION COUNT LOOP                        
         BE    *+8                 IF MARKET REPORT                             
         IC    RE,NSTATS                                                        
         LR    R0,RE                                                            
         SPACE 1                                                                
         L     R5,MYWORK                                                        
         CLI   DBSELSRC,C'M'                                                    
         BE    PB1_1                                                            
         CLI   DAYPOPT,C'Y'                                                     
         BE    PB1_1                                                            
         LA    R5,9(R5)            SKIP MARKET STATION IN STATION LIST          
PB1_1    CLI   UNIVOPT,C'Y'                                                     
         BNE   PB1A                                                             
         OC    ONEDPT,ONEDPT                                                    
         BNZ   PB1A                                                             
         LA    R5,16(R5)                                                        
         LA    R4,L'TDENT(R4)                                                   
PB1A     MVC   DPTLPTR,DPTLBASE                                                 
         SPACE 1                                                                
PB2      CLI   DAYPOPT,C'Y'                                                     
         BNE   PB2B                                                             
         L     RE,DPTLPTR                                                       
         LR    R6,RE                                                            
         MVC   SPACOPT,3(RE)                                                    
         CLI   DBSELSRC,C'M'                                                    
         BE    PB2A                                                             
         CLI   MAINDEM,C'S'                                                     
         BNE   PB2A                                                             
         GOTO1 =A(FILLTOTS),DMCB,(R4),TOTBUFF,(R9),(RC),RR=RELO                 
         GOTO1 =A(SHRBUFF),DMCB,(R4),TOTBUFF,TMPDEMOS,(R9),(RC),RR=RELO         
PB2A     CLI   UNIVOPT,C'Y'        2 SPACE ALREADY DONE                         
         BNE   *+8                                                              
         MVI   SPACOPT,1                                                        
*                                                                               
PB2B     OC    0(5,R4),0(R4)                                                    
         BZ    PB3                                                              
         LA    RE,4(R6)                                                         
         ST    RE,DPTLPTR                                                       
*                                                                               
         CLI   UNIVOPT,C'Y'        NEED BOX OF LINES FOR UNIV REPORT            
         BNE   PB2C                                                             
         ZIC   RE,LINE                                                          
         ZIC   RF,MAXLINES                                                      
         SR    RF,RE                                                            
         CH    RF,=H'3'                                                         
         BH    PB2C                                                             
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
PB2C     MVC   SPACING,SPACOPT                                                  
         GOTO1 =A(FORMLINE),DMCB,(R4),(R5),(R8),(R9),(RC),             X        
               (0,TMPDEMOS),RR=RELO                                             
         CLC   =C'CMB',P+1                                                      
         BNE   SPOOLIT                                                          
         CLI   ONEDPT,0                                                         
         BNE   SPOOLIT                                                          
         B     NOSPOOL                                                          
SPOOLIT  CLI   INTRCPT,C'Y'                                                     
         BE    NOSPOOL                                                          
         GOTO1 SPOOL,DMCB,(R8)     ONE MORE LINE FOR BOX BOTTOM                 
NOSPOOL  CLI   UNIVOPT,C'Y'                                                     
         BNE   PB3                                                              
         MVI   SPACING,1                                                        
         MVI   SHAREOPT,C'Y'       INDEX TO FIRST DEM                           
         LA    R3,P+1                                                           
         A     R3,DISP                                                          
         A     R3,DISPD                                                         
         LA    R3,2(R3)                                                         
         LR    R7,R3               SAVE PL DISPLACMENT                          
* -----> BAS   RE,EDTDEML                                                       
         GOTO1 =A(FORMLINE),DMCB,(R4),(R5),(R8),(R9),(RC),             X        
               (1,TMPDEMOS),RR=RELO                                             
         MVI   SHAREOPT,C'N'                                                    
         MVC   2344(5,R8),SPACES   CLEAR EXTRA # (2ND LINE)                     
         OC    COMBCNTR,COMBCNTR                                                
         BZ    PB2D                                                             
         MVC   2344(8,R8),SPACES                                                
PB2D     GOTO1 SPOOL,DMCB,(R8)     PRINT THE LINE                               
         LR    R3,R7                                                            
         GOTO1 VSUBR07,DMCB,('INDEXUE',(RC))                                    
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   SPACING,SPACOPT                                                  
         SPACE 1                                                                
PB3      CLI   DAYPOPT,C'Y'                                                     
         BNE   *+12                                                             
         LA    R5,16(R5)                                                        
         B     PB3A                                                             
         SPACE 1                                                                
         CLI   ONEDPT,0                                                         
         BE    *+12                                                             
         LA    R5,9(R5)                                                         
         B     PB3A                                                             
         SPACE 1                                                                
         TM    0(R4),X'80'         TOTAL LINE REQUIRED                          
         BZ    PB3A                                                             
         LR    R3,R4               SAVE MY INDEX                                
         NI    0(R4),X'7F'         POINT TO TOTAL BUFFER                        
         ZIC   RE,0(R4)                                                         
         MH    RE,=AL2(L'TDENT)                                                 
         LA    R4,BUFF(RE)                                                      
         GOTO1 =A(FORMLINE),DMCB,(R4),(R5),(R8),(R9),(RC),             X        
               (0,TMPDEMOS),RR=RELO                                             
         CLC   P+1(6),=C'7P-12M'                                                
         BNE   *+8                                                              
         MVI   INTRCPT,C'Y'                                                     
         MVI   SPACING,2           WITH EXTRA SPACING                           
         GOTO1 SPOOL,DMCB,(R8)     3RD LINE                                     
         LR    R4,R3               RESTORE MY INDEX                             
         SPACE 1                                                                
PB3A     LA    R4,L'TDENT(R4)                                                   
         BCT   R0,PB2                                                           
         CLI   SPANN,C' '                                                       
         BNH   PB3B                                                             
         L     RE,=A(VIO1)                                                      
         A     RE,RELO                                                          
         MVC   P(L'VIO1),0(RE)                                                  
         L     RE,=A(VIO2)                                                      
         A     RE,RELO                                                          
         MVC   P2(L'VIO2),0(RE)                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
PB3B     CLI   ONEDPT,0            SHOW STATION IN HEADLINE?                    
         BE    NOCMBS              YES, SKIP FOOTLINES                          
         MVC   SPACING,SPACOPT                                                  
         OC    COMBCNTR,COMBCNTR                                                
         BZ    NOCMBS                                                           
         GOTO1 SPOOL,DMCB,(R8)     SKIP A LINE FIRST                            
* SHOW ALL THE COMBOS                                                           
         MVC   SPACING,SPACOPT                                                  
         LA    RF,P                                                             
         L     RE,ULPNTR                                                        
         LH    R3,COMBCNTR                                                      
NEXTSTAP CLI   0(RE),0                                                          
         BE    SHOW_P                                                           
         CLI   0(RE),3             COMBO?                                       
         BE    COMB2P              YES, PUT COMBO TO P                          
         ZIC   R4,1(RE)                                                         
         AR    RE,R4                                                            
         B     NEXTSTAP            INSURE # COMBOS FOR THAT MARKET              
COMB2P   CLC   =CL9' ',2(RE)                                                    
         BNE   COMB2PA                                                          
         MVC   1(5,RF),11(RE)                                                   
         MVI   6(RF),C'/'                                                       
         MVC   7(5,RF),16(RE)                                                   
         LR    R4,RF                                                            
         LA    R4,11(R4)                                                        
         B     MORETSPS                                                         
COMB2PA  MVC   0(9,RF),2(RE)                                                    
         LR    R4,RF                                                            
         LA    R4,8(R4)            FIRST CHAR FROM END                          
MORETSPS CR    R4,RF               CANCEL OUT LEADING SPACES                    
         BH    *+8                 TO BEGINNING OF LABEL                        
         B     STOPTRLS                                                         
         CLI   0(R4),C' '          CANCEL OUT LEADING SPACES                    
         BNE   STOPTRLS                                                         
         BCTR  R4,0                                                             
         B     MORETSPS                                                         
STOPTRLS LA    R4,1(R4)                                                         
         MVI   0(R4),C'='                                                       
         LR    RF,R4                                                            
         LA    RF,1(RF)                                                         
         ZIC   R4,1(RE)                                                         
         AR    R4,RE               R4 POINTS TO NEXT RECORD                     
         LA    RE,11(RE)                                                        
STA_P_LP CR    RE,R4                                                            
         BL    PUTSTAP                                                          
         LR    RE,R4                                                            
         LA    RF,3(RF)                                                         
         BCT   R3,NEXTSTAP         INSURE # COMBOS FOR THAT MARKET              
         B     SHOW_P                                                           
PUTSTAP  MVC   0(4,RF),0(RE)                                                    
         MVI   4(RF),C'-'                                                       
         CLI   4(RE),C'C'                                                       
         BNE   STANOCO                                                          
         MVC   5(2,RF),=C'CO'                                                   
         B     PSTABND                                                          
STANOCO  CLI   4(RE),C'F'                                                       
         BNE   STANOFM                                                          
         MVC   5(2,RF),=C'FM'                                                   
         B     PSTABND                                                          
STANOFM  CLI   4(RE),C'A'                                                       
         BNE   STANOAM                                                          
         MVC   5(2,RF),=C'AM'                                                   
         B     PSTABND                                                          
STANOAM  CLI   4(RE),C'D'                                                       
         BNE   STANOFF                                                          
         MVC   5(2,RF),=C'FF'                                                   
         B     PSTABND                                                          
STANOFF  MVC   5(2,RF),=C'AA'                                                   
PSTABND  LA    RE,5(RE)                                                         
         LA    RF,7(RF)                                                         
         CR    RE,R4                                                            
         BNL   STA_P_LP                                                         
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
         B     STA_P_LP                                                         
*                                                                               
SHOW_P   GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
NOCMBS   MVI   FORCEHED,C'Y'       FORCE HEADS ON NEXT                          
         B     XIT                                                              
         SPACE 1                                                                
PB4      CLI   COMBNUM,0                                                        
         BE    XIT                                                              
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
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK                                                    
         SPACE 3                                                                
HOOK     NTR1                                                                   
         L     R7,ATWA                                                          
         GOTO1 VSUBR07,DMCB,('SETBOXE',(RC))                                    
         GOTO1 =A(SETTITLE),DMCB,(R8),(R9),(RC),RR=RELO                         
         GOTO1 VSUBR08,DMCB,('HH2E',(RC))                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET UP BOXES                                          
         SPACE 3                                                                
*              OUTPUT              BOXCOLS BOXROWS                              
         EJECT                                                                  
* *********************************************************************         
* DEMHOOK -    CALLS DEMHK BECAUSE NOT ENOUGH ROOM HERE FOR WHOLE THING         
* *********************************************************************         
DEMHOOK  NTR1                                                                   
         GOTO1 =A(DEMHK),DMCB,(R9),(RC),RR=RELO                                 
         B     XIT                                                              
*                                                                               
* ********************************************************************          
DEMHOOKR NTR1                      HOOK FOR RANKING                             
         XC    THISLINE,THISLINE                                                
         GOTO1 DEMOUT,DMCB,(C'D',RDEMO),DBLOCK,THISLINE                         
         ICM   RE,15,5(R5)                                                      
         A     RE,THISLINE                                                      
         STCM  RE,15,5(R5)                                                      
         ZIC   R1,NTIMES                                                        
         LA    R1,1(R1)                                                         
         STC   R1,NTIMES                                                        
         B     XIT                                                              
*                                                                               
* ********************************************************************          
*              COMMON ROUTINES                                                  
* ********************************************************************          
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
         SPACE 2                                                                
PATCH    DS    0H                  PATCH AREA                                   
         DC    XL32'00'                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ERRORS SPECS AND CONSTANTS                                       
         SPACE 3                                                                
*                                  MY ERROR MESSAGES                            
         SPACE 1                                                                
*ILTERR  DC    C'** ERROR ** INVALID FILTER'                                    
TOOMNY   DC    C'** ERROR ** LIMITED TO 100 STATIONS'                           
TOOMNYDM DC    C'** ERROR ** ONLY 1 DEMO ALLOWED'                               
TOOMNYCT DC    C'** ERROR ** ONLY 1 CATEGORY ALLOWED'                           
MKTERR   DC    C'** ERROR ** MARKET NOT FOUND'                                  
MKTERR2  DC    C'** MARKET MISSING OR MISSING M= IN USER LIST'                  
STAERR   DC    C'** ERROR ** INVALID DAYPART OR STATION '                       
LISTERR  DC    C'** ERROR ** NEED GOOD MARKET IN LIST'                          
SOONIT   DC    C'BOOKS X MARKETS > '                                            
SOONITSP DC    C'BKS X MKTS X DEMOS > '                                         
SOONITN  DC    C'5 REQUEST SOON OR OVERNIGHT'                                   
SOONITS  DC    C'12 REQUEST OVERNIGHT'                                          
SOONITO  DC    C'21 SPLIT THE REQUEST'                                          
ERRBOOK  DC    C'REQUESTED BOOK(S) NOT FOUND- MARKET'                           
RANKERR  DC    C'** ERROR ** RANK ONLY AVAILABLE FOR DAYPART REQUEST'           
BADMUL   DC    C'MARKET IS NOT SWEPT (N) TIMES A YEAR '                         
BADMULP  EQU   BADMUL+21                                                        
VIO1     DC    C'#=VIOLATION: SPECIAL STATION ACTIVITIES. DETAILS: P.13+        
                ARB MKT. REPORT'                                                
VIO2     DC    C'A = STATION CITED  Z = STATION LISTED BELOW THE LINE'          
         EJECT                                                                  
* LIST OF DAYS/DAYPART NUMBER/TABLE SLOT/SPACING FOR DAYPART OPTION             
********************CUME DEMO DAYPARTS******************                        
DPTLIST  DC    X'7C',AL1(01,01,1)  M-F 6-10A                                    
         DC    X'7C',AL1(02,02,1)  M-F 10A-3P                                   
         DC    X'7C',AL1(03,03,1)  M-F 3-7P                                     
         DC    X'7C',AL1(04,04,1)  M-F 7P-MID                                   
         DC    X'7C',AL1(07,05,2)  M-F 6-10A+3-7P                               
         DC    X'7F',AL1(06,06,1)  M-SU 6A-12M                                  
         DC    X'7C',AL1(06,07,1)  M-F 6A-12M                                   
         DC    X'03',AL1(06,08,1)  S-S 6A-12M                                   
         DC    X'7C',AL1(05,09,2)  M-F 6A-7PP                                   
         DC    X'7C',AL1(18,10,1)  M-F 6A-3P                                    
         DC    X'7C',AL1(09,11,1)  M-F 10A-7P                                   
         DC    X'7C',AL1(14,12,1)  M-F 6-10A+7P-MID                             
         DC    X'7C',AL1(11,13,1)  M-F 3P-12M                                   
         DC    X'7C',AL1(17,14,1)  M-F 10A-3P+7P-MID                            
         DC    X'7C',AL1(15,15,2)  M-F 6A-3P+7P-MID                             
         DC    X'7C',AL1(10,16,1)  M-F 10A-12M                                  
         DC    X'7C',AL1(13,17,1)  M-F 6-10A+3P-MID                             
         DC    X'02',AL1(01,18,2)  SAT 6-10A                                    
         DC    X'02',AL1(02,19,1)  SAT 10A-3P                                   
         DC    X'02',AL1(03,20,1)  SAT 3-7P                                     
         DC    X'02',AL1(04,21,1)  SAT 7P-MID                                   
         DC    X'01',AL1(01,22,2)  SUN 6-10A                                    
         DC    X'01',AL1(02,23,1)  SUN 10A-3P                                   
         DC    X'01',AL1(03,24,1)  SUN 3-7P                                     
         DC    X'01',AL1(04,25,1)  SUN 7P-12M                                   
         DC    X'FF'                                                            
         SPACE 2                                                                
* LIST OF DAYS/DAYPART NUMBER/TABLE SLOT/SPACING FOR DAYPART OPTION             
*****************REGULAR DEMO DAYPARTS******************                        
DPTLIST2 DC    X'7C',AL1(01,01,1)  M-F 6-10A                                    
         DC    X'7C',AL1(02,02,1)  M-F 10A-3P                                   
         DC    X'7C',AL1(03,03,1)  M-F 3-7P                                     
         DC    X'7C',AL1(04,04,1)  M-F 7P-MID                                   
         DC    X'7C',AL1(07,05,2)  M-F 6-10A+3-7P                               
         DC    X'7C',AL1(06,06,1)  M-F 6A-12M                                   
         DC    X'02',AL1(01,07,2)  SAT 6-10A                                    
         DC    X'02',AL1(02,08,1)  SAT 10A-3P                                   
         DC    X'02',AL1(03,09,1)  SAT 3-7P                                     
         DC    X'02',AL1(04,10,1)  SAT 7P-MID                                   
         DC    X'01',AL1(01,11,2)  SUN 6-10A                                    
         DC    X'01',AL1(02,12,1)  SUN 10A-3P                                   
         DC    X'01',AL1(03,13,1)  SUN 3-7P                                     
         DC    X'01',AL1(04,14,1)  SUN 7P-12M                                   
         DC    X'7F',AL1(06,15,2)  M-SU 6A-12M                                  
         DC    X'FF'                                                            
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
       ++INCLUDE RADSDPLC                                                       
         EJECT                                                                  
         DS    0H                                                               
* ********************************************************************          
* DEMHK   -    HOOK FOR READING IN DEMO VALUES INTO BUFF PTD TO BY R4           
* ********************************************************************          
DEMHK    NMOD1 0,**DEMHK**                                                      
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         CLI   RNK,C'Y'            IF CALLED FROM RANK, GO TO DEMHKR            
         BE    DEMHKR                                                           
*                                                                               
         OC    DBFACTOR,DBFACTOR   ANYTHING ACTIVE?                             
         BZ    DEMXIT                                                           
         CLI   CALCOPT,C'Y'                                                     
         BE    DMHK0                                                            
         CLI   UNIVOPT,C'Y'                                                     
         BNE   *+8                                                              
DMHK0    BAS   RE,SETUNIV                                                       
         L     RE,DBAREC                                                        
         USING DRKEY,RE                                                         
         CLI   DAYPOPT,C'Y'                                                     
         BNE   DMHK01                                                           
         L     R1,DPTLPTR                                                       
         ZIC   R6,2(R1)                                                         
*                                                                               
DMHK01   LA    RE,23(RE)                                                        
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
         CLI   ONEDPT,0            SINGLE DAYPART                               
         BE    DMHK1A              SAYS ITS STATIONS                            
         MVC   WORK+2(5),DBSELSTA                                               
         LR    RE,R5                                                            
         S     RE,MYWORK                                                        
         SRDL  RE,32                                                            
         D     RE,=F'9'                                                         
         LR    RE,RF                                                            
         LA    R4,BUFF                                                          
         B     DMHK2A                                                           
DMHK1A   CLI   DAYPOPT,C'Y'                                                     
         BNE   DMHK2                                                            
         GOTO1 DEFINE,DMCB,=C'PROGRAM',DBLOCK,WORK                              
         LR    RE,R6                                                            
         SLL   RE,4                                                             
         A     RE,MYWORK                                                        
         MVC   0(16,RE),WORK       SAVE PROGRAM NAME                            
*                                                                               
DMHK2    GOTO1 DEFINE,DMCB,=C'TIME',DBLOCK,WORK                                 
         LA    R4,BUFF                                                          
         USING TDD,R4                                                           
         ZIC   RE,WORK             START QH                                     
         SRL   RE,2                                                             
         CLI   DAYPOPT,C'Y'                                                     
         BNE   *+6                                                              
         LR    RE,R6                                                            
*                                                                               
DMHK2A   LA    RF,L'TDENT                                                       
         MR    RE,RE                                                            
         AR    R4,RF                                                            
         MVC   TDSHR(4),WORK+2                                                  
         LA    R3,TDVLMF                                                        
         CLI   DAYPOPT,C'Y'                                                     
         BNE   DMHK4                                                            
         L     RE,DBAREC                                                        
         USING DRKEY,RE                                                         
         MVC   TDDPDAY,DRHIGHD                                                  
         MVC   TDDPQH,DRHIQHR                                                   
         DROP  RE                                                               
         LA    R3,TDVLMF                                                        
         SPACE 1                                                                
DMHK4    ZIC   R1,TDCNT            COUNT SO I CAN AVERAGE LATER                 
         LA    R1,1(R1)                                                         
         STC   R1,TDCNT                                                         
*                                                                               
DEMHOOK0 ZIC   R1,NDEMOS                                                        
         LA    RE,THISLINE                                                      
DEMHOOK1 SR    R2,R2                                                            
         ICM   R2,7,0(R3)                                                       
         A     R2,0(RE)                                                         
         STCM  R2,7,0(R3)                                                       
         LA    RE,4(RE)                                                         
         LA    R3,3(R3)                                                         
         BCT   R1,DEMHOOK1                                                      
         SPACE 1                                                                
         CLI   TRACEOPT,C'Y'       OPTION TO TRACE                              
         BNE   DEMXIT                                                           
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
         B     DEMXIT                                                           
*                                                                               
* ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''            
DEMHKR   DS    0H                  HOOK FOR RANKING                             
         BAS   RE,SETUNIV                                                       
         XC    THISLINE,THISLINE                                                
         GOTO1 DEMOUT,DMCB,(C'D',RDEMO),DBLOCK,THISLINE                         
         ICM   RE,15,5(R5)                                                      
         A     RE,THISLINE                                                      
         STCM  RE,15,5(R5)                                                      
         ZIC   R1,NTIMES                                                        
         LA    R1,1(R1)                                                         
         STC   R1,NTIMES                                                        
         XC    UNIVBK,UNIVBK       ALLOW RNK=N TO RE-SET UNIVERSES              
         B     DEMXIT                                                           
*                                                                               
* ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''            
*                                                                               
DEMXIT   XMOD1                                                                  
         SPACE 1                                                                
*                                                                               
SETUNIV  NTR1  WORK=(R4,15)                                                     
         MVC   0(L'DEMOS,R4),DEMOS  SET A LIST FOR ME                           
         LA    RE,UNIVBK                                                        
         CLI   RNK,C'Y'                                                         
         BNE   SETU01                                                           
         MVC   0(3,R4),RDEMO       READ IN RANK DEMO'S UNIV                     
         MVI   4(R4),X'FF'                                                      
*                                                                               
SETU01   CLI   0(RE),0             CHECK IF DONE ALREADY                        
         BE    SETU02              NO - DO THIS NOW                             
         CLC   DBSELBK,0(RE)       HAVE THEM - GET OUT                          
         BE    SETUX                                                            
         LA    RE,2(RE)                                                         
         B     SETU01                                                           
         SPACE 1                                                                
SETU02   XC    THISLINE,THISLINE                                                
         LR    RE,R4               MODIFY LIST TO GET UNIVERSES                 
SETU10   CLI   0(RE),X'FF'                                                      
         BE    SETU20                                                           
         MVI   1(RE),C'U'                                                       
         LA    RE,3(RE)                                                         
         B     SETU10                                                           
*                                                                               
SETU20   GOTO1 DEMOUT,DMCB,(C'L',(R4)),DBLOCK,THISLINE                          
         OC    THISLINE(4),THISLINE NONE THERE - GET OUT                        
         BZ    SETUX                                                            
         ZIC   R0,NDEMOS           ADD THEM UP                                  
         LA    RE,THISLINE                                                      
         LA    R1,UNIVS                                                         
SETU30   SR    RF,RF                                                            
         ICM   RF,7,0(R1)                                                       
         A     RF,0(RE)                                                         
         STCM  RF,7,0(R1)                                                       
         LA    R1,3(R1)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,SETU30                                                        
*                                                                               
         LA    RE,UNIVBK           SAVE THE BOOK                                
         CLI   0(RE),0                                                          
         BE    *+12                                                             
         LA    RE,2(RE)                                                         
         B     *-12                                                             
         MVC   0(2,RE),DBSELBK                                                  
*                                                                               
SETUX    XIT1                                                                   
         EJECT                                                                  
* ********************************************************************          
* AVEBUF                                                                        
* ********************************************************************          
AVEBUF   NMOD1 0,**AVEBUF**                                                     
         L     R9,0(R1)                                                         
*                                                                               
         LA    R4,BUFF             AVERAGE THE BUFFER                           
         USING TDD,R4                                                           
         LA    R0,24                                                            
*                                                                               
AVEBUF1  CLI   ONEDPT,0                                                         
         BE    AVEBUF2                                                          
         ZIC   R0,NSTATS                                                        
AVEBUF2  CLI   TDCNT,2             DON'T NEED DIVIDE                            
         BL    AVEBUF6                                                          
         SR    R1,R1                                                            
         IC    R1,TDCNT                                                         
         LR    R5,R4               SAVE TAB START                               
         ZIC   R8,NDEMOS           LOOP FOR NUMBER OF DEMOS                     
AVEBUF4  SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,7,TDVLMF                                                      
         MH    RF,=H'100'                                                       
         DR    RE,R1                                                            
         AH    RF,=H'50'                                                        
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         STCM  RF,7,TDVLMF                                                      
*                                                                               
         LA    R4,3(R4)                                                         
         BCT   R8,AVEBUF4                                                       
         LR    R4,R5                                                            
AVEBUF6  LA    R4,L'TDENT(R4)                                                   
         BCT   R0,AVEBUF2                                                       
AVEXMOD1 XMOD1                                                                  
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DS    0H                                                               
ADDBUF   NMOD1 0,**ADDBUF**                                                     
         L     R9,0(R1)                                                         
*                                                                               
         LA    R4,BUFF             ADD TWO BUFFERS                              
         USING TDD,R4                                                           
         LA    R0,24                                                            
         CLI   ONEDPT,0                                                         
         BE    ADDBUF2                                                          
         ZIC   R0,NSTATS                                                        
ADDBUF2  MVI   TDCNT,1             DON'T NEED DIVIDE                            
         LR    R5,R4               SAVE TAB START                               
*        LA    R8,18               LOOP FOR 18 DEMOS                            
         ZIC   R8,NDEMOS           LOOP FOR NUMBER OF DEMOS                     
         LA    R1,2000(R4)                                                      
         MVC   0(TDCNT-TDENT,R1),0(R4)                                          
ADDBUF4  SR    RE,RE                                                            
         ICM   RE,7,TDVLMF                                                      
         ICM   RF,7,TDVLMF-TDENT(R1)                                            
         AR    RE,RF                                                            
         STCM  RE,7,TDVLMF-TDENT(R1)                                            
         LA    R4,3(R4)                                                         
         LA    R1,3(R1)                                                         
         BCT   R8,ADDBUF4                                                       
         LR    R4,R5                                                            
ADDBUF6  LA    R4,L'TDENT(R4)                                                   
         BCT   R0,ADDBUF2                                                       
         LA    RE,BUFF                                                          
         L     RF,=F'2000'                                                      
         XCEF                                                                   
         XMOD1                                                                  
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* *********************************************************************         
* RANKSTA - RANK STATION                                                        
*        RANK THE LIST OF STATIONS ON THE SPECIFIED DEMO, OR USE THE            
*        DEFAULT.  READ IN THE DEMO FOR THE RANKING SEPERATELY FROM             
*        THE ACTUAL DEMOS.  RANK STATIONS BASED ON THIS DEMO.                   
* *********************************************************************         
         DS    0H                                                               
RANKSTA  NMOD1 0,**RANKSTA**                                                    
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
RANKSTA1 L     R5,ASTATS                                                        
         MVI   RNK,C'Y'            WE'RE DOING RANKING, SET FLAG                
         MVC   SVNSTATS,NSTATS                                                  
         MVI   DBSELMED,C'R'                                                    
         XC    DBSELMK,DBSELMK                                                  
         LA    RE,DBRADIC                                                       
         ST    RE,DBEXTEND                                                      
         XC    0(128,RE),0(RE)                                                  
         USING DBRID,RE                                                         
         MVC   DBRID,=C'RADI'                                                   
         MVI   DBRCOPT,C'Y'                                                     
         MVC   DBSELMK,MKTNUM                                                   
         MVC   DBSELSTA,0(R5)                                                   
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBAREC,AIO1                                                      
         LA    R2,BOOKS                                                         
         ZIC   R0,NBOOKS                                                        
         MVC   DBSELBK,1(R2)                                                    
         MVC   DBBTYPE,3(R2)                                                    
         MVC   SVNSTATS,NSTATS                                                  
         MVC   DBFILE,=C'RDP'                                                   
         MVC   DBSELDAY,ONEDPT                                                  
         MVC   DBSELPRG+1(1),ONEDPT+1                                           
         XC    DBSELTIM,DBSELTIM                                                
         MVI   STAMISS,C'Y'                                                     
         L     R1,ULPNTR                                                        
CHKAGAIN CLI   0(R1),0                                                          
         BE    RNKSTA2P                                                         
         CLI   0(R1),1                                                          
         BNE   CHK4_2                                                           
         CLC   MKTNUM,2(R1)                                                     
         BNE   RNKSTA2P                                                         
         B     NEXTONE                                                          
CHK4_2   CLI   0(R1),2                                                          
         BNE   NEXTONE                                                          
         MVI   STAMISS,C'N'                                                     
         B     RNKSTA2P                                                         
NEXTONE  ZIC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     CHKAGAIN                                                         
RNKSTA2P CLI   STAMISS,C'Y'        STATION MISSING FOR THIS SET                 
         BE    RNKSTA2R            YES, FILL WITH OTHER STATIONS                
         MVI   NSTATS,0                                                         
         CLI   DBSELSRC,C'M'                                                    
         BE    RNKSTA2Q            NO MARKET STATION FOR CANADA                 
         MVI   NSTATS,1            DO THE MARKET STATION                        
RNKSTA2Q MVC   SVNSTATS,NSTATS                                                  
RNKSTA2R L     R5,ASTATS                                                        
         ZIC   R3,NSTATS                                                        
RNKSTA2S ZIC   R0,NBOOKS                                                        
         LA    R2,BOOKS                                                         
         MVI   NTIMES,0                                                         
         MVC   DBSELSTA,0(R5)                                                   
*                                                                               
RANKSTA2 MVC   DBSELBK,1(R2)                                                    
         MVC   DBBTYPE,3(R2)                                                    
*        MVC   SVNSTATS,NSTATS                                                  
         MVC   DBFILE,=C'RDP'                                                   
         MVC   DBSELDAY,ONEDPT                                                  
         MVC   DBSELPRG+1(1),ONEDPT+1                                           
         XC    DBSELTIM,DBSELTIM                                                
RANKSTA3 GOTO1 DEMAND,DMCB,DBLOCK,DEMHOOKR                                      
         LA    R2,4(R2)                                                         
         BCT   R0,RANKSTA2                                                      
         BAS   RE,AVERANK                                                       
*                                                                               
         LA    R5,9(R5)                                                         
         BCT   R3,RNKSTA2S                                                      
*                                                                               
RNKSTA3X XC    COMBCNTR,COMBCNTR                                                
         L     R1,ULPNTR                                                        
*                                                                               
* ADD COMBOS HERE IF THERE ARE ANY                                              
* THEIR KEYS WILL BE CMB?, WHERE ? IS 1-7                                       
*                                                                               
ALLCMBLP CLI   0(R1),0             COMBO HERE?                                  
         BE    SORTTHEM                                                         
         CLI   0(R1),3                                                          
         BE    INS_COMB            INSERT COMBO                                 
         CLI   0(R1),1                                                          
         BE    TESTMRKS                                                         
         CLI   0(R1),2                                                          
         BH    SORTTHEM                                                         
         LR    R7,R1               SAVE USER LIST POINTER                       
         ZIC   R4,1(R1)                                                         
         AR    R4,R1                                                            
         LR    R3,R4                                                            
         LA    R6,2(R1)                                                         
STAPCLOP CR    R6,R3                                                            
         BNL   STANXTST            NEXT STATION                                 
         MVC   0(5,R5),0(R6)                                                    
         MVC   DBSELSTA,0(R6)                                                   
         ZIC   R0,NBOOKS                                                        
         LA    R2,BOOKS                                                         
         MVI   NTIMES,0                                                         
STALP1   MVC   DBSELBK,1(R2)                                                    
         MVC   DBBTYPE,3(R2)                                                    
         GOTO1 DEMAND,DMCB,DBLOCK,DEMHOOKR                                      
         LA    R2,4(R2)                                                         
         BCT   R0,STALP1                                                        
         BAS   RE,AVERANK                                                       
         ZIC   R1,NSTATS                                                        
         LA    R1,1(R1)                                                         
         STC   R1,NSTATS                                                        
         LA    R6,5(R6)                                                         
         LA    R5,9(R5)            NEXT STATION                                 
         B     STAPCLOP                                                         
*                                                                               
STANXTST LR    R1,R7               RESTORE USER LIST POINTER                    
REGNEXT  ZIC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     ALLCMBLP                                                         
TESTMRKS CLC   2(2,R1),MKTNUM                                                   
         BNE   SORTTHEM                                                         
         B     REGNEXT                                                          
INS_COMB LH    R4,COMBCNTR         READ IN DATA FOR COMBO OVER STATNS           
         LA    R4,1(R4)                                                         
         STH   R4,COMBCNTR                                                      
         XC    0(9,R5),0(R5)       CLEAR THE RECORD                             
         MVC   0(5,R5),=C'CMB  '                                                
         STC   R4,3(R5)            PUT COMBO NUMBER IN STATION LIST             
         OI    3(R5),X'F0'         MAKE IT EBCIDIC                              
         SPACE 1                                                                
         ZIC   R4,1(R1)                                                         
         AR    R4,R1                                                            
         LR    R3,R4               R3 HOLDS NEXT COMBO                          
         LA    R6,11(R1)           FIRST PIECE OF COMBO                         
*                                                                               
* ALPHA-SORT LIST OF STATIONS FOR CONSISTENCY IN TSL/CUME CALCS                 
         SR    R2,R2               R2= # STATIONS IN COMBO                      
INS_CMB5 LA    R2,1(R2)            BUMP STATN COUNTER                           
         LA    R6,5(R6)            NEXT STATION                                 
         CR    R6,R3               FINISHED W/STATN LIST?                       
         BL    INS_CMB5                                                         
*                                                                               
         LA    R6,11(R1)           R6=FIRST STATION IN COMBO                    
         LR    R4,R1               SAVE R1                                      
         GOTO1 XSORT,DMCB,(R6),(R2),5,5,0                                       
         LR    R1,R4                                                            
*                                                                               
* ADD RANKING OF ALL THE PIECES OF THE COMBO INTO THE COMBO                     
         LR    R2,R1                                                            
         XC    ACCUM,ACCUM         ACCUM=SUM OF DEMO OVER STATIONS              
         XC    BUFF(L'TDENT),BUFF  WORK AREA FOR CALCRDF                        
         XC    PARM3,PARM3         HOLDS 1BYTE-PARM3 OF CALL TO CALCRDF         
         XC    CMBACCUM,CMBACCUM   CALCRDF USES TO HOLD FINAL RESULTS           
         LA    RE,CMBACCUM                                                      
         ST    RE,ACMBACC          NEED TO PASS ADDR TO CALCRDF                 
         MVI   CNOSTA,0                                                         
*                                                                               
CMBPCLOP CR    R6,R3               NO MORE PIECES?                              
         BNL   CMBPLPXT            NEXT COMBO                                   
         MVC   DBSELSTA,0(R6)                                                   
         ZIC   R0,NBOOKS                                                        
         LA    R4,BOOKS                                                         
         MVC   DEM,RDEMO+1          SAVE REAL DEMO TYPE                         
         MVI   NTIMES,0                                                         
*                                                                               
CMBLP1   MVC   DBSELBK,1(R4)                                                    
         MVC   DBBTYPE,3(R4)                                                    
         CLI   RDEMO+1,C'X'         IF TSL, READ IN CUME                        
         BNE   *+8                                                              
         MVI   RDEMO+1,C'C'                                                     
         GOTO1 DEMAND,DMCB,DBLOCK,DEMHOOK                                       
         MVC   RDEMO+1(1),DEM        RESTORE REAL DEMO TYPE                     
         LA    R4,4(R4)                                                         
         BCT   R0,CMBLP1                                                        
*                                                                               
         BAS   RE,AVERANK                                                       
         ICM   RE,15,5(R5)                                                      
         A     RE,ACCUM                                                         
         ST    RE,ACCUM                                                         
*                                                                               
         CLI   RDEMO+1,C'C'        APPLY RAND DUP FORM (RDF) TO CUME            
         BE    *+12                                                             
         CLI   RDEMO+1,C'X'        AND TO TSL ONLY                              
         BNE   CMBLP5                                                           
         MVI   PARM3+3,RDFRNKQ       PROCESS CUME FOR RANKING                   
         BAS   RE,CALLRDF                                                       
*                                                                               
CMBLP5   XC    5(5,R5),5(R5)                                                    
         LA    R6,5(R6)            NEXT STATION IN THIS COMBO                   
         B     CMBPCLOP                                                         
*                                                                               
CMBPLPXT CLI   RDEMO+1,C'X'        IF TSL, DO FINAL CALCT'N                     
         BNE   CMBPL2                                                           
         MVI   PARM3+3,RDFRNKQ+RDFTSLQ   PROCESS TSL FOR RANKING                
         BAS   RE,CALLRDF                                                       
*                                                                               
CMBPL2   ICM   RE,15,ACCUM         SAVE FINAL DEM VALUE IN (R5) BUFR            
         STCM  RE,15,5(R5)                                                      
         LR    R1,R2                                                            
*                                                                               
* GET NEXT COMBO                                                                
CMBNXTCB ZIC   R4,1(R1)            GET LENGTH OF COMBO                          
         AR    R1,R4               JUMP TO NEXT COMBO                           
         LA    R5,9(R5)            NEXT STATION                                 
         B     ALLCMBLP                                                         
*                                                                               
SORTTHEM ZIC   R4,NSTATS           GET THE AVERAGE                              
         LH    R5,COMBCNTR         INCLUDE THE COMBOS                           
         AR    R4,R5               NEW # OF SORT KEYS                           
         STC   R4,NSTATS                                                        
*                                                                               
SRTDEM2A L     R5,ASTATS                                                        
         CLI   DBSELSRC,C'M'                                                    
         BE    SORTDEM3                                                         
         LA    R5,9(R5)            SKIP MARKET STATION                          
SORTDEM3 GOTO1 XSORT,DMCB,(1,(R5)),(R4),9,4,5                                   
         CLC   NSTATS,MAXRANK                                                   
         BL    *+10                                                             
         MVC   NSTATS,MAXRANK                                                   
         XMOD1                                                                  
         SPACE 1                                                                
TDEMO    DC    XL4'FFFFFFFF'                                                    
DEM      DC    X'00'                                                            
ACCUM    DC    F'0'                                                             
PARM3    DC    F'0'                                                             
         SPACE 1                                                                
* *******************************************************************           
* CALLRDF -   CALL THE CALCRDF PROCEDURE TO CALCULATE THE RANDOM                
*        DUPLICATION FORM FOR CUMES AND READ IN IMPR IF TSL                     
*        INPUT:                                                                 
*              DUB - MASK TO CALL CALCRDF WITH                                  
*                                                                               
* *******************************************************************           
         SPACE 1                                                                
CALLRDF  NTR1                                                                   
         ZIC   R3,NDEMOS           SAVE AWAY REAL # DEMOS                       
         MVI   NDEMOS,1            ONLY RANK FOR ONE DEMO                       
         MVC   CMBACCUM+6(3),ACCUM+1 SUM OF CUMES                               
         MVC   BUFF+6(3),1+5(R5)   PASS CURRENT DEMO BY PUTTING IN BUFF         
         L     R2,PARM3                                                         
         LA    R4,BUFF                                                          
         GOTO1 =A(CALCRDF),DMCB,(R9),(RC),(R2),RR=RELO                          
*                                                                               
         MVC   ACCUM+1(3),CMBACCUM+6                                            
         STC   R3,NDEMOS           RESTORE REAL # DEMOS                         
*                                                                               
CALLRDX  B     XIT                                                              
*                                                                               
* *******************************************************************           
* AVERANK -                                                                     
* *******************************************************************           
AVERANK  NTR1                                                                   
         CLI   NTIMES,1                                                         
         BNH   XIT                                                              
         ZIC   R1,NTIMES                                                        
         SR    RE,RE                                                            
         ICM   RF,15,5(R5)                                                      
         SLDA  RE,1                                                             
         DR    RE,R1                                                            
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         STCM  RF,15,5(R5)         AVERAGED                                     
         B     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
* *********************************************************************         
* FILLBUFF -   ROUTINE TO FILL BUFFER FOR THIS STATION                          
*              INPUTS              R2=A(5-BYTE STATION)                         
*                                  R3=STATION NUMBER                            
*                                  R4=A(BUFFER FOR STATION)                     
*              OUTPUT              BUFFDEM(S)                                   
* *********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
FILLBUFF NMOD1 0,**FLBF**                                                       
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
         L     R5,12(R1)                                                        
         L     R9,16(R1)                                                        
         L     RC,20(R1)                                                        
*                                                                               
         XC    UNIVS,UNIVS                                                      
         XC    UNIVBK,UNIVBK                                                    
*                                                                               
         MVI   RNK,C'N'            TURN OFF RNK FLAG-CALL IS FRM FILBF          
         USING BUFFD,R4                                                         
         STC   R3,BUFFSTAT                                                      
         XC    BUFF,BUFF                                                        
         MVI   DBSELMED,C'R'                                                    
         USING STATENTD,R2                                                      
         XC    DBSELMK,DBSELMK                                                  
         LA    RE,DBRADIC                                                       
         ST    RE,DBEXTEND                                                      
         XC    0(128,RE),0(RE)                                                  
         USING DBRID,RE                                                         
         MVC   DBRID,=C'RADI'                                                   
         MVI   DBRCOPT,C'Y'                                                     
FILLB1   MVC   DBSELMK,MKTNUM                                                   
         MVC   DBSELSTA,ACTSTAT                                                 
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBAREC,AIO1                                                      
         SPACE 1                                                                
         LA    R2,BOOKS            SET UP FOR MULTIPLE BOOKS                    
         ZIC   R0,NBOOKS                                                        
         L     R5,MYWORK           STATION BUFFER                               
         OC    ONEDPT,ONEDPT                                                    
         BZ    *+10                                                             
         MVC   DBSELSTA,0(R5)      FIRST STATION FROM THE LIST                  
         SPACE 1                                                                
FILL2    MVC   DBSELBK,1(R2)                                                    
         MVC   DBBTYPE,3(R2)                                                    
         MVC   DBSELMK,MKTNUM                                                   
         XC    THISDEM,THISDEM     CLEAR BUFFER FOR STATION/BOOK                
         XC    THISWT,THISWT                                                    
         MVC   SVNSTATS,NSTATS                                                  
*                                                                               
         OC    ONEDPT,ONEDPT                                                    
         BZ    FILL3                                                            
         MVC   DBFILE,=C'RDP'                                                   
         MVC   DBSELDAY,ONEDPT                                                  
         MVC   DBSELPRG+1(1),ONEDPT+1                                           
         XC    DBSELTIM,DBSELTIM                                                
         B     FILL8                                                            
FILL3    L     RE,=A(DPTLIST)                                                   
         CLI   DEMOS+1,C'C'        CUME                                         
         BE    *+8                                                              
         CLI   DEMOS+1,C'F'        CUME RATING                                  
         BE    *+8                                                              
         CLI   DEMOS+1,C'E'        EXCLUSIVE CUME                               
         BE    *+8                                                              
         CLI   DEMOS+1,C'H'        EXCLUSIVE CUME RATING                        
         BE    *+8                                                              
         L     RE,=A(DPTLIST2)                                                  
         A     RE,RELO                                                          
         ST    RE,DPTLBASE                                                      
         ST    RE,DPTLPTR                                                       
         SPACE 1                                                                
         CLI   DAYPOPT,C'Y'        DAYPART OPTION                               
         BNE   FILL5                                                            
         L     RE,DPTLPTR                                                       
         XC    DBSELTIM,DBSELTIM                                                
         MVC   DBSELDAY,0(RE)                                                   
         XC    DBSELPRG,DBSELPRG                                                
         MVC   DBSELPRG+1(2),1(RE) DEIS: EXPLICIT LENGTH ADDED DEC/2018         
         MVC   DBFILE,=C'RDP'      READ TP FOR CUSTOM DAYPARTS                  
         B     FILL8                                                            
         SPACE 1                                                                
FILL5    MVI   DBSELDAY,X'04'      FRI WILL GET M-F(ALL WE NEED)                
FILL6    MVC   DBFILE,=C'TP '      READ TP FOR CUSTOM DAYPARTS                  
         XC    DBSELPRG,DBSELPRG                                                
         MVC   DBSELTIM(2),=H'0500'                                             
         MVC   DBSELTIM+2(2),=H'0445'                                           
         SPACE 1                                                                
FILL8    CLC   =C'CMB',0(R5)       COMBO?                                       
         BNE   FILL8A                                                           
         LA    RE,CMBACCUM                                                      
         ST    RE,ACMBACC                                                       
         CH    R0,=H'1'            ONLY NEED TO FILL ONCE (NOT AT EACH          
         BNE   *+8                 ITERANCE OF THIS R0-BOOK LOOP)               
         BAS   RE,FILLCMB                                                       
         MVC   DBSELBK,1(R2)       RESTORE CHANGED BOOKS                        
         MVC   DBBTYPE,3(R2)                                                    
         B     FILL8B                                                           
*                                                                               
FILL8A   GOTO1 DEMAND,DMCB,DBLOCK,DEMHOOK                                       
*                                                                               
FILL8B   OC    ONEDPT,ONEDPT                                                    
         BNZ   FILL11                                                           
         CLI   DAYPOPT,C'Y'                                                     
         BE    FILL10                                                           
         CLI   DBSELDAY,X'01'      SUNDAY                                       
         BE    FILL9                                                            
         CLI   DBSELDAY,X'04'      FRI - DO SAT NEXT                            
         BNE   *+12                 NO - JUST DO SUNDAY                         
         MVI   DBSELDAY,X'02'                                                   
         B     FILL8                                                            
         MVI   DBSELDAY,X'01'                                                   
         B     FILL8                                                            
         SPACE 1                                                                
FILL9    LA    R2,4(R2)                                                         
         BCT   R0,FILL2                                                         
         XMOD1                                                                  
*                                                                               
         SPACE 1                                                                
FILL10   L     RE,DPTLPTR          GET THE NEXT DAYPART                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BE    FILL9                                                            
         ST    RE,DPTLPTR                                                       
         MVC   DBSELDAY,0(RE)                                                   
         MVC   DBSELPRG+1(2),1(RE) DEIS: EXPLICIT LENGTH ADDED DEC/2018         
         XC    DBSELTIM,DBSELTIM                                                
         B     FILL8                                                            
         SPACE 1                                                                
FILL11   ZIC   R1,SVNSTATS                                                      
         BCTR  R1,0                                                             
         STC   R1,SVNSTATS                                                      
         LTR   R1,R1                                                            
         BZ    FILL12                                                           
         LA    R5,9(R5)                                                         
         MVC   DBSELSTA,0(R5)                                                   
         B     FILL8                                                            
         SPACE 1                                                                
FILL12   L     R5,MYWORK           RESET STATION LIST                           
         MVC   DBSELSTA,0(R5)                                                   
         B     FILL9                                                            
         DROP  R2                                                               
         EJECT                                                                  
* ********************************************************************          
* FILLCMB -    FILL AND DO COMBO CALCULATIONS                                   
* ********************************************************************          
*                                                                               
FILLCMB  NTR1                                                                   
         CLI   DAYPOPT,C'Y'        DAYPART OUTPUT?                              
         BNE   FILCMB1                                                          
         LA    R4,BUFF                                                          
         L     R1,DPTLPTR                                                       
         ZIC   RE,2(R1)                                                         
         LA    RF,L'TDENT                                                       
         MR    RE,RE                                                            
         AR    R4,RF                                                            
         ST    R4,ADPTBUF          SAVE ADR OF DISPL INTO BUFF                  
         XC    5(L'TDENT-5,R4),5(R4)                                            
*                                                                               
FILCMB1  BAS   RE,BUFFPLC          GET BACK COMBOS BUFF PLACE (R4)              
         XC    5(L'TDENT-5,R4),5(R4)                                            
         XC    CMBACCUM(L'TDENT),CMBACCUM                                       
         ZIC   R7,3(R5)            GET COMBO #                                  
         N     R7,=X'0000000F'                                                  
         L     R8,ULPNTR                                                        
*                                                                               
         MVI   CNOSTA,0            COUNT # STATIONS IN COMBO                    
*                                                                               
MORECMBS CLI   0(R8),3             IS IT A MARKET                               
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
*                                                                               
FIRSTCMB ZIC   R6,1(R8)                                                         
         AR    R6,R8                                                            
         ST    R6,NEXTCOMB                                                      
         LA    R6,11(R8)           FIRST PIECE IN COMBO                         
*                                                                               
*SORT STATIONS FOR CONSISTENCY IN RDF/TSL CALCULATIONS                          
         SR    R2,R2               R2= # STATIONS IN COMBO                      
FIRSTCM5 LA    R2,1(R2)            BUMP STATN COUNTER                           
         LA    R6,5(R6)            NEXT STATION                                 
         C     R6,NEXTCOMB         FINISHED W/STATN LIST?                       
         BL    FIRSTCM5                                                         
*                                                                               
         LA    R6,11(R8)           R6=FIRST STATION IN COMBO                    
         GOTO1 XSORT,DMCB,(R6),(R2),5,5,0                                       
*                                                                               
         MVC   DBSELSTA,0(R6)                                                   
*                                                                               
FLCMB70  LA    R2,BOOKS                                                         
         ZIC   R0,NBOOKS                                                        
         MVC   TMPDEM,DEMOS+1      SAVE DEMO CATEGORY                           
*                                                                               
FLCMB80  MVC   DBSELBK,1(R2)                                                    
         MVC   DBBTYPE,3(R2)                                                    
         CLI   DEMOS+1,C'X'        TSL?                                         
         BNE   FLCMB82                                                          
         LA    RE,DEMOS            SUBSITUTE TSL FOR IMPS                       
FLCMB81  MVI   1(RE),C'C'          SET TO READ IMPRESSIONS                      
         LA    RE,3(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   FLCMB81                                                          
*                                                                               
FLCMB82  GOTO1 DEMAND,DMCB,DBLOCK,DEMHOOK                                       
         LA    R2,4(R2)                                                         
         BCT   R0,FLCMB80                                                       
*                                                                               
         CLI   DAYPOPT,C'Y'                                                     
         BNE   FILCMB5                                                          
         CLI   DEMOS+1,C'C'        FOR DAY PART CUMES ONLY                      
         BNE   FILCMB5                                                          
         L     R4,ADPTBUF          PT TO DEMOS WE JUST READ IN                  
         B     *+8                                                              
FILCMB5  BAS   RE,BUFFPLC          ELSE PT TO TOP OF BUFFER                     
*                                                                               
         BAS   RE,AVECMB                                                        
         BAS   RE,ADDCMB                                                        
*                                                                               
         LA    RE,DEMOS            RESTORE REAL DEMO TYPE                       
FLCMB7   MVC   1(1,RE),TMPDEM        SET TO READ IMPRESSIONS                    
         LA    RE,3(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   FLCMB7                                                           
*                                                                               
         CLI   DEMOS+1,C'C'        CUME ?                                       
         BE    *+12                                                             
         CLI   DEMOS+1,C'X'        TSL  ?                                       
         BNE   FLCMB85                                                          
         GOTO1 =A(CALCRDF),DMCB,(R9),(RC),0,RR=RELO                             
*                                                                               
FLCMB85  XC    5(L'TDENT-5,R4),5(R4)  IF DAYPT=Y, CLR DISPL STRG TOO            
         BAS   RE,BUFFPLC          PT TO TOP OF BUFFER                          
         XC    5(L'TDENT-5,R4),5(R4)  CLR BUFFER FOR NEXT STATN DEMOS           
         LA    R6,5(R6)            NEXT STATION IN COMBO                        
         C     R6,NEXTCOMB         FINISHED COMBO?                              
         BNL   CHNGBUFF                                                         
         MVC   DBSELSTA,0(R6)                                                   
         B     FLCMB70             LOOP THRU ALL STATIONS IN COMBO              
*                                                                               
CHNGBUFF CLI   DAYPOPT,C'Y'                                                     
         BNE   CHGBF2                                                           
         CLI   DEMOS+1,C'X'        FOR DAY PART CUMES ONLY                      
         BE    *+12                                                             
         CLI   DEMOS+1,C'C'        FOR DAY PART CUMES ONLY                      
         BNE   CHGBF2                                                           
         L     R4,ADPTBUF          PT TO DEMOS WE JUST READ IN                  
*                                                                               
CHGBF2   CLI   DEMOS+1,C'X'        TSL? FINAL CALC--SET DMCB+8=1                
         BNE   CHGBF5                                                           
         GOTO1 =A(CALCRDF),DMCB,(R9),(RC),RDFTSLQ,RR=RELO                       
*                                                                               
CHGBF5   MVC   1(4,R4),0(R5)                                                    
         MVC   6(L'TDENT-6,R4),CMBACCUM+6                                       
*                                                                               
FILCMBX  B     XIT                                                              
         SPACE 3                                                                
*                                                                               
ADPTBUF  DC    F'00'               ADDR WHERE DAYPT DEMO READ INTO              
TMPDEM   DC    XL1'00'             HOLDS THE REAL CATEGORY TYPE                 
CMBACCUM DC    XL64'00'            COMBO ACCUM BUFFER                           
*                                                                               
         EJECT                                                                  
* *****************************************************************             
* BUFFPLC -    GET BACK COMBOS BUFFER PLACE  (R4) PTS TO BUFFER                 
* *****************************************************************             
BUFFPLC  NTR1                                                                   
         LR    RE,R5               GET BACK COMBOS BUFF PLACE                   
         S     RE,MYWORK                                                        
         SRDL  RE,32                                                            
         D     RE,=F'9'                                                         
         LR    RE,RF                                                            
         LA    R4,BUFF                                                          
         LA    RF,L'TDENT                                                       
         MR    RE,RE                                                            
         AR    R4,RF                                                            
         XIT1  REGS=(R4)                                                        
*                                                                               
         SPACE 3                                                                
* *****************************************************************             
* AVECMB-      CALCULATE AVERAGE OF DEMO OVER MULTI BOOKS                       
* *****************************************************************             
AVECMB   NTR1                                                                   
         CLI   5(R4),2                                                          
         BL    XIT                                                              
         ZIC   R1,NDEMOS                                                        
         ZIC   R2,5(R4)                                                         
         LA    R3,6(R4)                                                         
AVECMB10 SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,7,0(R3)                                                       
         SLDA  RE,1                                                             
         DR    RE,R2                                                            
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         STCM  RF,7,0(R3)                                                       
         LA    R3,3(R3)                                                         
         BCT   R1,AVECMB10                                                      
         B     XIT                                                              
*                                                                               
* *******************************************************************           
* ADDCMB-      ADD THIS STATION'S DEMO VALUES TO THE OTHERS IN COMBO            
* *******************************************************************           
ADDCMB   NTR1                                                                   
         ZIC   R1,NDEMOS                                                        
         LA    R2,6(R4)                                                         
         LA    R3,CMBACCUM+6                                                    
ADDCMB10 SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,7,0(R3)                                                       
         ICM   RE,7,0(R2)                                                       
         AR    RF,RE                                                            
         STCM  RF,7,0(R3)                                                       
         LA    R3,3(R3)                                                         
         LA    R2,3(R2)                                                         
         BCT   R1,ADDCMB10                                                      
         B     XIT                                                              
         EJECT                                                                  
* ********************************************************************          
* FILLTOTS -   ROUTINE TO FILL TOTALS FOR THIS STATION                          
* ********************************************************************          
         SPACE 2                                                                
         DS    0H                                                               
FILLTOTS NMOD1 0,**FLTS**                                                       
         L     R3,0(R1)            REAL BUFFER                                  
         L     R4,4(R1)            TOTALS BUFFER                                
         L     R9,8(R1)                                                         
         L     RC,12(R1)                                                        
*                                                                               
         XC    0(80,R4),0(R4)      CLEAR TOTALS BUFFER                          
         USING BUFFD,R4                                                         
*                                                                               
         MVI   DBSELMED,C'R'                                                    
         LA    RE,DBRADIC                                                       
         ST    RE,DBEXTEND                                                      
         XC    0(128,RE),0(RE)                                                  
         USING DBRID,RE                                                         
         MVC   DBRID,=C'RADI'                                                   
         MVI   DBRCOPT,C'Y'                                                     
FILLT1   MVC   DBSELMK,MKTNUM                                                   
*                                                                               
         LA    RF,DBSELSTA                                                      
         MVI   4(RF),C'A'                                                       
         EDIT  (2,MKTNUM),(4,(RF))                                              
*                                                                               
         LA    R2,4                                                             
FILLT1A  CLI   0(RF),C' '                                                       
         BNE   FILLT1B                                                          
         MVI   0(RF),C'0'                                                       
FILLT1B  LA    RF,1(RF)                                                         
         BCT   R2,FILLT1A                                                       
*                                                                               
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBAREC,AIO1                                                      
         SPACE 1                                                                
         LA    R2,BOOKS            SET UP FOR MULTIPLE BOOKS                    
         ZIC   R0,NBOOKS                                                        
         L     R5,MYWORK           STATION BUFFER                               
         SPACE 1                                                                
FILLT2   MVC   DBSELBK,1(R2)                                                    
         MVC   DBBTYPE,3(R2)                                                    
         MVC   DBSELMK,MKTNUM                                                   
         XC    THISDEM,THISDEM     CLEAR BUFFER FOR STATION/BOOK                
         XC    THISWT,THISWT                                                    
         MVC   SVNSTATS,NSTATS                                                  
*                                                                               
* DAYPOPT=C'Y' WHEN IN HERE                                                     
         L     RE,DPTLPTR                                                       
         XC    DBSELTIM,DBSELTIM                                                
         MVC   DBSELDAY,0(RE)                                                   
         XC    DBSELPRG,DBSELPRG                                                
         MVC   DBSELPRG+1(2),1(RE) DEIS: EXPLICIT LENGTH ADDED DEC/2018         
         MVC   DBFILE,=C'RDP'      READ TP FOR CUSTOM DAYPARTS                  
         GOTO1 DEMAND,DMCB,DBLOCK,DEMHOOKT                                      
         SPACE 1                                                                
FILLT9   LA    R2,4(R2)                                                         
         BCT   R0,FILLT2                                                        
*                                                                               
         LA    R2,20                                                            
FILLT20  L     RF,0(R4)                                                         
         SR    RE,RE                                                            
         CLI   NBOOKS,1                                                         
         BE    FILLT25                                                          
         M     RE,=F'10'                                                        
         ZIC   R3,NBOOKS           AVERAGE TOTALS FOR # OF BOOKS                
         DR    RE,R3                                                            
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         ST    RF,0(R4)                                                         
FILLT25  LA    R4,4(R4)                                                         
         BCT   R2,FILLT20                                                       
         XMOD1                                                                  
         SPACE 1                                                                
         EJECT                                                                  
DEMHOOKT NTR1                      FOR TOTALS                                   
*                                  ADD DEMOS TO THISBUFF                        
         OC    DBFACTOR,DBFACTOR   ANYTHING ACTIVE?                             
         BZ    DEMTXIT                                                          
         L     RE,DBAREC                                                        
         USING DRKEY,RE                                                         
         L     R1,DPTLPTR                                                       
         ZIC   R6,2(R1)                                                         
*                                                                               
DMHKT01  LA    RE,23(RE)                                                        
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
DMTHOOK0 ZIC   R1,NDEMOS                                                        
         LA    RE,THISLINE                                                      
DMTHOOK1 SR    R2,R2                                                            
         L     R2,0(R4)            GET PREVIOUS TOTAL                           
         A     R2,0(RE)            ADD TO CURRENT TOTAL                         
         ST    R2,0(R4)            SAVE IT                                      
         LA    RE,4(RE)                                                         
         LA    R4,4(R4)                                                         
         BCT   R1,DMTHOOK1                                                      
         SPACE 1                                                                
         CLI   TRACEOPT,C'T'       OPTION TO TRACE                              
         BNE   DEMTXIT                                                          
         MVC   P(5),DBSELSTA                                                    
         LA    R2,P+6                                                           
         GOTO1 DEFINE,DMCB,=C'DAY',DBLOCK,WORK                                  
         MVC   0(4,R2),WORK+2                                                   
         LA    R2,5(R2)                                                         
         GOTO1 DEFINE,DMCB,=C'TIME',DBLOCK,WORK                                 
         GOTO1 UNTIME,DMCB,WORK+2,(R2)                                          
         LA    R2,12(R2)                                                        
         L     R6,6(R3)                                                         
         EDIT  (R6),(5,(R2))                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,P+6                                                           
         ZIC   R3,NDEMOS                                                        
         LA    R5,THISLINE                                                      
DEMT10   L     R6,0(R5)                                                         
         EDIT  (R6),(5,(R2))                                                    
         LA    R2,7(R2)                                                         
         LA    R5,4(R5)                                                         
         BCT   R3,DEMT10                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
DEMTXIT  XIT1                                                                   
         EJECT                                                                  
         SPACE 1                                                                
DBRADIC  DC    CL128' '                                                         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* *********************************************************************         
* CALCRDF -    DO CALCULATIONS FOR RANDOM DUPLICATION FORMULA                   
*              PARM3 - X'01' BIT = FINAL TSL CALC INDICATOR                     
*                    - X'02' BIT = CALL ORIGINATED FROM RANKING                 
RDFTSLQ  EQU   X'01'                                                            
RDFRNKQ  EQU   X'02'                                                            
*                                                                               
*              PARM4 - 0=FILLCMB ORIGIN   1=RANK ORIGIN                         
* *********************************************************************         
*                                                                               
         DS    0H                                                               
CALCRDF  NMOD1 0,**CALCRDF**                                                    
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         TM    8+3(R1),RDFRNKQ     RANKING REQUEST?                             
         BNO   CLCR00                                                           
         LA    R4,BUFF             DEMOS ARE IN BUFF                            
*                                                                               
CLCR00   TM    8+3(R1),RDFTSLQ     FINAL TSL CALCULATION?                       
         BNO   CLCRDF1                                                          
         BAS   RE,TSLCALC                                                       
         B     CLCRDFX                                                          
*                                                                               
CLCRDF1  CLI   CNOSTA,0            AT BEGINING OF THIS COMBO?                   
         BNE   CLCRDF5             NO                                           
*                                                                               
         XC    IMPACCUM,IMPACCUM   INITIALIZE IMPR BUFFER TO 0                  
         XC    CUMERAT,CUMERAT     INIT CUMERTG BUFFER TO 1 FOR MULTP           
         LA    R1,CUMERAT                                                       
         LA    RE,19               19 BUCKETS                                   
         MVI   3(R1),1             INIT CUMERTG TO 1                            
         LA    R1,4(R1)            NEXT CUMERTG BUCKET                          
         BCT   RE,*-8                                                           
*                                                                               
CLCRDF5  BAS   RE,CUMERTG          CALC CUME RATING                             
         CLI   CNOSTA,2            IF 2 OR MORE STATNS, CALC NEWCUME            
         BL    HERE                                                             
         BAS   RE,NEWCUME          APPLY RAND DUP FORMULA ON CUME               
*                                                                               
HERE     CLI   DEMOS+1,C'X'        TSL ?                                        
         BNE   *+8                                                              
         BAS   RE,GETIMP           READ IMPRESSIONS INTO BUFFER                 
*                                                                               
         B     CLCRDFX                                                          
*                                                                               
CLCRDFX  XMOD1                                                                  
         SPACE 3                                                                
*                                                                               
IMPACCUM DC    XL(L'TDENT)'00'     HOLDS IMPRESSIONS FOR TSL                    
CUMERAT  DC    XL(4*19)'00'        HOLD CUME RATINGS FOR RDF CALC               
AVGUNIV  DC    F'140247'           AVG UNIVERSE FOR ALL BOOKS                   
AUNIV    DC    F'00'               A(UNIVS)                                     
TMPADR   DC    F'00'                                                            
TMPBUF   DC    XL(L'TDENT)'00'                                                  
*                                                                               
* ********************************************************************          
* CUMERTG -    CALCULATE THE CUME RATING FOR EACH DEMO                          
*              (R4) - PT TO CUMES JUST READ IN                                  
* ********************************************************************          
*                                                                               
CUMERTG  NTR1                                                                   
*                                                                               
         LA    RF,UNIVS                                                         
         ST    RF,AUNIV            SAVE ADDR OF UNIV FOR EA DEMO                
         ZIC   R1,CNOSTA           INCR # STATIONS IN COMBO                     
         LA    R1,1(R1)                                                         
         STC   R1,CNOSTA                                                        
*                                                                               
         LA    R4,6(R4)            CUME FOR THIS DEMO                           
         LA    R6,CUMERAT          CUME RATINGS                                 
         ZIC   R5,NDEMOS           NUMBER DEMOS IN LIST                         
*                                                                               
CUMERG1  L     RE,AUNIV            PT TO UNIV FOR THIS DEMO                     
         ZICM  RF,0(RE),(7)        RF = SUM OF UNIVERSES                        
         SR    RE,RE                                                            
         ZIC   R2,NBOOKS           R2 = # BOOKS                                 
         SLDA  RE,1                                                             
         DR    RE,R2                                                            
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         ST    RF,AVGUNIV                                                       
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
CUMERG8  LA    R4,3(R4)            NEXT DEMO IN BUFFER                          
         LA    R6,4(R6)            NEXT CUME RTG SAVE AREA                      
         L     RE,AUNIV                                                         
         LA    RE,3(RE)            NEXT UNIVERSE                                
         ST    RE,AUNIV                                                         
         BCT   R5,CUMERG1          DO ALL DEMOS                                 
*                                                                               
CUMERTGX XIT1                                                                   
         EJECT                                                                  
* ********************************************************************          
* GETIMP  -    READ IN IMPRESSIONS FOR TSL CALCULATION                          
* ********************************************************************          
*                                                                               
GETIMP   NTR1                                                                   
         CLI   RNK,C'Y'            IF RANKING, HANDLE BUFFERS DIFFRNTLY         
         BE    GTIMPRNK                                                         
*                                                                               
         LA    RE,DEMOS            SUBSITUTE TSL FOR IMPS                       
GETIMP2  MVI   1(RE),C'I'          SET TO READ IMPRESSIONS                      
         LA    RE,3(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   GETIMP2                                                          
*                                                                               
         MVC   TMPBUF,0(R4)        SAVE CUMES                                   
         XC    0(L'TDENT,R4),0(R4)   CLEAR BUFFER TO READ IN IMPS               
         ST    R4,TMPADR           SAVE ADDR OF WHERE DATA REALLY IS            
*                                                                               
         LR    RE,R5               PT TO TOP OF COMBOS BUFFER                   
         S     RE,MYWORK                                                        
         SRDL  RE,32                                                            
         D     RE,=F'9'                                                         
         LR    RE,RF                                                            
         LA    R4,BUFF                                                          
         LA    RF,L'TDENT                                                       
         MR    RE,RE                                                            
         AR    R4,RF                                                            
*                                                                               
GTIMP5   LA    R2,BOOKS            READ IN IMPRESSIONS FOR ALL BOOKS            
         ZIC   R0,NBOOKS                                                        
*                                                                               
GTIMP10  MVC   DBSELBK,1(R2)       GET SUM OF IMP OVER BOOKS                    
         MVC   DBBTYPE,3(R2)                                                    
         GOTO1 DEMAND,DMCB,DBLOCK,DEMHOOK                                       
         LA    R2,4(R2)                                                         
         BCT   R0,GTIMP10                                                       
*                                                                               
         CLI   DAYPOPT,C'Y'        IF DAYPT OPTN PT TO  POSN IN BUFF            
         BNE   *+8                                                              
         L     R4,TMPADR           PT TO IMP WE JUST READ IN                    
*                                                                               
         BAS   RE,IMPCMB           GET AVG & SUM OF IMPS                        
         MVC   0(L'TDENT,R4),TMPBUF  RESTORE ORIG BUFF DATA (CUMES)             
*                                                                               
         LA    RE,DEMOS            SUBSITUTE TSL FOR IMPS                       
GETIM15  MVI   1(RE),C'X'          SET TO READ TSL AGAIN                        
         LA    RE,3(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   GETIM15                                                          
*                                                                               
         B     GETIMPX                                                          
         SPACE 3                                                                
*                                                                               
GTIMPRNK DS    0H                  GET IMPRESSIONS FOR RANKING ON TSL           
         MVI   RDEMO+1,C'I'        SET TO READ IMPRESSIONS                      
*                                                                               
GTRNK5   LA    R2,BOOKS            READ IN IMPRESSIONS FOR ALL BOOKS            
         ZIC   R0,NBOOKS                                                        
         XC    TMPBUF,TMPBUF                                                    
         LA    R5,TMPBUF           READ IMPRESSION INTO TMP BUFF                
*                                                                               
* NOTE: DEMHOOKR SAVES A FULL WORD AT 5(R5). THIS WORKS GREAT SINCE             
*       THE IMPCMB IS EXPECTING 3BYTES AT 6(R5)-WHICH IS THE SAME THING         
*                                                                               
GTRNK10  MVC   DBSELBK,1(R2)                                                    
         MVC   DBBTYPE,3(R2)                                                    
         GOTO1 DEMAND,DMCB,DBLOCK,DEMHOOKR                                      
         LA    R2,4(R2)                                                         
         BCT   R0,GTRNK10                                                       
*                                                                               
         LA    R4,TMPBUF          PT TO THE IMPS JUST READ IN                   
         BAS   RE,IMPCMB                                                        
         MVI   RDEMO+1,C'X'       RESTORE REAL DEMO TYPE                        
*                                                                               
GETIMPX  XIT1                                                                   
*                                                                               
         EJECT                                                                  
* *********************************************************************         
* IMPCMB -     CALCULATE AVG AND SUM OF IMPRESSIONS OVER STATNS IN CMBO         
* *********************************************************************         
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
         LA    R3,3(R3)            NEXT SUM OF IMPS BUCKET                      
         LA    R4,3(R4)            NEXT IMP DEMO                                
         BCT   R1,IMPCMB2                                                       
*                                                                               
IMPCMBX  XIT1                                                                   
*                                                                               
* *******************************************************************           
* ADDCMB-      ADD THIS STATION'S DEMO VALUES TO THE OTHERS IN COMBO            
* *******************************************************************           
ADDCMB2  NTR1                                                                   
         ZIC   R1,NDEMOS                                                        
         LA    R2,6(R4)                                                         
         LA    R3,IMPACCUM+6                                                    
ADCMB210 SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,7,0(R3)                                                       
         ICM   RE,7,0(R2)                                                       
         AR    RF,RE                                                            
         STCM  RF,7,0(R3)                                                       
         LA    R3,3(R3)                                                         
         LA    R2,3(R2)                                                         
         BCT   R1,ADCMB210                                                      
         XIT1                                                                   
         EJECT                                                                  
* ********************************************************************          
* NEWCUME -  USE RANDOM DUPLICATION FORMULA TO CALCULATE NEW CUME               
*            RDF: CUME =(CUME1+CUME2) - (CUME1/UNIV * CUME2/UNIV *UNIV)         
*                      =CUME1+CUME2   - CUME1RATING * CUME2RATIG *UNIV)         
* ********************************************************************          
NEWCUME  NTR1                                                                   
         L     RF,ACMBACC          GET ADDR OF COMBO BUFFER                     
         LA    RF,6(RF)            SUM OF CUMES                                 
         LA    R4,6(R4)            THIS DEMO'S CUMES                            
         LA    R6,CUMERAT          CUME RATINGS                                 
         ZIC   R7,NDEMOS           NUMBER DEMOS IN BUFFER                       
         LA    RE,UNIVS                                                         
         ST    RE,AUNIV                                                         
*                                                                               
NEWCUM1  ST    RF,DMCB             SAVE ADDR OF CUMES FOR A MOMENT              
         L     RE,AUNIV            PT TO UNIV FOR THIS DEMO                     
         ZICM  RF,0(RE),(7)        RF = SUM OF UNIVERSES                        
         SR    RE,RE                                                            
         ZIC   R2,NBOOKS           R2 = # BOOKS                                 
         SLDA  RE,1                                                             
         DR    RE,R2               CALCULATE AVG UNIV FOR DEMO                  
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         ST    RF,AVGUNIV                                                       
         L     RF,DMCB             RESTORE PTR TO SUM OF CUMES                  
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
NEWCUM8  LA    RF,3(RF)            NXT CMBACCUM (SUM OF CUMES FOR DEMO)         
         LA    R4,3(R4)            NEXT CUME FOR THE NEXT DEMO                  
         LA    R6,4(R6)            NEXT DEMO'S CUME RATINGS                     
         L     RE,AUNIV            BUMP UNIVERSE PTR                            
         LA    RE,3(RE)            UNIV FOR NEXT DEMO                           
         ST    RE,AUNIV                                                         
         BCT   R7,NEWCUM1          GO THRU ALL DEMOS IN BUFFER                  
*                                                                               
NEWCUMEX XIT1                                                                   
         EJECT                                                                  
* ********************************************************************          
* TSLCALC -    READ IN IMPRESSIONS FOR TSL CALCULATION                          
* ********************************************************************          
         SPACE 1                                                                
TSLCALC  NTR1                                                                   
         BAS   RE,QTRHRSCT         COUNT # OF QTR HOURS IN DAYPART              
         ZIC   R2,NDEMOS           LOOP THRU ALL DEMOS IN BUFFER                
         L     RE,ACMBACC          PT TO CUMES IN COMBO BUFFER                  
         LA    RE,6(RE)            FIRST CUME                                   
         LA    RF,IMPACCUM+6       PT TO IMPRESSIONS                            
*                                                                               
TSLCLC03 SR    R4,R4               GO THRU EACH DEMO                            
         ZICM  R5,0(RF),(7)                                                     
         M     R4,=F'100'                                                       
         M     R4,NOQTRHRS         MULT BY # OF QTR HOURS                       
         OC    0(4,RE),0(RE)       CAN'T DIVIDE BY ZERO                         
         BZ    TSLCLC04            KEEP IT AS ZERO                              
         ZICM  R0,0(RE),(7)                                                     
         DR    R4,R0               DIVIDE BY CUME                               
         A     R5,=F'5'                                                         
         SR    R4,R4                                                            
         D     R4,=F'10'                                                        
         STCM  R5,7,0(RE)          REPLACE IT WITH TSL                          
*                                                                               
TSLCLC04 LA    RE,3(RE)            NEXT DEMO                                    
         LA    RF,3(RF)                                                         
         BCT   R2,TSLCLC03                                                      
         XIT1                                                                   
*                                                                               
TMPWRD   DC    F'01'                                                            
NOQTRHRS DC    F'80'                                                            
*------------------------------------------------------------------             
QTRHRSCT NTR1                                                                   
*                                                                               
         LA    R4,DPENTRY+1        DAYPOPT=N, DYPT IS IN DPENTRY                
         XC    NOQTRHRS,NOQTRHRS                                                
*                                                                               
         CLI   DAYPOPT,C'Y'        DAYPOPT=Y, DAYS COME FROM DPTLPTR            
         BNE   QHCUST1                                                          
*                                                                               
         L     R4,DPTLPTR                                                       
         L     RE,=A(STANDPTS)     PT TO STANDARD DAYPT TABLE                   
         A     RE,RELO                                                          
         USING STAND,RE                                                         
QHDAYSCH CLI   STANDDTS,X'FF'      END OF TABLE?                                
         BE    XIT                                                              
         CLC   0(1,R4),STANDDTS    CHECKING DAY RANGE                           
         BNE   QHDAYS2                                                          
         CLC   1(1,R4),STANDPRG                                                 
         BE    QHFNDDPT                                                         
QHDAYS2  LA    RE,L'STANDENT(RE)                                                
         B     QHDAYSCH                                                         
QHFNDDPT LR    R4,RE                                                            
*                                                                               
QHCUST1  CLI   0(R4),0             FINISHED?                                    
         BZ    QTRX                                                             
         ZIC   R1,2(R4)            END HOUR                                     
         ZIC   R2,1(R4)            START HOUR                                   
         SR    R1,R2                                                            
         MH    R1,=H'4'            4 QTR HOURS PER HOUR                         
         SR    RF,RF                                                            
         ZIC   R2,0(R4)                                                         
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
         LA    R4,3(R4)                                                         
         B     QHCUST1                                                          
*                                                                               
QTRX     XIT1                                                                   
         DROP  RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
* ********************************************************************          
* SHRBUFF -                                                                     
* ********************************************************************          
*                                                                               
SHRBUFF  NMOD1 0,**SHRBUFF**                                                    
         L     R4,0(R1)                                                         
         L     R6,4(R1)            HOLDS BUFFER OF TOTALS                       
         L     R2,8(R1)                                                         
         L     R9,12(R1)                                                        
         L     RC,16(R1)                                                        
*                                                                               
         LA    R4,6(R4)            POINT TO FIRST ONE                           
SHRB1    ZIC   R3,NDEMOS                                                        
SHRB2    CLI   1(R2),C'S'          IS IT A SHR?                                 
         BNE   SHRB10                                                           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,7,0(R4)          GET IMPRESSION                               
SHRB3    M     RE,=F'10000'                                                     
         L     R8,0(R6)                                                         
SHRB4    LTR   R8,R8                                                            
         BNZ   SHRB5                                                            
         SR    RF,RF                                                            
         B     SHRB8                                                            
SHRB5    DR    RE,R8                                                            
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
SHRB8    STCM  RF,7,0(R4)                                                       
SHRB10   LA    R2,3(R2)                                                         
         LA    R4,3(R4)                                                         
         LA    R6,4(R6)                                                         
         BCT   R3,SHRB2                                                         
SHRBUFFX XMOD1                                                                  
         SPACE 3                                                                
TDEMOS   DC    F'0'                                                             
         LTORG                                                                  
         EJECT                                                                  
IMPRESS  NMOD1 0,**IMPRESS**                                                    
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         CLI   CALCOPT,C'Y'                                                     
         BNE   IMPXIT                                                           
         ZIC   R1,NBOOKS           GET AVERAGE OF UNIVS                         
         LA    R4,UNIVS                                                         
         ZIC   R0,NDEMOS                                                        
IMPUNIV2 SR    RE,RE                                                            
         ICM   RE,7,0(R4)                                                       
         SRDA  RE,32                                                            
         SLDA  RE,1                                                             
         DR    RE,R1                                                            
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         STCM  RF,7,0(R4)                                                       
         LA    R4,3(R4)                                                         
         BCT   R0,IMPUNIV2                                                      
*                                                                               
         CLI   MAINDEM,C'F'                                                     
         BE    IMPAVE                                                           
         CLI   MAINDEM,C'G'                                                     
         BE    IMPAVE                                                           
         CLI   MAINDEM,C'H'                                                     
         BE    IMPAVE                                                           
         CLI   MAINDEM,C'P'                                                     
         BE    IMPAVE                                                           
         CLI   MAINDEM,C'R'                                                     
         BE    IMPAVE                                                           
         CLI   MAINDEM,C'S'                                                     
         BNE   IMPXIT                                                           
         CLI   DBSELSRC,C'M'       CANADA DON'T HAVE TOTALS                     
         BE    IMPXIT              DIVIDING IS NOT VALID                        
         CLI   DAYPOPT,C'Y'        DAYPARTS HAVE DIFFERENT FORMULA              
         BE    IMPXIT                HAVE TO LEAVE                              
*                                                                               
IMPAVE   LA    R4,BUFF                                                          
         USING TDD,R4                                                           
         LA    R4,L'TDENT(R4)      SKIP MARKET STATION                          
         LA    R0,24                                                            
         OC    ONEDPT,ONEDPT                                                    
         BE    IMPAVE2                                                          
         ZIC   R0,NSTATS                                                        
IMPAVE2  LR    R5,R4                                                            
         LA    R1,UNIVS                                                         
         CLI   MAINDEM,C'S'                                                     
         BNE   IMPAVE3                                                          
         LA    R1,BUFF                                                          
IMPAVE3  ZIC   R8,NDEMOS           LOOP FOR NUMBER OF DEMOS                     
IMPAVE4  SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,7,TDVLMF                                                      
         M     RE,=F'10000'                                                     
         SR    R3,R3                                                            
         ICM   R3,7,6(R1)                                                       
         CLI   MAINDEM,C'S'                                                     
         BE    IMPAVE40                                                         
         ICM   R3,7,0(R1)                                                       
IMPAVE40 LTR   R3,R3                                                            
         BNZ   IMPAVE41                                                         
         SR    RF,RF                                                            
         B     IMPAVE42                                                         
IMPAVE41 SR    RE,RE                                                            
         DR    RE,R3                                                            
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
IMPAVE42 STCM  RF,7,TDVLMF                                                      
IMPAVE4A LA    R4,3(R4)                                                         
         LA    R1,3(R1)            NEXT UNIVERSE                                
         BCT   R8,IMPAVE4                                                       
         LR    R4,R5                                                            
IMPAVE6  LA    R4,L'TDENT(R4)                                                   
         BCT   R0,IMPAVE2                                                       
IMPXIT   XMOD1                                                                  
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         DS    0H                                                               
EXPSTATS NMOD1 0,**EXPSTATS**                                                   
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         XC    PREVSTA,PREVSTA                                                  
         MVC   ATHISTAT,ASTATS                                                  
         MVI   NSTATS,0                                                         
         CLI   DBSELSRC,C'M'                                                    
         BE    EXPSTA1C                                                         
*                                                                               
         L     RF,ATHISTAT                                                      
         MVI   4(RF),C'A'                                                       
         EDIT  (2,MKTNUM),(4,(RF))                                              
*                                                                               
         LA    R2,4                                                             
         LR    R3,RF                                                            
EXPSTA1A CLI   0(R3),C' '                                                       
         BNE   EXPSTA1B                                                         
         MVI   0(R3),C'0'                                                       
EXPSTA1B LA    R3,1(R3)                                                         
         BCT   R2,EXPSTA1A                                                      
         LA    RF,9(RF)                                                         
         ST    RF,ATHISTAT                                                      
*                                                                               
         MVI   NSTATS,1                                                         
         CLI   MAXRANK,30                                                       
         BNL   EXPSTA1C                                                         
         ZIC   RF,MAXRANK                                                       
         LA    RF,1(RF)                                                         
         STC   RF,MAXRANK                                                       
         SPACE 1                                                                
EXPSTA1C MVC   DBAREC,AIO1                                                      
         MVI   DBFUNCT,DBGETMS                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'R'                                                    
*                                                                               
         ZIC   RF,NBOOKS         BASE BOOK LIST ON LATEST BOOK IN LIST          
         LA    RE,BOOKS                                                         
         XC    DBSELBK,DBSELBK                                                  
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
         B     XIT3                                                             
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
         BE    XIT3                                                             
         CLI   MLSTAT+4,C'C'       BYE,BYE IF SIMULCAST                         
         BE    XIT3                                                             
         CLI   MLSTAT+4,C'D'       BYE,BYE IF SIMULCAST                         
         BE    XIT3                                                             
*                                                                               
EXPHOOK1 CLI   MLSTAT,C'Z'         CUT OUT MARKET TOTALS                        
         BH    XIT3                                                             
         B     EXPHOOK4                                                         
         CLI   SPILLOPT,C'Y'       OPTION TO SHOW SPILL ONLY                    
         BNE   EXPHOOK2                                                         
         CLI   MLHOME,C'S'                                                      
         BNE   XIT3                                                             
         SPACE 1                                                                
EXPHOOK2 CLI   SPILLOPT,C'N'       OPTION TO OMIT SPILL                         
         BNE   EXPHOOK4                                                         
         CLI   MLHOME,C'S'                                                      
         BE    XIT3                                                             
         SPACE 1                                                                
EXPHOOK4 L     R2,ATHISTAT                                                      
         XC    0(9,R2),0(R2)       STATION + DEMO VALUE                         
         CLC   PREVSTA,MLSTAT                                                   
         BE    XIT                                                              
         MVC   0(5,R2),MLSTAT                                                   
         MVC   PREVSTA,MLSTAT                                                   
         LA    R2,9(R2)                                                         
         ST    R2,ATHISTAT                                                      
         AI    NSTATS,1                                                         
         CLI   NSTATS,102                                                       
         BNH   XIT3                                                             
         SH    R2,=H'9'                                                         
         ST    R2,ATHISTAT                                                      
         MVI   NSTATS,102                                                       
         B     XIT3                                                             
         DC    H'0'                OUT OF SPACE IN STATION LIST                 
XIT3     XMOD1                                                                  
         SPACE 2                                                                
FLTCMB   NTR1                                                                   
         LA    R1,1                                                             
         L     R7,ULPNTR                                                        
FLTCMB1  CLI   0(R7),X'FF'         END OF LIST                                  
         BE    FLTCMBX                                                          
         CLI   0(R7),0                                                          
         BE    FLTCMBX                                                          
         CLI   0(R7),3             COMBO IN LIST                                
         BE    FLTCMB2                                                          
         ZIC   R0,1(R7)                                                         
         AR    R7,R0                                                            
         B     FLTCMB1                                                          
*                                                                               
FLTCMB2  ZIC   R0,1(R7)            POINT TO STATIONS                            
         AR    R0,R7                                                            
         LA    R2,11(R7)                                                        
FLTCMB2A CR    R2,R0               END OF STATIONS?                             
         BNL   FLTCMB3                                                          
         CLC   MLSTAT,0(R2)                                                     
         BNE   *+10                                                             
         SR    R1,R1                                                            
         B     FLTCMBX                                                          
         LA    R2,5(R2)                                                         
         B     FLTCMB2A                                                         
*                                                                               
FLTCMB3  LR    R7,R0               NEXT ONE                                     
         B     FLTCMB1                                                          
*                                                                               
FLTCMBX  LTR   R1,R1                                                            
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------               
AFTPARSE NMOD1 0,**AFTPARSE**                                                   
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         L     RE,AIO2             CLEAR A BIG AREA                             
         LA    RF,2000                                                          
         XCEF                                                                   
* ASSUME THERE IS NO DRIVES                                                     
         MVI   ONEDPT,X'0000'                                                   
         MVI   DRIVESTA,C'N'                                                    
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
         MVC   ELEMSTOP(ELEMBDAT+1),=X'7E0102D300D4'                            
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
         BZ    AFTPARX2            NO STATIONS, LEAVE                           
         SPACE 1                                                                
* NOT EMPTY, PUT IN S=                                                          
         SPACE 1                                                                
         CLC   =C'AMDRIVE',HRSSTAT                                              
         BE    USEDRIVE                                                         
         CLC   =C'PMDRIVE',HRSSTAT                                              
         BE    USEDRIVE                                                         
         CLC   =C'DRIVE',HRSSTAT                                                
         BE    USEDRIVE                                                         
         SPACE 1                                                                
* COULD BE A DAYPART                                                            
         SPACE 1                                                                
         GOTO1 VSUBR07,DMCB,('EDITDPTE',(RC))                                   
         BNE   NOTDAYPT                                                         
         B     USEDAYPT                                                         
         SPACE 1                                                                
* MUST BE A STATION                                                             
         SPACE 1                                                                
NOTDAYPT MVI   HRSSTATH+4,X'C0'                                                 
         XC    CONHEAD,CONHEAD                                                  
         MVC   ELEMSTOP(ELEMBDAT+1),=X'7E01032800E2'                            
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
USEDRIVE MVC   ONEDPT,=X'7C01'                                                  
         MVI   DAYPOPT,C'N'                                                     
         MVI   DRIVESTA,C'Y'                                                    
         MVI   STAMISS,C'N'                                                     
         B     AFTPARXT                                                         
USEDAYPT MVI   DAYPOPT,C'N'                                                     
         MVI   DRIVESTA,C'N'                                                    
         MVI   STAMISS,C'N'                                                     
*                                                                               
AFTPARXT MVC   0(4,R1),0(R3)                                                    
AFTPARX2 LA    R1,OUTAREA1                                                      
         ST    R1,OUTAREA                                                       
         C     R1,OUTAREA          NO ERROR                                     
         XMOD1                                                                  
AFTERROR LA    R1,OUTAREA1                                                      
         ST    R1,OUTAREA                                                       
         LTR   R1,R1               ERROR                                        
         XMOD1                                                                  
         SPACE 3                                                                
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
*----------------------------------------------------------------               
*              SET UP TITLES                                                    
         SPACE 3                                                                
*              OUTPUTS             TITLES                                       
         SPACE 1                                                                
         DS    0H                                                               
SETTITLE NMOD1 0,**SETTITL**                                                    
         L     R8,0(R1)                                                         
         L     R9,4(R1)                                                         
         L     RC,8(R1)                                                         
*                                                                               
         LA    R3,H8+1                                                          
         A     R3,DISP                                                          
         CLI   UNIVOPT,C'Y'                                                     
         BNE   SETTIT1                                                          
         MVI   SKIPPER3,C'Y'                                                    
         SPACE 1                                                                
SETTIT1  DS    0C                                                               
         LA    R3,H10+1                                                         
         A     R3,DISP                                                          
         LR    RE,R3                                                            
         MVC   0(5,R3),CATTITS                                                  
         LA    R3,8(R3)                                                         
         CLI   UNIVOPT,C'Y'        NEED MORE ROOM                               
         BE    *+12                                                             
         CLI   DAYPOPT,C'Y'                                                     
         BNE   *+8                                                              
         LA    R3,4(R3)                                                         
         LR    RF,R3                                                            
         SR    RF,RE                                                            
         ST    RF,DISPD            SAVE DEMO DISPLACEMENTS                      
         CLI   ONEDPT,0                                                         
         BE    DC_DISP                                                          
         OC    COMBCNTR,COMBCNTR                                                
         BZ    DC_DISP                                                          
         LA    RF,4(RF)                                                         
         ST    RF,DISPD                                                         
         LA    R3,4(R3)                                                         
DC_DISP  LA    R4,DEMOS                                                         
         L     RF,DEMOCON                                                       
SETTLP   DS    0H                                                               
         GOTO1 (RF),DMCB,(R4),(5,WORK),DBLOCK,0                                 
         MVC   0(5,R3),WORK                                                     
         LA    R3,6(R3)                                                         
         CLI   SKIPPER3,C'Y'                                                    
         BNE   SETTIT1A                                                         
         LA    R3,2(R3)                                                         
         MVI   SKIPPER3,C'N'                                                    
SETTIT1A LA    R4,3(R4)                                                         
         CLI   0(R4),X'FF'         END OF DEMOS?                                
         BNE   SETTLP              NO                                           
         B     STITLXMD                                                         
*                                                                               
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
STITLXMD XMOD1                                                                  
         SPACE 1                                                                
SKIPPER3 DC    C'N'                                                             
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
PRUNIV   NMOD1 0,**PRUNIV**          PRINT THE UNIVERSES                        
         L     R8,0(R1)                                                         
         L     R9,4(R1)                                                         
         L     RC,8(R1)                                                         
*                                                                               
         GOTO1 =A(SETTITLE),DMCB,(R8),(R9),(RC),RR=RELO                         
         LA    R4,THISLINE                                                      
         USING TDD,R4                                                           
         MVC   TDVLMF(L'UNIVS),UNIVS                                            
         CLI   CALCOPT,C'Y'        TOOK AVERAGE BEFORE?                         
         BE    PRUNIV2A            YEAH, SKIP IT                                
         ZIC   R1,NBOOKS           FIRST GET THE AVERAGE                        
         LA    R4,TDVLMF                                                        
         ZIC   R0,NDEMOS                                                        
PRUNIV2  SR    RE,RE                                                            
         ICM   RE,7,0(R4)                                                       
         SRDA  RE,32                                                            
         SLDA  RE,1                                                             
         DR    RE,R1                                                            
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         STCM  RF,7,0(R4)                                                       
         LA    R4,3(R4)                                                         
         BCT   R0,PRUNIV2                                                       
PRUNIV2A LA    R4,THISLINE                                                      
         MVI   THISDEM,0                                                        
         MVI   SKIPPER,C'Y'                                                     
         MVC   THISDEM+1(3),TDVLMF                                              
         ZIC   R2,NDEMOS                                                        
         MVC   P(08),=C'POPS(00)'                                               
         LA    R3,P                                                             
         A     R3,DISP                                                          
         A     R3,DISPD                                                         
         LA    R4,TDVLMF                                                        
PRUNIV3  ICM   R1,7,0(R4)                                                       
         EDIT  (R1),(6,0(R3))                                                   
         LA    R4,3(R4)                                                         
         CLI   SKIPPER,C'Y'                                                     
         BNE   PRUNIV3A                                                         
         LA    R3,2(R3)                                                         
         MVI   SKIPPER,C'N'                                                     
PRUNIV3A LA    R3,6(R3)                                                         
         BCT   R2,PRUNIV3                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   SKIPPER,C'Y'                                                     
         LA    R4,THISLINE                                                      
         ZIC   R2,NDEMOS                                                        
         L     R7,ATWA                                                          
         MVC   P(08),=C'% DIST -'                                               
         MVC   P+8(7),HRSIDEM                                                   
         LA    R3,P+1                                                           
         A     R3,DISP                                                          
         A     R3,DISPD                                                         
         LA    R4,TDVLMF                                                        
PRUNIV4  SR    RE,RE                                                            
         ICM   RE,7,0(R4)                                                       
         SRDA  RE,32                                                            
         M     RE,=F'1000'                                                      
         SLDA  RE,1                                                             
         OC    THISDEM,THISDEM                                                  
         BNZ   NOTDIV01                                                         
         SR    RE,RE                                                            
         L     RF,=X'FFFFFFFF'                                                  
         B     *+8                                                              
NOTDIV01 D     RE,THISDEM                                                       
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         LR    R1,RF                                                            
         EDIT  (R1),(5,0(R3)),1                                                 
         LA    R4,3(R4)                                                         
         CLI   SKIPPER,C'Y'                                                     
         BNE   PRUNIV5                                                          
         LA    R3,2(R3)                                                         
         MVI   SKIPPER,C'N'                                                     
PRUNIV5  LA    R3,6(R3)                                                         
         BCT   R2,PRUNIV4                                                       
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   SPACING,SPACOPT                                                  
         MVI   SHAREOPT,C'N'                                                    
PRUNIVX  XMOD1                                                                  
         SPACE 3                                                                
SKIPPER  DC    C'Y'                                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              FORMAT AND PRINT A REPORT LINE                                   
         SPACE 3                                                                
*              INPUTS              R2=A(STATION BUFFER)                         
*                                  ASTATS=A(5 CHARACTER STATION CODES)          
*                                  NBOOKS                                       
*                                  DISP                                         
         SPACE 1                                                                
         USING TDD,R4                                                           
*                                                                               
         DS    0H                                                               
FORMLINE NMOD1 0,**FORMLINE**                                                   
         L     R4,0(R1)                                                         
         L     R5,4(R1)            ADDRESS OF STATION                           
         L     R8,8(R1)                                                         
         L     R9,12(R1)                                                        
         L     RC,16(R1)                                                        
         L     RF,20(R1)           ADDRESS OF TMPDEMOS                          
*                                                                               
         LA    RF,0(RF)            CLEAR HIGH ORDER BYTE                        
         CLI   20(R1),X'00'        CALL TO EDTDEML?                             
         BZ    FORMLN1             NO, FOR FORMLINE                             
         BAS   RE,EDTDEML                                                       
         B     FRMLNXIT                                                         
FORMLN1  LA    R3,P+1                                                           
         A     R3,DISP                                                          
         CLI   ONEDPT,0                                                         
         BE    *+14                                                             
         MVC   P+1(5),0(R5)                                                     
         B     FRMLN10                                                          
         SPACE 1                                                                
         CLI   DAYPOPT,C'Y'                                                     
         BNE   *+14                                                             
         MVC   P+1(16),0(R5)                                                    
         B     FRMLN10                                                          
         SPACE 1                                                                
         OC    TDEHR,TDEHR                                                      
         BNZ   *+10                                                             
         MVC   TDEHR,=H'2400'                                                   
         LR    R0,RF                                                            
         GOTO1 UNTIME,DMCB,TDSHR,P+1                                            
         LR    RF,R0                                                            
         CLC   P+1(5),=C'9-10A'                                                 
         BNE   *+8                                                              
         MVI   SPACING,2                                                        
         CLC   P+1(4),=C'2-3P'                                                  
         BNE   *+8                                                              
         MVI   SPACING,2                                                        
         CLC   P+1(4),=C'6-7P'                                                  
         BNE   *+8                                                              
         MVI   SPACING,2                                                        
         CLC   P+1(4),=C'12A-1A'                                                
         BNE   *+8                                                              
         MVI   SPACING,2                                                        
FRMLN10  A     R3,DISPD                                                         
         CLI   UNIVOPT,C'Y'                                                     
         BNE   FRMLN11                                                          
         MVI   SKIPPER2,C'Y'                                                    
FRMLN11  BAS   RE,EDTDEML          M-F                                          
         OC    ONEDPT,ONEDPT                                                    
         BZ    FRMLNXIT                                                         
         CLC   =C'CMB',P+1                                                      
         BNE   FRMLNXIT                                                         
*                                                                               
         ZIC   R7,3(R5)            GET COMBO #                                  
         N     R7,=X'0000000F'                                                  
         L     RE,ULPNTR                                                        
MORECMB2 CLI   0(RE),3             IS IT A COMBO                                
         BNE   MORECMB3                                                         
         BCTR  R7,0                                                             
         LTR   R7,R7                                                            
         BZ    SCNDCMB                                                          
         ZIC   R6,1(RE)                                                         
         AR    RE,R6                                                            
         B     MORECMB2                                                         
MORECMB3 ZIC   R6,1(RE)                                                         
         AR    RE,R6                                                            
         B     MORECMB2                                                         
*                                                                               
SCNDCMB  CLC   =CL9' ',2(RE)                                                    
         BNE   SCNDCMBA                                                         
         MVC   P+1(5),11(RE)                                                    
         MVI   P+6,C'/'                                                         
         MVC   P+7(5),16(RE)                                                    
         B     FRMLNXIT                                                         
SCNDCMBA MVC   P+1(9),2(RE)                                                     
FRMLNXIT XMOD1                                                                  
         SPACE 2                                                                
EDTDEML  NTR1                                                                   
         LA    R4,TDVLMF                                                        
         LR    R5,RF                                                            
         ZIC   R2,NDEMOS           MAX NUMBER OF DEMOS                          
         SR    R1,R1                                                            
         ICM   R1,7,0(R4)                                                       
         ST    R1,THISDEM          SAVE THE BASE DEMO                           
         MVC   BYTE,1(R5)                                                       
*                                                                               
EDTDMLP  CLI   0(R5),X'FF'         END OF DEMOS?                                
         BE    EDTDEMLX            YES                                          
         SR    R1,R1                                                            
         ICM   R1,7,0(R4)                                                       
         CLI   SHAREOPT,C'Y'                                                    
         BE    *+10                                                             
         MVC   BYTE,1(R5)                                                       
         BAS   RE,FORMDEM                                                       
         CLI   SHAREOPT,C'Y'                                                    
         BNE   *+8                                                              
         MVI   BYTE,X'01'          SET FOR COMP CALC ON SECOND DEMO             
         LA    R3,6(R3)                                                         
         CLI   SKIPPER2,C'Y'                                                    
         BNE   EDTDEM1                                                          
         LA    R3,2(R3)                                                         
         MVI   SKIPPER2,C'N'                                                    
EDTDEM1  LA    R4,3(R4)                                                         
         LA    R5,3(R5)                                                         
         BCT   R2,EDTDMLP                                                       
*                                                                               
EDTDEMLX XIT1  REGS=(R3)                                                        
         SPACE 2                                                                
         EJECT                                                                  
*              FORMAT FOR A BOOK                                                
         SPACE 3                                                                
*              INPUT               BYTE = DEMO MODIFIER                         
*                                  R1 = DEMO VALUE                              
*                                  R3=A(OUTPUT AREA)                            
         SPACE 1                                                                
FORMDEM  NTR1                                                                   
         CLI   BYTE,1                                                           
         BNE   FORMDEM1                                                         
         SR    R0,R0                                                            
         M     R0,=F'2000'                                                      
         OC    THISDEM,THISDEM                                                  
         BZ    FORMDEM2                                                         
         D     R0,THISDEM                                                       
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
FORMDEM1 EDIT  (R1),(5,0(R3)),1    DISPLAY DEMOS.                               
         CLI   BYTE,1              1 DEC. FOR COMP SHARE                        
         BE    FORMDEM2                                                         
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
         EJECT                                                                  
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
         SPACE 3                                                                
SKIPPER2 DC    C'N'                                                             
         SPACE 3                                                                
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
*ENDPTE  EQU   (GENDPT#-*)/4+1                                                  
EDITMBE  EQU   (EDITMB#-*)/4+1                                                  
EDITOPTE EQU   (EDITOPT#-*)/4+1                                                 
EDITDPTE EQU   (EDITDPT#-*)/4+1                                                 
INDEXUE  EQU   (INDEXU#-*)/4+1                                                  
SETBOXE  EQU   (SETBOX#-*)/4+1                                                  
SUMDPTE  EQU   (SUMDPT#-*)/4+1                                                  
TESTMKE  EQU   (TESTMK#-*)/4+1                                                  
         SPACE 2                                                                
VALDSTA# B     VALDSTA        1    VALIDATE A STAT. ON DEM FILE                 
*                                   (FROM SCREEN INPUT)                         
EDITML#  B     EDITML         2    EDIT A USER LIS                              
WRTWA#   B     WRTWA          3    WRITE A TWA (TWANUM=WHICH ONE)               
RESTWA#  B     RESTWA         4    RESTORE A TWA                                
M1INIT#  B     M1INIT         5    INITIALIZE A MKT LIST SCREEN                 
*ENDPT#  B     GENDPT         6    GENERATE DAYPART SCREEN                      
VALDST2# B     VALDST2        7    VALIDATE A STAT. ON DEM FILE                 
*                                   (FROM SCANNER BLOCK)                        
EDITMB#  B     EDITMB         8    VALIDATE LIST OF BOOKS FOR MKT               
EDITOPT# B     EDITOPT        9    VALIDATE THE OPTIONS                         
EDITDPT# B     EDITDPT       10    VALIDATE THE DAYPART                         
INDEXU#  B     INDEXU        11    INDEX DEM SHARES TO UNIV. SHARES             
SETBOX#  B     SETBOX        12    SET UP THE BOXES                             
SUMDPT#  B     SUMDPT        13    SUM THE HOURLY DAYPARTS                      
TESTMK#  B     TESTMRKT      14    TEST MARKET                                  
         EJECT                                                                  
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
EDITML1B MVC   CONHEAD(L'ERRLEN),ERRLEN                                         
         LA    RE,CONHEAD+L'ERRLEN    GIVE SOME IDEA OF WHERE                   
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
NEEDSCBN MVC   CONHEAD(21),=C'FIELD LENGTH TOO LONG'                            
         B     ULXIT                                                            
NEEDCMBN MVC   CONHEAD(54),=C'NEED COMBO NAME FOR COMBOS WITH MORE THANX        
                TWO STATIONS'                                                   
         B     ULXIT                                                            
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
         CLI   HRSSRCE,C'B'                                                     
         BNE   EDMLM1A                                                          
         MVC   TEMPMRKT,SLDMKTB                                                 
EDMLM1A  BAS   RE,GOTTAMKT                                                      
EDMLM2   CLI   0(R5),X'05'                                                      
         BNE   EDMLM3                                                           
         MVI   STAMISS,C'N'                                                     
         USING SLSELEM,R5                                                       
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
GOTMRKT  BAS   RE,GOTTAMKT                                                      
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
         MVI   STAMISS,C'N'        STATION NOT MISSING                          
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
         CLI   ELEMDATA,C'+'       COULD BE A COMBO LIST                        
         BE    EDITML2                                                          
         OI    HSTAT,X'1'                                                       
         B     EDSTAT1                                                          
         EJECT                                                                  
*-------------------------------------------------*                             
* SEE IF THIS IS IN COMBO LIST FIRST              *                             
* IF NOT THEN TREAT IT AS A REGULAR COMBO         *                             
* IF IT IS THEN EXTRACT THE MARKET AND COMBO INFO *                             
*-------------------------------------------------*                             
EDMLC    XC    CMBCOUNT,CMBCOUNT   INIT COMBO COUNTER                           
         MVI   STAMISS,C'N'        STATION NOT MISSING                          
         CLI   ELEMLEN,0           COMBO NAME AT LEAST 1                        
         BZ    EDITML1                                                          
         CLI   ELEMLEN,10          COMBO NAMES < 10 CHARS                       
         BH    NEEDSCBN                                                         
         OC    MKTNUM,MKTNUM       NEED A MARKET                                
         BNZ   *+14                                                             
         OC    PARAS(4),PARAS      FROM SOMEPLACE                               
         BZ    EDITML1                                                          
         CLI   ELEMSTOP,C'='       STANDARD COMBO?                              
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
         MVC   TEMPNAME,CLKNAME    SAVE NAME FOR LATER                          
         MVC   CLKMKAL,CITYCODE    ALPHA CODE                                   
         GOTO1 HIGH                SEARCH FOR IT                                
         CLC   KEY(13),KEYSAVE     DO WE HAVE IT?                               
         BNE   EDMLC0              NO, TRY PARTIAL SEARCH                       
         GOTO1 GETREC              YES, GET THE RECORD                          
         B     EDCMB                                                            
*                                                                               
EDMLC0   XC    KEY,KEY                                                          
         MVI   SUBSID,C'N'                                                      
         GOTO1 =A(CKSUBSID),DMCB,(R9),(RC),RR=RELO                              
         BNZ   EDMLC01                                                          
         MVI   SUBSID,C'Y'                                                      
*                                                                               
EDMLC01  XC    KEY,KEY                                                          
         MVI   GOTMATCH,C'N'                                                    
         USING CLKEY,R5                                                         
         MVC   CLKTYPE(2),=X'0D5E'                                              
         MVC   CLKAM,BAGYMD                                                     
         CLI   SUBSID,C'Y'                                                      
         BNE   AGY00001                                                         
         MVI   CLKAM,X'B2'         USE INTEREP                                  
AGY00001 MVC   CLKMKAL,CITYCODE    ALPHA CODE                                   
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
         BE    EDCMB_1             GOOD COMBO LIST                              
         LTR   R1,R1                                                            
         BZ    EDITML1C            NOT DELETED COMBO                            
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
         MVC   2(9,R4),=CL9' '     SET COMBO CAPTION                            
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
         BE    TESTEND                                                          
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
TESTEND1 CLC   11(4,R4),16(R4)                                                  
         BNE   TESTEND5                                                         
         MVC   2(4,R4),11(R4)                                                   
         MVC   6(3,R4),=C'/AF'                                                  
         B     TESTENDX                                                         
TESTEND5 MVC   2(4,R4),11(R4)                                                   
         MVI   6(R4),C'/'                                                       
         MVC   7(4,R4),16(R4)                                                   
TESTENDX CLI   ELEMSTOP,C'='                                                    
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
         BH    NOTYET3             YES, ALPHA MARKET AT MOST 3                  
         BCTR  RF,0                                                             
         MVC   CITYCODE,=CL3' '                                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CITYCODE(0),ELEMDATA                                             
         LA    R5,MEDTYPE                                                       
         GOTO1 VCTYMRKT                                                         
         BZ    NOTYET3                                                          
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
*        EDIT THE OPTION FIELDS                                                 
*                                                                               
EDITOPT  MVI   BOXOPT,C'N'         EDIT OPTION FIELD                            
         MVI   TRACEOPT,C'N'                                                    
         MVI   LEFTOPT,C'N'                                                     
         MVI   SPACOPT,1                                                        
         MVI   UNIVOPT,C'N'                                                     
         MVI   EXPANOPT,C'N'                                                    
         MVI   CALCOPT,C'Y'                                                     
         CLI   CTRY,C'C'           CANADA?                                      
         BNE   *+8                                                              
         MVI   CALCOPT,C'N'        YES, NO ADDITIONAL CALCULATIONS              
         MVI   ADDROPT,C'Y'                                                     
         MVI   AUTOOPT,C'N'                                                     
         CLI   5(R2),0                                                          
         BE    EQXIT2                                                           
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
         BNE   OPT5                                                             
         MVI   LEFTOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT5     CLC   12(7,R4),=C'DAYPART'                                             
         BNE   OPT7                                                             
         CLI   ONEDPT,0            NOT VALID IF DOING ONE DAYPART               
         BNE   BADOPT                                                           
         MVC   DAYPOPT,22(R4)                                                   
         B     OPTEND                                                           
         SPACE 1                                                                
OPT7     CLC   12(2,R4),=C'S   '   SPACING OPTION                               
         BNE   OPT8                                                             
         MVC   SPACOPT,11(R4)                                                   
         CLI   SPACOPT,1                                                        
         BL    BADOPT                                                           
         CLI   SPACOPT,3                                                        
         BH    BADOPT                                                           
         B     OPTEND                                                           
         SPACE 1                                                                
OPT8     CLC   12(5,R4),=C'TRACE'  OPTION TO TRACE DEMAND HOOKS                 
         BNE   OPT9                                                             
         MVC   TRACEOPT,22(R4)                                                  
         B     OPTEND                                                           
         SPACE 1                                                                
OPT9     CLC   12(4,R4),=C'UNIVERSE'                                            
         BNE   OPT10                                                            
         CLI   NDEMOS,MAXDEMOS     NOT VALID IF MORE THEN 18 DEMOS              
         BH    BADOPT                                                           
         MVC   UNIVOPT,22(R4)                                                   
         OC    IDEMO,IDEMO                                                      
         BNZ   OPTEND                                                           
         CLI   UNIVOPT,C'Y'                                                     
         BNE   OPTEND                                                           
         MVC   IDEMO,DEMOS                                                      
         MVI   IDEMO+2,133         SET TO L12+                                  
         MVC   HRSIDEM(7),=C'L12+   '                                           
         B     OPTEND                                                           
         SPACE 1                                                                
OPT10    CLC   12(4,R4),=C'SIMU'   OPTION TO SUPPRESS SIMULCAST STATS.          
         BNE   OPT20                                                            
         MVC   SIMULOPT,22(R4)                                                  
         B     OPTEND                                                           
         SPACE 1                                                                
OPT20    CLC   12(4,R4),=C'EXPA'   OPTION TO EXPAND COMBO STATIONS              
         BNE   OPT22                                                            
         MVC   EXPANOPT,22(R4)                                                  
         B     OPTEND                                                           
         SPACE 1                                                                
OPT22    CLC   12(4,R4),=C'CALC'   OPTION TO EXPAND COMBO STATIONS              
         BNE   OPT23                                                            
         CLC   22(3,R4),=C'BOOK'                                                
         BE    OPT22A                                                           
         B     OPTEND                                                           
OPT22A   MVI   CALCOPT,C'N'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT23    CLC   12(3,R4),=C'SEP'    OPTION TO USE MULTIPLE DEMOS                 
         BNE   OPT24                                                            
         MVC   SEPAOPT,22(R4)                                                   
         B     OPTEND                                                           
         SPACE 1                                                                
OPT24    CLC   12(4,R4),=C'ADDR'   OPTION TO USE MULTIPLE DEMOS                 
         BNE   OPT25                                                            
         MVC   ADDROPT,22(R4)                                                   
         B     OPTEND                                                           
         SPACE 1                                                                
OPT25    CLC   12(4,R4),=C'AUTO'   OPTION TO USE AUTO DELETION                  
         BNE   OPT30                                                            
         CLC   =C'DELETE',22(R4)                                                
         BNE   OPTEND                                                           
         MVI   AUTOOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT30    DS    0H                                                               
         SPACE 1                                                                
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
         B     NEQXIT2                                                          
         SPACE 1                                                                
OPTEND   LA    R4,32(R4)                                                        
         BCT   R0,OPT2                                                          
         B     EQXIT2                                                           
OPTERR   DC    C'** ERROR ** INVALID OPTION'                                    
         DS    0H                                                               
         EJECT                                                                  
SUMDPT   LA    RF,RNKDPTTB                                                      
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
         SPACE 1                                                                
         LA    R3,SUMDPTB                                                       
SUMDPT1  CLI   0(R3),X'FF'                                                      
         BE    XIT2                                                             
         ZIC   RE,0(R3)            START HOUR                                   
         ZIC   R4,1(R3)            END HOUR                                     
         LA    R4,1(R4)            SET THE BCTR COUNT                           
         SR    R4,RE                                                            
         MH    RE,=AL2(L'TDENT)                                                 
         LA    RE,BUFF(RE)                                                      
         LA    RE,TDVLMF-TDENT(RE) START OF DATA                                
         L     RF,AIO1             SUMMATION AREA                               
         XC    0(72,RF),0(RF)                                                   
         LA    R0,18               ***                                          
         STM   RE,R0,DMCB                                                       
SUMDPT4  SR    R1,R1               ADD ENTIRE HOUR                              
         ICM   R1,7,0(RE)                                                       
         A     R1,0(RF)                                                         
         ST    R1,0(RF)                                                         
         LA    RE,3(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,SUMDPT4                                                       
         LM    RE,R0,DMCB          CONTINUE FOR ENTIRE DAYPART                  
         LA    RE,L'TDENT(RE)                                                   
         ST    RE,DMCB                                                          
         BCT   R4,SUMDPT4                                                       
         SPACE 1                                                                
         ZIC   RE,0(R3)                                                         
         ZIC   R4,1(R3)                                                         
         LA    R4,1(R4)                                                         
         SR    R4,RE                                                            
         L     R1,AIO1                                                          
         LA    R0,18               ***                                          
SUMDPT8  L     RF,0(R1)                                                         
         MH    RF,=H'10'                                                        
         SR    RE,RE                                                            
         DR    RE,R4                                                            
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,SUMDPT8                                                       
         SPACE 1                                                                
*        NOW MOVE DATA TO FINAL DESTINATION                                     
         ZIC   RE,2(R3)                                                         
         MH    RE,=AL2(L'TDENT)                                                 
         LA    RE,BUFF(RE)                                                      
         MVC   TDSHR-TDD(4,RE),3(R3)                                            
         LA    RE,TDVLMF-TDENT(RE) START OF DATA                                
         L     RF,AIO1             SUMMATION AREA                               
         LA    R0,18               ***                                          
SUMDPT10 MVC   0(3,RE),1(RF)       MOVE AVG DATA TO BUFFER SLOT                 
         LA    RE,3(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,SUMDPT10                                                      
         SPACE 1                                                                
         LA    R3,7(R3)            DO NEXT DAYPART                              
         B     SUMDPT1                                                          
         SPACE 1                                                                
SUMDPTB  DC    AL1(1,4,24),AL2(0600,1000)                                       
         DC    AL1(5,9,25),AL2(1000,1500)                                       
         DC    AL1(10,13,26),AL2(1500,1900)                                     
         DC    AL1(14,18,27),AL2(1900,2400)                                     
         DC    X'FF'                                                            
         SPACE 1                                                                
RNKDPTTB DC    AL1(0,4)            5A-10A                                       
         DC    AL1(5,9)            10A-3P                                       
         DC    AL1(10,13)          3-7P                                         
         DC    AL1(14,19)          7P-1A                                        
         DC    X'FF',X'00'         END,ALIGN                                    
         EJECT                                                                  
         SPACE 1                                                                
SETBOX   CLI   BOXOPT,C'N'         OPTION TO SUPPRESS                           
         BE    XIT2                                                             
         L     R4,ABOX             HANDLE BOXES IF WE'RE OFF LINE               
         USING BOXD,R4                                                          
         LTR   R4,R4                                                            
         BZ    XIT2                                                             
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
         B     XIT2                                                             
         EJECT                                                                  
         SPACE 2                                                                
* VALIDATE STATION EXPRESSIONS (WTAE,WTAE/103,WABC-A)                           
*  USING DEMO FILE STATIONS AND MARKETS                                         
         SPACE 1                                                                
VALDSTA  XC    ACTSTAT,ACTSTAT                                                  
         XC    ACTMKT,ACTMKT                                                    
*                                                                               
         CLI   5(R2),0                                                          
         BE    XIT2                                                             
*                                                                               
         LA    R4,BLOCK                                                         
         XC    0(64,R4),0(R4)      CLEAR SCANNER TABLE                          
*                                                                               
         ZIC   R3,5(R2)                                                         
         LA    R1,8(R2)                                                         
VALDST1  CLI   0(R1),C','                                                       
         BNE   *+8                                                              
         MVI   0(R1),C'/'                                                       
         AHI   R1,1                                                             
         BCT   R3,VALDST1                                                       
*                                                                               
         GOTO1 SCANNER,DMCB,(L'SCLINE,(R2)),(X'80',(R4)),C',=/-'                
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
         B     S7BADSTA                                                         
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
         BE    S7BADSTA                                                         
         SPACE 2                                                                
* GET MARKET NAME                                                               
         SPACE 1                                                                
DSTA20   MVC   ACTMKT,DBACTRMK     MOVE MARKET NUMBER                           
         B     EQXIT2                                                           
*                                                                               
S7BADSTA MVI   DBERROR,X'10'                                                    
         B     NEQXIT2                                                          
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
*              EDIT DAYPART                                                     
         SPACE 3                                                                
*              AGENCY SPECIFIED    DAY/TIMES INPUT                              
         SPACE 1                                                                
EDITDPT  LA    R2,HRSSTATH                                                      
         GOTO1 ANY                                                              
         ZIC   R0,5(R2)                                                         
         LA    R1,HRSSTAT                                                       
         SPACE 1                                                                
VR1      CLI   0(R1),C'/'          SEE IF SLASH IS USED                         
         BE    VR1C                                                             
         CLI   0(R1),C'+'          CANADA USES '+'                              
         BE    VR1D                                                             
         CLI   0(R1),C','                                                       
         BE    VR1C                                                             
         CLI   0(R1),C'-'          EVERYTHING BEFORE '-' --> ','                
         BL    VR1C                                                             
         B     VR1D                                                             
VR1C     MVI   0(R1),C'/'                                                       
VR1D     MVC   DMCB+8(4),=C',=/='                                               
         LA    R1,1(R1)                                                         
         BCT   R0,VR1                                                           
         SPACE 1                                                                
VR2A     LA    R3,DPCDAY                                                        
         SR    R5,R5               (COUNT N'DAY/TIMES IN R5)                    
         MVI   SCANLEN,32                                                       
         XC    BLOCK(250),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(15,BLOCK)                                     
         LA    R4,BLOCK                                                         
         SPACE 1                                                                
VR10     ZIC   R0,0(R4)                                                         
         GOTO1 DAYVAL,DMCB,((R0),12(R4)),BYTE,WORK                              
         CLI   BYTE,0                                                           
         BNE   VR20                NON-ZERO MEANS VALID INPUT                   
         SPACE 1                                                                
         BAS   RE,FRIENDLY         CHECK FOR EASY EXPRESSIONS                   
         BNE   VR12                                                             
         MVC   HRSSTATH+5(1),0(R4) COPY THE LENGTH                              
         XC    HRSSTAT,HRSSTAT                                                  
         ZIC   R1,0(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     SETDPT                                                           
         MVC   HRSSTAT(0),12(R4)                                                
*        B     SETDPT                                                           
VR12     LA    R1,SPCLDAY          CHECK FOR SPECIAL DAYS                       
         LA    R0,SPCLDAYL                                                      
         SPACE 1                                                                
VR14     CLC   12(4,R4),1(R1)                                                   
         BNE   VR16                                                             
         MVC   BYTE,0(R1)          SET VALID INPUT                              
         B     VR20                                                             
         SPACE 1                                                                
VR16     LA    R1,1(R1)                                                         
         BCT   R0,VR14                                                          
         B     BADDPT                                                           
         SPACE 1                                                                
SPCLDAY  DS    0CL6                                                             
         DC    X'03',CL5'S-S  '    DAY VALUE/INPUT STRING                       
         DC    X'03',CL5'SA-S '                                                 
         DC    X'03',CL5'S-SU '                                                 
         DC    X'7F',CL5'M-S  '                                                 
SPCLDAYL EQU   *-SPCLDAY                                                        
         SPACE 1                                                                
VR20     LA    R1,DAYLIST                                                       
         LA    R0,DAYLISTL                                                      
         SPACE 1                                                                
VR22     CLC   BYTE,0(R1)                                                       
         BE    VR24                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,VR22                                                          
         B     BADDPT                                                           
         SPACE 1                                                                
DAYLIST  DC    X'7C'               MO-FR                                        
         DC    X'7E'               MO-SA                                        
         DC    X'7F'               MO-SU                                        
         DC    X'03'               SA-SU                                        
         DC    X'02'               SAT                                          
         DC    X'01'               SUN                                          
DAYLISTL EQU   *-DAYLIST                                                        
         SPACE 1                                                                
VR24     MVC   0(1,R3),BYTE        MOVE DAY TO LIST                             
         LA    R4,32(R4)           NEXT SCANNER ENTRY                           
         CLI   0(R4),0                                                          
         BNE   VR26                                                             
         MVI   ERROR,NOTIME        TELL USER TIME IS MISSING                    
         B     BADDPT                                                           
         SPACE 1                                                                
VR26     BAS   RE,VALTIME                                                       
         BNE   BADDPT                                                           
         SPACE 1                                                                
VR28     MVC   1(2,R3),HALF        MOVE TIME TO ELEMENT                         
         SPACE 1                                                                
         LA    R3,3(R3)            ADVANCE DAY/TIME LIST POINTER                
         LA    R4,32(R4)                                                        
         LA    R5,1(R5)                                                         
         CLI   0(R4),0             TEST ANY MORE INPUT THIS FIELD               
         BNE   VR29                                                             
         B     SETDPT                                                           
         SPACE 1                                                                
VR29     CH    R5,=H'5'                                                         
         BE    VR30                                                             
         SPACE 1                                                                
         BAS   RE,VALTIME          VALIDATE FOR ANOTHER TIME                    
         BNE   VR10                IF INVALID, TRY FOR ANOTHER DAY              
         SPACE 1                                                                
         LR    RE,R3                                                            
         SH    RE,=H'3'                                                         
         MVC   0(1,R3),0(RE)       COPY DAY FROM PREVIOUS                       
         B     VR28                 AND CONTINUE                                
         SPACE 1                                                                
VR30     MVI   ERROR,TOOMANY                                                    
         B     NEQXIT2                                                          
         SPACE 1                                                                
SETDPT   L     R2,=A(STANDPTS)     CONVERT TO DAYPART NUMBER                    
         USING STANDENT,R2                                                      
         CLI   DBSELSRC,C'M'                                                    
         BNE   *+8                                                              
         L     R2,=A(BBMDPTS)                                                   
         A     R2,RELO                                                          
         MVC   DPDESC,HRSSTAT                                                   
*                                                                               
SETDPT2  CLI   0(R2),X'FF'         NOT A STANDARD DAYPART                       
         BE    BADDPT                                                           
         CLC   STANDDTS(15),DPCDAY                                              
         BE    *+12                                                             
         LA    R2,L'STANDENT(R2)                                                
         B     SETDPT2                                                          
*                                                                               
         MVC   ONEDPT(1),DPCDAY    HAVE STANDARD DAYPART                        
         MVC   ONEDPT+1(1),STANDPRG SET DAY AND DAYPART NUM                     
         MVC   DPDESC,STANDESC                                                  
         B     EQXIT2                                                           
         SPACE 1                                                                
BADDPT   MVC   CONHEAD(L'INVDAYPT),INVDAYPT                                     
         MVI   ERROR,X'FF'                                                      
         B     NEQXIT2                                                          
INVDAYPT DC    C'** ERROR ** INVALID DAYPART OR STATION'                        
         DS    0H                                                               
         EJECT                                                                  
*              VALIDATE FOR FRIENDLY DAYPART EXPRESSION                         
         SPACE 3                                                                
FRIENDLY NTR1                                                                   
         CLI   DBSELSRC,C'M'       GWEN COOPER OF HDS SAID                      
         BE    CFRENDLY            CANADA WANTS THESE DAYPARTS TOO.             
         LA    R1,FRIENDS                                                       
         SPACE 1                                                                
FRIEND2  CLC   0(8,R1),BLOCK+12                                                 
         BE    FRIEND4                                                          
         LA    R1,23(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         BE    NEQXIT2                                                          
         B     FRIEND2                                                          
         SPACE 1                                                                
FRIEND4  MVC   DPCDAY(15),8(R1)                                                 
         B     EQXIT2                                                           
         SPACE 1                                                                
FRIENDS  DC    C'AMDRIVE ',X'7C',AL1(06,10),12X'00'                             
         DC    C'PMDRIVE ',X'7C',AL1(15,19),12X'00'                             
         DC    C'DRIVE   ',X'7C',AL1(06,10),X'7C',AL1(15,19),9X'00'             
         DC    X'FF'                                                            
         EJECT                                                                  
CFRENDLY LA    R1,CFRENDS                                                       
         SPACE 1                                                                
CFREND2  CLC   0(8+7,R1),BLOCK+12                                               
         BE    CFREND4                                                          
         LA    R1,23+7(R1)                                                      
         CLI   0(R1),X'FF'                                                      
         BE    NEQXIT2                                                          
         B     CFREND2                                                          
         SPACE 1                                                                
CFREND4  MVC   DPCDAY(15),8+7(R1)                                               
         B     EQXIT2                                                           
         SPACE 1                                                                
CFRENDS  DC    C'BREAKFAST      ',X'7C',AL1(01,01),12X'00'                      
         DC    C'DAY            ',X'7C',AL1(02,02),12X'00'                      
         DC    C'DRIVE          ',X'7C',AL1(03,03),12X'00'                      
         DC    C'EVENING        ',X'7C',AL1(04,04),12X'00'                      
         DC    C'BR+DA          ',X'7C',AL1(09,09),12X'00'                      
         DC    C'BR+DR          ',X'7C',AL1(10,10),12X'00'                      
         DC    C'BR+EV          ',X'7C',AL1(11,11),12X'00'                      
         DC    C'DA+DR          ',X'7C',AL1(12,12),12X'00'                      
         DC    C'DA+EV          ',X'7C',AL1(13,13),12X'00'                      
         DC    C'DR+EV          ',X'7C',AL1(14,14),12X'00'                      
         DC    C'BR+DA+DR       ',X'7C',AL1(15,15),12X'00'                      
         DC    C'BR+DA+EV       ',X'7C',AL1(16,16),12X'00'                      
         DC    C'BR+DR+EV       ',X'7C',AL1(17,17),12X'00'                      
         DC    C'DA+DR+EV       ',X'7C',AL1(18,18),12X'00'                      
         DC    C'BR+DA+DR+EV    ',X'7C',AL1(19,19),12X'00'                      
         DC    C'SATURDAY       ',X'02',AL1(05,05),12X'00'                      
         DC    C'SUNDAY         ',X'01',AL1(06,06),12X'00'                      
         DC    C'SA+SU          ',X'03',AL1(20,20),12X'00'                      
         DC    C'BR+SA          ',X'7E',AL1(21,21),12X'00'                      
         DC    C'DA+SA          ',X'7E',AL1(22,22),12X'00'                      
         DC    C'DR+SA          ',X'7E',AL1(23,23),12X'00'                      
         DC    C'BR+DR+SA       ',X'7E',AL1(24,24),12X'00'                      
         DC    C'BR+SA+SU       ',X'7F',AL1(25,25),12X'00'                      
         DC    C'DA+SA+SU       ',X'7F',AL1(26,26),12X'00'                      
         DC    C'DR+SA+SU       ',X'7F',AL1(27,27),12X'00'                      
         DC    C'EV+SA+SU       ',X'7F',AL1(28,28),12X'00'                      
         DC    C'BR+DA+DR+SA    ',X'7E',AL1(29,29),12X'00'                      
         DC    C'BR+DA+SA+SU    ',X'7F',AL1(30,30),12X'00'                      
         DC    C'BR+DA+DR+SA+SU ',X'7F',AL1(31,31),12X'00'                      
         DC    C'BR+DA+DR+EV+SA ',X'7E',AL1(32,32),12X'00'                      
         DC    C'ALL            ',X'7F',AL1(33,33),12X'00'                      
         DC    C'REACH          ',X'7F',AL1(07,07),12X'00'                      
         DC    C'M-S 5A-1A      ',X'7F',AL1(07,07),12X'00'                      
         DC    X'FF'                                                            
         EJECT                                                                  
*              ROUTINE TO VALIDATE A TIME EXPRESSION                            
         SPACE 3                                                                
VALTIME  NTR1                      VALIDATE TIME EXPRESSION                     
         ZIC   R0,0(R4)                                                         
         GOTO1 TIMVAL,DMCB,((R0),12(R4)),FULL                                   
         CLI   0(R1),X'FF'                                                      
         BE    NEQXIT2                                                          
         SPACE 1                                                                
         MVI   ERROR,BADSTTIM                                                   
         LH    R0,FULL                                                          
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LTR   R0,R0               TIME MUST BE HOUR ONLY                       
         BNZ   NEQXIT2                                                          
         STC   R1,HALF             SET START HOUR                               
         SPACE 1                                                                
         MVI   ERROR,BADNDTIM                                                   
         SR    R0,R0                                                            
         ICM   R0,3,FULL+2                                                      
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LTR   R0,R0                                                            
         BNZ   NEQXIT2                                                          
         STC   R1,HALF+1           SET END HOUR                                 
         B     EQXIT2                                                           
         EJECT                                                                  
*              EDIT OPTIONS                                                     
         SPACE 3                                                                
         EJECT                                                                  
*        INDEX DEMO DISTRIBUTION TO UNIVERSE DISTRIBUTION                       
         USING TDD,R4                                                           
INDEXU   ZIC   R6,NDEMOS                                                        
         BCTR  R6,0                SET TO BYPASS FIRST                          
         LA    R3,6(R3)                                                         
         XC    THISDEM,THISDEM     SET L12+ FOR UNIVERSE                        
         MVC   THISDEM+1(3),THISLINE+(TDVLMF-TDD)                               
         LA    R7,THISLINE+(TDVLMF-TDD)+3                                       
         LA    R8,TDVLMF+3         DEMOS FOR STATION                            
         SR    R2,R2                                                            
         ICM   R2,7,TDVLMF         DEMO FOR STATION                             
         LTR   R2,R2                                                            
         BZ    NEQXIT2                                                          
*                                                                               
INDEXU1  SR    RE,RE                                                            
         ICM   RE,7,0(R7)          UNIVERSE                                     
         SRDA  RE,32                                                            
         M     RE,=F'1000'                                                      
         SLDA  RE,1                                                             
         OC    THISDEM,THISDEM                                                  
         BNZ   NOTDIV02                                                         
         SR    RE,RE                                                            
         L     RF,=X'FFFFFFFF'                                                  
         B     *+8                                                              
NOTDIV02 D     RE,THISDEM                                                       
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         LR    R1,RF               SAVE THE UNIVERSE INDEX                      
         SR    RE,RE                                                            
         ICM   RE,7,0(R8)          STATION DEMOS                                
         SRDA  RE,32                                                            
         M     RE,=F'1000'                                                      
         SLDA  RE,1                                                             
         DR    RE,R2                                                            
         A     RF,=F'1'                                                         
         SRA   RF,1                HAVE DEMO INDEX HERE                         
         SR    RE,RE                                                            
         M     RE,=F'100'                                                       
         SLDA  RE,1                                                             
         LTR   R1,R1               SEE IF DIVISION BY ZERO                      
         BNZ   NOTDIV00                                                         
         SR    RE,RE                                                            
         L     RF,=X'FFFFFFFF'                                                  
         B     *+6                                                              
NOTDIV00 DR    RE,R1                                                            
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         LR    R1,RF                                                            
         EDIT  (R1),(5,0(R3))      EDIT TO NO DECIMALS                          
         LA    R3,6(R3)                                                         
         LA    R8,3(R8)                                                         
         LA    R7,3(R7)                                                         
         LTR   R6,R6                                                            
         BZ    EQXIT2                                                           
         BCT   R6,INDEXU1                                                       
         B     EQXIT2                                                           
         DROP  R4                                                               
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
*------------------------------------------------------------------             
* GOTTAMKT, SAVE MARKET NUMBER FOR EDMLM                                        
*------------------------------------------------------------------             
GOTTAMKT NTR1                                                                   
         ZIC   R4,NMARKET          KEEP TRACK OF # OF MARKETS                   
         LA    R4,1(R4)                                                         
         STC   R4,NMARKET                                                       
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
         MVC   MAXBOOK(1),HRSBOOK  SET NUMBER OF BOOKS                          
         NI    MAXBOOK,X'0F'                                                    
         CLI   MAXBOOK,4                                                        
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
         CLC   NBOOKS,MAXBOOK      NOT ENOUGH BOOKS                             
         BNE   FMULTBKX                                                         
*                                                                               
         ZIC   R0,MAXBOOK          DON'T ALLOW PREV. YEAR                       
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
         STC   R1,MAXBOOK                                                       
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
         CLC   NBOOKS,MAXBOOK      BOOK TABLE FULL - NEED A SLIDE               
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
ERRSTA   DC    C'INVALID STATION OR DAYPART '                                   
ERRLEN   DC    C'KEYWORD OR STATION TOO LONG '                                  
ERRCBL   DC    C'COMBO LIST STATION(S) INVALID, PRESS PF4 FOR AUTO-DELEX        
               TION'                                                            
NEEDKW   DC    C'KEYWORD MISSING '                                              
NEEDMKT  DC    C'MARKET MISSING '                                               
NEEDSL   DC    C'INVALID STATION '                                              
TEMPNAME DC    CL5' '                                                           
CMBELMER DC    C'COULD NOT FIND COMBO ELEMENT'                                  
GOTMATCH DC    C' '                                                             
FTYP     DC    X'00'                                                            
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
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
MAXDEMOS EQU   X'13'                                                            
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
         SPACE 2                                                                
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
RSUB     USING RREPSUB,R6                                                       
         LLC   R0,RSUB.RREPSCNT    # OF SUBSIDS                                 
         LA    R6,RSUB.RREPSCOD                                                 
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
         DROP  RSUB                                                             
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
SUBR08   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SR08**,RA,RR=R7                                              
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
HH2E     EQU   (HH2#-*)/4+1                                                     
         SPACE 2                                                                
HH2#     B     HH2           1     HEAD HOOK PART 2                             
         SPACE 2                                                                
HH2      MVC   BLOCK(42),SPACES    MARKET NUMBER AND NAME                       
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
         MVC   H6+8(24),HRSSTAT                                                 
         CLI   ONEDPT,0                                                         
         BNE   HKTITLE                                                          
         MVC   H6(7),=C'STATION'                                                
         XC    H6+8(24),H6+8                                                    
         CLI   COMBSW,C'Y'                                                      
         BNE   NOTCBSTA                                                         
         L     R2,ULPNTR                                                        
         ZIC   R3,1(R2)                                                         
         AR    R3,R2               ADDRESS OF NEXT RECORD                       
         LA    R4,H6+8                                                          
         LA    R2,11(R2)                                                        
CMB_2STA CR    R2,R3                                                            
         BNL   HKTITLE                                                          
         MVC   0(4,R4),0(R2)                                                    
         MVI   4(R4),C'-'                                                       
         CLI   4(R2),C'B'                                                       
         BNE   CBSNOAA                                                          
         MVC   5(2,R4),=C'AA'                                                   
         B     NXTCBSTA                                                         
CBSNOAA  CLI   4(R2),C'C'                                                       
         BNE   CBSNOCO                                                          
         MVC   5(2,R4),=C'CO'                                                   
         B     NXTCBSTA                                                         
CBSNOCO  CLI   4(R2),C'D'                                                       
         BNE   CBSNOFF                                                          
         MVC   5(2,R4),=C'FF'                                                   
         B     NXTCBSTA                                                         
CBSNOFF  CLI   4(R2),C'F'                                                       
         BNE   CBSNOFM                                                          
         MVC   5(2,R4),=C'FM'                                                   
         B     NXTCBSTA                                                         
CBSNOFM  MVC   5(2,R4),=C'AM'                                                   
NXTCBSTA LA    R2,5(R2)                                                         
         LA    R4,7(R4)                                                         
         CR    R2,R3                                                            
         BNL   CMB_2STA                                                         
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
         B     CMB_2STA                                                         
NOTCBSTA MVC   H6+8(5),ACTSTAT                                                  
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
HKTITLE  CLI   ADDROPT,C'N'                                                     
         BNE   NOTADDRO                                                         
         MVC   H2+76(32),SPACES                                                 
NOTADDRO LA    R2,H1+30            TITLE                                        
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
         CLI   DBRADEMT,C'R'                                                    
         BNE   NOCONDS                                                          
         LA    RF,CNDHDR                                                        
         MVC   H7+12(L'CNDHDR),0(RF)                                            
         LA    RF,CNDHDR2                                                       
         MVC   H8+12(L'CNDHDR2),0(RF)                                           
NOCONDS  OC    RDEMO,RDEMO                                                      
         BZ    HKTITLE1                                                         
         CLI   ONEDPT,0                                                         
         BZ    HKTITLE1                                                         
         MVC   H4+40(10),=C'RANKED BY '                                         
         GOTO1 DEMOCON,DMCB,RDEMO,(7,H4+50),DBLOCK,0                            
         CLI   DBSELSRC,C'M'       CANADIAN?                                    
         BNE   HKTITLE1                                                         
         CLI   H4+62,C'A'                                                       
         BE    HKTITLE1                                                         
         CLI   H4+62,C'M'          MSA=CMA                                      
         BNE   TITLFCA                                                          
         MVC   H4+62(3),=C'CMA'                                                 
         B     HKTITLE1                                                         
TITLFCA  MVC   H4+62(3),=C'FCA'    TSA=FCA                                      
         SPACE 1                                                                
HKTITLE1 CLI   UNIVOPT,C'Y'                                                     
         BNE   NOTUNIVY                                                         
*  PRINT LEDGER FOR UNIVERSE=Y                                                  
         MVC   H9,=CL132'1- AVE QRT HOUR AUDIENCE, 2- % AUDIENCE DISTRIX        
               BUTION TO INDEX DEMO, 3- INDEX % AUDIENCE DISTRIBUTION OX        
               VER % POPULATION DISTRIBUTION'                                   
         B     HOOKX                                                            
NOTUNIVY CLI   DAYPOPT,C'N'                                                     
         BNE   NOTUNIV2                                                         
         CLC   HRSOPT(5),=C'DAYPART'                                            
         BNE   NOTUNIV2                                                         
         MVC   H9+40(15),=C'MONDAY - SUNDAY'                                    
NOTUNIV2 CLI   SHAREOPT,C'Y'                                                    
         BNE   HOOKX                                                            
         MVC   H6+40(24),=C'AUDIENCE COMP. SHARE OF L12+'                       
         GOTO1 DEMOCON,DMCB,IDEMO,(7,H6+64),DBLOCK,0                            
         MVC   H6+72(7),=CL7' '                                                 
         ZIC   R1,HRSCATSH+5                                                    
         S     R1,=F'4'                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   H6+72(0),HRSCATS+4                                               
         LA    R1,1(R1)                                                         
         LR    R0,R1                                                            
         LA    R1,H6+72                                                         
         AR    R1,R0                                                            
         MVI   0(R1),C')'                                                       
         SPACE 1                                                                
HOOKX    B     XIT4                                                             
         SPACE 1                                                                
XIT4     XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO CONVERT BOOK FOR HEADINGS                             
         SPACE 3                                                                
         DS    0H                                                               
CONVBOOK NTR1                                                                   
         CLI   BOOKTYPE,C'B'       BLACK                                        
         BE    CNVEBTY                                                          
         CLI   BOOKTYPE,C'H'       AND HISPANIC - FUNNY BOOKS                   
         BE    CNVEBTY                                                          
         MVC   WORK(9),=C' FALL/NN '                                            
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
         B     XIT4                                                             
         SPACE 2                                                                
*NDHDR   DC    C'**CONDENSED MARKET DEMOS- EXCERCISE DISCRETION WHEN US         
*              NG THESE ESTIMATES**'                                            
CNDHDR   DC    C'DUE TO SMALLER SAMPLE SIZES IN CONDENSED MARKETS FOR TX        
               HIS DEMOGRAPHIC'                                                 
CNDHDR2  DC    C'AND/OR DAYPART THE USER SHOULD USE DISCRETION WHEN USIX        
               NG THESE ESTIMATES'                                              
*                                                                               
ARBTBL   DS    0CL6                                                             
         DC    C'ARB   '                                                        
         DC    C'RARB  '                                                        
         DC    C'DRARB '                                                        
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
ELEMDS   DSECT                                                                  
ELEMSTOP DS    C                                                                
ELEMLEN  DS    X                                                                
ELEMADDR DS    XL2                                                              
ELEMDISP DS    X                                                                
ELEMBDAT EQU   *-ELEMSTOP                                                       
ELEMDATA DS    0C                                                               
         SPACE 5                                                                
*              DSECT TO COVER ENTRY IN STANDARD DAYPART TABLE                   
         SPACE 1                                                                
STAND    DSECT                                                                  
STANDENT DS    0CL36                                                            
STANDDTS DS    XL15                UP TO 15 DAY/HOUR/HOUR                       
STANDPRG DS    XL1                 'PROGRAM' CODE                               
STANDESC DS    CL20                DESCRIPTION                                  
         SPACE 3                                                                
*              DSECT TO COVER ENTRY IN STANDARD DAYPART TABLE                   
         SPACE 1                                                                
TDD      DSECT                                                                  
TDENT    DS    0CL63                                                            
TDDPDAY  DS    C                                                                
TDDPQH   DS    C                                                                
         ORG   TDD                                                              
TDSQH    DS    CL1                                                              
TDSHR    DS    XL2                 START HOUR                                   
TDEHR    DS    XL2                 END HOUR                                     
TDCNT    DS    XL1                 STATION/BOOK COUNT                           
TDVLMF   DS    CL(3*19)            M-F DEMO VALUES                              
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
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DEALPHAMKD                                                     
       ++INCLUDE DEDBEXTRAD                                                     
       ++INCLUDE SPGENCUSDP                                                     
         EJECT                                                                  
       ++INCLUDE SPGENSTLST                                                     
       ++INCLUDE SPGENCOMBO                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
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
       ++INCLUDE SPRESDAD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPRESA3D                                                       
         EJECT                                                                  
         SPACE 2                                                                
*---------------------------------------------------------------------          
*              STORAGE FOR COMP                                                 
         SPACE 3                                                                
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
MAXBOOK  DS    XL1                 MAX BOOKS                                    
MAXRANK  DS    XL1                 MAX STATIONS TO RANK                         
TRACEOPT DS    CL1                 Y=TRACE DEMAND HOOKS                         
LEFTOPT  DS    CL1                 Y=LEFT ALIGN                                 
SPACOPT  DS    XL1                 SPACING                                      
DAYPOPT  DS    XL1                 DAYPART=NO OPTION                            
SHAREOPT DS    CL1                 SHARE=YES OPTION                             
SPILLOPT DS    CL1                 SPILL=NO OPTION                              
SIMULOPT DS    CL1                 N= SUPPRESS SIMULCAST STATIONS               
UNIVOPT  DS    CL1                 UNIV=YES OPTION                              
EXPANOPT DS    CL1                 EXPAND COMBO STATIONS                        
CALCOPT  DS    CL1                 CALCULATE IMPRESSION OPTION                  
SEPAOPT  DS    CL1                 MULTIPLE DEMO REPORTS                        
ADDROPT  DS    CL1                 USE ADDRESS IN REPORTS?                      
AUTOOPT  DS    CL1                 USE AUTO DELETE ON CMBLISTS?                 
ONEDPT   DS    CL2                                                              
NOA      DS    CL1                 NOT ON AIR FULL TIME IND.                    
SPANN    DS    CL1                 SPECIAL ANNOUNCMENT IND.                     
RELO     DS    A                                                                
RELO2    DS    A                                                                
MYBASE   DS    A                                                                
MYBASE2  DS    A                                                                
VSUBR07  DS    A                                                                
VSUBR08  DS    A                                                                
ULPNTR   DS    F                                                                
DPTLPTR  DS    F                                                                
DPTLBASE DS    F                                                                
         SPACE 1                                                                
COMBSV   DS    5F                                                               
COMBNUM  DS    C                                                                
COMBSW   DS    C                                                                
TWANUM   DS    C                                                                
COMAND2  DS    CL8                                                              
         SPACE 1                                                                
SVNDEM   DS    C                                                                
SVNSTATS DS    C                                                                
STASAVE  DS    CL5                 SAVE STATION                                 
DISP     DS    F                   DISPLACEMENT INTO PRINT LINE                 
DISPD    DS    F                   DEMO DISPLACEMENT INTO PRINT LINE            
MKTNUM   DS    H                   MARKET NUMBER SELECTED                       
ASTATS   DS    A                   ADDRESS OF STATION LIST                      
WSTATS   DS    F                   WIDTH OF STATION LIST                        
ATHISTAT DS    A                   ADDRESS OF STATION BEING PROCESSED           
THISDEM  DS    F                   BUFFER FOR STATION IN PROGRESS               
THISWT   DS    F                   BUFFER FOR STATION IN PROGRESS               
RDEMO    DS    CL3                 RANK DEMO                                    
IDEMO    DS    CL3                 INDEX DEMO                                   
UNIVBK   DS    CL11                5 BOOKS + 1                                  
UNIVS    DS    CL57                UNIVS FOR UNIV INDEX REPORT                  
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
MYWORK   DS    A                   SAVE PROGRAM NAMES FOR DAYPARTS              
THISLINE DS    XL144                                                            
COMBCNTR DS    H                                                                
NEXTCOMB DS    A                                                                
DRIVESTA DS    C                   DRIVE OPTION IN STATION                      
MEDTYPE  DS    C                                                                
SRCTYPE  DS    C                                                                
BOOKTYPE DS    C                                                                
CITYCODE DS    CL3                                                              
STATSV   DS    CL70                                                             
MISRDEMO DS    C                                                                
PREVSTA  DS    CL5                                                              
OUTAREA  DS    A                                                                
SUBSID   DS    C                   INTEREP SUBSID?                              
NMARKET  DS    X                   # OF MARKETS                                 
STAMISS  DS    C                                                                
MAINDEM  DS    C                                                                
PFKEY    DS    X                   ADJUSTED PFKEY VALUE                         
MKTSTAT  DS    CL5                 MARKET STATION FOR TOTALS                    
ADBRADIC DS    A                                                                
STALSTD  DS    C                   USED STATION LIST?                           
CNOSTA   DS    X                   NUMBER STATIONS IN COMBO                     
ACMBACC  DS    A                   A(CMBACCUM) BUFFER FOR DEMOS                 
NTIMES   DS    X                                                                
RNK      DS    C                   RANK FLAG                                    
         SPACE 1                                                                
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019SPRES30   12/09/20'                                      
         END                                                                    
