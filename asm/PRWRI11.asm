*          DATA SET PRWRI11    AT LEVEL 061 AS OF 07/17/02                      
*PHASE T40511A,*                                                                
*INCLUDE GETCOST                                                                
         TITLE 'T40511 - PROGRAM CHANGE LOG'                                    
*                                                                               
* EJOR  7/17/01  FOR YN, CLT KRM GETS AGY CODE 5                                
*                                                                               
* BOBY  3/20/01  ADD MEDIAVEST AS AGENCY                                        
*                                                                               
* BPLA  1/16/01  ADD MINDSHARE AS AGENCY                                        
*                                                                               
* BPLA  1/18/00  CHANGES FOR Y2K  - GFMON                                       
*                                                                               
* BOBY 4/05/98 DROP TYPE 'X' FROM REPORT                                        
* ROSA 10/2/90 ADD LOGIC TO READ B1X PROFILE (INVOICE MONTH DATE) L05           
* ROSA 9/25/90 ADD JWT AND FC TO AGENCY TABLE                     L04           
* ROSA 9/14/90 DO NOT INCLUDE AOR BILLS                           L03           
*                                                                               
* ROSA 9/13/90 ADD NEW "TOTAL" COLUMN AND 'X' AND 'P' TOTALS      L02           
* ROSA 6/14/90 REMOVE STORAGE TO NMOD                             L01           
         TITLE 'T40511 - GENERAL FOODS TAPE'                                    
T40511   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40511,RA                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         L     R1,TWADCONS                                                      
         L     R1,TSPFUSER-TWADCOND(R1)                                         
         ST    R1,ASAVE                                                         
         LA    RF,GFTEMP-SAVVALS(R1)                                            
         ST    RF,AGFTEMP          NEW DCB ADDRESS                              
         LA    RF,GFFILE-SAVVALS(R1)                                            
         ST    RF,AGFFILE          NEW DCB ADDRESS                              
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   CKRPMODE                                                         
*                                                                               
         L     R1,AOV1WRK                                                       
         LA    R1,24(R1)                                                        
         MVC   0(28,R1),=C'** PRWRI11 FORMUALE TABLE **'                        
         LA    R1,28(R1)                                                        
         ST    R1,AFORMTAB                                                      
         LH    RF,=Y(999*L'FORMTAB)                                             
         AR    R1,RF                POINT TO NEXT TABLE                         
         MVC   0(28,R1),=C'** PRWRI11  PRD TABLE    ***'                        
         LA    R1,28(R1)                                                        
         ST    R1,APRDTAB                                                       
         LH    RF,=Y(255*L'PRDTAB)                                              
         AR    R1,RF                POINT TO NEXT TABLE                         
         MVC   0(28,R1),=C'** PRWRI11  EST TABLE    ***'                        
         LA    R1,28(R1)                                                        
         ST    R1,AESTTAB                                                       
         LH    RF,=Y(999*L'ESTTAB)                                              
         AR    R1,RF                POINT TO NEXT TABLE                         
         MVC   0(28,R1),=C'** PRWRI11  EST TABLE    ***'                        
         LA    R1,28(R1)                                                        
         ST    R1,AESTZZT                                                       
*                                  REPORT CALLING MODE                          
CKRPMODE CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    INIT                                                             
         CLI   RPMODE,RPVAL        VALIDATION                                   
         BE    VALID                                                            
         CLI   RPMODE,RPINPUT      PRNTIO HOOK                                  
         BE    INPUT                                                            
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
         CLI   RPMODE,RPFINAL      FINAL HOOK                                   
         BE    FINAL                                                            
         CLI   RPMODE,RPRUNLST     RUNLAST                                      
         BE    LST                                                              
         B     XIT                                                              
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
NEXIT    LTR   RB,RB                                                            
         B     XIT                                                              
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
* INITIALIZATION                                                                
*                                                                               
INIT     CLI   TWAFIRST,0          TEST FIRST REQUEST                           
         BNE   INIT1                                                            
*                                                                               
         CLI   OFFLINE,C'Y'        SEE IF OFF-LINE                              
         BNE   INOCLEAR                                                         
*                                                                               
         ZAP   TAPECNT,=P'0'       YES-INITIALIZE TAPE COUNT                    
         MVI   FRSTLAST,C'Y'       REQUEST RUNFRST/RUNLAST                      
*                                                                               
         L     R0,ASAVE            SAVE INTER-REQUEST VALUES                    
         LA    R1,SAVVALSL         AND DCBS                                     
         LA    RE,SAVVALS                                                       
         LR    RF,R1                                                            
         MVC   SVAGYCD,AGENCYCD                                                 
         MVC   SVTIT,TITLE                                                      
         MVC   SVSUBTIT,SUBTITLE                                                
         MVC   SVSPLID,SPOOLID                                                  
         MVCL  R0,RE                                                            
         B     INIT2                                                            
INIT1    DS    0H                                                               
         L     RE,ASAVE            NO-RESTORE SAVED VALUES                      
         LA    RF,SAVVALL1                                                      
         LA    R0,SAVVALS                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
INIT2    OI    PBQREAD,PBQRDBLS    READ BILL RECORDS                            
         OI    PBQREAD,PBQRDBKS    READ BUCKET RECS - PASS ELEM AT              
*                                  A TIME                                       
         CLI   OFFLINE,C'Y'        IF ONLINE BYPASS                             
         BNE   INOCLEAR                                                         
         XC    PROGPROX,PROGPROX   CLEAR PROFILE                  L05           
*          DATA SET PPINF14    AT LEVEL 040 AS OF 02/08/90                      
***                           MUST READ PB1X PROFILE FOR          L05           
***                           INV MTH DISPLAY                     L05           
***            L05                                                              
         XC    WORK,WORK                                          L05           
         MVC   WORK(4),=C'PB1X'                                   L05           
         NI    WORK,X'BF'                   LOWER CASE            L05           
         MVC   WORK+4(2),PBQAGY                                   L05           
         MVC   WORK+6(1),PBMED                                   L05            
         GOTO1 GETPROF,DMCB,WORK,PROGPROX,DATAMGR             L05               
***                                                                             
         MVI   MYFIRSTH,8          SET DRIVER'S FIRST HEADLINE                  
         XC    MONTAB,MONTAB       INITIALIZE MONTH TABLE                       
         XC    SVPRD,SVPRD                                                      
         XC    SVEST,SVEST                                                      
         L     RE,APRDTAB        CLEAR PRODUCT AND ESTIMATE TABLES L01          
         LA    RF,255*L'PRDTAB                                                  
         XCEF  ,                                                                
         L     RE,AESTTAB                                                       
         LH    RF,=Y(999*L'ESTTAB)                                              
         XCEF  ,                                                                
*                                                                               
         L     RE,AESTZZT                                                       
         LH    RF,=Y(999*L'ESTTAB)                                              
         XCEF  ,                                                                
*                                                                               
         L     RE,AFORMTAB                                                      
         LH    RF,=Y(999*L'FORMTAB)                                             
         XCEF  ,                                                                
*                                                                               
INOCLEAR CLI   RPTSCRN,0           TEST USER REPORT SCREEN                      
         BE    INITX                                                            
         LA    R2,GFTTITH          TITLE                                        
         MVC   TITLE,SPACES                                                     
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
         OC    TITLE,SPACES                                                     
         GOTO1 CENTER,DMCB,TITLE,63                                             
*                                                                               
         MVI   PBQPRTYP,PBQPRMOS   INDICATE PERIOD IS FOR                       
*                                    MONTH OF SERVICE                           
INITX    B     XIT                                                              
         EJECT                                                                  
* FURTHER REQUEST VALIDATION                                                    
*                                                                               
VALID    DS    0H                  TEST PRD=POL REQUEST                         
         LA    R2,GFTMONH          VALIDATE BILLING MONTH                       
         GOTO1 ANY                                                              
*                                                                               
*        CHECK IF RFP SYMBOLIC PARAMETER ENTERED                                
*                                                                               
         OC    ARFPBLK,ARFPBLK     SKIP IF RFP NOT BEING USED                   
         BZ    VBMNRFPN                                                         
*                                                                               
         L     R4,ARFPBLK          ESTABLISH RFP BLOCK                          
         USING RFPBLK,R4                                                        
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,RFPVNUMS       NUMBER OF VIABLE SYMBOLIC NAMES              
         BZ    VBMNRFPN            NONE                                         
*                                                                               
         CLC   RFPVSYMB,WORK       MATCH INPUT TO SYMBOLIC PARAMETER            
         BE    *+10                   EXPANDED FORMAT                           
         CLC   RFPVSYME(L'RFPVSYME-1),WORK MATCH INPUT TO SYMBOLIC PARM         
         BE    *+16                  ESCAPE SEQ IN CASE LEFT OVER               
*                                     FROM LAST TRANSACTION                     
         LA    R4,RFPVSYML(R4)     BUMP TO NEXT SYMBOLIC PARAMETER              
         BCT   R0,*-24                                                          
         B     VBMNRFPN            NO SYMBOLIC PARAMETER FOUND                  
*                                                                               
         CLC   =AL2(PP#BMON),RFPVSYME+1 MUST BE BMON SYMBOLIC                   
****     CLC   =AL2(542),RFPVSYME+1 MUST BE BMON SYMBOLIC                       
         BNE   EINV                                                             
*                                                                               
         XC    8(L'GFTMON,R2),8(R2) INIT PERIOD FIELD                           
         MVC   8(L'RFPVSYME,R2),RFPVSYME REPLACE KEYWORD WITH ESC SEQ           
*                                                                               
         LA    RF,L'GFTMON         MAX LENGTH FOR PERIOD                        
         STC   RF,5(R2)            CHANGE LENGTH TO MAX INPUT FOR RFP           
         STC   RF,L'RFPVSYME-1+8(R2)   CHG LEN IN ESCAPE SEQ                    
*                                                                               
         B     VBMONX                                                           
*                                                                               
VBMNRFPN DS    0H                                                               
*                                                                               
         GOTO1 DATVAL,DMCB,(2,WORK),DUB                                         
         OC    0(4,R1),0(R1)                                                    
         BZ    EINV                                                             
         MVC   BILLMON,DUB         SAVE BILLING MONTH YYMM                      
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 DATCON,DMCB,DUB,(3,PBQBLLST)    MONTH OF BILLING                 
         MVC   DUB+4(2),=C'31'                                                  
         GOTO1 (RF),(R1),DUB,(3,PBQBLLND)      START/END DATES                  
*****                                                                           
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 (RF),(R1),DUB,(X'20',WORK)      REQUEST CARD FORMAT              
         MVC   BILLMON(4),WORK                                                  
*****                                                                           
**                                                                              
         MVC   SUBTITLE(12),=C'PERIOD FROM '                                    
         GOTO1 DATCON,DMCB,(3,PBQBST),(6,SUBTITLE+12)                           
         MVC   SUBTITLE+19(3),=C'TO '                                           
         GOTO1 DATCON,DMCB,(3,PBQBEND),(6,SUBTITLE+22)                          
*****                                                                           
**                                                                              
         OC    SUBTITLE,SPACES                                                  
         GOTO1 CENTER,DMCB,SUBTITLE,36                                          
*                                                                               
VBMONX   DS    0H                                                               
*                                                                               
         MVI   SUPTAP,C'N'                                                      
         LA    R2,GFTTAPH          OPTION TO SUPPRESS TAPE                      
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 ANY                                                              
         MVC   SUPTAP,WORK                                                      
         CLI   SUPTAP,C'Y'                                                      
         BE    XIT                                                              
         CLI   SUPTAP,C'N'                                                      
         BNE   EINV                                                             
         B     XIT                                                              
         EJECT                                                                  
* ERROR EXITS AND MESSAGES                                                      
*                                                                               
EINV     MVI   ERROR,INVALID                                                    
         B     CURSOR                                                           
*                                                                               
MYCURSOR MVI   ERROR,X'FE'                                                      
CURSOR   GOTO1 CURSERR                                                          
         EJECT                                                                  
* PRINTIO INPUT HOOK                                                            
*                                                                               
INPUT    L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   PBMODE,PBPROCCL     CLIENT FIRST                                 
         BNE   INP4                                                             
         MVI   ERRCD,0             INITIALIZE ERROR CODE                        
         MVI   AGENCYCD,C' '                                                    
*                                                                               
         CLC   PBAGY,=C'YN'        FOR YN                                       
         BNE   INP0                                                             
         CLC   PBCLT,=C'KRM'       CLT KRM GETS AGENCY CODE 5                   
         BNE   INP1                                                             
         MVI   AGENCYCD,C'5'                                                    
         B     INP4                                                             
*                                                                               
INP0     CLC   PBAGY,=C'H7'        IF MINDSHARE IS THE AGENCY                   
         BNE   INP1                                                             
         CLC   PBCLT,=C'KR '          CLIENT KR  IS JWT                         
         BE    *+10                                                             
         CLC   PBCLT,=C'KYP'          CLIENT KYP IS JWT                         
         BE    *+10                                                             
         CLC   PBCLT,=C'MG '          CLIENT MG  IS JWT                         
         BNE   *+12                                                             
         MVI   AGENCYCD,C'W'                                                    
         B     INP4                                                             
*                                                                               
         CLC   PBCLT,=C'KRG'          CLIENT KRG IS OM                          
         BNE   *+12                                                             
         MVI   AGENCYCD,C'M'                                                    
         B     INP4                                                             
*                                                                               
         OI    ERRCD,ERRAGY           ELSE INVALID AGENCY                       
         B     EINV                                                             
*                                                                               
INP1     DS    0H                                                               
*                                                                               
         LA    R1,AGYTAB           FIND GF AGENCY CODE                          
*                                                                               
INP2     CLI   0(R1),0                                                          
         BNE   *+12                                                             
         OI    ERRCD,ERRAGY                                                     
         B     EINV                WAS INP4                                     
*                                                                               
         CLC   PBAGY,0(R1)                                                      
         BE    *+12                                                             
         LA    R1,3(R1)                                                         
         B     INP2                                                             
*                                                                               
         MVC   AGENCYCD,2(R1)                                                   
*                                                                               
         B     INP4                                                             
*                                                                               
AGYTAB   DC    CL2'OM',CL1'M'                                                   
         DC    CL2'H9',CL1'Q'      MEDIAVEST                                    
         DC    CL2'YN',CL1'A'                                                   
         DC    CL2'JW',CL1'W'                                     L04           
         DC    CL2'FC',CL1'C'                                     L04           
         DC    CL2'WW',CL1'H'                                     L04           
         DC    CL2'SJ',CL1'S'   SJR TEST                                        
         DC    X'00'                                                            
*                                                                               
INP4     CLI   OFFLINE,C'Y'                                                     
         BNE   INPX                                                             
*                                                                               
         CLI   PBMODE,PBPROCBL     BILL RECORDS                                 
         BNE   CLIPBMOD                                                         
*                                                                               
         L     RF,AIO1                                            L04           
         USING PBILLRCD,RF                                        L04           
*                                                                               
*        SKIP AOR CHECK FOR YNR CLIENT KRX AND SJR                              
*                                                                               
         CLI   AGENCYCD,C'S'       SKIP IF SJR                                  
         BE    OCMONTAB                                                         
*                                                                               
         CLI   AGENCYCD,C'Y'       SKIP IF YNR AND                              
         BNE   *+14                                                             
         CLC   PBCLT,=C'KRX'       CLIENT KRX                                   
         BE    OCMONTAB                                                         
*                                                                               
         TM    PBILCMSW,X'20'      AOR BILL                       L04           
         BNO   OCMONTAB                                           L04           
*                                                                               
         MVI   PBMODE,00           CHANGE MODE TO BYPASS PROCESS  L04           
*                                  OF BILLING RECORD              L04           
         B     INPX                                               L04           
         DROP  RF                                                               
CLIPBMOD CLI   PBMODE,PBPROCBK     BUCKET RECORDS                               
         BNE   INPX                                                             
*                                  MONTAB IS A TABLE OF BINARY                  
*                                  YY/MM FROM REQ START TO REQ END              
*                                                                               
OCMONTAB OC    MONTAB,MONTAB       TEST MONTH TABLE SET YET                     
         BNZ   INP8                                                             
         LA    R3,MONTAB                                                        
         MVC   0(2,R3),PBQBST      BINARY                                       
         MVC   2(4,R3),PBQSTART    CHARACTERS                                   
*                                                                               
INP4C    MVC   FULL(2),0(R3)                                                    
         ZIC   R1,FULL+1           BUMP MONTH                                   
         LA    R1,1(R1)                                                         
         STC   R1,FULL+1                                                        
         CH    R1,=H'13'                                                        
         BL    INP5                SAME YEAR                                    
         MVI   FULL+1,1                                                         
         ZIC   R4,FULL                                                          
         LA    R4,1(R4)                                                         
         STC   R4,FULL                                                          
*                                                                               
INP5     LA    R3,6(R3)                                                         
         CLC   FULL(2),PBQBEND   CHECK VS. END YR MTH                           
         BH    INP6                                                             
         MVC   0(2,R3),FULL                                                     
         MVI   FULL+3,1                                                         
         GOTO1 DATCON,DMCB,(3,FULL),(0,DUB)                                     
         MVC   2(4,R3),DUB                                                      
         BCT   R0,INP4C                                                         
*                                                                               
INP6     MVC   0(2,R3),=X'FFFF'                                                 
*                                                                               
INP8     CLI   PBMODE,PBPROCBL     TEST BILL RECORD                             
*        BNE   INP8D                                                            
*        XC    PBQBLLST,PBQBLLST                                                
*        MVC   PBQBLLND,=X'FFFFFF'                                              
*                                                                               
INP8D    CLC   PBPROD,SVPRD       TEST CHANGE OF PRODUCT                        
         BE    INP9                                                             
*                                 MUST BUILD FOR EACH PRODUCT                   
         L     RE,AFORMTAB                                                      
         LH    RF,=Y(999*L'FORMTAB)                                             
         XCEF  ,                                                                
*                                                                               
*                                 MUST BUILD FOR EACH PRODUCT                   
         L     RE,AESTTAB                                         L01           
         LH    RF,=Y(999*L'ESTTAB)                                              
         XCEF  ,                                                                
*                                                                               
         XC    SVBFEST,SVBFEST                                                  
         BAS   RE,GETPRD           YES-GET GF CODES FOR PRODUCT                 
         MVC   SVPRD,PBPROD                                                     
*                                                                               
INP9     CLC   PBEST,SVEST        TEST CHANGE OF ESTIMATE                       
         BE    INP9D                                                            
         BAS   RE,GETEST           YES-GET GF CODES FOR ESTIMATE                
         MVC   SVEST,PBEST                                                      
*                                                                               
INP9D    CLC   PBEST,SVBFEST                                                    
         BE    INP10                                                            
         BAS   RE,GETFORM          YES-GET FORMULA FOR ESTIMATE                 
         MVC   SVBFEST,PBEST                                                    
*                                                                               
INP10    B     INPX                                                             
*                                                                               
INPX     B     XIT                                                              
         EJECT                                                                  
* GET GF PRODUCT CODES                                                          
*                         THIS ROUTINE BUILDS (IN PRDTAB)                       
*                         A TABLE OF PRODUCT CODES AND THE                      
*                         INFO FROM THE GF ESTIMATE RECS                        
*                                                                               
GETPRD   NTR1  ,                                                                
         NI    ERRCD,255-ERRPRD                                                 
         MVC   COMPYDIV,SPACES                                                  
         MVC   PRODCODE,SPACES                                                  
         L     R6,APRDTAB                                         L01           
GETP10   CLC   0(R6),0          END OF TABLE                                    
         BE    GETP20                                                           
         CLC   0(3,R6),PBPROD                                                   
         BNE   GETP15                                                           
         MVC   COMPYDIV,3(R6)                                                   
         MVC   PRODCODE,3+L'COMPYDIV(R6)                                        
         B     GETP30                                                           
*                                                                               
GETP15   LA    R6,L'PRDTAB(R6)                                                  
         B     GETP10                                                           
*                                                                               
GETP20   XC    KEY,KEY             NO-READ GF ESTIMATE RECORD                   
         LA    R5,KEY                 WITH ESTIMATE=0                           
         USING GFESTRD,R5                                                       
         MVC   PGSTKAGY,AGENCY                                                  
         MVC   PGSTKMED,PBMED                                                   
         MVI   PGSTKRCD,X'0B'                                                   
         MVC   PGSTKCLT,PBCLT                                                   
         MVC   PGSTKPRD,PBPROD                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(PGSTKEST-PGSTKEY),KEYSAVE   RECORD FOUND                     
         BNE   GETP27                                                           
         L     R5,AIO3                                                          
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         LA    R5,PGSTKEDQ(R5)     SCAN ELEMENTS FOR                            
         SR    R0,R0               DIVISION/BRAND AND PRODUCT CODE              
*                                                                               
GETP22   CLI   0(R5),0                                                          
         BE    GETP27                                                           
         CLI   0(R5),PGSTEIDQ                                                   
         BNE   GETP26                                                           
         USING PGSTELEM,R5                                                      
         LA    R1,COMPYDIV                                                      
         LA    RE,L'COMPYDIV-1                                                  
         CLC   PGSTNAME,QCOMPDIV                                                
         BE    GETP24                                                           
         LA    R1,PRODCODE                                                      
         LA    RE,L'PRODCODE-1                                                  
         CLC   PGSTNAME,QPRDCODE                                                
         BNE   GETP26                                                           
*                                                                               
GETP24   EX    RE,*+4                                                           
         MVC   0(0,R1),PGSTDATA                                                 
*                                                                               
GETP26   IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     GETP22                                                           
*                                                                               
GETP27   MVC   0(3,R6),PBPROD                                                   
         MVC   3(L'COMPYDIV,R6),COMPYDIV                                        
         MVC   3+L'COMPYDIV(L'PRODCODE,R6),PRODCODE                             
*                                                                               
         MVC   KEY,IOKEYSVE                                                     
         GOTO1 HIGH                                                             
*                                                                               
GETP30   CLC   COMPYDIV,SPACES     TEST FOR PRODUCT ERRORS                      
         BNH   GETP40                                                           
         CLC   PRODCODE,SPACES                                                  
         BH    GETPX                                                            
*                                                                               
GETP40   OI    ERRCD,ERRPRD                                                     
*                                                                               
GETPX    B     XIT                                                              
         EJECT                                                                  
* GET GF ESTIMATE CODES                                                         
*        ESTTAB IS NOW SET FOR 999 ESTIMATES SO INDEX LOGIC                     
*        CAN REMAIN THE SAME                                                    
*                                                                               
GETEST   NTR1  ,                                                                
***********                                                                     
********** USING ZZZ FOR NATURAL AND SUBNATURAL                                 
************                                                                    
         B     GETZZZE                                                          
***************************************************                             
* * * * * * * * * * * * * * *                                                   
***************************************************                             
         NI    ERRCD,255-ERREST                                                 
         MVC   NATURAL,SPACES                                                   
         MVC   SUBNATRL,SPACES                                                  
         MVC   HALF,PBEST                                                       
         LH    R6,HALF                                                          
         BCTR  R6,0                                                             
         MH    R6,=Y(L'ESTTAB)                                                  
         L     RE,AESTTAB                                                       
         AR    R6,RE                                                            
*        LA    R6,ESTTAB(R6)                                                    
         OC    0(L'ESTTAB,R6),0(R6)   TEST CODES FOUND YET                      
         BZ    GETE1                                                            
         MVC   NATURAL,0(R6)       YES                                          
         MVC   SUBNATRL,L'NATURAL(R6)                                           
         B     GETE30                                                           
*                                                                               
GETE1    XC    KEY,KEY             NO-READ GF ESTIMATE RECORD                   
         LA    R5,KEY                                                           
         USING GFESTRD,R5                                                       
         MVC   PGSTKAGY,AGENCY                                                  
         MVC   PGSTKMED,PBMED                                                   
         MVI   PGSTKRCD,X'0B'                                                   
         MVC   PGSTKCLT,PBCLT                                                   
         MVC   PGSTKPRD,PBPROD                                                  
         MVC   PGSTKEST,PBEST                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(PGSTKLNQ),KEYSAVE   TEST RECORD FOUND                        
         BNE   GETE7                                                            
         L     R5,AIO3                                                          
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         LA    R5,PGSTKEDQ(R5)     SCAN ELEMENTS FOR                            
         SR    R0,R0               NATURAL AND SUB-NATURAL CODES                
*                                                                               
GETE2    CLI   0(R5),0                                                          
         BE    GETE7                                                            
         CLI   0(R5),PGSTEIDQ                                                   
         BNE   GETE6                                                            
         USING PGSTELEM,R5                                                      
         LA    R1,NATURAL                                                       
         LA    RE,L'NATURAL-1                                                   
         CLC   PGSTNAME,QNATURAL                                                
         BE    GETE4                                                            
         LA    R1,SUBNATRL                                                      
         LA    RE,L'SUBNATRL-1                                                  
         CLC   PGSTNAME,QSUBNATL                                                
         BNE   GETE6                                                            
*                                                                               
GETE4    EX    RE,*+4                                                           
         MVC   0(0,R1),PGSTDATA                                                 
*                                                                               
GETE6    IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     GETE2                                                            
*                                                                               
GETE7    MVC   0(L'NATURAL,R6),NATURAL                                          
         MVC   L'NATURAL(L'SUBNATRL,R6),SUBNATRL                                
*                                                                               
*                                                                               
GETE29   MVC   KEY,IOKEYSVE                                                     
         GOTO1 HIGH                                                             
*                                                                               
*                                                                               
GETE30   CLC   NATURAL,SPACES      TEST FOR ESTIMATE ERRORS                     
         BNH   GETE40                                                           
         CLC   SUBNATRL,SPACES                                                  
         BH    GETEX                                                            
*                                                                               
GETE40   B     GETZZZE                                                          
*                                                                               
GETEX    B     XIT                                                              
*                                                                               
*                                                                               
*                                                                               
GETZZZE  DS    0H               LOOK FOR ZZZ ESTIMATES                          
         NI    ERRCD,255-ERREST                                                 
         MVC   NATURAL,SPACES                                                   
         MVC   SUBNATRL,SPACES                                                  
         MVC   HALF,PBEST                                                       
         LH    R6,HALF                                                          
         BCTR  R6,0                                                             
         MH    R6,=Y(L'ESTTAB)                                                  
         A     R6,AESTZZT                                                       
         OC    0(L'ESTTAB,R6),0(R6)   TEST CODES FOUND YET                      
         BZ    GETE1ZZ                                                          
         MVC   NATURAL,0(R6)       YES                                          
         MVC   SUBNATRL,L'NATURAL(R6)                                           
         B     GETE30ZZ                                                         
*                                                                               
GETE1ZZ  XC    KEY,KEY             NO-READ GF ESTIMATE RECORD                   
         LA    R5,KEY                                                           
         USING GFESTRD,R5                                                       
         MVC   PGSTKAGY,AGENCY                                                  
         MVC   PGSTKMED,PBMED                                                   
         MVI   PGSTKRCD,X'0B'                                                   
         MVC   PGSTKCLT,PBCLT                                                   
         MVC   PGSTKPRD,=C'ZZZ'       USE PRODUCT ZZZ                           
         MVC   PGSTKEST,PBEST                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(PGSTKLNQ),KEYSAVE   TEST RECORD FOUND                        
         BNE   GETE40ZZ                                                         
         L     R5,AIO3                                                          
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         LA    R5,PGSTKEDQ(R5)     SCAN ELEMENTS FOR                            
         SR    R0,R0               NATURAL AND SUB-NATURAL CODES                
*                                                                               
GETE2ZZ  CLI   0(R5),0                                                          
         BE    GETE7ZZ                                                          
         CLI   0(R5),PGSTEIDQ                                                   
         BNE   GETE6ZZ                                                          
         USING PGSTELEM,R5                                                      
         LA    R1,NATURAL                                                       
         LA    RE,L'NATURAL-1                                                   
         CLC   PGSTNAME,QNATURAL                                                
         BE    GETE4ZZ                                                          
         LA    R1,SUBNATRL                                                      
         LA    RE,L'SUBNATRL-1                                                  
         CLC   PGSTNAME,QSUBNATL                                                
         BNE   GETE6ZZ                                                          
*                                                                               
GETE4ZZ  EX    RE,*+4                                                           
         MVC   0(0,R1),PGSTDATA                                                 
*                                                                               
GETE6ZZ  IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     GETE2ZZ                                                          
*                                                                               
GETE7ZZ  MVC   0(L'NATURAL,R6),NATURAL                                          
         MVC   L'NATURAL(L'SUBNATRL,R6),SUBNATRL                                
*                                                                               
*                                                                               
GETE29ZZ MVC   KEY,IOKEYSVE                                                     
         GOTO1 HIGH                                                             
*                                                                               
*                                                                               
GETE30ZZ CLC   NATURAL,SPACES      TEST FOR ESTIMATE ERRORS                     
         BNH   GETE40ZZ                                                         
         CLC   SUBNATRL,SPACES                                                  
         BH    GETEXZZ                                                          
*                                                                               
GETE40ZZ OI    ERRCD,ERREST                                                     
*                                                                               
GETEXZZ  B     XIT                                                              
         EJECT                                                                  
* GET BILLING FORMULA                                                           
*       FORMTAB IS NOW SET FOR 999 ESTIMATES SO INDEX LOGIC                     
*        CAN REMAIN THE SAME                                                    
*                                                                               
GETFORM  NTR1  ,                                                                
*        NI    ERRCD,255-ERREST                                                 
         MVI   ETEST,0                                                          
         XC    EBFDATE,EBFDATE                                                  
         XC    EBFORM,EBFORM                                                    
         MVC   HALF,PBEST                                                       
         LH    R6,HALF                                                          
         BCTR  R6,0                                                             
         MH    R6,=Y(L'FORMTAB)                                                 
         A     R6,AFORMTAB                                                      
*        LA    R6,FORMTAB(R6)                                                   
         OC    0(L'FORMTAB,R6),0(R6)  TEST CODES FOUND YET                      
         BZ    GETF1                                                            
         MVC   ETEST,0(R6)       YES                                            
         MVC   EBFORM,1(R6)                                                     
         MVC   EBFDATE,6(R6)                                                    
         B     GETF30                                                           
*                                                                               
GETF1    DS    0H                                                               
GETF20   DS    0H                                                               
*                                                                               
         XC    KEY,KEY             READ ESTIMATE RECORD                         
         LA    R5,KEY                                                           
         USING PESTRECD,R5                                                      
         MVC   PESTKAGY,AGENCY                                                  
         MVC   PESTKMED,PBMED                                                   
         MVI   PESTKRCD,X'07'                                                   
         MVC   PESTKCLT,PBCLT                                                   
         MVC   PESTKPRD,PBPROD                                                  
         MVC   PESTKEST,PBEST                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE   TEST RECORD FOUND                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,AIO3                                                          
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         OC    0(1,R6),PESTTEST   TEST EST BIT                                  
         MVC   1(5,R6),PESTBILP                                                 
         MVC   6(3,R6),PESTBILP+5   EFF DATE                                    
         MVC   ETEST,PESTTEST                                                   
         MVC   EBFDATE,PESTBILP+5     EFFECTIVE DATE                            
         MVC   EBFORM,PESTBILP                                                  
         OC    PESTBILP(5),PESTBILP   CHK FOR EST LEVEL FORMULA                 
         BNZ   GETF29                                                           
*        NEXT TRY FOR AAA ESTIMATE FORMULA                                      
*                                                                               
         XC    KEY,KEY             READ ESTIMATE RECORD                         
         MVC   KEY(15),PESTREC                                                  
         LA    R5,KEY                                                           
         USING PESTRECD,R5                                                      
         MVC   PESTKPRD,=C'AAA'                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE   TEST RECORD FOUND                              
         BNE   GETF25                                                           
         L     R5,AIO3                                                          
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         OC    0(1,R6),PESTTEST   TEST EST BIT                                  
         MVC   1(5,R6),PESTBILP                                                 
         MVC   6(3,R6),PESTBILP+5     EFF DATE                                  
         MVC   ETEST,PESTTEST                                                   
         MVC   EBFDATE,PESTBILP+5     EFFECTIVE DATE                            
         MVC   EBFORM,PESTBILP                                                  
         OC    PESTBILP(5),PESTBILP   CHK FOR EST LEVEL FORMULA                 
         BNZ   GETF29                                                           
*                                                                               
*        NEXT TRY FOR PRODUCT FORMULA                                           
*                                                                               
GETF25   XC    KEY,KEY             READ PRODUCT RECORD                          
         L     R5,AIO3             HAS ESTREC OR AAA EST REC                    
         MVC   KEY(10),PESTREC                                                  
         LA    R5,KEY                                                           
         USING PPRDRECD,R5                                                      
         MVI   PPRDKRCD,X'06'                                                   
         MVC   PPRDKPRD,PBPROD                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE   TEST RECORD FOUND                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,AIO3                                                          
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   1(5,R6),PPRDBILP                                                 
         MVC   6(3,R6),PPRDBILP+5     EFF DATE                                  
         MVC   EBFDATE,PPRDBILP+5     EFFECTIVE DATE                            
         MVC   EBFORM,PPRDBILP                                                  
         OC    PPRDBILP(5),PPRDBILP   CHK FOR EST LEVEL FORMULA                 
         BNZ   GETF29                                                           
*        LAST TRY FOR PRD=AAA FORMULA                                           
*                                                                               
         XC    KEY,KEY             READ PRODUCT RECORD                          
         MVC   KEY(10),PPRDREC                                                  
         LA    R5,KEY                                                           
         USING PPRDRECD,R5                                                      
         MVC   PPRDKPRD,=C'AAA'                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE   TEST RECORD FOUND                              
         BNE   GETF28                                                           
         L     R5,AIO3                                                          
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   1(5,R6),PPRDBILP                                                 
         MVC   6(3,R6),PPRDBILP+5     EFF DATE                                  
         MVC   EBFDATE,PPRDBILP+5     EFFECTIVE DATE                            
         MVC   EBFORM,PPRDBILP                                                  
         OC    PPRDBILP(5),PPRDBILP   CHK FOR EST LEVEL FORMULA                 
         BNZ   GETF29                                                           
*                                                                               
GETF28   MVC   EBFORM,=X'0505000000'     SET DEFAULT TO G-CD                    
         MVC   1(5,R6),EBFORM             MOVE TO TABLE                         
*                                                                               
GETF29   MVC   KEY,IOKEYSVE                                                     
         GOTO1 HIGH                                                             
*                                                                               
*                                                                               
GETF30   B     GETFX               TEST FOR ESTIMATE ERRORS                     
*                                                                               
GETFX    B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK                                                                   
*                                                                               
DRHKNTR  NTR1  ,                                                                
*                                                                               
DRHOOK   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLINIT       DRIVER INITIALIZATION                        
         BE    DRVINIT                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLPUTSRT     PUT TO SORT                                  
         BE    PUTSRT                                                           
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         BE    HEADHK                                                           
         CLI   GLHOOK,GLPRINT      PRINT A LINE                                 
         BE    PRINT                                                            
*                                                                               
DRHOOKX  B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                                      
*                                                                               
RESOLVE  LA    R1,RTNLIST          SEARCH LIST FOR ROUTINE NAME                 
*                                                                               
RESOLVE2 CLI   0(R1),X'FF'                                                      
         BE    XIT                                                              
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
         MVC   GLAROUT,8(R1)       YES-RETURN ADDRESS                           
         B     XIT                                                              
         SPACE 1                                                                
RTNLIST  DS    0F                                                               
         DC    CL8'IKEY    ',A(IKEY)     GF REPORT                              
         DC    CL8'OKEY    ',A(OKEY)                                            
         DC    CL8'INET    ',A(INET)                                            
         DC    CL8'ONET    ',A(ONET)                                            
         DC    CL8'ICOM    ',A(ICOM)                                            
         DC    CL8'OCOM    ',A(OCOM)                                            
         DC    CL8'IDATE   ',A(IDATE)                                           
         DC    CL8'ODATE   ',A(ODATE)                                           
         DC    CL8'OFILLER ',A(OFILLER)                                         
         DC    CL8'IERR    ',A(IERR)                                            
         DC    CL8'OERR    ',A(OERR)                                            
         DC    CL8'LASTCOL ',A(LASTCOL)                                         
*                                                                               
         DC    CL8'IAGENCY ',A(IAGENCY)  AGENCY REPORT                          
         DC    CL8'OAGENCY ',A(OAGENCY)                                         
         DC    CL8'OMEDIA  ',A(OMEDIA)                                          
         DC    CL8'ICOMPANY',A(ICOMPANY)                                        
         DC    CL8'IDIV    ',A(IDIV)                                            
         DC    CL8'IPRDCODE',A(IPRDCODE)                                        
         DC    CL8'IESTIM  ',A(IESTIM)                                          
         DC    CL8'IMONTH  ',A(IMONTH)                                          
         DC    CL8'OMONTH  ',A(OMONTH)                                          
         DC    CL8'IINV    ',A(IINV)                                            
         DC    CL8'ITYPE   ',A(ITYPE)                                           
         DC    CL8'INAT    ',A(INAT)                                            
         DC    CL8'ISUB    ',A(ISUB)                                            
         DC    CL8'IMARKET ',A(IMARKET)                                         
         DC    CL8'I2NET   ',A(I2NET)                                           
         DC    CL8'O2NET   ',A(O2NET)                                           
         DC    CL8'LASTONE ',A(LASTONE)                                         
         DC    CL8'I2COM   ',A(I2COM)                                           
         DC    CL8'O2COM   ',A(O2COM)                                           
         DC    CL8'I2TOT   ',A(I2TOT)                             L02           
         DC    CL8'O2TOT   ',A(O2TOT)                             L02           
         DC    CL8'TOTAL   ',A(TOTAL)                                           
         DC    X'FF'                                                            
         EJECT                                                                  
* DRIVER INITIALIZATION                                                         
*                                                                               
DRVINIT  OI    GLINDS,GLPALDET     PRINT ALL DETAILS                            
         MVI   GLOPTS+2,1          REPORT 1 = TAPE RECORD MAP                   
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK TO EXECUTE ROUTINES                                               
*                                                                               
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         L     R6,PBGRS      PBGRS SHOULD HAVE ADDRESS OF BKELEM                
         USING BKELEM,R6                                                        
         L     R5,PBAIO1                                                        
         USING PBILLRCD,R5                                                      
         CLI   GLMODE,GLINPUT      TEST INPUT PHASE                             
         BNE   EXEC2                                                            
*******  MVI   INDATA,1            YES-ALL DATA IS SIGNIFICANT                  
         L     R1,GLADTENT                                                      
         CLI   DRINLEV-DRIND(R1),1 TEST LEVEL 1                                 
         BH    EXEC2                                                            
         MVI   REJECT,C'N'         YES-RESET THE REJECT SWITCH                  
         MVI   MONEY,C'N'          RESET MONEY SWITCH                           
*                                                                               
EXEC2    L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                                                               
         EJECT                                                                  
* I/O ROUTINES                                                                  
*                                                                               
IKEY     CLI   PBMODE,PBPROCBK     TEST BUCKET ELEM                             
         BNE   IKEY5                                                            
********                                                                        
         CLC   PBPROD,SVPRD        YES-TEST CHANGE OF PRODUCT                   
         BE    IKEY0                                                            
         BAS   RE,GETPRD           YES-GET GF CODES FOR PRODUCT                 
         MVC   SVPRD,PBPROD                                                     
IKEY0    MVC   GFCOMDIV,COMPYDIV                                                
         MVC   GFPRODCD,PRODCODE                                                
         MVC   GFAGY,AGENCYCD                                                   
*                                                                               
         CLI   GFAGY,C'A'          FOR YNR                                      
         BNE   IKEY1                                                            
*                                                                               
         CLC   PBCLT,=CL3'KRB'     CLIENT KRB HAS ITS OWN AGENCYCD              
         BE    *+10                                                             
         CLC   PBCLT,=CL3'KRX'     CLIENT KRX HAS SAME    AGENCYCD              
         BNE   *+8                                                              
         MVI   GFAGY,C'5'                                                       
*                                                                               
IKEY1    DS    0H                                                               
*                                                                               
         MVC   GFEST,SPACES                                                     
         MVC   GFEST(3),PBCLT                                                   
         MVC   GFEST+3(3),PBPROD                                                
         LH    R0,PBEST                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  GFEST+6(3),DUB                                                   
         LA    R1,MONTAB                                                        
IKEY3    CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BKYM,0(R1)                                                       
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   BKYM,0(R1)                                                       
         BNH   *+12                                                             
         LA    R1,6(R1)                                                         
         B     IKEY3                                                            
         MVC   GFMON,2(R1)                                                      
*                                                                               
         MVC   WORK(4),GFMON                                                    
         MVC   WORK+4(2),=C'01'      DAY TO 01                                  
         GOTO1 DATCON,DMCB,(0,WORK),(X'20',DUB)                                 
         MVC   GFMON(4),DUB                                                     
*                                                                               
         MVC   GFINV,XFF                                                        
         MVI   GFBLTYPE,C'X'                                                    
         B     IKEY10                   REST SAME AS IKEY4                      
*                                                                               
*****************************************                                       
IKEY5    CLI   PBMODE,PBPROCBL          PBILLREC                                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   GFCOMDIV,COMPYDIV                                                
         MVC   GFPRODCD,PRODCODE                                                
         MVC   GFAGY,AGENCYCD                                                   
*                                                                               
         CLI   GFAGY,C'A'          FOR YNR                                      
         BNE   IKEY6                                                            
*                                                                               
         CLC   PBCLT,=CL3'KRB'     CLIENT KRB HAS ITS OWN AGENCYCD              
         BE    *+10                                                             
         CLC   PBCLT,=CL3'KRX'     CLIENT KRX HAS SAME    AGENCYCD              
         BNE   *+8                                                              
         MVI   GFAGY,C'5'                                                       
*                                                                               
IKEY6    DS    0H                                                               
*                                                                               
         MVC   GFEST,SPACES                                                     
         MVC   GFEST(3),PBCLT                                                   
         MVC   GFEST+3(3),PBPROD                                                
         LH    R0,PBEST                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  GFEST+6(3),DUB                                                   
*                                                                               
         MVC   FULL(2),PBILKMOS                                                 
         MVI   FULL+2,1                                                         
******   GOTO1 DATCON,DMCB,(3,FULL),DUB                                         
         GOTO1 DATCON,DMCB,(3,FULL),(X'20',DUB)                                 
         MVC   GFMON,DUB                                                        
         MVC   GFINV(1),PBMED      INVOICE NUMBER                               
         MVI   GFINV+1,C'/'                                                     
         ZIC   R0,PBILKBMN+1                                                    
*        CVD   R0,DUB                                                           
*        OI    DUB+7,X'0F'                                                      
*        UNPK  GFINV+2(2),DUB                                                   
*        MVI   GFINV+4,C'/'                                                     
*        SR    RE,RE                                                            
*        ICM   RE,3,PBILKBNO                                                    
*        CVD   RE,DUB                                                           
*        OI    DUB+7,X'0F'                                                      
*        UNPK  GFINV+5(4),DUB                                                   
         BAS   RE,INVOICE                                         L05           
         MVI   GFBLTYPE,C'P'                                                    
*                                                                               
IKEY10   MVC   GFNAT,NATURAL                                                    
         MVC   GFSUBNAT,SUBNATRL                                                
         MVC   GFMKT,SPACES                                                     
         MVC   0(L'GFKEY,R2),GFKEY                                              
         B     XIT                                                              
*                                                                               
OKEY     MVC   GFREC,SPACES        INIT GF RECORD TO SPACES                     
         MVC   GFKEY,0(R2)                                                      
         CLC   GFINV,XFF           TEST BUY RECORD DATA                         
         BNE   *+10                                                             
         MVC   GFINV,SPACES        YES-INVOICE=SPACES                           
         MVC   0(L'GFKEY,R3),GFKEY  GF KEY OUTPUT                               
         MVI   REJECT,C'N'                                                      
         MVI   MONEY,C'N'                                                       
         CLI   GFBLTYPE,C'P'       TEST TYPE=BILL                               
         BNE   *+14                                                             
         MVC   SVKEY,GFKEY         YES-SAVE LATEST BILL KEY                     
         B     XIT                                                              
         CLI   GFBLTYPE,C'X'       TEST TYPE=BUY                                
         BNE   XIT                                                              
******** CLC   GFMON,BILLMON                                                    
******** BL    *+14                                                             
******** CLC   GFMON,BILLMON       YES-REJECT ANY MONTH PRIOR TO                
******** BL    *+14                    BILLING MONTH                            
******** CLC   SVKEY,GFKEY         TEST KEY SAME AS LAST BILL KEY               
******** BNE   XIT                                                              
         MVI   REJECT,C'Y'         YES-REJECT                                   
         B     XIT                                                              
*                                                                               
INET     CLI   PBMODE,PBPROCBL     BILL RECORD -                                
         BNE   INET5                                                            
         TM    PBILCMSW,X'20'      SKIP IF AOR BILL                             
         BO    *+10                                               L04           
         ZAP   0(8,R2),PBILLNET    NET INVOICE AMOUNT                           
*                                                                               
         B     INETX                                                            
*                                                                               
INET5    CLI   PBMODE,PBPROCBK     BUCKET ELEM                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   DUB,BKONET                                                       
         SP    DUB,BKOCD                                                        
         ZAP   0(8,R2),DUB         NET-CD ODRDERED                              
INETX    B     INETCOMX                                                         
*                                                                               
*                                  OUTPUT NET DOLLARS                           
ONET     DS    0H                                                               
         ZAP   GFPNET,0(8,R2)      NET DOLLARS TO TEMP RECORD                   
         BZ    *+8                                                              
         MVI   MONEY,C'Y'                                                       
*                                                                               
         UNPK  GFNET,GFPNET                                                     
         MVC   0(L'GFNET,R3),GFNET                                              
*                                                                               
         B     XIT                                                              
*                                                                               
ICOM     CLI   PBMODE,PBPROCBL     COMMISSION  - PBILLREC                       
         BNE   ICOM2                                                            
*                                                                               
* DETERMINE IF CD HAS BEEN SUBTRACTED FROM RECEIVABLE                           
*                                                                               
         ZAP   WORK(8),=P'0'                                                    
*                                                                               
         CLI   GFAGY,C'W'          IF AGENCY IS JWT                             
         BNE   BYSUBCD                                                          
*                                                                               
         TM    PBILBASA,4                                                       
         BO    BYSUBCD                                                          
*                                                                               
         ZAP   WORK(8),PBILLGRS      DETERMINE CD(GROSS)                        
         SP    WORK(8),PBILLBIL       LESS GROSS-CD   -- RESULT = CD            
*                                                                               
BYSUBCD  ZAP   DUB,PBILLRCV                                                     
         SP    DUB,WORK(8)         REDUCE RECEIVABLE BY CD                      
*                                                                               
         TM    PBILCMSW,X'20'      SKIP IF AOR BILL                             
         BO    *+10                                               L04           
         SP    DUB,PBILLNET        GROSS-AC-CD                                  
*                                                                               
         ZAP   0(8,R2),DUB                                                      
         B     ICOMX                                                            
*                                                                               
ICOM2    CLI   PBMODE,PBPROCBK     COMMISSION - BUCKET ELEM                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* DETERMINE IF CD HAS BEEN SUBTRACTED FROM RECEIVABLE                           
*                                                                               
         ZAP   DUB,BKOGRS          GROSS                                        
         CVB   RF,DUB                                                           
         ST    RF,WORK                                                          
         ZAP   DUB,BKOCD           CD                                           
         CVB   RF,DUB                                                           
         ST    RF,WORK+4                                                        
         ZAP   DUB,BKOGRS          GROSS                                        
         SP    DUB,BKONET          MINUS NET = AC                               
         CVB   R0,DUB                                                           
         ST    R0,WORK+8                                                        
*                                                                               
         MVC   MYWORK,EBFORM                                                    
         CLI   GFAGY,C'W'                                                       
         BNE   ICOMZ                                                            
         CLI   EBFORM,X'01'        GROSS                                        
         BE    SEEIFZ                                                           
         CLI   EBFORM,X'02'        NET                                          
         BNE   ICOMZ                                                            
SEEIFZ   OC    EBFORM+2(3),EBFORM+2  ENSURE PERCENT IS ZERO                     
         BNZ   ICOMZ                                                            
         MVI   MYWORK,X'06'        NET-CD                                       
*                                                                               
ICOMZ    GOTO1 =V(GETCOST),DMCB,MYWORK,WORK,(C'C',SPACES)                       
*                                                                               
         L     RF,DMCB+4           TRUE AGENCY COST                             
         CVD   RF,DUB                                                           
         SP    DUB,BKONET                                                       
         ZAP   0(8,R2),DUB                                                      
*                                                                               
         ICM   RF,15,WORK+4        CD                                           
         CVD   RF,DUB                                                           
         AP    0(8,R2),DUB         ADD CD BACK IN                               
*                                                                               
ICOMX    B     INETCOMX                                                         
*                                                                               
INETCOMX CP    0(8,R2),=P'0'       TEST ANY MONEY                               
         BE    *+8                                                              
         MVI   MONEY,C'Y'          YES                                          
*                                                                               
         B     XIT                                                              
*                                                                               
*                                  OUTPUT COMMISSION DOLLARS                    
OCOM     ZAP   GFPCOM,0(8,R2)                                                   
         BZ    *+8                                                              
         MVI   MONEY,C'Y'                                                       
*                                                                               
         UNPK  GFCOM,GFPCOM                                                     
         MVC   0(L'GFCOM,R3),GFCOM                                              
*                                                                               
         B     XIT                                                              
*                                                                               
IDATE    MVC   0(4,R2),BILLMON     BILLING MONTH                                
         B     XIT                                                              
*                                                                               
ODATE    MVC   GFDATE,0(R2)                                                     
         MVC   0(L'GFDATE,R3),GFDATE                                            
         B     XIT                                                              
*                                                                               
OFILLER  L     R1,GLADTENT                                                      
         ZIC   RE,DROLEN-DROD(R1)                                               
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R3),SPACES                                                   
         B     XIT                                                              
*                                                                               
IERR     MVC   0(1,R2),ERRCD       ERRORS                                       
         B     XIT                                                              
*                                                                               
OERR     MVC   ERRCD,0(R2)                                                      
         CLI   ERRCD,0                                                          
         BE    XIT                                                              
         LR    R1,R3                                                            
         TM    ERRCD,ERRAGY                                                     
         BZ    *+14                                                             
         MVC   0(4,R1),=C'AGY,'                                                 
         LA    R1,4(R1)                                                         
         TM    ERRCD,ERRPRD                                                     
         BZ    *+14                                                             
         MVC   0(4,R1),=C'PRD,'                                                 
         LA    R1,4(R1)                                                         
         TM    ERRCD,ERREST                                                     
         BZ    *+14                                                             
         MVC   0(3,R1),=C'EST'                                                  
         LA    R1,3(R1)                                                         
         BCTR  R1,0                                                             
         CLI   0(R1),C','                                                       
         BNE   XIT                                                              
         MVI   0(R1),C' '                                                       
         B     XIT                                                              
*                                                                               
*                                  LAST COLUMN - OUTPUT TAPE RECORD             
LASTCOL  CLI   REJECT,C'Y'         TEST RECORD REJECTED                         
         BE    XIT                 YES                                          
         CLI   MONEY,C'N'          TEST $0                                      
         BE    XIT                 YES                                          
         L     R3,AGFTEMP                                                       
         L     R5,AGFFILE                                                       
         LA    R6,GFREC                                                         
         CP    TAPECNT,=P'0'       TEST O/P FILES OPEN YET                      
         BH    LASTCOL4                                                         
         OPEN  ((R3),OUTPUT)       OPEN TEMP DISK FILE                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   SUPTAP,C'Y'         OPTION TO SUPPRESS TAPE                      
         BE    LASTCOL4                                                         
         MVC   DSNGF+13(2),PBAGY   NO-                                          
         CLI   AGENCYCD,C'W'       IF JWT                                       
         BNE   *+10                                                             
         MVC   DSNGF+13(2),=C'JW'                                               
         CLI   AGENCYCD,C'M'       IF ONM                                       
         BNE   *+10                                                             
         MVC   DSNGF+13(2),=C'OM'                                               
         L     RF,TWADCONS               FROM DDGENTWA                          
         L     RF,TDYNALLO-TWADCOND(RF)  RF = V(DYNALLOC)                       
         GOTO1 (RF),DMCB,DDGF,DSNGF                                             
         OPEN  ((R5),OUTPUT)       OPEN TAPE FILE                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LASTCOL4 MVC   GFMED,PBMED                                                      
         PUT   (R3),(R6)           PUT TO TEMP DISK FILE                        
         MVC   GFFILLER,SPACES                                                  
         AP    TAPECNT,=P'1'                                                    
         CLC   GFFILLER,SPACES                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   SUPTAP,C'Y'                                                      
         BE    XIT                                                              
         PUT   (R5),(R6)           PUT TO TAPE                                  
         B     XIT                                                              
         EJECT                                                                  
* INPUT/OUPUT ROUTINES FOR AGENCY REPORT                                        
*                                                                               
         SPACE 1                                                                
IAGENCY  MVC   0(1,R2),GFAGY                                                    
         B     XIT                                                              
*                                                                               
OAGENCY  MVC   0(6,R3),=C'AGENCY'                                               
         MVC   7(1,R3),0(R2)                                                    
         B     XIT                                                              
*                                                                               
OMEDIA   MVC   0(5,R3),=C'MEDIA'                                                
         MVC   7(1,R3),0(R2)                                                    
         B     XIT                                                              
*                                                                               
ICOMPANY MVC   0(1,R2),GFCOMDIV                                                 
         B     XIT                                                              
*                                                                               
IDIV     MVC   0(1,R2),GFCOMDIV+1                                               
         B     XIT                                                              
*                                                                               
IPRDCODE MVC   0(4,R2),GFPRODCD                                                 
         B     XIT                                                              
*                                                                               
IESTIM   MVC   0(12,R2),GFEST                                                   
         B     XIT                                                              
*                                                                               
IMONTH   MVC   0(4,R2),GFMON                                                    
         B     XIT                                                              
*                                                                               
OMONTH   MVC   DUB(4),0(R2)                                                     
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 DATCON,DMCB,DUB,(6,(R3))                                         
         B     XIT                                                              
*                                                                               
IINV     MVC   0(9,R2),GFINV                                                    
         B     XIT                                                              
*                                                                               
ITYPE    MVC   0(9,R2),GFBLTYPE                                                 
         B     XIT                                                              
*                                                                               
INAT     MVC   0(3,R2),GFNAT                                                    
         B     XIT                                                              
*                                                                               
ISUB     MVC   0(3,R2),GFSUBNAT                                                 
         B     XIT                                                              
*                                                                               
IMARKET  MVC   0(4,R2),GFMKT                                                    
         B     XIT                                                              
*                                                                               
I2NET    ZAP   0(8,R2),=P'0'                                                    
         MVC   8(16,R1),0(R2)                                     L02           
*                                                                               
         ZAP   0(8,R2),GFPNET                                                   
         BZ    XIT                                                              
*                                                                               
         MVI   MONEY,C'Y'                                                       
         BAS   RE,XORP                                            L02           
         B     XIT                                                              
*                                                                               
XORP     CLI   GFBLTYPE,C'P'                                      L02           
         BNE   *+14                                               L02           
         ZAP   8(8,R2),0(8,R2)                                    L02           
         B     XIT                                                L02           
*                                                                               
         CLI   GFBLTYPE,C'X'                                      L02           
         BNE   XIT                                                L02           
         ZAP   16(8,R2),0(8,R2)                                   L02           
         B     XIT                                                L02           
*                                                                               
*                                                                               
O2NET    TM    GLINDS,GLTOTLIN     TEST TOTAL                                   
         MVI   GLHOOK,GLEDIT                                                    
         BZ    XIT                                                              
         MVC   TOTNET,0(R2)        YES-SAVE TOTAL NET                           
         BAS   RE,LASTONE                                                       
         B     XIT                                                              
*                                                                               
I2COM    ZAP   0(8,R2),=P'0'                                                    
         MVC   8(16,R1),0(R2)                                     L02           
         ZAP   0(8,R2),GFPCOM                                                   
         BZ    XIT                                                              
         MVI   MONEY,C'Y'                                                       
         BAS   RE,XORP                                            L02           
         B     XIT                                                              
*                                                                               
O2COM    TM    GLINDS,GLTOTLIN     TEST TOTAL                                   
         MVI   GLHOOK,GLEDIT                                                    
         BZ    XIT                                                              
         MVC   TOTCOM,0(R2)        YES-SAVE TOTAL COMMISSION                    
         BAS   RE,LASTONE                                                       
         B     XIT                                                              
*                                                                               
*                                                                               
I2TOT    ZAP   0(8,R2),=P'0'                                      L02           
         MVC   8(16,R1),0(R2)                                     L02           
         ZAP   0(8,R2),GFPCOM                                     L02           
         BZ    I2TOTNET                                           L02           
         MVI   MONEY,C'Y'                                         L02           
I2TOTNET CP    GFPNET,=P'0'                                       L02           
         BE    I2TOTX                                             L02           
         MVI   MONEY,C'Y'                                         L02           
         AP    0(8,R2),GFPNET                                     L02           
I2TOTX   BAS   RE,XORP                                            L02           
         B     XIT                                                L02           
*                                                                               
O2TOT    TM    GLINDS,GLTOTLIN     TEST TOTAL                     L02           
         MVI   GLHOOK,GLEDIT                                                    
         BZ    XIT                                                L02           
         MVC   TOTCOM,0(R2)        YES-SAVE TOTAL COMMISSION                    
         BAS   RE,LASTONE                                         L02           
         B     XIT                                                L02           
*                                                                               
TOTAL    AP    TAPECNT,=P'1'       ADD ONE TO TAPE COUNT FOR TRAILER            
         MVC   0(7,R3),=C'**ALL**'                                              
         MVC   198(6,R3),=C'COUNT='                                             
         LA    R3,198+198(R3)                                                   
         UNPK  0(7,R3),TAPECNT     PRINT TAPE COUNT                             
         OI    6(R3),X'F0'                                                      
         B     XIT                                                              
         EJECT                                                                  
         TITLE 'DETERMINE IF LAST TOTAL'                                        
LASTONE  CLI   GLLEVEL,3                                          L02           
         BE    OIEBOPT                                                          
         CLI   GLLEVEL,4                                                        
         BNER  RE                                                 L02           
OIEBOPT  OI    EBOPT,EBOQZEN+EBOQMEY PRINT 0 AND MINUS            L02           
         MVI   EBDECS,2                                                         
         MVI   EBLOUT,14                                                        
         MVI   EBLIN,8                                                          
         MVI   EBTIN,C'P'                                                       
         ST    R3,EBAOUT                                                        
         ST    R2,EBAIN                                                         
         GOTO1 GLAEDITR,DMCB,EBLOCK                                             
         MVI   GLHOOK,0                                           L02           
         LA    R2,8(R2)                                                         
         ST    R2,EBAIN                                                         
         LA    R3,198(R3)                                                       
         ST    R3,EBAOUT                                                        
         BASR  RE,RF                                                            
         MVI   0(R3),C'P'                                                       
         LA    R3,198(R3)                                                       
         ST    R3,EBAOUT                                                        
         LA    R2,8(R2)                                                         
         ST    R2,EBAIN                                                         
         BASR  RE,RF                                                            
         MVI   0(R3),C'X'                                                       
         B     XIT                                                              
LEVELID  DC    X'0'                  LOWEST LEVEL TOTAL                         
         EJECT                                                                  
* DRIVER ABOUT TO PUT TO SORT                                                   
*                                                                               
PUTSRT   CLI   REJECT,C'Y'         TEST RECORD REJECTED                         
         BE    *+12                                                             
         CLI   MONEY,C'N'          OR $0                                        
         BNE   *+8                                                              
         MVI   GLHOOK,GLDONT       YES-TELL DRIVER TO REJECT                    
         B     XIT                                                              
         EJECT                                                                  
* HEADHOOK                                                                      
*                                                                               
HEADHK   L     R2,AH4                                                           
         A     R2,PWIDTH           R2=A(HEADLINE 5)                             
         A     R2,PWIDTH           R2=A(HEADLINE 5)                             
         XC    0(50,R2),0(R2)                                                   
         CLI   GLOPTS+2,1                                                       
         BNE   *+14                                                             
         MVC   53(24,R2),=C'** GF TAPE RECORD MAP **'                           
         B     HEADHKX                                                          
         GOTO1 GENHEAD                                                          
         CLI   GLRECNO,1                                                        
         BNE   *+14                                                             
         MVC   53(19,R2),=C'** AGENCY REPORT **'                                
         B     HEADHKX                                                          
         MVC   53(18,R2),=C'** AGENCY RECAP **'                                 
*                                                                               
HEADHKX  B     XIT                                                              
         EJECT                                                                  
* ABOUT TO PRINT A LINE                                                         
*                                                                               
PRINT    CLI   GLOPTS+2,2          TEST AGENCY REPORT                           
         BE    XIT                 YES-PRINT ALL LINES                          
         CLI   REJECT,C'Y'         IF LINE REJECTED                             
         BE    *+12                                                             
         CLI   MONEY,C'N'          OR $0,                                       
         BNE   *+8                                                              
         MVI   GLHOOK,GLDONT       TELL DRIVER                                  
         MVI   REJECT,C'N'                                                      
         MVI   MONEY,C'N'                                                       
         B     XIT                                                              
         EJECT                                                                  
* FINAL HOOK                                                                    
*                                                                               
FINAL    L     R0,ASAVE            SAVE INTER-REQUEST VALUES                    
         LA    R1,SAVVALL1                                                      
         LA    RE,SAVVALS                                                       
         LR    RF,R1                                                            
         MVC   SVAGYCD,AGENCYCD                                                 
         MVC   SVTIT,TITLE                                                      
         MVC   SVSUBTIT,SUBTITLE                                                
         MVC   SVSPLID,SPOOLID                                                  
         MVCL  R0,RE                                                            
         B     XIT                                                              
         EJECT                                                                  
* RUNLAST                                                                       
*                                                                               
LST      L     RE,ASAVE            RESTORE SAVED VALUES                         
         LA    RF,SAVVALL1                                                      
         LA    R0,SAVVALS                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         CP    TAPECNT,=P'0'       VERIFY TAPE OPEN                             
         BNH   LSTX                                                             
         GOTO1 VINIDRIV            YES-INITIALIZE DRIVER                        
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         LA    R1,DRHKNTR                                                       
         ST    R1,GLAHOOK                                                       
         MVI   GLOPTS+2,2          2ND REPORT = AGENCY REPORT                   
         OI    GLINDS,GLPALTOT                                                  
         MVI   MYFIRSTH,8                                                       
         MVI   GLMODE,GLINIT                                                    
         GOTO1 DRIVER,DMCB,(R4)                                                 
*                                                                               
         L     R3,AGFTEMP                                                       
         LA    R6,GFREC                                                         
         CLOSE ((R3))              CLOSE TEMP DISK FILE                         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         DS    0H                                                               
         OPEN  ((R3),INPUT)        OPEN TEMP DISK FILE FOR INPUT                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R1,LST90                                                         
         STCM  R1,7,33(R3)         RESET EODAD ADDRESS                          
*                                                                               
LST2     DS    0H                                                               
         GET   (R3),(R6)           GET A RECORD                                 
         MVC   PBMED,GFMED                                                      
         MVI   GLMODE,GLINPUT      CALL DRIVER FOR INPUT                        
         GOTO1 DRIVER,DMCB,(R4)                                                 
         B     LST2                                                             
*                                                                               
LST90    MVC   TITLE,SVTIT         RESTORE TITLES                               
         MVC   SUBTITLE,SVSUBTIT                                                
         MVC   SPOOLID,SVSPLID                                                  
         GOTO1 OPENPQ              INITIALIZE SPOOL                             
         MVI   GLMODE,GLOUTPUT     CALL DRIVER FOR OUTPUT                       
         GOTO1 DRIVER,DMCB,(R4)                                                 
         MVI   GFREC,C'9'          TAPE TOTAL RECORD                            
         MVC   GFREC+1(L'GFREC-1),GFREC  ALL NINES                              
         MVC   GFAGY,SVAGYCD       AGENCY CODE                                  
         MVI   GFBLTYPE,C'T'       TYPE OF BILLING = T                          
         ZAP   DUB,TOTNET          TOTAL NET                                    
         UNPK  GFNET,DUB                                                        
         ZAP   DUB,TOTCOM          TOTAL COMMISSION                             
         UNPK  GFCOM,DUB                                                        
         CLOSE ((R3))                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   SUPTAP,C'Y'                                                      
         BE    LSTX                                                             
         L     R5,AGFFILE          PUT TRAILER RECORD TO TAPE                   
         LA    R6,GFREC                                                         
         PUT   (R5),(R6)                                                        
         CLOSE ((R5))                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LSTX     B     XIT                                                              
         EJECT                                                                  
PROGPROX DC    XL16'0'                                                          
*                                                                               
INVOICE  NTR1                                                                   
*                                                                               
*                                                                               
*        PROGPROX+4  NOW HAS 'BASE' YEAR FOR INV MONTH DISPLAY                  
*        PROGPROX+5  ADD THIS MUNBER TO INVOICE MONTH                           
*                                                                               
MOSE7    LA    R5,PBILKBMN         POINT TO BILLED DATE/INV/                    
         ZIC   R0,1(R5)            MONTH                                        
***                                                                             
         SR    RF,RF                                                            
         ICM   RF,1,PROGPROX+4      INV MTH 'BASE' YEAR                         
         BZ    MOSE9                                                            
         ZIC   RE,0(R5)             BILLING YEAR                                
         SR    RE,RF                                                            
         BNP   MOSE9                                                            
         MH    RE,=H'12'                                                        
         AR    R0,RE                                                            
***                                                                             
MOSE9    DS    0H                                                               
         CLI   PROGPROX+5,0         SEE IF INCREMENTING INV MONTH               
         BE    MOSE9C                                                           
         ZIC   RE,PROGPROX+5                                                    
         AR    R0,RE                                                            
         CLI   PROGPROX+4,0        CHK FOR BASE YEAR                            
         BNE   MOSE9C                                                           
         CH    R0,=H'12'                                                        
         BNH   MOSE9C                                                           
         SH    R0,=H'12'                                                        
*                                                                               
MOSE9C   CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  GFINV+2(2),DUB+6(2)                                              
         MVI   GFINV+4,C'-'                                                     
         ICM   R0,3,2(R5)          INVOICE NUMBER                               
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  GFINV+5(4),DUB+5(3)                                              
         XIT1  ,                                                                
         LTORG                                                                  
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
         DS    0F                                                               
ASAVE    DS    A                                                                
AFORMTAB DS    A(FORMTAB)    FORMULA TABLE                                      
APRDTAB  DS    A(APRDTAB)    PRODUCT TABLE                                      
AESTTAB  DS    A(AESTTAB)    ESTIMATE TABLE                                     
AESTZZT  DS    A(AESTZZT)    POL TABLE                                          
TOTNET   DS    PL8                                                              
TOTCOM   DS    PL8                                                              
*                                                                               
QCOMPDIV DC    CL8'DIVBRNCD'                                                    
QPRDCODE DC    CL8'PROD CD '                                                    
QNATURAL DC    CL8'GF NTRL '                                                    
QSUBNATL DC    CL8'GFSBNTRL'                                                    
*                                                                               
COMPYDIV DS    CL2                 GF FIELDS                                    
PRODCODE DS    CL4                                                              
NATURAL  DS    CL3                                                              
SUBNATRL DS    CL3                                                              
ETEST    DS    CL1                TEST ESTIMATE BYTE X'80' = TEST               
EBFORM   DS    XL5                BILLING FORMULA                               
EBFDATE  DS    XL3                BILLING FORMUAL EFF DATE                      
AGENCYCD DS    CL1                                                              
*                                                                               
SVPRD    DS    CL3                                                              
SVEST    DS    XL2                                                              
SVBFEST  DS    XL2                                                              
REJECT   DS    CL1                                                              
MONEY    DS    CL1                                                              
BILLMON  DS    CL4                                                              
*                                                                               
ERRCD    DS    XL1                 ERROR BYTE                                   
ERRAGY   EQU   X'80'               AGENCY                                       
ERRPRD   EQU   X'40'               PRODUCT                                      
ERREST   EQU   X'08'               ESTIMATE                                     
*                                                                               
ZEROS    DC    CL8'00000000'                                                    
XFF      DC    XL12'FFFFFFFFFFFFFFFFFFFFFFFF'                                   
*                                                                               
MONTAB   DS    25XL6               MONTH TABLE                                  
*                                  ENTRIES ARE BINARY YYMM FOLLOWED             
*                                  BY CHARACTERS YYMM                           
*                                  EXAMPLE - X'5801' C'8801'                    
*                                                                               
         DC    25XL6'0'                                                         
         DC    25XL6'0'                                                         
SUPTAP   DS    CL1                                                              
DDGF     DC    CL8'GFFILE'                                                      
DSNGF    DC    CL20'PRTTAPE.PP0GFXX1'                                           
DDTEMP   DC    CL8'GFTEMP'                                                      
TMPALLOC DC    XL6'000003000003'                                                
MYWORK   DS    CL5                                                              
AGFTEMP  DS    A                   A(GFTEMP DCB IN SPFUSER)                     
AGFFILE  DS    A                   A(GFFILE DCB IN SPFUSER)                     
         EJECT                                                                  
* VALUES SAVED BETWEEN REQUESTS                                                 
*                                                                               
SAVVALS  DS    0X                                                               
*                                                                               
TAPECNT  DS    PL4                                                              
SVAGYCD  DS    CL1                                                              
SVTIT    DS    CL63                                                             
SVSUBTIT DS    CL36                                                             
SVSPLID  DS    CL3                                                              
*                                                                               
SAVVALL1 EQU   *-SAVVALS                                                        
*                                                                               
GFFILE   DCB   DDNAME=GFFILE,DSORG=PS,LRECL=80,BLKSIZE=800,            X        
               MACRF=(GM,PM),RECFM=FB                                           
*                                                                               
GFTEMP   DCB   DDNAME=GFTEMP,DSORG=PS,LRECL=80,BLKSIZE=800,            X        
               MACRF=(GM,PM),RECFM=FB,EODAD=LST90                               
SAVVALSL EQU   *-SAVVALS                                                        
*                                                                               
         EJECT                                                                  
GFREC    DS    0CL80           *** GF RECORD ***                                
*                                                                               
GFKEY    DS    0CL43                                                            
GFCOMDIV DS    CL2                                                              
GFPRODCD DS    CL4                                                              
GFAGY    DS    CL1                                                              
GFEST    DS    CL12                                                             
GFMON    DS    CL4                                                              
GFINV    DS    CL9                                                              
GFBLTYPE DS    CL1                                                              
GFNAT    DS    CL3                                                              
GFSUBNAT DS    CL3                                                              
GFMKT    DS    CL4                                                              
*                                                                               
GFNET    DS    CL10                                                             
GFCOM    DS    CL10                                                             
*                                                                               
GFDATE   DS    CL4                                                              
*                                                                               
GFFILLER DS    CL13                                                             
         ORG   GFFILLER                                                         
GFMED    DS    CL1                                                              
GFPNET   DS    PL6                                                              
GFPCOM   DS    PL6                                                              
         ORG                                                                    
         SPACE 2                                                                
SVKEY    DS    CL(GFMON-GFKEY+L'GFMON)    SAVED KEY                             
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*PRWRIWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DMPRTQL                                                                        
*DDBUFFALOD                                                                     
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*DRINTRECD                                                                      
*PPCLRST                                                                        
*PGENGRP                                                                        
*PPDDEQUS                                                                       
         PRINT   OFF                                                            
       ++INCLUDE PRWRIWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD                                                      
       ++INCLUDE PPCLRST                                                        
       ++INCLUDE PGENGRP                                                        
       ++INCLUDE PPDDEQUS                                                       
         PRINT   ON                                                             
PPRDRECD DSECT                                                                  
       ++INCLUDE PPRDREC                                                        
         EJECT                                                                  
PESTRECD DSECT                                                                  
       ++INCLUDE PESTREC                                                        
         EJECT                                                                  
PBILLRCD DSECT                                                                  
       ++INCLUDE PBILLREC                                                       
         EJECT                                                                  
GFESTRD  DSECT                                                                  
       ++INCLUDE PGESTREC                                                       
         EJECT                                                                  
PBKRECD  DSECT                                                                  
       ++INCLUDE PBKREC                                                         
       ++INCLUDE DDBKELEM                                                       
         EJECT                                                                  
       ++INCLUDE PRWRIFFD                                                       
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE PRWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRWRIE9D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDTWADCOND                                                     
         SPACE 2                                                                
*GERFPIOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE GERFPIOD                                                       
         PRINT ON                                                               
         SPACE 2                                                                
ESTABDS  DSECT                                                                  
*              ENTRIES INCLUDE NATURAL AND SUBNATURAL                           
ESTBNORM DS    CL3     NATURAL                                                  
ESTBSUBN DS    CL3     SUB-NATURAL                                              
         SPACE 3                                                                
FF       DSECT                                                    BUG01         
*******  DC    C'** PRWRI11 FORMULAE TABLE **'                    BUG01         
FORMTAB  DS    999XL(1+5+3)                                       BUG01         
* PRODUCT AND ESTIMATE TABLES FOR GF CODES                                      
*                                                                               
         DC    C'** PRWRI11 PRD TABLE **'                                       
PRDTAB   DS    255XL(3+L'COMPYDIV+L'PRODCODE)                                   
*                                                                               
         DC    C'** PRWRI11 EST TABLE **'                                       
ESTTAB   DS    999XL(L'NATURAL+L'SUBNATRL)                                      
*                                                                               
         DC    C'** PR11ZZ  EST TABLE **'                                       
ESTTABZZ DS    999XL(L'NATURAL+L'SUBNATRL)                                      
*********************************************************************           
*              ENTRIES INCLUDE TEST,FORMULA,EFF DATE                            
FORMTABD DSECT                                                                  
*              ENTRIES INCLUDE TEST,FORMULA,EFF DATE                            
FORMTST  DS    CL1     TEST ESTIMATE INDICATOR                                  
FORMFORM DS    CL5     INCLUDE TESTESTIMATE FORMULA                             
FORMEFFD DS    XL3     INCLUDE TESTEFFECTIVE DATE                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'061PRWRI11   07/17/02'                                      
         END                                                                    
