*          DATA SET SPREPTD02  AT LEVEL 025 AS OF 02/09/15                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 045021.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE SPTD02A                                                                  
*INCLUDE DLFLD                                                                  
*INCLUDE SPFMTINO                                                               
*INCLUDE GETUSER                                                                
*INCLUDE NETACC                                                                 
*INCLUDE NETNET                                                                 
*INCLUDE WIDE                                                                   
         TITLE 'SPREPTD02 - TAB DELIMITED INTERFACE'                            
***********************************************************************         
*   QOPT1 -  D=DUMP OUTPUT RECORDS                                              
*                                                                               
*   QOPT7 - Y= TRACE BUFFALO PUTS,READS                                         
*                                                                               
***********************************************************************         
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
*        BPLA   08/07  ADD ESTIMATE NUMBER TO OUTPUT FILE                       
*                                                                               
*        BPLA   04/06  VCLIST CHECKING CHANGED                                  
*                                                                               
*                                                                               
SPTD02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPTD02                                                         
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         LA    R8,SPACEND                                                       
         USING BILWRKD,R8                                                       
         L     R3,AWIDEC                                                        
         USING WIDED,R3                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,PRDFRST                                                     
         BE    FPRD                                                             
*                                                                               
         CLI   MODE,ESTFRST                                                     
         BE    FEST                                                             
*                                                                               
         CLI   MODE,PROCBILL                                                    
         BE    PROCBL                                                           
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BE    CLTF                                                             
         CLI   MODE,CLTLAST                                                     
         BE    CLTL                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         SPACE 2                                                                
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*        RUNFRST                                                                
         SPACE 2                                                                
RUNF     DS    0H                                                               
*                                  RELOCATE ADDRESSES                           
         RELOC RELO                                                             
         LA    R0,(ACONSX-ACONS)/4      NO. OF ADDRS                            
         LA    R2,ACONS                                                         
         LA    RE,RCONS                                                         
RUNF2    DS    0H                                                               
         L     RF,0(R2)                                                         
         A     RF,RELO                                                          
         ST    RF,0(RE)                                                         
         LA    R2,4(R2)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,RUNF2                                                         
*                                                                               
INIT5    MVI   NETOPT,C'N'         SET FOR NEW NETPAK                           
*                                                                               
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         CLI   MCNETPAK,C'Y'       SEE IF NETPAK                                
         BE    INIT7                                                            
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVI   NETOPT,0                                                         
INIT7    DS    0H                                                               
         MVI   FFS,X'FF'                                                        
         MVC   FFS+1(L'FFS-1),FFS                                               
         MVI   DOWNSW,0                                                         
         MVI   LASTDSW,0                                                        
*                                                                               
         MVI   DPAGEIND,0                                                       
*                                                                               
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         MVC   BOXWIDTH,=F'132'   START WITH 132                                
*                                 AT REQF SET TO 198                            
*                                 AT REQL RESET TO 132                          
*                                 AT RUNL RESET TO 198                          
*                                                                               
         MVC   BOXAWIDE,AWIDEC    ADDRESS OF WIDE PRINT LINES                   
         L     R3,AWIDEC                                                        
         MVI   BOXYORN,C'N'       NO BOXES                                      
         MVC   BOXCOLS(198),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
*                                                                               
         DROP  R1                                                               
*                                                                               
*                                                                               
*                                  SET BUFFALO PARAMS                           
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFFC                                      
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
*        REQFRST                                                                
         SPACE 2                                                                
REQF     DS    0H                                                               
*                                                                               
*                                                                               
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         L     R1,MCVREMO2         FOR SECOND PQ ENTRY - FILE                   
         USING REMOTED,R1                                                       
         OI    REMOTTYP,REMOTDLQ   SET AS DOWNLOAD                              
         DROP  R1                                                               
         DROP  RF                                                               
*                                                                               
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         MVC   BOXWIDTH,=F'198'   FOR WIDE DOWNLOADING                          
         DROP  R1                                                               
*                                                                               
         L     RF,=A(PROCNET)                                                   
         A     RF,RELO                                                          
         ST    RF,VPROCNET                                                      
*                                                                               
         L     RF,=A(NETBLK)                                                    
         A     RF,RELO                                                          
         ST    RF,ANETBLK                                                       
*                                                                               
         L     RF,=V(NETNET)                                                    
         A     RF,RELO                                                          
         ST    RF,ANETNET                                                       
*                                                                               
         L     RF,=V(NETACC)                                                    
         A     RF,RELO                                                          
         ST    RF,ANETACC                                                       
*                                                                               
         MVI   DOWNSW,0                                                         
*                                                                               
         MVC   SVQOPT1,QOPT1                                                    
         CLC   =C'ALL',QEST                                                     
         BNE   *+10                                                             
         MVC   QEST,SPACES                                                      
*                                                                               
         OI    DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
*                                                                               
         MVC   MYSTART,QSTART                                                   
         MVC   MYEND,QEND                                                       
         GOTO1 DATCON,DMCB,QSTART,(3,MYSTRTB)                                   
         GOTO1 DATCON,DMCB,QEND,(3,MYENDB)                                      
         GOTO1 DATCON,DMCB,QSTART,(2,BQSTARTP)                                  
         GOTO1 DATCON,DMCB,QEND,(2,BQENDP)                                      
*                                                                               
         MVC   QSTART,SPACES                                                    
         MVC   QEND,SPACES                                                      
         MVC   BQSTART,=X'000000'                                               
         MVC   BQEND,=X'FFFFFF'                                                 
*                                                                               
         ZAP   QTNET,=P'0'                                                      
         ZAP   QTGRS,=P'0'                                                      
         ZAP   QTCD,=P'0'                                                       
         ZAP   QTDUE,=P'0'                                                      
*                                                                               
         CLI   FIRST,0             FIRST TIME TEST                              
         BNE   REQF20                                                           
         MVI   FIRST,1                                                          
*                                                                               
         ZAP   GTNET,=P'0'                                                      
         ZAP   GTGRS,=P'0'                                                      
         ZAP   GTCD,=P'0'                                                       
         ZAP   GTDUE,=P'0'                                                      
         ZAP   GTINVS,=P'0'                                                     
         ZAP   GTLINS,=P'0'                                                     
*                                                                               
REQF20   DS    0H                                                               
         B     EXIT                                                             
         SPACE 3                                                                
PROCBL   GOTO1 APRBILL                                                          
         B     EXIT                                                             
*        CLTFRST                                                                
CLTF     DS    0H                                                               
         XC    B1PROF,B1PROF        FIRST READ B1 AND B1X PROFILES              
         XC    B1XPROF,B1XPROF      B1X PROFILE                                 
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SOB1'                                                 
         MVC   WORK+4(2),QAGY                                                   
         MVC   WORK+6(1),MED                                                    
         MVC   WORK+7(3),CLT                                                    
         L     RF,ADCLT                                                         
         USING CLTHDR,RF                                                        
*        USED TO CHECK FOR ANY OFFICE CODE                                      
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         DROP  RF                                                               
*                                                                               
FBC1     DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,B1PROF,DATAMGR                                 
         MVC   WORK(4),=C'SB1X'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR PAGE A               
         GOTO1 GETPROF,DMCB,WORK,B1XPROF,DATAMGR                                
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        PRDFRST                                                                
FPRD     DS    0H                                                               
         XC    PRDU1,PRDU1                                                      
         XC    PRDU2,PRDU2                                                      
         MVI   PUERROR,0           CLEAR PRD USER ERROR SW                      
*                                                                               
         L     RF,ADPRD                                                         
         CLC   =C'AAA',PKEYPRD-PRDHDR(RF)                                       
         BE    FPRDSAVE                                                         
*                                                                               
         GOTO1 VGETUSER,DMCB,(C'S',ADCLT),(C'P',ADPRD),PRDU1,PRDU2              
         CLI   DMCB,X'FF'                                                       
         BE    FPRDERR                                                          
         CLI   PRDU1+21,C' '    MUST FIND DATA                                  
         BNH   FPRDERR                                                          
         B     FPRD5                                                            
*                                                                               
FPRDERR  DS    0H                                                               
*                                                                               
         OI    PUERROR,X'01'        SET USER 1 ERROR                            
         B     FPRD5                                                            
*                                                                               
FPRD5    CLI   PRDU2,C' '   MISSING JOB NUMBER PRD USER 2                       
         BNH   FPRDERR2                                                         
         B     FPRDSAVE                                                         
*                                                                               
FPRDERR2 DS    0H                                                               
*                                                                               
         OI    PUERROR,X'02'      SET USER 2 ERROR                              
         B     FPRDSAVE                                                         
*                                                                               
         MVC   XP,XSPACES                                                       
         MVC   XP(43),=C'** MISSING JOB NUMBER - PRD USER FIELD 2 **'           
         L     RF,ADPRD                                                         
         MVC   XP+48(3),PKEYPRD-PRDHDR(RF)                                      
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         CLI   QOPT6,C'Y'            SEE IF TEST RUN                            
         BE    FPRDSAVE              CONTINUE - ELSE DIE                        
         MVC   XP(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
FPRDSAVE DS    0H                                                               
*                                                                               
         L     RF,ADPRD                                                         
         CLC   =C'AAA',PKEYPRD-PRDHDR(RF)                                       
         BNE   FPRDFORM                                                         
*                                                                               
         MVI   AAAPRD,C'Y'                                                      
         MVC   AAAFORMU(5),PBILLBAS-PRDHDR(RF)  MOVE FORMULA FOR AAA            
         B     FPRDX                                                            
*                                                                               
FPRDFORM DS    0H                                                               
         MVC   PRDFORMU(5),PBILLBAS-PRDHDR(RF)    BILL FORMULA                  
*                                                                               
*                                                                               
FPRDX    OC    PRDU1,SPACES                                                     
         OC    PRDU2,SPACES                                                     
         B     EXIT                                                             
         EJECT                                                                  
*        ESTFRST                                                                
FEST     DS    0H                                                               
         XC    ESTU1,ESTU1                                                      
         XC    ESTU2,ESTU2                                                      
         L     RF,ADPRD                                                         
         CLC   =C'AAA',PKEYPRD-PRDHDR(RF)     NOT FOR PRD=AAA                   
         BE    FESTSAVE                                                         
*                                                                               
         GOTO1 VGETUSER,DMCB,(C'P',ADCLT),(C'E',ADEST),ESTU1,ESTU2              
         CLI   DMCB,X'FF'                                                       
         BE    FESTERR                                                          
         CLI   ESTU1+21,C' '    MUST FIND DATA                                  
         BNH   FESTERR                                                          
         B     FESTSAVE                                                         
*                                                                               
FESTERR  DS    0H                                                               
*                                                                               
         B     FESTX        NOT REQUIRED?                                       
*                                                                               
         MVC   XP,XSPACES                                                       
         MVC   XP(37),=C'*** MISSING ESTIMATE USER FIELD 1 ***'                 
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         MVC   XP+41(3),EKEYPRD                                                 
         EDIT  (B1,EKEYEST),(3,XP+46),0                                         
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         CLI   QOPT6,C'Y'            SEE IF TEST RUN                            
         BE    FESTSAVE              CONTINUE - ELSE DIE                        
         MVC   XP(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
FESTSAVE DS    0H                                                               
*                                                                               
FESTX    OC    ESTU1,SPACES                                                     
         OC    ESTU2,SPACES                                                     
         B     EXIT                                                             
         SPACE 2                                                                
*        PRDLAST                                                                
PRDL     DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        CLT LAST                                                               
         SPACE 2                                                                
CLTL     DS    0H                                                               
         CLI   NETOPT,C'N'   NETPAK?                                            
         BE    CLTL5         YES - THEN GO READ UNITS                           
         GOTO1 ARNBILL       GO READ OE01 BILLING BACK-UP RECS                  
         B     CLTL10                                                           
*                                                                               
CLTL5    GOTO1 VPROCNET       UNIT PROCESSING                                   
*                                                                               
CLTL10   GOTO1 AREPRT                                                           
         B     EXIT                                                             
         SPACE 3                                                                
*        REQ LAST                                                               
REQL     DS    0H                                                               
         GOTO1 APRNT                                                            
         MVI   DOWNSW,C'F'    DO FOOTER                                         
         GOTO1 VDOWNLD,DMCB,(RA)                                                
         GOTO1 APRNT                                                            
*                                                                               
         LA    R2,XP                                                            
         USING LINED,R2                                                         
         MVC   XP(20),=C'** REQUEST TOTALS **'                                  
         EDIT  (P8,QTNET),(15,LNET),2,COMMAS=YES,FLOAT=-                        
         EDIT  (P8,QTGRS),(15,LGRS),2,COMMAS=YES,FLOAT=-                        
         EDIT  (P8,QTCD),(15,LCD),2,COMMAS=YES,FLOAT=-                          
         EDIT  (P8,QTDUE),(15,LDUE),2,COMMAS=YES,FLOAT=-                        
         MVI   SPACING,2                                                        
         GOTO1 APRNT                                                            
*****    MVC   LDDAT(20),=C'   COMMISSION TOTAL '                               
*****    ZAP   DUB,QTDUE                                                        
*****    SP    DUB,QTNET                                                        
*****    EDIT  (P8,DUB),(15,LGRS),2,COMMAS=YES,FLOAT=-                          
*****    MVI   SPACING,1                                                        
*****    GOTO1 APRNT                                                            
         MVC   LDDAT(20),=C'   BILL AMOUNT TOTAL'                               
         EDIT  (P8,QTDUE),(15,LGRS),2,COMMAS=YES,FLOAT=-                        
         MVI   SPACING,1                                                        
         GOTO1 APRNT                                                            
*                                                                               
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         MVC   BOXWIDTH,=F'132'   MUST RESET TO 132                             
         DROP  R1                                                               
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
*        RUN LAST                                                               
RUNL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         MVC   BOXWIDTH,=F'198'   RESET FOR WIDE PRINTING AGAIN                 
         DROP  R1                                                               
*                                                                               
         GOTO1 APRNT                                                            
         LA    R2,XP                                                            
         USING LINED,R2                                                         
         MVC   XP(16),=C'** RUN TOTALS **'                                      
         EDIT  (P8,GTNET),(15,LNET),2,COMMAS=YES,FLOAT=-                        
         EDIT  (P8,GTGRS),(15,LGRS),2,COMMAS=YES,FLOAT=-                        
         EDIT  (P8,GTCD),(15,LCD),2,COMMAS=YES,FLOAT=-                          
         EDIT  (P8,GTDUE),(15,LDUE),2,COMMAS=YES,FLOAT=-                        
         MVI   SPACING,2                                                        
         GOTO1 APRNT                                                            
*****    MVC   LDDAT(20),=C'   COMMISSION TOTAL '                               
*****    ZAP   DUB,GTDUE                                                        
*****    SP    DUB,GTNET                                                        
*****    EDIT  (P8,DUB),(15,LGRS),2,COMMAS=YES,FLOAT=-                          
*****    MVI   SPACING,1                                                        
*****    GOTO1 APRNT                                                            
         MVC   LDDAT(20),=C'   BILL AMOUNT TOTAL'                               
         EDIT  (P8,GTDUE),(15,LGRS),2,COMMAS=YES,FLOAT=-                        
         MVI   SPACING,1                                                        
         GOTO1 APRNT                                                            
*                                                                               
         MVC   XP+2(14),=C'INVOICE COUNT='                                      
         EDIT  (P8,GTINVS),(7,XP+21)                                            
         GOTO1 APRNT                                                            
*                                                                               
         CLI   ERROR,0                                                          
         BE    RUNL4                                                            
         MVC   XP(45),=C'***** ERRORS - NO OUTPUT FILE GENERATED *****'         
         GOTO1 APRNT                                                            
         B     RUNLX                                                            
*                                                                               
RUNL4    DS    0H                                                               
*                                                                               
RUNL8    DS    0H                                                               
*                                                                               
*                                                                               
RUNL10   DS    0H                                                               
RUNLX    DS    0H           CLOSE DOWNLOAD REPORT                               
         GOTO1 VDOWNLD,DMCB,(RA)                                                
         B     EXIT                                                             
*                                                                               
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
ACONS    DS    0F                                                               
         DC    A(PRNT)                                                          
         DC    A(PRBILL)                                                        
         DC    A(REPRT)                                                         
         DC    A(STABUCKC)                                                      
         DC    A(BUFFALOC)                                                      
         DC    V(DLFLD)                                                         
         DC    V(DOWNLD)                                                        
         DC    V(GETUSER)                                                       
         DC    V(SPFMTINO)                                                      
         DC    A(PROCNET)                                                       
         DC    A(NETBLK)                                                        
         DC    V(NETNET)                                                        
         DC    V(NETACC)                                                        
         DC    A(RNBILL)                                                        
         DC    V(WIDE)                                                          
ACONSX   EQU   *                                                                
         SPACE 2                                                                
         EJECT                                                                  
         TITLE 'RNBILL - READ SPOT BILLING DETAIL RECORDS'                      
RNBILL   CSECT                                                                  
         NMOD1 0,RNBILL                                                         
         SPACE 2                                                                
         MVC   KEY1,KEY                 SAVE KEY                                
         XC    KEY,KEY                                                          
RNB2     DS    0H                                                               
         MVC   KEY(2),=X'0E01'          RECORD TYPE                             
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         CLI   KPRD,0                                                           
         BE    RNB3                SKIP REST OF KEY NOW IF MULTI-PRD            
         MVC   KEY+5(1),KPRD                                                    
RNB2EST  DS    0H                                                               
         MVC   KEY+6(1),BEST                                                    
         XC    KEY+7(6),KEY+7                                                   
         CLI   BEST,0                                                           
         BE    RNB3                SKIP REST OF KEY NOW IF MULTI-EST            
RNB2MKT  DS    0H                                                               
         MVC   KEY+7(2),KMKT                                                    
         XC    KEY+9(4),KEY+9                                                   
         OC    KMKT,KMKT                                                        
         BZ    RNB3                SKIP STA NOW IF MULTI-MKT                    
RNB2STA  DS    0H                                                               
         MVC   KEY+9(3),KSTA                                                    
RNB3     DS    0H                                                               
         GOTO1 HIGH                                                             
         B     RNB4B                                                            
RNB4     DS    0H                                                               
         GOTO1 SEQ                                                              
RNB4B    DS    0H                                                               
         CLC   KEY(5),KEYSAVE      AGM/CLT                                      
         BNE   RNB40                                                            
         CLI   KPRD,0                                                           
         BE    RNB5                                                             
         CLC   KPRD,KEY+5          ONE PROD MUST BE EQUAL                       
         BNE   RNB40                                                            
*                                                                               
RNB5     DS    0H                                                               
         CLI   BEST,0                                                           
         BE    RNB7                                                             
*                                  ONE EST OR SERIES                            
         CLC   KEY+6(1),BEST                                                    
         BL    RNB2EST                                                          
         BE    RNB7                                                             
         CLI   BESTEND,0                                                        
         BE    RNB6D                                                            
         CLC   KEY+6(1),BESTEND                                                 
         BNH   RNB7                                                             
*                                                                               
RNB6D    DS    0H                  EST NOT OK                                   
         CLI   KPRD,0                                                           
         BNE   RNB40               DONE IF ONE PRD                              
         IC    RF,KEY+5            ELSE NEXT PROD                               
         LA    RF,1(RF)                                                         
         STC   RF,KEY+5                                                         
         B     RNB2EST                                                          
*                                                                               
RNB6H    DS    0H                  BUMP TO NEXT EST                             
         ZIC   RF,KEY+6                                                         
         LA    RF,1(RF)                                                         
         STC   RF,KEY+6                                                         
         B     RNB2MKT                                                          
*                                                                               
RNB7     DS    0H                  MARKET                                       
         OC    KMKT,KMKT                                                        
         BZ    RNB8                                                             
         CLC   KEY+7(2),KMKT       ONE MKT                                      
         BE    RNB9                                                             
         BL    RNB2MKT                                                          
*                                  MKT HIGH                                     
*                                  IF MULT-EST BUMP TO NEXT EST                 
RNB7B    DS    0H                                                               
         CLI   BEST,0                                                           
         BE    RNB6H                                                            
         CLI   BESTEND,0                                                        
         BNE   RNB6H                                                            
         B     RNB6D               NEXT PRD (IF MULTI-PRD)                      
*                                                                               
RNB8     DS    0H                  MULTI-MKT                                    
RNB9     DS    0H                                                               
         OC    KSTA,KSTA           STATION                                      
         BZ    RNB10                                                            
         CLC   KSTA,KEY+9          ONE STA                                      
         BL    RNB2STA                                                          
         BH    RNB7B               NEXT EST -PRD (IF MULTI)                     
         SPACE 3                                                                
*                                  HAVE GOOD KEY                                
RNB10    DS    0H                                                               
*                                                                               
         L     R7,ADSTABUC                                                      
         ST    R7,AREC                                                          
         USING STABUCK,R7                                                       
         GOTO1 GET                                                              
*                                       FIND PRD CODE                           
         L     RF,VCLIST                                                        
*                                                                               
RNB10A   DS    0H                                                               
         CLC   STABKPRD,3(RF)                                                   
         BE    RNB10B                                                           
         LA    RF,4(RF)                                                         
         CLI   0(RF),0       END OF LIST?                                       
         BNE   RNB10A                                                           
         DC    H'0'          MUST FIND THE PRODUCT                              
*                                                                               
RNB10B   DS    0H                                                               
         MVC   MYBKPRD,0(RF)            SAVE PRODUCT                            
*                                                                               
         MVI   CKESTREC,C'K'          SET FROM BUCKET                           
         GOTO1 =A(CKEST)                                                        
*                                                                               
         MVI   ELCODE,X'0E'                                                     
         LA    R2,STABELEM                                                      
         USING STABELEM,R2                                                      
*                                                                               
RNB11    DS    0H                                                               
         CLC   ELCODE,0(R2)                                                     
         BE    RNB13                                                            
*                                                                               
RNB12    DS    0H                                                               
         BAS   RE,RNBNXTEL                                                      
         BNE   RNB30                                                            
*                                                                               
RNB13    DS    0H                                                               
*                                  TEST IN REQ PERIOD                           
         CLC   STABBDT,BQSTARTP                                                 
         BL    RNB12                                                            
         CLC   STABBDT,BQENDP                                                   
         BH    RNB12                                                            
*                                                                               
RNB13T   DS    0H                                                               
         XC    X(250),X                                                         
         LA    R4,X                                                             
         USING INVD,R4                                                          
*                                                                               
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         MVC   INVEST,EKEYEST                                                   
         MVC   INVEDESC(20),EDESC     ESTIMATE NAME                             
         DROP  RF                                                               
*                                                                               
         OC    INVEDESC(L'INVEDESC),SPACES                                      
*                                                                               
         LA    RE,INVEDESC                                                      
         LA    RF,20                                                            
RNB22    CLI   0(RE),C','        CHANGE COMMAS TO SPACES                        
         BNE   RNB22C                                                           
         MVI   0(RE),C' '                                                       
         B     RNB23                                                            
*                                                                               
RNB22C   CLI   0(RE),C'&&'        CHANGE & TO /                                 
         BNE   RNB23                                                            
         MVI   0(RE),C'/'                                                       
         B     RNB23                                                            
*                                                                               
RNB23    LA    RE,1(RE)                                                         
         BCT   RF,RNB22                                                         
*                                                                               
         MVC   INVINV(1),STABPER+1    MONTH                                     
         MVC   INVINV+1(2),STABINV                                              
         NI    INVINV+1,X'FF'-(STABINV_REVERSAL+STABINV_REVERSED)               
         MVC   INVPRD,MYBKPRD                                                   
*                                                                               
         MVC   INVEU1D,ESTU1+21       ESTIMATE USER 1 DATA                      
*                                                                               
         MVC   INVEST,STABKEST                                                  
         MVC   INVMOS,STABPER                                                   
         MVC   INVRDAT,STABBDT                                                  
         GOTO1 MSUNPK,DMCB,STABKMKT,WORK,WORK+8                                 
         MVC   INVSTA(6),WORK+8                                                 
         MVC   INVGRS,STABGRS                                                   
         MVC   INVNET,STABNET                                                   
         XC    INVINS,INVINS                                                    
         MVC   HALF,STABSPTS     SPOTS                                          
         LH    R0,HALF                                                          
*                             MAKE ALWAYS POSITIVE                              
         LPR   RE,R0                                                            
*                                                                               
         ST    RE,INVINS                                                        
         XC    INVCD,INVCD       NO CD                                          
         MVC   INVDUE,INVNET      SET TO NET FOR NOW                            
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFFC,X                                    
         CLI   QOPT7,C'Y'          SEE IF TRACING                               
         BNE   RNB12                                                            
         MVC   XP(14),=C'**BUFF PUT D**'                                        
         GOTO1 APRNT                                                            
         GOTO1 HEXOUT,DMCB,X,XP+5,60,=C'N'                                      
         GOTO1 HEXOUT,DMCB,X+60,XP2+5,60,=C'N'                                  
         GOTO1 HEXOUT,DMCB,X+120,XP3+5,60,=C'N'                                 
         GOTO1 HEXOUT,DMCB,X+180,XP4+5,60,=C'N'                                 
         GOTO1 APRNT                                                            
         GOTO1 HEXOUT,DMCB,X+240,XP+5,10,=C'N'                                  
*                                                                               
         B     RNB12                                                            
*                                                                               
RNB30    DS    0H                                                               
         B     RNB4                                                             
*                                                                               
RNB40    DS    0H                                                               
RNBX     DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
RNBNXTEL DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    RNBNXTL2                                                         
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     RNBNXTEL                                                         
RNBNXTL2 DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
         DROP  R4,R7                                                            
         LTORG                                                                  
         EJECT                                                                  
         TITLE 'PROCNET - PROCESS NETPAK UNIT RECORDS'                          
PROCNET  CSECT                                                                  
         NMOD1 0,PROCNET                                                        
*                                                                               
*        RA AND R9 FOR SPWORKD                                                  
*                                                                               
*                                                                               
         L     R6,ANETBLK                                                       
         USING NETBLOCK,R6                                                      
*                                                                               
*                                                                               
         LA    RE,NETBLOCK                                                      
         LH    RF,=Y(NBBLKEND-NETBLOCK)                                         
         XCEF                                                                   
*                                                                               
*                                  SET SELECT OPTIONS                           
         MVI   NBUSER+13,C'N'     ALWAYS PASS PRE-EMPTED UNITS TO A8            
         MVC   NBSELAGY(3),QAGY    AGY/MED                                      
         MVC   NBSELCLI,CLT        CLT                                          
         MVC   NBSELPRD,=C'POL'    ALWAYS DO ALL PRDS                           
         MVC   NBSELEST(2),BEST    END ST/END                                   
         CLC   =C'NO',QEST                                                      
         BNE   *+10                                                             
         MVC   NBSELEFL,QESTEND    EST FILTER                                   
         MVC   NBSELSTR(12),=C'750101991231'                                    
         MVI   NBSELSTR+6,X'FB'                                                 
*                          THESE DATES ARE REALLY 1975 - 2019                   
*                          X'FC'  CAUSED NETIO TO RETURN                        
*                          END BEFORE START ERROR                               
*                          GOOD LUCK TO FUTURE GENERATIONS OF                   
*                          PROGRAMMERS                                          
*                                  SET DATA OPTIONS                             
         MVI   NBDATA,C'U'         UNITS                                        
         MVI   NBSEQ,C'D'          DATE SEQ                                     
         MVC   NBTRCOPT,RCTRACE    TRACE                                        
         MVI   NBFUNCT,NBFNORM     NORMAL FUNCTION                              
         MVI   NBSELPST,C'B'       PASS LOCKED PACKAGES                         
*                                                                               
         MVC   NBAIO,=A(VIRTLREC)  USE VIRTUAL REC AREA                         
         MVC   NBPRINT,PRINT                                                    
         MVC   NBLOADER,LOADER                                                  
         MVC   NBACOM,ACOMFACS                                                  
*                                                                               
         XC    X(250),X                                                         
         LA    R4,X                                                             
         USING INVD,R4                                                          
*                                                                               
NTU10    DS    0H                                                               
         GOTO1 NETIO,DMCB,NETBLOCK                                              
         CLI   NBERROR,NBINVEST                                                 
         BE    NTUXX                                                            
         CLI   NBERROR,NBINVPRD                                                 
         BE    NTUXX                                                            
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBPROCUN                                                  
         BE    NTU12                                                            
         CLI   NBMODE,NBREQLST                                                  
         BE    NTUXX                                                            
         CLI   NBMODE,NBVALCLI     SEE IF I JUST VALIDATED CLIENT               
         BNE   NTU10                                                            
         MVI   NBUSER+13,C'N'      ALWAYS PASS PRE-EMPTED UNITS                 
         B     NTU10                                                            
*                                                                               
         SPACE 3                                                                
* PROCESS UNIT                                                                  
         SPACE 2                                                                
NTU12    DS    0H                                                               
         L     R7,NBAIO                                                         
         USING NURECD,R7                                                        
*                                                                               
         CLI   NUPRD,0              SEE IF UNALLOCATED                          
         BE    NTU10                IF SO SKIP                                  
*                                                                               
         MVI   CKESTREC,C'U'         SET FROM UNIT                              
         GOTO1 =A(CKEST)                                                        
*                                                                               
PROCN1   DS    0H                                                               
*                                                                               
PROCN3   DS    0H                                                               
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         MVC   INVEDESC(20),EDESC                                               
*                                                                               
         LA    RE,INVEDESC                                                      
         LA    RF,20                                                            
ROB22    CLI   0(RE),C','        CHANGE COMMAS TO SPACES                        
         BNE   ROB22C                                                           
         MVI   0(RE),C' '                                                       
         B     ROB23                                                            
*                                                                               
ROB22C   CLI   0(RE),C'&&'       CHANGE & TO /                                  
         BNE   ROB23                                                            
         MVI   0(RE),C'/'                                                       
         B     ROB23                                                            
*                                                                               
ROB23    LA    RE,1(RE)                                                         
         BCT   RF,ROB22                                                         
*                                                                               
*                                                                               
         MVC   INVEST,EKEYEST                                                   
*                                                                               
         DROP  RF                                                               
*                                                                               
PROCN5   DS    0H                                                               
         MVC   SVNUPRD2,NUPRD2       SAVE SECOND PRODUCT                        
*                                                                               
         GOTO1 DATCON,DMCB,(2,NUKDATE),(3,WRKDATE)                              
         MVC   INVMOS,WRKDATE        Y/M - MOS                                  
*                                                                               
*        GET NET ORDERED TIME+INTEG+SPECIAL                                     
*        GET 15 % OF GROSS ORDERED TIME+INTEG+SPECIAL                           
*        THEIR PRD AAA FORMULA IS NET 15% OF GROSS                              
****                                                                            
****     DIFFERENT FOR Y&R??                                                    
****                                                                            
*                                                                               
         PRINT GEN                                                              
         GOTO1 ANETACC,DMCB,(38,NEACCNET),NETBLOCK,0                            
         GOTO1 ANETACC,DMCB,(28,NEACCGRS),NETBLOCK,(2,1500)                     
         GOTO1 ANETACC,DMCB,(28,NEACCGOS),NETBLOCK,0                            
         PRINT NOGEN                                                            
*                                                                               
         ZAP   GRSDUB,NEACCGOS+1(8)     GROSS?                                  
         ZAP   NETDUB,NEACCNET+1(8)     NET                                     
         ZAP   MYDUB,NEACCNET+1(8)                                              
         AP    MYDUB,NEACCGRS+1(8)      MYDUB = NET +15% GROSS                  
*****************************************************************               
*       NEED TO USE CONTIENTIAL'S FORMULA                                       
*****************************************************************               
*                                                                               
*        DO FIRST PRODUCT                                                       
*                                                                               
         L     R5,VCLIST       EXPANDED PRODUCT LIST                            
*                                                                               
NTU12A   CLC   3(1,R5),NUPRD                                                    
         BE    NTU12B                                                           
         LA    R5,4(R5)                                                         
         CLI   0(R5),0      END OF LIST?                                        
         BNE   NTU12A                                                           
         DC    H'0'         CAN'T FIND PRODUCT                                  
*                                                                               
NTU12B   MVC   INVPRD,0(R5)                                                     
*                                                                               
         ZAP   SDUB,MYDUB        SET SHARE TO FULL VALUES                       
         ZAP   SNETDUB,NETDUB                                                   
         ZAP   SGRSDUB,GRSDUB                                                   
         MVC   INVINS,=F'1'      SET FOR ONE UNIT                               
*                                                                               
         CLI   NUPRD2,0          DO I HAVE A SECOND PRD?                        
         BE    NTU12BX                                                          
*                                                                               
         CVB   R0,MYDUB                                                         
         ST    R0,WK                                                            
         CVB   R0,NETDUB                                                        
         ST    R0,WK+4                                                          
         CVB   R0,GRSDUB                                                        
         ST    R0,WK+8                                                          
         SR    RF,RF                                                            
         ICM   RF,3,NUP1SHR                                                     
         BAS   RE,SETSHR           GET SHARE FOR THIS PRD                       
*                                  DOES 3 AMOUNTS                               
         L     R0,WK                                                            
         CVD   R0,SDUB                                                          
         L     R0,WK+4                                                          
         CVD   R0,SNETDUB                                                       
         L     R0,WK+8                                                          
         CVD   R0,SGRSDUB                                                       
*                                                                               
*                                                                               
NTU12BX  DS    0H                                                               
*                                                                               
NTU12C   DS    0H                                                               
*                                                                               
         CVB   R0,SDUB               SDUB - AMOUNT DUE                          
         ST    R0,INVDUE                                                        
         CVB   R0,SNETDUB            SNETDUB IS NET                             
         ST    R0,INVNET                                                        
         CVB   R0,SGRSDUB                                                       
         ST    R0,INVGRS                                                        
         XC    INVCD,INVCD          ZERO CASH DISCOUNT                          
         MVC   INVSTA(L'NUKNET),NUKNET   NETWORK                                
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,X                                  
         CLI   QOPT7,C'Y'          SEE IF TRACING                               
         BNE   NTU13                                                            
         MVC   XP(14),=C'**BUFF PUT D**'                                        
         GOTO1 APRNT                                                            
         GOTO1 HEXOUT,DMCB,X,XP+5,60,=C'N'                                      
         GOTO1 HEXOUT,DMCB,X+60,XP2+5,60,=C'N'                                  
         GOTO1 HEXOUT,DMCB,X+120,XP3+5,60,=C'N'                                 
         GOTO1 HEXOUT,DMCB,X+180,XP4+5,60,=C'N'                                 
         GOTO1 APRNT                                                            
         GOTO1 HEXOUT,DMCB,X+240,XP+5,10,=C'N'                                  
*                                                                               
NTU13    CLI   SVNUPRD2,0          DO I HAVE A SECOND PRD?                      
         BE    NTU10               NO - THEN FINISHED                           
         CLI   NUPRD2,0            MEANS I DID IT ALREADY                       
         BE    NTU10                                                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,NUP1SHR        1ST PRD SHARE                                
         SH    RF,=H'10000'        COMPLEMENT                                   
         LCR   RF,RF                                                            
         CVB   R0,MYDUB                                                         
         ST    R0,WK                                                            
         CVB   R0,NETDUB                                                        
         ST    R0,WK+4                                                          
         CVB   R0,GRSDUB                                                        
         ST    R0,WK+8                                                          
         BAS   RE,SETSHR           GET SHARE FOR THIS PRD                       
*                                  DOES 3 AMOUNTS                               
         L     R0,WK                                                            
         CVD   R0,SDUB                                                          
         L     R0,WK+4                                                          
         CVD   R0,SNETDUB                                                       
         L     R0,WK+8                                                          
         CVD   R0,SGRSDUB                                                       
*                                                                               
*              MUST GET PRD CODE FROM CLIENT HEADER                             
NTU18A   L     R5,VCLIST         EXPANDED PRODUCT LIST                          
NTU18B   CLC   3(1,R5),NUPRD2                                                   
         BE    NTU18C                                                           
         LA    R5,4(R5)                                                         
         CLI   0(R5),0            END OF LIST?                                  
         BNE   NTU18B                                                           
         DC    H'0'                MUST FIND PRD                                
*                                                                               
NTU18C   MVC   INVPRD,0(R5)                                                     
         MVI   NUPRD2,0         SO I WON'T REDO                                 
         B     NTU12BX          PROCESS 2ND PRODUCT                             
*                                                                               
         DROP  R6                COVERED NETBLOCK                               
         DROP  R7                                                               
         DROP  R4                                                               
*                                                                               
NTUXX    XIT1                      REST IS SAME AS SPOT                         
         SPACE 2                                                                
NXTEL    DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NXTEL2                                                           
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NXTEL                                                            
*                                                                               
NXTEL2   DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                  SET CC NOT EQ                                
         SPACE 2                                                                
*                                                                               
CKPRD    CLC   QPRD,=C'ALL'                                                     
         BE    CKPYES                                                           
         CLC   QPRD,=C'   '                                                     
         BE    CKPYES                                                           
         CLC   QPRD,=C'POL'                                                     
         BE    CKPYES                                                           
         CLC   WPRD,BPRD                                                        
         BE    CKPYES              PRD NOT OK - RETURN WITH CC NE               
         BR    RE                                                               
*                                                                               
CKPYES   CR    RE,RE               PRD OK                                       
         BR    RE                                                               
         SPACE 2                                                                
         EJECT                                                                  
         SPACE 2                                                                
NTUDIV   DS    0H                                                               
         LTR   RF,RF               RF HAS SHARE PCT. 2 DECIMALS                 
         BP    *+8                                                              
         SR    R1,R1                                                            
         BR    RE                                                               
*                                                                               
         SLDA  R0,1                                                             
         D     R0,=F'10000'                                                     
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
SETSHR   NTR1                                                                   
         LA    R5,3                3 AMOUNTS                                    
         LA    R6,WK                                                            
*                                                                               
SETS2    DS    0H                                                               
         L     R1,0(R6)                                                         
         M     R0,=F'1'                                                         
         MR    R0,RF                                                            
         BAS   RE,NTUDIV                                                        
         ST    R1,0(R6)                                                         
         LA    R6,4(R6)                                                         
         BCT   R5,SETS2                                                         
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
         TITLE 'PRBILL  - PROCESS  BILL RECORDS'                                
PRBILL   CSECT                                                                  
         NMOD1 0,PRBILL                                                         
         TM    KEY+13,X'80'         SKIP DELETED                                
         BNZ   ROBX                                                             
*                                  PASS DATA TO SORT                            
ROB10    DS    0H                                                               
*                                                                               
         L     R6,ADBILL                                                        
         USING BILLREC,R6                                                       
*                                                                               
         CLC   BDATE,MYSTART                                                    
         BL    ROBX                                                             
         CLC   BDATE,MYEND                                                      
         BH    ROBX                                                             
         TM    BILSTAT,X'20'       SKIP AOR BILLS                               
         BNZ   ROBX                SKIP NET (USED FOR SOMETHING ELSE)           
*                                                                               
ROB10B   DS    0H                                                               
         CLI   PUERROR,0           ANY PRD USER ERRORS                          
         BE    ROB10BX                                                          
*                                                                               
         TM    PUERROR,X'01'                                                    
         BZ    ROB10B5                                                          
         MVC   XP,XSPACES                                                       
         MVC   XP(41),=C'** MISSING CLIENT # - PRD USER FIELD 1 **'             
         L     RF,ADPRD                                                         
         MVC   XP+45(3),PKEYPRD-PRDHDR(RF)                                      
         MVI   SPACING,2                                                        
         GOTO1 APRNT                                                            
         CLI   QOPT6,C'Y'            SEE IF TEST RUN                            
         BE    ROB10B5               CONTINUE - ELSE DIE                        
         MVC   XP(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 APRNT                                                            
         DC    H'0'              MUST DIE                                       
*                                                                               
ROB10B5  DS    0H                                                               
*                                                                               
         TM    PUERROR,X'02'                                                    
         BZ    ROB10B8                                                          
         MVC   XP,XSPACES                                                       
         MVC   XP(41),=C'** MISSING CLIENT # - PRD USER FIELD 1 **'             
         L     RF,ADPRD                                                         
         MVC   XP+45(3),PKEYPRD-PRDHDR(RF)                                      
         MVI   SPACING,2                                                        
         GOTO1 APRNT                                                            
         CLI   QOPT6,C'Y'            SEE IF TEST RUN                            
         BE    ROB10B8               CONTINUE - ELSE DIE                        
         MVC   XP(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 APRNT                                                            
         DC    H'0'              MUST DIE                                       
*                                                                               
ROB10B8  DS    0H                REALLY SHOULD NEVER GET HERE                   
*                                                                               
ROB10BX  MVI   CKESTREC,C'L'       FROM BILL                                    
         GOTO1 =A(CKEST)           MIGHT NEED TO RE-READ ESTIMATE               
         XC    X(250),X                                                         
         LA    R4,X                                                             
         USING INVD,R4                                                          
*                                                                               
         MVC   INVINV(1),BKEYMSRV       MTH OF SERV                             
         MVC   INVINV+1(2),BKEYINV                                              
*                                                                               
         MVC   INVPRD,BKEYPRD                                                   
         MVC   INVEST,BKEYEST                                                   
*                                                                               
         CLI   NETOPT,C'N'            NETWORK?                                  
         BNE   ROB20B2                                                          
         MVC   INVNSMED,BLMED         NETWORK SUBMEDIA                          
         CLI   BLMED,C' '             IS IT THERE?                              
         BH    *+8                                                              
         MVI   INVNSMED,C'N'          DEFAULT TO N                              
*                                                                               
ROB20B2  L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         MVC   INVEDESC(20),EDESC     ESTIMATE NAME                             
         DROP  RF                                                               
         OC    INVEDESC(L'INVEDESC),SPACES                                      
*                                                                               
         LA    RE,INVEDESC                                                      
         LA    RF,20                                                            
ROB20B5  CLI   0(RE),C','        CHANGE COMMAS TO SPACES                        
         BNE   ROB20B6                                                          
         MVI   0(RE),C' '                                                       
         B     ROB20B8                                                          
*                                                                               
ROB20B6  CLI   0(RE),C'&&'         CHANGE & TO /                                
         BNE   ROB20B8                                                          
         MVI   0(RE),C'/'                                                       
         B     ROB20B8                                                          
*                                                                               
ROB20B8  LA    RE,1(RE)                                                         
         BCT   RF,ROB20B5                                                       
*                                                                               
*                                                                               
****     BAS   RE,ROBSETU          PEUSER1 => INVEUSER                          
******                                                                          
**3/10/93ZAP   DUB,PBILLRCV            WAS PBILLRCV?                            
******   CVB   R0,DUB                                                           
******   ST    R0,INVGRS                                                        
*                                        (WILL CATCH BILL FORMULAS)             
         ZAP   DUB,BNETP         NET                                            
         CVB   R0,DUB                                                           
         ST    R0,INVNET                                                        
         ZAP   DUB,BGRSP                                                        
         CVB   R0,DUB                                                           
         ST    R0,INVGRS                                                        
         XC    INVCD,INVCD       NO CD FOR SPOT/NET                             
****                                                                            
         ZAP   DUB,BACTP        AMOUNT DUE                                      
         CVB   R0,DUB                                                           
         ST    R0,INVDUE                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(0,BDATE),(2,INVRDAT)                                
         GOTO1 (RF),(R1),(0,BQDATE),(2,INVINVD)                                 
         GOTO1 (RF),(R1),(3,BDUEDATE),(2,INVDUED)                               
         MVC   INVMOS,BKEYYSRV        YEAR AND MOS                              
*                                                                               
         MVC   INVPU1N,PRDU1         NAME OF PRD USER 1                         
         MVC   INVPU1D,PRDU1+21      DATA OF PRD USER 1                         
         MVC   INVPU2N,PRDU2         NAME OF PRD USER 2                         
         MVC   INVPU2D,PRDU2+21      DATA OF PRD USER 2                         
         MVC   INVEU1N,ESTU1         NAME OF EST USER 1                         
         MVC   INVEU1D,ESTU1+21      DATA OF EST USER 1                         
         MVC   INVEU2N,ESTU2         NAME OF EST USER 2                         
         MVC   INVEU2D,ESTU2+21      DATA OF EST USER 2                         
*                                                                               
         DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFFC,X                                    
         CLI   QOPT7,C'Y'          SEE IF TRACING                               
         BNE   ROBX                                                             
         MVC   XP(14),=C'**BUFF PUT I**'                                        
         GOTO1 APRNT                                                            
         GOTO1 HEXOUT,DMCB,X,XP+5,60,=C'N'                                      
         GOTO1 HEXOUT,DMCB,X+60,XP2+5,60,=C'N'                                  
         GOTO1 HEXOUT,DMCB,X+120,XP3+5,60,=C'N'                                 
         GOTO1 HEXOUT,DMCB,X+180,XP4+5,10,=C'N'                                 
         GOTO1 APRNT                                                            
         GOTO1 HEXOUT,DMCB,X+240,XP+5,10,=C'N'                                  
         GOTO1 APRNT                                                            
         GOTO1 APRNT    SKIP A LINE                                             
*                                                                               
         MVC   XP+5(60),X                                                       
         MVC   XP2+5(60),X+60                                                   
         MVC   XP3+5(60),X+120                                                  
         MVC   XP4+5(60),X+180                                                  
         GOTO1 APRNT                                                            
         MVC   XP+5(10),X+240                                                   
         GOTO1 APRNT                                                            
         B     ROBX                                                             
*                                                                               
ROB40    DS    0H                                                               
ROBX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         TITLE 'CKEST - READ ESTIMATE FOR VARIOUS RECORDS'                      
CKEST    CSECT                                                                  
         NMOD1 0,CKEST                                                          
*****                                                                           
*****    NOTE - DO NOT USE REGISTERS  R3, R8, R9, RA                            
*****           IN THIS CSECT                                                   
*****           THEY ARE USED FOR THE WHOLE PROGRAM                             
*****                                                                           
*                                                                               
         DS    0H                                                               
         MVC   PPGKEY,KEY                                                       
         MVC   PPGAREC,AREC                                                     
         MVI   CKESTSW,0    WILL BE SET TO X'01' IF I READ SOMETHING            
         XC    KEY,KEY                                                          
         L     RF,ADBUY                                                         
         USING BUYREC,RF                                                        
         MVC   KEY+1(1),BUYKAM    A/M                                           
         MVC   KEY+2(2),BUYKCLT   CLT                                           
         MVC   KEY+4(3),PRD       PRD                                           
         MVC   KEY+7(1),BUYKEST   EST                                           
         CLI   CKESTREC,C'B'        FROM SPOT BUY                               
         BE    CKEST3                                                           
*                                                                               
         DROP  RF                                                               
*                                                                               
         XC    KEY,KEY                                                          
         L     RF,ADSTABUC                                                      
         USING STABUCK,RF                                                       
         MVC   KEY+1(1),STABKAM   A/M                                           
         MVC   KEY+2(2),STABKCLT  CLT                                           
         MVC   KEY+4(3),MYBKPRD   PRD                                           
         MVC   KEY+7(1),STABKEST  EST                                           
         CLI   CKESTREC,C'K'        FROM STATION BUCKET REC                     
         BE    CKEST3                                                           
*                                                                               
         DROP  RF                                                               
*                                                                               
         L     RF,ADBILL                                                        
         USING BILLREC,RF                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(8),BILLREC      00/A/M/CLT/PRD/EST                           
         CLI   CKESTREC,C'L'        FROM BILL                                   
         BE    CKEST3                                                           
*                                                                               
         DROP  RF                                                               
*                                                                               
         L     R4,ANETBLK                                                       
         USING NETBLOCK,R4                                                      
         L     RF,NBAIO                                                         
         USING NURECD,RF                                                        
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),NUKAM                                                   
         MVC   KEY+2(2),NUKCLT                                                  
         MVC   KEY+4(3),=C'POL'     PRODUCT POL                                 
         MVC   KEY+7(1),NUKEST                                                  
         CLI   CKESTREC,C'U'        FROM A UNIT                                 
         BE    CKEST3                                                           
*                                                                               
         DC    H'0'                 ERROR - UNKNOWN RECORD TYPE                 
*                                                                               
         DROP  R4                                                               
         DROP  RF                                                               
*                                                                               
CKEST3   L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         CLC   ESTHDR(8),KEY        SEE IF I ALREADY HAVE EST                   
         BE    CKEST5                                                           
         MVI   CKESTSW,1                                                        
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                EST MUST BE ON FILE                          
         GOTO1 GETEST                                                           
******************************************************************              
CKEST5   DS    0H                                                               
         XC    ESTU1,ESTU1                                                      
         XC    ESTU2,ESTU2                                                      
         L     RF,ADPRD                                                         
         CLC   =C'AAA',PKEYPRD-PRDHDR(RF)   FOR PRD=AAA                         
         BE    CKESTSAV                                                         
*                                                                               
         GOTO1 VGETUSER,DMCB,(C'S',ADCLT),(C'E',ADEST),ESTU1,ESTU2              
         CLI   DMCB,X'FF'                                                       
         BE    CKESTERR                                                         
         CLI   ESTU1+21,C' '    MUST FIND DATA                                  
         BNH   CKESTERR                                                         
         B     CKESTSAV                                                         
*                                                                               
CKESTERR DS    0H                                                               
*                                                                               
         B     CKESTX       NOT REQUIRED?                                       
*                                                                               
         MVC   XP,XSPACES                                                       
         MVC   XP(37),=C'*** MISSING ESTIMATE USER FIELD 1 ***'                 
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         MVC   XP+41(3),EKEYPRD                                                 
         EDIT  (B1,EKEYEST),(3,XP+46),0                                         
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         CLI   QOPT6,C'Y'            SEE IF TEST RUN                            
         BE    CKESTSAV              CONTINUE - ELSE DIE                        
         MVC   XP(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
CKESTSAV DS   0H                                                                
*                                                                               
CKESTX   OC    ESTU1,SPACES                                                     
         OC    ESTU2,SPACES                                                     
*                                                                               
CKEST80  MVC   KEY,PPGKEY                                                       
         MVC   AREC,PPGAREC                                                     
         CLI   CKESTSW,1           SEE IF I READ SOMETHING                      
         BNE   CKESTXX             NO - SKIP READ HIGH                          
         GOTO1 HIGH                                                             
CKESTXX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
         TITLE 'REPRT - CREATE TAPE RECS AND PRINT REPORT'                      
REPRT    CSECT                                                                  
         NMOD1 0,REPRT                                                          
         SPACE 2                                                                
*                                                                               
         MVC   MYSVMODE,MODE                                                    
         MVI   MODE,REQFRST       TO INITIALIZE DOWNLOAD                        
         GOTO1 VDOWNLD,DMCB,(RA)                                                
         MVC   MODE,MYSVMODE                                                    
*                                                                               
         XC    SVHDR(250),SVHDR                                                 
         MVI   FORCEHED,C'Y'                                                    
         LA    R2,XP                                                            
         USING LINED,R2                                                         
         XC    BUFFREC(250),BUFFREC                                             
         LA    R4,BUFFREC                                                       
         USING INVD,R4                                                          
REP2     DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFFC,BUFFREC,0                           
         TM    DMCB+8,X'80'                                                     
         BNZ   REP50                                                            
*                                                                               
         CLI   HDRDONE,C'Y'        SEE IF ALREADY DONE                          
         BE    REP4BX                                                           
*                                                                               
         BAS   RE,HDROUT           DO HEADERS                                   
*                                                                               
         B     REP4BX                                                           
*                                                                               
REP4     DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFFC,BUFFREC,0                            
REP4B    DS    0H                                                               
         TM    DMCB+8,X'80'                                                     
         BNZ   REP50                                                            
*                                                                               
REP4BX   CLI   QOPT7,C'Y'          SEE IF TRACING                               
         BNE   REP6                                                             
         MVC   XP(12),=C'**BUFF OUT**'                                          
         GOTO1 APRNT                                                            
         GOTO1 HEXOUT,DMCB,BUFFREC,XP+5,60,=C'N'                                
         GOTO1 HEXOUT,DMCB,BUFFREC+60,XP2+5,60,=C'N'                            
         GOTO1 APRNT                                                            
         GOTO1 HEXOUT,DMCB,BUFFREC+120,XP+5,60,=C'N'                            
         GOTO1 HEXOUT,DMCB,BUFFREC+180,XP2+5,60,=C'N'                           
         GOTO1 APRNT                                                            
         GOTO1 HEXOUT,DMCB,BUFFREC+240,XP+5,10,=C'N'                            
         GOTO1 APRNT                                                            
*                                                                               
REP6     DS    0H                                                               
         OC    INVSTA,INVSTA       TEST FOR HEADER                              
         BNZ   REP10                                                            
*                                  INVOICE HEADER                               
         OC    SVHDR(250),SVHDR         ANY OLD HDR TO FINISH?                  
         BZ    *+8                                                              
         BAS   RE,ENDINV                                                        
*                                                                               
         GOTO1 APRNT                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(2,INVRDAT),(0,WORK)                                 
         GOTO1 AFMTINO,DMCB,WORK,(2,INVINV+1),(MED,B1PROF),B1XPROF              
*                                                                               
         L     RF,DMCB                                                          
         MVC   DINVFULL,0(RF)    FULL INVOICE NUMBER                            
*                                                                               
         L     RF,DMCB+12          MEDIA PORTION                                
         MVC   DINVNDH(2),0(RF)                                                 
         L     RF,DMCB+4                                                        
         MVC   DINVNDH+2(2),0(RF)    MONTH PORTION                              
         MVC   DINVNDH+4(4),3(RF)  INVOICE NUMBER                               
*                                                                               
******   MVI   DOWNSW,C'I'    INVOICE DATA  LINE 1                              
******   GOTO1 VDOWNLD,DMCB,(RA)                                                
*                                                                               
         MVI   DOWNSW,C'R'    INVOICE DATA  AMT DUE LINE 2                      
         GOTO1 VDOWNLD,DMCB,(RA)                                                
*                                                                               
         MVI   DOWNSW,C'P'    INVOICE DATA  NET LINE 2                          
         GOTO1 VDOWNLD,DMCB,(RA)                                                
*                                                                               
         CLC   INVDUE,INVNET   SEE IF NO COMMISSION                             
         BE    REP7                                                             
*                                                                               
         MVI   DOWNSW,C'G'    INVOICE DATA  COMMISSION LINE 2                   
         GOTO1 VDOWNLD,DMCB,(RA)                                                
*                                                                               
REP7     XC    ITGRS(12),ITGRS     CLEAR INV CHECKING TOTALS                    
         MVC   LINV,DINVFULL                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(2,INVINVD),(5,LIDAT)                                
         GOTO1 (RF),(R1),(2,INVDUED),(5,LDDAT)                                  
         GOTO1 (RF),(R1),(2,INVRDAT),(5,LRDAT)                                  
*                                                                               
         MVC   SVHDR(250),INVD     SAVE HEADER                                  
         AP    GTINVS,=P'1'        BUMP INVOICE COUNT                           
         B     REP30                                                            
*                                                                               
REP10    DS    0H                  DETAIL LINE                                  
*NOP*    BAS   RE,DETOUT           DO DETAIL OUTPUT                             
*NOP*    GOTO1 DATCON,DMCB,(3,INVPER),(0,WORK)                                  
*NOP*    GOTO1 DATCON,DMCB,(3,INVPER),(X'20',WORK)                              
*NOP*    MVC   LPER,WORK                                                        
*                                                                               
         EDIT  (B4,INVINS),(7,LINS),COMMAS=YES,FLOAT=-,ZERO=NOBLANK             
*                                                                               
         MVC   LSTA,INVSTA                                                      
         CLC   LSTA(5),=C'YCKDX'  SEE IF MANUAL BILL                            
         BNE   *+10                                                             
         MVC   LSTA(06),=C'MANUAL'                                              
*                                                                               
REP22    DS    0H                                                               
         EDIT  INVGRS,(15,LGRS),2,COMMAS=YES,FLOAT=-                            
         EDIT  INVNET,(15,LNET),2,COMMAS=YES,FLOAT=-                            
         EDIT  INVCD,(15,LCD),2,COMMAS=YES,FLOAT=-                              
         EDIT  INVDUE,(15,LDUE),2,COMMAS=YES,FLOAT=-                            
*                                                                               
         ICM   R0,15,INVGRS        ADD TO INV, REQ, AND RUN TOTALS              
         CVD   R0,DUB                                                           
         AP    QTGRS,DUB                                                        
         AP    GTGRS,DUB                                                        
         A     R0,ITGRS                                                         
         ST    R0,ITGRS                                                         
         ICM   R0,15,INVNET                                                     
         CVD   R0,DUB                                                           
         AP    QTNET,DUB                                                        
         AP    GTNET,DUB                                                        
         A     R0,ITNET                                                         
         ST    R0,ITNET                                                         
         ICM   R0,15,INVCD                                                      
         CVD   R0,DUB                                                           
         AP    QTCD,DUB                                                         
         AP    GTCD,DUB                                                         
         A     R0,ITCD                                                          
         ST    R0,ITCD                                                          
         AP    GTLINS,=P'1'                                                     
*                                                                               
         GOTO1 APRNT                                                            
*                                                                               
REP30    DS    0H                                                               
         B     REP4                                                             
*                                                                               
REP50    DS    0H                                                               
         OC    SVHDR(250),SVHDR         ANY HEADER TO FINISH UP?                
         BZ    REP60                                                            
         BAS   RE,ENDINV                                                        
*                                                                               
         GOTO1 APRNT                                                            
*                                                                               
REP60    GOTO1 BUFFALO,DMCB,=C'RESET',ABUFFC                                    
*                                                                               
REPX     DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
*        FINISH UP OLD HEADER                                                   
         SPACE 2                                                                
ENDINV   NTR1                                                                   
         MVC   LINS(15),=C'*INVOICE TOTAL*'                                     
         ICM   R0,15,SVHDR+INVGRS-INVD                                          
         EDIT  (R0),(15,LGRS),2,COMMAS=YES,FLOAT=-                              
         ICM   R0,15,SVHDR+INVNET-INVD                                          
         EDIT  (R0),(15,LNET),2,COMMAS=YES,FLOAT=-                              
         ICM   R0,15,SVHDR+INVCD-INVD                                           
         EDIT  (R0),(15,LCD),2,COMMAS=YES,FLOAT=-                               
         ICM   R0,15,SVHDR+INVDUE-INVD                                          
         EDIT  (R0),(15,LDUE),2,COMMAS=YES,FLOAT=-                              
*                                                                               
         OC    ITGRS(12),ITGRS       ANY DETAIL $?                              
         BZ    EINV6     PROBABLY A MANUAL BILL                                 
*                                                                               
         CLC   ITGRS,SVHDR+INVGRS-INVD                                          
         BNE   EINV4                                                            
         CLC   ITNET,SVHDR+INVNET-INVD                                          
         BNE   EINV4                                                            
         CLC   ITCD,SVHDR+INVCD-INVD                                            
         BNE   EINV4                                                            
*                                                                               
EINV6    L     R0,SVHDR+INVDUE-INVD  ADD TO DUE TOTALS                          
         CVD   R0,DUB                                                           
         AP    QTDUE,DUB                                                        
         AP    GTDUE,DUB                                                        
*                                                                               
         B     EINVX                                                            
*                                                                               
EINV4    DS    0H                                                               
         MVC   LCD+132(18),=C'**OUT OF BALANCE**'                               
         MVI   ERROR,C'Y'                                                       
*                                                                               
EINVX    DS    0H                                                               
         GOTO1 APRNT                                                            
         XIT1                                                                   
         EJECT                                                                  
*        HEADER OUTPUT                                                          
         SPACE 2                                                                
HDROUT   NTR1                                                                   
         MVI   DOWNSW,C'H'    INVOICE HEADER                                    
         GOTO1 VDOWNLD,DMCB,(RA)                                                
         MVI   HDRDONE,C'Y'                                                     
         XIT1                                                                   
         EJECT                                                                  
*        DETAIL OUTPUT                                                          
         SPACE 2                                                                
DETOUT   NTR1                                                                   
         MVI   DOWNSW,C'D'      INVOICE DETAIL                                  
         GOTO1 VDOWNLD,DMCB,(RA)                                                
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         TITLE 'PRNT - PRINT CONTROL MODULE'                                    
PRNT     CSECT                                                                  
         NMOD1 0,PRNT                                                           
*                                                                               
         MVI   RCSUBPRG,10                                                      
         CLI   MODE,RUNLAST                                                     
         BNE   *+8                                                              
         MVI   RCSUBPRG,0                                                       
*                                                                               
         MVC   XHEAD3+46(4),=C'FROM'                                            
         GOTO1 DATCON,DMCB,(0,MYSTART),(5,XHEAD3+51)                            
         MVC   XHEAD3+60(2),=C'TO'                                              
         GOTO1 (RF),(R1),(0,MYEND),(5,XHEAD3+63)                                
         GOTO1 REPORT                                                           
*                                                                               
PRNTX    DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
DOWNLD   CSECT                                                                  
         NMOD1 0,DOWNLD                                                         
         L     RA,0(R1)                                                         
         USING SPWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         LA    R1,DLCB                                                          
         USING DLCBD,R1                                                         
         XC    DLCB(L'DLCB),DLCB   CLEAR IT                                     
*                                                                               
         MVC   DLCBFLD,SPACES    CLEAR FIELD                                    
         OI    DLCBFLG1,DLCBFXTN                                                
         MVC   DLCXTND(7),MAXLINE                                               
         MVC   DLCBAPR,=A(DNPRINT)                                              
         LA    R0,XP                                                            
         ST    R0,DLCBAPL                                                       
         MVC   MYP,XP             SAVE PRINT LINE                               
*                                                                               
         MVC   XP,XSPACES                                                       
*                                                                               
         DROP  R2                                                               
*                                                                               
         CLI   MODE,RUNLAST       SEE IF END OF REPORT                          
         BE    DNP75                                                            
*                                                                               
         CLI   MODE,REQFRST       SEE IF I NEED TO INTIALIZE                    
         BE    DNP80                                                            
*                                                                               
         CLI   DOWNSW,C'H'        SEE IF DOING HEADERS                          
         BE    DNP10                                                            
         CLI   DOWNSW,C'F'        SEE IF DOING FOOTER                           
         BE    DNP60                                                            
*******  CLI   DOWNSW,C'I'        SEE IF INVOICE LINE 1                         
*******  BE    DNP40                                                            
         B     DNP50              MUST BE R,P, OR G (LINE 2'S)                  
*                                                                               
*        HEADER 1                                                               
*                                                                               
DNP10    MVC   DLCBFLD(14),=C'JOURNAL:FORMAT'                                   
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(13),=C'JournalNumber'                                    
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(11),=C'JournalType'                                      
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(10),=C'ToBePosted'                                       
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(13),=C'CompanyNumber'                                    
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(12),=C'LocationName'                                     
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(14),=C'LocalSpec2Name'                                   
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(14),=C'LocalSpec3Name'                                   
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVI   DLCBACT,DLCBEOL    SEND END OF LINE                              
         GOTO1 VDLFLD                                                           
*                                                                               
*        HEADER 2                                                               
*                                                                               
         MVC   DLCBFLD(21),=C'GENERALJOURNAL:FORMAT'                            
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(13),=C'JournalNumber'                                    
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(10),=C'LineNumber'                                       
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(11),=C'TypeOfEntry'                                      
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(13),=C'AccountNumber'                                    
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(09),=C'JobNumber'                                        
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(08),=C'TaskName'                                         
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(17),=C'TransactionNumber'                                
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(09),=C'EntryDate'                                        
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(08),=C'Currency'                                         
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(14),=C'AmountCurrency'                                   
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(09),=C'EntryText'                                        
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVI   DLCBACT,DLCBEOL    SEND END OF LINE                              
         GOTO1 VDLFLD                                                           
*                                                                               
*******  B     DNPX                                                             
*                                                                               
*        END OF HEADLINES                                                       
*                                                                               
*        INVOICE DATA - LINE 1                                                  
*                                                                               
DNP30    DS    0H                                                               
         MVC   DLCBFLD(07),=C'JOURNAL'                                          
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(05),=C'#KEEP'                                            
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(07),=C'General'                                          
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(01),=C'1'                                                
         MVI   DLCBTYP,C'N'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(03),=C'511'     FOR OFFICE G2                            
         L     RF,ADCLT                                                         
         CLI   COFFICE-CLTHDR(RF),X'49'  HEX FOR OFFICE G2                      
         BE    DNP30A                                                           
         MVC   DLCBFLD(03),=C'517'     FOR OFFICE G3                            
         CLI   COFFICE-CLTHDR(RF),X'4C'  HEX FOR OFFICE G3                      
         BE    *+6                                                              
*                                                                               
         DC    H'0'               INVALID OFFICE                                
*                                                                               
DNP30A   MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(04),=C'0190'                                             
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(04),=C'0510'                                             
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(04),=C'0510'                                             
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVI   DLCBACT,DLCBEOL    SEND END OF LINE                              
         GOTO1 VDLFLD                                                           
         B     DNPX                                                             
*                                                                               
*        INVOICE DATA - LINE 2                                                  
*                                                                               
DNP50    MVC   DLCBFLD(14),=C'GENERALJOURNAL'                                   
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(05),=C'#KEEP'                                            
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(05),=C'#KEEP'                                            
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(01),DOWNSW      R, P, OR G                               
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLI   DOWNSW,C'G'           ACCOUNT  - NONE FOR G                      
         BNE   DNP50A                                                           
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP50C                                                           
*                                                                               
DNP50A   MVC   DLCBFLD(06),INVPU1D   CLIENT NUMBER - PRD USER 1                 
         CLI   DOWNSW,C'R'                                                      
         BE    *+10                                                             
         MVC   DLCBFLD(10),=C'5101000290'  MUST BE P - USE FIXED CODE           
*                                                                               
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP50C   CLI   DOWNSW,C'G'           JOB NUMBER AND TASK ONLY FOR G             
         BE    DNP50G                                                           
*                                    OTHERWISE SEND 2 EMPTY FIELDS              
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP52                                                            
*                                                                               
*                                                                               
DNP50G   MVC   DLCBFLD(12),INVPU2D   JOB NUMBER - PRD USER2                     
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(06),=C'C00110'                                           
         CLI   QMED,C'T'                 SPOT - TV                              
         BE    DNP51                                                            
         MVC   DLCBFLD(06),=C'C00111'                                           
         CLI   QMED,C'R'                 SPOT - RADIO                           
         BE    DNP51                                                            
*                                                                               
         CLI   NETOPT,C'N'               NETWORK?                               
         BE    *+6                                                              
         DC    H'0'                      INVALID SPOT MEDIA                     
*                                        THEY DON'T USE X                       
         MVC   DLCBFLD(06),=C'C00120'                                           
         CLI   INVNSMED,C'N'             NET - NETWORK                          
         BE    DNP51                                                            
         MVC   DLCBFLD(06),=C'C00121'                                           
         CLI   INVNSMED,C'C'             NET - CABLE                            
         BE    DNP51                                                            
         MVC   DLCBFLD(06),=C'C00122'                                           
         CLI   INVNSMED,C'S'             NET - SYNDICATION                      
         BE    DNP51                                                            
         MVC   DLCBFLD(06),=C'C00123'                                           
         CLI   INVNSMED,C'T'             NET - RADIO                            
         BE    DNP51                                                            
         MVC   DLCBFLD(06),=C'C00124'                                           
         CLI   INVNSMED,C'O'             NET - UNWIRED (OTHER)                  
         BE    DNP51                                                            
         DC    H'0'                      INVALID MEDIA/SUBMEDIA                 
*                                                                               
DNP51    MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
**P52    MVC   DLCBFLD(03),=C'511'                                              
**       L     RF,ADCLT                                                         
**       CLI   COFFICE-CLTHDR(RF),X'49'     HEX FOR OFFICE G2                   
**       BE    DNP52A                                                           
**       MVC   DLCBFLD(03),=C'517'                                              
**       CLI   COFFICE-CLTHDR(RF),X'4C'     HEX FOR OFFICE G3                   
**       BE    DNP52A                                                           
**                                                                              
**       DC    H'0'               INVALID OFFICE                                
*                                         NO MEDIA NOR DASHES                   
DNP52    DS    0H                                                               
DNP52A   DS    0H                                                               
         MVC   DLCBFLD(8),DINVNDH  DDS INV.# (NO DASHES)                        
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         ST    R1,SAVER1      SAVE R1                                           
         GOTO1 DATCON,DMCB,(2,INVINVD),(X'20',WORK)                             
         L     R1,SAVER1      RESTORE R1                                        
*                                                                               
*        INVOICE DATE FORMAT NOW DD.MM.YYYY                                     
*                                                                               
******                         ASSUME LAST CENTURY                              
******  Y3K NOTE: CODE WILL WORK UNTIL 2090- I'LL BE DEAD                       
******                                                                          
         MVC   DLCBFLD(2),WORK+4       DAY                                      
         MVI   DLCBFLD+2,C'.'                                                   
         MVC   DLCBFLD+3(2),WORK+2     MONTH                                    
         MVI   DLCBFLD+5,C'.'                                                   
         MVC   DLCBFLD+6(2),=C'20'     CENTURY                                  
         CLC   WORK(2),=C'90'                                                   
         BL    *+10                                                             
         MVC   DLCBFLD+6,=C'19'  IF YEAR IS GREATER THAN 90                     
*                              SET CENTURY TO 19                                
         MVC   DLCBFLD+8(2),WORK      YEAR                                      
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(03),=C'USD'                                              
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         ST    R1,SAVER1                                                        
         MVC   WORK(20),SPACES                                                  
         MVC   MYFULL,INVDUE                                                    
         CLI   DOWNSW,C'R'      AMOUNT DUE                                      
         BE    DNP57                                                            
         MVC   MYFULL,INVNET                                                    
         L     R2,MYFULL                                                        
         LCR   R0,R2            SWITCH SIGNS                                    
         ST    R0,MYFULL                                                        
         CLI   DOWNSW,C'P'      NET AMOUNT                                      
         BE    DNP57                                                            
         L     R2,INVDUE                                                        
         S     R2,INVNET        DUE LESS NET = COMMISSION                       
         LCR   R0,R2            SWITCH SIGNS                                    
         ST    R0,MYFULL                                                        
         CLI   DOWNSW,C'G'      COMMISSION                                      
         BE    DNP57                                                            
*                                                                               
         DC    H'0'             INVALID DOWNSW                                  
*                                                                               
DNP57    L     R2,MYFULL                                                        
*                                                                               
         EDIT  (B4,MYFULL),(12,WORK),2,ALIGN=LEFT,FLOAT=-                       
         L     R1,SAVER1                                                        
         CLC   WORK(3),=C'.00'    ZERO $                                        
         BNE   *+10                                                             
         MVC   WORK(4),=C'0.00'                                                 
*                                                                               
         MVC   DLCBFLD(12),WORK                                                 
         MVI   DLCBTYP,C'N'       DATA FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
*        MONTH OF SERVICE                                                       
*                                                                               
         ST    R1,SAVER1                                                        
         MVC   WORK(2),INVMOS                                                   
         MVI   WORK+2,X'01'      DAY TO 01                                      
         GOTO1 DATCON,DMCB,(3,WORK),(6,WORK+6)                                  
         L     R1,SAVER1          RESTORE                                       
*                                                                               
         MVI   DLCXDELC,C' '    SUPPRESS DELIMITER                              
         MVC   DLCBFLD(15),=C'Media Billing -'                                  
         MVC   DLCBFLD+16(4),=C'Spot'                                           
         CLI   NETOPT,C'N'                                                      
         BNE   *+10                                                             
         MVC   DLCBFLD+16(4),=C'Net '                                           
         MVC   DLCBFLD+21(04),=C'TV -'                                          
         LA    RE,DLCBFLD+26           PLACE FOR MTH                            
         CLI   QMED,C'T'                                                        
         BE    DNP58                                                            
         MVC   DLCBFLD+21(07),=C'Radio -'                                       
         LA    RE,DLCBFLD+29           PLACE FOR MTH                            
         CLI   QMED,C'R'                                                        
         BE    DNP58                                                            
*                                                                               
         CLI   NETOPT,C'N'               NETWORK?                               
         BE    *+6                                                              
         DC    H'0'                      INVALID SPOT MEDIA                     
*                                        THEY DON'T USE X                       
         LA    RE,DLCBFLD+28                                                    
         MVC   DLCBFLD+20(07),=C'Cable -'                                       
         CLI   INVNSMED,C'C'                                                    
         BE    DNP58                                                            
         MVC   DLCBFLD+20(07),=C'Radio -'                                       
         CLI   INVNSMED,C'T'                                                    
         BE    DNP58                                                            
         LA    RE,DLCBFLD+30                                                    
         MVC   DLCBFLD+20(09),=C'Unwired -'                                     
         CLI   INVNSMED,C'O'                                                    
         BE    DNP58                                                            
         MVC   DLCBFLD+20(09),=C'Network -'                                     
         CLI   INVNSMED,C'N'                                                    
         BE    DNP58                                                            
         LA    RE,DLCBFLD+34                                                    
         MVC   DLCBFLD+20(13),=C'Syndication -'                                 
         CLI   INVNSMED,C'S'                                                    
         BE    DNP58                                                            
*                                                                               
         DC    H'0'         UNKNOWN MEDIA/SUBMEDIA                              
*                                                                               
DNP58    DS    0H                                                               
         MVI   DLCXDELC,C' '        RESTORE TO SPACE                            
         MVC   0(6,RE),WORK+6                                                   
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVI   DLCBACT,DLCBEOL    SEND END OF LINE                              
         GOTO1 VDLFLD                                                           
*                                                                               
         B     DNPX                                                             
*                                                                               
*        FOOTER                                                                 
*                                                                               
*                                                                               
DNP60    DS    0H                                                               
         MVC   DLCBFLD(12),=C'JOURNAL:POST'                                     
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(05),=C'#KEEP'                                            
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(07),=C'General'                                          
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(01),=C'1'                                                
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
*                                                                               
         MVC   DLCBFLD(03),=C'511'     FOR OFFICE G2                            
         L     RF,ADCLT                                                         
         CLI   COFFICE-CLTHDR(RF),X'49'    HEX FOR OFFICE G2                    
         BE    DNP60A                                                           
         MVC   DLCBFLD(03),=C'517'     FOR OFFICE G3                            
         CLI   COFFICE-CLTHDR(RF),X'4C'    HEX FOR OFFICE G3                    
         BE    DNP60A                                                           
*                                                                               
         CLI   QCLT,C'*'       OFFICE REQUEST?                                  
         BE    *+6                                                              
         DC    H'0'               SOMETHING WRONG                               
         MVC   DLCBFLD(03),=C'511'     FOR OFFICE G2                            
*                                                                               
         CLI   QCLT+1,X'49'    HEX FOR G2                                       
         BE    DNP60A                                                           
         MVC   DLCBFLD(03),=C'517'     FOR OFFICE G3                            
         CLI   QCLT+1,X'4C'    HEX FOR G3                                       
         BE    DNP60A                                                           
*                                 SUCH REQUESTS MAY HAVE READ                   
*                                 A CLIENT NOT IN THE OFFICE REQUESTED          
*                                                                               
         DC    H'0'               BAD OFFICE REQUESTED                          
*                                                                               
DNP60A   MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(04),=C'0190'                                             
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(04),=C'0510'                                             
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(04),=C'0510'                                             
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVI   DLCBACT,DLCBEOL    SEND END OF LINE                              
         GOTO1 VDLFLD                                                           
*                                                                               
         B     DNPX                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
DNP75    DS    0H                 REQLAST                                       
         B     DNPX               DO NOTHING?                                   
*                                                                               
DNP80    DS    0H                                                               
*                                                                               
         CLI   DPAGEIND,X'01'     HAVE PRINTED MY EMPTY LINES?                  
         BE    DNP85                                                            
*                                 THESE NEEDED FOR PIANO                        
         MVC   XP,XSPACES           JUST IN CASE                                
         MVC   SVLINE,LINE                                                      
         MVI   LINE,0                                                           
         MVC   SVFORCEH,FORCEHED                                                
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,100        SEE I KNOW IT DOEN'T EXIST                   
         MVI   RCWHATPR,2                                                       
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'Y'                                                    
**       MVI   RCSUBPRG,100        SEND ANOTHER                                 
**       MVI   RCWHATPR,2                                                       
**       GOTO1 REPORT                                                           
*                                                                               
         MVI   RCSUBPRG,X'10'     RESTORE                                       
         MVI   DPAGEIND,X'01'      SO I WON'T REDO                              
         MVI   RCWHATPR,1         RESET TO 1                                    
         MVC   LINE,SVLINE                                                      
         MVC   FORCEHED,SVFORCEH       AND FORCEHED                             
*                                                                               
DNP85    MVC   XP,XSPACES           JUST IN CASE                                
         MVI   DLCBACT,C'I'        START AND INTIALIZE REPORT                   
         GOTO1 VDLFLD                                                           
DNPX     DS    0H                                                               
         MVC   XP,MYP             RESTORE PRINT LINE                            
         XIT1                                                                   
         DROP  R1                                                               
         DROP  R4                                                               
         SPACE 2                                                                
*                                                                               
DNPRINT  NTR1                                                                   
         MVC   SVLINE,LINE                                                      
         MVC   SVFORCEH,FORCEHED                                                
         MVI   LINE,0                                                           
**       MVI   FORCEHED,C'N'                                                    
         MVI   RCWHATPR,2     SET TO SECOND SYSPRINT                            
         GOTO1 REPORT                                                           
         MVC   LINE,SVLINE             RESTORE LINE                             
         CLI   SVFORCEH,C'Y'     WAS IT Y?                                      
         BNE   *+8                                                              
         MVI   SVFORCEH,C'N'     CHANGE IT TO N                                 
         MVC   FORCEHED,SVFORCEH       AND FORCEHED                             
         MVI   RCWHATPR,1     RESET TO FIRST                                    
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
DMTHSW   DS    CL1         M IF PROCESSING MOS LINE                             
SAVER1   DS    F                                                                
MYWORK   DS    CL12                                                             
DLCB     DS    XL256                                                            
DNLINE   DS    CL198                                                            
         DS    0H                                                               
MAXLINE  DC    H'198'                                                           
DELIM    DC    C' '        FIELD DELIMITER - SPACE                              
EOTCHR   DC    C'"'       END OF TEXT FIELD DELIMITER (quote)                   
EOTALT   DC    X'00'       END OF TEXT CHR ALTERNATE                            
EOLCHR   DC    X'5E'       END OF LINE CHAR - SEMI-COLON                        
EORCHR   DC    C':'        END OF REPORT CHR                                    
*                                                                               
*        NORMAL DOWNLOAD FIELDS ARE:                                            
*        C' ',C'"',C'''',X'5E',C':'     5E IS SEMI-COLON                        
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDLCB                                                         
SPTD02   CSECT                                                                  
         EJECT                                                                  
*       BILLING WORK AREA DSECT                                                 
BILWRKD  DSECT                                                                  
BILWRK   DS    0C                                                               
*                    RELOACTED ADDRESSES                                        
RCONS    DS    0F                                                               
APRNT    DS    A                                                                
APRBILL  DS    A                                                                
AREPRT   DS    A                                                                
ADSTABUC DS    A                                                                
ABUFFC   DS    A                                                                
VDLFLD   DS    A                                                                
VDOWNLD  DS    A                                                                
VGETUSER DS    A                                                                
AFMTINO  DS    A                                                                
VPROCNET DS    A                                                                
ANETBLK  DS    A                                                                
ANETNET  DS    A                                                                
ANETACC  DS    A                                                                
ARNBILL  DS    A                                                                
AWIDEC   DS    A                                                                
*                                                                               
ELCODE   DS    CL1                                                              
NETOPT   DS    CL1    N- IF NETPAK                                              
*                                                                               
RELO     DS    A                                                                
*                                                                               
WRKDATE  DS    XL3                                                              
DOWNSW   DS    CL1    H- HEADERS, R,C,G, F=FOOTER                               
HDRDONE  DS    CL1    Y = HEADERS ALREADY DONE                                  
DPAGEIND DS    XL1    SET TO X'01 IF THE 2 BLANK LINE HAVE                      
*                     BEEN DONE TO START THE DOWNLOAD FILE                      
PUERROR  DS    XL1    IF NOT 0 - PRD USER ERROR WAS FOUND                       
LASTDSW  DS    CL1                                                              
MYSVMODE DS    XL1                                                              
CKESTSW  DS    CL1    SET TO X'01' IF CKEST READS A RECORD                      
CKESTREC DS    CL1                                                              
MYBKPRD  DS    CL3                                                              
PPGKEY   DS    CL32                                                             
PPGAREC  DS    A                                                                
WK       DS    XL24                                                             
WPRD     DS    CL1                                                              
SVNUPRD2 DS    CL1            SAVED NUPRD2                                      
SDUB     DS    D              USED FOR SHARES IN PROCNET                        
SNETDUB  DS    D              USED FOR SHARES IN PROCNET                        
SGRSDUB  DS    D              UESD FOR SHARES                                   
*                                                                               
NETDUB   DS    D                                                                
GRSDUB   DS    D                                                                
*                                                                               
NEACCNET DS    CL9         1 + 8 BYTE PACKED                                    
NEACCGRS DS    CL9         1 + 8 BYTE PACKED                                    
NEACCGOS DS    CL9         1 + 8 BYTE PACKED                                    
*                                                                               
MYFULL   DS    F                                                                
*                                                                               
MYSPACE  DS    CL20                                                             
MYP      DS    CL198                                                            
DINVFULL DS    CL11        FULL INVOICE NUMBER                                  
DINVNDH  DS    CL08        FULL INV - NO DASHES                                 
B1PROF   DS    CL16                                                             
B1XPROF  DS    CL16                                                             
*                                                                               
AAAPRD   DS    CL1                                                              
MYDUB    DS    D                                                                
SVLINE   DS    XL1                                                              
SVFORCEH DS    C                                                                
SAVPRDC  DS    CL3                                                              
SAVPRD   DS    X                                                                
X        DS    XL350       CURRENTLY ONLY 300 USED                              
*                                                                               
W        DS    CL132                                                            
SAVMODE  DS    X                                                                
MANRVNO  DS    H                                                                
MANMOS   DS    H                                                                
*                                                                               
PRDU1    DS    CL54       NAME AND DATA                                         
PRDU2    DS    CL38       NAME AND DATA                                         
ESTU1    DS    CL54       NAME AND DATA                                         
ESTU2    DS    CL38       NAME AND DATA                                         
*                                                                               
AAAFORMU DS    CL5                                                              
PRDFORMU DS    CL5                                                              
         DS    0F                                                               
         DS    0F                                                               
INVTOTS  DS    0XL8                                                             
INVTGRS  DS    F                                                                
INVTNET  DS    F                                                                
*                                                                               
INTC     DS    F                                                                
INTSTAT  DS    X                                                                
BUFFREC  DS    XL350      (CURRENTLY ONLY 300 USED)                             
OLDKEY   DS    XL60                                                             
KPRD     DS    X                                                                
KMKT     DS    XL2                                                              
KSTA     DS    XL3                                                              
HOLDPRD  DS    XL1                                                              
HOLDPRD2 DS    XL1                                                              
SAVR1    DS    F                                                                
FFS      DS    XL6'FF'                                                          
RECSW    DS    X                                                                
ERR      DS    X                                                                
TOTSTAR  DS    CL2                                                              
BILDAT   DS    H                                                                
INVNO    DS    H                                                                
PINVNO   DS    CL10                                                             
SINVNO   DS    H                                                                
SPINVNO  DS    CL10                                                             
MYSTART  DS    CL6               ORIGINAL START AND END                         
MYEND    DS    CL6                                                              
MYSTRTB  DS    CL3               ORIGINAL START AND END - BINARY                
MYENDB   DS    CL3                                                              
*                                                                               
PSTART   DS    CL8                                                              
PEND     DS    CL8                                                              
         DS    0D                                                               
SVHDR    DS    XL300                                                            
FIRST    DS    X                                                                
ERROR    DS    X                                                                
SVQOPT1  DS    C                                                                
*                                                                               
ITGRS    DS    F                                                                
ITNET    DS    F                                                                
ITCD     DS    F                                                                
*                                                                               
GTGRS    DS    PL8                                                              
GTNET    DS    PL8                                                              
GTCD     DS    PL8                                                              
GTDUE    DS    PL8                                                              
GTLINS   DS    PL8                                                              
GTINVS   DS    PL8                                                              
*                                                                               
QTGRS    DS    PL8                                                              
QTNET    DS    PL8                                                              
QTCD     DS    PL8                                                              
QTDUE    DS    PL8                                                              
*                                                                               
OUTREC   DS    XL100                                                            
*                                                                               
LINED    DSECT                                                                  
         DS    CL1                                                              
LINV     DS    CL10                                                             
         DS    CL1                                                              
LRDAT    DS    CL8                                                              
         DS    CL1                                                              
LIDAT    DS    CL8                                                              
         DS    CL1                                                              
LDDAT    DS    CL8                                                              
         DS    CL1                                                              
LINS     DS    CL7    WAS CL1 AND LPER (CL6)                                    
         DS    CL1                                                              
LSTA     DS    CL8                                                              
         DS    CL10                                                             
LGRS     DS    CL15                                                             
         DS    CL1                                                              
LNET     DS    CL15                                                             
         DS    CL1                                                              
LCD      DS    CL15                                                             
         DS    CL1                                                              
LDUE     DS    CL15                                                             
         DS    CL1                                                              
         DS    0C                                                               
         SPACE 3                                                                
*                                  DSECT FOR TABLE ENTRY                        
INVD     DSECT                                                                  
INVINV   DS    XL3                                                              
INVRDAT  DS    XL2                                                              
INVPRD   DS    CL3                                                              
INVEST   DS    XL1                                                              
INVMOS   DS    XL2               MONTH OF SERVICE (HEADER)                      
*                               INVPUB IS NULLS FOR INVOICES HEADERS            
INVSTA   DS    XL8                                                              
*                                                                               
INVINVD  DS    XL2                 HEADERS ONLY                                 
INVDUED  DS    XL2                      ''                                      
         ORG                                                                    
INVKL    EQU   *-INVD                                                           
*                                COMMENT DATA                                   
INVNSMED DS    CL1               SUB-MEDIA FOR NETWORK                          
*                                (MUST BILL BY SUBMEDIA)                        
INVEDESC DS    CL20              EST DESC                                       
*                                                                               
INVPU1N  DS    CL20              PRD USER FIELD 1 (NAME)                        
INVPU1D  DS    CL32              PRD USER FIELD 1 (DATA)                        
INVPU2N  DS    CL20              PRD USER FIELD 2 (NAME)                        
INVPU2D  DS    CL16              PRD USER FIELD 2 (DATA)                        
*                                                                               
INVEU1N  DS    CL20              EST USER FIELD 1 (NAME)                        
INVEU1D  DS    CL32              EST USER FIELD 1 (DATA)                        
INVEU2N  DS    CL20              EST USER FIELD 2 (NAME)                        
INVEU2D  DS    CL16              EST USER FIELD 2 (DATA)                        
*                                                                               
INVINS   DS    XL4               BILLED INSERTION COUNT                         
INVGRS   DS    XL4                                                              
INVNET   DS    XL4                                                              
INVCD    DS    XL4                                                              
INVDUE   DS    XL4                                                              
INVRL    EQU   *-INVD                                                           
         SPACE 2                                                                
         EJECT                                                                  
*                                  BUFFALO CSECT                                
         BUFF  LINES=3000,ROWS=1,COLUMNS=5,FLAVOR=BINARY,COMMENT=197,  X        
               KEYLIST=(23,A)                                                   
*                                                                               
VIRTLREC CSECT                                                                  
         DS    20000C                                                           
*                                                                               
NETBLK   CSECT                                                                  
         DS    1200C                                                            
*                                                                               
STABUCKC CSECT                                                                  
         DS    2000C                                                            
*                                                                               
NETBLKD  DSECT                                                                  
*                                                                               
       ++INCLUDE NENETRATED                                                     
         PRINT OFF                                                              
       ++INCLUDE NETBLOCKD                                                      
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDWIDED                                                        
       ++INCLUDE DDREMOTED                                                      
*                                                                               
         PRINT ON                                                               
       ++INCLUDE SPGENSTAB                                                      
       ++INCLUDE DDUCOMD                                                        
QOPT6    EQU   QGRP                                                             
QOPT7    EQU   QGRP+1                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025SPREPTD02 02/09/15'                                      
         END                                                                    
**PAN#1  CSECT                                                                  
         END                                                                    
