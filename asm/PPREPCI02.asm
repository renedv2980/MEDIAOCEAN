*          DATA SET PPREPCI02  AT LEVEL 031 AS OF 01/05/12                      
*PHASE PPCI02A                                                                  
*INCLUDE DLFLD                                                                  
*INCLUDE PPFMTINO                                                               
*INCLUDE GETUSER                                                                
         TITLE 'PPREPCI02 - CONTINENTAL INTERFACE'                              
*                                                                               
*  CHANGE LOG                                                                   
*                                                                               
*   BPLA  9/07     NEW MAIL CODE FOR CLIENT CNT                                 
*                                                                               
*   BPLA  8/07     ADD AN ESTIMATE NUMBER FIELD                                 
*                                                                               
*   BPLA  1/18/07  CALL LOG 0111190N                                            
*                  USE MAIL CODE HQSEM FOR MEDIA I                              
*                  CLEAR NAMES OF PRODUCT USER FIELDS FORM MEDIA I              
*                  IF NO DATA IS PRESENT OR BEGINS WITH '.'                     
*                                                                               
***********************************************************************         
*   QOPT1 -  D=DUMP OUTPUT RECORDS                                              
*                                                                               
*   QOPT7 - Y= TRACE BUFFALO PUTS,READS                                         
*                                                                               
***********************************************************************         
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
PPCI02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPCI02                                                         
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     R7,PPWORK2C                                                      
         USING PPWORK2D,R7                                                      
         LA    RC,SPACEND                                                       
         USING BILWRKD,RC                                                       
         L     R8,PPFILEC                                                       
         LA    R9,4095(R8)                                                      
         LA    R9,1(R9)                                                         
         USING PPFILED,R8,R9                                                    
         L     R3,AWIDEC                                                        
         USING WIDED,R3                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,PROCBUY                                                     
         BE    PROCBY                                                           
         CLI   MODE,PROCBIL                                                     
         BE    PROCBL                                                           
         CLI   MODE,FBUYPRO                                                     
         BE    FPRD                                                             
*                                                                               
         CLI   MODE,FBILPRO                                                     
         BE    FPRD                                                             
         CLI   MODE,FBILEST                                                     
         BE    FEST                                                             
*                                                                               
         CLI   MODE,FBUYCLI                                                     
         BE    CLTF                                                             
         CLI   MODE,LBUYCLI                                                     
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
         L     RF,=V(DLFLD)                                                     
         A     RF,RELO                                                          
         ST    RF,VDLFLD                                                        
         L     RF,=A(DOWNLD)                                                    
         A     RF,RELO                                                          
         ST    RF,VDOWNLD                                                       
         L     RF,=V(GETUSER)                                                   
         A     RF,RELO                                                          
         ST    RF,VGETUSER                                                      
         L     RF,=V(PPFMTINO)                                                  
         A     RF,RELO                                                          
         ST    RF,AFMTINO                                                       
*                                                                               
         MVI   FFS,X'FF'                                                        
         MVC   FFS+1(L'FFS-1),FFS                                               
         MVI   DOWNSW,0                                                         
         MVI   LASTDSW,0                                                        
*                                                                               
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         MVC   BOXWIDTH,=F'198'   NEED BIGGER LINE FOR DOWNLOAD                 
         L     R2,BOXAWIDE                                                      
         ST    R2,AWIDEC                                                        
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
         MVI   DOWNSW,0                                                         
*                                                                               
         MVC   SVQOPT1,QOPT1                                                    
         CLC   =C'ALL',QEST                                                     
         BNE   *+10                                                             
         MVC   QEST,SPACES                                                      
         MVI   FCRDACTV,C'N'                                                    
         CLI   QPRODUCT,C' '                                                    
         BE    INITA                                                            
         CLC   QPRODUCT,=C'ALL'                                                 
         BNE   *+8                                                              
*                                                                               
INITA    MVI   FCRDACTV,C'Y'                                                    
         OI    DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
*                                                                               
         MVC   MYSTART,QSTART                                                   
         MVC   MYEND,QEND                                                       
         GOTO1 DATCON,DMCB,QSTART,(3,MYSTRTB)                                   
         GOTO1 DATCON,DMCB,QEND,(3,MYENDB)                                      
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
PROCBY   GOTO1 APRBUY                                                           
         B     EXIT                                                             
         SPACE 3                                                                
PROCBL   GOTO1 APRBILL                                                          
         B     EXIT                                                             
*        CLTFRST                                                                
CLTF     DS    0H                                                               
*                                                                               
         XC    B1PROF,B1PROF        FIRST READ B1 AND B1X PROFILES              
         XC    B1XPROF,B1XPROF      B1X PROFILE                                 
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'POB1'                                                 
         MVC   WORK+4(2),QAGENCY                                                
         MVC   WORK+6(1),PCLTKMED                                               
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   FBC1                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
*                                                                               
FBC1     DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,B1PROF,DATAMGR                                 
         MVC   WORK(4),=C'PB1X'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR PAGE A               
         GOTO1 GETPROF,DMCB,WORK,B1XPROF,DATAMGR                                
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        PRDFRST                                                                
FPRD     DS    0H                                                               
         XC    PRDU1,PRDU1                                                      
         XC    PRDU2,PRDU2                                                      
         CLC   PPRDKPRD,=C'AAA'    NOT FOR PRD=AAA                              
         BE    FPRDSAVE                                                         
*                                                                               
         GOTO1 VGETUSER,DMCB,(C'P',PCLTREC),(C'P',PPRDREC),PRDU1,PRDU2          
         CLI   DMCB,X'FF'                                                       
         BE    FPRDERR                                                          
         CLI   PRDU1+21,C' '    MUST FIND DATA                                  
         BNH   FPRDERR                                                          
         B     FPRDSAVE                                                         
*                                                                               
FPRDERR  DS    0H                                                               
         B     FPRDSAVE       DON'T DIE IF MISSING                              
*                                                                               
         MVC   XP,XSPACES                                                       
         MVC   XP(36),=C'*** MISSING PRODUCT USER FIELD 1 ***'                  
         MVC   XP+40(3),PPRDKPRD                                                
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
         CLC   PPRDKPRD,=C'AAA'                                                 
         BNE   FPRDFORM                                                         
*                                                                               
         MVI   AAAPRD,C'Y'                                                      
         MVC   AAAFORMU(5),PPRDBILP         MOVE FORMULA FOR AAA                
         B     FPRDX                                                            
*                                                                               
FPRDFORM DS    0H                                                               
         MVC   PRDFORMU(5),PPRDBILP                                             
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
         CLC   PPRDKPRD,=C'AAA'    NOT FOR PRD=AAA                              
         BE    FESTSAVE                                                         
*                                                                               
         GOTO1 VGETUSER,DMCB,(C'P',PCLTREC),(C'E',PESTREC),ESTU1,ESTU2          
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
         MVC   XP+41(3),PESTKPRD                                                
         EDIT  (B2,PESTKEST),(3,XP+46),0                                        
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
         GOTO1 AREPRT                                                           
         B     EXIT                                                             
         SPACE 3                                                                
*        REQ LAST                                                               
REQL     DS    0H                                                               
         GOTO1 APRNT                                                            
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
         B     EXIT                                                             
         SPACE 3                                                                
*        RUN LAST                                                               
RUNL     DS    0H                                                               
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
         MVC   XP+2(18),=C'DETAIL LINE COUNT='                                  
         EDIT  (P8,GTLINS),(7,XP+21)                                            
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
         DC    A(PRBUY)                                                         
         DC    A(PRBILL)                                                        
         DC    A(REPRT)                                                         
         DC    A(BUFFALOC)                                                      
ACONSX   EQU   *                                                                
         SPACE 2                                                                
         EJECT                                                                  
PRBUY    CSECT                                                                  
         NMOD1 0,PRBUY                                                          
         LA    RC,SPACEND                                                       
*                                                                               
         CLI   PBDBFD,C'T'         SEE IF TEST BUY                              
         BE    RNBX                 IGNORE                                      
*                                                                               
         MVI   CKESTREC,C'B'       SET FROM BUY                                 
         GOTO1 =A(CKEST)           GO READ EST AND GET USERS                    
*                                                                               
RNB10    DS    0H                                                               
         MVI   ELCODE,X'26'                                                     
         LA    R2,PBDELEM                                                       
         USING PBILELEM,R2                                                      
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
         CLC   PBLDATE,MYSTRTB                                                  
         BL    RNB12                                                            
         CLC   PBLDATE,MYENDB                                                   
         BH    RNB12                                                            
*                                                                               
RNB13T   DS    0H                                                               
         XC    X(250),X                                                         
         XC    X+250(INVRL-250),X+250                                           
         LA    R4,X                                                             
         USING INVD,R4                                                          
*                                                                               
         MVC   INVINV(1),PBLDATE+1                                              
         MVC   INVINV+1(2),PBINVNO                                              
         GOTO1 DATCON,DMCB,(3,PBLDATE),(2,INVRDAT)                              
*                                                                               
RNB15    DS    0H                                                               
         MVC   INVEST,PBUYKEST                                                  
         MVC   INVPNAM,PUBNAME                                                  
         MVC   INVZNAM,PUBZNAME                                                 
         LA    RE,INVPNAM                                                       
         LA    RF,40             DOES INVPNAM AND INVZNAM                       
RNB16    CLI   0(RE),C','        CHANGE COMMAS TO SPACES                        
         BNE   RNB16C                                                           
         MVI   0(RE),C' '                                                       
         B     RNB17                                                            
*                                                                               
RNB16C   CLI   0(RE),C'&&'       CHANGE & TO /                                  
         BNE   RNB17                                                            
         MVI   0(RE),C'/'                                                       
         B     RNB17                                                            
*                                                                               
RNB17    LA    RE,1(RE)                                                         
         BCT   RF,RNB16                                                         
*                                                                               
         MVC   INVPRD,PBPRD               PRD FROM BILL ELEMEMT                 
         MVC   INVEU1D,ESTU1+21       ESTIMATE USER FIELD 1                     
*                                 - ONLY ONE NEEDED FOR LINEITEM RECS           
*                                                                               
         MVC   INVEDESC(20),PESTNAME                                            
         LA    RE,INVEDESC+19                                                   
RNB20    CLI   0(RE),C' '      SCAN BACKWARDS FOR NON-SPACE                     
         BH    RNB21                                                            
         BCT   RE,RNB20                                                         
*                                                                               
RNB21    MVC   2(L'PESTNAM2,RE),PESTNAM2                                        
*                                                                               
         OC    INVEDESC(L'INVEDESC),SPACES                                      
*                                                                               
         LA    RE,INVEDESC                                                      
         LA    RF,41             DOES BOTH NAMES                                
RNB22    CLI   0(RE),C','        CHANGE COMMAS TO SPACES                        
         BNE   RNB22C                                                           
         MVI   0(RE),C' '                                                       
         B     RNB23                                                            
*                                                                               
RNB22C   CLI   0(RE),C'&&'       CHANGE & TO /                                  
         BNE   RNB23                                                            
         MVI   0(RE),C'/'                                                       
         B     RNB23                                                            
*                                                                               
RNB23    LA    RE,1(RE)                                                         
         BCT   RF,RNB22                                                         
*                                                                               
         MVC   INVMOS(2),PBDBDATE   MOS                                         
         MVC   INVPUB,PBUYKPUB                                                  
         MVC   INVINS,=F'1'         INSERTION COUNTER                           
         MVC   INVGRS,PBGROSS                                                   
         ICM   R0,15,PBGROSS                                                    
         ICM   R1,15,PBAGYCOM                                                   
         SR    R0,R1                                                            
         ST    R0,INVNET                                                        
         MVC   INVCD,PBCSHDSC                                                   
*                                                                               
         MVC   INVDUE,INVNET       SET COST TO NET FOR NOW                      
*                                  USE BILL FORMULA IN FUTURE                   
*                                  USE GETCOST?                                 
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFFC,X                                    
*                                                                               
         CLI   QOPT7,C'Y'          SEE IF TRACING                               
         BNE   RNB12                                                            
         MVC   XP(14),=C'**BUFF PUT D**'                                        
         GOTO1 APRNT                                                            
         GOTO1 HEXOUT,DMCB,X,XP+5,60,=C'N'                                      
         GOTO1 HEXOUT,DMCB,X+60,XP2+5,60,=C'N'                                  
         GOTO1 APRNT                                                            
         GOTO1 HEXOUT,DMCB,X+120,XP+5,60,=C'N'                                  
         GOTO1 HEXOUT,DMCB,X+180,XP2+5,60,=C'N'                                 
         GOTO1 APRNT                                                            
         GOTO1 HEXOUT,DMCB,X+240,XP+5,60,=C'N'                                  
         GOTO1 APRNT                                                            
         B     RNB12                                                            
*                                                                               
RNB30    DS    0H                                                               
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
         SPACE 2                                                                
         EJECT                                                                  
         LTORG                                                                  
         TITLE 'PRBILL  - PROCESS  BILL RECORDS'                                
PRBILL   CSECT                                                                  
         NMOD1 0,PRBILL                                                         
         LA    RC,SPACEND                                                       
         TM    KEY+25,X'80'                                                     
         BNZ   ROBX                                                             
         CLC   RCSVPRD,=C'ZZZ'                                                  
         BE    ROBX                                                             
*                                                                               
         CLC   PBILKBMN,MYSTRTB                                                 
         BL    ROBX                                                             
         CLC   PBILKBMN,MYENDB                                                  
         BH    ROBX                                                             
*                                  PASS DATA TO SORT                            
ROB10    DS    0H                                                               
*                                                                               
         CLC   PBILLDAT,MYSTART                                                 
         BL    ROBX                                                             
         CLC   PBILLDAT,MYEND                                                   
         BH    ROBX                                                             
         TM    PBILCMSW,X'20'      SKIP AOR BILLS                               
         BNZ   ROBX                SKIP NET (USED FOR SOMETHING ELSE)           
*                                                                               
ROB10B   DS    0H                                                               
         XC    X(250),X                                                         
         XC    X+250(INVRL-250),X+250                                           
         LA    R4,X                                                             
         USING INVD,R4                                                          
*                                                                               
         MVC   INVINV(1),PBILKBMN+1                                             
         MVC   INVINV+1(2),PBILKBNO                                             
*                                                                               
         MVC   INVPRD,PBILKPRD                                                  
         MVC   INVEST,PBILKEST                                                  
         MVC   INVEDESC(20),PESTNAME                                            
         LA    RE,INVEDESC+19                                                   
ROB10E   CLI   0(RE),C' '      SCAN BACKWARDS FOR NON-SPACE                     
         BH    ROB10F                                                           
         BCT   RE,ROB10E                                                        
*                                                                               
ROB10F   MVC   2(L'PESTNAM2,RE),PESTNAM2                                        
         OC    INVEDESC(L'INVEDESC),SPACES                                      
*                                                                               
         LA    RE,INVEDESC                                                      
         LA    RF,41             DOES BOTH NAMES                                
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
****     BAS   RE,ROBSETU          PEUSER1 => INVEUSER                          
******                                                                          
**3/10/93ZAP   DUB,PBILLRCV            WAS PBILLRCV?                            
******   CVB   R0,DUB                                                           
******   ST    R0,INVGRS                                                        
*                                        (WILL CATCH BILL FORMULAS)             
         ZAP   DUB,PBILLNET      GROSS- AC - CD                                 
         CVB   R0,DUB                                                           
         ST    R0,INVNET                                                        
         ZAP   DUB,PBILLGRS                                                     
         SP    DUB,PBILLBIL      GROSS - CD                                     
         CVB   R0,DUB                                                           
         ST    R0,INVCD                                                         
         L     R1,INVNET         MUST ADD CD TO NET/NET TO GET NET              
         AR    R1,R0                                                            
         ST    R1,INVNET                                                        
******                                                                          
***3/10/93                                                                      
******   L     R1,INVGRS         MUST ADD CD TO G-CD TO GET GROSS               
******   AR    R1,R0                                                            
         ZAP   DUB,PBILLGRS        INVGRS SHOULD BE GROSS                       
         CVB   R1,DUB              ****** ADDED 3/10/92                         
         ST    R1,INVGRS                                                        
****                                                                            
         ZAP   DUB,PBILLRCV        AMOUNT DUE                                   
         CVB   R0,DUB                                                           
         ST    R0,INVDUE                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(0,PBILLDAT),(2,INVRDAT)                             
         GOTO1 (RF),(R1),(3,PBILINVD),(2,INVINVD)                               
         GOTO1 (RF),(R1),(3,PBILDUED),(2,INVDUED)                               
         MVC   INVMOS,PBILKMOS                                                  
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
         CLI   QMEDIA,C'I'           INTERACTIVE?                               
         BNE   ROB30                                                            
         CLC   INVPU1N(07),=C'ACCOUNT' ACCOUNT NUMBER?                          
         BNE   *+16                                                             
         MVC   INVPU1N,SPACES        IF SO CLEAR NAME                           
         MVC   INVPU1D,SPACES        AND DATA                                   
*                                                                               
         CLC   INVPU2N(07),=C'ACCOUNT' ACCOUNT NUMBER?                          
         BNE   *+16                                                             
         MVC   INVPU2N,SPACES        IF SO CLEAR NAME                           
         MVC   INVPU2D,SPACES        AND DATA                                   
*                                                                               
ROB30    DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFFC,X                                    
         CLI   QOPT7,C'Y'          SEE IF TRACING                               
         BNE   ROBX                                                             
         MVC   XP(14),=C'**BUFF PUT I**'                                        
         GOTO1 APRNT                                                            
         GOTO1 HEXOUT,DMCB,X,XP+5,60,=C'N'                                      
         GOTO1 HEXOUT,DMCB,X+60,XP2+5,60,=C'N'                                  
         GOTO1 APRNT                                                            
         GOTO1 HEXOUT,DMCB,X+120,XP+5,60,=C'N'                                  
         GOTO1 APRNT                                                            
         GOTO1 HEXOUT,DMCB,X+240,XP+5,60,=C'N'                                  
         GOTO1 APRNT                                                            
*                                                                               
         B     ROBX                                                             
*                                                                               
ROB40    DS    0H                                                               
ROBX     DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
CKEST    CSECT                                                                  
         NMOD1 0,CKEST                                                          
         LA    RC,SPACEND                                                       
*****                                                                           
*****    NOTE - DO NOT USE REGISTERS  R3, R8, R9, RA                            
*****           IN THIS CSECT                                                   
*****           THEY ARE USED FOR THE WHOLE PROGRAM                             
*****                                                                           
*                                                                               
         MVC   PPGKEY,KEY                                                       
         MVC   PPGAREC,AREC                                                     
         MVI   CKESTSW,0    WILL BE SET TO X'01' IF I READ SOMETHING            
         XC    KEY,KEY                                                          
         MVC   KEY(3),PBUYREC                                                   
         MVI   KEY+3,X'07'         MUST READ EST FOR START YEAR                 
         MVC   KEY+4(6),PBUYREC+4   CLT AND PRD                                 
         MVC   KEY+10(2),PBUYREC+19   EST                                       
         XC    KEY+12(19),KEY+12                                                
         CLI   CKESTREC,C'B'        FROM BUY                                    
         BE    CKEST3                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(3),PBILLREC                                                  
         MVI   KEY+3,X'07'         MUST READ EST FOR START YEAR                 
         MVC   KEY+4(6),PBILLREC+4  CLT AND PRD                                 
         MVC   KEY+10(2),PBILKEST     EST                                       
         XC    KEY+12(19),KEY+12                                                
         CLI   CKESTREC,C'L'        FROM BILL                                   
         BE    CKEST3                                                           
         DC    H'0'                 ERROR                                       
*                                                                               
CKEST3   CLC   PESTREC(12),KEY      SEE IF I ALREADY HAVE EST                   
         BE    CKEST5                                                           
         MVI   CKESTSW,1                                                        
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                EST MUST BE ON FILE                          
         LA    R0,PESTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
CKEST5   DS    0H                                                               
         XC    ESTU1,ESTU1                                                      
         XC    ESTU2,ESTU2                                                      
         CLC   PPRDKPRD,=C'AAA'    NOT FOR PRD=AAA                              
         BE    CKESTSAV                                                         
*                                                                               
         GOTO1 VGETUSER,DMCB,(C'P',PCLTREC),(C'E',PESTREC),ESTU1,ESTU2          
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
         MVC   XP+41(3),PESTKPRD                                                
         EDIT  (B2,PESTKEST),(3,XP+46),0                                        
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
         TITLE 'REPRT - CREATE TAPE RECS AND PRINT REPORT'                      
REPRT    CSECT                                                                  
         NMOD1 0,REPRT                                                          
         LA    RC,SPACEND                                                       
         SPACE 2                                                                
         XC    SVHDR(250),SVHDR                                                 
         XC    SVHDR+250(INVRL-250),SVHDR+250                                   
         MVI   FORCEHED,C'Y'                                                    
         LA    R2,XP                                                            
         USING LINED,R2                                                         
         XC    BUFFREC(250),BUFFREC                                             
         XC    BUFFREC+250(INVRL-250),BUFFREC+250                               
         LA    R4,BUFFREC                                                       
         USING INVD,R4                                                          
REP2     DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFFC,BUFFREC,0                           
         B     REP4B                                                            
REP4     DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFFC,BUFFREC,0                            
REP4B    DS    0H                                                               
         TM    DMCB+8,X'80'                                                     
         BNZ   REP50                                                            
*                                                                               
         CLI   QOPT7,C'Y'          SEE IF TRACING                               
         BNE   REP6                                                             
         MVC   XP(12),=C'**BUFF OUT**'                                          
         GOTO1 APRNT                                                            
         GOTO1 HEXOUT,DMCB,BUFFREC,XP+5,60,=C'N'                                
         GOTO1 HEXOUT,DMCB,BUFFREC+60,XP2+5,60,=C'N'                            
         GOTO1 APRNT                                                            
         GOTO1 HEXOUT,DMCB,BUFFREC+120,XP+5,60,=C'N'                            
         GOTO1 HEXOUT,DMCB,BUFFREC+180,XP2+5,60,=C'N'                           
         GOTO1 APRNT                                                            
         GOTO1 HEXOUT,DMCB,BUFFREC+240,XP+5,60,=C'N'                            
         GOTO1 APRNT                                                            
*                                                                               
REP6     DS    0H                                                               
         OC    INVPUB,INVPUB       TEST FOR HEADER                              
         BNZ   REP10                                                            
*                                  INVOICE HEADER                               
         OC    SVHDR(250),SVHDR         ANY OLD HDR TO FINISH?                  
         BZ    *+8                                                              
         BAS   RE,ENDINV                                                        
*                                                                               
         GOTO1 APRNT                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(2,INVRDAT),(0,WORK)                                 
         GOTO1 AFMTINO,DMCB,WORK,(2,INVINV+1),(PCLTKMED,B1PROF),B1XPROF         
*                                                                               
         L     RF,DMCB                                                          
         MVC   DINVFULL,0(RF)    FULL INVOICE NUMBER                            
*                                                                               
         BAS   RE,HDROUT           DO HEADER OUTPUT                             
         XC    ITGRS(12),ITGRS     CLEAR INV CHECKING TOTALS                    
         MVC   LINV,DINVFULL                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(2,INVINVD),(5,LIDAT)                                
         GOTO1 (RF),(R1),(2,INVDUED),(5,LDDAT)                                  
         GOTO1 (RF),(R1),(2,INVRDAT),(5,LRDAT)                                  
*                                                                               
         MVC   SVHDR(250),INVD     SAVE HEADER                                  
         MVC   SVHDR+250(INVRL-250),INVD+250                                    
         AP    GTINVS,=P'1'        BUMP INVOICE COUNT                           
         B     REP30                                                            
*                                                                               
REP10    DS    0H                  DETAIL LINE                                  
         BAS   RE,DETOUT           DO DETAIL OUTPUT                             
*NOP*    GOTO1 DATCON,DMCB,(3,INVPER),(0,WORK)                                  
*NOP*    GOTO1 DATCON,DMCB,(3,INVPER),(X'20',WORK)                              
*NOP*    MVC   LPER,WORK                                                        
*                                                                               
         EDIT  (B4,INVINS),(7,LINS),COMMAS=YES                                  
*                                                                               
         GOTO1 PUBEDIT,DMCB,INVPUB,LPUB                                         
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
         BZ    *+8                                                              
         BAS   RE,ENDINV                                                        
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'RESET',ABUFFC                                    
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
         CLC   ITGRS,SVHDR+INVGRS-INVD                                          
         BNE   EINV4                                                            
         CLC   ITNET,SVHDR+INVNET-INVD                                          
         BNE   EINV4                                                            
         CLC   ITCD,SVHDR+INVCD-INVD                                            
         BNE   EINV4                                                            
*                                                                               
         L     R0,SVHDR+INVDUE-INVD  ADD TO DUE TOTALS                          
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
         TITLE 'PRNT - PRINT CONTROL MODULE'                                    
PRNT     CSECT                                                                  
         NMOD1 0,PRNT                                                           
         LA    RC,SPACEND                                                       
         MVC   XHEAD3+50(4),=C'FROM'                                            
         GOTO1 DATCON,DMCB,(0,MYSTART),(5,XHEAD3+55)                            
         MVC   XHEAD3+64(2),=C'TO'                                              
         GOTO1 (RF),(R1),(0,MYEND),(5,XHEAD3+67)                                
         GOTO1 REPORT                                                           
*                                                                               
PRNTX    DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*          DATA SET PPREPGT02  AT LEVEL 026 AS OF 02/24/04                      
DOWNLD   CSECT                                                                  
         NMOD1 0,DOWNLD                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
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
         BE    DNP70                                                            
*                                                                               
         CLI   MODE,REQFRST       SEE IF I NEED TO INTIALIZE                    
         BE    DNP80                                                            
*                                                                               
         CLI   DOWNSW,C'H'          SEE IF INVOIVE HEADER                       
         BNE   DNP50                                                            
*****                                                                           
*****    DOWNLOAD BILLING INFO HERE                                             
*****                                                                           
*                                                                               
         CLI   LASTDSW,C'D'       WAS LAST CALL A DETAIL ITEM?                  
         BNE   DNP10                                                            
*                                 MUST SEND FOR INVOICE JUST DONE               
         MVC   DLCBFLD(7),=C'end,,,,'    end + 5 commas                         
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVI   DLCBACT,DLCBEOL    SEND END OF LINE                              
         GOTO1 VDLFLD                                                           
*                                                                               
*                                                                               
DNP10    MVC   DLCBFLD(7),=C'invoice'                                           
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(7),=C'invoice'                                           
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(4),=C'TRUE'                                              
         TM    INVDUE,X'80'       SEE IF NEGATIVE                               
         BO    *+10                                                             
         MVC   DLCBFLD(5),=C'FALSE'                                             
*                                                                               
*        FOR LAST FIELD ON THE INVOICE line - NO COMMA                          
*                                                                               
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(3),=C',,,' TO END WITH 5 COMMAS                          
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVI   DLCBACT,DLCBEOL    SEND END OF LINE                              
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(6),=C'header'                                            
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(10),DINVFULL                                             
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         ST    R1,SAVER1      SAVE R1                                           
         GOTO1 DATCON,DMCB,(2,INVINVD),(X'20',WORK)                             
         L     R1,SAVER1      RESTORE R1                                        
*                                                                               
*        INVOICE DATE FORMAT WAS YYYY-MM-DD                                     
*        INVOICE DATE FORMAT NOW MM/DD/YYYY                                     
*                                                                               
******                         ASSUME LAST CENTURY                              
******  Y3K NOTE: CODE WILL WORK UNTIL 2090- I'LL BE DEAD                       
******                                                                          
         MVC   DLCBFLD(2),WORK+2       MONTH                                    
         MVI   DLCBFLD+2,C'/'                                                   
         MVC   DLCBFLD+3(2),WORK+4     DAY                                      
         MVI   DLCBFLD+5,C'/'                                                   
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
         MVC   DLCBFLD(5),=C'HQSB1'        CODE FOR CLIENT CNT                  
         CLC   QCLIENT,=C'CNT'                                                  
         BE    DNP15                                                            
         MVC   DLCBFLD(5),=C'HQSEM'        MAIL CODE for media i                
         CLI   QMEDIA,C'I'                                                      
         BE    *+10                                                             
         MVC   DLCBFLD(5),=C'HQSA1'        MAIL CODE                            
DNP15    MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(4),=C'0.00'    DON'T KNOW WAHT THIS IS FOR               
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(7),=C'invoice'                                           
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
*                                  INVOICE AMOUNT                               
*******  ST    R1,SAVER1                                                        
*******  MVC   WORK(20),SPACES                                                  
*******  EDIT  (B4,INVDUE),(11,WORK),2,ALIGN=LEFT,FLOAT=-                       
*******  L     R1,SAVER1                                                        
         MVC   DLCBFLD(4),=C'0.00'    NO $ HERE NOW                             
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
*                                                                               
*        MONTH OF SERVICE                                                       
*                                                                               
         ST    R1,SAVER1                                                        
         MVC   WORK(2),INVMOS                                                   
         MVI   WORK+2,X'01'      DAY TO 01                                      
         GOTO1 DATCON,DMCB,(3,WORK),(6,WORK+6)                                  
         L     R1,SAVER1          RESTORE                                       
*                                                                               
         MVI   DLCXDELC,C' '              TO SUPPRESS COMMA                     
         MVC   DLCBFLD(6),WORK+6          MMM/YY                                
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
*        FOR LAST FIELDS ON HEADER LINE - NO DELIMITERS                         
*                                                                               
         MVC   HALF,INVEST                                                      
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DLCBFLD(3),DUB                                                   
*                                                                               
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(40),INVEDESC       ESTIMATE NAMES                        
*                                         CAN ONLY DO A MAX OF 40               
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  PRD USER 1 NAME                              
         MVC   DLCBFLD(20),INVPU1N                                              
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  PRD USER 1 DATA                              
         MVC   DLCBFLD(32),INVPU1D                                              
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  PRD USER 2 NAME                              
         MVC   DLCBFLD(20),INVPU2N                                              
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  PRD USER 1 DATA                              
         MVC   DLCBFLD(16),INVPU2D                                              
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
*  NOTE - ESTIMATE USER FIELD 1 NOW ON LINEITEM RECORD                          
*         (INSTEAD OF ESTIMATE NAME)                                            
*                                  EST USER 1 NAME                              
*****    MVC   DLCBFLD(20),INVEU1N                                              
*****    MVI   DLCBTYP,C'T'       TEXT FIELD                                    
*****    MVI   DLCBACT,DLCBPUT                                                  
*****    GOTO1 VDLFLD                                                           
*****                              EST USER 1 DATA                              
*****    MVC   DLCBFLD(32),INVEU1D                                              
*****    MVI   DLCBTYP,C'T'       TEXT FIELD                                    
*****    MVI   DLCBACT,DLCBPUT                                                  
*****    GOTO1 VDLFLD                                                           
*****                              EST USER 2 NAME                              
*****    MVC   DLCBFLD(20),INVEU2N                                              
*****    MVI   DLCBTYP,C'T'       TEXT FIELD                                    
*****    MVI   DLCBACT,DLCBPUT                                                  
*****    GOTO1 VDLFLD                                                           
*****                              EST USER 1 DATA                              
*****    MVC   DLCBFLD(16),INVEU2D                                              
*****    MVI   DLCBTYP,C'T'       TEXT FIELD                                    
*****    MVI   DLCBACT,DLCBPUT                                                  
*****                                                                           
*****    MVI   DLCBTYP,C'T'       TEXT FIELD                                    
*****    MVI   DLCBACT,DLCBPUT                                                  
*****    GOTO1 VDLFLD                                                           
**                                                                              
         MVI   DLCXDELC,C','     RESTORE TO COMMA                               
*                                                                               
         MVI   DLCBACT,DLCBEOL    SEND END OF LINE                              
         GOTO1 VDLFLD                                                           
         MVC   LASTDSW,DOWNSW                                                   
         B     DNPX                                                             
*                                                                               
         EJECT                                                                  
*****                                                                           
*****    DOWNLOAD DETAIL INFO HERE                                              
*****                                                                           
*                                                                               
DNP50    CLI   DOWNSW,C'D'   DETAIL ITEM?                                       
         BE    *+6                                                              
         DC    H'0'          BAD CALL                                           
         MVC   DLCBFLD(8),=C'lineitem'                                          
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         ST    R1,SAVER1      SAVE R1                                           
         MVI   DLCBFLD,C'1'  ALWAYS SET TO 1 (AS OF 3/05)                       
*                                                                               
*        I'VE STRONGLY SUGGESTTED THAT IT BE SET TO -1                          
*        IF THE LINEITEM AMOUNT IS NEGATIVE                                     
*        OTHERWISE POSTIVE ITEMS AND NEGATIVE ITEMS                             
*        WOULD APPEAR THE SAME AND THE INVOICE TOTAL                            
*        WOULD NOT BE ABLE TO BE ASCERTAINED WERE                               
*        A MIXTURE OF POSITIVE ANDD NEGATIVE LINEITEMS TO EXIST                 
*        FOR THE INVOICE - AS THE SIGN IS ONLY CARRIED IN THE HEADER            
*                                                                               
****     EDIT  (B4,INVINS),(5,DLCBFLD),ALIGN=LEFT                               
         L     R1,SAVER1      RESTORE R1                                        
*                                                                               
         MVI   DLCXDELC,0     NO DELIMITER                                      
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         MVI   DLCXDELC,C','  RESTORE TO COMMA                                  
*                                                                               
*        EMPTY UNIT OF MEASURE FIELD                                            
*                                                                               
         MVI   DLCBFLD,C','       TO GET 2 COMMAS                               
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
******   MVC   DLCBFLD(25),INVEDESC       ESTIMATE NAMES                        
         MVC   DLCBFLD(L'INVEU1D),INVEU1D  ESTIMATE USER 1 DATA                 
*                                      AUTHORIZATION #                          
*                                                                               
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   WORK(40),SPACES     BE SURE IT CLEARED                           
         MVC   WORK(20),INVPNAM    PUB NAME                                     
         LA    R2,WORK+19                                                       
DNP53    CLI   0(R2),C' '       SCAN BACKWARDS FOR NON-SPACE                    
         BNE   DNP54                                                            
         BCT   R2,DNP53                                                         
DNP54    MVC   2(20,R2),INVZNAM     ZONE NAME                                   
         OC    WORK(40),SPACES                                                  
         MVC   DLCBFLD(40),WORK    CAN ONLY DO A MAX OF 40                      
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         ST    R1,SAVER1                                                        
         MVC   WORK(20),SPACES                                                  
*******  EDIT  (B4,INVDUE),(11,WORK),2,ALIGN=LEFT,FLOAT=-                       
         MVC   MYFULL,INVDUE                                                    
         L     R2,MYFULL                                                        
         L     R0,MYFULL                                                        
         C     R0,=F'0'                                                         
         BNL   *+6                                                              
         LCR   R2,R0                                                            
         ST    R2,MYFULL                                                        
*                                                                               
         EDIT  (B4,MYFULL),(11,WORK),2,ALIGN=LEFT                               
         L     R1,SAVER1                                                        
         CLC   WORK(3),=C'.00'    ZERO $                                        
         BNE   *+10                                                             
         MVC   WORK(4),=C'0.00'                                                 
*                                                                               
         MVC   DLCBFLD(11),WORK                                                 
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
*        EMPTY TAX FIELD                                                        
*                                                                               
*        FOR LAST FIELD ON THE LINE - NO COMMA                                  
*                                                                               
         MVC   DLCBFLD(4),=C'0.00'     ZERO TAX                                 
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVI   DLCBACT,DLCBEOL    SEND END OF LINE                              
         GOTO1 VDLFLD                                                           
         MVC   LASTDSW,DOWNSW                                                   
         B     DNPX                                                             
*                                                                               
DNP70    DS    0H             END OF FILE?                                      
         CLI   LASTDSW,C'D'       WAS LAST DOWNSW A DETAIL ITEM?                
         BNE   DNPX              DON'T NEED TO SEND                             
*                                                                               
         MVC   DLCBFLD(7),=C'end,,,,'   END + 5 COMMAS                          
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVI   DLCBACT,DLCBEOL    SEND END OF LINE                              
         GOTO1 VDLFLD                                                           
         B     DNPX                                                             
*                                                                               
*                                                                               
DNP80    DS    0H                                                               
*                                 AND RETURN IN SAVVEND                         
         MVC   XP,XSPACES           JUST IN CASE                                
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
         MVI   FORCEHED,C'N'                                                    
         MVI   RCWHATPR,2     SET TO SECOND SYSPRINT                            
         GOTO1 REPORT                                                           
         MVC   LINE,SVLINE             RESTORE LINE                             
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
DELIM    DC    C','        FIELD DELIMITER - COMMA                              
EOTCHR   DC    X'00'       END OF TEXT FIELD DELIMITER                          
EOTALT   DC    X'00'       END OF TEXT CHR ALTERNATE                            
EOLCHR   DC    X'00'       END OF LINE CHAR - SEMI-COLON                        
EORCHR   DC    X'00'       END OF REPORT CHR                                    
*                                                                               
*        NORMAL DOWNLOAD FIELDS ARE:                                            
*        C' ',C'"',C'''',X'5E',C':'     5E IS SEMI-COLON                        
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDLCB                                                         
PPCI02   CSECT                                                                  
*       BILLING WORK AREA DSECT                                                 
BILWRKD  DSECT                                                                  
BILWRK   DS    0C                                                               
RCONS    DS    0F                                                               
APRNT    DS    A                                                                
APRBUY   DS    A                                                                
APRBILL  DS    A                                                                
AREPRT   DS    A                                                                
ABUFFC   DS    A                                                                
AWIDEC   DS    A                                                                
*                                                                               
RELO     DS    A                                                                
DOWNSW   DS    CL1    H- HEADER, D - DETAIL                                     
LASTDSW  DS    CL1                                                              
CKESTSW  DS    CL1    SET TO X'01' IF CKEST READS A RECORD                      
CKESTREC DS    CL1                                                              
PPGKEY   DS    CL32                                                             
PPGAREC  DS    A                                                                
*                                                                               
VDLFLD   DS    A                                                                
VDOWNLD  DS    A                                                                
VGETUSER DS    A                                                                
AFMTINO  DS    A                                                                
*                                                                               
MYFULL   DS    F                                                                
*                                                                               
MYSPACE  DS    CL20                                                             
MYP      DS    CL198                                                            
DINVFULL DS    CL11                                                             
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
LPUB     DS    CL17                                                             
         DS    CL1                                                              
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
INVEST   DS    XL2                                                              
INVMOS   DS    XL2               MONTH OF SERVICE (HEADER)                      
*                               INVPUB IS NULLS FOR INVOICES HEADERS            
INVPUB   DS    XL6                                                              
*                                                                               
INVINVD  DS    XL2                 HEADERS ONLY                                 
INVDUED  DS    XL2                      ''                                      
         ORG                                                                    
INVKL    EQU   *-INVD                                                           
*                                COMMENT DATA                                   
INVPNAM  DS    CL20              PUBNAME                                        
INVZNAM  DS    CL20              ZONE NAME                                      
INVEDESC DS    CL41              EST DESC (BOTH NAME LINES)                     
         DS    CL1               ALIGNMENT                                      
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
         BUFF  LINES=3000,ROWS=1,COLUMNS=5,FLAVOR=BINARY,COMMENT=258,  X        
               KEYLIST=(22,A)                                                   
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDWIDED                                                        
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031PPREPCI02 01/05/12'                                      
         END                                                                    
