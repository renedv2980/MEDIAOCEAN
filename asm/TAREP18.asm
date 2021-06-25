*          DATA SET TAREP18    AT LEVEL 051 AS OF 12/13/12                      
*PHASE T70318A,*                                                                
         TITLE 'T70318 - INVOICE REGISTER'                                      
T70318   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70318,R7                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         LA    RA,BUFF                                                          
         LA    RA,8(RA)                                                         
         USING TREGD,RA            INVOICE REGISTER DSECT                       
         EJECT                                                                  
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   MX                                                               
         BAS   RE,SETSYS           SET UP SYSIO BLOCK                           
         BAS   RE,INIT             INITIALIZE                                   
         BAS   RE,PREP             GET ALL INVOICE RECORDS                      
*                                                                               
         GOTO1 SORTER,DMCB,=C'GET' GET 1ST RECORD BEFORE LOOP                   
         L     R3,4(R1)            ADDRESS OF RECORD GOTTEN BY SORTER           
         LTR   R3,R3               EOF                                          
         BZ    M10                                                              
         BAS   RE,PRDET            AND PRINT THE REPORT                         
*                                                                               
M10      GOTO1 SORTER,DMCB,=C'END'                                              
*                                                                               
MX       B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SET UP BASIC SYSIO BLOCK                                               
*                                                                               
         SPACE 1                                                                
         USING T703FFD,R2                                                       
SETSYS   NTR1                                                                   
         L     R2,ATWA                                                          
         MVC   TIACOMFC,ACOMFACS                                                
         LA    R1,IOHOOK           ADDRESS OF HOOK IN PROGRAM                   
         ST    R1,TIHOOK                                                        
         MVC   TIACCS,TWAACCS      PASS THROUGH LIMIT ACCESS                    
         MVC   TIAUTH,TWAAUTH           AND AUTHORIZATION                       
         MVC   TIUSERID,TWAORIG         AND REQUESTING ID                       
         MVC   TRTWAAGY+8(2),TWAORIG                                            
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        INITIALIZATION                                                         
*                                                                               
         SPACE 1                                                                
INIT     NTR1                                                                   
*                                                                               
         L     R3,ATWA             SAVE ADDRS OF TWA ROUTINES                   
         USING T703FFD,R3                                                       
         L     R2,TWADCONS         SAVE ADDRS OF TWADCON ROUTINES               
         USING TWADCOND,R2                                                      
         MVC   BINSRCH,TBINSRCH                                                 
         MVC   PRNTBL,TPRNTBL                                                   
         DROP  R2,R3                                                            
*                                                                               
         LA    R1,MYSPECS          SET UP SPECS FOR REPORT                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK             SET HEADHOOK                                 
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R1,SRTAREA          INVOICE SORT AREA                            
         USING SRTINVD,R1                                                       
         XC    SRTAREA,SRTAREA                                                  
         XC    TNAME2H(L'TNAME2H+L'TNAME2),TNAME2H                              
         MVI   TNAME2H,L'TNAME2H+L'TNAME2                                       
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
*                                                                               
INITX    B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        USE SYSYIO TO READ ALL INVOICES WITHIN REQUESTED DATES                 
*                                                                               
         SPACE 1                                                                
PREP     NTR1                                                                   
         CLC   DUEPDTE,=X'000000'  REQUESTED FOR A SPECIFIC DUE DATE            
         BE    PREP10                                                           
         MVI   TIREAD,TLINCCDQ     USE DUE DATE PASSIVE PTR                     
         B     PREP20                                                           
*                                                                               
PREP10   MVI   TIREAD,TLINDCDQ     INVOICE BILL DATE RECS                       
*                                                                               
PREP20   XC    TIQSTART,TIQSTART                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
*                                                                               
PREPX    B     XIT                                                              
         SPACE 3                                                                
*                                                                               
*        HOOK TO SYSIO                                                          
*                                                                               
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      RECORD GOTTEN FROM SYSIO                     
         BE    PRINV                                                            
         B     XIT                                                              
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        PROCESS THE INVOICE RECORD PASSED BY SYSIO                             
*                                                                               
         SPACE 1                                                                
         USING SRTINVD,R3                                                       
PRINV    DS    0H                                                               
         LA    R3,SRTAREA                                                       
PRINV00  XC    SRTAREA,SRTAREA     CLEAR PREVIOUS RECORD                        
         L     R4,TIAREC           ADDRESS OF RECORD GOTTEN                     
         USING TLIND,R4                                                         
         MVC   SRTINV,TLININV      GET INVOICE NUMBER                           
         XC    SRTINV,COMPLM       UN-COMPLEMENT INVOICE NUMEBR                 
         MVC   SRTAGY,TLINAGY          & AGENCY                                 
         MVC   SVKEY,TIKEY         SAVE KEY TO RESTORE READ SEQ                 
         DROP  R4                                                               
*                                                                               
         L     R1,ATWA                                                          
         USING T703FFD,R1                                                       
         MVC   TRTPO,TIOFF         SET TP OFFICE FROM SYSIO                     
         CLI   SINOFFH+5,0         FILTER BY OFFICE                             
         BE    PRINV10                                                          
         CLC   TRTPO,SINOFF        SAME OFFICE AS REQUESTED ONE                 
         BNE   PRINVX                                                           
*                                                                               
PRINV10  CLI   SINDUEH+5,0         FILTER BY DUEDATE                            
         BE    PRINV12                                                          
         L     R4,TIAREC           ADDRESS OF RECORD GOTTEN                     
         USING TADDD,R4                                                         
         MVI   ELCODE,TADDELQ                                                   
         BAS   RE,GETEL            DUE DATE ELEMENT                             
         BNE   PRINVX                                                           
         CLC   TADDDATE,DUEPDTE                                                 
         BNE   PRINVX              DUE DATE REQUESTED                           
         DROP  R1                                                               
*                                                                               
PRINV12  MVC   SRTTPO,TRTPO                                                     
         L     R4,TIAREC           ADDRESS OF RECORD GOTTEN                     
         USING TAIND,R4                                                         
         MVI   ELCODE,TAINELQ                                                   
         BAS   RE,GETEL            STATUS ELEMENT MUST EXIST                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    TROPTS,TRNCW        CHECKS NOT YET WRITTEN                       
         BNO   PRINV14                                                          
         TM    TAINSTAT,TAINSCHK                                                
         BO    PRINVX              CHECK WRITTEN - SKIP                         
         B     PRINV17                                                          
*                                                                               
PRINV14  TM    TROPTS,TRURG        FILTERING ON URG                             
         BNO   PRINV15                                                          
         TM    TAINSTA2,TAINSURG                                                
         BNO   PRINVX                                                           
         B     PRINV17                                                          
*                                                                               
PRINV15  TM    TROPTS,TRCOD        IF FILTERING BY COD AND                      
         BNO   PRINV16                                                          
         TM    TAINSTAT,TAINSHLD      THIS IS NOT AN COD - SKIP                 
         BNO   PRINVX                                                           
         B     PRINV17                                                          
*                                                                               
PRINV16  TM    TAINSTAT,TAINSHLD   IF NOT FILTERING BY COD &                    
         BO    PRINVX                 THIS IS COD - SKIP                        
*                                                                               
PRINV17  MVC   SRTCID,TIISCI       COMM'L ID                                    
         MVC   SRTTYPE,SPACES      CLEAR TYPE FIELD                             
         MVI   LEGTYPE,0           SET DEFAULT LEGEND TYPE                      
         TM    TAINSTAT,TAINSHLD   IF THIS IS A COD INVOICE                     
         BNO   PRINV18                                                          
         OI    LEGTYPE,COD         ELSE IGNORE IT                               
         MVC   SRTTYPE,=C'COD '                                                 
*                                                                               
PRINV18  TM    TAINSTA2,TAINSURG   URGENT INVOICE                               
         BNO   PRINV20                                                          
         OI    LEGTYPE,URG                                                      
         MVC   SRTTYPE,=C'URG '                                                 
         DROP  R4                                                               
*                                                                               
PRINV20  L     R4,TIAREC                                                        
         USING TAPDD,R4                                                         
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         TM    TROPTS,TRBNP        IF FILTERING ON BNP                          
         BNO   PRINV22                                                          
         BAS   RE,GETEL            MUST HAVE PAYMENT DETAILS ELEMENT            
         BNE   PRINVX              ELSE SKIP RECORD                             
         TM    TAPDPST1,TAPDPBNP                                                
         BNO   PRINVX                                                           
         B     PRINV25                                                          
*                                                                               
PRINV22  BAS   RE,GETEL            IF NOT FILTERING - ELEMENT NOT               
         BNE   PRINV30             NECESSARILY THERE                            
*                                                                               
PRINV25  MVC   SRTEMP,TAPDEOR                                                   
         MVI   SRTCURR,C'U'                                                     
         TM    TAPDSTAT,TAPDSCAN   IF CANADIAN                                  
         BZ    PRINV26                                                          
         MVI   SRTCURR,C'C'        SET CURRENCY TO BE CANADIAN                  
         B     PRINV27                                                          
*                                                                               
PRINV26  TM    TAPDPST2,TAPDPEUR   EURO?                                        
         BZ    PRINV27                                                          
         MVI   SRTCURR,C'E'        SET CURRENCY TO BE EUROS                     
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAEUELQ      AND GET PAYMENT DETAILS IN EUROS             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
PRINV27  MVC   SRTCLT,TAPDCLI                                                   
                                                                                
         L     R2,TAPDPAYI                                                      
         CLI   TAPDLEN,TAPDLNQ                                                  
         BL    *+8                                                              
         A     R2,TAPDTXNW                                                      
         ST    R2,SRTPAYI                                                       
                                                                                
         L     R2,TAPDPAYC         CORP PAYMENT +                               
         A     R2,TAPDREXP         REIMBURSED EXPENSES                          
         CLI   TAPDLEN,TAPDLNQ                                                  
         BL    *+8                                                              
         S     R2,TAPDTXNW                                                      
         ST    R2,SRTPAYC                                                       
                                                                                
*        L     R2,TAPDMDED         MISC DEDUCTION +                             
*        A     R2,TAPDDUES         UNION DUES                                   
*        ST    R2,SRTMDED                                                       
         MVC   SRTPNH,TAPDPNH                                                   
         TM    TAPDPST1,TAPDPBNP   IF BILL NO PAYMENT SET FLAG                  
         BNO   PRINV30                                                          
         OI    LEGTYPE,BNP                                                      
         MVC   SRTTYPE,=C'BNP '                                                 
         DROP  R4                                                               
*                                                                               
PRINV30  L     R4,TIAREC                                                        
         USING TABDD,R4                                                         
         MVI   ELCODE,TABDELQ      GET BILLING DETAILS ELEMENT                  
         CLI   SRTCURR,C'E'                                                     
         BNE   *+8                                                              
         MVI   ELCODE,TABDELQ3     OR EURO-BASED DETAILS ELEMENT                
         BAS   RE,GETEL                                                         
         BNE   PRINV40                                                          
*                                                                               
         L     R1,TABDHND          INDIVIDUAL HANDLING                          
         A     R1,TABDTAX               + PAYROLL TAXES                         
         A     R1,TABDFICR              + FICA CREDITS                          
         ST    R1,SRTEORF               = EOR GROSS FEES                        
*                                                                               
         MVC   SRTNOEOR,TABDHNDC   NON EOR GROSS FEES                           
         L     R1,SRTNOEOR                                                      
*                                                                               
         CLI   TABDLEN,TABDLN2Q                                                 
         BL    *+16                                                             
         L     RF,TABDACOM         BROADCAST BUSINESS FEE                       
         A     RF,TABDSIGN         + SIGNATORY FEE                              
         ST    RF,SRTBBF                                                        
*                                                                               
         L     RF,TABDGST          COMBINE GST, PST AND CSF                     
         A     RF,TABDPST                                                       
         A     RF,TABDCSF                                                       
         ST    RF,SRTGST                                                        
*                                                                               
         MVC   SRTTOT,TABDTOT      INVOICE TOTAL                                
         DROP  R4                                                               
*                                                                               
PRINV40  DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'PUT',(R3)                                         
*&&DO                                                                           
         CLI   TGCUR,C'E'                                                       
         BE    PRINVX                                                           
         L     R4,TIAREC                                                        
         MVI   ELCODE,TABDELQ3     IF INVOICE HAS EURO BILL DETAILS             
         BAS   RE,GETEL            ELEMENT                                      
         BNE   PRINVX                                                           
         MVI   TGCUR,C'E'          ADD EURO-DETAILS TO SORTER AS WELL           
         B     PRINV00                                                          
*&&                                                                             
PRINVX   MVI   TGCUR,0                                                          
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*        PRINT INVOICE DETAILS                                                  
*        R3 - A(1ST SORTED RECORD)                                              
*                                                                               
         SPACE 1                                                                
         USING SRTINVD,R3                                                       
         USING P1LINED,R2                                                       
PRDET    NTR1                                                                   
         LA    R2,P                POINT TO PRINT LINE                          
*                                                                               
PRD10    DS    0H                                                               
         MVC   TYPE,SRTTYPE        TYPE OF INVOICE                              
         CLC   TYPE,=C'COD '                                                    
         BNE   *+10                                                             
         MVC   TYPE,=C'PUR '       CHANGE COD TO PUR, KEEP SORT ORDER           
         MVC   CLIENT,SRTCLT       CLIENT CODE                                  
         MVC   CID,SRTCID          COMMERCIAL ID                                
         GOTO1 TINVCON,DMCB,SRTINV,INVNUM,DATCON                                
         L     R1,SRTPAYI          INDIVIDUAL PAYMENT                           
         LA    RF,GROSSI                                                        
         BAS   RE,EDIT12                                                        
*                                                                               
         L     R1,SRTPAYC          CORP PAYMENT                                 
         LA    RF,GROSSC                                                        
         BAS   RE,EDIT12                                                        
*&&DO                                                                           
         L     R1,SRTMDED          MISC DEDUCTIONS + UNION DUES                 
         LA    RF,MISC                                                          
         BAS   RE,EDIT11                                                        
*&&                                                                             
         L     R1,SRTEORF          EOR GROSS FEES                               
         LA    RF,EORFEE                                                        
         BAS   RE,EDIT11                                                        
*                                                                               
         L     R1,SRTNOEOR         NON EOR GROSS FEES                           
         LA    RF,NOERFEE                                                       
         BAS   RE,EDIT11                                                        
*                                                                               
         L     R1,SRTPNH           P & H                                        
         LA    RF,PANDH                                                         
         BAS   RE,EDIT11                                                        
*                                                                               
         L     R1,SRTBBF           BROADCAST BUSINESS FEE                       
         LA    RF,BBF                                                           
         BAS   RE,EDIT11                                                        
*                                                                               
         L     R1,SRTGST           GST                                          
         LA    RF,GST                                                           
         BAS   RE,EDIT11                                                        
*                                                                               
         L     R1,SRTTOT           INVOICE TOTAL                                
         LA    RF,TOTAL                                                         
         BAS   RE,EDIT12                                                        
*                                                                               
         BAS   RE,SUMTOT           ADD TO TOTALS                                
         BAS   RE,PRINTIT          PRINT DETAIL LINE                            
*                                                                               
         MVC   LCURR,SRTCURR       SAVE LAST CURRENCY                           
         MVC   LEMP,SRTEMP                   EMPLOYER                           
         MVC   LTPO,SRTTPO                   OFFICE                             
         MVC   TRTPO,SRTTPO                  OFFICE                             
         MVC   LAGY,SRTAGY                   AGENCY                             
*                                                                               
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R3,4(R1)            ADDRESS OF RECORD GOTTEN BY SORTER           
         LTR   R3,R3               EOF                                          
         BZ    PDT                                                              
         BAS   RE,BREAK            CHECK IF BREAK IN SUMS                       
         B     PRD10                                                            
*                                                                               
PDT      OI    PRLEVEL,PRLGEN+PRLEMP+PRLOFF+PRLAGY                              
         OI    LSTAT,PRB2          SET FLAG - MIDDLE OF TOTALS                  
*                                                                               
PDT10    BAS   RE,PRBOT            PRINT THE TOTALS                             
         BNE   PDT10               MAKE SURE BOTTOM WAS PRINTED                 
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        THIS ROUTINE CHECKS IF THERE IS A BREAK IN TOTALS                      
*             EG - NEW AGENCY                                                   
*        R3 - A(NEXT RECORD TO BE SORTED)                                       
*                                                                               
         SPACE 1                                                                
BREAK    NTR1                                                                   
         USING SRTINVD,R3                                                       
         MVI   PRLEVEL,0                                                        
         MVI   LSTAT,0                                                          
         OC    0(SRTKEY,R3),0(R3)    END OF RUN - PRINT ALL TOTALS              
         BNZ   BR05                                                             
         OI    PRLEVEL,PRLGEN+PRLEMP+PRLOFF+PRLAGY                              
         B     BR30                                                             
*                                                                               
BR05     CLC   SRTCURR,LCURR       IF CURRENCY HAS CHANGED                      
         BE    BR07                                                             
         OI    PRLEVEL,PRLGEN+PRLEMP+PRLOFF+PRLAGY                              
         MVC   SVCURR,SRTCURR                                                   
         B     BR30                                                             
*                                                                               
BR07     CLC   SRTEMP,LEMP         IF EMPLOYER HAS CHANGED                      
         BE    BR10                                                             
         OI    PRLEVEL,PRLEMP+PRLOFF+PRLAGY     NEXT EMPLOYER                   
         MVC   SVEMP,SRTEMP                                                     
         B     BR30                                                             
*                                                                               
BR10     CLC   SRTTPO,TRTPO        IF OFFICE HAS CHANGED                        
         BE    BR20                                                             
         OI    PRLEVEL,PRLOFF+PRLAGY                                            
         MVC   SVTPO,SRTTPO                                                     
         B     BR30                       NEXT OFFICE                           
*                                                                               
BR20     CLC   SRTAGY,LAGY         IF AGENCY CHANGED                            
         BE    BRX                                                              
         OI    PRLEVEL,PRLAGY         NEXT AGENCY                               
         MVC   SVAGY,SRTAGY                                                     
*                                                                               
BR30     OI    LSTAT,PRB2          SET FLAG IN THE MIDDLE OF TOTALS             
*                                                                               
BR40     BAS   RE,PRBOT            AND PRINT THE TOTALS                         
         BNE   BR40                                                             
         MVI   LSTAT,0             CLEAR STATUS FLAG                            
*                                                                               
         CLI   FORCEHED,C'Y'       IF SET TO START A NEW PAGE                   
         BE    BRX                                                              
         ZIC   R2,LINE             OR IF THIS IS TOP OF NEXT PAGE               
         CH    R2,=H'13'              DON'T REPRINT AGY & TOP OF BOX            
         BNH   BRX                                                              
         ZIC   R1,MAXLINES         OR IF THIS IS THE BOTTOM OF THE              
         SR    R1,R2                                                            
         CH    R1,=H'10'              PAGE - DON'T PRINT MIDLINE                
         BNL   BR50                                                             
         MVI   FORCEHED,C'Y'       START NEW PAGE                               
         B     BRX                                                              
*                                                                               
BR50     DS    0H                                                               
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'88',SRTAGY),TNAME2H                       
         MVC   MID2(6),LTAGY       SET NEW AGENCY                               
         MVC   MID2+10(6),SRTAGY                                                
         MVC   MID2+20(L'TNAME2),TNAME2                                         
         OC    MID2,SPACES                                                      
         MVI   FORCEMID,C'Y'                                                    
         BAS   RE,PRINTIT                                                       
         GOTO1 BXTOP,DMCB,0,131,3                                               
         LA    R2,P                                                             
         BAS   RE,PRTOP            PRINT TOP OF BOX                             
         MVI   SPACING,2                                                        
         BAS   RE,PRINTIT                                                       
*                                                                               
BRX      DS    0H                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        PRINT TOP OF BOX                                                       
*        R2 - PRINTLINE OR HEADLINE                                             
*                                                                               
         SPACE 1                                                                
PRTOP    NTR1                                                                   
*                                                                               
         USING P1LINED,R2                                                       
         TM    LSTAT,PRB2          IF IN THE MIDDLE OF TOTALS                   
         BO    PRT05                  DON'T PRINT                               
         MVC   TYPE,LTTYPE         TYPE                                         
         MVC   CLIENT,LTCLIENT     CLIENT                                       
         MVC   CID,LTCID           COMMERCIAL ID                                
*                                                                               
PRT05    MVC   INVNUM,LTINV        INVOICE NUMBER                               
         MVC   GROSSI,LTGRSI       INDIVIDUAL GROSS WAGES                       
         MVC   GROSSC,LTGRSC       CORPORATE GROSS WAGES                        
         MVC   EORFEE,LTHNDI       EOR GROSS FEES                               
         MVC   NOERFEE,LTHNDC      NON EOR GROSS FEES                           
         MVC   PANDH,LTPNH         P & H CONTRIBUTION                           
         MVC   BBF,LTBBF           BROADCAST BUSINESS FEE                       
         MVC   GST,LTGST           GOODS AND SERVICES TAX                       
         MVC   TOTAL,LTTOT         INVOICE TOTAL                                
         LA    R1,P                                                             
         CR    R2,R1               CHECK IF FROM HEADHOOK OR PRDET              
         BNE   PRT10                                                            
         LA    R2,P2                                                            
         B     PRT20                                                            
*                                                                               
PRT10    LA    R2,HEAD11                                                        
*                                                                               
PRT20    TM    LSTAT,PRB2          IF IN THE MIDDLE OF TOTALS                   
         BO    PRT30                  DON'T PRINT                               
         MVC   CID,LTCID2             COMMERCIAL ID                             
*                                                                               
PRT30    MVC   INVNUM,LTINV2       INVOICE NUMBER                               
         MVC   GROSSI,LTGRSI2      INDIVIDUAL GROSS WAGES                       
         MVC   GROSSC,LTGRSI2      CORPORATE GROSS WAGES                        
         MVC   EORFEE,LTHNDI2      EOR GROSS FEES                               
         MVC   NOERFEE,LTHNDC2     NON EOR GROSS FEES                           
         MVC   PANDH,LTPNH2        P & H CONTRIBUTION                           
         MVC   BBF,LTBBF2          BROADCAST BUSINESS FEE                       
         MVC   TOTAL,LTTOT2        INVOICE TOTAL                                
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        PRINT BOTTOM BOX                                                       
*                                                                               
         SPACE 1                                                                
PRBOT    NTR1                                                                   
*                                                                               
         MVI   FORCE,C'N'          DEFAULT - NO PAGE EJECT                      
         ZIC   R2,LINE                                                          
         ZIC   R1,MAXLINES                                                      
         SR    R1,R2                                                            
         CH    R1,=H'3'                                                         
         BH    PRB03                                                            
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,PRINTIT                                                       
         B     NO                  HAVEN'T PRINTED BOTTOM YET                   
*                                                                               
         USING BOXD,R3                                                          
PRB03    L     R3,ABOX             HANDLE BOXES                                 
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+131,C'R'                                                 
         BAS   RE,BXCOL            SET COLUMNS                                  
         MVI   BOXREQ,C'B'         COLUMN CHANGE & MID LINE                     
         BAS   RE,PRINTIT                                                       
         LA    R2,P                AGENCY TOTALS                                
         USING P1LINED,R2                                                       
         TM    PRLEVEL,PRLAGY                                                   
         BNO   PBX                                                              
         MVC   P1TITLE,LTAGYTOT                                                 
         LH    R1,INVAGY                                                        
         LA    R4,AGYPAYI                                                       
         BAS   RE,PRSUB            PRINT AGENCY SUBTOTALS                       
*                                                                               
         LH    R1,BNPAGY           BILL NO PAYMENT TOTALS                       
         LTR   R1,R1                                                            
         BZ    PRB05                                                            
         MVC   P1TITLE,LTBAYTOT                                                 
         LA    R4,BAYPAYI          PRINT B-N-P SUBTOTAL                         
         BAS   RE,PRSUB                                                         
*                                                                               
PRB05    DS    0H                                                               
         NI    PRLEVEL,X'FF'-PRLAGY  TURN OFF AGY BREAK                         
         LH    R4,INVOFF           ADD AGENCY TOTALS TO OFFICE                  
         AH    R4,INVAGY               TOTALS                                   
         STH   R4,INVOFF                                                        
         LH    R4,BNPOFF                                                        
         AH    R4,BNPAGY                                                        
         STH   R4,BNPOFF                                                        
*                                                                               
         LA    R5,BOFPAYI          ADD AMOUNTS TO OFFICE TOTAL                  
         LA    R4,BAYPAYI                                                       
         BAS   RE,SUMAMT                                                        
*                                                                               
         XC    BAYPAYI(AGYAMTS),BAYPAYI   CLEAR AGENCY TOTALS & CTRS            
         XC    INVAGY,INVAGY                 FOR NEXT AGENCY                    
         XC    BNPAGY,BNPAGY                                                    
*                                                                               
PRB10    TM    PRLEVEL,PRLOFF      PRINT OUT OFFICE TOTALS                      
         BNO   PBX                                                              
         MVI   FORCE,C'Y'          FORCE A PAGE EJECT                           
         BAS   RE,PRINTIT          SKIP A LINE BETWEEN TOTALS                   
         MVC   P1TITLE,LTOFFTOT                                                 
         LH    R1,INVOFF                                                        
         LA    R4,OFFPAYI          PRINT OUT OFFICE SUBTOTALS FIRST             
         BAS   RE,PRSUB                                                         
*                                                                               
         LH    R1,BNPOFF                                                        
         LTR   R1,R1                                                            
         BZ    PRB15                                                            
         MVC   P1TITLE,LTBAYTOT                                                 
         LA    R4,BOFPAYI          THEN PRINT B-N-P SUBTOTAL                    
         BAS   RE,PRSUB                                                         
*                                                                               
PRB15    NI    PRLEVEL,X'FF'-PRLOFF TURN OFF OFFICE BREAK                       
         LH    R4,INVEMP           ADD OFFICE TOTALS TO EMPLOYER                
         AH    R4,INVOFF               TOTALS                                   
         STH   R4,INVEMP                                                        
         LH    R4,BNPEMP                                                        
         AH    R4,BNPOFF                                                        
         STH   R4,BNPEMP                                                        
*                                                                               
         LA    R5,BEMPAYI          ADD TO EMPLOYER TOTAL                        
         LA    R4,BOFPAYI                                                       
         BAS   RE,SUMAMT                                                        
*                                                                               
         XC    BOFPAYI(OFFAMTS),BOFPAYI   CLEAR OFFICE TOTALS & CTRS            
         XC    INVOFF(4),INVOFF                 FOR NEXT OFFICE                 
*                                                                               
PRB20    TM    PRLEVEL,PRLEMP      PRINT OUT EMPLOYER TOTALS                    
         BNO   PBX                                                              
         MVI   FORCE,C'Y'          FORCE A PAGE EJECT                           
         BAS   RE,PRINTIT                                                       
         MVC   P1TITLE,LTEMPTOT                                                 
         LH    R1,INVEMP                                                        
         LA    R4,EMPPAYI          PRINT OUT EMP SUBTOTALS FIRST                
         BAS   RE,PRSUB                                                         
*                                                                               
         LH    R1,BNPEMP                                                        
         LTR   R1,R1                                                            
         BZ    PRB25                                                            
         MVC   P1TITLE,LTBAYTOT                                                 
         LA    R4,BEMPAYI          THEN PRINT B-N-P SUBTOTAL                    
         BAS   RE,PRSUB                                                         
*                                                                               
PRB25    NI    PRLEVEL,X'FF'-PRLEMP TURN OFF EMPLOYER BREAK                     
         LH    R4,INVGEN           ADD EMPLOYER TOTALS TO GENERAL               
         AH    R4,INVEMP                                                        
         STH   R4,INVGEN               TOTALS                                   
         LH    R4,BNPGEN                                                        
         AH    R4,BNPEMP                                                        
         STH   R4,BNPGEN                                                        
*                                                                               
         LA    R5,BGEPAYI          ADD TO EMPLOYER TOTAL                        
         LA    R4,BEMPAYI                                                       
         BAS   RE,SUMAMT                                                        
*                                                                               
         XC    BEMPAYI(EMPAMTS),BEMPAYI   CLEAR EMP TOTALS & CTRS               
         XC    INVEMP(4),INVEMP                 FOR NEXT EMP                    
*                                                                               
PRB30    TM    PRLEVEL,PRLGEN      PRINT OUT GENERAL TOTALS                     
         BNO   PBX                                                              
         BAS   RE,PRINTIT                                                       
         MVC   P1TITLE,LTGENTOT                                                 
         LH    R1,INVGEN                                                        
         LA    R4,GENPAYI                                                       
         BAS   RE,PRSUB                                                         
*                                                                               
         LH    R1,BNPGEN                                                        
         LTR   R1,R1                                                            
         BZ    PRB35                                                            
         MVC   P1TITLE,LTBAYTOT                                                 
         LA    R4,BGEPAYI          THEN PRINT B-N-P SUBTOTAL                    
         BAS   RE,PRSUB                                                         
*                                                                               
PRB35    NI    PRLEVEL,X'FF'-PRLGEN TURN OFF GENERAL BREAK                      
         XC    BGEPAYI(GENAMTS),BGEPAYI   CLEAR GENERAL TOTALS & CTRS           
         XC    INVGEN(4),INVGEN                 FOR NEXT RUN                    
*                                                                               
PRB40    DS    0H                                                               
*                                                                               
PBX      DS    0H                                                               
         BAS   RE,BXBOT            CLOSE BOX                                    
*        MVI   SPACING,3                                                        
         BAS   RE,PRINTIT                                                       
         CLI   FORCE,C'Y'                                                       
         BNE   YES                                                              
         MVI   FORCEHED,C'Y'                                                    
         B     YES                                                              
         EJECT                                                                  
*                                                                               
*        ADD SUBTOTALS                                                          
*              R4 = A(AMOUNTS TO ADD)                                           
*              R5 = A(AMOUNTS TO ADD TO)                                        
*                                                                               
SUMAMT   NTR1                                                                   
         LA    R3,16               16 AMOUNTS TO ADD                            
*                                                                               
SAM10    L     R1,0(R4)            ADD AGENCY TOTALS TO OFFICE TOTALS           
         L     R2,0(R5)               (OFFICE TO EMPLOYER)                      
         AR    R2,R1                  (EMPLOYER TO GENERAL)                     
         ST    R2,0(R5)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R3,SAM10                                                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PRINT SUBTOTAL LINES                                                   
*              R4 = A(START OF AMOUNTS TO PRINT)                                
*                                                                               
PRSUB    NTR1                                                                   
         LA    RF,INVNUM+1         NUMBER OF INVOICES                           
         BAS   RE,EDIT5                                                         
*                                                                               
         L     R1,0(R4)            INDIVIDUAL GROSS                             
         LA    RF,GROSSI                                                        
         BAS   RE,EDIT12                                                        
*                                                                               
         LA    R4,4(R4)            CORPORATE GROSS                              
         L     R1,0(R4)                                                         
         LA    RF,GROSSC                                                        
         BAS   RE,EDIT12                                                        
*                                                                               
         LA    R4,4(R4)            EOR GROSS FEES                               
         L     R1,0(R4)                                                         
         LA    RF,EORFEE                                                        
         BAS   RE,EDIT11                                                        
*                                                                               
         LA    R4,4(R4)            NON EOR GROSS FEES                           
         L     R1,0(R4)                                                         
         LA    RF,NOERFEE                                                       
         BAS   RE,EDIT11                                                        
*                                                                               
         LA    R4,4(R4)            P & H                                        
         L     R1,0(R4)                                                         
         LA    RF,PANDH                                                         
         BAS   RE,EDIT11                                                        
*                                                                               
         LA    R4,4(R4)            BROADCAST BUSINESS FEE                       
         L     R1,0(R4)                                                         
         LA    RF,BBF                                                           
         BAS   RE,EDIT11                                                        
*                                                                               
         LA    R4,4(R4)            GST                                          
         L     R1,0(R4)                                                         
         LA    RF,GST                                                           
         BAS   RE,EDIT11                                                        
*                                                                               
         LA    R4,4(R4)            TOTAL                                        
         L     R1,0(R4)                                                         
         LA    RF,TOTAL                                                         
         BAS   RE,EDIT12                                                        
*                                                                               
         BAS   RE,PRINTIT                                                       
*                                                                               
PSUM     B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        SUM TOTALS                                                             
*                                                                               
         SPACE 1                                                                
         USING SRTINVD,R3                                                       
SUMTOT   NTR1                                                                   
         CLC   SRTTYPE,=C'BNP '                                                 
         BNE   SUM10                                                            
         LA    R4,BAYPAYI          B-N-P TOTAL                                  
         LH    R1,BNPAGY           INCREMENT NUMBER OF BNP                      
         LA    R1,1(R1)                                                         
         STH   R1,BNPAGY                                                        
         B     SUM20                                                            
*                                                                               
SUM10    LA    R4,AGYPAYI          AGENCY TOTALS                                
         LH    R1,INVAGY           INCREMENT NUMBER OF INVOICES                 
         LA    R1,1(R1)                                                         
         STH   R1,INVAGY                                                        
*                                                                               
SUM20    LA    R1,SRTPAYI          START OF AMOUNTS                             
         LA    R2,8                NUMBER OF AMOUNTS TO ADD UP                  
*                                                                               
SUM30    L     R5,0(R4)            LOAD TOTAL VALUE                             
         L     R6,0(R1)                                                         
         AR    R5,R6                                                            
         ST    R5,0(R4)            AND STORE SUM                                
         LA    R1,4(R1)            NEXT AMOUNT                                  
         LA    R4,4(R4)            NEXT TOTAL                                   
         BCT   R2,SUM30                                                         
         B     XIT                                                              
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        EDIT A LINE - INPUT (R1)                                               
*                      OUTPUT LENGTH = 5                                        
*                      OUTPUT ADDR   = RF                                       
*                                                                               
         SPACE 1                                                                
EDIT5    EDIT  (R1),(5,(RF)),ZERO=BLANK                                         
         BR    RE                                                               
         SPACE 1                                                                
*                                                                               
*        EDIT A LINE - INPUT (R1)                                               
*                      OUTPUT LENGTH = 11                                       
*                      OUTPUT ADDR   = RF                                       
*                                                                               
         SPACE 1                                                                
EDIT11   EDIT  (R1),(11,(RF)),2,MINUS=YES,ZERO=BLANK                            
         BR    RE                                                               
         SPACE 1                                                                
*                                                                               
*        EDIT A LINE - INPUT (R1)                                               
*                      OUTPUT LENGTH = 12                                       
*                      OUTPUT ADDR   = RF                                       
*                                                                               
         SPACE 1                                                                
EDIT12   EDIT  (R1),(12,(RF)),2,MINUS=YES,ZERO=BLANK                            
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
*        PRINT A LINE                                                           
*                                                                               
         SPACE 1                                                                
PRINTIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   SPACING,1           RESET SPACING                                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              HEADLINE/BOX ROUTINES (HEADHOOK)                                 
*              R3 = A(NEXT RECORD)                                              
*                                                                               
         SPACE 1                                                                
HOOK     NTR1                                                                   
         USING SRTINVD,R3                                                       
         L     R4,ABOX             NOW HANDLE BOXES                             
         USING BOXD,R4                                                          
*                                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
*                                                                               
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,SPACES                                                   
         LA    R2,9                                                             
         LA    R2,BOXROWS-1(R2)    SET TOP OF BOX                               
         MVI   0(R2),C'T'                                                       
         LA    R2,3(R2)                                                         
         MVI   0(R2),C'M'          SET MIDDLE OF BOX                            
*                                                                               
         CLI   PRLEVEL,0           IF IN THE DONE WITH TOTALS                   
         BNE   HOOK10              ENSURE CORRECT INFO FOR SPECS                
         OI    LSTAT,PRB           &  USE 2ND SET OF COLUMNS                    
*                                                                               
HOOK10   LA    R4,BOXCOLS                                                       
         MVI   0(R4),C'L'          LEFT HAND SIDE                               
         LA    R4,131(R4)                                                       
         MVI   0(R4),C'R'          RIGHT HAND SIDE                              
         BAS   RE,BXCOL            SET BOX COLUMNS                              
         MVI   BOXINIT,0                                                        
*                                                                               
         MVC   HEAD3+58(L'PERIOD),PERIOD                                        
         MVC   HEAD5(L'LTOFF),LTOFF      OFFICE                                 
*                                                                               
HOOK15   MVC   HEAD7(L'LTAGY),LTAGY      AGENCY                                 
         MVC   HEAD4(L'LTEMP),LTEMP      EMPLOYER                               
         TM    PRLEVEL,PRLGEN      IF AT END OF FILE & IN THE MIDDLE            
         BO    HOOK20                 OF TOTALS - LEAVE EMP/CURR                
         TM    PRLEVEL,PRLEMP                                                   
         BO    HOOK20                                                           
         MVC   SVCURR,SRTCURR                                                   
         MVC   SVEMP,SRTEMP                                                     
         B     HOOK30                                                           
*                                                                               
HOOK20   MVC   SVCURR,LCURR      USE LAST CURRENCY/EMPLOYER READ                
         MVC   SVEMP,LEMP                                                       
*                                                                               
HOOK30   MVC   HEAD3(L'LTCURR),LTCURR    CURRENCY                               
         MVC   HEAD3+10(3),=C'US$'                                              
         CLI   SVCURR,C'C'         IF CANADIAN THEN PUT CAN$                    
         BNE   *+10                                                             
         MVC   HEAD3+10(4),=C'CAN$'                                             
         CLI   SVCURR,C'E'         IF EUROS THEN PUT EURO                       
         BNE   HOOK35                                                           
         MVC   HEAD3+10(4),=C'EURO'                                             
*                                                                               
HOOK35   MVC   HEAD4+10(L'SVEMP),SVEMP                                          
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'8C',SVEMP),TNAME2H                        
         BNE   HOOK40                                                           
         MVC   HEAD4+20(L'TNAME2),TNAME2                                        
*                                                                               
HOOK40   DS    0H                                                               
         TM    PRLEVEL,PRLGEN      IF AT END OF FILE & IN THE MIDDLE            
         BO    HOOK50                                                           
         TM    PRLEVEL,PRLOFF                                                   
         BO    HOOK50                                                           
         MVC   SVTPO,SRTTPO                                                     
         B     HOOK60                                                           
*                                                                               
HOOK50   MVC   SVTPO,TRTPO         USE SAVED TPO OFFICE                         
*                                                                               
HOOK60   MVC   HEAD5+10(L'SVTPO),SVTPO                                          
         GOTO1 RECVAL,DMCB,TLOFCDQ,(X'8C',SVTPO),TNAME2H                        
         BNE   HOOK65                                                           
         MVC   HEAD5+20(L'TNAME2),TNAME2                                        
*                                                                               
HOOK65   TM    PRLEVEL,PRLGEN      IF AT END OF FILE & IN THE MIDDLE            
         BO    HOOK70                                                           
         TM    PRLEVEL,PRLAGY                                                   
         BO    HOOK70                                                           
         MVC   SVAGY,SRTAGY                                                     
         B     HOOK80                                                           
*                                                                               
HOOK70   MVC   SVAGY,LAGY          USE SAVED TPO OFFICE                         
*                                                                               
HOOK80   MVC   HEAD7+10(L'SVAGY),SVAGY                                          
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'8C',SVAGY),TNAME2H                        
         BNE   HOOK90                                                           
         MVC   HEAD7+20(L'TNAME2),TNAME2                                        
*                                                                               
HOOK90   DS    0H                                                               
         LA    R2,HEAD10                                                        
         BAS   RE,PRTOP            PRINT TOP OF REPORT                          
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*              ROUTINE TO POP IN A BOX TOP                                      
*                                                                               
*              P1 = BOXCOLS + X LHS                                             
*              P2 = BOXCOLS + X RHS                                             
*              P3 = MIDLNS                                                      
*                                                                               
         SPACE 1                                                                
BXTOP    NTR1                                                                   
         L     R3,ABOX                                                          
         USING BOXD,R3                                                          
         L     R4,0(R1)            LHS                                          
         L     RE,4(R1)            RHS                                          
         L     R2,8(R1)            MID LINES                                    
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,SPACES                                                   
         ZIC   RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'T'                                                       
         LTR   R2,R2               DO WE WANT A MIDDLE LINE                     
         BZ    BXT10                                                            
         AR    RF,R2               NUMBER OF LINES TO MIDDLE LINE               
         MVI   0(RF),C'M'                                                       
*                                                                               
BXT10    LA    R4,BOXCOLS(R4)                                                   
         MVI   0(R4),C'L'                                                       
         LA    RE,BOXCOLS(RE)                                                   
         MVI   0(RE),C'R'                                                       
*                                                                               
BXT30    OI    LSTAT,PRB           SET FLAG - WHICH LINE AND THEREFORE          
         BAS   RE,BXCOL                                                         
         MVI   BOXINIT,0                                                        
         BAS   RE,PRINTIT                                                       
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*              ROUTINE TO POP IN A BOX BOTTOM                                   
*                                                                               
         SPACE 1                                                                
BXBOT    NTR1                                                                   
         L     R3,ABOX                                                          
         USING BOXD,R3                                                          
         MVC   BOXROWS,SPACES                                                   
         ZIC   RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'B'                                                       
         MVI   BOXINIT,0                                                        
         B     XIT                                                              
         DROP  R3                                                               
         SPACE 1                                                                
*                                                                               
*              ROUTINE TO SET COLUMNS                                           
*                                                                               
         SPACE 1                                                                
BXCOL    NTR1                                                                   
         L     R3,ABOX                                                          
         USING BOXD,R3                                                          
         LA    R2,BOXCOLS                                                       
         USING P1LINED,R2                                                       
         TM    LSTAT,PRB2          MIDDLE LINE                                  
         BO    BXC10                                                            
         TM    LSTAT,PRB           TOP LINE                                     
         BNO   BXCX                                                             
         MVI   P1CO2,C'C'                                                       
         MVI   P1CO3,C'C'                                                       
*                                                                               
BXC10    MVI   P1CO4,C'C'                                                       
         MVI   P1CO5,C'C'                                                       
         MVI   P1CO6,C'C'                                                       
         MVI   P1CO7,C'C'                                                       
         MVI   P1CO8,C'C'                                                       
         MVI   P1CO9,C'C'                                                       
         MVI   P1CO10,C'C'                                                      
         MVI   P1CO11,C'C'                                                      
         MVI   P1CO12,C'C'                                                      
*                                                                               
BXCX     B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
YES      SR    RC,RC               SET CONDITION CODE                           
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
         SPACE 3                                                                
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,17,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=100'                                   
*                                                                               
*  CONSTANTS                                                                    
*                                                                               
*                                                                               
COMPLM   DC    X'FFFFFFFFFFFF'                                                  
*                                                                               
LTTYPE   DC    C'Type'                                                          
LTCLIENT DC    C'Client'                                                        
LTCID    DC    C' Commercial '                                                  
LTINV    DC    C' Inv   '                                                       
LTGRSI   DC    C' Individual '                                                  
LTGRSC   DC    C' Corporate  '                                                  
LTHNDI   DC    C' EOR Gross '                                                   
LTHNDC   DC    C'  Non EOR  '                                                   
LTPNH    DC    C'   P && H   '                                                  
LTBBF    DC    C' Broadcast '                                                   
LTGST    DC    C' GST / CSF '                                                   
LTTOT    DC    C'  Invoice   '                                                  
*                                                                               
LTCID2   DC    C'     ID     '                                                  
LTINV2   DC    C'Number'                                                        
LTGRSI2  DC    C'   Gross    '                                                  
LTHNDI2  DC    C'   Fees    '                                                   
LTHNDC2  DC    C'Gross Fees '                                                   
LTPNH2   DC    C'  Contrib   '                                                  
LTBBF2   DC    C' Bus. Fee  '                                                   
LTTOT2   DC    C'   Total    '                                                  
         SPACE 1                                                                
*                                                                               
LTAGYTOT DC    C'Agency Totals           '                                      
LTBAYTOT DC    C'Bill No Payment Totals  '                                      
LTOFFTOT DC    C'Office Totals           '                                      
LTEMPTOT DC    C'Employer Totals         '                                      
LTGENTOT DC    C'Totals                  '                                      
         SPACE 1                                                                
*                                                                               
LTCURR   DC    C'Currency'                                                      
LTEMP    DC    C'Employer'                                                      
LTOFF    DC    C'Office'                                                        
LTAGY    DC    C'Agency'                                                        
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        SPECS                                                                  
*                                                                               
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,107,REPORT                                                    
         SSPEC H1,123,PAGE                                                      
         SSPEC H2,107,REQUESTOR                                                 
         SPACE 1                                                                
         SSPEC H1,59,C'INVOICE  REGISTER'                                       
         SSPEC H2,59,17X'BF'                                                    
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        PRINT LINE DSECTS                                                      
*                                                                               
         SPACE 1                                                                
P1LINED  DSECT                     INVOICE REGISTER HEADINGS                    
*                                                                               
P1CO1    DS    CL1                 COLUMN FOR BOX                               
TYPE     DS    CL4                 TYPE                                         
P1CO2    DS    CL1                                                              
CLIENT   DS    CL6                 CLIENT                                       
P1CO3    DS    CL1                                                              
CID      DS    CL12                COMMERCIAL ID                                
         ORG   TYPE                                                             
P1TITLE  DS    CL24                SUBTOTAL NAME                                
P1CO4    DS    CL1                                                              
INVNUM   DS    CL6                 INVOICE NUMBER                               
P1CO5    DS    CL1                                                              
GROSSI   DS    CL12                INDIVIDUAL GROSS                             
P1CO6    DS    CL1                                                              
GROSSC   DS    CL12                CORPORATE GROSS                              
P1CO7    DS    CL1                                                              
EORFEE   DS    CL11                EOR GROSS FEES                               
P1CO8    DS    CL1                                                              
NOERFEE  DS    CL11                NON EOR GROSS FEES                           
P1CO9    DS    CL1                                                              
PANDH    DS    CL11                PNH CONTRIBUTION                             
P1CO10   DS    CL1                                                              
BBF      DS    CL11                BROADCAST BUSINESS FEE                       
P1CO11   DS    CL1                                                              
GST      DS    CL11                GST                                          
P1CO12   DS    CL1                                                              
TOTAL    DS    CL12                INVOICE TOTAL                                
P1CO13   DS    CL1                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*        SORT TABLE DSECT                                                       
*                                                                               
SRTINVD  DSECT                                                                  
*                                                                               
SRTCURR  DS    CL1                 CURRENCY                                     
SRTEMP   DS    CL3                 EMPLOYER OF RECORD                           
SRTTPO   DS    CL1                 TALENT PARTNERS OFFICE                       
SRTAGY   DS    CL6                 AGENCY                                       
SRTINV   DS    PL6                 INVOICE NUMBER                               
SRTKEY   EQU   *-SRTEMP            LENGTH OF SORT KEY                           
SRTTYPE  DS    CL4                 TYPE OF INVOICE                              
SRTCLT   DS    CL6                 CLIENT                                       
SRTCID   DS    CL12                COMMERCIAL ID                                
*                                                                               
SRTAMTS  DS    0F                                                               
SRTPAYI  DS    F                   PAYMENT AMOUNT FOR INDIVIDUALS               
SRTPAYC  DS    F                   PAYMENT AMOUNT FOR CORPORATIONS              
SRTEORF  DS    F                   EOR GROSS FEES                               
SRTNOEOR DS    F                   NON EOR GROSS FEES                           
SRTPNH   DS    F                   PENSION AND HEALTH                           
SRTBBF   DS    F                   BROADCAST BUSINESS FEE                       
SRTGST   DS    F                   GOODS AND SERVICES TAX                       
SRTTOT   DS    F                   INVOICE TOTAL                                
SRTLAMT  EQU   *-SRTAMTS                                                        
SRTILEN  EQU   *-SRTINVD                                                        
         EJECT                                                                  
         SPACE 1                                                                
       ++INCLUDE TAREGD                                                         
         EJECT                                                                  
         SPACE 1                                                                
*        OTHER DSECTS ARE HIDDEN IN HERE                                        
         SPACE 1                                                                
*TAREPFFD                                                                       
*TAREPF8D                                                                       
*DDGENTWA                                                                       
*DDTWADCOND                                                                     
*DDMASTD                                                                        
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*CTGENFILE                                                                      
*ACGENBOTH                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*TAREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAREPFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPF8D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051TAREP18   12/13/12'                                      
         END                                                                    
