*          DATA SET TAGEN12    AT LEVEL 140 AS OF 03/15/18                      
*PHASE T70212C,*                                                                
         TITLE 'T70212 - AGENCY MAINTENANCE'                                    
*---------------------------------------------------------------------*         
* GHOA 140 15MAR18 SPSUG-1239  ELIM NEED AGENCY STATUSES: NX, SX, CX  *         
*---------------------------------------------------------------------*         
T70212   RSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKLNQ,T70212,R7                                                
         LR    RF,RC                                                            
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         ST    RF,AMQMSG                                                        
         EJECT                                                                  
* MAIN DRIVER                                                                   
*                                                                               
MAIN     DS    0H                                                               
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE OVERLAY                           
         LA    RE,STATTAB                                                       
         ST    RE,ASTATTAB                                                      
*                                                                               
         BAS   RE,PASSPTRS         SAVE OR ADD PTRS FOR GIVEN MODE              
         BAS   RE,TESTDEL          TEST DELETE NOT ALLOWED                      
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    M10                                                              
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    M20                                                              
         CLI   MODE,DISPREC        DISPLAY THE RECORD                           
         BE    M30                                                              
         CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BE    M40                                                              
         CLI   MODE,XRECADD        DISPLAY THE RECORD                           
         BE    M30                                                              
         CLI   MODE,XRECPUT        IF RECORD CHANGED                            
         BNE   *+12                                                             
         BRAS  RE,ADDREQ           ADD OFFICE/NEW REQUEST                       
         B     M30                 AND DISPLAY THE RECORD                       
         CLI   MODE,XRECDEL        DISPLAY THE RECORD                           
         BE    M30                                                              
         CLI   MODE,XRECREST       DISPLAY THE RECORD                           
         BE    M30                                                              
         B     XIT                                                              
*                                                                               
M10      BAS   RE,VK               VALIDATE KEY                                 
         B     MX                                                               
*                                                                               
M20      BAS   RE,DK               DISPLAY KEY                                  
         B     MX                                                               
*                                                                               
M30      BAS   RE,DR               DISPLAY RECORD                               
         CLI   MODE,XRECPUT        IF RECORD CHANGED                            
         BNE   MX                                                               
         CLI   TDOFLAG,C'Y'        AND TDO REQUESTED                            
         BE    DOADDED             GIVE APPROPRIATE MESSAGE                     
         B     MX                  ELSE, JUST GIVE NORMAL MESSAGE               
*                                                                               
M40      BAS   RE,VR               VALDATE RECORD                               
         B     MX                                                               
*                                                                               
MX       B     XIT                                                              
         EJECT                                                                  
* SAVE OLD PASSIVE POINTERS OR ADD NEW PASSIVE POINTER DEPENDING ON             
* THE MODE.                                                                     
*                                                                               
PASSPTRS NTR1                                                                   
         CLI   MODE,VALREC         IF MODE VALREC, RECDEL, OR RECREST           
         BE    PP10                                                             
         CLI   MODE,RECDEL                                                      
         BE    PP10                                                             
         CLI   MODE,RECREST                                                     
         BNE   PP20                                                             
*                                  THEN SAVE CURRENT PASSIVE POINTER(S)         
PP10     GOTO1 SAVPTRS,DMCB,PPBLOCK                                             
         B     PPX                                                              
*                                                                               
PP20     CLI   MODE,XRECADD        ELSE IF MODE XRECADD, XRECPUT,               
         BE    PP30                    XRECDEL, OR XRECREST                     
         CLI   MODE,XRECPUT                                                     
         BE    PP30                                                             
         CLI   MODE,XRECDEL                                                     
         BE    PP30                                                             
         CLI   MODE,XRECREST                                                    
         BNE   PPX                                                              
*                                  THEN ADD PASSIVE POINTER(S)                  
PP30     GOTO1 ADDPTRS,DMCB,PPBLOCK                                             
         BRAS  RE,NFYVIT           AND NOTIFY VITA OF ACTION                    
*                                                                               
PPX      B     XIT                                                              
         EJECT                                                                  
* IF DELETING A RECORD, MAKE SURE THERE IS NO CMCL FOR THIS AGENCY              
*                                                                               
TESTDEL  NTR1                                                                   
         CLI   MODE,RECDEL         IF MODE IS RECDEL                            
         BNE   TDX                                                              
*                                                                               
         MVC   SVKEY,KEY           THEN SAVE KEY                                
*                                                                               
         XC    TGCID,TGCID         READ FOR FIRST CMCL FOR THIS AGY             
         GOTO1 RECVAL,DMCB,TLCOICDQ,0                                           
*                                  IF CMCL FOUND                                
         CLC   KEY(TLCOICID-TLCOPD),KEYSAVE                                     
         BE    ERRDEL              GIVE ERROR                                   
*                                                                               
         XC    TGATT,TGATT         READ FOR FIRST ATTN FOR THIS AGY             
         GOTO1 RECVAL,DMCB,TLATCDQ,0                                            
*                                  IF ATTN FOUND                                
         CLC   KEY(TLATATT-TLATD),KEYSAVE                                       
         BE    ERRDEL              GIVE ERROR                                   
*                                                                               
         MVC   KEY,SVKEY           ELSE RESTORE KEY                             
*                                                                               
TDX      B     XIT                                                              
         EJECT                                                                  
* VALIDATE THE KEY                                                              
*                                                                               
VK       NTR1                                                                   
*                                  VALIDATE AGENCY                              
         CLC   =C'N/A',SAYAGY                                                   
         JE    ERRINV                                                           
         CLI   SAYAGY,C' '                                                      
         JE    ERRINV                                                           
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'40',SAYAGYH)                              
         B     XIT                                                              
         EJECT                                                                  
* DISPLAY THE KEY                                                               
*                                                                               
DK       NTR1                                                                   
*                                                                               
         LA    R4,KEY              POINT TO KEY                                 
         USING TLAYD,R4                                                         
         MVC   SAYAGY,TLAYAGY      DISPLAY AGENCY                               
         OI    SAYAGYH+6,X'80'                                                  
         DROP  R4                                                               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* DISPLAY THE RECORD                                                            
*                                                                               
DR       NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
*                                                                               
         TWAXC SAYNAH                                                           
         XC    SAYTPCN,SAYTPCN                                                  
         OI    SAYTPCNH+6,X'80'                                                 
         XC    SAYRECN,SAYRECN                                                  
         OI    SAYRECNH+6,X'80'                                                 
*        XC    SAYSNME,SAYSNME                                                  
*        OI    SAYSNMEH+6,X'80'                                                 
*                                  DISPLAY LONG NAME                            
         GOTO1 CHAROUT,DMCB,TANAELQ,SAYNAH                                      
*                                  DISPLAY SHORT NAME                           
         GOTO1 CHAROUT,DMCB,TASNELQ,SAYSNH                                      
*                                  DISPLAY ADDRESS                              
         GOTO1 CHAROUT,DMCB,TAADELQ,(4,SAYADH)                                  
*                                  DISPLAY ATTENTION NAME                       
         GOTO1 CHAROUT,DMCB,TAFNELQ,SAYTATTH,TAFNTATT                           
*                                  DISPLAY EMAIL ADDRESS                        
         GOTO1 CHAROUT,DMCB,TACMELQ,SAYEMAIH,TACMTYPI                           
*                                  DISPLAY FAX NUMBER                           
         GOTO1 CHAROUT,DMCB,TANUELQ,SAYFAXH,TANUTFAX                            
*                                  DISPLAY ESTIMATE SETUP (JOBS)                
         GOTO1 CHAROUT,DMCB,TANUELQ,SAYJOBH,TANUTJOB                            
*                                  DISPLAY SIGNATORY                            
         GOTO1 CHAROUT,DMCB,TANUELQ,SAYSIGNH,TANUTSIG                           
*                                  DISPLAY STUDIO CODE                          
         GOTO1 CHAROUT,DMCB,TAFNELQ,SAYSTUDH,TAFNTSTU                           
*                                                                               
         CLI   SAYSIGNH+5,0        DO WE HAVE ONE?                              
         BE    DR2                 NO                                           
*                                  YES, READ AND DISPLAY NAME                   
         MVC   AIO,AIO2                                                         
         MVC   SVTGAGY,TGAGY       SAVE GLOBAL AGENCY                           
*        GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SAYSIGNH),SAYSNMEH                    
         MVC   AIO,AIO1                                                         
         MVC   TGAGY,SVTGAGY       RESTORE GLOBAL AGENCY                        
*                                                                               
DR2      L     R4,AIO              DISPLAY AGENCY ELEMENT                       
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAAYD,R4                                                         
*                                                                               
         MVC   SVBUNT,TAAYBUNT                                                  
*                                                                               
         CLI   TAAYTPOF,C'O'       OFFICE O IS SPECIAL                          
         BE    DR3                                                              
         CLI   TAAYTPOF,C'Q'       OFFICE Q IS SPECIAL                          
         BE    DR3                                                              
*                                                                               
         OI    SAYADRTH+1,X'2C'    LOW INTENSITY AND PROTECTED                  
         OI    SAYCCURH+1,X'2C'                                                 
         OI    SAYCSUTH+1,X'2C'                                                 
         OI    SAYCFICH+1,X'2C'                                                 
         OI    SAYCOFIH+1,X'2C'                                                 
         OI    SAYCWCRH+1,X'2C'                                                 
         OI    SAYCCRPH+1,X'2C'                                                 
         OI    SAYCHNDH+1,X'2C'                                                 
         OI    SAYCCANH+1,X'2C'                                                 
         B     DR4                                                              
*                                                                               
DR3      NI    SAYADRTH+1,X'F3'    REG INTENSITY                                
         NI    SAYCCURH+1,X'D3'    REG INTENSITY AND UN-PROTECTED               
         NI    SAYCSUTH+1,X'D3'                                                 
         NI    SAYCFICH+1,X'D3'                                                 
         NI    SAYCOFIH+1,X'D3'                                                 
         NI    SAYCWCRH+1,X'D3'                                                 
         NI    SAYCCRPH+1,X'D3'                                                 
         NI    SAYCHNDH+1,X'D3'                                                 
         NI    SAYCCANH+1,X'D3'                                                 
*                                                                               
DR4      OI    SAYADRTH+6,X'80'                                                 
         OI    SAYCCURH+6,X'80'                                                 
         OI    SAYCSUTH+6,X'80'                                                 
         OI    SAYCFICH+6,X'80'                                                 
         OI    SAYCOFIH+6,X'80'                                                 
         OI    SAYCWCRH+6,X'80'                                                 
         OI    SAYCCRPH+6,X'80'                                                 
         OI    SAYCHNDH+6,X'80'                                                 
         OI    SAYCCANH+6,X'80'                                                 
*                                                                               
         MVC   STATAY,TAAYSTAT     SAVE STATUS BYTES                            
         MVC   STATAY2,TAAYSTA2                                                 
         MVC   STATAY3,TAAYSTA3                                                 
         MVC   STATAY4,TAAYSTA4                                                 
         MVC   STATAY5,TAAYSTA5                                                 
         MVC   STATAY6,TAAYSTA6                                                 
         MVC   STATAY7,TAAYSTA7                                                 
         MVC   STATAYSG,TAAYSGNS                                                
*                                                                               
         MVC   SAYTEL,TAAYTEL      DISPLAY TELEPHONE NUMBER                     
         MVC   SAYTPOF,TAAYTPOF    DISPLAY TP OFFICE CODE                       
*                                                                               
         BRAS  RE,DRSERV           DISPLAY SERVICE LEVEL                        
*                                                                               
*                                  DISPLAY BILLING COPIES                       
         EDIT  (1,TAAYNBIL),(3,SAYNBIL),ALIGN=LEFT                              
*                                                                               
         BAS   RE,DRHLD            DISPLAY HOLDING FEE NOTICE                   
         BRAS  RE,DRFILT           DISPLAY FILTERS                              
*                                                                               
         OC    TAAYTID,TAAYTID     IF TPC USER ID DEFINED                       
         BZ    DR5                                                              
         XC    WORK(10),WORK       CALL USERVAL TO GET ALPHA USER ID            
         MVC   WORK+8(2),TAAYTID                                                
         MVC   AIO,AIO2                                                         
         GOTO1 USERVAL,DMCB,(X'80',WORK)                                        
         MVC   AIO,AIO1                                                         
         MVC   SAYTID,TGUSERID                                                  
*                                                                               
DR5      MVC   SAYTPC,TAAYTPC      DISPLAY DEFAULT TPC (AND NAME)               
*                                                                               
         OC    TGUSER,TGUSER       IF USER ID NOT DEFINED FROM ABOVE            
         BNZ   *+10                                                             
         MVC   TGUSER,TWAORIG      USE CONNECT ID FOR USER-ID                   
*                                                                               
         MVC   AIO,AIO2            USE AIO2 FOR RECVAL CALL TO DISPLAY          
         GOTO1 RECVAL,DMCB,TLSTCDQ,(X'88',SAYTPC),SAYTPCNH  STAFF NAME          
         MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
*                                  DISPLAY BILLING DAYS DUE                     
         CLI   TAAYDAYS,X'FF'                                                   
         BNE   *+12                                                             
         MVI   SAYDAY,C'0'         DISPLAY AS ZERO DAYS DUE                     
         B     DR8                                                              
         EDIT  (1,TAAYDAYS),(2,SAYDAY),ALIGN=LEFT                               
*                                                                               
DR8      MVC   SAYLBOX,TAAYLBOX    DISPLAY LOCK BOX CODE                        
         OI    SAYLBOXH+6,X'80'                                                 
*                                                                               
         MVC   SAYAGG,TAAYAGG      DISPLAY AGENCY GROUP CODE                    
         MVC   TGAGG,TAAYAGG       SAVE IN GLOBAL STORAGE                       
*                                                                               
         OC    TAAYIAGY,TAAYIAGY   IF ELEMENT HAS AGENCY                        
         BZ    DR10                                                             
         MVC   SAYRINV(2),=C'A='   THEN DISPLAY 'A=AGENCY'                      
         MVC   SAYRINV+2(6),TAAYIAGY                                            
*                                                                               
         MVC   AIO,AIO2            READ PARENT AGENCY RECORD INTO IO2           
         MVC   SVTGAGY,TGAGY       SAVE GLOBAL AGENCY                           
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A0',SAYRINV+2)                            
         MVC   AIO,AIO1                                                         
         MVC   TGAGY,SVTGAGY       RESTORE GLOBAL AGENCY                        
         BNE   DR30                SKIP NEXT INVOICE NUMBER IF ERROR            
*                                                                               
         L     R4,AIO2             R4 = A(AGENCY ELEMENT OF PARENT REC)         
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BE    DR20                GO DISPLAY NEXT INVOICE NUMBER               
         DC    H'0'                                                             
*                                  ELSE DISPLAY RESET INVOICE NUMBER            
DR10     MVO   RESWORK,TAAYRINV                                                 
         OI    RESWORK+2,X'0C'                                                  
         UNPK  SAYRINV(4),RESWORK                                               
         OI    SAYRINV+3,X'F0'                                                  
*                                  DISPLAY NEXT INVOICE NUMBER                  
DR20     GOTO1 TINVCON,DMCB,TAAYNINV,SAYNINV,DATCON                             
         OI    SAYNINVH+6,X'80'                                                 
*                                                                               
DR30     L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ      RE-GET AGENCY'S AGENCY ELEMENT               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    TAAYSTA3,TAAYSADV   SKIP IF NOT SET FOR AUTOMATIC                
         BZ    DR35                ADVICE NUMBERS                               
*                                                                               
         OC    TAAYAAGY,TAAYAAGY   IF ELEMENT HAS NEXT ADVICE AGENCY            
         BZ    DR31                                                             
         MVC   SAYRADV(2),=C'A='   THEN DISPLAY 'A=AGENCY'                      
         MVC   SAYRADV+2(6),TAAYAAGY                                            
*                                                                               
         MVC   AIO,AIO2            READ PARENT AGENCY RECORD INTO IO2           
         MVC   SVTGAGY,TGAGY       SAVE GLOBAL AGENCY                           
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A0',SAYRADV+2)                            
         MVC   AIO,AIO1                                                         
         MVC   TGAGY,SVTGAGY       RESTORE GLOBAL AGENCY                        
         BNE   DR35                SKIP NEXT ADVICE NUMBER IF ERROR             
*                                                                               
         L     R4,AIO2             R4 = A(AGENCY ELEMENT OF PARENT REC)         
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BE    DR32                                                             
         DC    H'0'                                                             
*                                                                               
DR31     MVO   RESWORK,TAAYRADV    IF NO PARENT AGENCY, DISPLAY                 
         OI    RESWORK+2,X'0C'     RESET ADVICE NUMBER                          
         UNPK  SAYRADV(4),RESWORK                                               
         OI    SAYRADV+3,X'F0'                                                  
*                                                                               
DR32     MVC   SAYNADV,TAAYNADV    DISPLAY NEXT ADVICE NUMBER                   
         OI    SAYNADVH+6,X'80'                                                 
         DROP  R4                                                               
*                                                                               
DR35     L     R4,AIO              DISPLAY BILLING RULES ELEMENT                
         MVI   ELCODE,TABRELQ                                                   
         BAS   RE,GETEL                                                         
         B     DR37                                                             
DR36     BAS   RE,NEXTEL                                                        
DR37     BNE   DR80                                                             
         USING TABRD,R4                                                         
*                                                                               
         TM    TABRSTAT,TABRSACP   SPECIAL ACOPY BILLING TYPE                   
         BO    DR36                                                             
         MVC   STATBR,TABRSTAT     SAVE STATUS BYTE                             
         MVC   OEORBR,TABROEOR     SAVE OVERRIDE EMPLOYER OF RECORD             
*                                                                               
         BRAS  RE,DRSTAT           DISPLAY STATUS CODES (& EMP OF REC)          
*                                                                               
         MVC   SAYRECV,TABRRECV    DISPLAY RECEIVABLE ACCOUNT                   
*                                                                               
         XC    WORK(10),WORK       CALL USERVAL WITH SIGN-ON USER ID TO         
         MVC   WORK+8(2),TWAORIG       GET SE NUMBER AND HEXCOMP                
         MVC   AIO,AIO2                                                         
         GOTO1 USERVAL,DMCB,(X'80',WORK)                                        
         MVC   AIO,AIO1                                                         
*                                                                               
         CLI   TGACCHX,0           IF WE HAVE ACC HEXCOMP                       
         BE    DR50                                                             
         MVC   KEY,SPACES          BUILD RECEIVABLE ACCOUNT KEY                 
         MVC   KEY(1),TGACCHX      HEX COMPONENT                                
         MVC   KEY+1(2),=C'SR'     UNIT S, LEDGER R                             
         MVC   KEY+3(12),TABRRECV  ACCOUNT NUMBER                               
*                                                                               
*                                  DISPLAY RECEIVABLE ACCOUNT NAME              
         MVC   AIO,AIO2            USE AIO2 FOR READACC CALL                    
         GOTO1 READACC,DMCB,SAYRECNH                                            
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
*                                  DISPLAY BILLING TYPE                         
DR50     CLI   TABRTYPE,X'C0'      ALPHABETIC OR NUMERIC?                       
         BNH   *+14                                                             
         MVC   SAYTYPE,TABRTYPE                                                 
         B     DR60                                                             
         EDIT  (1,TABRTYPE),(2,SAYTYPE),ALIGN=LEFT                              
DR60     EDIT  (1,TABRHRLS),(2,SAYAGRT),ALIGN=LEFT        APPL GRT RULE         
*                                                                               
         BRAS  RE,DRRATES          DISPLAY BILLING RATES                        
*                                                                               
         CLI   SAYTPOF,C'O'        OFFICE O, ADDITIONAL BILLING RATES           
         BE    *+12                                                             
         CLI   SAYTPOF,C'Q'        OFFICE Q, ADDITIONAL BILLING RATES           
         BNE   DR80                                                             
         L     R4,AIO              DISPLAY BILLING RULES ELEMENT                
         MVI   ELCODE,TABRELQ                                                   
         BAS   RE,GETEL                                                         
         B     DR71                                                             
DR70     BAS   RE,NEXTEL                                                        
DR71     BNE   DR80                                                             
         TM    TABRSTAT,TABRSACP   SPECIAL ACOPY BILLING TYPE, OFF O&Q          
         BZ    DR70                                                             
         EDIT  (1,TABRTYPE),(2,SAYTYP2),ALIGN=LEFT                              
         OI    SAYTYP2H+6,X'80'                                                 
         BAS   RE,DRRATES2         DISPLAY ADDITIONAL RATES                     
         DROP  R4                                                               
*                                                                               
DR80     L     R4,AIO              DISPLAY GUARANTEE HANDLING ELEMENT           
         MVI   ELCODE,TAGHELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DR90                                                             
         USING TAGHD,R4                                                         
*                                                                               
         OC    TAGHHDL,TAGHHDL                                                  
         BZ    DR85                                                             
         EDIT  (2,TAGHHDL),(4,SAYGHDL),ALIGN=LEFT                               
*                                                                               
DR85     OC    TAGHLIM,TAGHLIM                                                  
         BZ    DR90                                                             
         EDIT  (3,TAGHLIM),(6,SAYGLIM),ALIGN=LEFT                               
         DROP  R4                                                               
*                                                                               
DR90     GOTO1 CHAROUT,DMCB,TACMELQ,SAYCOMMH,TACMTYPG    COMMENT                
         GOTO1 CHAROUT,DMCB,TANUELQ,SAYDJOBH,TANUTDJB    DEFAULT JOB            
                                                                                
         GOTO1 ACTVOUT,DMCB,SAYLCHGH DISPLAY LAST CHANGED ELEMENT               
         MVC   KEY,SVKEY             RESTORE KEY                                
         B     XIT                                                              
         EJECT                                                                  
* DISPLAY HOLDING FEE NOTICE.                                                   
*                                                                               
DRHLD    NTR1                                                                   
         USING TAAYD,R4                                                         
         LA    R2,SAYHLD           R2 = A(STATUS CODES FIELD DATA)              
*                                                                               
         TM    TAAYHLDS,TAAYHSNO   IF NO HOLDING FEE NOTICE REQUIRED            
         BO    DRH50               THEN SKIP                                    
*                                                                               
         CLI   TAAYHLD,0           IF NO HOLDING FEE NOTICE THEN RETURN         
         BE    DRHX                                                             
*                                                                               
         TM    TAAYHLD,X'80'       IF MONTH INDICATOR IS ON                     
         BZ    *+12                                                             
         MVI   0(R2),C'M'          THEN DISPLAY 'M'                             
         B     *+8                                                              
         MVI   0(R2),C'W'          ELSE DISPLAY 'W'                             
*                                                                               
         MVI   1(R2),C','          DISLPAY A ','                                
*                                                                               
         MVC   BYTE,TAAYHLD        DISPLAY NUMBER OF WEEKS NOTICE               
         NI    BYTE,X'7F'                                                       
         ZIC   RF,BYTE                                                          
         EDIT  (RF),(3,2(R2)),ALIGN=LEFT                                        
*                                                                               
         AR    R2,R0               POINT R2 TO LAST NON-SPACE                   
         LA    R2,1(R2)                                                         
*                                                                               
DRH50    LA    RF,HOLDTAB          RF = A(HOLDING FEE STAT CODE TABLE)          
*                                                                               
DRH60    CLI   0(RF),X'FF'         WHILE NOT END OF TABLE                       
         BE    DRHX                                                             
*                                                                               
         MVC   BYTE,0(RF)          IF BIT IS ON IN STATUS BYTE                  
         NC    BYTE,TAAYHLDS                                                    
         BZ    DRH80                                                            
*                                                                               
         TM    TAAYHLDS,TAAYHSNO   IF NO HOLDING FEE NOTICE REQUIRED            
         BO    DRH70               THEN SKIP                                    
         MVI   1(R2),C','          THEN DISPLAY COMMA AND BUMP PAST             
         LA    R2,2(R2)                                                         
*                                                                               
DRH70    MVC   0(10,R2),1(RF)      DISPLAY CODE                                 
*                                                                               
         LA    R2,9(R2)            POINT R2 TO LAST NON-SPACE                   
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
DRH80    LA    RF,L'HOLDTAB(RF)    BUMP TO NEXT STATUS CODE TABLE ENTRY         
         B     DRH60               LOOP BACK                                    
*                                                                               
DRHX     B     XIT                                                              
         EJECT                                                                  
DRRATES2 NTR1                                                                   
         USING TABRD,R4                                                         
*                                                                               
         OC    TABRRATE,TABRRATE   IF NO RATES EXIST THEN DONE                  
         BZ    DRR2X                                                            
*                                                                               
         LA    R2,SAYCCURH         R2 = A(FIRST SCREEN FIELD)                   
         LA    R3,TABRRATE         R3 = A(FIRST FIELD IN ELEMENT)               
         LA    RF,4                DISPLAY THE 1ST FOUR                         
         BRAS  RE,OUTRATE                                                       
*                                                                               
         LA    R2,SAYCWCRH         DISPLAY WC FOR CORPS                         
         LA    R3,TABRWCRP                                                      
         LA    RF,1                RESET RF                                     
         BRAS  RE,OUTRATE                                                       
*                                                                               
         LA    R2,SAYCCRPH         DISPLAY CORP                                 
         LA    R3,TABRCORP                                                      
         LA    RF,3                RESET RF                                     
         BRAS  RE,OUTRATE                                                       
*                                                                               
DRR2X    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE THE RECORD                                                           
*                                                                               
VR       NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
         MVI   TARAFLAG,0                                                       
         MVI   PROSTAT,0                                                        
         MVI   OSTATAY6,0                                                       
         MVI   OSTATAY7,0                                                       
*                                                                               
         CLI   ACTNUM,ACTADD       IF NOT ADDING                                
         BE    VR00                SAVE VITA-NOTIFYING MQ MESSAGE               
         GOTOR BLDMQMSG,DMCB,(X'80',0)     BASED ON INITIAL STATE               
                                                                                
         USING TAAYD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ      SAVE ORIGINAL AGENCY STATUS BYTE             
         BRAS  RE,GETEL            6 AND 7                                      
         BNE   VR00                                                             
         MVC   OSTATAY6,TAAYSTA6                                                
         MVC   OSTATAY7,TAAYSTA7                                                
         DROP  R4                                                               
                                                                                
VR00     BRAS  RE,VALNAMES         VALIDATE LONG NAME                           
*                                                                               
         BAS   RE,VREMAIL          VALIDATE OPTIONAL EMAIL ADDRESS              
*                                                                               
         CLI   SAYSIGNH+5,0        SIGNATORY ENTERED?                           
         BE    VR12                YES                                          
*                                                                               
*        MVC   SAYSNME,SPACES      NO, CLEAR PREVIOUS NAME                      
*        OI    SAYSNMEH+6,X'80'    TRANSMIT                                     
*        B     VR12                DELETE ELEMENT                               
*                                                                               
*                                  VALIDATE SIGNATORY                           
VR10     MVC   AIO,AIO2 SWAP IO                                                 
         MVC   SVTGAGY,TGAGY       SAVE GLOBAL AGENCY                           
         GOTO1 RECVAL,DMCB,TLAYCDQ,SAYSIGNH                                     
         MVC   TGAGY,SVTGAGY       RESTORE GLOBAL AGENCY                        
         MVC   AIO,AIO1            RESTORE IO                                   
*                                                                               
*                                  ADD/REMOVE SIGNATORY ELEMENT                 
VR12     GOTO1 NAMIN,DMCB,TANUELQ,(X'80',SAYSIGNH),TANUTSIG                     
*                                                                               
*                                  VALIDATE OPTIONAL STUDIO CODE                
         GOTO1 NAMIN,DMCB,TAFNELQ,(X'80',SAYSTUDH),TAFNTSTU                     
*                                                                               
         BRAS  RE,VRSTAT           VALIDATE STATUS CODES                        
*                                                                               
         MVI   SVBTYPE,0                                                        
         MVI   SVBHRLS,0                                                        
         XC    SVGHHDL,SVGHHDL                                                  
         XC    SVSERV,SVSERV                                                    
                                                                                
         CLI   ACTNUM,ACTADD                                                    
         JE    VR13                                                             
*                                                                               
         USING TABRD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TABRELQ      IF IT DOES, THEN CAN'T CHANGE                
         BAS   RE,GETEL            BILLING TYPE OR SERVICE                      
         B     *+8                                                              
VR12A    BAS   RE,NEXTEL                                                        
         BNE   VR12B                                                            
         TM    TABRSTAT,TABRSACP   SKIP IF ADDITIONAL RATE                      
         BO    VR12A                                                            
         MVC   SVBTYPE,TABRTYPE    SAVE ORIGINAL BILLING TYPE                   
         MVC   SVBHRLS,TABRHRLS    SAVE ORIGINAL HANDLING RULES                 
*                                                                               
         USING TARAD,R4                                                         
VR12B    L     R4,AIO                                                           
         MVI   ELCODE,TARAELQ      TEST IF BILLING RATES ELEMENT                
         BAS   RE,GETEL            EXISTS                                       
         BNE   VR13                                                             
         OI    TARAFLAG,TARAFYES                                                
*                                                                               
         USING TAAYD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVSERV,=C'PAY'                                                   
         TM    TAAYMISC,TAAYPREM   PREMIUM SERVICE AGENCY?                      
         BZ    *+10                                                             
         MVC   SVSERV,=C'PRE'                                                   
*                                                                               
         USING TAGHD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAGHELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   SVGHHDL,TAGHHDL     SAVE ORIGINAL HANDLING RATE                  
*                                                                               
         USING TAAYD,R4            VALIDATE AGENCY ELEMENT                      
VR13     MVI   ELCODE,TAAYELQ                                                   
*                                                                               
         MVI   SVMISC,0                                                         
         L     R4,AIO              IF ELEMENT EXISTS                            
         BAS   RE,GETEL                                                         
         BNE   VR15                                                             
*                                                                               
         MVC   SVNINV,TAAYNINV     THEN SAVE ITS NEXT INVOICE NUMBER            
         MVC   SVNADV,TAAYNADV           AND ITS NEXT ADVICE NUMBER             
         MVC   SVTPOFF,TAAYTPOF          AND TP OFFICE CODE                     
         MVC   SVMISC,TAAYMISC           AND MISC FLAGS                         
         B     VR17                                                             
*                                                                               
VR15     XC    SVNINV,SVNINV       ELSE BUILD NEW ONE LATER                     
         XC    SVNADV,SVNADV                                                    
         MVI   SVTPOFF,0                                                        
*                                                                               
VR17     GOTO1 REMELEM             REMOVE AGENCY ELEMENT                        
*                                                                               
         LA    R4,ELEM             BUILD AGENCY ELEMENT                         
         XC    ELEM,ELEM                                                        
         MVI   TAAYEL,TAAYELQ                                                   
         MVI   TAAYLEN,TAAYLNQ                                                  
*                                                                               
         MVC   TAAYSTAT,STATAY     SAVE STATUS BYTES                            
         MVC   TAAYSTA2,STATAY2                                                 
         MVC   TAAYSTA3,STATAY3                                                 
         MVC   TAAYSTA4,STATAY4                                                 
         MVC   TAAYSTA5,STATAY5                                                 
         MVC   TAAYSTA6,STATAY6                                                 
         TM    OSTATAY6,TAAYSREG   IF OLD REG STATUS WAS ON THEN                
         BZ    *+8                 DONT ALLOW  IT TO BE TURNED OFF              
         OI    TAAYSTA6,TAAYSREG                                                
         MVC   TAAYSTA7,STATAY7                                                 
         MVC   TAAYSGNS,STATAYSG                                                
         MVC   TAAYBUNT,SVBUNT                                                  
*                                                                               
         CLI   SAYTELH+5,0         VALIDATE TELEPHONE NUMBER (OPTIONAL)         
         BE    VR20                                                             
         MVC   TAAYTEL,SAYTEL                                                   
         OC    TAAYTEL,SPACES                                                   
*                                                                               
*                                  VALIDATE TP OFFICE CODE                      
VR20     LA    R2,SAYTPOFH                                                      
         GOTO1 RECVAL,DMCB,TLOFCDQ,(R2)                                         
         CLC   OEORBR,=C'PP '      IF EMP=PP                                    
         BNE   VR22                                                             
         CLI   TGOFF,C'0'          OFFICE MUST BE A LETTER                      
         BNL   ERROFF                                                           
         B     VR23                                                             
VR22     CLC   OEORBR,=C'TP '      IF EMP=TP                                    
         BE    *+14                                                             
         OC    OEORBR,OEORBR       OR IF NO EMPLOYER                            
         BNZ   VR23                                                             
         CLI   TGOFF,C'F'          OFFICE F IS SPECIAL                          
         BE    VR23                                                             
         CLI   TGOFF,C'O'          OFFICE O IS FOR CHLOE                        
         BE    VR23                                                             
         CLI   TGOFF,C'0'          OFFICE MUST NOT BE A LETTER                  
         BL    ERRINV                                                           
VR23     MVC   TAAYTPOF,TGOFF      SAVE IN ELEMENT                              
         TM    TAAYSTA4,TAAYSNHD   IF NO HANDLING FOR PRINT AGENT CHK           
         BZ    *+12                                                             
         CLI   TGOFF,C'0'          OFFICE MUST BE A LETTER                      
         BNL   ERRNOHND                                                         
*                                                                               
         LA    R2,SAYNBILH         VALIDATE BILLING COPIES (REQUIRED)           
         GOTO1 ANY                                                              
         GOTO1 VALINUM                                                          
         CLI   ACTUAL,1            INPUT MUST BE AT LEAST 1                     
         BL    ERRBCOPY                                                         
         MVC   TAAYNBIL,ACTUAL                                                  
*                                                                               
         BRAS  RE,VRSERV           VALIDATE SERVICE TYPE                        
*                                                                               
VR25     BAS   RE,VRHLD            VALIDATE HOLDING FEE NOTICE (OPT)            
*                                                                               
         CLI   SAYTIDH+5,0         VALIDATE TPC USER-ID (OPTIONAL)              
         BE    VR30                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 USERVAL,DMCB,SAYTIDH                                             
         MVC   AIO,AIO1                                                         
         MVC   TAAYTID,TGUSER                                                   
         B     VR32                                                             
*                                                                               
VR30     CLI   SAYTPCH+5,0         VALIDATE DEFAULT TPC (OPTIONAL)              
         BE    VR35                                                             
VR32     MVC   TGUSER,TAAYTID      SET USER-ID FROM EL.                         
         OC    TGUSER,TGUSER       IF NOT DEFINED                               
         BNZ   *+10                                                             
         MVC   TGUSER,TWAORIG      USE CONNECT ID FOR USER-ID                   
         NI    SAYTPCH+4,X'DF'     INSURE WE ALWAYS RE-VALIDATE                 
         GOTO1 RECVAL,DMCB,TLSTCDQ,SAYTPCH                                      
         MVC   TAAYTPC,TGSTAF                                                   
*                                                                               
VR35     LA    R2,SAYDAYH          VALIDATE BILLING DAYS DUE  (OPT)             
         CLI   5(R2),0             TREAT NO INPUT AS BINARY ZERO                
         BE    VR38                                                             
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'00'                                                   
         BNE   VR37                                                             
         MVI   TAAYDAYS,X'FF'      TREAT ZERO INPUT AS X'FF'                    
         B     VR38                                                             
VR37     GOTO1 VALINUM                                                          
         MVC   TAAYDAYS,ACTUAL                                                  
*                                                                               
VR38     CLI   SAYAGGH+5,0         VALIDATE AGENCY GROUP CODE (OPT)             
         BE    VR40                                                             
         GOTO1 RECVAL,DMCB,TLAGCDQ,SAYAGGH                                      
         OC    SAYAGG,SPACES                                                    
         MVC   TAAYAGG,SAYAGG                                                   
*                                                                               
VR40     LA    R2,SAYRINVH         VALIDATE RESET INVOICE NUMBER (OPT)          
*                                                                               
         CLI   5(R2),0             IF BLANK THEN DEFAULT TO 1                   
         BNE   VR45                                                             
         MVC   TAAYRINV,=X'0001'                                                
         B     VR60                                                             
*                                                                               
VR45     OC    SAYRINV,=CL8' '     ELSE IF FIELD STARTS WITH 'A=' THEN          
         CLC   SAYRINV(2),=C'A='       VALIDATE AGENCY INSTEAD                  
         BNE   VR50                                                             
         MVC   SVTGAGY,TGAGY       SAVE GLOBAL AGENCY                           
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'80',SAYRINV+2)                            
         MVC   TGAGY,SVTGAGY       RESTORE GLOBAL AGENCY                        
         BNE   ERREXIT                                                          
         MVC   TAAYIAGY,SAYRINV+2                                               
         B     VR60                                                             
*                                                                               
VR50     TM    4(R2),X'08'         ELSE VALIDATE INVOICE NUMBER                 
         BZ    ERRINV                                                           
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         MP    DUB,=P'10'                                                       
         MVC   TAAYRINV,DUB+5                                                   
*                                                                               
         OC    TAAYRINV,TAAYRINV   '0000' NOT VALID                             
         BZ    ERRINV                                                           
*                                                                               
VR60     LA    R2,SAYRADVH         VALIDATE RESET ADVICE NUMBER (OPT)           
*                                                                               
         CLI   5(R2),0             IF BLANK                                     
         BNE   VR61                                                             
         TM    TAAYSTA3,TAAYSADV   AND AGENCY NOT SET FOR AUTOMATIC             
         BZ    VR64                ADVICE NUMBERS, SKIP                         
         MVC   TAAYRADV,=X'0000'   OTHERWISE, DEFAULT TO 0000                   
         B     VR64                                                             
*                                                                               
VR61     TM    TAAYSTA3,TAAYSADV   IF NOT BLANK, AGENCY MUST BE SET             
         BZ    ERRINV              FOR AUTOMATIC ADVICE NUMBERS                 
*                                                                               
         OC    SAYRADV,=CL8' '     IF FIELD STARTS WITH 'A=' THEN               
         CLC   SAYRADV(2),=C'A='       VALIDATE AGENCY INSTEAD                  
         BNE   VR62                                                             
         MVC   SVTGAGY,TGAGY       SAVE GLOBAL AGENCY                           
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'80',SAYRADV+2)                            
         MVC   TGAGY,SVTGAGY       RESTORE GLOBAL AGENCY                        
         BNE   ERREXIT                                                          
         MVC   TAAYAAGY,SAYRADV+2                                               
         B     VR64                                                             
*                                                                               
VR62     TM    4(R2),X'08'         ELSE VALIDATE ADVICE NUMBER                  
         BZ    ERRINV                                                           
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         MP    DUB,=P'10'                                                       
         MVC   TAAYRADV,DUB+5                                                   
*                                                                               
         CLC   TAAYRADV,=X'9500'   > 9500 NOT VALID                             
         BH    ERRINV                                                           
*                                                                               
VR64     MVC   TAAYNINV,SVNINV     INSERT NEXT INVOICE NUMBER                   
*                                                                               
         OC    TAAYNINV,TAAYNINV   IF NEXT INVOICE NEEDS TO BE BUILT            
         BNZ   VR65                                                             
         PACK  DUB,TGTODAY0+2(2)   THEN BUILD NEW NEXT INVOICE NUMBER           
         CVB   RF,DUB                                                           
         LA    RE,=C' ABCDEFGHIJKL'                                             
         LA    RF,0(RE,RF)                                                      
         MVC   WORK(1),0(RF)       GET THE MONTH                                
*                                                                               
         MVC   WORK+1(1),TGTODAY0+1     THE YEAR                                
*                                                                               
         MVO   RESWORK,TAAYRINV    USE RESET INVOICE NUMBER                     
         OI    RESWORK+2,X'0C'                                                  
         UNPK  WORK+2(4),RESWORK                                                
         OI    WORK+5,X'F0'                                                     
*                                                                               
         GOTO1 TINVCON,DMCB,WORK,TAAYNINV,DATCON                                
*                                                                               
VR65     MVC   TAAYNADV,SVNADV     INSERT NEXT ADVICE NUMBER                    
         LA    R2,SAYLBOXH         R2=A(LOCK BOX CODE)                          
         CLI   5(R2),0             IF LOCK BOX CODE                             
         BE    VR70                                                             
         LA    RE,LBOXTAB          RE=A(LOCK BOX CODE TABLE)                    
VR68     CLI   0(RE),X'FF'         TEST END OF TABLE                            
         BE    ERRINV                                                           
         CLC   8(1,R2),0(RE)       TEST FOR MATCH                               
         BE    *+12                                                             
         LA    RE,L'LBOXTAB(RE)                                                 
         B     VR68                                                             
         MVC   TAAYLBOX,8(R2)      SET CODE IN ELEMENT                          
*                                                                               
VR70     TM    TAAYSTAT,TAAYSEST   IF EST STATUS IS ON                          
         JZ    *+8                                                              
         OI    PROSTAT,PSDJNALL                                                 
         TM    TAAYSTA6,TAAYST10   OR TYPE 10 STATUS IS ON                      
         JZ    *+8                                                              
         OI    PROSTAT,PSDJNALL                                                 
         OC    TAAYBUNT,TAAYBUNT   OR BUSINESS UNIT IS PROVIDED                 
         JZ    *+8                                                              
         OI    PROSTAT,PSDJNALL                                                 
         TM    TAAYSTA7,TAAYSBBD   OR BBDO JOB VALIDATION IS ON                 
         JZ    *+8                                                              
         OI    PROSTAT,PSDJNALL    SET DEFAULT JOB NOT ALLOWED                  
                                                                                
         GOTO1 ADDELEM             ADD AGENCY ELEMENT                           
         DROP  R4                                                               
*                                                                               
         BRAS  RE,VRFILT           VALIDATE FILTERS                             
*                                                                               
         XR    R3,R3               DISPLACEMENT INTO FIELD                      
         LA    R2,SAYJOBH          VALIDATE OPTIONAL ESTIMATE SETUP             
         CLI   5(R2),0             IF INPUT                                     
         BE    VR72                                                             
         TM    STATBR,TABRSINT     CAN'T BE ON PRODUCTION INTERFACE             
         BO    ERRFLD                                                           
         TM    STATAY6,TAAYST10    OR TYPE 10 VALIDATION                        
         BO    ERRFLD                                                           
         CLI   5(R2),3             MUST BE 3 CHARACTERS LONG                    
         BNE   ERRFLD                                                           
         CLI   8(R2),C'1'          CLIENT LENGTH MUST BE BETWEEN 1-5            
         BL    ERRFLD                                                           
         CLI   8(R2),C'5'                                                       
         BNH   VR71                                                             
         TM    STATAY5,TAAYBUSU    AGENCY USING BUSINESS UNIT                   
         BZ    ERRFLD                                                           
         CLI   8(R2),C'6'          SO CLIENT LENGTH CAN BE 6                    
         BH    ERRFLD                                                           
VR71     AHI   R3,1                                                             
         CLI   9(R2),C'1'          PRODUCT LENGTH MUST BE BETWEEN 1-5           
         BL    ERRFLD                                                           
         CLI   9(R2),C'5'                                                       
         BH    ERRFLD                                                           
         AHI   R3,1                                                             
         CLI   10(R2),C'1'         JOB LENGTH MUST BE BETWEEN 1-6               
         BL    ERRFLD                                                           
         CLI   10(R2),C'6'                                                      
         BH    ERRFLD                                                           
VR72     GOTO1 NAMIN,DMCB,TANUELQ,(X'80',(R2)),TANUTJOB                         
*                                                                               
         MVI   ELCODE,TABRELQ      REMOVE BILLING RULES ELEMENT                 
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R4,ELEM             BUILD BILLING RULES ELEMENT                  
         USING TABRD,R4                                                         
         XC    ELEM,ELEM                                                        
         MVI   TABREL,TABRELQ                                                   
         MVI   TABRLEN,TABRLNQ                                                  
*                                                                               
         MVC   TABRSTAT,STATBR     SAVE STATUS BYTE                             
         TM    TABRSTAT,TABRSINT   IF INT STATUS IS ON                          
         JZ    *+8                                                              
         OI    PROSTAT,PSDJNALL    SET DEFAULT JOB NOT ALLOWED                  
         MVC   TABROEOR,OEORBR     SAVE OVERRIDE EMPLOYER OF RECORD             
*                                                                               
         LA    R2,SAYRECVH         VALIDATE RECEIVABLE ACCOUNT                  
*----------------------------------------------------------------------         
* remove validation - john bassett (2/26/2018)                                  
*        GOTO1 ANY                                                              
         CLI   SAYRECVH+5,0                                                     
         BE    VR74A                                                            
         GOTO1 ANY                                                              
*                                                                               
         CLC   WORK(1),TGOFF       FIRST BYTE MUST BE SAME AS TP OFFICE         
         BE    VR74                                                             
         CLI   TGOFF,C'2'          IF TP OFFICE IS 2                            
         BNE   ERRINV                                                           
         CLI   WORK,C'6'           THEN ALLOW 6                                 
         BNE   ERRINV                                                           
VR74     MVC   TABRRECV,WORK       SAVE IN ELEMENT                              
*                                                                               
         XC    WORK(10),WORK       CALL USERVAL WITH SIGN-ON USER ID TO         
         MVC   WORK+8(2),TWAORIG       GET SE NUMBER AND HEXCOMP                
         MVC   AIO,AIO2                                                         
         GOTO1 USERVAL,DMCB,(X'80',WORK)                                        
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   KEY,SPACES          BUILD RECEIVABLE ACCOUNT KEY                 
         MVC   KEY(1),TGACCHX      HEX COMPONENT                                
         MVC   KEY+1(2),=C'SR'     UNIT S, LEDGER R                             
         MVC   KEY+3(12),TABRRECV  ACCOUNT NUMBER                               
*                                                                               
         CLC   TWAAGY,=C'D3'       VALIDATE ACCOUNT RECORD ON ACCFILE           
         BE    VR74A               (SKIP THIS STEP IF ON FQA)                   
         CLC   TWAAGY,=C'DP'                                                    
         BE    VR74A                                                            
         MVC   AIO,AIO2            USE AIO2 FOR READS                           
         GOTO1 READACC,DMCB,SAYRECNH                                            
         MVC   AIO,AIO1            RESTORE AIO                                  
         BNE   ERREXIT             ERROR IF RECORD NOT FOUND                    
*                                                                               
VR74A    LA    R2,SAYTYPEH         VALIDATE BILLING TYPE                        
         MVC   TABRTYPE,SAYTYPE                                                 
         CLC   SAYAGG,=CL6'EMS'    AGROUP EMS MUST USE TYPE E                   
         BNE   *+16                TYPE E ONLY VALID FOR AGROUP EMS             
         CLI   SAYTYPE,TABRTYE     MUST BE EITHER C'E' OR NUMERIC               
         BNE   ERRINV                                                           
         B     VR75                                                             
         GOTO1 VALINUM                                                          
         MVC   TABRTYPE,ACTUAL                                                  
VR75     GOTO1 BTYPVAL,DMCB,TABRTYPE                                            
         BNE   ERRINV                                                           
*                                                                               
         CLC   SVBTYPE,TABRTYPE    IF BILLING TYPE IS CHANGING                  
         JE    VR80                                                             
         TM    TGBTSTAT,BTYPSDFT   CAN'T CHANGE TO DEFUNCT BILLING TYPE         
         JO    ERRINV                                                           
         TM    TARAFLAG,TARAFYES   CAN'T CHANGE IF AGY HAS BRATE                
         JO    ERRCHGBR                                                         
*                                                                               
VR80     LA    R2,SAYAGRTH         VALIDATE APPLIED GRT HANDLING RULE           
         CLI   SAYAGRTH+5,0                                                     
         BE    VR90                                                             
         GOTO1 VALINUM                                                          
         MVC   TABRHRLS,ACTUAL                                                  
         TM    TARAFLAG,TARAFYES   CAN'T CHANGE IF AGY HAS BRATE                
         JZ    VR90                                                             
         CLC   SVBHRLS,ACTUAL                                                   
         JE    VR90                                                             
         EDIT  (1,SVBHRLS),(2,SAYAGRT),ALIGN=LEFT                               
         OI    SAYAGRTH+6,X'80'                                                 
         J     ERRCHGBR                                                         
*                                                                               
VR90     XC    SVHDL,SVHDL                                                      
         LA    R2,SAYGHDLH                                                      
         CLI   SAYGHDLH+5,0        VALIDATE GUARANTEE HANDLING RATE             
         BE    VR95                                                             
         LA    R2,SAYGHDLH                                                      
         TM    4(R2),X'08'                                                      
         BZ    ERRINV                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,SVHDL                                                       
*                                                                               
         TM    TARAFLAG,TARAFYES   CAN'T CHANGE IF AGY HAS BRATE                
         JZ    VR95                                                             
         CLC   SVGHHDL,SVHDL                                                    
         JE    VR95                                                             
         EDIT  (2,SVGHHDL),(4,SAYGHDL),ALIGN=LEFT                               
         OI    SAYGHDLH+6,X'80'                                                 
         J     ERRCHGBR                                                         
*                                                                               
VR95     XC    SVLIM,SVLIM                                                      
         CLI   SAYGLIMH+5,0        VALIDATE GUARANTEE LIMIT                     
         BE    VR100                                                            
         LA    R2,SAYGLIMH                                                      
         TM    4(R2),X'08'                                                      
         BZ    ERRINV                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         STCM  R1,7,SVLIM                                                       
*                                                                               
VR100    BAS   RE,VRRATES          VALIDATE BILLING RATES                       
         GOTO1 ADDELEM             ADD BILLING RULES ELEMENT                    
*                                                                               
         CLI   SAYTPOF,C'O'        OFFICE O, ADDITIONAL BILLING RATES           
         BE    *+12                                                             
         CLI   SAYTPOF,C'Q'        OFFICE Q, ADDITIONAL BILLING RATES           
         BNE   VR150                                                            
         LA    R2,SAYTYP2H         VALIDATE BILLING TYPE                        
         MVC   TABRTYPE,SAYTYP2                                                 
         CLC   SAYAGG,=CL6'EMS'    AGROUP EMS MUST USE TYPE E                   
         BNE   *+16                TYPE E ONLY VALID FOR AGROUP EMS             
         CLI   SAYTYP2,TABRTYE     MUST BE EITHER C'E' OR NUMERIC               
         BNE   ERRINV                                                           
         B     VR130                                                            
         GOTO1 VALINUM                                                          
         MVC   TABRTYPE,ACTUAL                                                  
VR130    GOTO1 BTYPVAL,DMCB,TABRTYPE                                            
         BNE   ERRINV                                                           
*                                                                               
         OI    TABRSTAT,TABRSACP   SPECIAL ACOPY BILLING TYPE                   
         XC    TABRRATE,TABRRATE                                                
         BAS   RE,VRRATES2         VALIDATE BILLING RATES                       
         GOTO1 ADDELEM             ADD BILLING RULES ELEMENT                    
         DROP  R4                                                               
*                                                                               
VR150    MVI   ELCODE,TAGHELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
         OC    SVHDL(L'SVHDL+L'SVLIM),SVHDL                                     
         BZ    VR210                                                            
*                                                                               
         USING TAGHD,R4                                                         
         LA    R4,ELEMENT          BUILD GUARANTEE HANDLING ELEMENT             
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAGHEL,TAGHELQ                                                   
         MVI   TAGHLEN,TAGHLNQ                                                  
         MVC   TAGHHDL,SVHDL                                                    
         MVC   TAGHLIM,SVLIM                                                    
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
*                                                                               
VR210    DS    0H                                                               
*                                                                               
*                                   VALIDATE OPTIONAL COMMENT                   
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',SAYCOMMH),TACMTYPG                     
*                                                                               
         CLI   SAYDJOBH+5,0         IF DEFAULT JOB IS PROVIDED                  
         JE    VR220                VALIDATE AND SAVE IT                        
         LA    R2,SAYDJOBH                                                      
         TM    PROSTAT,PSDJNALL                                                 
         JO    ERRINV                                                           
         GOTOR VALPJOB,DMCB,SAYDJOB                                             
         JNE   ERRINV                                                           
VR220    GOTO1 NAMIN,DMCB,TANUELQ,(X'80',SAYDJOBH),TANUTDJB                     
*                                                                               
         GOTO1 ACTVIN,DMCB,SAYLCHGH UPDATE LAST CHANGED ELEMENT                 
*                                                                               
VRX      MVC   KEY,SVKEY           RESTORE KEY                                  
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* VALIDATE REQUIRED EMAIL ADDRESS                                               
*                                                                               
VREMAIL  NTR1                                                                   
         LA    R2,SAYEMAIH                                                      
         CLI   5(R2),0                                                          
         BE    ERRMISS             EMAIL ADDRESS REQUIRED                       
         LA    RE,35                                                            
         LA    RF,SAYEMAI                                                       
VREM20   CLI   0(RF),C'@'          THERE MUST BE A '@'                          
         BE    VREM30                                                           
         AHI   RF,1                                                             
         BCT   RE,VREM20                                                        
         B     ERRINV                                                           
*                                                                               
VREM30   CLI   0(RF),C'.'          THERE MUST BE A '.'                          
         BE    VREM40                                                           
         AHI   RF,1                                                             
         BCT   RE,VREM30                                                        
         B     ERRINV                                                           
*                                                                               
VREM40   GOTO1 NAMIN,DMCB,TACMELQ,(X'40',SAYEMAIH),TACMTYPI                     
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* VALIDATE HOLDING FEE NOTICE FIELD.                                            
*                                                                               
VRHLD    NTR1                                                                   
         USING TAAYD,R4                                                         
         LA    R2,SAYHLDH          R2 = A(FIELD HEADER)                         
         GOTO1 ANY                                                              
*                                  SCAN FIELD INTO SCANNER BLOCK                
         GOTO1 SCANNER,DMCB,(R2),BLOCK                                          
*                                                                               
         LA    R5,BLOCK            R5 = A(SCANNER BLOCK)                        
         USING SCAND,R5                                                         
         ZIC   R0,4(R1)            R0 = NUMBER OF SCAN BLOCK ENTRIES            
         SR    R3,R3               R3 = DISPLACEMMENT INTO FIELD                
*                                                                               
VRH10    CLC   SCDATA1(6),NOHFN+1  TEST NO NOTICES REQUIRED                     
         BE    VRH50                                                            
         CLC   SCDATA1(6),NC+1     OR IF TESTING PAPER/ELECTRONIC STAT          
         BE    VRH50                                                            
         CLC   SCDATA1(6),EL+1                                                  
         BE    VRH50                                                            
         CLC   SCDATA1(6),PE+1                                                  
         BE    VRH50                                                            
         CLI   4(R1),2             ELSE MUST BE AT LEAST 2 FIELDS               
         BL    ERRINV                                                           
*                                                                               
         ZIC   RF,4(R1)            FIRST FIELD MUST BE 'M' OF 'W'               
         CR    R0,RF                                                            
         BNE   VRH30                                                            
*                                                                               
         CLI   SCLEN2,0            AND RHS MUST BE EMPTY                        
         BNE   ERRFLD                                                           
*                                                                               
         CLI   SCLEN1,1            ERROR IF LHS MORE THAN 1 CHAR                
         BNE   ERRFLD                                                           
*                                                                               
         CLI   SCDATA1,C'M'        IF LHS IS 'M'                                
         BNE   VRH20                                                            
         MVI   TAAYHLD,X'80'       THEN INDICATE MOTHLY NOTICE IN REC           
         B     VRH90                                                            
*                                                                               
VRH20    CLI   SCDATA1,C'W'        ELSE LHS MUST BE 'W'                         
         BNE   ERRFLD                                                           
         B     VRH90                                                            
*                                                                               
VRH30    BCTR  RF,0                SECOND FIELD MUST BE NUMBER OF WEEKS         
         CR    R0,RF                                                            
         BNE   VRH50                                                            
*                                                                               
         CLI   SCLEN2,0            RHS MUST BE EMPTY                            
         BNE   ERRFLD                                                           
*                                                                               
         CLI   SCLEN1,0            LHS CAN BE EMPTY                             
         BE    VRH90                                                            
         TM    SCVAL1,X'80'        LHS MUST BE NUMERIC                          
         BZ    ERRFLD                                                           
*        OC    SCBIN1,SCBIN1       LHS MUST BE NUMERIC                          
*        BZ    ERRFLD                                                           
*                                                                               
VRH40    CLC   SCBIN1,=F'127'      NUMBER CAN'T BE GREATER THAN 127             
         BH    ERRFLD                                                           
*                                                                               
         OC    TAAYHLD,SCBIN1+3    PUT AMNT IN LOWER 7 BITS OF TAAYHLD          
         B     VRH90                                                            
*                                                                               
VRH50    CLI   SCLEN2,0            ERROR IF RHS EXISTS                          
         BNE   ERRFLD                                                           
         LA    RF,HOLDTAB          LOOK IN STATUS TABLE FOR MATCH               
*                                                                               
VRH60    CLI   0(RF),X'FF'         ERROR IF END OF TABLE FOUND                  
         BE    ERRFLD                                                           
*                                                                               
         ZIC   RE,SCLEN1           IF MATCH THEN SAVE BIT                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SCDATA1(0),1(RF)                                                 
         BE    VRH70                                                            
*                                                                               
         LA    RF,L'HOLDTAB(RF)    ELSE TRY NEXT TABLE ENTRY                    
         B     VRH60                                                            
*                                                                               
VRH70    LA    RE,NC               CANNOT HAVE MORE THAN ONE                    
         CR    RF,RE               ELECTRONIC/PRINTING OPTION                   
         BL    VRH80                                                            
         TM    TAAYHLDS,TAAYHSNO                                                
         BO    HFPENA                                                           
         TM    TAAYHLDS,TAAYHSPO+TAAYHSEL+TAAYHSPE+TAAYHSNC                     
         BNZ   HFMTOS                                                           
*                                                                               
VRH80    OC    TAAYHLDS,0(RF)      SET APPROPRIATE BIT IN APPR BYTE             
*                                                                               
*        TM    TAAYHLDS,TAAYHSEL+TAAYHSPE                                       
*        BNZ   ERRFLD              TEMPORARILY DISALLOW PE AND EL               
*                                                                               
VRH90    ZIC   RF,SCLEN1           BUMP R3 TO NEXT STATUS CODE                  
         LA    R3,1(R3,RF)                                                      
*                                                                               
         LA    R5,SCANNEXT         BUMP R5 TO NEXT SCANNER ENTRY                
*                                                                               
         BCT   R0,VRH10            REPEAT FOR EACH SCANNER ENTRY                
         DROP  R5                                                               
*                                                                               
         TM    TAAYHLDS,TAAYHSNO                                                
         BO    VRHX                                                             
         TM    TAAYHLDS,TAAYHSPO+TAAYHSEL+TAAYHSPE+TAAYHSNC                     
         BZ    HFPENS                                                           
VRHX     B     XIT                                                              
         EJECT                                                                  
* VALIDATE BILLING RATES AND UPDATE BILLING RULES ELEMENT.                      
*                                                                               
VRRATES  NTR1                                                                   
         USING TABRD,R4                                                         
*                                                                               
         CLI   TABRTYPE,TABRTY99   IF BILLING TYPE 99                           
         BE    VRRX                                                             
         TM    TABRSTAT,TABRSSRC   OR STANDARD RATE CARD IS USED                
         BO    VRRX                THEN DONE                                    
*                                                                               
         LA    R0,4                                                             
         LA    R5,TABRRATE         R5=A(FIELD IN ELEMENT)                       
         LA    R2,SAYRCURH         VALIDATE BILLING RATES                       
*                                                                               
VRR10    CLI   5(R2),0                                                          
         BE    VRR20                                                            
         BAS   RE,GETRATE                                                       
*                                                                               
VRR20    BAS   RE,BUMP             BUMP TO NEXT FIELD                           
         L     R5,MYFULL                                                        
         L     R2,FULL                                                          
         BCT   R0,VRR10            AND CONTINUE                                 
*                                                                               
         LA    R0,3                                                             
         LA    R5,TABRCORP         R5=A(FIELD IN ELEMENT)                       
         LA    R2,SAYRCRPH         VALIDATE BILLING RATES                       
*                                                                               
VRR30    CLI   5(R2),0                                                          
         BE    VRR40                                                            
         BAS   RE,GETRATE                                                       
*                                                                               
VRR40    BAS   RE,BUMP             BUMP TO NEXT FIELD                           
         L     R5,MYFULL                                                        
         L     R2,FULL                                                          
         BCT   R0,VRR30            AND CONTINUE                                 
*                                                                               
         LA    R2,SAYRCANH         CANADIAN HANDLING                            
         CLI   TABRTYPE,TABRTY24   BILLING TYPE 24                              
         BNE   *+14                                                             
         OC    TABRCAN,TABRCAN                                                  
         BNZ   ERR24CAN                                                         
*                                                                               
         LA    R5,TABRWCRP         VALIDATE WC FOR CORPS                        
         LA    R2,SAYRWCRH                                                      
         CLI   5(R2),0                                                          
         BE    VRRX                                                             
         BAS   RE,GETRATE                                                       
*                                                                               
VRRX     B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE ADDITIONAL BILLING RATES AND UPDATE BILLING RULES ELEMENT.           
*                                                                               
VRRATES2 NTR1                                                                   
         USING TABRD,R4                                                         
*                                                                               
         LA    R0,4                                                             
         LA    R5,TABRRATE         R5=A(FIELD IN ELEMENT)                       
         LA    R2,SAYCCURH         VALIDATE BILLING RATES                       
*                                                                               
VRR210   CLI   5(R2),0                                                          
         BE    VRR220                                                           
         BAS   RE,GETRATE                                                       
*                                                                               
VRR220   BAS   RE,BUMP             BUMP TO NEXT FIELD                           
         L     R5,MYFULL                                                        
         L     R2,FULL                                                          
         BCT   R0,VRR210           AND CONTINUE                                 
*                                                                               
         LA    R0,3                                                             
         LA    R5,TABRCORP         R5=A(FIELD IN ELEMENT)                       
         LA    R2,SAYCCRPH         VALIDATE BILLING RATES                       
*                                                                               
VRR230   CLI   5(R2),0                                                          
         BE    VRR240                                                           
         BAS   RE,GETRATE                                                       
*                                                                               
VRR240   BAS   RE,BUMP             BUMP TO NEXT FIELD                           
         L     R5,MYFULL                                                        
         L     R2,FULL                                                          
         BCT   R0,VRR230           AND CONTINUE                                 
*                                                                               
         LA    R2,SAYCCANH         CANADIAN HANDLING                            
         CLI   TABRTYPE,TABRTY24   BILLING TYPE 24                              
         BNE   *+14                                                             
         OC    TABRCAN,TABRCAN                                                  
         BNZ   ERR24CAN                                                         
*                                                                               
         LA    R5,TABRWCRP         VALIDATE WC FOR CORPS                        
         LA    R2,SAYCWCRH                                                      
         CLI   5(R2),0                                                          
         BE    VRR2X                                                            
         BAS   RE,GETRATE                                                       
*                                                                               
VRR2X    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        ROUTINE GETS AMOUNTS FROM FIELDS                                       
*        R2 - FIELD WITH AMOUNT IN IT                                           
*        R5 - A(FIELD IN TABR TO INSERT INTO)                                   
*                                                                               
GETRATE  NTR1                                                                   
         ZIC   RF,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(RF) VALIDATE RATE XX.XX%                     
         CLI   0(R1),0                                                          
         BNE   ERRINV                                                           
         CLC   4(4,R1),=F'5000'    DISALLOW GT 50% (ALSO NEG. RATES)            
         BH    ERRINV                                                           
         MVC   0(2,R5),6(R1)       SAVE 2 BYTES                                 
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
*        ROUTINE BUMPS TO NEXT  FIELDS                                          
*        R5 - A(FIELD IN TABR TO INSERT INTO)                                   
*                                                                               
BUMP     NTR1                                                                   
         LA    R5,2(R5)            BUMP TO NEXT FIELD IN ELEMENT                
         XR    RF,RF                                                            
         IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         TM    1(R2),X'20'         BUMP TO NEXT UNPROTECTED FIELD               
         BO    *-10                                                             
         ST    R2,FULL                                                          
         ST    R5,MYFULL                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS                                                           
*                                                                               
ERRINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     ERREXIT                                                          
*                                                                               
ERRMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     ERREXIT                                                          
*                                                                               
ERRRNF   MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     ERREXIT                                                          
*                                                                               
ERRDEL   MVI   ERROR,ERINVDEL      CANNOT DELETE RECORD                         
         B     ERREXIT                                                          
*                                                                               
ERROFF   MVI   ERROR,ERINVOFF      INVALID PRINT OFFICE CODE                    
         B     ERREXIT                                                          
*                                                                               
ERRNOHND MVC   MYMSGNO,=Y(ERNOHND) MUST BE PRINT AGENY FOR NOHNDPAGT            
         B     TWOBYTE                                                          
*                                                                               
ERRPO    MVC   MYMSGNO,=Y(ERCPOPO)  MUST REQUIRE PO TO USE CLOSEPO              
         B     TWOBYTE                                                          
*                                                                               
ERREMP   MVC   MYMSGNO,=Y(ERCPOEMP) EMPLOYER MUST BE PP FOR CLOSEPO             
         B     TWOBYTE                                                          
*                                                                               
ERRINT   MVC   MYMSGNO,=Y(ERCPOINT) MUST INTERFACE W/PROD FOR CLOSEPO           
         B     TWOBYTE                                                          
*                                                                               
ERRPGEMP MVC   MYMSGNO,=Y(ERPGINV)  EMPLOYER PG NO LONGER EXISTS                
         B     TWOBYTE                                                          
*                                                                               
HFMTOS   MVC   MYMSGNO,=Y(ERHFMTOS) CANNOT SET MORE THAN 1 PA/EL STATUS         
         B     TWOBYTE                                                          
*                                                                               
HFPENA   MVC   MYMSGNO,=Y(ERHFPENA) PAPER/ELECTRONIC STATUS NOT ALLOWED         
         B     TWOBYTE                                                          
*                                                                               
HFPENS   MVC   MYMSGNO,=Y(ERHFPENS) PAPER/ELECTRONIC STATUS NOT SET             
         B     TWOBYTE                                                          
*                                                                               
ERR24CAN MVC   MYMSGNO,=Y(ERMUS501)  CANADIAN HANDLING IS 0 FOR BILL 24         
         J     TWOBYTE                                                          
*                                                                               
ERRCHGBR MVC   MYMSGNO,=Y(ERRBRATE)  CANT CHANGE BRATE EXISTS                   
         J     TWOBYTE                                                          
*                                                                               
ERRBCOPY MVC   MYMSGNO,=Y(ERBCOPY) MUST HAVE AT LEAST 2 BILLING COPIES          
         OI    GENSTAT2,USGETTXT                                                
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         B     ERREXIT                                                          
*                                                                               
ERRFLD   MVI   ERROR,INVALID       INVALID WITH DISP INTO FIELD                 
         STC   R3,ERRDISP                                                       
         B     ERREXIT                                                          
*                                                                               
ERRMFLD  MVI   ERROR,MISSING       MISSING WITH DISP INTO FIELD                 
         STC   R3,ERRDISP                                                       
         B     ERREXIT                                                          
*                                                                               
ERRSIGR  MVC   MYMSGNO,=Y(ERSIGRPR)  SIGNATORY RATE ENTERED PREVIOUSLY          
         STC   R3,ERRDISP                                                       
         J     TWOBYTE                                                          
*                                                                               
ERRSIGC  MVC   MYMSGNO,=Y(ERSIGCPR)  SIGNATORY CAP ENTERED PREVIOUSLY           
         STC   R3,ERRDISP                                                       
         J     TWOBYTE                                                          
*                                                                               
COMPERR  MVI   ERROR,ERCOMPT       INCOMPATIBLE WITH PREV INPUT                 
         STC   R3,ERRDISP                                                       
         B     ERREXIT                                                          
*                                                                               
DOADDED  MVI   MYMSGNO1,98         SET ADVICE OFFICE CHANGE REQUESTED           
         LA    R2,CONRECH                                                       
         B     INFEND                                                           
*                                                                               
TWOBYTE  MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
*                                                                               
INFEND   OI    GENSTAT2,USGETTXT   SET INFO MESSAGE                             
ERREXIT  GOTO1 EXIT,DMCB,0                                                      
         EJECT                                                                  
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
MQMSGLNQ EQU   750                                                              
WORKLNQ  EQU   MQMSGLNQ                                                         
         SPACE                                                                  
DVCARD   DC    CL80'DOXXOAGENCY 0202OF 0301R 0408STAFF-ID 0503DDS 0607OX        
               AGENCY*'                                                         
*                                                                               
PFTAB    DS    0X                   TABLE OF PFKEY ACTIONS                      
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'AGY',CL8'LIST'                                        
PF13X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3' ',CL8'CLIENT',CL8'LIST'                                     
PF14     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
PF14X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF15X-*,15,0,(PF15X-PF15)/KEYLNQ,0)                          
         DC    CL3' ',CL8'AGROUP',CL8'DIS'                                      
PF15     DC    AL1(KEYTYGLB,L'TGAGG-1),AL2(TGAGG-TGD)                           
PF15X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF16X-*,16,0,0,0)                                            
         DC    CL3' ',CL8'INTF',CL8'LIST'                                       
PF16X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF17X-*,17,0,0,0)                                            
         DC    CL3' ',CL8'ATTN',CL8'LIST'                                       
PF17X    EQU   *                                                                
         SPACE                                                                  
         DC    X'FF'                                                            
*                                                                               
STATTAB  DS    0CL18               TABLE OF STATUS CODES AND THEIR BITS         
         DC    AL1(TAAYSCOD,0,0,0,0,0,0,0),CL10'PUR'                            
         DC    AL1(TAAYSAPO,0,0,0,0,0,0,0),CL10'PO'                             
         DC    AL1(TAAYS13W,0,0,0,0,0,0,0),CL10'13W'                            
*        DC    AL1(TAAYSCTX,0,0,0,0,0,0,0),CL10'CATAX'                          
*        DC    AL1(TAAYSATX,0,0,0,0,0,0,0),CL10'ACATAX'                         
         DC    AL1(TAAYSNFT,0,0,0,0,0,0,0),CL10'NOFTRK'                         
         DC    AL1(TAAYSEST,0,0,0,0,0,0,0),CL10'EST'                            
         DC    AL1(TAAYSPAC,0,0,0,0,0,0,0),CL10'PRAC'                           
         DC    AL1(0,TAAYSNDF,0,0,0,0,0,0),CL10'NODEF'                          
         DC    AL1(0,TAAYSTRK,0,0,0,0,0,0),CL10'TRK'                            
         DC    AL1(0,TAAYSRET,0,0,0,0,0,0),CL10'RETRO'                          
         DC    AL1(0,TAAYSHND,0,0,0,0,0,0),CL10'HAND'                           
         DC    AL1(0,TAAYSPCH,0,0,0,0,0,0),CL10'PCH'                            
         DC    AL1(0,TAAYSOVN,0,0,0,0,0,0),CL10'OV'                             
         DC    AL1(0,TAAYSMAL,0,0,0,0,0,0),CL10'MAIL'                           
         DC    AL1(0,TAAYSCHI,0,0,0,0,0,0),CL10'CHI'                            
         DC    AL1(0,0,TAAYSLCK,0,0,0,0,0),CL10'LOCK'                           
         DC    AL1(0,0,TAAYSPRI,0,0,0,0,0),CL10'PRMAIL'                         
         DC    AL1(0,0,TAAYSRES,0,0,0,0,0),CL10'SOAPRES'                        
         DC    AL1(0,0,TAAYS30M,0,0,0,0,0),CL10'30M'                            
         DC    AL1(0,0,TAAYSNSD,0,0,0,0,0),CL10'NSA'                            
         DC    AL1(0,0,TAAYSNCS,0,0,0,0,0),CL10'NCA'                            
         DC    AL1(0,0,TAAYSPNX,0,0,0,0,0),CL10'PRNX'                           
         DC    AL1(0,0,TAAYSADV,0,0,0,0,0),CL10'ADV'                            
         DC    AL1(0,0,0,TAAYSNGT,0,0,0,0),CL10'NOGTRK'                         
         DC    AL1(0,0,0,TAAYSCPO,0,0,0,0),CL10'CLOSEPO'                        
         DC    AL1(0,0,0,TAAYSLN,0,0,0,0),CL10'LN'                              
         DC    AL1(0,0,0,TAAYSNHD,0,0,0,0),CL10'NOHNDPAGT'                      
         DC    AL1(0,0,0,TAAYINTC,0,0,0,0),CL10'CIN'                            
         DC    AL1(0,0,0,TAAYDCAE,0,0,0,0),CL10'NCLE'                           
         DC    AL1(0,0,0,TAAYAEXT,0,0,0,0),CL10'EXT'                            
         DC    AL1(0,0,0,TAAYPOV,0,0,0,0),CL10'POV'                             
*        DC    AL1(0,0,0,0,TAAYSPTI,0,0,0),CL10'SX'          SPSUG-1239         
*        DC    AL1(0,0,0,0,TAAYNETI,0,0,0),CL10'NX'          SPSUG-1239         
         DC    AL1(0,0,0,0,TAAYISPL,0,0,0),CL10'IS'                             
         DC    AL1(0,0,0,0,TAAYBUSU,0,0,0),CL10'BU'                             
         DC    AL1(0,0,0,0,TAAYNOGL,0,0,0),CL10'NG'                             
         DC    AL1(0,0,0,0,TAAYCIHR,0,0,0),CL10'CIHR'                           
*        DC    AL1(0,0,0,0,TAAYNETC,0,0,0),CL10'CX'          SPSUG-1239         
         DC    AL1(0,0,0,0,TAAYSOTH,0,0,0),CL10'OTH'                            
******   DC    AL1(0,0,0,0,0,TABRS75K,0,0),CL10'MAX75'                          
         DC    AL1(0,0,0,0,0,TABRSNWK,0,0),CL10'NWK'                            
         DC    AL1(0,0,0,0,0,TABRSSRC,0,0),CL10'SRC'                            
         DC    AL1(0,0,0,0,0,TABRSINT,0,0),CL10'INT'                            
         DC    AL1(0,0,0,0,0,TABRSCIN,0,0),CL10'CLINT'                          
         DC    AL1(0,0,0,0,0,TABRSQTR,0,0),CL10'ESTP'                           
         DC    AL1(0,0,0,0,0,0,TAAYSFIL,0),CL10'FP'                             
         DC    AL1(0,0,0,0,0,0,TAAYVIBE,0),CL10'VB'                             
         DC    AL1(0,0,0,0,0,0,TAAYJPCY,0),CL10'CSF'                            
         DC    AL1(0,0,0,0,0,0,TAAYSTPS,0),CL10'TPSIG'                          
         DC    AL1(0,0,0,0,0,0,TAAYSPLH,0),CL10'SPLH'                           
         DC    AL1(0,0,0,0,0,0,TAAYST10,0),CL10'T10'                            
         DC    AL1(0,0,0,0,0,0,TAAYSTCA,0),CL10'CAN'                            
         DC    AL1(0,0,0,0,0,0,TAAYSREG,0),CL10'REG'                            
         DC    AL1(0,0,0,0,0,0,0,TAAYSPPL),CL10'P+'                             
         DC    AL1(0,0,0,0,0,0,0,TAAYSTSM),CL10'SM'                             
******** DC    AL1(0,0,0,0,0,0,0,TAAYSCDT),CL10'CDT'                            
         DC    AL1(0,0,0,0,0,0,0,TAAYSTWK),CL10'WK'                             
         DC    AL1(0,0,0,0,0,0,0,TAATSPLA),CL10'PRTLA'                          
         DC    AL1(0,0,0,0,0,0,0,TAAYSBBD),CL10'BDJ'                            
         DC    X'FF'                                                            
*                                                                               
HOLDTAB  DS    0CL11               TABLE OF STATUS CODES FOR HLD FEE            
         DC    AL1(TAAYHS2D),CL10'NO2ND'                                        
         DC    AL1(TAAYHSCO),CL10'COMM'                                         
NOHFN    DC    AL1(TAAYHSNO),CL10'NOHFN'                                        
PO       DC    AL1(TAAYHSPO),CL10'PO'                                           
EL       DC    AL1(TAAYHSEL),CL10'EL'                                           
PE       DC    AL1(TAAYHSPE),CL10'PE'                                           
NC       DC    AL1(TAAYHSNC),CL10'NC'                                           
         DC    X'FF'                                                            
         SPACE 2                                                                
*              TABLE OF LOCK BOX CODES - WHEN ADDING NEW CODE                   
*              MUST ADD CODE/MESSAGE TO LBOXTAB IN TABIPRT                      
*                                                                               
LBOXTAB  DS    0CL1                TABLE OF LOCK BOX CODES                      
         DC    AL1(TAAYLCHI)       CHICAGO                                      
**NO-OP* DC    AL1(TAAYLNY)        NY                                           
         DC    AL1(TAAYLDDS)       NY - DDS ADDRESS                             
         DC    AL1(TAAYLP)         PRINT PAYROLL SERVICES IN CHICAGO            
**NO-OP* DC    AL1(TAAYLLA)        LA                                           
**NO-OP* DC    AL1(TAAYLOTH)       OTHERS                                       
         DC    AL1(TAAYLT)         TP IN CHICAGO                                
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE STATUS CODES AND SET CORRESPONDING BITS IN EITHER AGENCY OR          
* BILLING RULES STATUS BYTE.  ALSO VALIDATE OVERRIDE EMPLOYER OF RECORD         
* IF ONE IS GIVEN.                                                              
*                                                                               
VRSTAT   NTR1  BASE=*,LABEL=*                                                   
         XC    STATBYTS,STATBYTS   CLEAR STATUS BYTES                           
         XC    OEORBR,OEORBR       CLEAR OVERRIDE EMPLOYER OF RECORD            
******** XC    SVBUNT,SVBUNT                                                    
         BRAS  RE,DELELEMS                                                      
*                                                                               
         LA    R2,SAYSTATH         R2 = A(STATUS CODES FIELD)                   
         CLI   5(R2),0                                                          
         JE    VRSX                                                             
*                                  SCAN FIELD INTO SCANNER BLOCK                
         GOTO1 SCANNER,DMCB,(R2),(15,BLOCK)                                     
         CLI   4(R1),0                                                          
         JE    ERRINV                                                           
*                                                                               
         LA    R5,BLOCK            R5 = A(SCANNER BLOCK)                        
         USING SCAND,R5                                                         
         ZIC   R0,4(R1)            R0 = NUMBER OF SCAN BLOCK ENTRIES            
         MVC   SCENTRS,4(R1)                                                    
         SR    R3,R3               R3 = DISPLACEMMENT INTO FIELD                
*                                                                               
VRS100   CLI   SCLEN1,3            IF LHS IS 'EMP'                              
         JNE   VRS130                                                           
         CLC   =C'EMP',SCDATA1                                                  
         JNE   VRS130                                                           
*                                  THEN RHS MUST HAVE EMP OF RECORD             
         CLC   SCDATA2(3),=C'PG '     EMPLOYER PG IS NO LONGER VALID            
         JE    ERRPGEMP                                                         
         GOTO1 RECVAL,DMCB,TLEMCDQ,('80',SCDATA2)                               
         JNE   ERRFLD                                                           
         MVC   OEORBR,SCDATA2                                                   
         ZIC   RF,SCLEN2           BUMP R3 PAST RHS ON SCREEN                   
         LA    R3,1(R3,RF)                                                      
         J     VRS900                                                           
*                                                                               
VRS130   CLC   =C'VEND',SCDATA1    ELSE IF LHS IS 'VEND'                        
         JNE   VRS180                                                           
         CLI   SCLEN2,0            CHECK FOR DATA FIRST                         
         JNE   VRS150                                                           
         ZIC   RF,SCLEN1           BUMP R3 PAST LHS ON SCREEN                   
         LA    R3,1(R3,RF)                                                      
         J     ERRMFLD             SET MISSING INPUT FIELD                      
*                                  VALIDATE FREE FORM VENDOR NUMBER             
VRS150   MVC   SCDATA2-8+5(1),SCLEN2    INSERT FAKE FIELD LENGTH                
         GOTO1 NAMIN,DMCB,TANUELQ,SCDATA2-8,TANUTVND                            
         ZIC   RF,SCLEN2           BUMP R3 PAST RHS ON SCREEN                   
         LA    R3,1(R3,RF)                                                      
         J     VRS900                                                           
*                                                                               
VRS180   CLI   SCLEN1,2            IF LHS IS 'CO'                               
         JNE   VRS200                                                           
         CLC   =C'CO',SCDATA1                                                   
         JNE   VRS200                                                           
         CLI   SCLEN2,4                                                         
         JNE   ERRFLD              MUST BE 4 CHARACTERS                         
         MVC   SCDATA2-8+5(1),SCLEN2    INSERT FAKE FIELD LENGTH                
         GOTO1 NAMIN,DMCB,TANUELQ,SCDATA2-8,TANUTCO                             
         ZIC   RF,SCLEN2           BUMP R3 PAST RHS ON SCREEN                   
         LA    R3,1(R3,RF)                                                      
         J     VRS900                                                           
*                                                                               
VRS200   CLC   =C'SIG',SCDATA1     SCAN FOR ALL THE SIG'S                       
         JNE   VRS600                                                           
*        CLI   SCLEN1,3            IF LHS IS 'SIG'                              
*        JNE   VRS600                                                           
         MVI   ELCODE,TANUELQ      SEE IF SIG RATE ENTERED BEFORE               
         GOTO1 GETL,DMCB,(1,=AL1(TANUTSGN))                                     
         JE    ERRSIGR                                                          
                                                                                
         BRAS  RE,SIGNCHK                                                       
         JNE   ERRMFLD                                                          
         MVI   STATAYSG,0          SIGNATORY CALCULATION RULE                   
         CLC   =CL10'SIG',SCDATA1  WAGES + TAXES + PNH + HANDLING               
         JE    VRS500                                                           
         MVI   STATAYSG,1          WAGES ONLY                                   
         CLC   =CL10'SIGW',SCDATA1                                              
         JE    VRS500                                                           
         MVI   STATAYSG,2          WAGES + TAXES                                
         CLC   =CL10'SIGWT',SCDATA1                                             
         JE    VRS500                                                           
         MVI   STATAYSG,3          WAGES + PNH                                  
         CLC   =CL10'SIGWP',SCDATA1                                             
         JE    VRS500                                                           
         MVI   STATAYSG,4          WAGES + HANDLING                             
         CLC   =CL10'SIGWH',SCDATA1                                             
         JE    VRS500                                                           
         MVI   STATAYSG,5          WAGES + TAXES + PNH                          
         CLC   =CL10'SIGWTP',SCDATA1                                            
         JE    VRS500                                                           
         MVI   STATAYSG,6          WAGES + PNH + HANDLING                       
         CLC   =CL10'SIGWPH',SCDATA1                                            
         JE    VRS500                                                           
         MVI   STATAYSG,7          WAGES + TAXES + HANDLING                     
         CLC   =CL10'SIGWTH',SCDATA1                                            
         JNE   ERRMFLD                                                          
                                                                                
VRS500   ZIC   RF,SCLEN2                                                        
         GOTO1 CASHVAL,DMCB,SCDATA2,(RF)                                        
         CLI   0(R1),0                                                          
         JNE   ERRFLD                                                           
         CLC   4(4,R1),=F'10000'                                                
         JH    ERRFLD                                                           
         MVC   SCDATA2-8+5(1),SCLEN2    INSERT FAKE FIELD LENGTH                
         GOTO1 NAMIN,DMCB,TANUELQ,SCDATA2-8,TANUTSGN                            
         ZIC   RF,SCLEN2                                                        
         LA    R3,1(R3,RF)                                                      
         J     VRS900                                                           
*                                                                               
VRS600   CLI   SCLEN1,3            IF LHS IS 'CAP'                              
         JNE   VRS680                                                           
         CLC   =C'CAP',SCDATA1                                                  
         JNE   VRS680                                                           
         MVI   ELCODE,TANUELQ      SEE IF SIG CAP ENTERED BEFORE                
         GOTO1 GETL,DMCB,(1,=AL1(TANUTSCP))                                     
         JE    ERRSIGC                                                          
                                                                                
         BRAS  RE,SIGNCHK                                                       
         JNE   ERRMFLD                                                          
         ZIC   RF,SCLEN2                                                        
         GOTO1 CASHVAL,DMCB,SCDATA2,(RF)                                        
         CLI   0(R1),0                                                          
         JNE   ERRFLD                                                           
         CLC   4(4,R1),=F'100000000'                                            
         JH    ERRFLD                                                           
         MVC   SCDATA2-8+5(1),SCLEN2    INSERT FAKE FIELD LENGTH                
         GOTO1 NAMIN,DMCB,TANUELQ,SCDATA2-8,TANUTSCP                            
         ZIC   RF,SCLEN2                                                        
         LA    R3,1(R3,RF)                                                      
         J     VRS900                                                           
*                                                                               
VRS680   CLC   =C'UNIT',SCDATA1    IF LHS IS 'UNIT'                             
         JNE   VRS700                                                           
         ZIC   RF,SCLEN2                                                        
         GOTO1 CASHVAL,DMCB,(0,SCDATA2),(X'40',(RF))                            
         CLI   0(R1),0                                                          
         JNE   ERRFLD                                                           
         L     RF,4(R1)                                                         
         STCM  RF,3,SVBUNT                                                      
*                                                                               
         ZIC   RF,SCLEN2                                                        
         LA    R3,1(R3,RF)                                                      
         J     VRS900                                                           
*                                                                               
VRS700   CLI   SCLEN2,0            ELSE ERROR IF RHS EXISTS                     
         JNE   ERRFLD                                                           
         L     RF,ASTATTAB         LOOK IN STATUS TABLE FOR MATCH               
VRS730   CLI   0(RF),X'FF'         ERROR IF END OF TABLE FOUND                  
         JE    ERRFLD                                                           
*                                                                               
         ZIC   RE,SCLEN1           IF MATCH THEN SAVE BIT                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         CLC   SCDATA1(0),8(RF)                                                 
         JE    VRS750                                                           
*                                                                               
         LA    RF,L'STATTAB(RF)    ELSE TRY NEXT TABLE ENTRY                    
         J     VRS730                                                           
*                                                                               
VRS750   DS    0H                                                               
*&&DO                                                                           
VRS750   TM    0(RF),TAAYSCTX+TAAYSATX                                          
         JZ    *+12                                                             
         TM    STATAY,TAAYSCTX+TAAYSATX                                         
         JNZ   COMPERR                                                          
*&&                                                                             
         TM    1(RF),TAAYSHND+TAAYSMAL+TAAYSPCH+TAAYSOVN+TAAYSCHI               
         JNZ   VRS780                                                           
         TM    2(RF),TAAYSPRI                                                   
         JZ    VRS800                                                           
*                                                                               
VRS780   TM    STATAY2,TAAYSHND+TAAYSMAL+TAAYSPCH+TAAYSOVN+TAAYSCHI             
         JNZ   COMPERR                                                          
         TM    STATAY3,TAAYSPRI                                                 
         JNZ   COMPERR                                                          
*                                                                               
VRS800   TM    0(RF),TAAYSEST      IF EST KEYWORD ENTERED                       
         JZ    VRS830                                                           
         TM    STATAY6,TAAYST10    ENSURE T10 KEYWORD NOT ALREADY               
         JO    COMPERR             ENTERED                                      
                                                                                
VRS830   TM    5(RF),TABRSINT      IF INT KEYWORD ENTERED                       
         JZ    VRS850                                                           
         TM    STATAY6,TAAYST10    ENSURE T10 KEYWORD NOT ALREADY               
         JO    COMPERR             ENTERED                                      
                                                                                
VRS850   TM    6(RF),TAAYST10      IF T10 KEYWORD ENTERED                       
         JZ    VRS860                                                           
         TM    STATBR,TABRSINT     ENSURE INT KEYWORD NOT ALREADY               
         JO    COMPERR             ENTERED                                      
         TM    STATAY,TAAYSEST     ENSURE EST KEYWORD NOT ALREADY               
         JO    COMPERR             ENTERED                                      
                                                                                
VRS860   TM    6(RF),TAAYSREG      REG STATUS ONLY VALID FOR TAL2               
         JZ    VRS880              TEST SYSTEM                                  
         CLC   =C'D2',TWAAGY                                                    
         JE    VRS880                                                           
         CLC   =C'D3',TWAAGY                                                    
         JNE   ERRFLD                                                           
*                                                                               
VRS880   TM    7(RF),TAATSPLA      PRINT CALIFORNIA CHECKS IN LA                
         JZ    VRS890                                                           
         CLC   OEORBR,=C'P+ '      IS ONLY VALID FOR EMPLOYER P+                
         JNE   ERRFLD                                                           
*                                                                               
VRS890   OC    STATBYTS,0(RF)      SET APPROPRIATE BIT IN APPR BYTE             
*                                                                               
VRS900   ZIC   RF,SCLEN1           BUMP R3 TO NEXT STATUS CODE                  
         LA    R3,1(R3,RF)                                                      
*                                                                               
         LA    R5,SCANNEXT         BUMP R5 TO NEXT SCANNER ENTRY                
*                                                                               
         BCT   R0,VRS100           REPEAT FOR EACH SCANNER ENTRY                
*                                                                               
         TM    STATAY4,TAAYSCPO    'CLOSEPO' OPTION SET?                        
         JZ    VRS910              NO, DONE                                     
         TM    STATAY,TAAYSAPO     YES, IS 'PO' SET?                            
         JZ    ERRPO               NO, ERROR                                    
         CLC   =C'PP',OEORBR       YES, IS EMP=PP?                              
         JNE   ERREMP              NO, AN ERROR ALSO                            
         TM    STATBR,TABRSINT     YES, ON PRODUCTION INTERFACE?                
         JZ    ERRINT              NO, AN ERROR ALSO                            
         DROP  R5                                                               
*                                                                               
VRS910   TM    STATAY7,TAAYSPPL    IF PAYROLL PLUS AGENCY                       
         JZ    VRS930                                                           
         TM    STATAY7,TAAYSTSM    TAX USING SEMI-MONTHLY FREQUENCY             
         JZ    VRS920                                                           
         TM    STATAY7,TAAYSTWK                                                 
         JZ    VRS940                                                           
         J     ERRINV                                                           
VRS920   TM    STATAY7,TAAYSTWK    OR WEEKLY MUST BE SET                        
         JZ    ERRMISS                                                          
         J     VRS940                                                           
                                                                                
VRS930   TM    STATAY7,TAAYSTSM+TAAYSTWK                                        
         JNZ   ERRINV                                                           
                                                                                
VRS940   CLI   ACTNUM,ACTCHA       IF ACTION IS CHANGE                          
         JNE   VRS1000                                                          
         MVI   TGBYTE,0                                                         
         OC    TGBYTE,OSTATAY7     AND PRODUCTIONS PLUS STATUS                  
         NI    TGBYTE,TAAYSPPL                                                  
         MVI   TGBYTE2,0           IS CHANGING ....                             
         OC    TGBYTE2,STATAY7                                                  
         NI    TGBYTE2,TAAYSPPL                                                 
         CLC   TGBYTE,TGBYTE2                                                   
         JE    VRS1000                                                          
                                                                                
         USING TLCOD,R3                                                         
         LA    R3,KEY              ... ENSURE NO COMMERCIALS/                   
         XC    KEY,KEY             EVENTS EXIST FOR AGENCY                      
         MVI   TLCOD,TLCOCDQ                                                    
         MVC   TLCOAGY,TGAGY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(TLCOCLI-TLCOD),KEYSAVE                                       
         JE    ERRINV                                                           
         DROP  R3                                                               
                                                                                
VRS1000  TM    STATAY6,TAAYST10    IF T10 KEYWORD ENTERED                       
         JZ    VRSX                THEN VALIDATE INTFACE RECORD                 
*                                                                               
         LA    R2,SAYSTATH         R2 = A(STATUS CODES FIELD)                   
         MVC   AIO,AIO2                                                         
*                                                                               
         USING TLIFD,R3                                                         
         LA    R3,KEY                                                           
         XC    TLIFKEY,TLIFKEY                                                  
         MVI   TLIFCD,TLIFCDQ                                                   
         MVC   TLIFAGY,TGAGY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLIFKEY),KEYSAVE                                           
         BNE   VRS1050                                                          
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         USING TAIFD,R4                                                         
         MVI   ELCODE,TAIFELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   VRS1050                                                          
*                                                                               
         ZIC   R0,TAIFNWCS         N'SUB-ELEMENTS                               
         LA    R3,TAIFWCS                                                       
VRS1002  LA    R1,WCSET1           CHECK IF VALID SET 1                         
VRS1004  CLI   0(R1),X'FF'                                                      
         BE    VRS1008                                                          
         CLC   0(1,R3),0(R1)       SAME WORK CODE?                              
         BNE   VRS1006                                                          
         CLI   2(R1),1             VALID FOR THIS SET?                          
         BNE   VRS1020             NO - TEST SET 2                              
         MVI   2(R1),C'X'          MARK IT SET                                  
         B     VRS1008             YES                                          
*                                                                               
VRS1006  AHI   R1,L'WCSET1         CHECK NEXT ENTRY IN SET 1                    
         B     VRS1004                                                          
*                                                                               
VRS1008  ZIC   RF,1(R1)            BUMP TO NEXT WORK CODE IN ELEMENT            
         AR    R3,RF                                                            
         BCT   R0,VRS1002                                                       
*                                                                               
         LA    R1,WCSET1           TEST ALL REQUIRED CODES ARE SET              
VRS1010  CLI   0(R1),X'FF'                                                      
         BE    VRS1050             YES - EXIT                                   
         CLI   2(R1),1             VALID WORK CODE NOT SET?                     
         BE    ERRINV                                                           
         AHI   R1,L'WCSET1                                                      
         B     VRS1010                                                          
*                                                                               
VRS1020  ZIC   R0,TAIFNWCS         N'SUB-ELEMENTS                               
         LA    R3,TAIFWCS                                                       
VRS1022  LA    R1,WCSET2           CHECK IF VALID SET 2                         
VRS1024  CLI   0(R1),X'FF'                                                      
         BE    VRS1028                                                          
         CLC   0(1,R3),0(R1)       SAME WORK CODE?                              
         BNE   VRS1026                                                          
         CLI   2(R1),1             VALID FOR THIS SET?                          
         BNE   ERRINV              NO - ERROR                                   
         MVI   2(R1),C'X'          MARK IT SET                                  
         B     VRS1028             YES                                          
*                                                                               
VRS1026  AHI   R1,L'WCSET1         CHECK NEXT ENTRY IN SET 2                    
         B     VRS1024                                                          
*                                                                               
VRS1028  ZIC   RF,1(R1)            BUMP TO NEXT WORK CODE IN ELEMENT            
         AR    R3,RF                                                            
         BCT   R0,VRS1022                                                       
*                                                                               
         LA    R1,WCSET2           TEST ALL REQUIRED CODES ARE SET              
VRS1030  CLI   0(R1),X'FF'                                                      
         BE    VRS1050             YES - EXIT                                   
         CLI   2(R1),1             VALID WORK CODE NOT SET?                     
         BE    ERRINV                                                           
         AHI   R1,L'WCSET2                                                      
         B     VRS1030                                                          
*                                                                               
VRS1050  L     RF,AIO1                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLAYKEY),0(RF)                                             
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
VRSX     J     XIT                                                              
         LTORG                                                                  
*                                                                               
* VALID TYPE 10 WORK CODE SETS                                                  
*                                                                               
* LAYOUT: WORK CODE EQUATE, LENGTH IN TAIFNWCS, 1=VALID 0=INVALID               
*                                                                               
WCSET1   DS    0XL3                                                             
         DC    AL1(10),AL1(4),AL1(1)  RES                                       
         DC    AL1(11),AL1(5),AL1(0)  C=ZZZ                                     
         DC    AL1(12),AL1(5),AL1(1)  U                                         
         DC    AL1(129),AL1(3),AL1(1) ALL                                       
         DC    AL1(131),AL1(3),AL1(1) P&H                                       
         DC    AL1(133),AL1(3),AL1(1) T&H                                       
         DC    AL1(135),AL1(3),AL1(0) HND                                       
         DC    AL1(136),AL1(3),AL1(0) TAX                                       
         DC    X'FF'                                                            
*                                                                               
WCSET2   DS    0XL3                                                             
         DC    AL1(10),AL1(4),AL1(1)  RES                                       
         DC    AL1(11),AL1(5),AL1(1)  C=ZZZ                                     
         DC    AL1(12),AL1(5),AL1(1)  U                                         
         DC    AL1(129),AL1(3),AL1(1) ALL                                       
         DC    AL1(131),AL1(3),AL1(1) P&H                                       
         DC    AL1(133),AL1(3),AL1(0) T&H                                       
         DC    AL1(135),AL1(3),AL1(1) HND                                       
         DC    AL1(136),AL1(3),AL1(1) TAX                                       
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
*              ROUTINE ADDS TDO REQUEST IF THERE WAS AN OFFICE CODE             
*              CHANGE (TDO CHANGES THE OFFICE CODE ON ADVICE RECORDS)           
         SPACE 1                                                                
ADDREQ   NTR1  BASE=*,LABEL=*                                                   
         MVI   TDOFLAG,C'N'        SET TDO NOT REQUESTED                        
         CLC   TGOFF,SVTPOFF       IF SAVED TP OFFICE WAS CHANGED               
         JE    XIT                                                              
*                                                                               
         LA    R3,BLOCK            ADD OFFICE/NEW REQUEST                       
         USING REQD,R3                                                          
         XC    REQHDR,REQHDR       CLEAR HEADER                                 
*                                                                               
         MVC   REQUEST,DVCARD                                                   
         MVC   REQUEST+2(2),TGCTALPH CONNECT ALPHA USER ID                      
         MVC   REQUEST+4(1),TGOFF    TP OFFICE                                  
         MVC   REQUEST+5(6),TGAGY    AGENCY                                     
*                                                                               
         MVC   REQUEST+29(L'TGCTSTAF),TGCTSTAF STAFF ID                         
         MVC   REQUEST+50(L'SVTPOFF),SVTPOFF   OLD OFFICE CODE                  
         MVC   REQUEST+51(L'TGAGY),TGAGY       AND AGENCY CODE                  
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMADD '),REQFILE,(R3),(R3)                    
         CLI   8(R1),0                                                          
         JE    *+6                                                              
         DC    H'0'                                                             
         MVI   TDOFLAG,C'Y'        SET TDO REQUESTED                            
         J     XIT                                                              
*                                                                               
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
* DISPLAY STATUS CODES FROM AGENCY AND BILLING RULES STATUS BYTES.              
* ALSO DISPLAY OVERRIDE EMPLOYER OF RECORD IN SAME FIELD IF ONE EXISTS.         
*                                                                               
DRSTAT   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,SAYSTAT          R2 = A(STATUS CODES FIELD DATA)              
*                                                                               
         OC    OEORBR,OEORBR       IF OVERRIDE EMP OF RECORD EXISTS             
         JZ    DRS5                                                             
*                                                                               
         MVC   0(4,R2),=C'EMP='    DISPLAY IT                                   
         MVC   4(3,R2),OEORBR                                                   
*                                                                               
         LA    R2,6(R2)            AND POINT R2 TO LAST NON-SPACE               
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         BCT   R2,*-8                                                           
*                                  IF VENDOR NUMBER EXISTS                      
DRS5     MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=C'V')                                              
         JNE   DRS6                                                             
         CLI   0(R2),C' '          THEN IF NOT FIRST CODE                       
         JNH   *+12                                                             
         MVI   1(R2),C','          THEN DISPLAY COMMA AND BUMP PAST             
         LA    R2,2(R2)                                                         
         MVC   0(5,R2),=C'VEND='   DISPLAY VENDOR TAG                           
         LA    R2,5(R2)                                                         
*                                                                               
         L     R4,TGELEM           GET NUMBER FROM FREE FORM NUMBER EL          
         USING TANUD,R4                                                         
         ZIC   RF,TANULEN          DISPLAY NUMBER                               
         SH    RF,=H'4'                                                         
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R2),TANUMBER                                                 
         LA    R2,0(R2,RF)         POINT R2 TO LAST NON-SPACE                   
         DROP  R4                                                               
*                                  DISPLAY COMMANY CODE & OFFICE                
DRS6     MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTCO))                                      
         JNE   DRS7                                                             
         CLI   0(R2),C' '          THEN IF NOT FIRST CODE                       
         JNH   *+12                                                             
         MVI   1(R2),C','          THEN DISPLAY COMMA AND BUMP PAST             
         LA    R2,2(R2)                                                         
         MVC   0(3,R2),=C'CO='     DISPLAY CO TAG                               
         LA    R2,3(R2)                                                         
*                                                                               
         L     R4,TGELEM           GET NUMBER FROM FREE FORM NUMBER EL          
         USING TANUD,R4                                                         
         ZIC   RF,TANULEN          DISPLAY NUMBER                               
         SH    RF,=Y(TANULNQ+1)                                                 
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R2),TANUMBER                                                 
         LA    R2,0(R2,RF)         POINT R2 TO LAST NON-SPACE                   
         DROP  R4                                                               
*                                                                               
DRS7     BRAS  RE,DRSSGN           DISPLAY SIGNATORY FEE                        
         BRAS  RE,DRSSCP           DISPLAY SIGNATORY FEE CAP                    
*                                                                               
         OC    SVBUNT,SVBUNT                                                    
         JZ    DRS9                                                             
         CLI   0(R2),C' '          THEN IF NOT FIRST CODE                       
         JNH   *+12                                                             
         MVI   1(R2),C','          THEN DISPLAY COMMA AND BUMP PAST             
         LA    R2,2(R2)                                                         
         MVC   0(5,R2),=C'UNIT='   DISPLAY VENDOR TAG                           
         LA    R2,5(R2)                                                         
         EDIT  SVBUNT,(5,(R2)),0,ALIGN=LEFT                                     
         AHI   R0,-1                                                            
         AR    R2,R0               R0 CONTAINS SIGNIFICANT CHARS                
*                                                                               
DRS9     L     RF,ASTATTAB         RF = A(STATUS CODE TABLE)                    
DRS10    CLI   0(RF),X'FF'         WHILE NOT END OF TABLE                       
         JE    XIT                                                              
*                                                                               
         MVC   DUB,0(RF)           IF BIT IS ON IN STATUS BYTE                  
         NC    DUB,STATBYTS                                                     
         JZ    DRS30                                                            
*                                                                               
         CLI   0(R2),C' '          THEN IF NOT FIRST CODE                       
         JNH   *+12                                                             
         MVI   1(R2),C','          THEN DISPLAY COMMA AND BUMP PAST             
         LA    R2,2(R2)                                                         
*                                                                               
         MVC   0(10,R2),8(RF)      DISPLAY CODE                                 
*                                                                               
         LA    R2,9(R2)            POINT R2 TO LAST NON-SPACE                   
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
DRS30    LA    RF,L'STATTAB(RF)    BUMP TO NEXT STATUS CODE TABLE ENTRY         
*                                                                               
         J     DRS10               LOOP BACK                                    
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* DISPLAY BILLING RULES ELEMENT RATES.                                          
DRRATES  NTR1  BASE=*,LABEL=*                                                   
         USING TABRD,R4                                                         
         CLI   TABRTYPE,TABRTY99   IF BILLING TYPE 99 THEN NO CHARGE            
         JE    XIT                                                              
                                                                                
         TM    TABRSTAT,TABRSSRC   IF USING STANDARD RATE CARD                  
         JZ    DRR10               THEN MOVE RATES TO ELEM FOR DISPLAY          
         GOTO1 BTYPVAL,DMCB,TABRTYPE                                            
         MVC   TABRRATE,TGBSRC                                                  
                                                                                
DRR10    OC    TABRRATE,TABRRATE   IF NO RATES EXIST THEN DONE                  
         JZ    XIT                                                              
                                                                                
         LA    R2,SAYRCURH         R2 = A(FIRST SCREEN FIELD)                   
         LA    R3,TABRRATE         R3 = A(FIRST FIELD IN ELEMENT)               
         LA    RF,4                DISPLAY THE 1ST FOUR                         
         BRAS  RE,OUTRATE                                                       
                                                                                
         LA    R2,SAYRWCRH         DISPLAY WC FOR CORPS                         
         LA    R3,TABRWCRP                                                      
         LA    RF,1                RESET RF                                     
         BRAS  RE,OUTRATE                                                       
                                                                                
         LA    R2,SAYRCRPH         DISPLAY CORP                                 
         LA    R3,TABRCORP                                                      
         LA    RF,3                RESET RF                                     
         BRAS  RE,OUTRATE                                                       
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        OUTPUT BILLING RATES                                                   
*                                                                               
OUTRATE  NTR1  BASE=*,LABEL=*                                                   
OUT10    EDIT  (2,0(R3)),(5,8(R2)),2,ALIGN=LEFT,ZERO=NOBLANK                    
         XR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         TM    1(R2),X'20'         BUMP TO NEXT UNPROTECTED FIELD               
         JO    *-10                                                             
         LA    R3,2(R3)            AND TO NEXT FIELD IN ELEMENT                 
         BCT   RF,OUT10                                                         
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
* DISPLAY FILTERS                                                               
*                                                                               
DRFILT   NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO              GET FILTERS ELEMENT                          
         USING TAFLD,R4                                                         
         MVI   ELCODE,TAFLELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   SAYFILT,TAFLFLT1                                                 
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO VALIDATE NAMES                                        
         SPACE                                                                  
VALNAMES NTR1  BASE=*,LABEL=*                                                   
         GOTO1 NAMIN,DMCB,TANAELQ,SAYNAH                                        
*                                  VALIDATE SHORT NAME                          
         GOTO1 NAMIN,DMCB,TASNELQ,(X'80',SAYSNH)                                
*                                  VALIDATE ADDRESS                             
         GOTO1 ADDRIN,DMCB,SAYADH                                               
*                                  VALIDATE OPTIONAL FAX NUMBER                 
         GOTO1 NAMIN,DMCB,TANUELQ,(X'80',SAYFAXH),TANUTFAX                      
*                                                                               
*                                  VALIDATE OPTIONAL ATTENTION NAME             
         GOTO1 NAMIN,DMCB,TAFNELQ,(X'80',SAYTATTH),TAFNTATT                     
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO DELETE CERTAIN ELEMENTS                               
         SPACE                                                                  
DELELEMS NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,TANUELQ      DELETE VENDOR ELEMENT                        
         GOTO1 DELL,DMCB,(1,=C'V')                                              
         MVI   ELCODE,TANUELQ      DELETE CO ELEMENT                            
         GOTO1 DELL,DMCB,(1,=AL1(TANUTCO))                                      
         MVI   ELCODE,TANUELQ      DELETE SIGNATORY FEE ELEMENT                 
         GOTO1 DELL,DMCB,(1,=AL1(TANUTSGN))                                     
         MVI   ELCODE,TANUELQ      DELETE SIGNATORY FEE CAP ELEMENT             
         GOTO1 DELL,DMCB,(1,=AL1(TANUTSCP))                                     
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO DISPLAY SIGNATORY FEE STATUS                          
         SPACE                                                                  
DRSSGN   NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTSGN))                                     
         JNE   DRSSIGX                                                          
         CLI   0(R2),C' '          THEN IF NOT FIRST CODE                       
         JNH   *+12                                                             
         MVI   1(R2),C','          THEN DISPLAY COMMA AND BUMP PAST             
         LA    R2,2(R2)                                                         
         CLI   STATAYSG,0                                                       
         BNE   DRSSGN20                                                         
         MVC   0(4,R2),=C'SIG='    DISPLAY SIGNATORY FEE TAG                    
         LA    R2,4(R2)                                                         
         B     DRSSGN90                                                         
                                                                                
DRSSGN20 MVC   0(5,R2),=C'SIGW='   DISPLAY SIGNATORY FEE TAG                    
         CLI   STATAYSG,1                                                       
         BNE   DRSSGN30                                                         
         LA    R2,5(R2)                                                         
         B     DRSSGN90                                                         
                                                                                
DRSSGN30 MVC   0(6,R2),=C'SIGWT='  DISPLAY SIGNATORY FEE TAG                    
         CLI   STATAYSG,2                                                       
         BE    DRSSGN35                                                         
         MVC   0(6,R2),=C'SIGWP='                                               
         CLI   STATAYSG,3                                                       
         BE    DRSSGN35                                                         
         MVC   0(6,R2),=C'SIGWH='                                               
         CLI   STATAYSG,4                                                       
         BNE   DRSSGN50                                                         
DRSSGN35 LA    R2,6(R2)                                                         
         B     DRSSGN90                                                         
                                                                                
DRSSGN50 MVC   0(7,R2),=C'SIGWTP=' DISPLAY SIGNATORY FEE TAG                    
         CLI   STATAYSG,5                                                       
         BE    DRSSGN55                                                         
         MVC   0(7,R2),=C'SIGWPH='                                              
         CLI   STATAYSG,6                                                       
         BE    DRSSGN55                                                         
         MVC   0(7,R2),=C'SIGWTH='                                              
DRSSGN55 LA    R2,7(R2)                                                         
                                                                                
DRSSGN90 L     R4,TGELEM           GET NUMBER FROM FREE FORM NUMBER EL          
         USING TANUD,R4                                                         
         ZIC   RF,TANULEN          DISPLAY NUMBER                               
         SHI   RF,TANULNQ+1                                                     
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R2),TANUMBER                                                 
         LA    R2,0(R2,RF)         POINT R2 TO LAST NON-SPACE                   
DRSSIGX  XIT1  REGS=(R2)                                                        
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO DISPLAY SIGNATORY FEE CAP STATUS                      
         SPACE                                                                  
DRSSCP   NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTSCP))                                     
         JNE   DRSSCPX                                                          
         CLI   0(R2),C' '          THEN IF NOT FIRST CODE                       
         JNH   *+12                                                             
         MVI   1(R2),C','          THEN DISPLAY COMMA AND BUMP PAST             
         LA    R2,2(R2)                                                         
         MVC   0(4,R2),=C'CAP='    DISPLAY SIGNATORY FEE CAP TAG                
         LA    R2,4(R2)                                                         
         SPACE                                                                  
         L     R4,TGELEM           GET NUMBER FROM FREE FORM NUMBER EL          
         USING TANUD,R4                                                         
         ZIC   RF,TANULEN          DISPLAY NUMBER                               
         SHI   RF,TANULNQ+1                                                     
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R2),TANUMBER                                                 
         LA    R2,0(R2,RF)         POINT R2 TO LAST NON-SPACE                   
DRSSCPX  XIT1  REGS=(R2)                                                        
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CHECK BOTH SIGN= AND SCAP= ARE PRESENT                
         SPACE                                                                  
SIGNCHK  NTR1  BASE=*,LABEL=*                                                   
         USING SCAND,R5                                                         
         LA    R5,BLOCK                                                         
         ZIC   R0,SCENTRS                                                       
SC10     CLC   SCDATA1(3),=C'SIG'                                               
         JE    SC20                                                             
         LA    R5,SCANNEXT                                                      
         BCT   R0,SC10                                                          
         J     NO                                                               
         SPACE                                                                  
SC20     LA    R5,BLOCK                                                         
         ZIC   R0,SCENTRS                                                       
SC30     CLC   SCDATA1(3),=C'CAP'                                               
         JE    YES                                                              
         LA    R5,SCANNEXT                                                      
         BCT   R0,SC30                                                          
         J     NO                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* DISPLAY SERVICE FIELD                                                         
*                                                                               
DRSERV   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,SAYSERVH                                                      
         USING TAAYD,R4                                                         
         TM    TAAYMISC,TAAYPREM   PREMIUM SERVICE AGENCY?                      
         JNO   DSERV10                                                          
         MVC   SAYSERV,=C'PRE'     PREMIUM SERVICES                             
         J     DSERVX                                                           
DSERV10  MVC   SAYSERV,=C'PAY'     PAYROLL (DEFAULT)                            
         DROP  R4                                                               
*                                                                               
DSERVX   OI    SAYSERVH+6,X'80'                                                 
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* VALIDATE SERVICE FIELD                                                        
*                                                                               
VRSERV   NTR1  BASE=*,LABEL=*                                                   
         USING TAAYD,R4                                                         
         LA    R2,SAYSERVH                                                      
         CLI   5(R2),0             NO INPUT                                     
         JE    VRSERV10                                                         
         CLC   =C'PAY',8(R2)       PAYROLL (DEFAULT)                            
         JE    VRSERV10                                                         
         CLC   =C'PRE',8(R2)       PREMIUM SERVICES                             
         JNE   ERRINV                                                           
         OI    TAAYMISC,TAAYPREM                                                
*                                                                               
VRSERV10 TM    TARAFLAG,TARAFYES   CAN'T CHANGE IF AGY HAS BRATE                
         JZ    XIT                                                              
         CLC   SVSERV,SAYSERV                                                   
         JE    XIT                                                              
         MVC   SAYSERV,SVSERV                                                   
         OI    SAYSERVH+6,X'80'                                                 
         J     ERRCHGBR                                                         
*                                                                               
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE BUILDS VITA-NOTIFYING MQ MESSAGE                     *         
*        ON ENTRY ... P1 BYTE 0 = X'80' SAVE TO WSSVR BLOCK           *         
*                     AIO1      = A(AGENCY RECORD)                    *         
***********************************************************************         
                                                                                
BLDMQMSG NTR1  BASE=*,LABEL=*                                                   
         MVC   TGBYTE,0(R1)        TGBYTE = WSSVR SAVE STATUS                   
                                                                                
         L     R2,AMQMSG           R2=A(MQ MESSAGE BLOCK)                       
         BAS   RE,BLDIMMSG         BUILD INSERT/MODIFY MESSAGE                  
         JE    BMQ10                                                            
         BAS   RE,BLDDMSG          OR BUILD DELETE MESSAGE                      
         JNE   XIT                 LENGTH RETURNED IN R3                        
                                                                                
         USING FAWSSVRD,R1                                                      
BMQ10    TM    TGBYTE,X'80'        IF SAVING TO WSSVR                           
         JZ    BMQX                DO SO NOW                                    
         LA    R1,WORK                                                          
         MVC   FAWSTOKN,=CL4'INIT'                                              
         MVI   FAWSACTN,FAWSASVE                                                
         ST    R2,FAWSADR                                                       
         STH   R3,FAWSLEN                                                       
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0                                                        
         JE    BMQX                                                             
         DC    H'00'                                                            
         DROP  R1                                                               
                                                                                
BMQYES   XR    RC,RC                                                            
BMQNO    LTR   RC,RC                                                            
BMQX     XIT1  REGS=(R3)                                                        
                                                                                
***********************************************************************         
*        ROUTINE BUILDS VITA-NOTIFYING INSERT/MODIFY MESSAGE          *         
*        ON ENTRY ... AIO1 = A(AGENCY RECORD)                         *         
*                     R2   = A(MQ MESSAGE BLOCK)                      *         
***********************************************************************         
                                                                                
BLDIMMSG NTR1                                                                   
         CLI   MODE,XRECDEL        IF NOT DELETING RECORD                       
         JE    BMQNO                                                            
                                                                                
         USING IMMSGD,R2                                                        
         LHI   R3,IMMLNQ                                                        
         LR    RE,R2                                                            
         LR    RF,R3                                                            
         LA    R0,IMMSG                                                         
         LR    R1,R3               COPY INSERT MQ MESSAGE TEMPLATE              
         MVCL  RE,R0               INTO BLOCK                                   
                                                                                
         CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         JE    BIMM10                                                           
         CLI   ACTNUM,ACTREST      OR RESTORE                                   
         JNE   BIMM20                                                           
BIMM10   MVC   IMROT,=C'<acpinsert>'                                            
         MVC   IMINS,BMQTRUE       SET INSERT AND MODIFY STATUS                 
         MVC   IMMOD,BMQFALSE                                                   
         MVC   IMROX,=C'</acpinsert>'                                           
         J     BIMM30                                                           
                                                                                
BIMM20   MVC   IMROT,=C'<acpmodify>'                                            
         MVC   IMINS,BMQFALSE      IF ACTION IS CHANGE                          
         MVC   IMMOD,BMQTRUE       SET INSERT AND MODIFY STATUS                 
         MVC   IMROX,=C'</acpmodify>'                                           
                                                                                
         USING TLAYD,R4                                                         
BIMM30   L     R4,AIO1                                                          
         MVC   IMCOD,TLAYAGY       COPY AGENCY CODE INTO XML                    
         GOTO1 ELIMCHAR,DMCB,(L'IMCOD,IMCOD)                                    
         DROP  R4                                                               
                                                                                
         USING TAAYD,R4                                                         
         MVI   ELCODE,TAAYELQ      GET AGENCY ELEMENT                           
         BRAS  RE,GETEL                                                         
         BNE   BIMM40                                                           
                                                                                
         MVC   IMPHO,TAAYTEL       COPY AGENCY PHONE NUMBER                     
         OC    IMPHO,SPACES                                                     
         MVC   IMOFF,TAAYTPOF      AND OFFICE CODE INTO XML                     
         OC    IMOFF,SPACES                                                     
                                                                                
         MVI   IMLCK,C'N'                                                       
         TM    TAAYSTA3,TAAYSLCK   COPY LOCKED STATUS INTO XML                  
         BZ    *+8                                                              
         MVI   IMLCK,C'Y'                                                       
                                                                                
******** MVI   IMPDF,C'N'                                                       
******** TM    TAAYSTA6,TAAYSFIL                                                
******** BZ    *+8                                                              
         MVI   IMPDF,C'Y'          COPY FILLABLE PDF STATUS INTO XML            
                                                                                
         MVI   IM13W,C'Y'                                                       
         TM    TAAYSTAT,TAAYS13W   COPY CALCULATE 13 WEEKS AS 3 MONTHS          
         BZ    *+8                 STATUS INTO XML                              
         MVI   IM13W,C'N'                                                       
                                                                                
******** MVI   IMTPY,C'N'                                                       
******** TM    TAAYSTA6,TAAYVIBE   COPY ENABLED FOR VITA TIMESHEETS             
******** BZ    *+8                 AND PAY STATUS INTO XML                      
         MVI   IMTPY,C'Y'                                                       
                                                                                
         MVI   IMORQ,C'N'          COPY PRD REQUIRED STATUS INTO XML            
         TM    TAAYSTA6,TAAYST10                                                
         BZ    *+8                                                              
         MVI   IMORQ,C'Y'                                                       
                                                                                
         MVI   IMPRQ,C'N'                                                       
         TM    TAAYSTAT,TAAYSAPO   COPY PO REQUIRED STATUS INTO XML             
         BZ    *+8                                                              
         MVI   IMPRQ,C'Y'                                                       
         TM    TAAYSTA4,TAAYPOV                                                 
         BZ    *+8                                                              
         MVI   IMPRQ,C'Y'                                                       
                                                                                
         MVI   IMPVL,C'N'                                                       
         TM    TAAYSTA4,TAAYSCPO   COPY PO VALIDATED STATUS INTO XML            
         BZ    *+8                                                              
         MVI   IMPVL,C'Y'                                                       
         TM    TAAYSTA4,TAAYPOV                                                 
         BZ    *+8                                                              
         MVI   IMPVL,C'Y'                                                       
                                                                                
         MVI   IMJRQ,C'N'                                                       
         TM    TAAYSTAT,TAAYSEST   COPY JOB REQUIRED STATUS INTO XML            
         BZ    *+8                                                              
         MVI   IMJRQ,C'Y'                                                       
         TM    TAAYSTA6,TAAYST10                                                
         BZ    *+8                                                              
         MVI   IMJRQ,C'Y'                                                       
         OC    TAAYBUNT,TAAYBUNT                                                
         BZ    *+8                                                              
         MVI   IMJRQ,C'Y'                                                       
                                                                                
         MVI   IMBAG,C'N'          COPY betaAgency STATUS INTO XML              
         TM    TAAYSTA6,TAAYVIBE                                                
         BZ    *+8                                                              
         MVI   IMBAG,C'Y'                                                       
                                                                                
         MVI   IMJVL,C'N'                                                       
         OC    TAAYBUNT,TAAYBUNT   COPY JOB VALIDATED STATUS INTO XML           
         BZ    *+8                                                              
         MVI   IMJVL,C'Y'                                                       
         TM    TAAYSTA6,TAAYST10                                                
         BZ    *+8                                                              
         MVI   IMJVL,C'Y'                                                       
         TM    TAAYSTAT,TAAYSEST                                                
         BZ    BIMM40                                                           
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTJOB))                                     
         BNE   BIMM40                                                           
         MVI   IMJVL,C'Y'                                                       
         DROP  R4                                                               
                                                                                
         USING TABRD,R4                                                         
BIMM40   MVC   IMEMP,=C'TP '       COPY EMPLOYER INTO XML                       
         L     R4,AIO                                                           
         MVI   ELCODE,TABRELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   BIMM50                                                           
         OC    TABROEOR,TABROEOR                                                
         BZ    *+10                                                             
         MVC   IMEMP,TABROEOR                                                   
                                                                                
         TM    TABRSTAT,TABRSINT                                                
         BZ    BIMM50                                                           
         MVI   IMORQ,C'Y'          COPY PRD REQUIRED STATUS                     
         MVI   IMJRQ,C'Y'          JOB REQUIRED STATUS                          
         MVI   IMJVL,C'Y'          AND JOB VALIDATED STATUS INTO XML            
         DROP  R4                                                               
                                                                                
         USING TANAD,R4                                                         
BIMM50   L     R4,AIO              COPY AGENCY NAME INTO XML                    
         MVI   ELCODE,TANAELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   BIMM60                                                           
         ZIC   RE,TANALEN                                                       
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   IMNAM(0),TANANAME                                                
         GOTO1 ELIMCHAR,DMCB,(L'IMNAM,IMNAM)                                    
         DROP  R4                                                               
                                                                                
         USING TAADD,R4                                                         
BIMM60   L     R4,AIO                                                           
         MVI   ELCODE,TAADELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   BIMM70                                                           
         ZIC   RE,TAADLEN                                                       
         SHI   RE,3                                                             
         LA    RF,TAADADD                                                       
         DROP  R4                                                               
                                                                                
         MVC   IMAD1,0(RF)         COPY 1ST ADDRESS LINE INTO XML               
                                                                                
         SHI   RE,L'IMAD1                                                       
         LTR   RE,RE                                                            
         BZ    BIMM70                                                           
         LA    RF,L'IMAD1(RF)                                                   
         MVC   IMAD2,0(RF)         COPY 2ND ADDRESS LINE INTO XML               
                                                                                
         SHI   RE,L'IMAD2                                                       
         LTR   RE,RE                                                            
         BZ    BIMM70                                                           
         LA    RF,L'IMAD2(RF)                                                   
         MVC   IMAD3,0(RF)         COPY 3RD ADDRESS LINE INTO XML               
                                                                                
         SHI   RE,L'IMAD3                                                       
         LTR   RE,RE                                                            
         BZ    BIMM70                                                           
         LA    RF,L'IMAD3(RF)                                                   
         MVC   IMAD4,0(RF)         COPY 4TH ADDRESS LINE INTO XML               
                                                                                
BIMM70   GOTO1 ELIMCHAR,DMCB,(L'IMAD1,IMAD1)                                    
         GOTO1 ELIMCHAR,DMCB,(L'IMAD2,IMAD2)                                    
         GOTO1 ELIMCHAR,DMCB,(L'IMAD3,IMAD3)                                    
         GOTO1 ELIMCHAR,DMCB,(L'IMAD4,IMAD4)                                    
         J     BMQYES                                                           
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ROUTINE BUILDS VITA-NOTIFYING DELETE MESSAGE                 *         
*        ON ENTRY ... AIO1 = A(AGENCY RECORD)                         *         
*                     R2   = A(MQ MESSAGE BLOCK)                      *         
***********************************************************************         
                                                                                
BLDDMSG  NTR1                                                                   
         CLI   MODE,XRECDEL        IF DELETING RECORD                           
         JNE   BMQNO                                                            
                                                                                
         USING DMSGD,R2                                                         
         LHI   R3,DMLNQ                                                         
         LR    RE,R2                                                            
         LR    RF,R3                                                            
         LA    R0,DMSG                                                          
         LR    R1,R3               COPY DELETE MQ MESSAGE TEMPLATE              
         MVCL  RE,R0               INTO BLOCK                                   
                                                                                
         MVC   DCOD,SAYAGY         COPY AGENCY CODE INTO XML                    
         GOTO1 ELIMCHAR,DMCB,(L'DCOD,DCOD)                                      
         J     BMQYES                                                           
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
BMQTRUE  DC    CL5'true'                                                        
BMQFALSE DC    CL5'false'                                                       
                                                                                
IMMSG    DC    CL16' '                                                          
         DC    C'<?xml version="1.0" encoding="UTF-8"?>'                        
         DC    CL11' '                                                          
         DC    C'<agency isinsert="'                                            
         DC    CL5' '                                                           
         DC    C'" ismodify="'                                                  
         DC    CL5' '                                                           
         DC    C'" code="'                                                      
         DC    CL6' '                                                           
         DC    C'" name="'                                                      
         DC    CL36' '                                                          
         DC    C'" add1="'                                                      
         DC    CL30' '                                                          
         DC    C'" add2="'                                                      
         DC    CL30' '                                                          
         DC    C'" add3="'                                                      
         DC    CL30' '                                                          
         DC    C'" add4="'                                                      
         DC    CL30' '                                                          
         DC    C'" phone="'                                                     
         DC    CL12' '                                                          
         DC    C'" office="'                                                    
         DC    CL1' '                                                           
         DC    C'" employer="'                                                  
         DC    CL3' '                                                           
         DC    C'" locked="'                                                    
         DC    CL1' '                                                           
         DC    C'" fillablePDF="'                                               
         DC    CL1' '                                                           
         DC    C'" calculate13WeeksAs3Months="'                                 
         DC    CL1' '                                                           
         DC    C'" productRequired="'                                           
         DC    CL1' '                                                           
         DC    C'" poRequired="'                                                
         DC    CL1' '                                                           
         DC    C'" poValidated="'                                               
         DC    CL1' '                                                           
         DC    C'" jobRequired="'                                               
         DC    CL1' '                                                           
         DC    C'" jobValidated="'                                              
         DC    CL1' '                                                           
         DC    C'" timesheetPay="'                                              
         DC    CL1' '                                                           
         DC    C'" betaAgency="'                                                
         DC    CL1' '                                                           
         DC    C'">'                                                            
         DC    C'</agency>'                                                     
         DC    CL12' '                                                          
IMMLNQ   EQU   *-IMMSG                                                          
                                                                                
DMSG     DC    CL16' '                                                          
         DC    C'<?xml version="1.0" encoding="UTF-8"?>'                        
         DC    C'<acpdelete>'                                                   
         DC    C'<agency isdelete="true" '                                      
         DC    C'code="'                                                        
         DC    CL6' '                                                           
         DC    C'">'                                                            
         DC    C'</agency>'                                                     
         DC    C'</acpdelete>'                                                  
DMLNQ    EQU   *-DMSG                                                           
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE OUTPUTS MQ MESSAGE                                   *         
*        ON ENTRY ... AIO1 = A(AGENCY RECORD)                         *         
***********************************************************************         
                                                                                
NFYVIT   NTR1  BASE=*,LABEL=*                                                   
         GOTOR BLDMQMSG,DMCB,0     BUILD UPDATED MQ MESSAGE                     
                                                                                
         USING FAWSSVRD,R1                                                      
         CLI   MODE,XRECPUT        IF JUST CHANGED RECORD                       
         JNE   NV10                                                             
         LA    R1,WORK                                                          
         XC    WORK(FAWSSVRL),WORK                                              
         MVC   FAWSTOKN,=CL4'INIT'                                              
         MVI   FAWSACTN,FAWSARST   RECALL INITIAL-STATE BASED                   
         MVC   FAWSADR,AIO3        MESSAGE INTO AIO3                            
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R1                                                               
                                                                                
         L     RE,AMQMSG                                                        
         LR    RF,R3               IF CHANGE HAS BEEN MADE THAT                 
         L     R0,AIO3             WILL AFFECT THE MESSAGE                      
         LR    R1,R3               SEND THE UPDATED MESSAGE                     
         CLCL  RE,R0                                                            
         JE    XIT                                                              
                                                                                
NV10     GOTO1 NTFYVITA,DMCB,AMQMSG,(R3),0                                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE FILTERS                                             *         
***********************************************************************         
                                                                                
VRFILT   NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,TAFLELQ      REMOVE EXISTING ELEMENT                      
         GOTO1 REMELEM                                                          
                                                                                
         CLI   SAYFILTH+5,0        TEST FOR INPUT                               
         JE    XIT                                                              
         XC    ELEMENT,ELEMENT     BUILD NEW ELEMENT                            
         LA    R4,ELEMENT                                                       
         USING TAFLD,R4                                                         
         MVI   TAFLEL,TAFLELQ                                                   
         MVI   TAFLLEN,TAFLLNQ                                                  
         OC    SAYFILT,SPACES                                                   
         MVC   TAFLFLT1(4),SAYFILT                                              
         GOTO1 ADDELEM                                                          
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAVALPJB                                                       
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR12D                                                       
         EJECT                                                                  
* LOCAL VARIABLES                                                               
*                                                                               
MYFULL   DS    F                                                                
OSTATAY6 DS    X                   ORIGINAL 6TH STATYS BYTE FOR AGY EL          
OSTATAY7 DS    X                   ORIGINAL 7TH STATYS BYTE FOR AGY EL          
STATBYTS DS    0CL8                STATUS BYTES FOR BOTH AY AND BR ELS          
STATAY   DS    X                   STATUS BYTE FOR AGENCY ELEMENT               
STATAY2  DS    X                   2ND STATUS BYTE FOR AGENCY ELEMENT           
STATAY3  DS    X                   3RD STATUS BYTE FOR AGENCY ELEMENT           
STATAY4  DS    X                   4TH STATUS BYTE FOR AGENCY ELEMENT           
STATAY5  DS    X                   5TH STATUS BYTE FOR AGENCY ELEMENT           
STATBR   DS    X                   STATUS BYTE FOR BILLING RULES ELEM           
STATAY6  DS    X                   6TH STATUS BYTE FOR AGENCY ELEMENT           
STATAY7  DS    X                   7TH STATUS BYTE FOR AGENCY ELEMENT           
STATAYSG DS    X                   SIGNATORY FEE CALCULATION                    
TDOFLAG  DS    CL1                 TDO REQUESTED ADDED FLAG                     
OEORBR   DS    CL3                 OVERRIDE EMPLOYER OF RECORD                  
RESWORK  DS    PL3                 WORK AREA FOR DISPLAYING RESET INV #         
SVNINV   DS    XL6                 SAVED NEXT INVOICE NUMBER                    
SVNADV   DS    XL6                 SAVED ADVICE NUMBER                          
SVTPOFF  DS    CL1                 SAVED TALENT PARTNERS OFFICE CODE            
SVHDL    DS    XL(L'TAGHHDL)       SAVED GUARANTEE HANDLING RATE                
SVLIM    DS    XL(L'TAGHLIM)       SAVED GUARANTEE LIMIT                        
SVKEY    DS    XL48                BACK UP OF KEY                               
SVTGAGY  DS    CL(L'TGAGY)         SAVED GLOBAL AGENCY                          
SVBUNT   DS    XL2                 SAVED BUSINESS UNIT                          
SCENTRS  DS    XL1                 SCANNER ENTRIES                              
AMQMSG   DS    A                   A(MQ MESSAGE BLOCK)                          
*                                                                               
SVBTYPE  DS    XL1                 ORIGINAL BILLING TYPE                        
SVBHRLS  DS    XL1                 ORIGINAL HANDLING RULES                      
SVGHHDL  DS    XL2                 ORIGINAL HANDLING RATE                       
SVSERV   DS    CL3                 ORIGINAL SERVICE                             
SVMISC   DS    XL1                 ORIGINAL MISC FLAGS                          
TARAFLAG DS    XL1                 AGENCY HAS BRATE                             
TARAFYES EQU   X'80'                                                            
PROSTAT  DS    XL1                 PROGRAM STATUS                               
PSDJNALL EQU   X'80'               DEFAULT JOB NOT ALLOWED                      
*                                                                               
ASTATTAB DS    A                   A(STATUS TABLE)                              
*                                                                               
PPBLOCK  DS    XL(5*38+1)          BLOCK FOR 5 DIRECTORY POINTERS               
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FAWSSVRD                                                       
         PRINT ON                                                               
***********************************************************************         
*        DSECT COVERS VITA-NOTIFYING INSERT/UPDATE MESSAGES           *         
***********************************************************************         
                                                                                
IMMSGD   DSECT                                                                  
         DS    CL16                                                             
         DS    CL38                                                             
IMROT    DS    CL11                                                             
         DS    CL18                                                             
IMINS    DS    CL5                                                              
         DS    CL12                                                             
IMMOD    DS    CL5                                                              
         DS    CL8                                                              
IMCOD    DS    CL6                                                              
         DS    CL8                                                              
IMNAM    DS    CL36                                                             
         DS    CL8                                                              
IMAD1    DS    CL30                                                             
         DS    CL8                                                              
IMAD2    DS    CL30                                                             
         DS    CL8                                                              
IMAD3    DS    CL30                                                             
         DS    CL8                                                              
IMAD4    DS    CL30                                                             
         DS    CL9                                                              
IMPHO    DS    CL12                                                             
         DS    CL10                                                             
IMOFF    DS    CL1                                                              
         DS    CL12                                                             
IMEMP    DS    CL3                                                              
         DS    CL10                                                             
IMLCK    DS    CL1                                                              
         DS    CL15                                                             
IMPDF    DS    CL1                                                              
         DS    CL29                                                             
IM13W    DS    CL1                                                              
         DS    CL19                                                             
IMORQ    DS    CL1                                                              
         DS    CL14                                                             
IMPRQ    DS    CL1                                                              
         DS    CL15                                                             
IMPVL    DS    CL1                                                              
         DS    CL15                                                             
IMJRQ    DS    CL1                                                              
         DS    CL16                                                             
IMJVL    DS    CL1                                                              
         DS    CL16                                                             
IMTPY    DS    CL1                                                              
         DS    CL14                                                             
IMBAG    DS    CL1                                                              
         DS    CL2                                                              
         DS    CL9                                                              
IMROX    DS    CL12                                                             
                                                                                
***********************************************************************         
*        DSECT COVERS VITA-NOTIFYING DELETE MESSAGES                  *         
***********************************************************************         
                                                                                
DMSGD    DSECT                                                                  
         DS    CL16                                                             
         DS    CL38                                                             
         DS    CL11                                                             
         DS    CL24                                                             
         DS    CL6                                                              
DCOD     DS    CL6                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'140TAGEN12   03/15/18'                                      
         END                                                                    
