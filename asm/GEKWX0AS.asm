*          DATA SET GEKWX0AS   AT LEVEL 039 AS OF 05/01/02                      
*PHASE TF200AA,*                                                                
*INCLUDE GETLIST                                                                
         TITLE 'GEKWX0A - TF200A - SEND, ADDSEND, AUTHORIZE, PRINT'             
*                                                                               
*********************************************************************           
*                                                                   *           
*   GEKWX0A - TF200A - KWX, SEND ADDSEND, AUTHORIZE AND PRINT       *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* AUG30/89 (MRR) --- FIX BUG WHEN LOOKING FOR END OF LIST MARKER    *           
*                     SO THAT WE DON'T USE THE DEST NUMBER ITSELF.  *           
*                                                                   *           
* MAR06/90 (MRR) --- DROP SUPPORT OF REP STATION RECORD OPTION BYTE *           
*                     4 --- WESTERN UNION FLAG - IT'S ALWAYS ON.    *           
*                                                                   *           
* 27MAR90  (EFJ) --- DEFAULT TEAM TO 'T' IF NO TEAM - USED TO BE    *           
*                    ONLY FOR WU (FIX 06MAR90 FIX) - PER KARI       *           
*                                                                   *           
* 03APR90  (SDE) --- CHANGE BILLING INFO DSECT TO ALLOW UP TO 25    *           
*                    BYTES FOR DESTINATION                          *           
*                                                                   *           
* FEB11/91 (MRR) --- >MAKE ACTIVE/KEEP ONLY ACTIVE                  *           
*                    >DROP STATION AFFL FROM W.U. HEADER            *           
*                                                                   *           
* APR10/91 (MRR) --- >FIX PROCESSING OF CALL LETTERS W/ A BAND      *           
*                     (I.E., 'A', 'F' OR 'C'                        *           
*                                                                   *           
* JAN20/92 (MRR) --- >DO EDICT STUFF, PUT OUT A '++DDS' LINE        *           
*                                                                   *           
* MAR06/92 (SKU) --- MOVE TEAM CODE TO COL 55 AND ELIMINATE         *           
*                    AFFILIATE IN BILLING INFO                      *           
*                                                                   *           
* APR20/92 (SKU) --- SET BILL CODE TO R FOR INTEREPS                *           
*                                                                   *           
* OCT16/92 (SKU) --- ADD SYSTEM INFO FOR NEW SPOOLKEY FIELDS        *           
*                                                                   *           
* MAR14/93 (SKU) --- REMOVE REP AND OFF INFO BEFORE *HDR*           *           
*                                                                   *           
* SEP08/94 (SKU) --- USE STATION'S FAX # IF SEND VIA EASYLINK       *           
*                                                                   *           
* JUL14/95 (SKU) --- REMOVE TWX FORMAT RESTRICTION                  *           
*                                                                   *           
* OCT17/95 (SKU) --- ADD SUPPORT FOR INTERNATIONAL FAX AND MAIL BOX *           
*                                                                   *           
* DEC15/95 (SKU) --- ADJUST FOR NEW DDS START TIME                  *           
*                                                                   *           
* JAN24/96 (SKU) --- FIX .NP COMMAND                                *           
*                                                                   *           
* MAY20/96 (SKU) --- VALIDATE SEND USERID'S PRINCIPLE ID AGAINST    *           
*                    THE SENDER'S PRINCIPLE ID                      *           
*                                                                   *           
* MAY24/96 (SKU) --- INCREASE MAX DEST ID FROM 50 TO 200            *           
*                                                                   *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
KWX0A    CSECT                                                                  
         NMOD1 150,**KWX0A*,R9                                                  
         USING KWX0A+4096,R9       R9=2ND BASE                                  
         LR    R2,RC                                                            
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         ST    R2,FULL             FULL POINTS TO 1000-BYTE IO AREA,            
*                                  4 BYTES SLACK AND 134 BYTE EDIT AREA         
         USING TWAD,RA                                                          
*                                                                               
T001     ST    RB,ABASE2           SAVE SOME ADDS                               
         ST    R9,A2NDBAS2                                                      
         LA    RE,PRINT                                                         
         ST    RE,APRINT                                                        
         LA    RE,ACCOUNT                                                       
         ST    RE,AACCOUNT                                                      
         ST    RD,AREGSAV2                                                      
         LA    RE,EXTRA            CLEAR AREA OF W/S FOR PRINT FLDS             
         LA    RF,1000                                                          
         XCEF                                                                   
         B     T010                                                             
         EJECT                                                                  
*              FURTHER VALIDATION OF PRE-PROCESSED PARAMETERS                   
*                                                                               
T010     CLI   ACTION,SEN          MUST BE ENDED BEFORE SEND IF CHKSUM          
         BNE   T011                                                             
         TM    MSGSTAT,ENDED                                                    
         BO    T011                                                             
         TM    MSGSTAT,CHECKSUM                                                 
         BNO   T011                                                             
         MVI   FERN,BKNENDED                                                    
         B     ERROR                                                            
*                                                                               
T011     CLI   ACTION,PRI          VALIDATE USER STRING                         
         BE    T015                                                             
         CLI   FXADDS,0                                                         
         BNE   T011A                                                            
         MVI   FERN,NOADDEES                                                    
         MVI   SYNTAX,C'Y'                                                      
         B     ERROR                                                            
T011A    BAS   RE,CHKADDS                                                       
         BZ    ERROR                                                            
*                                                                               
T012     CLI   ACTION,AUT          VALIDATE ACCESS                              
         BNE   T013                MANDATORY FOR AUTHORIZE                      
         CLI   FXACCESS,0                                                       
         BNE   T014                                                             
         MVI   FERN,MISSING                                                     
         MVI   FNDX,2                                                           
         MVI   SYNTAX,C'Y'                                                      
         B     ERROR                                                            
T013     CLI   ACTION,SEN          OPTIONAL FOR SEND - 'WRITE' IF GIVEN         
         BNE   T015                                                             
         CLI   FXACCESS,0                                                       
         BE    T015                                                             
         CLI   ACCESS,C'W'                                                      
         BE    T014                                                             
         MVI   FERN,INVALID                                                     
         MVC   FNDX,FXACCESS                                                    
         MVI   SYNTAX,C'Y'                                                      
         B     ERROR                                                            
*                                                                               
T014     LA    RF,FRMNDEX          ONLY OWNER CAN CHANGE ACCESS                 
         CLI   SAVMODE,FORMAT                                                   
         BE    *+8                                                              
         LA    RF,MSGNDEX                                                       
         CLC   TWAUSRID,0(RF)                                                   
         BE    T015                                                             
         MVC   FNDX,FXACCESS                                                    
         MVI   FERN,INVNOWNR                                                    
         B     ERROR                                                            
*                                                                               
T015     CLI   ACTION,AUT          VALIDATE SPACE                               
         BE    T280                                                             
         CLI   FXSPACE,0                                                        
         BNE   *+8                                                              
         MVI   SPACE,C'2'          DEFAULTS TO DOUBLE                           
         CLI   SPACE,C'1'                                                       
         BL    T017                                                             
         CLI   SPACE,C'3'                                                       
         BH    T017                                                             
         NI    SPACE,X'0F'         CONVERT TO BINARY                            
         B     T020                                                             
T017     MVC   FNDX,FXSPACE                                                     
         MVI   FERN,INVALID                                                     
         MVI   SYNTAX,C'Y'                                                      
         B     ERROR                                                            
*                                                                               
T020     CLI   FXREF,0             VALIDATE REF=NO (SUPPRESS REF NUMS)          
         BE    T100                                                             
         CLI   REFLO,C'N'                                                       
         BE    T100                                                             
         MVC   FNDX,FXREF                                                       
         MVI   FERN,INVALID                                                     
         MVI   SYNTAX,C'Y'                                                      
         B     ERROR                                                            
         EJECT                                                                  
*              INITIALISE FOR PRINTING                                          
*                                                                               
T100     DS    0H                  SAVE DATE,TIME,SENDER,REF                    
         GOTO1 AGETFACT,PARAS,0                                                 
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         MVC   TODAY(2),FADATE+6                                                
*&&UK                                                                           
         MVC   TODAY+2(2),FADATE+3                                              
         MVC   TODAY+4(2),FADATE                                                
*&&                                                                             
*&&US                                                                           
         MVC   TODAY+2(2),FADATE                                                
         MVC   TODAY+4(2),FADATE+3                                              
*&&                                                                             
         AP    FATIME,=P'60000'                                                 
         UNPK  DUB,FATIME                                                       
         MVC   TIMENOW,DUB+2                                                    
*                                                                               
T105     MVC   FROM(2),SAVTAGID    SENDER = AG/III                              
         MVI   FROM+2,C'/'                                                      
         MVC   FROM+3(3),LASTINIT                                               
         MVC   WORK(2),TODAY+4     REF=DDNNNNNN                                 
         EDIT  FASIN,(6,WORK+2),FILL=0,WRK=WORK+20                              
         DROP  RF                                                               
*                                                                               
T110     CLI   ACTION,PRI          MODIFY IF ACTION IS PRINT                    
         BNE   T120                                                             
         LA    RF,IO               USING MESSAGE BOOK HEADER DETAILS            
         USING HDRD,RF             OF KWX LAST SENT FROM BOOK                   
         OC    HDKWXDSC,HDKWXDSC   IF ANY                                       
         BNZ   T115                                                             
         MVC   WORK+2(6),=C'-DRAFT'                                             
         MVC   FROM,SPACES                                                      
         B     T120                                                             
T115     MVC   WORK(8),HDKWXDSC                                                 
         MVC   TODAY,HDKWXDAT                                                   
         MVC   TIMENOW,HDKWXTIM                                                 
         MVC   FROM,HDKWXFRM                                                    
         MVC   ADDS+1(L'HDKWXTO),HDKWXTO                                        
         MVI   ADDS,L'HDKWXTO                                                   
         DROP  RF                                                               
*                                                                               
T120     LA    R8,PLKWX            BUILD INITIALISE PRINT LINE IN PLKWX         
         USING PQPLD,R8                                                         
         XC    PLKWX,PLKWX                                                      
         MVI   QLSOFLAB,X'00'                                                   
         MVI   QLEXTRA,X'FF'       SET NEW CALL                                 
         MVI   QLFLAG,X'00'                                                     
         MVI   QLSUBID,C'R'                                                     
         MVC   QLSUBID+1(2),SAVTPOW                                             
         MVI   QLLINET,X'C0'       FIXED LEN WITH CC                            
         MVI   QLLINEW,132         FIXED LEN 132                                
         MVC   QLRETNL,=H'36'      LIVE RETAIN HOURS                            
         MVC   QLRETND,=H'2'       DEAD RETAIN HOURS                            
         MVC   QLDESC(8),WORK                                                   
         MVI   QLLPP,0                                                          
*                                                                               
         MVI   QLSYS,C'R'          FORM/COPIES COLUMN                           
         MVC   QLPRG,=C'KX'                                                     
*                                                                               
T125     MVC   PLC,SPACES          BUILD MESSAGE HEADER IN PLC                  
         MVI   PLC,PR1SP1                                                       
         MVC   PLC+1(12),=CL12'START OF KWX'                                    
         MVC   PLC+14(11),QLDESC                                                
         MVC   PLC+26(5),=CL5'FROM-'                                            
         MVC   PLC+31(6),FROM                                                   
         GOTO1 ADATCON,PARAS,(0,TODAY),(8,PLC+39)                               
         MVC   PLC+48(3),=CL3'AT '                                              
         MVC   PLC+51(4),TIMENOW                                                
         MVC   PLC+56(9),=CL9'BOOKNAME-'                                        
         LA    R7,KEY              BOOKNAME-OWNER.IIIINNN                       
         USING CTIREC,R7                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),MSGBOOK                                              
         BAS   RE,READ                                                          
         BZ    ERROR                                                            
         LA    R6,CTIDATA                                                       
         SR    R5,R5                                                            
T130     CLI   0(R6),0                                                          
         BE    ERROR                                                            
         CLI   0(R6),X'02'                                                      
         BE    *+14                                                             
         IC    R5,1(R6)                                                         
         AR    R6,R5                                                            
         B     T130                                                             
         USING CTDSCD,R6                                                        
         MVC   PLC+65(10),CTDSC                                                 
         DROP  R6                                                               
         LA    R6,PLC+74                                                        
         CLI   0(R6),C' '                                                       
         BH    *+8                                                              
         BCT   R6,*-8                                                           
         MVI   1(R6),C'.'                                                       
         MVC   2(4,R6),MSGBOOK+2                                                
         UNPK  6(1,R6),MSGBOOK+7(1)                                             
         OI    6(R6),X'F0'                                                      
         MVC   WORK(1),MSGBOOK+6                                                
         MVI   WORK+1,X'0F'                                                     
         UNPK  DUB(3),WORK(2)                                                   
         MVC   7(2,R6),DUB                                                      
*                                                                               
T135     L     R7,AIOB             BUILD BASIC KWXFILE ACCOUNTING REC           
         USING KWXACD,R7                                                        
         XC    KWXACD(KWXALENQ+4),KWXACD                                        
         MVI   KWXRTYP,KWXRTYPQ                                                 
         MVI   KWXAEL,KWXAELQ                                                   
         MVI   KWXALEN,KWXALENQ                                                 
         MVC   KWXAUSER,TWAUSRID                                                
         MVC   KWXAAUTH(3),LASTINIT                                             
         MVC   KWXADATE,TODAY                                                   
         MVC   KWXATIME,TIMENOW                                                 
         MVC   KWXAREF,QLDESC                                                   
         MVC   KWXASENT,ADDS+1                                                  
         OC    KWXASENT,SPACES                                                  
         LA    R5,500(R7)                                                       
         B     T140                                                             
         EJECT                                                                  
*              PRINT KWX'S UNDER CONTROL OF DESTIDS TABLE                       
*                                                                               
T140     L     R6,ADESTIDS         PREPARE FOR DESTIDS LOOP                     
         USING DID,R6                                                           
         MVI   FIRSTID,C'Y'                                                     
         XC    ERRNUM,ERRNUM                                                    
         MVI   TYPE,REPORT                                                      
         CLI   ACTION,PRI          IF ACTION IS PRINT, SET ONE DESTID           
         BNE   T145                ENTRY FOR SELF AS A FILE COPY                
         MVI   DIDLEN,DISHORT                                                   
         MVC   DINUM,TWAUSRID                                                   
         MVI   DICOPIES,1                                                       
         MVI   DICCS,0                                                          
         MVI   DESTNUM+1,1                                                      
         MVI   SCOPY,C'F'                                                       
*                                                                               
T145     CLI   DID,0               ANY MORE DESTINATIONS                        
         BE    T260                NO                                           
*                                                                               
         ZIC   R4,DICOPIES         R4 = NO OF COPIES FOR ID                     
         MVI   QLSUBID,C'R'                                                     
         MVC   QLSRCID,DINUM                                                    
         CLI   DITYPE,C'S'         TEST REP STATION                             
         BNE   *+10                                                             
         MVC   QLSRCID,TWAUSRID    USE ORIGIN ID FOR GRAPHNET STATIONS          
*                                                                               
         CLC   QLSRCID,TWAUSRID                                                 
         BNE   T150                                                             
         CLI   SCOPY,C'F'          FILE COPY(IES)                               
         BNE   T150                                                             
         MVI   QLSUBID,C'F'                                                     
         MVI   FCOPYNOW,C'Y'                                                    
         MVI   FIRSTID,C'N'        DISABLE ACCOUNTING                           
*                                                                               
T150     MVI   QLCLASS,C'K'                                                     
         MVI   QLSTAT,0                                                         
         CLI   DITYPE,C'S'         TEST REP STATION                             
         BNE   T165                NO                                           
*                                                                               
         MVI   QLCLASS,C'G'        USE CLASS 'G' FOR GRAPHNET                   
         L     R3,FULL                                                          
         GOTO1 ADATAMGR,DMCB,=C'GETREC',=C'REPFIL',DIRSTADA,(R3),WORK           
         CLI   DMCB+8,0            GET REP STATION RECORD                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RSTAREC,R3                                                       
         L     R3,FULL             RESTORE A(IO AREA)                           
         MVC   RBILL,SPACES        BILLING INFORMATION                          
         MVI   RBILCC,PR1SP1       CARRIAGE CONTROL                             
         MVC   RBILHDR,=C'*HDR*'   HEADER TAG                                   
         MVC   WORK(5),RSTAKSTA    WE MAY NEED STATION CALL FOR WEST U.         
*                                                                               
         MVI   RBILTEAM,C'T'       AS PER KARI                                  
*                                                                               
         MVI   RBILPAGE,C'P'       TO ENABLE PAGE EJECT                         
*                                                                               
         CLI   RSTAGRUP,C'R'       AS PER KARI, AGAIN                           
         BNE   *+8                 R FOR INTEREPS                               
         MVI   RBILTEAM,C'R'                                                    
         DROP  R3                                                               
*                                                                               
T154     DS    0H                                                               
         L     R3,FULL             RESTORE A(IO AREA)                           
         CLC   TWAACCS(2),=C'O='   TEST OFFICE CODE IS IN TWA                   
         BNE   T160                NO                                           
*                                                                               
         MVI   BYTE,X'04'          OFFICE/TEAM ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   T160                THERE ARE NO TEAM CODES                      
         USING RSTAOTEL,R3                                                      
T155     CLC   RSTAOTOF,TWAACCS+2  TEST MATCH ON OFFICE CODE                    
         BE    *+16                GOT IT                                       
         BAS   RE,NEXTEL           NO, TRY AGAIN                                
         BE    T155                                                             
         B     T160                NO ELEMENT FOR THIS OFFICE                   
         CLI   RBILTEAM,C'R'                                                    
         BE    *+14                                                             
         MVC   RBILTEAM,RSTAOTTM                                                
         B     T160                                                             
         MVC   RBILTEAM+1(2),RSTAOTTM+1                                         
         DROP  R3                                                               
*                                                                               
T160     MVC   RBILDEST(5),WORK    PUT CALL LETTERS IN HEADLINE                 
*                                                                               
         L     R3,FULL             RESTORE A(IO AREA)                           
         MVI   BYTE,X'08'          XTRA DESCRIPTION ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   T165                                                             
         USING RSTAXXEL,R3                                                      
         OC    RSTAOFAX,RSTAOFAX   IF FAX# VALID, USE IT INSTEAD OF             
         BZ    T165                STATION CALL LETTERS                         
*                                                                               
* MAIL BOX                                                                      
*                                                                               
         CLC   =C'MB=',RSTAOFAX                                                 
         BNE   T161                                                             
         MVC   RBILDEST(8),RSTAOFAX+3                                           
         B     T165                                                             
*                                                                               
* INTERNATINOAL FAX NUMBER                                                      
*                                                                               
T161     DS    0H                                                               
         CLI   RSTAOFAX,0          INTERNATIONAL?                               
         BNE   T163                                                             
         CLI   RSTAOFAX+1,0        HUH? NO LENGTH? SKIP IT!                     
         BE    T165                                                             
         MVC   RBILDEST(4),=C'FAX '                                             
*                                                                               
         ZIC   RE,RSTAOFAX+1       INTERNATIONAL CODE                           
         LA    RF,RBILDEST+4                                                    
         EDIT  (RE),(3,0(RF)),FILL=0                                            
*                                                                               
         UNPK  WORK(16),RSTAOFAX+3(8)                                           
         ZIC   RF,RSTAOFAX+2       LENGTH OF SIGNIFICANT DIGITS                 
         LA    RE,16               MAXIMUM LENGTH OF FIELD                      
         SR    RE,RF               GET SIGNIFICANT OFFSET                       
         LA    RF,WORK             A(UNPACKED NUMBER)                           
         AR    RF,RE               ADD OFFSET                                   
         ZIC   RE,RSTAOFAX+2       GET LENGTH OF FAX# FIELD AGAIN               
         BCTR  RE,0                DECREMENT FOR EX                             
         EX    RE,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   RBILDEST+7(0),0(RF)                                              
*                                                                               
         EX    RE,*+8              DISPLAY NUMBER IN ETI                        
         B     *+10                                                             
         MVC   RBILFORM+3(0),0(RF)                                              
         ZIC   RE,RSTAOFAX+1                                                    
         LA    RF,RBILFORM                                                      
         EDIT  (RE),(3,0(RF)),FILL=0                                            
         B     T165                                                             
*                                                                               
* NORMAL FAX NUMBER                                                             
*                                                                               
T163     DS    0H                                                               
         LA    RE,RSTAOFAX                                                      
                                                                                
T163A    DS    0H                                                               
         CLI   0(RE),0                                                          
         BE    T164                                                             
         CLI   0(RE),C' '                                                       
         BE    T164                                                             
         CLI   0(RE),C'0'          MUST BE NUMERIC                              
         BL    T165                                                             
         CLI   0(RE),C'9'                                                       
         BH    T165                                                             
         LA    RF,RSTAOFAX                                                      
         LA    RF,L'RSTAOFAX(RF)                                                
         LA    RE,1(RE)                                                         
         CR    RE,RF                                                            
         BNH   T163A                                                            
                                                                                
T164     DS    0H                  FAX NUMBER MUST BE EXACTLY 10 CHARS          
         LA    RF,RSTAOFAX                                                      
         SR    RE,RF                                                            
         CH    RE,=H'10'                                                        
         BNE   T165                                                             
         MVC   RBILDEST(4),=C'FAX '                                             
         MVC   RBILDEST+4(10),RSTAOFAX                                          
                                                                                
         MVI   RBILFORM,C'('       FORMATTED FAX# FOR $ETI                      
         MVC   RBILFORM+1(3),RSTAOFAX    AREA CODE                              
         MVI   RBILFORM+4,C')'                                                  
         MVC   RBILFORM+6(3),RSTAOFAX+3  PREFIX                                 
         MVI   RBILFORM+9,C'-'                                                  
         MVC   RBILFORM+10(4),RSTAOFAX+6 SUFFIX                                 
         DROP  R3                                                               
*                                                                               
T165     MVC   PLB,PQPLD           LOOP FOR A COPY                              
         BAS   RE,PRINT            INITIALISE                                   
*                                                                               
         CLI   DITYPE,C'S'         TEST REP STATION                             
         BNE   T170                NO                                           
         MVC   RBILLAST,=C'........'                                            
         SR    R1,R1                                                            
         ICM   R1,3,WORK+1         REPORT NUMBER                                
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RBILPQNO,DUB                                                     
*                                                                               
         MVC   PLB,PLA                                                          
         BAS   RE,PRINT            PRINT BILLING INFORMATION                    
*                                                                               
T170     EQU   *                                                                
         CLI   QLCLASS,C'G'        EASI-LINK'ABLE                               
         BNE   T175                                                             
         BAS   RE,DOEDICT                                                       
         L     RF,FULL                                                          
         LA    RF,1004(RF)                                                      
         MVC   PLB,0(RF)                                                        
         BAS   RE,PRINT                                                         
T175     EQU   *                                                                
*                                                                               
         MVI   PLB,HEADOF                                                       
         BAS   RE,ACCOUNT                                                       
         BAS   RE,PRINT            NEW PAGE                                     
*                                                                               
         MVC   WORK(L'PLC),PLC                                                  
         CLI   DITYPE,C'S'         TEST REP STATION                             
         BNE   *+16                NO                                           
         MVC   WORK+26(11),SPACES  DON'T PRINT 'FROM-'                          
         MVC   WORK+56(30),SPACES  DON'T PRINT 'BOOKNAME-'                      
         MVC   PLB,WORK                                                         
         BAS   RE,PRINT            HEADING LINE 1                               
*                                                                               
         MVI   PLB,PR1SP2          HEADING LINE 2 (SENT TO)                     
         MVC   PLB+1(8),=CL8'SENT TO-'                                          
         CLI   SCOPY,C'Y'                                                       
         BE    T190                                                             
*                                                                               
         CLI   DITYPE,C'S'         TEST REP STATION                             
         BE    *+14                YES                                          
         MVC   PLB+9(L'DINAM),DINAM                                             
         B     T180                                                             
*                                                                               
         L     R3,FULL             RESTORE A(IO AREA)                           
         USING RSTAREC,R3                                                       
         MVC   PLB+9(L'RSTAKSTA),RSTAKSTA                                       
         DROP  R3                                                               
*                                                                               
T180     CLI   DIDLEN,DISHORT      GIVE LIST OR ID NAME                         
         BE    T185                                                             
         ZIC   R1,DIDLEN           PLUS CC NAMES IN BRACKETS IF ANY             
         LA    RF,DISHORT+1                                                     
         SR    R1,RF                                                            
         LA    RF,PLB+9+L'DINAM                                                 
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'('                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   2(0,RF),DICCS                                                    
         LA    RF,3(R1,RF)                                                      
         MVI   0(RF),C')'                                                       
T185     CLI   QLSUBID,C'R'                                                     
         BE    T200                                                             
         CLI   DITYPE,C'S'         TEST REP STATION                             
         BE    T200                YES                                          
         CLC   DINUM,TWAUSRID      UNLESS IT IS SENDER/FILE COPY - THIS         
         BNE   T200                CASE SHOW ALL ADDRESSEES                     
T190     CLI   ADDS,0                                                           
         BNE   *+8                                                              
         MVI   PLB,SP1             UNLESS THERE ARE NONE                        
         MVC   PLB+9(L'ADDS-1),ADDS+1                                           
T200     BAS   RE,PRINT                                                         
*                                                                               
T230     DS    0H                  PREPARE FOR MESSAGE BOOK READ LOOP           
         SR    R2,R2               R2 = CHUNK NUMBER                            
*                                                                               
T232     LA    R2,1(R2)            GET A CHUNK                                  
         GOTO1 AGETCHNK,PARAS,(SAVMODE,(R2))                                    
         TM    DMCB+8,X'80'        EOF                                          
         BO    T250                                                             
         CLI   DMCB+8,0                                                         
         BNE   ERROR                                                            
         LA    R3,IO+2                                                          
         USING FLDHDRD,R3                                                       
         SR    R0,R0               R0 = FLDADR COMPARAND                        
         SR    R1,R1                                                            
         NI    USAGE,ALL-X'10'     CLEAR EOL-USAGE INDICATOR                    
*                                                                               
T233     TM    FLDOLEN,EOL         CHECK FOR EOL BIT SET IN ANY FLD IN          
         BO    T234                CHUNK - IF NOT PRINT LINE=SCRN LINE          
         IC    R1,0(R3)                                                         
         AR    R3,R1                                                            
         CLI   0(R3),0                                                          
         BNE   T233                                                             
         B     *+14                                                             
T234     OI    USAGE,X'10'                                                      
         XC    HALF,HALF                                                        
         LA    R3,IO+2                                                          
*                                                                               
T235     CLI   0(R3),0             LOOP FOR A LINE                              
         BE    T232                END OF CHUNK                                 
         AH    R0,=H'80'                                                        
         MVC   PLB,SPACES                                                       
         MVI   PLB,PR1SP1                                                       
*                                                                               
T237     CLI   0(R3),0             LOOP FOR A FIELD                             
         BE    T240                                                             
         LH    RE,FLDADR                                                        
         SRDL  RE,32                                                            
         D     RE,=F'80'                                                        
         TM    USAGE,X'10'         IS LINE CONTROL VIA EOL INDICATORS           
         BNZ   T237A                                                            
         CH    R0,FLDADR                                                        
         BNH   T240                                                             
         B     T237B                                                            
T237A    CH    R0,FLDADR                                                        
         BH    *+12                                                             
         AH    R0,=H'80'                                                        
         MVI   HALF+1,79                                                        
         AH    RE,HALF                                                          
T237B    CLC   FLDDATA(3),=C'.NP'  FORCE NEW PAGE BY KEYWORD                    
         BE    *+14                                                             
         CLC   FLDDATA(5),=C'PAGE='                                             
         BNE   T237C                                                            
         ZAP   LINE,MAXLINE                                                     
         B     T238                                                             
T237C    LA    RE,PLB(RE)                                                       
         ZIC   R1,FLDLEN                                                        
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),FLDDATA                                                  
         TM    FLDIIND,REFNUM      REF NUMBER FIELD                             
         BZ    T238                                                             
         MVC   0(3,RE),SPACES      PRINT UNLESS REF=NO                          
         CLI   FXREF,0                                                          
         BNE   T238                                                             
         EDIT  (R2),(3,0(RE)),FILL=0                                            
T238     ZIC   R1,FLDLEN                                                        
         TM    FLDOLEN,EOL                                                      
         BNZ   *+10                                                             
         AR    R3,R1                                                            
         B     T237                                                             
         AR    R3,R1                                                            
         XC    HALF,HALF                                                        
*                                                                               
T240     CLI   SCOPY,C'Y'          WE HAVE A PRINT LINE IN PLB                  
         CLC   PLB+1(6),=C'SPACE=' CHECK FOR KEYWORDS - SPACE=N                 
         BNE   T242                                                             
         CLI   SPACE,0                                                          
         BNE   T235                                                             
         CLI   PLB+7,C'1'                                                       
         BL    T235                                                             
         CLI   PLB+7,C'3'                                                       
         BH    T235                                                             
         MVC   SPACEING,PLB+7                                                   
         NI    SPACEING,X'0F'                                                   
         B     T235                                                             
*                                                                               
T242     CLI   SCOPY,C'Y'          INCLUDED REPORTS XTRACTS VIA REPORT=         
         BE    T245                NOT EXPANDED IN SENDER'S COPY                
         CLC   PLB+1(7),=C'REPORT='                                             
         BNE   T245                                                             
         LA    R3,PLB+1            PASS REPORT-HANDLING ROUTINE                 
         OC    PLB+1(133),SPACES   A(PRINT LINE) TO TREAT AS A CARD             
         BAS   RE,KEYREPT          IMAGE                                        
         BNZ   T232                                                             
         B     T255                                                             
*                                                                               
T245     SR    R1,R1               RESET SPACEING AS REQUIRED IE                
         ICM   R1,1,SPACE          1. SPACE=N PARAM IN ACTION OVERRIDES         
         BNZ   *+12                2. SPACE=N IN PRINT LINE   OVERRIDES         
         ICM   R1,1,SPACEING       3. PREVIOUS SPACE=N IN PRINT LINE            
         BZ    T248                                                             
         LA    R1,SPACETAB-1(R1)                                                
         MVC   PLB(1),0(R1)                                                     
         B     T248                                                             
SPACETAB DC    AL1(PR1SP1,PR1SP2,PR1SP3)                                        
*                                                                               
T248     BAS   RE,ACCOUNT          PRINT A LINE                                 
         BAS   RE,PRINT                                                         
         B     T235                                                             
*                                                                               
T250     MVI   PLB,SP1             CLOSE MESSAGE                                
         BAS   RE,PRINT                                                         
         MVI   PLB,PR1SP1                                                       
         MVC   PLB+1(10),=C'END OF KWX'                                         
         MVC   PLB+12(8),QLDESC                                                 
         BAS   RE,PRINT                                                         
         MVI   PLB,TRMINATE                                                     
         BAS   RE,PRINT                                                         
*                                                                               
T252     CLI   SCOPY,C'Y'                                                       
         BNL   T280                                                             
         CLI   FCOPYNOW,C'Y'       DONT ACCOUNT FOR 'F'ILE COPY                 
         BE    *+14                                                             
         MVC   0(2,R5),QLSRCID                                                  
         LA    R5,2(R5)                                                         
         MVI   FIRSTID,C'N'                                                     
         BCT   R4,T150             REPEAT IF MULTIPLE COPIES                    
*                                                                               
T254     ZIC   R0,DIDLEN           BUMP TO NEXT DESTID                          
         AR    R6,R0                                                            
         MVI   FCOPYNOW,C'N'                                                    
         B     T145                                                             
*                                                                               
T255     CLI   SCOPY,C'Y'          DESTID IN ERROR                              
         BNL   T280                                                             
         CLI   FCOPYNOW,C'Y'                                                    
         BE    T254                                                             
         LH    R1,ERRNUM                                                        
         LA    R1,1(R1)                                                         
         STH   R1,ERRNUM                                                        
         MVI   DIERR,C'Y'                                                       
         B     T254                                                             
         EJECT                                                                  
*              POST-PRINTING                                                    
*                                                                               
T260     LA    R2,KWXACTH          COMPLETION MESSAGE                           
         LH    R3,DESTNUM                                                       
         SH    R3,ERRNUM                                                        
         BNZ   *+12                                                             
         MVI   FERN,NOADDEES                                                    
         B     ERROR                                                            
         CLI   SCOPY,C'F'                                                       
         BNE   *+6                                                              
         BCTR  R3,0                                                             
         MVC   KWXHEAD(21),=CL21'KWX DD999999 SENT TO'                          
         MVC   KWXHEAD+4(8),QLDESC                                              
         CLI   ACTION,PRI                                                       
         BNE   *+14                                                             
         MVC   KWXHEAD+24(11),=CL11'PRINT QUEUE'                                
         B     OKXIT                                                            
         LA    RF,KWXHEAD+24                                                    
         EDIT  (R3),(3,0(RF)),ALIGN=LEFT,ZERO=NOBLANK                           
         OI    0(RF),C'0'                                                       
         AR    RF,R0                                                            
         SR    R0,R0                                                            
         MVC   1(13,RF),=C' DESTINATIONS'                                       
         CH    R3,=H'1'                                                         
         BNE   T262                                                             
         MVI   13(RF),C' '                                                      
*                                                                               
T262     LH    R3,DESTNUM          COMPLETE ACCOUNTING RECORD                   
         SH    R3,ERRNUM                                                        
         CLI   SCOPY,C'F'                                                       
         BNE   *+6                                                              
         BCTR  R3,0                                                             
         LTR   R3,R3                                                            
         BZ    T270                                                             
         LA    R3,500(R7)                                                       
         LA    R4,2                                                             
         BCTR  R5,0                                                             
         LA    R1,KWXANEXT                                                      
T264     LR    R6,R1               BUILD DEST ID STRING ELEMENTS OF UP          
         USING KWXDEL,R6           TO 125 ID'S PER ELEMENT                      
         LA    R0,125                                                           
         MVI   KWXDEL,KWXDELQ                                                   
         MVI   KWXDLEN,125*2+2     MAX EL LEN                                   
         LA    R1,KWXDID                                                        
         B     *+12                                                             
T266     BCT   R0,*+8                                                           
         B     T264                                                             
         MVC   0(2,R1),0(R3)                                                    
         LA    R1,2(R1)                                                         
         BXLE  R3,R4,T266                                                       
         LR    R0,R1               SET LAST ELEMENT LENGTH                      
         SR    R0,R6                                                            
         STC   R0,KWXDLEN                                                       
         MVI   0(R1),0                                                          
         SR    R1,R7               SET RECORD LENGTH                            
         LA    R1,1(R1)                                                         
         STH   R1,KWXRLEN                                                       
*                                                                               
T268     B     *+4                 WRITE ACCOUNTING RECORD                      
         MVC   AKWX,DMCB+8                                                      
*                                                                               
T270     CLI   SCOPY,0             SENDER'S COPY IF REQUIRED                    
         BNE   T280                                                             
         MVI   QLSUBID,C'S'                                                     
         MVC   QLSRCID,TWAUSRID                                                 
         MVI   SCOPY,C'Y'                                                       
         B     T150                                                             
         DROP  R3,R7                                                            
         EJECT                                                                  
*              UPDATE HEADER WITH AUTHORIZATION AND MESSAGE DETAILS             
*                                                                               
T280     DS    0H                  GET REC 1 INTO IO                            
         GOTO1 AGETCHNK,PARAS,(SAVMODE,0)                                       
         BZ    ERROR                                                            
*                                                                               
T285     L     R6,ADESTIDS         UPDATE AUTHORIZATION                         
         USING DID,R6                                                           
         L     R4,DMCB+12                                                       
         USING HDRD,R4                                                          
         SR    R0,R0                                                            
         MVI   WORK+2,READACC      PUT ACCESS BITS INTO WORK+2                  
         CLC   DESTNUM,=H'1'       DEFAULT IS READ FOR MULTIPLE ADDRESS         
         BNE   *+8                 AND WRITE FOR SINGLE ONES                    
         MVI   WORK+2,WRITACC                                                   
         CLI   ACCESS,0                                                         
         BE    T286                                                             
         MVI   WORK+2,READACC                                                   
         CLI   ACCESS,C'R'                                                      
         BE    T286                                                             
         MVI   WORK+2,WRITACC                                                   
         CLI   ACCESS,C'W'                                                      
         BE    T286                                                             
         MVI   WORK+2,0                                                         
*                                                                               
T286     CLI   DIDLEN,0            LOOP FOR AN ID                               
         BE    T290                                                             
         MVC   WORK(2),DINUM                                                    
         LA    R7,HDACCS                                                        
         LA    RF,HDACCMAX                                                      
T287     CLI   0(R7),X'FF'                                                      
         BNE   *+12                                                             
         MVI   3(R7),X'FF'                                                      
         B     T288                                                             
         CLC   DINUM,0(R7)                                                      
         BE    T288                                                             
         LA    R7,L'HDACCS(R7)                                                  
         BCT   RF,T287                                                          
         B     T290                                                             
T288     MVC   0(3,R7),WORK                                                     
T289     IC    R0,DIDLEN                                                        
         AR    R6,R0                                                            
         B     T286                                                             
*                                                                               
T290     CLI   ACTION,AUT          MESSAGE DETAILS                              
         BE    T295                                                             
         MVC   HDKWXDSC,QLDESC                                                  
         MVC   HDKWXFRM,FROM                                                    
         MVC   HDKWXTO,ADDS+1                                                   
         MVC   HDKWXDAT,TODAY                                                   
         MVC   HDKWXTIM,TIMENOW                                                 
         OI    MSGSTAT,SENT                                                     
         MVC   HDMSTAT,MSGSTAT                                                  
*                                                                               
T295     GOTO1 APUTCHNK,PARAS,(SAVMODE,0),(R4)                                  
         DROP  R4                                                               
*                                                                               
T300     CLI   ACTION,AUT                                                       
         BNE   OKXIT                                                            
         MVC   KWXHEAD(29),=CL29'ACTION COMPLETED - ENTER NEXT'                 
         B     OKXIT                                                            
         EJECT                                                                  
*              ROUTINE TO VALIDATE A USER STRING AND BUILD A BLOCK OF           
*              IDS IN DESTIDS (SEE DSECT DID)                                   
*              USED FOR ADDRESSEE STRINGS, AUTHORIZE USER STRINGS ETC.          
*                                                                               
CHKADDS  NTR1                                                                   
         LA    R3,ADDS             USING THE SUBSET OF THE ACTION PARMS         
         ZIC   R5,0(R3)            IN ADDS+1 (L IN ADDS), BUILD A               
         AR    R5,R3               FIXED-LENGTH BLOCK IN IOB WITH               
         LA    R3,1(R3)            ONE ENTRY PER ID/LIST NAME                   
         LA    R4,1                                                             
         L     R8,AIOB                                                          
         USING SCAND,R8                                                         
         MVI   SCANSIGN,C'+'                                                    
         CLI   0(R3),C'+'                                                       
         BNZ   *+8                                                              
         LA    R3,1(R3)                                                         
*                                                                               
CHKA10   LR    R6,R3               R6=START OF AN ID/LIST NAME                  
         SR    R1,R1               R1=ITS LENGTH                                
CHKA20   CLI   0(R3),C'+'                                                       
         BE    CHKA30                                                           
         CLI   0(R3),C'-'                                                       
         BE    CHKA30                                                           
         CLI   0(R3),C'<'                                                       
         BE    CHKA30                                                           
         MVI   FERN,DELIMIT                                                     
         CLI   0(R3),C','                                                       
         BE    ERROR                                                            
         MVI   FERN,INVBRACK                                                    
         CLI   0(R3),C'('                                                       
         BE    ERROR                                                            
         LA    R1,1(R1)                                                         
         BXLE  R3,R4,CHKA20                                                     
*                                                                               
CHKA30   MVC   SCANNAME,SPACES     BUILD SCAND ENTRY                            
         MVI   SCANMYMK,C'N'                                                    
         MVI   SCANNUM,1                                                        
         MVI   SCANCCSL,0                                                       
         CLC   0(3,R6),=C'MY.'     SET SCANMYMK=Y IF LIST IS FORCED TO          
         BNE   CHKA40              BE UNDER THIS USERID NOT SAVTKWID            
         LA    R6,3(R6)                                                         
         SH    R1,=H'3'                                                         
         MVI   SCANMYMK,C'Y'                                                    
*                                                                               
CHKA40   CH    R1,=H'8'            MOVE IN NAME                                 
         BNH   *+8                                                              
         LA    R1,8                                                             
         STC   R1,SCANLEN                                                       
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BM    CHKA50                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SCANNAME(0),0(R6)                                                
*                                                                               
CHKA50   CLI   0(R3),C'<'          HANDLE BRACKETED NUMBER OF COPIES            
         BNE   CHKA100             (1-9) AND CC NAMES                           
         MVI   FERN,NOQUALS                                                     
         CLI   SCANSIGN,C'-'       BRACKETS INVALID IF SIGN IS MINUS            
         BE    ERROR                                                            
         TM    ACTSTAT,PRINTYPE    OR THIS IS NOT A PRINT ACTION                
         BNO   ERROR                                                            
         TM    1(R3),X'F0'         IF CHAR AFTER '(' IS NUMERIC, ASSUME         
         BNO   CHKA60              ITS 1-9 NO OF COPIES                         
         MVC   SCANNUM,1(R3)                                                    
         NI    SCANNUM,X'0F'       STORE IN BINARY                              
         MVI   FERN,INVNUM                                                      
         BZ    ERROR                                                            
         LA    R3,2(R3)                                                         
         CLI   0(R3),C'>'          IS IT JUST NO OF COPIES OR CC NAMES          
         BE    CHKA90              TOO                                          
         CLI   0(R3),C','                                                       
         BNE   ERROR                                                            
CHKA60   LA    R3,1(R3)            HANDLE CC NAMES                              
         LA    RF,SCANCCS                                                       
         SR    R1,R1               R1=L'CC NAME STRING                          
CHKA70   CLI   0(R3),C'>'                                                       
         BE    CHKA80                                                           
         MVC   0(1,RF),0(R3)                                                    
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BXLE  R3,R4,CHKA70                                                     
         MVI   FERN,NOCBRACK                                                    
         B     ERROR                                                            
CHKA80   STC   R1,SCANCCSL                                                      
CHKA90   LA    R3,1(R3)                                                         
*                                                                               
CHKA100  LA    R8,SCANL(R8)        BUMP TO NEXT                                 
         MVC   SCANSIGN,0(R3)                                                   
         BXLE  R3,R4,CHKA10                                                     
         MVI   SCANSIGN,0          TERMINATE BLOCK                              
*                                                                               
*                                                                               
CHKA110  L     R8,AIOB             NOW GOT THROUGH SCAND BLOCK BUILDING         
         L     R6,ADESTIDS                                                      
         MVI   0(R6),0             DESTIDS BLOCK WHICH HAS ONE ENTRY            
         CLI   SCAND+SCANL,0       PER USER ID - IE LISTS ARE EXPLODED          
         BE    *+8                 AND +/- EXPRESSIONS RESOLVED                 
         MVI   SUBFNDX,1                                                        
*                                                                               
CHKA120  CLI   0(R8),0                                                          
         BE    CHKA320                                                          
         MVI   FERN,INVALID                                                     
         CLI   SCANLEN,0                                                        
         BE    ERROR                                                            
         CLI   SCANLEN,6                                                        
         BH    ERROR                                                            
*                                                                               
CHKA130  CLI   SCANMYMK,C'Y'                                                    
         BE    CHKA200                                                          
         CLC   SCANNAME(4),=CL4'ME' '-ME' MEANS NO COPIES FOR SENDER            
         BNE   CHKA140              '+ME' MEANS 'F' COPY INSTEAD OF 'S'         
         MVI   SCOPY,C'N'                                                       
         CLI   SCANSIGN,C'-'                                                    
         BE    CHKA300                                                          
         MVI   SCOPY,C'F'                                                       
         L     R7,ATIA                                                          
         MVC   0(2,R7),TWAUSRID                                                 
         XC    2(2,R7),2(R7)                                                    
         B     CHKA160                                                          
*                                                                               
CHKA140  LA    R7,KEY              READ FOR ID RECORD                           
         USING CTIREC,R7                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,SPACES                                                    
         ZIC   R1,SCANLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CTIKID(0),SCANNAME                                               
         BAS   RE,READ                                                          
         BZ    CHKA170                                                          
*                                                                               
* USER ID RECORD FOUND. CHECK IF SEND-TO USER ID'S PRINCIPLE ID MATCH           
* THAT OF THE SENDER'S PRINCIPLE ID                                             
*                                                                               
         SR    R5,R5                                                            
         LA    R6,CTIDATA                                                       
CHKA142  CLI   0(R6),X'03'         FIND PRINCIPLE ID ELEMENT                    
         BE    CHKA145                                                          
         BH    CHKA148             NO PRINCIPLE ID, MUST BE PRINCIPLE           
         IC    R5,1(R6)            USER                                         
         AR    R6,R5                                                            
         B     CHKA142                                                          
*                                                                               
CHKA145  DS    0H                  PRINCIPLE ID ELEMENT FOUND                   
         CLC   SAVTKWID,2(R6)                                                   
         BNE   CHKA170             NO MATCH, CHECK STATION RECORD               
*                                                                               
CHKA148  DS    0H                                                               
         SR    R5,R5                                                            
         LA    R6,CTIDATA                                                       
CHKA150  CLI   0(R6),X'02'                                                      
         BE    *+14                                                             
         IC    R5,1(R6)                                                         
         AR    R6,R5                                                            
         B     CHKA150                                                          
         USING CTDSCD,R6                                                        
         L     R7,ATIA             PUT ID NUMBER IN TIA    AND GO TO            
         MVC   0(2,R7),CTDSC       ADD/SUBTRACT IT FROM OUR DESTID              
         XC    2(2,R7),2(R7)                                                    
CHKA160  MVI   4(R7),X'FF'         BLOCK                                        
         MVI   TYPE,C'I'                                                        
         DROP  R6,R7                                                            
         B     CHKA220                                                          
*                                                                               
CHKA170  GOTO1 AGETFACT,PARAS,0    FIND SYSID                                   
         L     RE,0(R1)                                                         
         USING FACTSD,RE                                                        
         CLI   FASYSID,1           TEST /TST SYSTEM                             
         BE    CHKA175             YES                                          
*                                                                               
         LA    RF,FACIDTAB         CHECK THE SYSTEM TYPE                        
         USING FACITABD,RF                                                      
CHKA173  CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                THIS BETTER EXIST ON FACIDTAB                
*                                                                               
         CLC   FACIID,FASYSID                                                   
         BE    *+12                                                             
         LA    RF,L'FACITAB(RF)                                                 
         B     CHKA173                                                          
*                                                                               
         TM    FACIFL,FACIREP      ARE WE ON THE REP SYSTEM?                    
         BZ    CHKA200             NO                                           
         DROP  RE,RF                                                            
*                                                                               
CHKA175  DS    0H                                                               
         LA    R6,WORK             YES - MUST VALIDATE FOR REP STATION          
         USING RSTARECD,R6                                                      
         XC    RSTAKEY,RSTAKEY                                                  
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,TWAAGY                                                  
         MVC   RSTAKSTA,SPACES                                                  
*                                                                               
         MVC   RSTAKSTA,SCANNAME                                                
         CLI   SCANSIGN+SCANL,C'-' TEST FOR A BAND ENTRY                        
         BNE   CHKA180             NO                                           
         CLI   SCANLEN+SCANL,1                                                  
         BNE   CHKA180                                                          
         MVC   RSTAKSTA+4(1),SCANNAME+SCANL                                     
*                                                                               
CHKA180  EQU   *                                                                
         CLI   RSTAKSTA+4,C'T'     TEST TV                                      
         BNE   CHKA190             NO                                           
         MVI   RSTAKSTA+4,C' '                                                  
*                                                                               
CHKA190  GOTO1 ADATAMGR,DMCB,=C'DMREAD',=C'REPDIR',WORK,WORK                    
         CLI   DMCB+8,X'10'        TEST KEY FOUND                               
         BE    CHKA200             NO                                           
*                                                                               
         L     R3,FULL                                                          
         GOTO1 ADATAMGR,DMCB,=C'GETREC',=C'REPFIL',WORK+28,(R3),WORK+32         
         CLI   DMCB+8,0            GET REP STATION RECORD                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   FERN,NOTGRAPH                                                    
         MVI   BYTE,X'05'          EXTENDED DESCRIPTION ELEMENT                 
         BAS   RE,GETEL            LOOK FOR ELEMENT                             
         BNE   ERROR               NOT THERE - NOT A GRAPHNET STATION           
         USING RSTAXEL,R3                                                       
         CLC   RSTARID,=X'0406'    TEST GRAPHNET STATION                        
         BNE   ERROR               NO                                           
         DROP  R3                                                               
*                                                                               
*        MVI   FERN,NOTTWX                                                      
*        CLC   =C'TWX',FRMBOOK+3   TEST FORMAT=TWX                              
*        BNE   ERROR               NO - ERROR                                   
*                                                                               
         L     R7,ATIA             PUT REP D/A IN TIA AND GO TO                 
         MVC   0(4,R7),WORK+28     ADD/SUBTRACT IT FROM OUR DESTID              
         MVI   4(R7),X'FF'         BLOCK                                        
         MVI   TYPE,C'S'                                                        
         B     CHKA220                                                          
         DROP  R6                                                               
*                                                                               
CHKA200  MVC   KEY(2),SAVTKWID     OR LOOK FOR A LIST                           
         CLI   SCANMYMK,C'Y'                                                    
         BNE   *+10                                                             
         MVC   KEY(2),TWAUSRID                                                  
         MVC   KEY+2(6),SCANNAME                                                
         MVI   KEY+8,0                                                          
         GOTO1 =V(GETLIST),PARAS,KEY,ATIA,ADATAMGR,RR=RB                        
         MVI   FERN,LISTNXST                                                    
         TM    PARAS,X'10'          NOT FOUND                                   
         BO    ERROR                                                            
         MVI   FERN,LISTLEVS       TOO MANY LIST LEVELS                         
         TM    PARAS,4                                                          
         BO    ERROR                                                            
         MVI   FERN,INVALID                                                     
         CLI   PARAS,0                                                          
         BNE   ERROR                                                            
         MVI   FERN,NOQUALS                                                     
         CLC   SCANNUM(2),=X'0100' COPIES/CC'S INVALID FOR A LIST               
         BNE   ERROR                                                            
         MVI   TYPE,C'A'                                                        
*                                                                               
         ZIC   R1,PARAS+4          NUMBER OF ENTRIES IN LIST                    
         MH    R1,=H'4'                                                         
         L     R7,ATIA                                                          
         AR    R1,R7                                                            
         MVI   0(R1),X'FF'         NEW END OF REFORMATTED LIST                  
*                                                                               
         CLI   0(R7),X'FF'         BUMP TO END OF OLD FORMAT LIST               
         BE    CHKA210                                                          
         AH    R7,=H'2'                                                         
         B     *-12                                                             
*                                                                               
CHKA210  CR    R1,R7               TEST ANY MORE IDS TO MOVE                    
         BE    CHKA220             NO                                           
         SH    R1,=H'4'            BACK UP NEW POINTER                          
         SH    R7,=H'2'            BACK UP OLD POINTER                          
         MVC   0(2,R1),0(R7)       PUSH UP ID                                   
         XC    2(2,R1),2(R1)                                                    
         B     CHKA210                                                          
*                                                                               
CHKA220  L     R7,ATIA             ADD (IF NOT DUPLICATE), OR SUBTRACT          
         USING DID,R6              FROM/TO DESTIDS BLOCK                        
*                                                                               
CHKA230  CLI   0(R7),X'FF'         LOOP FOR AN INTERMEDIATE BLOCK ENTRY         
         BE    CHKA300                                                          
         L     R6,ADESTIDS                                                      
         CLI   DITYPE,C'S'         TEST REP STATION                             
         BE    CHKA240             YES                                          
         CLC   TWAUSRID,0(R7)      IGNORE SENDER UNLESS INDIVIDUALLY            
         BNE   CHKA240             SPECIFIED                                    
         C     R7,ATIA                                                          
         BNE   CHKA290                                                          
         CLI   4(R7),X'FF'                                                      
         BNE   CHKA290                                                          
*                                                                               
CHKA240  CLI   0(R6),0             LOOP FOR A DESTIDS ENTRY                     
         BNE   CHKA260             AT END SKIP IF '-', ADD IF '+'               
         CLI   SCANSIGN,C'-'                                                    
         BE    CHKA290                                                          
         MVI   FERN,TOOMANY                                                     
         C     R6,ADIDMAX                                                       
         BNL   ERROR                                                            
*                                                                               
         MVC   DITYPE,TYPE                                                      
         MVC   DICOPIES,SCANNUM                                                 
         MVI   DIERR,0                                                          
         CLI   DITYPE,C'S'         TEST REP STATION                             
         BNE   CHKA250                                                          
*                                                                               
         MVI   DIDLEN,DISHORT                                                   
         XC    DINUM,DINUM                                                      
         XC    DINAM,DINAM                                                      
         MVC   DIRSTADA,0(R7)                                                   
         B     CHKA290                                                          
*                                                                               
CHKA250  MVC   DINUM,0(R7)                                                      
         MVC   DINAM,SCANNAME                                                   
         MVC   DICCS(L'SCANCCS),SCANCCS                                         
         ZIC   RF,SCANCCSL         SET VARIABLE LENGTH DEPENDING ON             
         LA    RF,(DICCS-DID)(RF)  SIZE OF CC NAME STRING                       
         STC   RF,DIDLEN                                                        
         AR    RF,R6                                                            
         MVI   0(RF),0                                                          
         B     CHKA290                                                          
*                                                                               
CHKA260  CLI   DITYPE,C'S'         TEST REP STATION                             
         BNE   *+14                NO                                           
         CLC   DIRSTADA,0(R7)      YES - TEST MATCH ON DISK ADDRESS             
         B     *+10                                                             
         CLC   DINUM,0(R7)         ON MATCH SKIP IF '+', REMOVE IF'-'           
         BNE   CHKA280                                                          
         CLI   SCANSIGN,C'+'                                                    
         BE    CHKA290                                                          
         ZIC   R1,DIDLEN                                                        
         LA    R5,0(R1,R6)                                                      
CHKA270  IC    R1,0(R5)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R5)                                                    
         LTR   R1,R1                                                            
         BZ    CHKA290                                                          
         AR    R5,R1                                                            
         AR    R6,R1                                                            
         B     CHKA270                                                          
*                                                                               
CHKA280  ZIC   R1,DIDLEN           BUMP TO NEXT IN DESTIDS BLOCK                
         AR    R6,R1                                                            
         B     CHKA240                                                          
*                                                                               
CHKA290  LA    R7,4(R7)            BUMP TO NEXT IN INTERMEDIATE BLOCK           
         B     CHKA230                                                          
*                                                                               
CHKA300  LA    R8,SCANL(R8)        BUMP TO NEXT IN SCAND BLOCK                  
         CLI   SCANSIGN,C'-'       SKIP ENTRY IF BAND FOR STA CALLS             
         BNE   CHKA310                                                          
         CLI   SCANLEN,1                                                        
         BE    CHKA300                                                          
CHKA310  EQU   *                                                                
         ZIC   R1,SUBFNDX                                                       
         LA    R1,1(R1)                                                         
         STC   R1,SUBFNDX                                                       
         B     CHKA120                                                          
*                                                                               
CHKA320  MVI   SUBFNDX,0           COUNT NUMBER OF IDS                          
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         L     R6,ADESTIDS                                                      
CHKA330  CLI   0(R6),0                                                          
         BE    CHKA340                                                          
         LA    R1,1(R1)                                                         
         IC    R0,DIDLEN                                                        
         AR    R6,R0                                                            
         B     CHKA330                                                          
CHKA340  LTR   R1,R1                                                            
         MVI   FERN,NOADDEES       NONE                                         
         BZ    ERROR                                                            
         CH    R1,=H'200'                                                       
         MVI   FERN,EXADDEES       MORE THAN 200                                
         BH    ERROR                                                            
         STH   R1,DESTNUM                                                       
         B     OKXIT                                                            
         DROP  R6,R8                                                            
         EJECT                                                                  
*              ROUTINE TO HANDLE NESTED 'REPORT=' STRING IN AN                  
*              UNFORMATTED BOOK - VIA AN OVERLAY                                
*              ON ENTRY R3 = A(REPORT= STRING IN PRINT LINE)                    
*              ON EXIT  CC = EQU IF ERROR                                       
*                                                                               
KEYREPT  NTR1                                                                   
         ICM   RF,15,ABASE3                                                     
         BNZ   KYR10                                                            
         GOTO1 ACALLOV,PARAS,(32,0),0                                           
         CLI   PARAS+4,X'FF'                                                    
         BE    ERROR                                                            
         L     RF,PARAS                                                         
KYR10    GOTO1 (RF),PARAS,(RC)                                                  
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO COUNT PAGES/LINES/WORDS/CHARACTERS IN A KWX           
*              ON ENTRY PLB = PRINT LINE WITH SPACE AT +133                     
*                       R7  = A(ACCOUNT RECORD) COVERED BY KWXACD               
         SPACE 1                                                                
ACCOUNT  CLI   FIRSTID,C'Y'                                                     
         BNER  RE                                                               
         NTR1  BASE=ABASE2                                                      
         L     R9,A2NDBAS2                                                      
         USING KWXACD,R7                                                        
         LM    R1,R4,KWXAPAGE                                                   
         CLI   PLB,HEADOF                                                       
         BL    *+8                                                              
         LA    R1,1(R1)            INCREMENT PAGE COUNT                         
         LA    R5,PLB+1                                                         
         LA    RE,1                                                             
         LA    RF,PLB+133                                                       
         MVI   ACCMARK,0           SET MARKER TO INDICATE BLANK LINE            
         SPACE 1                                                                
ACC1     CLI   0(R5),C'A'          INCREMENT CHARACTER COUNT                    
         BL    ACC2                                                             
         MVI   ACCMARK,X'60'       X'40' = WORD, X'20' = LINE                   
         LA    R4,1(R4)                                                         
         B     ACCBXLE                                                          
         SPACE 1                                                                
ACC2     TM    ACCMARK,X'40'       INCREMENT WORD COUNT                         
         BZ    ACCBXLE                                                          
         LA    R3,1(R3)                                                         
         NI    ACCMARK,X'BF'                                                    
ACCBXLE  BXLE  R5,RE,ACC1                                                       
         SPACE 1                                                                
ACCEND   TM    ACCMARK,X'20'       INCREMENT LINE COUNT                         
         BZ    *+8                                                              
         LA    R2,1(R2)                                                         
         STM   R1,R4,KWXAPAGE                                                   
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
*              DATAMGR CALLS                                                    
*              ON EXIT CC = EQU AFTER AN ERROR                                  
*              AFTER PRINT PLB IS CLEARED TO SPACES                             
*              AFTER A PRINT ERROR ROUTINE RETURNS TO T255                      
         SPACE 1                                                                
*                                                                               
READ     NTR1                                                                   
         GOTO1 ADATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY,KEY,WORK                 
         CLI   DMCB+8,0                                                         
         BNE   ERROR                                                            
         B     OKXIT                                                            
WRITE    NTR1                                                                   
         LA    R2,=C'DMADD'                                                     
         LA    R3,=C'KWXFILE'                                                   
         LA    R4,DUB                                                           
DM2      LA    R5,KEY                                                           
         LA    R6,WORK                                                          
DMALL    STM   R2,R6,DMCB                                                       
         GOTO1 ADATAMGR,DMCB                                                    
         CLI   DMCB+8,0                                                         
         BE    OKXIT                                                            
         B     ERROR                                                            
         EJECT                                                                  
DOEDICT  NTR1                                                                   
*                                                                               
         L     R3,FULL                                                          
         LA    R3,1004(R3)                                                      
         MVI   0(R3),X'09'                                                      
         LA    R3,1(R3)                    SPACING CONTROL                      
         MVC   0(132,R3),SPACES                                                 
         USING EDIDDSID,R3                                                      
*                                                                               
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST,=C'RE'                                                   
         MVC   EDIPROG,=C'KWX'                                                  
         MVC   EDIIDEN,=C'TRN'                                                  
         L     RF,FULL             RESTORE A(IO AREA)                           
         USING RSTAREC,RF                                                       
         MVC   EDIRKXRP,RSTAKREP            REP CODE                            
         DROP  RF                                                               
         CLC   =C'O=',TWAACCS                                                   
         BNE   *+10                                                             
         MVC   EDIRKXOF,TWAACCS+2           OFFICE CODE                         
*                                                                               
         L     RE,ADESTIDS         PREPARE FOR DESTIDS LOOP                     
         USING DID,RE                                                           
         CLI   DITYPE,C'S'         TEST REP STATION                             
         DROP  RE                                                               
         BNE   DOED50              NOPE                                         
         L     RF,FULL             RESTORE A(IO AREA)                           
         USING RSTAREC,RF                                                       
         MVC   EDIRKXST(L'RSTAKSTA),RSTAKSTA                                    
         DROP  RF                                                               
DOED50   EQU   *                                                                
*                                                                               
         MVC   EDIRKXBC,MSGBOOK             KWX BOOK CODE                       
         LA    RF,PLKWX                                                         
         USING PQPLD,RF                                                         
         MVC   EDIRKXNM,QLDESC              KWX NUMBER                          
         DROP  RF                                                               
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
         DROP  R3                                                               
         EJECT                                                                  
PRINT    NTR1  BASE=ABASE2                                                      
         L     R9,A2NDBAS2                                                      
*                                                                               
DM3      CLI   PLB,INITIAL         PAGE CONTROL                                 
         BE    DM30                INITIALISE                                   
         CLI   PLB,TRMINATE                                                     
         BE    DM3B                TERMINATE                                    
         CLI   PLB,HEADOF                                                       
         BNE   *+14                                                             
DM30     ZAP   LINE,=P'0'                                                       
         B     DM3B                HEAD OF PAGE                                 
         AP    LINE,=P'1'                                                       
         CLI   PLB,SP1                                                          
         BNH   DM3A                SPACE 1 OR PRINT 1 SPACE 1                   
         AP    LINE,=P'1'                                                       
         CLI   PLB,PR1SP2                                                       
         BE    DM3A                SPACE 2 AFTER WRITE                          
         CLI   PLB,X'13'                                                        
         BE    DM3A                SPACE 2 IMMEDIATE                            
         AP    LINE,=P'1'                                                       
         CLI   PLB,PR1SP3                                                       
         BE    DM3A                SPACE 3                                      
         DC    H'0'                UNRECOGNISED CONTROL CHARACTER               
*                                                                               
DM3A     CP    LINE,MAXLINE        CHECK FOR PAGE FULL                          
         BL    DM3B                                                             
         ZAP   LINE,=P'0'                                                       
         MVC   WORK,SPACES                                                      
         MVI   WORK,HEADOF                                                      
         GOTO1 ADATAMGR,DMCB,=C'DMPRINT',=C'PRTQUE',0,WORK,ATIA                 
         MVC   PASAVE,WORK                                                      
         TM    DMCB+8,X'FE'                                                     
         BNZ   DMER                                                             
*                                                                               
DM3B     EQU   *                                                                
         GOTO1 ADATAMGR,DMCB,=C'DMPRINT',=C'PRTQUE',0,PLB,ATIA                  
         MVC   WORK(1),PLB                                                      
         USING PQPLD,R8                                                         
         LA    R8,PLB                                                           
         MVC   WORK+1(2),QLREPRNO                                               
         DROP  R8                                                               
*                                                                               
         MVC   PLB,SPACES                                                       
         TM    DMCB+8,X'FE'                                                     
         BNZ   *+14                                                             
         MVC   PBSAVE,WORK                                                      
         B     OKXIT                                                            
         CLI   PASAVE,TRMINATE                                                  
         BE    DM4                                                              
DMER     MVI   PLA,TRMINATE       IF ERROR DO TRMINATE READ                     
         GOTO1 APSEQ                                                            
DM4      L     RD,AREGSAV2                                                      
         LM    RE,RC,12(RD)                                                     
         TM    DMCB+8,X'80'                                                     
         BNO   T255                                                             
         MVI   FERN,ENDFILE                                                     
         B     ERROR                                                            
         SPACE 3                                                                
         GETEL R3,34,BYTE                                                       
         EJECT                                                                  
*              EXITS BACK TO ROOT                                               
*                                                                               
ERROR    SR    R0,R0               CC = EQU FOR ERROR                           
         B     EXIT                                                             
*                                                                               
MOREXIT  LNR   RB,RB               CC = NEG FOR MORE INPUT                      
*                                                                               
OKXIT    LTR   RB,RB               CC = POS OK COMMLETED                        
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
MAXLINE  DC    PL2'64'                                                          
       ++INCLUDE FACIDTAB                                                       
       ++INCLUDE FACIDTABD                                                      
         EJECT                                                                  
*              DSECT TO COVER A DESTID BLOCK ENTRY                              
*              USED FOR ADDRESSEES AND OTHER PARAMETERS INVOLVING USER          
*              IDS - EG AUTHORIZE                                               
*                                                                               
DID      DSECT                                                                  
DIDLEN   DS    CL1       B         LENGTH OF ENTRY - L'DICCS + 12               
DINUM    DS    CL2       B         USER ID NUMBER                               
DINAM    DS    CL6       C         USER ID NAME OR ADDRESSEE LIST NAME          
         ORG   DINUM                                                            
DIRSTADA DS    XL4       X         REP STATION RECORD DISK ADDRESS              
         ORG                                                                    
DITYPE   DS    CL1       C         I=ID, A=ADDRESSEE LIST, S=STATION            
DIERR    DS    CL1       C         Y=ERROR                                      
DICOPIES DS    CL1       B         1-9 PRINT QUEUE COPIES                       
DISHORT  EQU   *-DID               LENGTH WITHOUT DICCS                         
DICCS    DS    0C        C         STRING OF CC NAMES SEPARATED BY ','S         
         SPACE 3                                                                
*              DSECT TO COVER A PSEUDO-SCANNER BLOCK ENTRY BUILT IN             
*              PROCESSING OF A USER/ADDRESSEE STRING                            
*                                                                               
SCAND    DSECT                                                                  
SCANSIGN DS    CL1       C         +/-                                          
SCANLEN  DS    CL1       B         LENGTH OF NAME                               
SCANMYMK DS    CL1       C         Y=ADDRESSEE LIST IS UNDER MY ID              
SCANNAME DS    CL8       C         ID OR LIST NAME                              
SCANNUM  DS    CL1       B         1-9 PRINT QUEUE COPIES                       
SCANCCSL DS    CL1       B         SIGNIFICANT LENGTH OF SCANCCS                
SCANCCS  DS    CL40      C         STRING OF CC NAMES SEPARATED BY ','S         
SCANL    EQU   *-SCAND                                                          
         SPACE 3                                                                
       ++INCLUDE SRKWXACD                                                       
         SPACE 3                                                                
* NESTED INCLUDES                                                               
* REGENSTA                                                                      
* CTGENFILE                                                                     
* FAFACTS                                                                       
* DMPRTQL                                                                       
* GEKWXDSECT                                                                    
* EDIDDSHD                                                                      
* EDILINKD                                                                      
         PRINT OFF                                                              
RSTARECD DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE GEKWXDSECT                                                     
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
         PRINT ON                                                               
         SPACE 3                                                                
*              DSECT TO COVER A PRINT LINE FOR REP BILLING INFORMATION          
*                                                                               
GWS      DSECT                                                                  
         ORG   PLA                                                              
RBILL    DS    0CL134    C                                                      
RBILCC   DS    XL1       B         CARRIAGE CONTROL                             
RBILREP  DS    CL2       C     01  REP CODE                                     
RBILOFF  DS    CL2       C     03  OFFICE CODE                                  
RBILHDR  DS    CL5       C     05  '*HDR*'                                      
RBILDEST DS    CL24      C     10  DESTINATION CODE                             
         DS    CL1       C     34  ALWAYS BLANK                                 
         DS    CL1       C     35  SPARE                                        
RBILPAGE DS    CL1       C     36  P                                            
         DS    CL2       C     37  SPARE                                        
RBILFORM DS    CL16      C     39  SPARE                                        
RBILTEAM DS    CL3       C     55  TEAM CODE                                    
         DS    CL17            58                                               
RBILLAST DS    0CL8      C     75  DOT FILLED                                   
         DS    CL2       C                                                      
RBILPQNO DS    CL5       C         DDS PRINT QUEUE NUMBER                       
         DS    CL1       C                                                      
         ORG                                                                    
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039GEKWX0AS  05/01/02'                                      
         END                                                                    
