*          DATA SET RESFM33    AT LEVEL 099 AS OF 06/22/07                      
*PHASE T81833A                                                                  
         TITLE 'T81833 - RESFM33 - CONTRACT BUY COPY'                           
***********************************************************************         
*                                                                     *         
*  RESFM33 (T81833) --- CONTRACT BUY COPY'                            *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 01DEC95 RHV DATE OF BIRTH                                           *         
* 26FEB96 RHV COPY ORD SCRN ADV&AGY INFO TO TARGET K'S W/SAME STA.    *         
* 01MAR96 RHV LOCKOUT TARGET COMBO PARENT STATIONS                    *         
* 06MAR96 RHV SKIP SOURCE BUY COMBO PLACEHOLDERS                      *         
* 28MAR96 RHV SUPPORT X'40' & '41' & '0B' ELEMENTS                    *         
* 09SEP96 RHV FLAG BUYS MOVED BY BUYCOPY                              *         
* 31OCT96 RHV SUPPORT 'L' (LOW POWER) STATIONS                        *         
* 10JUL97 RHV FIX 8D/8E KEY GEN BUG                                   *         
* 09NOV97 JRD ALTERNATE CALENDAR BUCKETS                              *         
* 12MAY99 RHV REVISED FLIGHT DATES                                    *         
* 12JUL99 SKU FIX RBUYDTIN FLAGS                                      *         
* 04APR00 RHV NBC HOME MARKET SUPPORT                                 *         
* 15SEP00 SKU FIX LENGTH BUG                                          *         
* 11JAN01 RHV SPORTS BUYS                                             *         
* 09FEB01 BU  TRADE BUY FLAG SETTING                                  *         
* 18APR01 RHV CHECK TARGET CON VS. PAR DATA SECURITY RESTRICTIONS     *         
* 15MAY01 RHV COPY PGM NAME FIELD/FUNCTION                            *         
* 29MAY01 BU  DAILY PACING: SET TO REP PROFILE                        *         
* 11MAR02 RHV COPY COVERSHEET NAME FIELD/FUNCTION                     *         
* 05APR05 HQ  FIX ALTERNATE WEEK # BUG                                *         
* 15MAR07 HQ  FIX OFFTEAM COMPARISON FOR 1 CHARACTER TEAM             *         
* 22JUN07 SKU FIX BUYLINE ADDED FLAGGING                              *         
*                                                                     *         
***********************************************************************         
T81833   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 4000,*T81833*,R7,RR=R3                                           
         LR    R0,RC               JRD - NEEDED A SAFE K READ                   
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         ST    R0,AIO5                                                          
*                                                                               
*                                  ESTABLISH A(REPFACS)                         
         GOTO1 CALLOV,DMCB,0,X'D9000AAC',0                                      
         MVC   VREPFACS,0(R1)                                                   
*                                                                               
         L     RE,ATWA             PUT TABLES IN TWA+8,000                      
*                                  (USED TO SAY '10,000)                        
         AH    RE,=H'8000'                                                      
         USING TABLES,RE                                                        
         LA    RF,STATAB                                                        
         ST    RF,ASTATAB               SAVE A(TARGET STATION TABLE)            
         LA    RF,CONTAB                                                        
         ST    RF,ACONTAB               SAVE A(TARGET CONTRACT TABLE)           
         LA    RF,REPPROF                                                       
         ST    RF,AREPPROF              SAVE A(REP PROFILE BYTE)                
         DROP  RE                                                               
         AH    RE,=H'6000'         PUT A(WORKSPACE FOR BUCKUP) AT 14K           
         ST    RE,AWRKSPAC                                                      
*                                                                               
         CLI   BCPSTAT,0                                                        
         BNE   *+12                                                             
         MVI   STAT,0                                                           
         MVI   BCPSTAT,1                                                        
*                                                                               
         MVI   LOCALMKT,C'N'       INITIALIZE LOCAL MKT REPS                    
         XC    WORK,WORK                                                        
         GOTO1 (RFGPROF,VREPFACS),DMCB,('RREPQCNT',WORK),0,0,RFBLOCK            
         TM    WORK+2+CNTRPEAB,CNTRPEAA   CONTRACT PROF #46                     
         BZ    MAIN050             NO - SKIP ADDITIONAL CHECK                   
*                                                                               
         XC    WORK,WORK                                                        
         GOTOX (RFGETID,VREPFACS),DMCB,(RA),WORK,0,RFBLOCK                      
         CLI   WORK+4,C'L'         5 POSITION OF SIGNON = 'L'?                  
         BNE   *+8                                                              
         MVI   LOCALMKT,C'Y'       HOME MARKET IN PROGRESS                      
*                                                                               
MAIN050  DS    0H                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BNE   *+8                                                              
         BAS   RE,VK                                                            
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   *+8                                                              
         BAS   RE,PR                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                        *         
***********************************************************************         
VK       NTR1                                                                   
*                                                                               
         TM    STAT,FAILBUY+FAILACL                                             
         BNZ   INFEND                                                           
*                                                                               
VK005    LA    R2,BCPCONH               VALIDATE SOURCE CONTRACT                
         CLI   5(R2),0                  TRAP ZERO LEN                           
         BNE   VK010                                                            
         MVC   RERROR,=AL2(INFCON)                                              
         B     INFEND                                                           
*                                                                               
VK010    TM    4(R2),X'08'              TRAP NOT NUMERIC                        
         BO    VK020                                                            
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
VK020    EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,1               SET TO READ REP RECORD                       
         MVC   KEY+25(2),AGENCY    INSERT AGENCY CODE                           
         GOTO1 HIGH                READ REP RECORD                              
         CLC   KEY(27),KEYSAVE     REP RECORD FOUND?                            
         BE    *+6                 YES                                          
         DC    H'0'                NO  - MUST BE ON FILE                        
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC              PUT REP REC IN IO2                           
         L     R6,AIO              SET A(RECORD)                                
         USING RREPREC,R6                                                       
         L     R1,AREPPROF         SET A(REP PROFILE BYTE)                      
         MVC   0(1,R1),RREPPROF+27 SAVE DAILY PACING BYTE                       
*                                                                               
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY                  BUILD KEY FOR CONTRACT LOOKUP           
         LA    R6,KEY                                                           
         USING RCONREC,R6                                                       
         MVI   RCONPTYP,X'8C'           REC TYPE                                
         MVC   RCONPREP,AGENCY          REP CODE                                
         ZIC   R4,5(R2)                 CONVERT CON NUM TO 9'S COMP.            
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5)                                              
         MVO   WORK(5),WORK+10(5)                                               
         MVC   CON9S,WORK            SAVE CONTRACT NUM                          
         PACK  CONREV+3(1),CON9S(1)  SAVE REVERSED 9'S COMP NUMBER              
         PACK  CONREV+2(1),CON9S+1(1)                                           
         PACK  CONREV+1(1),CON9S+2(1)                                           
         PACK  CONREV(1),CON9S+3(1)                                             
         MVC   RCONPCON,WORK            9'S COMPLIMENT CONTRACT NUM             
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCONKEY),KEYSAVE CONTRACT FOUND?                           
         BE    VK030                                                            
         MVC   RERROR,=AL2(ERRCON)      NO - CONTRACT NOT FOUND ERR             
         B     MYERR                                                            
VK030    MVC   AIO,AIO2                                                         
         GOTO1 GETREC                   PUT SOURCE CONTRACT REC IN IO2          
         L     R6,AIO                                                           
         MVC   CONREG,RCONKCON                                                  
         MVI   SMEDIA,C'T'              DEFUALT VALUE FOR SOURCE MEDIA          
         CLI   RCONKSTA+4,C'A'          IS RADIO STATION?                       
         BNE   *+8                                                              
         MVI   SMEDIA,C'R'                                                      
         CLI   RCONKSTA+4,C'F'          IS RADIO STATION?                       
         BNE   *+8                                                              
         MVI   SMEDIA,C'R'                                                      
         MVC   BCPCSTA(4),RCONKSTA      SHOW SOURCE STA ONSCREEN                
         OI    BCPCSTAH+6,X'80'                                                 
         CLI   BCPCSTA+5,C' '                                                   
         BNE   *+14                                                             
         MVI   BCPCSTA+5,C'-'                                                   
         MVC   BCPCSTA+6(1),RCONKSTA+5                                          
*                                                                               
         XC    SDEVSAL,SDEVSAL                                                  
         XC    SDEVTYP,SDEVTYP                                                  
         L     R3,AIO2                                                          
         MVI   ELCODE,X'18'                                                     
         BAS   RE,GETEL                                                         
         BNE   VK030A                                                           
         USING RCONDVEL,R3                                                      
         MVC   SDEVSAL,RCONDVSP                                                 
         MVC   SDEVTYP,RCONDVCT                                                 
         DROP  R3                                                               
*                                                                               
VK030A   XC    KEY,KEY                  LOOKUP STATION REC FOR MKT              
         LA    R3,KEY                                                           
         USING RSTAREC,R3                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,AGENCY                                                  
         MVC   RSTAKSTA,RCONKSTA                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVC   BCPCMKT,RSTAMKT          SHOW MARKET ONSCREEN                    
         OI    BCPCMKTH+6,X'80'                                                 
         DROP  R3                                                               
*                                                                               
         CLC   RCONPRD,SPACES           IS THERE A PRODUCT CODE                 
         BNE   VK032                    YES, GO LOOKUP PRODUCT REC              
         L     R3,AIO2                  SOURCE CONTRACT                         
         MVI   ELCODE,X'05'             EXPANSION ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   VK031                    IF NOT THERE, BLANK FIELD               
         USING RCONEXEL,R3                                                      
         MVC   BCPCPRD,RCONEXPR         PRODUCT NAME                            
         DROP  R3                                                               
         B     *+10                                                             
VK031    MVC   BCPCPRD,SPACES                                                   
         OI    BCPCPRDH+6,X'80'                                                 
         B     VK035                                                            
VK032    XC    KEY,KEY                  LOOKUP PRODUCT REC                      
         LA    R3,KEY                                                           
         USING RPRDREC,R3                                                       
         MVI   RPRDKTYP,X'09'                                                   
         MVC   RPRDKADV,RCONKADV                                                
         MVC   RPRDKPRD,RCONPRD                                                 
         MVC   RPRDKREP,AGENCY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'RPRDKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVC   BCPCPRD,RPRDNAME         SHOW PRODUCT ONSCREEN                   
         OI    BCPCPRDH+6,X'80'                                                 
         DROP  R3                                                               
*                                                                               
VK035    XC    KEY,KEY                  LOOKUP ADV REC                          
         LA    R3,KEY                                                           
         USING RADVREC,R3                                                       
         MVI   RADVKTYP,X'08'                                                   
         MVC   RADVKADV,RCONKADV                                                
         MVC   RADVKREP,AGENCY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'RADVKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVC   BCPCADV,RADVNAME         SHOW ADVERTISER ONSCREEN                
         OI    BCPCADVH+6,X'80'                                                 
         DROP  R3                                                               
*                                                                               
         XC    KEY,KEY                  LOOKUP AGENCY                           
         LA    R3,KEY                                                           
         USING RAGYREC,R3                                                       
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKAGY,RCONKAGY                                                
         MVC   RAGYKAOF,RCONKAOF                                                
         MVC   RAGYKREP,AGENCY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'RAGYKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO3                                                         
         L     R3,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   BCPCAGY,RAGYNAM1         SHOW AGENCY NAME ONSCREEN               
         OI    BCPCAGYH+6,X'80'                                                 
         DROP  R3                                                               
*                                                                               
         TM    STAT,REFRESH                                                     
         BNO   VK037                                                            
***>     BAS   RE,SS                    SHOW FULL SCREEN                        
         GOTO1 =A(SS),RR=Y              SHOW FULL SCREEN                        
         OI    BCPDATH+6,X'40'          CURSOR ON DATE                          
         OI    BCPDATH+6,X'80'                                                  
         NI    STAT,X'FF'-REFRESH       SCREEN DISPLAYED FLAG                   
         MVC   RERROR,=AL2(INFREQ)                                              
         B     INFEND                                                           
*                                                                               
VK037    XC    KEY,KEY                  CHECK SOURCE SALESMAN REC               
         LA    R3,KEY                                                           
         USING RSALREC,R3                                                       
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,AGENCY                                                  
         MVC   RSALKSAL,RCONSAL                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSALKEY),KEYSAVE   SHOULD REALLY BE THERE!                 
         MVC   RERROR,=AL2(ERRSALF)                                             
         BNE   MYERR                                                            
         MVC   AIO,AIO3                                                         
         L     R3,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   SALLEAV,RSALLEAV                                                 
         MVC   SALOFF,RSALOFF                                                   
         MVC   SALTEAM,RSALTEAM                                                 
         MVC   SALNAME,RSALNAME                                                 
         DROP  R3                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,TODAY)                                      
         SPACE 1                                                                
         XC    KEY,KEY                  LOOKUP BUY REC FOR CONTRACT             
         LA    R3,KEY                                                           
         USING RBUYREC,R3                                                       
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,AGENCY                                                  
         MVC   RBUYKCON,CONREV                                                  
         GOTO1 HIGH                            IS THERE AT LEAST 1              
         CLC   KEY(RBUYKPLN-RBUYREC),KEYSAVE   BUY REC FOR THIS CON             
         BE    VK040                                                            
         MVC   RERROR,=AL2(ERRNOBUY)    NO - ERROR                              
         B     MYERR                                                            
         DROP  R3                                                               
         SPACE 1                                                                
VK040    LA    R2,BCPDATH               VALIDATE HEADLINE DATES HERE            
         CLI   5(R2),0                                                          
         BE    VK075                                                            
VK050    GOTO1 SCANNER,DMCB,BCPDATH,(2,WORK2),C',=-='                           
         CLI   DMCB+4,2                                                         
         BE    VK060                                                            
         MVC   RERROR,=AL2(ERRDAT)      INVALID DATE RANGE                      
         B     MYERR                                                            
VK060    MVC   RERROR,=AL2(ERRSDT)                                              
         GOTO1 DATVAL,DMCB,WORK2+12,WORK                                        
         OC    DMCB(4),DMCB             VALID START DATE?                       
         BZ    MYERR                    NO - ERROR                              
         GOTO1 DATCON,DMCB,WORK,(3,SDATE)                                       
         SPACE 1                                                                
         MVC   RERROR,=AL2(ERREDT)                                              
         GOTO1 DATVAL,DMCB,WORK2+44,WORK+6                                      
         OC    DMCB(4),DMCB             VALID END DATE?                         
         BZ    MYERR                    NO - ERROR                              
         GOTO1 DATCON,DMCB,WORK+6,(3,EDATE)                                     
         SPACE 1                                                                
         MVC   RERROR,=AL2(ERRSVE)                                              
         CLC   EDATE,SDATE              END DATE VS. START DATE                 
         BL    MYERR                                                            
         SPACE 1                                                                
         SR    R4,R4                    COUNT K WEEKS                           
         MVC   WORK+12(6),WORK                                                  
VK070    LA    R4,1(R4)                                                         
         GOTO1 ADDAY,DMCB,WORK+12,WORK+18,7                                     
         MVC   WORK+12(6),WORK+18                                               
         CLC   WORK+12(6),WORK+6                                                
         BNH   VK070                                                            
         STC   R4,KWEEKS                                                        
         SPACE 1                        CALC MONTHS IN DATE RANGE               
         GOTO1 GETBROAD,DMCB,(1,WORK),WORK+12,GETDAY,ADDAY                      
         GOTO1 (RF),(R1),(1,WORK+6),WORK+24                                     
         GOTO1 DATCON,(R1),WORK+18,(3,DUB)                                      
         GOTO1 (RF),(R1),WORK+30,(3,DUB+3)                                      
         IC    R4,DUB                                                           
         LA    R4,1(R4)                                                         
         STC   R4,DUB                                                           
         MVC   RERROR,=AL2(ERRCAL)                                              
         CLC   DUB+3(2),DUB                                                     
         BH    MYERR                    GREATER THAN 1 YR                       
         SPACE 1                                                                
VK075    LA    R2,BCPCCCH               VALIDATE COPY CONTRACT COMMENTS         
         CLI   5(R2),0                                                          
         BNE   VK080                                                            
         MVI   8(R2),CCCDEF                                                     
         OI    6(R2),X'80'                                                      
VK080    CLI   8(R2),C'Y'                                                       
         BE    VK090                                                            
         CLI   8(R2),C'N'                                                       
         BE    VK090                                                            
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
VK090    LA    R2,BCPCBCH               VALIDATE COPY BUYLINE COMMENTS          
         CLI   5(R2),0                                                          
         BNE   VK100                                                            
         MVI   8(R2),CBCDEF                                                     
         OI    6(R2),X'80'                                                      
VK100    CLI   8(R2),C'Y'                                                       
         BE    VK110                                                            
         CLI   8(R2),C'N'                                                       
         BE    VK110                                                            
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
VK110    LA    R2,BCPCPGH               VALIDATE COPY PROGRAM NAME              
         CLI   5(R2),0                                                          
         BNE   VK120                                                            
         MVI   8(R2),CPGDEF                                                     
         OI    6(R2),X'80'                                                      
VK120    CLI   8(R2),C'Y'                                                       
         BE    VK130                                                            
         CLI   8(R2),C'N'                                                       
         BE    VK130                                                            
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
VK130    LA    R2,BCPCPCH               VALIDATE COPY COVERSHEET                
         CLI   5(R2),0                                                          
         BNE   VK140                                                            
         MVI   8(R2),CPCDEF                                                     
         OI    6(R2),X'80'                                                      
VK140    CLI   8(R2),C'Y'                                                       
         BE    VK150                                                            
         CLI   8(R2),C'N'                                                       
         BE    VK150                                                            
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
VK150    EQU   *                                                                
         SPACE 1                                                                
*                                                                               
* LOOP THRU COPY TARGETS ON SCREEN                                              
* VALIDATE TARGETS & BUILD TARGET CONTRACT/STATION TABLES                       
*                                                                               
         LA    R2,BCPCON1H              FIRST TARGET CON/STA                    
         MVI   TNUM,0                   ZERO TARGET COUNTER                     
         L     R4,ACONTAB                                                       
         ST    R4,CONPTR                SAVE A(CON TABLE)                       
         L     R4,ASTATAB                                                       
         ST    R4,STAPTR                SAVE A(STA TABLE)                       
         LA    R4,BCPCONXH                                                      
         ST    R4,LASTCON               SAVE A(LAST TWA FIELD)                  
         SPACE 1                                                                
         USING BCPCON1H,R2                                                      
VK500    CLI   5(R2),0                  FIELD BLANK?                            
         BNE   VK505                    NO - VALIDATE                           
         MVI   BCPSA1,0                 YES - CLEAR S/A & SKIP                  
         OI    BCPSA1H+6,X'80'                                                  
         B     VK900                                                            
VK505    CLI   WHEN,X'40'               'NOW' REQUEST?                          
         BNE   VK510                    NO - OK TO RUN MORE THAN MAX            
         MVC   RERROR,=AL2(ERRONLN)                                             
         CLI   TNUM,MAXONLN             MAX NUMBER OF TARGETS REACHED?          
         BE    MYERR                                                            
VK510    TM    4(R2),X'08'              TARGET IS CONTRACT NUMBER               
         BO    VK600                                                            
         B     VK700                    TARGET IS STATION                       
         MVC   RERROR,=AL2(ERRTARG)                                             
         B     MYERR                                                            
         SPACE 1                                                                
VK600    EQU   *                        VALIDATE TARGET CONTRACT                
         CLI   BCPSA1H+5,0              ZERO LEN SA FIELD?                      
         BNE   VK605                    NO - VALIDATE                           
         MVI   BCPSA1,SADEF             YES - PROVIDE DEFAULT                   
         OI    BCPSA1H+6,X'80'                                                  
         B     VK610                                                            
VK605    CLI   BCPSA1,C'S'                                                      
         BE    VK610                                                            
         CLI   BCPSA1,C'A'                                                      
         BE    VK610                                                            
         LA    R2,BCPSA1H                                                       
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         SPACE 1                                                                
VK610    XC    KEY,KEY                  BUILD KEY FOR CONTRACT LOOKUP           
         LA    R3,KEY                                                           
T        USING RCONREC,R3                                                       
         MVI   T.RCONPTYP,X'8C'         REC TYPE                                
         MVC   T.RCONPREP,AGENCY        REP CODE                                
         ZIC   R4,5(R2)                 CONVERT CON NUM TO 9'S COMP.            
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5)                                              
         MVO   WORK(5),WORK+10(5)                                               
         MVC   T.RCONPCON,WORK          9'S COMPLIMENT CONTRACT NUM             
         GOTO1 HIGH                                                             
         CLC   KEY(L'T.RCONKEY),KEYSAVE CONTRACT FOUND?                         
         BE    VK615                                                            
         MVC   RERROR,=AL2(ERRCON)      NO - CONTRACT NOT FOUND ERR             
         B     MYERR                                                            
VK615    MVC   AIO,AIO1                                                         
         GOTO1 GETREC                   PUT TARGET CONTRACT REC IN IO1          
         L     R3,AIO                                                           
         CLI   SMEDIA,C'T'              SOURCE MATCH TARGET MEDIA?              
         BNE   VK617                                                            
         CLI   T.RCONKSTA+4,C'T'                                                
         BE    VK619                                                            
         CLI   T.RCONKSTA+4,C' '                                                
         BE    VK619                                                            
         CLI   T.RCONKSTA+4,C'L'                                                
         BE    VK619                                                            
         MVC   RERROR,=AL2(ERRMED)                                              
         B     MYERR                                                            
VK617    CLI   T.RCONKSTA+4,C'F'                                                
         BE    VK619                                                            
         CLI   T.RCONKSTA+4,C'A'                                                
         BE    VK619                                                            
         MVC   RERROR,=AL2(ERRMED)                                              
         B     MYERR                                                            
VK619    CLC   T.RCONKOFF,RCONKOFF      SAME OFFICE AS SOURCE?                  
         BE    VK620                                                            
         MVC   RERROR,=AL2(ERROFF)                                              
         B     MYERR                                                            
VK620    CLC   T.RCONKAGY,RCONKAGY      SAME AGENCY AS SOURCE                   
         BE    VK625                                                            
         MVC   RERROR,=AL2(ERRAGY)                                              
         B     MYERR                                                            
VK625    CLC   T.RCONKAOF,RCONKAOF      SAME AGENCY OFFICE AS SOURCE            
         BE    VK630                                                            
         MVC   RERROR,=AL2(ERRAOF)                                              
         B     MYERR                                                            
VK630    CLC   T.RCONKADV,RCONKADV      SAME ADVERTISER AS SOURCE               
         BE    VK630A                                                           
         MVC   RERROR,=AL2(ERRADV)                                              
         B     MYERR                                                            
VK630A   CLC   T.RCONKCON,RCONKCON      SAME SOURCE & TARGET CON                
         BNE   VK631                                                            
         MVC   RERROR,=AL2(ERRTVS)                                              
         B     MYERR                                                            
VK631    CLC   T.RCONTYPE,RCONTYPE      SAME CONTRACT TYPE                      
         BE    VK632                                                            
         MVC   RERROR,=AL2(ERRTYP)                                              
         B     MYERR                                                            
VK632    CLC   T.RCONSAL,RCONSAL        SAME SALESPERSON                        
         BE    VK633                                                            
         MVC   RERROR,=AL2(ERRSAL)                                              
         B     MYERR                                                            
VK633    L     R3,AIO1                                                          
         MVC   RERROR,=AL2(ERRDEV)                                              
         MVI   ELCODE,X'18'                                                     
         BAS   RE,GETEL                                                         
         BE    VK634                                                            
         CLC   SDEVSAL,=X'000000'                                               
         BNE   MYERR                                                            
         CLC   SDEVTYP,=X'0000'                                                 
         BNE   MYERR                                                            
         B     VK635                                                            
         PUSH  USING                                                            
         USING RCONDVEL,R3                                                      
VK634    CLC   RCONDVSP,SDEVSAL                                                 
         BNE   MYERR                                                            
         CLC   RCONDVCT,SDEVTYP                                                 
         BNE   MYERR                                                            
         DROP  R3                                                               
VK635    L     R3,AIO1                                                          
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   VK638                    EL NOT FOUND, OK                        
         USING RCONSEND,R3                                                      
         CLC   RCONSSID,=X'0000'        IF REP SENDING ID NULL THEN             
         BE    VK638                    CONTRACT HAS NOT BEEN SENT              
         MVC   RERROR,=AL2(ERRSND)                                              
         B     MYERR                                                            
         DROP  R3                                                               
VK638    L     R3,AIO1                  IS TARGET CON PART OF COMBO?            
         MVI   ELCODE,X'17'                                                     
         BAS   RE,GETEL                                                         
         BNE   VK640                                                            
         MVC   RERROR,=AL2(ERRTCOM)                                             
         B     MYERR                                                            
         POP   USING                                                            
*                                                                               
VK640    XC    WORK,WORK                BUILD PAR DATA SECURITY BLOCK           
         LA    R1,WORK                                                          
         L     R3,AIO1                                                          
         USING SBLOCK,R1                                                        
         MVC   SBSTATN(L'RSTAKSTA),T.RCONKSTA   CHECK TARGET STATION            
         MVC   SBSALES(L'RCONSAL),T.RCONSAL     AND SALESMAN                    
         DROP  R1                                                               
         GOTO1 (RFCKSEC,REPFACS),DMCB,WORK,CONHEADH,ATIOB,RFBLOCK               
         BE    VK645                    PASS PAR DATA SECURITY?                 
         OI    6(R2),X'40'+X'80'        NO!                                     
         L     RD,BASERD                                                        
         B     EXIT                                                             
*                                                                               
VK645    L     R3,AIO1                                                          
         L     R4,ACONTAB                                                       
VK650    C     R4,CONPTR                                                        
         BE    VK660                                                            
         CLC   T.RCONKCON,0(R4)                                                 
         BNE   VK655                                                            
         MVC   RERROR,=AL2(ERRDUP)                                              
         B     MYERR                                                            
VK655    LA    R4,TABCONQ(R4)                                                   
         B     VK650                                                            
VK660    L     R4,CONPTR                                                        
         MVC   0(4,R4),T.RCONKCON       PUT CON IN TARGET CON TABLE             
         MVC   4(1,R4),BCPSA1           PUT S/A OPT IN TABLE                    
         MVI   5(R4),0                  ZERO CON STATUS BYTE IN TABLE           
         MVC   6(5,R4),T.RCONKSTA       PUT TARGET K STA IN TABLE               
         MVC   11(6,R4),T.RCONDATE      PUT TARGET K S&E DATES IN TABLE         
         L     R3,AIO1                                                          
         MVI   ELCODE,X'1E'                                                     
         BAS   RE,GETEL                                                         
         BNE   VK670                                                            
         LA    R3,RCONRFLT-RCONRFEL(R3) REVISED DATE                            
         OC    0(L'RCONRFLT,R3),0(R3)   HAVE ONE?                               
         BZ    VK670                                                            
         MVC   11(6,R4),0(R3)           YES - USE IT                            
VK670    L     R3,AIO1                  BACK TO CONTRACT                        
         MVI   17(R4),0                 ZERO FLAG BYTE#2                        
         LA    R4,TABCONQ(R4)                                                   
         ST    R4,CONPTR                                                        
         ZIC   R4,TNUM                  INCREMENT TARGET COUNTER                
         LA    R4,1(R4)                                                         
         STC   R4,TNUM                                                          
         B     VK900                                                            
         DROP  T                                                                
*                                                                               
VK700    EQU   *                                                                
         CLI   BCPDATH+5,0                                                      
         BNE   VK702                                                            
         LA    R2,BCPDATH                                                       
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
VK702    MVC   STATION,SPACES           VALIDATE TARGET STATION                 
         MVI   BCPSA1,0                                                         
         OI    BCPSA1H+6,X'80'                                                  
         GOTO1 SCANNER,DMCB,(R2),(1,WORK),C',=,-'                               
         MVC   RERROR,=AL2(ERRSTA)                                              
         TM    WORK+2,X'40'                                                     
         BNO   MYERR                                                            
         ZIC   R4,WORK                  L'FIRST INPUT FIELD                     
         CH    R4,=H'3'                 AT LEAST 3 CALL LETTERS?                
         BL    MYERR                                                            
         CH    R4,=H'4'                 MORE THAN 4 CALL LETTERS?               
         BH    MYERR                                                            
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   STATION(0),WORK+12       SAVE STATION CALL LETTERS               
         CLI   WORK+1,0                 ZERO LENGTH BAND?                       
         BE    VK705                                                            
         CLI   WORK+1,1                 ONLY 1 CHAR BAND ALLOWED                
         BNE   MYERR                                                            
         CLI   WORK+22,C'F'                                                     
         BE    VK705                                                            
         CLI   WORK+22,C'A'                                                     
         BE    VK705                                                            
         CLI   WORK+22,C'C'                                                     
         BE    VK705                                                            
         CLI   WORK+22,C'L'                                                     
         BE    VK705                                                            
         CLI   WORK+22,C'T'                                                     
         BNE   MYERR                                                            
         MVI   WORK+22,C' '                                                     
VK705    MVC   STATION+4,WORK+22                                                
*                                                                               
         CLI   SALLEAV,0               IS THERE A SALESMAN LEAVE DATE?          
         BE    VK708                                                            
         CLC   SALLEAV,TODAY           SALESMAN LEAVE DATE VS TODAY             
         BH    VK708                                                            
         MVC   RERROR,=AL2(ERRSALD)                                             
         LA    R2,BCPCONH                                                       
         B     MYERR                                                            
*                                                                               
VK708    MVC   AIO,AIO1                 USE IO1 FOR TARGET STA RECS             
         MVC   RERROR,=AL2(ERRSTAF)     DEFAULT ERR MSG                         
         XC    CBUFF,CBUFF              CLEAR COMBO BUFFER                      
         LA    R5,CBUFF                                                         
         CLI   STATION+4,C'C'           IS STATION A COMBO?                     
         BE    VK710                                                            
         MVC   0(5,R5),STATION          NO - PUT STA IN BUFFER                  
         LA    R5,5(R5)                                                         
         MVI   0(R5),X'FF'              END BUFFER                              
         MVI   BUFFNUM,1                1 STATION IN BUFFER                     
         B     VK730                                                            
VK710    EQU   *                                                                
         MVC   RERROR,=AL2(ERRCOMT)    *******CHANGE*********                   
         B     MYERR                   *TARGET COMBO LOCKOUT*                   
*                                      **********************                   
         XC    KEY,KEY                  BUILD KEY FOR COMBO PARENT              
         LA    R3,KEY                                                           
         USING RSTAREC,R3                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,AGENCY                                                  
         MVC   RSTAKSTA,STATION                                                 
         DROP  R3                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTAKEY),KEYSAVE   COMBO ON FILE?                          
         BE    VK715                                                            
         B     MYERR                                                            
VK715    GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         SR    R1,R1                    CLEAR COUNTER                           
         MVC   RERROR,=AL2(ERRCOMB)     USE COMBO ERR MSG                       
         MVI   ELCODE,X'0A'             GET COMBO STATION EL                    
         BAS   RE,GETEL                                                         
         BNE   MYERR                    SHOULD HAVE AT LEAST 1 CHILD            
         USING RSTACSEL,R3                                                      
VK725    MVC   0(5,R5),RSTACS           PUT CHILD CALL LTRS IN BUFFER           
         LA    R1,1(R1)                 INCREMENT COUNTER                       
         LA    R5,5(R5)                 INCREMENT POINTER                       
         BAS   RE,NEXTEL                                                        
         BE    VK725                    LOOP THRU ALL STATIONS IN COMBO         
         MVI   0(R5),X'FF'              MARK END OF TABLE                       
         STC   R1,BUFFNUM               SAVE COUNTER                            
         DROP  R3                                                               
*                                                                               
VK730    LA    R5,CBUFF                                                         
VK735    CLI   0(R5),X'FF'              END OF COMBO BUFFER?                    
         BE    VK900                    YES  - DONE                             
         XC    KEY,KEY                  BUILD KEY FOR STATION                   
         LA    R3,KEY                                                           
         USING RSTAREC,R3                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,AGENCY                                                  
         MVC   RSTAKSTA,0(R5)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTAKEY),KEYSAVE   STATION ON FILE?                        
         BNE   MYERR                                                            
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         CLI   SMEDIA,C'R'              SOURCE MATCH TARGET MEDIA?              
         BNE   VK737                                                            
         CLI   RSTAKSTA+4,C'A'                                                  
         BE    VK739                                                            
         CLI   RSTAKSTA+4,C'F'                                                  
         BE    VK739                                                            
         MVC   RERROR,=AL2(ERRMED)                                              
         B     MYERR                                                            
VK737    CLI   RSTAKSTA+4,C'T'                                                  
         BE    VK739                                                            
         CLI   RSTAKSTA+4,C'L'                                                  
         BE    VK739                                                            
         CLI   RSTAKSTA+4,C' '                                                  
         BE    VK739                                                            
         MVC   RERROR,=AL2(ERRMED)                                              
         B     MYERR                                                            
VK739    CLI   RSTAEND,0                IS THERE A STATION LEAVE DATE?          
         BE    VK740                                                            
         CLC   RSTAEND,EDATE            STATION LEAVE VS. HEAD EDATE            
         MVC   RERROR,=AL2(ERREND)                                              
         BNH   MYERR                                                            
VK740    CLC   RSTASTRT,SDATE           STATION JOIN VS. HEAD SDATE             
         MVC   RERROR,=AL2(ERRSTRT)                                             
         BH    MYERR                                                            
         TM    RSTASTAT,X'40'           STATION LOCK OUT?                       
         MVC   RERROR,=AL2(ERRLOK)                                              
         BO    MYERR                                                            
*                                                                               
         L     R6,AIO2                                                          
         XC    WORK,WORK                BUILD PAR DATA SECURITY BLOCK           
         LA    R1,WORK                                                          
         USING SBLOCK,R1                                                        
         MVC   SBSTATN(L'RSTAKSTA),RSTAKSTA   CHECK TARGET STATION              
         MVC   SBSALES(L'RCONSAL),RCONSAL     AND SALESMAN                      
         DROP  R1                                                               
*                                                                               
         GOTO1 (RFCKSEC,REPFACS),DMCB,WORK,CONHEADH,ATIOB,RFBLOCK               
         BE    VK742                    PASS PAR DATA SECURITY?                 
         OI    6(R2),X'40'+X'80'        NO!                                     
         L     RD,BASERD                                                        
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
VK742    MVI   OTFLAG,0                 VALIDATE OFFICE-TEAM HERE               
         MVC   RERROR,=AL2(ERROT)                                               
         USING RSTAOTEL,R3                                                      
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         B     VK750                                                            
VK745    BAS   RE,NEXTEL                                                        
VK750    BNE   VK760                                                            
         CLC   RSTAOTOF,SALOFF          MATCH ON OFFICE?                        
         BNE   VK745                                                            
         OC    RSTAOTTM,SPACES                                                  
         MVC   FULL(L'RSTAOTTM),RSTAOTTM                                        
         OC    FULL(L'RSTAOTTM),SPACES                                          
         CLC   FULL(L'RSTAOTTM),SALTEAM MATCH ON TEAM?                          
         BE    VK765                                                            
         MVI   OTFLAG,X'FF'                                                     
         B     VK745                                                            
VK760    CLI   OTFLAG,X'FF'                                                     
         BE    MYERR                                                            
         DROP  R3                                                               
*                                                                               
VK765    L     R4,STAPTR                                                        
         MVC   0(5,R4),0(R5)            PUT STATION FROM CBUFF IN TAB           
         MVC   5(1,R4),BUFFNUM          NUMBER OF COMBO SIBLINGS                
         LA    R4,TABSTAQ(R4)           INCREMENT STA LIST PTR                  
         ST    R4,STAPTR                SAVE STATION LIST PTR                   
         CLI   WHEN,X'40'               'NOW' REQUEST?                          
         BNE   VK770                    NO - OK TO RUN MORE THAN MAX            
         CLI   TNUM,MAXONLN             MAX NUMBER OF TARGETS REACHED?          
         BNE   VK770                                                            
         MVC   RERROR,=AL2(ERRONLN)                                             
         B     MYERR                                                            
VK770    ZIC   R4,TNUM                  INCREMENT TARGET COUNTER                
         LA    R4,1(R4)                                                         
         STC   R4,TNUM                                                          
         LA    R5,5(R5)                 NEXT STATION IN COMBO BUFFER            
         B     VK735                                                            
*                                                                               
         DROP  R2                                                               
         SPACE 1                                                                
VK900    C     R2,LASTCON                                                       
         BE    VK910                                                            
         LA    R2,CONLEN(R2)                                                    
         B     VK500                                                            
*                                                                               
VK910    EQU   *                                                                
         CLI   TNUM,0                                                           
         BNE   VK915                                                            
         CLI   STAT,0                                                           
         BNE   VK912                                                            
         LA    R2,BCPDATH                                                       
         MVC   RERROR,=AL2(INFREQ)                                              
         B     INFEND                                                           
VK912    LA    R2,BCPCON1H                                                      
         MVC   RERROR,=AL2(ERRNOT)                                              
         B     MYERR                                                            
*                                                                               
VK915    L     R4,CONPTR                CLOSE UP TABLES & LEAVE                 
         MVI   0(R4),X'FF'                                                      
         L     R4,STAPTR                                                        
         MVI   0(R4),X'FF'                                                      
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+8                                                              
         OI    STAT,NOPAUSE                                                     
*                                                                               
         TM    STAT,NOPAUSE                                                     
         BO    VKX                                                              
         OI    STAT,NOPAUSE                                                     
         LA    R2,CONSERVH                                                      
         OI    BCPCONH+6,X'01'                                                  
         OI    BCPCONH+6,X'80'                                                  
         MVC   RERROR,=AL2(INFCONFM)                                            
         B     INFEND                                                           
VKX      NI    STAT,X'FF'-NOPAUSE                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS REPORT                                                      *         
***********************************************************************         
PR       NTR1                                                                   
*                                                                               
* COUNT NUMBER OF SOURCE BUYLINES                                               
* LOOP THRU TARGET STATION TABLE, CREATE NEW CONTRACT RECS                      
* APPEND NEW CONTRACT RECS TO TARGET CONTRACT TABLE                             
* ASSIGN #OF EXISTING BUYS TO S/A FIELD IN TABLE ENTRIES                        
* IF EXISTING BUYS NOT = 0 FOR S SELECTIONS, ENTER X'FF'                        
* TEST NUM OF EXISTING BUYS TO CONFIRM SPACE LEFT FOR ALL SOURCE BUYS           
*                                                                               
         XC    KEY,KEY                  READ SOURCE K INTO IO2                  
         LA    R6,KEY                                                           
         USING RCONREC,R6                                                       
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,AGENCY                                                  
         MVC   RCONPCON,CON9S                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         DROP  R6                                                               
*                                                                               
* COUNT NUMBER OF SOURCE BUYLINES ***********************************           
*                                                                               
         SR    R4,R4                                                            
         XC    KEY,KEY                  READ ALL SOURCE BUY RECS                
         LA    R3,KEY                                                           
         USING RBUYREC,R3                                                       
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,AGENCY                                                  
         MVC   RBUYKCON,CONREV                                                  
         GOTO1 HIGH                                                             
PR010    CLC   KEY(RBUYKPLN-RBUYKEY),KEYSAVE                                    
         BNE   PR015                                                            
         CLC   RBUYKMLN,RBUYKLIN        DO NOT COUNT MAKEGOODS                  
         BNE   PR011                                                            
         CLC   RBUYKMLN(2),=X'FFFF'     DO NOT COUNT PLAN RECORD                
         BE    PR011                                                            
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R3,AIO3                                                          
         CLI   RBUYCHGI,C'C'            SKIP CANCELLED LINES                    
         BE    PR011                                                            
         LA    R4,1(R4)                 COUNT+1                                 
PR011    GOTO1 SEQ                                                              
         LA    R3,KEY                                                           
         B     PR010                                                            
         DROP  R3                                                               
PR015    STC   R4,SBUYS                 # OF BUYS IN 'SBUYS'                    
*                                                                               
* LOOP THRU TARGET CON TABLE, CHECK ROOM LEFT FOR APPEND BUYS                   
*                                                                               
         L     R4,ACONTAB                                                       
PR020    CLI   0(R4),X'FF'                                                      
         BE    PR040                                                            
*                                                                               
         SR    R0,R0                    CONVERT TARGET CON NUMBER               
         L     R1,0(R4)                                                         
         SLDA  R0,4                                                             
         STM   R0,R1,DUB                                                        
         OI    DUB+7,X'0C'              PACKED W/SIGN                           
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5)                                              
         MVO   WORK(5),WORK+10(5)                                               
         MVC   TCON9S,WORK              SAVE 9'S COMP CON NUMBER                
         PACK  TCONREV+3(1),TCON9S(1)   SAVE REVERSED 9'S COMP NUMBER           
         PACK  TCONREV+2(1),TCON9S+1(1)                                         
         PACK  TCONREV+1(1),TCON9S+2(1)                                         
         PACK  TCONREV(1),TCON9S+3(1)                                           
*                                                                               
         MVI   SKPFLAG,0                                                        
         CLI   4(R4),C'S'               IF SKIP OPTION SELECTED                 
         BNE   *+8                                                              
         MVI   SKPFLAG,C'Y'             TURN ON SKIP OPTION FLAG                
         MVI   4(R4),0                  ZERO & USE S/A BYTE AS COUNTER          
*                                                                               
         OI    DMINBTS,X'08'            FIND DELETED RECS ALSO                  
         XC    KEY,KEY                  LOOKUP ANY EXISTING TARGET BUYS         
         LA    R3,KEY                                                           
         USING RBUYREC,R3                                                       
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,AGENCY                                                  
         MVC   RBUYKCON,TCONREV                                                 
         GOTO1 HIGH                                                             
PR025    CLC   KEY(RBUYKPLN-RBUYKEY),KEYSAVE   BUY FOR THIS K ?                 
         BNE   PR030                                                            
         CLC   RBUYKMLN(2),=X'FFFF'     SKIP PLAN RECORDS                       
         BE    PR026                                                            
         CLI   SKPFLAG,C'Y'                                                     
         BNE   PR025A                                                           
         TM    KEY+27,X'80'             DELETED?                                
         BO    PR025A                                                           
         OI    5(R4),SKIP               TRIP SKIP FLAG FOR THIS K               
         B     PR035                                                            
PR025A   CLC   RBUYKLIN,4(R4)                                                   
         BNH   PR026                                                            
         MVC   4(1,R4),RBUYKLIN         LAST USED BUYLINE NUM IN S/A            
PR026    GOTO1 SEQ                                                              
         B     PR025                                                            
         DROP  R3                                                               
*                                                                               
PR030    EQU   *                                                                
         ZIC   R0,4(R4)                 HIGHEST BUY IN TARGET K                 
         ZIC   R2,SBUYS                 HIGHEST BUY IN SOURCE K                 
         AR    R0,R2                                                            
         CH    R0,=H'255'               DOEST TOTAL EXCEED 255 MAX?             
         BNH   *+8                                                              
         OI    5(R4),NOROOM             TRIP NO ROOM FLAG FOR THIS K            
PR035    LA    R4,TABCONQ(R4)                                                   
         B     PR020                                                            
*                                                                               
PR040    EQU   *                                                                
*                                                                               
* LOOP THRU TARGET STATION TABLE, CREATE NEW CONTRACT RECS ***********          
* APPEND NEW CONTRACT RECS TO TARGET CONTRACT TABLE ******************          
*                                                                               
PR100    EQU   *                                                                
         L     R4,ASTATAB               TARGET STATION TABLE                    
*                                                                               
PR105    CLI   0(R4),X'FF'              END OF TABLE?                           
         BE    PR190                                                            
*                                                                               
* FIND LAST USED K NUMBER, ADD NUM OF STATIONS IN COMBO                         
         LA    R3,KEY                   LOOKUP NEXT AVAIL K NUM                 
         XC    KEY,KEY                                                          
         USING RCONREC,R3                                                       
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,AGENCY                                                  
         OI    DMINBTS,X'08'            FIND DELETED RECS ALSO                  
         MVI   RDUPDATE,C'Y'            READ FOR UPDATE                         
         GOTO1 HIGH                                                             
         CLC   KEY(RCONPCON-RCONREC),KEYSAVE                                    
         BE    *+6                                                              
         DC    H'0'                     HAS TO BE SOMETHING OUT THERE           
*                                                                               
         SR    R0,R0                    CONVERT K NUM                           
         ICM   R1,15,RCONPCON                                                   
         DROP  R3                                                               
         SLDA  R0,4                                                             
         STM   R0,R1,DUB                                                        
         OI    DUB+7,X'0C'              PWS 9'S COMP                            
         CVB   R5,DUB                                                           
*                                                                               
* BUILD LIST OF RELATED STATIONS & K NUMBERS IN CBUFF TO ENABLE                 
* CREATION OF X'17' COMBO CONTROL ELEMENT IN K RECORD LATER                     
         XC    CBUFF,CBUFF                                                      
         LA    R1,CBUFF                                                         
         ZIC   R2,5(R4)                 # OF STATIONS IN COMBO                  
         LR    RE,R5                                                            
         SR    RE,R2                                                            
PR106    CVD   RE,DUB                                                           
*                                                                               
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5)                                              
         MVO   WORK(5),WORK+10(5)                                               
         MVC   0(4,R1),WORK             PWOS K NUM                              
         AH    RE,=H'1'                                                         
         LA    R1,9(R1)                                                         
         BCT   R2,PR106                                                         
         MVI   0(R1),X'FF'                                                      
*                                                                               
         LA    R1,CBUFF                                                         
         ZIC   R2,5(R4)                 # OF STATIONS IN COMBO                  
         LR    RE,R4                                                            
PR107    MVC   4(5,R1),0(RE)                                                    
         LA    R1,9(R1)                                                         
         LA    RE,TABSTAQ(RE)                                                   
         BCT   R2,PR107                                                         
*                                                                               
         ZIC   R2,5(R4)                 REVERSE LIST ORDER                      
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
PR108    SH    R1,=H'9'                                                         
         MVC   0(9,RE),0(R1)                                                    
         LA    RE,9(RE)                                                         
         BCT   R2,PR108                                                         
         MVI   0(RE),X'FF'                                                      
         XC    CBUFF,CBUFF                                                      
         MVC   CBUFF,WORK               DONE CREATING LIST                      
*                                                                               
         ZIC   R2,5(R4)                 # OF STATIONS IN COMBO                  
         SR    R5,R2                    SUBTRACT # OF STATIONS IN COMBO         
PR110    CVD   R5,DUB                                                           
         MVC   WORK+10(5),DUB+3                                                 
         MVO   WORK(5),WORK+10(5)                                               
         MVC   TCON9S,WORK              PWOS 9'S COMP K NUM                     
                                                                                
         ST    R4,STAPTR                SAVE STATION TABLE LOCATION             
         GOTO1 =A(ADDK),DMCB,(RC),RR=RELO    ***ADD NEW CONTRACT                
         AH    R5,=H'1'                 NEXT K NUM                              
         LA    R4,TABSTAQ(R4)           NEXT TARGET STATION TABLE ENTRY         
         BCT   R2,PR110                 LOOP FOR # OF STATIONS IN COMBO         
*                                       (1 IF NOT COMBO)                        
         B     PR105                                                            
PR190    EQU   *                                                                
*                                                                               
         XC    BYTE,BYTE           SET FLAG                                     
****COPY BUYS****                                                               
         GOTO1 =A(BILDCOPY),DMCB,(RC),RR=RELO                                   
*                                                                               
****UPDATE TARGET CONTRACTS                                                     
         BAS   RE,UC                                                            
*                                                                               
****UPDATE TARGET COVERSHEETS                                                   
         CLI   BCPCPC,C'Y'                                                      
         BNE   *+8                                                              
         BAS   RE,UV                                                            
*                                                                               
****DISPLAY RESULTS                                                             
         CLI   OFFLINE,C'N'                                                     
         BNE   *+8                                                              
         BAS   RE,DR                                                            
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+8                                                              
         BAS   RE,RP                                                            
*                                                                               
PRX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
* UPDATE CONTRACTS (TARGET) ROUTINE                                   *         
***********************************************************************         
UC       NTR1                                                                   
*                                                                               
         L     R4,ACONTAB                                                       
UC020    CLI   0(R4),X'FF'              END OF TAB?                             
         BE    UCX                                                              
*                                                                               
         TM    17(R4),NEWCON            IF NEW K, ALREADY BEEN DONE             
         BO    UC120                                                            
*                                                                               
         CLI   5(R4),0                  ONLY SUCCESSFUL K'S                     
         BNE   UC120                                                            
*                                                                               
         L     R3,AIO2                  SOURCE K                                
         USING RCONREC,R3                                                       
*                                                                               
         CLC   RCONKSTA,6(R4)           SOURCE & TARGET STATION MATCH           
         BNE   UCX                                                              
*                                                                               
         SR    R5,R5                    USE AS INDICATOR FOR LATER              
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL                                                         
         BNE   UC025                                                            
         LA    R5,2(R3)                                                         
         DROP  R3                                                               
*                                                                               
UC025    DS    0H                                                               
         XC    KEY,KEY                  LOOKUP TARGET K                         
         LA    R3,KEY                                                           
         USING RCONREC,R3                                                       
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,AGENCY                                                  
         SR    R0,R0                    CONVERT K NUM                           
         ICM   R1,15,0(R4)                                                      
         SLDA  R0,4                                                             
         STM   R0,R1,DUB                                                        
         OI    DUB+7,X'0C'              PWS K NUM                               
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5)                                              
         MVO   WORK(5),WORK+10(5)                                               
         MVC   RCONPCON,WORK            PWOS 9'S COMP K NUM                     
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
*                                                                               
         TM    RCONMODR,X'30'      BUY 1 ADDED?                                 
         BNZ   UC030               YES ALREADY                                  
         OI    RCONMODR,X'30'      ELSE MARK IT SO                              
         MVC   RCONCREA,TODAY      AND STAMP DATE                               
         DROP  R3                                                               
*                                                                               
UC030    DS    0H                                                               
         LTR   R5,R5               HAVE 9F ELEM IN SOURCE?                      
         BZ    UC090               NO                                           
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL                                                         
         BNE   UC050                                                            
*                                                                               
         USING RCONXXEL,R3                                                      
         MVC   RCONXADV,0(R5)                                                   
         MVC   RCONXAGY,10(R5)                                                  
         B     UC090                                                            
         DROP  R3                                                               
*                                                                               
UC050    XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING RCONXXEL,R3                                                      
         MVI   RCONXXEL,X'9F'                                                   
         MVI   RCONXXEL+1,40                                                    
         MVC   RCONXADV,0(R5)                                                   
         MVC   RCONXAGY,10(R5)                                                  
         GOTO1 ADDELEM                                                          
         DROP  R3                                                               
*                                                                               
UC090    EQU   *                                                                
         L     R3,AIO2                  SOURCE K                                
         USING RCONREC,R3                                                       
*                                                                               
         CLI   RCONKSTA+4,C'A'     RADIO STATION?                               
         BE    UC110               YES - DON'T CHANGE FLAG                      
         CLI   RCONKSTA+4,C'F'     RADIO STATION?                               
         BE    UC110               YES - DON'T CHANGE FLAG                      
         MVI   ELCODE,X'1E'        FIND 1E ELEMENT                              
         BAS   RE,GETEL                                                         
         BE    UC100               FOUND                                        
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING RCONRFEL,R3                                                      
         MVI   RCONRFEL,X'1E'                                                   
         MVI   RCONRFEL+1,22                                                    
         OC    RCONRF1,BYTE        ADD TRADE FLAG FOR TV ORDERS                 
*                                     BYTE MAY/MAY NOT BE TRADE                 
         GOTO1 ADDELEM                                                          
         B     UC110                                                            
         DROP  R3                                                               
UC100    EQU   *                                                                
         OC    2(1,R3),BYTE        ADD TRADE FLAG FOR TV ORDERS                 
*                                     BYTE MAY/MAY NOT BE TRADE                 
*                                                                               
UC110    EQU   *                                                                
         GOTO1 PUTREC                                                           
*                                                                               
UC120    LA    R4,TABCONQ(R4)                                                   
         B     UC020                                                            
*                                                                               
UCX      XIT1                                                                   
*                                                                               
***********************************************************************         
* UPDATE COVERSHEETS ROUTINE                                          *         
***********************************************************************         
UV       NTR1                                                                   
*                                                                               
         L     R4,ACONTAB                                                       
UV020    CLI   0(R4),X'FF'              END OF TAB?                             
         BE    UVX                                                              
*                                                                               
         CLI   5(R4),0                  ONLY SUCCESSFUL K'S                     
         BNE   UV120                                                            
*                                                                               
         XC    KEY,KEY                  BUILD KEY FOR 1ST SOURCE REC            
         LA    R3,KEY                                                           
         USING RCOVREC,R3                                                       
         MVI   RCOVKTYP,X'49'                                                   
         MVI   RCOVKNAM,X'FF'                                                   
         MVC   RCOVKNAM+4(4),CONREG                                             
         MVC   RCOVKREP,AGENCY                                                  
UV030    GOTO1 HIGH                                                             
         CLC   KEY(L'RCOVKEY),KEYSAVE                                           
         BNE   UV080                                                            
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVC   RCOVKNAM+4(4),0(R4)    REC FOR NEW K                             
*                                                                               
         LA    R3,KEY                                                           
         MVC   RCOVKNAM+4(4),0(R4)    TARGET KEY                                
         OI    DMINBTS,X'88'       UPDATE & DELETES                             
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCOVKEY),KEYSAVE                                           
         BNE   UV050               NEW REC                                      
         NI    KEY+27,X'FF'-X'80'  MAKE SURE NOT DELETED                        
         GOTO1 WRITE                                                            
         MVC   AIO,AIO3                                                         
         OI    DMINBTS,X'88'       UPDATE & DELETES                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         GOTO1 PUTREC              OVERWRITE W/SOURCE REC                       
         B     UV060                                                            
*                                                                               
UV050    DS    0H                                                               
         GOTO1 ADDREC                                                           
*                                                                               
UV060    DS    0H                  NEXT SOURCE RECORD                           
         MVC   KEY,KEYSAVE         WHERE WE LEFT OFF                            
         MVC   RCOVKNAM+4(4),CONREG                                             
         CLI   RCOVKSEQ,255                                                     
         BE    UV100                                                            
         ZIC   R1,RCOVKSEQ                                                      
         LA    R1,1(R1)            NEXT REC                                     
         STC   R1,RCOVKSEQ                                                      
         B     UV030                                                            
*                                                                               
UV080    DS    0H                  DELETE OLD RECS ON TARGET                    
         MVC   KEY,KEYSAVE                                                      
         MVC   RCOVKNAM+4(4),0(R4)                                              
         OI    DMINBTS,X'80'       UPDATE                                       
UV085    GOTO1 HIGH                                                             
         CLC   KEY(L'RCOVKEY),KEYSAVE                                           
         BNE   UV100               NO OLD RECS TO DELETE                        
         OI    KEY+27,X'80'        DELETE                                       
         GOTO1 WRITE                                                            
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         OI    29(R1),X'80'        DELETE                                       
         GOTO1 PUTREC                                                           
         CLI   RCOVKSEQ,255                                                     
         BE    UV100                                                            
         ZIC   R1,RCOVKSEQ                                                      
         LA    R1,1(R1)            NEXT REC                                     
         STC   R1,RCOVKSEQ                                                      
         B     UV085                                                            
         DROP  R3                                                               
*                                                                               
UV100    DS    0H                                                               
         XC    KEY,KEY                  LOOKUP TARGET K                         
         LA    R3,KEY                                                           
         USING RCONREC,R3                                                       
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,AGENCY                                                  
         SR    R0,R0                    CONVERT K NUM                           
         ICM   R1,15,0(R4)                                                      
         SLDA  R0,4                                                             
         STM   R0,R1,DUB                                                        
         OI    DUB+7,X'0C'              PWS K NUM                               
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5)                                              
         MVO   WORK(5),WORK+10(5)                                               
         MVC   RCONPCON,WORK            PWOS 9'S COMP K NUM                     
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         OI    DMINBTS,X'80'       UPDATE                                       
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVI   ELCODE,X'A6'                                                     
         BAS   RE,GETEL                                                         
         BNE   UV105                                                            
         GOTO1 REMELEM             DELETE OLD COVERSHEET ELEM                   
*                                                                               
UV105    DS    0H                                                               
         L     R3,AIO2                  SOURCE K                                
         BAS   RE,GETEL                                                         
         BNE   UV110               NO ELEM ON SOURCE                            
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING RCONCVEL,R1                                                      
         MVI   RCONCVCD,X'A6'                                                   
         MVI   RCONCVLN,RCONCVLQ                                                
         MVI   RCONCVNM,X'FF'                                                   
         MVC   RCONCVNM+4(4),0(R4)                                              
         GOTO1 ADDELEM                                                          
         DROP  R1                                                               
*                                                                               
UV110    DS    0H                                                               
         GOTO1 PUTREC                                                           
*                                                                               
UV120    LA    R4,TABCONQ(R4)                                                   
         B     UV020                                                            
         DROP  R3                                                               
*                                                                               
UVX      XIT1                                                                   
*                                                                               
***********************************************************************         
* DISPLAY RESULTS ONSCREEN (ONLINE VERSION)                           *         
***********************************************************************         
*                                                                               
DR       NTR1                                                                   
*                                                                               
         LA    R3,BCPCONXH              LOOP TO CLEAR & PROTECT BOTTOM          
         LA    R2,BCPCON1H              OF SCREEN                               
         USING BCPCON1H,R2                                                      
DR010    XC    BCPCON1,BCPCON1          CLEAR FIELD                             
         OI    BCPCON1H+1,X'20'         PROTECT FIELD                           
         OI    BCPCON1H+6,X'80'                                                 
         MVI   BCPSA1,0                                                         
         OI    BCPSA1H+1,X'20'                                                  
         OI    BCPSA1H+6,X'80'                                                  
         CR    R2,R3                                                            
         LA    R2,BCPCON2-BCPCON1(R2)                                           
         BNE   DR010                                                            
         DROP  R2                                                               
*                                                                               
         MVC   M4,BCPM4                                                         
         XC    BCPM4,BCPM4                                                      
*                                                                               
         MVC   M5,BCPM5                                                         
         XC    BCPM5,BCPM5                                                      
         OI    BCPM5H+6,X'80'                                                   
*                                                                               
         MVC   M6,BCPM6                                                         
         XC    BCPM6,BCPM6                                                      
*                                                                               
         MVC   BCPM4(L'SCRN1),SCRN1                                             
         OI    BCPM4H+6,X'80'                                                   
         MVC   BCPM6(L'SCRN2),SCRN2                                             
         OI    BCPM6H+6,X'80'                                                   
*                                                                               
         OI    BCPDATH+1,X'20'                                                  
         OI    BCPDATH+6,X'80'                                                  
*                                                                               
         OI    BCPCCCH+1,X'20'                                                  
         OI    BCPCCCH+6,X'80'                                                  
*                                                                               
         OI    BCPCBCH+1,X'20'                                                  
         OI    BCPCBCH+6,X'80'                                                  
*                                                                               
         OI    BCPCPGH+1,X'20'                                                  
         OI    BCPCPGH+6,X'80'                                                  
*                                                                               
         OI    BCPCPCH+1,X'20'                                                  
         OI    BCPCPCH+6,X'80'                                                  
*                                                                               
         L     R4,ACONTAB                                                       
         LA    R2,BCPCON1H                                                      
         USING BCPCON1H,R2                                                      
DR015    CLI   0(R4),X'FF'                                                      
         BE    DR050                                                            
*                                                                               
         SR    R0,R0                    CONVERT K NUM                           
         ICM   R1,15,0(R4)                                                      
         SLDA  R0,4                                                             
         STM   R0,R1,DUB                                                        
         OI    DUB+7,X'0F'              PWS                                     
         UNPK  BCPCON2,DUB                                                      
         OI    BCPCON2H+6,X'80'                                                 
*                                                                               
         MVC   BCPCON3(4),6(R4)                                                 
         OI    BCPCON3H+6,X'80'                                                 
         CLI   10(R4),C' '                                                      
         BE    DR020                                                            
         MVI   BCPCON3+4,C'-'                                                   
         MVC   BCPCON3+5(1),10(R4)                                              
*                                                                               
DR020    MVC   BCPCON4,=C'COMPLETE'                                             
         OI    BCPCON4H+6,X'80'                                                 
         TM    5(R4),SKIP                                                       
         BNO   DR021                                                            
         MVC   BCPCON4,=C'SKIPPED '                                             
DR021    TM    5(R4),NOROOM                                                     
         BNO   DR022                                                            
         MVC   BCPCON4,=C'NO ROOM '                                             
DR022    TM    5(R4),FAILBUY+FAILACL                                            
         BZ    DR030                                                            
         MVC   BCPCON4,=C'*FAILED*'                                             
         DROP  R2                                                               
*                                                                               
         TM    5(R4),FAILBUY                                                    
         BNO   *+8                                                              
         OI    STAT,FAILBUY                                                     
*                                                                               
         TM    5(R4),FAILACL                                                    
         BNO   *+8                                                              
         OI    STAT,FAILACL                                                     
*                                                                               
DR030    LA    R4,TABCONQ(R4)                                                   
         LA    R2,BCPCONR-BCPCON1(R2)                                           
         B     DR015                                                            
*                                                                               
DR050    TM    STAT,FAILBUY+FAILACL                                             
         BZ    DR060                                                            
         MVC   RERROR,=AL2(INFFAIL)                                             
         B     INFEND                                                           
*                                                                               
DR060    MVC   RERROR,=AL2(INFCMPO)                                             
         OI    STAT,MORE                                                        
         OI    STAT,REFRESH             REQUEST SCREEN REFRESH                  
         OI    BCPCONH+6,X'01'                                                  
         OI    BCPCONH+6,X'80'                                                  
         LA    R2,BCPCONH                                                       
         B     INFEND                                                           
*                                                                               
DRX      EQU   *                                                                
***********************************************************************         
* PRINT RESULT REPORT                                                 *         
***********************************************************************         
*                                                                               
RP       NTR1                                                                   
*                                                                               
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HHOOK                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         L     R4,ACONTAB                                                       
RP015    CLI   0(R4),X'FF'                                                      
         BE    RP050                                                            
*                                                                               
         TM    17(R4),ISCOMBO                                                   
         BNO   RP016A                                                           
         MVC   P+10(5),=C'COMBO'                                                
         B     RP017                                                            
RP016A   TM    17(R4),NEWCON                                                    
         BNO   RP016B                                                           
         MVC   P+10(7),=C'STATION'                                              
         B     RP017                                                            
RP016B   MVC   P+10(6),=C'NUMBER'                                               
*                                                                               
RP017    SR    R0,R0                    CONVERT K NUM                           
         ICM   R1,15,0(R4)                                                      
         SLDA  R0,4                                                             
         STM   R0,R1,DUB                                                        
         OI    DUB+7,X'0F'              PWS                                     
         UNPK  P+26(8),DUB                                                      
*                                                                               
         MVC   P+39(4),6(R4)                                                    
         CLI   10(R4),C' '                                                      
         BE    RP018                                                            
         MVI   P+43,C'-'                                                        
         MVC   P+44(1),10(R4)                                                   
*                                                                               
RP018    GOTO1 DATCON,DMCB,(X'13',11(R4)),(11,P+51)                             
*                                                                               
         CLI   5(R4),0                                                          
         BNE   RP020                                                            
         MVC   P+75(13),=C'COPY COMPLETE'                                       
         B     RP030                                                            
RP020    TM    5(R4),SKIP                                                       
         BNO   RP021                                                            
         MVC   P+75(20),=C'**CONTRACT SKIPPED**'                                
RP021    TM    5(R4),NOROOM                                                     
         BNO   RP022                                                            
         MVC   P+75(11),=C'**NO ROOM**'                                         
RP022    TM    5(R4),FAILBUY+FAILACL                                            
         BZ    RP030                                                            
         MVC   P+75(17),=C'** COPY FAILED **'                                   
*                                                                               
         TM    5(R4),FAILBUY                                                    
         BZ    *+8                                                              
         OI    STAT,FAILBUY                                                     
*                                                                               
         TM    5(R4),FAILACL                                                    
         BZ    *+8                                                              
         OI    STAT,FAILACL                                                     
*                                                                               
RP030    GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R4,TABCONQ(R4)                                                   
         B     RP015                                                            
*                                                                               
RP050    GOTO1 SPOOL,DMCB,(R8)                                                  
         TM    STAT,FAILBUY+FAILACL                                             
         BZ    RP060                                                            
         MVC   P(L'MSGFAIL),MSGFAIL                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
*                                                                               
RP060    MVC   P(L'MSGOK),MSGOK                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
*                                                                               
RPX      EQU   *                                                                
*                                                                               
***********************************************************************         
*                                                                               
HEDSPECS DS    0H                                                               
         SPROG 0                                                                
         PSPEC H1,1,AGYNAME                                                     
         PSPEC H1,38,C'BUYCOPY RESULT REPORT'                                   
         PSPEC H2,38,C'---------------------'                                   
         PSPEC H2,1,AGYADD                                                      
         PSPEC H1,76,RUN                                                        
         PSPEC H1,103,PAGE                                                      
         PSPEC H2,76,REQUESTOR                                                  
         PSPEC H3,1,SPACES                                                      
         PSPEC H4,1,SPACES                                                      
         SPACE 1                                                                
         DC    X'00'                                                            
***********************************************************************         
* HEADHOOK ROUTINE                                                    *         
***********************************************************************         
*                                                                               
HHOOK    NTR1                                                                   
*                                                                               
         L     R3,AIO2                                                          
         USING RCONREC,R3                                                       
*                                                                               
         MVC   H5(16),=C'SOURCE CONTRACT:'                                      
         MVC   H5+18(8),BCPCON                                                  
         MVC   H5+35(8),=C'STATION:'                                            
         MVC   H5+44(6),BCPCSTA                                                 
         MVC   H5+68(7),=C'MARKET:'                                             
         MVC   H5+76(20),BCPCMKT                                                
         MVC   H6+35(7),=C'AGENCY:'                                             
         MVC   H6+43(20),BCPCAGY                                                
         MVC   H6+68(11),=C'ADVERTISER:'                                        
         MVC   H6+80(20),BCPCADV                                                
*                                                                               
         LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         USING ROFFREC,R5                                                       
         MVI   ROFFKTYP,X'04'                                                   
         MVC   ROFFKREP,AGENCY                                                  
         MVC   ROFFKOFF,RCONKOFF                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'ROFFKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R5,AIO                                                           
*                                                                               
         MVC   H7+35(12),=C'SALESPERSON:'                                       
         MVC   H7+48(20),SALNAME                                                
         MVC   H7+68(7),=C'OFFICE:'                                             
         MVC   H7+76(20),ROFFNAME                                               
         DROP  R5                                                               
         MVC   H8+35(8),=C'PRODUCT:'                                            
         MVC   H8+44(20),BCPCPRD                                                
         MVC   H9+35(14),=C'CONTRACT TYPE:'                                     
         MVC   H9+50(1),RCONTYPE                                                
         MVC   H8+68(16),=C'DEV SALESPERSON:'                                   
         MVC   H9+68(9),=C'DEV TYPE:'                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING RDSPREC,R5                                                       
         MVI   RDSPKTYP,X'3A'                                                   
         MVC   RDSPKREP,AGENCY                                                  
         MVC   RDSPKSAL,SDEVSAL                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDSPKEY),KEYSAVE                                           
         BNE   HH100                                                            
         GOTO1 GETREC                                                           
         L     R5,AIO                                                           
         MVC   H8+85(20),RDSPNAME                                               
         DROP  R5                                                               
*                                                                               
HH100    XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING RDCTREC,R5                                                       
         MVI   RDCTKTYP,X'3B'                                                   
         MVC   RDCTKREP,AGENCY                                                  
         MVC   RDCTKCTY,SDEVTYP                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDCTKEY),KEYSAVE                                           
         BNE   HH110                                                            
         GOTO1 GETREC                                                           
         L     R5,AIO                                                           
         MVC   H9+78(20),RDCTDESC                                               
         DROP  R5                                                               
*                                                                               
HH110    CLC   PAGE,=X'0001'                                                    
         BE    *+10                                                             
         MVC   H6+5(11),=C'(CONTINUED)'                                         
*                                                                               
         MVI   H10,0                                                            
         MVI   H11,0                                                            
         MVC   H13+10(11),=C'TARGET TYPE'                                       
         MVC   H14+10(11),=C'-----------'                                       
         MVC   H13+26(8),=C'CONTRACT'                                           
         MVC   H14+26(8),=C'--------'                                           
         MVC   H13+39(7),=C'STATION'                                            
         MVC   H14+39(7),=C'-------'                                            
         MVC   H13+51(14),=C'HEADLINE DATES'                                    
         MVC   H14+51(17),=C'-----------------'                                 
         MVC   H13+75(6),=C'RESULT'                                             
         MVC   H14+75(18),=C'------------------'                                
*                                                                               
         DROP  R3                                                               
*                                                                               
HHOOKX   B     EXIT                                                             
*                                                                               
***********************************************************************         
*                                                                               
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGE EQUATES                                                         
*                                                                               
ERRCON   EQU   525                                                              
ERRNOBUY EQU   526                                                              
ERRDAT   EQU   527                                                              
ERRSDT   EQU   79                                                               
ERREDT   EQU   80                                                               
ERRSVE   EQU   64                                                               
ERRCAL   EQU   528                                                              
ERRONLN  EQU   529                                                              
ERRTARG  EQU   530                                                              
ERROFF   EQU   531                                                              
ERRAGY   EQU   532                                                              
ERRAOF   EQU   533                                                              
ERRADV   EQU   534                                                              
ERRSND   EQU   535                                                              
ERRDUP   EQU   536                                                              
ERRTVS   EQU   537                                                              
ERRSTA   EQU   538                                                              
ERRSTAF  EQU   539                                                              
ERRCOMB  EQU   540                                                              
ERREND   EQU   541                                                              
ERRSTRT  EQU   542                                                              
ERRLOK   EQU   543                                                              
ERRSALF  EQU   544                                                              
ERRSALD  EQU   545                                                              
ERROT    EQU   546                                                              
ERRTCOM  EQU   548                                                              
ERRMED   EQU   555                                                              
ERRNOT   EQU   559                                                              
ERRTYP   EQU   560                                                              
ERRSAL   EQU   561                                                              
ERRDEV   EQU   562                                                              
ERRCOMT  EQU   575                                                              
*                                                                               
INFCMPO  EQU   151                                                              
INFCON   EQU   152                                                              
INFREQ   EQU   153                                                              
INFFAIL  EQU   154                                                              
INFCONFM EQU   155                                                              
*                                                                               
IOLEN    EQU   L'IO                                                             
CONLEN   EQU   BCPCON2H-BCPCON1H                                                
*                                                                               
MAXONLN  EQU   5      MAXIMUM TARGET CONTRACTS ALLOWED ONLINE                   
*                     WARNING: DO NOT INCREASE MAXONLN TO MORE THAN 8           
*                              OR RESULTS DISPLAY WILL BE BEYOND BOTTOM         
*                              OF SCREEN!                                       
*                                                                               
CCCDEF   EQU   C'N'   COPY CONTRACT COMMENTS? DEFAULT                           
CBCDEF   EQU   C'N'   COPY BUYLINE COMMENTS? DEFAULT                            
CPGDEF   EQU   C'N'   COPY PROGRAM NAME? DEFAULT                                
CPCDEF   EQU   C'N'   COPY PROGRAM NAME? DEFAULT                                
SADEF    EQU   C'S'   SKIP/APPEND? DEFAULT                                      
*                                                                               
SCRN1    DC    CL25'BUYCOPY RESULT SUMMARY:'                                    
SCRN2    DC    CL60'                CONTRACT        STATION         RES+        
               ULT'                                                             
*                                                                               
MSGFAIL  DC    CL67'NOTE: BUYCOPY PROCESS FAILED ON ONE OR MORE CONTRAC+        
               TS. CONTACT DDS.'                                                
MSGOK    DC    CL30'BUYCOPY COMPLETED SUCCESSFULLY'                             
*                                                                               
         GETEL R3,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
*                                                                               
INFEND   MVI   RMSGTYPE,C'I'                                                    
         B     MYERR2                                                           
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         EJECT                                                                  
MYERR    MVI   RMSGTYPE,C'E'                                                    
         NI    STAT,X'FF'-NOPAUSE                                               
MYERR2   TM    STAT,FIRSTVK                                                     
         BO    *+8                                                              
         OI    STAT,FIRSTVK                                                     
         OI    GENSTAT2,USGETTXT   GENCON MUST CALL GETTXT, NOT GETMSG          
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTINDX,RINDEX       MESSAGE INDEX                                
         MVC   GTMSGNO,RERROR      MESSAGE NUMBER                               
         MVC   GTMTYP,RMSGTYPE     MESSAGE TYPE                                 
         MVC   GTLTXT,RTXTLEN      LENGTH OF OPTIONAL TEXT                      
         MVC   GTATXT,RTXTADR      A(OPTIONAL TEXT)                             
         CLC   RERROR,=H'60'       IF MESSAGE NUMBER <= 60                      
         BH    *+8                                                              
         MVI   GTMSYS,X'FF'        USE GENERAL SYSTEM                           
         DROP  RF                                                               
         SPACE 1                                                                
         GOTO1 ERREX                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB,R7                                                            
***********************************************************************         
* SHOW SCREEN ROUTINE                                                 *         
***********************************************************************         
SS       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         NI    BCPCSTAH+1,X'FF'-X'0C'   TURN OFF ZERO INTENSITY                 
         OI    BCPCSTAH+6,X'80'         TRANSMIT                                
*                                                                               
         NI    BCPCMKTH+1,X'FF'-X'0C'   TURN OFF ZERO INTENSITY                 
         OI    BCPCMKTH+6,X'80'         TRANSMIT                                
*                                                                               
         NI    BCPCAGYH+1,X'FF'-X'0C'   TURN OFF ZERO INTENSITY                 
         OI    BCPCAGYH+6,X'80'         TRANSMIT                                
*                                                                               
         NI    BCPCADVH+1,X'FF'-X'0C'   TURN OFF ZERO INTENSITY                 
         OI    BCPCADVH+6,X'80'         TRANSMIT                                
*                                                                               
         NI    BCPCPRDH+1,X'FF'-X'0C'   TURN OFF ZERO INTENSITY                 
         OI    BCPCPRDH+6,X'80'         TRANSMIT                                
*                                                                               
         NI    BCPM1H+1,X'FF'-X'0C'     TURN OFF ZERO INTENSITY                 
         OI    BCPM1H+6,X'80'           TRANSMIT                                
*                                                                               
         NI    BCPM2H+1,X'FF'-X'0C'     TURN OFF ZERO INTENSITY                 
         OI    BCPM2H+6,X'80'           TRANSMIT                                
*                                                                               
         NI    BCPM3H+1,X'FF'-X'0C'     TURN OFF ZERO INTENSITY                 
         OI    BCPM3H+6,X'80'           TRANSMIT                                
*                                                                               
         NI    BCPM3BH+1,X'FF'-X'0C'    TURN OFF ZERO INTENSITY                 
         OI    BCPM3BH+6,X'80'          TRANSMIT                                
*                                                                               
         NI    BCPM3CH+1,X'FF'-X'0C'    TURN OFF ZERO INTENSITY                 
         OI    BCPM3CH+6,X'80'          TRANSMIT                                
*                                                                               
         NI    BCPM4H+1,X'FF'-X'0C'     TURN OFF ZERO INTENSITY                 
         OI    BCPM4H+6,X'80'           TRANSMIT                                
*                                                                               
         NI    BCPM5H+1,X'FF'-X'0C'     TURN OFF ZERO INTENSITY                 
         OI    BCPM5H+6,X'80'           TRANSMIT                                
*                                                                               
         NI    BCPM6H+1,X'FF'-X'0C'     TURN OFF ZERO INTENSITY                 
         OI    BCPM6H+6,X'80'           TRANSMIT                                
*                                                                               
         NI    BCPDATH+1,X'FF'-X'0C'   TURN OFF ZERO INTENSITY                  
         NI    BCPDATH+1,X'FF'-X'20'   UNPROTECT                                
         OI    BCPDATH+6,X'80'         TRANSMIT                                 
*                                                                               
         NI    BCPCCCH+1,X'FF'-X'0C'   TURN OFF ZERO INTENSITY                  
         NI    BCPCCCH+1,X'FF'-X'20'   UNPROTECT                                
         OI    BCPCCCH+6,X'80'         TRANSMIT                                 
*                                                                               
         NI    BCPCBCH+1,X'FF'-X'0C'   TURN OFF ZERO INTENSITY                  
         NI    BCPCBCH+1,X'FF'-X'20'   UNPROTECT                                
         OI    BCPCBCH+6,X'80'         TRANSMIT                                 
*                                                                               
         NI    BCPCPGH+1,X'FF'-X'0C'   TURN OFF ZERO INTENSITY                  
         NI    BCPCPGH+1,X'FF'-X'20'   UNPROTECT                                
         OI    BCPCPGH+6,X'80'         TRANSMIT                                 
*                                                                               
         NI    BCPCPCH+1,X'FF'-X'0C'   TURN OFF ZERO INTENSITY                  
         NI    BCPCPCH+1,X'FF'-X'20'   UNPROTECT                                
         OI    BCPCPCH+6,X'80'         TRANSMIT                                 
*                                                                               
         TM    STAT,MORE                                                        
         BNO   SS090                                                            
         MVC   BCPM4,M4                                                         
         MVC   BCPM5,M5                                                         
         MVC   BCPM6,M6                                                         
         OI    BCPCONH+6,X'01'                                                  
         OI    BCPCONH+6,X'80'                                                  
*                                                                               
SS090    EQU   *                                                                
*                                                                               
         LA    R2,BCPCON1H                                                      
         LA    R3,BCPCONXH                                                      
         USING BCPCON1H,R2                                                      
SS100    NI    BCPCON1H+1,X'FF'-X'20'   UNPROTECT                               
         NI    BCPSA1H+1,X'FF'-X'20'    UNPROTECT                               
         XC    BCPCON1,BCPCON1          CLEAR FIELD                             
         MVI   BCPSA1,0                 CLEAR FIELD                             
         OI    BCPCON1H+6,X'80'         TRANSMIT                                
         OI    BCPSA1H+6,X'80'          TRANSMIT                                
         DROP  R2                                                               
         CR    R2,R3                                                            
         LA    R2,BCPCON2H-BCPCON1H(R2)                                         
         BNE   SS100                                                            
*                                                                               
SSX      XIT1                                                                   
         LTORG                                                                  
***********************************************************************         
*   ADD NEW CONTRACT ROUTINE                                          *         
***********************************************************************         
* ADD NEW CONTRACT FOR STATION CALL LETTERS POINTED TO BY STAPTR                
* WITH K NUMBER IN TCON9S                                                       
* ADD K NUMBER TO CONTAB & SET STATUS BYTE                                      
*                                                                               
ADDK     DS    0H                                                               
         NMOD1 0,**ADDK**                                                       
         L     RC,0(R1)                                                         
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         L     R4,STAPTR                POINT R4 TO STATION IN TABLE            
*                                                                               
         PACK  TCONREV+3(1),TCON9S(1)   SAVE REVERSED 9'S COMP NUMBER           
         PACK  TCONREV+2(1),TCON9S+1(1)                                         
         PACK  TCONREV+1(1),TCON9S+2(1)                                         
         PACK  TCONREV(1),TCON9S+3(1)                                           
*                                                                               
         SR    R0,R0                    CONVERT K NUM                           
         ICM   R1,15,TCON9S                                                     
         SLDA  R0,4                                                             
         STM   R0,R1,DUB                                                        
         OI    DUB+7,X'0C'              PWS 9'S COMP                            
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5)                                              
         MVO   WORK(5),WORK+10(5)                                               
         MVC   TCON,WORK                PWOS K NUMBER                           
*                                                                               
         L     R6,AIO2                                                          
S        USING RCONREC,R6               S=SOURCE CONTRACT                       
*                                                                               
         LA    R3,KEY                   READ TARGET K STATION INTO IO3          
         USING RSTAREC,R3                                                       
         XC    KEY,KEY                                                          
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,AGENCY                                                  
         MVC   RSTAKSTA,0(R4)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVC   TCONGRP,RSTAGRUP         SAVE STATION GRP/SUBGRP                 
         MVC   TSTASTAT,RSTASTAT        SAVE STATION STAUS BYTE                 
*                                                                               
         XC    TTRAFFIC,TTRAFFIC        GET TARGET K STA TRAFFIC TYPE           
         MVI   ELCODE,X'05'             DEFAULT = NULL                          
         BAS   RE,GETEL3                                                        
         BNE   AK020                                                            
         USING RSTAXEL,R3                                                       
         CLC   RSTARID,=X'0000'                                                 
         BE    AK020                                                            
         CLC   RSTARID,=X'0406'                                                 
         MVI   TTRAFFIC,X'40'           CASE OF GRAPHNET                        
         BE    AK020                                                            
         MVI   TTRAFFIC,X'80'           CASE OF ACE                             
         DROP  R3                                                               
*                                                                               
****BUILD TARGET KEY******                                                      
*                                                                               
AK020    MVC   AIO,AIO1                 BUILD TARGET K IN IO1                   
*                                                                               
         L     R0,AIO                   CLEAR IO AREA                           
         ZIC   R1,=AL2(IOLEN),2                                                 
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R5,AIO                                                           
T        USING RCONREC,R5               T=TARGET CONTRACT                       
*                                                                               
         XC    T.RCONKEY,T.RCONKEY      BUILD TARGET KEY                        
         MVI   T.RCONKTYP,X'0C'                                                 
         MVC   T.RCONKREP,AGENCY                                                
         MVC   T.RCONKGRP,TCONGRP                                               
         MVC   T.RCONKSTA,0(R4)                                                 
         MVC   T.RCONKOFF,S.RCONKOFF                                            
         MVC   T.RCONKAGY,S.RCONKAGY                                            
         MVC   T.RCONKAOF,S.RCONKAOF                                            
         MVC   T.RCONKADV,S.RCONKADV                                            
         MVC   T.RCONKCON,TCON                                                  
         MVC   T.RCONCNTL,S.RCONCNTL                                            
         MVI   T.RCONLEN+1,RCONCMEL-RCONREC                                     
*                                                                               
*****BUILD TARGET X'01' ELEMENT                                                 
*                                                                               
         MVI   T.RCONCODE,X'01'                                                 
         MVC   T.RCONELLN,S.RCONELLN                                            
         MVC   T.RCONBUYR,S.RCONBUYR                                            
         MVC   T.RCONPRD,S.RCONPRD                                              
         MVC   T.RCONTEM,S.RCONTEM                                              
         MVC   T.RCONSAL,S.RCONSAL                                              
         MVC   T.RCONDATE(3),SDATE                                              
         MVC   T.RCONDATE+3(3),EDATE                                            
         MVI   T.RCONMOD,X'FF'                                                  
         XC    T.RCONMODD,T.RCONMODD                                            
         XC    T.RCONMODR,T.RCONMODR                                            
         MVC   T.RCONMODR+1(1),TTRAFFIC                                         
         OI    T.RCONMODR,X'04'         CREATED BY BUYCOPY FLAG                 
         OI    T.RCONMODR,X'30'         BUYLINE 1 ADDED                         
         MVC   T.RCONCREA,TODAY                                                 
*                                       FILL IN CORRECT RATING SERVICE          
         CLI   4(R4),C'A'                                                       
         BNE   *+8                                                              
         MVI   T.RCONRTGS,C'A'          CASE OF A BAND STATION                  
         CLI   4(R4),C'F'                                                       
         BNE   *+8                                                              
         MVI   T.RCONRTGS,C'A'          CASE OF F BAND STATION                  
         CLI   4(R4),C'T'                                                       
         BNE   *+8                                                              
         MVI   T.RCONRTGS,C'N'          CASE OF T BAND STATION                  
         CLI   4(R4),C' '                                                       
         BNE   *+8                                                              
         MVI   T.RCONRTGS,C'N'          CASE OF BLANK (T) BAND STATION          
         CLI   4(R4),C'L'                                                       
         BNE   *+8                                                              
         MVI   T.RCONRTGS,C'N'          CASE OF L BAND STATION                  
*                                                                               
         MVI   T.RCONRMON,0                                                     
         XC    T.RCONINVD,T.RCONINVD                                            
         MVC   T.RCONWKS,KWEEKS                                                 
         MVC   T.RCONHDRD,TODAY                                                 
         MVC   T.RCONTYPE,S.RCONTYPE                                            
         XC    T.RCONRESD,T.RCONRESD                                            
         MVC   T.RCONCTGY,S.RCONCTGY                                            
*                                                                               
******BUILD TARGET '02' ELEMENTS                                                
*                                                                               
         CLI   BCPCCC,C'Y'              COPY CONTRACT COMMENTS?                 
         BNE   AK030                                                            
         L     R3,AIO2                  SOURCE CONTRACT                         
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL3                GET ANY CONTRACT COMMENTS               
         BNE   AK030                                                            
         LA    R2,T.RCONCMEL                                                    
         USING RCONCMEL,R3                                                      
AK025    ZIC   RF,RCONCMLN              COMMENT EL LENGTH FOR EX MOVE           
         BCTR  RF,0                     ADJUST LENGTH                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),RCONCMEL                                                 
         GOTO1 VRECUP,DMCB,(2,0(R5)),ELEM,(R2)                                  
         ZIC   RF,ELEM+1                                                        
         AR    R2,RF                                                            
         BAS   RE,NEXTEL3               NEXT COMMENT ELEMENT                    
         BE    AK025                                                            
AK030    EQU   *                                                                
         DROP  R3                                                               
*                                                                               
****BUILD TARGET X'05' ELEMENT                                                  
*                                                                               
         L     R3,AIO2                  SOURCE CONTRACT                         
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL3                GET X'05' ELEMENT                       
         BNE   AK080                    SKIP COPY IF DOESN'T EXIST              
         USING RCONEXEL,R3                                                      
         ZIC   R2,RCONEXLN                                                      
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),RCONEXEL                                                 
         GOTO1 ADDELEM                                                          
AK080    EQU   *                                                                
         DROP  R3                                                               
*                                                                               
****BUILD TARGET X'10' ELEMENT                                                  
*                                                                               
         L     R3,AIO2                                                          
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL3                                                        
         BNE   AK100                                                            
         USING RCONBPEL,R3                                                      
         ZIC   R2,RCONBPLN                                                      
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),RCONBPEL                                                 
         LA    R3,ELEM                                                          
         MVC   RCONBPDT,TODAY                                                   
         XC    RCONBPRF,RCONBPRF                                                
         GOTO1 ADDELEM                                                          
AK100    EQU   *                                                                
         DROP  R3                                                               
*                                                                               
****BUILD TARGET X'11' ELEMENTS                                                 
*                                                                               
         L     R3,AIO2                                                          
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL3                                                        
         BNE   AK130                                                            
         USING RCONBCEL,R3                                                      
AK110    ZIC   R2,RCONBCLN                                                      
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),RCONBCEL                                                 
         GOTO1 ADDELEM                                                          
         BAS   RE,NEXTEL3                                                       
         BE    AK110                                                            
AK130    EQU   *                                                                
         DROP  R3                                                               
*                                                                               
****BUILD TARGET X'12' ELEMENT                                                  
*                                                                               
         L     R3,AIO2                                                          
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL3                                                        
         BNE   AK160                    IF NO 12 ELEMENT, SKIP 40 & 41          
         XC    ELEM,ELEM                                                        
         USING RSARXEL,R3                                                       
         ZIC   R2,RSARXLEN                                                      
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),RSARXEL                                                  
         LA    R3,ELEM                                                          
         MVC   RSARXLAD,TODAY                                                   
         MVI   RSARXLEN,RSARXLTH        FORCE NEW EL LENGTH                     
         GOTO1 ADDELEM                                                          
         DROP  R3                                                               
*                                                                               
****BUILD TARGET X'40' & '41' & '0B' ELEMENTS                                   
*                                                                               
         MVI   ELCODE,X'0B'                                                     
AK140    L     R3,AIO2                                                          
         BAS   RE,GETEL3                                                        
         BNE   AK150                                                            
         XC    ELEM,ELEM                                                        
         ZIC   R2,1(R3)                                                         
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R3)                                                    
         LA    R3,ELEM                                                          
         GOTO1 ADDELEM                                                          
AK150    CLI   ELCODE,X'0B'                                                     
         BNE   AK151                                                            
         MVI   ELCODE,X'40'                                                     
         B     AK140                                                            
AK151    CLI   ELCODE,X'40'                                                     
         BNE   AK152                                                            
         MVI   ELCODE,X'41'                                                     
         B     AK140                                                            
AK152    EQU   *                                                                
*                                                                               
****BUILD TARGET X'18' ELEMENT                                                  
*                                                                               
AK160    L     R3,AIO2                                                          
         MVI   ELCODE,X'18'                                                     
         BAS   RE,GETEL3                                                        
         BNE   AK170                                                            
         USING RCONDVEL,R3                                                      
         XC    ELEM,ELEM                                                        
         MVC   ELEM(RCONDVI#-RCONDVEL),RCONDVEL                                 
         LA    R3,ELEM                                                          
         MVI   RCONDVLN,RCONDVIL-RCONDVEL                                       
         USING RCONDVEL,R3                                                      
         GOTO1 ADDELEM                                                          
AK170    EQU   *                                                                
         DROP  R3                                                               
*                                                                               
****BUILD TARGET X'1E' ELEMENT                                                  
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING RCONRFEL,R3                                                      
         MVI   RCONRFEL,X'1E'                                                   
         MVI   RCONRFLN,RCONRFLQ                                                
         CLI   LOCALMKT,C'Y'       LOCAL MARKET ORDER                           
         BNE   *+8                 NO - DON'T NEED THIS ELEM                    
         MVI   RCONRF1,X'20'       YES - MARK AS LOCAL ORDER                    
         GOTO1 ADDELEM                                                          
         DROP  R3                                                               
*                                                                               
*****BUILD TARGET X'1F' ELEMENT                                                 
*                                                                               
         LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING RCONXEL,R3                                                       
         MVI   RCONXEL,X'1F'                                                    
         MVI   RCONXEL+1,24                                                     
         OI    RCONCONF,X'80'                                                   
         MVC   RCONSTAT,TSTASTAT                                                
         GOTO1 ADDELEM                                                          
         DROP  R3                                                               
*                                                                               
*                                                                               
*****BUILD TARGET X'20' ELEMENT                                                 
*                                                                               
         LA    R3,ELEM                                                          
         USING RCONSEND,R3                                                      
         XC    ELEM,ELEM                                                        
         MVI   RCONSNCO,X'20'                                                   
         MVI   RCONSNLN,RCONSN3Q                                                
         OI    RCONSENF,X'10'                                                   
         MVI   RCONSRV,1                                                        
         GOTO1 ADDELEM                                                          
         DROP  R3                                                               
*                                                                               
*****BUILD TARGET X'9F' ELEMENT                                                 
*                                                                               
         L     R3,AIO2                                                          
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL3                                                        
         BNE   AK210                                                            
         USING RCONXXEL,R3                                                      
         ZIC   R2,RCONXXEL+1                                                    
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),RCONXXEL                                                 
         LA    R3,ELEM                                                          
         MVI   RCONXXEL+1,40            FORCE NEW LENGTH                        
         CLC   S.RCONKSTA,0(R4)                                                 
         BE    *+16                                                             
         XC    RCONXADV,RCONXADV                                                
         XC    RCONXAGY,RCONXAGY                                                
         GOTO1 ADDELEM                                                          
AK210    EQU   *                                                                
         DROP  R3                                                               
*                                                                               
*****BUILD TARGET X'A2' ELEMENT                                                 
*                                                                               
         L     R3,AIO2                                                          
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL3                                                        
         BNE   AK220                                                            
         USING RCONIEL,R3                                                       
         ZIC   R2,RCONILN                                                       
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),RCONIEL                                                  
         GOTO1 ADDELEM                                                          
AK220    EQU   *                                                                
         DROP  R3                                                               
*                                                                               
* BUILD TARGET '17' ELEMENTS                                                    
*                                                                               
         LA    R2,ELEM                                                          
         LA    R3,CBUFF                                                         
         MVI   OTFLAG,C'N'              DEFAULT                                 
         CLI   9(R3),X'FF'              NOT A COMBO                             
         BE    AK250                                                            
         MVI   OTFLAG,C'Y'              IS A COMBO                              
         XC    ELEM,ELEM                                                        
         MVI   0(R2),X'17'                                                      
         MVI   1(R2),RCONCBST-RCONCBEL  MIN LENGTH                              
         LA    R2,2(R2)                                                         
AK230    CLI   0(R3),X'FF'                                                      
         BE    AK245                                                            
         MVC   0(5,R2),4(R3)            CALL LETTERS                            
         MVC   5(4,R2),0(R3)            K NUMBER                                
         LA    R2,9(R2)                                                         
         LA    R3,9(R3)                                                         
         ZIC   R1,ELEM+1                INCREASE ELEMENT LENGTH                 
         LA    R1,9(R1)                                                         
         STC   R1,ELEM+1                                                        
         B     AK230                                                            
AK245    GOTO1 ADDELEM                                                          
AK250    EQU   *                                                                
*                                                                               
****ADD NEW K RECORD HERE                                                       
*                                                                               
         GOTO1 ADDREC                                                           
         MVC   TDSKADD,KEY              DISK ADDRESS FOR PASSIVE PTRS           
         DROP  T                                                                
         DROP  S                                                                
*                                                                               
***ADD PASSIVE PTRS HERE****                                                    
*                                                                               
         L     R6,AIO                   NEW K RECORD                            
         USING RCONREC,R6                                                       
*                                                                               
* CREATE PTR 1                                                                  
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         MVI   0(R2),X'8C'                                                      
         MVC   21(02,R2),RCONKREP                                               
         MVC   23(04,R2),TCON9S                                                 
         MVC   27(1,R2),RCONCNTL                                                
         MVC   28(4,R2),TDSKADD                                                 
         GOTO1 ADD                                                              
*                                                                               
* CREATE PTR 2                                                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         MVI   0(R2),X'9C'                                                      
         MVC   02(02,R2),RCONKREP                                               
         MVC   04(02,R2),RCONKOFF                                               
         MVC   06(02,R2),RCONKGRP                                               
         MVC   08(05,R2),RCONKSTA                                               
         MVC   13(04,R2),RCONKADV                                               
         MVC   17(04,R2),RCONKAGY                                               
         MVC   21(02,R2),RCONKAOF                                               
         MVC   23(04,R2),RCONKCON                                               
         MVC   27(1,R2),RCONCNTL                                                
         MVC   28(4,R2),TDSKADD                                                 
         GOTO1 ADD                                                              
*                                                                               
* CREATE PTR 3                                                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         MVI   0(R2),X'AC'                                                      
         MVC   01(2,R2),RCONKREP                                                
         MVC   03(2,R2),RCONKOFF                                                
         MVC   05(2,R2),RCONTEM                                                 
* INVERT SALESMAN CODE FOR LAST NAME HIGH                                       
         LA    RE,RCONSAL+2        LAST INITIAL                                 
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,R0                                                            
         MVC   07(1,R2),0(RE)                                                   
         MVC   08(2,R2),RCONSAL                                                 
         CLI   RCONSAL+2,C' '      ONLY 2 INITIALS?                             
         BNE   *+8                                                              
         MVI   9(R2),C' '                                                       
         MVC   10(5,R2),RCONKSTA                                                
         MVC   15(4,R2),RCONKAGY                                                
         MVC   19(4,R2),RCONKADV                                                
         MVC   23(4,R2),RCONKCON                                                
         MVC   27(1,R2),RCONCNTL                                                
         MVC   28(4,R2),TDSKADD                                                 
         GOTO1 ADD                                                              
*                                                                               
* CREATE PTR 4                                                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         MVI   0(R2),X'BC'                                                      
         MVC   02(02,R2),RCONKREP                                               
         MVC   04(02,R2),RCONCTGY                                               
         MVC   06(02,R2),RCONKOFF                                               
         MVC   08(05,R2),RCONKSTA                                               
         MVC   13(04,R2),RCONKAGY                                               
         MVC   17(02,R2),RCONKAOF                                               
         MVC   19(04,R2),RCONKADV                                               
         MVC   23(04,R2),RCONKCON                                               
         MVC   27(1,R2),RCONCNTL                                                
         MVC   28(4,R2),TDSKADD                                                 
         GOTO1 ADD                                                              
* CREATE PTR BF                                                                 
         XC    KEY,KEY                                                          
         MVI   0(R2),X'BF'                                                      
         MVC   02(02,R2),RCONKREP                                               
         MVC   04(06,R2),RCONKAGY                                               
         MVC   10(04,R2),RCONKADV                                               
         MVC   14(05,R2),RCONKSTA                                               
         MVC   19(02,R2),RCONKOFF                                               
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(2,21(R2))                            
         MVC   23(04,R2),RCONKCON                                               
         MVC   27(1,R2),RCONCNTL                                                
         MVC   28(4,R2),TDSKADD                                                 
         GOTO1 ADD                                                              
*                                                                               
* CREATE PTR 5                                                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         MVI   0(R2),X'CC'                                                      
         MVC   01(02,R2),RCONKREP                                               
         MVC   03(05,R2),RCONKSTA                                               
         MVC   08(02,R2),RCONKOFF                                               
         MVC   10(02,R2),RCONTEM                                                
* INVERT SALESMAN                                                               
         LA    RE,RCONSAL+2                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,R0                                                            
         MVC   12(1,R2),0(RE)                                                   
         MVC   13(2,R2),RCONSAL                                                 
         CLI   RCONSAL+2,C' '                                                   
         BNE   *+8                                                              
         MVI   14(R2),C' '                                                      
*                                                                               
         MVC   15(04,R2),RCONKADV                                               
         MVC   19(04,R2),RCONKAGY                                               
         MVC   23(04,R2),RCONKCON                                               
         MVC   27(1,R2),RCONCNTL                                                
         MVC   28(4,R2),TDSKADD                                                 
         GOTO1 ADD                                                              
*                                                                               
* CREATE PTR 6 (BOP)                                                            
*                                                                               
         LA    R2,KEY                                                           
         XC    KEY,KEY                                                          
         L     R3,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL3                                                        
         BNE   NOBOP               NO BOP                                       
*                                                                               
         MVI   0(R2),X'DC'         CREATE BOP POINTER                           
         MVC   5(2,R2),RCONKREP                                                 
         MVC   7(4,R2),RCONKADV                                                 
         MVC   11(3,R2),TODAY      NEW POINTER GETS TODAYS DATE                 
         MVC   18(5,R2),RCONKSTA                                                
         MVC   23(4,R2),RCONKCON                                                
         MVC   27(1,R2),RCONCNTL                                                
         MVC   28(4,R2),TDSKADD                                                 
         GOTO1 ADD                                                              
NOBOP    EQU   *                                                                
*                                                                               
* CREATE PTR 7 (SAR)                                                            
*                                                                               
         LA    R2,KEY                                                           
         XC    KEY,KEY                                                          
         L     R3,AIO                                                           
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL3                                                        
         BNE   NOSAR               NO SAR                                       
*                                                                               
         MVI   0(R2),X'EC'                                                      
         MVC   21(2,R2),RCONKREP                                                
         MVC   23(4,R2),TCONREV                                                 
         MVC   27(1,R2),RCONCNTL                                                
         MVC   28(4,R2),TDSKADD                                                 
         GOTO1 ADD                                                              
NOSAR    EQU   *                                                                
*                                                                               
* CREATE '8D' PTRS                                                              
*                                                                               
         L     R3,AIO                                                           
         LA    R2,KEY                                                           
* - GET FLIGHT START/END DATES AND SAVE IN WORK+40                              
         GOTO1 DATCON,DMCB,(3,RCONDATE),(2,WORK+40)    START DATE               
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(2,WORK+42)  END DATE                 
* - GET DEMO FROM BOP OR SAR ELEMENT AND SAVE IN WORK+50                        
         XC    WORK+50(3),WORK+50                                               
         MVI   ELCODE,X'12'        SAR ELEMENT                                  
         BAS   RE,GETEL3                                                        
         BE    PTR8D10                                                          
         L     R3,AIO                                                           
         MVI   ELCODE,X'10'        BOP ELEMENT                                  
         BAS   RE,GETEL3                                                        
         BE    PTR8D20                                                          
         B     PTR8D50                                                          
                                                                                
* SAR ELEMENT                                                                   
         USING RSARCO,R3                                                        
PTR8D10  MVC   WORK+50(3),RSARDEM    DEMO                                       
                                                                                
******************************************************************              
* NEED TO CHECK FOR PRIMARY DEMO                                                
         LA    RF,8                                                             
         LA    RE,RSARDEM                                                       
PTR8D12  TM    0(RE),X'40'         IS IT MARKED AS PRIMARY DEMO ?               
         BO    PTR8D18             YES                                          
         LA    RE,3(RE)            NO - CHECK OTHER DEMOS                       
         MVC   WORK+50(3),0(RE)                                                 
         BCT   RF,PTR8D12                                                       
         MVC   WORK+50(3),RSARDEM   USE 1ST DEMO AS DEFAULT                     
********************************************************************            
                                                                                
PTR8D18  NI    WORK+50,X'FF'-X'40' CLEAR PRIMARY DEMO INDICATOR                 
         B     PTR8D50                                                          
                                                                                
                                                                                
* BOP ELEMENT                                                                   
         USING RCONBPEL,R3                                                      
PTR8D20  MVC   WORK+50(3),RCONBPDM+1                                            
                                                                                
******************************************************************              
* NEED TO CHECK FOR PRIMARY DEMO                                                
         LA    RF,6                                                             
         LA    RE,RCONBPDM+1                                                    
PTR8D22  TM    0(RE),X'40'             IS IT MARKED AS PRIMARY DEMO ?           
         BO    PTR8D30                 YES                                      
         LA    RE,3(RE)                NO - CHECK OTHER DEMOS                   
         MVC   WORK+50(3),0(RE)                                                 
         BCT   RF,PTR8D22                                                       
         MVC   WORK+50(3),RCONBPDM+1   USE 1ST DEMO AS DEFAULT                  
********************************************************************            
                                                                                
PTR8D30  NI    WORK+50,X'FF'-X'40'           CLEAR MAIN DEMO INDICATOR          
                                                                                
* BUILD BASIC KEY IN WORK                                                       
PTR8D50  XC    WORK(32),WORK                                                    
         LA    R3,WORK                                                          
         MVI   0(R3),X'8D'                                                      
         MVC   1(2,R3),RCONKREP                                                 
         MVC   8(2,R3),WORK+40    START DATE                                    
         MVC   10(2,R3),WORK+42    END DATE                                     
         MVC   12(4,R3),RCONKCON   CONTRACT NUMBER                              
*                                                                               
* ID = 1 = AGENCY / ADVERTISER                                                  
         XC    KEY,KEY                                                          
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),1                                                         
         MVC   17(6,R2),RCONKAGY       AGENCY(4) AND OFFICE(2)                  
         MVC   23(4,R2),RCONKADV       ADVERTISER                               
         MVC   27(1,R2),RCONCNTL                                                
         MVC   28(4,R2),TDSKADD                                                 
         GOTO1 ADD                                                              
                                                                                
* ID = 2 = SALESPERSON/CONTRACT TYPE/GROUP-SUBG/CATEGORY/TEAM                   
         XC    KEY,KEY                                                          
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),2                                                         
         MVC   17(3,R2),RCONSAL                                                 
         MVC   20(1,R2),RCONTYPE                                                
         MVC   21(2,R2),RCONKGRP                                                
         MVC   23(2,R2),RCONCTGY                                                
         MVC   25(2,R2),RCONTEM                                                 
         MVC   27(1,R2),RCONCNTL                                                
         MVC   28(4,R2),TDSKADD                                                 
         GOTO1 ADD                                                              
                                                                                
* ID = 3 = OFFICE(SALESPERSON)/DEMO/CREATION DATE                               
         XC    KEY,KEY                                                          
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),3                                                         
         MVC   17(2,R2),RCONKOFF      SALESPERSON OFFICE                        
* CREATION DATE YMD -> BINARY                                                   
         GOTO1 DATCON,DMCB,(3,RCONCREA),(2,22(R2))                              
         MVC   19(3,R2),WORK+50       DEMO                                      
         MVC   27(1,R2),RCONCNTL                                                
         MVC   28(4,R2),TDSKADD                                                 
         GOTO1 ADD                                                              
*                                                                               
* CREATE '8E' PTRS                                                              
*                                                                               
*   SIMILAR TO X'8D' POINTERS BUT HAVE STATION IN KEY INSTEAD OF 0'S            
*                                                                               
* WORK HAS BASIC KEY- REPLACE ZEROS OF X'8D' WITH STATION                       
         MVI   WORK,X'8E'                                                       
         MVC   WORK+3(5),RCONKSTA                                               
                                                                                
* ID = 1 = AGENCY / ADVERTISER                                                  
         XC    KEY,KEY                                                          
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),1                                                         
         MVC   17(6,R2),RCONKAGY       AGENCY(4) AND OFFICE(2)                  
         MVC   23(4,R2),RCONKADV       ADVERTISER                               
         MVC   27(1,R2),RCONCNTL                                                
         MVC   28(4,R2),TDSKADD                                                 
         GOTO1 ADD                                                              
                                                                                
* ID = 2 = SALESPERSON/CONTRACT TYPE/GROUP-SUBG/CATEGORY/TEAM                   
         XC    KEY,KEY                                                          
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),2                                                         
         MVC   17(3,R2),RCONSAL                                                 
         MVC   20(1,R2),RCONTYPE                                                
         MVC   21(2,R2),RCONKGRP                                                
         MVC   23(2,R2),RCONCTGY                                                
         MVC   25(2,R2),RCONTEM                                                 
         MVC   27(1,R2),RCONCNTL                                                
         MVC   28(4,R2),TDSKADD                                                 
         GOTO1 ADD                                                              
                                                                                
* ID = 3 = OFFICE(SALESPERSON)/DEMO/CREATION DATE                               
         XC    KEY,KEY                                                          
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),3                                                         
         MVC   17(2,R2),RCONKOFF      SALESPERSON OFFICE                        
* CREATION DATE YMD -> BINARY                                                   
         GOTO1 DATCON,DMCB,(3,RCONCREA),(2,22(R2))                              
         MVC   19(3,R2),WORK+50    DEMO                                         
         MVC   27(1,R2),RCONCNTL                                                
         MVC   28(4,R2),TDSKADD                                                 
         GOTO1 ADD                                                              
                                                                                
*                                                                               
* ADD '9D' POINTER                                                              
*          DATA SET REGENPTR   AT LEVEL 016 AS OF 03/18/98                      
*                                                                               
         LA    R2,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   0(R2),X'9D'                                                      
         MVC   1(2,R2),RCONKREP                                                 
         MVC   3(5,R2),RCONKSTA                                                 
         MVC   8(4,R2),RCONKADV                                                 
         MVC   23(4,R2),RCONKCON                                                
* USE PROD CODE OR NAME                                                         
         MVI   12(R2),X'FF'            DEFAULT IS PROD CODE                     
         MVC   13(3,R2),RCONPRD    * DOESN'T ALLLOW COPY                        
         L     R3,AIO              * IF NO '05' ELEM                            
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL3                                                        
         BNE   P9DX                                                             
         USING RCONEXEL,R3                                                      
         MVC   12(9,R2),RCONEXPR        SET PRODUCT NAME                        
P9DX     DS    0H                                                               
         MVC   28(4,R2),TDSKADD                                                 
         GOTO1 ADD                                                              
         DROP  R3                                                               
*                                                                               
***INCREMENT PRODUCT COUNTER HERE****                                           
*                                                                               
         L     R2,CONPTR                WRITE NEW K TO TABLE, SET BYTES         
         MVC   0(4,R2),TCON             PUT K NUMBER IN TABLE                   
         MVI   4(R2),0                  LAST BUYLINE NUMBER = 0                 
         MVI   5(R2),0                  STATUS                                  
         MVC   6(5,R2),RCONKSTA         K STATION                               
         MVC   11(6,R2),RCONDATE        K S&E DATES                             
         MVI   17(R2),0                 ZERO FLAGS #2                           
         OI    17(R2),NEWCON            NEW K FLAG                              
         CLI   OTFLAG,C'N'                                                      
         BE    *+8                                                              
         OI    17(R2),ISCOMBO           IF COMBO, SET FLAG                      
         LA    R2,TABCONQ(R2)           NEXT AVAIL SPOT IN TABLE                
         MVI   0(R2),X'FF'              NEW END OF TABLE                        
         ST    R2,CONPTR                POINTER TO NEW END OF TABLE             
*                                                                               
         DROP  R6                                                               
AKX      XIT1                                                                   
*                                                                               
GETEL3   AH    R3,DATADISP                                                      
FIRSTEL3 CLI   0(R3),0                                                          
         BNE   *+10                                                             
         CLI   0(R3),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R3)                                                     
         BCR   8,RE                                                             
NEXTEL3  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,RF                                                            
         B     FIRSTEL3                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
***********************************************************************         
*   BUY COPY SUBROUTINE                                               *         
***********************************************************************         
*                                                                               
* GIVEN SOURCE AND TARGET CONTRACT NUMS, AND A/S OPTION, LOOP THRU              
* SOURCE BUYLINES                                                               
* FOR EACH SOURCE BUYLINE, COPY TO EVERY CONTRACT IN TARGET CON TABLE           
* GET NEXT SOURCE BUYLINE                                                       
*                                                                               
BILDCOPY DS    0H                                                               
         NMOD1 0,*BCOPY**                                                       
         L     RC,0(R1)                                                         
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
SB       USING RBUYREC,R3               SB = SOURCE BUY                         
         MVI   SB.RBUYKTYP,X'0B'                                                
         MVC   SB.RBUYKREP,AGENCY                                               
         MVC   SB.RBUYKCON,CONREV                                               
         MVI   DMINBTS,0                NO DELETED RECORDS                      
         GOTO1 HIGH                                                             
BCOP0020 CLC   KEY(SB.RBUYKPLN-SB.RBUYKEY),KEYSAVE                              
         BNE   BCNOMORE                                                         
         MVC   KEY2,KEY                 SAVE KEY FOR READ SEQ LOOP              
         CLC   SB.RBUYKMLN,SB.RBUYKLIN  DO NOT USE MAKEGOODS                    
         BNE   BCNEXT                                                           
         CLC   SB.RBUYKMLN(2),=X'FFFF'  DO NOT USE PLAN RECORDS                 
         BE    BCNEXT                                                           
*                                                                               
         MVC   AIO,AIO3                 KEEP SOURCE BUY IN IO3                  
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         USING RBUYREC,R3                                                       
         TM    RBUYCOMB,X'80'           SKIP COMBO BUY PLACEHOLDERS             
         BO    BCNEXT                                                           
         CLI   RBUYCHGI,C'C'       SKIP CANCELLED BUYS                          
         BE    BCNEXT                                                           
         DROP  R3                                                               
*                                                                               
         L     R4,ACONTAB               BEGINNING OF TARGET K TABLE             
*                                                                               
BCOP0040 CLI   0(R4),X'FF'              LAST K IN TABLE?                        
         BE    BCNEXT                                                           
         CLI   5(R4),0                  CHECK K STATUS                          
         BNE   BCOP0520                                                         
*                                                                               
         L     R6,AIO2                  SOURCE K IN IO2                         
         USING RCONREC,R6                                                       
*                                                                               
** DO K NUMBER CONVERSIONS                                                      
         MVC   TCON,0(R4)                                                       
         SR    R0,R0                    CONVERT K NUM                           
         ICM   R1,15,TCON                                                       
         SLDA  R0,4                                                             
         STM   R0,R1,DUB                                                        
         OI    DUB+7,X'0C'              PWS K NUM                               
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5)                                              
         MVO   WORK(5),WORK+10(5)                                               
         MVC   TCON9S,WORK              PWOS 9'S COMP K NUM                     
         PACK  TCONREV+3(1),TCON9S(1)   SAVE REVERSED 9'S COMP NUMBER           
         PACK  TCONREV+2(1),TCON9S+1(1)                                         
         PACK  TCONREV+1(1),TCON9S+2(1)                                         
         PACK  TCONREV(1),TCON9S+3(1)                                           
*                                                                               
         MVC   AIO,AIO1                 USE IO1 FOR NEW BUY REC                 
*                                                                               
         L     R0,AIO                   CLEAR IO AREA                           
         ZIC   R1,=AL2(IOLEN),2                                                 
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         MVCL  R0,RE                                                            
*                                                                               
* BUILD BUYREC KEY                                                              
*                                                                               
         L     R3,AIO3                  SOURCE BUY                              
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVDTIN,RBUYDTIN-RBUYDTEL(R3)                                     
         L     R3,AIO3                                                          
*                                                                               
         L     R5,AIO                   TARGET BUY                              
         USING RBUYREC,R5                                                       
*                                                                               
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,AGENCY                                                  
         MVC   RBUYKCON,TCONREV                                                 
         MVC   RBUYKPLN,=XL3'FFFFFF'    DEFAULT                                 
*****    CLC   RCONKSTA,6(R4)           TARGET STA VS SOURCE STA                
*****    BNE   *+10                     ***CHANGED - DO NOT COPY PLAN**         
*****    MVC   RBUYKPLN,SB.RBUYKPLN                                             
         ZIC   R1,4(R4)                 LAST LINE NUMBER USED                   
         LA    R1,1(R1)                                                         
         STC   R1,RBUYKMLN                                                      
         STC   R1,RBUYKLIN                                                      
         STC   R1,4(R4)                 NEW LAST LINE NUMBER USED               
*                                                                               
         MVI   RBUYLEN+1,RBUYDYEL-RBUYREC LEN = KEY + '01' ELEMENT              
*                                                                               
* BUILD BUYREC '01' ELEMENT                                                     
*                                                                               
         MVI   RBUYCODE,X'01'                                                   
         MVI   RBUYELLN,RBUYDYEL-RBUYELEM                                       
         MVC   RBUYNW,SB.RBUYNW                                                 
         MVC   RBUYCOMB,SB.RBUYCOMB                                             
         NI    RBUYCOMB,X'FF'-X'80'     TURN OFF '80' BIT                       
         MVC   RBUYCOS,SB.RBUYCOS                                               
         XC    RBUYCLS,RBUYCLS                                                  
         XC    RBUYSEC,RBUYSEC                                                  
         CLC   RCONKSTA,6(R4)           TARGET STA VS SOURCE STA                
         BNE   *+16                                                             
         MVC   RBUYCLS,SB.RBUYCLS                                               
         MVC   RBUYSEC,SB.RBUYSEC                                               
         MVC   RBUYCREA,TODAY                                                   
         MVC   RBUYCARD,SB.RBUYCARD                                             
         XC    RBUYCHGD,RBUYCHGD                                                
         MVI   RBUYKMOD,X'FF'                                                   
         MVC   RBUYCHGI,=C'A '                                                  
*                                                                               
         CLC   11(6,R4),RCONDATE        MATCH SOURCE & TARGET HEADINE?          
         BNE   BCOP0060                                                         
         MVC   RBUYTSPT,SB.RBUYTSPT                                             
         MVC   RBUYTCOS,SB.RBUYTCOS                                             
         MVC   RBUYTWKS,SB.RBUYTWKS                                             
         B     BCOP0220                                                         
*                                                                               
BCOP0060 MVC   BSEDAYS,SB.RBUYSTED      SAVE S-E DAYS                           
         ZIC   R2,BSEDAYS                                                       
         SRL   R2,4                     ISOLATE START DAY                       
         STC   R2,BSDAY                 SAVE BUY START DAY                      
BCOP0080 NI    BSEDAYS,X'0F'            ISOLATE END DAY                         
         MVC   BEDAY,BSEDAYS            SAVE BUY END DAY                        
*                                                                               
* COMPUTE START DATE                                                            
         GOTO1 DATCON,DMCB,(3,11(R4)),(0,WORK)                                  
BCOP0100 GOTO1 GETDAY,DMCB,WORK,FULL                                            
         CLC   FULL(3),SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BSDAY,0(R1)                                                      
         BE    BCOP0120                                                         
         GOTO1 ADDAY,DMCB,WORK,WORK+6,1                                         
         MVC   WORK(6),WORK+6                                                   
         B     BCOP0100                                                         
BCOP0120 GOTO1 DATCON,DMCB,(0,WORK),(3,BSDATE)                                  
*                                                                               
* COMPUTE END DATE                                                              
         GOTO1 DATCON,DMCB,(3,14(R4)),(0,WORK)                                  
BCOP0140 GOTO1 GETDAY,DMCB,WORK,FULL                                            
         CLC   FULL(3),SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BEDAY,0(R1)                                                      
         BE    BCOP0160                                                         
         GOTO1 ADDAY,DMCB,WORK,WORK+6,-1                                        
         MVC   WORK(6),WORK+6                                                   
         B     BCOP0140                                                         
BCOP0160 GOTO1 DATCON,DMCB,(0,WORK),(3,BEDATE)                                  
*                                                                               
         SR    R2,R2                    COUNT WEEKS                             
         GOTO1 DATCON,DMCB,(3,BSDATE),(0,WORK)                                  
         GOTO1 DATCON,DMCB,(3,BEDATE),(0,WORK+6)                                
         MVC   WORK+12(6),WORK                                                  
BCOP0180 LA    R2,1(R2)                                                         
*                                                                               
         SR    RF,RF                                                            
         LA    RF,7                                                             
         TM    SVDTIN,X'40'      ALTERNATE WEEK                                 
         BZ    *+8                                                              
         LA    RF,14                                                            
*                                                                               
         GOTO1 ADDAY,DMCB,WORK+12,WORK+18,(RF)                                  
         MVC   WORK+12(6),WORK+18                                               
         CLC   WORK+12(6),WORK+6                                                
         BNH   BCOP0180                                                         
         STC   R2,BWEEKS                                                        
*                                                                               
         TM    SB.RBUYSFG,X'40'    SPORTS BUY?                                  
         BZ    BCOP0200            NO                                           
         MVC   RBUYTSPT,SB.RBUYTSPT  PRESERVE SPOT & COST INFO                  
         MVC   RBUYTCOS,SB.RBUYTCOS                                             
         XC    RBUYCOS,RBUYCOS     CLEAR RATE FIELD                             
         MVC   RBUYTWKS,BWEEKS                                                  
         B     BCOP0220                                                         
*                                                                               
BCOP0200 ZIC   R1,BWEEKS                                                        
         MVI   HALF,0                                                           
         MVC   HALF+1,RBUYNW                                                    
         MH    R1,HALF                                                          
         STCM  R1,3,RBUYTSPT                                                    
*                                                                               
         ICM   R1,15,RBUYCOS                                                    
         MVC   HALF,RBUYTSPT                                                    
         MH    R1,HALF                                                          
         STCM  R1,15,RBUYTCOS                                                   
*                                                                               
         MVC   RBUYTWKS,BWEEKS                                                  
*                                                                               
BCOP0220 MVC   RBUYSTED,SB.RBUYSTED                                             
         MVC   RBUYDUR,SB.RBUYDUR                                               
         MVC   RBUYFLT,SB.RBUYFLT                                               
         MVI   RBUYVER,1                                                        
         MVC   RBUYTYP,SB.RBUYTYP                                               
         MVC   RBUYDPT,SB.RBUYDPT                                               
         MVI   RBUYRTS,0                                                        
         TM    17(R4),ISCOMBO                                                   
         BNO   *+8                                                              
         OI    RBUYRTS,X'40'                                                    
         MVI   RBUYAGBL,0                                                       
         MVI   RBUYFLG2,X'40'                                                   
         TM    SB.RBUYFLG2,X'02'   TRADE BUY FLAG SET?                          
         BNO   BCOP0240            NO                                           
         OI    RBUYFLG2,X'02'      YES                                          
         OI    BYTE,X'08'          SET CONTRACT TRADE FLAG                      
BCOP0240 EQU   *                                                                
*                                                                               
         DROP  R5                                                               
*                                                                               
* BUILD BUYREC '02' ELEMENT                                                     
*                                                                               
         L     R3,AIO3                                                          
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL2                                                        
         BNE   BCOP0280                                                         
BCOP0260 ZIC   R2,1(R3)                                                         
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R3)                                                    
         GOTO1 ADDELEM                                                          
         BAS   RE,NEXTEL2                                                       
         BE    BCOP0260                                                         
BCOP0280 EQU   *                                                                
*                                                                               
* BUILD BUYREC X'03' ELEMENT                                                    
*                                                                               
         LA    R5,ELEM                                                          
         L     R3,AIO3                                                          
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL2                                                        
         BNE   BCOP0360                                                         
         USING RBUYDTEL,R3                                                      
BCOP0300 XC    ELEM,ELEM                                                        
         ZIC   R2,RBUYDTLN                                                      
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),RBUYDTEL                                                 
         MVC   BFLAGS,RBUYDTIN                                                  
         DROP  R3                                                               
         CLC   11(6,R4),RCONDATE        MATCH SOURCE & TARGET HEADINE?          
         BE    BCOP0340                                                         
         L     R3,AIO3                                                          
         USING RBUYDTEL,R5                                                      
         MVI   RBUYDTCD,X'03'                                                   
         MVI   RBUYDTLN,11                                                      
         MVC   RBUYDTST,BSDATE                                                  
         MVC   RBUYDTED,BEDATE                                                  
         MVC   RBUYDTIN,BFLAGS                                                  
         CLI   RBUYDTIN,0                                                       
         BNE   BCOP0320                                                         
         MVI   RBUYDTIN,X'80'      DEFAULT WEEKLY                               
*                                                                               
BCOP0320 DS    0H                                                               
         MVC   RBUYDTNW,SB.RBUYNW                                               
         MVC   RBUYDTWK,BWEEKS                                                  
*                                                                               
BCOP0340 GOTO1 ADDELEM                                                          
         CLC   11(6,R4),RCONDATE        MATCH SOURCE & TARGET HEADINE?          
         BNE   BCOP0360                                                         
         BAS   RE,NEXTEL2                                                       
         BE    BCOP0300                                                         
BCOP0360 EQU   *                                                                
         DROP  R5                                                               
         BAS   RE,RATEBILD         CALCULATE SPORTS BUY RATE                    
*                                                                               
* BUILD BUYREC X'04' ELEMENTS                                                   
*                                                                               
         CLI   BCPCBC,C'Y'              COPY BUYLINE COMMENTS?                  
         BNE   BCOP0400                                                         
         L     R5,AIO1                                                          
         USING RBUYREC,R5                                                       
         L     R3,AIO3                  SOURCE BUY                              
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL2                GET ANY BUYLINE COMMENTS                
         BNE   BCOP0400                                                         
         L     R2,AIO1                                                          
         MVC   HALF,RBUYLEN                                                     
         AH    R2,HALF                                                          
BCOP0380 ZIC   RF,1(R3)                 COMMENT EL LENGTH FOR EX MOVE           
         BCTR  RF,0                     ADJUST LENGTH                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R3)                                                    
         GOTO1 VRECUP,DMCB,(2,0(R5)),ELEM,(R2)                                  
         ZIC   RF,ELEM+1                                                        
         AR    R2,RF                                                            
         BAS   RE,NEXTEL2               NEXT COMMENT ELEMENT                    
         BE    BCOP0380                                                         
         DROP  R5                                                               
BCOP0400 EQU   *                                                                
*                                                                               
* BUILD BUYREC X'10' ELEMENT                                                    
*                                                                               
         LA    R5,ELEM                                                          
         USING RBUYXXEL,R5                                                      
         XC    ELEM,ELEM                                                        
         MVI   RBUYXXCD,RBUYXXCQ                                                
         MVI   RBUYXXLN,RBUYXXLQ                                                
         MVI   RBUYXXMD,X'FF'                                                   
         GOTO1 ADDELEM                                                          
         DROP  R5                                                               
*                                                                               
* BUILD BUYREC X'21' PGM NAME ELEMENT                                           
*                                                                               
         CLI   BCPCPG,C'Y'              COPY PROGRAM?                           
         BNE   BCOP0420                                                         
         L     R5,AIO1                                                          
         USING RBUYREC,R5                                                       
         L     R3,AIO3                  SOURCE BUY                              
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL2                GET PGM NAME                            
         BNE   BCOP0420                                                         
         L     R2,AIO1                                                          
         MVC   HALF,RBUYLEN                                                     
         AH    R2,HALF                                                          
         ZIC   RF,1(R3)                 COMMENT EL LENGTH FOR EX MOVE           
         BCTR  RF,0                     ADJUST LENGTH                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R3)                                                    
         GOTO1 VRECUP,DMCB,(2,0(R5)),ELEM,(R2)                                  
         DROP  R5                                                               
BCOP0420 EQU   *                                                                
*                                                                               
* COPY SPORTS BUY DESCRIPTIVES                                                  
*                                                                               
         L     R3,AIO3                  SOURCE BUY                              
         MVI   ELCODE,X'81'                                                     
         BAS   RE,GETEL2                GET ANY DESCRIPTIVES                    
         BNE   BCOP0440                                                         
         MVC   ELEM,0(R3)                                                       
         GOTO1 ADDELEM                                                          
BCOP0440 DS    0H                                                               
         L     R3,AIO3                  SOURCE BUY                              
         MVI   ELCODE,X'82'                                                     
         BAS   RE,GETEL2                GET ANY DESCRIPTIVES                    
         BNE   BCOP0460                                                         
         MVC   ELEM,0(R3)                                                       
         GOTO1 ADDELEM                                                          
BCOP0460 EQU   *                                                                
*                                                                               
* ADD NEW BUYREC HERE                                                           
*                                                                               
         OI    GENSTAT4,NODUPDIE        DON'T DIE ON FAILED ADD                 
         MVI   DMOUTBTS,0               CLEAR DM OUTBITS                        
         GOTO1 ADDREC                                                           
         NI    GENSTAT4,X'FF'-NODUPDIE  RESTORE DON'T DIE OPTION                
         CLI   DMCB+8,0                                                         
         BE    *+12                                                             
         OI    5(R4),FAILBUY                                                    
         B     BCOP0520                                                         
*                                                                               
         ZIC   R1,ADDCOUNT                                                      
         LA    R1,1(R1)                                                         
         STC   R1,ADDCOUNT                                                      
         CLI   ADDCOUNT,100                                                     
         BNH   BCOP0480                                                         
         MVI   ADDCOUNT,0                                                       
         GOTO1 DATAMGR,DMCB,=C'DMUNLK',=C'REPDIR'                               
         GOTO1 DATAMGR,DMCB,=C'DMUNLK',=C'REPFIL'                               
*                                                                               
         DROP  R6                                                               
         DROP  SB                                                               
*                                                                               
* BUILD & UPDATE K X'03' ELEMENT                                                
*                                                                               
BCOP0480 EQU   *                                                                
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING RCONREC,R3                                                       
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,AGENCY                                                  
         MVC   RCONPCON,TCON9S                                                  
         GOTO1 HIGH                                                             
         DROP  R3                                                               
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO5                                                         
         GOTO1 GETREC                                                           
*                                                                               
         GOTOX (RFCHKALT,VREPFACS),DMCB,(0,AIO5),ACOMFACS                       
         MVC   BUCKFLGS(1),0(R1)                                                
         L     R1,AREPPROF         SET A(REP PROFILE FOR DAILY PACING)          
         CLI   0(R1),C'Y'          REP USING 'DAILY PACING'?                    
         BNE   BCOP0500            NO                                           
         OI    BUCKFLGS,X'08'      SET DAILY PACING CALL                        
BCOP0500 EQU   *                                                                
*                                                                               
         GOTOX (RFBUCKUP,VREPFACS),DMCB,AIO1,(BUCKFLGS,AIO5),          +        
               ACOMFACS,GETBROAD,VRECUP,AWRKSPAC                                
         BE    *+12                                                             
         OI    5(R4),FAILACL                                                    
         B     BCOP0520                                                         
*                                                                               
         BAS   RE,UPDKMODF         UPDATE CONTRACT RCONMODR FLAGS               
*                                                                               
         BAS   RE,UPDATE1E         SET TRADE FLAG IN HDR IF NEEDED              
*                                                                               
         GOTO1 PUTREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
BCOP0520 LA    R4,TABCONQ(R4)           NEXT K IN TABLE                         
         B     BCOP0040                                                         
*                                                                               
BCNEXT   MVC   KEY,KEY2                 RESTORE KEY                             
         GOTO1 HIGH                     RESTORE READ SEQ SEQUENCE               
         GOTO1 SEQ                      GET NEXT SOURCE BUY                     
         LA    R3,KEY                                                           
         B     BCOP0020                                                         
*                                                                               
BCNOMORE EQU   *                                                                
*                                                                               
BCX      XIT1                                                                   
         EJECT                                                                  
RATEBILD NTR1                                                                   
         L     R4,AIO1             SET A(NEW BUY RECORD)                        
         USING RBUYREC,R4                                                       
         TM    RBUYDUR,X'40'       SPORTS BUY?                                  
         BNO   RBIL0080            NO  - EXIT ROUTINE                           
         SR    RE,RE               YES - CALCULATE RATE FOR BUY                 
         LA    R6,RBUYELEM                                                      
RBIL0020 EQU   *                                                                
         CLI   0(R6),0             END OF RECORD REACHED?                       
         BE    RBIL0060            YES                                          
         CLI   0(R6),3             EFFECTIVE DATE ELEMENT?                      
         BNE   RBIL0040            NO                                           
         ZIC   RF,RBUYDTWK-RBUYDTEL(R6)                                         
*                                  YES - ACCUMULATE NUMBER OF WEEKS             
         AR    RE,RF                                                            
RBIL0040 EQU   *                                                                
         ZIC   RF,1(R6)                                                         
         AR    R6,RF               BUMP TO NEXT ELEMENT                         
         B     RBIL0020            GO BACK AND CHECK                            
RBIL0060 EQU   *                                                                
         SR    R2,R2               PREP FOR DIVISION                            
         ZICM  R3,RBUYTCOS,4       ORIGINAL ENTERED COST                        
         LTR   RE,RE               # WEEKS ZERO?                                
         BZ    RBIL0080            YES - CAN'T DIVIDE                           
         DR    R2,RE               DIVIDE TOTAL COST BY # OF WEEKS              
         STCM  R3,15,RBUYCOS       AVERAGE COST PER WEEK OF TOTAL COST          
RBIL0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
***>>>                                                                          
*                                                                               
UPDATE1E NTR1                                                                   
         L     R3,AIO5             A(SOURCE CONTRACT)                           
         USING RCONREC,R3                                                       
*                                                                               
         CLI   RCONKSTA+4,C'A'     RADIO STATION?                               
         BE    UPDA110             YES - DON'T CHANGE FLAG                      
         CLI   RCONKSTA+4,C'F'     RADIO STATION?                               
         BE    UPDA110             YES - DON'T CHANGE FLAG                      
         DROP  R3                                                               
         MVI   ELCODE,X'1E'        FIND 1E ELEMENT                              
         BAS   RE,GETEL2                                                        
         BE    UPDA100             FOUND                                        
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING RCONRFEL,R3                                                      
         MVI   RCONRFEL,X'1E'                                                   
         MVI   RCONRFEL+1,22                                                    
         OC    RCONRF1,BYTE        ADD TRADE FLAG FOR TV ORDERS                 
*                                     BYTE MAY/MAY NOT BE TRADE                 
         GOTO1 ADDELEM                                                          
         B     UPDA110                                                          
         DROP  R3                                                               
UPDA100  EQU   *                                                                
         OC    2(1,R3),BYTE        ADD TRADE FLAG FOR TV ORDERS                 
*                                     BYTE MAY/MAY NOT BE TRADE                 
UPDA110  EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
***>>>                                                                          
*                                                                               
UPDKMODF NTR1                                                                   
         L     R3,AIO5             A(CONTRACT RECORD)                           
         USING RCONREC,R3                                                       
         OI    RCONMODR,X'10'      BUY ADDED                                    
*                                                                               
         L     R4,AIO1             A(BUY RECORD)                                
         USING RBUYREC,R4                                                       
         CLI   RBUYKLIN,1                                                       
         BNE   UPDK100                                                          
         DROP  R4                                                               
*                                                                               
         OI    RCONMODR,X'20'      BUY 1 ADDED                                  
         MVC   RCONCREA,TODAY                                                   
         DROP  R3                                                               
*                                                                               
UPDK100  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
         GETEL2 R3,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS               
         LTORG                                                                  
         DROP RB                                                                
***********************************************************************         
         EJECT                                                                  
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE RESFMFFD          BASE SCREEN                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMB3D          REPORT SCREEN                                
       ++INCLUDE RESFMWORKD                                                     
         EJECT                                                                  
         ORG   SYSSPARE                                                         
***********************************************************************         
*    PROGRAM SAVED STORAGE HERE                                       *         
***********************************************************************         
VREPFACS DS    A                   ADDRESS OF REPFACS MODULE                    
AIO5     DS    A                   ADDRESS OF SAFE IO AREA                      
CONREG   DS    CL4                      CONTRACT NUMBER                         
CON9S    DS    CL4                      CON NUM 9'S COMP                        
CONREV   DS    CL4                      CON NUM 9'S COMP/REVERSED               
SMEDIA   DS    C                        SOURCE K MEDIA R/T                      
SBUYS    DS    X                        NUMBER OF SOURCE BUYS                   
DATES    DS    0CL6                                                             
SDATE    DS    CL3                      CON HEAD START DATE YMD BINARY          
EDATE    DS    CL3                      CON HEAD END DATE YMD BINARY            
KWEEKS   DS    X                        CONTRACT WEEKS                          
BWEEKS   DS    X                        TARGET BUYLINE WEEKS                    
BFLAGS   DS    X                   TARGET BUY FLAGS                             
WORK2    DS    CL64                                                             
CONPTR   DS    F                        A(NEXT ENTRY IN CONTAB)                 
STAPTR   DS    F                        A(NEXT ENTRY IN STATAB)                 
LASTCON  DS    F                        A(LAST CON FIELD ON SCREEN)             
TNUM     DS    X                        # OF TARGETS TOTAL                      
TODAY    DS    CL3                      TODAY'S DATE YMD BIN                    
SALOFF   DS    CL2                      SOURCE SALESMAN OFFICE                  
SALTEAM  DS    CL2                      SOURCE SALESMAN TEAM                    
SALLEAV  DS    CL3                      SOURCE SALESMAN LEAVE DATE              
SALNAME  DS    CL20                     SOURCE SALESMAN NAME                    
SDEVSAL  DS    CL3                      SOURCE K DEV SALESPERSON                
SDEVTYP  DS    CL2                      SOURCE K DEV CONTRACT TYPE              
STATION  DS    CL5                      TARGET STATION CALL LETTERS             
TCONREV  DS    CL4                      TARGET K NUM 9'S & REVERSED             
TCON9S   DS    CL4                      TARGET K NUM 9'S COMP.                  
TCON     DS    CL4                      TARGET K NUM                            
BSEDAYS  DS    X                        TARGET BUY S-E DAYS                     
BSDAY    DS    X                        TARGET BUY START DAY                    
BEDAY    DS    X                        TARGET BUY END DAY                      
BSDATE   DS    CL3                      TARGET BUY START DATE                   
BEDATE   DS    CL3                      TARGET BUY END DATE                     
BUFFNUM  DS    X                        NUM OF STATIONS IN CBUFF                
SKPFLAG  DS    X                        SKIP OPTION SELECTED FLAG               
OTFLAG   DS    X                        OFFICE MATCH TEAM REQ FLAG              
ASTATAB  DS    A                        A(TARGET STATION TABLE)                 
ACONTAB  DS    A                        A(TARGET CONTRACT TABLE)                
AREPPROF DS    A                        A(REP PROFILE BYTE)                     
AWRKSPAC DS    A                        A(BUCKUP WORKSPACE IN TDA)              
TDSKADD  DS    A                        DISK ADDRESS OF NEW K                   
TTRAFFIC DS    X                        TARGET K STATION TRAFFIC TYPE           
TCONGRP  DS    CL2                      TARGET K STATION GROUP                  
TSTASTAT DS    X                        TARGET K STATION STATUS BYTE            
STAT     DS    X                        PROGRAM FLOW CONTROL BYTE               
REFRESH  EQU   X'01'                                                            
NOPAUSE  EQU   X'02'                                                            
*******  EQU   X'04'               FAILBUY                                      
FIRSTVK  EQU   X'08'                                                            
MORE     EQU   X'80'                                                            
*******  EQU   X'40'               FAILACL                                      
ALTWEEK  EQU   X'20'               ALTERNATE WEEK                               
*                                                                               
LOCALMKT DS    C                   LOCAL MARKET STATION?                        
BUCKFLGS DS    X                   BUCKET FLAGS                                 
ADDCOUNT DS    X                        BC LOOP ADDREC COUNTER                  
CBUFF    DS    CL37                     COMBO BUFFER - LIST OF CHILDREN         
KEY2     DS    CL48                                                             
M4       DS    CL60                                                             
M5       DS    CL80                                                             
M6       DS    CL80                                                             
WORK3    DS    CL300                                                            
SVDTIN   DS    X                                                                
*                                                                               
***********************************************************************         
TABLES   DSECT                                                                  
*                                                                               
REPPROF  DS    CL1       REP PROFILE SAVE AREA                                  
*                                                                               
STATAB   DS    CL1321    TABLE OF ALL TARGET STATIONS FOR VALIDATION            
TABSTAQ  EQU   6         5 BYTE STATION CALL LETTERS + 1 BYTE NUMBER            
*                        OF RELATED STATIONS (INCLUDING SELF)                   
*                        LENGTH = 55 INPUT FIELDS * MAX OF 4 RELATED            
*                        STATIONS * 6 BYTE ENTRIES + 1 END MARK                 
*                                                                               
CONTAB   DS    CL3961    TABLE OF ALL TARGET CONTRACTS, EXISTING AND            
TABCONQ  EQU   18        NEW. 4 BYTE K NUM + 1 BYTE S/A OPTION + 1 BYTE         
*                        STATUS + 5 BYTE STATION CALL LETTERS.                  
*                        LENGTH = 55 INPUT FIELDS * MAX OF 4 RELATED            
*                        STATIONS * 18 BYTE ENTRIES + 1 END MARK                
*  CL4  K NUMBER PWOS                                                           
*  XL1  S/A OPTION THEN USED TO TRACK LAST BUYLINE # USED                       
*  XL1  K STATUS BYTE & ERROR FLAGS (BELOW) BYTE 1                              
*  CL5  K STATION CALL LETTERS                                                  
*  CL3  K HEADLINE START DATE                                                   
*  CL3  K HEADLINE END DATE                                                     
*  XL1  K FLAGS BYTE #2                                                         
*                                                                               
SKIP     EQU   X'01'    (BYTE 1)                                                
NOROOM   EQU   X'02'    (BYTE 1)                                                
FAILBUY  EQU   X'04'    (BYTE 1)                                                
FAILACL  EQU   X'40'    (BYTE 1)                                                
*                                                                               
ISCOMBO  EQU   X'01'    (BYTE 2)                                                
NEWCON   EQU   X'02'    (BYTE 2)                                                
*                                                                               
TABSLENQ EQU   *-TABLES                                                         
***********************************************************************         
         EJECT                                                                  
       ++INCLUDE REGENCON                                                       
       ++INCLUDE REGENBUY                                                       
       ++INCLUDE REGENSTA                                                       
       ++INCLUDE REGENSAL                                                       
       ++INCLUDE REGENAGY                                                       
       ++INCLUDE REGENPRD                                                       
       ++INCLUDE REGENADV                                                       
       ++INCLUDE REGENOFF                                                       
       ++INCLUDE REGENDSP                                                       
       ++INCLUDE REGENDCT                                                       
       ++INCLUDE REGENREPA                                                      
       ++INCLUDE RECNTPROF                                                      
       ++INCLUDE REGENSBLK                                                      
       ++INCLUDE REGENCOV                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'099RESFM33   06/22/07'                                      
         END                                                                    
