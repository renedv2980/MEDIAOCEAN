*          DATA SET ACREPXV08A AT LEVEL 100 AS OF 05/01/02                      
*PHASE ACXV02B                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE SCANNER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'SCRIBE KEYWORD AND FILTERS - FILE SEARCH'                       
***********************************************************************         
*  SEARCH FOR ROW AND/OR COLUMN KEYWORDS AND                          *         
*  SEARCH FOR COLUMN FILTERS BY TYPE OF FILTER                        *         
*                                                                     *         
*  TABLES TO BE UPDATED:                                              *         
*    KEYWRDS  - THE KEYWORDS TO BE SCANNED FOR                        *         
*    KEYWRDS2 - THE KEYWORDS TO EXCLUDE REPORTING ON                  *         
*    FILTERS  - THE FILTER TYPES TO BE SEARCHED FOR                   *         
*    RPTTYPES - THE REPORT TYPES TO BE CONSIDERED (NULL MEANS ALL)    *         
*                                                                     *         
*  INPUT FIELDS:                                                      *         
*    QSELECT  - BYTES 0-1 - HEXADECIMAL COMPANY CODE IN CHARACTER     *         
*                           BLANK MEANS ALL COMPANIES                 *         
*                                                                     *         
*               BYTE  2   - ACCPAK ID                                 *         
*                           BLANK MEANS ALL ACCPAK IDS                *         
*    QSTART   - START DATE IN CHARACTER YYMMDD                        *         
*    QEND     - END   DATE IN CHARACTER YYMMDD                        *         
*    UPSI     - DEBUGGING REQUESTS                                    *         
*                 BIT 0 = DUMP GET REQUESTS                           *         
*                 BIT 1 = DUMP PUT REQUESTS                           *         
*                 BIT 2 = DUMP UPDATED ELEMENTS                       *         
*    QOPT1    - LIMIT KEYWORD SCAN:                                   *         
*                 A     = ALL        CHARACTER  FORMATS               *         
*                 C     = CHARACTER  FORMAT                           *         
*                 T     = TRANSLATED FORMAT                           *         
*                 BLANK = ALL FORMATS                                 *         
*    QOPT2    - SEARCH FOR SPECIAL REPORT TYPES                       *         
*                 C     = CHARACTER  REPORT TYPE                      *         
*                 T     = TRANSLATED REPORT TYPE                      *         
*                 M     = CHARACTER  REPORT TYPES NOT IN THE TABLE    *         
*                         AND        FORMAT NAMES OF  NULLS           *         
*                 BLANK = DO NOT SEARCH FOR SPECIAL REPORT TYPES      *         
*    RCWRITE  - WRITE REQUEST FROM WRITE= JOB INPUT CARD              *         
*                 N     = NO                                          *         
*                 Y     = YES                                         *         
*                 BLANK = YES                                         *         
*    RCLANG   - LANGUAGE                                              *         
*                                                                     *         
*  CURRENT ITEMS SCANNED FOR:                                         *         
*    KEYWORDS     - NONE                                              *         
*    REPORT TYPES - ALL                                               *         
**   KEYWORDS     - F1, F2, F3, F4, F5 AND OF                         *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING ACWORKD,RA                                                       
         USING ACXVD,RC                                                         
ACXV02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXV**,R9                                                    
         L     RA,0(,R1)                                                        
         LA    RC,SPACEND                                                       
         EJECT ,                                                                
ACXV02A  CLI   MODE,REQFRST                                                     
         BNE   XIT                                                              
         ZAP   TOTRECS,=P'0'       SYSTEM     TOTAL     RECORDS                 
         ZAP   TOTFNDS,=P'0'       SYSTEM     TOTAL     FOUND                   
         ZAP   FTOTRECS,=P'0'      FULL       TOTAL     RECORDS                 
         ZAP   FTOTFNDS,=P'0'      FULL       TOTAL     FOUND                   
         ZAP   CPYFNDS,=P'0'       COMPANY    TOTAL     FOUND                   
         ZAP   CPYRECS,=P'0'       COMPANY    TOTAL     RECORDS                 
         ZAP   DUMPCNT,=P'0'                                                    
         ZAP   DUMPECNT,=P'0'                                                   
         ZAP   PDUMP,=P'0'                                                      
         MVC   REQDATED,SPACES                                                  
         MVC   REQUEST#,SPACES                                                  
         MVI   FORCEHED,YES                                                     
         MVI   FCRESET,YES                                                      
*                                                                               
         USING MASTD,RE            MASTER     DSECT                             
         L     RE,ADMASTC          ->    MASTER    DSECT                        
         L     R2,MCUTL            ->    UTL                                    
         ST    R2,AUTL             SAVE  ADDR OF   UTL                          
         MVC   SAVESE,4(R2)        SAVE  SE   ID   FOR  CTL  FILES              
         MVC   UPSI,MCUPSI         SAVE  UPSI BYTE                              
         DROP  RE                                                               
*                                                                               
         MVI   FILTSEL,0                                                        
         GOTO1 HEXIN,DMCB,QSELECT,FILTSEL,2                                     
         OC    DMCB+12(4),DMCB+12                                               
         BNZ   *+8                                                              
         MVI   FILTSEL,0                                                        
*                                                                               
         XC    TEXTMSG,TEXTMSG                                                  
         XC    RQSTART,RQSTART                                                  
         MVC   RQEND,=X'FFFFFF'                                                 
*                                                                               
         CLC   QSTART,SPACES                                                    
         BNH   DT010                                                            
         GOTO1 DATCON,DMCB,(0,QSTART),(1,RQSTART)                               
*                                                                               
DT010    DS    0H                                                               
         CLC   QEND,SPACES                                                      
         BNH   DT020                                                            
         GOTO1 DATCON,DMCB,(0,QEND),(1,RQEND)                                   
*                                                                               
DT020    DS    0H                                                               
         MVC   DICPARM,=C'SU  '    PARM FOR  DICTATE                            
*&&US*&& MVI   DICPARM+2,CTWKACC   ACCPAK    SYSTEM                             
*&&UK*&& MVI   DICPARM+2,X'06'     ACCPAK    SYSTEM                             
         MVC   DICPARM+3,RCLANG    LANGUAGE                                     
         EJECT ,                                                                
         USING KEYWRDSD,R2         KEYWORDS  DSECT                              
         L     R2,=A(KEYWRDS)      ->   KEYWORD   TABLE                         
DT030    DS    0H                                                               
         CLI   0(R2),EOT           END  OF   TABLE ?                            
         BE    DT040               YES, DONE                                    
*                                  TRANSLATE KEYWORD                            
         GOTO1 EXPDNTRY,DMCB,(L'KEYWDIC,KEYWDIC),KEYWTEXT                       
         LA    R2,KEYWRDLQ(,R2)    ->   NEXT KEYWORD   ENTRY                    
         B     DT030               TRANSLATE NEXT KEYWORD                       
         DROP  R2                                                               
*                                                                               
         USING RPTTYPED,R2         REPORT    TYPES     DSECT                    
DT040    DS    0H                                                               
         MVI   LIMRTYPS,NO         LIMIT     REPORT    TYPES                    
         L     R2,=A(RPTTYPES)     ->   REPORT    TYPES     TABLE               
*                                                                               
DT050    DS    0H                                                               
         CLI   0(R2),EOT           END  OF   TABLE ?                            
         BE    DT060               YES, DONE                                    
         MVI   LIMRTYPS,YES        LIMIT     REPORT    TYPES                    
*                                  TRANSLATE REPORT    TYPE                     
         GOTO1 EXPDNTRY,DMCB,(L'RPTTDIC,RPTTDIC),RPTTTEXT                       
         LA    R2,RPTTYPLQ(,R2)    ->   NEXT REPORT    TYPE ENTRY               
         B     DT050               TRANSLATE NEXT KEYWORD                       
         DROP  R2                                                               
*                                                                               
         USING KEYWRDSD,R2         KEYWORDS  DSECT                              
DT060    L     R2,=A(KEYWRDS2)     ->   KEYWORD   EXCLUDE   TABLE               
*                                                                               
DT070    CLI   0(R2),EOT           END  OF   TABLE ?                            
         BE    DT080               YES, DONE                                    
*                                  TRANSLATE KEYWORD                            
         GOTO1 EXPDNTRY,DMCB,(L'KEYWDIC,KEYWDIC),KEYWTEXT                       
         LA    R2,KEYWRDLQ(,R2)    ->   NEXT KEYWORD   ENTRY                    
         B     DT070               TRANSLATE NEXT KEYWORD                       
         DROP  R2                  KEEP IT   CLEAN                              
*                                                                               
         USING RPTTYPED,R2         REPORT    TYPES     DSECT                    
DT080    L     R2,=A(RPTNAMES)     ->   REPORT    NAMES     TABLE               
*                                                                               
DT090    CLI   0(R2),EOT           END  OF   TABLE ?                            
         BE    DT100               YES, DONE                                    
*                                  TRANSLATE REPORT    TYPE NAME                
         GOTO1 EXPDNTRY,DMCB,(L'RPTTDIC,RPTTDIC),RPTTTEXT                       
         LA    R2,RPTTYPLQ(,R2)    ->   NEXT REPORT    TYPE ENTRY               
         B     DT090               TRANSLATE NEXT KEYWORD                       
         DROP  R2                  KEEP IT   CLEAN                              
*                                                                               
DT100    DS    0H                                                               
         EJECT ,                                                                
         USING CTWREC,R6                                                        
         XC    SKEY,SKEY           CLEAR SYS  LIST REC  KEY                     
         LA    R6,SKEY             ->    SYSTEM    LIST KEY                     
         MVI   CTWKTYP,CTWKTYPQ    REC   TYPE      C'W' SYSTEM LISTS            
         MVI   CTWKREC,CTWKRSYS    SUB   REC  TYPE C'S' SYSTEM LIST             
         B     REQF05                                                           
*                                                                               
REQFNXAC DS    0H                  GET   TO   NEXT ACCPAK    RECORD             
         TM    READSEQ,READSSEQ    READ  SEQUENTIAL ?                           
         BO    REQF10              YES,  SKIP READ HIGH                         
*                                                                               
REQF05   DS    0H                                                               
         BAS   RE,DMCTHGH          READ  HIGH                                   
         L     R6,AIOSL            ->    I/O  AREA                              
*                                  SAME  KEY  ?                                 
         CLC   SKEY(CTWDATA-CTWREC),0(R6)                                       
         BNE   REQF15              NO,   GOT  NEXT SYS  LIST RECORD             
*                                                                               
REQF10   DS    0H                  READ  NEXT SEQUENTIAL     RECORD             
         BAS   RE,DMCTSEQ                                                       
*                                                                               
REQF15   DS    0H                                                               
         OI    READSEQ,READSSEQ    SO    FAR  CAN  READ SEQUENTIAL              
         L     R6,AIOSL            ->    I/O  AREA                              
*                                  SAVE  DIRECTORY KEY                          
         MVC   SKEY(CTWDATA-CTWREC),0(R6)                                       
*                                                                               
         CLI   CTWKTYP,CTWKTYPQ    SYSTEM     LIST RECORD ?                     
         BNE   REQFEX              NO,   EXIT                                   
         CLI   CTWKREC,CTWKRSYS    SYSTEM     LIST ?                            
         BNE   REQF30              NO,   GET  NEXT RECORD                       
*&&US*&& CLI   CTWKSYSN,CTWKACC    SYSTEM     NUMBER =  ACCPAK ?                
*&&UK*&& CLI   CTWKSYSN,X'06'      SYSTEM     NUMBER =  ACCPAK ?                
         BNE   REQF30              NO,   GET  NEXT RECORD                       
*                                                                               
         USING CTLSTD,R5           LIST  ELEMENT   DSECT                        
         LA    R5,CTWDATA          ->    ELEMENTS                               
REQF20   DS    0H                                                               
         CLI   0(R5),X'00'         END   OF   RECORD ?                          
         BE    REQF30              YES,  GET  NEXT RECORD                       
         CLI   CTLSTEL,CTLSTELQ    X'A4' ELEMENT ?                              
         BNE   REQF25              NO,   GET  NEXT ELEMENT                      
*&&US*&& CLI   CTLSTDTA+7,CTWKACC  ACCPAK     LIST DATA ?                       
*&&UK*&& CLI   CTLSTDTA+7,X'06'    ACCPAK     LIST DATA ?                       
         BNE   REQF25              NO,   GET  NEXT ELEMENT                      
*                                  YES,  FOUND     ACCPAK    LIST DATA          
         CLI   QSELECT+2,C' '      LIMIT THE  ACCPACK   SYSTEMS ?               
         BE    REQF22              NO,   CONTINUE                               
*                                  FOUND REQUESTED SYSTEM ?                     
         CLC   QSELECT+2(1),CTLSTDTA+3                                          
         BNE   REQF25              NO,   GET  NEXT ELEMENT                      
*                                                                               
REQF22   DS    0H                                                               
         MVC   SYSSE,CTLSTDTA+8    SAVE  SE   NUMBER                            
         MVC   ACCSYSID,CTLSTDTA   SAVE  ACCPAK    SYSTEM    ID                 
*                                                                               
         L     R2,AUTL             ->    UTL                                    
         MVC   4(1,R2),SYSSE       SET   UTL  FOR  THIS ACCPAK    FILE          
*                                                                               
         L     R2,AIOSL                                                         
         BAS   RE,DMOPNACC         OPEN  ACC  FILE                              
*                                  TURN  OFF  READ SEQUENTIAL                   
         NI    READSEQ,X'FF'-READSSEQ                                           
         BAS   RE,RS               GET   FIRST     ACCOUNT   RECORD             
*                                                                               
         L     R2,AUTL             ->    UTL                                    
         MVC   4(1,R2),SAVESE      RESTORE    SE   ID   FOR  CTL  FILES         
*                                                                               
REQF25   DS    0H                  GET   TO   NEXT ACCPAK    ELEMENT            
         ZIC   R2,1(,R5)           CURR  ELEMENT   LENGTH                       
         AR    R5,R2               NEXT  ELEMENT   ADDR                         
         B     REQF20              TRY   NEXT ELEMENT                           
*                                                                               
REQF30   DS    0H                  GET   NEXT RECORD                            
         B     REQFNXAC            GET   NEXT SEQUENTIAL     RECORD             
         DROP  R5,R6                                                            
*                                                                               
         USING PSD,R4                                                           
REQFEX   DS    0H                  EXIT                                         
         GOTO1 ACREPORT            SKIP  A    LINE                              
         LA    R4,P                                                             
         MVC   PSDSYSIH(20),=C'TOTAL OF ALL SYSTEMS'                            
         ZAP   WORK(8),FTOTFNDS         GET  NUMBER OF MATCHES                  
         OI    WORK+7,X'0F'                                                     
         MVC   PSDRMATH,=C'RECORDS MATCHED='                                    
         UNPK  PSDRMAT,WORK(8)                                                  
         ZAP   WORK(8),FTOTRECS         GET  NUMBER OF RECORDS                  
         OI    WORK+7,X'0F'                                                     
         MVC   PSDTRECH,=C'TOTAL RECORDS='                                      
         UNPK  PSDTREC,WORK(8)                                                  
         GOTO1 ACREPORT                                                         
*                                                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT ,                                                                
         USING ACKEYD,R3                                                        
RS       NTR1                                                                   
         XC    XKEY,XKEY                                                        
         XC    PRVKEY,PRVKEY                                                    
         LA    R3,XKEY                                                          
         MVI   ACCSTYPE,ACCSEQU    SET KEY FOR RS RECORDS                       
         MVI   ACCSSREC,ACCSSEQU                                                
         CLI   FILTSEL,0                                                        
         BZ    *+10                                                             
         MVC   ACCSCMP,FILTSEL                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCOUNT,XKEY,AIO                             
*                                                                               
RS10     DS    0H                                                               
         L     R3,AIO                                                           
         CLC   0(2,R3),XKEY        CHECK RECORD TYPE                            
         BNE   RS90                                                             
         CLC   0(3,R3),PRVKEY      SAME COMP CODE?                              
         BE    RS20                YES, PROCESS   NEW  FORMAT                   
         MVC   SVFORMNM,SPACES     CLEAR     SAVED     FORMAT    NAME           
         NC    PRVKEY,PRVKEY       1ST  ENTRY ?                                 
         BZ    RS12                YES, SKIP                                    
         BAS   RE,PRTCPYTO         PRINT     COMPANY   TOTALS                   
*                                                                               
         CLI   FILTSEL,0           COMPANY FILTERING                            
         BE    RS12                NO,  SKIP                                    
         CLC   FILTSEL,ACCSCMP     YES, SHOW ONLY ONE COMPANY                   
         BNE   RS95                                                             
*                                                                               
RS12     DS    0H                  GET  COMPANY   DATA                          
         L     R3,AIO              ->   RECORD                                  
         MVC   SVKEY,0(R3)         SAVE RCD  KEY                                
         MVC   CLOGO,SPACES        CLEAR     COMPANY   LOGO                     
         MVC   CALPHA,SPACES       CLEAR     COMPANY   ALPHA     CODE           
         MVC   CKEY,SPACES         CLEAR     COMPANY   KEY                      
         MVC   CKEY(1),ACCSCMP     COMPANY   ID                                 
         BAS   RE,DMCTRDC          READ COMPANY   RECORD                        
         L     R3,AIO              ->   RECORD                                  
         LA    R4,ACRECORD         ->   1ST  ELEMENT                            
*                                                                               
RS14     DS    0H                                                               
         CLI   0(R4),0             END  OF   RECORD ?                           
         BE    RS18                YES, RESTORE   RECORD                        
         CLI   0(R4),CPYELQ        COMPANY   ELEMENT ?                          
         BE    RS16                YES, PROCESS                                 
         ZIC   R1,1(,R4)           GET  ELEMENT   LENGTH                        
         AR    R4,R1               BUMP TO   NEXT ELEMENT                       
         B     RS14                TRY  NEXT ELEMENT                            
*                                                                               
         USING CPYELD,R4           MAP  COMPANY   ELEMENT                       
RS16     MVC   CLOGO,CPYLOGO       GET  COMPANY   LOGO                          
         MVC   CALPHA,CPYALPHA     GET  COMPANY   ALPHA     CODE                
         DROP  R4                                                               
*                                                                               
RS18     BAS   RE,DMCTRDSV         RESTORE   RECORD                             
*                                                                               
RS20     L     R3,AIO                                                           
         MVC   PRVKEY,0(R3)                                                     
         MVC   XKEY,0(R3)                                                       
         MVI   WRTSW,NO            SET    WRITE     SWITCH                      
         MVI   FOUNDSW,0           CLEAR  FOUND     SWITCH                      
         MVI   FOUNDSW2,0          CLEAR  EXCLUDE   SWITCH                      
         MVI   FOUND3,0                                                         
         MVI   DMPSW,YES           SET    DUMP SWITCH                           
         MVC   REPTCODE,SPACES                                                  
         MVC   REPTNAME,SPACES                                                  
         MVC   FNDKEYWD,SPACES                                                  
         MVC   FNDRPTYP,SPACES                                                  
         MVC   REQDATED,SPACES                                                  
         MVC   REQUEST#,SPACES                                                  
*                                                                               
         USING FILTD,RF                                                         
         L     RF,=A(FILTERS)                                                   
RS21     CLI   0(RF),EOT                                                        
         BE    RS22                                                             
         XC    FILTADR,FILTADR                                                  
         LA    RF,FILTERLQ(,RF)                                                 
         B     RS21                                                             
         DROP  RF                                                               
*                                                                               
RS22     LA    R4,ACRECORD                                                      
         CLI   FILTSEL,0                                                        
         BE    RS25                                                             
         CLC   FILTSEL,2(R3)       MATCH  ON   COMPANY                          
         BE    RS25                YES,   CONTINUE                              
         OI    FOUNDSW2,FNDSKIPR   SKIP   THIS RECORD                           
         B     RS70                                                             
         DROP  R3                                                               
*                                                                               
         USING RESRECD,R3          SCRIBE RECORD                                
RS25     DS    0H                                                               
         MVC   FORMNAME,RESKFORM   SAVE   FORMAT    NAME                        
         OC    FORMNAME,FORMNAME   IS     FORMAT    NULL ?                      
         BNZ   RS28                NO,    SKIP                                  
         OI    FOUNDSW,FNDRPTTN    YES,   INDICATE  FOUND                       
         MVC   FNDRPTYP,=CL6'*NULL*'                                            
         MVC   FORMNAME,=CL8'*-NULL-*'                                          
         B     RS30                CONTINUE                                     
*                                                                               
RS28     DS    0H                                                               
         CLI   RESKSEQ,RESKSTXT    TEXT   ONLY RECORD ?                         
         BNE   RS30                NO,    CONTINUE                              
         CLC   FORMNAME,SVFORMNM   SAME   FORMAT    NAME ?                      
         BNE   *+8                 NO,    SKIP                                  
         OI    FOUNDSW2,FNDSKIPR   YES,   SKIP RECORD                           
         DROP  R3                                                               
*                                                                               
RS30     DS    0H                                                               
         TM    FOUNDSW2,FNDSKIPR   SKIP   RECORD ?                              
         BO    RS70                DONE                                         
         CLI   0(R4),0                                                          
         BE    RS70                                                             
         MVI   TRANSLAT,NO         ASSUME NOT  TRANSLATED                       
         CLI   0(R4),FFNELQ        X'25'  FREE FORM SCRIBE (STYELQ)             
         BE    RP10                                                             
         CLI   0(R4),PACELQ        X'A1'  PERSON    ACTIVITY ELEMENT            
         BE    A110                                                             
         CLI   0(R4),X'C0'         X'C0'  OLD       SCRIBE   ELEMENT            
         BE    OLDEL                                                            
         CLI   0(R4),RRWELQ        X'C2'  ROW       ELEMENT                     
         BE    RW10                                                             
         CLI   0(R4),RCLELQ        X'C3'  COLUMN    ELEMENT                     
         BE    CL10                                                             
         CLI   0(R4),RPFELQ        X'C4'  PROFILE   ELEMENT                     
         BE    PF10                                                             
         CLI   0(R4),RFLELQ        X'C5'  FILTER    ELEMENT                     
         BE    FL10                                                             
         CLI   0(R4),PTRELQ        X'FA'  POINTER   ELEMENT                     
         BE    FA10                                                             
         CLI   0(R4),DTSELQ        X'FB'  DATE/TIME STAMP                       
         BE    FB10                                                             
*                                                                               
RS35     DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,1(,R4)                                                        
         AR    R4,R1                                                            
         B     RS30                                                             
         EJECT ,                                                                
         USING PACELD,R4           PERSON ACTIVITY    ELEMENT  X'A1'            
A110     DS    0H                                                               
         LA    RF,PACDATE          LAST   ACTIVITY    DATE                      
         GOTO1 DATCON,DMCB,(1,(RF)),(21,LASTACTD)                               
         B     RS35                                                             
         DROP  R4                                                               
         EJECT ,                                                                
         USING PTRELD,R4           POINTER     ELEMENT     X'FA'                
FA10     DS    0H                                                               
         ZIC   R1,PTRLN            ELEMENT     LENGTH                           
         SH    R1,=Y(PTRLN1Q)                                                   
         SRL   R1,2                DIVIDE BY   4                                
         CVD   R1,DUB              NUMBER OF   REQUESTS                         
         OI    DUB+7,X'0F'                                                      
         UNPK  REQUEST#,DUB        SAVE   NUMBER                                
         B     RS35                                                             
         DROP  R4                                                               
*                                                                               
         USING DTSELD,R4           DATE/TIME   STAMP     ELEMENT  X'FB'         
FB10     DS    0H                  REQUEST     DATE                             
         GOTO1 DATCON,DMCB,(2,DTSDATE),(1,REQDATE)                              
         CLC   REQDATE,RQSTART                                                  
         BL    FB95                                                             
         CLC   REQDATE,RQEND                                                    
         BH    FB95                                                             
         GOTO1 DATCON,DMCB,(2,DTSDATE),(21,REQDATED)                            
         B     RS35                CONTINUE                                     
*                                                                               
FB95     DS    0H                  SKIP   ELEMENT                               
         OI    FOUNDSW2,FNDSKIPR   IGNORE THIS RECORD                           
         B     RS35                CONTINUE                                     
         DROP  R4                                                               
         EJECT ,                                                                
         USING STYELD,R4           FREE   FORM   SCRIBE      EL   X'25'         
RP10     DS    0H                  REPORT TYPE                                  
         OI    FOUNDSW2,FNDFFEL    FOUND  FREE   FORM   ELEMENT                 
         MVC   REPTCODE,STYCODE    SAVE   REPORT TYPE   CODE                    
         MVC   REPTNAME,STYNAME    SAVE   REPORT TYPE   NAME                    
*                                  INVALID       FORMAT ?                       
         CLC   FNDRPTYP,=CL6'*NULL*'                                            
         BNE   *+10                NO,    SKIP                                  
         MVC   FNDRPTYP,REPTNAME   YES,   INSERT REPORT TYPE NAME               
         CLI   LIMRTYPS,YES        LIMIT  REPORT TYPES                          
         BNE   RP30                NO,    SKIP                                  
         L     RE,=A(RPTTYPES)     ->     REPORT TYPES TABLE                    
*                                                                               
         USING RPTTYPED,RE         REPORT TYPE   ENTRY                          
RP20     DS    0H                                                               
         CLI   0(RE),EOT           END    OF     TABLE ?                        
         BE    RP95                YES,   SKIP   RECORD                         
         CLC   RPTTDIC,REPTNAME    MATCH  DICTIONARY   ENTRY ?                  
         BE    RP30                YES,   GOOD   ENTRY                          
         CLC   RPTTTEXT,REPTNAME   MATCH  TEXT   ENTRY ?                        
         BE    RP30                YES,   GOOD   ENTRY                          
         LA    RE,RPTTYPLQ(,RE)    TRY    NEXT   ENTRY                          
         B     RP20                                                             
*                                                                               
RP30     DS    0H                  GOOD   ENTRY                                 
         CLI   QOPT2,TRANRPTS      SEARCH FOR    TRANSLATED REPORTS ?           
         BNE   RP50                NO,    SKIP                                  
         CLI   REPTNAME,ESCHIGHQ   ABOVE  ESCAPE SEQUENCE ?                     
         BH    RP90                YES,   CONTINUE                              
         OI    FOUNDSW,FNDRPTTN    FOUND  SPECIAL      REPORT TYPE              
         OC    REPTNAME,REPTNAME   NULL   REPORT TYPE  ?                        
         BNZ   RP40                NO,    SKIP                                  
         MVC   REPTNAME,=CL8'**NULL**'                                          
         B     RP90                CONTINUE                                     
*                                                                               
RP40     DS    0H                                                               
         MVC   FNDRPTYP,SPACES     CLEAR  FOR    EXPDNTRY                       
         GOTO1 EXPDNTRY,DMCB,(L'REPTNAME,REPTNAME),FNDRPTYP                     
         B     RP90                CONTINUE                                     
*                                                                               
RP50     DS    0H                                                               
         CLI   QOPT2,CHARRPTS      SEARCH FOR    CHARACTER  REPORTS ?           
         BNE   RP60                NO,    SKIP                                  
         CLI   REPTNAME,ESCHIGHQ   ABOVE  ESCAPE SEQUENCE ?                     
         BNH   RP90                NO,    CONTINUE                              
         OI    FOUNDSW,FNDRPTTN    FOUND  SPECIAL      REPORT TYPE              
         MVC   FNDRPTYP,REPTNAME                                                
         B     RP90                CONTINUE                                     
*                                                                               
RP60     DS    0H                                                               
         CLI   QOPT2,CHARRPTM      SEARCH FOR    MISSING    CHAR RPTS ?         
         BNE   RP90                NO,    SKIP                                  
         CLI   REPTNAME,ESCHIGHQ   ABOVE  ESCAPE SEQUENCE ?                     
         BNH   RP90                NO,    CONTINUE                              
         L     RE,=A(RPTNAMES)     ->     REPORT NAMES TABLE                    
*                                                                               
*                                                                               
         USING RPTTYPED,RE         REPORT TYPE   ENTRY                          
RP70     DS    0H                                                               
         CLI   0(RE),EOT           END    OF     TABLE ?                        
         BE    RP80                YES,   GOOD   RECORD                         
         CLC   RPTTTEXT,REPTNAME   MATCH  TEXT   ENTRY ?                        
         BE    RP90                YES,   GOOD   ENTRY                          
         LA    RE,RPTTYPLQ(,RE)    TRY    NEXT   ENTRY                          
         B     RP70                                                             
*                                                                               
RP80     DS    0H                  REPORT NAME   NOT   IN     TABLE             
         OI    FOUNDSW,FNDRPTTN    FOUND  SPECIAL      REPORT TYPE              
         MVC   FNDRPTYP,REPTNAME                                                
         B     RP90                CONTINUE                                     
*                                                                               
RP90     DS    0H                  GOOD   ENTRY                                 
         MVC   WORK(L'REPTNAME),REPTNAME                                        
         MVC   REPTNAME,SPACES     CLEAR  FOR    EXPDNTRY                       
         GOTO1 EXPDNTRY,DMCB,(L'REPTNAME,WORK),REPTNAME                         
*                                                                               
         TM    STYSTAT,STYS2ND     MORE   THAN   ONE    RECORD ?                
         BZ    RPEX                NO,    EXIT                                  
         BAS   RE,DMCTARD          REREAD LAST   ACCOUNT     RECORD             
         BAS   RE,DMCTAOVR         READ   OVERFLOW      RECORD                  
*                                                                               
         USING RESRECD,R3          MAP    SCRIBE RECORD                         
         L     R3,AIO              ->     FIRST  RECORD                         
         LH    R0,RESRLEN          GET    LENGTH OF     1ST  RECORD             
         BCTR  R0,0                MINUS  ONE                                   
         AR    R0,R3               GET    ADDR   TO     EXTEND                  
         L     R3,AIOOVR           GET    ADDR   OF     2ND  RECORD             
         MVC   XKEY,0(R3)          SAVE   LAST   RCD    READ                    
         LH    RF,RESRLEN          GET    LENGTH OF     2ND  RECORD             
*                                  GET    LENGTH TO     BE   MOVED              
         SH    RF,=Y(ACRECORD-ACKEYD)                                           
*                                                                               
         USING ACKEYD,R3           MAP    GENERAL       RECORD                  
         LA    RE,ACRECORD         ADDR   OF     SECOND RCD  DATA               
         LR    R1,RF               GET    LENGTH TO     BE   MOVED              
         MVCL  R0,RE               MOVE   2ND    RECORD TO   1ST  RCD           
         L     R3,AIO              ->     FIRST  RECORD                         
         B     RPEX                EXIT                                         
*                                                                               
RP95     DS    0H                  SKIP   RECORD                                
         OI    FOUNDSW2,FNDSKIPR   SKIP   RECORD                                
*                                                                               
RPEX     DS    0H                  EXIT                                         
         B     RS35                RETURN                                       
         DROP  R3,R4,RE                                                         
         EJECT ,                                                                
         USING RRWELD,R4                                                        
RW10     DS    0H                  ROW    ELEMENT  X'C2'                        
         TM    RRWOPT,RRWNEWEL     NEW    ROW  ELEMENT ?                        
         BO    RW20                YES,   SKIP                                  
         LA    RE,RRWDATA                                                       
         LA    R1,L'RRWDATA                                                     
         B     RW30                CONTINUE                                     
*                                                                               
RW20     DS    0H                                                               
         LA    RE,RRWNDATA         ->     ROW  ELEMENT                          
         SR    R1,R1                                                            
         ICM   R1,1,RRWDATLN       ROW    DATA LENGTH                           
         BZ    RS35                NONE,  RETURN                                
*                                                                               
RW30     DS    0H                                                               
         TM    RRWOPT2,RRWDICT     TRANSLATED ROW ?                             
         BZ    *+8                 NO,    SKIP                                  
         MVI   TRANSLAT,YES        SAY    TRANSLATED                            
         B     GEN00                                                            
         EJECT ,                                                                
         USING RCLELD,R4                                                        
CL10     DS    0H                  COLUMN ELEMENT  X'C3'                        
*        TM    RCLOPT,RCLEQU                                                    
*        BZ    RS35                                                             
         MVC   P,SPACES                                                         
         TM    RCLOPT,RCLNEWEL     NEW    COL  ELEMENT ?                        
         BO    CL20                YES,   SKIP                                  
         LA    RE,RCLDATA                                                       
         LA    R1,L'RCLDATA                                                     
         B     CL30                CONTINUE                                     
*                                                                               
CL20     DS    0H                                                               
         LA    RE,RCLNDATA         ->     COL  ELEMENT                          
         SR    R1,R1                                                            
         ICM   R1,1,RCLDATLN       COL    DATA LENGTH                           
         BZ    RS35                NONE,  RETURN                                
*                                                                               
CL30     DS    0H                                                               
         MVC   P+2(8),FORMNAME                                                  
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   P+15(0),0(RE)                                                    
         GOTO1 ACREPORT                                                         
         TM    RCLOPT2,RCLDICT     TRANSLATED COLUMN ?                          
         BZ    *+8                 NO,    SKIP                                  
         MVI   TRANSLAT,YES        SAY    TRANSLATED                            
         B     GEN00                                                            
         DROP  R4                                                               
         EJECT ,                                                                
GEN00    DS    0H                                                               
         MVC   WORK,SPACES                                                      
         LA    RF,WORK                                                          
         CLI   TRANSLAT,YES        TRANSLATED     DATA ?                        
         BE    GEN10               YES, SKIP                                    
*                                                                               
GEN05    DS    0H                  FIND THE  KEYWORD   WHEN WE   HAVE           
         CLI   0(RE),C' '               CHARACTER DATA                          
         BE    GEN10                                                            
         CLI   0(RE),C','                                                       
         BE    GEN10                                                            
         CLI   0(RE),X'00'                                                      
         BE    GEN10                                                            
         CLI   0(RE),X'01'                                                      
         BE    GEN10                                                            
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(,RF)                                                        
         LA    RE,1(,RE)                                                        
         BCT   R1,GEN05                                                         
*                                                                               
GEN10    DS    0H                  IS    THE  KEYWORD   IN   TABLE ?            
         L     RF,=A(KEYWRDS)                                                   
*                                                                               
         USING KEYWRDSD,RF                                                      
*                                                                               
GEN20    DS    0H                                                               
         CLI   0(RF),EOT           END  OF   TABLE ?                            
         BNE   GEN25               NO,  CONTINUE                                
         CLI   QOPT1,CHARALL       ALL  CHAR KEYWORDS ?                         
         BNE   GEN60               NO,  INCLUDE   KW   TBL  DONE                
         CLI   TRANSLAT,YES        TRANSLATED     DATA ?                        
         BE    GEN60               YES, INCLUDE   KW   TBL  DONE                
         CLI   0(R4),RRWELQ        ROW  ELEMENT ?                               
         BE    GEN22               YES, FOUND     KEYWORD                       
*                                                                               
         USING RCLELD,R4           COLUMN    ELEMENT                            
*                                                                               
         TM    RCLOPT,RCLEQU       EQUATION ?                                   
         BO    GEN60               YES, INCLUDE   KW   TBL  DONE                
         CLI   RCLSPCL,0           SPECIAL   COLUMN ?                           
         BE    GEN22               NO,  FOUND     KEYWORD                       
         CLI   RCLSPCL,RCLSPCME    EXCHANGE  RATE OR   CUME ?                   
         BNH   GEN60               YES, INCLUDE   KW   TBL  DONE                
*                                                                               
GEN22    DS    0H                                                               
         OI    FOUNDSW,FNDKYW      KEYWORD   FOUND                              
         MVC   FNDKEYWD,WORK       SAVE KEYWORD                                 
         B     GEN90                                                            
*                                                                               
GEN25    DS    0H                                                               
         CLI   TRANSLAT,YES        TRANSLATED     DATA ?                        
         BE    GEN30               YES, SKIP                                    
         CLI   QOPT1,TRANONLY      TRANSLATED     ONLY REQUESTED ?              
         BE    GEN60               YES, SKIP                                    
         CLC   WORK(6),KEYWTEXT    TEXT MATCH ?                                 
         BE    GEN50               YES, FOUND     KEYWORD                       
         B     GEN40               NO,  CONTINUE                                
*                                                                               
GEN30    DS    0H                                                               
         CLI   QOPT1,CHARONLY      CHARACTER      ONLY REQUESTED ?              
         BE    GEN60               YES, SKIP                                    
         CLC   0(2,RE),KEYWDIC+1   DICTIONARY     ENTRY     MATCH ?             
         BE    GEN50               YES, FOUND     KEYWORD                       
*                                                                               
GEN40    DS    0H                                                               
         LA    RF,KEYWRDLQ(,RF)    NEXT KEYWORD   ENTRY                         
         B     GEN20                                                            
*                                                                               
GEN50    DS    0H                                                               
         OI    FOUNDSW,FNDKYW      KEYWORD   FOUND                              
         MVC   FNDKEYWD,KEYWTEXT   SAVE TEXT                                    
         B     GEN90                                                            
*                                                                               
GEN60    DS    0H                                                               
         L     RF,=A(KEYWRDS2)     ->   EXCLUDE   TABLE                         
*                                                                               
GEN70    DS    0H                                                               
         CLI   0(RF),EOT                                                        
         BE    GEN90                                                            
         CLI   TRANSLAT,YES        TRANSLATED     DATA ?                        
         BE    GEN75               YES, SKIP                                    
         CLI   QOPT1,TRANONLY      TRANSLATED     ONLY REQUESTED ?              
         BE    GEN90               YES, SKIP                                    
         CLC   WORK(6),KEYWTEXT    TEXT MATCH ?                                 
         BE    GEN85               YES, FOUND     EXCLUDE                       
         B     GEN80               NO,  CONTINUE                                
*                                                                               
GEN75    DS    0H                                                               
         CLI   QOPT1,CHARONLY      CHARACTER      ONLY REQUESTED ?              
         BE    GEN90               YES, SKIP                                    
         CLC   0(2,RE),KEYWDIC+1   DICTIONARY     ENTRY     MATCH ?             
         BE    GEN85               YES, FOUND     EXCLUDE                       
*                                                                               
GEN80    DS    0H                                                               
         LA    RF,KEYWRDLQ(,RF)    NEXT KEYWORD   ENTRY                         
         B     GEN70                                                            
*                                                                               
GEN85    DS    0H                                                               
         OI    FOUNDSW2,FNDEXC     IGNORE    THIS RECORD                        
         B     GEN90                                                            
         DROP  RF                                                               
*                                                                               
GEN90    B     RS35                                                             
         EJECT ,                                                                
         USING RPFELD,R4           PROFILE       DATA ELEMENT   X'C4'           
PF10     DS    0H                  PROFILE   ELEMENT                            
         B     RS35                                                             
*                                                                               
         DROP  R4                                                               
         EJECT ,                                                                
         USING RFLELD,R4                                                        
         USING FILTD,RF                                                         
FL10     DS    0H                  FILTER    ELEMENT   X'C5'                    
         L     RF,=A(FILTERS)                                                   
         CLI   RFLTYPE,RFLLDG      FND  UNIT/LEDGER ?                           
         BNE   FL20                NO,  SKIP                                    
         OI    FOUNDSW2,FNDULEL    SAY  FOUND     UNIT/LEDGER                   
*                                                                               
FL20     CLI   0(RF),EOT           END  OF   TABLE ?                            
         BE    FL90                YES, EXIT                                    
         CLC   RFLTYPE,FILTTYPE    FILTER    MATCH ?                            
         BNE   FL32                YES, GOOD                                    
         CLI   FILTTYPE,RFLWC                                                   
         BE    FL22                                                             
         OI    FOUND3,FNDWCT                                                    
         B     FL23                                                             
*                                                                               
FL22     OI    FOUND3,FNDWC                                                     
*                                                                               
FL23     OI    FOUNDSW,FNDFILT     FOUND FILTER                                 
         STCM  R4,15,FILTADR       SAVE OFF ADDRESS OF FILTER                   
*                                                                               
FL32     LA    RF,FILTERLQ(,RF)    NEXT FILTER    ENTRY                         
         B     FL20                                                             
*                                                                               
FL90     B     RS35                                                             
         DROP  R4,RF                                                            
         EJECT                                                                  
         SPACE 1                                                                
OLDEL    DS    0H                  OLD  SCRIBE    ELEMENT   X'C0'               
         OI    FOUNDSW,FNDC0EL     FOUND     OLD  SCRIBE    ELEMENT             
         B     RS35                CONTINUE                                     
         EJECT ,                                                                
***********************************************************************         
*  END OF FORMAT                                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R3                                                       
RS70     L     R3,AIO                                                           
         MVC   SVFORMNM,FORMNAME   SAVE FORMAT    NAME                          
*                                                                               
         TM    FOUNDSW2,FNDSKIPR   SKIP RECORD    ON ?                          
         BO    RS80                YES, SKIP                                    
*                                                                               
         TM    FOUNDSW2,FNDULEL    ANY  UNIT/LEDGER    ELEMENT ?                
         BO    *+8                 YES, SKIP                                    
         OI    FOUNDSW,FNDMULEL    NO,  MISSING   UNIT/LEDGER                   
*                                                                               
         TM    FOUNDSW2,FNDFFEL    ANY  FREE FORM (STYELD)  ELEMENT ?           
         BO    *+8                 YES, SKIP                                    
         OI    FOUNDSW,FNDMFFEL    NO,  MISSING   FREE FORM ELEMENT             
*                                                                               
         TM    FOUND3,FNDWCT                                                    
         BO    *+8                                                              
         NI    FOUNDSW,X'FF'-FNDFILT                                            
         CLI   FOUNDSW,0           ANYTHING  TO   REPORT ?                      
         BE    RS80                NO,  SKIP                                    
*                                  FOUND     REPORT    TYPE      OR             
*                                            OLD  SCRIBE    EL   OR             
*                                            MISSING   U/L  ELEMENT ?           
         TM    FOUNDSW,FNDRPTTN+FNDC0EL+FNDMULEL                                
         BNZ   RS70B               YES, CONTINUE                                
         TM    FOUNDSW2,FNDEXC     EXCLUDE   FOUND ?                            
         BO    RS80                YES, SKIP THIS RECORD                        
*                                                                               
RS70B    DS    0H                                                               
         AP    CPYFNDS,=P'1'       KEEP COUNT                                   
         AP    TOTFNDS,=P'1'       KEEP COUNT                                   
         AP    FTOTFNDS,=P'1'      KEEP COUNT                                   
*                                                                               
         USING PDLD,R4             DETAIL    OUTPUT    LINE                     
         LA    R4,P                                                             
         TM    FOUNDSW,FNDRPTTN    FOUND     REPORT    TYPE ?                   
         BZ    RS70C               NO,  SKIP                                    
         BAS   RE,SETUPPLN         SET  UP   PRINT     LINE                     
         MVC   PDLFNDWH,=CL15'FND REPORT TYPE'                                  
         MVC   PDLFNDKW(L'FNDRPTYP),FNDRPTYP      REPORT    TYPE                
         GOTO1 ACREPORT                                                         
*                                                                               
RS70C    DS    0H                                                               
         TM    FOUNDSW,FNDC0EL     OLD       SCRIBE         RECORD ?            
         BZ    RS70E               NO,  SKIP                                    
         BAS   RE,SETUPPLN         SET  UP   PRINT     LINE                     
         MVC   PDLFNDWH,=CL15'FND X''C0'' EL'                                   
         GOTO1 ACREPORT                                                         
*                                                                               
RS70E    DS    0H                                                               
         TM    FOUNDSW,FNDMULEL    MISSING   UNIT/LEDGER ?                      
         BZ    RS70G               NO,  SKIP                                    
         BAS   RE,SETUPPLN         SET  UP   PRINT     LINE                     
         MVC   PDLFNDWH,=CL15'MISSING U/L'                                      
         GOTO1 ACREPORT                                                         
*                                                                               
RS70G    DS    0H                                                               
         TM    FOUNDSW,FNDMFFEL    MISSING   FREE FORM (STYELD)  EL ?           
         BZ    RS70I               NO,  SKIP                                    
         BAS   RE,SETUPPLN         SET  UP   PRINT     LINE                     
         MVC   PDLFNDWH,=CL15'MISSING STYELD'                                   
         GOTO1 ACREPORT                                                         
*                                                                               
RS70I    DS    0H                                                               
*                                                                               
RS70Z    DS    0H                                                               
         TM    FOUNDSW2,FNDEXC     EXCLUDE   FOUND ?                            
         BO    RS75                YES, DONE                                    
         TM    FOUNDSW,FNDKYW+FNDFILT                                           
*                                  FOUND     KEYWORD   OR   FILTER ?            
         BZ    RS75                NO,  DONE                                    
         BAS   RE,SETUPPLN         SET  UP   PRINT     LINE                     
*                                                                               
         DS    0H                  FOUND     KEYWORD AND/OR FILTER ?            
         TM    FOUNDSW,FNDKYW      FOUND     KEYWORD ?                          
         BZ    RS72                NO,  PROCESS   FILTER                        
         TM    FOUNDSW,FNDFILT     FOUND     FILTER ALSO ?                      
         BO    RS71                YES, PROCESS   BOTH                          
*                                  FOUND     KEYWORD     ONLY                   
         MVC   PDLFNDWH,=CL15'FOUND KEYWORD'                                    
         MVC   PDLFNDKW,FNDKEYWD   KEYWORD                                      
         B     RS74                                                             
*                                                                               
RS71     DS    0H                  FOUND     KEYWORD   AND  FILTER              
         MVC   PDLFNDWH,=CL15'FND KW + FILTER'                                  
         MVC   PDLFNDKW,FNDKEYWD   KEYWORD                                      
         B     RS74                                                             
*                                                                               
RS72     DS    0H                  FOUND     FILTER   ONLY                      
         MVC   PDLFNDWH,=CL15'FOUND FILTER'                                     
*                                                                               
RS74     DS    0H                                                               
         GOTO1 ACREPORT                                                         
         DROP  R4                                                               
*                                                                               
         USING FILTD,R4                                                         
         USING RFLELD,RE                                                        
         TM    FOUNDSW,FNDFILT     FOUND FILTER ?                               
         BZ    RS75                                                             
         L     R4,=A(FILTERS)                                                   
RS74A    CLI   0(R4),EOT                                                        
         BE    RS75                                                             
         ICM   RE,15,FILTADR                                                    
         BZ    RS74L                                                            
         CLI   0(RE),RFLELQ        X'C5'                                        
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+3(L'FILTDESC),FILTDESC                                         
*                                                                               
         MVI   P+3+L'FILTDESC,C'('                                              
         SR    R1,R1                                                            
         IC    R1,RFLSEQ                                                        
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+4+L'FILTDESC(2),DUB                                            
         MVI   P+6+L'FILTDESC,C')'                                              
*                                                                               
         TM    RFLIND,RFLXCLD                                                   
         BZ    *+8                                                              
         MVI   P+9+L'FILTDESC,C'*'                                              
*                                                                               
         SR    R1,R1                                                            
         IC    R1,RFLLN                                                         
         CLI   RFLTYPE,RFLTTYPE    TRANSACTION TYPE ?                           
         BE    RS74B2                                                           
         CLI   RFLTYPE,RFLBUDGT    BUDGET ?                                     
         BE    RS74B1                                                           
         AHI   R1,-(RFLLNQ+1)                                                   
         BM    RS74L                                                            
         EX    R1,*+4                                                           
         MVC   P+10+L'FILTDESC(0),RFLDATA                                       
         GOTO1 ACREPORT                                                         
         B     RS74L                                                            
*                                                                               
RS74B1   SR    RF,RF                                                            
         ICM   RF,3,RFLDATA                                                     
         LA    R1,1                                                             
         LA    RE,RFLDATA                                                       
         LA    R6,P+10+L'FILTDESC                                               
         B     RS74C2                                                           
*                                                                               
RS74B2   AHI   R1,-RFLLNQ                                                       
         SR    RF,RF                                                            
         LA    RE,RFLDATA                                                       
         LA    R6,P+10+L'FILTDESC                                               
*                                                                               
RS74C1   IC    RF,0(,RE)                                                        
RS74C2   CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB                                                      
*                                                                               
         LA    RF,2                                                             
         LA    R5,WORK                                                          
RS74D    CLI   0(R5),C'0'                                                       
         BNE   RS74E                                                            
         LA    R5,1(,R5)                                                        
         BCT   RF,RS74D                                                         
*                                                                               
RS74E    EX    RF,*+4                                                           
         MVC   0(0,R6),0(R5)                                                    
         LA    RE,1(,RE)                                                        
         LA    R6,1(RF,R6)                                                      
         MVI   0(R6),C','                                                       
         LA    R6,1(,R6)                                                        
         BCT   R1,RS74C1                                                        
*                                                                               
         BCTR  R6,0                                                             
         MVI   0(R6),C' '                                                       
         GOTO1 ACREPORT                                                         
*                                                                               
RS74L    LA    R4,FILTERLQ(,R4)                                                 
         B     RS74A                                                            
         DROP  R4,RE                                                            
*                                                                               
RS75     DS    0H                                                               
*        CLI   WRTSW,YES                                                        
*        BNE   RS80                                                             
*        AP    CPYFNDS,=P'1'                                                    
*        AP    TOTFNDS,=P'1'                                                    
*        AP    FTOTFNDS,=P'1'                                                   
*        BAS   RE,DMPPUT           RECORD     AFTER    FIX                      
*        CLI   RCWRITE,NO                                                       
*        BE    RS80                                                             
*        GOTO1 DATAMGR,DMCB,DMWRT,ACCOUNT,XKEY,AIO                              
*                                                                               
RS80     DS    0H                                                               
         BAS   RE,DMCTARD          REREAD    LAST      ACCOUNT   RCD            
         BAS   RE,DMCTASEQ         READ      NEXT      ACCOUNT   RCD            
*                                                                               
         AP    CPYRECS,=P'1'       UPDATE    COMPANY   RECORDS                  
         AP    TOTRECS,=P'1'       UPDATE    SYSTEM    TOTAL     RCDS           
         AP    FTOTRECS,=P'1'      UPDATE    FULL      TOTAL     RCDS           
         B     RS10                                                             
         EJECT ,                                                                
***********************************************************************         
*  END OF COMPANY                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING PSD,R4                                                           
         SPACE 1                                                                
RS90     DS    0H                                                               
         BAS   RE,PRTCPYTO         PRINT     COMPANY   TOTALS                   
*                                                                               
RS95     DS    0H                                                               
         LA    R4,P                                                             
         MVC   PSDSYSIH,=C'ACC SYS='                                            
         MVC   PSDSYSID,ACCSYSID        GET  ACCPAK SYSTEM ID                   
         MVC   PSDCPYCH(6),=C'TOTALS'                                           
         ZAP   WORK(8),TOTFNDS          GET  NUMBER OF MATCHES                  
         OI    WORK+7,X'0F'                                                     
         MVC   PSDRMATH,=C'RECORDS MATCHED='                                    
         UNPK  PSDRMAT,WORK(8)                                                  
         ZAP   WORK(8),TOTRECS          GET  NUMBER OF RECORDS                  
         OI    WORK+7,X'0F'                                                     
         MVC   PSDTRECH,=C'TOTAL RECORDS='                                      
         UNPK  PSDTREC,WORK(8)                                                  
         GOTO1 ACREPORT                                                         
         CP    TOTFNDS,=P'0'            SKIP A LINE IF THERE WAS DATA           
         BE    RS97                                                             
         GOTO1 ACREPORT                                                         
*                                                                               
RS97     DS    0H                                                               
         ZAP   CPYFNDS,=P'0'                                                    
         ZAP   CPYRECS,=P'0'                                                    
         ZAP   TOTRECS,=P'0'       SYSTEM     TOTAL     RECORDS                 
         ZAP   TOTFNDS,=P'0'       SYSTEM     TOTAL     FOUND                   
         MVC   REQDATED,SPACES                                                  
         MVC   REQUEST#,SPACES                                                  
*                                                                               
         B     XIT                                                              
         DROP  R3,R4               KEEP IT   CLEAN                              
         EJECT ,                                                                
***********************************************************************         
*  SET UP PRINT LINE FOR END OF FORMAT LOGIC                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R3                                                       
         USING PDLD,R4             DETAIL    OUTPUT    LINE                     
         SPACE 1                                                                
SETUPPLN NTR1                                                                   
         L     R3,AIO                                                           
         LA    R4,P                                                             
         GOTO1 HEXOUT,DMCB,RESKCPY,PDLCPYCD,1     COMPANY   CODE                
         MVC   PDLFORM,FORMNAME              FORMAT                             
         MVC   PDLRPTTC,REPTCODE             REPORT    TYPE CODE                
         MVC   PDLRPTYP,REPTNAME             REPORT    TYPE NAME                
         MVC   PDLLREQD,REQDATED             LAST REQUEST   DATE                
         MVC   PDLLREQX,REQUEST#             NUM  OF        REQUESTS            
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
DMCTHGH  DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,SKEY,AIOSL                            
         B     DMERR                                                            
*                                                                               
DMCTSEQ  DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRSEQ,CTFILE,SKEY,AIOSL                            
         B     DMERR                                                            
*                                                                               
DMCTARD  DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMREAD,ACCOUNT,XKEY,AIO                             
         B     DMERR                                                            
*                                                                               
DMCTASEQ DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCOUNT,XKEY,AIO                             
         B     DMERR                                                            
*                                                                               
DMCTAOVR DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCOUNT,XKEY,AIOOVR                          
         B     DMERR                                                            
*                                                                               
DMOPNACC DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMOPEN,ACCOUNT,ACFILEL                              
         B     DMERROK                                                          
*                                                                               
DMCTRDC  DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMREAD,ACCOUNT,CKEY,AIO                             
         B     DMERR                                                            
*                                                                               
DMCTRDSV DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMREAD,ACCOUNT,SVKEY,AIO                            
         B     DMERR                                                            
         SPACE 3                                                                
DMERR    DS    0H                                                               
         MVC   BYTE,8(R1)                                                       
         NI    BYTE,X'FF'-(X'10'+X'02') IGNORE RNF/RECORD DELETED               
         CLI   BYTE,0                                                           
         BE    DMERROK                                                          
         DC    H'0'                                                             
*                                                                               
DMERROK  DS    0H                                                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO GET TABLE DICTIONARY ENTRIES                            *         
*                                                                     *         
*    ON INPUT:                                                        *         
*       PARM LIST:                                                    *         
*          P1  BYTE  0   = LENGTH  OF THE DICTIONARY ENTRY            *         
*              BYTES 1-3 = ADDRESS OF THE DICTIONARY ENTRY            *         
*                                                                     *         
*          P2  BYTES 0-3 = ADDRESS OF THE TEXT       ENTRY            *         
***********************************************************************         
         SPACE 1                                                                
EXPDNTRY NTR1                                                                   
         L     R2,0(,R1)           GET  DICTIONARY     ENTRY                    
         LA    R2,0(,R2)           CLEAR     HIGH ORDER     BYTE                
         L     R3,4(,R1)           GET  TEXT           ENTRY                    
         LA    R3,0(,R3)           CLEAR     HIGH ORDER     BYTE                
         ZIC   R4,0(,R1)           GET  DICTIONARY     ENTRY     LENGTH         
         BCTR  R4,0                MINUS     ONE  FOR  EXECUTE                  
         EXCLC R4,0(R3),SPACES     WAS  TEXT DEFINED ?                          
         BNE   XIT                 YES, RETURN                                  
*                                                                               
         EXMVC R4,0(R3),0(R2)      MOVE DICTIONARY     VALUE TO  TEXT           
         CLI   0(R3),ESCHIGHQ      TEST FOR  ESCAPE    SEQUENCE                 
         BNL   XIT                                                              
*                                  TRANSLATE                                    
         GOTO1 ADDICTAT,DMCB,DICPARM,(R3),0                                     
         B     XIT                 RETURN                                       
         EJECT ,                                                                
***********************************************************************         
*  PRINT TOTALS FOR COMPANY                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING PSD,R4                                                           
         SPACE 1                                                                
PRTCPYTO NTR1                                                                   
         CLI   PRVKEY+2,X'00'           ANY  COMPANY ?                          
         BE    PRTCPYTX                 NO,  EXIT                               
*                                                                               
         LA    R4,P                     ->   OUTPUT TEXT                        
         MVC   PSDSYSIH,=C'ACC SYS='                                            
         MVC   PSDSYSID,ACCSYSID        GET  ACCPAK SYSTEM ID                   
         MVC   PSDCPYCH,=C'COMP='                                               
         MVC   CPYCDE,PRVKEY+2          GET  COMPANY CODE                       
         GOTO1 HEXOUT,DMCB,CPYCDE,PSDCPYCD,1                                    
         MVC   PSDCPYAH,=C'ALPHA='      GET  COMPANY ALPHA CODE                 
         MVC   PSDCPYAL,CALPHA                                                  
         MVC   PSDCPYLH,=C'LOGO='       GET  COMPANY LOGO                       
         MVC   PSDCPYLO,CLOGO                                                   
         ZAP   WORK(8),CPYFNDS          GET  NUMBER OF MATCHES                  
         OI    WORK+7,X'0F'                                                     
         MVC   PSDRMATH,=C'RECORDS MATCHED='                                    
         UNPK  PSDRMAT,WORK(8)                                                  
         ZAP   WORK(8),CPYRECS          GET  NUMBER OF RECORDS                  
         OI    WORK+7,X'0F'                                                     
         MVC   PSDTRECH,=C'TOTAL RECORDS='                                      
         UNPK  PSDTREC,WORK(8)                                                  
         GOTO1 ACREPORT                                                         
         CP    CPYFNDS,=P'0'            SKIP A LINE IF THERE WAS DATA           
         BE    PRTCPYTX                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
PRTCPYTX DS    0H                                                               
         ZAP   CPYRECS,=P'0'       COMPANY   TOTAL     RECORDS                  
         ZAP   CPYFNDS,=P'0'       COMPANY   TOTAL     FOUND                    
         B     XIT                 RETURN                                       
*                                                                               
         DROP  R4                  KEEP IT CLEAN                                
*                                                                               
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO DELETE AN ELEMENT                                       *         
*                                                                     *         
*        P1   BYTE  0    ELEMENT CODE 8                               *         
*             BYTES 1-3  A(RECORD)                                    *         
*        P2   BYTE  0    LENGTH OF SEARCH ARGUMENT                    *         
*             BYTES 1-3  A(SEARCH ARGUMENT)                           *         
***********************************************************************         
         SPACE 1                                                                
DELEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'D',=C'ACCBIG '),((R4),(R2)),((R5),(R3))            
         B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO ADD AN ELEMENT                                          *         
*                                                                     *         
*        P1   A(RECORD)                                               *         
*        P2   A(ELEMENT)                                              *         
***********************************************************************         
         SPACE 1                                                                
ADDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         GOTO1 HELLO,DMCB,(C'P',=C'ACCBIG '),(R2),(R3)                          
         CLI   DMCB+12,0                                                        
         BE    XIT                                                              
         TM    DMCB+12,X'05'                                                    
         BNZ   *+6                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         MVC   P(30),=CL30'RECORD TOO BIG TO ADD TO'                            
         GOTO1 ACREPORT                                                         
         MVI   WRTSW,NO                                                         
         B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*  DUMP ROUTINE                                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING ACKEYD,R3           GENERAL   RECORD    DSECT                    
         SPACE 1                                                                
DMPGET   TM    UPSI,UPSIGET        MUST REQUEST   DUMP                          
         BZR   RE                                                               
         CLI   DMPSW,YES           ALREADY   DUMPED    THIS RECORD ?            
         BNER  RE                                                               
         MVI   DMPSW,NO                                                         
         NTR1  ,                                                                
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,TEXTMSG                                                       
         ICM   R6,8,=AL1(L'TEXTMSG)                                             
         CLC   TEXTMSG,SPACES                                                   
         BH    DUMP                                                             
         LA    R6,=C'GET'                                                       
         ICM   R6,8,=AL1(3)                                                     
         B     DUMP                                                             
*                                                                               
DMPPUT   TM    UPSI,UPSIPUT        MUST REQUEST   DUMP                          
         BZR   RE                                                               
         CLI   WRTSW,YES           WILL WE   WRITE/PUT THIS RECORD ?            
         BNER  RE                  NO,  SO   DO   NOT  SHOW PUT                 
         MVI   DMPSW,NO                                                         
         NTR1  ,                                                                
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,TEXTMSG                                                       
         ICM   R6,8,=AL1(L'TEXTMSG)                                             
         CLC   TEXTMSG,SPACES                                                   
         BH    DUMP                                                             
         LA    R6,=C'PUT'                                                       
         ICM   R6,8,=AL1(3)                                                     
         SPACE 1                                                                
DUMP     SR    R8,R8                                                            
         ICM   R8,3,ACLENGTH       RECORD    LENGTH                             
         B     DUMPNOW                                                          
*                                                                               
DMPELMNT TM    UPSI,UPSIELMT       MUST REQUEST   DUMP                          
         BZR   RE                                                               
*        CLI   CHGELMNT,YES        DID  WE   CHANGE    THIS ELEMENT ?           
*        BNER  RE                  NO,  SO   DO   NOT  SHOW ELEMENT             
*        MVI   DMPSW,NO                                                         
         NTR1  ,                                                                
         AP    DUMPECNT,=P'1'                                                   
         ZAP   DUB,DUMPECNT                                                     
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         GOTO1 PRNTBL,DMCB,(L'FNAMEC,FNAMEC),FORMNAME,1,               X        
               L'FORMNAME,=C'1C'                                                
         GOTO1 PRNTBL,DMCB,(L'RTYPEC,RTYPEC),REPTCODE,1,               X        
               L'REPTCODE,=C'1C'                                                
         GOTO1 PRNTBL,DMCB,(L'RTYPEN,RTYPEN),REPTNAME,1,               X        
               L'REPTNAME,=C'1C'                                                
         LR    R3,R4               DUMP THE   ELEMENT                           
         SR    R8,R8                                                            
         IC    R8,1(,R3)           ELEMENT    LENGTH                            
         LA    R6,TEXTMSG                                                       
         ICM   R6,8,=AL1(L'TEXTMSG)                                             
         CLC   TEXTMSG,SPACES                                                   
         BH    DUMPNOW                                                          
         LA    R6,=C'ELEMENT'                                                   
         ICM   R6,8,=AL1(7)                                                     
*        B     DUMPNOW                                                          
*                                                                               
DUMPNOW  GOTO1 PRNTBL,DMCB,(R6),(R3),C'DUMP',(R8),=C'1D'                        
         XC    TEXTMSG,TEXTMSG                                                  
         B     XIT                                                              
*                                                                               
         DROP  R3                  KEEP IT   CLEAN                              
         EJECT ,                                                                
***********************************************************************         
*  EQUATES                                                            *         
***********************************************************************         
         SPACE 1                                                                
TURNOFF  EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
CHARALL  EQU   C'A'                CHARACTER  FORMAT KEYWORDS ** ALL **         
CHARONLY EQU   C'C'                CHARACTER  FORMAT KEYWORDS ONLY              
TRANONLY EQU   C'T'                TRANSLATED FORMAT KEYWORDS ONLY              
CHARRPTS EQU   C'C'                CHARACTER  FORMAT REPORT   TYPES             
CHARRPTM EQU   C'M'                CHARACTER  FORMAT REPORT   TYPES             
*                                             NOT IN TABLE    AND               
*                                             FORMAT NAMES OF NULLS             
TRANRPTS EQU   C'T'                TRANSLATED FORMAT REPORT   TYPES             
EOT      EQU   X'FF'               END OF TABLE                                 
ESCHIGHQ EQU   48                  X'30' ABOVE ESCAPE SEQUENCE                  
MXRLNQ   EQU   2100                MAX   LENGTH    OF   RECORD                  
         SPACE 3                                                                
*        SUBDIVISION OF BATCH TYPES                                             
         SPACE 1                                                                
TY30DI   EQU   229                 TYPE 30 DIFFERENCE (FOREIGN CURR)            
TY06MN   EQU   230                 TYPE 06 MANUAL BILLING                       
TY30CH   EQU   231                 TYPE 30 CHECK                                
TY30OF   EQU   232                 TYPE 30 OFFSET                               
TY30WO   EQU   233                 TYPE 30 WRITE OFF                            
TY30TT   EQU   234                 TYPE 30 TRANSFER TO                          
TY30TF   EQU   235                 TYPE 30 TRANSFER FROM                        
         EJECT ,                                                                
***********************************************************************         
*  CONSTANTS                                                          *         
***********************************************************************         
         SPACE 1                                                                
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
HEXIN    DC    V(HEXIN)                                                         
*                                                                               
AIO      DC    A(IO)               ADDR  I/O  AREA FOR  ACCOUNT     REC         
AIOSL    DC    A(IOSL)             ADDR  I/O  AREA FOR  SYSTEM LIST REC         
AIOOVR   DC    A(IOOVR)            ADDR  I/O  AREA FOR  2ND    ACCT REC         
*                                                                               
XIT      XIT1                                                                   
*                                                                               
EVERY    DC    PL4'1'                                                           
MAXDUMP  DC    PL4'500'                                                         
*                                                                               
ACCOUNT  DC    CL8'ACCOUNT '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
DMOPEN   DC    CL8'OPEN    '                                                    
ACFILEL  DC    C'NACCDIR NACCMST X'                                             
FNAMEC   DC    C'FORMAT NAME'                                                   
RTYPEC   DC    C'REPORT TYPE CODE'                                              
RTYPEN   DC    C'REPORT TYPE NAME'                                              
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  KEYWORD TABLES                                                     *         
***********************************************************************         
         SPACE 1                                                                
KEYWRDS  DS    0F                  KEYWORDS INCLUDE TABLE                       
         DCDD  AC#RSPYC,6          PAYC                                         
         DC    CL6'  '                                                          
         DCDD  AC#RSPYN,6          PAYN                                         
         DC    CL6'  '                                                          
         DCDD  AC#RSPYT,6          PAYTYP                                       
         DC    CL6' '                                                           
         DCDD  AC#RSPYD,6          PAYDTE                                       
         DC    CL6' '                                                           
         DCDD  AC#RSP$T,6          P$TOT                                        
         DC    CL6' '                                                           
         DCDD  AC#RSP$S,6          P$SAL                                        
         DC    CL6' '                                                           
         DCDD  AC#RSP$B,6          P$BEN                                        
         DC    CL6' '                                                           
         DCDD  AC#RSP$P,6          P$PEN                                        
         DC    CL6' '                                                           
         DCDD  AC#RSPLT,6          PRTOT                                        
         DC    CL6' '                                                           
         DCDD  AC#RSPLS,6          PRSAL                                        
         DC    CL6' '                                                           
         DCDD  AC#RSPLB,6          PRBEN                                        
         DC    CL6' '                                                           
         DCDD  AC#RSPLP,6          PRPEN                                        
         DC    CL6' '                                                           
*&&DO                                                                           
         DCDD  AC#RSWOH,6                                                       
         DC    CL6' '                                                           
         DCDD  AC#RSWOP,6                                                       
         DC    CL6' '                                                           
         DCDD  AC#RSWO$,6                                                       
         DC    CL6' '                                                           
         DCDD  AC#RSPTC,6                                                       
         DC    CL6' '                                                           
         DCDD  AC#RSMTH,6                                                       
         DC    CL6' '                                                           
         DCDD  AC#RSRY1,6                                                       
         DC    CL6' '                                                           
*&&                                                                             
*                                                                               
*&&DO                                                                           
         DC    CL6'='              =                                            
         DC    CL6' '                                                           
*&&                                                                             
*                                                                               
*&&DO                                                                           
         DCDD  AC#RSAF1,6          F1                                           
         DC    CL6'  '                                                          
*                                                                               
         DCDD  AC#RSAF2,6          F2                                           
         DC    CL6'  '                                                          
*                                                                               
         DCDD  AC#RSAF3,6          F3                                           
         DC    CL6'  '                                                          
*                                                                               
         DCDD  AC#RSAF4,6          F4                                           
         DC    CL6'  '                                                          
*                                                                               
         DCDD  AC#RSAF5,6          F5                                           
         DC    CL6'  '                                                          
*                                                                               
         DCDD  AC#RSOFF,6          OF = OFFICE                                  
         DC    CL6'  '                                                          
*                                                                               
         DCDD  AC#RSCNO,6          CKNO                                         
         DC    CL6'CKNO'                                                        
*&&                                                                             
*                                                                               
*&&DO                                                                           
         DCDD  AC#RSPO$,6                                                       
         DC    CL6'  '                                                          
*                                                                               
         DCDD  AC#RSBLC,6                                                       
         DC    CL6'  '                                                          
         DCDD  AC#RSGRA,6                                                       
         DC    CL6'  '                                                          
         DCDD  AC#RSGCD,6                                                       
         DC    CL6'  '                                                          
         DCDD  AC#RSNTA,6                                                       
         DC    CL6'  '                                                          
         DCDD  AC#RSNCD,6                                                       
         DC    CL6'  '                                                          
         DCDD  AC#RSCOM,6                                                       
         DC    CL6'  '                                                          
         DCDD  AC#RSCDA,6                                                       
         DC    CL6'  '                                                          
*&&                                                                             
*                                                                               
         DC    AL1(EOT)                                                         
*                                                                               
KEYWRDS2 DS    0F                  KEYWORDS EXCLUDE TABLE                       
*&&DO                                                                           
         DCDD  AC#RSWC,6                                                        
         DC    CL6'  '                                                          
         DCDD  AC#RSWCN,6                                                       
         DC    CL6'  '                                                          
         DCDD  AC#RSTSK,6                                                       
         DC    CL6'  '                                                          
         DCDD  AC#RSGPT,6                                                       
         DC    CL6'  '                                                          
*&&                                                                             
*                                                                               
         DC    AL1(EOT)                                                         
*                                                                               
         EJECT ,                                                                
***********************************************************************         
*  FILTER TYPE TABLE                                                  *         
***********************************************************************         
         SPACE 1                                                                
FILTERS  DS    0F                                                               
*&&DO                                                                           
         DC    AL1(RFLOFF)                                                      
         DC    CL4'OFF='                                                        
         DC    AL4(0)                                                           
         DC    AL1(RFLBUDGT)                                                    
         DC    CL4'BUD='                                                        
         DC    AL4(0)                                                           
*                                                                               
         DC    AL1(RFLTTYPE)                                                    
         DC    CL4'WC='                                                         
         DC    AL4(0)                                                           
*&&                                                                             
*&&DO                                                                           
         DC    AL1(RFLWC)                                                       
         DC    CL4'WC='                                                         
         DC    AL4(0)                                                           
         DC    AL1(RFLTWTP)                                                     
         DC    CL4'TM='                                                         
         DC    AL4(0)                                                           
         DC    AL1(RFLOPTP)                                                     
         DC    CL4'OP='                                                         
         DC    AL4(0)                                                           
*&&                                                                             
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*  REPORT TYPES TABLE (IF EMPTY, THEN ALL REPORT TYPES)               *         
***********************************************************************         
         SPACE 1                                                                
RPTTYPES DS    0F                                                               
         DCDD  AC#RS498,6          PERSON                                       
         DC    CL6'  '                                                          
*                                                                               
         DCDD  AC#RSRCV,6          RCV                                          
         DC    CL6'  '                                                          
         DCDD  AC#RSINC,6          INC                                          
         DC    CL6'  '                                                          
         DCDD  AC#RSPAY,6          PAY                                          
         DC    CL6'  '                                                          
         DCDD  AC#RSEXP,6          EXP                                          
         DC    CL6'  '                                                          
         DCDD  AC#RS497,6          PROD                                         
         DC    CL6'  '                                                          
         DCDD  AC#RS540,6          CASH                                         
         DC    CL6'  '                                                          
         DCDD  AC#RS544,6          P&L                                          
         DC    CL6'  '                                                          
         DCDD  AC#GLG,6            G/L                                          
         DC    CL6'  '                                                          
*&&DO                                                                           
         DC    CL6'FI'             FINANCIAL                                    
         DC    CL6'  '                                                          
         DC    CL6'M2'             MANPOWER                                     
         DC    CL6'  '                                                          
         DC    CL6'IV'             INVOICE                                      
         DC    CL6'  '                                                          
*&&                                                                             
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*  REPORT TYPE NAMES TABLE - CONTAINS LIST OF ALL REPORT TYPES        *         
***********************************************************************         
         SPACE 1                                                                
RPTNAMES DS    0F                                                               
         DCDD  AC#RSRCV,6          RCV    - US                                  
         DC    CL6'RCV'                                                         
         DCDD  AC#RSRCV,6          DEB    - UK                                  
         DC    CL6'DEB'                                                         
         DCDD  AC#RSRCV,6          CREAN. - FRENCH                              
         DC    CL6'CREAN.'                                                      
*                                                                               
         DCDD  AC#RSINC,6          INC    - US                                  
         DC    CL6'INC'                                                         
         DCDD  AC#RSINC,6          BENEF  - FRENCH                              
         DC    CL6'BENEF'                                                       
         DCDD  AC#RSINC,6          INK    - DUTCH                               
         DC    CL6'INK'                                                         
*                                                                               
         DCDD  AC#RSPAY,6          PAY    - US                                  
         DC    CL6'PAY'                                                         
         DCDD  AC#RSPAY,6          CRD    - UK                                  
         DC    CL6'CRD'                                                         
         DCDD  AC#RSPAY,6          APAYER - FRENCH                              
         DC    CL6'APAYER'                                                      
         DCDD  AC#RSPAY,6          KRED   - GERMAN                              
         DC    CL6'KRED'                                                        
         DCDD  AC#RSPAY,6          TEBET  - DUTCH                               
         DC    CL6'TEBET'                                                       
*                                                                               
         DCDD  AC#RSEXP,6          EXP    - US                                  
         DC    CL6'EXP'                                                         
         DCDD  AC#RSEXP,6          FRAIS  - FRENCH                              
         DC    CL6'FRAIS'                                                       
         DCDD  AC#RSEXP,6          AUF    - GERMAN                              
         DC    CL6'AUF'                                                         
         DCDD  AC#RSEXP,6          ONK    - DUTCH                               
         DC    CL6'ONK'                                                         
*                                                                               
         DCDD  AC#RS497,6          PROD   - US                                  
         DC    CL6'PROD'                                                        
*                                                                               
         DCDD  AC#RS498,6          PERSON - US                                  
         DC    CL6'PERSON'                                                      
         DCDD  AC#RS498,6          COST   - FRENCH                              
         DC    CL6'COST'                                                        
         DCDD  AC#RS498,6          PERSOO - DUTCH                               
         DC    CL6'PERSOO'                                                      
*                                                                               
         DCDD  AC#RS540,6          CASH   - US                                  
         DC    CL6'CASH'                                                        
         DCDD  AC#RS540,6          ESP}CE - FRENCH                              
         DC    CL6'ESP}CE'                                                      
         DCDD  AC#RS540,6          KOSTEN - GERMAN (US ONLY)                    
         DC    CL6'KOSTEN'                                                      
         DCDD  AC#RS540,6          GELD   - DUTCH                               
         DC    CL6'GELD'                                                        
*                                                                               
         DCDD  AC#RS544,6          P&L    - US                                  
         DC    CL6'P&&L'                                                        
         DCDD  AC#RS544,6          P+P    - FRENCH                              
         DC    CL6'P+P'                                                         
         DCDD  AC#RS544,6          G&V    - GERMAN                              
         DC    CL6'G&&V'                                                        
         DCDD  AC#RS544,6          W/V    - DUTCH                               
         DC    CL6'W/V'                                                         
*                                                                               
         DCDD  AC#GLG,6            G/L    - US AND UK                           
         DC    CL6'G/L'                                                         
         DCDD  AC#GLG,6            GL     - OLD IN UK                           
         DC    CL6'GL'                                                          
         DCDD  AC#GLG,6            GLG    - FRENCH                              
         DC    CL6'GLG'                                                         
         DCDD  AC#GLG,6            BKT    - GERMAN                              
         DC    CL6'BKT'                                                         
         DCDD  AC#GLG,6            AG     - DUTCH                               
         DC    CL6'AG'                                                          
*                                                                               
         DC    CL6'FI'             FINANCIAL - US ONLY                          
         DC    CL6'  '                                                          
         DC    CL6'M2'             MANPOWER                                     
         DC    CL6'  '                                                          
         DC    CL6'IV'             INVOICE                                      
         DC    CL6'  '                                                          
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*  TRANSACTION TYPE TABLE                                             *         
***********************************************************************         
         SPACE 1                                                                
TTYPETAB DS    0C                                                               
         DC    AL1(TY30DI)        DIFFERENCE TYPE 30                            
         DC    CL4'DI'                                                          
         DC    AL1(TY30CH)        CHECK TYPE 30                                 
         DCDD  AC#RSTCH,4                                                       
         DC    AL1(TY30OF)        OFFSET TYPE 30                                
         DCDD  AC#RSTOF,4                                                       
         DC    AL1(TY30WO)        WRITE-OFF TYPE 30                             
         DCDD  AC#RSTWO,4                                                       
         DC    AL1(TY30TT)        TRANSFERED FROM TYPE 30                       
         DCDD  AC#RSTTT,4                                                       
         DC    AL1(TY30TF)        TRANSFERED TO TYPE 30                         
         DCDD  AC#RSTTF,4                                                       
         DC    AL1(TY06MN)        MANUAL BILLING TYPE 06                        
         DC    CL4'M'                                                           
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*  BUFFERS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         DC    F'0'                IOAREA     #1 - ACCOUNT   RECORD             
IO       DC    (MXRLNQ*2)X'00'                                                  
*                                                                               
         DC    F'0'                IOAREA     #2 - SYSTEM    LIST REC           
IOSL     DC    (MXRLNQ)X'00'                                                    
*                                                                               
         DC    F'0'                IOAREA     #3 - SYSTEM    LIST REC           
IOOVR    DC    (MXRLNQ)X'00'                                                    
*                                                                               
         EJECT ,                                                                
***********************************************************************         
*  VARIABLES                                                          *         
***********************************************************************         
         SPACE 1                                                                
ACXVD    DSECT                                                                  
AUTL     DS    A                   UTL                                          
*                                                                               
DICPARM  DS    3A                  PARAMETER  LIST FOR  DICTATE                 
*                                                                               
RQSTART  DS    PL3                                                              
RQEND    DS    PL3                                                              
FILTSEL  DS    XL1                                                              
*                                                                               
LASTACTD DS    CL10                LAST  ACTIVITY  DATE (MMMDD/YYYY)            
REQDATE  DS    PL3                 LAST  REQUEST   DATE (PACKED)                
REQDATED DS    CL10                LAST  REQUEST   DATE (MMMDD/YYYY)            
REQUEST# DS    CL3                 NUM   OF   OVERNIGHT REQUESTS                
*                                                                               
NEWTYPE  DS    CL1                                                              
NEWCODE  DS    CL6                                                              
SAVELDG  DS    CL2                                                              
CPYCDE   DS    XL1                                                              
*                                                                               
CPYRECS  DS    PL8                                                              
CPYFNDS  DS    PL8                                                              
TOTRECS  DS    PL8                                                              
TOTFNDS  DS    PL8                                                              
FTOTRECS DS    PL8                 FULL  TOTAL     RECORDS                      
FTOTFNDS DS    PL8                 FULL  TOTAL     FOUND                        
*                                                                               
CLOGO    DS    CL(L'CPYLOGO)       COMPANY    LOGO                              
CALPHA   DS    CL(L'CPYALPHA)      COMPANY    ALPHA     CODE                    
*                                                                               
MNFLKEY  DS    CL(L'ACCKEY)                                                     
CKEY     DS    CL(L'ACCKEY)        COMPANY    KEY                               
SVKEY    DS    CL(L'ACCKEY)        SAVE RECORD     KEY                          
XKEY     DS    CL(L'ACCKEY)                                                     
PRVKEY   DS    CL(L'ACCKEY)                                                     
SKEY     DS    CL(L'ACCKEY)        DIRECTORY  KEY  FOR  SYS  LIST RCD           
*                                                                               
TEXTMSG  DS    CL12                                                             
*                                                                               
LIMRTYPS DS    CL1                 LIMIT REPORT    TYPES                        
FORMNAME DS    CL(L'RESKFORM)      FORMAT     NAME                              
SVFORMNM DS    CL(L'RESKFORM)      FORMAT     NAME SAVE AREA                    
REPTCODE DS    CL(L'STYCODE)       REPORT     TYPE CODE                         
REPTNAME DS    CL(L'STYNAME)       REPORT     TYPE NAME                         
FNDRPTYP DS    CL(L'STYNAME)       REPORT     TYPE NAME THAT WAS  FOUND         
NPARMS   DS    XL1                 #     OF   PARAMETERS                        
*                                                                               
FOUNDSW  DS    XL1                 FOUND SWITCHES TO   REPORT                   
FNDKYW   EQU   X'80'               .     KEYWORD                                
FNDFILT  EQU   X'40'               .     FILTER                                 
FNDRPTTN EQU   X'20'               .     REPORT    TYPE NAME                    
FNDMULEL EQU   X'10'               .     MISSING   UNIT/LEDGER    EL            
FNDMFFEL EQU   X'08'               .     MISSING   FREE FORM (STYELD)EL         
FNDC0EL  EQU   X'04'               .     OLD  SCRIBE    ELEMENT                 
*                                                                               
FOUNDSW2 DS    XL1                 FOUND SWITCHES FOR  EXCLUDE                  
FNDSKIPR EQU   X'80'               .     SKIP THIS RECORD                       
FNDEXC   EQU   X'40'               .     EXCLUDE   KYW  AND  FILTER             
FNDULEL  EQU   X'20'               .     UNIT/LEDGER    ELEMENT                 
FNDFFEL  EQU   X'10'               .     FREE FORM (STYELD)  ELEMENT            
*                                                                               
FOUND3   DS    XL1                                                              
FNDWC    EQU   X'80'                                                            
FNDWCT   EQU   X'40'                                                            
*                                                                               
TRANSLAT DS    CL1                 TRANSLATED BIT  ON  (Y/N)                    
FNDKEYWD DS    CL6                 LAST  KEYWORD   THAT WAS  FOUND              
*                                                                               
READSEQ  DS    X                   READ  DIRECTORY      SEQUENTIALLY            
READSSEQ EQU   X'80'               READ  SYSTEM    LIST SEQUENTIALLY            
*                                                                               
SAVESE   DS    X                   SE    NUM  FOR  CTL  FILE                    
SYSSE    DS    X                   SE    NUM  FOR  ACCPAK                       
ACCSYSID DS    CL4                 ACCPAK     SYSTEM    ID                      
UPSI     DS    X                   JOB   UPSI BYTE                              
UPSIGET  EQU   X'80'               .     DUMP GET  REQUESTS                     
UPSIPUT  EQU   X'40'               .     DUMP PUT  REQUESTS                     
UPSIELMT EQU   X'20'               .     DUMP UPDATED   ELEMENTS                
*                                                                               
WRTSW    DS    CL1                                                              
DMPSW    DS    CL1                                                              
*                                                                               
ELM      DS    CL255                                                            
BLOCK    DS    6CL32               DATA  BLOCK     FOR  SCANNER                 
*                                                                               
DUMPCNT  DS    PL4                 RECORDS      DUMPED                          
DUMPECNT DS    PL4                 ELEMENTS     DUMPED                          
PDUMP    DS    PL4                 TOTAL        DUMPED                          
         EJECT ,                                                                
***********************************************************************         
*  KEYWORDS DSECT                                                     *         
***********************************************************************         
         SPACE 1                                                                
KEYWRDSD DSECT                                                                  
*                                  KEYWORD   ENTRY                              
KEYWDIC  DS    XL6                 KEYWORD - DICTIONARY ENTRY                   
KEYWTEXT DS    CL6                 KEYWORD - TEXT                               
KEYWRDLQ EQU   *-KEYWRDSD          LENGTH OF A KEYWORD ENTRY                    
         EJECT ,                                                                
***********************************************************************         
*  FILTERS DSECT                                                      *         
***********************************************************************         
         SPACE 1                                                                
FILTD    DSECT                                                                  
*                                  FILTER   ENTRY                               
FILTTYPE DS    XL1                 FILTER - TYPE                                
FILTDESC DS    CL4                 FILTER - DESCRIPTION                         
FILTADR  DS    AL4                 A(FILTER ELEMENT)                            
FILTERLQ EQU   *-FILTD             LENGTH OF A FILTER ENTRY                     
         EJECT ,                                                                
***********************************************************************         
*  REPORT TYPES TO BE CONSIDERED                                      *         
*                                                                     *         
*  IF THIS TABLE IS EMPTY, THEN ALL REPORT TYPES ARE VALID            *         
***********************************************************************         
         SPACE 1                                                                
RPTTYPED DSECT                                                                  
*                                  REPORT TYPE ENTRY                            
RPTTDIC  DS    XL6                 REPORT TYPE - DICTIONARY ENTRY               
RPTTTEXT DS    CL6                 REPORT TYPE - TEXT                           
RPTTYPLQ EQU   *-RPTTYPED          LENGTH OF A KEYWORD ENTRY                    
         EJECT ,                                                                
***********************************************************************         
*  SUMMARY OUTPUT LINE                                                *         
***********************************************************************         
         SPACE 1                                                                
PSD      DSECT                                                                  
*                                  SUMMARY LINE                                 
PSDSYSIH DS    CL8                 ACC SYS=                                     
PSDSYSID DS    CL4                 ACC     SYS  ID                              
         DS    CL1                                                              
PSDCPYCH DS    CL5                 COMP=                                        
PSDCPYCD DS    CL2                 COMPANY CODE                                 
         DS    CL3                                                              
PSDCPYAH DS    CL6                 ALPHA=                                       
PSDCPYAL DS    CL2                 ALPHA   CODE                                 
         DS    CL1                                                              
PSDCPYLH DS    CL5                 LOGO=                                        
PSDCPYLO DS    CL7                 COMPANY LOGO                                 
         DS    CL3                                                              
PSDRMATH DS    CL16                RECORDS MATCHED=                             
PSDRMAT  DS    CL8                 NUMBER                                       
         DS    CL3                                                              
PSDTRECH DS    CL14                TOTAL RECORDS=                               
PSDTREC  DS    CL8                 NUMBER                                       
PSDLNQ   EQU   *-PSD               LENGTH  OF   A   SUMMARY OUTPUT LINE         
         EJECT ,                                                                
***********************************************************************         
*  DETAIL OUTPUT LINE                                                 *         
***********************************************************************         
         SPACE 1                                                                
PDLD     DSECT                                                                  
*                                  DETAIL  LINE                                 
         DS    CL2                                                              
PDLCPYCD DS    CL2                 COMPANY CODE                                 
         DS    CL1                                                              
PDLFORM  DS    CL8                 FORMAT  NAME                                 
         DS    CL1                                                              
PDLRPTTC DS    CL1                 REPORT  TYPE CODE                            
         DS    CL1                                                              
PDLRPTYP DS    CL6                 REPORT  TYPE                                 
         DS    CL1                                                              
PDLFNDWH DS    CL15                FOUND   WHAT                                 
         DS    CL1                                                              
PDLFNDKW DS    CL6                 FOUND   KEYWORD                              
         DS    CL1                                                              
PDLFNDFL DS    CL19                FOUND   FILTER                               
         DS    CL1                                                              
PDLLREQD DS    CL10                LAST    REQUEST DATE                         
         DS    CL1                                                              
PDLLREQX DS    CL3                 LAST    REQUEST TIMES                        
PDLLNQ   EQU   *-PDLD              LENGTH OF A DETAIL OUTPUT LINE               
         EJECT ,                                                                
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACSCRDSECT                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACSCRDSECT                                                     
         PRINT ON                                                               
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*ACMASTD                                                                        
*DDREPMASTD                                                                     
*DDMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*DMDTFIS                                                                        
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
*ACDDEQUS                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'100ACREPXV08A05/01/02'                                      
         END                                                                    
