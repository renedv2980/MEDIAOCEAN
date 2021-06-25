*          DATA SET ACREPXV08  AT LEVEL 151 AS OF 07/09/01                      
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
***********************************************************************         
         SPACE 1                                                                
ACXV02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXV**,R9                                                    
*                                                                               
         USING ACWORKD,RA                                                       
         USING ACXVD,RC                                                         
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
DT010    CLC   QEND,SPACES                                                      
         BNH   DT020                                                            
         GOTO1 DATCON,DMCB,(0,QEND),(1,RQEND)                                   
*                                                                               
DT020    MVC   DICPARM,=C'SU  '    PARM FOR  DICTATE                            
*&&US*&& MVI   DICPARM+2,CTWKACC   ACCPAK    SYSTEM                             
*&&UK*&& MVI   DICPARM+2,X'06'     ACCPAK    SYSTEM                             
         MVC   DICPARM+3,RCLANG    LANGUAGE                                     
*                                                                               
         USING KYWD,R5                                                          
         L     R5,=A(KEYWRDS)                                                   
         LA    R0,KEYWRD#          NUMBER OF ENTRIES                            
         LTR   R0,R0                                                            
         BZ    DT030                                                            
DT025    OC    KYWDD#,KYWDD#                                                    
         BZ    DT028               DON'T TRANSLATE                              
         MVC   KYWTEXT,SPACES                                                   
         MVI   KYWTEXT,ESCJLFT                                                  
         MVC   KYWTEXT+1(2),KYWDD#                                              
         MVI   KYWTEXT+3,6                                                      
         GOTO1 ADDICTAT,DMCB,DICPARM,KYWTEXT,0                                  
*                                                                               
DT028    LA    R5,KYWLNQ(,R5)                                                   
         BCT   R0,DT025                                                         
         DROP  R5                                                               
         EJECT ,                                                                
***********************************************************************         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING CTWREC,R6                                                        
DT030    XC    CTKEY,CTKEY         CLEAR SYS  LIST REC  KEY                     
         LA    R6,CTKEY            ->    SYSTEM    LIST KEY                     
         MVI   CTWKTYP,CTWKTYPQ    REC   TYPE      C'W' SYSTEM LISTS            
         MVI   CTWKREC,CTWKRSYS    SUB   REC  TYPE C'S' SYSTEM LIST             
*&&US*&& MVI   CTWKSYSN,CTWKACC    SYSTEM NUMBER =  ACCPAK                      
*&&UK*&& MVI   CTWKSYSN,X'06'      SYSTEM NUMBER =  ACCPAK                      
         MVC   TKEY,CTKEY                                                       
         BAS   RE,DMCTREAD         READ                                         
*                                                                               
         USING CTLSTD,R5           FILE INFO ELEMENT                            
         L     R6,AIO3             IO AREA                                      
         LA    R5,CTWDATA          ->   ELEMENTS                                
REQF20   CLI   0(R5),0             EOR ?                                        
         BE    REQF30              YES, GET NEXT RECORD                         
         CLI   CTLSTEL,CTLSTELQ    X'A4' ELEMENT ?                              
         BNE   REQF25              NO, GET NEXT ELEMENT                         
*&&US*&& CLI   CTLSTDTA+7,CTWKACC  ACCPAK LIST DATA ?                           
*&&UK*&& CLI   CTLSTDTA+7,X'06'    ACCPAK LIST DATA ?                           
         BNE   REQF25              NO, GET NEXT ELEMENT                         
*                                  YES, FOUND ACCPAK LIST DATA                  
         CLI   QSELECT+2,C' '      LIMIT THE  ACCPACK SYSTEMS ?                 
         BE    REQF22              NO,  CONTINUE                                
         CLC   QSELECT+2(1),CTLSTDTA+3                                          
         BNE   REQF25              NO, GET NEXT ELEMENT                         
*                                                                               
REQF22   MVC   SYSSE,CTLSTDTA+8    SAVE SE NUMBER                               
         MVC   ACCSYSID,CTLSTDTA   SAVE ACCPAK SYSTEM ID                        
*                                                                               
         L     R2,AUTL             ->    UTL                                    
         MVC   4(1,R2),SYSSE       SET   UTL  FOR  THIS ACCPAK    FILE          
         L     R2,AIO3                                                          
         BAS   RE,DMOPNACC         OPEN  ACC  FILE                              
         BAS   RE,RS               GET   FIRST     ACCOUNT   RECORD             
         L     R2,AUTL             ->    UTL                                    
         MVC   4(1,R2),SAVESE      RESTORE    SE   ID   FOR  CTL  FILES         
*                                                                               
REQF25   ZIC   R2,1(,R5)           CURR  ELEMENT   LENGTH                       
         AR    R5,R2               NEXT  ELEMENT   ADDR                         
         B     REQF20              TRY   NEXT ELEMENT                           
*                                                                               
REQF30   MVC   TKEY,CTKEY          RESTORE SAVED KEY                            
         DROP  R5,R6                                                            
*                                                                               
         USING PSD,R4                                                           
REQFEX   GOTO1 ACREPORT            SKIP  A    LINE                              
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
         B     XIT                                                              
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*        PROCESS THIS ACCFILE OF SCRIBE RECORDS                       *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R3                                                       
RS       NTR1                                                                   
         XC    RSKEY,RSKEY                                                      
         XC    PRVKEY,PRVKEY                                                    
         XC    PRVCPY,PRVCPY                                                    
         LA    R3,RSKEY                                                         
         MVI   RESKTYP,RESKTYPQ    X'2D' SCRIBE RECORDS                         
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         CLI   FILTSEL,0                                                        
         BZ    *+10                                                             
         MVC   RESKCPY,FILTSEL                                                  
*                                                                               
         L     R3,AIO1             READ FOR FIRST FORMAT ON FILE                
         MVC   TKEY,RSKEY                                                       
         BAS   RE,DMACHIGH                                                      
*                                                                               
RS10     L     R3,AIO1                                                          
         MVI   REREAD,NO                                                        
         CLC   0(2,R3),=X'2D02'    CHECK RECORD TYPE                            
         BNE   RS90                FINISHED                                     
         MVC   SVFORMNM,SPACES     CLEAR SAVED FORMAT NAME                      
         CLC   RESKCPY,PRVCPY      SAME COMP CODE?                              
         BE    RS10A               YES, PROCESS NEW FORMAT                      
         MVC   PRVCPY,RESKCPY                                                   
         BAS   RE,PRTCPYTO         PRINT COMPANY TOTALS                         
         MVC   CLOGO,SPACES        CLEAR COMPANY LOGO                           
         MVC   CALPHA,SPACES       CLEAR COMPANY ALPHA CODE                     
         CLI   FILTSEL,0           COMPANY FILTERING                            
         BE    RS10A               NO,  SKIP                                    
         CLC   FILTSEL,RESKCPY     YES, SHOW ONLY ONE COMPANY                   
         BNE   NEXTFMT                                                          
*                                                                               
RS10A    CLI   RESKSEQ,RESKSREG    NORMAL FORMAT                                
         BNE   NEXTFMT             READ SEQUENTIAL, SKIP THIS REC               
         MVC   RSKEY,0(R3)                                                      
*                                                                               
         USING STYELD,R4                                                        
         LR    R4,R3               ->   1ST  ELEMENT                            
         AH    R4,DATADISP                                                      
RS10B    CLI   0(R4),0                                                          
         BE    RS11                                                             
         CLI   0(R4),STYELQ        X'25' ELEMENT                                
         BNE   RS10D                                                            
         TM    STYSTAT,STYS2ND     2ND RECORD ON FILE ?                         
         BZ    RS11                NO SO OK AS IS                               
*                                                                               
         L     R3,AIO2                                                          
         BAS   RE,DMACSEQ          READ SEQUENTIAL                              
*                                                                               
         CLC   RESKEY(RESKFRML),RSKEY                                           
         BNE   RS11                NOT THERE, MIGHT BE A PROBLEM                
         CLI   RESKSEQ,RESKS2ND                                                 
         BNE   RS11                NOT SURE                                     
         LR    R4,R3               ->   1ST  ELEMENT                            
         AH    R4,DATADISP                                                      
         MVC   RSKEY,0(R3)         NEW RE-SET KEY                               
*                                                                               
         L     R3,AIO1             FIRST RECORD                                 
         AH    R3,RESRLEN          POINT TO END OF RECORD                       
RS10C    CLI   0(R4),0             EOR ?                                        
         BE    RS11                APPEND AIO2 TO AIO1                          
         ZIC   RF,1(,R4)           MOVE ONE EXTRA CHARACTER FOR EOR             
         EX    RF,*+4                                                           
         MVC   0(0,R3),0(R4)                                                    
         AR    R3,RF                                                            
         AR    R4,RF                                                            
         B     RS10C                                                            
*                                                                               
RS10D    SR    RF,RF                                                            
         IC    RF,1(,R4)                                                        
         AR    R4,RF                                                            
         B     RS10B                                                            
*                                                                               
         USING KYWD,R5                                                          
RS11     LA    R0,KEYWRD#                                                       
         LTR   R0,R0                                                            
         BZ    RS12                                                             
         L     R5,=A(KEYWRDS)      RESET SOME INDICATORS IN TABLE               
         NI    KYWIND,X'FF'-KYWXCLD                                             
         LA    R5,KYWLNQ(,R5)                                                   
         BCT   R0,*-8                                                           
*                                                                               
         USING CPYRECD,R3                                                       
RS12     LA    R3,CKEY             GET COMPANY DATA                             
         MVC   CKEY,SPACES         CLEAR COMPANY KEY                            
         MVC   CPYKCPY,RSKEY+2     MOVE IN COMPANY CODE                         
         L     R3,AIO2             GET COMPANY DATA                             
         MVI   REREAD,YES                                                       
         MVC   TKEY,CKEY                                                        
         BAS   RE,DMACREAD         READ COMPANY   RECORD                        
         LR    R4,R3               ->   1ST  ELEMENT                            
         AH    R4,DATADISP         ->   1ST  ELEMENT                            
*                                                                               
         USING CPYELD,R4                                                        
RS14     CLI   0(R4),0             END  OF   RECORD ?                           
         BE    RS20                YES, RESTORE   RECORD                        
         CLI   0(R4),CPYELQ        COMPANY   ELEMENT ?                          
         BNE   RS16                YES, PROCESS                                 
         MVC   CLOGO,CPYLOGO       GET  COMPANY   LOGO                          
         MVC   CALPHA,CPYALPHA     GET  COMPANY   ALPHA     CODE                
         B     RS20                                                             
*                                                                               
RS16     ZIC   R1,1(,R4)           GET  ELEMENT   LENGTH                        
         AR    R4,R1               BUMP TO   NEXT ELEMENT                       
         B     RS14                TRY  NEXT ELEMENT                            
         DROP  R4                                                               
*                                                                               
         USING RESRECD,R3                                                       
RS20     L     R3,AIO1                                                          
         MVC   PRVKEY,0(R3)                                                     
         MVI   WRTSW,NO            SET    WRITE     SWITCH                      
         MVI   FOUNDSW,0           CLEAR  FOUND     SWITCH                      
         MVI   FOUNDSW2,0          CLEAR  EXCLUDE   SWITCH                      
         MVI   DMPSW,YES           SET    DUMP SWITCH                           
         MVC   REPTCODE,SPACES                                                  
         MVC   REPTNAME,SPACES                                                  
         MVC   FNDKEYWD,SPACES                                                  
         MVC   FNDFILTR,SPACES                                                  
         MVC   FNDRPTYP,SPACES                                                  
         MVC   REQDATED,SPACES                                                  
         MVC   REQUEST#,SPACES                                                  
         LR    R4,R3                                                            
         AH    R4,DATADISP                                                      
*                                                                               
RS25     MVC   FORMNAME,RESKFORM   SAVE   FORMAT    NAME                        
         OC    FORMNAME,FORMNAME   IS     FORMAT    NULL ?                      
         BNZ   RS30                NO,    SKIP                                  
         OI    FOUNDSW,FNDRPTTN    YES,   INDICATE  FOUND                       
         MVC   FNDRPTYP,=CL6'*NULL*'                                            
         MVC   FORMNAME,=CL8'*-NULL-*'                                          
*                                                                               
RS30     CLI   0(R4),0                                                          
         BE    RS70                                                             
         MVI   DICTFORM,NO         ASSUME NOT  TRANSLATED                       
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
NEXTELM  SR    R1,R1                                                            
         IC    R1,1(,R4)                                                        
         AR    R4,R1                                                            
         B     RS30                                                             
         EJECT ,                                                                
***********************************************************************         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING PACELD,R4           PERSON ACTIVITY    ELEMENT  X'A1'            
A110     LA    RF,PACDATE          LAST   ACTIVITY    DATE                      
         GOTO1 DATCON,DMCB,(1,(RF)),(21,LASTACTD)                               
         B     NEXTELM                                                          
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING PTRELD,R4           POINTER     ELEMENT     X'FA'                
FA10     ZIC   R1,PTRLN            ELEMENT     LENGTH                           
         SHI   R1,PTRLN1Q                                                       
         SRL   R1,2                DIVIDE BY   4                                
         CVD   R1,DUB              NUMBER OF   REQUESTS                         
         OI    DUB+7,X'0F'                                                      
         UNPK  REQUEST#,DUB        SAVE   NUMBER                                
         B     NEXTELM                                                          
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING DTSELD,R4           DATE/TIME   STAMP     ELEMENT  X'FB'         
FB10     GOTO1 DATCON,DMCB,(2,DTSDATE),(1,REQDATE)                              
         CLC   REQDATE,RQSTART                                                  
         BL    NEXTFMT                                                          
         CLC   REQDATE,RQEND                                                    
         BH    NEXTFMT                                                          
         GOTO1 DATCON,DMCB,(2,DTSDATE),(21,REQDATED)                            
         B     NEXTELM             CONTINUE                                     
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING STYELD,R4           FREE   FORM   SCRIBE      EL   X'25'         
RP10     MVC   REPTCODE,STYCODE    SAVE   REPORT TYPE   CODE                    
         MVC   REPTNAME,STYNAME    SAVE   REPORT TYPE   NAME                    
*                                                                               
         USING REPTYD,RE           REPORT TYPE   ENTRY                          
         L     RE,=A(RPTTYPES)     ->     REPORT TYPES TABLE                    
RP20     CLI   0(RE),EOT           END    OF     TABLE ?                        
         BE    NEXTFMT             YES,   SKIP   RECORD                         
         CLC   REPTDD#,STYNAME+1   MATCH  DICTIONARY   ENTRY ?                  
         BE    NEXTELM             YES,   GOOD   ENTRY                          
         CLC   REPTTEXT,STYNAME    MATCH  TEXT   ENTRY ?                        
         BE    NEXTELM             YES,   GOOD   ENTRY                          
         LA    RE,REPTYLNQ(,RE)    TRY    NEXT   ENTRY                          
         B     RP20                                                             
         DROP  R4,RE                                                            
         EJECT ,                                                                
***********************************************************************         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING RRWELD,R4                                                        
RW10     TM    RRWOPT2,RRWDICT     TRANSLATED ROW ?                             
         BZ    *+8                 NO,    SKIP                                  
         MVI   DICTFORM,YES        SAY    TRANSLATED                            
         SR    RF,RF                                                            
*                                                                               
RW20     LA    R6,RRWDATA                                                       
         LA    RF,L'RRWDATA                                                     
         TM    RRWOPT,RRWNEWEL     NEW    ROW  ELEMENT ?                        
         BZ    RW24                YES,   SKIP                                  
         LA    R6,RRWNDATA         ->     ROW  ELEMENT                          
         ICM   RF,1,RRWDATLN       ROW    DATA LENGTH                           
         BNZ   *+10                NONE,  RETURN                                
         BAS   RE,KILL                                                          
         DC    H'00'                                                            
*                                                                               
RW24     CLI   DICTFORM,YES        IS IT CONVERTED TO CHARACTER FORM?           
         BE    RW30                YES SO JUST KEEP GOING                       
         LR    RE,R6                                                            
         LR    R1,RF                                                            
         SR    RF,RF                                                            
*                                                                               
RW26     CLI   0(RE),C','          AT END OF KEYWORD ?                          
         BE    RW28                                                             
         CLI   0(RE),X'01'         AT END OF KEYWORD ?                          
         BE    RW28                                                             
         CLI   0(RE),C' '          AT END ?                                     
         BNH   RW28                                                             
         LA    RE,1(,RE)           NEXT CHARACTER                               
         LA    RF,1(,RF)           COUNT THE LENGTH                             
         BCT   R1,RW26                                                          
         BCTR  RF,0                                                             
*                                                                               
RW28     LTR   RF,RF                                                            
         BNZ   *+10                                                             
         BAS   RE,KILL                                                          
         DC    H'00'                                                            
         TM    RRWOPT,RRWNEWEL     NEW    ROW  ELEMENT ?                        
         BO    RW30                                                             
         BAS   RE,KILL                                                          
*        DC    H'00'                                                            
*                                                                               
         USING KYWD,R5                                                          
RW30     GOTO1 FINDKYWD,DMCB,((RF),(R6))                                        
         BNE   NEXTELM             KEYWORD NOT IN LIST                          
         TM    KYWIND,KYWXCLD                                                   
         BO    NEXTFMT             EXCLUDE THIS FORMAT                          
         B     NEXTELM                                                          
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R4                                                        
CL10     TM    RCLOPT,RCLEQU                                                    
         BO    NEXTELM             DON'T PROCESS COLUMN CALS                    
         CLI   RCLSPCL,0                                                        
         BNE   NEXTELM                                                          
         SR    RF,RF                                                            
         TM    RCLOPT2,RCLDICT     TRANSLATED COL ?                             
         BZ    *+8                 NO,    SKIP                                  
         MVI   DICTFORM,YES        SAY    TRANSLATED                            
*                                                                               
CL20     LA    R6,RCLDATA                                                       
         LA    RF,L'RCLDATA                                                     
         TM    RCLOPT,RCLNEWEL     NEW    COL  ELEMENT ?                        
         BZ    CL24                YES,   SKIP                                  
         LA    R6,RCLNDATA         ->     COL  ELEMENT                          
         ICM   RF,1,RCLDATLN       COL    DATA LENGTH                           
         BNZ   CL24                NONE,  RETURN                                
         BAS   RE,KILL                                                          
         DC    H'00'                                                            
*                                                                               
CL24     CLI   DICTFORM,YES        IS IT CONVERTED TO CHARACTER FORM?           
         BE    CL30                NO SO JUST KEEP GOING                        
         LR    RE,R6                                                            
         LR    R1,RF                                                            
         SR    RF,RF                                                            
*                                                                               
CL26     CLI   0(RE),C','          AT END OF KEYWORD ?                          
         BE    CL28                                                             
         CLI   0(RE),X'01'         AT END OF KEYWORD ?                          
         BE    CL28                                                             
         CLI   0(RE),C' '          AT END ?                                     
         BNH   CL28                                                             
         LA    RE,1(,RE)           NEXT CHARACTER                               
         LA    RF,1(,RF)           COUNT THE LENGTH                             
         BCT   R1,CL26                                                          
         BCTR  RF,0                                                             
*                                                                               
CL28     LTR   RF,RF                                                            
         BNZ   *+10                                                             
         BAS   RE,KILL                                                          
         DC    H'00'                                                            
         TM    RCLOPT,RCLNEWEL     NEW    COL  ELEMENT ?                        
         BO    CL30                                                             
         BAS   RE,KILL                                                          
*        DC    H'00'                                                            
*                                                                               
         USING KYWD,R5                                                          
CL30     GOTO1 FINDKYWD,DMCB,((RF),(R6))                                        
         BNE   NEXTELM             KEYWORD NOT IN LIST                          
         TM    KYWIND,KYWXCLD                                                   
         BO    NEXTFMT             EXCLUDE THIS FORMAT                          
         B     NEXTELM                                                          
         DROP  R5                                                               
         EJECT ,                                                                
***********************************************************************         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R4           PROFILE       DATA ELEMENT   X'C4'           
PF10     DS    0H                  PROFILE   ELEMENT                            
*        TM    RPFDNOPT,RPFDNPAD                                                
*        BZ    NEXTELM                                                          
*        OI    FOUNDSW2,FNDPAD                                                  
*        MVC   P+2(8),FORMNAME                                                  
*        MVC   P+10(6),FNDRPTYP                                                 
*        MVC   P+20(30),=CL30'PAD WITH ZEROS'                                   
*        GOTO1 ACREPORT                                                         
         B     NEXTELM                                                          
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R4                                                        
FL10     DS    0H                  FILTER    ELEMENT   X'C5'                    
         CLI   RFLTYPE,RFLLDG      FND  UNIT/LEDGER ?                           
         BNE   *+8                 NO,  SKIP                                    
         OI    FOUNDSW2,FNDULEL    SAY  FOUND     UNIT/LEDGER                   
*                                                                               
         USING FLTD,RF                                                          
         L     RF,=A(FILTERS)                                                   
FL20     DS    0H                                                               
         CLI   0(RF),EOT           END  OF   TABLE ?                            
         BE    FL90                YES, EXIT                                    
         CLC   RFLTYPE,FLTTYPE     FILTER    MATCH ?                            
         BE    FL30                YES, GOOD                                    
         LA    RF,FLTLNQ(,RF)      NEXT FILTER    ENTRY                         
         B     FL20                                                             
*                                                                               
FL30     OI    FOUNDSW,FNDFILT     FOUND     FILTER                             
         MVC   FNDFILTR,FLTDESC                                                 
*                                                                               
FL90     B     NEXTELM                                                          
         DROP  R4,RF                                                            
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
OLDEL    DS    0H                  OLD  SCRIBE    ELEMENT   X'C0'               
         OI    FOUNDSW,FNDC0EL     FOUND     OLD  SCRIBE    ELEMENT             
         B     NEXTELM             CONTINUE                                     
         EJECT ,                                                                
***********************************************************************         
*  END OF FORMAT                                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R3                                                       
RS70     DS    0H                                                               
         L     R3,AIO1                                                          
         MVC   SVFORMNM,FORMNAME   SAVE FORMAT    NAME                          
         LA    RF,KEYWRD#                                                       
         LTR   RF,RF                                                            
         BZ    RS70B                                                            
         TM    FOUNDSW,FNDKYW                                                   
         BZ    NEXTFMT                                                          
*                                                                               
RS70B    DS    0H                                                               
*        TM    FOUNDSW2,FNDPAD                                                  
*        BZ    NEXTFMT                                                          
         AP    CPYFNDS,=P'1'       KEEP COUNT                                   
         AP    TOTFNDS,=P'1'       KEEP COUNT                                   
         AP    FTOTFNDS,=P'1'      KEEP COUNT                                   
         BAS   RE,SETUPPLN         PRINT OUT FORMAT DETAILS                     
*                                                                               
RS75     DS    0H                                                               
*                                                                               
NEXTFMT  L     R3,AIO1                                                          
         NI    FOUNDSW,X'FF'-FNDKYW-FNDFILT                                     
         CLI   REREAD,YES          RESET TO READ NEXT RECORD                    
         BNE   RS80                                                             
         MVC   TKEY,RSKEY                                                       
         BAS   RE,DMACREAD                                                      
*                                                                               
RS80     BAS   RE,DMACSEQ                                                       
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
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT ,                                                                
***********************************************************************         
*  SET UP PRINT LINE FOR END OF FORMAT LOGIC                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R3                                                       
         USING PDLD,R4             DETAIL    OUTPUT    LINE                     
SETUPPLN NTR1                                                                   
         L     R3,AIO1                                                          
         LA    R4,P                                                             
         GOTO1 HEXOUT,DMCB,RESKCPY,PDLCPYCD,1     COMPANY   CODE                
         MVC   PDLFORM,FORMNAME              FORMAT                             
         MVC   PDLRPTTC,REPTCODE             REPORT    TYPE CODE                
         MVC   PDLRPTYP,REPTNAME             REPORT    TYPE NAME                
         MVC   PDLLREQD,REQDATED             LAST REQUEST   DATE                
         MVC   PDLLREQX,REQUEST#             NUM  OF        REQUESTS            
         LA    RE,PDLDATA                                                       
*        GOTO1 ACREPORT                                                         
*                                                                               
         USING KYWD,R5                                                          
         TM    FOUNDSW,FNDKYW                                                   
         BZ    SETUPP30                                                         
         MVC   0(10,RE),=CL10'KEYWORDS='                                        
         LA    RE,11(,RE)                                                       
         LA    R0,KEYWRD#                                                       
         LTR   R0,R0                                                            
         BZ    SETUPP30                                                         
         L     R5,=A(KEYWRDS)                                                   
SETUPP10 TM    KYWIND,KYWFND                                                    
         BZ    SETUPP20                                                         
         NI    KYWIND,X'FF'-KYWFND                                              
         MVC   0(6,RE),KYWTEXT     MOVE KEYWORD TO PRINT LINE                   
         LA    RE,7(,RE)                                                        
*                                                                               
SETUPP20 LA    R5,KYWLNQ(,R5)                                                   
         BCT   R0,SETUPP10                                                      
*        GOTO1 ACREPORT                                                         
*                                                                               
SETUPP30 TM    FOUNDSW,FNDFILT                                                  
         BZ    SETUPP90                                                         
*                                                                               
SETUPP90 GOTO1 ACREPORT                                                         
         B     XIT                                                              
         DROP  R3,R4,R5                                                         
         EJECT ,                                                                
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
         SPACE 1                                                                
DMCTREAD LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMREAD,CTFILE,TKEY,AIO3                             
         CLI   DMCB+8,0                                                         
         BE    DMERROK                                                          
         DC    H'00'                                                            
*                                                                               
DMCTHIGH LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,TKEY,AIO3                             
         B     DMERR                                                            
*                                                                               
DMCTSEQ  LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRSEQ,CTFILE,TKEY,AIO3                             
         B     DMERR                                                            
*                                                                               
DMOPNACC LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMOPEN,ACCOUNT,ACFILEL                              
         B     DMERROK                                                          
*                                                                               
DMACREAD LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMREAD,ACCOUNT,TKEY,(R3)                            
         CLI   DMCB+8,0                                                         
         BE    DMERROK                                                          
         DC    H'0'                                                             
*                                                                               
DMACSEQ  LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCOUNT,TKEY,(R3)                            
         B     DMERR                                                            
*                                                                               
DMACHIGH LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCOUNT,TKEY,(R3)                            
         B     DMERR                                                            
*                                                                               
DMERR    MVC   BYTE,8(R1)                                                       
         NI    BYTE,X'FF'-(X'10'+X'02') IGNORE RNF/RECORD DELETED               
         CLI   BYTE,0                                                           
         BE    DMERROK                                                          
         DC    H'0'                                                             
*                                                                               
DMERROK  LR    RE,R0                                                            
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
PRTCPYTO NTR1                                                                   
         CLI   PRVKEY+2,X'00'           ANY  COMPANY ?                          
         BE    PRTCPYTX                 NO,  EXIT                               
         GOTO1 ACREPORT                                                         
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
         DROP  R4                                                               
         EJECT ,                                                                
         USING RESRECD,R3                                                       
         USING PDLD,R4             DETAIL    OUTPUT    LINE                     
KILL     NTR1                                                                   
         L     R3,AIO1                                                          
         LA    R4,P                                                             
         MVC   P+1(30),=5CL6'*DIED*'                                            
         GOTO1 ACREPORT                                                         
         GOTO1 HEXOUT,DMCB,RESKCPY,PDLCPYCD,1     COMPANY   CODE                
         MVC   PDLFORM,FORMNAME              FORMAT                             
         MVC   PDLRPTTC,REPTCODE             REPORT    TYPE CODE                
         MVC   PDLRPTYP,REPTNAME             REPORT    TYPE NAME                
         MVC   PDLLREQD,REQDATED             LAST REQUEST   DATE                
         MVC   PDLLREQX,REQUEST#             NUM  OF        REQUESTS            
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         DROP  R3,R4                                                            
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
         USING ACCRECD,R3          GENERAL   RECORD    DSECT                    
DMPGET   TM    UPSI,UPSIGET        MUST REQUEST   DUMP                          
         BZR   RE                                                               
         CLI   DMPSW,YES           ALREADY   DUMPED    THIS RECORD ?            
         BNER  RE                                                               
         MVI   DMPSW,NO                                                         
*                                                                               
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
         ICM   R8,3,ACCRLEN        RECORD    LENGTH                             
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
ESCJLFT  EQU   34                  X'22' ABOVE ESCAPE SEQUENCE                  
ESCHIGHQ EQU   48                  X'30' ABOVE ESCAPE SEQUENCE                  
K        EQU   1024                MAX   LENGTH    OF   RECORD                  
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
AIO1     DC    A(IO1)              4K FOR FORMAT RECORD                         
AIO2     DC    A(IO2)              2K FOR ANYTHING YOU WANT                     
AIO3     DC    A(IO3)              2K SYSTEM LIST                               
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
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING KYWD,R5                                                          
         USING ACWORKD,RA                                                       
         USING ACXVD,RC                                                         
FINDKYWD NMOD1 0,**FKYW**                                                       
         LA    RC,SPACEND                                                       
         L     R6,0(,R1)           GET KEYWORD DATA                             
         SR    R2,R2                                                            
         IC    R2,0(,R1)           GET LENGTH OF DATA                           
         BCTR  R2,0                                                             
         LA    R0,KEYWRD#                                                       
         LTR   R0,R0                                                            
         BZ    FKYWNEQ                                                          
         L     R5,=A(KEYWRDS)      IS THE KEYWORD IN TABLE ?                    
         CLI   DICTFORM,NO         TEXT FORM ?                                  
         BE    FKYW20              YES                                          
*                                                                               
FKYW10   CLC   KYWDD#,0(R6)        DICTIONARY     ENTRY     MATCH ?             
         BE    FKYW60              YES, FOUND     KEYWORD                       
         LA    R5,KYWLNQ(,R5)                                                   
         BCT   R0,FKYW10                                                        
         B     FKYWNEQ                                                          
*                                                                               
FKYW20   EX    R2,*+8                                                           
         BE    FKYW60                                                           
         CLC   KYWTEXT(0),0(R6)                                                 
         LA    R5,KYWLNQ(,R5)                                                   
         BCT   R0,FKYW20                                                        
         B     FKYWNEQ             NO,  CONTINUE                                
*                                                                               
FKYW60   OI    KYWIND,KYWFND       MARK FOUND                                   
         OI    FOUNDSW,FNDKYW                                                   
         B     FKYWEQ                                                           
*                                                                               
FKYWNEQ  SR    R5,R5                                                            
         CLI   *,0                 SET CC AS NOT EQUAL                          
         B     FKYWXIT                                                          
*                                                                               
FKYWEQ   CLI   *+1,0               SET CC AS EQUAL                              
*                                                                               
FKYWXIT  XIT1  REGS=(R5)                                                        
         SPACE 1                                                                
         LTORG                                                                  
         DROP  R5                                                               
***********************************************************************         
*  KEYWORD TABLES                                                     *         
***********************************************************************         
         SPACE 1                                                                
KEYWRDS  DS    0F                  KEYWORDS INCLUDE TABLE                       
         DC    AL1(0),AL2(AC#RSTYT),CL6' '          TTYPE                       
*&&DO                                                                           
         DC    AL1(0),AL2(AC#RSTIM),CL6' '          TIME                        
         DC    AL1(0),AL2(AC#RSSUS),CL6' '          STATUS                      
         DC    AL1(0),AL2(AC#RSMTH),CL6' '          MONTH                       
         DC    AL1(0),AL2(AC#RSRY1),CL6' '          YEAR                        
         DC    AL1(0),AL2(AC#RSPYC),CL6' '          PAYC                        
         DC    AL1(0),AL2(AC#RSPYN),CL6' '          PAYN                        
         DC    AL1(0),AL2(AC#RSPYT),CL6' '          PAYT                        
         DC    AL1(0),AL2(AC#RSPYD),CL6' '          PAYD                        
         DC    AL1(0),AL2(AC#RSP$T),CL6' '          P$TOT                       
         DC    AL1(0),AL2(AC#RSP$S),CL6' '          P$SAL                       
         DC    AL1(0),AL2(AC#RSP$B),CL6' '          P$BEN                       
         DC    AL1(0),AL2(AC#RSP$P),CL6' '          P$PEN                       
         DC    AL1(0),AL2(AC#RSPLT),CL6' '          PRTOT                       
         DC    AL1(0),AL2(AC#RSPLS),CL6' '          PRSAL                       
         DC    AL1(0),AL2(AC#RSPLB),CL6' '          PRBEN                       
         DC    AL1(0),AL2(AC#RSPLP),CL6' '          PRPEN                       
*&&                                                                             
KEYWRD#  EQU   (*-KEYWRDS)/KYWLNQ                                               
         EJECT ,                                                                
***********************************************************************         
*  FILTER TYPE TABLE                                                  *         
***********************************************************************         
         SPACE 1                                                                
FILTERS  DS    0F                                                               
*        DC    AL1(RFLOFF)                                                      
*        DC    CL4'OF='                                                         
*        DC    AL1(RFLTWTP)                                                     
*        DC    CL4'TM='                                                         
*        DC    AL1(RFLOPTP)                                                     
*        DC    CL4'OP='                                                         
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*  REPORT TYPES TABLE (IF EMPTY, THEN ALL REPORT TYPES)               *         
***********************************************************************         
         SPACE 1                                                                
RPTTYPES DS    0F                                                               
         DC    AL2(AC#RS497),CL6' '     PROD                                    
         DC    AL2(AC#RS498),CL6' '     PERSON                                  
         DC    AL2(AC#RSRCV),CL6' '     RCV                                     
         DC    AL2(AC#RSINC),CL6' '     INC                                     
         DC    AL2(AC#RSPAY),CL6' '     PAY                                     
         DC    AL2(AC#RSEXP),CL6' '     EXP                                     
         DC    AL2(AC#RS540),CL6' '     CASH                                    
         DC    AL2(AC#RS544),CL6' '     P&L                                     
         DC    AL2(AC#GLG),CL6' '       G/L                                     
*&&DO                                                                           
         DC    AL2(0),CL6'FI'           FINANCIAL                               
         DC    AL2(0),CL6'M2'           MANPOWER                                
         DC    AL2(0),CL6'IV'           INVOICE                                 
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
         DC    C'**AIO1**'                                                      
IO1      DC    (K*4)X'00'                                                       
*                                                                               
         DC    F'0'                IOAREA     #2 - SYSTEM    LIST REC           
         DC    C'**AIO2**'                                                      
IO2      DC    (K*2)X'00'                                                       
*                                                                               
         DC    F'0'                IOAREA     #3 - SYSTEM    LIST REC           
         DC    C'**AIO3**'                                                      
IO3      DC    (K*2)X'00'                                                       
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
PRVCPY   DS    XL1                 PREVIOUS COMPANY PROCESSED                   
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
CKEY     DS    CL(L'ACCKEY)        COMPANY KEY                                  
SVKEY    DS    CL(L'ACCKEY)        SAVED KEY                                    
CTKEY    DS    CL(L'ACCKEY)        SAVED CONTROL FILE KEY                       
RSKEY    DS    CL(L'ACCKEY)                                                     
PRVKEY   DS    CL(L'ACCKEY)                                                     
SKEY     DS    CL(L'ACCKEY)        DIRECTORY  KEY  FOR  SYS  LIST RCD           
TKEY     DS    CL(L'ACCKEY)        DM KEY                                       
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
FNDPAD   EQU   X'08'               .     FOUND WHAT I WAS LOOKING FOR           
*                                                                               
DICTFORM DS    CL1                 DICTIONARY FORM (Y/N)                        
FNDKEYWD DS    CL6                 LAST  KEYWORD   THAT WAS  FOUND              
FNDFILTR DS    CL19                LAST  FILTER    THAT WAS  FOUND              
*                                                                               
REREAD   DS    C                   Y OR N                                       
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
KYWD     DSECT                                                                  
KYWIND   DS    XL1                 KEYWORD INDICATORS                           
KYWXCLD  EQU   X'80'                  EXCLUDE TYPE KEYWORD                      
KYWFND   EQU   X'10'                  KEYWORD FOUND ON FORMAT                   
KYWDD#   DS    XL2                 KEYWORD - DICTIONARY ENTRY                   
KYWTEXT  DS    CL6                 KEYWORD - TEXT                               
KYWLNQ   EQU   *-KYWD                                                           
         EJECT ,                                                                
***********************************************************************         
*  FILTERS DSECT                                                      *         
***********************************************************************         
         SPACE 1                                                                
FLTD     DSECT                                                                  
FLTTYPE  DS    XL1                 FILTER - TYPE                                
FLTDESC  DS    CL4                 FILTER - DESCRIPTION                         
FLTLNQ   EQU   *-FLTD                                                           
         EJECT ,                                                                
***********************************************************************         
*  REPORT TYPES TO BE CONSIDERED                                      *         
*                                                                     *         
*  IF THIS TABLE IS EMPTY, THEN ALL REPORT TYPES ARE VALID            *         
***********************************************************************         
         SPACE 1                                                                
REPTYD   DSECT                                                                  
REPTDD#  DS    XL2                 REPORT TYPE - DICTIONARY ENTRY               
REPTTEXT DS    CL6                 REPORT TYPE - TEXT                           
REPTYLNQ EQU   *-REPTYD            LENGTH OF A KEYWORD ENTRY                    
         EJECT ,                                                                
***********************************************************************         
*  SUMMARY OUTPUT LINE                                                *         
***********************************************************************         
         SPACE 1                                                                
PSD      DSECT                                                                  
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
         DS    CL2                                                              
PDLCPYCD DS    CL2                 COMPANY CODE                                 
         DS    CL1                                                              
PDLFORM  DS    CL8                 FORMAT  NAME                                 
         DS    CL1                                                              
PDLRPTTC DS    CL1                 REPORT  TYPE CODE                            
         DS    CL1                                                              
PDLRPTYP DS    CL6                 REPORT  TYPE                                 
         DS    CL1                                                              
PDLLREQD DS    CL10                LAST    REQUEST DATE                         
         DS    CL1                                                              
PDLLREQX DS    CL3                 LAST    REQUEST TIMES                        
         DS    CL1                                                              
PDLDATA  DS    CL41                FOUND   WHAT                                 
PDLLNQ   EQU   *-PDLD              LENGTH OF A DETAIL OUTPUT LINE               
         EJECT ,                                                                
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
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
**PAN#1  DC    CL21'151ACREPXV08 07/09/01'                                      
         END                                                                    
