*          DATA SET ACREPXV10  AT LEVEL 161 AS OF 08/02/00                      
*PHASE ACXV02C                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE SCANNER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'VENDOR RECORDS - FILE SEARCH'                                   
***********************************************************************         
*  SEARCH FOR VENDOR RECORDS WITH MULTIPLE PAY ELEMENTS               *         
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
*    RCWRITE  - WRITE REQUEST FROM WRITE= JOB INPUT CARD              *         
*                 N     = NO                                          *         
*                 Y     = YES                                         *         
*                 BLANK = YES                                         *         
*    RCLANG   - LANGUAGE                                              *         
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
         ZAP   RCDSEQID,=P'0'      RECORDS    TOTAL     WITH SAME ID            
         ZAP   CPYEQID,=P'0'       COMPANY    TOTAL     WITH SAME ID            
         ZAP   TOTEQID,=P'0'       SYSTEM     TOTAL     WITH SAME ID            
         ZAP   FTOTEQID,=P'0'      FULL       TOTAL     WITH SAME ID            
         ZAP   CPYNEID,=P'0'       COMPANY    TOTAL     WITH DIFF IDS           
         ZAP   TOTNEID,=P'0'       SYSTEM     TOTAL     WITH DIFF IDS           
         ZAP   FTOTNEID,=P'0'      FULL       TOTAL     WITH DIFF IDS           
         ZAP   RCDDRPCR,=P'0'      RECORDS    TOTAL     DR'S PER  CR            
         ZAP   CPYMXDPC,=P'0'      COMPANY    TOTAL     DR'S PER  CR            
         ZAP   TOTMXDPC,=P'0'      SYSTEM     TOTAL     DR'S PER  CR            
         ZAP   FTOTMDPC,=P'0'      FULL       TOTAL     DR'S PER  CR            
         ZAP   RCDCRPID,=P'0'      RECORDS    TOTAL     CR'S PER  ID            
         ZAP   CPYMXCPI,=P'0'      COMPANY    TOTAL     CR'S PER  ID            
         ZAP   TOTMXCPI,=P'0'      SYSTEM     TOTAL     CR'S PER  ID            
         ZAP   FTOTMCPI,=P'0'      FULL       TOTAL     CR'S PER  ID            
         ZAP   DUMPCNT,=P'0'                                                    
         ZAP   DUMPECNT,=P'0'                                                   
         ZAP   PDUMP,=P'0'                                                      
         MVC   REQDATED,SPACES                                                  
         MVC   REQUEST#,SPACES                                                  
         MVI   FORCEHED,YES                                                     
         MVI   FCRESET,YES                                                      
         XC    SVTXKEY,SVTXKEY     CLEAR TRANSACTION    KEY  SAVE AREA          
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
         LA    R4,P                      TOTAL     FOR  RUN                     
         MVC   PSDSYSIH(L'CTOTRUN),CTOTRUN                                      
*                                                                               
         LA    R1,FTOTFNDS         ->    RUN  TOTALS                            
         BAS   RE,SUMSTAT          OUTPUT     RUN  TOTALS                       
         B     XIT                                                              
         DROP  R4                                                               
         EJECT ,                                                                
         USING TRNRECD,R3                                                       
RS       NTR1                                                                   
         XC    XKEY,XKEY                                                        
         MVI   XKEY,X'41'                                                       
         XC    PRVKEY,PRVKEY                                                    
         XC    CPYCDE,CPYCDE       COMPANY   CODE                               
         MVI   RSSWS,0             CLEAR     RS   SWITCHES                      
         LA    R3,XKEY                                                          
         L     R5,=A(ULTABLE)      ->   UNIT LEDGER    TABLE                    
*                                  SET  KEY  FOR  VENDOR    RECORDS             
         CLI   FILTSEL,0                                                        
         BZ    *+10                                                             
         MVC   TRNKCPY,FILTSEL                                                  
*                                                                               
RS05     DS    0H                  GET  1ST/NEXT  VENDOR    UNIT LEDGER         
         LA    R3,XKEY                                                          
*                                                                               
RS06     DS    0H                                                               
         XC    TRNKACT(L'XKEY-L'TRNKCPY-L'TRNKUNT-L'TRNKLDG),TRNKACT            
         MVC   TRNKULA(L'TRNKUNT+L'TRNKLDG),0(R5)                               
         BAS   RE,DMCTARDH         READ HIGH ACCOUNT   RECORD                   
         TM    DMCB+8,X'90'        EOF  OR   RCD  NOT  FOUND ?                  
         BNZ   RS90                YES, PRINT     COMPANY & ACCPAK TOTS         
         L     R3,AIO              NOTE:     FOR  X'FE'     RECORDS,            
*                                            THE  RCD  MAY  NOT  BE             
*                                            VALID;    THEREFORE,               
         CLC   XKEY,0(R3)          WAS  A    VALID     RCD  READ ?              
         BH    RS90                NO,  END  OF   ACCPAK                        
*                                                                               
RS10     DS    0H                  RECORD    FOUND                              
         L     R3,AIO                                                           
         CLC   TRNKCPY,PRVKEY      SAME COMPANY ?                               
         BE    RS18                YES, CONTINUE                                
         NC    PRVKEY,PRVKEY       1ST  RCD  ON   ACT  FILE ?                   
         BNZ   RS11                NO,  CONTINUE                                
         CLI   FILTSEL,0           COMPANY   FILTERING ?                        
         BE    RS12                NO,  GET  COMPANY   DATA                     
         CLC   FILTSEL,TRNKCPY     YES, SHOW ONLY ONE  COMPANY ?                
         BNE   RS90                NO,  PRINT     COMPANY & ACCPAK TOTS         
         B     RS12                YES, GET  COMPANY   DATA                     
*                                                                               
RS11     DS    0H                  END  LAST COMPANY   DATA                     
         BAS   RE,PRTCPYTO         PRINT     COMPANY   TOTALS                   
         CLI   FILTSEL,0           COMPANY   FILTERING ?                        
         BE    RS12                NO,  SKIP                                    
         CLC   FILTSEL,TRNKCPY     YES, SHOW ONLY THIS COMPANY ?                
         BNE   RS90                NO,  PRINT     COMPANY & ACCPAK TOTS         
*                                                                               
RS12     DS    0H                  GET  NEW  COMPANY   DATA                     
         ZAP   CPYEQID,=P'0'       COMPANY   WITH THE  SAME ID                  
         ZAP   RCDSEQID,=P'0'      RECORDS   WITH THE  SAME ID                  
         ZAP   CPYMXDPC,=P'0'      COMPANY   MAX   DR'S PER  CR                 
         ZAP   RCDDRPCR,=P'0'      RECORD          DR'S PER  CR                 
         ZAP   CPYMXCPI,=P'0'      COMPANY   MAX   CR'S PER  ID                 
         ZAP   RCDCRPID,=P'0'      RECORD          CR'S PER  ID                 
*                                  TURN OFF  CR    TX   FOUND                   
         NI    RSSWS,TURNOFF-RSCREDIT                                           
*                                                                               
         MVC   SVKEY,0(R3)         SAVE RCD  KEY                                
         MVC   CLOGO,SPACES        CLEAR     COMPANY   LOGO                     
         MVC   CALPHA,SPACES       CLEAR     COMPANY   ALPHA     CODE           
         MVC   CKEY,SPACES         CLEAR     COMPANY   KEY                      
         MVC   CKEY(1),TRNKCPY     COMPANY   ID                                 
         BAS   RE,DMCTRDC          READ COMPANY   RECORD                        
         L     R3,AIO              ->   RECORD                                  
         LA    R4,ACCORFST(,R3)    ->   1ST  ELEMENT                            
*                                                                               
RS14     DS    0H                                                               
         CLI   0(R4),0             END  OF   RECORD ?                           
         BE    RS17                YES, RESTORE   RECORD                        
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
RS17     BAS   RE,DMCTRDSV         RESTORE   RECORD                             
*                                                                               
RS18     L     R3,AIO              ->   RECORD                                  
         L     R5,=A(ULTABLE)      ->   UNIT LEDGER    TABLE                    
*                                  ANY  UNIT LEDGER ?                           
         NC    TRNKUNT(L'TRNKUNT+L'TRNKLDG),TRNKUNT                             
         BZ    RS05                NO,  GET  VENDOR    RECORD                   
*                                  SAME VENDOR    RECORD ?                      
         CLC   TRNKUNT(L'TRNKUNT+L'TRNKLDG),PRVKEY+L'TRNKCPY                    
         BE    RS20                YES, CONTINUE                                
*                                                                               
RS19     DS    0H                  VALID     VENDOR    RECORD ?                 
         CLC   TRNKUNT(L'TRNKUNT+L'TRNKLDG),0(R5)                               
         BE    RS20                YES, CONTINUE  THIS VENDOR                   
         MVC   XKEY,0(R3)          SAVE THIS COMPANY   ID                       
         BL    RS05                LOW, READ HIGH FOR  THIS UNIT LEDGER         
         LA    R5,2(,R5)           HIGH,     GET  NEXT TABLE     ENTRY          
         CLI   0(R5),0             ANY  ENTRY ?                                 
         BNE   RS19                YES, CONTINUE  LOOP                          
         BAS   RE,PRTCPYTO         PRINT     COMPANY   TOTALS                   
         CLI   FILTSEL,0           COMPANY   FILTERING ?                        
         BNE   RS95                YES, PRINT     ACCPAK    TOTALS              
         CLI   TRNKCPY,X'FE'       LAST POSSIBLE  COMPANY ?                     
         BNL   RS95                YES, PRINT     ACCPAK    TOTALS              
         LA    R3,XKEY             END  READING   FOR  THIS COMPANY             
         ZIC   RE,TRNKCPY          BUMP TO   NEXT COMPANY                       
         LA    RE,1(,RE)                                                        
         STC   RE,TRNKCPY                                                       
         L     R5,=A(ULTABLE)      ->   UNIT LEDGER    TABLE                    
         B     RS06                GET  NEXT COMPANY                            
*                                                                               
RS20     L     R3,AIO                                                           
         MVC   PRVKEY,0(R3)                                                     
         MVC   XKEY,0(R3)                                                       
         MVI   WRTSW,NO            SET    WRITE     SWITCH                      
         MVI   FOUNDSW,0           CLEAR  FOUND     SWITCH                      
         MVI   FOUNDSW2,0          CLEAR  EXCLUDE   SWITCH                      
         MVI   FOUND3,0                                                         
         MVI   DMPSW,YES           SET    DUMP SWITCH                           
         MVC   ULACCT,TRNKULA      SAVE   U/L  AND  ACCOUNT                     
         MVC   OFFICE,TRNKOFF      SAVE   OFFICE                                
         MVC   CONTRA,TRNKULC      SAVE   CONTRA    U/L  AND  ACCOUNT           
         MVC   REFID,TRNKREF       SAVE   REFERENCE      ID                     
         MVC   RCDDATE,SPACES      INIT   TRANSACTION    DATE FIELD             
         CLC   TRNKDATE,SPACES     ANY    TRANSACTION    DATE FIELD ?           
         BH    RS20A               YES,   CONTINUE                              
         OI    FOUNDSW2,FNDSKIPR   SKIP   THIS RECORD                           
         B     RS70                END    THIS RECORD                           
*                                                                               
*                                  SAVE   TRANSACTION    DATE                   
RS20A    GOTO1 DATCON,DMCB,(1,TRNKDATE),(X'20',RCDDATE)                         
*                                                                               
         TM    TRNKSTAT,TRNSPEEL   TRANSACTION PEELED ?                         
         BZ    RS21                NO,    CONTINUE                              
*                                  THIS   SHOULD    NOT  OCCUR,                 
*                                  SINCE  GENERALLY THE  TX   DATE FLD          
*                                  IS     SPACES                                
         OI    FOUNDSW2,FNDSKIPR   SKIP   THIS RECORD                           
         B     RS70                END    THIS RECORD                           
*                                                                               
RS21     CLC   SVTXKEY,0(R3)       SAME   TRANSACTION    ID ?                   
         BE    RS22                YES,   SKIP                                  
*                                  NEW    ID                                    
         MVC   SVTXKEY,0(R3)       SAVE   TRANSACTION    ID                     
         ZAP   RCDSEQID,=P'1'      CLEAR  RCDS WITH THE  SAME ID                
         AP    CPYNEID,=P'1'       ADD    COMPANY   DIFFERENT IDS               
         AP    TOTNEID,=P'1'       ADD    TOTAL     DIFFERENT IDS               
         AP    FTOTNEID,=P'1'      ADD    FULL TOT  DIFFERENT IDS               
         ZAP   KVOID,=P'0'         CLEAR  KEY  VOID COUNT                       
         ZAP   RCDDRPCR,=P'0'      CLEAR  DR'S PER  CR                          
         ZAP   RCDCRPID,=P'0'      CLEAR  CR'S PER  ID                          
*                                  TURN   OFF  CR   TX   FOUND                  
         NI    RSSWS,TURNOFF-RSCREDIT                                           
         B     RS23                CONTINUE                                     
*                                                                               
RS22     AP    RCDSEQID,=P'1'      ADD    ONE  TO   SAME ID   COUNT             
*                                                                               
         USING TRNELD,R4           MAP    TRANSACTION    ELEMENT                
RS23     LA    R4,ACCORFST(,R3)    ->     1ST  ELEMENT                          
*                                                                               
         CLI   TRNEL,TRNELQ        X'44'  TRANSACTION ELEMENT ?                 
         BE    RS30                YES,   SKIP                                  
         OI    FOUNDSW2,FNDNOTRN   NOT    TRANSACTION    ELEMENT                
         B     RS70                END    THIS ELEMENT                          
*                                                                               
RS30     DS    0H                                                               
         TM    FOUNDSW2,FNDSKIPR   SKIP   RECORD ?                              
         BO    RS70                YES,   DONE WITH RECORD                      
         CLI   0(R4),0             END    OF   RECORD ?                         
         BE    RS70                YES,   DONE WITH RECORD                      
         CLI   0(R4),TRNELQ        X'44'  TRANSACTION        ELEMENT            
         BE    TR10                                                             
         CLI   0(R4),XPYELQ        X'46'  EXTRA     PAYMENT  ELEMENT            
         BE    XP10                                                             
         CLI   0(R4),MPYELQ        X'64'  MANUAL    PAY      ELEMENT            
         BE    MP10                                                             
         CLI   0(R4),PACELQ        X'A1'  PERSON    ACTIVITY ELEMENT            
         BE    A110                                                             
         CLI   0(R4),FFTELQ        X'DB'  FREE FORM          ELEMENT            
         BE    FF10                                                             
         CLI   0(R4),PTRELQ        X'FA'  POINTER            ELEMENT            
         BE    FA10                                                             
         CLI   0(R4),DTSELQ        X'FB'  DATE/TIME STAMP    ELEMENT            
         BE    FB10                                                             
*                                                                               
RS35     DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,1(,R4)                                                        
         AR    R4,R1                                                            
         B     RS30                                                             
*                                                                               
         DROP  R3                                                               
         EJECT ,                                                                
         USING TRNELD,R4           TRANSACTION ELEMENT   X'44'                  
TR10     DS    0H                                                               
         TM    TRNSTAT,TRNSDR      DEBIT ?                                      
         BZ    TR20                NO,    SKIP                                  
         OI    FOUND3,FNDDR        SAY    DEBIT                                 
*                                                                               
         TM    TRNSTAT,TRNSREV     REVERSAL    TX ?                             
         BZ    TR15                NO,    SKIP                                  
         OI    FOUNDSW2,FNDSKIPR   SKIP   THIS RECORD                           
         B     TR30                END    THIS ELEMENT                          
*                                                                               
TR15     DS    0H                                                               
         TM    RSSWS,RSCREDIT      CREDIT TX   FOUND     BEFORE    DR           
         BO    TR18                YES,   CONTINUE                              
         OI    FOUNDSW2,FNDSKIPR   SKIP   THIS RECORD                           
         B     TR30                END    THIS ELEMENT                          
*                                                                               
TR18     DS    0H                                                               
         AP    RCDDRPCR,=P'1'      ADD    ONE  TO   DR'S PER  CR                
         CP    RCDDRPCR,CPYMXDPC   UPDATE COMPANY   MAX  DR'S PER  CR ?         
         BNH   *+10                NO,    SKIP                                  
         ZAP   CPYMXDPC,RCDDRPCR   YES,   UPDATE    IT                          
*                                                                               
TR20     DS    0H                  CREDIT                                       
         OI    FOUND3,FNDCR        SAY    CREDIT                                
         OI    RSSWS,RSCREDIT      SAY    CR   TX   FOUND                       
         ZAP   RCDDRPCR,=P'0'      CLEAR  DR'S PER  CR                          
         AP    RCDCRPID,=P'1'      ADD    ONE  TO   CR'S PER  ID                
         CP    RCDCRPID,CPYMXCPI   UPDATE COMPANY   MAX  CR'S PER  ID ?         
         BNH   *+10                NO,    SKIP                                  
         ZAP   CPYMXCPI,RCDCRPID   YES,   UPDATE    IT                          
*                                                                               
TR30     DS    0H                                                               
         OI    FOUND3,FNDTRAN      SAY    FOUND     TRNELD                      
         B     RS35                RETURN                                       
         DROP  R4                                                               
         EJECT ,                                                                
         USING XPYELD,R4           EXTRA  PAYMENT   ELEMENT   X'46'             
XP10     DS    0H                                                               
         LA    RF,L'XPYINV         LENGTH OF   INVOICE   NUMBER                 
         LA    R1,XPYINV-1(RF)     ->     LAST BYTE OF   INVOICE   NO.          
*                                                                               
XP20     DS    0H                  FIND   LENGTH    OF   INVOICE   NO.          
         CLI   0(R1),C' '          BLANK ?                                      
         BH    XP30                NO,    GOT  LENGTH                           
         BCTR  R1,0                ->     PREVIOUS  BYTE                        
         BCT   RF,XP20             TEST   PREVIOUS  BYTE                        
         B     XP40                NONE,  SKIP                                  
*                                                                               
XP30     DS    0H                  GOT    INVOICE   NUMBER    LENGTH            
         CHI   RF,12               LENGTH >=   12 ?                             
         BL    XP40                NO,    CONTINUE                              
         OI    FOUNDSW,FNDBINV#    FOUND  INVOICE   >=   12   CHARS             
         BCTR  RF,0                MINUS  ONE  FOR  EXECUTE                     
         MVC   INVNUM,SPACES       SAVE   INVOICE   NUMBER                      
         EXMVC RF,INVNUM,XPYINV                                                 
*                                                                               
XP40     DS    0H                                                               
         B     RS35                RETURN                                       
         DROP  R4                                                               
         EJECT ,                                                                
         USING MPYELD,R4           MANUAL PAYMENT   ELEMENT   X'64'             
MP10     DS    0H                                                               
         TM    FOUND3,FNDDR        DEBIT ?                                      
         BO    MP30                YES,   NOT  INTERESTED     IN  FIND          
         TM    FOUND3,FNDMPY       PREVIOUS         MPYELD                      
         BZ    MP20                NO,    SKIP                                  
         OI    FOUNDSW,FND2MPY     SAY    MORE THAN ONE  MPYELD                 
*                                                                               
MP20     DS    0H                                                               
         OI    FOUND3,FNDMPY       SAY    FOUND     MPYELD                      
         CLC   MPYNO,SPACES        NUMBER IS   SPACES ?                         
         BNE   MP30                YES,   SKIP                                  
         OC    MPYDTE,MPYDTE       ANY    ADVANCED  PAYMENT   DATE ?            
         BZ    MP30                                                             
         OI    FOUNDSW,FNDMPYN#    FOUND  MPY  EL   WITHOUT   NUMBER            
*                                                                               
MP30     DS    0H                                                               
         B     RS35                RETURN                                       
         DROP  R4                                                               
         EJECT ,                                                                
         USING PACELD,R4           PERSON ACTIVITY  ELEMENT   X'A1'             
A110     DS    0H                                                               
         LA    RF,PACDATE          LAST   ACTIVITY  DATE                        
         GOTO1 DATCON,DMCB,(1,(RF)),(21,LASTACTD)                               
         B     RS35                                                             
         DROP  R4                                                               
         EJECT ,                                                                
         USING TRNRECD,R3          TRANSACTION      RECORD                      
         USING FFTELD,R4           FREE   FORM ELEMENT   X'DB'                  
FF10     DS    0H                                                               
*                                  127+   TRANSACTIONS ?                        
         CLI   TRNKREF+MBVOFFSQ,MBVINDIQ                                        
         BNE   FF50                NO,    RETURN                                
*                                                                               
FF20     DS    0H                                                               
         CLI   FFTTYPE,FFTTKREF    KEY    REFERENCE NO.  FOR KEY VOID ?         
         BNE   FF50                NO,    RETURN                                
         CLC   REFID,FFTDATA       SAME   ID   AS   REFID ?                     
         BE    FF50                YES,   SKIP (NOT INTERESTED IN THIS)         
         AP    KVOID,=P'1'         ADD    ONE  TO   KEY  VOID COUNT             
         MVC   KVOIDID,FFTDATA     SAVE   THE  ID                               
         CP    KVOID,=P'1'                                                      
         BNE   *+8                                                              
         OI    FOUNDSW,FNDKVOID    FOUND  KEY  VOID                             
         CP    KVOID,=P'25'                                                     
         BNE   *+8                                                              
         OI    FOUNDSW,FNDKVOID    FOUND  KEY  VOID                             
         CP    KVOID,=P'50'                                                     
         BNE   *+8                                                              
         OI    FOUNDSW,FNDKVOID    FOUND  KEY  VOID                             
         CP    KVOID,=P'75'                                                     
         BNE   *+8                                                              
         OI    FOUNDSW,FNDKVOID    FOUND  KEY  VOID                             
         CP    KVOID,=P'100'                                                    
         BNE   *+8                                                              
         OI    FOUNDSW,FNDKVOID    FOUND  KEY  VOID                             
*                                                                               
FF50     DS    0H                                                               
         B     RS35                                                             
         DROP  R3,R4                                                            
*                                                                               
         EJECT ,                                                                
         USING PTRELD,R4           POINTER     ELEMENT   X'FA'                  
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
***********************************************************************         
*  END OF RECORD                                                      *         
***********************************************************************         
         SPACE 1                                                                
RS70     DS    0H                                                               
         TM    FOUNDSW2,FNDNOTRN   NO   TRANSACTION    ELEMENT                  
         BO    RS85                YES, DO   NOT  ADD  TO   COUNT               
*                                                                               
         TM    FOUNDSW2,FNDSKIPR   SKIP RECORD    ON ?                          
         BO    RS80                YES, SKIP                                    
*                                                                               
         BAS   RE,FIXCEQID         FIX  COMPANY   WITH THE  SAME ID             
*                                                                               
         CLI   FOUNDSW,0           ANYTHING  TO   REPORT ?                      
         BE    RS80                NO,  SKIP                                    
         AP    CPYFNDS,=P'1'       KEEP COUNT                                   
         AP    TOTFNDS,=P'1'       KEEP COUNT                                   
         AP    FTOTFNDS,=P'1'      KEEP COUNT                                   
*                                                                               
         USING PDLD,R4             DETAIL    OUTPUT    LINE                     
         LA    R4,P                                                             
*                                                                               
         TM    FOUNDSW,FND2MPY     FOUND     MULTIPLE  MPYELD ?                 
         BZ    RS71                NO,  SKIP                                    
         BAS   RE,SETUPPLN         SET  UP   PRINT     LINE                     
         MVC   PDLFNDWH,=CL23'MULTIPLE MANUAL PAY ELS'                          
         GOTO1 ACREPORT                                                         
*                                                                               
RS71     DS    0H                                                               
         TM    FOUNDSW,FNDMPYN#    FOUND     MPY  EL   WITHOUT   NUM ?          
         BZ    RS72                NO,  SKIP                                    
         BAS   RE,SETUPPLN         SET  UP   PRINT     LINE                     
         MVC   PDLFNDWH,=CL23'MPYELD W/OUT # W/ DATE'                           
         GOTO1 ACREPORT                                                         
*                                                                               
RS72     DS    0H                                                               
         TM    FOUNDSW,FND#EQID    FOUND     OVER MAX  WITH SAME ID ?           
         BZ    RS73                NO,  SKIP                                    
         BAS   RE,SETUPPLN         SET  UP   PRINT     LINE                     
         MVC   PDLFNDWH,=CL23'>= 128 RCDS W/ SAME ID'                           
         GOTO1 ACREPORT                                                         
*                                                                               
RS73     DS    0H                                                               
         TM    FOUNDSW,FND@EQID    FOUND     OVER MAX  WITH SAME ID ?           
         BZ    RS74                NO,  SKIP                                    
         BAS   RE,SETUPPLN         SET  UP   PRINT     LINE                     
         MVC   PDLFNDWH,=CL23'>= 250 RCDS W/ SAME ID'                           
         GOTO1 ACREPORT                                                         
*                                                                               
RS74     DS    0H                                                               
         TM    FOUNDSW,FNDKVOID    FND  KEY  REF  NO.  FOR  BANK VOID ?         
         BZ    RS75                NO,  SKIP                                    
         BAS   RE,SETUPPLN         SET  UP   PRINT     LINE                     
         MVC   PDLFNDWH,=CL23'VOID ID:IIIIII-XXXXXXX'                           
         MVC   PDLFNDWH+8(6),KVOIDID                                            
         ZAP   WORK(8),KVOID       KEY  VOID NUMBER                             
         OI    WORK+7,X'0F'                                                     
         UNPK  PDLFNDWH+15(7),WORK(8)                                           
         GOTO1 ACREPORT                                                         
*                                                                               
RS75     DS    0H                                                               
         TM    FOUNDSW,FNDBINV#    FOUND     WIDE INVOICE   NUMBER ?            
         BZ    RS76                NO,  SKIP                                    
         BAS   RE,SETUPPLN         SET  UP   PRINT     LINE                     
         MVC   PDLFNDWH,=CL23'INV NO. XXXXXXXXXXXXXX'                           
         MVC   PDLFNDWH+8(L'INVNUM),INVNUM                                      
         GOTO1 ACREPORT                                                         
*                                                                               
RS76     DS    0H                                                               
         DROP  R4                                                               
*                                                                               
RS80     DS    0H                                                               
         AP    CPYRECS,=P'1'       UPDATE    COMPANY   RECORDS                  
         AP    TOTRECS,=P'1'       UPDATE    SYSTEM    TOTAL     RCDS           
         AP    FTOTRECS,=P'1'      UPDATE    FULL      TOTAL     RCDS           
*                                                                               
*                                  COMPANY   RCDS WITH THE  SAME ID:            
         CP    CPYEQID,TOTEQID          >    TOTAL     WITH SAME ID             
         BNH   *+10                NO,  SKIP                                    
         ZAP   TOTEQID,CPYEQID     UPDATE                                       
*                                  COMPANY   RCDS WITH THE  SAME ID:            
         CP    CPYEQID,FTOTEQID         >    TOTAL     WITH SAME ID             
         BNH   *+10                NO,  SKIP                                    
         ZAP   FTOTEQID,CPYEQID    UPDATE                                       
*                                                                               
         CP    CPYMXDPC,TOTMXDPC   UPDATE ACCPAK    MAX  DR'S PER  CR ?         
         BNH   *+10                NO,    SKIP                                  
         ZAP   TOTMXDPC,CPYMXDPC   YES,   UPDATE    IT                          
         CP    CPYMXDPC,FTOTMDPC   UPDATE TOTAL     MAX  DR'S PER  CR ?         
         BNH   *+10                NO,    SKIP                                  
         ZAP   FTOTMDPC,CPYMXDPC   YES,   UPDATE    IT                          
*                                                                               
         CP    CPYMXCPI,TOTMXCPI   UPDATE ACCPAK    MAX  CR'S PER  ID ?         
         BNH   *+10                NO,    SKIP                                  
         ZAP   TOTMXCPI,CPYMXCPI   YES,   UPDATE    IT                          
         CP    CPYMXCPI,FTOTMCPI   UPDATE TOTAL     MAX  CR'S PER  ID ?         
         BNH   *+10                NO,    SKIP                                  
         ZAP   FTOTMCPI,CPYMXCPI   YES,   UPDATE    IT                          
*                                                                               
RS85     DS    0H                                                               
         TM    RSSWS,RSREREAD      REREAD    LAST      ACCOUNT   RCD ?          
         BZ    RS86                NO,  SKIP                                    
         BAS   RE,DMCTARD          REREAD    LAST      ACCOUNT   RCD            
*                                  TURN OFF  REREAD    SWITCH                   
         NI    RSSWS,TURNOFF-RSREREAD                                           
*                                                                               
RS86     DS    0H                                                               
         BAS   RE,DMCTASEQ         READ      NEXT      ACCOUNT   RCD            
         TM    BYTE+8,X'90'        EOF  OR   RCD  NOT  FOUND                    
         BNZ   RS90                YES, PRINT     COMPANY & ACCPAK TOTS         
         B     RS10                PROCESS   RECORD    FOUND                    
         EJECT ,                                                                
***********************************************************************         
*  END OF ACCPAK SYSTEM                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING PSD,R4                                                           
         SPACE 1                                                                
RS90     DS    0H                                                               
         BAS   RE,PRTCPYTO         PRINT     COMPANY   TOTALS                   
*                                                                               
RS95     DS    0H                                                               
         LA    R4,P                                                             
         MVC   PSDSYSIH,CACCSYS         ACC  SYS=                               
         MVC   PSDSYSID,ACCSYSID        GET  ACCPAK    SYSTEM    ID             
*                                                                               
*                                       TOTALS                                  
         MVC   PSDCPYCH(L'CTOTALS),CTOTALS                                      
*                                                                               
         LA    R1,TOTFNDS          ->   ACCPAK    TOTALS                        
         BAS   RE,SUMSTAT          OUTPUT    ACCPAK    TOTALS                   
         CP    TOTFNDS,=P'0'       SKIP A    LINE IF   THERE   WAS DATA         
         BE    RS97                                                             
         GOTO1 ACREPORT                                                         
         DROP  R4                                                               
*                                                                               
RS97     DS    0H                                                               
         ZAP   CPYFNDS,=P'0'                                                    
         ZAP   CPYRECS,=P'0'                                                    
         ZAP   TOTRECS,=P'0'       SYSTEM     TOTAL     RECORDS                 
         ZAP   TOTFNDS,=P'0'       SYSTEM     TOTAL     FOUND                   
         ZAP   CPYNEID,=P'0'       COMPANY    DIFFERENT IDS                     
         ZAP   TOTNEID,=P'0'       SYSTEM     DIFFERENT IDS                     
         ZAP   CPYMXDPC,=P'0'      COMPANY    MAX  DR'S PER  CR                 
         ZAP   TOTMXDPC,=P'0'      SYSTEM     MAX  DR'S PER  CR                 
         ZAP   CPYMXCPI,=P'0'      COMPANY    MAX  CR'S PER  ID                 
         ZAP   TOTMXCPI,=P'0'      SYSTEM     MAX  CR'S PER  ID                 
         MVC   REQDATED,SPACES                                                  
         MVC   REQUEST#,SPACES                                                  
         XC    SVTXKEY,SVTXKEY     CLEAR     SAVE TRANSACTION    KEY            
*                                                                               
         B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*  SET UP PRINT LINE FOR END OF RECORD LOGIC                          *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R3          TRANSACTION    RECORD                        
         USING PDLD,R4             DETAIL    OUTPUT    LINE                     
         SPACE 1                                                                
SETUPPLN NTR1                                                                   
         L     R3,AIO                                                           
         LA    R4,P                                                             
*                                  COMPANY   CODE                               
         GOTO1 HEXOUT,DMCB,TRNKCPY,PDLCPYCD,1                                   
         MVC   PDLUL,UL            UNIT      LEDGER                             
         MVC   PDLACC,ACCT         ACCOUNT                                      
         MVC   PDLOFF,OFFICE       OFFICE                                       
         MVC   PDLCUL,CNTUL        CONTRA    UNIT LEDGER                        
         MVC   PDLCNTRA,CNTACCT    CONTRA    ACCOUNT                            
         MVC   PDLDATE,RCDDATE     TRANSACTION    DATE                          
         MVC   PDLREF,REFID        REFERENCE ID                                 
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
DMCTARDH DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCOUNT,XKEY,AIO                             
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
         MVC   BYTE,8(R1)                                                       
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
*  UPDATE COMPANY WITH THE SAME ID COUNT                              *         
***********************************************************************         
         SPACE 1                                                                
FIXCEQID NTR1  ,                                                                
         CP    CPYEQID,RCDSEQID    LAST RCD  WITH NEW  COMPANY   HIGH ?         
         BNL   FIXCEQ10            NO,  SKIP                                    
         ZAP   CPYEQID,RCDSEQID    USE  NEW  MAX  RCDS WITH SAME ID             
*                                                                               
*                                  NOTE:     WE   ONLY GENERATE  THESE          
*                                            MESSAGES  ONCE FOR  THE            
*                                            SAME ID                            
FIXCEQ10 CP    RCDSEQID,=P'128'    128  RCDS WITH SAME ID ?                     
         BNE   *+8                 NO,  SKIP                                    
         OI    FOUNDSW,FND#EQID    YES, SET  FOUND     MESSAGE                  
*                                                                               
         CP    RCDSEQID,=P'250'    250  RCDS WITH SAME ID ?                     
         BNE   *+8                 NO,  SKIP                                    
         OI    FOUNDSW,FND@EQID    YES, SET  FOUND     MESSAGE                  
*                                                                               
FIXCEQ20 B     XIT                 RETURN    TO   CALLER                        
         EJECT ,                                                                
***********************************************************************         
*  PRINT TOTALS FOR COMPANY                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING PSD,R4                                                           
         SPACE 1                                                                
PRTCPYTO NTR1                                                                   
*                                       ANY  COMPANY ?                          
         CLI   PRVKEY+(TRNKCPY-TRNRECD),0                                       
         BE    PRTCPYTX                 NO,  EXIT                               
*                                       ALREADY   PRINTED   COMPANY             
*                                                 TOTALS ?                      
         CLC   PRVKEY+(TRNKCPY-TRNRECD)(1),CPYCDE                               
         BE    PRTCPYTX                 YES, EXIT                               
*                                                                               
         LA    R4,P                     ->   OUTPUT    TEXT                     
         MVC   PSDSYSIH,CACCSYS         ACC  SYS=                               
         MVC   PSDSYSID,ACCSYSID        GET  ACCPAK    SYSTEM    ID             
         MVC   PSDCPYCH,CCMP            CMP=                                    
*                                                                               
*                                       GET  COMPANY   CODE                     
         MVC   CPYCDE,PRVKEY+(TRNKCPY-TRNRECD)                                  
         GOTO1 HEXOUT,DMCB,CPYCDE,PSDCPYCD,1                                    
         MVC   PSDCPYAH,CALPHAEQ        GET  COMPANY   ALPHA     CODE           
         MVC   PSDCPYAL,CALPHA                                                  
         MVC   PSDCPYLH,CLOGOEQ         GET  COMPANY   LOGO                     
         MVC   PSDCPYLO,CLOGO                                                   
*                                                                               
         LA    R1,CPYFNDS          ->        COMPANY   TOTALS                   
         BAS   RE,SUMSTAT          OUTPUT    COMPANY   TOTALS                   
         CP    CPYFNDS,=P'0'       SKIP A    LINE IF   THERE   WAS DATA         
         BE    PRTCPYTX                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
PRTCPYTX DS    0H                                                               
         ZAP   CPYRECS,=P'0'       COMPANY   TOTAL     RECORDS                  
         ZAP   CPYFNDS,=P'0'       COMPANY   TOTAL     FOUND                    
         ZAP   CPYNEID,=P'0'       COMPANY   DIFFERENT IDS                      
         B     XIT                 RETURN                                       
*                                                                               
         DROP  R4                                                               
*                                                                               
         EJECT ,                                                                
***********************************************************************         
* GENERATE SUMMARY STATISTICS                                         *         
*                                                                     *         
*        R1    ADDRESS OF STATISTICS                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING SUMSTATD,R1         MAP  SUMMARY   STATISTICS                    
         USING PSD,R4              MAP  SUMMARY   OUTPUT    LINE                
         SPACE 1                                                                
*                                                                               
SUMSTAT  NTR1                                                                   
         LA    R4,P                     TOTAL     FOR  RUN                      
*                                                                               
         ZAP   WORK(8),SUMFNDS          GET  NUMBER    OF   MATCHES             
         OI    WORK+7,X'0F'                                                     
         MVC   PSDRMATH,CRCDMTCH        RCDS MATCHED=                           
         UNPK  PSDRMAT,WORK(8)                                                  
*                                                                               
         ZAP   WORK(8),SUMRECS          GET  NUMBER    OF   RECORDS             
         OI    WORK+7,X'0F'                                                     
         MVC   PSDTRECH,CTOTRCDS        TOT  RCDS=                              
         UNPK  PSDTREC,WORK(8)                                                  
         CP    SUMRECS,=P'100000000000'                                         
         BL    *+8                                                              
         MVI   PSDTREC-1,C'*'           INDICATE  OVERFLOW                      
*                                                                               
         ZAP   WORK(8),SUMEQID          GET  MAX  EQUAL     ID                  
         OI    WORK+7,X'0F'                                                     
         MVC   PSDEQIDH,CMAXEQID        MAX  EQ   ID=                           
         UNPK  PSDEQID,WORK(8)                                                  
*                                                                               
         ZAP   WORK(8),SUMNEID          GET  NUMBER    OF   IDS                 
         OI    WORK+7,X'0F'                                                     
         MVC   PSDNEIDH,CIDS            IDS=                                    
         UNPK  PSDNEID,WORK(8)                                                  
         CP    SUMNEID,=P'100000000000'                                         
         BL    *+8                                                              
         MVI   PSDNEID-1,C'*'           INDICATE  OVERFLOW                      
*                                                                               
         ZAP   WORK(8),SUMMXCPI         GET  MAX  CR'S  PER  ID                 
         OI    WORK+7,X'0F'                                                     
         MVC   PSDDRPCH,CCRPID          MAX  CR/ID=                             
         UNPK  PSDDRPC,WORK(8)                                                  
*                                                                               
         GOTO1 ACREPORT                                                         
*                                                                               
         CP    SUMMXDPC,=P'1'           MAX  DR'S PER   CR   >    1             
         BNH   SUMSTATX                 NO,  SKIP THIS  LINE                    
         ZAP   WORK(8),SUMMXDPC         GET  MAX  DR'S  PER  CR                 
         OI    WORK+7,X'0F'                                                     
         MVC   PSDDRPCH,CDRPCR          MAX  DR/CR=                             
         UNPK  PSDDRPC,WORK(8)                                                  
*                                                                               
         GOTO1 ACREPORT                                                         
*                                                                               
SUMSTATX B     XIT                 RETURN    TO   CALLER                        
*                                                                               
         DROP  R1,R4                                                            
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
DUMPNOW  GOTO1 PRNTBL,DMCB,(R6),(R3),C'DUMP',(R8),=C'1D'                        
         XC    TEXTMSG,TEXTMSG                                                  
*        B     XIT                                                              
         SPACE 3                                                                
XIT      XIT1                                                                   
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
*  EQUATES                                                            *         
***********************************************************************         
         SPACE 1                                                                
TURNOFF  EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
EOT      EQU   X'FF'               END OF TABLE                                 
ESCHIGHQ EQU   48                  X'30' ABOVE ESCAPE SEQUENCE                  
MXRLNQ   EQU   2100                MAX   LENGTH    OF   RECORD                  
*                                                                               
MBVOFFSQ EQU   3                   OFFSET TO BANK VOID 127+ TRANS IND           
MBVINDIQ EQU   C'*'                OFFSET TO BANK VOID 127+ TRANS IND           
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
EVERY    DC    PL4'1'                                                           
MAXDUMP  DC    PL4'500'                                                         
*                                                                               
ACCOUNT  DC    CL8'ACCOUNT '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
DMOPEN   DC    CL8'OPEN    '                                                    
ACFILEL  DC    C'NACCDIR NACCMST X'                                             
*                                                                               
CTOTRUN  DC    C'TOTAL FOR RUN:'   TOTAL   FOR     RUN  :                       
CRCDMTCH DC    C'RCDS MATCHED='    RECORDS MATCHED      =                       
CTOTRCDS DC    C'TOT RCDS='        TOTAL   RECORDS      =                       
CMAXEQID DC    C'MAX EQ ID='       MAX     EQUAL   ID   =                       
CIDS     DC    C'IDS='             IDS                  =                       
CACCSYS  DC    C'ACC SYS='         ACC     SYSTEM       =                       
CCMP     DC    C'CMP='             COMPANY              =                       
CALPHAEQ DC    C'ALPHA='           ALPHA   CODE         =                       
CLOGOEQ  DC    C'LOGO='            COMPANY LOGO         =                       
CDRPCR   DC    C'MAX DR/CR='       DR'S    PER     CR   =                       
CCRPID   DC    C'MAX CR/ID='       CR'S    PER     ID   =                       
CTOTALS  DC    C'TOTALS'           TOTALS                                       
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  UL TABLE                                                           *         
***********************************************************************         
         SPACE 1                                                                
ULTABLE  DS    0CL2                UNIT LEDGER TABLE                            
         DC    CL2'SP'                                                          
         DC    CL2'SQ'                                                          
         DC    CL2'SS'                                                          
         DC    CL2'ST'                                                          
         DC    CL2'SU'                                                          
         DC    CL2'SV'                                                          
         DC    CL2'SW'                                                          
         DC    CL2'SX'                                                          
         DC    CL2'SY'                                                          
         DC    AL1(0)                                                           
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
CPYCDE   DS    XL1                                                              
*                                                                               
*                                  FULL  TOTAL    (RUN):                        
FTOTFNDS DS    PL8                 .     FOUND                                  
FTOTRECS DS    PL8                 .     RECORDS                                
FTOTEQID DS    PL2                 .     WITH THE  SAME ID                      
FTOTNEID DS    PL8                 .     WITH DIFFERENT IDS                     
FTOTMDPC DS    PL2                 .     MAX  DR'S PER  CR                      
FTOTMCPI DS    PL2                 .     MAX  CR'S PER  ID                      
*                                                                               
*                                  TOTAL (ACCPAK):                              
TOTFNDS  DS    PL8                 .     FOUND                                  
TOTRECS  DS    PL8                 .     RECORDS                                
TOTEQID  DS    PL2                 .     WITH THE  SAME ID                      
TOTNEID  DS    PL8                 .     WITH DIFFERENT IDS                     
TOTMXDPC DS    PL2                 .     MAX  DR'S PER  CR                      
TOTMXCPI DS    PL2                 .     MAX  CR'S PER  ID                      
*                                                                               
*                                  COMPANY:                                     
CPYFNDS  DS    PL8                 .     FOUND                                  
CPYRECS  DS    PL8                 .     RECORDS                                
CPYEQID  DS    PL2                 .     WITH THE  SAME ID                      
CPYNEID  DS    PL8                 .     WITH DIFFERENT IDS                     
CPYMXDPC DS    PL2                 .     MAX  DR'S PER  CR                      
CPYMXCPI DS    PL2                 .     MAX  CR'S PER  ID                      
*                                                                               
*                                  RECORDS:                                     
RCDSEQID DS    PL2                 .     WITH THE  SAME ID                      
RCDDRPCR DS    PL2                 .     DR'S PER  CR                           
RCDCRPID DS    PL2                 .     CR'S PER  ID                           
*                                                                               
CLOGO    DS    CL(L'CPYLOGO)       COMPANY    LOGO                              
CALPHA   DS    CL(L'CPYALPHA)      COMPANY    ALPHA     CODE                    
*                                                                               
CKEY     DS    CL(L'ACCKEY)        COMPANY    KEY                               
SVKEY    DS    CL(L'ACCKEY)        SAVE RECORD     KEY                          
XKEY     DS    CL(L'ACCKEY)                                                     
PRVKEY   DS    CL(L'ACCKEY)                                                     
SKEY     DS    CL(L'ACCKEY)        DIRECTORY  KEY  FOR  SYS  LIST RCD           
*                                                                               
TEXTMSG  DS    CL12                                                             
*                                                                               
ULACCT   DS    0CL14               UNIT  LEDGER   PLUS ACCOUNT                  
UL       DS    CL2                 UNIT  LEDGER                                 
ACCT     DS    CL12                                                             
*                                                                               
OFFICE   DS    CL2                 OFFICE                                       
*                                                                               
CONTRA   DS    0CL14               CONTRA                                       
CNTUL    DS    CL2                 CONTRA     UNIT LEDGER                       
CNTACCT  DS    CL12                CONTRA     ACCOUNT                           
*                                                                               
RCDDATE  DS    CL6                 RECORD     DATE                              
REFID    DS    CL6                 REFERENCE  ID                                
NPARMS   DS    XL1                 #     OF   PARAMETERS                        
*                                                                               
KVOID    DS    PL4                 MAX   NO.  OF   KEY  VOID NUMBERS            
KVOIDID  DS    CL6                 KEY   VOID REFERENCE ID                      
*                                                                               
INVNUM   DS    CL(L'XPYINV)        INVOICE    NUMBER                            
*                                                                               
SVTXKEY  DS    XL(TRNKSBR-TRNRECD) SAVE  TRANSACTION    KEY                     
*                                                                               
FOUNDSW  DS    XL1                 FOUND SWITCHES  TO   REPORT                  
FND2MPY  EQU   X'80'               .     MORE THAN ONE  MPYEL   (X'64')         
FNDMPYN# EQU   X'40'               .     MPY  EL   WITHOUT   NUM(X'64')         
FND#EQID EQU   X'20'               .     >=   #    WITH THE  SAME ID            
FND@EQID EQU   X'10'               .     >=   @    WITH THE  SAME ID            
FNDKVOID EQU   X'08'               .     KEY  REF  NO.  FOR  KEY  VOID          
FNDBINV# EQU   X'04'               .     WIDE INVOICE   NO.                     
*                                                                               
FOUNDSW2 DS    XL1                 FOUND SWITCHES  FOR  EXCLUDE                 
FNDSKIPR EQU   X'80'               .     SKIP THIS RECORD                       
FNDNOTRN EQU   X'40'               .     NO   TRAN EL  (X'44')                  
*                                                                               
FOUND3   DS    XL1                 FOUND SWITCHES  FOR  VENDOR    RCD           
FNDTRAN  EQU   X'80'               .     TRANSACTION    EL  (X'44')             
FNDDR    EQU   X'40'               .     DEBIT                                  
FNDCR    EQU   X'20'               .     CREDIT                                 
FNDMPY   EQU   X'10'               .     MANUAL    PAY  EL  (X'64')             
*                                                                               
RSSWS    DS    X                   RS    SWITCHES                               
RSREREAD EQU   X'80'               .     REREAD    FROM ACCPAK                  
RSCREDIT EQU   X'40'               .     CREDIT    TRANSACTION    FOUND         
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
*  SUMMARY STATISTICS                                                 *         
***********************************************************************         
         SPACE 1                                                                
SUMSTATD DSECT                                                                  
*                                  SUMMARY    STATISTICS                        
SUMFNDS  DS    PL8                 .     FOUND                                  
SUMRECS  DS    PL8                 .     RECORDS                                
SUMEQID  DS    PL2                 .     WITH THE  SAME ID                      
SUMNEID  DS    PL8                 .     WITH DIFFERENT IDS                     
SUMMXDPC DS    PL2                 .     MAX  DR'S PER  CR                      
SUMMXCPI DS    PL2                 .     MAX  CR'S PER  ID                      
         EJECT ,                                                                
***********************************************************************         
*  SUMMARY OUTPUT LINE                                                *         
***********************************************************************         
         SPACE 1                                                                
PSD      DSECT                                                                  
*                                  SUMMARY LINE                                 
PSDSYSIH DS    CL(L'CACCSYS)       ACC SYS=                                     
PSDSYSID DS    CL4                 ACC     SYS  ID                              
         DS    CL1                                                              
PSDCPYCH DS    CL(L'CCMP)          CMP=                                         
PSDCPYCD DS    CL2                 COMPANY CODE                                 
         DS    CL1                                                              
PSDCPYAH DS    CL(L'CALPHAEQ)      ALPHA=                                       
PSDCPYAL DS    CL2                 ALPHA   CODE                                 
         DS    CL1                                                              
PSDCPYLH DS    CL(L'CLOGOEQ)       LOGO=                                        
PSDCPYLO DS    CL7                 COMPANY LOGO                                 
         DS    CL2                                                              
PSDRMATH DS    CL(L'CRCDMTCH)      RCDS MATCHED=                                
PSDRMAT  DS    CL8                 NUMBER                                       
         DS    CL1                                                              
PSDTRECH DS    CL(L'CTOTRCDS)      TOT RCDS=                                    
PSDTREC  DS    CL11                NUMBER                                       
         DS    CL2                                                              
PSDEQIDH DS    CL(L'CMAXEQID)      MAX EQ ID=                                   
PSDEQID  DS    CL3                 NUMBER                                       
         DS    CL1                                                              
PSDNEIDH DS    CL(L'CIDS)          IDS=                                         
PSDNEID  DS    CL11                NUMBER                                       
         DS    CL1                                                              
PSDCRPIH DS    CL(L'CCRPID)        CR/ID=                                       
PSDCRPI  DS    CL3                 NUMBER                                       
         ORG   PSDCRPIH                                                         
PSDDRPCH DS    CL(L'CDRPCR)        DR/CR=                                       
PSDDRPC  DS    CL3                 NUMBER                                       
         ORG   ,                                                                
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
PDLUL    DS    CL2                 UNIT    LEDGER                               
         DS    CL1                                                              
PDLACC   DS    CL12                ACCOUNT NAME                                 
         DS    CL1                                                              
PDLOFF   DS    CL2                 OFFICE  CODE                                 
         DS    CL3                                                              
PDLCUL   DS    CL2                 CONTRA  UNIT    LEDGER                       
         DS    CL1                                                              
PDLCNTRA DS    CL12                CONTRA  ACCOUNT NAME                         
         DS    CL3                                                              
PDLDATE  DS    CL6                 DATE                                         
         DS    CL1                                                              
PDLREF   DS    CL6                 REF     ID                                   
         DS    CL1                                                              
PDLFNDWH DS    CL23                PROBLEM FOUND                                
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
**PAN#1  DC    CL21'161ACREPXV10 08/02/00'                                      
         END                                                                    
