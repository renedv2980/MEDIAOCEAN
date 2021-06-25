*          DATA SET ACREPX502  AT LEVEL 097 AS OF 05/01/02                      
*PHASE ACX502B                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE SCANNER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'SCAN FOR SJ TYPE 47 RECORDS WITH BAD TYPE 48 RECORDS'           
***********************************************************************         
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
ACX502   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACX5**,R9                                                    
*                                                                               
         USING ACWORKD,RA                                                       
         USING ACX5D,RC                                                         
         L     RA,0(,R1)                                                        
         LA    RC,SPACEND                                                       
         EJECT ,                                                                
         SPACE 1                                                                
ACX502A  CLI   MODE,REQFRST                                                     
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
         USING TRNRECD,R3          MAP   TRANSACTION    RECORD                  
RS       NTR1                                                                   
         XC    XKEY,XKEY                                                        
         XC    PRVKEY,PRVKEY                                                    
         LA    R3,XKEY                                                          
         MVI   CPYHEX,X'3F'        CLEAR COMPANY                                
*                                                                               
RS05     DS    0H                                                               
         GOTO1 NXTCPY              GET   1ST  OR   NEXT COMPANY                 
         BNE   RS90                NONE  FOUND,    END  OF   ACCPAK             
*                                                                               
RS10     DS    0H                                                               
         L     R3,AIO              ->    SJ   RECORD                            
         CLC   TRNKCPY(3),XKEY     CHECK RECORD    TYPE (SJ)                    
         BE    RS20                YES,  CONTINUE  COMPANY                      
         NC    PRVKEY,PRVKEY       1ST   ENTRY ?                                
         BZ    *+8                 YES,  SKIP                                   
         BAS   RE,PRTCPYTO         PRINT COMPANY   TOTALS                       
         B     RS05                GET   NEXT COMPANY                           
*                                                                               
RS20     DS    0H                                                               
         MVC   PRVKEY,0(R3)        SAVE   KEY                                   
         MVC   XKEY,0(R3)                                                       
         MVI   WRTSW,NO            SET    WRITE     SWITCH                      
         MVI   FOUNDSW,0           CLEAR  FOUND     SWITCH                      
         MVI   FOUNDSW2,0          CLEAR  EXCLUDE   SWITCH                      
         MVI   DMPSW,YES           SET    DUMP SWITCH                           
*                                                                               
         USING TRNELD,R4           MAP    TRANSACTION    ELEMENT                
         LA    R4,ACCORFST(,R3)    ->     TRANSACTION    ELEMENT                
*                                                                               
         MVC   JOBNAME,TRNKULA     SAVE   JOB       NAME                        
         MVC   VNDNAME,TRNKULC     SAVE   VENDOR    NAME                        
         MVC   WCOROFF,TRNANAL     SAVE   WORK CODE OR   OFFICE                 
         MVC   TDATE,TRNKDATE      SAVE   TRANSACTION    DATE                   
*                                                                               
RS30     DS    0H                                                               
         CLC   JOBNAME,SPACES      ANY   JOB  NAME ?                            
         BE    RS50                NO,   SKIP RECORD                            
*                                                                               
RS40     DS    0H                                                               
*        CLI   TRNTYPE,TRNTEPRD    TYPE  47 ?                                   
*        BE    EST47               YES,  PROCESS   ESTIMATE                     
         CLI   TRNTYPE,TRNTEPRV    TYPE  48 ?                                   
         BE    REV48               YES,  PROCESS   REVERSAL                     
*                                                                               
RS50     DS    0H                                                               
         MVI   FOUNDSW2,FNDSKIPR   SKIP  RECORD                                 
         B     RS70                                                             
         DROP  R3,R4               KEEP  IT   CLEAN                             
*                                                                               
         EJECT ,                                                                
***********************************************************************         
*  END OF JOB                                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R3          MAP   TRANSACTION    RECORD                  
         SPACE 1                                                                
RS70     DS    0H                                                               
         L     R3,AIO                                                           
*                                                                               
         TM    FOUNDSW2,FNDSKIPR   SKIP RECORD    ON ?                          
         BO    RS80                YES, SKIP                                    
*                                                                               
         CLI   FOUNDSW,0           ANYTHING  TO   REPORT ?                      
         BE    RS80                NO,  SKIP                                    
*                                                                               
*                                  FOUND     ERROR                              
         AP    CPYFNDS,=P'1'       KEEP COUNT                                   
         AP    TOTFNDS,=P'1'       KEEP COUNT                                   
         AP    FTOTFNDS,=P'1'      KEEP COUNT                                   
*                                                                               
         USING PDLD,R4             DETAIL    OUTPUT    LINE                     
         LA    R4,P                                                             
         TM    FOUNDSW,FND48       FOUND     TYPE 48 ?                          
         BZ    RS70C               NO,  SKIP                                    
         BAS   RE,SETUPPLN         SET  UP   PRINT     LINE                     
         MVC   PDLFNDWH,=CL15'FND TYPE 48'                                      
         GOTO1 ACREPORT                                                         
*                                                                               
RS70C    DS    0H                                                               
         DROP  R4                                                               
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
         MVC   PRVJOBNM,JOBNAME    PREVIOUS   JOB       NAME                    
         MVC   PRVVNDNM,VNDNAME    PREVIOUS   VENDOR    NAME                    
         MVC   PRVWCOOF,WCOROFF    PREVIOUS   WORK CODE OR   OFFICE             
         MVC   PRVTDATE,TDATE      PREVIOUS   TRANSACTION    DATE               
         BAS   RE,DMCTARD          REREAD     LAST      ACCOUNT   RCD           
         BAS   RE,DMCTASEQ         READ       NEXT      ACCOUNT   RCD           
*                                                                               
         AP    CPYRECS,=P'1'       UPDATE     COMPANY   RECORDS                 
         AP    TOTRECS,=P'1'       UPDATE     SYSTEM    TOTAL     RCDS          
         AP    FTOTRECS,=P'1'      UPDATE     FULL      TOTAL     RCDS          
         B     RS10                                                             
         EJECT ,                                                                
***********************************************************************         
*  END OF ACCPAK                                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING PSD,R4                                                           
         SPACE 1                                                                
RS90     DS    0H                                                               
*                                                                               
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
         DROP  R3,R4               KEEP  IT   CLEAN                             
         EJECT ,                                                                
***********************************************************************         
*  PROUCESS TYPE 48 REVERSALS                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R3          MAP   TRANSACTION    RECORD                  
         USING TRNELD,R4           MAP   TRANSACTION    ELEMENT                 
         SPACE 1                                                                
REV48    DS    0H                                                               
         MVC   WORKCODE,TRNKWORK   SAVE  WORK CODE                              
         CLC   TRNKDATE,RQSTART    BEFORE     START     DATE ?                  
         BL    REV48NG             YES,  SKIP                                   
         CLC   TRNKDATE,RQEND      AFTER      END       DATE ?                  
         BH    REV48NG             YES,  SKIP                                   
         OI    FOUNDSW,FND48       SAY   FOUND     TYPE 48                      
         B     REV48OK             STOP  HERE FOR  NOW                          
*                                                                               
REV48NG  DS    0H                                                               
         OI    FOUNDSW2,FNDSKIPR   SKIP  RECORD                                 
*                                                                               
REV48OK  DS    0H                                                               
         B     RS70                                                             
         EJECT ,                                                                
***********************************************************************         
*  FIND NEXT COMPANY                                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R3          MAP   TRANSACTION   RECORD                   
         SPACE 1                                                                
NXTCPY   NTR1                                                                   
*                                                                               
NXTCPY10 DS    0H                                                               
         LA    R3,XKEY             ->    KEY                                    
         XC    XKEY(L'XKEY),XKEY   CLEAR KEY                                    
         CLI   CPYHEX,X'FE'        LAST  COMPANY   POSSIBLE ?                   
         BE    NXTCPYNG            YES,  EXIT                                   
         ZIC   R2,CPYHEX           GET   LAST COMPANY  CODE                     
         LA    R2,1(,R2)           PLUS  1                                      
         STC   R2,CPYHEX           SAVE  NEW  COMPANY  CODE                     
         STC   R2,TRNKCPY          SET   COMPANY                                
         MVC   TRNKUNT(2),SJ       SET   TRANSACTION   TYPE                     
         CLI   FILTSEL,0           SPECIFIC   COMPANY  REQUESTED ?              
         BZ    NXTCPY20            NO,   SKIP                                   
         NC    PRVKEY,PRVKEY       1ST   ENTRY ?                                
         BNZ   NXTCPYNG            NO,   EXIT                                   
         MVC   TRNKCPY,FILTSEL     USE   REQUESTED     COMPANY                  
*                                                                               
NXTCPY20 DS    0H                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCOUNT,XKEY,AIO                             
         TM    DMCB+8,X'80'        END   OF   FILE ?                            
         BO    NXTCPYNG            YES,  EXIT                                   
*                                                                               
         L     R4,AIO              ->    RECORD    READ                         
         MVC   CPYHEX,0(R4)        SAVE  COMPANY   CODE                         
         CLC   TRNKCPY,0(R4)       FOUND VALID     COMPANY ?                    
         BNE   NXTCPY10            NO,   FIND NEXT COMPANY                      
*                                                                               
NXTCPY30 DS    0H                                                               
         CLC   TRNKUNT(2),SJ       CHECK RECORD    TYPE (SJ)                    
         BNE   NXTCPY10            NO,   GET  NEXT COMPANY                      
         MVC   PRVJOBNM,SPACES     CLEAR PREVIOUS  JOB       NAME               
         MVC   PRVVNDNM,SPACES     CLEAR PREVIOUS  VENDOR    NAME               
         MVC   PRVWCOOF,SPACES     PREVIOUS   WORK CODE OR   OFFICE             
         XC    PRVTDATE,PRVTDATE   PREVIOUS   TRANSACTION    DATE               
*                                                                               
         L     R3,AIO              ->    RECORD                                 
         MVC   SVKEY,0(R3)         SAVE  RCD  KEY                               
         MVC   CLOGO,SPACES        CLEAR COMPANY   LOGO                         
         MVC   CALPHA,SPACES       CLEAR COMPANY   ALPHA     CODE               
         MVC   CKEY,SPACES         CLEAR COMPANY   KEY                          
         MVC   CKEY(1),TRNKCPY     COMPANY    ID                                
         BAS   RE,DMCTRDC          READ  COMPANY   RECORD                       
         L     R3,AIO              ->    RECORD                                 
         LA    R4,ACCORFST(,R3)    ->    1ST  ELEMENT                           
*                                                                               
NXTCPY40 DS    0H                                                               
         CLI   0(R4),0             END   OF   RECORD ?                          
         BE    NXTCPY60            YES,  RESTORE   RECORD                       
         CLI   0(R4),CPYELQ        COMPANY    ELEMENT ?                         
         BE    NXTCPY50            YES,  PROCESS                                
         ZIC   R1,1(,R4)           GET   ELEMENT   LENGTH                       
         AR    R4,R1               BUMP  TO   NEXT ELEMENT                      
         B     NXTCPY40            TRY   NEXT ELEMENT                           
*                                                                               
         USING CPYELD,R4           MAP   COMPANY   ELEMENT                      
NXTCPY50 DS    0H                                                               
         MVC   CLOGO,CPYLOGO       GET   COMPANY   LOGO                         
         MVC   CALPHA,CPYALPHA     GET   COMPANY   ALPHA     CODE               
         DROP  R4                                                               
*                                                                               
NXTCPY60 DS    0H                                                               
         BAS   RE,DMCTRDSV         RESTORE    RECORD                            
*                                                                               
NXTCPYOK DS    0H                  GOOD  EXIT                                   
         SR    R1,R1                                                            
         B     NXTCPYEX                                                         
*                                                                               
NXTCPYNG DS    0H                  ERROR EXIT                                   
         LA    R1,1                                                             
*                                                                               
NXTCPYEX LTR   R1,R1               EXIT                                         
         B     XIT                                                              
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
         GOTO1 HEXOUT,DMCB,CPYHEX,PDLCPYCD,1 COMPANY    CODE                    
         MVC   PDLJOB,JOBNAME                JOB        NAME                    
         MVC   PDLVND,VNDNAME                VENDOR     NAME                    
         MVC   PDLWC,WORKCODE                WORK  CODE                         
         GOTO1 HEXOUT,DMCB,TDATE,PDLDATE,3   TRANSACTION    DATE                
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
         MVC   CPYCDE,CPYHEX            GET  COMPANY CODE                       
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
         GOTO1 PRNTBL,DMCB,(L'JOBNAMEC,JOBNAMEC),JOBNAME,1,            X        
               L'JOBNAME,=C'1C'                                                 
         GOTO1 PRNTBL,DMCB,(L'VNDNAMEC,VNDNAMEC),VNDNAME,1,            X        
               L'VNDNAME,=C'1C'                                                 
         GOTO1 PRNTBL,DMCB,(L'WCC,WCC),WORKCODE,1,                     X        
               L'WORKCODE,=C'1C'                                                
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
EOT      EQU   X'FF'               END OF TABLE                                 
ESCHIGHQ EQU   48                  X'30' ABOVE ESCAPE SEQUENCE                  
MXRLNQ   EQU   2100                MAX   LENGTH    OF   RECORD                  
         SPACE 3                                                                
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
X5       DC    CL2'X5'                                                          
*                                                                               
SJ       DC    C'SJ'                                                            
SK       DC    C'SK'                                                            
*                                                                               
EVERY    DC    PL4'1'                                                           
MAXDUMP  DC    PL4'500'                                                         
*                                                                               
ACCOUNT  DC    CL8'ACCOUNT '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
DMOPEN   DC    CL8'OPEN    '                                                    
ACFILEL  DC    C'NACCDIR NACCMST X'                                             
*                                                                               
JOBNAMEC DC    C'JOB NAME'                                                      
VNDNAMEC DC    C'VENDOR NAME'                                                   
WCC      DC    C'WORK CODE'                                                     
         EJECT ,                                                                
         LTORG                                                                  
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
ACX5D    DSECT                                                                  
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
CPYHEX   DS    CL1                 COMPANY    HEX       CODE                    
CLOGO    DS    CL(L'CPYLOGO)       COMPANY    LOGO                              
CALPHA   DS    CL(L'CPYALPHA)      COMPANY    ALPHA     CODE                    
*                                                                               
MNFLKEY  DS    CL(L'ACCKEY)                                                     
CKEY     DS    CL(L'ACCKEY)        COMPANY    KEY                               
SVKEY    DS    CL(L'ACCKEY)        SAVE  RECORD    KEY                          
XKEY     DS    CL(L'ACCKEY)                                                     
PRVKEY   DS    CL(L'ACCKEY)                                                     
SKEY     DS    CL(L'ACCKEY)        DIRECTORY  KEY  FOR  SYS  LIST RCD           
*                                                                               
TEXTMSG  DS    CL12                                                             
*                                                                               
JOBNAME  DS    CL(L'TRNKULA)       JOB                  NAME                    
PRVJOBNM DS    CL(L'TRNKULA)       PREVIOUS   JOB       NAME                    
VNDNAME  DS    CL(L'TRNKULC)       VENDOR               NAME                    
PRVVNDNM DS    CL(L'TRNKULC)       PREVIOUS   VENDOR    NAME                    
WORKCODE DS    CL(L'TRNKWORK)      WORK  CODE                                   
OFFICE   DS    CL(L'TRNOFFC)       OFFICE                                       
WCOROFF  DS    CL(L'TRNKWORK)      WORK  CODE OR   OFFICE                       
PRVWCOOF DS    CL(L'TRNKWORK)      PREVIOUS   WORK CODE OR   OFFICE             
TDATE    DS    CL(L'TRNKDATE)      TRANSACTION     DATE                         
PRVTDATE DS    CL(L'TRNKDATE)      TRANSACTION     DATE                         
*                                                                               
NPARMS   DS    XL1                 #     OF   PARAMETERS                        
*                                                                               
FOUNDSW  DS    XL1                 FOUND SWITCHES  TO   REPORT                  
FND48    EQU   X'80'               .     TYPE 48   RECORD                       
*                                                                               
FOUNDSW2 DS    XL1                 FOUND SWITCHES  FOR  EXCLUDE                 
FNDSKIPR EQU   X'80'               .     SKIP THIS RECORD                       
*                                                                               
TRANSLAT DS    CL1                 TRANSLATED BIT  ON  (Y/N)                    
FNDJOB   DS    CL14                LAST  JOB  THAT WAS  FOUND                   
FNDFILTR DS    CL19                LAST  FILTER    THAT WAS  FOUND              
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
FILTERSD DSECT                                                                  
*                                  FILTER   ENTRY                               
FILTTYPE DS    XL1                 FILTER - TYPE                                
FILTDESC DS    CL4                 FILTER - DESCRIPTION                         
FILTERLQ EQU   *-FILTERSD          LENGTH OF A FILTER ENTRY                     
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
PDLJOB   DS    CL14                JOB     NAME                                 
         DS    CL1                                                              
PDLVND   DS    CL14                VENDOR  NAME                                 
         DS    CL1                                                              
PDLWC    DS    CL2                 WORK    CODE                                 
         DS    CL1                                                              
PDLOFF   DS    CL2                 OFFICE                                       
         DS    CL1                                                              
PDLDATE  DS    CL6                 TRANSACTION  DATE                            
         DS    CL1                                                              
PDLFNDWH DS    CL15                FOUND   WHAT                                 
         DS    CL1                                                              
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
**PAN#1  DC    CL21'097ACREPX502 05/01/02'                                      
         END                                                                    
