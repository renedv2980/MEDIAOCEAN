*          DATA SET TAREP1F    AT LEVEL 059 AS OF 07/17/14                      
*PHASE T7031FC,*                                                                
*INCLUDE SMTP                                                                   
*                                                                               
         TITLE 'T7031F - PRODUCTION INTERFACE and PRODERR REPORT'               
T7031F   CSECT                                                                  
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         NMOD1 0,T7031F,R7                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         LA    RA,BUFF                                                          
         LA    RA,8(RA)                                                         
         USING TPIND,RA            PRODUCTION INTERFACE DSECT                   
         EJECT                                                                  
         CLI   MODE,PRINTREP                                                    
         BNE   WRKX                                                             
         L     R1,=A(SAVERC)                                                    
         ST    RC,0(R1)            SAVE RC FOR NMOD ROUTINES                    
         L     R3,ATWA                                                          
         USING T703FFD,R3                                                       
         L     R2,TWADCONS         GET ADDRESS OF PRNTBL                        
         USING TWADCOND,R2                                                      
         MVC   PRNTBL,TPRNTBL                                                   
         MVC   PNLOGOC,TLOGOC      GET A(LOGOC)                                 
         L     R4,PNLOGOC          A(LOGOC)                                     
         USING LOGOD,R4                                                         
         MVC   SVLOGO1,LOGO1                                                    
*                                                                               
         MVC   AMASTD,TWAMASTC     SAVE ADDRESS OF MASTER                       
         L     R1,AMASTD                                                        
         USING MASTD,R1                                                         
         MVC   PNLOGO,MCVLOGO      GET A(LOGO)                                  
         MVC   VUTL,MCUTL          A(UTL)                                       
         CLI   MCPOSTNG,C'N'       IF POSTING = N IN MASTER                     
         BNE   MAIN05                                                           
         OI    PNOPTION,PNNOPOST   THEN SET POST = N                            
         DROP  R1,R2,R3                                                         
*                                                                               
MAIN05   BAS   RE,INIT                                                          
*                                                                               
         MVC   WKID,=C'TBI'        PROCESS ALL TBI WORKER FILES                 
         BAS   RE,PROCWK                                                        
         XC    ID,ID               FORCE RESET OF WORKER FILE ID                
         MVC   WKID,=C'TPB'        PROCESS ALL TPB WORKER FILES                 
         BAS   RE,PROCWK                                                        
*                                                                               
         CLI   RECNUM,PE           IF PROCESSING ERROR REPORT                   
         BE    MAINX               DONE                                         
*                                                                               
         LA    R2,P                SET ADDRESS TO PRINT AT                      
         ST    R2,SAVER2                                                        
         BAS   RE,PRTREP           PRINT REPORT                                 
         L     R2,SAVER2                                                        
         BAS   RE,JOBTOTS          END OF FILE - PRINT LAST JOB TOTALS          
         L     R2,SAVER2                                                        
         BAS   RE,CLITOTS          CLIENT TOTALS                                
         L     R2,SAVER2                                                        
         TM    IFSTAT,TAIFSOFS     SORT BY OFFICE                               
         BNO   MAIN15                                                           
         BAS   RE,OFFTOTS          OFFICE TOTALS                                
         L     R2,SAVER2                                                        
*                                                                               
MAIN15   BAS   RE,RUNTOTS                                                       
         BAS   RE,CNTTOTS          THAT DID NOT INTERFACE                       
*                                                                               
MAINX    TM    WRKRERR,SMTPOPN     SMTP OPENED?                                 
         BZ    MAINX2                                                           
         GOTOR =V(SMTP),DMCB,('SMTPAEND',0)       DETACH SMTP                   
MAINX2   B     XIT                                                              
         SPACE 1                                                                
YES      SR    RC,RC               SET CONDITION CODE                           
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 1                                                                
YESR3    CR    RC,RC                                                            
         XIT1 REGS=(R3)                                                         
         EJECT                                                                  
*              ROUTINE TO INITIALIZE                                            
*                                                                               
INIT     NTR1                                                                   
         L     R3,=A(NOTPOST)                                                   
         ST    R3,ANXTPOST                                                      
         MVI   NPOSTHK,C'N'                                                     
         MVI   CNTPAGE,C'N'                                                     
         LA    R3,CNTLTAB          1ST ENTRY IN CONTROL TABLE                   
         ST    R3,SVCNTL                                                        
         MVI   PRTYES,C'N'         DEFAULT - NOTHING PRINTED                    
         MVI   PRTEND,C'N'                                                      
         MVI   COUNTER,0                                                        
         XC    COUNTCNT,COUNTCNT                                                
         XC    ID,ID               SET UP NEW ID                                
         XC    HAGENCY,HAGENCY                                                  
         MVI   FORCEHED,C'Y'       SKIP TO NEXT PAGE                            
         MVI   PGNUM,0             START FROM PAGE 1                            
         ZAP   JOBAMNT,=P'0'       CLEAR SOME THINGS                            
         ZAP   CLIAMNT,=P'0'                                                    
         ZAP   OFFAMNT,=P'0'                                                    
         ZAP   RUNAMNT,=P'0'                                                    
         ZAP   NINTAMT,=P'0'                                                    
*                                                                               
         ZAP   DBTAMNT,=P'0'                                                    
         ZAP   CRDAMNT,=P'0'                                                    
         MVI   WRKRERR,0                                                        
         MVC   SPECS,=A(MYSPECS)   SET SPECS                                    
         MVC   HEADHOOK,=A(HDHOOK) SET HEAD HOOK ROUTINE                        
*                                                                               
         XC    SVAGY,SVAGY                                                      
         XC    SVPCLI,SVPCLI                                                    
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD    OPEN SORTER                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS WORKER FILES                                  
*                                                                               
PROCWK   NTR1                                                                   
         CLI   RECNUM,PE           IF PROCESSING ERROR REPORT                   
         BNE   WRK10                                                            
         BAS   RE,PROCERRS         LOOK FOR DUMMY'D ACCOUNTS                    
         B     WRKX                                                             
*                                                                               
WRK10    BAS   RE,INITA            SET WORKER ID/OPEN ACC FILE                  
         BE    WRK30                                                            
         B     WRKX                                                             
*                                                                               
WRK20    BAS   RE,WKREAD           READ FILE                                    
         BNE   WRK25                                                            
         TM    PNOPTION,PNTRACE                                                 
         BNO   WRK30                                                            
         LH    R2,WORKIO                                                        
         GOTO1 =A(MYTRACE),DMCB,=C'READ WRKR REC',WORKIO+4,(R2)                 
         B     WRK30                                                            
*                                                                               
WRK25    BAS   RE,JOBTOTS          PRINT LAST TOTALS                            
*                                                                               
         BRAS  RE,SENDMAIL         NOTIFY TEAM                                  
         TM    WRKRERR,EMAILNTF                                                 
         BZ    WRK25X                                                           
*                                  CAN PUT ON KEEP CUZ AFTER FIXING IT          
*                                  DATA CONTROL WILL RUN TPN ON IT              
*                                  THE MISSING ACCOUNTS WILL SHOW UP            
*                                  ( GH - JAN 25, 2011)                         
         BAS   RE,ONKEEP           PUT WORKER FILE ON KEEP                      
         NI    WRKRERR,X'FF'-EMAILNTF                                           
*                                                                               
WRK25X   ZAP   DBTAMNT,=P'0'       RESET DEBIT AND CREDIT TOTALS                
         ZAP   CRDAMNT,=P'0'                                                    
         BAS   RE,CLOSE            CLOSE FILE                                   
*                                                                               
WRK26    BAS   RE,INDEXRD          RE-READ THIS INDEX RECORD                    
         TM    PNOPTION,PNTRACE    TEST IF TRACE ON                             
         BNO   WRK27                                                            
         LA    R3,L'INDEX                                                       
         GOTO1 =A(MYTRACE),DMCB,=C'INDEX WRKR REC',INDEX,(R3)                   
WRK27    B     WRK10               AND LOOK FOR MORE                            
*                                                                               
WRK30    LA    R6,WORKIO+4         HANDLE RECORDS FROM WKFILE                   
         USING PSHEADD,R6                                                       
         CLC   PSHDACC+1(2),=C'SJ' PRODUCTION POSTINGS                          
         BNE   *+12                                                             
         BAS   RE,SJREC                                                         
         B     WRK20                                                            
*                                                                               
         CLC   PSHDACC+1(2),=C'SV' PAYABLE POSTINGS                             
         BE    *+14                                                             
         CLC   PSHDACC+1(2),=C'SW'                                              
         BNE   *+12                                                             
         BAS   RE,SVREC                                                         
         B     WRK20                                                            
*                                                                               
         CLC   0(2,R6),=X'521D'    MUST BE TRAILER RECORD                       
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    PNOPTION,PNTRACE                                                 
         BNO   WRK40                                                            
         LH    R2,WORKIO                                                        
         GOTO1 =A(MYTRACE),DMCB,=C'REWRITE WRKR REC',WORKIO+4,(R2)              
*                                                                               
WRK40    BAS   RE,WKWRITE            WRITE IT BACK                              
         B     WRK20                                                            
*                                                                               
WRKX     B     XIT                                                              
         EJECT                                                                  
*        ROUTINE READS WORKER FILES PROCESSED BY THE PN                         
*        PRINTS SUMMARY OF ACCOUNTS KICKED OUT                                  
         SPACE 1                                                                
PROCERRS NTR1                                                                   
PROCERR2 BAS   RE,INITA            GET A WORKER FILE                            
         BNE   XIT                                                              
*                                                                               
PROCERR4 LA    R6,WORKIO+4                                                      
         USING PSHEADD,R6                                                       
         LA    R4,70(R6)           R4=A(TRANSACTION ELE) FOR PUTTAB             
*                                                                               
         CLC   PSHDACC+1(2),=C'SJ'                IF SJ ACCOUNT                 
         BNE   PROCERR6                                                         
         CLC   PSHDACC+3(12),=CL12'CCCPPPJJJJJJ'  IS DUMMY'D                    
         BNE   PROCERR8                                                         
         MVC   WCNAME(L'PSHDSBAC),PSHDSBAC  SAVE CONTRA ACCOUNT FOR SV          
         BAS   RE,GETACTAD         SET SOME INFO FROM ACTAD ELEMENT             
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACTAD,R3                                                         
         MVC   ORIGJOB+3(12),ACTAEST                                            
         MVC   TALAGY,ACTAAGY                                                   
         MVC   SUBSTAT,ACTASTAT                                                 
         MVC   SUBSTAT2,ACTASTA2                                                
         BAS   RE,PUTTABSJ                                                      
         B     PROCERR8                                                         
*                                                                               
PROCERR6 CLC   PSHDACC+1(2),=C'SV'                IF SV ACCOUNT                 
         BNE   PROCERR8                                                         
         CLC   PSHDACC+3(12),=CL12'$$DON"T POST'  IS DUMMY'D                    
         BNE   PROCERR8                                                         
         MVI   SUBSTAT,0            CLEAR ERROR CODE                            
         MVC   PSHDACC,WCNAME       RESTORE ORIGINAL CONTRA ACCOUNT             
         BAS   RE,PUTTABSV                                                      
*                                                                               
PROCERR8 BAS   RE,WKREAD           GET NEXT WORKER FILE RECORD                  
         BNE   PROCERR9                                                         
         TM    PNOPTION,PNTRACE                                                 
         BNO   PROCERR4                                                         
         LH    R2,WORKIO                                                        
         GOTO1 =A(MYTRACE),DMCB,=C'READ WRKR REC',WORKIO+4,(R2)                 
         B     PROCERR4                                                         
*                                                                               
PROCERR9 GOTO1 =A(NOINT)           PRINT OUT ERRORS IF ANY                      
         BAS   RE,CLRNPOST         RESET NOTPOSTED TABLE                        
         B     PROCERR2                                                         
         DROP  R3                                                               
         EJECT                                                                  
                                                                                
*              ROUTINE TO SET WKFILE ID AND FIND FILE                           
*                                                                               
         SPACE 1                                                                
         USING UKRECD,R2                                                        
INITA    NTR1                                                                   
         XC    RECORD,RECORD                                                    
         LA    R2,INDEX                                                         
         OC    ID,ID                                                            
         BNZ   INIT4                                                            
         ZAP   OFFAMNT,=P'0'                                                    
         ZAP   CLIAMNT,=P'0'                                                    
         ZAP   JOBAMNT,=P'0'                                                    
*                                                                               
         XC    INDEX,INDEX                                                      
         MVI   LENKEY,5                                                         
         OC    PROCAGY,PROCAGY                                                  
         BZ    INIT2                                                            
         MVC   ID(2),PROCAGY    IF PROCESSING A SINGLE AGENCY                   
         MVI   LENKEY,7                                                         
*                                                                               
INIT2    MVC   ID+2(L'WKID),WKID   SET WORKER FILE KEY                          
         PACK  DUB(2),RCDATE+3(3)                                               
         MVC   ID+6(1),DUB                                                      
         MVI   ID+7,C'P'                                                        
         OC    SEQNO,SEQNO                                                      
         BZ    INIT4                                                            
         MVC   ID+10(2),SEQNO      PROCESS A SINGLE WORKER FILE                 
         MVI   LENKEY,11                                                        
*                                                                               
INIT4    BAS   RE,INDEXRD          GET INDEX OF (NEXT) EXISTING FILE            
         BNE   NO                                                               
         TM    PNOPTION,PNTRACE    TEST IF TRACE ON                             
         BNO   INIT12                                                           
         LA    R3,L'INDEX                                                       
         GOTO1 =A(MYTRACE),DMCB,=C'INDEX WRKR REC',INDEX,(R3)                   
*                                                                               
INIT12   TM    INDEX+12,X'08'      DON'T READ KEEP WORKER FILES                 
         BO    INIT4                                                            
         LA    R3,INDEX                                                         
         LA    R4,ID                                                            
         CLI   LENKEY,5                                                         
         BNE   INIT13                                                           
         LA    R3,INDEX+2                                                       
         LA    R4,ID+2                                                          
*                                                                               
INIT13   ZIC   R1,LENKEY           COMPARE FOR CORRECT AMOUNT                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),0(R4)                                                    
         BNE   INIT4                                                            
*                                                                               
         BAS   RE,CHKWID           CHECK WORKER FILE ID                         
         BNE   INIT4                                                            
*                                                                               
         BAS   RE,WKREAD           READ WORKER FILE                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    PNOPTION,PNTRACE    TEST IF TRACE ON                             
         BNO   INIT15                                                           
         LH    R2,WORKIO                                                        
         GOTO1 =A(MYTRACE),DMCB,=C'READ WRKR REC',WORKIO+4,(R2)                 
*                                                                               
INIT15   CLI   RECNUM,PE           IF NOT ERROR REPORT                          
         BE    INIT18                                                           
         OC    PROCAGY,PROCAGY     IF RUNNING FOR ALL AGENCIES                  
         BNZ   *+12                                                             
         BAS   RE,CHKPPN           CHECK OKAY TO PROCESS W/THIS RUN             
         BNE   INIT4                                                            
*                                                                               
INIT18   MVI   SVAGY,X'FF'         FORCE INIT OF IF REC FOR NEW WK FILE         
         BAS   RE,GETINT           GET INTERFACE INFO                           
         BNE   INIT4                                                            
*                                                                               
         BAS   RE,OPENACC          OPEN ACC FILE                                
         B     YES                                                              
         DROP  R2                                                               
         SPACE 2                                                                
*              ROUTINE CHECKS IF WORKER FILE SHOULD BE PROCESSED                
         SPACE 1                                                                
CHKWID   NTR1                                                                   
         LA    R1,EWIDTAB          R1=A(IDS ALWAYS EXCLUDED FROM PROC)          
CHKWID2  CLI   0(R1),X'FF'         TEST END OF TABLE                            
         BE    YES                                                              
         CLC   0(L'EWIDTAB,R1),INDEX   IF MATCH ON WORKER FILE ID               
         BE    NO                      SKIP IT                                  
         LA    R1,L'EWIDTAB(R1)                                                 
         B     CHKWID2                                                          
         SPACE 2                                                                
*              ROUTINE TO RESET NOTPOSTED TABLE                                 
         SPACE 1                                                                
CLRNPOST NTR1                                                                   
         L     RE,=A(NOTPOST)                                                   
         ST    RE,ANXTPOST                                                      
         LH    RF,=Y(400*NPOSTLN)                                               
         XCEFL                                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CHECKS IF TPN IS PRINTED AT AGENCY - IF SO               
*              SKIP WORKER FILE WHEN PROCESSING ALL AGENCIES                    
         SPACE                                                                  
CHKPPN   NTR1                                                                   
         LA    R4,WORKIO+4                                                      
         MVI   ELCODE,ACTAELQ      GET TALENT DETAILS ELEMENT                   
         BAS   RE,FIRSTEL                                                       
         BNE   NO                                                               
         USING ACTAD,R4                                                         
*                                                                               
         MVC   AIO,AIO2            SET TO TALENT                                
         L     R1,VUTL             READ AGENCY LEVEL INTERFACE RECORD           
         MVC   4(1,R1),SVSYS                                                    
         XC    TGPCLI,TGPCLI       INSURE NO PRODUCTION CLIENT                  
         GOTO1 RECVAL,DMCB,TLIFCDQ,(X'A4',ACTAAGY)                              
*        BE    *+6                                                              
*        DC    H'0'                                                             
         BNE   NO                                                               
         L     R1,VUTL                                                          
         MVC   4(1,R1),TGACCSE                                                  
         DROP  R4                                                               
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAIFELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAIFD,R4                                                         
         TM    TAIFSTAT,TAIFSPPN   IF PRINTING PN AT AGENCY                     
         BO    NO                  SKIP                                         
         B     YES                                                              
         EJECT                                                                  
*        OPEN ACC FILE - CHECKING FIRST IF IT'S AN EMULATED FILE                
*                                                                               
         USING TAIFD,R4                                                         
OPENACC  NTR1                                                                   
         MVC   AIO,AIO2                                                         
         L     R1,VUTL             SET TO TALENT                                
         MVC   4(1,R1),SVSYS                                                    
         GOTO1 USERVAL,DMCB,(X'80',TAIFAGY)                                     
         DROP  R4                                                               
         MVC   PNAGY,TGUSERID                                                   
         BAS   RE,GETADDR          GET ADDRESS                                  
*                                                                               
         CLI   RECNUM,PE           IF NOT ERROR REPORT                          
         BE    XIT                                                              
         L     R1,VUTL                                                          
         MVC   4(1,R1),TGACCSE                                                  
         GOTO1 DATAMGR,DMCB,=C'DTFADD',=C'ACCOUNT'                              
         L     RE,12(R1)           GET A(ACCOUNT DCB)                           
         TM    ISFTYPE-ISDTF(RE),ISFTEMU    IF BIT TURNED ON                    
         BO    OACC25                       THEN IT'S AN EMULATED FILE          
*                                                                               
         L     R4,AIO                                                           
         LA    R5,ACCLIST                                                       
         LA    R2,EMUTAB           POINT TO EMULATION TABLE                     
OACC10   CLI   0(R2),X'FF'                                                      
         BE    OACC30              REACHED END OF TABLE                         
         CLC   TGACCSE,0(R2)       EMULATE THIS ACC FILE (SE NUMBER)            
         BE    OACC20                                                           
         LA    R2,1(R2)            BUMP TO NEXT ACC FILE                        
         B     OACC10                                                           
*                                                                               
OACC20   OI    ISFTYPE-ISDTF(RE),ISFTEMU                                        
OACC25   LA    R5,EMULIST                                                       
OACC30   GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'ACCOUNT',(R5),(R4)                    
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE EXTRACTS DESTINATION ADDRESS FROM ID RECORD              
*                                                                               
GETADDR  NTR1                                                                   
         MVC   AGYADDR,SPACES                                                   
         L     R4,AIO              R4=A(RECORD)                                 
         USING CTIKEY,R4                                                        
         LA    R4,CTIDATA          R4=A(FIRST EL.)                              
         MVI   ELCODE,X'30'                                                     
         BAS   RE,FIRSTEL          LOOK FOR DESTINATION DETAILS EL.             
         BNE   XIT                                                              
         USING CTDSTD,R4                                                        
         MVC   AGYADDR1,CTDSTADD                                                
         CLI   CTDSTLEN,100                                                     
         BNH   XIT                                                              
         MVC   AGYADDR2,CTDSTAD2                                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS JOB POSTINGS                                  
*                                                                               
         SPACE 1                                                                
         USING TRANSD,R4                                                        
         USING PSHEADD,R6          R6=A(WORKER FILE RECORD)                     
SJREC    NTR1                                                                   
         BAS   RE,GETINT           GET INTERFACE INFO                           
         BNE   NO                                                               
         LA    R4,70(R6)           R4=A(TRANSACTION ELEMENT)                    
         CLI   TRNSEL,X'44'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRNSTYPE,50         USE TYPE 50 FOR INTERFACE POSTINGS           
*                                                                               
         TM    TRNSSTAT,X'80'      DEBIT?                                       
         BZ    *+14                                                             
         AP    DBTAMNT,TRNSAMNT    TOTAL UP DEBITS                              
         B     *+10                                                             
*                                                                               
         AP    CRDAMNT,TRNSAMNT    TOTAL UP CREDITS                             
*                                                                               
         MVC   ORIGJOB,PSHDACC     SAVE THIS JOB CODE                           
         MVC   MEDIA,PSHDSBNM+6    MEDIA IS IN 7TH BYTE OF C/A NAME             
         BAS   RE,REFRESH          REFRESH NAMES, ETC.                          
*                                                                               
         BAS   RE,UPD4             UPDATE THE PAYABLE ELEMENT                   
*                                                                               
         BAS   RE,DECIDE           DECIDE WHETHER TO POST SJ/SV                 
         BAS   RE,DETAILS          PUT THE RECORD INTO PRINT FORMAT             
         MVC   PSHDSBNM,SPACES                                                  
         BAS   RE,CONAME           GET ACTUAL CONTRA-ACCOUNT NAME               
*                                                                               
         CLI   NOPOST,C'Y'         IF NOT POSTING ACCOUNTS                      
         BNE   SJ40                                                             
         CLI   CLIPROB,C'Y'        AND IT IS A CLIENT PROBLEM                   
         BNE   *+8                                                              
         BAS   RE,PUTTABSJ         PUT IN TABLE FOR LATER SUMMARY               
         BAS   RE,GETACTAD         GET ACTAD ELEMENT                            
         BNE   SJ40                                                             
         USING ACTAD,R3            RETURNED R3=A(ACTAD ELEMENT)                 
         MVC   ACTASTAT,SUBSTAT    SET REASON DEFAULTING                        
         CLI   CLIPROB,C'Y'        IF NOT CLIENT PROBLEM                        
         BE    SJ40                                                             
         OI    ACTASTA2,ACTASTP    SET STATUS TP MUST HANDLE                    
*                                                                               
SJ40     TM    PNOPTION,PNTRACE                                                 
         BNO   SJ50                                                             
         LH    R2,WORKIO                                                        
         GOTO1 =A(MYTRACE),DMCB,=C'REWRITE WRKR REC',WORKIO+4,(R2)              
SJ50     BAS   RE,WKWRITE          WRITE BACK THE RECORD NOW                    
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE DECIDES IF WE SHOULD MAKE SJ/SV POSTINGS                 
*                                  XIT NOPOST SET                               
DECIDE   NTR1                                                                   
         MVI   NOPOST,C'N'         RESET SWITCH                                 
         MVI   CLIPROB,C'N'        TP FIXES/DON'T SHOW IN NOINT SUMMARY         
         CLI   SUBSW,C'Y'          IF DEFAULTED JOB                             
         BNE   XIT                                                              
*                                                                               
         CLI   SUBSTAT,ACTASXJB    BECAUSE ACCOUNT IS XJOB                      
         BE    DECIDE2                                                          
         CLI   SUBSTAT,ACTASCLI    OR BECAUSE CLIENT NOT FOUND                  
         BNE   DECIDE5                                                          
         TM    IFSTAT,TAIFSNIC     & WE ARE REQUIRING THE CLIENT                
         BZ    DECIDE5                                                          
DECIDE2  MVC   PSHDACC+3(12),=CL12'CCCPPPJJJJJJ'  DUMMY THE ACCOUNT             
         MVC   JOBNAME,SPACES                                                   
         MVI   JOBNAME,C'*'                                                     
         MVC   JOBNAME+1(12),=CL12'CCCPPPJJJJJ'                                 
         MVI   JOBNAME+14,C'*'                                                  
         GOTO1 SQUASHER,DMCB,JOBNAME,16                                         
         MVI   NOPOST,C'Y'         AND DON'T POST                               
         MVI   CLIPROB,C'Y'        SET CLIENT TAKES CARE OF                     
         B     XIT                                                              
*                                                                               
DECIDE5  CLI   YNDEF,0             AND THERE WAS NO DEFAULT JOB                 
         BE    DECIDE8                                                          
         CLI   DEFSTAT,0           DEFAULT JOB HAS NO ERRORS                    
         BE    XIT                 YES, LEAVE                                   
         MVC   SUBSTAT,DEFSTAT     NO, DON'T POST                               
         B     DECIDE2                                                          
DECIDE8  MVI   NOPOST,C'Y'         SET NOT TO POST (ACCOUNT DUMMY'D)            
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
*              ROUTINE TO PROCESS PAYABLE POSTINGS                              
*                                                                               
         SPACE 1                                                                
         USING TRANSD,R4                                                        
         USING PSHEADD,R6          R6=A(WORKER FILE RECORD)                     
SVREC    NTR1                                                                   
         MVC   PSHDSBNM,SPACES                                                  
         BAS   RE,CONAME           GET ACTUAL CONTRA-ACCOUNT NAME               
*                                                                               
         LA    R4,70(R6)           R4=A(TRANSACTION ELEMENT)                    
         CLI   TRNSEL,X'44'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    TRNSSTAT,X'80'      DEBIT?                                       
         BZ    *+14                                                             
         AP    DBTAMNT,TRNSAMNT    TOTAL UP DEBITS                              
         B     *+10                                                             
*                                                                               
         AP    CRDAMNT,TRNSAMNT    TOTAL UP CREDITS                             
*                                                                               
         MVC   TRNSANAL,OFFICE     POST TO OFFICE OF PROD. CLIENT               
         CLI   OVEROFF,C' '        IF WE HAVE PAYABLE OFFICE OVERRIDE           
         BNH   *+10                                                             
         MVC   TRNSANAL(1),OVEROFF INSURE WE ARE POSTING TO IT                  
*                                                                               
         MVI   TRNSTYPE,50         USE TYPE 50 FOR INTERFACE POSTINGS           
         BAS   RE,UPD4F                                                         
         CLI   SUBSW,C'Y'          IF WE USED DEFAULT OPTION                    
         BNE   *+8                                                              
         BAS   RE,CHASV            CHANGE PAYABLE POSTING                       
*                                                                               
         TM    PNOPTION,PNTRACE                                                 
         BNO   SV10                                                             
         LH    R2,WORKIO                                                        
         GOTO1 =A(MYTRACE),DMCB,=C'REWRITE WRKR REC',WORKIO+4,(R2)              
         GOTO1 =A(MYTRACE),DMCB,=C'OFFICE',OFFICE,2                             
SV10     BAS   RE,WKWRITE          WRITE BACK THE RECORD NOW                    
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
*              ROUTINE TO UPDATE PAYABLE POSTING BASED ON DEFAULT JOB           
*                                                                               
         SPACE 1                                                                
         USING PSHEADD,R6          R6=A(WORKER FILE RECORD)                     
CHASV    NTR1                                                                   
         MVC   PSHDSBAC,SPACES     CONTRA-ACCOUNT S/B PRODUCTION CLIENT         
         MVC   PSHDSBAC(1),ORIGJOB HEXCOMP IS 1ST BYTE                          
         MVC   PSHDSBAC+1(2),=C'SJ'                                             
         CLI   NOPOST,C'Y'         IF DIDN'T POST SJ SIDE                       
         BNE   CH10                                                             
         CLI   CLIPROB,C'Y'        AND IT IS A CLIENT PROBLEM                   
         BNE   *+8                                                              
         BAS   RE,PUTTABSV         PUT SV ACCOUNT TO TABLE TOO                  
         MVC   PSHDACC+3(12),=CL12'$$DON"T POST'                                
         B     CH20                                                             
*                                                                               
CH10     CLC   SUBJOB,=CL12'CCCPPPJJJJJJ'                                       
         BE    CH20                                                             
         MVC   PSHDSBAC+3(3),SUBJOB   USE DEFAULT CLIENT                        
*                                                                               
CH20     BAS   RE,CONAME           GET C/A NAME                                 
         LA    R4,70(R6)           LOOK FOR '23' ELEMENT                        
         MVI   ELCODE,X'23'                                                     
         BAS   RE,FIRSTEL                                                       
         BNE   XIT                                                              
         USING ACOTHERD,R4                                                      
         MVC   ACOTNUM(3),SUBJOB+3    MOVE DEFAULT PRODUCT                      
         MVC   ACOTNUM+6(6),SUBJOB+6  AND JOB TO ELEMENT                        
         B     XIT                                                              
         DROP  R4,R6                                                            
         SPACE 2                                                                
*              ROUTINE UPDATES JOB DETAILS FOR PAYABLE POSTING                  
*                                                                               
         USING TRCPJD,R3                                                        
UPD4F    NTR1                                                                   
         LA    R3,70(R6)                                                        
*                                                                               
UPD10    CLI   0(R3),0             IF END OF RECORD                             
         BE    XIT                 THEN EXIT                                    
         CLI   0(R3),X'4F'                                                      
         BE    UPD20                                                            
         ZIC   R1,1(R3)            BUMP TO CORRECT ELEMENT                      
         AR    R3,R1                                                            
         B     UPD10                                                            
*                                                                               
UPD20    MVI   TRCPTYPE,C'J'       SET THIS IS A JOB                            
         MVC   TRCPCLI(18),SPACES                                               
         MVC   TRCPCLI(3),ORIGJOB+3  MOVE IN ORIGINAL JOB NUMBER WE             
         MVC   TRCPPROD(3),ORIGJOB+6 TRIED TO POST TO                           
         MVC   TRCPJOB,ORIGJOB+9                                                
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE UPDATES PAYABLE ACCOUNT ELEMENT ON SJ                    
*                                                                               
         USING PAKELD,R3                                                        
UPD4     NTR1                                                                   
         LA    R3,70(R6)                                                        
*                                                                               
UPD402   CLI   0(R3),0             IF END OF RECORD                             
         BE    XIT                 THEN EXIT                                    
         CLI   0(R3),PAKELQ                                                     
         BE    UPD404                                                           
         ZIC   R1,PAKLN            BUMP TO CORRECT ELEMENT                      
         AR    R3,R1                                                            
         B     UPD402                                                           
*                                                                               
UPD404   MVC   PAKOFF,OFFICE       SET THE OFFICE                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*        GET INTERFACE INFO                                                     
*                                                                               
GETINT   NTR1                                                                   
         LA    R6,WORKIO+4         1ST SHOULD BE PRODUCTION POSTING             
         USING PSHEADD,R6                                                       
         MVC   MEDIA,PSHDSBNM+6    MEDIA IS IN 7TH BYTE OF C/A NAME             
         MVC   TGPCLI(3),PSHDACC+3 SET PRODUCTION CLIENT                        
         OC    TGPCLI,SPACES       PADDED WITH SPACES                           
*                                                                               
         LA    R4,WORKIO+4                                                      
         MVI   ELCODE,ACTAELQ      GET TALENT DETAILS ELEMENT                   
         USING ACTAD,R4                                                         
         BAS   RE,FIRSTEL                                                       
         BNE   GINO                SKIP THIS RECORD IF NO ACTAD ELEM            
*                                                                               
GI5      MVC   TGAGY,ACTAAGY       AGENCY FROM INTERFACE                        
         MVC   TGCLI,ACTACLI       CLIENT (FOR LATER USE)                       
         CLC   SVAGY,ACTAAGY       IF SAME AGENCY                               
         BNE   GI6                                                              
         CLC   SVPCLI(3),PSHDACC+3 AND PRODUCTION CLIENT                        
         BE    GIYES               DON'T RE-READ INTERFACE                      
*                                                                               
GI6      MVC   SVAGY,ACTAAGY       SET AGENCY                                   
         MVC   SVPCLI(3),PSHDACC+3 AND PRODUCTION CLIENT                        
         MVC   AIO,AIO1                                                         
         L     R1,VUTL             SET TO TALENT                                
         MVC   4(1,R1),SVSYS                                                    
         GOTO1 RECVAL,DMCB,TLIFCDQ,(X'A4',0)    USE GLOBAL STORAGE              
*        BE    *+6                                                              
*        DC    H'0'                                                             
         BNE   GINO                                                             
         L     R1,VUTL                                                          
         MVC   4(1,R1),TGACCSE                                                  
         DROP  R4                                                               
*                                                                               
         USING TAIFD,R4                                                         
         MVI   ELCODE,TAIFELQ      GET INTERFACE ELEMENT                        
         MVC   SUBJOB,=CL12'CCCPPPJJJJJJ'                                       
         XC    YNDEF,YNDEF        SET DEFAULT JOB NOT FOUND                     
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IFSTAT,TAIFSTAT     STATUS BYTE                                  
*                                                                               
         OC    TAIFTV,TAIFTV                                                    
         BZ    GI10                                                             
         MVC   SUBJOB,TAIFTV       SAVE TV DEFAULT JOB ALREADY                  
         OI    YNDEF,MEDDEF        MEDIA DEFAULT JOB FOUND                      
*                                                                               
GI10     CLI   MEDIA,C'T'                                                       
         BE    GI20                                                             
         CLI   MEDIA,C'R'          IF MEDIA IS RADIO                            
         BNE   GI20                                                             
         OC    TAIFRAD,TAIFRAD                                                  
         BZ    GI20                                                             
         MVC   SUBJOB,TAIFRAD      SET RADIO DEFAULT JOB                        
         OI    YNDEF,MEDDEF        MEDIA DEFAULT JOB FOUND                      
*                                                                               
GI20     DS    0H                                                               
GIYES    SR    RC,RC               SET CONDITION CODE                           
GINO     LTR   RC,RC                                                            
         XIT1 REGS=(R4)                                                         
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*              REFRESH HEADERS ETC                                              
*                                                                               
         SPACE 1                                                                
         USING PSHEADD,R6          R6=A(WORKER FILE RECORD)                     
REFRESH  NTR1                                                                   
         L     R4,=A(MYIO)                                                      
         USING ACKEYD,R4                                                        
*                                                                               
         BAS   RE,REFAGY           REFRESH AGENCY                               
         BE    *+6                                                              
         DC    H'0'                MUST BE FOUND                                
*                                                                               
         BAS   RE,REFCLI           REFRESH CLIENT                               
         BNE   SUB                 NOT FOUND - TRY FOR DEFAULT                  
*                                                                               
         BAS   RE,REFPRD           REFRESH PRODUCT                              
         BNE   SUB                 NOT FOUND - TRY FOR DEFAULT                  
*                                                                               
         MVC   ACKEYACC(15),PSHDACC HEX/SJ/CLI/PDRD/JOB                         
         CLC   JOB,PSHDACC+9       IF JOB DIDN'T CHANGE                         
         BNE   *+16                                                             
         CLI   SUBSW,C'Y'          AND IT DEFAULT'D PREVIOUSLY                  
         BE    SUB                 DEFAULT AGAIN                                
         B     XIT                 ELSE ALL DONE                                
*                                                                               
         MVI   SUBSW,C'N'          ELSE, RESET SUB SWITCHES                     
         MVI   SUBSTAT,0                                                        
         BAS   RE,REFJOB                 AND REFRESH JOB                        
         BNE   SUB                 NOT FOUND - TRY FOR DEFAULT                  
*                                                                               
         BAS   RE,REFACC           REFRESH ACCOUNT                              
         BNE   SUB                 NOT FOUND - TRY FOR DEFAULT                  
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO REFRESH AGENCY INFORMATION                            
*                                  XIT - IF CC NEQ, RECORD NOT FOUND            
         USING ACKEYD,R4                                                        
REFAGY   NTR1                                                                   
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),PSHDACC      HEX                                     
         CLC   HAGENCY,ACKEYACC                                                 
         BE    RFAGYX                                                           
         MVC   HAGENCY,ACKEYACC                                                 
         MVC   AGYNAME,MISSNAME                                                 
         XC    CLIENT,CLIENT                                                    
         BAS   RE,GOGET                                                         
         CLI   DMCB+8,0            IF RECORD FOUND                              
         BNE   NO                                                               
         LA    R3,AGYNAME          GET AGENCY NAME                              
         BAS   RE,LONG                                                          
RFAGYX   B     YES                                                              
         SPACE 2                                                                
*              ROUTINE TO REFRESH CLIENT INFORMATION                            
*                                  XIT - IF CC NEQ, SUBSTAT SET                 
         USING ACKEYD,R4                                                        
REFCLI   NTR1                                                                   
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(6),PSHDACC HEX/SJ/CLI                                   
         CLC   PSHDACC+3(3),SPACES                                              
         BH    RFCLI20                                                          
         MVC   CLINAME,MISSNAME                                                 
         MVI   PRODUCT,X'FF'                                                    
         MVI   PRDNAME,X'FF'                                                    
         B     NO                                                               
*                                                                               
RFCLI20  CLC   CLIENT,PSHDACC+3                                                 
         BE    RFCLIX                                                           
         MVC   CLIENT,PSHDACC+3                                                 
         MVC   CLINAME,MISSNAME                                                 
         MVI   PRODUCT,X'FF'                                                    
         XC    PRDNAME,PRDNAME                                                  
         BAS   RE,GOGET                                                         
         CLI   DMCB+8,0            IF RECORD NOT FOUND                          
         BE    *+12                                                             
         MVI   SUBSTAT,ACTASCLI    SET CLIENT MISSING                           
         B     NO                  EXIT WITH CC NOT EQUAL                       
*                                                                               
         BAS   RE,GETOFF           GET OFFICE                                   
         TM    YNDEF,MEDDEF        IF THERE ISN'T A MEDIA DEFAULT JOB           
         BO    *+8                                                              
         BAS   RE,GETDEF           GET DEFAULT JOB BY OFFICE                    
         LA    R3,CLINAME                                                       
         BAS   RE,LONG             GET CLIENT NAME                              
RFCLIX   B     YES                                                              
         EJECT                                                                  
*        ROUTINE TO REFRESH PRODUCT INFORMATION                                 
*                                  XIT - IF CC NEQ, SUBSTAT SET                 
         USING ACKEYD,R4                                                        
REFPRD   NTR1                                                                   
         MVC   ACKEYACC(9),PSHDACC HEX/SJ/CLI/PRD                               
         CLC   PRODUCT,PSHDACC+6                                                
         BE    RFPRDX                                                           
         MVC   PRODUCT,PSHDACC+6                                                
         MVC   PRDNAME,MISSNAME                                                 
         XC    JOB,JOB                                                          
         BAS   RE,GOGET                                                         
         CLI   DMCB+8,0            IF RECORD NOT FOUND                          
         BE    *+12                                                             
         MVI   SUBSTAT,ACTASPRD    SET PRODUCT NOT FOUND                        
         B     NO                  EXIT WITH CC NOT EQUAL                       
*                                                                               
         LA    R3,PRDNAME                                                       
         BAS   RE,LONG             ELSE, GET PRODUCT NAME                       
RFPRDX   B     YES                                                              
         DROP  R4                                                               
         SPACE 2                                                                
*        ROUTINE TO REFRESH JOB INFORMATION                                     
*                                  XIT - IF CC NEQ, SUBSTAT SET                 
         USING ACKEYD,R4                                                        
REFJOB   NTR1                                                                   
         MVC   JOB,PSHDACC+9                                                    
         CLC   JOB,SPACES          IF NO JOB SPECIFIED                          
         BH    YES                                                              
         MVI   SUBSTAT,ACTASJOB    SET JOB ERROR                                
         B     NO                  AND SET CC NOT EQUAL                         
         EJECT                                                                  
*        ROUTINE TO REFRESH ACCOUNT INFORMATION                                 
*                                  XIT - IF CC NEQ, SUBSTAT SET                 
         USING ACKEYD,R4                                                        
REFACC   NTR1                                                                   
         BAS   RE,GOGET                                                         
         CLI   DMCB+8,0            IF ACCOUNT NOT FOUND                         
         BE    *+12                                                             
         MVI   SUBSTAT,ACTASACC    SET INVALID ACCOUNT                          
         B     NO                  AND SET CC NOT EQUAL                         
*                                                                               
         BAS   RE,GETSTAT          GET JOB STATUS                               
         CLI   LOCKED,C'Y'         IF ACCOUNT LOCKED                            
         BNE   *+12                                                             
         MVI   SUBSTAT,ACTASLCK    SET LOCKED                                   
         B     NO                  AND SET CC NOT EQUAL                         
         CLI   CLOSED,C'Y'         IF ACCOUNT CLOSED                            
         BNE   *+12                                                             
         MVI   SUBSTAT,ACTASCLO    SET CLOSED                                   
         B     NO                  AND SET CC NOT EQUAL                         
         CLI   XJOB,C'Y'           IF EXPENSE JOB                               
         BNE   *+12                                                             
         MVI   SUBSTAT,ACTASXJB    SET XJOB                                     
         B     NO                  AND SET CC NOT EQUAL                         
         CLI   DRAFT,C'Y'          IF DRAFT JOB                                 
         BNE   *+12                                                             
         MVI   SUBSTAT,ACTASDRF    SET DRAFT JOB                                
         B     NO                  AND SET CC NOT EQUAL                         
*                                                                               
         LA    R3,JOBNAME          GET JOB NAME                                 
         BAS   RE,LONG                                                          
         BAS   RE,FORMAT                                                        
         B     YES                                                              
         SPACE 2                                                                
*              GET STATUS OF JOB RECORD                                         
         USING ACKEYD,R4           R4=A(JOB RECORD)                             
GETSTAT  NTR1                                                                   
         MVI   LOCKED,C'N'                                                      
         MVI   CLOSED,C'N'                                                      
         MVI   XJOB,C'N'                                                        
         MVI   DRAFT,C'N'                                                       
                                                                                
         TM    ACSTATUS,X'40'      TEST JOB IS CLOSED                           
         BZ    *+8                                                              
         MVI   CLOSED,C'Y'                                                      
         TM    ACSTATUS,X'20'      TEST JOB IS LOCKED                           
         BZ    *+8                                                              
         MVI   LOCKED,C'Y'                                                      
                                                                                
         TM    TGSYSTA2,TASYSPDJ   IF PROHIBITING DRAFT JOBS                    
         BZ    GSTA10                                                           
         TM    ACSTATUS,X'08'      TEST FOR DRAFT                               
         BZ    *+8                                                              
         MVI   DRAFT,C'Y'                                                       
                                                                                
GSTA10   LA    R4,ACRECORD         NOW LOOK FOR JOB ELEMENT                     
         MVI   ELCODE,ACJBELQ                                                   
         BAS   RE,FIRSTEL                                                       
         BNE   GSTAX                                                            
         USING ACJOBD,R4                                                        
         TM    ACJBSTAT,ACJBXJOB   TEST JOB IS X-JOB                            
         BZ    *+8                                                              
         MVI   XJOB,C'Y'                                                        
GSTAX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        CHANGE JOBS TO DEFAULTS JOBS ON INTERFACE REC                          
*                                                                               
SUB      MVI   SUBSW,C'Y'                                                       
         MVC   CLIENT,PSHDACC+3                                                 
         MVC   PRODUCT,PSHDACC+6                                                
         MVC   JOB,PSHDACC+9                                                    
         MVC   JOBNAME,SPACES                                                   
         MVI   JOBNAME,C'*'                                                     
         MVC   JOBNAME+1(3),SUBJOB                                              
         MVC   JOBNAME+4(3),SUBJOB+3                                            
         MVC   JOBNAME+7(6),SUBJOB+6                                            
         MVI   JOBNAME+14,C'*'                                                  
         GOTO1 SQUASHER,DMCB,JOBNAME,16                                         
         BAS   RE,FORMAT                                                        
*                                                                               
         CLC   CLINAME,MISSNAME    IF WE DIDN'T FIND CLIENT                     
         BNE   SUB6                                                             
         L     R4,=A(MYIO)         READ DEFAULT CLIENT                          
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(3),PSHDACC  COMPANY/UNIT/LEDGER                         
         MVC   ACKEYACC+3(3),SUBJOB CLIENT                                      
*        BAS   RE,GOGET                                                         
         XR    R1,R1                                                            
         IC    R1,SUBSTAT                                                       
         BAS   RE,REFACC                                                        
         MVC   DEFSTAT,SUBSTAT                                                  
         STC   R1,SUBSTAT                                                       
         BE    SUB4                                                             
         MVC   OFFICE,SPACES                                                    
         MVC   PNOFFICE,OFFICE                                                  
         B     *+8                                                              
SUB4     BAS   RE,GETOFF           GET ITS OFFICE                               
*                                                                               
         TM    YNDEF,MEDDEF        IF THERE ISN'T A MEDIA DEFAULT JOB           
         BO    SUB6                                                             
         BAS   RE,GETDEF           GET DEFAULT JOB BY OFFICE                    
*                                                                               
SUB6     MVC   PSHDACC+3(12),SUBJOB                                             
         L     R4,=A(MYIO)                                                      
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC(15),PSHDACC HEX/SJ/CLI/PDRD/JOB                         
         XR    R1,R1                                                            
         IC    R1,SUBSTAT                                                       
         BAS   RE,REFACC                                                        
         MVC   DEFSTAT,SUBSTAT                                                  
         STC   R1,SUBSTAT                                                       
                                                                                
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*        ROUTINE GETS DEFAULT JOB BY OFFICE IF DEFINED ON INTERFACE REC         
*                                                                               
GETDEF   NTR1                                                                   
         NI    YNDEF,X'FF'-OFFDEF  CLEAR PREVIOUS OFFICES                       
         MVC   SUBJOB,=CL12'CCCPPPJJJJJJ'     INFO                              
         USING TAIOD,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TAIOELQ      GET INTERFACE OFFICE DEFAULT JOB             
         BAS   RE,GETEL                                                         
         B     GETS15                                                           
*                                                                               
GETS10   BAS   RE,NEXTEL                                                        
*                                                                               
GETS15   BNE   GETS30                                                           
         CLC   TAIOOFF,OFFICE                                                   
         BNE   GETS10                                                           
*                                                                               
GETS20   MVC   SUBJOB,TAIOJOB                                                   
         OI    YNDEF,OFFDEF                                                     
*                                                                               
GETS30   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE RETURNS A(ACTAD ELEMENT) IN R3                           
*                                                                               
GETACTAD NTR1                                                                   
         LA    R3,70(R6)           R4=A(TRANSACTION ELEMENT)                    
*                                                                               
UPDACTA5 CLI   0(R3),0             IF END OF RECORD                             
         BE    NO                  THEN EXIT                                    
         CLI   0(R3),ACTAELQ                                                    
         BE    YESR3                                                            
         ZIC   R1,1(R3)            BUMP TO CORRECT ELEMENT                      
         AR    R3,R1                                                            
         B     UPDACTA5                                                         
         SPACE 2                                                                
*        FORMAT JOB # & NAME ON PRINT LINE                                      
*                                                                               
FORMAT   NTR1                                                                   
         LA    R3,SRTREC                                                        
         USING SRTKEY,R3                                                        
         MVC   PJOB(6),JOB                                                      
         MVC   PSRTJBNM,JOBNAME                                                 
         B     XIT                                                              
         DROP  R3                                                               
         SPACE 2                                                                
*              GET CONTRA ACCOUNT NAME                                          
*                                                                               
         SPACE 1                                                                
CONAME   NTR1                                                                   
         L     R4,=A(MYIO)                                                      
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(15),PSHDSBAC    CONTRA ACCOUNT                          
         BAS   RE,GOGET                                                         
         CLI   DMCB+8,0                                                         
         BNE   XIT                 CAN'T FIND RECORD                            
         LA    R3,PSHDSBNM                                                      
         BAS   RE,LONG                                                          
         B     XIT                                                              
         EJECT                                                                  
*              REFRESH SUBSIDIARY ROUTINES                                      
*                                                                               
GOGET    NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R4),(R4)                    
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE GETS LONG NAME                                           
*                                                                               
         USING ACKEYD,R4                                                        
LONG     NTR1                                                                   
         LA    R4,ACRECORD         R4=A(FIRST ELEMENT)                          
         MVI   ELCODE,X'20'                                                     
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACNAMED,R4                                                       
         MVC   0(36,R3),SPACES                                                  
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R3),ACNMNAME                                                 
         SPACE 2                                                                
*              ROUTINE TO GET CLIENT OFFICE                                     
*                                                                               
         USING ACKEYD,R4                                                        
GETOFF   NTR1                                                                   
         LA    R4,ACRECORD                                                      
         MVC   OFFICE,SPACES                                                    
         MVI   ELCODE,X'24'                                                     
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                MISSING PROFILE ELEMENT                      
         USING ACPROFD,R4                                                       
         CLC   ACPROFFC,SPACES                                                  
         BNH   *+10                                                             
         MVC   OFFICE,ACPROFFC                                                  
         MVC   PNOFFICE,ACPROFFC                                                
         B     XIT                                                              
         EJECT                                                                  
*              DETAILS, TOTALS                                                  
*                                                                               
         USING TRANSD,R4                                                        
DETAILS  NTR1                                                                   
         USING SRTKEY,R2                                                        
         LA    R2,SRTREC                                                        
         XC    0(170,R2),0(R2)                                                  
         XC    170(170,R2),170(R2)                                              
*                                                                               
         MVC   PSRTAGY,PNAGY       AGENCY                                       
         MVC   PSRTAYNM,AGYNAME                                                 
         MVC   PSRTOFF,PNOFFICE    OFFICE                                       
         MVC   POFFICE,PNOFFICE                                                 
         MVC   PSTAT,IFSTAT                                                     
         MVC   PSRTCLI,CLIENT      CLIENT                                       
         MVC   PCLIENT,CLIENT                                                   
         MVC   PSRTCLNM,CLINAME                                                 
         MVC   PSRTPRD,PRODUCT     PRODUCT                                      
         MVC   PPROD,PRODUCT                                                    
         MVC   PSRTPRNM,PRDNAME                                                 
         MVC   PSRTJOB,JOB         JOB                                          
         MVC   PJOB,JOB                                                         
         MVC   PSRTJBNM,JOBNAME                                                 
         MVC   PJOBSW,SUBSW                                                     
         MVC   PSRTWC,PSHDANAL     WORK CODE                                    
         MVC   PWRKCDE,PSHDANAL                                                 
         MVC   PSRTINV,TRNSREF                                                  
         MVC   PSAYADDR,AGYADDR    AGENCY ADDRESS                               
*                                                                               
         MVC   PINV,TRNSREF                                                     
         MVC   PPACAMT,TRNSAMNT                                                 
         MVC   PWRKCDE,PSHDANAL    WORK CODE                                    
         BAS   RE,WRKNAME          GET WORK CODE NAME                           
         MVC   PWCNAME,WCNAME                                                   
         DROP  R4                                                               
*                                                                               
DET10    ZIC   R1,1(R4)            FIND X'9D' ELEMENT TO GET TALENT             
         AR    R4,R1               AGENCY                                       
         CLI   0(R4),0                                                          
         BE    DET20                                                            
         CLI   0(R4),X'9D'                                                      
         BNE   DET10                                                            
         USING ACTAD,R4                                                         
         MVC   PAGY,ACTAAGY                                                     
         MVC   TALAGY,ACTAAGY                                                   
*                                                                               
DET20    GOTO1 MYTRAC2,DMCB,=C'PUT SORTER RECORD',SRTREC                        
         GOTO1 SORTER,DMCB,=C'PUT',(R2)                                         
         MVI   PRTYES,C'Y'         SOMETHING TO PRINT                           
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*        GET WORK CODE NAME                                                     
*                                                                               
WRKNAME  NTR1                                                                   
         L     R4,=A(MYIO)                                                      
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVI   ACKEYACC,X'0A'           RECORD TYPE                             
         MVC   ACKEYACC+1(3),PSHDACC    HEX/UNIT/LEDGER                         
         MVC   ACKEYACC+4(2),PWRKCDE    WORK CODE                               
*                                                                               
         CLC   WORKCD,ACKEYACC+4                                                
         BE    XIT                                                              
         MVC   WCNAME,MISSNAME                                                  
         BAS   RE,GOGET                                                         
         CLI   DMCB+8,0            IF RECORD NOT FOUND - EXIT                   
         BNE   XIT                                                              
         LA    R4,ACRECORD              A(1ST ELEMET)                           
         MVI   ELCODE,ACANELQ                                                   
         BAS   RE,FIRSTEL                                                       
         BNE   XIT                                                              
         USING ACANALD,R4                                                       
         MVC   WCNAME,ACANDESC     SET NAME                                     
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*        GET SORTED RECORD & PRINT REPORT                                       
*                                                                               
PRTREP   NTR1                                                                   
         CLI   PRTYES,C'Y'         IF ANYTHING PRINTED                          
         BNE   XIT                                                              
         MVC   PNAGY,PSRTAGY                                                    
         MVC   AGYNAME,PSRTAYNM                                                 
         XC    CLIENT,CLIENT       CLEAR LAST RECORD'S FIELDS                   
         XC    PRODUCT,PRODUCT                                                  
         XC    JOB,JOB                                                          
         XC    WORKCD,WORKCD                                                    
         LA    R2,P                                                             
         ST    R2,SAVER2                                                        
         USING LLINE,R2            R2=A(PRINT LINE)                             
*                                                                               
PRT10    GOTO1 MYTRAC2,DMCB,=C'GET SORTER RECORD',SRTREC                        
         GOTO1 SORTER,DMCB,=C'GET'                                              
         USING SRTKEY,R3                                                        
         L     R3,4(R1)            ADDRESS OF RECORD GOTTEN BY SORTER           
         LTR   R3,R3               EOF                                          
         BZ    PRX                                                              
*                                                                               
         MVI   PGBREAK,C'N'        DEFAULT                                      
         L     R2,SAVER2                                                        
         CLC   PSRTAGY(BREAK),PNAGY   REACHED A BREAK POINT                     
         BE    PRT80                                                            
         BAS   RE,JOBTOTS          PRINT OUT JOB TOTALS                         
         L     R2,SAVER2                                                        
*                                                                               
         CLC   PSRTCLI,CLIENT      IF BREAK WAS AT CLIENT                       
         BE    PRT30                                                            
         BAS   RE,CLITOTS          PRINT OUT CLIENT TOTALS                      
         L     R2,SAVER2                                                        
         MVI   PGBREAK,C'Y'        SET PAGE BREAK                               
*                                                                               
PRT30    TM    IFSTAT,TAIFSOFS     SORT BY OFFICE                               
         BNO   PRT40                                                            
         CLC   PSRTOFF,OFF         IF BREAK WAS AT OFFICE                       
         BE    PRT40                                                            
         BAS   RE,OFFTOTS          PRINT OUT OFFICE TOTALS                      
         L     R2,SAVER2                                                        
         MVI   PGBREAK,C'Y'        SET PAGE BREAK                               
*                                                                               
PRT40    CLC   PSRTAGY,PNAGY       IF BREAK WAS AT AGENCY                       
         BE    PRT50                                                            
         BAS   RE,RUNTOTS          PRINT OUT AGENCY TOTALS                      
         L     R2,SAVER2                                                        
         B     PRT60                                                            
*                                                                               
PRT50    CLI   PGBREAK,C'Y'        IF PAGE BREAK REQUIRED                       
         BNE   PRT80                                                            
         B     PRT70                                                            
*                                                                               
PRT60    MVI   FORCEHED,C'Y'                                                    
         L     R4,PNLOGOC          A(LOGOC)                                     
         USING LOGOD,R4                                                         
         MVI   LOGOTYPE,C'S'       DO START LOGO                                
         MVC   LOGO2,PSRTAGY       SET AGY NAME                                 
         MVC   LOGONAME,PSRTAYNM                                                
         MVC   LOGOADD,PSAYADD1                                                 
         MVC   LOGOADD2,PSAYADD2                                                
         MVC   LOGOADD3,PSAYADD3                                                
         GOTO1 PNLOGO,DMCB,(R4)                                                 
*                                                                               
PRT70    MVC   PNAGY,PSRTAGY       SET STARTING AGENCY                          
         MVC   AGYNAME,PSRTAYNM    SET AGY NAME                                 
         MVI   OFF,X'FF'           CLEAR LAST AGENCY'S FIELDS                   
         MVI   CLIENT,X'FF'                                                     
***      MVC   OFF,SPACES          CLEAR LAST AGENCY'S FIELDS                   
***      MVC   CLIENT,SPACES                                                    
***      MVC   PRODUCT,SPACES                                                   
***      MVC   JOB,SPACES                                                       
         MVI   FORCEHED,C'Y'                                                    
         LA    R2,P                SET ADDRESS TO PRINT AT                      
         ST    R2,SAVER2                                                        
         DROP  R4                                                               
*                                                                               
PRT80    MVC   LOFFICE,POFFICE     OFFICE                                       
         MVC   IFSTAT,PSTAT        SET STATUS                                   
         MVC   LCLIENT,PCLIENT     CLIENT                                       
         MVC   LPROD,PPROD         PRODUCT                                      
         MVC   LJOB,PJOB           JOB                                          
         MVC   LWRKCDE,PWRKCDE     WORK CODE                                    
         MVC   LWCNAME,PWCNAME     WC NAME                                      
         MVC   LAGY,PAGY                                                        
         MVC   LINV,PINV                                                        
*                                                                               
         CLC   POFFICE,OFF                                                      
         BNE   PRT85                                                            
         MVC   LOFFICE,SPACES                                                   
*                                                                               
PRT85    MVC   OFF,POFFICE                                                      
         CLC   PSRTCLI,CLIENT      DON'T REPEAT NAMES                           
         BNE   PRT90                                                            
         MVC   LCLIENT,SPACES                                                   
         B     PRT100                                                           
*                                                                               
PRT90    MVC   CLIENT,PSRTCLI                                                   
         MVI   PRODUCT,X'FF'                                                    
***      MVC   PRODUCT,SPACES      CLEAR PRODUCT IF CLIENT CHANGED              
         GOTO1 CHOPPER,DMCB,(L'PSRTCLNM,PSRTCLNM),(16,LCLNAME),(C'P',4)         
*                                                                               
PRT100   CLC   PSRTPRD,PRODUCT                                                  
         BNE   PRT110                                                           
         MVC   LPROD,SPACES                                                     
         B     PRT120                                                           
*                                                                               
PRT110   MVC   PRODUCT,PSRTPRD                                                  
         MVI   JOB,X'FF'                                                        
***      MVC   JOB,SPACES          IF PRODUCT CHANGED - CLEAR JOB               
         GOTO1 CHOPPER,DMCB,(L'PSRTPRNM,PSRTPRNM),(16,LPRNAME),(C'P',4)         
*                                                                               
PRT120   CLC   PSRTJOB,JOB         IF JOB CHANGED                               
         BNE   PRT130                                                           
         MVC   LJOB,SPACES                                                      
         B     PRT140                                                           
*                                                                               
PRT130   MVC   JOB,PSRTJOB                                                      
         MVI   WORKCD,X'FF'                                                     
***      MVC   WORKCD,SPACES       CLEAR WORK CODE                              
         GOTO1 CHOPPER,DMCB,(L'PSRTJBNM,PSRTJBNM),(16,LJBNAME),(C'P',4)         
*                                                                               
PRT140   CLC   PSRTWC,WORKCD                                                    
         BNE   PRT150                                                           
         MVC   LWRKCDE,SPACES      IF SAME WORK CODE - OMIT NAME                
         MVC   LWCNAME,SPACES       & CODE                                      
*                                                                               
PRT150   MVC   WORKCD,PSRTWC                                                    
         AP    JOBAMNT,PPACAMT                                                  
         EDIT  PPACAMT,(12,LAMOUNT),2,FLOAT=-                                   
         BAS   RE,PRINTIT                                                       
         MVI   PRTEND,C'Y'                                                      
         B     PRT10                                                            
*                                                                               
PRX      GOTO1 SORTER,DMCB,=C'END'                                              
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
         USING PSHEADD,R6                                                       
JOBTOTS  NTR1                                                                   
         USING LLINE,R2                                                         
         CLI   PRTEND,C'Y'         WAS ANYTHING PRINTED                         
         BNE   XIT                                                              
         MVC   LWCNAME,=C'JOB TOTAL       '                                     
         EDIT  JOBAMNT,(12,LAMOUNT),2,FLOAT=-                                   
         AP    RUNAMNT,JOBAMNT                                                  
**NO-OP  TM    IFSTAT,TAIFSOFS     IF SORTING BY OFFICE                         
**NO-OP  BNO   JOB10                                                            
         AP    CLIAMNT,JOBAMNT     ADD TO CLIENT COUNTER                        
*                                                                               
JOB10    ZAP   JOBAMNT,=P'0'                                                    
         MVI   COUNTER,4           FORCE PRINT                                  
         BAS   RE,PRINTIT                                                       
         BAS   RE,PRINTIT2                                                      
*                                                                               
         LA    R2,P                                                             
         ST    R2,SAVER2           NEW PLACE TO START PRINTING                  
         B     XIT                                                              
         DROP  R2,R6                                                            
         SPACE 2                                                                
         USING PSHEADD,R6                                                       
CLITOTS  NTR1                                                                   
         USING LLINE,R2                                                         
         CLI   PRTEND,C'Y'         WAS ANYTHING PRINTED                         
         BNE   XIT                                                              
         MVC   LWCNAME,=C'CLIENT TOTAL    '                                     
         EDIT  CLIAMNT,(12,LAMOUNT),2,FLOAT=-                                   
         AP    OFFAMNT,CLIAMNT                                                  
         ZAP   CLIAMNT,=P'0'                                                    
         MVI   COUNTER,4           FORCE PRINT                                  
         BAS   RE,PRINTIT                                                       
         BAS   RE,PRINTIT2                                                      
*                                                                               
         LA    R2,P                                                             
         ST    R2,SAVER2           NEW PLACE TO START PRINTING                  
         B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
*                                                                               
         USING PSHEADD,R6                                                       
OFFTOTS  NTR1                                                                   
         USING LLINE,R2                                                         
         CLI   PRTEND,C'Y'         WAS ANYTHING PRINTED                         
         BNE   XIT                                                              
         MVC   LWCNAME,=C'OFFICE TOTAL    '                                     
         EDIT  OFFAMNT,(12,LAMOUNT),2,FLOAT=-                                   
         ZAP   OFFAMNT,=P'0'                                                    
         MVI   COUNTER,4           FORCE PRINT                                  
         BAS   RE,PRINTIT                                                       
         BAS   RE,PRINTIT2                                                      
*                                                                               
         LA    R2,P                                                             
         ST    R2,SAVER2           NEW PLACE TO START PRINTING                  
         B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
RUNTOTS  NTR1                                                                   
         CLI   PRTEND,C'Y'         WAS ANYTHING PRINTED                         
         BNE   RUN10                                                            
         USING LLINE,R2                                                         
         L     R2,SAVER2                                                        
         MVC   LWCNAME(12),=C'AGENCY TOTAL'                                     
         EDIT  RUNAMNT,(12,LAMOUNT),2,FLOAT=-                                   
         MVI   COUNTER,4                                                        
         BAS   RE,PRINTIT                                                       
*                                                                               
         GOTO1 =A(NOINT)           PRINT OUT TABLE OF NO POST INVOICES          
*                                                                               
         L     R3,SVCNTL           NEXT ENTRY IN CONTROL TABLE                  
         USING CNTLD,R3                                                         
         LA    R1,CNTLTAB          CHECK IF AT END OF TABLE                     
         LH    R2,=AL2(CNTLTABX)                                                
         AR    R1,R2                                                            
         CR    R3,R1                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CNTAGY,PNAGY        SET AGENCY                                   
         MVC   CNTAMT,RUNAMNT      & AMOUNT                                     
         LA    R3,CNTLLEN(R3)      BUMP TABLE                                   
         ST    R3,SVCNTL                                                        
         LH    R3,COUNTCNT         INCREMENT # OF WORKER FILES                  
         LA    R3,1(R3)                                                         
         STH   R3,COUNTCNT                                                      
*                                                                               
         L     R4,PNLOGOC          A(LOGOC)                                     
         USING LOGOD,R4                                                         
         MVI   LOGOTYPE,C'E'       DO END LOGO                                  
         GOTO1 PNLOGO,DMCB,(R4)                                                 
         MVI   FORCEHED,C'Y'       SKIP TO NEXT PAGE                            
*                                                                               
RUN10    XC    ID,ID               SET UP NEW ID                                
         MVI   FORCEHED,C'Y'       SKIP TO NEXT PAGE                            
         MVI   PGNUM,0             START FROM PAGE 1                            
         ZAP   RUNAMNT,=P'0'       CLEAR SOME THINGS                            
         B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*        PRINT OUT TOTAL FOR DATA CONTROL                                       
*                                                                               
CNTTOTS  NTR1                                                                   
         CLI   PRTEND,C'Y'         IF PRINTED DETAIL REPORT                     
         BNE   XIT                                                              
*                                                                               
         L     R1,AMASTD           R1=A(MASTER)                                 
         USING MASTD,R1                                                         
         L     R5,MCVREMOT         R5=A(REMOTE PRINTING BLOCK)                  
         USING REMOTED,R5                                                       
         MVC   SVREMKEY,REMOTKEY   SAVE REMOTE KEY                              
         XC    REMOTKEY,REMOTKEY   FORCE CONTROL PGS TO PRINT LOCALLY           
         DROP  R1                                                               
*                                                                               
         MVI   CNTPAGE,C'Y'                                                     
         L     R4,PNLOGOC          A(LOGOC)                                     
         USING LOGOD,R4                                                         
         MVI   LOGOTYPE,C'S'       START CONTROL LOGOS                          
         MVC   LOGO1,=C'CONTROL'                                                
         MVC   LOGO2,SPACES                                                     
         MVC   LOGONAME,CNTNAME                                                 
         MVC   LOGOADD,CNTADDR                                                  
         MVC   LOGOADD2,SPACES                                                  
         MVC   LOGOADD3,SPACES                                                  
         GOTO1 PNLOGO,DMCB,(R4)                                                 
         MVI   FORCEHED,C'Y'                                                    
         MVI   PGNUM,0                                                          
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,3,COUNTCNT       NUMBER OF WORKER FILES                       
         BZ    CNTX                                                             
         LA    R3,CNTLTAB          1ST ENTRY IN CONTROL TABLE                   
         USING CNTLD,R3                                                         
         MVI   COUNTER,0                                                        
         LA    R2,HEAD4                                                         
         USING LLINE,R2                                                         
         MVC   LCLIENT+6(14),=C'POSTING TOTALS'                                 
         LA    R2,HEAD5                                                         
         MVC   LCLIENT+6(14),=31X'BF'                                           
*                                                                               
CNT10    L     R2,SAVER2                                                        
         EDIT  CNTAMT,(12,LCLIENT),2,FLOAT=-                                    
         MVC   LCLIENT+14(6),CNTAGY                                             
         BAS   RE,PRINTIT                                                       
         LA    R3,CNTLLEN(R3)      BUMP TABLE                                   
         BCT   R4,CNT10                                                         
*                                                                               
CNTX     MVI   COUNTER,4                                                        
         BAS   RE,PRINTIT                                                       
*                                                                               
         L     R4,PNLOGOC          A(LOGOC)                                     
         USING LOGOD,R4                                                         
         MVI   LOGOTYPE,C'E'       END CONTROL LOGOS                            
         GOTO1 PNLOGO,DMCB,(R4)                                                 
         MVC   LOGO1,SVLOGO1                                                    
         MVC   REMOTKEY,SVREMKEY   RESTORE REMOTE KEY                           
         B     XIT                                                              
         DROP  R2,R3,R4,R5                                                      
         EJECT                                                                  
*        PUT ENTRY INTO TABLE OF INVOICES THAT DON'T INTERFACE                  
*        THESE ARE ONLY THE ONES THAT THE CLIENT NEEDS TO BE AWARE OF           
         SPACE 1                                                                
         USING TRANSD,R4                                                        
         USING PSHEADD,R6                                                       
PUTTABSV XR    R0,R0               SET ADDING SV ACCOUNT                        
         B     *+8                                                              
         SPACE 1                                                                
PUTTABSJ LA    R0,1                SET ADDING SJ ACCONT                         
         SPACE 1                                                                
         NTR1                                                                   
         L     R3,ANXTPOST                                                      
         USING NPOSTD,R3                                                        
         L     R1,=A(NOTPOST)      CHECK IF AT END OF TABLE                     
         LH    R2,=AL2(NOTPOSTX)                                                
         AR    R1,R2                                                            
         CR    R3,R1                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   NPPAGY,PNAGY                                                     
         MVC   NPAGY,TALAGY                                                     
         MVC   NPWC,PSHDANAL                                                    
         MVC   NPINV,TRNSREF                                                    
         MVC   NPAMT,TRNSAMNT                                                   
         MVC   NPDET,TRNSSTAT                                                   
*                                                                               
         LTR   R0,R0               IF SJ ACCOUNT                                
         BZ    PUTTAB10                                                         
         OI    NPDET,NPDSJ         INDICATE SO                                  
         CLI   RECNUM,PE           IF TP ERROR REPORT                           
         BNE   PUTTAB5                                                          
         CLI   SUBSTAT2,ACTASTP    AND TP MUST FIX THIS ONE                     
         BNE   PUTTAB5                                                          
         OI    NPDET,NPDTPFIX      SET INDICATOR FOR TALENT                     
PUTTAB5  MVC   NPACC,ORIGJOB+3     SET ACCOUNT                                  
         MVC   NPSTAT,SUBSTAT      AND ERROR CONDITION                          
         B     PUTTABX                                                          
*                                                                               
PUTTAB10 MVC   NPACC,PSHDACC+3     SET SV ACCOUNT INFO                          
*                                                                               
PUTTABX  LA    R3,NPOSTLN(R3)      SET A(NEXT POSITION IN TABLE)                
         ST    R3,ANXTPOST                                                      
         OI    WRKRERR,ACCTERRS    MARK WE HAVE ACCTING ERRORS                  
         TM    PNOPTION,PNTRACE                                                 
         BNO   XIT                                                              
         L     R3,=A(NOTPOST)                                                   
         LH    R2,=Y(400*NPOSTLN)                                               
         GOTO1 =A(MYTRACE),DMCB,=C'NOPOST TABLE',(R3),(R2)                      
         B     XIT                                                              
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
*                                                                               
*        PRINT A LINE                                                           
*        INPUT - SAVER2 - LINE TO PRINT ON                                      
*                                                                               
         SPACE 1                                                                
PRINTIT  NTR1                                                                   
         ZIC   R1,COUNTER                                                       
         LA    R1,1(R1)            BUMP COUNTER                                 
         STC   R1,COUNTER                                                       
*                                                                               
         L     R2,SAVER2                                                        
         CH    R1,=H'4'            PRINT EVERY 4 LINES                          
         BL    PRNT10                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   COUNTER,0           CLEAR LINE COUNTER                           
         LA    R2,P                SET R2 = PRINT LINE                          
         B     PRNTX                                                            
*                                                                               
PRNT10   DS    0H                                                               
         LA    R2,132(R2)                                                       
*                                                                               
PRNTX    ST    R2,SAVER2           BUMP TO NEXT PRINT LINE                      
         MVI   SPACING,1           RESET SPACING                                
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
*        SIMPLE PRINT ROUTINE                                                   
*                                                                               
PRINTIT2 NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   SPACING,1                                                        
         B     XIT                                                              
         EJECT                                                                  
*              WORKER FILE I/O ROUTINES                                         
         SPACE 1                                                                
INDEXRD  LA    R3,WORKIO                                                        
         MVC   COMMAND2,=CL6'INDEX'                                             
         B     GO                                                               
*                                                                               
WKREAD   LA    R3,WORKIO                                                        
         MVC   COMMAND2,=CL6'READ'                                              
         B     GO                                                               
*                                                                               
CLOSE    LA    R3,RECORD                                                        
         MVC   COMMAND2,=CL6'CLOSE'                                             
         B     GO                                                               
*                                                                               
ONKEEP   LA    R3,WORKIO                                                        
         MVC   COMMAND2,=CL6'KEEP'                                              
         B     GO                                                               
*                                                                               
WKWRITE  LA    R3,WORKIO                                                        
         TM    PNOPTION,PNNOPOST                                                
         BOR   RE                                                               
         MVC   COMMAND2,=CL6'WRITE'                                             
         B     GO                                                               
*                                                                               
GO       LA    R2,INDEX                                                         
         L     R4,=A(WRKBUFF)                                                   
         B     GOFILE                                                           
*                                                                               
GOFILE   NTR1                                                                   
         GOTO1 DATAMGR,DMCB,COMMAND2,=C'WKFILE',(R2),(R3),(R4)                  
         CLI   DMCB+8,0                                                         
         B     XIT                                                              
*                                                                               
MYTRAC2  NTR1                                                                   
         TM    PNOPTION,PNTRACE    ELSE, PUT TO SORT                            
         BNO   MYTRAC2X                                                         
         LM    R2,R3,0(R1)         R2=A(LITERAL), R3=A(I/O AREA)                
         ZIC   R4,0(R1)            R4=L'LITERAL                                 
         GOTO1 TRACE,DMCB,(R3),0,(R2),(R4)                                      
MYTRAC2X B     XIT                                                              
         SPACE 1                                                                
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,27,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=336'                                   
         EJECT                                                                  
*                                                                               
*  CONSTANTS                                                                    
*                                                                               
EMULIST  DC    C'NACCDIR NACCMST NACCARC X'                                     
ACCLIST  DC    C'NACCFIL X'                                                     
*                                                                               
         SPACE 1                                                                
LTPAGE   DC    C'Page'                                                          
*                                                                               
LTAGENCY DC    C'Agency'                                                        
LTOFFICE DC    C'Off'                                                           
LTCLIENT DC    C'Cli'                                                           
LTCLNAME DC    C'  Client Name   '                                              
LTPROD   DC    C'Prd'                                                           
LTPRNAME DC    C'  Product Name  '                                              
LTJOB    DC    C'  Job  '                                                       
LTJBNAME DC    C'    Job Name    '                                              
LTINV    DC    C'  Inv  '                                                       
LTAGY    DC    C'Agency'                                                        
LTWRKCDE DC    C'WC'                                                            
LTWCNAME DC    C' Work Code Name '                                              
LTAMOUNT DC    C'   Amount   '                                                  
LTTTL2   DC    CL43'Items Must Be Entered Via The Input Program'                
*                                                                               
MISSNAME DC    CL36'*** MISSING ***'                                            
CNTNAME  DC    CL33'******** INTERNAL CONTROL *******'                          
CNTADDR  DC    CL33'******** DO NOT SEND OUT  *******'                          
*                                                                               
*              TABLE OF WORKER FILE ID NUMBERS NEVER PROCESSED                  
         SPACE                                                                  
EWIDTAB  DS    0CL2                                                             
         DC    X'0937'            TPNY                                          
         DC    X'077C'            TPLA                                          
         DC    X'0756'            TPCH                                          
         DC    X'076A'            TPNOSH                                        
         DC    X'08E4'            DPS2                                          
         DC    X'0BC0'            PPSCH                                         
         DC    X'0BCE'            PPSNY                                         
         DC    X'0BCD'            PPSLA                                         
         DC    X'FF'                                                            
         SPACE 2                                                                
SAVERC   DC    A(0)                                                             
         LTORG                                                                  
         SPACE 2                                                                
         EJECT                                                                  
*                                                                               
*              OTHER AREAS NOT DIRECTLY ADDRESSABLE                             
*        SPECS                                                                  
*                                                                               
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,99,REPORT                                                     
         SSPEC H2,99,REQUESTOR                                                  
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*        PRINT INVOICE THAT DID NOT INTERFACE                                   
         SPACE 1                                                                
         DS    0D                                                               
NOINT    NMOD1 0,*NOINT*                                                        
         L     RC,SAVERC                                                        
*                                                                               
         MVI   FORCEHED,C'Y'       SKIP TO NEXT PAGE                            
         MVI   NPOSTHK,C'Y'                                                     
         XR    R0,R0               NOTHING PRINTED                              
         ZAP   NINTAMT,=P'0'       CLEAR TOTAL                                  
         USING NPOSTD,R3                                                        
         L     R3,=A(NOTPOST)      A(TABLE)                                     
         LH    R4,=AL2(NOTPOSTX)                                                
         AR    R4,R3               A(END OF TABLE)                              
         LA    R2,P                                                             
         USING LLINE2,R2                                                        
*                                                                               
NO10     CR    R3,R4               IF WE REACHED THE END OF THE TABLE           
         BNL   NO30                                                             
         OC    0(NPOSTLN,R3),0(R3) OR NO MORE INPUT - EXIT                      
         BZ    NO30                                                             
*                                                                               
         CLI   RECNUM,PE           IF PROCESSING ERROR REPORT                   
         BNE   NO12                                                             
         LTR   R0,R0               IF NOT FIRST LINE                            
         BZ    NO11                                                             
         CLC   SVINV,NPINV         AND CHANGE IN INVOICE #                      
         BE    NO15                                                             
         GOTO1 SPOOL,DMCB,(R8)     SKIP A LINE                                  
NO11     MVC   SVINV,NPINV                                                      
         B     NO15                                                             
*                                                                               
NO12     CLC   PNAGY,NPPAGY        PRINT OUT ENTIRE AGENCY                      
         BNE   NO20                                                             
*                                                                               
NO15     LA    R0,1                SET PRINTED NOINT SUMMARY                    
         MVC   L2ACC(2),=C'SJ'                                                  
         TM    NPDET,NPDSJ                                                      
         BO    *+10                                                             
         MVC   L2ACC(2),=C'SV'                                                  
         MVC   L2ACC+2(L'NPACC),NPACC                                           
         MVC   L2WC,NPWC                                                        
         MVC   L2TAAGY,NPAGY                                                    
         MVC   L2INV,NPINV                                                      
         TM    NPDET,X'80'       IF AMOUNT IS A DEBIT                           
         BZ    NO16                                                             
         EDIT  NPAMT,(12,L2AMTDB),2,FLOAT=-                                     
         B     NO18                                                             
NO16     EDIT  NPAMT,(12,L2AMTCR),2,FLOAT=-                                     
         AP    NINTAMT,NPAMT       TOTAL FOR THIS AGENCY                        
*                                                                               
NO18     LA    R1,ERRTBL                                                        
NO19     CLI   0(R1),X'FF'                                                      
         BE    NO19X                                                            
         CLC   0(1,R1),NPSTAT                                                   
         BE    *+12                                                             
         LA    R1,L'ERRTBL(R1)                                                  
         B     NO19                                                             
*                                                                               
         MVC   L2STAT(17),1(R1)                                                 
         CLI   RECNUM,PE           IF TP ERROR REPORT                           
         BNE   NO19X                                                            
         TM    NPDET,NPDTPFIX      AND TP MUST FIX                              
         BZ    NO19X                                                            
         LA    RE,L2STAT+L'L2STAT                                               
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVC   1(5,RE),=CL5' - TP' GIVE THEM REMINDER                           
*                                                                               
NO19X    GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
NO20     LA    R3,NPOSTLN(R3)                                                   
         B     NO10                                                             
*                                                                               
NO30     LTR   R0,R0               IF  HEADINGS WERE PRINTED                    
         BZ    NOX                                                              
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   L2INV(5),=C'TOTAL'                                               
         EDIT  NINTAMT,(12,L2AMTDB),2,FLOAT=-                                   
         EDIT  NINTAMT,(12,L2AMTCR),2,FLOAT=-                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
NOX      MVI   NPOSTHK,C'N'                                                     
         XIT1                                                                   
*                                                                               
ERRTBL   DS    0CL18                                                            
         DC    AL1(ACTASCLI),CL17'CLIENT NOT FOUND'                             
         DC    AL1(ACTASPRD),CL17'PRODUCT NOT FOUND'                            
         DC    AL1(ACTASJOB),CL17'JOB NOT FOUND'                                
         DC    AL1(ACTASACC),CL17'INVALID ACCOUNT'                              
         DC    AL1(ACTASCLO),CL17'CLOSED JOB'                                   
         DC    AL1(ACTASLCK),CL17'LOCKED JOB'                                   
         DC    AL1(ACTASXJB),CL17'EXPENSE JOB'                                  
         DC    AL1(ACTASDRF),CL17'DRAFT JOB'                                    
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
* E-MAIL ERROR REPORT                                                           
*---------------------------------------------------------------------          
SENDMAIL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    PNOPTION,PNNOPOST   POSTING=NO?                                  
         BO    SENDMX                                                           
         CP    DBTAMNT,CRDAMNT     DEBITS NOT EQUAL CREDITS?                    
         BNE   SM05                                                             
*                                  AS PER TOM, MISSING POSTINGS ARE OK          
*                                  MANUAL ENTRY WILL BE DONE BY TP              
         B     SENDMX              NOTHING TO NOTIFY, LEAVE                     
*                                                                               
         USING UKRECD,RF                                                        
SM05     LA    RF,INDEX            NOTIFY TALENT PROGRAMMERS                    
         MVC   OOBUSID,TGUSERID                                                 
         MVC   OOBSYSP(4),UKSYSPRG                                              
         CLI   OOBSYSP+3,0                                                      
         BNE   *+8                                                              
         MVI   OOBSYSP+3,C'*'                                                   
*                                                                               
         MVC   FULL(1),UKDAY                                                    
         MVI   FULL+1,X'0C'                                                     
         UNPK  OOBDAYN(3),FULL(2)                                               
         MVC   OOBCLASS(1),UKCLASS                                              
         EDIT  (B2,UKFILNO),(4,OOBSEQN),0,LEFT,FILL=0                           
         EDIT  DBTAMNT,(10,BODYL5A1),2                                          
         EDIT  CRDAMNT,(10,BODYL5A2),2                                          
*                                                                               
         TM    WRKRERR,SMTPOPN                                                  
         BO    SM15                                                             
         GOTOR =V(SMTP),DMCB,('SMTPAINI',JESMAIL)                               
         OI    WRKRERR,SMTPOPN                                                  
*                                                                               
SM15     GOTOR =V(SMTP),DMCB,('SMTPAPRS',TOWHO),(L'SUBJDSC,SUBJDSC)             
*                                                                               
         MVC   BODYERR(L'OOBUSID),OOBUSID     MOVE IN ERROR MSG                 
         LA    RE,BODYERR                                                       
SM20     CLI   0(RE),C' '                     FOUND A SPACE                     
         BNH   SM30                                                             
         AHI   RE,1                                                             
         B     SM20                                                             
*                                                                               
SM30     MVI   0(RE),C','                                                       
         AHI   RE,1                                                             
         MVC   0(OOBLENQ,RE),OOBSYSP                                            
*                                                                               
         GOTOR (RF),DMCB,('SMTPAPTL',BODYLIN1)                                  
         GOTOR (RF),DMCB,('SMTPAPTL',BODYLIN2)                                  
*                                                                               
         CP    DBTAMNT,CRDAMNT     DEBITS NOT EQUAL CREDITS?                    
         BE    SM40                                                             
         GOTOR (RF),DMCB,('SMTPAPTL',BODYLIN3)                                  
         GOTOR (RF),DMCB,('SMTPAPTL',BODYLIN4)                                  
         GOTOR (RF),DMCB,('SMTPAPTL',BODYLIN5)                                  
         GOTOR (RF),DMCB,('SMTPAPTL',BODYLIN2)                                  
*                                                                               
SM40     TM    WRKRERR,ACCTERRS    ARE THERE ALSO ACCTING ERRORS?               
         BZ    SM90                YES, DON'T SEND NOTIFICATION YET             
         GOTOR (RF),DMCB,('SMTPAPTL',BODYLIN6)                                  
*                                                                               
         USING NPOSTD,R3                                                        
         L     R3,=A(NOTPOST)      A(TABLE)                                     
         LH    R4,=AL2(NOTPOSTX)                                                
         AR    R4,R3               A(END OF TABLE)                              
*                                                                               
SM50     CR    R3,R4               IF WE REACHED THE END OF THE TABLE           
         BNL   SM90                                                             
         OC    0(NPOSTLN,R3),0(R3) OR NO MORE INPUT - SEND EMAIL                
         BZ    SM90                                                             
         MVC   BODYLIN7,BODYLIN2                                                
         LA    R2,BODYLIN7                                                      
         USING LLINE2,R2                                                        
         MVC   L2ACC(2),=C'SJ'                                                  
         TM    NPDET,NPDSJ                                                      
         BO    *+10                                                             
         MVC   L2ACC(2),=C'SV'                                                  
         MVC   L2ACC+2(L'NPACC),NPACC                                           
         MVC   L2WC,NPWC                                                        
         MVC   L2TAAGY,NPAGY                                                    
         MVC   L2INV,NPINV                                                      
         TM    NPDET,X'80'       IF AMOUNT IS A DEBIT                           
         BZ    SM56                                                             
         EDIT  NPAMT,(12,L2AMTDB),2,FLOAT=-                                     
         B     SM58                                                             
SM56     EDIT  NPAMT,(12,L2AMTCR),2,FLOAT=-                                     
*                                                                               
SM58     L     R1,=A(ERRTBL)                                                    
SM59     CLI   0(R1),X'FF'                                                      
         BE    SM60                                                             
         CLC   0(1,R1),NPSTAT                                                   
         BE    *+12                                                             
         LA    R1,L'ERRTBL(R1)                                                  
         B     SM59                                                             
*                                                                               
         MVC   L2STAT(17),1(R1)                                                 
SM60     GOTOR (RF),DMCB,('SMTPAPTL',BODYLIN7)                                  
*                                                                               
SM80     LA    R3,NPOSTLN(R3)                                                   
         B     SM50                                                             
*                                                                               
SM90     GOTOR (RF),DMCB,('SMTPASND',0)                                         
         NI    WRKRERR,X'FF'-ACCTERRS                                           
         OI    WRKRERR,EMAILNTF                                                 
*                                                                               
SENDMX   XIT1                                                                   
*-------------------------------------------------------------------            
OOBDEF   DC    0CL40                                                            
OOBUSID  DC    CL6'      '                                                      
OOBSYSP  DC    C'TNN*'                                                          
OOBDAYN  DC    C'DD'                                                            
OOBCLASS DC    C'C,'                                                            
OOBSEQN  DC    C'SSSS'                                                          
OOBLENQ  EQU   *-OOBSYSP                                                        
         DC    CL(L'OOBDEF-(*-OOBDEF))' '     SPARE SPACES                      
*                                                                               
JESMAIL  DC    CL8'JESMAIL '                                                    
*                                                                               
TOWHO    DC    C'US-TALENT_TEAM:'                                               
*                                                                               
SUBJDSC  DC    0CL80                                                            
         DC    C'** Worker File Error / Out-Of-Balance'                         
         DC    CL(L'SUBJDSC-(*-SUBJDSC))' '     SPARE SPACES                    
BODYLIN1 DC    0CL80                                                            
BODYERR  DC    C'                  '            ERROR MESSAGE                   
         DC    CL(L'BODYLIN1-(*-BODYLIN1))' '   SPARE SPACES                    
*                                                                               
BODYLIN2 DC    CL80' '                                                          
BODYLIN3 DC    CL80'Debits not equal to Credits'                                
BODYLIN4 DC    CL80'==========================='                                
BODYLIN5 DC    0CL80                                                            
BODYL5A1 DC    CL10'9999999.99'                                                 
         DC    C' vs '                                                          
BODYL5A2 DC    CL10'9999999.99'                                                 
         DC    CL(L'BODYLIN5-(*-BODYLIN5))' '   SPARE SPACES                    
BODYLIN6 DC    CL80'** Account postings have errors.'                           
BODYLIN7 DC    CL80' '                                                          
BODYLIN8 DC    CL80' '                                                          
         LTORG                                                                  
         DROP  R3,RF                                                            
         EJECT                                                                  
*        SET INFO FOR TRACE ROUTINE                                             
*                                                                               
         DS    0D                                                               
MYTRACE  NMOD1 0,MYTRACE                                                        
         L     RC,SAVERC                                                        
*                                                                               
         L     R2,0(R1)            A(LITERAL)                                   
         L     R5,8(R1)            SET LENGTH OF RECORD                         
         SH    R5,=H'4'                - HEADER                                 
         L     R4,4(R1)            A(RECORD)                                    
         GOTO1 PRNTBL,DMCB,(R2),(R4),C'DUMP',(R5),=C'2D'                        
         XIT1                                                                   
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              HEADLINE/BOX ROUTINES (HEADHOOK)                                 
*                                                                               
         DS    0D                                                               
HDHOOK   NMOD1 0,**HOOK*                                                        
         L     RC,SAVERC                                                        
*                                                                               
         ZIC   R1,PGNUM            INCREMENT PAGE NUMBER                        
         LA    R1,1(R1)                                                         
         STC   R1,PGNUM                                                         
         LA    R2,HEAD1                                                         
         MVC   116(4,R2),LTPAGE                                                 
         EDIT  PGNUM,(2,121(R2))                                                
*                                                                               
         CLI   RECNUM,PE                                                        
         BE    HOOK10                                                           
         MVC   HEAD1+50(20),=CL20'Production Interface'                         
         MVC   HEAD2+50(20),=20X'BF'                                            
         B     HOOK20                                                           
*                                                                               
HOOK10   MVC   HEAD1+46(27),=CL27'Production Interface Errors'                  
         MVC   HEAD2+46(27),=27X'BF'                                            
         SPACE 1                                                                
HOOK20   L     R3,ABOX             NOW HANDLE BOXES                             
         USING BOXD,R3                                                          
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,SPACES                                                   
*                                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
*                                                                               
         CLI   CNTPAGE,C'Y'                                                     
         BE    HOOKX                                                            
         LA    R2,HEAD4                                                         
         MVC   1(6,R2),LTAGENCY                                                 
         MVC   9(6,R2),PNAGY                                                    
         MVC   17(36,R2),AGYNAME   AGENCY NAME                                  
*                                                                               
         CLI   NPOSTHK,C'Y'                                                     
         BNE   *+12                                                             
         BAS   RE,NOPOSTHK                                                      
         B     HOOKX                                                            
*                                                                               
         BAS   RE,BXCOL                                                         
         LA    R2,6                                                             
         LA    R2,BOXROWS-1(R2)    SET TOP OF BOX                               
         MVI   0(R2),C'T'                                                       
         MVI   66(R2),C'B'         SET BOTTOM ALWAYS PAST END OF PAGE           
         LA    R2,2(R2)                                                         
         MVI   0(R2),C'M'          SET MIDDLE OF BOX                            
*                                                                               
         LA    R2,HEAD7                                                         
         USING LLINE,R2                                                         
         MVC   LOFFICE(3),LTOFFICE    PUT OUT HEADLINES                         
         MVC   LCLIENT,LTCLIENT                                                 
         MVC   LCLNAME,LTCLNAME                                                 
         MVC   LPROD,LTPROD                                                     
         MVC   LPRNAME,LTPRNAME                                                 
         MVC   LJOB,LTJOB                                                       
         MVC   LJBNAME,LTJBNAME                                                 
         MVC   LAGY,LTAGY                                                       
         MVC   LINV,LTINV                                                       
         MVC   LWRKCDE,LTWRKCDE                                                 
         MVC   LWCNAME,LTWCNAME                                                 
         MVC   LAMOUNT,LTAMOUNT                                                 
*                                                                               
HOOKX    XIT1                                                                   
         DROP  R2,R3                                                            
         EJECT                                                                  
*        PRINT HEADINGS FOR NO POST SUMMARY                                     
*                                                                               
NOPOSTHK NTR1                                                                   
         MVC   HEAD6+45(31),=C'Invoices That Did Not Interface'                 
         MVC   HEAD7+45(31),=31X'BF'                                            
         MVC   HEAD8+39(L'LTTTL2),LTTTL2                                        
*                                                                               
         LA    R2,HEAD10                                                        
         USING LLINE2,R2                                                        
         MVC   L2ACC(7),=CL14'Account'                                          
         MVC   L2WC,=C'WC'                                                      
         MVC   L2TAAGY,=C'Agency'                                               
         MVC   L2INV(7),=C'Invoice'                                             
         MVC   L2AMTDB+6(6),=C' Debit'                                          
         MVC   L2AMTCR+6(6),=C'Credit'                                          
         MVC   L2STAT(5),=C'Error'                                              
*                                                                               
         LA    R2,HEAD11                                                        
         MVC   L2ACC(7),=31X'BF'                                                
         MVC   L2WC,=31X'BF'                                                    
         MVC   L2TAAGY,=31X'BF'                                                 
         MVC   L2INV(7),=31X'BF'                                                
         MVC   L2AMTDB+6(6),=31X'BF'                                            
         MVC   L2AMTCR+6(6),=31X'BF'                                            
         MVC   L2STAT(6),=31X'BF'                                               
         B     HOOKX                                                            
         EJECT                                                                  
*              ROUTINE TO SET COLUMNS                                           
*                                                                               
BXCOL    NTR1                                                                   
         L     R3,ABOX                                                          
         USING BOXD,R3                                                          
         LA    R2,BOXCOLS                                                       
         USING LLINE,R2                                                         
         MVI   LLHS,C'L'           LHS                                          
         MVI   LCOL1,C'C'          COLUMNS                                      
         MVI   LCOL2,C'C'                                                       
         MVI   LCOL3,C'C'                                                       
         MVI   LCOL4,C'C'                                                       
         MVI   LCOL5,C'C'                                                       
         MVI   LCOL6,C'C'                                                       
         MVI   LCOL7,C'C'                                                       
         MVI   LCOL8,C'C'                                                       
         MVI   LCOL9,C'C'                                                       
         MVI   LCOL10,C'C'                                                      
         MVI   LCOL11,C'C'                                                      
         MVI   LRHS,C'R'           RHS                                          
         B     HOOKX                                                            
         DROP  R2,R3                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              ASSORTED WORK AREAS                                              
*                                                                               
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'**MYIO**'                                                      
MYIO     DS    1004C                                                            
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'**WRK1**'                                                      
WRKBUFF  DS    5000C                                                            
         SPACE 2                                                                
*                                                                               
         DS    0D                                                               
NOTPOST  DS    400CL(NPOSTLN)                                                   
NOTPOSTX EQU   *-NOTPOST                                                        
         EJECT                                                                  
*---------------------------------------------------------------                
         DS    0D                                                               
         DC    CL8'**SSB **'                                                    
SSB      CSECT                                                                  
         DC    X'0000FF',X'00',XL252'00'                                        
*---------------------------------------------------------------                
         EJECT                                                                  
       ++INCLUDE TAPIND                                                         
         EJECT                                                                  
SRTKEY   DSECT                                                                  
PSRTAGY  DS    CL6                                                              
PSRTOFF  DS    CL2                                                              
PSRTCLI  DS    CL3                 SORT KEY                                     
PSRTPRD  DS    CL3                                                              
PSRTJOB  DS    CL6                                                              
PSRTWC   DS    CL2                                                              
PSRTINV  DS    CL6                 INVOICE NUMBER                               
PSRTREC  EQU   *-SRTKEY                                                         
*                                                                               
PAGY     DS    CL6                 TALENT AGENCY                                
POFFICE  DS    CL2                 OFFICE CODE                                  
PCLIENT  DS    CL3                 CLIENT CODE                                  
PPROD    DS    CL3                 PRODUCT CODE                                 
PJOB     DS    CL6                 JOB CODE                                     
PWRKCDE  DS    CL2                 WORK CODE                                    
PWCNAME  DS    CL16                WORK CODE NAME                               
PINV     DS    CL6                 INVOICE NUMBER                               
PSTAT    DS    XL1                 STATUS BYTE                                  
PAMOUNT  DS    CL12                AMOUNT                                       
PJOBSW   DS    CL1                 SWITCH - THIS JOB USES DEFAULT               
PPACAMT  DS    PL6                 SAVE PACKED AMOUNT                           
PSRTAYNM DS    CL36                AGENCY NAME                                  
PSRTCLNM DS    CL36                CLIENT NAME                                  
PSRTPRNM DS    CL36                PRODUCT NAME                                 
PSRTJBNM DS    CL36                JOB NAME                                     
PSAYADDR DS    0CL99               AGENCY ADDRESS                               
PSAYADD1 DS    CL33                                                             
PSAYADD2 DS    CL33                                                             
PSAYADD3 DS    CL33                                                             
PLNQ     EQU   *-SRTKEY                                                         
         EJECT                                                                  
LLINE    DSECT                                                                  
LLHS     DS    CL1                 LHS COLUMN                                   
LOFFICE  DS    CL2                 OFFICE CODE                                  
         DS    CL1                                                              
LCOL1    DS    CL1                                                              
LCLIENT  DS    CL3                 CLIENT CODE                                  
LCOL2    DS    CL1                                                              
LCLNAME  DS    CL16                CLIENT NAME                                  
LCOL3    DS    CL1                                                              
LPROD    DS    CL3                 PRODUCT CODE                                 
LCOL4    DS    CL1                                                              
LPRNAME  DS    CL16                PRODUCT NAME                                 
LCOL5    DS    CL1                                                              
LJOB     DS    CL6                 JOB CODE                                     
LCOL6    DS    CL1                                                              
LJBNAME  DS    CL16                JOB NAME                                     
LCOL7    DS    CL1                                                              
LWRKCDE  DS    CL2                 WORK CODE                                    
LCOL8    DS    CL1                                                              
LWCNAME  DS    CL16                WORK CODE NAME                               
LCOL9    DS    CL1                                                              
LAGY     DS    CL6                 TALENT AGENCY                                
LCOL10   DS    CL1                                                              
LINV     DS    CL6                 INVOICE NUMBER                               
LCOL11   DS    CL1                                                              
         DS    CL1                                                              
LAMOUNT  DS    CL12                AMOUNT                                       
         DS    CL1                                                              
LRHS     DS    CL1                                                              
LLNQ     EQU   *-LLINE                                                          
         SPACE 2                                                                
*                                                                               
LLINE2   DSECT                                                                  
L2ACC    DS    CL14                ACCOUNT                                      
         DS    CL2                                                              
L2WC     DS    CL2                 WORK CODE                                    
         DS    CL2                                                              
L2TAAGY  DS    CL6                 TALENT AGY                                   
         DS    CL2                                                              
L2INV    DS    CL6                 INVOICE                                      
         DS    CL2                                                              
L2AMTDB  DS    CL12                AMOUNT                                       
         DS    CL2                                                              
L2AMTCR  DS    CL12                AMOUNT                                       
         DS    CL2                                                              
L2STAT   DS    CL22                STATUS                                       
         EJECT                                                                  
*TAREPFFD                                                                       
*TAREPF0D                                                                       
*DDGENTWA                                                                       
*DDTWACOND                                                                      
*DMDTFIS                                                                        
*DDMASTD                                                                        
*DMWRKRK                                                                        
*DDSPOOLD                                                                       
*DDLOGOD                                                                        
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*CTGENFILE                                                                      
*ACGENBOTH                                                                      
*ACGENPOST                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*TAREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAREPFFD                                                       
       ++INCLUDE TAREPF0D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE DMDTFIS                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DMWRKRK                                                        
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSMTPD                                                        
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'059TAREP1F   07/17/14'                                      
         END                                                                    
