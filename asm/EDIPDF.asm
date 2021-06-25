*          DATA SET EDIPDF     AT LEVEL 003 AS OF 10/26/20                      
***********************************************************************         
* Process to convert a MQ message sent from PQSCAN and ship it to a   *         
* PDF converter on the MQ Broker.                                     *         
* It send an formated message to the PDF converter wrapped in XML.    *         
* It also sends a message to EDICT subtask EDINJE via the LOG queue,  *         
* which I call the STATUS queue. The message this sends is the one    *         
* that will add a record to EDICT file.                               *         
***********************************************************************         
*PHASE EDIPDFA                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE MODLINK                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SCANNER                                                                
*                                                                               
EDIPDF   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,EDIPDF,WORK=VWRKAREA                                           
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         L     RA,ACOMMON                                                       
         LHI   R9,4*K                                                           
         AR    R9,RA                                                            
         USING COMMON,RA,R9                                                     
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
         ST    RD,SAVERD           SET EXIT FROM CHAIN RD                       
*                                                                               
         L     RC,AWORKD           GET SOME W/S FOR THIS MODULE                 
         USING WORKD,RC                                                         
         USING DESTTABD,DESTNTRY                                                
         BRAS  RE,INIT             INITIALISE IT ALL                            
         BNE   MAINX                                                            
*                                                                               
MAIN02   BRAS  RE,WAIT             ISSUE 'GET' AND WAIT FOR A MESSAGE           
         BNE   MAINX               OPERATOR REQUESTED CANCEL JOB                
*                                                                               
         OC    DATALEN,DATALEN     EMPTY MESSAGE?                               
         BZ    MAIN02              YES - SKIP THIS ONE, GET NEXT ONE            
         BRAS  RE,SENDPDF          SEND MQ OUT TO CONVERT TO PDF                
         B     MAIN02              AND BACK FOR NEXT                            
*                                                                               
MAINX    BRAS  RE,MQCLS            FREE ANY MQ CONNECTIONS THAT EXIST           
         B     XBASE                                                            
*                                                                               
VWRKAREA DC    V(WORKAREA)                                                      
ACOMMON  DC    A(COMMON)                                                        
AWORKD   DC    A(WORKD)                                                         
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
INIT     NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,1                                                             
         BRAS  RE,SYSMESS          STARTING INITIALISE                          
*                                                                               
         LR    R0,RC                                                            
         LHI   R1,WORKL                                                         
         XR    RF,RF                                                            
         MVCL  R0,RE               CLEAR NBASE W/S                              
*                                                                               
         ZAP   LINE,P99                                                         
         MVC   TITLE,VCTITLE                                                    
*                                                                               
         LA    R2,WORK             EXTRACT MVS JOBNAME                          
         EXTRACT (2),FIELDS=TIOT                                                
         L     R2,WORK                                                          
         MVC   MVSNAME,0(R2)                                                    
*                                                                               
         BRAS  RE,GETCARDS         READ IN INPUT CARDS                          
         BL    EXITL                                                            
         BRAS  RE,VALCARDS         VALIDATE INPUT CARDS                         
         BL    EXITL                                                            
         BRAS  RE,OPENCTFL         OPEN CTFILE IF NOT DONE YET                  
*                                                                               
         ZAP   LINE,P99                                                         
         MVC   TITLE,DWTITLE                                                    
         L     RF,=A(LINUKUS)      Translate table                              
         MVC   CHARTAB,0(RF)                                                    
         MVI   CHARTAB,C' '        Make Null to space                           
*                                                                               
         BRAS  RE,MQINIT                                                        
         BL    EXITL                                                            
         BRAS  RE,SETOPS           SET UP OPERATOR INPUT                        
         BL    EXITL                                                            
*                                                                               
         L     R0,MAXMSGLN         TWO BUFFERS REQUIRED INPUT + OUTPUT          
         SLL   R0,1                                                             
*                                                                               
         GETMAIN RU,LV=(0),LOC=(ANY,ANY),BNDRY=PAGE                             
         LTR   RF,RF                                                            
         BZ    INIT02                                                           
         LHI   R0,21                                                            
         BRAS  RE,SYSMESS          GETMAIN FAILED                               
         ABEND 911,DUMP                                                         
*                                                                               
INIT02   ST    R1,AMSGIN           A(INPUT BUFFER)                              
         A     R1,MAXMSGLN                                                      
         ST    R1,AMSGOUT          A(OUTPUT BUFFER)                             
*                                                                               
         LHI   R0,2                                                             
         BRAS  RE,SYSMESS          COMPLETED INITIALISE                         
         B     EXITOK                                                           
*                                                                               
VCTITLE  DC    CL(L'TITLE)'Input cards to MQ EDI PDF receive job'               
DWTITLE  DC    CL(L'TITLE)'MQ EDI PDF send output log'                          
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALISE MQ QUEUES                                                *         
* NOTE: IN A DELIBERATE ATTEMPT TO SIMPLIFY THE NAMES OF THE MQ       *         
*       QUEUE MANAGER, THE MASTER QUEUE AND LOG QUEUES ARE ALL HARD   *         
***********************************************************************         
MQINIT   NTR1  BASE=*,LABEL=*                                                   
         CLI   MQINIFLG,YES        MUST BE A RE-CONNECT                         
         JE    MQINI99                                                          
*                                                                               
         MVI   CONOK,NO                                                         
         LHI   R0,9                                                             
         BRAS  RE,SYSMESS          BEGINNING MQ INITIALISE                      
*                                                                               
*        MVC   MQLBLPDF+14(1),DSPACE                                            
         MVC   INPQ_OBJECTNAME,QINPUTA                                          
         MVC   OUTQ_OBJECTNAME,QOUTPUTA                                         
         MVC   LOGQ_OBJECTNAME,QLOGA                                            
         CLI   DSPACE,C'A'         ADV QUEUES                                   
         BE    MQINI50                                                          
*                                                                               
         CLI   DSPACE,C'R'         REP QUEUES                                   
         BNE   MQINI30                                                          
         MVC   INPQ_OBJECTNAME,QINPUTR                                          
         MVC   OUTQ_OBJECTNAME,QOUTPUTR                                         
         MVC   LOGQ_OBJECTNAME,QLOGR                                            
         B     MQINI50                                                          
*                                                                               
MQINI30  CLI   DSPACE,C'Q'         FQA QUEUES                                   
         BNE   MQINI35                                                          
         MVC   INPQ_OBJECTNAME,QINPUTQ                                          
         MVC   OUTQ_OBJECTNAME,QOUTPUTQ                                         
         MVC   LOGQ_OBJECTNAME,QLOGQ                                            
         MVC   QMGR,QMGRQ                                                       
         B     MQINI50                                                          
*                                                                               
MQINI35  CLI   DSPACE,C'C'         CSC QUEUES                                   
         BNE   MQINI40                                                          
         MVC   INPQ_OBJECTNAME,QINPUTC                                          
         MVC   OUTQ_OBJECTNAME,QOUTPUTC                                         
         MVC   LOGQ_OBJECTNAME,QLOGC                                            
         MVC   QMGR,QMGRC                                                       
         B     MQINI50                                                          
*                                                                               
MQINI40  CLI   DSPACE,C'T'         TEST QUEUES                                  
         JNE   *+2                                                              
         MVC   INPQ_OBJECTNAME,QINPUTT                                          
         MVC   OUTQ_OBJECTNAME,QOUTPUTT                                         
         MVC   LOGQ_OBJECTNAME,QLOGT                                            
         MVC   QMGR,QMGRT                                                       
         B     MQINI50                                                          
*                                                                               
MQINI50  MVC   PLINE(20),=CL20'Queue Manager:'                                  
         MVC   PLINE+21(L'QMGR),QMGR                                            
         BRAS  RE,PRNT                                                          
         MVC   PLINE(20),=CL20'  Input Queue:'                                  
         MVC   PLINE+21(L'INPQ_OBJECTNAME),INPQ_OBJECTNAME                      
         BRAS  RE,PRNT                                                          
         MVC   PLINE(20),=CL20' Output Queue:'                                  
         MVC   PLINE+21(L'OUTQ_OBJECTNAME),OUTQ_OBJECTNAME                      
         BRAS  RE,PRNT                                                          
         MVC   PLINE(20),=CL20'Logging Queue:'                                  
         MVC   PLINE+21(L'LOGQ_OBJECTNAME),LOGQ_OBJECTNAME                      
         BRAS  RE,PRNT                                                          
*                                                                               
         BLDL  0,ENTRYPTS          BUILD LIST OF EXTERNAL ENTRY PTS             
         LTR   RF,RF                                                            
         JNZ   *+2                 BAD RETURN FROM BLDL MACRO                   
*                                                                               
         LOAD  DE=CSQBCONN                                                      
         ST    R0,AMQCONN                                                       
         LOAD  DE=CSQBGET                                                       
         ST    R0,AMQGET                                                        
         LOAD  DE=CSQBOPEN                                                      
         ST    R0,AMQOPEN                                                       
         LOAD  DE=CSQBCOMM                                                      
         ST    R0,AMQCMIT                                                       
         LOAD  DE=CSQBCLOS                                                      
         ST    R0,AMQCLOSE                                                      
         LOAD  DE=CSQBDISC                                                      
         ST    R0,AMQDISC                                                       
         LOAD  DE=CSQBPUT                                                       
         ST    R0,AMQPUT                                                        
*                                                                               
MQINI99  MVI   MQINIFLG,YES        Set INIT FLAG to Y - Reconnect here          
         LA    R2,MQCONN                                                        
         BRAS  RE,CALLMQ           CONNECT TO MQ                                
         JNE   *+2                                                              
*                                                                               
         LHI   R0,11                                                            
         BRAS  RE,SYSMESS          CONNECTED TO MQ QMGR                         
*                                                                               
         LA    RF,INPQ             SET QUEUE OBJECT FOR INPUT QUEUE             
         ST    RF,MQOPNQNM                                                      
         MVC   MQOPNHOB,=A(INPQHOB) SET A(RETURN FOR HOBJ)                      
         LHI   RF,MQOO_INPUT_AS_Q_DEF+MQOO_SAVE_ALL_CONTEXT                     
         ST    RF,OPN_OPTS                                                      
         LA    R2,MQOPEN                                                        
         BRAS  RE,CALLMQ           OPEN MASTER QUEUE                            
         JNE   *+2                                                              
*                                                                               
         LA    RF,OUTQ             SET QUEUE OBJECT FOR OUTPUT QUEUE            
         ST    RF,MQOPNQNM                                                      
         MVC   MQOPNHOB,=A(OUTQHOB) SET A(RETURN FOR HOBJ)                      
         L     RF,=AL4(MQOO_OUTPUT+MQOO_SET_IDENTITY_CONTEXT)                   
         ST    RF,OPN_OPTS                                                      
         LA    R2,MQOPEN                                                        
         BRAS  RE,CALLMQ           OPEN OUTPUT QUEUE                            
         JNE   *+2                                                              
*                                                                               
         LA    RF,LOGQ             SET QUEUE OBJECT FOR LOG QUEUE               
         ST    RF,MQOPNQNM                                                      
         MVC   MQOPNHOB,=A(LOGQHOB) SET A(RETURN FOR HOBJ)                      
         LHI   RF,MQOO_OUTPUT+MQOO_PASS_ALL_CONTEXT                             
         ST    RF,OPN_OPTS                                                      
         LA    R2,MQOPEN                                                        
         BRAS  RE,CALLMQ           OPEN OUTPUT QUEUE                            
         JNE   *+2                                                              
*                                                                               
         LHI   R0,12                                                            
         BRAS  RE,SYSMESS          OPENED MASTER QUEUE                          
*                                                                               
         LHI   R0,10                                                            
         BRAS  RE,SYSMESS          COMPLETED MQ INITIALISE                      
         MVI   CONOK,YES                                                        
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* QUEUE INFORMATION                                                   *         
***********************************************************************         
QMGRT    DC    CL48'MQ7T'                                                       
QMGRQ    DC    CL48'MQ7Q'                                                       
QMGRC    DC    CL48'MQ7C'                                                       
*                                                                               
QINPUTA  DC    CL48'DDS.EDIPDF.QUEUE'                                           
QINPUTR  DC    CL48'DDS.EDIPDF.QUEUE'                                           
QINPUTT  DC    CL48'DDS.EDIPDF.QUEUE'                                           
QINPUTQ  DC    CL48'DDS.EDIPDF.QUEUE'                                           
QINPUTC  DC    CL48'DDS.EDIPDF.QUEUE'                                           
*                                                                               
QOUTPUTA DC    CL48'DDS.PDFSND.ADV.QUEUE'                                       
QOUTPUTR DC    CL48'DDS.PDFSND.REP.QUEUE'                                       
QOUTPUTQ DC    CL48'DDS.PDFSND.FQA.QUEUE'                                       
QOUTPUTT DC    CL48'DDS.PDFSND.TST.QUEUE'                                       
QOUTPUTC DC    CL48'DDS.PDFSND.CSC.QUEUE'                                       
*OUTPUTT DC    CL48'DDS.AHYD.TEST.LOCALQ'                                       
*                                                                               
QLOGA    DC    CL48'DDS.EDICT.STATUS.QUEUE'                                     
QLOGR    DC    CL48'DDS.EDICT.STATUS.QUEUE'                                     
QLOGT    DC    CL48'DDS.EDICT.STATUS.QUEUE'                                     
QLOGQ    DC    CL48'DDS.EDICT.STATUS.QUEUE'                                     
QLOGC    DC    CL48'DDS.EDICT.STATUS.QUEUE'                                     
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO WAIT UNTIL POSTED OR A MESSAGE ARRIVES                   *         
***********************************************************************         
WAIT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
WAIT02   XC    TIMERECB,TIMERECB   CLEAR TIMER ECB                              
         XC    GETECB,GETECB       CLEAR SIGNAL ECB                             
         XC    MSGDESC_MSGID,MSGDESC_MSGID                                      
         XC    MSGDESC_CORRELID,MSGDESC_CORRELID                                
*                                                                               
         LHI   RF,MQGMO_SET_SIGNAL+MQGMO_ACCEPT_TRUNCATED_MSG                   
         ST    RF,GETOPTS_OPTIONS                                               
         LA    RF,GETECB           TELL IT TO SIGNAL AND SET ECB                
         ST    RF,GETOPTS_SIGNAL1                                               
         LHI   RF,MQWI_UNLIMITED                                                
         ST    RF,GETOPTS_WAITINTERVAL                                          
*                                                                               
         MVC   MQGETBUF,AMSGIN     MESSAGE                                      
*                                                                               
         LA    R2,MQGET                                                         
         BRAS  RE,CALLMQ           CALL MQ TO FLAG WHEN MESSAGE ARRIVES         
         BE    EXITOK              YOU DON'T NEED TO WAIT                       
         BH    WAIT04              SIGNAL ACCEPTED FOR MQGET, WAIT              
*                                  ERROR ON GET,                                
         CLC   MQ_RC,=A(MQRC_SIGNAL_OUTSTANDING)                                
         JE    WAIT04              ALREADY SET UP TO SIGNAL                     
*                                  ERROR ON GET,                                
         CLC   MQ_RC,=A(MQRC_CONNECTION_BROKEN)                                 
         JNE   WAIT03              DIE=BROKEN CONNECTION, RECYCLE               
*                                  OTHERWISE,                                   
         BRAS  RE,RECON            TRY RECONNECT                                
*                                  OTHERWISE,                                   
WAIT03   BRAS  RE,SETTIMER               SET TIMER, RETRY LATER                 
*                                                                               
WAIT04   WAIT  1,ECBLIST=ECBLST                                                 
*                                                                               
         TM    GETECB,X'40'        MQ SIGNALS MESSAGE ARRIVED                   
         BO    WAIT02              YES - GO GET IT                              
*                                                                               
         L     RF,AOPERECB                                                      
         TM    0(RF),X'40'         OPERATOR INTERRUPT                           
         BZ    *+8                                                              
         BRAS  RE,CHKOPER                                                       
         CLI   OPERSTOP,YES        OPERATOR STOP REQUESTED?                     
         BE    EXITL               YES                                          
*                                                                               
         TM    TIMERECB,X'40'                                                   
         BO    WAIT02              TIMER POPPED, TRY GET AGAIN                  
*                                                                               
         OC    STIMER1,STIMER1                                                  
         BZ    WAIT04                                                           
         STIMERM CANCEL,ID=STIMER1                                              
         LTR   RF,RF               CANCEL TIMER                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     WAIT04              UNKNOWN COMMAND, GO BACK TO WAIT             
         EJECT                                                                  
***********************************************************************         
*TRY TO RECONNECT TO QUEUE MANAGER                                    *         
***********************************************************************         
RECON    NTR1  ,                                                                
*                                                                               
         MVC   PLINE(16),=CL16'Connectionbroken'                                
         BRAS  RE,PRNT                                                          
         XR    R3,R3                                                            
*                                                                               
         LA    R2,MQDISC                                                        
         BRAS  RE,CALLMQ           DISCONNECT MQ QMGR                           
*                                                                               
RECON1   STIMERM SET,ID=STIMER2,BINTVL=WAITR,WAIT=YES                           
         LTR   RF,RF               WAIT 30 SEC                                  
         JNZ   *+2                                                              
*                                                                               
         LA    R3,1(R3)                                                         
         CHI   R3,10                                                            
         JNL   *+2                 TOO MANY RETRIES                             
*                                                                               
         MVC   PLINE(16),=CL16'Reconnect       '                                
         EDIT  (R3),(5,PLINE+10),ALIGN=LEFT                                     
         BRAS  RE,PRNT                                                          
*                                                                               
         BRAS  RE,MQINIT                                                        
         JL    RECON1                                                           
*                                                                               
RECONX   XIT1                                                                   
*                                                                               
WAITR    DC    A(30*100)           WAIT 30 SEC                                  
STIMER2  DS    XL4                 FOR TIMER POPS                               
                                                                                
         LTORG                                                                  
***********************************************************************         
*SET TIMER                                                            *         
***********************************************************************         
SETTIMER NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   PLINE(16),=CL16'Set Timer'                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         STIMERM SET,ID=STIMER1,BINTVL=WAITSECS,EXIT=TIMERXIT                   
         LTR   RF,RF               WAIT 30 SEC                                  
         BZ    *+6                                                              
         DC    H'0'                                                             
STIMERX  XIT1                                                                   
*                                                                               
WAITSECS DC    A(30*100)            WAIT 30 SEC                                 
STIMER1  DS    XL4                 FOR TIMER POPS                               
*                                                                               
TIMERXIT SAVE  (14,12),,*                                                       
         LR    RB,RF                                                            
         USING TIMERXIT,RB                                                      
         POST  TIMERECB                                                         
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
TIMERECB DS    F                                                                
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISCONNECT FROM MQ CLEANLY                               *         
***********************************************************************         
MQCLS    NTR1  BASE=*,LABEL=*                                                   
         CLI   CONOK,YES                                                        
         BNE   EXITOK                                                           
*                                                                               
         LHI   R0,14                                                            
         BRAS  RE,SYSMESS          CLOSING QUEUES                               
*                                                                               
         XR    RF,RF                                                            
         ST    RF,CLS_OPTS                                                      
         MVC   MQCLSHOB,=A(INPQHOB)                                             
         LA    R2,MQCLOSE                                                       
         BRAS  RE,CALLMQ           CLOSE INPUT QUEUE                            
         JNE   *+2                                                              
*                                                                               
         XR    RF,RF                                                            
         ST    RF,CLS_OPTS                                                      
         MVC   MQCLSHOB,=A(OUTQHOB)                                             
         LA    R2,MQCLOSE                                                       
         BRAS  RE,CALLMQ           CLOSE OUTPUT QUEUE                           
         JNE   *+2                                                              
*                                                                               
         XR    RF,RF                                                            
         ST    RF,CLS_OPTS                                                      
         MVC   MQCLSHOB,=A(LOGQHOB)                                             
         LA    R2,MQCLOSE                                                       
         BRAS  RE,CALLMQ           CLOSE LOG QUEUE                              
         JNE   *+2                                                              
*                                                                               
         LHI   R0,16                                                            
         BRAS  RE,SYSMESS          CLOSED MQ MASTER QUEUE                       
         LA    R2,MQDISC                                                        
         BRAS  RE,CALLMQ           DISCONNECT MQ QMGR                           
         JNE   *+2                                                              
*                                                                               
         LHI   R0,15                                                            
         BRAS  RE,SYSMESS          COMPLETED MQ DEALLOCATION                    
         MVI   CONOK,NO                                                         
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SEND MESSAGE OUT TO BE CONVERTED TO PDF                             *         
***********************************************************************         
SENDPDF  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,CLRFLDS          CLEAN UP BEFORE PROCESSING THIS MSG          
         SAM31                                                                  
*                                                                               
         L     R7,ADSTAEFT         R7 = CURRENT DESTINATION TABLE ENTRY         
         USING DSTAEFTD,R7                                                      
*                                                                               
         L     R4,AMSGIN           INPUT MESSAGE BUFFER                         
         USING MSGHDRD,R4                                                       
*                                                                               
         PACK  DUB,MSGHLEN         Convert header length into binary            
         CVB   R1,DUB                                                           
         STH   R1,HDRLEN                                                        
*                                                                               
         CHI   R1,MSGHDRLQ         Wrong size expecting MSGHDLQ2                
         JNH   *+2                                                              
         MVC   USERID,MSGUSR       GET LOCAL COPIES OF HEADER INFO              
         MVC   USERIDH,MSGUSRNO                                                 
         MVC   RPTSUBID,MSGPQSUB                                                
         MVC   RPTPQNUM,MSGPQNUM   PQ report number         (Chr Hex)           
         MVC   RPTHDATE,MSGHTIME   Creation Date            (Chr Hex)           
         MVC   RPTHTIME,MSGHTIME+4 Creation Time            (Chr Hex)           
         MVC   RPTLOGNO,MSGLOGNO   Logical report number                        
         MVC   RPTPQTYP,MSGPQTYP   PQ report type                               
         MVC   SECAGY,MSGSECAG     Security agency                              
         MVC   PQDSPACE,MSGDSPCE   PQ DSPACE                                    
*                                                                               
         GOTO1 VHEXI31,DMCB,MSGUSRNO,PQUSER#,L'MSGUSRNO,0                       
         GOTO1 VHEXI31,DMCB,MSGPQNUM,PQRPT#,L'MSGPQNUM,0                        
         GOTO1 VHEXI31,DMCB,MSGPERID,PQPID#,L'MSGPERID,0                        
*                                                                               
         AH    R4,HDRLEN           R4 = FIRST LINE OF MESSAGE                   
         BRAS  RE,PERINFO          Get person information                       
         BE    SPDF02                                                           
         BRAS  RE,ERRMSG                                                        
         B     EXITOK                                                           
                                                                                
*                                                                               
SPDF02   BRAS  RE,GETNXTL          GET NEXT LINE INTO R / RLEN                  
         BNE   SPDF04              End of MQ message                            
*                                                                               
         CLI   GOTHDR,YES          TEST ALREADY HAVE HEADER                     
         BE    SPDF03              YES                                          
         BRAS  RE,PRCHDR           PROCESS THIS CARD AS IF HEADER               
         B     SPDF02              NEXT CARD                                    
*                                                                               
SPDF03   CLC   =C'++DDS',R+1       IS THIS A DDS CONTROL CARD                   
         BNE   SPDF04              NO - THEN WE ARE INTO THE PDF PROPER         
         BRAS  RE,PRCDDS                                                        
         B     SPDF02                                                           
*                                                                               
SPDF04   CLI   GOTHDR,YES          TEST ALREADY HAVE HEADER                     
         BNE   NOHDR               PRT WARNING MSG AND EXIT                     
         BRAS  RE,BLDDEST          DEST (EDICT RECORD)                          
         ICM   R3,15,AMSGOUT       R3 = MQ PDF OUTPUT AREA                      
*                                                                               
         LH    RF,UIDLEN                                                        
         SHI   RF,15                                                            
         JP    SPDF06              Any UUID supplied?                           
         MVI   ERROR,21            MISSING UID number                           
         BRAS  RE,ERRMSG                                                        
         MVC   PLINE,R+1                                                        
         BRAS  RE,PRNT                                                          
*        B     EXITOK              Counitue anyway                              
                                                                                
*********************************************************************           
* START BUILDING PDF MESSAGE IN MSGOUT                                          
*********************************************************************           
SPDF06   XC    NUMLINES,NUMLINES   RESET LINE COUNTER                           
         BRAS  RE,GETIDN           GET SOME ID INFORMATION FROM CTIREC          
*                                                                               
         SAM31                                                                  
         L     R0,AMSGOUT          CLEARING LIKE THIS IS LIKELY                 
         L     R1,MAXMSGLN         OVERKILL                                     
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         BRAS  RE,TRACELOG                                                      
*                                                                               
         L     R3,AMSGOUT          Build header info for PDF process            
         BRAS  RE,DATETIME         Build date/time information                  
         BRAS  RE,BLDXREF          Build unique reference for tracking          
         BRAS  RE,BLDXML           Build xml header information                 
*                                                                               
         BRAS  RE,ADDCRLF                                                       
         BRAS  RE,LINESUP          Print out header/xml                         
         L     R0,NUMLINES                                                      
         AHI   R0,-1                                                            
         ST    R0,NUMLINES         Don't count header/xml                       
         B     SPDF21              Now deal with first line of report           
*                                                                               
SPDF10   BRAS  RE,LINESUP                                                       
*                                                                               
*        CLI   DRTEST,C'Y'         DISASTER RECOVERY TEST MODE?                 
*        BNE   *+8                                                              
*        BRAS  RE,PDRTEST                                                       
*                                                                               
SPDF20   BRAS  RE,GETNXTL          GET NEXT LINE INTO R / RLEN                  
         BNE   SPDF50              End of MQ message                            
         CLI   SKIPBXF,YES                                                      
         BE    SPDF20                                                           
         CLC   =C'*** BXF REPORT SECTION ***',R+1                               
         BNE   SPDF21                                                           
         MVI   SKIPBXF,YES                                                      
         B     SPDF20                                                           
*                                                                               
SPDF21   LR    RF,R3               MAKE SURE WE CAN FIT MORE DATA               
         AH    RF,RLEN                                                          
         AHI   RF,32               APPROXIMATION FOR MAX SIZE                   
         S     RF,AMSGOUT                                                       
         C     RF,MAXMSGLN                                                      
         BH    TOOBIG                                                           
*                                                                               
SPDF22   CLI   R,X'8B'             New page before                              
         BNE   SPDF24                                                           
         CLI   GOTDATA,NO          Avoid any blank pages at the start           
         BE    SPDF20              Skip and do not trace                        
         MVC   0(L'NEWPAGE,R3),NEWPAGE    Signal new page                       
         AHI   R3,L'NEWPAGE                                                     
         LH    RF,NUMPAGES         Increment page counter                       
         AHI   RF,1                                                             
         STH   RF,NUMPAGES                                                      
         B     SPDF10              Next line                                    
*                                                                               
SPDF24   CLI   R,X'0B'             Single space before data                     
         BNE   SPDF25                                                           
         MVC   0(L'SINGLESP,R3),SINGLESP                                        
         AHI   R3,L'SINGLESP                                                    
         B     SPDF10              Next line                                    
*                                                                               
SPDF25   CLI   R,X'13'             Double space before data                     
         BNE   SPDF26                                                           
         MVC   0(L'DOUBLESP,R3),DOUBLESP                                        
         AHI   R3,L'DOUBLESP                                                    
         B     SPDF10              Next line                                    
*                                                                               
SPDF26   CLI   R,X'1B'             Triple space                                 
         BNE   SPDF30                                                           
         MVC   0(L'TRIPLESP,R3),TRIPLESP                                        
         AHI   R3,L'TRIPLESP                                                    
         B     SPDF10              Next line                                    
*                                                                               
SPDF30   LH    RF,RLEN             See if blank line                            
         SHI   RF,1                                                             
         BZ    SPDF40              If only 1 char then control char             
         EX    RF,CLCR                                                          
         BE    SPDF40              Yes, then just add control chars             
*                                  Find end of TEXT                             
SPDF32   MVI   GOTDATA,YES         Not an empty PDF                             
         LA    R5,R+1                                                           
         LH    RF,RLEN                                                          
         SHI   RF,-2               1 for control char & one for EX              
         LARL  RE,XMLTRTAB                                                      
         EX    RF,TRTXML2          Replace &, <, > with control char            
         BZ    SPDF38              Good as is                                   
         LA    R1,R+1              Find all the invalid XML chars               
         AHI   RF,1                                                             
*                                                                               
SPDF33   CLI   0(R1),C'&&'                                                      
         BNE   SPDF34                                                           
         MVC   0(4,R3),=C'&&amp'                                                
         MVI   4(R3),X'5E'         Semicolon                                    
         AHI   R3,5                                                             
         B     SPDF37                                                           
*                                                                               
SPDF34   CLI   0(R1),C'<'                                                       
         BNE   SPDF35                                                           
         MVC   0(3,R3),=C'&&lt'                                                 
         MVI   3(R3),X'5E'         Semicolon                                    
         AHI   R3,4                                                             
         B     SPDF37                                                           
*                                                                               
SPDF35   CLI   0(R1),C'>'                                                       
         BNE   SPDF36                                                           
         MVC   0(3,R3),=C'&&gt'                                                 
         MVI   3(R3),X'5E'         Semicolon                                    
         AHI   R3,4                                                             
         B     SPDF37                                                           
*                                                                               
SPDF36   MVC   0(1,R3),0(R1)                                                    
         AHI   R3,1                                                             
*                                                                               
SPDF37   AHI   R1,1                Next character in print line                 
         BCT   RF,SPDF33                                                        
         B     SPDF40                                                           
*                                                                               
SPDF38   EX    RF,MVCMQBUF                                                      
         AHI   RF,1                                                             
         AR    R3,RF               Point to end of new data in buffer           
*                                                                               
SPDF40   CLI   R,X'09'             Single space after                           
         BNE   SPDF42              No                                           
         MVC   0(L'SINGLESP,R3),SINGLESP                                        
         AHI   R3,L'SINGLESP                                                    
         B     SPDF10                                                           
*                                                                               
SPDF42   CLI   R,X'11'             Double space after                           
         BNE   SPDF44              No                                           
         MVC   0(L'DOUBLESP,R3),DOUBLESP                                        
         AHI   R3,L'DOUBLESP                                                    
         B     SPDF10                                                           
*                                                                               
SPDF44   CLI   R,X'19'             Triple space after                           
         BNE   SPDF46              No                                           
         MVC   0(L'TRIPLESP,R3),TRIPLESP                                        
         AHI   R3,L'TRIPLESP                                                    
         B     SPDF10                                                           
*                                                                               
SPDF46   CLI   R,X'89'             Form Feed after data                         
         BNE   SPDF10              No                                           
         CLI   GOTDATA,NO          Avoid any blank pages at the start           
         BE    SPDF20              Skip and do not trace                        
         MVC   0(L'NEWPAGE,R3),NEWPAGE                                          
         AHI   R3,L'NEWPAGE                                                     
         LH    RF,NUMPAGES         Increment page counter                       
         AHI   RF,1                                                             
         STH   RF,NUMPAGES                                                      
         B     SPDF10                                                           
                                                                                
**********************************************************************          
* Now finish up report to send                                                  
**********************************************************************          
SPDF50   GOTOR XMLTAG,XMLPARM,=C'Report',ENDTAG                                 
         GOTOR XMLTAG,XMLPARM,=C'ReportEnvelope',ENDTAG                         
         BRAS  RE,LINESUP                                                       
         L     R0,NUMLINES                                                      
         AHI   R0,-1                                                            
         ST    R0,NUMLINES         Don't count end xml                          
*                                                                               
         L     RE,APAGES           Number of pages                              
         LLH   RF,NUMPAGES                                                      
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(4,RE),DUB                                                      
*                                                                               
         L     RE,ALINES           Number of Lines                              
         L     RF,NUMLINES                                                      
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(5,RE),DUB                                                      
*                                                                               
         CLI   GOTDATA,YES         EMPTY PDF?                                   
         BNE   NODATA              YES - SKIP THIS                              
         MVC   MSGDESC_FORMAT,=CL8'MQSTR'                                       
         ICM   RF,15,PUTOPTS_OPTIONS                                            
         A     RF,=AL4(MQPMO_SET_IDENTITY_CONTEXT)                              
         STCM  RF,15,PUTOPTS_OPTIONS                                            
*                                                                               
         MVC   MQPUTBUF,AMSGOUT    PUT THIS MESSAGE TO MQ                       
         S     R3,AMSGOUT                                                       
         ST    R3,DATALEN          SET MESSAGE LENGTH                           
*                                                                               
         MVC   MQPUTHOB,=A(OUTQHOB)                                             
         LA    R2,MQPUT            MQ PUT                                       
         BRAS  RE,CALLMQ                                                        
         BNE   CANTSEND                                                         
*                                                                               
         LA    R2,MQCMIT           MQ COMMIT                                    
         BRAS  RE,CALLMQ                                                        
         BNE   CANTSEND                                                         
*                                                                               
         ICM   RF,15,PUTOPTS_OPTIONS                                            
         S     RF,=AL4(MQPMO_SET_IDENTITY_CONTEXT)                              
         STCM  RF,15,PUTOPTS_OPTIONS                                            
*                                                                               
         BRAS  RE,TRACELOG                                                      
         BRAS  RE,SENTMSG          RPT MESSAGE AS SENT                          
*                                                                               
         L     R7,ADSTAEFT         TOP of table                                 
         USING DSTAEFTD,R7                                                      
SPDF81   CLI   0(R7),X'FF'         EOT                                          
         BE    EXITOK                                                           
         OC    DSTAEDST,DSTAEDST                                                
         BZ    EXITOK                                                           
*                                                                               
         BRAS  RE,POSTSENT         ADD MQ MESSAGE FOR THIS ONE                  
         BNE   SPDF90                                                           
*                                                                               
SPDF85   MVC   PLINE(L'SENTRPT),SENTRPT                                         
         MVC   PLINE+L'SENTRPT(L'DSTAEDST),DSTAEDST                             
         MVC   PLINE+L'SENTRPT+L'DSTAEDST+3(L'REFNUM),REFNUM                    
         BRAS  RE,PRNT                                                          
*                                                                               
SPDF90   AHI   R7,DSTAEFTL         NEXT DESTINATION                             
         B     SPDF81                                                           
         DROP  R7                                                               
**********************************************************************          
* NO VALID DESTINATIONS - OUTPUT ERROR MESSAGE                                  
**********************************************************************          
SPDF100  MVC   PLINE(L'CANTSND),CANTSND                                         
         BRAS  RE,PRNT                                                          
*                                                                               
         L     R7,ADSTAEFT     *** MARK ALL DESTINATIONS JUNK                   
         USING DSTAEFTD,R7                                                      
SPDF120  CLI   0(R7),X'FF'         EOT                                          
         BE    SPDF140                                                          
         OC    DSTAEDST,DSTAEDST                                                
         BZ    SPDF140                                                          
*                                                                               
         MVI   EERRORCD,EDFERNFQ   NO PDF NUMBER                                
         BRAS  RE,POSTSENT                                                      
         BNE   SPDF130                                                          
*                                                                               
         MVC   PLINE+00(L'JUNKRPT),JUNKRPT                                      
         LA    RF,PLINE+L'JUNKRPT                                               
         MVC   0(L'DSTAEDST,RF),DSTAEDST                                        
         AHI   RF,L'DSTAEDST+3                                                  
         MVC   0(L'REFNUM,RF),REFNUM                                            
*                                                                               
         MVC   ERRMSG1R,PLINE                                                   
         GOTO1 VDMGR,DMCB,=C'OPMSG',('ERRMSG1Q',ERRMSG1)                        
         BRAS  RE,PRNT                                                          
*                                                                               
         L     RE,JREPCNT                                                       
         AHI   RE,1                                                             
         ST    RE,JREPCNT          INCREMENT JUNK REPORT COUNTER                
*                                                                               
SPDF130  AHI   R7,DSTAEFTL         NEXT DESTINATION                             
         B     SPDF120                                                          
         DROP  R7                                                               
*                                                                               
SPDF140  CLC   JREPCNT,JREPMAX                                                  
         BH    TOOJREP                                                          
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* Error exits                                                                   
***********************************************************************         
NOHDR    MVC   HDRMUID,USERID                                                   
         MVC   HDRMSUB,RPTSUBID                                                 
*                                                                               
         GOTO1 VHEXI31,DMCB,RPTPQNUM,HALF,4,0                                   
         SR    R0,R0                                                            
         ICM   R0,3,HALF                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  HDRMNUM,DUB                                                      
*                                                                               
         MVC   PLINE(HDRMISSQ),HDRMISS                                          
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
*                                                                               
NODATA   MVC   DATMUID,USERID                                                   
         MVC   DATMSUB,RPTSUBID                                                 
*                                                                               
         GOTO1 VHEXI31,DMCB,RPTPQNUM,HALF,4,0                                   
         SR    R0,R0                                                            
         ICM   R0,3,HALF                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DATMNUM,DUB                                                      
*                                                                               
         MVC   PLINE(DATMISSQ),DATMISS                                          
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
*                                                                               
TOOJREP  WTO   'EDIPDF - TOO MANY JUNK REPORTS, PLEASE RECYCLE THIS JOB+        
               .'                                                               
         DC    H'0'                                                             
*                                                                               
CANTSEND GOTO1 VLOGIO,DMCB,X'FF000001',=C'EDIPDF - Error sending to PDF+        
               - Transmissions suspended, please check'                         
         ABEND 701                                                              
*                                                                               
TOOBIG   GOTO1 VLOGIO,DMCB,X'FF000001',=C'EDIPDF - Report is too big to+        
                fit in buffer - must increase MAXMSGLEN'                        
         ABEND 701,DUMP                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
JUNKRPT  DC    C'Report marked junk - '                                         
SENTRPT  DC    C'Report marked sent - '                                         
CANTSND  DC    C'Unable to send report - no valid destinations '                
HDRMISS  DC    C'Missing Header, skip report: '                                 
HDRMUID  DS    CL8                                                              
         DC    C','                                                             
HDRMSUB  DS    CL3                                                              
         DC    C','                                                             
HDRMNUM  DS    CL5                                                              
HDRMISSQ EQU   *-HDRMISS                                                        
DATMISS  DC    C'Empty PDF, skip report: '                                      
DATMUID  DS    CL8                                                              
         DC    C','                                                             
DATMSUB  DS    CL3                                                              
         DC    C','                                                             
DATMNUM  DS    CL5                                                              
DATMISSQ EQU   *-DATMISS                                                        
*                                                                               
TSTHDRT  DC    X'444453545354'     DDSTST                                       
TSTHDRA  DC    X'444453414456'     DDSADV                                       
TSTHDRR  DC    X'444453524550'     DDSREP                                       
TSTHDRQ  DC    X'444453465141'     DDSFQA                                       
TSTHDRF  DC    32X'20'                                                          
         EJECT                                                                  
***********************************************************************         
* BLD XML information for PDF processing                              *         
***********************************************************************         
BLDXML   NTR1  BASE=*,LABEL=*                                                   
         L     R0,AMSGOUT          Start of Message out buffer                  
         GOTOR XMLTAG,XMLPARM,=C'ReportEnvelope',STARTAG                        
         GOTOR XMLTAGS,XMLPARM,=C'Header',=C'OPTICAPDF*******'                  
         GOTOR XMLTAGS,XMLPARM,=C'Environment',(L'DSPACE,DSPACE)                
         GOTOR XMLTAGS,XMLPARM,=C'RequestDate',(L'YYYYMMDD,YYYYMMDD)            
         GOTOR XMLTAGS,XMLPARM,=C'RequestTime',(L'HHMMSS,HHMMSS)                
         GOTOR XMLTAGS,XMLPARM,=C'TimeStamp',(L'YMDHMS,YMDHMS)                  
         LR    RE,R3                                                            
         AHI   RE,7                        L'<Pages>                            
         ST    RE,APAGES                   Fill in value later                  
         GOTOR XMLTAGS,XMLPARM,=C'Pages',=C'????'                               
*                                                                               
         LR    RE,R3                                                            
         AHI   RE,7                        L'<Lines>                            
         ST    RE,ALINES                   Fill in value later                  
         GOTOR XMLTAGS,XMLPARM,=C'Lines',=C'?????'                              
*                                                                               
         LARL  RE,HDR                                                           
         USING EDIHDRD,RE                                                       
         MVI   ORIENT,C'L'                                                      
         LA    RF,L'LANDSCAP                                                    
         LA    R5,LANDSCAP                                                      
         CLI   EDIWIDE,EDIWIDEQ    Landscape - C'W'                             
         BE    BLDXML10                                                         
         CLI   EDIWIDE,EDILANDQ    Landscape - C'L'                             
         BE    BLDXML10                                                         
         LA    RF,L'PORTRAIT                                                    
         LA    R5,PORTRAIT                                                      
         MVI   ORIENT,C'P'                                                      
         DROP  RE                                                               
*                                                                               
BLDXML10 GOTOR XMLTAGS,XMLPARM,=C'Orientation',((RF),(R5))                      
         GOTOR XMLTAGS,XMLPARM,=C'UserId',(USERNLN,USERNME)                     
*                                                                               
         MVC   WORK(L'RPTSUBID),RPTSUBID                                        
         MVI   WORK+3,C','                                                      
         LLH   RF,PQRPT#                   PQ Report number                     
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  WORK+4(5),DUB                                                    
         GOTOR XMLTAGS,XMLPARM,=C'PQReportId',(9,WORK)                          
*                                                                               
         CLI   FNAMELN,0                                                        
         BE    BLDXML20                                                         
         GOTOR XMLTAGS,XMLPARM,=C'FirstName',(FNAMELN,FNAME)                    
*                                                                               
BLDXML20 CLI   LNAMELN,0                                                        
         BE    BLDXML24                                                         
         GOTOR XMLTAGS,XMLPARM,=C'LastName',(LNAMELN,LNAME)                     
*                                                                               
BLDXML24 CLI   EMAILLN,0                                                        
         BE    BLDXML30                                                         
         GOTOR XMLTAGS,XMLPARM,=C'eMail',(EMAILLN,EMAIL)                        
*                                                                               
BLDXML30 GOTOR XMLTAGS,XMLPARM,=C'SecAgency',(L'SECAGY,SECAGY)                  
*                                                                               
         LLC   RF,PIDLN                                                         
         BCTR  RF,0                                                             
         MVC   WORK(0),PID                                                      
         EX    RF,*-6                                                           
         LA    R6,WORK+1(RF)       Bump past PId text                           
         MVI   0(R6),C'('                                                       
         MVI   6(R6),C')'                                                       
                                                                                
         LLH   RF,PQPID#           PId number                                   
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  1(5,R6),DUB                                                      
         AHI   R6,7                        Bump past (#####)                    
         LA    RE,WORK                                                          
         SR    R6,RE               Length of field combined field               
         GOTOR XMLTAGS,XMLPARM,=C'PId',((R6),WORK)                              
*                                                                               
         CLI   SENDDLNS,YES        DO WE WANT DELIVERY NOTIFICATIONS?           
         BNE   BLDXML60                                                         
         GOTOR XMLTAGS,XMLPARM,=C'NotifyKey',('XREFLNQ',REFNUM)                 
                                                                                
**********************************************************************          
* In the case of optica the SUB card has report type.                           
* This will distiguish between Station vs. Cable intructions                    
**********************************************************************          
BLDXML60 GOTOR XMLTAGS,XMLPARM,=C'ReportType',(3,SUB+15)                        
         LH    RF,UIDLEN                                                        
         SHI   RF,15               UUID is 15 from start                        
         JP    *+6                                                              
         XR    RF,RF                                                            
         GOTOR XMLTAGS,XMLPARM,=C'UUID',((RF),UID+15)                           
*                                                                               
         GOTOR XMLTAG,XMLPARM,=C'Report',STARTAG                                
                                                                                
**********************************************************************          
* Make sure the XML has all printable characters.                               
* You may need to use the correct language XLATE table one day                  
**********************************************************************          
         L     RE,AMSGOUT          Start of Message out buffer                  
         LR    R1,R3               R3 = Address within MQ buffer                
         SR    R1,RE               R1 = length of data to translate             
*                                                                               
BLDXML70 CHI   R1,256                                                           
         BNH   BLDXML72                                                         
         TR    0(256,RE),CHARTAB                                                
         AHI   RE,256                                                           
         SHI   R1,256                                                           
         BP    BLDXML70            More to translate                            
         B     BLDXML90                                                         
*                                                                               
BLDXML72 BCTR  R1,0                                                             
         EX    R1,TRNSLTE          Get rid of funky characters                  
*                                                                               
BLDXML90 B     EXITR3                                                           
         LTORG                                                                  
                                                                                
**********************************************************************          
* XML lables                                                                    
**********************************************************************          
*QLBLPDF DC    CL16'OPTICAPDF*******'                                           
*QHLABEL DS    CL16                LABEL for EDISKED                            
         EJECT                                                                  
***********************************************************************         
* Remove spaces at end of value                                                 
***********************************************************************         
REMSPACE CLI   0(R3),C' '          Remove space                                 
         JH    REMSPACX                                                         
         BRCT  R3,REMSPACE                                                      
*                                                                               
REMSPACX AHI   R3,1                Back to space                                
         BR    RE                                                               
                                                                                
***********************************************************************         
* Format single XML tag                                                         
* P1 = tag                                                                      
* P2 = 0 start tag                                                              
*      1 end   tag                                                              
***********************************************************************         
XMLTAG   STM   RE,R2,XMLREGS                                                    
         MVI   0(R3),C'<'          Open < for XML tag                           
         AHI   R3,1                                                             
         CLI   7(R1),1             P2=0 start, P2=1 end                         
         JL    XMLS10                                                           
         JH    *+2                 Invalid call                                 
         MVI   0(R3),C'/'                                                       
         AHI   R3,1                                                             
*                                                                               
XMLS10   LLC   RF,0(R1)            Length of tag                                
         BCTR  RF,0                                                             
         L     RE,0(,R1)                                                        
         NILH  GRE,X'00FF'                                                      
         EXRL  RF,EX_TAG                                                        
         LA    R3,1(RF,R3)         Increment R3                                 
         MVI   0(R3),C'>'          Close tag                                    
         AHI   R3,1                                                             
         LM    RE,R2,XMLREGS                                                    
         BSM   0,RE                                                             
                                                                                
***********************************************************************         
* Format XML tags and field                                                     
***********************************************************************         
XMLTAGS  STM   RE,R2,XMLREGS                                                    
         MVI   0(R3),C'<'          Open < for XML tag                           
         AHI   R3,1                                                             
         LLC   RF,0(R1)            Length of tag                                
         BCTR  RF,0                                                             
         L     RE,0(,R1)                                                        
         NILH  GRE,X'00FF'                                                      
         EXRL  RF,EX_TAG                                                        
         LA    R3,1(RF,R3)         Increment R3                                 
         MVI   0(R3),C'>'          Close tag                                    
         AHI   R3,1                                                             
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,4(R1)          Length of field                              
         JZ    XMLT62              No length then just end XML                  
         BCTR  RF,0                                                             
         L     RE,4(,R1)                                                        
         NILH  GRE,X'00FF'         Clear HOB                                    
*                                                                               
         LARL  R2,XMLTRTAB                                                      
         EXRL  RF,TRTXML           Replace &, <, > with control char            
         JZ    XMLT50              Good as is                                   
         AHI   RF,1                                                             
*                                                                               
XMLT05   CLI   0(RE),C'&&'                                                      
         JNE   XMLT10                                                           
         MVC   0(4,R3),=C'&&amp'                                                
         MVI   4(R3),X'5E'         Semicolon                                    
         AHI   R3,5                                                             
         J     XMLT40                                                           
*                                                                               
XMLT10   CLI   0(RE),C'<'                                                       
         JNE   XMLT20                                                           
         MVC   0(3,R3),=C'&&lt'                                                 
         MVI   3(R3),X'5E'         Semicolon                                    
         AHI   R3,4                                                             
         J     XMLT40                                                           
*                                                                               
XMLT20   CLI   0(RE),C'>'                                                       
         JNE   XMLT30                                                           
         MVC   0(3,R3),=C'&&gt'                                                 
         MVI   3(R3),X'5E'         Semicolon                                    
         AHI   R3,4                                                             
         J     XMLT40                                                           
*                                                                               
XMLT30   MVC   0(1,R3),0(RE)       Move one character at a time                 
         AHI   R3,1                                                             
*                                                                               
XMLT40   AHI   RE,1                                                             
         JCT   RF,XMLT05           Next character                               
         J     XMLT60                                                           
*                                                                               
XMLT50   DS    0H                                                               
*        LLC   RF,4(R1)            Length of field                              
*        BCTR  RF,0                                                             
         EXRL  RF,EX_XML                                                        
         LA    R3,1(RF,R3)                                                      
*                                                                               
XMLT60   BRAS  RE,REMSPACE                                                      
         LM    RE,R2,XMLREGS                                                    
*                                                                               
XMLT62   MVI   0(R3),C'<'          End XML tag                                  
         MVI   1(R3),C'/'                                                       
         AHI   R3,2                                                             
         L     RE,0(,R1)           Reload Tag address                           
         NILH  GRE,X'00FF'                                                      
         LLC   RF,0(,R1)           Tag length                                   
         BCTR  RF,0                                                             
         EXRL  RF,EX_TAG           Move in tag                                  
         LA    R3,1(RF,R3)         Increment R3                                 
         MVI   0(R3),C'>'          Close tag                                    
         AHI   R3,1                                                             
*                                                                               
         LM    RE,R2,XMLREGS                                                    
         BSM   0,RE                                                             
         LTORG                                                                  
*                                                                               
STARTAG  EQU   0                                                                
ENDTAG   EQU   1                                                                
NODATAQ  EQU   0                                                                
TRNSLTE  TR    0(0,RE),CHARTAB                                                  
TRTXML   TRT   0(0,RE),0(R2)                                                    
EX_TAG   DS    0H                                                               
EX_XML   MVC   0(0,R3),0(RE)                                                    
XMLREGS  DS    5F                               RE,RF,R0,R1                     
***********************************************************************         
* EX instructions to move data to MQBUFFER                                      
***********************************************************************         
CLCR     CLC   R+1(0),XSPACES                                                   
TRTXML2  TRT   0(0,R5),0(RE)                                                    
MVCMQBUF MVC   0(0,R3),R+1                                                      
*VCUUID  MVC   0(0,R3),UID+15                                                   
*VCUSER  MVC   0(0,R3),USERNME                                                  
*VCFNM   MVC   0(0,R3),FNAME                                                    
*VCLNM   MVC   0(0,R3),LNAME                                                    
MVCEML   MVC   0(0,R3),EMAIL                                                    
*VCPID   MVC   0(0,R3),PID                                                      
         EJECT                                                                  
***********************************************************************         
* CLEAR ALL FIELDS BEFORE STARTING PROCESSING PDF                     *         
***********************************************************************         
CLRFLDS  NTR1  BASE=*,LABEL=*                                                   
         MVI   GOTHDR,NO                                                        
         MVI   GOTDATA,NO          No real data yet                             
         XC    REQUESTR,REQUESTR                                                
         XC    LASTOUT,LASTOUT                                                  
*        XC    FAXSUBC,FAXSUBC                                                  
         XC    NUMLINES,NUMLINES                                                
         XC    COVPGLNS,COVPGLNS   NO COVER PAGE YET                            
         XC    PDFDATA,PDFDATA                                                  
         MVC   NUMPAGES,=H'1'      AT LEAST ONE PAGE WILL BE SENT               
         XC    LSTSDST,LSTSDST     ASSUME NO REPORT IS SENDABLE                 
         XC    RPTLDSTS,RPTLDSTS                                                
         XC    HDR,HDR                                                          
*                                                                               
         L     R2,ALINETAB         CLEAR OUT THE ++DDS LINES                    
         USING LINETABD,R2                                                      
CFL02    XC    LINELEN,LINELEN                                                  
         MVC   LINEIN,XSPACES                                                   
         AHI   R2,LINETABL                                                      
         CLI   LINEID,X'FF'                                                     
         BNE   CFL02                                                            
*                                                                               
         L     R0,ADSTAEFT         CLEAR OUT DESTINATION TABLE                  
         LHI   R1,DSTAEFTL*DSTMAXQ                                              
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     EXITOK                                                           
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*  Gather information about the requestor of report                             
***********************************************************************         
PERINFO  NTR1  BASE=*,LABEL=*                                                   
         MVC   EMAIL,XSPACES                                                    
         MVC   PID,XSPACES                                                      
         MVC   FNAME,XSPACES                                                    
         MVI   FNAMELN,0                                                        
         MVC   LNAME,XSPACES                                                    
         MVI   LNAMELN,0                                                        
         MVC   USERNME,XSPACES                                                  
         MVI   USERNLN,0                                                        
*                                                                               
         LA    R4,IOKEY                                                         
         USING CTIREC,R4                                                        
         XC    IOKEY,IOKEY                                                      
         MVI   CTIKTYP,CTIKTYPQ    C'I'                                         
         MVC   CTIKNUM,PQUSER#                                                  
         GOTO1 VDMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',IOKEY,AIO,0                 
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         L     R4,AIO                                                           
         LA    R3,CTIDATA                                                       
         DROP  R4                                                               
*                                                                               
         USING CTDSCD,R3                                                        
PERIN02  CLI   0(R3),0             EOR                                          
         BE    PERIN08                                                          
         CLI   CTDSCEL,CTDSCELQ    X'02'                                        
         BE    PERIN04                                                          
         LLC   RF,CTDSCLEN                                                      
         AR    R3,RF                                                            
         B     PERIN02                                                          
*                                                                               
MVCUNME  MVC   USERNME,CTDSC                                                    
*                                                                               
PERIN04  LLC   RF,CTDSCLEN         Save off User id name                        
         SHI   RF,(CTDSC-CTDSCD)+1                                              
         EX    RF,MVCUNME          Save off User id name                        
         LA    RE,USERNME(RF)      Point to end of data                         
PERIN05  CLI   0(RE),C' '                                                       
         BH    PERIN06                                                          
         SHI   RF,1                                                             
         JNP   *+2                                                              
         BCT   RE,PERIN05                                                       
         DC    H'00'                                                            
         DROP  R3                                                               
*                                                                               
PERIN06  AHI   RF,1                                                             
         STC   RF,USERNLN          Length of user name                          
*                                                                               
                                                                                
PERIN08  LA    R4,IOKEY                                                         
         USING SA0REC,R4                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,SECAGY                                                   
         MVC   SA0KNUM,PQPID#                                                   
         GOTO1 VDMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',IOKEY,AIO,0                 
         CLI   8(R1),0                                                          
         JNE   PERIER03                                                         
         L     R4,AIO                                                           
         LA    R3,SA0DATA                                                       
         DROP  R4                                                               
*                                                                               
         USING SAPALD,R3                                                        
PERIN10  CLI   0(R3),0             EOR                                          
         JE    PERINFX                                                          
         CLI   SAPALEL,SAPALELQ    X'C3'                                        
         BE    PERIN20                                                          
         LLC   RF,SAPALLN                                                       
         AR    R3,RF                                                            
         B     PERIN10                                                          
*                                                                               
PERIN20  MVC   PID,SAPALPID        Save off PID                                 
         LA    RE,PID+L'PID-1                                                   
         LA    RF,L'PID-1                                                       
PERIN21  CLI   0(RE),C' '                                                       
         BH    PERIN21A                                                         
         SHI   RF,1                                                             
         JNP   PERIER04                                                         
         BCT   RE,PERIN21                                                       
         DC    H'00'                                                            
         DROP  R3                                                               
*                                                                               
PERIN21A AHI   RF,1                                                             
         STC   RF,PIDLN            Save length of PID                           
*                                                                               
         LA    R4,IOKEY                                                         
         USING SAPEREC,R4                                                       
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ    C'F'                                         
         MVI   SAPESUB,SAPESUBQ    X'04'                                        
         MVC   SAPEAGY,SECAGY      Security Agency                              
         MVC   SAPEPID,PID         PID                                          
         GOTO1 VDMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',IOKEY,AIO,0                 
         CLI   8(R1),0                                                          
         JNE   PERIER05                                                         
         L     R4,AIO                                                           
         LA    R3,SAPEDATA         Point to first element                       
         DROP  R4                                                               
*                                                                               
PERIN22  CLI   0(R3),0             EOR                                          
         BE    PERINFX             Done                                         
         CLI   0(R3),SANAMELQ      Name element  (X'C5')                        
         BE    PERIN25                                                          
         CLI   0(R3),SAPEEELQ      eMail element (X'E5')                        
         BE    PERIN30                                                          
*                                                                               
PERIN24  LLC   RF,1(,R3)           Next element                                 
         AR    R3,RF                                                            
         B     PERIN22                                                          
*                                                                               
         USING SANAMD,R3                                                        
PERIN25  LA    R2,SANAMES                                                       
         TM    SANAMIND,SANAMIFN   First name                                   
         BZ    PERIN26                                                          
         LLC   RF,0(,R2)           Get length of name                           
         STC   RF,FNAMELN          Save length of name                          
         SHI   RF,1                                                             
         EX    RF,MVCFNAME                                                      
         LA    R2,2(RF,R2)                                                      
*                                                                               
PERIN26  TM    SANAMIND,SANAMIMN   Middle name                                  
         BZ    PERIN28                                                          
         LLC   RF,0(,R2)           next name                                    
         LA    R2,1(RF,R2)                                                      
*                                                                               
PERIN28  TM    SANAMIND,SANAMILN   Last name                                    
         BZ    PERIN24                                                          
         LLC   RF,0(,R2)           next name                                    
         STC   RF,LNAMELN          Save length of name                          
         SHI   RF,1                                                             
         EX    RF,MVCLNAME                                                      
         B     PERIN24                                                          
         DROP  R3                                                               
*                                                                               
         USING SAPEED,R3                                                        
MVCEMAIL MVC   EMAIL(0),SAPEEID                                                 
*                                                                               
PERIN30  LLC   RF,SAPEELN                                                       
         SHI   RF,SAPEELNQ+1                                                    
         EX    RF,MVCEMAIL                                                      
         AHI   RF,1                                                             
         STC   RF,EMAILLN          Save length of email                         
         B     PERIN24                                                          
         DROP  R3                                                               
*                                                                               
PERIER03 MVI   ERROR,22                                                         
         B     PERINFX                                                          
PERIER04 MVI   ERROR,23                                                         
         B     PERINFX                                                          
PERIER05 MVI   ERROR,24                                                         
         B     PERINFX                                                          
*                                                                               
PERINFX  CLI   ERROR,0                                                          
         J     EXITOK                                                           
*                                                                               
MVCFNAME MVC   FNAME(0),1(R2)                                                   
MVCLNAME MVC   LNAME(0),1(R2)                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
****************************************************************                
*  Print out DATE/TIME information                                              
****************************************************************                
DATETIME NTR1  BASE=*,LABEL=*                                                   
         TIME  DEC                                                              
         ST    R0,PRNTDUB                                                       
         MVI   PRNTDUB+4,X'0F'                                                  
         UNPK  PRNTTIME,PRNTDUB(5)                                              
         MVC   HHMMSS,PRNTTIME                                                  
         GOTO1 VDATCON,DMCB,(5,1),(25,REALBDT)                                  
         GOTO1 VDATCON,DMCB,(3,REALBDT),(23,YMD)                                
         GOTO1 VDATCON,DMCB,(3,REALBDT),(20,YYYYMMDD)                           
*                                                                               
         LLC   R1,REALBHH          Binary hours                                 
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  HH,DUB                                                           
*                                                                               
         LLC   R1,REALBMM          Binary minutes                               
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MM,DUB                                                           
*                                                                               
         LLC   R1,REALBSS          Binary seconds                               
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  SS,DUB                                                           
         J     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET NEXT LINE INTO R AND RLEN                                       *         
* NTRY: R4 = CURRENT BUFFER POINTER                           MSGIN   *         
*       R3 = A(OUTPUT BUFFER) AFTER GETTING THE HDR           MSGOUT  *         
***********************************************************************         
GETNXTL  NTR1  BASE=*,LABEL=*                                                   
         XC    RLEN,RLEN           WE SET LENGTH OF THIS LINE HERE              
         MVC   R,XSPACES           WE MOVE INPUT A LINE AT A TIME HERE          
*                                                                               
         LR    R0,R4               CHECK NOT PAST END OF BUFFER                 
         S     R0,AMSGIN                                                        
         C     R0,DATALEN                                                       
         BNL   EXITL               End of MQ message                            
*                                                                               
         PACK  DUB,0(4,R4)         GET LENGTH OF THIS LINE                      
         CVB   R1,DUB                                                           
         AHI   R4,4                GO PAST LENGTH                               
*                                                                               
         LR    R0,R4               Copy line into R and len into RLEN           
         STH   R1,RLEN                                                          
         LA    RE,R                                                             
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
         AH    R4,RLEN             Move pointer in MSGIN                        
*                                  CC is Control Character                      
         CLC   =C'*** BXF REPORT SECTION ***',R+1                               
         BE    EXITL               Force end of report                          
*                                                                               
         TR    R+1(L'R-1),CHARTAB  nulls -> spaces, leave CC alone              
         CR    RB,RB               Set CC equal  (Condition Code)               
         XIT1  REGS=(R4)                                                        
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESS *HDR* CARD - THIS SHOULD BE THE FIRST CARD                  *         
***********************************************************************         
         USING DSTAEFTD,R7                                                      
         USING EDIHDRD,R2                                                       
PRCHDR   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,R+1                                                           
         CLC   =C'*HDR*',EDIHDR    IS THIS THE HEADER                           
         BNE   EXITL               NO - LOOP UNTIL YOU GET IT                   
         MVI   GOTHDR,YES                                                       
*                                                                               
*        CLC   =C'EDICT=',EDIEDICT OVERRIDE FOR KEY OF RECORD?                  
*        JNE   *+10                NO                                           
*        MVC   USERID,EDIOVKEY                                                  
         MVC   DSTAEDST,EDIDESID   SET FIRST DESTINATION                        
         MVC   DSTAEFRM,EDIFDEST   SET FORMATTED DESTINATION                    
         AHI   R7,DSTAEFTL         GO TO NEXT                                   
*                                                                               
         LH    R1,RLEN             SAVE HEADER STRIPPING OFF THE CC             
         AHI   R1,-1                                                            
         STH   R1,HDRLEN                                                        
         LA    R0,HDR                                                           
         LA    RE,R+1                                                           
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         B     EXITR7              RETURN R7                                    
         DROP  R2,R7               EDIHDRD, DSTAEFTD                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESS ++DDS CARD(S)                                               *         
***********************************************************************         
PRCDDS   NTR1  BASE=*,LABEL=*                                                   
         L     R2,ALINETAB         NOW DEAL WITH OTHER CONTROL CARDS            
         USING LINETABD,R2         THESE SHOULD BE SINGLETONS                   
*                                                                               
PDDS04   CLC   LINEID,R+12         MATCH CONTROL CARD?                          
         BE    PDDS06              YES                                          
         AHI   R2,LINETABL                                                      
         CLI   LINEID,X'FF'        EOT?                                         
         BNE   PDDS04              NO - NEXT CARD                               
*                                                                               
         MVI   ERROR,20            UNKNOWN ++DDS CARD                           
         BRAS  RE,ERRMSG                                                        
         MVC   PLINE,R+1                                                        
         BRAS  RE,PRNT                                                          
         B     EXITL               IGNORE IT FOR NOW                            
*                                                                               
PDDS06   LH    R1,RLEN             MOVE FROM R  - STRIP OFF LEADING CC          
         AHI   R1,-1                                                            
         STH   R1,LINELEN                                                       
         LA    R0,LINEIN                                                        
         LA    RE,R+1                                                           
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     EXITOK              GET NEXT CARD                                
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* OUTPUT DRTEST HEADER ON PDF                                         *         
***********************************************************************         
PDRTEST  NTR1  BASE=*,LABEL=*                                                   
         MVC   00(50,R3),STARS     LINE OF ASTERIXES                            
         AHI   R3,50                                                            
         BRAS  RE,ADDCRLF                                                       
         BRAS  RE,LINESUP                                                       
*                                                                               
         BRAS  RE,ADDCRLF          SPACE LINE                                   
         BRAS  RE,LINESUP                                                       
*                                                                               
         MVC   00(L'TSTPDF,R3),TSTPDF                                           
         AHI   R3,L'TSTPDF                                                      
         BRAS  RE,ADDCRLF                                                       
         BRAS  RE,LINESUP                                                       
*                                                                               
         BRAS  RE,ADDCRLF          SPACE LINE                                   
         BRAS  RE,LINESUP                                                       
*                                                                               
         MVC   00(50,R3),STARS     LINE OF ASTERIXES                            
         AHI   R3,50                                                            
         BRAS  RE,ADDCRLF                                                       
         BRAS  RE,LINESUP                                                       
*                                                                               
         BRAS  RE,ADDCRLF          SPACE LINE                                   
         BRAS  RE,LINESUP                                                       
         B     EXITR3                                                           
*                                                                               
TSTPDF   DC    C'THIS IS A TEST PDF TRANSMISSION - PLEASE DISREGARD'            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT 'PDF SENT' MESSAGE TO CONSOLE                     *         
***********************************************************************         
SENTMSG  NTR1  BASE=*,LABEL=*                                                   
         MVC   PLINE(L'PDFSNT),PDFSNT                                           
         LA    R2,PLINE+L'PDFSNT                                                
         EDIT  NUMLINES,(6,0(R2)),ALIGN=LEFT                                    
         AR    R2,R0                                                            
         MVC   0(L'LINESNT,R2),LINESNT                                          
         AHI   R2,L'LINESNT                                                     
         EDIT  NUMPAGES,(6,0(R2)),ALIGN=LEFT                                    
         AR    R2,R0                                                            
         MVC   0(L'PAGESNT,R2),PAGESNT                                          
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   PLINE(19),=CL19'Reference number = '                             
         MVC   PLINE+19(L'REFNUM),REFNUM                                        
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
         LTORG                                                                  
*                                                                               
PDFSNT   DC    C'PDF sent to converter '                                        
LINESNT  DC    C' lines in '                                                    
PAGESNT  DC    C' pages using MQ buffer'                                        
         EJECT                                                                  
***********************************************************************         
* GET DATA FROM EDICT RECORD FOR THIS USER                            *         
***********************************************************************         
BLDDEST  NTR1  BASE=*,LABEL=*                                                   
         LA    R5,KEY                                                           
         USING EDIKEYD,R5                                                       
         XC    EDIKEY,EDIKEY       CLEAR KEY                                    
         MVI   EDIKSYS,EDIKSYSQ    SYSTEM                                       
         MVI   EDITYPE,EDITYPEQ    RECORD TYPE                                  
         MVC   EDINAME,USERID      USER ALPHA                                   
*                                  FORCE TO READ FROM DISK                      
         GOTO1 VDMGR,DMCB,(X'24',DMRDHI),CTFILE,(R5),AIO,0                      
*                                                                               
         L     R5,AIO                                                           
         CLC   EDIKEY,KEY                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BD02     L     R4,AIO                                                           
         AHI   R4,EDIELDQ          POINT TO FIRST ELEMENT                       
         XR    RF,RF                                                            
         USING EDILNKD,R4                                                       
*                                                                               
BD04     CLI   EDILNKEL,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   EDILNKEL,EDILNKEQ                                                
         BE    BD06                                                             
         IC    RF,EDILNKLN                                                      
         BXH   R4,RF,BD04                                                       
*                                                                               
BD06     MVC   DESTNAME,EDINAME    USERID                                       
         MVC   DESTMETS,EDIMETHS   METHOD OF SENDING TRANSMISSIONS              
         MVC   DESTMETR,EDIMETHR   METHOD OF RECEIVING TRANSMISSIONS            
         MVC   DESTPDFN,=CL8'OPTICA'  PDF ACC NUMBER                            
         MVC   DESTUIDN,USERID                                                  
         LARL  RE,TRNSLTE                                                       
         TR    DESTNTRY,0(RE)                                                   
         B     EXITOK                                                           
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALL MQ                                                  *         
* NTRY: R2          = A(PARAMETER STRUCTURE)                          *         
* EXIT: MQ_CC       = MQ COMPLETION CODE                              *         
*       MQ_RC       = MQ REASON CODE                                  *         
*       CC SET EQ     WHEN CALL OKAY                                  *         
***********************************************************************         
CALLMQ   NTR1  BASE=*,LABEL=*                                                   
         SAM31                                                                  
*                                                                               
         L     RF,16(R2)           RF = A(MQ ROUTINE)                           
         LA    R3,20(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
*                                                                               
         SAM24                                                                  
         CLC   MQ_CC,=A(MQCC_OK)                                                
         BNE   CMQ30                                                            
         LA    R3,MQCOMND                                                       
CMQ02    OC    0(4,R3),0(R3)                                                    
         BZ    CMQ20               Not found, just report it                    
         C     R2,0(,R3)           Which command is it                          
         BE    CMQ10                                                            
         AHI   R3,8                                                             
         B     CMQ02                                                            
*                                                                               
CMQ10    L     RE,4(,R3)           A(A(HOB))                                    
         L     RE,0(,RE)           A(HOB)                                       
         MVC   PLINE,XSPACES                                                    
         MVC   PLINE+50(L'OUTQ_OBJECTNAME),OUTQ_OBJECTNAME-OUTQ(RE)             
CMQ20    MVI   PLINE,C'+'          '+' MEANS IT'S AN MQ CALL                    
         MVC   PLINE+1(16),0(R2)   PRINT ROUTINE NAME AND RETURN CODES          
         MVC   PLINE+30(12),=C'Completed ok'                                    
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
*                                                                               
CMQ30    LA    RF,MQGET            MQGET CAN HAVE NZ CC                         
         CR    R2,RF               IF SO THE FOLLOWING 2 FIELDS ARE SET         
         BNE   CMQ40                                                            
         CLC   MQ_CC,=A(MQCC_WARNING)                                           
         BNE   CMQ40                                                            
         CLC   MQ_RC,=A(MQRC_SIGNAL_REQUEST_ACCEPTED)                           
         BNE   CMQ40                                                            
         MVI   PLINE,C'+'          '+' MEANS IT'S AN MQ CALL                    
         MVC   PLINE+1(16),0(R2)   PRINT ROUTINE NAME AND RETURN CODES          
         MVC   PLINE+30(35),=CL35'WAITING FOR NEW MESSAGE'                      
         BRAS  RE,PRNT                                                          
         B     EXITH               FLAG THAT WE NEED TO WAIT                    
*                                                                               
CMQ40    MVI   PLINE,C'+'                                                       
         MVC   PLINE+1(16),0(R2)                                                
         MVC   PLINE+20(09),=C'**ERROR**'                                       
         MVC   PLINE+30(08),=CL08'Warning '                                     
         CLC   MQ_CC,=A(MQCC_WARNING)                                           
         BE    CMQ60                                                            
         MVC   PLINE+30(08),=CL08'Failed  '                                     
         CLC   MQ_CC,=A(MQCC_FAILED)                                            
         BE    CMQ60                                                            
         MVC   PLINE+30(08),=CL08'Unknown '                                     
         EDIT  MQ_CC,(7,PLINE+38),ALIGN=LEFT,ZERO=NOBLANK                       
*                                                                               
CMQ60    MVC   PLINE+46(3),=C'RC='                                              
         EDIT  MQ_RC,(5,PLINE+49),ALIGN=LEFT,ZERO=NOBLANK                       
*                                                                               
         L     RF,AWHY                                                          
CMQ80    CLI   0(RF),X'FF'         SEE IF WE HAVE TEXT FOR THE PROBLEM          
         BE    CMQ90                                                            
         CLC   MQ_RC,0(RF)                                                      
         BE    *+12                                                             
         AHI   RF,28                                                            
         B     CMQ80                                                            
*                                                                               
         MVC   PLINE+60(24),4(RF)                                               
*                                                                               
CMQ90    BRAS  RE,PRNT                                                          
         B     EXITL                                                            
*                                                                               
MQCOMND  DC    A(MQGET,MQGETHOB)                                                
         DC    A(MQPUT,MQPUTHOB)                                                
         DC    A(MQOPEN,MQOPNHOB)                                               
         DC    A(MQCLOSE,MQCLSHOB)                                              
         DC    A(0,0)                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SET UP OPERATOR COMMUNICATIONS                                      *         
***********************************************************************         
SETOPS   NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,7                                                             
         BRAS  RE,SYSMESS          BEGINNING SETTING OPERATOR COMMS             
*                                                                               
         EXTRACT ACOMM,FIELDS=COMM                                              
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
         MVC   ECBLST,AOPERECB                                                  
*                                                                               
         L     R2,COMCIBPT         GET A(CIB)                                   
         USING CIBNEXT,R2                                                       
         LA    R3,COMCIBPT         SET A(A(CIB))                                
         DROP  RF                                                               
*                                                                               
         CLI   CIBVERB,CIBSTART    TEST FOR 'S JOBNAME' (IE NOT BATCH)          
         BNE   SETOP2                                                           
         DROP  R2                                                               
         QEDIT ORIGIN=(R3),BLOCK=(R2)        RELEASE THE CIB                    
*                                                                               
SETOP2   QEDIT ORIGIN=(R3),CIBCTR=1          ACCEPT MODIFY COMMANDS             
*                                                                               
         LHI   R0,8                                                             
         BRAS  RE,SYSMESS          COMPLETED SETTING OPERATOR COMMS             
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READ IN ALL INPUT CARDS                                             *         
* FORMAT INTO A SCANNER BLOCK AND SAVE FOR VALIDATION                 *         
***********************************************************************         
GETCARDS NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,3                                                             
         BRAS  RE,SYSMESS          STARTING READING INPUT CARDS                 
*                                                                               
         L     R3,ACARDSIN                                                      
GCD02    GOTO1 VCARDS,PLIST,CARDIO,=C'RE00'                                     
         MVC   PLINE(L'CARDIO),CARDIO                                           
         BRAS  RE,PRNT             ALWAYS PRINT THE LINE                        
         CLI   CARDIO,C'*'         IGNORE COMMENTS                              
         BE    GCD02                                                            
         CLC   =C'/*',CARDIO       END OF CARDS?                                
         BE    GCDX                YES                                          
*                                                                               
         CLI   0(R3),X'FF'         OVERFLOW                                     
         BNE   GCD04                                                            
         MVI   ERROR,05            TOO MANY PARAMETERS                          
         BRAS  RE,ERRMSG                                                        
         B     EXITL                                                            
*                                                                               
GCD04    MVC   0(L'CARDIO,R3),CARDIO                                            
         AHI   R3,L'CARDIO                                                      
         B     GCD02               NEXT                                         
*                                                                               
GCDX     MVI   0(R3),X'FF'         FLAG END OF CARDS                            
         LHI   R0,4                                                             
         BRAS  RE,SYSMESS          COMPLETED READING INPUT CARDS                
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE PARAMETER CARDS IN SCANNER BLOCK                           *         
***********************************************************************         
VALCARDS NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,5                                                             
         BRAS  RE,SYSMESS          STARTING VALIDATE INPUT CARDS                
         MVI   GOTERR,C'N'                                                      
*                                                                               
         L     R3,ACARDSIN                                                      
VCD02    CLI   0(R3),X'FF'                                                      
         BE    VCDX                                                             
*                                                                               
         L     R4,ACARDTAB         TABLE OF SUPPORTED PARAMETERS                
         USING CARDTABD,R4                                                      
VCD04    CLI   0(R4),255           END OF TABLE?                                
         BE    VCD08                                                            
         TM    CFLAG,CFBIG         BIG CARDS ARE NOT SCANNED                    
         BZ    VCD06                                                            
*                                                                               
         XR    RF,RF                                                            
         IC    RF,CMAX                                                          
         EX    RF,*+8              MATCH TEXT STRING                            
         BE    VCD20                                                            
         CLC   CARDTXT(0),0(R3)                                                 
*                                                                               
VCD06    AHI   R4,CARDTABL         TRY NEXT ENTRY                               
         B     VCD04                                                            
*                                                                               
VCD08    GOTO1 VSCANNER,PLIST,(C'C',0(R3)),ASCANTAB,0                           
         XR    R2,R2                                                            
         ICM   R2,1,4(R1)          RF=NUMBER OF INPUT PARAMETERS                
         BNZ   VCD10                                                            
         MVC   PLINE(L'CARDIO),0(R3)                                            
         BRAS  RE,PRNT                                                          
         MVI   ERROR,04                                                         
         BRAS  RE,ERRMSG                                                        
         B     VCD18                                                            
*                                                                               
VCD10    L     R5,ASCANTAB                                                      
         USING SCANBLKD,R5                                                      
*                                                                               
VCD12    XR    RF,RF               RECONSTRUCT INPUT PARAMETER                  
         IC    RF,SC1STLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PLINE(0),SC1STFLD                                                
         LA    RE,PLINE+1(RF)                                                   
*                                                                               
         CLI   SC2NDLEN,0          ANY SECOND HALF TO PARAMETER                 
         BE    VCD14                                                            
         MVI   0(RE),C'='                                                       
         MVC   1(L'SC2NDFLD,RE),SC2NDFLD                                        
*                                                                               
VCD14    BRAS  RE,PRNT             PRINT RECONSTRUCTED PARAMETER                
*                                                                               
         L     R4,ACARDTAB         TABLE OF SUPPORTED PARAMETERS                
         USING CARDTABD,R4                                                      
         XR    RF,RF                                                            
         IC    RF,SC1STLEN         LENGTH OF PARAMETER                          
         BCTR  RF,0                                                             
*                                                                               
VCD16    CLI   0(R4),255           END OF TABLE?                                
         BNE   *+16                                                             
         MVI   ERROR,01            FLAG UNKNOWN PARAMETER CARD                  
         BRAS  RE,ERRMSG                                                        
         B     VCD22                                                            
*                                                                               
         CLM   RF,1,CMIN           CHECK LENGTHS ARE OK                         
         BL    VCD18                                                            
         CLM   RF,1,CMAX                                                        
         BH    VCD18                                                            
         EX    RF,*+8              MATCH TEXT STRING                            
         BE    VCD20                                                            
         CLC   SC1STFLD(0),CARDTXT                                              
*                                                                               
VCD18    AHI   R4,CARDTABL         TRY NEXT ENTRY                               
         B     VCD16                                                            
*                                                                               
VCD20    ICM   RF,15,CARDVAL       GO AND VALIDATE THIS INPUT                   
         BASR  RE,RF                                                            
         BE    *+8                                                              
         BRAS  RE,ERRMSG           PRINT ERROR MESSAGE                          
*                                                                               
VCD22    AHI   R5,SCBLKLQ          NEXT LINE IN SCANTAB                         
         BCT   R2,VCD12            ANY MORE PARMS INPUT?                        
*                                                                               
VCD24    AHI   R3,L'CARDIO                                                      
         B     VCD02                                                            
*                                                                               
VCDX     ZAP   LINE,P99            FORCE PAGE THROW                             
         LHI   R0,6                                                             
         BRAS  RE,SYSMESS          ENDED VALIDATE INPUT CARDS                   
*                                                                               
         CLI   GOTERR,C'Y'         SET CC BASED ON ERRORS                       
         BNE   EXITOK                                                           
         B     EXITL                                                            
         DROP  R4,R5                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF DSPACE= CARD                                          *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
         USING SCANBLKD,R5                                                      
VCDSPACE NTR1  BASE=*,LABEL=*                                                   
         CLI   SC2NDLEN,1                                                       
         BE    *+12                                                             
         MVI   ERROR,02                                                         
         B     EXITL                                                            
*                                                                               
         ICM   RF,15,ASSB                                                       
         MVC   SSODSPAC-SSOOFF(L'SSODSPAC,RF),SC2NDFLD                          
*                                                                               
         MVC   DSPACE,SSODSPAC-SSOOFF(RF)                                       
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF DDSIO= CARD                                           *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
VCDDSIO  NTR1  BASE=*,LABEL=*                                                   
         CLI   SC2NDLEN,8                                                       
         BNH   *+12                                                             
         MVI   ERROR,03                                                         
         B     EXITL                                                            
*                                                                               
         LT    RF,VMODLIB        MODLIB                                         
         JZ    *+2                                                              
         MVC   0(8,RF),SC2NDFLD                                                 
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF DRTEST= CARD                                          *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
VCDR     NTR1  BASE=*,LABEL=*                                                   
         MVC   DRTEST,SC2NDFLD                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF DEBUG= CARD                                           *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
VCDEBUG  NTR1  BASE=*,LABEL=*                                                   
         MVC   DEBUG,SC2NDFLD                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF TRACE= CARD                                           *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
VCTRACE  NTR1  BASE=*,LABEL=*                                                   
         MVC   TRACE,SC2NDFLD                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF QMGR= OVERRIDE CARD                                   *         
* NTRY: R3     = A(INPUT CARD)                                        *         
***********************************************************************         
VCQMGR   NTR1  BASE=*,LABEL=*                                                   
         MVC   QMGR(L'SC2NDFLD),SC2NDFLD                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF LOGGING=CARD (DEFAULT IS 'Y' UNLESS YOU HAVE AN 'N')  *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
VCLOG    NTR1  BASE=*,LABEL=*                                                   
         MVC   LOG,SC2NDFLD                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF DLNS=CARD (DEFAULT IS 'Y' UNLESS YOU HAVE AN 'N')     *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
VCDLNS   NTR1  BASE=*,LABEL=*                                                   
         MVC   SENDDLNS,SC2NDFLD                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF JUNKREP=CARD  (NUMBER)                                *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
VJREP    NTR1  BASE=*,LABEL=*                                                   
         OI    SC2NDVAL,SCNUMQ     IS THIS A NUMBER?                            
         BZ    EXITOK              NO - USE DEFAULT AND EXIT OK                 
         MVC   JREPMAX,SC2NDNUM                                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT ERROR MESSAGE                                      *         
***********************************************************************         
ERRMSG   NTR1  BASE=*,LABEL=*                                                   
         MVI   GOTERR,C'Y'         SET GOT AN ERROR                             
*                                                                               
         LA    RF,ERRND                                                         
         CLI   ERROR,0             UNDEFINED ERROR                              
         BE    ERRM02                                                           
*                                                                               
         LLC   RF,ERROR            INDEX INTO ERROR TABLE                       
         BCTR  RF,0                                                             
         MHI   RF,L'ERRMSGS                                                     
         A     RF,AERRMSG          RF = A(ERROR MESSAGE)                        
*                                                                               
ERRM02   MVC   PLINE,XSPACES                                                    
         MVC   PLINE(L'ERRHDR),ERRHDR                                           
         MVC   PLINE+L'ERRHDR(L'ERRMSGS),0(RF)                                  
         BRAS  RE,PRNT                                                          
*                                                                               
         MVI   ERROR,0                                                          
         B     EXITOK                                                           
*                                                                               
ERRHDR   DC    C'*** ERROR *** '                                                
ERRND    DC    CL45'Improperly defined error'                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT PLINE TO A PRINT LINE                             *         
***********************************************************************         
TRACELOG CLI   TRACE,C'F'                                                       
         BNER  RE                                                               
*                                                                               
PRNT     NTR1  BASE=*,LABEL=*                                                   
         TIME  DEC                                                              
         ST    R0,PRNTDUB                                                       
         MVI   PRNTDUB+4,X'0F'                                                  
         UNPK  PRNTTIME,PRNTDUB(5)                                              
         MVC   P+00(2),PRNTTIME                                                 
         MVI   P+02,C':'                                                        
         MVC   P+03(2),PRNTTIME+2                                               
         MVI   P+05,C':'                                                        
         MVC   P+06(2),PRNTTIME+4                                               
         MVI   P+08,C'.'                                                        
         MVC   P+09(2),PRNTTIME+6                                               
         MVC   P+12(L'PLINE),PLINE                                              
         MVC   PLINE,XSPACES                                                    
         GOTO1 VPRINTER                                                         
         B     EXITOK                                                           
*                                                                               
PRNTDUB  DS    D                   FOR PRNT ROUTINE ONLY                        
PRNTTIME DS    CL9                 FOR PRNT ROUTINE ONLY                        
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PUT A MESSAGE TO THE OPERATOR CONSOLE AND OPTIONALLY GET A REPLY.   *         
* NTRY: R0        = MESSAGE NUMBER                                    *         
*       XTRAMESS  = OPTIONAL CL8 TO BE INSERTED INTO MESSAGE          *         
*       XTRAMES2  = OPTIONAL CL16 TO BE INSERTED INTO MESSAGE         *         
*       R0 ==  0  = SPECIAL MESSAGE AT 0(R1)                          *         
*                                                                     *         
* 1. IF THE FIRST CHARACTER OF THE MESSAGE IS AN 'X' JUST PUT OUT THE *         
* MESSAGE                                                             *         
*                                                                     *         
* 2. IF IT IS A NUMBER THIS IS THE # OF CHARACTERS FOR THE RESPONSE   *         
* RESPONSE IS RETURNED IN 'REPLY' - "DUMP" AND "EOJ" HANDLED IN HERE  *         
*                                                                     *         
* 3. ELSE THIS IS A MULTILINE MESSAGE AND IT LOOPS UNTIL (1) OR (2)   *         
***********************************************************************         
SYSMESS  NTR1  BASE=*,LABEL=*                                                   
         LR    R2,R1                                                            
         AHI   R0,-1                                                            
         BM    SYM02               R0 == 0 MEANS R1=A(MESSAGE)                  
*                                                                               
         MHI   R0,L'SYSMSGS                                                     
         L     R2,ASYSMSGS                                                      
         AR    R2,R0               R2=A(SYSMSGS ENTRY)                          
*                                                                               
SYM02    MVC   MESSAGE,XSPACES     BUILD MESSAGE                                
         MVC   MESSAGE(8),=CL08'EDPQSCAN'                                       
         MVC   MESSAGE+9(L'SYSMSGS-1),1(R2)                                     
*                                                                               
         LA    R0,MESSAGE          NOW REPLACE SUBSTITUTE CHARS                 
         LA    R1,MESSAGE+L'MESSAGE-1                                           
SYM04    CR    R0,R1                                                            
         BE    SYM10                                                            
         CLC   0(18,R1),=18C'X'                                                 
         BE    SYM08                                                            
         CLC   0(8,R1),=18C'X'                                                  
         BE    SYM06                                                            
         BCT   R1,SYM04                                                         
         DC    H'0'                                                             
*                                                                               
SYM06    MVC   0(8,R1),XTRAMESS                                                 
         B     SYM10                                                            
*                                                                               
SYM08    MVC   0(18,R1),XTRAMES2                                                
         B     SYM10                                                            
*                                                                               
SYM10    CLI   0(R2),C'0'                                                       
         BH    SYM12                                                            
         GOTO1 VLOGIO,DMCB,X'FF000001',(L'MESSAGE,MESSAGE)                      
         CLI   0(R2),C'X'                                                       
         BE    SYM16                                                            
         AHI   R2,L'SYSMSGS        SPECIAL MULTILINE MESSAGE                    
         B     SYM10                                                            
*                                                                               
SYM12    GOTO1 VLOGIO,DMCB,1,(L'MESSAGE,MESSAGE)                                
                                                                                
         XR    R0,R0                                                            
         ICM   R0,1,0(R2)                                                       
         N     R0,=X'0000000F'                                                  
         GOTO1 VLOGIO,DMCB,0,((R0),REPLY)                                       
*                                                                               
         CLC   REPLY(4),=C'DUMP'   CHECK FOR DUMP REPLY                         
         BNE   SYM14                                                            
         ABEND 666,DUMP                                                         
*                                                                               
SYM14    CLC   REPLY(3),=C'EOJ'    CHECK FOR EOJ REPLY                          
         BNE   SYM16                                                            
         ABEND 666                                                              
*                                                                               
SYM16    MVC   XTRAMESS,XSPACES    CLEAR THESE OUT                              
         MVC   XTRAMES2,XSPACES                                                 
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OPEN CONTROL FILE IF REQUIRED                            *         
***********************************************************************         
OPENCTFL NTR1  BASE=*,LABEL=*                                                   
         CLI   CTOPEN,YES                                                       
         BE    EXITOK                                                           
         MVI   CTOPEN,YES                                                       
         GOTO1 VDMGR,DMCB,DMOPEN,CONTROL,CTFLIST,AIO,0                          
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THE OPERATOR HAS INTERRUPTED WITH EITHER A 'STOP' OR 'MODIFY'       *         
* COMMAND.  EXAMINE THE COMMAND AND TAKE THE APPROPRIATE ACTION.      *         
***********************************************************************         
CHKOPER  NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,19                                                            
         BRAS  RE,SYSMESS          OPERATOR COMMAND MESSAGE                     
*                                                                               
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         L     R2,COMCIBPT         A(CIB)                                       
         LA    R3,COMCIBPT         A(A(CIB))                                    
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         BNE   CH02                                                             
         MVI   OPERSTOP,YES        YES -- SET STOP FLAG                         
         LHI   R0,20                                                            
         BRAS  RE,SYSMESS          OUTPUT OPERATOR STOP MESSAGE                 
         B     CHX                                                              
*                                                                               
CH02     CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER 'MODIFY'?                 
         BE    *+6                 YES                                          
         DC    H'0'                WHAT DID THE OPERATOR DO?                    
*                                                                               
         LA    R4,COMMTAB                                                       
         USING COMMTABD,R4                                                      
         XR    RF,RF                                                            
CH04     CLI   0(R4),X'FF'         EOT                                          
         BE    CHBAD               BAD COMMAND                                  
*                                                                               
         IC    RF,COMMLEN          GET MINIMUM LENGTH                           
         CH    RF,CIBDATLN         CHECK STRING LENGTH                          
         BL    CH06                                                             
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8              MATCH COMMAND IN TABLE                       
         BE    CH08                PASS                                         
         CLC   COMMCMD(0),CIBDATA                                               
*                                                                               
CH06     AHI   R4,COMMTABL         NEXT ENTRY                                   
         B     CH04                                                             
*                                                                               
CH08     ICM   RF,15,COMMRTN       GET PROCESSING ROUTINE                       
         BASR  RE,RF                                                            
         B     CHX                                                              
*                                                                               
CHBAD    LHI   R0,21                                                            
         BRAS  RE,SYSMESS          OUTPUT UNKNOWN COMMAND MESSAGE               
*                                                                               
CHX      L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         LA    R3,COMCIBPT         A(A(CIB))                                    
         QEDIT ORIGIN=(R3),BLOCK=(R2)  FREE THE CIB                             
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
COMMTAB  DC    CL8'????????',AL1(8,0,0,0),AL4(OPDUMMY)                          
         DC    X'FF'                                                            
*                                                                               
OPDUMMY  BR    RE                                                               
         LTORG                                                                  
***********************************************************************         
* Command table DSECT                                                           
***********************************************************************         
COMMTABD DSECT ,                   COVERS COMMTAB ABOVE                         
COMMCMD  DS    CL8                 INPUT COMMAND                                
COMMLEN  DS    X                   MINIMUM LENGTH                               
         DS    XL3                                                              
COMMRTN  DS    AL4                 A(PROCESSING ROUTINE)                        
COMMTABL EQU   *-COMMTABD                                                       
         EJECT                                                                  
***********************************************************************         
* SENT MQ MESSAGE TO LOG FOR THIS DESTINATION                         *         
* NTRY:  R7    = DSTAEFTD                                             *         
***********************************************************************         
* MSG_ACT     3  ACTION                       <CRLF>         Required           
* MSG_REF#   27  REFERENCE NUMBER             <CRLF>         Required           
* MSG_METH    1  METHOD                       <CRLF>         Required           
* MSG_ACC#    8  ACCOUNT # / BDF file         <CRLF>         N/A                
* MSG_BFN     9  BDF FILE NAME                <CRLF>                            
* MSG_DTT     1  DESTINATION TYPE (R/F)       <CRLF>                            
* MSG_HDR    80  HEADER                       <CRLF>         N/A                
* MSG_TRN    80  TRN CARD                     <CRLF>                            
* MSG_MQN    80  MQ QUEUE ID # (CTL/MQDEF)    <CRLF>         Optional           
* MSG_PQS    80  FINISHED PQ CLASS/STATUS     <CRLF>         Optional           
* MSG_DST    25  DESTINATION NAME             <CRLF>                            
* MSG_FDST   16  FORMATTED DESTINATION NAME   <CRLF>                            
* MSG_PQRT    2  PQ REPORT TYPE               <CRLF>                            
* MSG_BSID    1  BDE SENDER ID                <CRLF>         N/A                
* MSG_UUID   80  Unique Id (PDF only for now) <CRLF>         Optional           
* MSG_EMAIL  80  Email Address                <CRLF>         Optional           
***********************************************************************         
EDIPDF   CSECT                                                                  
         USING DSTAEFTD,R7                                                      
POSTSENT NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
*                                                                               
         MVC   0(3,R3),=C'ADD'                                                  
         TM    DSTAEFLG,DSTAEFJQ   IS THIS DEST GOOD?                           
         BNO   *+10                YES                                          
         MVC   0(3,R3),=C'JNK'                                                  
         AHI   R3,3                                                             
         BRAS  RE,ADDCRLF                                                       
*                                                                               
*        BRAS  RE,BLDXREF          REFERENCE NUMBER RETURNS IN REFNUM           
*                                                                               
         MVC   0(L'REFNUM,R3),REFNUM                                            
         AHI   R3,L'REFNUM                                                      
         BRAS  RE,ADDCRLF                                                       
*                                  SET METH=PDF                                 
         MVI   0(R3),EDIPDFQ       C'O' PDF                                     
         AHI   R3,1                                                             
         BRAS  RE,ADDCRLF                                                       
*                                                                               
*        MVC   0(8,R3),=CL8'000001'                                             
*        AHI   R3,8                                                             
*        BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   0(L'DSTTYPE,R3),DSTTYPE                                          
         AHI   R3,L'DSTTYPE                                                     
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   0(80,R3),HDR        WE WANT ALL THE *HDR* CARD                   
         AHI   R3,80                                                            
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   0(80,R3),TRN        WE WANT ALL THE ++TRN CARD                   
         AHI   R3,80                                                            
         BRAS  RE,ADDCRLF                                                       
*                                                                               
*        OC    MQNLEN,MQNLEN                                                    
*        BZ    POSTSN20            OPTIONAL                                     
*        MVC   0(80,R3),MQN        WE WANT ALL THE ++MQN CARD IF THERE          
*        AHI   R3,80                                                            
*                                                                               
POSTSN20 BRAS  RE,ADDCRLF                                                       
*                                                                               
*        OC    PQSLEN,PQSLEN                                                    
*        BZ    POSTSN22            OPTIONAL                                     
*        MVC   0(80,R3),PQS        WE WANT ALL THE ++PQS CARD IF THERE          
*        AHI   R3,80                                                            
*                                                                               
POSTSN22 BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   0(L'DSTAEDST,R3),DSTAEDST                                        
         AHI   R3,L'DSTAEDST                                                    
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   0(L'DSTAEFRM,R3),DSTAEFRM                                        
         AHI   R3,L'DSTAEFRM                                                    
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   0(L'RPTPQTYP,R3),RPTPQTYP                                        
         AHI   R3,L'RPTPQTYP                                                    
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVI   0(R3),C' '          No BDE Sender id (should be opt)             
         AHI   R3,L'RPTBSID                                                     
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   0(80,R3),UID        We want all the ++UDID card if there         
         AHI   R3,80                                                            
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   0(80,R3),XSPACES                                                 
         LLC   RF,EMAILLN                                                       
         BCTR  RF,0                                                             
         EXRL  RF,MVCEML                                                        
         LA    R3,1(RF,R3)                                                      
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         LA    RF,WORK                                                          
         ST    RF,MQPUTBUF                                                      
         SR    R3,RF                                                            
         ST    R3,DATALEN                                                       
*                                                                               
         MVC   MQPUTHOB,=A(LOGQHOB)                                             
         LA    R2,MQPUT                                                         
         BRAS  RE,CALLMQ                                                        
         LA    R2,MQCMIT           MQ COMMIT                                    
         BRAS  RE,CALLMQ                                                        
*                                                                               
         XC    EACTION,EACTION                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* GET INFORMATION FROM USER ID RECORD                                 *         
***********************************************************************         
GETIDN   NTR1  BASE=*,LABEL=*                                                   
         XC    AGYPOWER,AGYPOWER                                                
         XC    SVAGYPOW,SVAGYPOW                                                
         XC    AGYNAME,AGYNAME                                                  
         XC    AGYADDR,AGYADDR                                                  
*                                                                               
         XC    KEY,KEY             BUILD ID RECORD KEY                          
X        USING CTIREC,KEY                                                       
         MVI   X.CTIKTYP,CTIKTYPQ                                               
         MVC   X.CTIKNUM,PQUSER#                                                
         GOTO1 VDMGR,DMCB,DMREAD,CTFILE,KEY,AIO,0                               
         CLI   8(R1),0             USERID NOT FOUND                             
         BNE   EXITL                                                            
         DROP  X                                                                
*                                                                               
         L     R4,AIO                                                           
         AHI   R4,28                                                            
         XR    RF,RF                                                            
*                                                                               
         USING CTDSCD,R4                                                        
GIDN02   CLI   0(R4),0                                                          
         BE    EXITL                                                            
         CLI   0(R4),CTAGYELQ      GET AGENCY POWERCODE                         
         BNE   GIDN04                                                           
*                                                                               
         MVC   AGYPOWER,CTAGYID-CTAGYD(R4)                                      
         MVC   SVAGYPOW,AGYPOWER                                                
*                                                                               
GIDN04   CLI   0(R4),CTORGELQ      GET ORIGIN DETAILS                           
         BNE   GIDN08                                                           
         MVC   AGYNAME,CTORGNAM-CTORGD(R4)                                      
         MVC   AGYADDR,CTORGADD-CTORGD(R4)                                      
*                                                                               
GIDN08   IC    RF,CTDSCLEN                                                      
         BXH   R4,RF,GIDN02                                                     
         DC    H'00'                                                            
         EJECT                                                                  
***********************************************************************         
* CREATE UNIQUE REFERENCE NUMBER FOR ETI FILE                         *         
* NTRY: R7 = A(CURRENT DEST IN THE TABLE)                             *         
* EXIT: REFNUM    = REFERENCE NUMBER                                  *         
***********************************************************************         
BLDXREF  NTR1  BASE=*,LABEL=*                                                   
         MVC   REFNUM,XSPACES                                                   
         USING XREFKEYD,R2                                                      
         LA    R2,REFNUM                                                        
         MVI   XREFVER#,VERSION1   VERSION 1                                    
         MVC   XREFDSPC,DSPACE     ADV/REP                                      
         CLI   DRTEST,YES                                                       
         BNE   *+8                                                              
         MVI   XREFDSPC,C'D'          DR                                        
*                                                                               
         MVC   XREFUSRD,USERIDH                                                 
         MVC   XREFSUBD,RPTSUBID                                                
         MVC   XREFRPT#,RPTPQNUM                                                
         MVC   XREFCDAT,RPTHDATE                                                
         MVC   XREFCTIM,RPTHTIME                                                
         MVC   XREFLOG#,RPTLOGNO                                                
*                                                                               
         LR    RF,R7                                                            
         L     RE,ADSTAEFT                                                      
         SR    RF,RE               ADDRESS DIFFERENCE                           
         SR    RE,RE               PREPARE FOR DIVIDE                           
         LHI   R1,DSTAEFTL                                                      
         DR    RE,R1                                                            
         AHI   RF,1                DEST#                                        
         STCM  RF,1,RPTLDSTS                                                    
*                                                                               
         LR    RF,R7                                                            
         AHI   RF,DSTAEFTL         TEST THIS IS LAST DESTINATION                
         CLI   0(RF),X'FF'         EOT?                                         
         BE    *+14                                                             
         OC    DSTAEDST-DSTAEFTD(L'DSTAEDST,RF),DSTAEDST-DSTAEFTD(RF)           
         BNZ   *+8                                                              
         OI    RPTLDSTS,X'80'      FLAG AS LAST DESTINATION                     
*                                                                               
         GOTO1 VHEXO31,DMCB,RPTLDSTS,XREFDES#,L'RPTLDSTS,0                      
         AHI   R2,XREFLNQ                                                       
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* COMMON STORAGE/ROUTINES                                             *         
***********************************************************************         
         ORG   EDIPDF+(((*-EDIPDF)/4096)+1)*4096                                
COMMON   DC    CL8'*COMMON*'                                                    
SAVERD   DC    F'0'                                                             
*                                                                               
*                                                                               
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
EXITR3   CR    RB,RB                                                            
         XIT1  REGS=(R3)                                                        
*                                                                               
EXITR7   CR    RB,RB                                                            
         XIT1  REGS=(R7)                                                        
*                                                                               
XBASE    L     RD,SAVERD                                                        
         XBASE ,                                                                
*                                                                               
ADDCRLF  MVC   0(L'CRLF,R3),CRLF                                                
         AHI   R3,L'CRLF                                                        
         BR    RE                                                               
*                                                                               
*                                                                               
LINESUP  NTR1  ,                   INCREMENT LINE COUNT AND LOG IF REQ          
         L     R0,NUMLINES                                                      
         AHI   R0,1                                                             
         ST    R0,NUMLINES                                                      
*                                                                               
         CLI   TRACE,C'F'          FULL TRACE PRINTS ALL LINES                  
         BNE   EXITOK                                                           
         ICM   R7,15,LASTOUT                                                    
         BNZ   *+8                                                              
         L     R7,AMSGOUT                                                       
         ST    R3,LASTOUT                                                       
*                                                                               
         LR    R1,R3                                                            
         SR    R1,R7                                                            
         BM    EXITOK                                                           
LINESUP2 CHI   R1,L'PLINE          Can only print so much at time               
         BNH   LINESUP4                                                         
         MVC   PLINE,0(R7)                                                      
         BRAS  RE,PRNT                                                          
         SHI   R1,L'PLINE                                                       
         AHI   R7,L'PLINE                                                       
         B     LINESUP2                                                         
*                                                                               
LINESUP4 BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PLINE(0),0(R7)                                                   
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
ARZERO   DC    16F'0'                                                           
*                                                                               
         DS    0D                                                               
         DC    CL8'HCONN==>'                                                    
HCONN    DC    F'0',F'0'           MQ QMGR CONNECTION HANDLE                    
*                                                                               
         DC    CL8'QMGR===>'                                                    
QMGR     DC    CL48'MQ1P'                                                       
*                                                                               
         DC    CL8'MSGBUFFS'                                                    
AMSGIN   DC    A(0)                INPUT MESSAGE BUFFER                         
AMSGOUT  DC    A(0)                OUTPUT MESSAGE BUFFER                        
MAXMSGLN DC    A(4*K*K)            MAX MESSAGE LENGTH                           
*                                                                               
         LTORG                                                                  
*                                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
VERSION1 EQU   C'1'                                                             
K        EQU   1024                                                             
*                                                                               
MQINIFLG DC    AL1(NO)             SET TO Y WHEN INITIALISED                    
TRACE    DC    AL1(NO)                                                          
GOTHDR   DC    AL1(NO)                                                          
GOTDATA  DC    AL1(NO)                                                          
SKIPBXF  DC    AL1(NO)                                                          
DRTEST   DC    AL1(NO)                                                          
DEBUG    DC    AL1(NO)                                                          
SENDDLNS DC    AL1(YES)            'Y' IF WE WANT DLNS FROM PDF NODE            
LOG      DC    AL1(NO)             'Y' FOR FULL LOGGING                         
FNAMELN  DS    X                                                                
FNAME    DS    CL15                                                             
LNAMELN  DS    X                                                                
LNAME    DS    CL40                                                             
EMAILLN  DS    X                                                                
EMAIL    DS    CL52                                                             
PIDLN    DS    X                                                                
PID      DS    CL8                                                              
USERNLN  DS    X                                                                
USERNME  DS    CL10                                                             
DSPACE   DC    C' '                                                             
*                                                                               
JREPMAX  DC    F'10'               MAX # OF JUNK REP ALLOWED                    
JREPCNT  DC    F'1'                # OF JUNK REP COUNTER                        
*                                                                               
VADDAY   DC    V(ADDAY)                                                         
VCPRINT  DC    V(CPRINT)                                                        
VCARDS   DC    V(CARDS)                                                         
VMODLIB  DC    V(MODLIB)                                                        
VDMGR    DC    V(DATAMGR)                                                       
VDADDS   DC    V(DADDS)                                                         
VDATCON  DC    V(DATCON)                                                        
VHELLO   DC    V(HELLO)                                                         
VPRINTER DC    V(PRINTER)                                                       
VHEXOUT  DC    V(HEXOUT)                                                        
VHEXO31  DC    V(HEXO31)                                                        
VHEXIN   DC    V(HEXIN)                                                         
VHEXI31  DC    V(HEXI31)                                                        
VLOGIO   DC    V(LOGIO)                                                         
VSCANNER DC    V(SCANNER)                                                       
*                                                                               
ASYSMSGS DC    A(SYSMSGS)                                                       
ALINETAB DC    A(LINETAB)                                                       
ASCANTAB DC    A(SCANTAB)                                                       
AERRMSG  DC    A(ERRMSGS)                                                       
ACARDTAB DC    A(CARDTAB)                                                       
ACARDSIN DC    A(CARDSIN)                                                       
*                                                                               
ATSTIDS  DC    A(TSTIDTAB)                                                      
ATSTAGS  DC    A(TSTAGTAB)                                                      
ADSTAEFT DC    A(DSTAEFT)                                                       
AWHY     DC    A(MQ_REASON_CODE_TABLE)                                          
ASSB     DC    A(SSB)                                                           
AUTL     DC    A(UTL)                                                           
AIO      DC    A(IO)                                                            
*                                                                               
LASTOUT  DC    A(0)                                                             
AOPERECB DC    A(0)                A(ECB OF OPERATOR INTERRUPT)                 
ACOMM    DC    A(0)                A(COMMUNICATIONS PARAMETER LIST)             
*                                                                               
APAGES   DS    A                                                                
ALINES   DS    A                                                                
*                                                                               
MESSAGE  DS    CL60                                                             
CHARTAB  DS    CL256                                                            
NEWPAGE  DS    0CL7                                                             
         DC    C'/PAGE',X'0D25'                                                 
*                                                                               
FORMFEED DC    X'0D0C'                                                          
*                                                                               
SINGLESP DS    0XL2                                                             
CRLF     DC    XL2'0D25'          EBCDIC  CR / LF                               
DOUBLESP DC    XL4'0D250D25'                                                    
TRIPLESP DC    XL6'0D250D250D25'                                                
*                                                                               
XTRAMESS DS    XL8'00'                                                          
XTRAMES2 DS    XL18'00'                                                         
*                                                                               
LANDSCAP DC    C'LANDSCAPE'                                                     
PORTRAIT DC    C'PORTRAIT'                                                      
ORIENT   DS    C                   L or P                                       
*                                                                               
DMOPEN   DC    CL8'DMOPEN  '                                                    
DMREAD   DC    CL8'DMREAD  '                                                    
DMRDHI   DC    CL8'DMRDHI  '                                                    
DMRSEQ   DC    CL8'DMRSEQ  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
GENDIR   DC    CL8'GENDIR  '                                                    
GENFIL   DC    CL8'GENFIL  '                                                    
*                                                                               
CTOPEN   DC    AL1(NO)                                                          
CONTROL  DC    CL8'CONTROL '                                                    
CTFLIST  DC    C'NCTFILE NGENDIR NGENFIL X'                                     
*                                                                               
P99      DC    P'99'                                                            
*                                                                               
         DS    0D                                                               
         DC    CL08'ECBLIST*'                                                   
ECBLST   DC    A(0)                A(OPERATOR ECB IS STORED HERE)               
         DC    X'00',AL3(GETECB)                                                
         DC    X'80',AL3(TIMERECB) TIMER POP                                    
         DC    CL08'ECBLISTX'                                                   
*                                                                               
GETECB   DC    A(0),CL12'<===GETECB'                                            
*                                                                               
         DC    CL8'RLEN===>'                                                    
RLEN     DS    H                                                                
R        DS    XL256                                                            
         DC    CL8'HDRLEN=>'                                                    
HDRLEN   DS    H                                                                
HDR      DS    XL256                                                            
*                                                                               
STARS    DC    80C'*'                                                           
XSPACES  DC    256C' '                                                          
*                                                                               
ERRMSG1  DS    0C                                                               
         DC    C'AUTONOTE*US-MF_FAC_NOTIFY:'                                    
ERRMSG1R DS    CL(L'JUNKRPT+L'DSTAEDST)                                         
ERRMSG1Q EQU   *-ERRMSG1                                                        
         EJECT                                                                  
***********************************************************************         
* PARAMETER LISTS TO FACILITATE MQ CALLS                              *         
* CL16 EBCDIC  ROUTINE NAME                                           *         
* A    A(ROUTINE)                                                     *         
* PARAMETERS (STANDARD IBM FORMAT)                                    *         
***********************************************************************         
         DS    0D                                                               
MQCONN   DC    CL16'MQ QMGR connect'                                            
AMQCONN  DC    A(0)                                                             
         DC    A(QMGR)                                                          
         DC    A(HCONN)                                                         
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQOPEN   DC    CL16'MQ Open queue'                                              
AMQOPEN  DC    A(0)                                                             
         DC    A(HCONN)                                                         
MQOPNQNM DC    A(0)                CMQODA                                       
         DC    A(OPN_OPTS)                                                      
MQOPNHOB DC    A(0)                HANDLE TO OBJECT (RETURNED)                  
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQCMIT   DC    CL16'MQ Commit'                                                  
AMQCMIT  DC    A(0)                                                             
         DC    A(HCONN)                                                         
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQCLOSE  DC    CL16'MQ Close Queue'                                             
AMQCLOSE DC    A(0)                                                             
         DC    A(HCONN)                                                         
MQCLSHOB DC    A(0)                                                             
         DC    A(CLS_OPTS)                                                      
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQDISC   DC    CL16'MQ Disconnect'                                              
AMQDISC  DC    A(0)                                                             
         DC    A(HCONN)                                                         
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQGET    DC    CL16'MQ Get'                                                     
AMQGET   DC    A(0)                                                             
         DC    A(HCONN)                                                         
MQGETHOB DC    A(INPQHOB)          QUEUE HOB                                    
         DC    A(MSGDESC)                                                       
         DC    A(GETOPTS)                                                       
         DC    A(MAXMSGLN)         MAX BUFFER LENGTH                            
MQGETBUF DC    A(0)                BUFFER ADDRESS                               
         DC    A(DATALEN)                                                       
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQPUT    DC    CL16'MQ Put'                                                     
AMQPUT   DC    A(0)                                                             
         DC    A(HCONN)                                                         
MQPUTHOB DC    A(OUTQHOB)          QUEUE NAME                                   
         DC    A(MSGDESC)                                                       
         DC    A(PUTOPTS)                                                       
MQPUTLEN DC    A(DATALEN)                                                       
MQPUTBUF DC    A(0)                                                             
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
OPN_OPTS DS    F                   MQOPEN OPTIONS                               
CLS_OPTS DS    F                   MQCLOSE OPTIONS                              
MQ_CC    DS    F                   COMPLETION CODE                              
MQ_RC    DS    F                   QUALIFIES COMPLETION CODE                    
DATALEN  DS    F                   LENGTH OF THE MESSAGE                        
*                                                                               
         DS    0D                                                               
         DC    CL8'INPQ===>'                                                    
INPQHOB  DC    F'0',C'<==>'                                                     
INPQ     CMQODA  LIST=YES          OBJECT DESCRIPTOR                            
*                                                                               
         DS    0D                                                               
         DC    CL8'OUTQ===>'                                                    
OUTQHOB  DC    F'0',C'<==>'                                                     
OUTQ     CMQODA  LIST=YES          OBJECT DESCRIPTOR                            
*                                                                               
         DS    0D                                                               
         DC    CL8'LOGQ===>'                                                    
LOGQHOB  DC    F'0',C'<==>'                                                     
LOGQ     CMQODA  LIST=YES          OBJECT DESCRIPTOR                            
*                                                                               
         DS    0D                                                               
         DC    CL8'MSGDESC>'                                                    
MSGDESC  CMQMDA  LIST=YES          MESSAGE DESCRIPTOR                           
*                                                                               
         DS    0D                                                               
         DC    CL8'GETOPTS>'                                                    
GETOPTS  CMQGMOA LIST=YES          GET MESSAGE OPTIONS                          
*                                                                               
         DS    0D                                                               
         DC    CL8'PUTOPTS>'                                                    
PUTOPTS  CMQPMOA DSECT=NO,LIST=YES PUT MESSAGE OPTIONS                          
         EJECT                                                                  
***********************************************************************         
* TABLE OF INPUT LINES ON DDS CONTROL CARDS                           *         
***********************************************************************         
LINETAB  DS    0F                                                               
*        DC    CL3'HDR',X'00'                                                   
*DRLEN   DS    H                                                                
*DR      DS    XL256                                                            
         DC    CL3'UID',X'00'                                                   
UIDLEN   DS    H                                                                
UID      DS    XL256                                                            
         DC    CL3'SUB',X'00'                                                   
SUBLEN   DS    H                                                                
SUB      DS    XL256                                                            
         DC    CL3'RCP',X'00'                                                   
RCPLEN   DS    H                                                                
RCP      DS    XL256                                                            
         DC    CL3'FIL',X'00'                                                   
FILLEN   DS    H                                                                
FIL      DS    XL256                                                            
         DC    CL3'EXT',X'00'                                                   
EXTLEN   DS    H                                                                
EXT      DS    XL256                                                            
         DC    CL3'DSN',X'00'                                                   
DSNLEN   DS    H                                                                
DSN      DS    XL256                                                            
         DC    CL3'TRN',X'00'                                                   
TRNLEN   DS    H                                                                
TRN      DS    XL256                                                            
         DC    CL3'DXC',X'00'                                                   
DXCLEN   DS    H                                                                
DXC      DS    XL256                                                            
         DC    CL3'MQN',X'00'                                                   
MQNLEN   DS    H                                                                
MQN      DS    XL256                                                            
         DC    CL3'PQS',X'00'                                                   
PQSLEN   DS    H                                                                
PQS      DS    XL256                                                            
         DC    X'FF'                                                            
*                                                                               
LINETABD DSECT                                                                  
LINEID   DS    CL3                                                              
         DS    C                                                                
LINELEN  DS    H                                                                
LINEIN   DS    CL256                                                            
LINETABL EQU   *-LINETABD                                                       
*                                                                               
         EJECT                                                                  
***********************************************************************         
* NON-ADDRESSIBLE STORAGE AREAS                                       *         
***********************************************************************         
EDIPDF   CSECT                                                                  
         DS    0D                                                               
         DC    CL16'DSTADST+DSTADST+'                                           
DSTAEFT  DS    (DSTMAXQ)XL(DSTAEFTL)                                            
         DC    X'FFFFFFFF'                                                      
         DC    CL16'DSTADST-DSTADST-'                                           
DSTMAXQ  EQU   150                                                              
*                                                                               
DSTAEFTD DSECT                                                                  
DSTTYPE  DS    C                   DESTINATION TYPE (PDF)                       
DSTAEFLG DS    X                   STATUS                                       
DSTAEFJQ EQU   X'80'               JUNK                                         
DSTAEDST DS    CL25                PDF DESTINATION                              
DSTAEFRM DS    CL16                FORMATTED DESTINATION                        
DSTAEFTL EQU   *-DSTAEFTD                                                       
*                                                                               
EDIPDF   CSECT                                                                  
*                                                                               
         DS    0D                                                               
         DC    CL16'SCANTAB+SCANTAB+'                                           
SCANTAB  DS    (PARMCNTQ)CL(SCBLKLQ)                                            
         DC    CL16'SCANTAB-SCANTAB-'                                           
*                                                                               
         DS    0D                                                               
         DC    CL16'SSB+SSB+SSB+SSB+'                                           
SSB      DC    XL2'0000',X'FF',1021X'00'                                        
         DC    CL16'SSB-SSB-SSB-SSB-'                                           
*                                                                               
         DS    0D                                                               
         DC    CL16'UTL+UTL+UTL+UTL+'                                           
UTL      DC    F'0',X'0A',251X'00'                                              
         DC    CL16'UTL-UTL-UTL-UTL-'                                           
*                                                                               
         DS    0D                                                               
         DC    CL16'+IO++IO++IO++IO+'                                           
IO       DC    2048X'00'           CTFILE/GENFIL I/O AREA                       
         DC    CL16'-IO--IO--IO--IO-'                                           
                                                                                
***********************************************************************         
* &, >, < are invalid for XML. Need to replace with                             
* (all are followed by a semicolon which I can not type in this editor          
* &amp                                                                          
* &GT                                                                           
* &LT                                                                           
***********************************************************************         
XMLTRTAB DC    XL256'00'                                                        
         ORG   XMLTRTAB+C'&&'                                                   
         DC    X'FF'                                                            
         ORG   XMLTRTAB+C'>'                                                    
         DC    X'FF'                                                            
         ORG   XMLTRTAB+C'<'                                                    
         DC    X'FF'                                                            
         ORG                                                                    
*                                                                               
         EJECT                                                                  
***********************************************************************         
* INCLUDED DATA                                                       *         
***********************************************************************         
* FACTRYXLAT                                                                    
         PRINT OFF                                                              
       ++INCLUDE FACTRYXLAT                                                     
         PRINT ON                                                               
*DDMQREASON                        MQ REASON CODES                              
         PRINT OFF                                                              
       ++INCLUDE DDMQREASON                                                     
         PRINT ON                                                               
*TSTIDTAB                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDEDITSTID                                                     
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* VALID INPUT CARD TABLE                                              *         
***********************************************************************         
         DS    0D                                                               
CARDTAB  DC    CL10'DSPACE    ',AL1(5,5,0,0),AL4(VCDSPACE)                      
         DC    CL10'DDSIO     ',AL1(4,4,0,0),AL4(VCDDSIO)                       
         DC    CL10'DEBUG     ',AL1(4,4,0,0),AL4(VCDEBUG)                       
         DC    CL10'DRTEST    ',AL1(5,5,0,0),AL4(VCDR)                          
         DC    CL10'TRACE     ',AL1(4,4,0,0),AL4(VCTRACE)                       
         DC    CL10'LOGGING   ',AL1(6,6,0,0),AL4(VCLOG)                         
         DC    CL10'DLNS      ',AL1(3,3,0,0),AL4(VCDLNS)                        
         DC    CL10'QMGR      ',AL1(3,3,CFBIG,0),AL4(VCQMGR)                    
         DC    CL10'JUNKREP   ',AL1(6,6,0,0),AL4(VJREP)                         
         DC    X'FF'                                                            
*                                                                               
CARDSIN  DC    15CL80' '                                                        
         DC    X'FF'                                                            
*                                                                               
CARDTABD DSECT                     INPUT CARD PARAMETERS                        
CARDTXT  DS    CL10                TEXT                                         
CMIN     DS    X                   MIN LEN FOR EXECUTE (-1)                     
CMAX     DS    X                   MAX LEN FOR EXECUTE (-1)                     
CFLAG    DS    X                   FLAGS                                        
CFBIG    EQU   X'80'                                                            
         DS    X                   N/D                                          
CARDVAL  DS    AL4                 A(VALIDATION ROUTINE)                        
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
EDIPDF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGE TABLE - NEED NOT BE ADDRESSIBLE                       *         
***********************************************************************         
ERRMSGS  DS    0CL45                                                            
EMSG01   DC    CL45'Undefined parameter card                    '               
EMSG02   DC    CL45'DSPACE= parameter can only be 1 byte long   '               
EMSG03   DC    CL45'DDSIO= parameter cannot be more than 8 long '               
EMSG04   DC    CL45'Input line is not valid for SCANNER         '               
EMSG05   DC    CL45'Too many parameters in total                '               
EMSG06   DC    CL45'The parameter must be a number              '               
EMSG07   DC    CL45'This userid is not valid                    '               
EMSG08   DC    CL45'This userid has no 02 element - this is bad '               
EMSG09   DC    CL45'Too many filters - this one is ignored      '               
EMSG10   DC    CL45'This user has no EDICT record               '               
EMSG11   DC    CL45'Unable to open print queue report           '               
EMSG12   DC    CL45'Unable to write first line to PQ            '               
EMSG13   DC    CL45'Unable to write page eject to PQ            '               
EMSG14   DC    CL45'Print line is too long to handle            '               
EMSG15   DC    CL45'Unable to write print line to PQ            '               
EMSG16   DC    CL45'Unable to close print queue report          '               
EMSG17   DC    CL45'Must erase partial PQ report                '               
EMSG18   DC    CL45'Unable to find incoming EDICT user id       '               
EMSG19   DC    CL45'TYPE= must be A(DV) or R(EP)                '               
EMSG20   DC    CL45'Unknown ++DDS card - please check           '               
EMSG21   DC    CL45'Missing ++UID card - please check           '               
EMSG22   DC    CL45'Person record not found - SA0REC            '               
EMSG23   DC    CL45'Missing PID number                          '               
EMSG24   DC    CL45'Person record not found - SAPEREC           '               
         EJECT                                                                  
***********************************************************************         
* CONSOLE MESSAGE TABLE - NEED NOT BE ADDRESSIBLE                     *         
***********************************************************************         
SYSMSGS  DS    0CL50                                                            
SMSG01   DC    CL50'XBeginning initialisation                      '            
SMSG02   DC    CL50'XCompleted initialisation                      '            
SMSG03   DC    CL50'XBeginning reading input cards                 '            
SMSG04   DC    CL50'XCompleted reading input cards                 '            
SMSG05   DC    CL50'XBeginning validating input cards              '            
SMSG06   DC    CL50'XCompleted validating input cards              '            
SMSG07   DC    CL50'XBeginning setting operator comms              '            
SMSG08   DC    CL50'XCompleted setting operator comms              '            
SMSG09   DC    CL50'XBeginning MQ Initialisation                   '            
SMSG10   DC    CL50'XCompleted MQ Initialisation                   '            
SMSG11   DC    CL50'XObtained  MQ QMGR handle                      '            
SMSG12   DC    CL50'XOpened    MQ Master Queue                     '            
SMSG13   DC    CL50'XOpened    MQ Log Queue                        '            
SMSG14   DC    CL50'XBeginning MQ Deallocation                     '            
SMSG15   DC    CL50'XCompleted MQ Deallocation                     '            
SMSG16   DC    CL50'XClosed    MQ Master Queue                     '            
SMSG17   DC    CL50'XClosed    MQ Log Queue                        '            
SMSG18   DC    CL50'X** WARNING ** Logging Suppressed              '            
SMSG19   DC    CL50'XIncoming operator command                     '            
SMSG20   DC    CL50'X** WARNING ** Operator requested "STOP"       '            
SMSG21   DC    CL50'X** WARNING ** Unknown operator command ignored'            
SMSG22   DC    CL50'XOpened all subsidiary MQ Queues               '            
SMSG23   DC    CL50'XClosed all subsidiary MQ Queues               '            
SMSG24   DC    CL50'XNUMPRTS inadequate - abending call programmer '            
SMSG25   DC    CL50'X** ERROR ** Unknown userid XXXXXXXXXXXXXXXXXX '            
SMSG26   DC    CL50'XBeginning building PQ list                    '            
SMSG27   DC    CL50'XCompleted building PQ list                    '            
SMSG28   DC    CL50'X** ERROR ** Mailbox XXXXXXXX is unknown       '            
SMSG29   DC    CL50'4** ERROR ** DSTTAB size inadequate must ABEND '            
SMSG30   DC    CL50'XPDF -- Transmissions suspended (MQ issue)     '            
         EJECT                                                                  
***********************************************************************         
* W/S AREA                                                            *         
***********************************************************************         
         ORG   EDIPDF+(((*-EDIPDF)/4096)+1)*4096                                
         DC    CL16'*WORKD***WORKD**'                                           
WORKD    DS    0X                                                               
DUB      DS    D                                                                
DMWORK   DS    12D                                                              
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE1    DS    X                                                                
BYTE2    DS    X                                                                
*                                                                               
PLIST    DS    8F                                                               
DMCB     DS    8F                                                               
XMLPARM  DS    8F                                                               
*                                                                               
PACKOF4B DS    PL4                 KEEP FULLWORD ALIGNED                        
WORK     DS    XL256               DITTO                                        
*                                                                               
NUMLINES DS    F                   NUMBER OF TRANSMITTED LINES/REPORT           
NUMPAGES DS    H                   PAGE COUNTER FOR PDF TRANSMISSION            
*                                                                               
REQUESTR DS    CL3                                                              
AGYPOWER DS    CL2                 AGENCY POWER CODE                            
SVAGYPOW DS    CL2                 SAVED AGENCY POWER CODE                      
AGYNAME  DS    CL33                                                             
AGYADDR  DS    CL33                                                             
PDFDATA  DS    XL21                PDF-SPECIFIC TRANSFER DATA                   
ROUTCODE DS    CL5                 DARE ROUTING CODE                            
MEDIA    DS    C                   DARE MEDIA                                   
REFNUM   DS    CL27                VDUUUUSSSNNNNNTTTTTTTTLLLLSS                 
DESTNTRY DS    XL(DESTTBLQ)        TEMP STORAGE FOR 1 DESTTBL  ENTRY            
*                                                                               
DSTNAME  DS    CL8                 DESTINATION NAME                             
DSTUIDN  DS    XL2                 USERID                                       
*ONGFXPG DS    C                   'Y' = WE SHOULD SEND 'FAXL' COMMAND          
LASTEJCT DS    C                   'Y' = LAST LINE PRINTED WAS /PAGE            
*                                                                               
LSTSDST  DS    C                                                                
*                                                                               
YYYYMMDD DS    CL8                 DATE                                         
HHMMSS   DS    CL6                 TIME                                         
*                                                                               
REALBDT  DS    XL6                 YMDHMS binary                                
         ORG   REALBDT                                                          
REALBYY  DS    X                   Binary year                                  
REALBMO  DS    X                   Binary month                                 
REALBDD  DS    X                   Binary day                                   
*                                                                               
REALBHH  DS    X                   Binary hours                                 
REALBMM  DS    X                   Binary minutes                               
REALBSS  DS    X                   Binary seconds                               
         ORG                                                                    
YMDHMS   DS    CL19                YYYY-MM-DD HH:MM:SS                          
         ORG   YMDHMS                                                           
YMD      DS    CL10                                                             
         DC    C' '                                                             
HH       DS    CL2                                                              
         DC    C':'                                                             
MM       DS    CL2                                                              
         DC    C':'                                                             
SS       DS    CL2                                                              
         ORG                                                                    
*                                                                               
CONOK    DS    X                                                                
OPERSTOP DS    X                                                                
PLINE    DS    CL100               OUTPUT PRINT LINE                            
REPLY    DS    CL8                                                              
CARDIO   DS    CL80                                                             
COVPGLNS DS    H                   NUMBER OF LINES IN COVER PAGE                
ERROR    DS    X                                                                
GOTERR   DS    X                                                                
PARMCNT  DS    X                   NO OF I/P CARDS                              
PARMCNTQ EQU   100                 MAX NUMBER OF I/P CARDS                      
MVSNAME  DS    CL8                                                              
KEY      DS    XL64                FOR CTFILE/GENDIR READS                      
IOKEY    DS    XL24                                                             
*                                                                               
EACTION  DS    X                   ACTION(S)                                    
EACTJNKQ EQU   C'J'                 UNSENDABLE                                  
*                                                                               
EERRORCD DS    X                   ERROR CODE                                   
EDAYNUM  DS    X                   PWOS DAY SENT/DLVRD/CAN                      
*                                                                               
PQUSER#  DS    H                                                                
PQRPT#   DS    H                                                                
PQPID#   DS    H                                                                
SECAGY   DS    CL2                                                              
PQDSPACE DS    C                   Report DSPACE A or R / T,C,Q                 
*                                                                               
USERID   DS    CL8                 REPORT USERID (ALPHA)                        
USERIDH  DS    CL4                 REPORT USERID (HEX AS CHARS)                 
USERIDNO DS    XL2                 REPORT USERID                                
RPTSUBID DS    CL3                 REPORT SUB-ID                                
RPTPQNUM DS    CL4                 REPORT REFERENCE NUMBER                      
RPTHDATE DS    CL4                 Report creation date                         
RPTHTIME DS    CL4                 Report creation time                         
RPTLOGNO DS    CL4                 LOGICAL REPORT SEQ. WITHIN PHYSICAL          
RPTLDSTS DS    XL1                 LOG. REP. DEST. SEQ. WITHIN PHYSICAL         
RPTPQTYP DS    XL2                 PQ REPORT TYPE                               
RPTBSID  DS    C                   BDE Sender Id (N/A)                          
WORKL    EQU   *-WORKD                                                          
*                                                                               
         DS    0D                                                               
         DC    CL16'ENTRYPTSENTRYPTS'                                           
ENTRYPTS DC    Y((ENTRYLSQ-ENTRYSTQ)/60) NUMBER OF TABLE ENTRIES                
         DC    H'60'                     MUST REMAIN AS 60                      
ENTRYSTQ EQU   *                                                                
CSQBCLOS DC    CL8'CSQBCLOS'                                                    
         DC    XL52'00'                                                         
CSQBCOMM DC    CL8'CSQBCOMM'                                                    
         DC    XL52'00'                                                         
CSQBCONN DC    CL8'CSQBCONN'                                                    
         DC    XL52'00'                                                         
CSQBDISC DC    CL8'CSQBDISC'                                                    
         DC    XL52'00'                                                         
CSQBGET  DC    CL8'CSQBGET'                                                     
         DC    XL52'00'                                                         
CSQBOPEN DC    CL8'CSQBOPEN'                                                    
         DC    XL52'00'                                                         
CSQBPUT  DC    CL8'CSQBPUT'                                                     
         DC    XL52'00'                                                         
ENTRYLSQ EQU   *                                                                
*                                                                               
         CMQA    LIST=YES,EQUONLY=NO                                            
         EJECT                                                                  
***********************************************************************         
* RD CHAIN CSECT                                                      *         
***********************************************************************         
WORKAREA CSECT                                                                  
         DC    200000X'00'                                                      
         EJECT                                                                  
***********************************************************************         
* Notify message DSECT Version 1.                                               
* ADD<CRLF>XFRE<CRLF>DDMMMYY HH:MM<CRLF>????????<CRLF>EMAIL<CRLF>               
*                                                                               
* DLN<CRLF>XFRE<CRLF>DDMMMYY HH:MM<CRLF>TRACKING<CRLF>EMAIL<CRLF>               
* CAN<CRLF>XFRE<CRLF>DDMMMYY HH:MM<CRLF>TRACKING<CRLF>ERRNUM<CRLF>              
* REJ<CRLF>XFRE<CRLF>DDMMMYY HH:MM<CRLF>TRACKING<CRLF>ERRNUM<CRLF>              
***********************************************************************         
XREFKEYD DSECT                                                                  
XREFVER# DS    C                   Version 1                                    
XREFDSPC DS    C                   Data Space                                   
XREFUSRD DS    XL4                 User id in hex                               
XREFSUBD DS    XL3                 SUB id                                       
XREFRPT# DS    XL4                 Report number in hex                         
XREFCDAT DS    XL4                 Creation date compressed in hex              
XREFCTIM DS    XL4                 Creation time in hex                         
XREFLOG# DS    XL4                                                              
XREFDES# DS    XL2                                                              
XREFLNQ  EQU   *-XREFKEYD                                                       
***********************************************************************         
* DESTINATION TABLE DSECT (CUT DOWN FROM EDICT)                       *         
***********************************************************************         
DESTTABD DSECT                                                                  
DESTNAME DS    CL8                 DESTINATION NAME                             
DESTUIDN DS    XL2                 DDS USERID NUMBER                            
DESTMETS DS    C                   METHOD OF SENDING TRANSMISSIONS              
DESTMETR DS    C                   METHOD OF RECEVING TRANSMISSIONS             
DESTPDFN DS    CL8                 Not relavant                                 
DESTTBLQ EQU   *-DESTTABD                                                       
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* DDEDICTFIL                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDEDICTFIL                                                     
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* CTGENEDICT                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTGENEDICT                                                     
         PRINT ON                                                               
* CTGENSTAD                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENSTAD                                                      
         PRINT ON                                                               
* CTGENAGRD                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENAGRD                                                      
         PRINT ON                                                               
* EDISCANHDR                                                                    
         PRINT OFF                                                              
       ++INCLUDE EDISCANHDR                                                     
         PRINT ON                                                               
* EDIDESTD                                                                      
         PRINT OFF                                                              
EDIHDRD  DSECT                                                                  
       ++INCLUDE EDIDESTD                                                       
         PRINT ON                                                               
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
* IEZCIB                                                                        
         PRINT OFF                                                              
         DSECT                                                                  
         IEZCIB                                                                 
         PRINT ON                                                               
* IEZCOM                                                                        
         PRINT OFF                                                              
         IEZCOM                                                                 
         PRINT ON                                                               
* DMGREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMGREQUS                                                       
         PRINT ON                                                               
* IHASDWA                                                                       
         PRINT OFF                                                              
         IHASDWA                                                                
         PRINT ON                                                               
* IEFZB4D2                                                                      
         PRINT OFF                                                              
         IEFZB4D2                                                               
         PRINT ON                                                               
* IHAPSA                                                                        
         PRINT OFF                                                              
         IHAPSA                                                                 
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003EDIPDF    10/26/20'                                      
         END                                                                    
