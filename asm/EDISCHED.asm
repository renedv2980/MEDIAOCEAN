*          DATA SET EDISCHED   AT LEVEL 014 AS OF 11/15/19                      
*PHASE EDSCHDA                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE FATABOFF                                                               
*INCLUDE DMDMGRL                                                                
*INCLUDE SCANNER                                                                
*INCLUDE NUMVAL                                                                 
*INCLUDE RUNIT                                                                  
*INCLUDE DATCON                                                                 
*                                                                               
**********************************************************************          
* PROGRAM is used to route MQ messages based on a 16 byte header.               
* Note: if we ever need to read distigush between ADV and REP then              
*       we would need to run a seperate version with DSPACE=R in                
*       the parm deck.                                                          
**********************************************************************          
EDISCHED START                                                                  
         PRINT NOGEN                                                            
         NBASE 0,EDISCHED,RA,R9,WORK=VWRKAREA                                   
         ENTRY SSB                                                              
         ENTRY UTL                                                              
*                                                                               
         L     R7,ACOMMON                                                       
         LR    R8,R7                                                            
         AHI   R8,4096                                                          
         USING COMMON,R7,R8                                                     
         L     RC,AWORKD           GET SOME W/S FOR THIS MODULE                 
         USING WORKD,RC                                                         
         ST    RD,SAVERD           SET EXIT FROM CHAIN RD                       
*                                                                               
         BRAS  RE,INIT             INITIALISE IT ALL                            
         JNE   *+2                 DIE=FAIL AT INITIALIZATION                   
*                                                                               
MAIN02   BRAS  RE,WAIT             ISSUE 'GET' AND WAIT FOR A MESSAGE           
         BNE   MAINX               OPERATOR REQUESTED CANCEL JOB                
*                                                                               
         BRAS  RE,PUTMSG           PUT MESSAGE TO CORRECT QUEUE                 
         B     MAIN02              AND BACK FOR NEXT                            
*                                                                               
MAINX    BRAS  RE,MQCLS            FREE ANY MQ CONNECTIONS THAT EXIST           
         CLOSE SYSPRINT                                                         
         B     XBASE                                                            
*                                                                               
VWRKAREA DC    V(WORKAREA)                                                      
ACOMMON  DC    A(COMMON)                                                        
AWORKD   DC    A(WORKD)                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO WAIT UNTIL POSTED OR A MESSAGE ARRIVES                   *         
***********************************************************************         
WAIT     NTR1  ,                                                                
         CLI   OPERSTOP,YES        OPERATOR STOP REQUESTED?                     
         BE    EXITL               YES                                          
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
         MVC   MQGETBUF,AMSGBUFF   MESSAGE                                      
*                                                                               
         LAY   R2,MQGET                                                         
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
WAIT04   EQU   *                                                                
         WAIT  1,ECBLIST=ECBLST                                                 
         MVC   EMAILCNT,=F'0'            RESET EMAIL COUNTER                    
*                                                                               
         TM    GETECB,X'40'        MQ SIGNALS MESSAGE ARRIVED                   
         BO    WAIT02              YES - GO GET IT                              
*                                                                               
         TM    TIMERECB,X'40'                                                   
         BO    WAIT02              TIMER POPPED, TRY GET AGAIN                  
*                                                                               
         OC    STIMER1,STIMER1                                                  
         BZ    WAIT08                                                           
         STIMERM CANCEL,ID=STIMER1                                              
         LTR   RF,RF               CANCEL TIMER                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WAIT08   L     RF,AOPERECB                                                      
         TM    0(RF),X'40'         OPERATOR INTERRUPT                           
         BZ    WAIT30                                                           
         BRAS  RE,CHKOPER                                                       
         CLI   BADOPCMD,C'Y'                                                    
         BE    WAIT04              BAD OPS COMMAND, WAIT AGAIN                  
*                                                                               
WAIT30   CLI   OPERSTOP,YES        OPERATOR STOP REQUESTED?                     
         BE    EXITL               YES                                          
         B     WAIT04                                                           
                                                                                
***********************************************************************         
* ROUTINE TO PUT FROM MASTER INTO CORRECT OUTPUT QUEUE                *         
***********************************************************************         
PUTMSG   NTR1  ,                                                                
         SAM31                                                                  
*                                                                               
         L     R3,AMSGBUFF                                                      
         XC    ERRORCD,ERRORCD     CLEAR ERROR CODE                             
         XC    ERRQ,ERRQ           WANT ERROR MESSAGE MIRRORING?                
         CLC   16(8,R3),=CL8'ERRORTO='                                          
         BNE   *+10                                                             
         MVC   ERRQ,24(R3)                                                      
*                                                                               
         MVC   WORK(L'SCHQID),0(R3)      SAVE REAL HEADER                       
*                                                                               
         CLC   =C'FP=',0(R3)       NEED TO WORK OUT ROUTING INFO?               
         BE    *+14                NO                                           
         CLC   =C'FQ=',0(R3)                                                    
         BNE   *+12                                                             
         BRAS  RE,GETFAC           CHANGE TO FACPAK=????*****                   
         BL    PUTM06              ERROR                                        
*                                                                               
*&&US                                                                           
         CLC   =C'01;',0(R3)       SPECIAL BDE SERVER STATUS MSG?               
         BNE   *+12                                                             
         BRAS  RE,GETBDEM          CHANGE TO EDICT?_BDE******                   
         BL    PUTM06              ERROR                                        
*                                                                               
*&&                                                                             
*                                                                               
         XR    R0,R0               R0 = LOW                                     
         LHI   R1,SCHQTABN-1       R1 = HIGH                                    
*                                                                               
PUTM02   CR    R0,R1               WHILE LOW <= HIGH                            
         BH    PUTM06              R0 POINTS TO INSERTION POINT                 
*                                                                               
         LR    R5,R0               R5 = MID                                     
         AR    R5,R1                                                            
         SRL   R5,1                MID=(HIGH+LOW)/2                             
*                                                                               
         LR    R4,R5                                                            
         MHI   R4,SCHQTABL                                                      
         A     R4,ASCHQTAB         R4=A(TEST ENTRY)                             
         USING SCHQTABD,R4                                                      
*                                                                               
         CLC   WORK(L'SCHQID),SCHQID                                            
         BE    PUTM08              KEY == K(MID)                                
         BL    *+12                                                             
         LA    R0,1(R5)            IF CC HIGH : LOW = MID+1                     
         B     PUTM02                                                           
         LR    R1,R5               IF CC LOW : HIGH = MID-1;                    
         BCTR  R1,0                                                             
         B     PUTM02                                                           
*                                                                               
PUTM04   LR    R4,R0                                                            
         MHI   R4,SCHQTABL                                                      
         A     R4,ASCHQTAB                                                      
         CLC   SCHQID,WORK                                                      
         BE    PUTM08              KEY == K(MID)                                
*                                                                               
*                              *** NO MATCH                                     
PUTM06   CLI   LOG,NO              SUPPRESSED LOGGING?                          
         BE    PUTM10              YES                                          
*                                                                               
         LA    RF,LOGQHOB          PUT MESSAGE TO MASTER LOG QUEUE              
         ST    RF,MQPUTQN                                                       
         LHI   RF,MQPMO_PASS_ALL_CONTEXT                                        
         ST    RF,PUTOPTS_OPTIONS                                               
         MVC   PUTOPTS_CONTEXT,MAINQHOB                                         
         MVC   MQPUTBUF,AMSGBUFF                                                
         MVC   DATALENP,DATALEN                                                 
         MVC   MSGDESC_PERSISTENCE,=A(MQPER_PERSISTENCE_AS_Q_DEF)               
*                                                                               
         MVC   PLINE+55(48),LOGQ_OBJECTNAME    PARK QNAME                       
         LAY   R2,MQPUT                                                         
         BRAS  RE,CALLMQ                                                        
         BE    PUTM10                                                           
         DC    H'0'                                                             
*                              *** MATCH                                        
         USING SCHQTABD,R4                                                      
PUTM08   EQU   *                                                                
         TM    SCHQFLG1,SCHQ1NLG   FORCE SUPPRESS LOGGING                       
         BO    PUTM10                                                           
*                                                                               
         TM    SCHQFLG1,SCHQ1ALG   TEST ALWAYS LOG FOR THIS TYPE                
         BO    *+12                                                             
         CLI   LOG,NO              SUPPRESSED LOGGING?                          
         BE    PUTM10                                                           
*                                                                               
         LA    R0,SCHQHLOG         LOCAL LOGGING QUEUE                          
         L     RF,SCHQCMQL         CHECK IF THERE IS A LOCAL LOG Q              
         MVC   PLINE+55(48),MAINQ_OBJECTNAME-MAINQ(RF)  PARK QNAME              
         CLI   MAINQ_OBJECTNAME-MAINQ(RF),C' '                                  
         BH    *+14                                                             
         LA    R0,LOGQHOB          PUT MESSAGE TO MASTER LOG QUEUE              
         MVC   PLINE+55(48),LOGQ_OBJECTNAME             PARK QNAME              
*                                                                               
         ST    R0,MQPUTQN                                                       
         LHI   RF,MQPMO_PASS_ALL_CONTEXT                                        
         ST    RF,PUTOPTS_OPTIONS                                               
         MVC   PUTOPTS_CONTEXT,MAINQHOB                                         
         MVC   MQPUTBUF,AMSGBUFF                                                
         MVC   DATALENP,DATALEN                                                 
         MVC   MSGDESC_PERSISTENCE,=A(MQPER_PERSISTENCE_AS_Q_DEF)               
*                                                                               
         LAY   R2,MQPUT                                                         
         BRAS  RE,CALLMQ                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PUTM10   OC    ERRORCD,ERRORCD     ERROR SET ALREADY?                           
         BNZ   PUTM12              YES                                          
         CLC   SCHQID,WORK            MATCHED QUEUE?                            
         BE    PUTM20                 YES - PUT TO IT AND YOU ARE DONE          
         MVI   FERN,8              BAD MESSAGE                                  
         B     PUTM200                YES - PUT TO IT AND YOU ARE DONE          
*                                                                               
         LHI   R0,ERRHDRQ          BAD HEADER (QUEUE MATCH) MESSAGE             
         ST    R0,ERRORCD                                                       
*                                                                               
PUTM12   OC    ERRQ,ERRQ           ECHOING BAD MESSAGE TO A QUEUE?              
         BNZ   *+12                YES                                          
         MVI   FERN,8              NO - DUMP IT TO DEAD MSQ QUEUE               
         B     PUTM200                                                          
*                                                                               
         MVC   24(16,R3),SPACES    CLEAR QUEUE DETAILS                          
*                                                                               
         L     R0,ERRORCD          SET ERROR NUMBER IN ITS PLACE                
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  24(08,R3),DUB       8 DIGIT NUMBER YOU WILL NOTE                 
*                                                                               
         USING SCHQTABD,R4                                                      
         L     R4,ASCHQTAB                                                      
PUTM14   CLI   0(R4),X'FF'         EOT - BAD ECHO QUEUE HEADER                  
         BE    PUTM18                                                           
         CLC   SCHQID,ERRQ         DO WE KNOW THIS QUEUE?                       
         BE    PUTM16              YES                                          
         AHI   R4,SCHQTABL                                                      
         B     PUTM14                                                           
*                                                                               
PUTM16   MVI   FERN,6              OUTPUT RETURN TO SENDER MESSAGE              
         BRAS  RE,ERRMSG                                                        
         MVC   PLINE+L'ERRHDR(17),=CL17'Error number is:'                       
         UNPK  PLINE+L'ERRHDR+17(8),DUB                                         
*                                                                               
         MVC   PLINE+L'ERRHDR+26(02),=C'Q='                                     
         MVC   PLINE+L'ERRHDR+28(16),WORK                                       
         BRAS  RE,PRNT                                                          
         B     PUTM20                                                           
*                                                                               
PUTM18   MVI   FERN,7              OUTPUT BAD RETURN TO ADDRESS                 
         B     PUTM200                                                          
*                                                                               
PUTM20   EQU   *                                                                
         LA    R0,SCHQHOBJ         SET PUT QUEUE OBJECT                         
         L     RF,SCHQCMQ                                                       
         MVC   PLINE+55(48),MAINQ_OBJECTNAME-MAINQ(RF)  PARK QNAME              
         CLC   =CL5'NONE ',MAINQ_OBJECTNAME-MAINQ(RF)                           
         BE    PUTM48                                                           
         CLI   MAINQ_OBJECTNAME-MAINQ(RF),C' '                                  
         BH    *+12                                                             
         MVI   FERN,9              NO OUTPUT QUEUE DEFINED                      
         B     PUTM50                                                           
*                                  CHECK IF MSG SIZE TOO BIG                    
         ICM   RE,15,DATALEN                                                    
         C     RE,SCHQMAXL                                                      
         BNH   *+12                                                             
         MVI   FERN,11             MSG TOO BIG                                  
         B     PUTM50              PUT TO DEAD LETTER QUEUE                     
*                                                                               
         LT    RF,SCHQHOOK                                                      
         BZ    PUTM40                                                           
         MVI   HK_MODE,HKM_PRC     HOOK MODE = PROCESS                          
         BASR  RE,RF                                                            
         BNE   PUTM50              ERROR, PUT TO DEAD LETTER QUEUE              
*                                                                               
PUTM40   ST    R0,MQPUTQN                                                       
         MVC   MQPUTBUF,AMSGBUFF                                                
         MVC   DATALENP,DATALEN                                                 
         TM    SCHQFLG2,SCHQ2LCT   IS LAST CHAR IN LABEL CTRL CHAR?             
         BZ    PUTM41              NO - GO AWAY                                 
         L     RF,AMSGBUFF         YES - REPLACE IT WITH C'*'                   
         MVI   L'SCHQID-1(RF),C'*'                                              
*                                                                               
PUTM41   TM    SCHQFLG2,SCHQ2DMH   DROP MESSAGE HEADER?                         
         BZ    PUTM42              NO - GO AWAY                                 
         L     RE,MQPUTBUF         YES - SHIFT MSGBUF AND CUT DATALEN           
         AHI   RE,L'SCHQID                                                      
         ST    RE,MQPUTBUF                                                      
         ICM   RE,15,DATALEN                                                    
         AHI   RE,-(L'SCHQID)                                                   
         ST    RE,DATALENP                                                      
*                                                                               
PUTM42   LHI   RF,MQPMO_PASS_ALL_CONTEXT                                        
         ST    RF,PUTOPTS_OPTIONS                                               
         MVC   PUTOPTS_CONTEXT,MAINQHOB                                         
*                                                                               
         LAY   R2,MQPUT                                                         
         BRAS  RE,CALLMQ                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PUTM48   LAY   R2,MQCMIT           COMMIT THIS TRANSACTION                      
         BRAS  RE,CALLMQ                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LT    RF,SCHQHOOK                                                      
         BZ    PUTM60                                                           
         MVI   HK_MODE,HKM_CMT     HOOK MODE = COMMIT                           
         BASR  RE,RF                                                            
         BE    PUTM60                                                           
         DC    H'0'                                                             
*                                                                               
PUTM50   BRAS  RE,ERRMSG                                                        
         LA    R0,DEADQHOB         PUT MESSAGE TO DEAD MSG LOG QUEUE            
         ST    R0,MQPUTQN                                                       
         MVC   MSGDESC_PERSISTENCE,=A(MQPER_PERSISTENCE_AS_Q_DEF)               
         MVC   PLINE+55(48),DEADQ_OBJECTNAME            PARK QNAME              
         MVC   MQPUTBUF,AMSGBUFF                                                
         MVC   DATALENP,DATALEN                                                 
         LHI   RF,MQPMO_PASS_ALL_CONTEXT                                        
         ST    RF,PUTOPTS_OPTIONS                                               
         MVC   PUTOPTS_CONTEXT,MAINQHOB                                         
         BRAS  RE,ERREMAIL          EMAIL ERROR                                 
*                                                                               
         LAY   R2,MQPUT                                                         
         BRAS  RE,CALLMQ                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LAY   R2,MQCMIT           COMMIT THIS TRANSACTION SET AND EXIT         
         BRAS  RE,CALLMQ                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PUTM60   EQU   *                                                                
         LT    R4,SCHQXLNK         Another queue to put to (Fan out)            
         BZ    PUTMX               No, done                                     
         B     PUTM20              Yes, lets do it again                        
*                                                                               
PUTM200  BRAS  RE,ERRMSG                                                        
         LA    R0,DEADQHOB         PUT MESSAGE TO DEAD MSG LOG QUEUE            
         ST    R0,MQPUTQN                                                       
         MVC   MSGDESC_PERSISTENCE,=A(MQPER_PERSISTENCE_AS_Q_DEF)               
         MVC   PLINE+55(48),DEADQ_OBJECTNAME            PARK QNAME              
         MVC   MQPUTBUF,AMSGBUFF                                                
         MVC   DATALENP,DATALEN                                                 
         LHI   RF,MQPMO_PASS_ALL_CONTEXT                                        
         ST    RF,PUTOPTS_OPTIONS                                               
         MVC   PUTOPTS_CONTEXT,MAINQHOB                                         
         BRAS  RE,ERREMAIL          EMAIL ERROR                                 
*                                                                               
         LAY   R2,MQPUT                                                         
         BRAS  RE,CALLMQ                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LAY   R2,MQCMIT           COMMIT THIS TRANSACTION SET AND EXIT         
         BRAS  RE,CALLMQ                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PUTMX    CLI   TRACE,NO            ANY TRACING TO BE DONE?                      
         BE    EXITOK              NO                                           
         BRAS  RE,TRACEIT          TRACE MESSAGE                                
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
INIT     NTR1  ,                                                                
         OPEN  (SYSPRINT,OUTPUT)   INITIALISE PRINT OUTPUT                      
         MVI   PCC,C' '                                                         
         MVC   P,PCC                                                            
         MVI   ETYPE,C'P'          SET DEFAULT AS PRODUCTION                    
         MVI   DSPACE,C'P'         SET DEFAULT AS PRODUCTION                    
*                                                                               
         LR    R0,RC                                                            
         LHI   R1,WORKL                                                         
         XR    RF,RF                                                            
         MVCL  R0,RE               CLEAR ALL THAT LOVELY W/S                    
*                                                                               
         LHI   R0,1                                                             
         BRAS  RE,SYSMESS          STARTING INITIALISE                          
*                                                                               
         BRAS  RE,GETCARDS         READ IN INPUT CARDS                          
         BL    EXITL                                                            
         BRAS  RE,VALCARDS         VALIDATE INPUT CARDS                         
         BL    EXITL                                                            
*                                                                               
         BRAS  RE,SRTSCHQT         SORT SCHQTAB                                 
*                                                                               
         BRAS  RE,OPENCTFL         OPEN CTFILE AND SET SSB,ALET                 
*                                                                               
         BRAS  RE,MQINIT                                                        
         BL    EXITL                                                            
         BRAS  RE,SETOPS           SET UP OPERATOR INPUT                        
         BL    EXITL                                                            
*                                                                               
         L     R0,MAXMSGLN                                                      
         SLL   R0,1                X 2                                          
         GETMAIN RU,LV=(0),LOC=(ANY,ANY),BNDRY=PAGE                             
         LTR   RF,RF                                                            
         BZ    INIT02                                                           
         LHI   R0,21                                                            
         BRAS  RE,SYSMESS          GETMAIN FAILED                               
         ABEND 911,DUMP                                                         
*                                                                               
INIT02   ST    R1,AMSGBUFF         A(1st msg buffer)                            
         A     R1,MAXMSGLN                                                      
         ST    R1,AMSGBUF2         A(2nd msg buffer)                            
*                                                                               
INITX    LHI   R0,2                                                             
         BRAS  RE,SYSMESS          COMPLETED INITIALISE                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
*CONVERT BDE STATUS MESSAGE TO EDICTA/R LABEL                         *         
***********************************************************************         
GETBDEM  NTR1  ,                                                                
         CLC   WORK+3(2),=C'4A'    EDICTA FORMAT?                               
         BE    GBDEM10                                                          
         CLC   WORK+3(2),=C'1R'    EDICTR FORMAT?                               
         BE    GBDEM20                                                          
         CLC   WORK+3(2),=C'1T'    EDICTT FORMAT?                               
         BE    GBDEM30                                                          
         B     EXITL                                                            
GBDEM10  MVC   WORK+00(16),=CL16'EDICTA_BDE******'                              
         B     EXITOK                                                           
GBDEM20  MVC   WORK+00(16),=CL16'EDICTR_BDE******'                              
         B     EXITOK                                                           
GBDEM30  MVC   WORK+00(16),=CL16'EDICTT_BDE******'                              
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET FACPAK GIVEN 'FP=UUUUUUUUUUSSS' INPUT IN HEADER      *         
* WHERE UUUUUUUUUU IS THE USER ID                                     *         
*       SSS        IS THE 3 CHR SYSTEM CODE (SPOT=SPT, PRNT=PRT ETC)  *         
*                                                                     *         
* NTRY: R3     = A(MESSAGE HEADER)                                    *         
***********************************************************************         
GETFAC   NTR1  ,                                                                
         BRAS  RE,OPENCTFL                                                      
         LHI   R0,ERRSE                                                         
         ST    R0,ERRORCD                                                       
*                                                                               
         MVC   FPLABEL,0(R3)                                                    
         MVC   USERID,3(R3)                                                     
         MVC   SYSALPH,13(R3)                                                   
         SAM24                                                                  
*                                                                               
         LA    R1,SYSLST                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         AHI   R1,6                                                             
         USING SYSLSTD,R1                                                       
         CLC   SYSLSHRT,SYSALPH   MATCH SYSTEM                                  
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     EXITL                                                            
*                                                                               
         MVC   BYTE,SYSLNUM       OV SYSTEM NUMBER                              
         LHI   R0,ERRUSER                                                       
         ST    R0,ERRORCD                                                       
         DROP  R1                                                               
*                                                                               
         CLC   =C'AGY=',USERID                                                  
         BNE   *+14                                                             
         MVC   HALF,USERID+4                                                    
         B     GFAC04                                                           
*                                                                               
         XC    KEY,KEY                                                          
X        USING CTIREC,KEY                                                       
         MVI   X.CTIKTYP,CTIKTYPQ                                               
         MVC   X.CTIKID,USERID                                                  
         GOTO1 VDMGR,DMCB,DMREAD,CTFILE,KEY,IO                                  
         CLI   8(R1),0                                                          
         BNE   EXITL               BAD USERID                                   
*                                                                               
         LA    R2,IO                                                            
         AHI   R2,CTIDATA-CTIREC                                                
         USING CTAGYD,R2                                                        
         XR    RF,RF                                                            
GFAC02   CLI   CTAGYEL,0                                                        
         BE    EXITL                                                            
         CLI   CTAGYEL,CTAGYELQ                                                 
         BE    *+12                                                             
         IC    RF,CTAGYLEN                                                      
         BXH   R2,RF,GFAC02                                                     
*                                                                               
         MVC   HALF,CTAGYID                                                     
*                                                                               
X        USING CT5REC,KEY                                                       
GFAC04   XC    KEY,KEY                                                          
         MVI   X.CT5KTYP,CT5KTYPQ  RECORD ID                                    
         MVC   X.CT5KALPH,HALF     SET AGENCY ALPHA                             
         GOTO1 VDMGR,DMCB,DMREAD,CTFILE,KEY,IO                                  
         CLI   8(R1),0                                                          
         BNE   EXITL               BAD AGY ALPHA                                
*                                                                               
         LA    R2,IO                                                            
         AHI   R2,CT5DATA-CT5REC                                                
         USING CTSYSD,R2                                                        
         XR    RF,RF                                                            
GFAC06   CLI   CTSYSEL,0           MUST FIND AUTH ELEMENT                       
         BE    GFAC80                                                           
         CLI   CTSYSEL,CTSYSELQ    FIND SYSTEM AUTH ELEMENT                     
         BNE   *+14                                                             
         CLC   CTSYSNUM,BYTE       MATCH SYSTEM                                 
         BE    *+12                                                             
         IC    RF,CTSYSLEN                                                      
         BXH   R2,RF,GFAC06                                                     
*                                                                               
         MVC   BYTE,CTSYSSE        OVERLAY REAL SYSTEM                          
         DROP  R2,X                                                             
*                                                                               
         GOTO1 VDMOD000,DMCB,AFINDSYS,(1,0)                                     
         L     R1,4(R1)            R1=A(SYSFILES LIST)                          
         AHI   R1,-4                                                            
         L     R2,0(R1)            EXTRACT SYSSTAB (FACPAK ID TAB)              
*                                                                               
         MVC   WORK+00(16),=CL16'FACPAK=XXXX*****'                              
*                                                                               
         XR    RF,RF                                                            
         IC    RF,BYTE                                                          
         AR    RF,R2                                                            
         MVC   BYTE,0(RF)          FACPAK 1 CHARACTER NAME                      
*                                                                               
         CLI   ETYPE,C'T'          TST GETS OVERRIDDEN                          
         BNE   *+14                                                             
*&&US*&& MVC   WORK+07(04),=CL4'TST '                                           
*&&UK*&& MVC   WORK+07(04),=CL4'TST ' (for now)                                 
         B     GFAC60                                                           
*                                                                               
         CLI   ETYPE,C'C'                                                       
         BNE   *+14                                                             
         MVC   WORK+07(04),=CL4'CSC '                                           
         B     GFAC60                                                           
*                                                                               
         CLI   ETYPE,C'Q'          FQA GETS OVERRIDDEN                          
         BNE   *+14                                                             
         MVC   WORK+07(04),=CL4'FQA '                                           
         B     GFAC60                                                           
*                                                                               
         CLI   BYTE,255            UPDATE ANYWHERE GOES TO ADV1                 
         BNE   *+14                                                             
         MVC   WORK+07(04),=CL4'ADV1'                                           
         B     GFAC60                                                           
*                                                                               
         ICM   RF,15,ASSB                                                       
         MVC   BYTE2,SSODSPAC-SSOOFF(RF)                                        
*                                                                               
*&&US                                                                           
*ASSUME HARD-CODED VALUES FOR REP? IN TABLE SYSSTAB, DMFILESUS                  
*WHEN WE REUSE THE REP? ENTRIES IN FACIDTAB, WE WILL NEED CHECK ALSO            
*     THE SYSTEM# (SYSLNUM) TO WHICH FACPAK TO GO.                              
*                                                                               
         CLI   BYTE,X'04'          REPA?                                        
         JE    GFAC35                                                           
         CLI   BYTE,X'0E'          REPB?                                        
         JE    GFAC35                                                           
         CLI   BYTE,X'09'          REPC?                                        
         JNE   GFAC40                                                           
GFAC35   MVI   BYTE2,C'R'          Only look up the REP's FACIDTAB              
*&&                                                                             
*                                                                               
         USING FACIDD,R1                                                        
GFAC40   L     R1,=V(FACIDTAB)                                                  
         CLC   =C'????',0(R1)      IS THIS THE OLD TABLE?                       
         JE    GFAC50              YES- THIS IS THE FACIDTAB ITSELF             
GFAC42   CLI   FACIDSPC,X'FF'      End of table                                 
         JE    *+2                 Death=No table for this Dspace               
         CLC   FACIDSPC,BYTE2                                                   
         JE    GFAC45              Found them                                   
         AHI   R1,FACIDLNQ                                                      
         J     GFAC42                                                           
*                                                                               
GFAC45   L     R1,FACAID                                                        
         USING FACITABD,R1                                                      
GFAC50   CLI   FACISN4,255                                                      
         JE    GFAC60                                                           
         CLC   FACIID,BYTE                                                      
         JE    *+12                                                             
         AHI   R1,L'FACITAB                                                     
         J     GFAC50                                                           
         MVC   WORK+07(04),FACISN4                                              
         J     GFAC60                                                           
         DROP  R1                                                               
*                                                                               
GFAC60   DS    0H                                                               
         CLC   =C'FQ=',FPLABEL     SPECIAL QA OVERRIDE FOR TESTING              
         BNE   *+10                                                             
         MVC   WORK+07(04),=CL4'FQA '                                           
         XC    ERRORCD,ERRORCD                                                  
         B     EXITOK                                                           
*                                                                               
GFAC80   LHI   R0,ERRAUTH                                                       
         ST    R0,ERRORCD                                                       
         B     EXITL                                                            
*                                                                               
       ++INCLUDE FASYSLST                                                       
         EJECT                                                                  
***********************************************************************         
* INITIALISE MQ QUEUES                                                *         
* NOTE: IN A DELIBERATE ATTEMPT TO SIMPLIFY THE NAMES OF THE MQ       *         
*       QUEUE MANAGER, THE MASTER QUEUE AND LOG QUEUES ARE ALL HARD   *         
***********************************************************************         
MQINIT   NTR1  ,                                                                
         MVI   CONOK,NO                                                         
         LHI   R0,9                                                             
         BRAS  RE,SYSMESS          BEGINNING MQ INITIALISE                      
*                                                                               
         MVC   MAINQ_OBJECTNAME,QMASTERP                                        
         MVC   LOGQ_OBJECTNAME,QLOGGERP                                         
         MVC   DEADQ_OBJECTNAME,QDEADMGP                                        
         CLI   ETYPE,C'T'                                                       
         BNE   MQINI01C                                                         
         MVC   MAINQ_OBJECTNAME,QMASTERT                                        
         MVC   LOGQ_OBJECTNAME,QLOGGERT                                         
         MVC   DEADQ_OBJECTNAME,QDEADMGT                                        
         B     MQINI02                                                          
*                                                                               
MQINI01C CLI   ETYPE,C'C'                                                       
         BNE   MQINI01Q                                                         
         MVC   MAINQ_OBJECTNAME,QMASTERC                                        
         MVC   LOGQ_OBJECTNAME,QLOGGERC                                         
         MVC   DEADQ_OBJECTNAME,QDEADMGC                                        
         B     MQINI02                                                          
*                                                                               
MQINI01Q CLI   ETYPE,C'Q'                                                       
         BNE   MQINI02                                                          
         MVC   MAINQ_OBJECTNAME,QMASTERQ                                        
         MVC   LOGQ_OBJECTNAME,QLOGGERQ                                         
         MVC   DEADQ_OBJECTNAME,QDEADMGQ                                        
         B     MQINI02                                                          
*                                                                               
MQINI02  EQU   *                                                                
*                                  ANY OVERRIDE QNAMES?                         
         CLC   QINP_OV,SPACES                                                   
         BE    *+10                                                             
         MVC   MAINQ_OBJECTNAME,QINP_OV                                         
         CLC   QLOG_OV,SPACES                                                   
         BE    *+10                                                             
         MVC   LOGQ_OBJECTNAME,QLOG_OV                                          
         CLC   QDEAD_OV,SPACES                                                  
         BE    *+10                                                             
         MVC   DEADQ_OBJECTNAME,QDEAD_OV                                        
*                                                                               
         BLDL  0,ENTRYPTS          BUILD LIST OF EXTERNAL ENTRY PTS             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                BAD RETURN FROM BLDL MACRO                   
*                                                                               
         LOAD  DE=CSQBCONN                                                      
         ST    R0,AMQCT                                                         
         LOAD  DE=CSQBOPEN                                                      
         ST    R0,AMQOPENQ                                                      
         LOAD  DE=CSQBGET                                                       
         ST    R0,AMQGET                                                        
         LOAD  DE=CSQBCOMM                                                      
         ST    R0,AMQCMIT                                                       
         LOAD  DE=CSQBCLOS                                                      
         ST    R0,AMQCLOSE                                                      
         LOAD  DE=CSQBDISC                                                      
         ST    R0,AMQDISC                                                       
         LOAD  DE=CSQBPUT                                                       
         ST    R0,AMQPUT                                                        
         ST    R0,AMQPUT2                                                       
*                                                                               
         LAY   R2,MQCT                                                          
         BRAS  RE,CALLMQ           CONNECT TO MQ                                
         JNE   EXITL               EXIT LOW IF UNABLE                           
*                                                                               
         LHI   R0,11                                                            
         BRAS  RE,SYSMESS          CONNECTED TO MQ QMGR                         
*                                                                               
         MVC   PLINE+00(08),=CL08'Opening:'                                     
         MVC   PLINE+09(16),=CL16'Main Queue'                                   
         MVI   PLINE+26,C'='                                                    
         MVC   PLINE+28(48),MAINQ_OBJECTNAME                                    
         BRAS  RE,PRNT                                                          
*                                                                               
         LA    RF,MAINQ            SET QUEUE OBJECT FOR MASTER QUEUE            
         ST    RF,MQOPNQNM                                                      
         LA    RF,MAINQHOB         SET A(RETURN FOR HOBJ)                       
         ST    RF,MQOPNHOB                                                      
*        LHI   RF,MQOO_INPUT_AS_Q_DEF+MQOO_SAVE_ALL_CONTEXT                     
         LHI   RF,MQOO_INPUT_EXCLUSIVE+MQOO_SAVE_ALL_CONTEXT                    
         ST    RF,OPN_OPTS                                                      
         LAY   R2,MQOPENQ                                                       
         BRAS  RE,CALLMQ           OPEN MASTER QUEUE                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LHI   R0,12                                                            
         BRAS  RE,SYSMESS          OPENED MASTER QUEUE                          
*                                                                               
         MVC   PLINE+00(08),=CL08'Opening:'                                     
         MVC   PLINE+09(16),=CL16'Log Queue'                                    
         MVI   PLINE+26,C'='                                                    
         MVC   PLINE+28(48),LOGQ_OBJECTNAME                                     
         BRAS  RE,PRNT                                                          
*                                                                               
         LA    RF,LOGQ             SET QUEUE OBJECT FOR LOG QUEUE               
         ST    RF,MQOPNQNM                                                      
         LA    RF,LOGQHOB          SET A(RETURN FOR HOBJ)                       
         ST    RF,MQOPNHOB                                                      
         LHI   RF,MQOO_OUTPUT+MQOO_PASS_ALL_CONTEXT                             
         ST    RF,OPN_OPTS                                                      
         LAY   R2,MQOPENQ                                                       
         BRAS  RE,CALLMQ           OPEN LOG QUEUE                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LHI   R0,13                                                            
         BRAS  RE,SYSMESS          OPENED LOG QUEUE                             
*                                                                               
         CLI   LOG,NO                                                           
         BNE   MQINI10                                                          
         LHI   R0,18                                                            
         BRAS  RE,SYSMESS          WARNING LOGGING SUPPRESSED                   
*                                                                               
MQINI10  MVC   PLINE+00(08),=CL08'Opening:'                                     
         MVC   PLINE+09(16),=CL16'Dead Msg Queue'                               
         MVI   PLINE+26,C'='                                                    
         MVC   PLINE+28(48),DEADQ_OBJECTNAME                                    
         BRAS  RE,PRNT                                                          
*                                                                               
MQINI18  LA    RF,DEADQ            SET QUEUE OBJECT FOR LOG QUEUE               
         ST    RF,MQOPNQNM                                                      
         LA    RF,DEADQHOB         SET A(RETURN FOR HOBJ)                       
         ST    RF,MQOPNHOB                                                      
         LHI   RF,MQOO_OUTPUT+MQOO_PASS_ALL_CONTEXT                             
         ST    RF,OPN_OPTS                                                      
         LAY   R2,MQOPENQ                                                       
         BRAS  RE,CALLMQ           OPEN DEAD MSG QUEUE                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LHI   R0,24                                                            
         BRAS  RE,SYSMESS          OPENED DEAD MSG QUEUE                        
*                                                                               
         USING SCHQTABD,R4                                                      
         L     R4,ASCHQTAB                                                      
MQINI30  CLI   0(R4),X'FF'         EOT                                          
         BE    MQINI86                                                          
*                                                                               
         ICM   RF,15,SCHQCMQ                                                    
         BZ    MQINI80             NO QUEUE TO OPEN - SKIP LOG Q ALSO           
         MVC   MAINQ_OBJECTNAME-MAINQ(48,RF),SCHQNPRD                           
*                                                                               
         MVC   PLINE+00(08),=CL08'Opening:'                                     
         MVC   PLINE+09(16),SCHQID                                              
         MVI   PLINE+26,C'='                                                    
         L     RF,SCHQCMQ                                                       
         MVC   PLINE+28(48),MAINQ_OBJECTNAME-MAINQ(RF)                          
*                                                                               
         CLC   =CL5'NONE ',MAINQ_OBJECTNAME-MAINQ(RF)                           
         BE    MQINI32                                                          
         CLI   MAINQ_OBJECTNAME-MAINQ(RF),C' '                                  
         BNH   MQINI32                                                          
         B     MQINI35                                                          
MQINI32  MVC   PLINE+00(08),=CL08'Skipped:'                                     
         BRAS  RE,PRNT                                                          
         B     MQINI80             NO QUEUE TO OPEN - SKIP LOG Q ALSO           
*                                                                               
MQINI35  BRAS  RE,PRNT                                                          
*                                                                               
         L     RF,SCHQCMQ          SET QUEUE OBJECT FOR LOCAL QUEUE             
         ST    RF,MQOPNQNM                                                      
         LA    RF,SCHQHOBJ         SET A(RETURN FOR HOBJ)                       
         ST    RF,MQOPNHOB                                                      
         LHI   RF,MQOO_OUTPUT+MQOO_PASS_ALL_CONTEXT                             
         ST    RF,OPN_OPTS                                                      
*                                                                               
         LAY   R2,MQOPENQ                                                       
         BRAS  RE,CALLMQ           OPEN LOCAL QUEUE                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    SCHQFLG1,SCHQ1NLG   SUPPRESS LOGGING                             
         BO    MQINI40                                                          
         TM    SCHQFLG1,SCHQ1ALG   ALWAYS LOG?                                  
         BO    *+12                                                             
         CLI   LOG,NO              WANT TO LOG?                                 
         BE    MQINI40                                                          
*                                                                               
         LT    RF,SCHQCMQL                                                      
         BZ    MQINI40                                                          
         MVC   MAINQ_OBJECTNAME-MAINQ(48,RF),SCHQLPRD                           
*                                                                               
         CLI   MAINQ_OBJECTNAME-MAINQ(RF),C' '                                  
         BNH   MQINI40             NO QUEUE TO OPEN                             
*                                                                               
         MVC   PLINE+04(13),=CL13'Logging Queue'                                
         MVI   PLINE+26,C'='                                                    
         MVC   PLINE+28(48),MAINQ_OBJECTNAME-MAINQ(RF)                          
         BRAS  RE,PRNT                                                          
*                                                                               
         L     RF,SCHQCMQL         SET QUEUE OBJECT FOR LOG QUEUE               
         ST    RF,MQOPNQNM                                                      
         LA    RF,SCHQHLOG         SET A(RETURN FOR HOBJ)                       
         ST    RF,MQOPNHOB                                                      
         LHI   RF,MQOO_OUTPUT+MQOO_PASS_ALL_CONTEXT                             
         ST    RF,OPN_OPTS                                                      
*                                                                               
         LAY   R2,MQOPENQ                                                       
         BRAS  RE,CALLMQ           OPEN LOGGING QUEUE                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MQINI40  LR    R5,R4               Save R4                                      
MQINI42  LT    R4,SCHQXLNK         Multiple MQs                                 
         BZ    MQINI48             None, or no more                             
         LT    RF,SCHQCMQ                                                       
         BZ    MQINI48             NO QUEUE TO OPEN                             
         MVC   MAINQ_OBJECTNAME-MAINQ(48,RF),SCHQNPRD                           
*                                                                               
         CLC   =CL5'NONE ',MAINQ_OBJECTNAME-MAINQ(RF)                           
         BE    MQINI48             NO QUEUE TO OPEN                             
         CLI   MAINQ_OBJECTNAME-MAINQ(RF),C' '                                  
         BNH   MQINI48             NO QUEUE TO OPEN                             
*                                                                               
         MVC   PLINE+00(08),=CL08'Opening:'                                     
         MVC   PLINE+09(16),SCHQID                                              
         MVI   PLINE+26,C'='                                                    
         L     RF,SCHQCMQ                                                       
         MVC   PLINE+28(48),MAINQ_OBJECTNAME-MAINQ(RF)                          
         BRAS  RE,PRNT                                                          
*                                                                               
         L     RF,SCHQCMQ          SET QUEUE OBJECT FOR LOCAL QUEUE             
         ST    RF,MQOPNQNM                                                      
         LA    RF,SCHQHOBJ         SET A(RETURN FOR HOBJ)                       
         ST    RF,MQOPNHOB                                                      
         LHI   RF,MQOO_OUTPUT+MQOO_PASS_ALL_CONTEXT                             
         ST    RF,OPN_OPTS                                                      
*                                                                               
         LAY   R2,MQOPENQ                                                       
         BRAS  RE,CALLMQ           OPEN LOCAL QUEUE                             
         BE    MQINI42             Get next                                     
         DC    H'0'                                                             
*                                                                               
MQINI48  LR    R4,R5               Restore R4                                   
*                                                                               
MQINI80  AHI   R4,SCHQTABL         NEXT IN LIST                                 
         B     MQINI30                                                          
         DROP  R4                                                               
*                                                                               
MQINI86  LHI   R0,22                                                            
         BRAS  RE,SYSMESS          OPENED OTHER QUEUES                          
*                                                                               
         LHI   R0,10                                                            
         BRAS  RE,SYSMESS          COMPLETED MQ INITIALISE                      
         MVI   CONOK,YES                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISCONNECT FROM MQ CLEANLY                               *         
***********************************************************************         
MQCLS    NTR1  ,                                                                
         CLI   CONOK,YES                                                        
         BNE   EXITOK                                                           
*                                                                               
         LHI   R0,14                                                            
         BRAS  RE,SYSMESS          OPENED MASTER QUEUE                          
*                                                                               
         LA    RF,MAINQHOB         SET A(MAIN QUEUE HOBJ)                       
         ST    RF,MQCLSHOB                                                      
         XR    RF,RF                                                            
         ST    RF,CLS_OPTS                                                      
         LAY   R2,MQCLOSE                                                       
         BRAS  RE,CALLMQ           OPEN MASTER QUEUE                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LHI   R0,16                                                            
         BRAS  RE,SYSMESS          CLOSED MQ MASTER QUEUE                       
*                                                                               
         CLI   LOG,NO                                                           
         BE    MQCLS02                                                          
*                                                                               
         LA    RF,LOGQHOB          SET A(LOG QUEUE HOBJ)                        
         ST    RF,MQCLSHOB                                                      
         XR    RF,RF                                                            
         ST    RF,CLS_OPTS                                                      
         LAY   R2,MQCLOSE                                                       
         BRAS  RE,CALLMQ           CLOSE LOG QUEUE                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LHI   R0,17                                                            
         BRAS  RE,SYSMESS          CLOSED MQ LOG QUEUE                          
*                                                                               
MQCLS02  LA    RF,DEADQHOB         SET A(DEAD MSG QUEUE HOBJ)                   
         ST    RF,MQCLSHOB                                                      
         XR    RF,RF                                                            
         ST    RF,CLS_OPTS                                                      
         LAY   R2,MQCLOSE                                                       
         BRAS  RE,CALLMQ           CLOSE DEAD MSG QUEUE                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LHI   R0,25                                                            
         BRAS  RE,SYSMESS          CLOSED MQ DEAD MSG QUEUE                     
*                                                                               
         USING SCHQTABD,R4                                                      
         L     R4,ASCHQTAB                                                      
*                                                                               
MQCLS04  CLI   0(R4),X'FF'                                                      
         BE    MQCLS30                                                          
*                                                                               
         ICM   RF,15,SCHQCMQ                                                    
         BZ    MQCLS20                                                          
         CLC   =CL5'NONE ',MAINQ_OBJECTNAME-MAINQ(RF)                           
         BE    MQCLS20                                                          
         CLI   MAINQ_OBJECTNAME-MAINQ(RF),C' '                                  
         BNH   MQCLS20                                                          
*                                                                               
         LA    RF,SCHQHOBJ         SET A(RETURN FOR HOBJ)                       
         ST    RF,MQCLSHOB                                                      
         XR    RF,RF                                                            
         ST    RF,CLS_OPTS                                                      
         LAY   R2,MQCLOSE                                                       
         BRAS  RE,CALLMQ           CLOSE QUEUE                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    SCHQFLG1,SCHQ1NLG   SUPPRESS LOGGING                             
         BO    MQCLS10                                                          
         TM    SCHQFLG1,SCHQ1ALG   ALWAYS LOG?                                  
         BO    *+12                                                             
         CLI   LOG,NO              WANT TO LOG?                                 
         BE    MQCLS10                                                          
*                                                                               
         L     RF,SCHQCMQL                                                      
         CLI   MAINQ_OBJECTNAME-MAINQ(RF),C' '                                  
         BNH   MQCLS10                                                          
*                                                                               
         LA    RF,SCHQHLOG         SET A(RETURN FOR HOBJ)                       
         ST    RF,MQCLSHOB                                                      
         XR    RF,RF                                                            
         ST    RF,CLS_OPTS                                                      
         LAY   R2,MQCLOSE                                                       
         BRAS  RE,CALLMQ           CLOSE QUEUE                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MQCLS10  DS    0H                                                               
         LR    R5,R4               Save R4                                      
MQCLS12  LT    R4,SCHQXLNK         Another queue to close?                      
         JZ    MQCLS18             No                                           
         ICM   RF,15,SCHQCMQ                                                    
         BZ    MQCLS18                                                          
         CLC   =CL5'NONE ',MAINQ_OBJECTNAME-MAINQ(RF)                           
         BE    MQCLS18                                                          
         CLI   MAINQ_OBJECTNAME-MAINQ(RF),C' '                                  
         BNH   MQCLS18                                                          
*                                                                               
         LA    RF,SCHQHOBJ         SET A(RETURN FOR HOBJ)                       
         ST    RF,MQCLSHOB                                                      
         XR    RF,RF                                                            
         ST    RF,CLS_OPTS                                                      
         LAY   R2,MQCLOSE                                                       
         BRAS  RE,CALLMQ           Close queue                                  
         BE    MQCLS12             Check if another                             
         DC    H'0'                                                             
*                                                                               
MQCLS18  LR    R4,R5               Restore R4                                   
*                                                                               
MQCLS20  AHI   R4,SCHQTABL                                                      
         B     MQCLS04                                                          
         DROP  R4                                                               
*                                                                               
MQCLS30  LHI   R0,23                                                            
         BRAS  RE,SYSMESS          CLOSED QUEUES                                
*                                                                               
         LAY   R2,MQDISC                                                        
         BRAS  RE,CALLMQ           DISCONNECT MQ QMGR                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LHI   R0,15                                                            
         BRAS  RE,SYSMESS          COMPLETED MQ DEALLOCATION                    
         MVI   CONOK,NO                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALL MQ                                                  *         
* NTRY: R2          = A(PARAMETER STRUCTURE)                          *         
* EXIT: MQ_CC       = MQ COMPLETION CODE                              *         
*       MQ_RC       = MQ REASON CODE                                  *         
*       CC SET EQ     WHEN CALL OKAY                                  *         
***********************************************************************         
CALLMQ   NTR1  ,                                                                
         SAM31                                                                  
*                                                                               
         L     RF,16(R2)           RF = A(MQ ROUTINE)                           
         LA    R3,20(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
*                                                                               
         SAM24                                                                  
         CLC   MQ_CC,=A(MQCC_OK)                                                
         BE    CMQ02                                                            
*                                                                               
         LA    RF,MQGET            MQGET CAN HAVE NZ CC                         
         CR    R2,RF               IF SO THE FOLLOWING 2 FIELDS ARE SET         
         BNE   CMQ04                                                            
         CLC   MQ_CC,=A(MQCC_WARNING)                                           
         BNE   CMQ04                                                            
         CLC   MQ_RC,=A(MQRC_SIGNAL_REQUEST_ACCEPTED)                           
         BNE   CMQ04                                                            
*                                                                               
         CLI   TRACE,NO                                                         
         BE    EXITH                                                            
         MVI   PLINE,C'+'          '+' MEANS IT'S AN MQ CALL                    
         MVC   PLINE+1(16),0(R2)   PRINT ROUTINE NAME AND RETURN CODES          
         MVC   PLINE+30(35),=CL35'Waiting for new message'                      
         BRAS  RE,PRNT                                                          
         B     EXITH               FLAG THAT WE NEED TO WAIT                    
*                                                                               
CMQ02    CLI   TRACE,NO                                                         
         BE    EXITOK                                                           
         MVI   PLINE,C'+'          '+' MEANS IT'S AN MQ CALL                    
         MVC   PLINE+1(16),0(R2)   PRINT ROUTINE NAME AND RETURN CODES          
         MVC   PLINE+30(12),=C'Completed ok'                                    
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
*                                                                               
CMQ04    MVI   PLINE,C'+'                                                       
         MVC   PLINE+1(16),0(R2)                                                
         MVC   PLINE+20(09),=C'**ERROR**'                                       
         MVC   PLINE+30(08),=CL08'Warning '                                     
         CLC   MQ_CC,=A(MQCC_WARNING)                                           
         BE    CMQ06                                                            
         MVC   PLINE+30(08),=CL08'Failed  '                                     
         CLC   MQ_CC,=A(MQCC_FAILED)                                            
         BE    CMQ06                                                            
         MVC   PLINE+30(08),=CL08'Unknown '                                     
         EDIT  MQ_CC,(7,PLINE+38),ALIGN=LEFT,ZERO=NOBLANK                       
*                                                                               
CMQ06    MVC   PLINE+46(3),=C'RC='                                              
         EDIT  MQ_RC,(5,PLINE+49),ALIGN=LEFT,ZERO=NOBLANK                       
*                                                                               
         L     RF,AWHY                                                          
CMQ08    CLI   0(RF),X'FF'         SEE IF WE HAVE TEXT FOR THE PROBLEM          
         BE    CMQ10                                                            
         CLC   MQ_RC,0(RF)                                                      
         BE    *+12                                                             
         AHI   RF,28                                                            
         B     CMQ08                                                            
         MVC   PLINE+94(24),4(RF)                                               
*                                                                               
CMQ10    BRAS  RE,PRNT                                                          
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* SET UP OPERATOR COMMUNICATIONS                                      *         
***********************************************************************         
SETOPS   NTR1  ,                                                                
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
         EJECT                                                                  
***********************************************************************         
* READ IN ALL INPUT CARDS                                             *         
* FORMAT INTO A SCANNER BLOCK AND SAVE FOR VALIDATION                 *         
***********************************************************************         
GETCARDS NTR1  ,                                                                
         LHI   R0,3                                                             
         BRAS  RE,SYSMESS          STARTING READING INPUT CARDS                 
*                                                                               
         USING SCANBLKD,R3                                                      
         LAY   R3,SCANTAB          SAVE IN SCANTAB FOR PROCESSING               
         MVC   FULL,ASCHQTAB                                                    
*                                                                               
GCD010   GOTO1 VCARDS,PLIST,CARDIO,=C'RE00'                                     
         MVC   PLINE(L'CARDIO),CARDIO                                           
         BRAS  RE,PRNT             ALWAYS PRINT THE LINE                        
*                                                                               
         CLI   CARDIO,C'*'         IGNORE COMMENTS                              
         BE    GCD010                                                           
         CLC   =C'/*',CARDIO       END OF CARDS?                                
         BE    GCDX                YES                                          
*                                                                               
         CLC   =C'QINPUT=',CARDIO  OVERRDIDE INPUTQ?                            
         BNE   *+14                                                             
         MVC   QINP_OV,CARDIO+7                                                 
         B     GCD010              NEXT CARD                                    
                                                                                
         CLC   =C'QLOG=',CARDIO    OVERRDIDE LOGQ?                              
         BNE   *+14                                                             
         MVC   QLOG_OV,CARDIO+5                                                 
         B     GCD010              NEXT CARD                                    
                                                                                
         CLC   =C'QDEAD=',CARDIO   OVERRDIDE DEADQ?                             
         BNE   *+14                                                             
         MVC   QDEAD_OV,CARDIO+6                                                
         B     GCD010              NEXT CARD                                    
*                                                                               
         CLC   =C'QLABEL=',CARDIO                                               
         BE    GCD020                                                           
         CLC   =C'QLABELC=',CARDIO                                              
         BE    GCD020                                                           
         CLC   =C'QNAME=',CARDIO                                                
         BE    GCD020                                                           
         CLC   =C'QLLOG=',CARDIO                                                
         BE    GCD020                                                           
         CLC   =C'QLLOGOPTION=',CARDIO                                          
         BE    GCD020                                                           
         CLC   =C'QEXTRA=',CARDIO                                               
         BE    GCD020                                                           
         CLC   =C'QHOOK=',CARDIO                                                
         BE    GCD020                                                           
         CLC   =C'QMAXSIZE=',CARDIO                                             
         BE    GCD020                                                           
         CLC   =C'QLOPTIONS=',CARDIO                                            
         BE    GCD020                                                           
         CLC   =C'QLEMAIL=',CARDIO                                              
         BNE   GCD030                                                           
*                                                                               
GCD020   BRAS  RE,GCQINF                                                        
         BE    GCD010              NEXT CARD                                    
         BRAS  RE,ERRMSG                                                        
         B     EXITL                                                            
*                                                                               
GCD030   GOTO1 VSCANNER,PLIST,(C'C',CARDIO),((R3))                              
         XR    RF,RF                                                            
         ICM   RF,1,4(R1)          RF=NUMBER OF INPUT PARAMETERS                
         BZ    GCD040                                                           
*                                                                               
         XR    R0,R0               INCREMENT NUMBER OF PARAMETERS               
         IC    R0,PARMCNT                                                       
         AR    R0,RF                                                            
         CHI   R0,PARMCNTQ                                                      
         BH    GCD060                                                           
         STC   R0,PARMCNT                                                       
*                                                                               
         MHI   RF,SCBLKLQ          GO TO NEXT FREE SLOT IN SCANTAB              
         AR    R3,RF                                                            
         B     GCD010                                                           
                                                                                
GCD040   MVI   FERN,04             INVALID FORMAT TO PARAMETER CARD             
         BRAS  RE,ERRMSG                                                        
         B     EXITL                                                            
*                                                                               
GCD060   MVI   FERN,05             TOO MANY PARAMETERS                          
         BRAS  RE,ERRMSG                                                        
         B     EXITL                                                            
*                                                                               
GCDX     LHI   R0,4                                                             
         BRAS  RE,SYSMESS          COMPLETED READING INPUT CARDS                
         BRAS  RE,PRNT             SPACE LINE                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* READ IN ALL MESSAGE LABEL, QNAME, QLOG INFORMATION                  *         
***********************************************************************         
GCQINF   NTR1  ,                                                                
         CLC   =C'QLABEL=',CARDIO                                               
         BNE   GCQ010                                                           
         BRAS  RE,VCQLABEL                                                      
         B     EXIT                                                             
*                                                                               
GCQ010   EQU   *                                                                
         CLC   =C'QLABELC=',CARDIO                                              
         BNE   GCQ015                                                           
         BRAS  RE,VCQLABLC                                                      
         B     EXIT                                                             
*                                                                               
GCQ015   EQU   *                                                                
         CLC   =C'QNAME=',CARDIO                                                
         BNE   GCQ020                                                           
         BRAS  RE,VCQNAME                                                       
         B     EXIT                                                             
*                                                                               
GCQ020   EQU   *                                                                
         CLC   =C'QEXTRA',CARDIO                                                
         BNE   GCQ030                                                           
         BRAS  RE,VCEXTRA                                                       
         B     EXIT                                                             
*                                                                               
GCQ030   EQU   *                                                                
         CLC   =C'QLLOG=',CARDIO                                                
         BNE   GCQ035                                                           
         BRAS  RE,VCQLLOG                                                       
         B     EXIT                                                             
*                                                                               
GCQ035   EQU   *                                                                
         CLC   =C'QLLOGOPTION=',CARDIO                                          
         BNE   GCQ040                                                           
         BRAS  RE,VCQLLGO                                                       
         B     EXIT                                                             
*                                                                               
GCQ040   EQU   *                                                                
         CLC   =C'QHOOK=',CARDIO                                                
         BNE   GCQ050                                                           
         BRAS  RE,VCQHOOK                                                       
         B     EXIT                                                             
*                                                                               
GCQ050   EQU   *                                                                
         CLC   =C'QMAXSIZE=',CARDIO                                             
         BNE   GCQ060                                                           
         BRAS  RE,VCQMAXL                                                       
         B     EXIT                                                             
*                                                                               
GCQ060   EQU   *                                                                
         CLC   =C'QLOPTIONS=',CARDIO                                            
         BNE   GCQ070                                                           
         BRAS  RE,VCQLOPT                                                       
         B     EXIT                                                             
*                                                                               
GCQ070   EQU   *                                                                
         CLC   =C'QLEMAIL=',CARDIO                                              
         BNE   GCQ080                                                           
         BRAS  RE,VCQSEND2                                                      
         B     EXIT                                                             
*                                                                               
GCQ080   EQU   *                                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE PARAMETER CARDS IN SCANNER BLOCK                           *         
***********************************************************************         
VALCARDS NTR1  ,                                                                
         LHI   R0,5                                                             
         BRAS  RE,SYSMESS          STARTING VALIDATE INPUT CARDS                
         MVI   GOTERR,NO                                                        
*                                                                               
         XR    R2,R2                                                            
         ICM   R2,1,PARMCNT                                                     
         BZ    EXITOK                                                           
*                                                                               
         USING SCANBLKD,R3                                                      
VCD02    LAY   R3,SCANTAB                                                       
*                                                                               
VCD04    XR    RF,RF               RECONSTRUCT INPUT PARAMETER                  
         IC    RF,SC1STLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PLINE(0),SC1STFLD                                                
         LA    RE,PLINE+1(RF)                                                   
*                                                                               
         CLI   SC2NDLEN,0          ANY SECOND HALF TO PARAMETER                 
         BE    VCD06                                                            
         MVI   0(RE),C'='                                                       
         XR    RF,RF               RECONSTRUCT INPUT PARAMETER                  
         IC    RF,SC2NDLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RE),SC2NDFLD                                                 
*                                                                               
VCD06    BRAS  RE,PRNT             PRINT RECONSTRUCTED PARAMETER                
*                                                                               
         USING CARDTABD,R4                                                      
         L     R4,ACARDTAB         TABLE OF SUPPORTED PARAMETERS                
         XR    RF,RF                                                            
         IC    RF,SC1STLEN         LENGTH OF PARAMETER                          
         BCTR  RF,0                                                             
*                                                                               
VCD08    CLI   0(R4),255           END OF TABLE?                                
         BNE   *+16                                                             
         MVI   FERN,01             FLAG UNKNOWN PARAMETER CARD                  
         BRAS  RE,ERRMSG                                                        
         B     VCD14                                                            
*                                                                               
         CLM   RF,1,CMIN           CHECK LENGTHS ARE OK                         
         BL    VCD10                                                            
         CLM   RF,1,CMAX                                                        
         BH    VCD10                                                            
         EX    RF,*+8              MATCH TEXT STRING                            
         BE    VCD12                                                            
         CLC   SC1STFLD(0),CARDTXT                                              
*                                                                               
VCD10    AHI   R4,CARDTABL         TRY NEXT ENTRY                               
         B     VCD08                                                            
*                                                                               
VCD12    ICM   RF,15,CARDVAL       GO AND VALIDATE THIS INPUT                   
         BASR  RE,RF                                                            
         BE    *+8                                                              
         BRAS  RE,ERRMSG           PRINT ERROR MESSAGE                          
*                                                                               
VCD14    AHI   R3,SCBLKLQ          NEXT LINE IN SCANTAB                         
         BCT   R2,VCD04            ANY MORE PARMS INPUT?                        
*                                                                               
         LHI   R0,6                                                             
         BRAS  RE,SYSMESS          END VALIDATE INPUT CARDS                     
         CLI   GOTERR,YES          SET CC BASED ON ERRORS                       
         BNE   EXITOK                                                           
         B     EXITL                                                            
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF DSPACE= CARD                                          *         
***********************************************************************         
         USING SCANBLKD,R3                                                      
VCDSPACE NTR1  ,                                                                
         CLI   SC2NDLEN,1                                                       
         BE    *+12                                                             
         MVI   FERN,02                                                          
         B     EXITL                                                            
*                                                                               
         ICM   RF,15,ASSB                                                       
         MVC   SSODSPAC-SSOOFF(L'SSODSPAC,RF),SC2NDFLD                          
*                                                                               
                                                                                
         CLI   SSODSPAC-SSOOFF(RF),C'T'                                         
         BNE   VCDSP10                                                          
         MVC   QMGR,QMGRT                                                       
         B     VCDSP90                                                          
*                                                                               
VCDSP10  CLI   SSODSPAC-SSOOFF(RF),C'Q'                                         
         BNE   VCDSP20                                                          
         MVC   QMGR,QMGRQ                                                       
         B     VCDSP90                                                          
*                                                                               
VCDSP20  CLI   SSODSPAC-SSOOFF(RF),C'C'                                         
         BNE   VCDSP30                                                          
         MVC   QMGR,QMGRC                                                       
         B     VCDSP90                                                          
                                                                                
VCDSP30  MVI   ETYPE,C'P'          SET MQ BROKER QUEUES TO PRODUCTION           
         B     EXITOK                                                           
*                                                                               
VCDSP90  MVC   ETYPE,SC2NDFLD      SET MQ BROKER QUEUES TO TEST                 
         MVC   DSPACE,SC2NDFLD                                                  
         B     EXITOK                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF DDSIO= CARD                                           *         
***********************************************************************         
VCDDSIO  NTR1  ,                                                                
         CLI   SC2NDLEN,8          DDSIO REPLACEMENT MAX LENGTH                 
         BNH   *+12                                                             
         MVI   FERN,03                                                          
         B     EXITL                                                            
*                                                                               
         ICM   RF,15,VDDSIO                                                     
         MVC   0(8,RF),SC2NDFLD                                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF TRACE= CARD                                           *         
* WE SUPPORT Y/N/L/V       L=LIMITED, V=VERBOSE                       *         
***********************************************************************         
VCTRACE  NTR1  ,                                                                
         MVC   TRACE,SC2NDFLD                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF QMGR= OVERRIDE CARD (OVERRIDES MQ1P DEFAULT CL10 MAX) *         
***********************************************************************         
VCQMGR   NTR1  ,                                                                
         MVC   QMGR(L'SC2NDFLD),SC2NDFLD                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF LOGGING=CARD (DEFAULT IS 'Y' UNLESS YOU HAVE AN 'N')  *         
***********************************************************************         
VCLOG    NTR1  ,                                                                
         MVC   LOG,SC2NDFLD                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF ETYPE=CARD                                            *         
***********************************************************************         
VCETYPE  NTR1  ,                                                                
         MVC   ETYPE,SC2NDFLD      OVERRIDE WHAT IS SET IN DSPACE=              
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF EMAIL=CARD (DEFAULT IS 'Y')                           *         
***********************************************************************         
VCEMAIL  NTR1  ,                                                                
         MVC   EMAIL,SC2NDFLD                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF RUNACK=CARD (DEFAULT IS 'N')                          *         
***********************************************************************         
VCRUNACK NTR1  ,                                                                
         MVC   RUNACK,SC2NDFLD                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF QLABEL= CARD                                          *         
* VALIDATION OF QLABELC= CARD                                         *         
* VALIDATION OF QNAME= CARD                                           *         
* VALIDATION OF QLLOG= CARD           OPTIONAL                        *         
* VALIDATION OF QLLOGOPTION= CARD     OPTIONAL                        *         
* VALIDATION OF QLOPTIONS= CARD       OPTIONAL (can have mulitple)    *         
* INPUT: FULL=A(CURRENT SCHQTAB ENTRY)                                *         
*        CARDIO=INPUT CARD                                            *         
* ALL THESE PARMS SHOULD COME IN AS A SET, START WITH                 *         
*    QLABEL= AND THEN QNAME=                                          *         
*                WITH QLLOG=/QLLOGOPTION=/QLOPTIONS= AS OPTIONAL      *         
* OR                                                                  *         
*    QLABELC= AND THEN QNAME=                                         *         
*                WITH QLLOG=/QLLOGOPTION=/QLOPTIONS= AS OPTIONAL      *         
***********************************************************************         
         USING SCHQTABD,R4                                                      
VCQLABEL NTR1  ,                   QLABEL=    (REQUIRE)                         
         L     R4,FULL                                                          
         AHI   R4,SCHQTABL         BUMP TO NEXT ENTRY BEFORE ADD                
         ST    R4,FULL                                                          
*                                                                               
         C     R4,=A(SCHQTXX)      END OF THE TABLE                             
         BNH   *+12                    (LAST RESERVE ENTRY)                     
         MVI   FERN,14                                                          
         B     EXITL                                                            
*                                                                               
         MVC   SCHQID,CARDIO+7                                                  
         B     EXITOK                                                           
***********************************************************************         
*                                                                               
***********************************************************************         
VCQLABLC NTR1  ,                   QLABELC=    (REQUIRE)                        
         L     R4,FULL                                                          
         AHI   R4,SCHQTABL         BUMP TO NEXT ENTRY BEFORE ADD                
         ST    R4,FULL                                                          
*                                                                               
         C     R4,=A(SCHQTXX)      END OF THE TABLE                             
         BNH   *+12                    (LAST RESERVE ENTRY)                     
         MVI   FERN,14                                                          
         B     EXITL                                                            
*                                                                               
         MVC   SCHQID,CARDIO+8                                                  
         OI    SCHQFLG2,SCHQ2LCT                                                
         B     EXITOK                                                           
***********************************************************************         
*                                                                               
***********************************************************************         
VCQNAME  NTR1  ,                   QNAME=     (REQUIRE)                         
         L     R4,FULL                                                          
         MVC   SCHQNPRD,CARDIO+6                                                
         B     EXITOK                                                           
***********************************************************************         
* VALIDATION OF SEND TO (send error email too)                                  
***********************************************************************         
VCQSEND2 NTR1  ,                                                                
         L     R4,FULL                                                          
         MVC   SCHQSEND,CARDIO+8  QLEMAIL=    (Optional)                        
         LA    RE,CARDIO+50                                                     
         LA    RF,42                                                            
VCQSND10 CLI   0(RE),C' '                                                       
         BH    VCQSND20                                                         
         SHI   RF,1                                                             
         BCT   RE,VCQSND10                                                      
         B     VCQSND22                                                         
*                                                                               
VCQSND20 CHI   RF,24                                                            
         JNH   EXITOK                                                           
*                                                                               
VCQSND22 MVI   FERN,20                                                          
         B     EXITL                                                            
***********************************************************************         
*                                                                               
***********************************************************************         
VCQLLOG  NTR1  ,                   QLLOG=      (OPTIONAL)                       
         L     R4,FULL                                                          
         MVC   SCHQLPRD,CARDIO+6                                                
         B     EXITOK                                                           
***********************************************************************         
*                                                                               
***********************************************************************         
VCQLLGO  NTR1  ,                   QLLOGOPTION=   (OPTIONAL)                    
         L     R4,FULL                                                          
         CLC   =C'NEVER',CARDIO+12                                              
         BE    VCQLL10                                                          
         CLC   =C'ALWAYS',CARDIO+12                                             
         BE    VCQLL20                                                          
         MVI   FERN,18             INVALID LOGGING OPTION                       
         B     EXITL                                                            
*                                                                               
VCQLL10  OI    SCHQFLG1,SCHQ1NLG                                                
         B     EXITOK                                                           
VCQLL20  OI    SCHQFLG1,SCHQ1ALG                                                
         B     EXITOK                                                           
***********************************************************************         
*                                                                               
***********************************************************************         
VCQLOPT  NTR1  ,                   QLOPTIONS=GENERAL OPTIONS (OPTIONAL)         
         L     R4,FULL                                                          
         CLC   =C'DROPHEADER',CARDIO+10                                         
         BE    VCQOPT10                                                         
         MVI   FERN,19             INVALID OPTION                               
         B     EXITL                                                            
*                                                                               
VCQOPT10 OI    SCHQFLG2,SCHQ2DMH                                                
         B     EXITOK                                                           
***********************************************************************         
* Extra queue should be last in list for queue if you want to pick up           
*       same values as the root queue                                           
***********************************************************************         
X        USING SCHQTABD,R5                                                      
VCEXTRA  NTR1  ,                   QEXTRA=    (OPTIONAL) multiple               
         L     R4,FULL                                                          
         LR    R5,R4                                                            
VCEXTRA2 LR    RF,R5               Remember last good one                       
         LT    R5,X.SCHQXLNK                                                    
         BNZ   VCEXTRA2                                                         
         LR    R5,RF               Restore last good one                        
         L     RF,AQPOOL           Next entry                                   
         AHI   RF,SCHQTABL                                                      
         ST    RF,AQPOOL           Set new current queue pool entry             
         LLC   RF,POOL#            How many do we have total                    
         AHI   RF,1                                                             
         CHI   RF,QPOOLNQ          Max out?                                     
         JE    *+2                 Dump here  H'00'                             
         STC   RF,POOL#            Up it my one                                 
         MVC   X.SCHQXLNK,AQPOOL   Link to new Queue                            
         L     R5,AQPOOL           Now set new values in new queue              
         MVC   X.SCHQNPRD,CARDIO+7                                              
         MVC   X.SCHQID,SCHQID     Copy it over                                 
         OC    SCHQMAXL,SCHQMAXL                                                
         BZ    *+10                                                             
         MVC   X.SCHQMAXL,SCHQMAXL Copy max size message                        
         MVI   X.SCHQFLG1,0        No duplicate log for Extra Q                 
         MVC   X.SCHQFLG2,SCHQFLG2 Copy flag2                                   
*AH3     MVC   X.SCHQHOOK,SCHQHOOK Copy hook address (Not sure yet)             
         B     EXITOK                                                           
         DROP  X                                                                
***********************************************************************         
*                                                                               
***********************************************************************         
VCQHOOK  NTR1  ,                   QHOOK=     (OPTIONAL)                        
         L     R4,FULL                                                          
*                                                                               
         USING HOOKTABD,RE                                                      
         L     RE,=A(HOOKTAB)                                                   
VCQHK10  CLI   0(RE),X'FF'                                                      
         BNE   *+12                                                             
         MVI   FERN,15             UNDEFINED HOOK ROUTINE                       
         B     EXITL                                                            
*                                                                               
         CLC   HOOKTXT,CARDIO+6    MATCH ON HOOK ROUTINE NAME                   
         BE    VCQHK20             YES                                          
         AHI   RE,HOOKTABL         NO - TRY NEXT ENTRY                          
         B     VCQHK10                                                          
*                                                                               
VCQHK20  MVC   SCHQHOOK,HOOKADR    SAVE THE ADDRESS                             
         B     EXITOK                                                           
         DROP  RE                                                               
***********************************************************************         
*                                                                               
***********************************************************************         
VCQMAXL  NTR1  ,                   QMAXSIZE=  (OPTIONAL)                        
         L     R4,FULL                                                          
*                                                                               
         GOTO1 VNUMVAL,DMCB,CARDIO+9,(2,0)                                      
         CLI   DMCB,0                                                           
         BE    *+12                                                             
         MVI   FERN,16             NOT NUMERIC                                  
         B     EXITL                                                            
         L     R1,DMCB+4           #M                                           
         CHI   R1,MAXL#MB                                                       
         BNH   *+12                                                             
         MVI   FERN,17             EXCEED MAX SIZE                              
         B     EXITL                                                            
*                                                                               
         SR    R0,R0               PREPARE FOR MULTIPLY                         
         M     R0,=A(MB)           R1 = R1 * MB                                 
         ST    R1,SCHQMAXL         STORE MAX SIZE FOR MESSAGE TYPE              
         B     EXITOK                                                           
*                                                                               
         DROP  R4                  SCHQTABD                                     
         DROP  R3                  SCANBLKD                                     
         EJECT                                                                  
***********************************************************************         
* SORT THE SCHQTAB                                                    *         
***********************************************************************         
SRTSCHQT NTR1  ,                                                                
         GOTO1 VXSORT,DMCB,ASCHQTAB,SCHQTABN,SCHQTABL,L'SCHQID,0                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT ERROR MESSAGE                                      *         
***********************************************************************         
ERRMSG   NTR1  ,                                                                
         MVI   GOTERR,YES          SET GOT AN ERROR                             
*                                                                               
         LA    RF,ERRND                                                         
         CLI   FERN,0              UNDEFINED ERROR                              
         BE    ERRM02                                                           
*                                                                               
         XR    RF,RF               INDEX INTO ERROR TABLE                       
         IC    RF,FERN                                                          
         BCTR  RF,0                                                             
         MHI   RF,L'ERRMSGS                                                     
         A     RF,AERRMSG          RF = A(ERROR MESSAGE)                        
*                                                                               
ERRM02   MVC   PLINE,SPACES                                                     
         MVC   PLINE(L'ERRHDR),ERRHDR                                           
         MVC   PLINE+L'ERRHDR(L'ERRMSGS),0(RF)                                  
         CLI   FERN,11             Msg too big?                                 
         BE    *+8                 No                                           
         CLI   FERN,13             Msg too big?                                 
         BNE   ERRM05              No                                           
         L     RE,DATALEN          So how big is it?                            
         SRL   RE,20               Shift out Megabytes                          
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLINE+L'ERRHDR+L'ERRMSGS+3(3),DUB                                
         MVI   PLINE+L'ERRHDR+L'ERRMSGS+6,C'M'                                  
*                                                                               
ERRM05   BRAS  RE,PRNT                                                          
         MVC   SVERR,FERN                                                       
         MVI   FERN,0                                                           
         B     EXITOK                                                           
*                                                                               
ERRHDR   DC    C'   *** ERROR *** '                                             
ERRND    DC    CL45'Improperly defined error'                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT PLINE TO A PRINT LINE                             *         
***********************************************************************         
PRNT     NTR1  ,                                                                
         CLI   NOTIME,YES                                                       
         BE    PRNT02                                                           
*                                                                               
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
*                                                                               
PRNT02   MVC   P+12(L'PLINE),PLINE                                              
         MVC   PLINE,SPACES                                                     
         L     RF,ATRNTBL                                                       
         TR    P,0(RF)                                                          
         PUT   SYSPRINT,PCC                                                     
         MVI   PCC,C' '                                                         
         MVC   P,PCC                                                            
*                                                                               
         MVI   NOTIME,NO                                                        
         B     EXITOK                                                           
*                                                                               
PRNTDUB  DS    D                   FOR PRNT ROUTINE ONLY                        
PRNTTIME DS    CL9                 FOR PRNT ROUTINE ONLY                        
         EJECT                                                                  
***********************************************************************         
* PUT A MESSAGE TO THE OPERATOR CONSOLE AND OPTIONALLY GET A REPLY.   *         
* NTRY: R0        = MESSAGE NUMBER                                    *         
*       XTRAMESS  = OPTIONAL CL8 TO BE INSERTED INTO MESSAGE          *         
*       R0 ==  0  = SPECIAL MESSAGE AT 0(R1)                          *         
*                                                                     *         
* 1. IF THE FIRST CHARACTER OF THE MESSAGE IS AN 'X' JUST PUT OUT THE *         
* MESSAGE                                                             *         
* 2. IF IT IS A NUMBER THIS IS THE # OF CHARACTERS FOR THE RESPONSE   *         
* RESPONSE IS RETURNED IN 'REPLY' - "DUMP" AND "EOJ" HANDLED IN HERE  *         
* 3. ELSE THIS IS A MULTILINE MESSAGE AND IT LOOPS UNTIL (1) OR (2)   *         
***********************************************************************         
SYSMESS  NTR1  ,                                                                
         LR    R6,R1                                                            
         AHI   R0,-1                                                            
         BM    SYM02               R0 == 0 MEANS R1=A(MESSAGE)                  
*                                                                               
         MHI   R0,L'SYSMSGS                                                     
         L     R6,ASYSMSGS                                                      
         AR    R6,R0               R6=A(SYSMSGS ENTRY)                          
*                                                                               
SYM02    MVC   MESSAGE,SPACES      BUILD MESSAGE                                
         MVC   MESSAGE(8),=CL08'EDISCHED'                                       
         MVC   MESSAGE+9(L'SYSMSGS-1),1(R6)                                     
*                                                                               
         LA    R0,MESSAGE          NOW REPLACE SUBSTITUTE CHARS                 
         LA    R1,MESSAGE+L'MESSAGE-1                                           
SYM04    CR    R0,R1                                                            
         BE    SYM06                                                            
         CLC   0(8,R1),=8C'X'                                                   
         BE    *+8                                                              
         BCT   R1,SYM04                                                         
         MVC   0(8,R1),XTRAMESS                                                 
*                                                                               
SYM06    CLI   0(R6),C'0'                                                       
         BH    SYM08                                                            
         GOTO1 VLOGIO,DMCB,X'FF000001',(L'MESSAGE,MESSAGE)                      
         CLI   0(R6),C'X'                                                       
         BE    SYM12                                                            
         AHI   R6,L'SYSMSGS        SPECIAL MULTILINE MESSAGE                    
         B     SYM06                                                            
*                                                                               
SYM08    GOTO1 VLOGIO,DMCB,1,(L'MESSAGE,MESSAGE)                                
                                                                                
         XR    R0,R0                                                            
         ICM   R0,1,0(R6)                                                       
         N     R0,=X'0000000F'                                                  
         GOTO1 VLOGIO,DMCB,0,((R0),REPLY)                                       
*                                                                               
         CLC   REPLY(4),=C'DUMP'   CHECK FOR DUMP REPLY                         
         BNE   SYM10                                                            
         ABEND 666,DUMP                                                         
*                                                                               
SYM10    CLC   REPLY(3),=C'EOJ'    CHECK FOR EOJ REPLY                          
         BNE   SYM12                                                            
         MVI   OPERSTOP,YES        TREAT AS OPERATOR STOP                       
         MVC   REPLY,SPACES                                                     
*                                                                               
SYM12    MVC   XTRAMESS,SPACES     CLEAR THIS OUT                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TRACE THE INCOMING MESSAGE AND PRINT EXTRACTED HARDCOPY  *         
***********************************************************************         
TRACEIT  NTR1  ,                                                                
         ICM   R4,15,DATALEN                                                    
         BZ    EXITOK                                                           
*                                                                               
         L     R2,AMSGBUFF                                                      
         SAM31                                                                  
*                                                                               
         CLI   TRACE,C'L'          LIMITED TRACE?                               
         BNE   TRAC02              NO                                           
         MVC   PLINE(12),=CL12'New message:'                                    
         CHI   R4,100                                                           
         BL    *+8                                                              
         LHI   R4,100                                                           
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   PLINE+12(0),0(R2)                                                
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
*                                                                               
TRAC02   MVC   PLINE(50),=CL50'*** New Message - Trace Info follows:'           
         BRAS  RE,PRNT                                                          
*                                                                               
         CLI   TRACE,C'V'          VERBOSE HEADER DETAILS?                      
         BNE   TRAC04              NO                                           
*                                                                               
         MVC   PLINE(30),=CL30'*** MSG Descriptor details:'                     
         BRAS  RE,PRNT                                                          
         MVI   NOTIME,YES                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   PLINE+00(15),=CL15'Strucid       '                               
         MVC   PLINE+16(L'MSGDESC_STRUCID),MSGDESC_STRUCID                      
         MVC   PLINE+41(15),=CL15'Version       '                               
         MVC   FULL,MSGDESC_VERSION                                             
         BRAS  RE,HEX57                                                         
         MVI   NOTIME,YES                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   PLINE+00(15),=CL15'Report Options'                               
         MVC   FULL,MSGDESC_REPORT                                              
         BRAS  RE,HEX16                                                         
         MVC   PLINE+41(15),=CL15'Msgtype       '                               
         MVC   FULL,MSGDESC_MSGTYPE                                             
         BRAS  RE,HEX57                                                         
         MVI   NOTIME,YES                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   PLINE+00(15),=CL15'Expiry        '                               
         MVC   FULL,MSGDESC_EXPIRY                                              
         BRAS  RE,HEX16                                                         
         MVC   PLINE+41(15),=CL15'Feedback      '                               
         MVC   FULL,MSGDESC_FEEDBACK                                            
         BRAS  RE,HEX57                                                         
         MVI   NOTIME,YES                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   PLINE+00(15),=CL15'Encoding      '                               
         MVC   FULL,MSGDESC_ENCODING                                            
         BRAS  RE,HEX16                                                         
         MVC   PLINE+41(15),=CL15'Codedcharsetid'                               
         MVC   FULL,MSGDESC_CODEDCHARSETID                                      
         BRAS  RE,HEX57                                                         
         MVI   NOTIME,YES                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   PLINE+00(15),=CL15'Format        '                               
         MVC   PLINE+16(L'MSGDESC_FORMAT),MSGDESC_FORMAT                        
         MVC   PLINE+41(15),=CL15'Priority      '                               
         MVC   FULL,MSGDESC_PRIORITY                                            
         BRAS  RE,HEX57                                                         
         MVI   NOTIME,YES                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   PLINE+00(15),=CL15'Persistence    '                              
         MVC   FULL,MSGDESC_PERSISTENCE                                         
         BRAS  RE,HEX16                                                         
         MVC   PLINE+41(15),=CL15'Msgid          '                              
         MVC   PLINE+57(L'MSGDESC_MSGID),MSGDESC_MSGID                          
         MVI   NOTIME,YES                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   PLINE+00(15),=CL15'Correlid       '                              
         MVC   PLINE+16(L'MSGDESC_CORRELID),MSGDESC_CORRELID                    
         MVC   PLINE+41(15),=CL15'Backoutcount   '                              
         MVC   FULL,MSGDESC_BACKOUTCOUNT                                        
         BRAS  RE,HEX57                                                         
         MVI   NOTIME,YES                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   PLINE+00(15),=CL15'Replytoq       '                              
         MVC   PLINE+16(L'MSGDESC_REPLYTOQ),MSGDESC_REPLYTOQ                    
         MVI   NOTIME,YES                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   PLINE+00(15),=CL15'Replytoqmgr    '                              
         MVC   PLINE+16(L'MSGDESC_REPLYTOQMGR),MSGDESC_REPLYTOQMGR              
         MVI   NOTIME,YES                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   PLINE+00(15),=CL15'Useridentifier '                              
         MVC   PLINE+16(12),MSGDESC_USERIDENTIFIER                              
         MVC   PLINE+41(15),=CL15'Accountingtoken'                              
         MVC   PLINE+57(32),MSGDESC_ACCOUNTINGTOKEN                             
         MVI   NOTIME,YES                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   PLINE+00(15),=CL15'Applidentdata  '                              
         MVC   PLINE+16(32),MSGDESC_APPLIDENTITYDATA                            
         MVC   PLINE+41(15),=CL15'Putappltype    '                              
         MVC   FULL,MSGDESC_PUTAPPLTYPE                                         
         BRAS  RE,HEX57                                                         
         MVI   NOTIME,YES                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   PLINE+00(15),=CL15'Putapplname    '                              
         MVC   PLINE+16(28),MSGDESC_PUTAPPLNAME                                 
         MVI   NOTIME,YES                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   PLINE+00(15),=CL15'Putdate        '                              
         MVC   PLINE+16(L'MSGDESC_PUTDATE),MSGDESC_PUTDATE                      
         MVC   PLINE+41(15),=CL15'Puttime        '                              
         MVC   PLINE+57(L'MSGDESC_PUTTIME),MSGDESC_PUTTIME                      
         MVI   NOTIME,YES                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
*&&DO                                                                           
*THERE FIELDS SEEM NO LONGER VALID, 4/2/07, YYUN                                
         MVC   PLINE+00(15),=CL15'Applorigindata '                              
         MVC   PLINE+16(4),MSGDESC_APPLORIGINDATA                               
         MVC   PLINE+41(15),=CL15'Groupid        '                              
         MVC   PLINE+57(L'MSGDESC_GROUPID),MSGDESC_GROUPID                      
         MVI   NOTIME,YES                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   PLINE+00(15),=CL15'Msgseqnumber   '                              
         MVC   FULL,MSGDESC_MSGSEQNUMBER                                        
         BRAS  RE,HEX16                                                         
         MVC   PLINE+00(15),=CL15'Offset         '                              
         MVC   FULL,MSGDESC_OFFSET                                              
         BRAS  RE,HEX57                                                         
         MVI   NOTIME,YES                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   PLINE+00(15),=CL15'Msgflags       '                              
         MVC   FULL,MSGDESC_MSGFLAGS                                            
         BRAS  RE,HEX16                                                         
         MVC   PLINE+41(15),=CL15'Originallength '                              
         MVC   FULL,MSGDESC_ORIGINALLENGTH                                      
         BRAS  RE,HEX57                                                         
         MVI   NOTIME,YES                                                       
         BRAS  RE,PRNT                                                          
         MVI   NOTIME,YES                                                       
         BRAS  RE,PRNT                                                          
*&&                                                                             
*                                                                               
TRAC04   MVC   PLINE(20),=CL20'*** MSG Descriptor:'                             
         MVI   NOTIME,YES                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         LA    R2,MSGDESC                                                       
         LHI   R4,MSGDESC_LENGTH                                                
         BRAS  RE,HEXCHAR                                                       
*                                                                               
TRAC06   MVI   NOTIME,YES                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   PLINE(20),=CL20'*** Message Follows:'                            
         BRAS  RE,PRNT                                                          
*                                                                               
         CLI   TRACE,C'V'          VERBOSE DETAILS?                             
         BNE   TRAC10              NO                                           
         MVI   NOTIME,YES                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         L     R2,AMSGBUFF         TEXT DUMP THE MESSAGE                        
         SAM31                                                                  
         CLC   =C'FAXING',0(R2)                                                 
         BNE   *+12                                                             
         BRAS  RE,FAXLOG                                                        
         B     TRAC10                                                           
*                                                                               
         L     R4,DATALEN                                                       
         XC    FULL,FULL                                                        
*                                                                               
TRAC08   SAM31                                                                  
         LR    RF,R4                                                            
         CHI   RF,X'80'                                                         
         BL    *+8                                                              
         LHI   RF,X'80'                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PLINE+8(0),0(R2)                                                 
         SAM24                                                                  
*                                                                               
         GOTO1 VHEXOUT,DMCB,FULL+1,PLINE,3,0                                    
         MVI   NOTIME,YES                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         AHI   R4,-X'80'           LAST OF MESSAGE                              
         BNP   TRAC10                                                           
         AHI   R2,X'80'                                                         
*                                                                               
         L     RF,FULL                                                          
         AHI   RF,X'80'                                                         
         ST    RF,FULL                                                          
         B     TRAC08                                                           
*                                                                               
TRAC10   MVI   NOTIME,YES                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         L     R2,AMSGBUFF                                                      
         ICM   R4,15,DATALEN                                                    
         BRAS  RE,HEXCHAR                                                       
         B     EXITOK                                                           
*                                                                               
HEX16    NTR1  ,                                                                
         GOTO1 VHEXOUT,DMCB,FULL,PLINE+16,4,0                                   
         B     EXITOK                                                           
*                                                                               
HEX57    NTR1  ,                                                                
         GOTO1 VHEXOUT,DMCB,FULL,PLINE+57,4,0                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT OUT FAX DETAILS                                    *         
***********************************************************************         
FAXLOG   NTR1  ,                                                                
         L     R2,AMSGBUFF         TEXT DUMP THE MESSAGE                        
         SAM31                                                                  
         PACK  DUB,16(4,R2)                                                     
         CVB   R4,DUB                                                           
*                                                                               
FLOG02   LR    RF,R4                                                            
         CHI   RF,L'P                                                           
         BL    *+8                                                              
         LHI   RF,L'P                                                           
         BCTR  RF,0                                                             
         EX    RF,FLOGMV                                                        
         LA    R2,1(RF,R2)                                                      
*                                                                               
         PUT   SYSPRINT,PCC                                                     
         MVI   PCC,C' '                                                         
         MVC   P,PCC                                                            
*                                                                               
         AHI   R4,-L'P             LAST OF MESSAGE                              
         BP    FLOG02                                                           
*                                                                               
         L     R4,DATALEN                                                       
         CVB   R0,DUB                                                           
         SR    R4,R0                                                            
*                                                                               
FLOG04   LTR   R4,R4                                                            
         BNP   EXITOK                                                           
*                                                                               
FLOG06   PACK  DUB,0(4,R2)                                                      
         CVB   RF,DUB                                                           
         AHI   R2,4                                                             
         AHI   R4,-4                                                            
*                                                                               
FLOG08   CHI   RF,L'P                                                           
         BNH   FLOG10                                                           
*                                                                               
         ST    RF,FULL                                                          
         LHI   RF,L'P                                                           
         BCTR  RF,0                                                             
         EX    RF,FLOGMV                                                        
         PUT   SYSPRINT,PCC                                                     
         MVI   PCC,C' '                                                         
         MVC   P,PCC                                                            
*                                                                               
         L     RF,FULL                                                          
         AHI   RF,-L'P                                                          
         AHI   R4,-L'P                                                          
         AHI   R2,L'P                                                           
         BP    FLOG08                                                           
         B     FLOG04                                                           
*                                                                               
FLOG10   BCTR  RF,0                                                             
         EX    RF,FLOGMV                                                        
         AHI   RF,1                                                             
         AR    R2,RF                                                            
         SR    R4,RF                                                            
*                                                                               
         PUT   SYSPRINT,PCC                                                     
         MVI   PCC,C' '                                                         
         MVC   P,PCC                                                            
         B     FLOG04                                                           
*                                                                               
FLOGMV   MVC   P(0),0(R2)                                                       
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HEX/CHAR DUMP 32 CHARACTERS AT A TIME TO PLINE           *         
* NTRY: R2 = A(DATA)                                                  *         
*       R4 = L'DATA                                                   *         
***********************************************************************         
HEXCHAR  NTR1  ,                                                                
         XC    FULL,FULL           FULL = CURRENT DISPLACEMENT                  
         MVI   MUSTPRNT,NO                                                      
*                                                                               
HEX02    XC    WRK,WRK                                                          
         MVC   WORKC,SPACES                                                     
*                                                                               
         SAM31                                                                  
         LR    RF,R4                                                            
         CHI   RF,32               32 AT A TIME EXCEPT FOR LAST LINE            
         BL    *+8                                                              
         LHI   RF,32                                                            
                                                                                
         BCTR  RF,0                                                             
         EX    RF,WRKMV                                                         
         EX    RF,WRKCMV                                                        
         SAM24                                                                  
                                                                                
         MVC   WORK,SPACES                                                      
         LA    R0,1(RF)                                                         
         GOTO1 VHEXOUT,DMCB,WRK,WORK,(R0),0                                     
*                                                                               
         LHI   R0,31               REFORMAT IF ALL CHRS EQUAL IN LINE           
         LA    RF,WORK                                                          
         CLC   0(2,RF),2(RF)                                                    
         BNE   HEX04               DIFFERENT - JUST GO PRINT                    
         AHI   RF,2                                                             
         BCT   R0,*-14                                                          
                                                                                
         MVC   WORK+08(08),=CL08'--SAME--'                                      
         MVC   WORK+16(48),SPACES                                               
         MVC   WORKC+4(28),SPACES                                               
*                                                                               
         CLI   MUSTPRNT,YES        FORCE PRINT LAST LINE?                       
         BE    *+14                YES                                          
         CLC   WRKL,WRK            SEE IF SAME AS LAST LINE                     
         BE    HEX08               YES - SKIP OVER                              
*                                                                               
HEX04    GOTO1 VHEXOUT,DMCB,FULL+1,PLINE,3,0                                    
         LHI   R0,8                FORMAT DUMP PRINT LINE                       
         LA    R3,WORK                                                          
         LA    R6,PLINE+8                                                       
                                                                                
HEX06    MVC   0(8,R6),0(R3)                                                    
         LA    R6,9(,R6)                                                        
         LA    R3,8(,R3)                                                        
         CHI   R0,5                                                             
         BNE   *+8                                                              
         AHI   R6,1                                                             
         BCT   R0,HEX06                                                         
*                                                                               
         MVC   PLINE+083(16),WORKC                                              
         MVC   PLINE+100(16),WORKC+16                                           
*                                                                               
         MVI   NOTIME,YES                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
HEX08    CHI   R4,32               JUST DID THE LAST LINE?                      
         BNH   EXITOK              YES                                          
*                                                                               
         MVC   WRKL,WRK                                                         
         L     RF,FULL                                                          
         AHI   RF,32                                                            
         ST    RF,FULL                                                          
*                                                                               
         AHI   R2,32                                                            
         AHI   R4,-32                                                           
*                                                                               
         CHI   R4,32                                                            
         BH    *+8                                                              
         MVI   MUSTPRNT,YES                                                     
         B     HEX02                                                            
*                                                                               
WRKMV    MVC   WRK(0),0(R2)                                                     
WRKCMV   MVC   WORKC(0),0(R2)                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OPEN CONTROL FILE IF REQUIRED                            *         
***********************************************************************         
OPENCTFL NTR1  ,                                                                
         CLI   CTOPEN,YES                                                       
         BE    EXITOK                                                           
         MVI   CTOPEN,YES                                                       
         GOTO1 VDMGR,DMCB,DMOPEN,CONTROL,CTFLIST,IO,0                           
*&&UK                                                                           
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+0(4),=X'20008001'                                           
         GOTO1 ALOCKSPC,DMCB                                                    
*&&                                                                             
         B     EXITOK                                                           
*                                                                               
         DS    0D                                                               
         DC    C'CTOPEN='                                                       
CTOPEN   DC    AL1(NO)                                                          
CONTROL  DC    CL8'CONTROL '                                                    
CTFLIST  DC    C'NCTFILE X'                                                     
         EJECT                                                                  
***********************************************************************         
* SEND AN EMAIL WHEN A MESSAGE PUT INTO DEAD MSG QUEUE                *         
***********************************************************************         
         USING SCHQTABD,R4                                                      
ERREMAIL NTR1  ,                                                                
*                                                                               
         CLC   EMAILCNT,=F'5'      SKIP EMAIL AFTER 5 DIFF LABEL                
         JH    EXITOK              5 PER EACH GET-WAIT CYCLE                    
*                                                                               
         MVC   P,SPACES                                                         
         SAM31                                                                  
*&&US*&& MVC   P(25),=CL25'AUTONOTE*US-MF_FAC_NOTIFY'                           
*&&UK*&& MVC   P(25),=CL25'AUTONOTE*UK-MF_FAC_NOTIFY'                           
         CLI   EMAIL,NO                                                         
         JE    EXITOK                                                           
         CLC   SCHQSEND,SPACES                                                  
         JNH   *+10                Use default email                            
         MVC   P+9(24),SCHQSEND    Use queue specific email                     
*                                                                               
         LA    RF,30                                                            
         LA    RE,P+40                                                          
ERREM10  CLI   0(RE),C' '                                                       
         JH    ERREM12                                                          
         SHI   RE,1                                                             
         BCT   RF,ERREM10                                                       
         DC    H'00'                Bug in code                                 
*                                                                               
ERREM12  AHI   RE,1                                                             
         MVC   0(17,RE),=C':Cannot Process -'                                   
         CLI   SVERR,11                                                         
         JE    *+8                                                              
         CLI   SVERR,13                                                         
         JNE   *+10                                                             
         MVC   0(17,RE),=C':Message too big-'                                   
         AHI   RE,17                                                            
         L     RF,AMSGBUFF                                                      
         MVC   0(50,RE),0(RF)                                                   
         MVC   THLABEL,0(RF)                                                    
         SAM24                                                                  
*                                                                               
         CLC   SVLABEL,THLABEL     SAME AS LAST ERROR LABEL?                    
         JE    ERREM40             YES - SKIP THE EMAIL FOR NOW                 
         MVC   SVLABEL,THLABEL                                                  
         OC    P,SPACES                                                         
         GOTO1 VDMGR,DMCB,=C'OPMSG',(100,P)                                     
*                                                                               
         L     R1,EMAILCNT                                                      
         AHI   R1,1                                                             
         ST    R1,EMAILCNT                                                      
*                                                                               
ERREM40  DS    0H                                                               
         MVI   SVERR,0                                                          
         MVC   P,SPACES                                                         
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* THE OPERATOR HAS INTERRUPTED WITH EITHER A 'STOP' OR 'MODIFY'       *         
* COMMAND.  EXAMINE THE COMMAND AND TAKE THE APPROPRIATE ACTION.      *         
***********************************************************************         
CHKOPER  NTR1  ,                                                                
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
         MVI   BADOPCMD,NO                                                      
*                                                                               
         LA    R4,COMMTAB                                                       
         USING COMMTABD,R4                                                      
         XR    RF,RF                                                            
CH04     CLI   0(R4),X'FF'         EOT                                          
         BE    CHBAD               BAD COMMAND                                  
*                                                                               
         IC    RF,COMMLEN          GET MINIMUM LENGTH                           
         CH    RF,CIBDATLN         CHECK STRING LENGTH                          
         BH    CH06                                                             
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
         MVI   BADOPCMD,YES                                                     
*                                                                               
CHX      L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         LA    R3,COMCIBPT         A(A(CIB))                                    
         QEDIT ORIGIN=(R3),BLOCK=(R2)  FREE THE CIB                             
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
COMMTAB  DC    CL8'????????',AL1(8,0,0,0),AL4(OPDUMMY)                          
         DC    CL8'TRACE   ',AL1(5,0,0,0),AL4(OPTRACE)                          
         DC    X'FF'                                                            
*                                                                               
OPDUMMY  BR    RE                                                               
*                                                                               
OPTRACE  CLC   CIBDATLN,=H'7'      TRACE=?                                      
         BNE   CHBAD                                                            
         CLI   CIBDATA+5,C'='                                                   
         BNE   CHBAD                                                            
         MVC   TRACE,CIBDATA+6                                                  
         MVC   PLINE(16),=CL16'TRACE changed to'                                
         MVC   PLINE+18(1),TRACE                                                
         LR    R0,RE                                                            
         BRAS  RE,PRNT                                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
COMMTABD DSECT                     COVERS COMMTAB ABOVE                         
COMMCMD  DS    CL8                 INPUT COMMAND                                
COMMLEN  DS    X                   MINIMUM LENGTH                               
         DS    XL3                                                              
COMMRTN  DS    AL4                 A(PROCESSING ROUTINE)                        
COMMTABL EQU   *-COMMTABD                                                       
         EJECT ,                                                                
EDISCHED CSECT                                                                  
***********************************************************************         
*MQ MESSAGE HOOK ROUTINES                                             *         
***********************************************************************         
HKRUNIT  NTR1  ,                                                                
*                                                                               
         OC    DATEC,DATEC         TEST WE HAVE ESTABLISHED DATE/ASID           
         JNZ   HKRUN20                                                          
         GOTOR VDATCON,DMCB,(5,0),(2,DATEC)                                     
         EXTRACT ASIDFLD,'S',FIELDS=(ASID)                                      
*                                                                               
HKRUN20  EQU   *                                                                
         CLI   HK_MODE,HKM_PRC     PROCESS MESSAGE MODE                         
         BNE   HKRUN50                                                          
*                                                                               
         CLI   RUNACK,YES                                                       
         BNE   *+8                                                              
         BRAS  RE,SNDACK01         SEND BACK ACK01 MESSAGE                      
*                                                                               
         XC    WKEY,WKEY           CLEAR MQ MESSAGE ID                          
         L     R3,AMSGBUFF                                                      
         SAM31 ,                                                                
         USING LM_D,R3             R3=A(INPUT MESSAGE)                          
         MVC   AGENCY,LM_AGY       EXTRACT AGENCY AND RUNNER CLASS              
         MVC   CLASS,LM_LAB+L'RUNCLASS                                          
         SAM24 ,                                                                
         DROP  R3                                                               
*                                  TEST IF RUNNER IS AVALIABLE                  
         GOTOR VRUNIT,DMCB,(CLASS,0),(X'FF',AGENCY),0                           
         JNE   HKRUN90             NO - RETURN TO SENDER                        
*                                                                               
         MVC   WKEYASID,ASIDFLD+2  BUILD MQ MESSAGE ID AND ADD TO QUEUE         
         MVC   WKEYDATE,DATEC                                                   
         TIME  TU                                                               
         STCM  R0,15,WKEYTIME                                                   
*                                                                               
         XC    MSGDESC_MSGID,MSGDESC_MSGID                                      
         MVC   MSGDESC_MSGID(L'WKEY),WKEY                                       
         B     EXITOK                                                           
*                                                                               
HKRUN50  EQU   *                                                                
         CLI   HK_MODE,HKM_CMT                                                  
         BNE   EXITOK                                                           
*                                                                               
         GOTOR VRUNIT,DMCB,(CLASS,WKEY),AGENCY,0                                
         JE    EXITOK                                                           
         B     EXITL                                                            
*                                                                               
HKRUN90  DS    0H                                                               
         CLI   RUNACK,YES                                                       
         BE    HKRUN95                                                          
         L     R1,AMSGBUFF                                                      
         AHI   R1,22               R1=A(CL16 SENDER'S MESSAGE LABEL)            
         BRAS  RE,RTSNDER          RETURN MESSAGE BACK TO SENDER                
         B     HKRUN98                                                          
*                                                                               
HKRUN95  BRAS  RE,SENDERR          SEND BACK NO RUNNER ERROR MESSAGE            
*                                                                               
HKRUN98  MVI   FERN,10             NO RUNNER AVALIABLE                          
         B     EXITL                                                            
*                                                                               
RUNCLASS DC    C'RUNNERCLASS'                                                   
DATEC    DC    XL2'00'             TODAY'S DATE (COMPRESSED)                    
ASIDFLD  DC    F'0'                EDISCHED ASID                                
         EJECT ,                                                                
***********************************************************************         
*SEND BACK ACK01 MESSAGE                                              *         
*INPUT:  MESSAGE IS IN AMSGBUFF                                       *         
***********************************************************************         
SNDACK01 NTR1  ,                                                                
         SAM31                                                                  
         L     R1,AMSGBUFF         BUILD ACK01 MESSAGE                          
         L     R2,AMSGBUF2                                                      
         MVC   0(16,R2),22(R1)     GET MSG LABEL (OPTIMIZER*******)             
         MVC   16(4,R2),=CL4'0064' LEN OF KEY FOLLOW                            
         MVC   20(64,R2),38(R1)    KEY                                          
         MVC   84(6,R2),=CL6'ACK-01'                                            
         SAM24                                                                  
*                                                                               
         L     R1,AMSGBUF2                                                      
         LA    R2,90                                                            
         BRAS  RE,PUT2MSG          PUT OUT THE MESSAGE                          
SACK1X   XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
*SEND BACK ERROR MESSAGE FOR NO RUNNER                                *         
*INPUT:  MESSAGE IS IN AMSGBUFF                                       *         
***********************************************************************         
SENDERR  NTR1  ,                                                                
         SAM31                                                                  
         L     R1,AMSGBUFF         BUILD NO RUNNER ERROR MESSAGE                
         L     R2,AMSGBUF2                                                      
         MVC   0(16,R2),22(R1)     GET MSG LABEL (OPTIMIZER*******)             
         MVC   16(4,R2),=CL4'0064' LEN OF KEY FOLLOW                            
         MVC   20(64,R2),38(R1)    KEY                                          
         MVC   84(15,R2),=CL15'ERROR-NO RUNNER'                                 
         SAM24                                                                  
*                                                                               
         L     R1,AMSGBUF2                                                      
         LA    R2,99                                                            
         BRAS  RE,PUT2MSG          PUT OUT THE MESSAGE                          
SERRX    XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
*PUT OUT A MESSAGE BUILT INTERNALLY                                   *         
*INPUT:  R1=A(MESSAGE)                                                *         
*        R2=L'MESSAGE                                                 *         
***********************************************************************         
PUT2MSG  NTR1  ,                                                                
         ST    R1,MQPUTBF2                                                      
         ST    R2,DATALEN2                                                      
         SAM31                                                                  
         MVC   WORK(16),0(R1)                                                   
*                                                                               
         MVC   PLINE+30(80),0(R1)                                               
         SAM24                                                                  
         BRAS  RE,PRNT                                                          
*                                                                               
         USING SCHQTABD,R4                                                      
         L     R4,ASCHQTAB                                                      
P2MSG10  CLI   0(R4),X'FF'         EOT - BAD MESSAGE HEADER                     
         BE    P2MSG20                   ERROR AND EXIT                         
         CLC   SCHQID,WORK         DO WE KNOW THIS QUEUE?                       
         BE    P2MSG30             YES                                          
         AHI   R4,SCHQTABL                                                      
         B     P2MSG10                                                          
*                                                                               
P2MSG20  EQU   *                                                                
         MVI   FERN,18             BAD MESSAGE HEADER                           
         BRAS  RE,ERRMSG                                                        
         B     P2MSGX                                                           
*                                                                               
P2MSG30  DS    0H                                                               
         LA    R0,SCHQHOBJ         SET PUT QUEUE OBJECT                         
         L     RF,SCHQCMQ                                                       
         MVC   PLINE+55(48),MAINQ_OBJECTNAME-MAINQ(RF)  PARK QNAME              
         CLC   =CL5'NONE ',MAINQ_OBJECTNAME-MAINQ(RF)                           
         BE    P2MSGX              NO QUEUE TO PUT TO                           
         CLI   MAINQ_OBJECTNAME-MAINQ(RF),C' '                                  
         BH    P2MSG40                                                          
         MVI   FERN,9              NO OUTPUT QUEUE DEFINED                      
         BRAS  RE,ERRMSG                                                        
         B     P2MSGX                                                           
*                                  CHECK IF MSG SIZE TOO BIG                    
P2MSG40  EQU   *                                                                
         ICM   RE,15,DATALEN2                                                   
         C     RE,SCHQMAXL                                                      
         BNH   P2MSG50                                                          
         MVI   FERN,13             MSG TOO BIG TO RETURN TO SENDER              
         BRAS  RE,ERRMSG                                                        
         B     P2MSGX                                                           
*                                                                               
P2MSG50  ST    R0,MQPUTQN2                                                      
         LHI   RF,MQPMO_PASS_ALL_CONTEXT                                        
         ST    RF,PUTOPTS_OPTIONS                                               
         MVC   PUTOPTS_CONTEXT,MAINQHOB                                         
*                                                                               
         LAY   R2,MQPUT2                                                        
         BRAS  RE,CALLMQ                                                        
         BE    P2MSGX                                                           
         DC    H'0'                                                             
*                                                                               
P2MSGX   XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
*RETURN THE MESSAGE BACK TO THE SENDER                                *         
*INPUT:  R1=A(CL16 MESSAGE LABEL OF THE SENDER)                       *         
***********************************************************************         
RTSNDER  NTR1  ,                                                                
         SAM31                                                                  
         MVC   WORK(16),0(R1)                                                   
         SAM24                                                                  
*                                                                               
         USING SCHQTABD,R4                                                      
         L     R4,ASCHQTAB                                                      
RTSDR10  CLI   0(R4),X'FF'         EOT - BAD ECHO QUEUE HEADER                  
         BE    RTSDRX                    JUST EXIT                              
         CLC   SCHQID,WORK         DO WE KNOW THIS QUEUE?                       
         BE    RTSDR20             YES                                          
         AHI   R4,SCHQTABL                                                      
         B     RTSDR10                                                          
*                                                                               
RTSDR20  EQU   *                                                                
         LA    R0,SCHQHOBJ         SET PUT QUEUE OBJECT                         
         L     RF,SCHQCMQ                                                       
         CLC   =CL5'NONE ',MAINQ_OBJECTNAME-MAINQ(RF)                           
         BE    RTSDRX              NO QUEUE TO PUT TO                           
         MVC   PLINE+55(48),MAINQ_OBJECTNAME-MAINQ(RF)  PARK QNAME              
         CLI   MAINQ_OBJECTNAME-MAINQ(RF),C' '                                  
         BH    RTSDR30                                                          
         MVI   FERN,9              NO OUTPUT QUEUE DEFINED                      
         BRAS  RE,ERRMSG                                                        
         B     RTSDRX                                                           
*                                  CHECK IF MSG SIZE TOO BIG                    
RTSDR30  EQU   *                                                                
         ICM   RE,15,DATALEN                                                    
         C     RE,SCHQMAXL                                                      
         BNH   RTSDR40                                                          
         MVI   FERN,13             MSG TOO BIG TO RETURN TO SENDER              
         BRAS  RE,ERRMSG                                                        
         B     RTSDRX                                                           
*                                                                               
RTSDR40  ST    R0,MQPUTQN                                                       
         MVC   MQPUTBUF,AMSGBUFF                                                
         MVC   DATALENP,DATALEN                                                 
         LHI   RF,MQPMO_PASS_ALL_CONTEXT                                        
         ST    RF,PUTOPTS_OPTIONS                                               
         MVC   PUTOPTS_CONTEXT,MAINQHOB                                         
*                                                                               
         LAY   R2,MQPUT                                                         
         BRAS  RE,CALLMQ                                                        
         BE    RTSDR50                                                          
         DC    H'0'                                                             
*                                                                               
RTSDR50  MVI   FERN,12             OUTPUT RETURN TO SENDER MESSAGE              
         BRAS  RE,ERRMSG                                                        
*                                                                               
RTSDRX   XIT1                                                                   
                                                                                
***********************************************************************         
*TRY TO RECONNECT TO QUEUE MANAGER                                    *         
***********************************************************************         
RECON    NTR1  ,                                                                
*                                                                               
         MVC   PLINE(16),=CL16'Connectionbroken'                                
         BRAS  RE,PRNT                                                          
         XR    R3,R3                                                            
*                                                                               
         LAY   R2,MQDISC                                                        
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
         SPACE 2                                                                
TIMERECB DS    F                                                                
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMMON STORAGE/ROUTINES                                             *         
***********************************************************************         
         DROP  R9,RA                                                            
*                                                                               
         DS    0D                                                               
COMMON   DC    CL16'*COMMON**COMMON*'                                           
SAVERD   DC    F'0'                                                             
*                                                                               
EXITL    CLI   *,255               SET CC LOW                                   
         J     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         J     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         J     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
XBASE    L     RD,SAVERD                                                        
         XBASE ,                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
ERRUSER  EQU   1                   THESE COVER THE ERRORS PASSED BACK           
ERRSE    EQU   2                   WHEN THERE IS AN ERRORTO CARD IN             
ERRHDRQ  EQU   3                   THE HEADER                                   
ERRAUTH  EQU   4                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
VCPRINT  DC    V(CPRINT)                                                        
VCARDS   DC    V(CARDS)                                                         
VDDSIO   DC    V(DDSIO)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VLOGIO   DC    V(LOGIO)                                                         
VXSORT   DC    V(XSORT)                                                         
VDMGR    DC    V(DATAMGR)                                                       
VDMOD000 DC    V(DMOD000)                                                       
AFINDSYS DC    A(00000002)                                                      
VSCANNER DC    V(SCANNER)                                                       
VDATCON  DC    V(DATCON)                                                        
VNUMVAL  DC    V(NUMVAL)                                                        
VRUNIT   DC    V(RUNIT)                                                         
ALOCKSPC DC    V(LOCKSPC)                                                       
*                                                                               
ASYSMSGS DC    A(SYSMSGS)                                                       
ASCHQTAB DC    A(SCHQTAB)                                                       
AQPOOL   DC    A(QPOOLTAB)                                                      
AERRMSG  DC    A(ERRMSGS)                                                       
ACARDTAB DC    A(CARDTAB)                                                       
AWHY     DC    A(MQ_REASON_CODE_TABLE)                                          
ASSB     DC    A(SSB)                                                           
ATRNTBL  DC    A(TRNTBL)                                                        
MESSAGE  DS    CL60                                                             
XTRAMESS DS    XL8'00'                                                          
*                                                                               
DMREAD   DC    CL8'DMREAD  '                                                    
DMOPEN   DC    CL8'DMOPEN  '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
*                                                                               
PONE     DC    P'1'                                                             
P99      DC    P'99'                                                            
LOG      DC    AL1(YES)                                                         
RUNACK   DC    AL1(NO)                                                          
*                                                                               
         DS    0D                                                               
         DC    CL08'ECBLIST*'                                                   
ECBLST   DC    A(0)                A(OPERATOR ECB IS STORED HERE)               
         DC    A(GETECB)                                                        
         DC    X'80',AL3(TIMERECB) TIMER POP                                    
         DC    CL08'ECBLISTX'                                                   
*                                                                               
GETECB   DC    A(0)                                                             
AOPERECB DC    A(0)                A(ECB OF OPERATOR INTERRUPT)                 
ACOMM    DC    A(0)                A(COMMUNICATIONS PARAMETER LIST)             
*                                                                               
         DC    CL08'MSGBUFF*'                                                   
AMSGBUFF DC    A(0)                MESSAGE BUFFER                               
         DC    CL08'MSGBUF2*'                                                   
AMSGBUF2 DC    A(0)                MESSAGE BUFFER 2                             
*                                                                               
MAXMSGLN DC    A(MAXL#MB*MB)       MAX MESSAGE LENGTH                           
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
K        EQU   1024                                                             
MAXL#MB  EQU   50                  50M                                          
MB       EQU   1*K*K               1M                                           
*                                                                               
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(167)          
SPACES   DC    166C' '                                                          
         EJECT                                                                  
***********************************************************************         
* MQ STUFF                                                            *         
***********************************************************************         
         DS    0D                                                               
         DC    CL08'QMGR===>'                                                   
*&&US                                                                           
QMGR     DC    CL48'MQ1P'                                                       
QMGRT    DC    CL48'MQ7T'                                                       
QMGRQ    DC    CL48'MQ7Q'                                                       
QMGRC    DC    CL48'MQ7C'                                                       
*&&                                                                             
*&&UK                                                                           
QMGR     DC    CL48'MQ1L'                                                       
QMGRT    DC    CL48'MQTL'                                                       
QMGRQ    DC    CL48'MQQL'                                                       
QMGRC    DC    CL48'MQCL'                                                       
*&&                                                                             
         DC    CL8'HCONN==>'                                                    
HCONN    DC    F'0',F'0'           MQ QMGR CONNECTION HANDLE                    
         DC    CL8'OPN_OPTS'                                                    
OPN_OPTS DC    F'0',F'0'           MQOPEN OPTIONS                               
         DC    CL8'CLS_OPTS'                                                    
CLS_OPTS DC    F'0',F'0'           MQCLOSE OPTIONS                              
         DC    CL8'MQ_CC/RC'                                                    
MQ_CC    DC    F'0'                COMPLETION CODE                              
MQ_RC    DC    F'0'                QUALIFIES COMPLETION CODE                    
         DC    CL8'DATALEN '                                                    
DATALEN  DC    F'0',F'0'           LENGTH OF THE MESSAGE (MQGET)                
         DC    CL8'DATALEN2'                                                    
DATALEN2 DC    F'0',F'0'           LENGTH OF THE MESSAGE (MQPUT2)               
         DC    CL8'DATALENP'                                                    
DATALENP DC    F'0',F'0'           LENGTH OF THE MESSAGE (MQPUT)                
*                                                                               
         CMQA    LIST=YES,EQUONLY=NO                                            
         EJECT                                                                  
*                                                                               
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
         DS    0D                                                               
         DC    CL8'MAINQ==>'                                                    
MAINQHOB DC    F'0',F'0'                                                        
MAINQ    CMQODA  LIST=YES  OBJECT DESCRIPTOR                                    
*                                                                               
         DS    0D                                                               
         DC    CL8'LOGQ===>'                                                    
LOGQHOB  DC    F'0',F'0'                                                        
LOGQ     CMQODA  LIST=YES  OBJECT DESCRIPTOR                                    
*                                                                               
         DS    0D                                                               
         DC    CL8'DEADQ==>'                                                    
DEADQHOB DC    F'0',F'0'                                                        
DEADQ    CMQODA  LIST=YES  OBJECT DESCRIPTOR                                    
*                                                                               
MSGDESC  CMQMDA  LIST=YES          MESSAGE DESCRIPTOR                           
         ORG   MSGDESC_FORMAT                                                   
         DC    CL8'MQSTR'          EVERYTHING IS A STRING                       
         ORG                                                                    
*                                                                               
GETOPTS  CMQGMOA LIST=YES          GET MESSAGE OPTIONS                          
PUTOPTS  CMQPMOA DSECT=NO,LIST=YES PUT MESSAGE OPTIONS                          
*&&US                                                                           
QMASTERP DC    CL48'DDS.BROKER.LOCALQ'                                          
QMASTERT DC    CL48'DDS.BROKER.TEST.LOCALQ'                                     
QMASTERQ DC    CL48'DDS.BROKER.FQA.LOCALQ'                                      
QMASTERC DC    CL48'DDS.BROKER.CSC.LOCALQ'                                      
QLOGGERP DC    CL48'DDS.BROKER.LOG.LOCALQ'                                      
QLOGGERT DC    CL48'DDS.BROKER.LOG.TEST.LOCALQ'                                 
QLOGGERQ DC    CL48'DDS.BROKER.LOG.FQA.LOCALQ'                                  
QLOGGERC DC    CL48'DDS.BROKER.LOG.CSC.LOCALQ'                                  
QDEADMGP DC    CL48'DDS.BROKER.DEADMSG.LOCALQ'                                  
QDEADMGT DC    CL48'DDS.BROKER.DEADMSG.TEST.LOCALQ'                             
QDEADMGQ DC    CL48'DDS.BROKER.DEADMSG.FQA.LOCALQ'                              
QDEADMGC DC    CL48'DDS.BROKER.DEADMSG.CSC.LOCALQ'                              
*&&                                                                             
*&&UK                                                                           
QMASTERP DC    CL48'LDDS.BROKER.LOCALQ'                                         
QMASTERT DC    CL48'LDDS.BROKER.TEST.LOCALQ'                                    
QMASTERQ DC    CL48'LDDS.BROKER.FQA.LOCALQ'                                     
QMASTERC DC    CL48'LDDS.BROKER.CSC.LOCALQ'                                     
QLOGGERP DC    CL48'LDDS.BROKER.LOG.LOCALQ'                                     
QLOGGERT DC    CL48'LDDS.BROKER.LOG.TEST.LOCALQ'                                
QLOGGERQ DC    CL48'LDDS.BROKER.LOG.FQA.LOCALQ'                                 
QLOGGERC DC    CL48'LDDS.BROKER.LOG.CSC.LOCALQ'                                 
QDEADMGP DC    CL48'LDDS.BROKER.DEADMSG.LOCALQ'                                 
QDEADMGT DC    CL48'LDDS.BROKER.DEADMSG.TEST.LOCALQ'                            
QDEADMGQ DC    CL48'LDDS.BROKER.DEADMSG.FQA.LOCALQ'                             
QDEADMGC DC    CL48'LDDS.BROKER.DEADMSG.CSC.LOCALQ'                             
*&&                                                                             
QINP_OV  DC    CL48' '             OVERRIDE QNAME FOR INPUTQ                    
QLOG_OV  DC    CL48' '             OVERRIDE QNAME FOR LOGQ                      
QDEAD_OV DC    CL48' '             OVERRIDE QNAME FOR DEADQ                     
*                                                                               
***********************************************************************         
* PARAMETER LISTS TO FACILITATE MQ CALLS                              *         
*                                                                     *         
*  CL16 EBCDIC ROUTINE NAME                                           *         
*  F    A(ROUTINE)                                                    *         
*  PARAMETERS (STANDARD IBM FORMAT)                                   *         
***********************************************************************         
         DS    0D                                                               
MQCT     DC    CL16'MQ QMGR connect'                                            
AMQCT    DC    A(0)                                                             
         DC    A(QMGR)                                                          
         DC    A(HCONN)                                                         
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQOPENQ  DC    CL16'MQ Open queue'                                              
AMQOPENQ DC    A(0)                                                             
         DC    A(HCONN)                                                         
MQOPNQNM DC    A(0)                CMQODA                                       
         DC    A(OPN_OPTS)                                                      
MQOPNHOB DC    A(0)                HANDLE TO OBJECT (RETURNED)                  
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQGET    DC    CL16'MQ Get'                                                     
AMQGET   DC    A(0)                                                             
         DC    A(HCONN)                                                         
         DC    A(MAINQHOB)         QUEUE HOB                                    
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
MQPUTQN  DC    A(0)                QUEUE NAME                                   
         DC    A(MSGDESC)                                                       
         DC    A(PUTOPTS)                                                       
         DC    A(DATALENP)                                                      
MQPUTBUF DC    AL4(0)                                                           
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQPUT2   DC    CL16'MQ Put 2'                                                   
AMQPUT2  DC    A(0)                                                             
         DC    A(HCONN)                                                         
MQPUTQN2 DC    A(0)                QUEUE NAME                                   
         DC    A(MSGDESC)                                                       
         DC    A(PUTOPTS)                                                       
         DC    A(DATALEN2)                                                      
MQPUTBF2 DC    AL4(0)                                                           
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
         EJECT                                                                  
***********************************************************************         
* TABLE OF KNOWN BROKER QUEUES                                        *         
* ROUTING PERFORMED BY RESOLUTION OF THE 16 BYTE HEADER ON MSG FRONT  *         
* THIS FIRST TABLE IS FOR BINSRCH AND MUST BE ORDERED                 *         
***********************************************************************         
         MACRO                                                                  
&TAG     MQQUE &COUNT=1,&MB=2                                                   
         GBLA  &Q                                                               
         GBLA  &QNUM(5)                                                         
         GBLC  &QTG(5)                                                          
         LCLA  &N                                                               
&Q        SETA  &Q+1                                                            
&QNUM(&Q) SETA  &COUNT                                                          
&QTG(&Q)  SETC  '&TAG'                                                          
&TAG.TAB DS    0L                                                               
         DC    XL(SCHQTABL)'00'                                                 
.NEXT    ANOP                                                                   
         AIF   (&N EQ &COUNT).DONE                                              
&N       SETA  &N+1                                                             
         DC    16X'FF',CL48' ',CL48' '                                          
         DC    F'0',A(&TAG.Q&N),F'0',A(&TAG.L&N)                                
         DC    CL(L'SCHQSEND)' '                                                
         DC    X'00',3X'00',A(&MB*MB),F'0',A(0)                                 
                                                                                
         AGO   .NEXT                                                            
.DONE    ANOP                                                                   
&TAG.NQ  EQU   ((*-&TAG.TAB)/SCHQTABL)                                          
&TAG.TXX DC    (SCHQTABL)X'FF'                                                  
&TAG.TABN EQU   &N+2                                                            
         DC    CL16'END_OF_TABLE'                                               
         MEXIT                                                                  
         MEND                                                                   
         MACRO                                                                  
.**********************************************************************         
.* Create MQ macro CMQODA for each queue                                        
.**********************************************************************         
         MQMACL                                                                 
         GBLA  &Q                                                               
         GBLA  &QNUM(5)                                                         
         GBLC  &QTG(5)                                                          
         LCLA  &N                                                               
         LCLA  &L                                                               
         LCLC  &TAG                                                             
.AGAIN   ANOP                                                                   
&L       SETA  &L+1                                                             
         AIF  (&L GT &Q).EXIT                                                   
&TAG     SETC '&QTG(&L)'                                                        
&C       SETA &QNUM(&L)                                                         
&N       SETA 0                                                                 
.NEXT    ANOP                                                                   
         AIF   (&N EQ &C).DONE                                                  
&N       SETA  &N+1                                                             
&TAG.X&N DS    0C                  CMQODA queue and log                         
&TAG.Q&N CMQODA LIST=NO                                                         
&TAG.L&N CMQODA LIST=NO                                                         
         AGO   .NEXT                                                            
.DONE    AGO   .AGAIN                                                           
.EXIT    MEXIT                                                                  
         MEND                                                                   
*                                                                               
         PRINT GEN                                                              
SCHQ     MQQUE COUNT=99,MB=2                                                    
QPOOL    MQQUE COUNT=40,MB=2                                                    
         PRINT NOGEN                                                            
                                                                                
*****************************************************************               
* If you change the DSECT make sure you change the MACRO MQQUE                  
*****************************************************************               
SCHQTABD DSECT                                                                  
SCHQID   DS    CL16                QUEUE HEADER (UNIQUE IDENTIFIER)             
SCHQNPRD DS    CL48                QUEUE NAME                                   
SCHQLPRD DS    CL48                LOG QUEUE NAME                               
SCHQHOBJ DS    F                   HOBJ FOR THIS QUEUE OBJECT                   
SCHQCMQ  DS    F                   OBJECT DESCRIPTOR QUEUE                      
SCHQHLOG DS    F                   HOBJ FOR LOG QUEUE OBJECT                    
SCHQCMQL DS    F                   OBJECT DESCRIPTOR LOGGING QUEUE              
*                                                                               
SCHQSEND DS    CL24                EMAIL error notification                     
*                                                                               
SCHQFLG1 DS    X                   FLAG1                                        
SCHQ1ALG EQU   X'80'               ALWAYS LOG (EVEN IF LOGGING=N)               
SCHQ1NLG EQU   X'40'               NEVER  LOG (EVEN IF LOGGING=Y)               
SCHQFLG2 DS    X                   FLAG2                                        
SCHQ2LCT EQU   X'80'               LAST CHAR IN LABEL AS CTRL CHAR              
SCHQ2DMH EQU   X'40'               DROP MESSAGE HEADER SENDING OUT              
         DS    2X                  SPARES                                       
SCHQMAXL DS    F                   MAX MESSAGE SIZE                             
SCHQHOOK DS    F                   ROUTINE TO PREPROCESS MESSAGE                
SCHQXLNK DS    A                   Link to multiple queue puts                  
SCHQTABL EQU   *-SCHQTABD                                                       
*                                                                               
EDISCHED CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* NON-ADDRESSIBLE STORAGE AREAS                                       *         
***********************************************************************         
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
       ++INCLUDE DDMQREASON                                                     
         EJECT                                                                  
***********************************************************************         
* VALID QHOOK CARD TABLE                                              *         
***********************************************************************         
         DS    0D                                                               
HOOKTAB  DC    CL8'RUNIT   ',AL4(HKRUNIT)                                       
         DC    X'FF'                                                            
*                                                                               
HOOKTABD DSECT ,                   QHOOK CARD PARAMETERS                        
HOOKTXT  DS    CL8                 TEXT                                         
HOOKADR  DS    AL4                 A(HOOK ROUTINE)                              
HOOKTABL EQU   *-HOOKTABD                                                       
EDISCHED CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALID INPUT CARD TABLE                                              *         
***********************************************************************         
         DS    0D                                                               
CARDTAB  DC    CL10'DSPACE    ',AL1(5,5,0,0),AL4(VCDSPACE)                      
         DC    CL10'DDSIO     ',AL1(4,4,0,0),AL4(VCDDSIO)                       
         DC    CL10'TRACE     ',AL1(4,4,0,0),AL4(VCTRACE)                       
         DC    CL10'QMGR      ',AL1(3,3,0,0),AL4(VCQMGR)                        
         DC    CL10'LOGGING   ',AL1(6,6,0,0),AL4(VCLOG)                         
         DC    CL10'RUNACK    ',AL1(5,5,0,0),AL4(VCRUNACK)                      
         DC    CL10'ETYPE     ',AL1(4,4,0,0),AL4(VCETYPE)                       
         DC    CL10'EMAIL     ',AL1(4,4,0,0),AL4(VCEMAIL)                       
         DC    X'FF'                                                            
*                                                                               
CARDTABD DSECT ,                   INPUT CARD PARAMETERS                        
CARDTXT  DS    CL10                TEXT                                         
CMIN     DS    X                   MIN LEN FOR EXECUTE (-1)                     
CMAX     DS    X                   MAX LEN FOR EXECUTE (-1)                     
         DS    XL2                 N/D                                          
CARDVAL  DS    AL4                 A(VALIDATION ROUTINE)                        
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
EDISCHED CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* SCHEDULE MQ DESCRIPTORS - REQUIRED TO OUTPUT TO THE QUEUES          *         
*   MACRO generates the number of entries base on MQQUE macro                   
***********************************************************************         
         MQMACL                                                                 
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
EMSG06   DC    CL45'Bad message header - returning to sender    '               
EMSG07   DC    CL45'Bad "return to" address in header message   '               
EMSG08   DC    CL45'Bad message - dump to dead message queue    '               
EMSG09   DC    CL45'No output queue defined for this msg label  '               
EMSG10   DC    CL45'No Runner can get this request              '               
EMSG11   DC    CL45'Message too big for target application      '               
EMSG12   DC    CL45'Return message to the sender                '               
EMSG13   DC    CL45'Message too big, cannot return to sender    '               
EMSG14   DC    CL45'Too many QLABEL= parms, need to incr SCHQTAB'               
EMSG15   DC    CL45'Undefined Hook routine name                 '               
EMSG16   DC    CL45'Max Message Size must be number in MB       '               
EMSG17   DC    CL45'Exceed the maximum message size             '               
EMSG18   DC    CL45'Invalid Logging Option                      '               
EMSG19   DC    CL45'Invalid Option                              '               
EMSG20   DC    CL45'Email too long, only 16 character allowed   '               
         EJECT                                                                  
***********************************************************************         
* CONSOLE MESSAGE TABLE - NEED NOT BE ADDRESSIBLE                     *         
* FIRST BYTE:- X=NO REPLY, n=MAX REPLY SIZE                           *         
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
SMSG24   DC    CL50'XOpened    MQ Dead Msg Queue                   '            
SMSG25   DC    CL50'XClosed    MQ Dead Msg Queue                   '            
         EJECT                                                                  
***********************************************************************         
* TRANSLATE TABLE FOR LOGGING                                         *         
***********************************************************************         
TRNTBL   DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  X'00-0F'                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  X'10-1F'                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  X'20-2F'                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  X'30-3F'                    
         DC    X'404B42434445464748494A4B4C4D4E4F'  X'40-4F'                    
         DC    X'505152535455565758495A5B5C5D5E5F'  X'50-5F'                    
         DC    X'606162636465666768696A6B6C6D6E6F'  X'60-6F'                    
         DC    X'707172737475767778797A7B7C7D7E7F'  X'70-7F'                    
         DC    X'808182838485868788898A8B8C8D8E8F'  X'80-8F'                    
         DC    X'909192939495969798999A9B9C9D9E9F'  X'90-9F'                    
         DC    X'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF'  X'A0-AF'                    
         DC    X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'  X'B0-BF'                    
         DC    X'C0C1C2C3C4C5C6C7C8C94BCBCCCDCECF'  X'C0-CF'                    
         DC    X'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'  X'D0-DF'                    
         DC    X'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'  X'E0-EF'                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'  X'F0-FF'                    
         EJECT                                                                  
***********************************************************************         
* W/S AREA - UNUSUAL IN THAT IT'S IN THE PROGRAMS AREA ITSELF         *         
***********************************************************************         
         DS    0D                                                               
WORKD    DC    CL8'*WORKD**'                                                    
DUB      DS    D                                                                
PLIST    DS    8F                                                               
DMCB     DS    8F                                                               
FULL     DS    F                                                                
AERRQ    DS    A                                                                
ERRORCD  DS    F                                                                
HALF     DS    H                                                                
EMAILCNT DS    F                                                                
*                                                                               
CONOK    DS    X                                                                
OPERSTOP DS    X                                                                
BADOPCMD DS    C                   (YES/NO)                                     
PCC      DS    C                                                                
P        DS    CL166                                                            
PLINE    DS    CL150               OUTPUT PRINT LINE                            
REPLY    DS    CL8                                                              
CARDIO   DS    CL80                                                             
SVLABEL  DS    CL(L'SCHQID)                                                     
THLABEL  DS    CL(L'SCHQID)                                                     
FERN     DS    X                                                                
SVERR    DS    X                                                                
ETYPE    DS    X                                                                
DSPACE   DS    X                                                                
EMAIL    DS    X                                                                
TRACE    DS    X                                                                
NOTIME   DS    X                                                                
MUSTPRNT DS    X                                                                
GOTERR   DS    X                                                                
PARMCNT  DS    X                   NO OF I/P CARDS                              
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
PARMCNTQ EQU   15                  MAX NUMBER OF I/P CARDS                      
WORK     DS    CL64                                                             
FPLABEL  DS    CL3                                                              
USERID   DS    CL10                                                             
SYSALPH  DS    CL3                                                              
POOL#    DS    X                   Number in entries used in QPOOL              
ERRQ     DS    CL16                ERROR QUEUE FROM ERRORTO=                    
HK_MODE  DS    C                   HOOK MODE                                    
HKM_PRC  EQU   1                        PROCESS                                 
HKM_CMT  EQU   2                        COMMIT                                  
*                                                                               
*                                                                               
* variables for runner messages                                                 
CLASS    DS    CL(L'TSTYPE)        RUNNER work class                            
AGENCY   DS    CL(L'LM_AGY)        Agency alpha id                              
WKEY     DS    0XL(L'TQWKEY)       ** Worker key **                             
WKEYZERO DS    XL(L'TQMQZERO)      MQ message indicator                         
WKEYASID DS    XL(L'TQMQASID)      ASID of scheduler                            
WKEYDATE DS    XL(L'TQMQDATE)      Date scheduled (compressed)                  
WKEYTIME DS    XL(L'TQMQTIME)      Time scheduled (TU's)                        
         ORG   WKEY+L'WKEY                                                      
*                                                                               
*                                                                               
WORKC    DS    CL32                                                             
WRK      DS    XL32                                                             
WRKL     DS    XL32                                                             
KEY      DS    CL25                                                             
IO       DS    2048X                                                            
SCANTAB  DS    (PARMCNTQ)CL(SCBLKLQ)                                            
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* RD CHAIN CSECT                                                      *         
***********************************************************************         
WORKAREA CSECT                                                                  
         DC    200000X'00'                                                      
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* FACIDTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
* Included books for runner messages                                            
         PRINT OFF                                                              
       ++INCLUDE DDRUNNERD                                                      
       ++INCLUDE DDLINKD                                                        
       ++INCLUDE FATABSRUN                                                      
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
**PAN#1  DC    CL21'014EDISCHED  11/15/19'                                      
         END                                                                    
