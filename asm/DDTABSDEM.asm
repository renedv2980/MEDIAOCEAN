*          DATA SET DDTABSDEM  AT LEVEL 008 AS OF 02/13/20                      
*PHASE TABSDEMA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE LOADER                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE CBJOB                                                                  
                                                                                
         TITLE 'Monitor Special MQ messages, update DEMO tables'                
                                                                                
***********************************************************************         
* This program is responsible for the following DEMO data space tables:         
*      - US-RNTRK - COMSCORE LOCAL DEMO TABLE                                   
*      - US-CSLON - COMSCORE LOCAL DEMOS BY NUMBER                              
*      - US-CSNAT - COMSCORE NATIONAL DEMO TABLE                                
*      - US-RNTNE - COMSCORE NETWORK CODE DEFINITIONS                           
*      - COMJOBS* - COMSCORE BATCH JOBS TABLE                                   
***********************************************************************         
TABSDEM  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*TABSDEM,ACHAIN                                                
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         LARL  RC,COMMON           COMMON STORAGE AREA                          
         USING COMMON,RC                                                        
         LARL  R9,LITCON           LITERALS AND CONSTANTS                       
         USING LITCON,R9                                                        
         LARL  R8,MQDEFS           MQ DEFINED STORAGE                           
         USING MQDEFS,R8                                                        
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         BRAS  RE,VALCARDS ------- VALIDATE JCL INPUT CARDS                     
         JNE   STOP                INVALID CARD                                 
*                                                                               
         BRAS  RE,INITIAL -------- INITIALIZE/CONNECT/OPEN                      
*                                                                               
MAIN010  BRAS  RE,BROBACK -------- BROWSE BACKUP QUEUE                          
         BE    MAIN030             UPDATE TABLES IF NECESSARY                   
*                                                                               
MAIN020  BRAS  RE,GETWAIT -------- GET/WAIT FOR NEXT MESSAGE                    
*                                                                               
         CLI   OPSTOP,YES                                                       
         BE    STOP                OPERATOR STOPPED JOB                         
         CLI   OPBUILD,YES                                                      
         BE    MAIN010             OPERATOR REQUEST                             
*                                                                               
MAIN030  BRAS  RE,PROCMQ --------- PROCESS MQ MESSAGE                           
*                                                                               
         BRAS  RE,FINIMQ --------- FINISH WITH MQ MESSAGE                       
         BNE   MAIN010             TABLES STILL TO BUILD                        
*                                                                               
         B     MAIN020 ----------- NEXT MESSAGE                                 
*                                                                               
STOP     BRAS  RE,JOBCAN --------- DISCONNECT/CLOSE                             
         XBASE                                                                  
*                                                                               
ACHAIN   DC    A(R13CHAIN)                                                      
                                                                                
***********************************************************************         
* INITIAL ROUTINE                                                               
***********************************************************************         
INITIAL  NTR1                                                                   
*                                                                               
         MVI   INDI,0              CLEAR PROGRAM INDICATOR                      
         MVI   DMBITS,0            CLEAR DATA MANAGER BITS                      
*                                                                               
         GOTOR PRNT,INITIALIZE                                                  
         MVC   TITLE(14),=C'MQ TABS UPDATE'                                     
*                                                                               
         BLDL  0,ENTRYPTS          LIST OF APPC/MVS ENTRY POINTS                
         LTR   RF,RF                                                            
         JNZ   *+2                 BAD RETURN FROM BLDL MACRO                   
*                                                                               
         LOAD  DE=CSQBCONN         CONNECT                                      
         ST    R0,CSQBCONN_STRUCTURE                                            
*                                                                               
         LOAD  DE=CSQBCOMM         COMMIT                                       
         ST    R0,CSQBCOMM_STRUCTURE                                            
*                                                                               
         LOAD  DE=CSQBDISC         DISCONNECT                                   
         ST    R0,CSQBDISC_STRUCTURE                                            
*                                                                               
         LOAD  DE=CSQBOPEN         OPEN                                         
         ST    R0,CSQBOPEN_STRUCTURE                                            
         ST    R0,CSQBOPEN_STRUCTURE_KQ                                         
         ST    R0,CSQBOPEN_STRUCTURE_DQ                                         
*                                                                               
         LOAD  DE=CSQBGET          GET                                          
         ST    R0,CSQBGET_STRUCTURE                                             
         ST    R0,CSQBGET_STRUCTURE_KQ                                          
*                                                                               
         LOAD  DE=CSQBPUT          PUT                                          
         ST    R0,CSQBPUT_STRUCTURE                                             
         ST    R0,CSQBPUT_STRUCTURE_KQ                                          
         ST    R0,CSQBPUT_STRUCTURE_DQ                                          
*                                                                               
         LOAD  DE=CSQBCLOS         CLOSE                                        
         ST    R0,CSQBCLOS_STRUCTURE                                            
         ST    R0,CSQBCLOS_STRUCTURE_KQ                                         
         ST    R0,CSQBCLOS_STRUCTURE_DQ                                         
*                                                                               
         BRAS  RE,SETOPS           SET OPERATOR STOP ECB                        
*                                                                               
         L     R3,MSGMAX           GET MQ BUFFER STORAGE                        
         LR    R4,R3                                                            
         AHI   R3,8                LENGTH WITH EYECATCHER *MQTMSG*              
*                                                                               
         SAM31 ,                                                                
         GETMAIN RU,LV=(3),LOC=(ANY,ANY),BNDRY=PAGE                             
         LTR   RF,RF                                                            
         JNZ   *+2                 PROBLEM GETTING STORAGE                      
*                                                                               
         MVC   0(8,R1),=C'*MQTMSG*'                                             
         LA    R1,8(,R1)                                                        
         ST    R1,AMSGIN           A(MQ MESSAGE)                                
         AR    R4,R1               ADD MAX LENGTH OF MESSAGE                    
         ST    R4,AMSGINX          A(END OF MESSAGE)                            
*                                                                               
         LA    R1,DMHLNQ(,R1)      BUMP PAST MESSAGE HEADER                     
         ST    R1,ADATAOUT         A(MESSAGE DATA)                              
         SAM24 ,                                                                
*                                                                               
         LA    R2,CSQBCONN_STRUCTURE  CONNECT TO QUEUE MANAGER                  
         BRAS  RE,CALLMQ                                                        
         JNE   *+2                                                              
*                                                                               
         XC    OPENOPT,OPENOPT     INIT MQ OPTIONS                              
         XC    OPENOPT2,OPENOPT2                                                
         MVC   OBJDESC_OBJECTTYPE,=A(MQOT_Q)                                    
*                                                                               
         GOTO1 VLOADER,DMCB,DEMADDR,0,0  GET DEMADDR                            
         OC    DMCB+4(4),DMCB+4                                                 
         JZ    *+2                                                              
         MVC   ADEMADDR,4(R1)                                                   
*                                                                               
         OI    INDI,INBUILD        BUILD TABLE AT START OF JOB                  
*                                                                               
         L     R1,=A(MSGTAB)                                                    
         ST    R1,ASMSGTAB         START OF MESSAGE TABLE                       
         ST    R1,ACMSGTAB                                                      
         MVC   SVMQHDR,SPACES                                                   
*                                                                               
         XC    DUB,DUB             GET JOB TABLE HEADER FROM TABS               
         MVC   DUB(4),=AL4(DTJOB)                                               
         OI    DUB,X'20'                                                        
         GOTO1 VLOCKSPC,DUB                                                     
         ICM   RF,15,4(R1)                                                      
         JZ    *+2                                                              
         MVC   DSPJTAB,0(RF)       SAVE JOBTAB HEADER FROM DSPACE               
*                                                                               
JOB      USING DMSPACED,DSPJTAB                                                 
         NI    JOB.DSPTFRST,X'3F'                                               
         BRAS  RE,ARSOFF                                                        
         DROP  JOB                                                              
*                                                                               
         XC    DUB,DUB             GET CLASS TABLE HEADER FROM TABS             
         MVC   DUB(4),=AL4(DTDCLASS)                                            
         OI    DUB,X'20'                                                        
         GOTO1 VLOCKSPC,DUB                                                     
         ICM   RF,15,4(R1)                                                      
         JZ    *+2                                                              
         MVC   DSPCTAB,0(RF)       SAVE JOBTAB HEADER FROM DSPACE               
*                                                                               
CLS      USING DMSPACED,DSPCTAB                                                 
         NI    CLS.DSPTFRST,X'3F'                                               
         BRAS  RE,ARSOFF                                                        
         DROP  CLS                                                              
*                                                                               
INIX     J     EXIT                                                             
                                                                                
***********************************************************************         
* GET / WAIT FOR THE NEXT MQ MESSAGE                                            
***********************************************************************         
GETWAIT  NTR1                                                                   
*                                                                               
         MVC   SVMQHDR,SPACES      CLEAR SAVED MQ MESSAGE HEADER                
*                                                                               
         SAM31                                                                  
         L     R2,AMSGIN           SPACE FILL START OF INPUT BUFFER             
         MVC   0(L'SPACES,R2),SPACES                                            
         SAM24                                                                  
*                                                                               
         CLC   OPENOPT,=A(MQOO_INPUT_AS_Q_DEF)  IS QUEUE OPEN                   
         BE    GW010                            YES: CONTINUE                   
         MVC   OPENOPT,=A(MQOO_INPUT_AS_Q_DEF)  NO: WELL, OPEN IT               
         MVC   OBJDESC_OBJECTNAME,QQ                                            
         LA    R2,CSQBOPEN_STRUCTURE                                            
         BRAS  RE,CALLMQ                                                        
         JNE   *+2                                                              
*                                                                               
GW010    MVC   MSGDESC_CORRELID,MQCI_NONE                                       
         MVC   MSGDESC_MSGID,MQMI_NONE                                          
         MVC   AGETMSG,AMSGIN                                                   
*                                                                               
         MVC   GMSGOPTS_OPTIONS,=A(MQGMO_SET_SIGNAL)                            
         MVC   GMSGOPTS_SIGNAL1,=A(INPQECB)                                     
         MVC   GMSGOPTS_WAITINTERVAL,=A(MQWI_UNLIMITED)                         
         MVC   GMSGOPTS_MATCHOPTIONS,=A(MQMO_NONE)                              
*                                                                               
         MVC   OBJDESC_OBJECTNAME,QQ  INPUT QUEUE NAME                          
         LA    R2,CSQBGET_STRUCTURE                                             
*                                                                               
         TM    INPQECB,X'80'       ARE WE STILL WAITING ON INPUT?               
         BO    GW020               YES: GO DIRECTLY TO WAIT                     
         XC    INPQECB,INPQECB                                                  
*                                                                               
         BRAS  RE,CALLMQ           TRY TO GET A MESSAGE                         
         BE    GW030               GOT A MESSAGE                                
         CLC   COMPCODE,=A(MQCC_WARNING)                                        
         JNE   *+2                                                              
         CLC   REASON,=A(MQRC_SIGNAL_REQUEST_ACCEPTED)                          
         JNE   *+2                                                              
*                                                                               
         TM    INDI,INBUILD        WAS A BUILD NEEDED?                          
         BZ    GW020               NO: THEN WAIT                                
         GOTOR NOTIFY,NOBUMPTY     YES: CAN'T BUILD, TROUBLE                    
*                                                                               
GW020    GOTOR PRNT,ABOUTTOWAIT                                                 
         WAIT  1,ECBLIST=ECBLST    WAIT FOR COMPLETION/OPERATOR                 
         GOTOR PRNT,WAITCOMPLETE                                                
*                                                                               
         BRAS  RE,CHKOPS           OPERATOR INTERRUPT                           
         CLI   OPSTOP,YES          STOP                                         
         BE    GWSTOPX                                                          
         CLI   OPBUILD,YES         BUILD                                        
         BE    GWOKX                                                            
*                                                                               
         TM    INPQECB,X'40'       HOW DID WE GET OUT OF WAIT?                  
         JZ    *+2                                                              
*                                                                               
         L     RF,INPQECB          BITS 2-31 OF ECB ARE COMP CODE               
         SLL   RF,2                                                             
         SRL   RF,2                                                             
         C     RF,=A(MQEC_MSG_ARRIVED)  ANY OTHER RESULT NO GOOD                
         JNE   *+2                                                              
         XC    INPQECB,INPQECB     CLEAR ECB BEFORE MQGET CALL                  
         B     GW010               A MESSAGE IS WAITING: GET IT                 
*                                                                               
GW030    OI    INDI,INFORCE        NEW MESSAGE FORCES A BUILD                   
*                                                                               
GWOKX    J     EXITEQ                                                           
GWSTOPX  J     EXITNE              SET CC NOT EQUAL-JOB STOPPED                 
                                                                                
***********************************************************************         
* PROCESS THE MQ MESSAGE                                                        
***********************************************************************         
         USING MSGTABD,R5                                                       
PROCMQ   NTR1                                                                   
*                                                                               
         NI    INDI,X'FF'-INBADMS-INNOBAD GOOD MESSAGE FOR NOW                  
         MVI   NOTYMSG#,NOBUMESS   BUT PREPARE FOR BAD MESSAGE                  
*                                                                               
         SAM31                                                                  
         L     R2,AMSGIN           VALIDATE MESSAGE HEADER                      
         USING DEMQHDRD,R2                                                      
         MVC   SVMQHDR,DEMQHDRD    SAVE MESSAGE HEADER                          
*                                                                               
         MVC   P,SPACES                                                         
         MVC   PMSG(L'DMHDESC),DMHDESC DESCRIPTOR                               
         MVC   P+50(50),L'DMHDESC(R2)                                           
         GOTOR PRNT,PROCESSMESS                                                 
*                                                                               
         L     R5,ASMSGTAB         VALID MQ MESSAGE TABLE                       
PM010    CLI   0(R5),EOT           END OF TABLE                                 
         BE    PMNOX               BAD MESSAGE                                  
         CLC   DMHDESC,MSGTTAG     DESCRIPTOR CORRECT                           
         BE    PM012               NO                                           
         AHI   R5,MSGTLNQ                                                       
         B     PM010                                                            
*                                                                               
PM012    ST    R5,ACMSGTAB         CURRENT TABLE ENTRY                          
         ICM   RF,15,MSGTROUT      PICK UP A(PROCESSING ROUTINE)                
         JZ    *+2                 TABLE SETUP IS INCORRECT                     
         BASR  RE,RF               AND CALL IT                                  
         BNE   PMNOX                                                            
*                                                                               
PMOKX    J     EXITEQ                                                           
*                                                                               
PMNOX    LLC   R1,NOTYMSG#         NOTIFICATION MESSAGE NUMBER                  
         GOTOR NOTIFY              ALERT BAD MESSAGE                            
         TM    INDI,INNOBAD        DO WE WANT TO SKIP BAD MSG QUEUE?            
         BO    *+8                 YES: SAVING NOT DESIRED OR NEEDED            
         BRAS  RE,PUTBADM          PUT TO BAD MESSAGE QUEUE                     
         OI    INDI,INBADMS        BAD MESSAGE FOUND                            
         J     EXITNE                                                           
         DROP  R5                                                               
                                                                                
***********************************************************************         
* COMMIT MESSAGE / PUT BACKUP MESSAGE / TABLE BUILD REQUIREMENTS                
***********************************************************************         
FINIMQ   NTR1                                                                   
*                                                                               
         BRAS  RE,COMINPUT         COMMIT MQ MESSAGE                            
                                                                                
         L     R5,ACMSGTAB         CURRENT TABLE ENTRY                          
         USING MSGTABD,R5                                                       
*                                                                               
         TM    MSGTOPT,MSGTOPQ     THIS MESSAGE TYPE REQUIRED PQ?               
         BZ    FM010               NO: NO NEED TO RELEASE                       
*                                                                               
         GOTO1 VDATAMGR,DMCB,ISGENQ,C'TASK'  RELEASE TASK LOCKS                 
*                                                                               
FM010    TM    MSGTOPT,MSGTOBU     THIS MESSAGE TYPE REQUIRES BACKUP?           
         BZ    FMOKX               NO: DON'T BACK UP                            
*                                                                               
         TM    INDI,INBADMS        BAD MESSAGE FOUND                            
         BO    FM100               YES: DON'T BACK IT UP                        
*                                                                               
         MVC   SVMQLEN,DATALEN     SAVE DATA LENGTH                             
*                                                                               
         SAM31                                                                  
         L     R2,AMSGIN           CURRENT MESSAGE                              
         USING DEMQHDRD,R2                                                      
         MVC   SVHDTAG,DMHDTAG     SAVE DMHDTAG                                 
         MVC   DMHDTAG,BOOKMARK    MARK MSG AS CURRENT BACKUP                   
         SAM24                                                                  
         OI    INDI,INNOLOG        NO LOG MESSAGES                              
         BRAS  RE,PUTBACK          PUT BACKUP MESSAGE                           
*                                                                               
FM020    BRAS  RE,GETBACK          READ ALL MESSAGES FROM BACKUP                
         JNE   *+2                 NO BACKUP MESSAGES, CAN'T BE                 
*                                                                               
         SAM31                                                                  
         L     R2,AMSGIN           VALIDATE MESSAGE HEADER                      
         CLC   DMHDESC,SVMQHDR+(DMHDESC-DEMQHDRD)                               
         BNE   FM050               NOT THE TYPE WE JUST PROCESSED               
         CLC   DMHDTAG,BOOKMARK    IS THIS THE CURRENT BACKUP?                  
         BNE   FM040               NO: DITCH IT                                 
         MVC   DMHDTAG,SVHDTAG     RESTORE TAG                                  
         SAM24                                                                  
         B     FM060                                                            
FM040    SAM24                                                                  
         B     FM020                                                            
FM050    SAM24                                                                  
         BRAS  RE,PUTBACK          PUT BACK LAST BACKUP MESSAGE                 
         B     FM020               BAD MESSAGE IN BACKUP QUEUE                  
*                                                                               
FM060    MVC   DATALEN,SVMQLEN     RESTORE SAVED LENGTH                         
         NI    INDI,X'FF'-INNOLOG  TURN LOG MESSAGES BACK ON                    
*                                                                               
         BRAS  RE,PUTBACK          PUT BACK LAST BACKUP MESSAGE                 
         BRAS  RE,COMBACK          COMMIT BACKUP MESSAGE                        
*                                                                               
FM100    L     R5,ASMSGTAB         TABLE OF VALID MESSAGES                      
FM102    CLI   0(R5),EOT           END OF TABLE                                 
         BNE   FM104               NO                                           
         NI    INDI,X'FF'-INBUILD-INFORCE FINISHED WITH TABLE BUILDS            
         B     FMOKX                                                            
*                                                                               
FM104    TM    MSGTOPT,MSGTODS     THIS MESSAGE TYPE REQUIRES BUILD?            
         BZ    FM106               NO: NO NEED TO CHECK                         
         TM    MSGTIND,MSGTIDS     HAS THIS TABLE BEEN BUILT?                   
         BZ    FMNOX               NO: NEED TO PROCESS                          
*                                                                               
FM106    AHI   R5,MSGTLNQ          YES: NEXT TABLE ENTRY                        
         B     FM102                                                            
*                                                                               
FMOKX    J     EXITEQ                                                           
FMNOX    J     EXITNE                                                           
         DROP  R2,R5                                                            
                                                                                
***********************************************************************         
* JOB CANCELED, CLOSE QUEUE, DISCONNECT                                         
***********************************************************************         
JOBCAN   NTR1                                                                   
*                                                                               
         BRAS  RE,CLOINPUT         CLOSE INPUT QUEUE                            
         BRAS  RE,CLOBACK          CLOSE BACKUP QUEUE                           
         BRAS  RE,CLOBADM          CLOSE BAD MESSAGE QUEUE                      
*                                                                               
         LA    R2,CSQBDISC_STRUCTURE  DISCONNECT QUEUE MANAGER                  
         BRAS  RE,CALLMQ                                                        
*                                                                               
         GOTOR PRNT,EXITING                                                     
         J     EXIT                                                             
                                                                                
***********************************************************************         
* MQ ROUTINES                                                                   
***********************************************************************         
* COMMIT INPUT QUEUE                                                            
*----------------------------------------------------------------------         
COMINPUT NTR1                                                                   
*                                                                               
         OC    OPENOPT,OPENOPT     IS THE QUEUE OPEN?                           
         JZ    EXITEQ              NO: JUST EXIT                                
*                                                                               
         MVC   OBJDESC_OBJECTNAME,QQ                                            
         LA    R2,CSQBCOMM_STRUCTURE                                            
         BRAS  RE,CALLMQ                                                        
         JE    EXITEQ                                                           
         DC    H'0'                                                             
                                                                                
*----------------------------------------------------------------------         
* CLOSE INPUT QUEUE                                                             
*----------------------------------------------------------------------         
CLOINPUT NTR1                                                                   
*                                                                               
         OC    OPENOPT,OPENOPT     IS THE QUEUE OPEN?                           
         JZ    EXITEQ              NO: JUST EXIT                                
*                                                                               
         MVC   CLOSOPT,=A(MQCO_NONE)  SET CLOSE OPTIONS                         
         MVC   OBJDESC_OBJECTNAME,QQ                                            
         LA    R2,CSQBCLOS_STRUCTURE                                            
         BRAS  RE,CALLMQ                                                        
         JE    EXITEQ                                                           
         DC    H'0'                                                             
                                                                                
*----------------------------------------------------------------------         
* PUT TO BAD MESSAGE QUEUE                                                      
*----------------------------------------------------------------------         
PUTBADM  NTR1                                                                   
*                                                                               
         OC    OPENOPT3,OPENOPT3   ALREADY OPEN FOR OUTPUT?                     
         JNZ   PD010               YES                                          
         BRAS  RE,OPNBADM          OPEN FOR OUTPUT                              
*                                                                               
PD010    MVC   APUTMDED,AMSGIN                                                  
         MVC   PMSGOPTS_OPTIONS,=AL4(MQPMO_FAIL_IF_QUIESCING)                   
         MVC   OBJDESC_OBJECTNAME,DQ                                            
         LA    R2,CSQBPUT_STRUCTURE_DQ                                          
         BRAS  RE,CALLMQ                                                        
         JNE   *+2                                                              
*                                  COMMIT BAD MESSAGE                           
         MVC   OBJDESC_OBJECTNAME,DQ                                            
         LA    R2,CSQBCOMM_STRUCTURE                                            
         BRAS  RE,CALLMQ                                                        
         JE    EXITEQ                                                           
         DC    H'0'                                                             
                                                                                
*----------------------------------------------------------------------         
* OPEN BAD MESSAGE QUEUE                                                        
*----------------------------------------------------------------------         
OPNBADM  NTR1                                                                   
*                                                                               
         MVC   OPENOPT3,=A(MQOO_OUTPUT)                                         
         MVC   OBJDESC_OBJECTNAME,DQ                                            
         LA    R2,CSQBOPEN_STRUCTURE_DQ                                         
         BRAS  RE,CALLMQ                                                        
         JE    EXITEQ                                                           
         DC    H'0'                                                             
                                                                                
*----------------------------------------------------------------------         
* CLOSE BAD MESSAGE QUEUE                                                       
*----------------------------------------------------------------------         
CLOBADM  NTR1                                                                   
         OC    OPENOPT3,OPENOPT3   IS THE QUEUE OPEN?                           
         JZ    EXITEQ              NO: JUST EXIT                                
*                                                                               
         MVC   CLOSOPT,=A(MQCO_NONE)  SET CLOSE OPTIONS                         
         MVC   OBJDESC_OBJECTNAME,DQ                                            
         LA    R2,CSQBCLOS_STRUCTURE_DQ                                         
         BRAS  RE,CALLMQ                                                        
         JE    EXITEQ                                                           
         DC    H'0'                                                             
                                                                                
*----------------------------------------------------------------------         
* OPEN BACKUP QUEUE FOR BROWSE                                                  
*----------------------------------------------------------------------         
OBRBACK  NTR1                                                                   
*                                                                               
         MVC   OPENOPT2,=A(MQOO_BROWSE)                                         
         MVC   OBJDESC_OBJECTNAME,KQ                                            
         LA    R2,CSQBOPEN_STRUCTURE_KQ                                         
         BRAS  RE,CALLMQ                                                        
         JE    EXITEQ                                                           
         DC    H'0'                                                             
                                                                                
*----------------------------------------------------------------------         
* OPEN BACKUP QUEUE FOR INPUT                                                   
*----------------------------------------------------------------------         
OINBACK  NTR1                                                                   
*                                                                               
         MVC   OPENOPT2,=A(MQOO_INPUT_AS_Q_DEF)                                 
         MVC   OBJDESC_OBJECTNAME,KQ                                            
         LA    R2,CSQBOPEN_STRUCTURE_KQ                                         
         BRAS  RE,CALLMQ                                                        
         JE    EXITEQ                                                           
         DC    H'0'                                                             
                                                                                
*----------------------------------------------------------------------         
* OPEN BACKUP QUEUE FOR OUTPUT                                                  
*----------------------------------------------------------------------         
OOPBACK  NTR1                                                                   
*                                                                               
         MVC   OPENOPT2,=A(MQOO_OUTPUT)                                         
         MVC   OBJDESC_OBJECTNAME,KQ                                            
         LA    R2,CSQBOPEN_STRUCTURE_KQ                                         
         BRAS  RE,CALLMQ                                                        
         JE    EXITEQ                                                           
         DC    H'0'                                                             
                                                                                
*----------------------------------------------------------------------         
* GET MESSAGE FROM BACKUP QUEUE                                                 
*----------------------------------------------------------------------         
GETBACK  NTR1                                                                   
*                                                                               
         CLC   OPENOPT2,=A(MQOO_INPUT_AS_Q_DEF) ALREADY OPEN FOR INPUT?         
         JE    GB010                            YES                             
         MVC   SVINDI,INDI         SAVE INDICATORS                              
         OI    INDI,INNOLOG        SUPRESS LOG MESSAGES                         
         BRAS  RE,CLOBACK          CLOSE                                        
         BRAS  RE,OINBACK          OPEN FOR INPUT                               
         MVC   INDI,SVINDI         RESTORE INDICATORS                           
*                                                                               
GB010    MVC   AGETMBK,AMSGIN                                                   
         MVC   MSGDESC_CORRELID,MQCI_NONE                                       
         MVC   MSGDESC_MSGID,MQMI_NONE                                          
         MVC   GMSGOPTS_OPTIONS,=A(MQGMO_SET_SIGNAL)                            
         XC    INPQECB,INPQECB                                                  
         MVC   GMSGOPTS_SIGNAL1,=A(INPQECB)                                     
         MVC   GMSGOPTS_WAITINTERVAL,=A(MQWI_UNLIMITED)                         
         MVC   GMSGOPTS_MATCHOPTIONS,=A(MQMO_NONE)                              
*                                                                               
         MVC   OBJDESC_OBJECTNAME,KQ                                            
         LA    R2,CSQBGET_STRUCTURE_KQ                                          
         BRAS  RE,CALLMQ           TRY TO GET A MESSAGE                         
         BE    GBOKX               GOT A MESSAGE                                
         CLC   COMPCODE,=A(MQCC_WARNING)                                        
         JNE   *+2                                                              
         CLC   REASON,=A(MQRC_SIGNAL_REQUEST_ACCEPTED)                          
         JNE   *+2                 MESSAGE FAILURE                              
*                                                                               
GBNOX    J     EXITNE              SET CC NOT EQUAL-NO MESSAGE                  
GBOKX    J     EXITEQ              SET CC EQUAL-MESSAGE RECEIVED                
                                                                                
*----------------------------------------------------------------------         
* BROWSE A MESSAGE FROM BACKUP QUEUE                                            
*----------------------------------------------------------------------         
         USING MSGTABD,R5                                                       
BROBACK  NTR1                                                                   
*                                                                               
         MVC   SVMQHDR,SPACES      CLEAR SAVED MQ MESSAGE HEADER                
*                                                                               
         CLC   OPENOPT2,=A(MQOO_BROWSE) ALREADY OPEN FOR BROWSE?                
         JE    BB010               YES                                          
         MVC   SVINDI,INDI         SAVE INDICATORS                              
         OI    INDI,INNOLOG        SUPRESS LOG MESSAGES                         
         BRAS  RE,CLOBACK          CLOSE                                        
         BRAS  RE,OBRBACK          OPEN FOR BROWSE                              
         MVC   INDI,SVINDI         RESTORE INDICATORS                           
*                                                                               
BB010    MVC   AGETMBK,AMSGIN                                                   
         MVC   GMSGOPTS_OPTIONS,=A(MQGMO_BROWSE_FIRST)                          
BB020    MVC   MSGDESC_CORRELID,MQCI_NONE                                       
         MVC   MSGDESC_MSGID,MQMI_NONE                                          
         MVC   GMSGOPTS_WAITINTERVAL,=A(MQWI_UNLIMITED)                         
         MVC   GMSGOPTS_MATCHOPTIONS,=A(MQMO_NONE)                              
*                                                                               
         MVC   OBJDESC_OBJECTNAME,KQ                                            
         LA    R2,CSQBGET_STRUCTURE_KQ                                          
         BRAS  RE,CALLMQ           TRY TO GET A MESSAGE                         
         BE    BB050               MESSAGE FOUND                                
*                                                                               
         GOTOR NOTIFY,NOBACKUP     MISSING MESSAGE IN BACKUP QUEUE              
*                                                                               
         L     R5,ASMSGTAB         LIST OF DEMO TABLES                          
BB040    CLI   0(R5),EOT           END OF TABLE                                 
         BE    BBNOX                                                            
         TM    MSGTOPT,MSGTODS     THIS MESSAGE TYPE REQUIRES BUILD?            
         BZ    BB045               NO: SKIP THIS ENTRY                          
         OI    MSGTIND,MSGTIDS     MARK ALL AS ATTEMPTED TO BUILD               
BB045    AHI   R5,MSGTLNQ                                                       
         B     BB040                                                            
*                                                                               
BB050    SAM31                                                                  
         L     R2,AMSGIN           VALIDATE MESSAGE HEADER                      
         USING DEMQHDRD,R2                                                      
         L     R5,ASMSGTAB         LIST OF DEMO TABLES                          
BB060    CLI   0(R5),EOT           END OF TABLE                                 
         BE    BB070               BAD MESSAGE IN BACKUP QUEUE                  
         TM    MSGTOPT,MSGTODS     THIS MESSAGE TYPE REQUIRES BUILD?            
         BZ    BB065               NO: SKIP THIS ENTRY                          
         CLC   DMHDESC,MSGTTAG     DESCRIPTOR MATCH                             
         BE    BB080               YES: CHECK IF THIS IS ONE WE WANT            
BB065    AHI   R5,MSGTLNQ          NO: TRY NEXT TABLE ENTRY                     
         B     BB060                                                            
*                                                                               
BB070    SAM24                                                                  
         GOTOR NOTIFY,NOBBMESS     BAD MESSAGE IN BACKUP QUEUE                  
         MVC   GMSGOPTS_OPTIONS,=A(MQGMO_BROWSE_NEXT) SETUP FOR NEXT            
         B     BB020                                                            
*                                                                               
BB080    SAM24                                                                  
         ST    R5,ACMSGTAB         CURRENT TABLE ENTRY                          
         MVC   GMSGOPTS_OPTIONS,=A(MQGMO_BROWSE_NEXT) SETUP FOR NEXT            
         TM    MSGTIND,MSGTIDS     DO WE NEED TO BUILD THIS TABLE?              
         BO    BB020               NO: THEN GET NEXT                            
         OI    MSGTIND,MSGTIDS     MARK IT FOR AN ATTEMPT TO BUILD              
         B     BBOKX               YES: THEN PROCESS THIS MESSAGE               
         DROP  R2                                                               
*                                                                               
BBNOX    J     EXITNE                                                           
BBOKX    J     EXITEQ                                                           
         DROP  R5                                                               
                                                                                
*----------------------------------------------------------------------         
* PUT BACKUP MQ MESSAGE                                                         
*----------------------------------------------------------------------         
PUTBACK  NTR1                                                                   
*                                                                               
         CLC   OPENOPT2,=A(MQOO_OUTPUT) ALREADY OPEN FOR OUTPUT?                
         JE    PB010               YES                                          
         MVC   SVINDI,INDI         SAVE INDICATORS                              
         OI    INDI,INNOLOG        SUPRESS LOG MESSAGES                         
         BRAS  RE,CLOBACK          CLOSE                                        
         BRAS  RE,OOPBACK          OPEN FOR OUTPUT                              
         MVC   INDI,SVINDI         RESTORE INDICATORS                           
*                                                                               
PB010    MVC   APUTMBK,AMSGIN                                                   
         MVC   PMSGOPTS_OPTIONS,=AL4(MQPMO_FAIL_IF_QUIESCING)                   
         MVC   OBJDESC_OBJECTNAME,KQ                                            
         LA    R2,CSQBPUT_STRUCTURE_KQ                                          
         BRAS  RE,CALLMQ                                                        
         JE    EXITEQ                                                           
         DC    H'0'                                                             
                                                                                
*----------------------------------------------------------------------         
* COMMIT BACKUP MQ MESSAGE                                                      
*----------------------------------------------------------------------         
COMBACK  NTR1                                                                   
*                                                                               
         MVC   OBJDESC_OBJECTNAME,KQ                                            
         LA    R2,CSQBCOMM_STRUCTURE                                            
         BRAS  RE,CALLMQ                                                        
         JE    EXITEQ                                                           
         DC    H'0'                                                             
                                                                                
*----------------------------------------------------------------------         
* CLOSE BACKUP QUEUE                                                            
*----------------------------------------------------------------------         
CLOBACK  NTR1                                                                   
         OC    OPENOPT2,OPENOPT2   IS THE QUEUE OPEN?                           
         JZ    EXITEQ              NO: JUST EXIT                                
*                                                                               
         MVC   CLOSOPT,=A(MQCO_NONE)  SET CLOSE OPTIONS                         
         MVC   OBJDESC_OBJECTNAME,KQ                                            
         LA    R2,CSQBCLOS_STRUCTURE_KQ                                         
         BRAS  RE,CALLMQ                                                        
         JE    EXITEQ                                                           
         DC    H'0'                                                             
                                                                                
***********************************************************************         
* CALL MQ                                                                       
* UPON ENTRY,  R2 POINTS TO PARAMETER STRUCTURE                                 
* UPON RETURN, COMPCODE CONTAINS THE MQ COMPLETION CODE                         
*              REASON CONTAINS THE MQ REASON CODE                               
***********************************************************************         
CALLMQ   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SAM31 ,                   SWITCH TO 31-BIT MODE                        
         L     RF,0(R2)            RF = A(MQ ROUTINE)                           
         LA    R3,24(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
         SAM24 ,                   SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         BRAS  RE,CHKOPS           OPERATOR INTERRUPT                           
*                                                                               
         MVI   P,C'+'              "+" MEANS IT'S AN MQ CALL                    
         MVC   P+1(16),4(R2)       PRINT NAME AND RETURN CODES                  
         MVC   P+50(L'OBJDESC_OBJECTNAME),OBJDESC_OBJECTNAME                    
*                                                     -----------------         
         CLC   COMPCODE,=A(MQCC_OK)                    CALL OK                  
         BNE   CMQ010                                 -----------------         
         TM    INDI,INNOLOG         DON'T DISPLAY INFO LOG?                     
         BO    CMQOKX               YES: SKIP IT                                
         MVC   PMSG(12),=C'COMPLETED OK'                                        
         GOTOR PRNT,MESSAGESET                                                  
         B     CMQOKX                                                           
*                                                     -----------------         
CMQ010   CLC   COMPCODE,=A(MQCC_WARNING)               CALL WARNING             
         BNE   CMQ020                                 -----------------         
         CLC   REASON,=A(MQRC_SIGNAL_REQUEST_ACCEPTED)                          
         BE    CMQ015                                                           
         CLC   REASON,=A(MQRC_NO_MSG_AVAILABLE)                                 
         BNE   CMQ030                                                           
CMQ015   TM    INDI,INNOLOG        DON'T DISPLAY INFO LOG?                      
         BO    CMQNOX              YES: SKIP IT                                 
         MVC   PMSG(16),=C'NO MESSAGE FOUND'                                    
         GOTOR PRNT,MESSAGESET                                                  
         J     CMQNOX                                                           
*                                                     -----------------         
CMQ020   MVC   PMSG(12),=C'** FAILED **'               CALL FAILED              
         CLC   COMPCODE,=A(MQCC_FAILED)               -----------------         
         BNE   CMQ030                                                           
         CLC   REASON,=A(MQRC_NO_MSG_AVAILABLE)                                 
         BE    CMQ015              JUST WARN IF NO MESSAGE FOUND                
CMQ030   MVC   P+70(9),=C'REASON = '                                            
         EDIT  REASON,(5,P+79),ALIGN=LEFT,ZERO=NOBLANK                          
         MVC   P+86(13),=C'*** ERROR ***'                                       
         GOTOR PRNT,MESSAGESET                                                  
         DC    H'0'                                                             
*                                                                               
CMQOKX   J     EXITEQ              SET CC EQUAL                                 
CMQNOX   J     EXITNE              SET CC NOT EQUAL                             
                                                                                
***********************************************************************         
* SET UP OPERATOR COMMUNICATIONS                                                
***********************************************************************         
SETOPS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         EXTRACT ACOMM,FIELDS=COMM                                              
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
*                                                                               
         L     R2,COMCIBPT         GET A(CIB)                                   
         USING CIBNEXT,R2                                                       
         LA    R3,COMCIBPT         SET A(A(CIB))                                
         DROP  RF                                                               
*                                                                               
         CLI   CIBVERB,CIBSTART    TEST FOR 'S JOBNAME'                         
         BNE   SETOP2              (IE NOT BATCH)                               
         DROP  R2                                                               
         QEDIT ORIGIN=(R3),BLOCK=(R2)  RELEASE THE CIB                          
SETOP2   QEDIT ORIGIN=(R3),CIBCTR=1    ACCEPT MODIFY COMMANDS                   
*                                                                               
         J     EXIT                                                             
                                                                                
***********************************************************************         
* CHECK FOR OPERATOR COMMANDS                                                   
***********************************************************************         
CHKOPS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,AOPERECB         A(OPERATOR ECB)                              
         TM    0(RF),X'40'         DID THE OPERATOR INTERRUPT?                  
         BZ    COXIT               NO                                           
*                                                                               
         L     RF,ACOMM            SET RF TO COMM BLOCK                         
         USING COMLIST,RF                                                       
         L     R2,COMCIBPT         A(CIB)                                       
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         BNE   CO010               NO                                           
         LA    R6,OPERSTOP                                                      
         MVI   OPSTOP,YES                                                       
         J     CO020                                                            
*                                                                               
CO010    CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER 'MODIFY'?                 
         JNE   *+2                 Then what is it?                             
         LA    R6,OPERMODIFY                                                    
         CLC   =C'BUILD',CIBDATA                                                
         BNE   CO020                                                            
         LA    R6,OPERBUILD                                                     
         MVI   OPBUILD,YES                                                      
         MVI   INDI,INFORCE+INBUILD                                             
         DROP  R2                                                               
*                                                                               
         L     R5,ASMSGTAB         LIST OF DEMO TABLES                          
         USING MSGTABD,R5                                                       
CO015    CLI   0(R5),EOT           END OF TABLE                                 
         BE    CO020                                                            
         NI    MSGTIND,X'FF'-MSGTIDS UNMARK ATTEMPTED BUILD STATUS              
         AHI   R5,MSGTLNQ                                                       
         B     CO015                                                            
         DROP  R5                                                               
*                                                                               
CO020    GOTOR PRNT,(R6)                                                        
*                                                                               
         L     RF,ACOMM            RESET OPER COMMS                             
         USING COMLIST,RF                                                       
         ICM   R2,15,COMCIBPT      A(CIB)                                       
         BZ    COXIT                                                            
         LA    R3,COMCIBPT         A(A(CIB))                                    
         QEDIT ORIGIN=(R3),BLOCK=(R2)  FREE THE CIB                             
         DROP  RF                                                               
*                                                                               
COXIT    J     EXIT                                                             
                                                                                
***********************************************************************         
* UPDATE COMSCORE LICENSE RECORDS                                               
***********************************************************************         
UPDTLDC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   PMSG(L'DMHDESC),SVMQHDR DESCRIPTOR                               
         GOTOR PRNT,UPDATERECS                                                  
*                                                                               
         L     R2,AMSGIN           VALIDATE MESSAGE HEADER                      
         USING DEMQHDRD,R2                                                      
*                                                                               
         L     R5,ACMSGTAB         CURRENT TABLE ENTRY                          
         USING MSGTABD,R5                                                       
*                                                                               
LC020    CLC   DMHDTAG,=C'ELEN='   INFO HAS ENTRY LENGTH                        
         BNE   LCERX1              NO                                           
         CLC   DMHDLEN,=C'0000'                                                 
         BL    LCERX1                                                           
         PACK  DUB,DMHDLEN         PICK UP LENGTH                               
         CVB   R3,DUB                                                           
         STCM  R3,15,SVMQELEN      SAVE LENGTH OF ENTRY                         
*                                                                               
         ICM   R1,15,DATALEN                                                    
         AHI   R1,-DMHLNQ          SUBTRACT HEADER LENGTH                       
         AR    R1,R2               CALCULATE END OF MESSAGE                     
         ST    R1,MESSEND          SAVE END OF MESSAGE                          
*                                                                               
         LA    R2,DMHDATA                                                       
         USING LDCMSGD,R2                                                       
*                                                                               
         XC    LICCODE,LICCODE     INIT VARIABLES AND INDICATORS                
         XC    LICSEQ,LICSEQ                                                    
         NI    PINDI,X'FF'-PIELER-PIREFR  RESET ERR & REFRESH INDICATOR         
*                                                                               
         CLI   LDCACTN,C'R'        REFRESH / FULL LOAD                          
         BNE   LC040               NO                                           
         OI    PINDI,PIREFR                                                     
         XC    REFRDATE,REFRDATE                                                
         XC    REFRTIME,REFRTIME                                                
*                                                                               
LC040    BRAS  RE,OPENCTL          OPEN THE CONTROL SYSTEM                      
         BNE   LCERX2                                                           
                                                                                
*----------------------------------------------------------------------         
* PROCESS LICENSE RECORD                                                        
*----------------------------------------------------------------------         
LC050    C     R2,MESSEND          CHECK FOR END OF MESSAGE                     
         BNL   LC500                                                            
*                                                                               
         NI    PINDI,X'FF'-PIRNF-PIRFDEL  RESET RECORD INDICATORS               
*                                                                               
         CLC   LICCODE,LDCLICEN    SAME LICENSE AS PREVIOUS?                    
         BNE   LC055               NO: MUST PROCESS                             
         OC    LICSEQ,LICSEQ       DO WE HAVE A SEQUENCE NUMBER?                
         BNZ   LC200               YES: NO NEED TO CHECK LICENSE                
*                                                                               
LC055    MVC   LICCODE,LDCLICEN    SET LICENSE CODE                             
         XC    LICSEQ,LICSEQ       CLEAR LICENSE SEQUENCE                       
*                                                                               
         LA    R3,KEY                                                           
         USING CSLRECD,R3                                                       
         XC    CSLKEY,CSLKEY                                                    
         MVI   CSLKMIN,CSLKMIQ                                                  
         MVI   CSLKREC,CSLKREQ                                                  
         MVC   CSLKLIC,LICCODE                                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,(DMBITS,DMREAD),GENDIR,KEY,KEY                     
         CLI   8(R1),0                                                          
         BE    LC080                                                            
         TM    8(R1),X'10'         RECORD NOT FOUND, NEED TO ADD                
         BO    LC070                                                            
         TM    8(R1),X'02'         RECORD MARKED DELETED?                       
         BZ    LCE01               NO: DISK ERROR READING FOR LICENSE           
         OI    PINDI,PIRFDEL       RECORD FOUND BUT DELETED                     
         B     LC080                                                            
*                                                                               
LC070    OI    PINDI,PIRNF         RECORD NOT FOUND, NEED TO ADD                
*                                                                               
         L     R3,AIO                                                           
         XC    CSLKEY(CSLFIRST+CSLSLNQ+2),CSLKEY                                
         MVI   CSLKMIN,CSLKMIQ                                                  
         MVI   CSLKREC,CSLKREQ                                                  
         MVC   CSLKLIC,LICCODE                                                  
         MVC   CSLRLEN,=AL2(CSLFIRST+CSLSLNQ+1) LENGTH W/ FIRST ELEMENT         
*                                          CSLFIRST+CSLSLNQ+1                   
         USING CSLSD,R4                    CSLFIRST+CSLSLNQ+2                   
         LA    R4,CSLFIRST(,R3)                                                 
         MVI   CSLSEL,CSLSELQ      X'05' COMSCORE LICENSE SEQUENCE ELEM         
         MVI   CSLSLN,CSLSLNQ                                                   
         B     LC090                                                            
         DROP  R4                                                               
*                                                                               
LC080    GOTO1 VDATAMGR,DMCB,(DMBITS,GETREC),GENFIL,CSLDA,AIO,AIOWRK            
         CLI   8(R1),0                                                          
         BNE   LCE02                                                            
*                                                                               
LC090    L     R3,AIO                                                           
         LA    R4,CSLFIRST(,R3)                                                 
LC100    CLI   0(R4),0             LICENSE NOT FOUND                            
         BE    LC180               . YES                                        
         CLI   0(R4),CSLSELQ       X'05' COMSCORE LICENSE SEQUENCE #            
         BE    LC160                                                            
         CLI   0(R4),CSFLELQ       X'0F' COMSCORE FULL LICENSE CODE             
         BE    LC170                                                            
LC110    LLC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     LC100                                                            
*                                                                               
         USING CSLSD,R4                                                         
LC160    XR    RF,RF                                                            
         ICM   RF,3,CSLSEQ                                                      
         AHI   RF,1                BUMP 1                                       
         STCM  RF,3,CSLSEQ         AND SAVE                                     
         MVC   LICSEQ,CSLSEQ       GRAB CURRENT SEQUENCE NUMBER                 
         B     LC110                                                            
*                                                                               
         USING CSFLD,R4                                                         
LC170    CLC   CSFLIC,LICCODE      COMPARE FULL LICENSE NUMBER                  
         BNE   LC110               NOT THE ONE, KEEP SEARCHING                  
         MVC   LICSEQ,CSFLSEQ                                                   
         B     LC200               ALREADY HERE, SO MOVE ON                     
*                                                                               
NEW      USING CSFLD,RF                                                         
LC180    OC    LICSEQ,LICSEQ       ANY SEQUENCE DEFINED?                        
         BNZ   *+8                                                              
         MVI   LICSEQ+1,1          SET FIRST IN SEQUENCE                        
*                                                                               
         LA    RF,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   NEW.CSFLEL,CSFLELQ  X'0F' COMSCORE FULL LICENSE ELEM             
         MVI   NEW.CSFLLN,CSFLLNQ  LENGTH                                       
         MVC   NEW.CSFLIC,LICCODE  FULL LICENSE NUMBER                          
         MVC   NEW.CSFLSEQ,LICSEQ  NEXT AVAILABLE SEQUENCE NUMBER               
         DROP  NEW                                                              
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',GENFIL),(R3),ELEM,0,TO24=YES                   
         CLI   12(R1),0                                                         
         BNE   LCE03               ERROR ADDING LICENSE ELEMENT                 
*                                                                               
LC190    OC    LICSEQ,LICSEQ       MAKE SURE WE HAVE A LICENSE SEQ #            
         BZ    LCE04               NO: THAT'S A PROBLEM                         
*                                                                               
         BRAS  RE,SETGACT          SET GENFIL ACTIVITY ELEMENT                  
*                                                                               
         CLI   RWRITE,C'Y'         WRITING TO FILE?                             
         BNE   LC200               NO: SKIP PUT                                 
         TM    PINDI,PIRNF                                                      
         BZ    LC195                                                            
         GOTO1 VDATAMGR,DMCB,ADDREC,GENFIL,DA,AIO,AIOWRK                        
         B     LC196                                                            
LC195    GOTO1 VDATAMGR,DMCB,PUTREC,GENFIL,AIO,AIO,AIOWRK                       
LC196    CLI   8(R1),0                                                          
         BNE   LCE05               ERROR UPDATING RECORD ON CHANGE              
                                                                                
*----------------------------------------------------------------------         
* PROCESS LICENSE DEMO DEFINITION RECORD                                        
*----------------------------------------------------------------------         
LC200    LA    R3,KEY                                                           
         USING LDCRECD,R3                                                       
         XC    LDCKEY,LDCKEY                                                    
         MVI   LDCKMIN,LDCKMIQ     DEMO SYSTEM (D)                              
         MVI   LDCKREC,LDCKREQ     LICENSE DEMO DEFINITION RECORD (D)           
         MVC   LDCKLIC,LICCODE     LICENSE CODE (FIRST 10)                      
         MVC   LDCKSEQ,LICSEQ      INTERNAL LICENSE SEQUENCE NUMBER             
         MVC   LDCKOXC,LDCOXCD     OX DEMO CATEGORY CODE                        
*                                                                               
         NI    PINDI,X'FF'-PIRNF-PIRFDEL  RESET RECORD INDICATORS               
*                                                                               
         CLI   LDCACTN,C'D'        DELETE?                                      
         BNE   LC300                                                            
*                                                                               
         GOTO1 VDATAMGR,DMCB,(DMBITS,DMREAD),GENDIR,KEY,KEY                     
         CLI   8(R1),0                                                          
         BE    LC260                                                            
         TM    8(R1),X'10'         RECORD NOT FOUND, NO NEED TO DELETE          
         BO    LC400                                                            
         TM    8(R1),X'02'         RECORD MARKED DELETED ALREADY                
         BO    LC400                                                            
         B     LCE06               DISK ERROR READING FOR LICENSE               
*                                                                               
LC260    GOTO1 VDATAMGR,DMCB,(DMBITS,GETREC),GENFIL,LDCDA,AIO,AIOWRK            
         CLI   8(R1),0                                                          
         BNE   LCE07                                                            
*                                                                               
         CLI   RWRITE,C'Y'         WRITING TO FILE?                             
         BNE   LC400               NO: SKIP PUT                                 
*                                                                               
         L     R3,AIO                                                           
         OI    LDCRSTAT,X'80'      DELETE RECORD                                
         GOTO1 VDATAMGR,DMCB,PUTREC,GENFIL,AIO,AIO,AIOWRK                       
         CLI   8(R1),0                                                          
         BNE   LCE08               ERROR UPDATING RECORD ON REMOVAL             
*                                                                               
         LA    R3,KEY                                                           
         OI    LDCDSTAT,X'80'      DELETE RECORD                                
         GOTO1 VDATAMGR,DMCB,DMWRT,GENDIR,KEY,KEY                               
         CLI   8(R1),0                                                          
         BNE   LCE09                                                            
         B     LC400                                                            
                                                                                
*----------------------------------------------------------------------         
* ADD/CHANGE COMSCORE CUSTOM LICENCE CATEGORY RECORD                            
*----------------------------------------------------------------------         
LC300    CLI   LDCACTN,C'A'        Modify?                                      
         BE    LC310                                                            
         CLI   LDCACTN,C'R'        REFRESH / FULL LOAD                          
         BNE   LCE10               NO: INVALID ACTION                           
*                                                                               
LC310    GOTO1 VDATAMGR,DMCB,(DMBITS,DMREAD),GENDIR,KEY,KEY                     
         CLI   8(R1),0                                                          
         BE    LC350                                                            
         TM    8(R1),X'02'         MARKED DELETED, need to restore              
         BO    LC330                                                            
         TM    8(R1),X'10'         NOT FOUND, NEED TO add                       
         BO    LC340                                                            
         B     LCE11               DISK ERROR READING FOR LICENSE               
*                                                                               
LC330    OI    PINDI,PIRFDEL       RECORD FOUND BUT DELETED                     
         B     LC350                                                            
*                                                                               
LC340    OI    PINDI,PIRNF         RECORD NOT FOUND, NEED TO ADD                
*                                                                               
         L     R3,AIO                                                           
         XC    LDCKEY(LDCFIRST+CSFLLNQ+2),LDCKEY                                
         MVI   LDCKMIN,LDCKMIQ     DEMO SYSTEM (D)                              
         MVI   LDCKREC,LDCKREQ     LICENSE DEMO CATEGORY RECORD (D)             
         MVC   LDCKLIC,LICCODE     LICENSE CODE (FIRST 10)                      
         MVC   LDCKSEQ,LICSEQ      INTERNAL LICENSE SEQUENCE NUMBER             
         MVC   LDCKOXC,LDCOXCD     OX DEMO CATEGORY CODE                        
         MVC   LDCRLEN,=AL2(LDCFIRST+CSFLLNQ+1)                                 
*                                                                               
         LA    R4,CSLFIRST(,R3)                                                 
         USING CSFLD,R4                                                         
         MVI   CSFLEL,CSFLELQ      BUILD FULL ELEMENT                           
         MVI   CSFLLN,CSFLLNQ                                                   
         MVC   CSFLIC,LICCODE                                                   
         MVC   CSFLSEQ,LICSEQ                                                   
         B     LC360                                                            
*                                                                               
LC350    GOTO1 VDATAMGR,DMCB,(DMBITS,GETREC),GENFIL,LDCDA,AIO,AIOWRK            
         CLI   8(R1),0                                                          
         BNE   LCE12                                                            
*                                                                               
         L     R3,AIO                                                           
         LA    R4,LDCFIRST(,R3)                                                 
LC360    CLI   0(R4),0                                                          
         BE    LC380                                                            
         CLI   0(R4),CSFLELQ       X'0F' COMSCORE FULL LICENSE CODE             
         BE    LC370                                                            
         LLC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     LC360                                                            
*                                                                               
         USING CSFLD,R4                                                         
LC370    MVI   CSFLEL,CSFLELQ      X'0F' COMSCORE FULL LICENSE ELEM             
         MVI   CSFLLN,CSFLLNQ      LENGTH                                       
         MVC   CSFLIC,LICCODE      FULL LICENSE NUMBER                          
         MVC   CSFLSEQ,LICSEQ      NEXT AVAILABLE SEQUENCE NUMBER               
         B     LC390                                                            
*                                                                               
NEW      USING CSFLD,RF                                                         
LC380    LA    RF,ELEM                                                          
         MVI   NEW.CSFLEL,CSFLELQ  X'0F' COMSCORE FULL LICENSE ELEM             
         MVI   NEW.CSFLLN,CSFLLNQ  LENGTH                                       
         MVC   NEW.CSFLIC,LICCODE  FULL LICENSE NUMBER                          
         MVC   NEW.CSFLSEQ,LICSEQ  NEXT AVAILABLE SEQUENCE NUMBER               
         DROP  NEW                                                              
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',GENFIL),(R3),ELEM,0,TO24=YES                   
         CLI   12(R1),0                                                         
         BNE   LCE13               ERROR ADDING LICENSE ELEMENT                 
*                                                                               
LC390    BRAS  RE,SETGACT          SET GENFIL ACTIVITY ELEMENT                  
*                                                                               
         CLI   RWRITE,C'Y'         WRITING TO FILE?                             
         BNE   LC400               NO: SKIP PUT                                 
*                                                                               
         NI    LDCRSTAT,X'FF'-X'80' ALWAYS TURN OFF DELETE BIT                  
*                                                                               
         TM    PINDI,PIRNF                                                      
         BZ    LC395                                                            
         GOTO1 VDATAMGR,DMCB,ADDREC,GENFIL,DA,AIO,AIOWRK                        
         CLI   8(R1),0                                                          
         BNE   LCE14               ERROR ADDING RECORD                          
         B     LC400                                                            
*                                                                               
LC395    GOTO1 VDATAMGR,DMCB,PUTREC,GENFIL,AIO,AIO,AIOWRK                       
         CLI   8(R1),0                                                          
         BNE   LCE15               ERROR UPDATING RECORD ON CHANGE              
*                                                                               
         TM    PINDI,PIRFDEL       RECORD FOUND BUT DELETED?                    
         BZ    LC400               NO: MOVE ON                                  
*                                                                               
         LA    R3,KEY                                                           
         NI    LDCDSTAT,X'FF'-X'80' TURN OFF THE DELETE BIT                     
         GOTO1 VDATAMGR,DMCB,DMWRT,GENDIR,KEY,KEY                               
         CLI   8(R1),0                                                          
         BNE   LCE16               ERROR UPDATING RECORD ON CHANGE              
                                                                                
*----------------------------------------------------------------------         
* ADVANCE TO NEXT ENTRY                                                         
*----------------------------------------------------------------------         
LC400    A     R2,SVMQELEN         BUMP TO NEXT MESSAGE                         
         B     LC050                                                            
                                                                                
*----------------------------------------------------------------------         
* FINISHED PROCESSING                                                           
*----------------------------------------------------------------------         
LC500    TM    PINDI,PIREFR        FULL REFRESH / LOAD?                         
         BZ    LC510                                                            
         BRAS  RE,LDCREFR          REMOVE UNWANTED RECORDS ON REFRESH           
         BE    LC510                                                            
         BRAS  RE,CLOSECTL         CLOSE THE CONTROL SYSTEM                     
         B     LCERX3                                                           
*                                                                               
LC510    BRAS  RE,CLOSECTL         CLOSE THE CONTROL SYSTEM                     
*                                                                               
         CLI   RWRITE,C'Y'         WRITING TO FILE?                             
         BE    LC520                                                            
         GOTOR PRNT,NOUPDATE       SHOW NO UPDATE IF NOT WRITING                
         B     LC530                                                            
LC520    MVC   PMSG(L'DMHDESC),SVMQHDR DESCRIPTOR                               
         GOTOR PRNT,FINISHED                                                    
LC530    TM    PINDI,PIELER        ELEMENT ERROR?                               
         JO    LCERX5              YES: EXIT IN ERROR                           
         J     EXITEQ              NO: EXIT EQUAL                               
                                                                                
*----------------------------------------------------------------------         
* MESSAGE ELEMENT ERRORS                                                        
*----------------------------------------------------------------------         
LCE01    MVC   PMSG,=CL44'*E01 DISK ERROR READING LICENSE RECORD     '          
         B     LCE0X                                                            
LCE02    MVC   PMSG,=CL44'*E02 DISK ERROR GETTING LICENSE RECORD     '          
         B     LCE0X                                                            
LCE03    MVC   PMSG,=CL44'*E03 UNABLE TO ADD ELEMENT TO LICENSE REC  '          
         B     LCE0X                                                            
LCE04    MVC   PMSG,=CL44'*E04 UNABLE TO CALCULATE LICENSE SEQUENCE #'          
         B     LCE0X                                                            
LCE05    MVC   PMSG,=CL44'*E05 UNABLE TO ADD/PUT LICENSE RECORD      '          
         B     LCE0X                                                            
LCE06    MVC   PMSG,=CL44'*E06 DISK ERROR READING  LIC/DEM/CAT REC   '          
         B     LCE0X                                                            
LCE07    MVC   PMSG,=CL44'*E07 DISK ERROR GETTING LIC/DEM/CAT REC    '          
         B     LCE0X                                                            
LCE08    MVC   PMSG,=CL44'*E08 ERROR DELETING LIC/DEM/CAT REC        '          
         B     LCE0X                                                            
LCE09    MVC   PMSG,=CL44'*E09 ERROR DELETING LIC/DEM/CAT REC        '          
         B     LCE0X                                                            
LCE10    MVC   PMSG,=CL44'*E10 Invalid action in message             '          
         B     LCE0X                                                            
LCE11    MVC   PMSG,=CL44'*E11 Disk error reading lic/dem/cat rec    '          
         B     LCE0X                                                            
LCE12    MVC   PMSG,=CL44'*E12 disk error getting lic/dem/cat rec    '          
         B     LCE0X                                                            
LCE13    MVC   PMSG,=CL44'*E13 error adding lic/dem/cat rec          '          
         B     LCE0X                                                            
LCE14    MVC   PMSG,=CL44'*E14 ERROR ADDING LIC/DEM/CAT PASSIVE      '          
         B     LCE0X                                                            
LCE15    MVC   PMSG,=CL44'*E15 ERROR CHANGING LIC/DEM/CAT RECORD     '          
         B     LCE0X                                                            
LCE16    MVC   PMSG,=CL44'*E16 ERROR CHANGING LIC/DEM/CAT RECORD     '          
         B     LCE0X                                                            
LCE0X    GOTOR PRNT,MESSAGESET     PRINT ERROR AND MOVE ON                      
         OI    PINDI,PIELER        INDICATE ELEMENT ERROR                       
         B     LC400                                                            
                                                                                
*----------------------------------------------------------------------         
* MESSAGE ERRORS                                                                
*----------------------------------------------------------------------         
LCERX1   MVC   PMSG,=CL44'*ER21* INVALID ELEMENT LENTH IN MQ MSG     '          
         J     LCERX                                                            
LCERX2   MVC   PMSG,=CL44'*ER22* ERROR OPENING CONTROL SYSTEM        '          
         J     LCERX                                                            
LCERX3   MVC   PMSG,=CL44'*ER23* ERROR DELETING RECORDS ON REFRESH   '          
         J     LCERX                                                            
LCERX5   MVC   PMSG,=CL44'*ER25* ELEMENT ERROR IN LICENSE MESSAGE    '          
*                                                                               
LCERX    MVC   NOTYMESS,PMSG                                                    
         MVI   NOTYMSG#,NOTYSET                                                 
         GOTOR PRNT,MESSAGESET     ERROR MESSAGE OUTPUT                         
         GOTOR PRNT,NOUPDATE       SHOW NO UPDATE ON ERROR                      
         J     EXITNE                                                           
         DROP  R2,R3,R4,R5                                                      
                                                                                
***********************************************************************         
* UPDATE COMSCORE CUSTOM DEMO CATEGORY RECORDS                                  
***********************************************************************         
UPDTCDC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   PMSG(L'DMHDESC),SVMQHDR DESCRIPTOR                               
         GOTOR PRNT,UPDATERECS                                                  
*                                                                               
         L     R2,AMSGIN           VALIDATE MESSAGE HEADER                      
         USING DEMQHDRD,R2                                                      
*                                                                               
         L     R5,ACMSGTAB         CURRENT TABLE ENTRY                          
         USING MSGTABD,R5                                                       
*                                                                               
CD020    CLC   DMHDTAG,=C'ELEN='   INFO HAS ENTRY LENGTH                        
         BNE   CDERX1              NO                                           
         CLC   DMHDLEN,=C'0000'                                                 
         BL    CDERX1                                                           
*                                                                               
         PACK  DUB,DMHDLEN         PICK UP LENGTH                               
         CVB   R3,DUB                                                           
         STCM  R3,15,SVMQELEN      SAVE LENGTH OF ENTRY                         
*                                                                               
         ICM   R1,15,DATALEN                                                    
         AHI   R1,-DMHLNQ          SUBTRACT HEADER LENGTH                       
         AR    R1,R2               CALCULATE END OF MESSAGE                     
         ST    R1,MESSEND          SAVE END OF MESSAGE                          
*                                                                               
         LA    R2,DMHDATA                                                       
         USING CDCMSGD,R2                                                       
*                                                                               
         NI    PINDI,X'FF'-PIELER-PIREFR  RESET ERR & REFRESH INDICATOR         
*                                                                               
         CLI   CDCACTN,C'R'        REFRESH / FULL LOAD                          
         BNE   CD040               NO                                           
         OI    PINDI,PIREFR                                                     
         XC    REFRDATE,REFRDATE                                                
         XC    REFRTIME,REFRTIME                                                
*                                                                               
CD040    BRAS  RE,OPENCTL          OPEN THE CONTROL SYSTEM                      
         BNE   CDERX2                                                           
                                                                                
*----------------------------------------------------------------------         
* PROCESS CUSTOM DEMO RECORD                                                    
*----------------------------------------------------------------------         
CD050    C     R2,MESSEND          CHECK FOR END OF MESSAGE                     
         BNL   CD500                                                            
*                                                                               
         NI    PINDI,X'FF'-PIRNF-PIRFDEL  RESET RECORD INDICATORS               
*                                                                               
         LA    R3,KEY                                                           
         USING CDCRECD,R3                                                       
         XC    CDCKEY,CDCKEY                                                    
         MVI   CDCKMIN,CDCKMIQ     C'D' DEMO SYSTEM                             
         MVI   CDCKREC,CDCKREQ     C'C' CUSTOM CATEGORY                         
         MVC   CDCKOXC,CDCOXCD                                                  
                                                                                
*----------------------------------------------------------------------         
* DELETE COMSCORE CUSTOM DEMO CATEGORY                                          
*----------------------------------------------------------------------         
         CLI   CDCACTN,C'D'        DELETE?                                      
         BNE   CD300                                                            
*                                                                               
         GOTO1 VDATAMGR,DMCB,(DMBITS,DMREAD),GENDIR,KEY,KEY                     
         CLI   8(R1),0                                                          
         BE    CD260                                                            
         TM    8(R1),X'10'         RECORD NOT FOUND, NO NEED TO DELETE          
         BO    CD400                                                            
         TM    8(R1),X'02'         RECORD MARKED DELETED ALREADY                
         BO    CD400                                                            
         B     CDE01               DISK ERROR READING FOR LICENSE               
*                                                                               
CD260    GOTO1 VDATAMGR,DMCB,(DMBITS,GETREC),GENFIL,CDCDA,AIO,AIOWRK            
         CLI   8(R1),0                                                          
         BNE   CDE02                                                            
*                                                                               
         L     R3,AIO                                                           
         OI    CDCRSTAT,X'80'      DELETE RECORD                                
*                                                                               
         LA    R4,CDCFIRST(,R3)                                                 
CD262    CLI   0(R4),0                                                          
         BE    CDE03               NO NUMBER FOUND                              
         CLI   0(R4),CDCELQ        X'0A' CUSTOM DEMO CATEGORY ELEMENT           
         BE    CD266                                                            
CD264    LLC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     CD262                                                            
*                                                                               
         USING CDCELD,R4                                                        
CD266    MVC   LICCAT,CDCNUM       GET NUMBER TO DELETE PASSIVE                 
*                                                                               
         CLI   RWRITE,C'Y'         WRITING TO FILE?                             
         BNE   CD270               NO: SKIP PUT                                 
         GOTO1 VDATAMGR,DMCB,PUTREC,GENFIL,AIO,AIO,AIOWRK                       
         CLI   8(R1),0                                                          
         BNE   CDE04               ERROR UPDATING RECORD ON REMOVAL             
*                                                                               
         LA    R3,KEY                                                           
         OI    CDCDSTAT,X'80'      DELETE RECORD                                
         GOTO1 VDATAMGR,DMCB,DMWRT,GENDIR,KEY,KEY                               
         CLI   8(R1),0                                                          
         BNE   CDE05                                                            
*                                                                               
CD270    LA    R3,KEY                                                           
         XC    CDCKEY,CDCKEY                                                    
         MVI   CDCKMIN,CDCKMIQ     C'D' DEMO SYSTEM                             
         MVI   CDCKREC,CDCKREQ     C'C' CUSTOM CATEGORY                         
         MVC   CDCKNUM,LICCAT      DEMO CATEGORY NUMBER                         
*                                                                               
         GOTO1 VDATAMGR,DMCB,(DMBITS,DMREAD),GENDIR,KEY,KEY                     
         CLI   8(R1),0                                                          
         BE    CD280                                                            
         TM    8(R1),X'10'         RECORD NOT FOUND, NO NEED TO DELETE          
         BO    CD400                                                            
         TM    8(R1),X'02'         RECORD MARKED DELETED ALREADY                
         BO    CD400                                                            
         B     CDE06               DISK ERROR READING FOR LICENSE               
*                                                                               
CD280    OI    CDCDSTAT,X'80'      DELETE RECORD                                
*                                                                               
         CLI   RWRITE,C'Y'         WRITING TO FILE?                             
         BNE   CD400               NO: SKIP PUT                                 
         GOTO1 VDATAMGR,DMCB,DMWRT,GENDIR,KEY,KEY                               
         CLI   8(R1),0                                                          
         BNE   CDE07                                                            
         B     CD400                                                            
                                                                                
*----------------------------------------------------------------------         
* ADD/CHANGE COMSCORE CUSTOM DEMO CATEGORY                                      
*----------------------------------------------------------------------         
CD300    CLI   CDCACTN,C'A'        Modify?                                      
         BE    CD310               NO: INVALID ACTION                           
         CLI   CDCACTN,C'R'        Refresh / Full Load?                         
         BNE   CDE08               NO: INVALID ACTION                           
                                                                                
NEW      USING CDCELD,RF                                                        
CD310    LA    RF,ELEM             BUILD ELEMENT FROM MESSAGE                   
         XC    ELEM,ELEM                                                        
         MVI   NEW.CDCEL,CDCELQ    X'0A' CUSTOM DEMO CATEGORY ELEMENT           
         MVI   NEW.CDCLN,CDCLNQ    LENGTH                                       
*                                                                               
         CLI   CDCLOCAL,C'Y'       LOCAL DEMO                                   
         BNE   *+8                                                              
         OI    NEW.CDCFLAG,CDCFLOC X'80' LOCAL DEMO                             
         CLI   CDCNATNL,C'Y'       NATIONAL DEMO                                
         BNE   *+8                                                              
         OI    NEW.CDCFLAG,CDCFNAT X'40' NATIONAL DEMO                          
*                                                                               
         LA    R1,CDCMNUM+L'CDCMNUM-1                                           
         CLI   0(R1),C' '          BUMP PAST SPACE TO SIGNIFICANT DIGIT         
         BH    *+12                                                             
         AHI   R1,-1                                                            
         B     *-12                                                             
         LA    RE,CDCMNUM                                                       
         SR    R1,RE               CALCULATE LENGTH OF NUMBER                   
         BM    CDE09               MINUS LENGTH IS NO GOOD                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,CDCMNUM(0)      DEMO CATEGORY NUMBER                         
         TP    DUB                 IS THIS A VALID DECIMAL NUMBER?              
         BNE   CDE09               NO: THEN ERROR                               
         CVB   R1,DUB              YES: CONVERT                                 
         STCM  R1,15,NEW.CDCNUM    AND STORE                                    
*                                                                               
         MVC   NEW.CDCNAME,CDCMNAME  DEMO CATEGORY NAME                         
         DROP  NEW                                                              
*                                                                               
         GOTO1 VDATAMGR,DMCB,(DMBITS,DMREAD),GENDIR,KEY,KEY                     
         CLI   8(R1),0                                                          
         BE    CD350                                                            
         TM    8(R1),X'02'         MARKED DELETED, need to restore              
         BO    CD330                                                            
         TM    8(R1),X'10'         NOT FOUND, NEED TO add                       
         BO    CD340                                                            
         B     CDE09               DISK ERROR READING FOR LICENSE               
*                                                                               
CD330    NI    CDCDSTAT,X'FF'-X'80' TURN OFF THE DELETE BIT                     
         OI    PINDI,PIRFDEL       RECORD FOUND BUT DELETED                     
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMWRT,GENDIR,KEY,KEY                               
         CLI   8(R1),0                                                          
         BE    CD350                                                            
         B     CDE10               DISK ERROR WRITING DIRECTORY                 
*                                                                               
CD340    L     R3,AIO                                                           
         OI    PINDI,PIRNF         RECORD NOT FOUND, NEED TO ADD                
         XC    CDCKEY(CDCFIRST+CDCLNQ+2),CDCKEY                                 
         MVI   CDCKMIN,CDCKMIQ     C'D' DEMO SYSTEM                             
         MVI   CDCKREC,CDCKREQ     C'C' CUSTOM CATEGORY                         
         MVC   CDCKOXC,CDCOXCD                                                  
         MVC   CDCRLEN,=AL2(CDCFIRST+CDCLNQ+1)                                  
         LA    R4,CDCFIRST(,R3)                                                 
         B     CD370                                                            
*                                                                               
CD350    GOTO1 VDATAMGR,DMCB,(DMBITS,GETREC),GENFIL,CDCDA,AIO,AIOWRK            
         CLI   8(R1),0                                                          
         BNE   CDE11                                                            
*                                                                               
         L     R3,AIO                                                           
         LA    R4,CDCFIRST(,R3)                                                 
CD360    CLI   0(R4),0                                                          
         BE    CD380                                                            
         CLI   0(R4),CDCELQ        X'0A' CUSTOM DEMO CATEGORY ELEMENT           
         BE    CD370                                                            
         LLC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     CD360                                                            
*                                                                               
CD370    GOTO1 VHELLO,DMCB,(C'D',GENFIL),('CDCELQ',AIO),0,0,TO24=YES            
CD380    GOTO1 VHELLO,DMCB,(C'P',GENFIL),AIO,ELEM,0,TO24=YES                    
         CLI   12(R1),0                                                         
         BNE   CDE12               ERROR ADDING ELEMENT                         
         BRAS  RE,SETGACT          SET GENFIL ACTIVITY ELEMENT                  
*                                                                               
         CLI   RWRITE,C'Y'         WRITING TO FILE?                             
         BNE   CD400               NO: SKIP PUT                                 
         NI    CDCRSTAT,X'FF'-X'80' ALWAYS TURN OFF DELETE BIT                  
*                                                                               
         TM    PINDI,PIRNF         NEED TO ADD RECORD?                          
         BZ    CD395               NO: CHANGING                                 
         GOTO1 VDATAMGR,DMCB,ADDREC,GENFIL,DA,AIO,AIOWRK                        
         CLI   8(R1),0                                                          
         BE    CD400                                                            
         B     CDE13               ERROR ADDING RECORD                          
*                                                                               
CD395    GOTO1 VDATAMGR,DMCB,PUTREC,GENFIL,AIO,AIO,AIOWRK                       
         CLI   8(R1),0                                                          
         BNE   CDE14               ERROR UPDATING RECORD                        
                                                                                
*----------------------------------------------------------------------         
* ADVANCE TO NEXT ENTRY                                                         
*----------------------------------------------------------------------         
CD400    A     R2,SVMQELEN         BUMP TO NEXT MESSAGE                         
         B     CD050                                                            
                                                                                
*----------------------------------------------------------------------         
* FINISHED PROCESSING                                                           
*----------------------------------------------------------------------         
CD500    TM    PINDI,PIREFR        FULL REFRESH / LOAD?                         
         BZ    CD510                                                            
         BRAS  RE,CDCREFR          REMOVE UNWANTED RECORDS ON REFRESH           
         BE    CD510                                                            
         BRAS  RE,CLOSECTL         CLOSE THE CONTROL SYSTEM                     
         B     CDERX3                                                           
*                                                                               
CD510    BRAS  RE,CLOSECTL         CLOSE THE CONTROL SYSTEM                     
*                                                                               
         CLI   RWRITE,C'Y'         WRITING TO FILE?                             
         BE    CD520                                                            
         GOTOR PRNT,NOUPDATE       SHOW NO UPDATE IF NOT WRITING                
         B     CD530                                                            
CD520    MVC   PMSG(L'DMHDESC),SVMQHDR DESCRIPTOR                               
         GOTOR PRNT,FINISHED                                                    
*                                                                               
CD530    TM    PINDI,PIELER        ELEMENT ERROR?                               
         JO    CDERX5              YES: EXIT IN ERROR                           
         J     EXITEQ              NO: EXIT EQUAL                               
                                                                                
*----------------------------------------------------------------------         
* MESSAGE ELEMENT ERRORS                                                        
*----------------------------------------------------------------------         
CDE01    MVC   PMSG,=CL44'*E01 Disk error reading Custom Demo record '          
         B     CDE0X                                                            
CDE02    MVC   PMSG,=CL44'*E02 Disk error getting Custom Demo record '          
         B     CDE0X                                                            
CDE03    MVC   PMSG,=CL44'*E03 Unable to find Custom Demo Number     '          
         B     CDE0X                                                            
CDE04    MVC   PMSG,=CL44'*E04 Error deleting Custom Demo record     '          
         B     CDE0X                                                            
CDE05    MVC   PMSG,=CL44'*E05 Error deleting Custom Demo record     '          
         B     CDE0X                                                            
CDE06    MVC   PMSG,=CL44'*E06 Disk error reading Custom Demo record '          
         B     CDE0X                                                            
CDE07    MVC   PMSG,=CL44'*E07 Disk error getting Custom Demo record '          
         B     CDE0X                                                            
CDE08    MVC   PMSG,=CL44'*E08 Invalid action in message             '          
         B     CDE0X                                                            
CDE09    MVC   PMSG,=CL44'*E09 Error calculating Demo Category number'          
         B     CDE0X                                                            
CDE10    MVC   PMSG,=CL44'*E10 Disk error reading Custom Demo record '          
         B     CDE0X                                                            
CDE11    MVC   PMSG,=CL44'*E11 Disk error getting Custom Demo record '          
         B     CDE0X                                                            
CDE12    MVC   PMSG,=CL44'*E12 Unable to add element to Custom Demo  '          
         B     CDE0X                                                            
CDE13    MVC   PMSG,=CL44'*E13 Disk error when adding Custom Demo    '          
         B     CDE0X                                                            
CDE14    MVC   PMSG,=CL44'*E14 Disk error when updating Custom Demo  '          
         B     CDE0X                                                            
CDE0X    GOTOR PRNT,MESSAGESET     PRINT ERROR AND MOVE ON                      
         OI    PINDI,PIELER        INDICATE ELEMENT ERROR                       
         B     CD400                                                            
                                                                                
*----------------------------------------------------------------------         
* MESSAGE ERRORS                                                                
*----------------------------------------------------------------------         
CDERX1   MVC   PMSG,=CL44'*ER31* INVALID ELEMENT LENTH IN MQ MSG     '          
         J     CDERX                                                            
CDERX2   MVC   PMSG,=CL44'*ER32* ERROR OPENING CONTROL SYSTEM        '          
         J     CDERX                                                            
CDERX3   MVC   PMSG,=CL44'*ER33* ERROR DELETING RECORDS ON REFRESH   '          
         J     CDERX                                                            
CDERX5   MVC   PMSG,=CL44'*ER35* ELEMENT ERROR IN CUSTOM DEMO MESSAGE'          
*                                                                               
CDERX    MVC   NOTYMESS,PMSG                                                    
         MVI   NOTYMSG#,NOTYSET                                                 
         GOTOR PRNT,MESSAGESET     ERROR MESSAGE OUTPUT                         
         GOTOR PRNT,NOUPDATE       SHOW NO UPDATE ON ERROR                      
         J     EXITNE                                                           
         DROP  R2,R3,R4,R5                                                      
                                                                                
***********************************************************************         
* UPDATE THE TABLE IN DATA SPACE                                                
***********************************************************************         
UPDTTAB  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   PMSG(L'DMHDESC),SVMQHDR  DESCRIPTOR                              
         GOTOR PRNT,UPDATING                                                    
*                                                                               
         L     R2,AMSGIN           VALIDATE MESSAGE HEADER                      
         USING DEMQHDRD,R2                                                      
*                                                                               
         L     R5,ACMSGTAB         CURRENT TABLE ENTRY                          
         USING MSGTABD,R5                                                       
*                                                                               
         XC    DELIST,DELIST                                                    
         MVC   DELIST(1),MSGTID#   DEMADDR ID#                                  
         MVI   DELIST+4,X'FF'      End of list                                  
                                                                                
UT020    CLC   DMHDTAG,=C'ELEN='   INFO HAS ENTRY LENGTH                        
         BNE   UTNOX               NO                                           
         CLC   DMHDLEN,=C'0000'                                                 
         BL    UTNOX                                                            
         PACK  DUB,DMHDLEN         PICK UP LENGTH                               
         CVB   R3,DUB                                                           
         STCM  R3,15,SVMQELEN                                                   
         DROP  R2                                                               
*                                                                               
         CLI   RWRITE,YES          UPDATE THE DATA SPACE                        
         BNE   UTNUX               NO UPDATE WANTED: EXIT                       
*                                                                               
         L     R2,ADATAOUT         A(MESSAGE DATA)                              
         AHI   R2,-DMTLNQ          SET PRE-MESSAGE INFO FOR DEMADDR             
         USING DEMQTABD,R2                                                      
         MVC   DMTNTRYL,SVMQELEN   ENTRY LENGTH                                 
         ICM   R1,15,DATALEN                                                    
         AHI   R1,-DMHLNQ          SUBTRACT HEADER LENGTH                       
         STCM  R1,15,DMTMESSL      MESSAGE DATA LENGTH                          
         DROP  R2                                                               
*                                                                               
         SAM24                                                                  
         TM    INDI,INFORCE        USE THE FORCE?                               
         BO    UT030               YES                                          
         GOTOR ADEMADDR,DMCB,(X'FF',DELIST),ACOMFACS,ADATAOUT,C'$BLD'           
         B     UT040                                                            
UT030    GOTOR ADEMADDR,DMCB,(X'FE',DELIST),ACOMFACS,ADATAOUT,C'$BLD'           
*                                                                               
UT040    MVC   PMSG(L'DMHDESC),SVMQHDR  DESCRIPTOR                              
         GOTOR PRNT,FINISHED                                                    
*                                                                               
         SAM31                                                                  
         L     R2,AMSGIN            A(MESSAGE)                                  
         MVC   0(DMHLNQ,R2),SVMQHDR RESTORE MESSAGE HEADER                      
*                                                                               
         L     R5,ACMSGTAB                                                      
         OI    MSGTIND,MSGTIDS      INDICATE TABLE HAS BEEN BUILT               
         B     UTOKX                                                            
*                                                                               
UTNUX    GOTOR PRNT,NOUPDATE                                                    
UTOKX    J     EXITEQ                                                           
UTNOX    GOTOR PRNT,NOUPDATE       NO UPDATE DONE                               
         J     EXITNE                                                           
         DROP  R5                                                               
                                                                                
***********************************************************************         
* PROCESSING ROUTINE FOR COMSCORE BATCH JOB RESPONSE MESSAGE                    
***********************************************************************         
SCOBARSP NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   PMSG(L'DMHDESC),SVMQHDR  DESCRIPTOR                              
         GOTOR PRNT,PROCESSBATCH                                                
*                                                                               
         MVC   RSDSN,SPACES                                                     
*                                                                               
         SAM31                                                                  
         L     R2,AMSGIN           CURRENT MESSAGE                              
         USING DEMQHDRD,R2                                                      
*                                                                               
         ICM   R1,15,DATALEN       LENGTH OF MESSAGE                            
         AHI   R1,-L'DMHDESC-1     SUBTRACT DESCRIPTION LENGTH                  
         BNP   SCOBEX1             MUST HAVE A DSN                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RSDSN(0),DMHDSN     DATA SET NAME                                
*                                                                               
         DROP  R2                                                               
         SAM24                                                                  
*                                                                               
         GOTO1 VCBJOB,DMCB,('CBJPOST',RSDSN),0                                  
         JE    EXITEQ                                                           
*                                                                               
         MVC   PMSG,=CL44'*ER12* UNABLE TO POST COMSCORE BATCH JOB'             
         B     SCOBEX                                                           
SCOBEX1  MVC   PMSG,=CL44'*ER13* MISSING DSN FOR BATCH RESPONSE   '             
SCOBEX   MVC   NOTYMESS,PMSG                                                    
         MVI   NOTYMSG#,NOTYSET    SET CUSTOM MESSAGE                           
         GOTOR PRNT,MESSAGESET     ERROR MESSAGE OUTPUT                         
         J     EXITNE                                                           
                                                                                
***********************************************************************         
* PROCESSING ROUTINE FOR COMSCORERSP MESSAGE                                    
***********************************************************************         
SCORERSP NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   PMSG(L'DMHDESC),SVMQHDR  DESCRIPTOR                              
         GOTOR PRNT,UPDATEPQ                                                    
*                                                                               
         MVC   RSDSN,SPACES                                                     
*                                                                               
         SAM31                                                                  
         L     R2,AMSGIN           CURRENT MESSAGE                              
         USING DEMQHDRD,R2                                                      
*                                                                               
         ICM   R1,15,DATALEN       LENGTH OF MESSAGE                            
         AHI   R1,-L'DMHDESC-1     SUBTRACT DESCRIPTION LENGTH                  
         BNP   SCER11              MUST HAVE A DSN                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RSDSN(0),DMHDSN     DATA SET NAME                                
*                                                                               
         DROP  R2                                                               
         SAM24                                                                  
*                                                                               
         MVC   RSORIG,SPACES       INIT ORIGIN ID AND PRINT QUEUE #             
         MVC   PQREPT,SPACES                                                    
*                                                                               
         LA    RE,RSDSN                                                         
         LHI   RF,L'RSDSN                                                       
SC010    CLC   =C'.ID',0(RE)       FIND USER ID NUMBER IN DSN                   
         BE    SC030                                                            
         CLC   =C'.PQ',0(RE)       FIND PQ REPORT NUMBER IN DSN                 
         BE    SC040                                                            
SC020    LA    RE,1(,RE)                                                        
         BCT   RF,SC010                                                         
         B     SCER11              ERROR: DID NOT FINE USER AND PQ              
SC030    MVC   RSORIG,3(RE)        ORIGIN ID                                    
         B     SC020                                                            
SC040    MVC   PQREPT,3(RE)        PQ REPORT NUMBER                             
*                                                                               
         LA    RE,RSORIG                                                        
         LHI   RF,L'RSORIG                                                      
SC050    CLI   0(RE),C'0'          MUST BE 0-9                                  
         BL    SCER11              INVALID ID NUMBER                            
         CLI   0(RE),C'9'                                                       
         BH    SCER11              INVALID ID NUMBER                            
         LA    RE,1(,RE)                                                        
         BCT   RF,SC050                                                         
*                                                                               
         LA    RE,PQREPT                                                        
         LHI   RF,L'PQREPT                                                      
SC060    CLI   0(RE),C'0'          MUST BE 0-9                                  
         BL    SCER11              INVALID PQ REPORT NUMBER                     
         CLI   0(RE),C'9'                                                       
         BH    SCER11              INVALID PQ REPORT NUMBER                     
         LA    RE,1(,RE)                                                        
         BCT   RF,SC060                                                         
*                                                                               
         PACK  DUB,RSORIG                                                       
         CVB   R1,DUB                                                           
         STCM  R1,3,RSORIG#        CONVERT TO ORIGIN ID NUMBER                  
         LTR   R1,R1               MAKE SURE WE HAVE A ORIGIN NUMBER            
         BZ    SCER01                                                           
*                                                                               
         PACK  DUB,PQREPT                                                       
         CVB   R1,DUB                                                           
         STCM  R1,3,PQREPT#        CONVERT TO REPORT NUMBER                     
*                                                                               
         LA    R4,PQX                                                           
         USING UKRECD,R4                                                        
         XC    UKINDEX(UKRECX-UKRECD),UKINDEX CLEAR USER INDEX                  
         MVC   UKSRCID,RSORIG#     SET USER ID                                  
         MVC   UKREPNO,PQREPT#     REPORT NUMBER                                
         OI    UKFLAG,UKFLNUM      FIND ON REPORT # AND GET DATA                
*                                                                               
         MVC   PQF,PQFILE          RESET TO GENERIC                             
         GOTO1 VDATAMGR,DMCB,INDEX,PQF,PQX,PQR,APQB                             
         CLI   DMCB+8,0                                                         
         BE    SC120                                                            
         TM    DMCB+8,X'90'        NOT FOUND                                    
         BO    SCER02                                                           
         B     SCER03                                                           
*                                                                               
SC120    MVI   PQCFLAG,0           INIT PQ CARDS FOUND FLAG                     
*                                                                               
SC130    GOTO1 VDATAMGR,DMCB,READ,PQF,PQX,PQR,APQB                              
         CLI   DMCB+8,X'90'        END OF FILE                                  
         BE    SC200                                                            
         CLI   DMCB+8,0                                                         
         BNE   SCER04                                                           
*                                                                               
         CLC   =C'COMSCORE=1',PQR+1 LOOK FOR COMSCORE PHASE ONE CARD            
         BNE   SC140                                                            
         MVI   PQR+10,C'2'          UPDATE TO COMSCORE PHASE TWO                
         OI    PQCFLAG,PQCFCOM      FOUND COMSCORE CARD                         
         B     SC190                                                            
*                                                                               
SC140    CLC   =C'MVSDSN=',PQR+1    LOOK FOR EMPTY DSN CARD                     
         BNE   SC150                                                            
         TM    PQCFLAG,PQCFCOM      MAKE SURE WE PROCESSED COMSCORE             
         BZ    SCER01                                                           
         MVC   PQR+8(L'RSDSN),RSDSN DSN NAME                                    
         OI    PQCFLAG,PQCFDSN      FOUND DSN CARD                              
         B     SC190                                                            
*                                                                               
SC150    CLC   =C'LOGO=',PQR+1      LOOK FOR LOGO CARD                          
         BNE   SC130                                                            
         LA    R1,PQR                                                           
         LHI   RF,L'PQR                                                         
SC160    CLC   =C'QC=',0(R1)        LOOK FOR QUEUE CLASS IN CARD                
         BNE   SC170                                                            
         MVC   PQJCLASS,3(R1)                                                   
         OI    PQCFLAG,PQCFQC       FOUND QUEUE CLASS CARD                      
SC170    CLC   =C'TY=',0(R1)        LOOK FOR QUEUE TYPE IN CARD                 
         BNE   SC180                                                            
         MVC   PQQTYPE,3(R1)                                                    
         OI    PQCFLAG,PQCFTY       FOUND QUEUE CLASS TYPE CARD                 
SC180    LA    R1,1(,R1)                                                        
         BCT   RF,SC160                                                         
         B     SC130                                                            
*                                                                               
SC190    GOTO1 VDATAMGR,DMCB,WRITE,PQF,PQX,PQR,APQB  UPDATE RECORD              
         CLI   DMCB+8,0                                                         
         BNE   SCER06                                                           
         B     SC130                                                            
*                                                                               
SC200    TM    PQCFLAG,PQCFALL      REQUIRED CARDS IN REPORT?                   
         BNO   SCER05               NO:  THAT'S A PROBLEM                       
*                                                                               
         MVI   WAITCNT,0            CLEAR WAIT COUNT                            
*                                                                               
SC210    GOTO1 VDATAMGR,DMCB,JOUNSET,PQF,PQX,PQR,APQB  RESET JOB OUT            
         CLI   DMCB+8,0                                                         
         BNE   SCER07                                                           
*                                                                               
         GOTO1 VDATAMGR,DMCB,ACTIVATE,PQF,PQX,PQR,APQB  ACTIVATE REPORT         
         CLI   DMCB+8,0                                                         
         BNE   SCER08                                                           
*                                                                               
SC220    BRAS  RE,FINDJOB           FIND REPORT IN DATA SPACE                   
         BE    SCOKX                FOUND AND PROCESSED                         
         BL    SCER09               REPORT NOT FOUND IN DATA SPACE              
*                                                                               
         CLI   WAITCNT,WAITCNTX     MAXIMUM WAIT COUNT                          
         BNL   SCER10               YES: REPORT ERROR                           
*                                                                               
         L     R1,ONESEC            ONE SECOND IN TIME UNITS                    
         MHI   R1,WAITTIMX          NUMBER OF SECONDS TO WAIT                   
         ST    R1,FULL                                                          
         STIMER WAIT,TUINTVL=FULL                                               
*                                                                               
         LLC   R1,WAITCNT           BUMP WAIT COUNT                             
         AHI   R1,1                                                             
         STC   R1,WAITCNT                                                       
         B     SC210                                                            
*                                                                               
SCOKX    J     EXITEQ                                                           
*                                                                               
SCER01   MVC   PMSG,=CL44'*ER01* INVALID CARD IN PQ REPORT        '             
         J     SCERX2                                                           
SCER02   MVC   PMSG,=CL44'*ER02* REPORT NOT FOUND ON PQ           '             
         J     SCERX2                                                           
SCER03   MVC   PMSG,=CL44'*ER03* DISK ERROR ON PQ INDEX CALL      '             
         J     SCERX2                                                           
SCER04   MVC   PMSG,=CL44'*ER04* DISK ERROR ON READ OF PQ         '             
         J     SCERX2                                                           
SCER05   MVC   PMSG,=CL44'*ER05* REPORT ON PQ BUT MISSING VALUES  '             
         J     SCERX2                                                           
SCER06   MVC   PMSG,=CL44'*ER06* DISK ERROR ON WRITE              '             
         J     SCERX2                                                           
SCER07   MVC   PMSG,=CL44'*ER07* ERROR SETTTING REPORT ATTRB      '             
         J     SCERX2                                                           
SCER08   MVC   PMSG,=CL44'*ER08* ERROR ACTIVATING REPORT          '             
         J     SCERX2                                                           
SCER09   MVC   PMSG,=CL44'*ER09* JOB NOT FOUND IN CLASS TABLE     '             
         J     SCERX1                                                           
SCER10   MVC   PMSG,=CL44'*ER10* JOB IN TABLE ENTRY NOT ON HOLD   '             
         J     SCERX2                                                           
SCER11   MVC   PMSG,=CL44'*ER11* INVALID DATA SET NAME IN MESSAGE '             
         J     SCERX2                                                           
SCERX1   OI    INDI,INNOBAD            THIS DOESN'T REQUIRE MSG SAVED           
SCERX2   MVC   NOTYMESS,PMSG                                                    
         MVI   NOTYMSG#,NOTYSET                                                 
         GOTOR PRNT,MESSAGESET         ERROR MESSAGE OUTPUT                     
         J     EXITNE                                                           
SCERX3   MVC   NOTYMESS,PMSG                                                    
         MVI   NOTYMSG#,NOTYSET                                                 
         GOTOR PRNT,MESSAGESET         ERROR MESSAGE AND MARK REPORT            
         GOTO1 VDATAMGR,DMCB,ERROR,PQF,PQX,PQR,APQB  REPORT IN ERROR            
         J     EXITNE                                                           
                                                                                
                                                                                
***********************************************************************         
* Find JOB                                                                      
***********************************************************************         
FINDJOB  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,LOCKCLS                                                       
         BRAS  RE,LOCKJOB                                                       
*                                                                               
         XC    PQJPREV,PQJPREV                                                  
         XC    PQJENTRY,PQJENTRY                                                
*                                                                               
CLS      USING DMSPACED,DSPCTAB                                                 
         L     RF,VSSB                                                          
         LAM   AR2,AR2,SSOTBLET-SSOOFF(RF)                                      
         CPYA  AR3,AR2                                                          
         CPYA  AR4,AR2                                                          
         ICM   R2,15,CLS.DSPTFRST                                               
         ICM   R4,15,CLS.DSPTEND                                                
         SAC   512                                                              
         DROP  CLS                                                              
*                                                                               
         USING JCLASSD,R2                                                       
FJOB08   CLC   JCCLASS,PQJCLASS    MATCH ON JOB CLASS                           
         BNE   FJOB10                                                           
         CLC   JCTYPE,PQQTYPE      QUEUE TYPE                                   
         BE    FJOB12                                                           
FJOB10   AHI   R2,JCLASSL          NEXT CLASS                                   
         CR    R2,R4                                                            
         BL    FJOB08                                                           
         B     FJOBNO              CAN'T FIND CLASS                             
*                                                                               
         USING TBJOBTAB,R3                                                      
FJOB12   ICM   R3,15,JCFRUN        FIRST RUNNING JOB                            
         BZ    FJOBNO              NOT FOUND                                    
*                                                                               
FJOB20   CLC   TBJPQUSR,RSORIG#    MATCH ON USER ID #                           
         BNE   FJOB30                                                           
         CLC   TBJPQSEQ,PQREPT#    MATCH ON PQ REPORT #                         
         BNE   FJOB30                                                           
         B     FJOB40                                                           
*                                                                               
FJOB30   ST    R3,PQJPREV          STORE JOB ENTRY LOCATION                     
         ICM   R3,15,TBJNXT        NEXT REPORT                                  
         BNZ   FJOB20                                                           
         B     FJOBNO              NOT FOUND                                    
*                                                                               
FJOB40   TM    TBJSTAT,TBJHOLD     IS THIS REPORT ON HOLD                       
         BZ    FJOBNOH             NO: THAT IS A PROBLEM                        
         ST    R3,PQJENTRY                                                      
*                                                                               
         CLC   PQJENTRY,JCFRUN     IS THIS THE FIRST ENTRY?                     
         BNE   FJOB50              NO: MOVE ON                                  
         MVC   JCFRUN,TBJNXT       YES: NEXT ENTRY IS NOW FIRST                 
                                                                                
FJOB50   CLC   PQJENTRY,JCLRUN     IS THIS THE LAST ENTRY?                      
         BNE   FJOB55              NO: MOVE ON                                  
         MVC   JCLRUN,PQJPREV      YES: PREVIOUS ENTRY IS NOW LAST              
*                                                                               
PRE      USING TBJOBTAB,R4                                                      
FJOB55   ICM   R4,15,PQJPREV       IS THERE A PREVIOUS JOB?                     
         BZ    FJOB60              NO: NO PREVIOUS TO UPDATE                    
         MVC   PRE.TBJNXT,TBJNXT   YES: ADJUST JOB NEXT POINTER                 
         DROP  PRE                                                              
*                                                                               
FJOB60   XC    TBJNXT,TBJNXT       CLEAR CURRENT ENTRY'S NEXT                   
*                                                                               
         LH    RF,JCNRUN                                                        
         AHI   RF,-1                                                            
         STH   RF,JCNRUN           DECREMENT RUNNING COUNT                      
*                                                                               
         ICM   R3,15,JCFSUB        FIRST SUBMITTED JOB FOR THIS CLASS           
         BNZ   FJOB70                                                           
         MVC   JCFSUB,PQJENTRY     THIS IS THE FIRST JOB IN THE LIST            
         B     FJOB90                                                           
*                                                                               
FJOB70   ICM   R4,15,TBJNXT        IS THIS THE END OF THE CHAIN                 
         BZ    FJOB80              YES: SET POINTER                             
         LR    R3,R4               NO: KEEP GOING DOWN THE CHAIN                
         B     FJOB70                                                           
FJOB80   MVC   TBJNXT,PQJENTRY     NEXT JOB IS OUR JOB                          
*                                                                               
FJOB90   MVC   JCLSUB,PQJENTRY     THIS IS THE LAST JOB IN THE LIST             
*                                                                               
         LH    RF,JCNSUB                                                        
         AHI   RF,1                                                             
         STH   RF,JCNSUB           UPDATE CURRENT SUBMIT COUNT                  
*                                                                               
         LH    RF,JCNTSUB                                                       
         AHI   RF,1                                                             
         STH   RF,JCNTSUB          UPDATE TOTAL SUBMIT COUNT                    
*                                                                               
         ICM   R3,15,PQJENTRY                                                   
         XC    TBJRTIME,TBJRTIME   REMOVE TIME AND RUN AGAIN                    
         NI    TBJSTAT,X'FF'-TBJHOLD  TURN OFF HOLD BIT  O SUBMIT               
         B     FJOBYES                                                          
         DROP  R2,R3                                                            
*                                                                               
FJOBNO   MVI   EXITCC,LO           JOB NOT FOUND                                
         B     FJOBX                                                            
FJOBNOH  MVI   EXITCC,HI           JOB NOT ON HOLD                              
         B     FJOBX                                                            
FJOBYES  MVI   EXITCC,EQ           JOB FOUND AND TAKEN OFF HOLD                 
*                                                                               
FJOBX    BRAS  RE,ARSOFF                                                        
         BRAS  RE,UNLKJOB                                                       
         BRAS  RE,UNLKCLS                                                       
         J     EXITC               SET CONDITION CODE ON EXIT                   
                                                                                
***********************************************************************         
* SET GENFIL ACTIVITY ELEMENT                                                   
***********************************************************************         
SETGACT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 VDATCON,DMCB,(X'05',0),(X'03',DUB)                               
*                                                                               
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING GACTELD,R4                                                       
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',GENFIL),('GACTELQ',AIO),0,0,TO24=YES           
         CLI   12(R1),0                                                         
         BNE   SETGA10                                                          
         ICM   R1,15,16(R1)                                                     
         BZ    SETGA10                                                          
         MVC   GACTEL(GACTLNQ),0(R1)  COPY ELEMENT                              
         B     SETGA20                                                          
*                                                                               
SETGA10  MVI   GACTEL,GACTELQ      ELEMENT CODE                                 
         MVI   GACTLN,GACTLNQ      ELEMENT LENGTH                               
         MVC   GACTADT,DUB         DATE RECORD ADDED (BINARY)                   
*                                                                               
SETGA20  MVC   GACTCDT,DUB         DATE RECORD LAST CHANGED (BINARY)            
         TIME  BIN                 TIME IS ON MY SIDE                           
         STCM  R0,7,GACTTIM        ACTIVITY TIME                                
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',GENFIL),('GACTELQ',AIO),0,0,TO24=YES           
         GOTO1 VHELLO,DMCB,(C'P',GENFIL),AIO,ELEM,0,TO24=YES                    
*                                                                               
         TM    PINDI,PIREFR        IS THIS A FULL REFRESH?                      
         JZ    EXIT                NO: EXIT                                     
         OC    REFRDATE,REFRDATE   DO WE HAVE A START DATE/TIME?                
         JNZ   EXIT                YES: EXIT                                    
         MVC   REFRDATE,GACTCDT    SET REFRESH START DATE                       
         MVC   REFRTIME,GACTTIM    SER REFRESH START TIME                       
         J     EXIT                                                             
         DROP  R4                                                               
                                                                                
***********************************************************************         
* OPEN THE CONTROL SYSTEM                                                       
***********************************************************************         
OPENCTL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    PINDI,PICTOP        CONTROL FILE OPENED?                         
         JO    EXITEQ              YES: EXIT                                    
         OI    PINDI,PICTOP                                                     
*                                                                               
         MVI   GENDIR-1,C'N'                                                    
         MVI   GENFIL-1,C'N'                                                    
         MVI   CTRCVR-1,C'N'                                                    
         MVI   DMBITS,X'08'        READ FOR DELETES                             
*                                                                               
         CLI   RWRITE,C'Y'         WRITING?                                     
         BNE   OPEN010                                                          
*                                                                               
         MVI   GENDIR-1,C'U'                                                    
         MVI   GENFIL-1,C'U'                                                    
         MVI   CTRCVR-1,C'U'                                                    
         OI    DMBITS,X'80'        READ FOR UPDATES                             
*                                                                               
OPEN010  GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,FLIST,AIO                           
         CLI   8(R1),0                                                          
         JNE   EXITNE                                                           
         J     EXITEQ                                                           
                                                                                
***********************************************************************         
* CLOSE THE CONTROL SYSTEM                                                      
***********************************************************************         
CLOSECTL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    PINDI,PICTOP        CONTROL FILE OPENED?                         
         JZ    EXITEQ              NO: NO NEED TO CLOSE                         
         NI    PINDI,X'FF'-PICTOP                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMCLSE,CONTROL,FLIST,AIO                           
*                                                                               
         J     EXITEQ                                                           
                                                                                
***********************************************************************         
* REMOVE UNWANTED LICENSE DEMO CATEGORY RECORDS ON FULL REFRESH                 
***********************************************************************         
LDCREFR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    PINDI,PIREFR        IS THIS A FULL REFRESH?                      
         JZ    EXITEQ              NO: EXIT                                     
*                                                                               
         NI    DMBITS,X'FF'-X'08'  NO NEED TO READ FOR DELETES                  
*                                                                               
         LA    R3,KEY                                                           
         USING LDCRECD,R3                                                       
         XC    LDCKEY,LDCKEY                                                    
         MVI   LDCKMIN,LDCKMIQ     C'D' DEMO SYSTEM                             
         MVI   LDCKREC,LDCKREQ     C'D' LICENSE DEMO CATEGORY                   
*                                                                               
         GOTO1 VDATAMGR,DMCB,(DMBITS,DMRDHI),GENDIR,KEY,KEY                     
         B     LDCR012                                                          
LDCR010  GOTO1 VDATAMGR,DMCB,(DMBITS,DMRSEQ),GENDIR,KEY,KEY                     
LDCR012  CLI   8(R1),0                                                          
         BNE   LDCR500                                                          
*                                                                               
         LA    R3,KEY                                                           
         CLI   LDCKMIN,LDCKMIQ     C'D' DEMO SYSTEM                             
         BNE   LDCR500                                                          
         CLI   LDCKREC,LDCKREQ     C'D' LICENSE DEMO CATEGORY                   
         BNE   LDCR500                                                          
*                                                                               
         GOTO1 VDATAMGR,DMCB,(DMBITS,GETREC),GENFIL,LDCDA,AIO,AIOWRK            
         CLI   8(R1),0                                                          
         BNE   LDCR010                                                          
*                                                                               
         L     R3,AIO                                                           
         LA    R4,LDCFIRST(,R3)                                                 
LDCR040  CLI   0(R4),0                                                          
         BE    LDCR010                                                          
         CLI   0(R4),GACTELQ       X'FE' ACTIVITY ELEMENT                       
         BE    LDCR060                                                          
         LLC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     LDCR040                                                          
*                                                                               
         USING GACTELD,R4                                                       
LDCR060  CLC   REFRDATE,GACTCDT    REFRESH START DATE                           
         BL    LDCR010             ACTIVITY IS AFTER REFRESH                    
         BH    LDCR100             ACTIVITY IS BEFORE REFRESH                   
         CLC   REFRTIME,GACTTIM    REFRESH START TIME                           
         BNH   LDCR010             ACTIVITY IS SAME OR AFTER REFRESH            
*                                                                               
LDCR100  CLI   RWRITE,C'Y'         WRITING TO FILE?                             
         BNE   LDCR010             NO: SKIP PUT                                 
*                                                                               
         OI    LDCRSTAT,X'80'      DELETE RECORD                                
         GOTO1 VDATAMGR,DMCB,PUTREC,GENFIL,AIO,AIO,AIOWRK                       
         CLI   8(R1),0                                                          
         BNE   LDCRERX                                                          
*                                                                               
         LA    R3,KEY                                                           
         OI    LDCDSTAT,X'80'      DELETE RECORD                                
         GOTO1 VDATAMGR,DMCB,DMWRT,GENDIR,KEY,KEY                               
         CLI   8(R1),0                                                          
         BNE   LDCRERX                                                          
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMREAD,GENDIR,KEY,KEY                              
         CLI   8(R1),0                                                          
         BNE   LDCRERX                                                          
         J     LDCR010                                                          
*                                                                               
LDCR500  J     EXITEQ                                                           
LDCRERX  J     EXITNE                                                           
         DROP  R3,R4                                                            
                                                                                
***********************************************************************         
* REMOVE UNWANTED CUSTOM DEMO CATEGORY RECORDS ON FULL REFRESH                  
***********************************************************************         
CDCREFR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    PINDI,PIREFR        IS THIS A FULL REFRESH?                      
         JZ    EXITEQ              NO: EXIT                                     
*                                                                               
         NI    DMBITS,X'FF'-X'08'  NO NEED TO READ FOR DELETES                  
*                                                                               
         LA    R3,KEY                                                           
         USING CDCRECD,R3                                                       
         XC    CDCKEY,CDCKEY                                                    
         MVI   CDCKMIN,CDCKMIQ     C'D' DEMO SYSTEM                             
         MVI   CDCKREC,CDCKREQ     C'C' CUSTOM CATEGORY                         
*                                                                               
         GOTO1 VDATAMGR,DMCB,(DMBITS,DMRDHI),GENDIR,KEY,KEY                     
         B     CDCR012                                                          
CDCR010  GOTO1 VDATAMGR,DMCB,(DMBITS,DMRSEQ),GENDIR,KEY,KEY                     
CDCR012  CLI   8(R1),0                                                          
         BNE   CDCR500                                                          
*                                                                               
         LA    R3,KEY                                                           
         CLI   CDCKMIN,CDCKMIQ     C'D' DEMO SYSTEM                             
         BNE   CDCR500                                                          
         CLI   CDCKREC,CDCKREQ     C'C' CUSTOM CATEGORY                         
         BNE   CDCR500                                                          
*                                                                               
         GOTO1 VDATAMGR,DMCB,(DMBITS,GETREC),GENFIL,CDCDA,AIO,AIOWRK            
         CLI   8(R1),0                                                          
         BNE   CDCR010                                                          
*                                                                               
         L     R3,AIO                                                           
         LA    R4,CDCFIRST(,R3)                                                 
CDCR040  CLI   0(R4),0                                                          
         BE    CDCR010                                                          
         CLI   0(R4),GACTELQ       X'FE' ACTIVITY ELEMENT                       
         BE    CDCR060                                                          
         LLC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     CDCR040                                                          
*                                                                               
         USING GACTELD,R4                                                       
CDCR060  CLC   REFRDATE,GACTCDT    REFRESH START DATE                           
         BL    CDCR010             ACTIVITY IS AFTER REFRESH                    
         BH    CDCR100             ACTIVITY IS BEFORE REFRESH                   
         CLC   REFRTIME,GACTTIM    REFRESH START TIME                           
         BNH   CDCR010             ACTIVITY IS SAME OR AFTER REFRESH            
*                                                                               
CDCR100  CLI   RWRITE,C'Y'         WRITING TO FILE?                             
         BNE   CDCR010             NO: SKIP PUT                                 
*                                                                               
         OI    CDCRSTAT,X'80'      DELETE RECORD                                
         GOTO1 VDATAMGR,DMCB,PUTREC,GENFIL,AIO,AIO,AIOWRK                       
         CLI   8(R1),0                                                          
         BNE   CDCRERX                                                          
*                                                                               
         LA    R3,KEY                                                           
         OI    CDCDSTAT,X'80'      DELETE RECORD                                
         GOTO1 VDATAMGR,DMCB,DMWRT,GENDIR,KEY,KEY                               
         CLI   8(R1),0                                                          
         BNE   CDCRERX                                                          
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMREAD,GENDIR,KEY,KEY                              
         CLI   8(R1),0                                                          
         BNE   CDCRERX                                                          
         J     CDCR010                                                          
*                                                                               
CDCR500  J     EXITEQ                                                           
CDCRERX  J     EXITNE                                                           
         DROP  R3,R4                                                            
                                                                                
***********************************************************************         
* VALIDATE CARDS                                                                
***********************************************************************         
VALCARDS NTR1  BASE=*,LABEL=*                                                   
         MVI   EXITCC,EQ                                                        
*                                                                               
         CLI   CARDPARM,CARDPASS   PROGRAM PASSING CARD                         
         BE    VC015               YES: THEN NOT READING                        
VC010    CLI   CARDPARM,CARDPASS   PROGRAM PASSING CARD                         
         BE    VC100               YES: THEN NOT READING                        
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         MVC   P(L'CARD),CARD                                                   
         GOTO1 VPRINTER                                                         
         CLC   =C'/*',CARD         END OF DECK                                  
         BE    VC100                                                            
         CLC   =C'XX',CARD         END OF DECK                                  
         BE    VC100                                                            
*                                                                               
VC015    LA    R2,CARD                                                          
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    VC010                                                            
*                                                                               
VC020    LA    R4,CARDTAB                                                       
         CLI   CARDPARM,CARDPASS   PROGRAM PASSED CARD                          
         BNE   VC021               NO: THEN SET WITH CARDTAB                    
         LA    R4,CARDPST          YES: USE PROGRAM PASS TABLE                  
VC021    LLC   R1,7(R4)            GET LEN FOR COMPARE                          
         BCTR  R1,0                                                             
         EX    R1,*+8              EXECUTE KEYWORD TEST                         
         B     *+10                                                             
         CLC   0(0,R2),0(R4)                                                    
         BE    VC030                                                            
         LA    R4,14(R4)           TRY NEXT ENTRY                               
         CLI   0(R4),0                                                          
         BNE   VC021                                                            
         B     VCEIK               ERROR INVALID KEYWORD                        
*                                                                               
VC030    LA    R2,1(R2,R1)         POINT TO DELIMITER                           
         CLI   0(R2),C'='          VALID DELIMITER                              
         BNE   VCEID                                                            
         LA    R2,1(,R2)                                                        
*                                                                               
         LR    R1,R2               GET LEN FOR MOVE                             
VC040    TM    9(R4),X'10'         ONLY ONE PARAMETER IN CARD                   
         BO    *+12                YES: ALLOW COMMAS                            
         CLI   0(R1),C','                                                       
         BE    VC050                                                            
         CLI   0(R1),C' '                                                       
         BE    VC050                                                            
         CLI   0(R1),0                                                          
         BE    VC050                                                            
         LA    R1,1(R1)                                                         
         B     VC040                                                            
*                                                                               
VC050    SR    R1,R2               ANYTHING THERE?                              
         BZ    VCEMP               NO                                           
         OI    10(R4),X'01'        MARK PARAMETER ENTERED                       
         BCTR  R1,0                                                             
         SR    RF,RF                                                            
         ICM   RF,7,11(R4)         GET ADDRESS FOR MOVE                         
         TM    9(R4),X'80'         IF ROUTINE                                   
         BZ    VC060                                                            
         BASR  RE,RF               GOTO ROUTINE                                 
         B     VC080                                                            
*                                                                               
VC060    CLM   R1,1,8(R4)          CHECK MAX LEN                                
         BNL   VCETL                                                            
         LLC   RE,8(R4)            PAD OUT TO SPACES                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),SPACES                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R2)       MOVE TO OUTPUT AREA                          
*                                                                               
VC080    TM    9(R4),X'10'         ONLY ONE PARAMETER IN CARD                   
         BO    VC010               YES: NEXT CARD                               
         LA    R1,CARD+L'CARD-1                                                 
VC081    CLI   0(R2),C','          TEST FOR ANOTHER                             
         LA    R2,1(R2)                                                         
         BE    VC020               GO FIND TABLE ENTRY                          
         CR    R2,R1               TEST FOR END OF CARD                         
         BL    VC081                                                            
         B     VC010               NEXT CARD                                    
*                                                                               
VC100    LA    R2,CARDTAB          PROGRAM PASSED CARD                          
         CLI   CARDPARM,CARDPASS   NO: THEN SET WITH CARDTAB                    
         BNE   VC101               YES: USE PROGRAM PASS TABLE                  
         LA    R4,CARDPST                                                       
VC101    TM    10(R2),X'02'        REQUIRED                                     
         BZ    *+12                                                             
         TM    10(R2),X'01'        PROVIDED                                     
         BZ    VCEMR                                                            
         LA    R2,14(R2)           TRY NEXT ENTRY                               
         CLI   0(R2),0                                                          
         BNE   VC101                                                            
*                                                                               
         L     RF,VSSB             SET DATA SPACE                               
         MVC   SSODSPAC-SSOOFF(1,RF),DSPACE                                     
*                                                                               
         L     RF,=V(DDSIO)        SET DDSIO                                    
         MVC   0(8,RF),DDSIO                                                    
*                                                                               
         CLC   QMGR,SPACES         WAS QMGR SET BY PARM?                        
         BH    VC200               YES: CONTINUE                                
         LA    RF,QMGRTAB          NO: GET IT FROM TABLE                        
VC110    CLI   0(RF),X'FF'         END OF TABLE                                 
         BE    VC120                                                            
         CLC   DSPACE,0(RF)        MATCH WITH DATA SPACE                        
         BE    VC120               YES: GET QUEUE MANAGER                       
         LA    RF,L'QMGRTAB(,RF)                                                
         B     VC110                                                            
VC120    MVC   QMGR(L'QMGRTAB-1),1(RF)                                          
*                                                                               
VC200    J     EXITC               EXIT AND CHECK CONDITION CODE                
                                                                                
VCEIK    MVC   PMSG(20),=CL20'Invalid Keyword'                                  
         J     VCER                                                             
VCEID    MVC   PMSG(20),=CL20'Invalid Delimeter'                                
         J     VCER                                                             
VCEMP    MVC   PMSG(20),=CL20'Missing Parameter'                                
         J     VCER                                                             
VCETL    MVC   PMSG(20),=CL20'Parameter Too Long'                               
         J     VCER                                                             
VCEMR    MVC   PMSG(20),=CL20'Missing Card'                                     
         J     VCER                                                             
VCER     MVC   P+50(30),0(R2)      BAD PARAMETER                                
         GOTOR PRNT,MESSAGESET                                                  
         MVI   EXITCC,NE           SET EXIT CONDITION CODE                      
         J     EXITNE                                                           
                                                                                
*----------------------------------------------------------------------         
* CARD TABLE                                                                    
*----------------------------------------------------------------------         
* CL7'KEYWORD',AL1(KEYWRD LEN,OP LEN),X'FLAGS',AL3(OUTPUT)                      
* FLAGS  X'8000'  A(OUTPUT) IS A(ROUTINE)                                       
*        X'1000'  ONLY PARAMETER IN CARD, COMMAS ALLOWED                        
*        X'0002'  VALUE IS REQUIRED                                             
*        X'0001'  PARAMETER PROVIDED                                            
*----------------------------------------------------------------------         
CARDTAB  DS    0F            +7 +8    +9+10 +11                                 
         DC    C'DDSIO  ',AL1(5,10),X'0000',AL3(DDSIO)                          
         DC    C'DSPACE ',AL1(6,01),X'0002',AL3(DSPACE)                         
         DC    C'WRITE  ',AL1(5,03),X'0000',AL3(RWRITE)                         
         DC    C'UPDATE ',AL1(6,03),X'0000',AL3(RWRITE)                         
         DC    C'QMGR   ',AL1(4,48),X'0000',AL3(QMGR)                           
         DC    C'QUEUE  ',AL1(5,48),X'0000',AL3(QQ)                             
         DC    C'BACKQ  ',AL1(5,48),X'0000',AL3(KQ)                             
         DC    C'BADMQ  ',AL1(5,48),X'0000',AL3(DQ)                             
         DC    C'NOTIFY ',AL1(6,60),X'1000',AL3(NOTYLIST)                       
         DC    C'DEMADDR',AL1(7,08),X'0000',AL3(DEMADDR)                        
         DC    X'0000'                                                          
*                                                                               
CARDPST  DS    0F                                                               
         DC    X'0000'                                                          
                                                                                
***********************************************************************         
* EMAIL NOTIFICATION                                                            
* ON ENTRY - R1 = NOTIFICATION ENTRY NUMBER                                     
***********************************************************************         
NOTIFY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   MSGWTXT,SPACES                                                   
         MVC   MSGWTXT(9),=C'AUTONOTE*'                                         
         MVC   MSGWTXT+9(L'NOTYLIST),NOTYLIST                                   
*                                                                               
         LA    R2,MSGWTXT+9+L'NOTYLIST-1                                        
NM010    CLI   0(R2),C' '                                                       
         BH    NM020                                                            
         BCT   R2,NM010                                                         
*                                                                               
NM020    MVI   1(R2),C':'          PLACE COLON AT END OF RECIPIENTS             
         LA    R2,2(,R2)                                                        
*                                                                               
         MVC   0(10,R2),=CL10'*WARNING*'                                        
         LA    R2,10(,R2)                                                       
*                                                                               
         CHI   R1,NOTYSET          NOTIFICATION MESSAGE SET                     
         BNE   NM025               NO                                           
         CLC   NOTYMESS,SPACES     DID WE SET THE MESSAGE?                      
         BNH   NM022               YES: CONTINUE                                
         LA    R3,NOTYMESS         POINT TO CUSTUM MESSAGE FIELD                
         B     NM026                                                            
NM022    LHI   R1,NOBUMESS         NO: RESET TO GENERIC                         
*                                                                               
NM025    MHI   R1,L'NOTYTAB        ADD NOTIFICATION MESSAGE                     
         LA    R3,NOTYTAB                                                       
         AR    R3,R1                                                            
NM026    MVC   0(L'NOTYTAB,R2),0(R3)                                            
*                                                                               
         LA    R2,L'NOTYTAB-1(,R2)                                              
NM030    CLI   0(R2),C' '                                                       
         BH    NM040                                                            
         BCT   R2,NM030                                                         
*                                                                               
NM040    LA    R2,2(,R2)                                                        
         MVC   0(L'DMHDESC,R2),SVMQHDR   MESSAGE DESCRIPTOR                     
         LA    R2,L'DMHDESC(,R2)                                                
*                                                                               
         LA    R1,MSGW                                                          
         SR    R2,R1                                                            
         STCM  R2,3,MSGWLEN                                                     
         WTO   TEXT=MSGW                                                        
*                                                                               
         MVC   NOTYMESS,SPACES     RESET CUSTOM MESSAGE                         
         J     EXIT                                                             
                                                                                
*----------------------------------------------------------------------         
* NOTIFICATION MESSAGE TABLE                                                    
*----------------------------------------------------------------------         
NOBACKUP EQU 0               . MISSING MESSAGE IN BACKUP QUEUE                  
NOBUMPTY EQU 1               . NO MESSAGE FOUND FOR TABLE BUILD                 
NOBUMESS EQU 2               . BAD MESSAGE IN INPUT QUEUE                       
NOBBMESS EQU 3               . BAD MESSAGE IN BACKUP QUEUE                      
NOTYSET  EQU 255             . CUSTOM MESSAGE HAS ALREADY BEEN SET              
*                                                                               
NOTYTAB  DC    CL44'Backup Queue missing message'                               
         DC    CL44'Build Failed no message in backup or input'                 
         DC    CL44'Input Queue bad message'                                    
         DC    CL44'Backup Queue bad message'                                   
                                                                                
***********************************************************************         
* PRINT LOG                                                                     
*    ON ENTRY - R1 = LOG ENTRY NUMBER                                           
***********************************************************************         
PRNT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CHI   R1,MESSAGESET       MESSAGE MANUALLY SET                         
         BE    PRNT010                                                          
         MHI   R1,L'LOGTAB                                                      
         LA    R2,LOGTAB                                                        
         AR    R2,R1                                                            
         MVC   P(L'LOGTAB),0(R2)   PRINT DESCRIPTION                            
*                                                                               
PRNT010  TIME  DEC                 PRINT TIME                                   
         ST    R0,DOUBLE                                                        
         MVI   DOUBLE+4,X'0F'                                                   
         UNPK  PRNTTIME,DOUBLE(5)                                               
         MVC   P+18(2),PRNTTIME                                                 
         MVI   P+20,C':'                                                        
         MVC   P+21(2),PRNTTIME+2                                               
         MVI   P+23,C':'                                                        
         MVC   P+24(2),PRNTTIME+4                                               
         MVI   P+26,C'.'                                                        
         MVC   P+27(2),PRNTTIME+6                                               
         GOTO1 VPRINTER                                                         
         MVC   P,SPACES                                                         
         J     EXIT                                                             
                                                                                
*----------------------------------------------------------------------         
* JOB LOG MESSAGE TABLE                                                         
*----------------------------------------------------------------------         
INITIALIZE     EQU 0                                                            
ABOUTTOWAIT    EQU 1                                                            
WAITCOMPLETE   EQU 2                                                            
EXITING        EQU 3                                                            
UPDATING       EQU 4                                                            
FINISHED       EQU 5                                                            
NOUPDATE       EQU 6                                                            
NOMESSAGE      EQU 7                                                            
OPERATOR       EQU 8                                                            
OPERSTOP       EQU 9                                                            
OPERBUILD      EQU 10                                                           
OPERMODIFY     EQU 11                                                           
OPERCANCEL     EQU 12                                                           
PROCESSMESS    EQU 13                                                           
UPDATEPQ       EQU 14                                                           
PROCESSBATCH   EQU 15                                                           
UPDATERECS     EQU 16                                                           
MESSAGESET     EQU 255                                                          
*                                                                               
LOGTAB   DC    CL17'Initialize'       0                                         
         DC    CL17'AboutToWait'      1                                         
         DC    CL17'WaitComplete'     2                                         
         DC    CL17'Exiting'          3                                         
         DC    CL17'UpdatingTable'    4                                         
         DC    CL17'UpdateComplete'   5                                         
         DC    CL17'NoUpdate'         6                                         
         DC    CL17'NoMessageFound'   7                                         
         DC    CL17'OperatorCommand'  8                                         
         DC    CL17'OperatorStop'     9                                         
         DC    CL17'OperatorBuild'    10                                        
         DC    CL17'OperatorModify'   11                                        
         DC    CL17'OperatorCancel'   12                                        
         DC    CL17'ProcessMessage'   13                                        
         DC    CL17'UpdatingReport'   14                                        
         DC    CL17'ProcessingBatch'  15                                        
         DC    CL17'UpdatingRecords'  16                                        
                                                                                
***********************************************************************         
* LOCK/UNLOCK DATA SPACE TABLES / CLEAR ACCESS REGISTERS                        
***********************************************************************         
LOCKJOB  NTR1  BASE=*,LABEL=*                                                   
         CLI   LOCKEDJB,YES        Job table locked?                            
         JE    EXIT                Yes                                          
         XC    DUB,DUB             JOB TABLE                                    
         LAY   RF,DTJOB            X'8005'                                      
         ST    RF,DUB                                                           
         GOTOR VLOCKSPC,DUB                                                     
         ICM   RF,15,4(R1)         LOCK JOB TABLE                               
         MVI   LOCKEDJB,YES                                                     
         J     EXIT                                                             
*                                                                               
UNLKJOB  NTR1  BASE=*,LABEL=*                                                   
         CLI   LOCKEDJB,NO         Locked ?                                     
         JE    EXIT                Not locked                                   
         XC    DUB,DUB             JOB TABLE                                    
         LAY   RF,DTJOB            X'8005'                                      
         ST    RF,DUB                                                           
         OI    DUB,X'10'                                                        
         GOTOR VLOCKSPC,DUB                                                     
         MVI   LOCKEDJB,NO         Set to not locked                            
         J     EXIT                                                             
*                                                                               
LOCKCLS  NTR1  BASE=*,LABEL=*                                                   
         CLI   LOCKEDCL,YES        Class table locked?                          
         JE    EXIT                YES                                          
         XC    DUB,DUB             CLASS TABLE                                  
         LAY   RF,DTDCLASS         X'8027'                                      
         ST    RF,DUB                                                           
         GOTOR VLOCKSPC,DUB                                                     
         ICM   RF,15,4(R1)         LOCK CLASS TABLE                             
         MVI   LOCKEDCL,YES                                                     
         J     EXIT                                                             
*                                                                               
UNLKCLS  NTR1  BASE=*,LABEL=*                                                   
         CLI   LOCKEDCL,NO         Locked class table?                          
         JE    EXIT                No, so don't unlock                          
         XC    DUB,DUB             CLASS TABLE                                  
         LAY   RF,DTDCLASS         X'8027'                                      
         ST    RF,DUB                                                           
         OI    DUB,X'10'                                                        
         GOTOR VLOCKSPC,DUB                                                     
         MVI   LOCKEDCL,NO         Unlocked                                     
         J     EXIT                                                             
*                                                                               
ARSOFF   SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         BR    RE                                                               
                                                                                
***********************************************************************         
* COMMON EXITS                                                                  
***********************************************************************         
EXITLO   MVI   EXITCC,LO                                                        
         J     EXITC                                                            
EXITNE   MVI   EXITCC,NE                                                        
         J     EXITC                                                            
EXITHI   MVI   EXITCC,HI                                                        
         J     EXITC                                                            
EXITEQ   MVI   EXITCC,EQ                                                        
         J     EXITC                                                            
EXITC    CLI   EXITCC,EQ                                                        
EXIT     XIT1                                                                   
*                                                                               
LO       EQU   0                                                                
NE       EQU   0                                                                
EQ       EQU   1                                                                
HI       EQU   2                                                                
                                                                                
***********************************************************************         
* COMMON STORAGE AREA                                                           
***********************************************************************         
COMMON   DS    0D                                                               
*                                                                               
DMCB     DS    8F                                                               
DUB      DS    D                                                                
DOUBLE   DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
DMBITS   DS    X                                                                
CARD     DS    CL80                                                             
WORK     DS    CL256                                                            
*                                                                               
DA       DC    F'0'                DISK ADDRESS                                 
KEY      DS    CL48                KEY                                          
ELEM     DS    CL256               ELEMENT                                      
*                                                                               
DSPACE   DC    C' '                DATA SPACE                                   
DDSIO    DC    CL10'DDSIO'         DDSIO VERSION                                
RWRITE   DC    CL3'YES'            UPDATE DATA SPACE / WRITE TO FILE            
QMGR     DC    CL48' '             MQ QUEUE MANAGER                             
QQ       DC    CL48' '             INPUT MESSAGE MQ QUEUE                       
KQ       DC    CL48' '             BACKUP MQ QUEUE                              
DQ       DC    CL48' '             BAD MESSAGE QUEUE                            
NOTYLIST DC    CL60'US-MF_FAC_NOTIFY'          EMAIL LIST                       
*                                                                               
EXITCC   DC    XL1'00'                                                          
*                                                                               
WAITCNT  DC    XL1'00'                                                          
WAITCNTX EQU   3                   WAIT 3 TIMES                                 
WAITTIMX EQU   1                   1 SECOND                                     
*                                                                               
PRNTTIME DS    CL9                                                              
*                                                                               
REFRDATE DS    CL3                 DATE OF FULL LOAD                            
REFRTIME DS    CL3                 TIME OF FULL LOAD                            
*                                                                               
PQF      DC    CL8'PRTQUE'         PRINT QUEUE FILE                             
PQX      DC    XL80'00'            PRINT QUEUE INDEX                            
PQRL     DC    XL4'00'             PRINT QUEUE RECORD LENGTH                    
PQR      DC    CL166' '            PRINT QUEUE RECORD                           
*                                                                               
PQREPT   DC    CL6' '              PQ REPORT NUMBER (TEXT)                      
PQREPT#  DC    XL2'00'             PQ REPORT #                                  
PQQTYPE  DC    CL1' '              QUEUE TYPE                                   
PQJCLASS DC    CL2' '              JOB CLASS                                    
PQCFLAG  DC    XL1'00'             PQ JCL CARD FLAG                             
PQCFCOM  EQU   X'01'               . COMSCORE= CARD FOUND                       
PQCFDSN  EQU   X'02'               . MVSDSN= CARD FOUND                         
PQCFQC   EQU   X'04'               . QC= JOB CLASS CARD FOUND                   
PQCFTY   EQU   X'08'               . TY= QUEUE TYPE CARD FOUND                  
PQCFALL  EQU   X'0F'               . ALL PQ CARD FLAGS                          
*                                                                               
PQJPREV  DS    A                   PREVIOUS JOB ENTRY                           
PQJENTRY DS    A                   CURRENT JOB ENTRY                            
*                                                                               
MESSEND  DS    A                   END OF CURRENT MESSAGE                       
*                                                                               
RSORIG   DC    CL6' '              RESPONSE USER ID NUMBER (TEXT)               
RSORIG#  DC    XL2'00'             RESPONSE USER ID #                           
RSJOBN   DC    CL5' '              RESPONSE JOB NUMBER (TEXT)                   
RSJOB#   DC    XL2'00'             RESPONSE JOB #                               
RSDSN    DC    CL44' '             RESPONSE DSN TO BE ADDED TO JCL              
*                                                                               
SVMQLEN  DS    F                   SAVED MESSAGE LENGTH                         
SVMQELEN DS    F                   SAVED MESSAGE ENTRY LENGTH                   
SVMQHDR  DS    CL(DMHLNQ)          SAVED MESSAGE HEADER                         
SVHDTAG  DS    CL(L'DMHDTAG)       SAVED MESSAGE HEADER                         
*                                                                               
DELIST   DC    8XL4'00'            DEMADDR TABLE BUILD LIST                     
         DC    X'FF'               MAX END OF LIST                              
*                                                                               
INDI     DC    X'00'                                                            
INBUILD  EQU   X'80'               . BUILD TABLE ON START OF JOB                
INFORCE  EQU   X'40'               . FORCE TABLE BUILD                          
INBADMS  EQU   X'20'               . BAD MESSAGE FOUND                          
INNOBAD  EQU   X'10'               . DO NOT PUT TO BAD MESSAGE QUEUE            
INNOLOG  EQU   X'02'               . NO LOG MESSAGE WANTED                      
INNOMSG  EQU   X'01'               . NO MESSAGE FOUND                           
SVINDI   DC    X'00'               . SAVED INDICATOR                            
*                                                                               
PINDI    DC    X'00'               PROGRAM INDICATOR                            
PICTOP   EQU   X'80'               . CONTROL SYSTEM OPENED                      
PIRNF    EQU   X'40'               . RECORD NOT FOUND                           
PIRFDEL  EQU   X'20'               . RECORD FOUND BUT DELETED                   
PIELER   EQU   X'10'               . MESSAGE ELEMENT ERROR                      
PIREFR   EQU   X'01'               . REFRESH / FULL LOAD                        
*                                                                               
CARDPARM DC    C'J'                INITALIZE READING CARDS FROM JCL             
CARDJCL  EQU   C'J'                . CARD FROM JCL DECK                         
CARDPASS EQU   C'P'                . PASSING A CARD TO VALCARDS                 
*                                                                               
ECBLST   DS    0F                                                               
         DC    X'00',AL3(INPQECB)  A(INPUT QUEUE ECB)                           
AOPERECB DC    X'80',AL3(0)        A(STOPECB)                                   
INPQECB  DC    F'0'                ECB FOR MQ GET FROM INPUT QUEUE              
OPSTOP   DC    AL1(NO)             'Y' IF OPERATOR WANTS TO STOP                
OPBUILD  DC    AL1(NO)             'Y' IF OPERATOR WANTS TO BUILD               
*                                                                               
LOCKEDCL DC    AL1(NO)             CLASS TABLE LOCKED?                          
LOCKEDJB DC    AL1(NO)             JOB TABLE LOCKED?                            
*                                                                               
         DS    0F                                                               
DSPJTAB  DC    XL64'00'            JOBS TABLE IN DATA SPACE                     
DSPCTAB  DC    XL64'00'            CLASS TABLE IN DATA SPACE                    
*                                                                               
ACOMM    DC    A(0)                A(COMMUNICATIONS PARAMETER LIST)             
AMSGIN   DC    A(0)                A(MESSAGE)                                   
AMSGINX  DC    A(0)                A(END OF MESSAGE)                            
ADATAOUT DC    A(0)                A(DATA TO SEND TO DEMADDR)                   
ASMSGTAB DC    A(0)                A(START OF MESSAGE TABLE)                    
ACMSGTAB DC    A(0)                A(CURRENT MESSAGE TABLE ENTRY)               
*                                                                               
MSGW     DS    0CL134              WTO OUTPUT MESSAGE                           
MSGWLEN  DS    XL2                                                              
MSGWTXT  DS    CL132                                                            
*                                                                               
NOTYMSG# DS    X                   MESSAGE                                      
NOTYMESS DC    CL44' '             CUSTOM MESSAGE                               
*                                                                               
LICCODE  DS    CL32                LICENSE CODE                                 
LICSEQ   DS    XL2                 LICENSE SEQUENCE NUMBER                      
LICCAT   DS    F                   CATEGORY NUMBER                              
*                                                                               
FLIST    DS    0CL8                                                             
         DC    CL1'N'                                                           
CTFILE   DC    CL7'CTFILE'                                                      
         DC    CL1'N'                                                           
GENDIR   DC    CL7'GENDIR'                                                      
         DC    CL1'N'                                                           
GENFIL   DC    CL7'GENFIL'                                                      
         DC    CL1'N'                                                           
CTRCVR   DC    CL7'CTRCVR'                                                      
         DC    CL1'X'                                                           
         DC    CL7'XXXXXX'                                                      
                                                                                
***********************************************************************         
* LITERALS AND CONSTANTS                                                        
***********************************************************************         
LITCON   DS    0D                                                               
*                                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
EOT      EQU   X'FF'                                                            
*                                                                               
         DS    0D                                                               
VDATAMGR DC    V(DATAMGR)                                                       
VLOCKSPC DC    V(LOCKSPC)                                                       
VPRINTER DC    V(PRINTER)                                                       
VLOADER  DC    V(LOADER)                                                        
VHELLO   DC    V(HELLO)                                                         
VDATCON  DC    V(DATCON)                                                        
VCBJOB   DC    V(CBJOB)                                                         
*                                                                               
ACOMFACS DC    A(COMFACS)                                                       
ADEMADDR DC    A(0)                                                             
AIO      DC    A(IO)                                                            
AIOWRK   DC    A(IOWRK)                                                         
APQB     DC    A(PQB)                                                           
VSSB     DC    A(SSB)                                                           
*                                                                               
ARZERO   DC    16F'0'              USED TO CLEAR ACCESS REGISTERS               
*                                                                               
DEMADDR  DC    CL8'T00ADE'         DEMADDR PHASE                                
ONESEC   DC    F'38400'            ONE SECOND IN TIME UNITS                     
*                                                                               
         LTORG                                                                  
*                                                                               
CONTROL  DC    CL8'CONTROL'                                                     
DMOPEN   DC    CL8'DMOPEN'                                                      
DMCLSE   DC    CL8'DMCLSE'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
DMADD    DC    CL8'DMADD'                                                       
GETREC   DC    CL8'GETREC'                                                      
ADDREC   DC    CL8'ADDREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
BOOKMARK DC    CL5'BKMRK'          BOOKMARK STRING                              
PQFILE   DC    CL8'PRTQUE'         GENERIC USER BASED PRINT QUEUE               
ACTIVATE DC    CL3'ACT'            DMPRTQ ACTIONS                               
ERROR    DC    CL3'ERR'                                                         
INDEX    DC    CL3'IND'                                                         
JOUNSET  DC    CL3'JOU'                                                         
READ     DC    CL3'REA'                                                         
WRITE    DC    CL3'WRI'                                                         
ISGENQ   DC    CL7'ISGENQ'         DMISGENQ                                     
*                                                                               
QMGRTAB  DS    0CL5                QUEUE MANAGER TABLE                          
         DC    C'C',CL4'MQ7C'      CSC                                          
         DC    C'Q',CL4'MQ7Q'      QA                                           
         DC    C'T',CL4'MQ7T'      TST                                          
         DC    X'FF',CL4'MQ1P'     PRODUCTION (DEFAULT)                         
*                                                                               
         DS    0D                                                               
         DC    CL16'*SSB*SSB*SSB*SSB'                                           
       ++INCLUDE FASSBOFF                                                       
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
*        ORG   SSOSTAT2                                                         
*        DC    AL1(SSOSROLC)       OFFLINE RECOVERY COPIES                      
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
*                                                                               
         DS    0D                                                               
         DC    CL16'*UTL*UTL*UTL*UTL'                                           
UTL      DC    F'0',X'0A',XL3'00',XL252'00'                                     
                                                                                
***********************************************************************         
* VALID MQ MESSAGES HANDLED BY THIS PROGRAM                                     
***********************************************************************         
         DS    0D                                                               
         DC    CL16'*MSGTAB**MSGTAB*'                                           
MSGTAB   DS    0X                                                               
*                                                                               
         DC    CL16'Rentrak Demos***'                                           
         DC    AL1(MSGTODS+MSGTOBU),AL3(0)                                      
         DC    X'E8',AL1(L'RENCODE),AL2(0)                                      
         DC    A(UPDTTAB)                                                       
*                                                                               
         DC    CL16'Local Demos N***'                                           
         DC    AL1(MSGTODS+MSGTOBU),AL3(0)                                      
         DC    X'EB',AL1(L'CSDDESC),AL2(0)                                      
         DC    A(UPDTTAB)                                                       
*                                                                               
         DC    CL16'Rentrak Ntwks***'                                           
         DC    AL1(MSGTODS+MSGTOBU),AL3(0)                                      
         DC    X'E9',AL1(L'RTKNET),AL2(0)                                       
         DC    A(UPDTTAB)                                                       
*                                                                               
         DC    CL16'National Demos**'                                           
         DC    AL1(MSGTODS+MSGTOBU),AL3(0)                                      
         DC    X'EA',AL1(L'RENCODE),AL2(0)                                      
         DC    A(UPDTTAB)                                                       
*                                                                               
         DC    CL16'comScoreRSP*****'                                           
         DC    AL1(MSGTOPQ),AL3(0)                                              
         DC    AL4(0)                                                           
         DC    A(SCORERSP)                                                      
*                                                                               
         DC    CL16'cScoreBatchRSP**'                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    A(SCOBARSP)                                                      
*                                                                               
         DC    CL16'comScoreLDC*****'                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    A(UPDTLDC)                                                       
*                                                                               
         DC    CL16'comScoreCDC*****'                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    A(UPDTCDC)                                                       
*                                                                               
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* MQSERIES                                                                      
*  PARAMETER LISTS FOR MQSERIES CALLS                                           
*   F    A(ROUTINE)                                                             
*   CL16 EBCDIC ROUTINE NAME                                                    
*   XL1  FLAGS                                                                  
*   XL3  SPARE                                                                  
*   PARAMETERS (STANDARD IBM FORMAT)                                            
***********************************************************************         
                          DS    0D                                              
MQDEFS                    DC    CL8'*MQDEFS*'                                   
*                                                                               
CSQBCONN_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_CONNECT'                                
                          DC    A(0)                                            
                          DC    A(QMGR)                                         
                          DC    A(HCONN)                                        
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCOMM_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_COMMIT'                                 
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBDISC_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_DISCONNECT'                             
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*----------------------------------------------------------------------         
* INPUT QUEUE STRUCTURES                                                        
*----------------------------------------------------------------------         
CSQBOPEN_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_OPEN'                                   
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(OBJDESC)                                      
                          DC    A(OPENOPT)                                      
                          DC    A(HOBJ)                                         
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBGET_STRUCTURE         DS    A                                               
                          DC    CL16'MQ_GET'                                    
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(HOBJ)                                         
                          DC    A(MSGDESC)                                      
                          DC    A(GMSGOPTS)                                     
                          DC    A(MSGMAX)                                       
AGETMSG                   DC    A(0)                                            
                          DC    A(DATALEN)                                      
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBPUT_STRUCTURE         DS    A                                               
                          DC    CL16'MQ_PUT'                                    
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(HOBJ)                                         
                          DC    A(MSGDESC)                                      
                          DC    A(PMSGOPTS)                                     
                          DC    A(DATALEN)                                      
APUTMSG                   DC    AL4(0)                                          
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCLOS_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_CLOSE'                                  
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(HOBJ)                                         
                          DC    A(CLOSOPT)                                      
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*----------------------------------------------------------------------         
* BACKUP QUEUE STRUCTURES                                                       
*----------------------------------------------------------------------         
CSQBOPEN_STRUCTURE_KQ     DS    A                                               
                          DC    CL16'MQ_OPEN_KQ'                                
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(OBJDESC)                                      
                          DC    A(OPENOPT2)                                     
                          DC    A(HOBJ2)                                        
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBGET_STRUCTURE_KQ      DS    A                                               
                          DC    CL16'MQ_GET_KQ'                                 
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(HOBJ2)                                        
                          DC    A(MSGDESC)                                      
                          DC    A(GMSGOPTS)                                     
                          DC    A(MSGMAX)                                       
AGETMBK                   DC    A(0)                                            
                          DC    A(DATALEN)                                      
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBPUT_STRUCTURE_KQ      DS    A                                               
                          DC    CL16'MQ_PUT_KQ'                                 
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(HOBJ2)                                        
                          DC    A(MSGDESC)                                      
                          DC    A(PMSGOPTS)                                     
                          DC    A(DATALEN)                                      
APUTMBK                   DC    AL4(0)                                          
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCLOS_STRUCTURE_KQ     DS    A                                               
                          DC    CL16'MQ_CLOSE_KQ'                               
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(HOBJ2)                                        
                          DC    A(CLOSOPT)                                      
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*----------------------------------------------------------------------         
* BAD MESSAGE QUEUE STRUCTURES                                                  
*----------------------------------------------------------------------         
CSQBOPEN_STRUCTURE_DQ     DS    A                                               
                          DC    CL16'MQ_OPEN_DQ'                                
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(OBJDESC)                                      
                          DC    A(OPENOPT3)                                     
                          DC    A(HOBJ3)                                        
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBPUT_STRUCTURE_DQ      DS    A                                               
                          DC    CL16'MQ_PUT_DQ'                                 
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(HOBJ3)                                        
                          DC    A(MSGDESC)                                      
                          DC    A(PMSGOPTS)                                     
                          DC    A(DATALEN)                                      
APUTMDED                  DC    AL4(0)                                          
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCLOS_STRUCTURE_DQ     DS    A                                               
                          DC    CL16'MQ_CLOSE_DQ'                               
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(HOBJ3)                                        
                          DC    A(CLOSOPT)                                      
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
ENTRYPTS DS    0F                                                               
         DC    Y((ENTRYLSQ-ENTRYSTQ)/60)   NUMBER OF TABLE ENTRIES              
         DC    H'60'                       MUST REMAIN AS 60                    
ENTRYSTQ EQU   *                                                                
*                                                                               
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
CSQBPUT  DC    CL8'CSQBPUT'                                                     
         DC    XL52'00'                                                         
CSQBOPEN DC    CL8'CSQBOPEN'                                                    
         DC    XL52'00'                                                         
ENTRYLSQ EQU   *                                                                
                                                                                
*----------------------------------------------------------------------         
* MQSERIES VALUES                                                               
*----------------------------------------------------------------------         
HCONN    DS    F                   MQ QMGR CONNECTION HANDLE                    
HOBJ     DS    F                   OBJECT HANDLE  (INPUT QUEUE)                 
HOBJ2    DS    F                   OBJECT HANDLE  (BACKUP QUEUE)                
HOBJ3    DS    F                   OBJECT HANDLE  (BAD MESSAGE QUEUE)           
OPENOPT  DS    F                   MQOPEN OPTIONS (INPUT QUEUE)                 
OPENOPT2 DS    F                   MQOPEN OPTIONS (BACKUP QUEUE)                
OPENOPT3 DS    F                   MQOPEN OPTIONS (BAD MESSAGE QUEUE)           
CLOSOPT  DS    F                   MQCLOSE OPTIONS                              
COMPCODE DS    F                   COMPLETION CODE                              
REASON   DS    F                   QUALIFIES COMPLETION CODE                    
DATALEN  DS    F                   LENGTH OF THE MESSAGE                        
MSGMAX   DC    A(4*1024*1024)      4M MESSAGE MAXIMUM                           
*                                                                               
         CMQA   LIST=YES,EQUONLY=NO                                             
OBJDESC  CMQODA LIST=YES           OBJECT DESCRIPTOR                            
MSGDESC  CMQMDA LIST=YES           MESSAGE DESCRIPTOR                           
         ORG    MSGDESC_FORMAT                                                  
         DC     CL8'MQSTR   '      MQFMT_STRING  FOR DATA FORMAT                
         ORG                                                                    
GMSGOPTS CMQGMOA LIST=YES          GET MESSAGE OPTIONS                          
PMSGOPTS CMQPMOA LIST=YES          PUT MESSAGE OPTIONS                          
                                                                                
***********************************************************************         
* COMMON FACILITIES                                                             
***********************************************************************         
         DS    0D                                                               
         DC    CL8'*COMFACS'                                                    
COMFACS  DC    V(DATAMGR)                                                       
         DC    63A(0)                                                           
         DC    V(PROTON)                                                        
         DC    V(PROTOFF)                                                       
         DC    3A(0)                                                            
         DC    V(LOCKUP)                                                        
         DC    V(MASTC)            MASTC                                        
         DC    V(LOCKSPC)          LOCKSPACE                                    
         DC    8A(0)                                                            
                                                                                
***********************************************************************         
* IO AREA                                                                       
***********************************************************************         
         DS    0D                                                               
         DC    CL8'*IOW*IOW'                                                    
IOWRK    DS    12D                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'*IO1*IO1'                                                    
IO       DC    2048X'00'                                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'*SOBSOB*'                                                    
PQB      DC    14336X'00'                                                       
         DC    CL8'*EOBEOB*'                                                    
                                                                                
***********************************************************************         
* RD CHAIN                                                                      
***********************************************************************         
         DS    0D                                                               
         DC    CL8'R13CHAIN'                                                    
R13CHAIN DS    5000D               WORKING STORAGE                              
                                                                                
***********************************************************************         
* MASTC CSECT                                                                   
***********************************************************************         
MASTC    CSECT                                                                  
         PRINT  OFF                                                             
       ++INCLUDE DDMASTC                                                        
         PRINT  ON                                                              
                                                                                
***********************************************************************         
* MESSAGE DEFINITION AND PROCESSING TABLE                                       
***********************************************************************         
MSGTABD  DSECT                                                                  
MSGTTAG  DS    CL16                MQ MESSAGE LABEL                             
*                                                                               
MSGTOPT  DS    XL1                 MESSAGE OPTIONS                              
MSGTODS  EQU   X'80'               . BUILD TABLE IN DATA SPACE                  
MSGTOPQ  EQU   X'40'               . PRINT QUEUE ACCESS REQUIRED                
MSGTOBU  EQU   X'10'               . BACKUP REQUIRED                            
MSGTIND  DS    XL1                 APPLICATION PROCESSING INDICATOR             
MSGTIDS  EQU   X'80'               . BUILD HAS BEEN ATTEMPTED                   
         DS    XL2                 N/D                                          
*                                                                               
MSGTID#  DS    XL1                 ID NUMBER IN DEMADDR                         
MSGTKEY  DS    AL1                 SORT KEY LENGTH                              
MSGTDSP  DS    AL1                 SORT KEY DISPLACEMENT                        
         DS    XL1                 N/D                                          
*                                                                               
MSGTROUT DS    AL4                 A(PROCESSING ROUTINE)                        
*                                                                               
MSGTLNQ  EQU   *-MSGTABD                                                        
                                                                                
***********************************************************************         
* OTHER DSECTS                                                                  
***********************************************************************         
       ++INCLUDE DDCBJOBD                                                       
       ++INCLUDE DDTABSDEMD                                                     
       ++INCLUDE GEGENGEN                                                       
       ++INCLUDE GEGENCDC                                                       
* DMPRTQK                                                                       
         PRINT  OFF                                                             
       ++INCLUDE DMPRTQK                                                        
         PRINT ON                                                               
* CTGENEDICT                                                                    
         PRINT  OFF                                                             
       ++INCLUDE CTGENEDICT                                                     
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         ORG P+30                                                               
PMSG     DS    CL44                                                             
         PRINT ON                                                               
* DDMQREASON                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDMQREASON                                                     
         PRINT ON                                                               
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
* FATABSJOB                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSJOB                                                      
         PRINT ON                                                               
* FATABSDEQU                                                                    
         PRINT OFF                                                              
       ++INCLUDE FATABSDEQU                                                     
         PRINT ON                                                               
* IEZCIB                                                                        
         PRINT OFF                                                              
         IEZCIB                                                                 
         PRINT ON                                                               
* IEZCOM                                                                        
         PRINT OFF                                                              
         IEZCOM                                                                 
         PRINT ON                                                               
* DCBD                                                                          
         PRINT OFF                                                              
         DCBD  DSORG=PS,DEVD=DA                                                 
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008DDTABSDEM 02/13/20'                                      
         END                                                                    
