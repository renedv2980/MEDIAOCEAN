*          DATA SET DDEDINFX   AT LEVEL 057 AS OF 03/09/16                      
*PHASE EDINFXA                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE EDISUB                                                                 
*INCLUDE BINSR31                                                                
*INCLUDE PERVAL                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*                                                                               
         TITLE 'DDEDINFX -- PROCESS NEW EASYLINK MQ MESSAGES'                   
         MACRO                                                                  
&NAME    PRNT  &A,&PRINT=                                                       
&NAME    DS    0H                                                               
         AIF   (T'&A EQ 'O').NOMOVE                                             
         MVC   P(17),=CL17'&A'                                                  
.NOMOVE  ANOP                                                                   
         TIME  DEC                                                              
         ST    R0,PRNTDUB                                                       
         MVI   PRNTDUB+4,X'0F'                                                  
         UNPK  PRNTTIME,PRNTDUB(5)                                              
         MVC   P+18(2),PRNTTIME                                                 
         MVI   P+20,C':'                                                        
         MVC   P+21(2),PRNTTIME+2                                               
         MVI   P+23,C':'                                                        
         MVC   P+24(2),PRNTTIME+4                                               
         MVI   P+26,C'.'                                                        
         MVC   P+27(2),PRNTTIME+6                                               
         AIF   ('&PRINT' EQ 'ALWAYS').SKIP                                      
         CLI   TRACEFLG,C'Y'                                                    
         BNE   *+10                                                             
.SKIP    L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         MEND                                                                   
         EJECT                                                                  
***********************************************************************         
* PROCESS EASYLINK MQ MESSAGES                                                  
***********************************************************************         
EDIMQI   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*EDIMQI,=A(R13CHAIN)                                           
*                                                                               
         LR    R5,RC                                                            
         L     RC,=A(COMMWORK)     COMMON STORAGE AREA                          
         LR    R7,RC                                                            
         AHI   R7,4096                                                          
         USING COMMWORK,RC,R7                                                   
*                                                                               
         SHI   R5,4                                                             
         L     R5,0(R5)                                                         
         L     R5,0(R5)            A(R1 PARAMETERS FROM ATTACH)                 
         USING SUBPARMD,R5                                                      
*                                                                               
         MVC   AXMTTBL,SXMTTBLE    A(TRANSMIT TABLE)                            
         MVC   DATAMGR,SADTAMGR                                                 
         MVC   EDCTADD,SEDCTADD                                                 
         MVC   DRTEST,SDRTEST                                                   
         MVC   TRACEFLG,STRACEON   'Y' = PRINT DETAILED TRACE                   
         ENTRY TRACEFLG                                                         
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         L     RF,=V(PRNTER)       A(SYSPRINT DCB)                              
         MVC   DCBDDNAM-IHADCB(8,RF),=C'NFXTRACE'  DDNAME=NFXTRACE              
*                                                                               
         MVC   TITLE(41),=C'EDICT: SUBTASK TO EASYLINK MQ MESSAGE'              
         PRNT  START_SUBTASK,PRINT=ALWAYS                                       
*                                                                               
         L     RF,SMAJORNM                                                      
         MVC   MAJORNAM,0(RF)      MAJOR RESOURCE NAME                          
*                                                                               
         BRAS  RE,READCRDS         READ PARAMETER CARDS                         
         BRAS  RE,INITIAL          INITIALIZE                                   
*                                                                               
         LA    R2,CSQBCONN_STRUCTURE                                            
         BRAS  RE,CALLMQ           CONNECT TO MQ QUEUE MANAGER                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,GETMQBUF                                                      
*                                                                               
         LA    R2,CSQBDISC_STRUCTURE                                            
         BRAS  RE,CALLMQ           DISCONNECT FROM MQ QUEUE MANAGER             
         BE    *+6                                                              
         DC    H'0'                                                             
         PRNT  EXITING,PRINT=ALWAYS                                             
         XBASE                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE                                                                    
***********************************************************************         
INITIAL  NTR1  BASE=*,LABEL=*                                                   
         PRNT  INITIALIZE,PRINT=ALWAYS                                          
*                                                                               
* CALCULATE TIME ADJUSTMENT BETWEEN UTC AND LT(LOCAL TIME)                      
*                                                                               
         TIME  BIN,ZONE=LT                                                      
         LR    R3,R0                   R3=LOCAL TIME                            
         TIME  BIN,ZONE=UTC                                                     
         LR    R2,R0                   R2=UTC TIME                              
*                                                                               
         A     R3,=A(6*60*60*100)      ADD 6 HOURS TO LOCAL TIME                
         C     R3,=A(24*60*60*100)                                              
         BNH   *+8                                                              
         S     R3,=A(24*60*60*100)     MINUS 24 HOURS                           
         SR    R3,R2                                                            
         LR    R4,R3               SAVE SIGN(+/-) IN R4 FOR LATER CHECK         
         LPR   R3,R3                                                            
*                                                                               
         SR    R2,R2               PREPARE FOR DIVIDE                           
         D     R2,=A(60*100)       /6000 TO GET MIN (R2=SEC,R3=MIN)             
         SR    R2,R2               PREPARE FOR DIVIDE AGAIN                     
         D     R2,=A(60)           /60 TO GET HR+MIN (R2=MIN,R3=HR)             
*                                                                               
         MHI   R3,100              SHIFT HOUR FOR 2 DIGITS                      
         AR    R3,R2               + MIN TO STORE IN R3 HHMM                    
         CVD   R3,DUB                                                           
         MVC   TIMEADJ,DUB+4                                                    
         OI    TIMEADJ+3,X'0D'-X'0C'     ADJUST THE SIGN                        
         LTR   R4,R4                                                            
         BM    *+8                                                              
         OI    TIMEADJ+3,X'0F'                                                  
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(20,TODAY8)                                
         MVC   DELPERD+9(8),TODAY8   FOR PERVAL                                 
*                                                                               
         L     R1,SSTOPECB           BUILD ECBLIST                              
         STCM  R1,7,ASTOPECB+1                                                  
*                                                                               
         BLDL  0,ENTRYPTS            BUILD LIST OF MQ CALL ENTRY PTS            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                  BAD RETURN FROM BLDL MACRO                 
*                                                                               
         LOAD  DE=CSQBCONN                                                      
         ST    R0,CSQBCONN_STRUCTURE                                            
         LOAD  DE=CSQBOPEN                                                      
         ST    R0,CSQBOPEN_STRUCTURE                                            
         LOAD  DE=CSQBGET                                                       
         ST    R0,CSQBGET_STRUCTURE                                             
         LOAD  DE=CSQBCOMM                                                      
         ST    R0,CSQBCOMM_STRUCTURE                                            
         LOAD  DE=CSQBCLOS                                                      
         ST    R0,CSQBCLOS_STRUCTURE                                            
         LOAD  DE=CSQBDISC                                                      
         ST    R0,CSQBDISC_STRUCTURE                                            
         LOAD  DE=CSQBPUT1                                                      
         ST    R0,CSQBPUT1_STRUCTURE                                            
                                                                                
***********************************************************************         
* FROM 2007, THE DAYLIGHT SAVING TIME BEGINS                                    
* THE SECOND SUNDAY OF MARCH AND ENDS THE FIRST SUNDAY OF NOVEMBER              
***********************************************************************         
         GOTO1 =V(DATCON),DMCB,(5,0),TODAY                                      
         MVC   WORK(2),TODAY       EBCDIC YEAR                                  
         MVC   WORK+2(4),=C'0307'  START LOOKING AT MARCH 8                     
*                                                                               
DAYLITE5 GOTO1 =V(ADDAY),DMCB,WORK,WORK,F'1'                                    
         GOTO1 =V(GETDAY),DMCB,WORK,WORK+6                                      
         CLI   DMCB,7              IS IT SUNDAY?                                
         BNE   DAYLITE5            NO                                           
*                                                                               
         CLC   TODAY,WORK                                                       
         BL    DAYLITEX            TODAY IS PRIOR TO START OF DST               
*                                                                               
         MVC   WORK(2),TODAY       EBCDIC YEAR                                  
         MVC   WORK+2(4),=C'1031'  START LOOKING AT OCT 31                      
*                                                                               
DAYLITE7 GOTO1 =V(ADDAY),DMCB,WORK,WORK,F'1'                                    
         GOTO1 =V(GETDAY),DMCB,WORK,WORK+6                                      
         CLI   DMCB,7              IS IT SUNDAY?                                
         BNE   DAYLITE7            NO                                           
         GOTO1 =V(ADDAY),DMCB,WORK,WORK,F'-1' SATURDAY IS END OF DST            
*                                                                               
         CLC   TODAY,WORK                                                       
         BH    DAYLITEX            TODAY IS AFTER THE END OF DST                
*                                                                               
         MVI   DSTFLAG,C'Y'        IT'S DAYLIGHT SAVING TIME!                   
         MVC   P+30(6),TODAY                                                    
         PRNT  DAYLIGHTSAVING,PRINT=ALWAYS                                      
DAYLITEX EQU   *                                                                
                                                                                
**********************************************************************          
* USE ALIAS QUEUE                                                               
**********************************************************************          
         MVC   OBJDESC2_OBJECTNAME,=CL48'DDS.BROKER.QUEUE'                      
*&&DO                                                                           
         MVC   OBJDESC2_OBJECTNAME,=CL48'DDS.BROKER.LOCALQ'                     
         CLI   MAJORNAM+5,C'T'     TEST EDICT?                                  
         BE    *+12                YES                                          
         CLI   MAJORNAM+5,C'Q'     FQA EDICT?                                   
         BNE   *+10                NO                                           
         MVC   OBJDESC2_OBJECTNAME,=CL48'DDS.BROKER.TEST.LOCALQ'                
*&&                                                                             
*                                                                               
* SET (#) TASK CODE (NF) & TCB ADDR IN ISGENQ / CLEAR TASK ENQUEUES (K)         
*                                                                               
         GOTO1 SADTAMGR,DMCB,(0,=CL7'ISGENQ'),C'#05K'                           
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* READ PARAMETER CARDS BETWEEN "++NFX" AND "++NFXEND" CARDS                     
*                                                                               
*   *......          "*" IN COLUMN ONE IS A COMMENT CARD                        
*   TRACE=YES        OVERRIDE TRACE FLAG                                        
*   MQQMGRNAME=CL48      MQ QUEUE MANAGER NAME (DEFAULT=' ')                    
*   MQQUEUENAME=CL48     MQ QUEUE NAME (DEFAULT=' ')                            
*   MQCALL=NO        DO ALL MQSERISE CALLS (DEFAULT=YES)                        
***********************************************************************         
READCRDS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
RC10     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++NFX',CARD      LOOK FOR START OF PARAMETERS                 
         BNE   RC10                                                             
*                                                                               
RC20     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++NFXEND',CARD   LOOK FOR END OF PARAMETERS                   
         BE    RCX                                                              
*                                                                               
         MVC   P(80),CARD          PRINT ALL PARAMETER CARDS                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   CARD,C'*'           COMMENT CARD?                                
         BE    RC20                YES                                          
*                                                                               
         LA    RE,CARD             BLANK DELIMITS A COMMENT                     
         LA    R0,C' '                                                          
         SRST  R0,RE               R0 = A(FIRST BLANK)                          
         LR    R1,R0                                                            
         LA    R1,1(R1)            BUMP TO NEXT CHARACTER IN CARD               
         C     R1,=A(CARD+79)      STOP AT END OF CARD                          
         BH    *+12                                                             
         MVI   0(R1),C' '          REPLACE COMMENT WITH BLANKS                  
         B     *-16                                                             
*                                                                               
         CLC   =C'MQQMGRNAME=',CARD       MQQMGRNAME=                           
         BNE   *+14                                                             
         MVC   QMGRNAME,CARD+11                                                 
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQUEUENAME=',CARD      MQQUEUENAME=                          
         BNE   *+14                                                             
         MVC   OBJDESC_OBJECTNAME,CARD+12                                       
         B     RC20                                                             
*                                                                               
         CLC   =C'TRACE=',CARD     TRACE=                                       
         BNE   RC30                                                             
         CLC   =C'NO',CARD+6                                                    
         BE    RC20                DON'T OVERRIDE                               
         CLC   =C'YES',CARD+6                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRACEFLG,C'Y'       TRACE=YES                                    
         B     RC20                                                             
*                                                                               
RC30     CLC   =C'MQCALL=',CARD    MQCALL=                                      
         BNE   RC40                                                             
         CLC   =C'NO',CARD+7                                                    
         BNE   RC20                                                             
         MVI   DOMQCALL,C'N'       MQCALL=NO                                    
         B     RC20                                                             
*                                                                               
RC40     EQU   *                                                                
         B     RC20                                                             
*                                                                               
RCX      GOTO1 =V(PRINTER)         SKIP A LINE                                  
         XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET MQ MESSAGE                                                                
***********************************************************************         
GETMQBUF NTR1  BASE=*,LABEL=*                                                   
*                                                                               
MQBUF05  EQU   *                                                                
*                                                                               
         L     RE,=A(MQBUFF)                                                    
         L     RF,=A(MQBUFFL)      CLEAR BUFFER                                 
         XCEFL                                                                  
*                                                                               
         MVC   OBJDESC_OBJECTTYPE,=A(MQOT_Q)  OBJECT IS A QUEUE                 
         LA    RF,MQOO_INPUT_AS_Q_DEF                                           
         ST    RF,OPEN_OPTIONS                                                  
*                                                                               
         MVC   P+30(L'OBJDESC_OBJECTNAME),OBJDESC_OBJECTNAME                    
         PRNT  ABOUT_TO_OPEN_Q,ALWAYS                                           
*                                                                               
         LA    R2,CSQBOPEN_STRUCTURE                                            
         BRAS  RE,CALLMQ           OPEN QUEUE                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   MSGDESC_CORRELID,MQCI_NONE                                       
         MVC   MSGDESC_MSGID,MQMI_NONE                                          
*                                                                               
         MVC   MQBUFFLENGTH,=A(MQBUFFL)                                         
         MVC   GETMSGOPTS_OPTIONS,=A(MQGMO_SET_SIGNAL+MQGMO_ACCEPT_TRUN+        
               CATED_MSG)                                                       
         XC    INPQECB,INPQECB                                                  
         MVC   GETMSGOPTS_SIGNAL1,=A(INPQECB)                                   
         MVC   GETMSGOPTS_WAITINTERVAL,=A(MQWI_UNLIMITED)                       
*                                                                               
MQBUF10  MVI   OVERSIZE,C'N'       ASSUME NO OVERSIZE                           
         LA    R2,CSQBGET_STRUCTURE                                             
         BRAS  RE,CALLMQ           TRY TO GET A MESSAGE                         
         BE    MQBUF30             GOT A MESSAGE                                
         CLC   COMPCODE,=A(MQCC_WARNING)                                        
         BNE   MQBUF20                                                          
         CLC   REASON,=A(MQRC_TRUNCATED_MSG_ACCEPTED)                           
         BNE   *+12                                                             
         MVI   OVERSIZE,C'Y'                                                    
         B     MQBUF30             GOT AN OVERSIZE MESSAGE                      
         CLC   REASON,=A(MQRC_SIGNAL_REQUEST_ACCEPTED)                          
         BE    MQBUF20                                                          
         DC    H'0'                NO OTHER WARNING IS ACCEPTABLE               
*                                                                               
MQBUF20  PRNT  ABOUTTOWAIT                                                      
*                                                                               
         WAIT  1,ECBLIST=ECBLST    WAIT FOR COMPLETION OR OPERATOR              
*                                                                               
         PRNT  WAITCOMPLETE                                                     
*                                                                               
         L     RF,ASTOPECB                                                      
         TM    0(RF),X'40'         STOP?                                        
         BZ    *+18                NO                                           
         XC    0(4,RF),0(RF)                                                    
         MVI   OPERSTOP,C'Y'                                                    
         B     MQBUF90                                                          
*                                                                               
         TM    INPQECB,X'40'                                                    
         BO    *+6                                                              
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
         L     RF,INPQECB          BITS 2-31 OF ECB CONTAIN COMP. CODE          
         SLL   RF,2                                                             
         SRL   RF,2                                                             
         C     RF,=A(MQEC_MSG_ARRIVED)                                          
         BE    *+6                                                              
         DC    H'0'                NO OTHER RESULT ACCEPTABLE (FOR NOW)         
         XC    INPQECB,INPQECB     CLEAR ECB BEFORE MQGET CALL                  
         B     MQBUF10             A MESSAGE IS WAITING: GET IT                 
*                                                                               
*                                                                               
MQBUF30  CLC   MSGDESC_MSGTYPE,=A(MQMT_DATAGRAM)                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   R3,15,DATALENGTH    R6 = LENGTH OF DATA IN MESSAGE               
         BNZ   *+6                                                              
         DC    H'0'                NO DATA RECEIVED -- HOW?                     
*                                                                               
         CHI   R3,1000                                                          
         BNH   *+8                                                              
         LHI   R3,1000                                                          
*                                                                               
         MVC   DMCB+04,=A(MQBUFF)                                               
         GOTO1 =V(PRNTBL),DMCB,=C'MQ_GET: EDICT NOTIFICAITON MQBUFF',  +        
               ,C'DUMP',(R3),=C'1D'                                             
*                                                                               
         BRAS  RE,PROMQBUF                                                      
*                                                                               
         CLC   =C'JNK',MSG_ACT                                                  
         BNE   MQBUF80                                                          
         BRAS  RE,ERRDFAX                                                       
*                                                                               
MQBUF80  LA    R2,CSQBCOMM_STRUCTURE                                            
         BRAS  RE,CALLMQ           COMMIT                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CLOSE_OPTIONS,=A(MQCO_NONE)                                      
         LA    R2,CSQBCLOS_STRUCTURE                                            
         BRAS  RE,CALLMQ           CLOSE QUEUE                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 SADTAMGR,DMCB,(0,=CL7'ISGENQ'),C'#05K'                           
*                                                                               
         B     MQBUF05             GET NEXT MESSAGE IF ANY                      
*                                                                               
MQBUF90  MVC   CLOSE_OPTIONS,=A(MQCO_NONE)                                      
         LA    R2,CSQBCLOS_STRUCTURE                                            
         BRAS  RE,CALLMQ           CLOSE QUEUE                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MQBUFX   XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* PROCESS MQ MESSAGE                                                            
***********************************************************************         
PROMQBUF NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,=A(MQBUFF)                                                    
         MVC   MSG_ACT,0(R3)       ACTION                                       
         CLC   =C'ADD',MSG_ACT                                                  
         BE    PMBADD                                                           
         CLC   =C'JNK',MSG_ACT                                                  
         BE    PMBADD                                                           
         CLC   =C'INB',MSG_ACT                                                  
         BE    PMBINB                                                           
         CLC   =C'01',MSG_ACT                                                   
         BE    PMBBDE                                                           
         CLC   =C'DLN',MSG_ACT                                                  
         BE    PMBUPD                                                           
         CLC   =C'CAN',MSG_ACT                                                  
         BE    PMBUPD                                                           
         CLC   =C'ACC',MSG_ACT                                                  
         BE    PMBUPD                                                           
         CLC   =C'REJ',MSG_ACT                                                  
         BE    PMBUPD                                                           
         CLC   =C'REA',MSG_ACT                                                  
         BE    PMBUPD                                                           
         B     PMBUKN                                                           
*                                                                               
PMBADD   BRAS  RE,PROMADD                                                       
         B     PMBX                                                             
PMBINB   BRAS  RE,PROMINB                                                       
         B     PMBX                                                             
PMBBDE   BRAS  RE,PROMBDE                                                       
         B     PMBX                                                             
PMBUPD   BRAS  RE,PROMUPD                                                       
         B     PMBX                                                             
PMBUKN   BRAS  RE,PROMUKN                                                       
         B     PMBX                                                             
*                                                                               
PMBX     XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
PROMUPD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,EZDLNCAN                                                      
*                                                                               
PMUX     XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
***********************************************************************         
PROMUKN  NTR1  BASE=*,LABEL=*                                                   
*                                  PRINT OUT MSG DESCRIPTOR                     
         LHI   R3,MSGDESC_LENGTH      R3 = LENGTH OF MSG DESCRIPTOR             
         MVC   DMCB+04,=A(MSGDESC)                                              
         GOTO1 =V(PRNTBL),DMCB,=C'MSGDESC',,C'DUMP',(R3),=C'1D'                 
*                                                                               
*                                  THEN EMAIL YYUN/AWIL                         
         LAY   R4,MQBUFF                                                        
         MVC   OPMSG5A,0(R4)                                                    
         OC    OPMSG5A,SPACES      JUST IN CASE OF BINARY DATA                  
         BRAS  RE,CHKOPM                                                        
         BNE   PMUKNX                                                           
         GOTO1 DATAMGR,DMCB,=C'OPMSG',('OPMSG5Q',OPMSG5)                        
*                                                                               
PMUKNX   XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
***********************************************************************         
PROMBDE  NTR1  BASE=*,LABEL=*                                                   
         LAY   R4,MQBUFF                                                        
         BRAS  RE,GETBDENF         PARSE BDE NOTIFCIATION                       
         BNE   PMBEX                                                            
         OC    NEWCREF,NEWCREF                                                  
         BZ    PMBE20                                                           
         BRAS  RE,CHKNWBNF         CHECK NEW BDE NOTIFICATION                   
         B     PMBEX                                                            
*                                                                               
PMBE20   BRAS  RE,CHKBDENF         CHECK OLD BDE NOTIFICATION                   
*                                                                               
PMBEX    XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS LEFTOVER SND/RCV MESSAGE                                    *         
* INPUT:R1=A(SAVED SND/RCV MESSAGE)                                   *         
* EXIT :CLEAR SAVED MESSAGE                                           *         
***********************************************************************         
PROMBSRM NTR1  BASE=*,LABEL=*                                                   
         LR    R4,R1                                                            
         MVC   P+30(10),=CL10'REPROCESS SND/RCV'                                
         MVC   P+40(3),=CL3'SND'                                                
         C     R1,=A(SVBDERCV)                                                  
         BL    *+10                                                             
         MVC   P+40(3),=CL3'RCV'                                                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         BRAS  RE,GETBDENF         PARSE BDE NOTIFCIATION, R4=A(MSG)            
         BNE   PMBSRMX                                                          
         OC    NEWCREF,NEWCREF                                                  
         BZ    PMBSRM20                                                         
         BRAS  RE,CHKNWBNF         CHECK NEW BDE NOTIFICATION                   
         B     PMBSRMX                                                          
*                                                                               
PMBSRM20 BRAS  RE,CHKBDENF         CHECK OLD BDE NOTIFICATION                   
*                                                                               
PMBSRMX  XC    0(L'SVBDESND,R4),0(R4)    CLEAR SAVED "SND/RCV" MESSAGE          
         XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* VERSIONCONTROLNUM:    CL2                                                     
* DDSREFNUM:            CL18 (MAX CL30)                                         
* SERVERDATE:           CL10 AS MMMDD/YYYY                                      
* SERVERTIME:           CL8  AS HH:MM:SS IN 24-HR FORMAT                        
* EVENTTYPE:            CL3  (SND, RCV, DEL)                                    
* EVENTDATE:            CL10 AS MMMDD/YYYY                                      
* EVENTTIME:            CL8  AS HH:MM:SS IN 24-HR FORMAT                        
* DOCID:                CL9                                                     
* SENDERCN:             CL20                                                    
*                                                                               
**********************************************************************          
GETBDENF NTR1  BASE=*,LABEL=*                                                   
         LA    R3,FLDTAB                                                        
*                                                                               
GBNF10   MVC   FIELD+1(L'FIELD-1),0(R4)                                         
         LR    R2,R4                                                            
*                                                                               
         LA    RF,L'FIELD                                                       
         CLI   0(R4),X'5E'                                                      
         BE    GBNF20                                                           
         AHI   R4,1                                                             
         BCT   RF,*-12                                                          
         B     GBNFERR                                                          
*                                                                               
GBNF20   LR    RE,R4                                                            
         SR    RE,R2                                                            
*                                                                               
         STC   RE,FIELD                                                         
         OC    0(L'FLDTAB,R3),0(R3)                                             
         BZ    *+14                                                             
         L     RF,0(R3)                                                         
         BASR  RE,RF                                                            
         BNE   GBNFX                                                            
*                                                                               
         AHI   R4,1                                                             
         AHI   R3,L'FLDTAB                                                      
         CLI   0(R3),X'FF'                                                      
         BNE   GBNF10                                                           
         B     GBNFOK                                                           
*                                                                               
GBNFERR  MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(15),=C'UNKNOWN MESSAGE'                                     
         GOTO1 =V(PRINTER)                                                      
         MVC   P+11(L'FIELD),FIELD                                              
         GOTO1 =V(PRINTER)                                                      
         B     GBNFBAD                                                          
*                                                                               
GBNFOK   CR    RB,RB                                                            
         B     *+6                                                              
GBNFBAD  LTR   RB,RB                                                            
GBNFX    XIT1                                                                   
*                                                                               
FLDTAB   DS    0F                                                               
         DC    A(GVCNUM)                                                        
         DC    A(GCUSTREF)                                                      
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(GEVTYPE)                                                       
         DC    A(GEVDAY)                                                        
         DC    A(GEVTIME)                                                       
         DC    A(GDOCID)                                                        
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
*                                                                               
**********************************************************************          
GVCNUM   NTR1  BASE=*,LABEL=*                                                   
         ZIC   RE,FIELD                                                         
         CHI   RE,L'VCNUM                                                       
         BNE   *+14                                                             
         MVC   VCNUM,FIELD+1                                                    
         B     GVCNOK                                                           
*                                                                               
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(30),=C'INVALID VERSION CONTROL NUMBER'                      
         GOTO1 =V(PRINTER)                                                      
         B     GVCNBAD                                                          
*                                                                               
GVCNOK   CR    RB,RB                                                            
         B     *+6                                                              
GVCNBAD  LTR   RB,RB                                                            
GVCNX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*                                                                               
**********************************************************************          
GCUSTREF NTR1  BASE=*,LABEL=*                                                   
         ZIC   RE,FIELD                                                         
         CHI   RE,L'CUSTREF                                                     
         BE    *+12                                                             
         CHI   RE,L'NEWCREF                                                     
         BNE   GCR10                                                            
         MVC   CUSTREF,FIELD+1                                                  
         XC    NEWCREF,NEWCREF     ASSUME NOT THE NEW STYLE                     
*                                                                               
         CLC   CUSTREF+1(1),MAJORNAM+5   MATCH ON EDICT (ADV/REP)?              
         BNE   GCR10               NO -- DLN CAME IN OVER WRONG LINE            
*                                                                               
         CLI   CUSTREF,C'4'        VERSION 4 CUSTOMER REF. NUMBER?              
         BNE   GCR10                                                            
*                                                                               
         CLC   TODAY8,CUSTREF+10   NEED TO RESET TODAY'S DATE?                  
         BNL   GCR05               NO                                           
         GOTO1 =V(DATCON),DMCB,(5,0),(20,TODAY8)                                
         MVC   DELPERD+9(8),TODAY8 FOR PERVAL                                   
*                                                                               
GCR05    MVC   DELPERD(8),CUSTREF+10   DATE OF ORIGINAL SEND (YYYYMMDD)         
         GOTO1 =V(PERVAL),DMCB,(L'DELPERD,DELPERD),WORK                         
         CLI   DMCB+4,0                                                         
         BNE   GCR10               MAINFRAME DATE MUST BE SCREWED UP!           
         LA    RF,WORK                                                          
         CLC   PVALNDYS-PERVALD(2,RF),=AL2(28)                                  
         BNH   GCROK               DLN ISN'T MORE THAN 28 DAYS OLD              
*                                                                               
GCR10    CLI   CUSTREF,C'1'        VERSION 1 CUSTOMER REF. NUMBER?              
         BE    GCR20                                                            
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(33),=C'INVALID CUSTOMER REFERENCE NUMBER'                   
         MVC   P+70(L'CUSTREF),CUSTREF                                          
         GOTO1 =V(PRINTER)                                                      
         B     GCRBAD              IGNORE THIS ONE                              
*                                                                               
GCR20    EQU   *                                                                
         MVC   NEWCREF,FIELD+1                                                  
         MVC   P+11(10),=CL10'NEWCREF='                                         
         MVC   P+22(L'NEWCREF),NEWCREF                                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
GCROK    CR    RB,RB                                                            
         B     *+6                                                              
GCRBAD   LTR   RB,RB                                                            
GCRNX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*                                                                               
**********************************************************************          
GEVTYPE  NTR1  BASE=*,LABEL=*                                                   
         ZIC   RE,FIELD                                                         
         CHI   RE,L'EVTYPE                                                      
         BNE   GETYP20                                                          
         MVC   EVTYPE,FIELD+1                                                   
*                                                                               
         CLC   EVTYPE,=C'SND'                                                   
         BE    GETYPOK                                                          
         CLC   EVTYPE,=C'RCV'                                                   
         BE    GETYPOK                                                          
         CLC   EVTYPE,=C'DEL'                                                   
         BE    GETYPOK                                                          
*                                                                               
GETYP20  MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(17),=C'INVALID EVENT TYPE'                                  
         GOTO1 =V(PRINTER)                                                      
         B     GETYPBAD                                                         
*                                                                               
GETYPOK  CR    RB,RB                                                            
         B     *+6                                                              
GETYPBAD LTR   RB,RB                                                            
GETYPX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*                                                                               
**********************************************************************          
GEVDAY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZIC   RE,FIELD                                                         
         CHI   RE,10               CL10, MMMDD/YYYY                             
         BNE   GEDAYBAD                                                         
*                                                                               
         GOTO1 =V(DATVAL),DMCB,(10,FIELD+1),WORK                                
         OC    DMCB,DMCB                                                        
         BNE   GEDAY10                                                          
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(17),=C'INVALID EVENT DATE'                                  
         GOTO1 =V(PRINTER)                                                      
         B     GEDAYBAD            IGNORE THIS ONE                              
*                                                                               
GEDAY10  GOTO1 =V(HEXIN),DMCB,FIELD+4,EVDAY,2                                   
*                                                                               
GEDAYOK  CR    RB,RB                                                            
         B     *+6                                                              
GEDAYBAD LTR   RB,RB                                                            
GEDAYX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*                                                                               
**********************************************************************          
GEVTIME  NTR1  BASE=*,LABEL=*                                                   
         ZIC   RE,FIELD                                                         
         CHI   RE,8                CL8, HH:MM:SS                                
         BNE   GETIMBAD                                                         
*                                                                               
         MVC   FULL(2),FIELD+1                                                  
         MVC   FULL+2(2),FIELD+4                                                
         GOTO1 =V(HEXIN),DMCB,FULL,EVTIME,4                                     
         CLC   =F'2',DMCB+12                                                    
         BE    GETIMOK                                                          
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(17),=C'INVALID EVENT TIME'                                  
         GOTO1 =V(PRINTER)                                                      
         B     GETIMBAD            IGNORE THIS ONE                              
*                                                                               
GETIMOK  CR    RB,RB                                                            
         B     *+6                                                              
GETIMBAD LTR   RB,RB                                                            
GETIMX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*                                                                               
**********************************************************************          
GDOCID   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZIC   RE,FIELD                                                         
         CHI   RE,L'DOCID                                                       
         BNE   *+14                                                             
         MVC   DOCID,FIELD+1                                                    
         B     GDIDOK                                                           
*                                                                               
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(26),=C'INVALID DOCUMENT ID NUMBER'                          
         GOTO1 =V(PRINTER)                                                      
         B     GDIDBAD                                                          
*                                                                               
GDIDOK   CR    RB,RB                                                            
         B     *+6                                                              
GDIDBAD  LTR   RB,RB                                                            
GDIDX    XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
*                                                                               
**********************************************************************          
CHKBDENF NTR1  BASE=*,LABEL=*                                                   
         MVI   RETRY,C'N'          ASSUME NO RETRY YET                          
         LA    R1,DMCB                                                          
         USING CONVDSKD,R1                                                      
         MVC   CONVDMGR,DATAMGR    A(DATAMGR)                                   
         MVI   CONVACTN,CONVDSKO   CONVERT REFERENCE NUMBER TO DSKADDR          
         LA    RE,CUSTREF+2                                                     
         ST    RE,CONVOFF                                                       
         GOTO1 =V(CONVDSKA)                                                     
         MVC   FULL,CONVDSK                                                     
         DROP  R1                                                               
*                                                                               
         LA    R6,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R6)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(6),E,8)  ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         LA    R2,TABLNTRY         ASSUME WE'LL FIND REPORT IN TABLE            
         MVI   BYTE,C'T'           'T' FOR TABLE                                
*                                                                               
         L     R6,AXMTTBL          A(TRANSMIT TABLE)                            
         USING XMTTABLD,R3                                                      
         LA    R3,XMTNTRY                                                       
*                                                                               
CHKBF50  MVC31 XMTNTRY,0(R6)                                                    
         CLI   0(R3),0             END OF TABLE?                                
         BNE   *+16                                                             
         L     R2,FULL             YES -- USE EDICT FILE DISK ADDRESS           
         MVI   BYTE,C'F'           'F' FOR FILE                                 
         B     CHKBF60                                                          
         CLI   XMTSOURC,XMTSRCPQ   PRINT QUEUE ENTRY?                           
         BNE   *+14                                                             
         CLC   FULL,XMTDSKAD       FIND MATCH ON DISK ADDRESS                   
         BE    *+12                                                             
         AHI   R6,XMTTBLQ          BUMP TO NEXT ENTRY                           
         B     CHKBF50                                                          
         MVC   TABLNTRY,0(R3)      SAVE XMIT TABLE ENTRY                        
         DROP  R3                                                               
*                                                                               
CHKBF60  LA    R6,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R6)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(6),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
CHKBF65  LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
*                                                                               
         MVC   EXMTTBLE,AXMTTBL                                                 
         ST    R2,ETBLNTRY                                                      
*                                                                               
         CLC   EVTYPE,=C'RCV'                                                   
         BNE   *+12                                                             
         MVI   EACTION,EACTDLVQ                                                 
         B     CHKBF70                                                          
*                                                                               
         CLC   EVTYPE,=C'SND'                                                   
         BNE   *+12                                                             
         MVI   EACTION2,EACTNTFQ                                                
         B     CHKBF70                                                          
*                                                                               
         CLC   EVTYPE,=C'DEL'                                                   
         BNE   *+12                                                             
         MVI   EACTION2,EACTDELQ                                                
         B     CHKBF70                                                          
*                                                                               
CHKBF70  CLI   BYTE,C'F'           DISK ADDRESS BEING PASSED?                   
         BNE   *+8                                                              
         OI    EACTION,EACTFILQ    YES                                          
         MVC   EMAJORNM,=A(MAJORNAM)                                            
         MVC   EABDEDTA,=A(DOCID)                                               
         MVC   EDAYNUM,EVDAY       DAY                                          
         MVC   ETIME,EVTIME        TIME                                         
         MVC   EADTAMGR,DATAMGR                                                 
         GOTO1 =V(POSTSENT)                                                     
         BNE   CHKBF80             COULD NOT POST EDICT FILE                    
         DROP  R1                                                               
*                                  EDICT FILE DISK ADDRESS                      
         GOTO1 =V(HEXOUT),DMCB,FULL,P+30,4,=C'TOG'                              
         PRNT  EDICTFILEPOSTED,PRINT=ALWAYS                                     
         B     CHKBFX                                                           
*                                                                               
CHKBF80  CLC   EVTYPE,=C'SND'                                                   
         BE    CHKBF90             NO RETRY FOR "SND" MESSAGE                   
*                                                                               
*SOMETIMES, A "RCV"/"DEL" MAY COME BEFORE SENDER MARKS THE REPORT SENT.         
*SO, IN THIS CASE, WE SHOULD WAIT 2 MIN AND RETRY AGAIN.                        
*IT IS OKAY TO WAIT FOR THIS LONG TIME BECASUE SENDER TAKES ABOUT 3 MIN         
*TO SEND A REPORT.                                                              
*                                                                               
         CLI   RETRY,C'Y'          RETRY ALREADY?                               
         BE    CHKBF90                                                          
*                                                                               
         PRNT  WAIT_AND_RETRY,PRINT=ALWAYS                                      
         STIMER WAIT,BINTVL=WAITSECS                                            
         MVI   RETRY,C'Y'          RETRY NOW                                    
         B     CHKBF65                                                          
*                                                                               
CHKBF90  MVC   P+30(13),=C'*** ERROR ***'                                       
         PRNT  CANTPOST,PRINT=ALWAYS                                            
*                                                                               
CHKBFX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
WAITSECS DC    F'12000'            DEFAULT WAIT TIME = 2 MINUTES                
RETRY    DS    C                                                                
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
CHKNWBNF NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    AXMTNTRY,AXMTNTRY                                                
         XC    XMTNTRY,XMTNTRY                                                  
*                                                                               
         USING XMTTABLD,R2                                                      
         LA    R2,XMTNTRY                                                       
*                                                                               
         MVC   MSG_REF#,NEWCREF                                                 
*                                                                               
         MVC   XMTSUBID,MSG_REF#+06                                             
         GOTO1 =V(HEXIN),DMCB,MSG_REF#+02,XMTUSRID,4,0                          
         GOTO1 =V(HEXIN),DMCB,MSG_REF#+09,XMTREFNO,4,0                          
         GOTO1 =V(HEXIN),DMCB,MSG_REF#+13,XMTCRDAT,4,0                          
         GOTO1 =V(HEXIN),DMCB,MSG_REF#+17,XMTCRTIM,4,0                          
         MVI   XMTLOGNO,0                                                       
         GOTO1 =V(HEXIN),DMCB,MSG_REF#+21,XMTLOGNO+0,4,0                        
         MVI   XMTDSTNO,0                                                       
         GOTO1 =V(HEXIN),DMCB,MSG_REF#+25,XMTDSTNO+1,2,0                        
         NI    XMTDSTNO+1,X'FF'-X'80'   TURN OFF THIS BIT                       
*                                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,XMTUSRID       PQ REPORT SENDING USERID                     
         D     R0,NUMPQS                                                        
         AHI   R0,1                                                             
         STC   R0,XMTPRTQ          PRINT QUEUE NUMBER                           
         MVI   XMTSOURC,XMTSRCPQ   PRINT QUEUE REPORT                           
*                                                                               
         MVC   P+11(13),=C'***MSG_REF#**'                                       
         MVC   P+30(L'MSG_REF#),MSG_REF#                                        
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P+11(13),=C'*** XMTNTRY**'                                       
         MVC   P+30(L'XMTNTRY),XMTNTRY                                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R9,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R9)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(9),E,8)  ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         SAM31                                                                  
         L     RF,AXMTTBL                                                       
         AHI   RF,-8               RF = A(ENTRIES#)                             
         MVC   DMCB+8(4),0(RF)     NUMBER OF ENTRIES IN TABLE                   
         GOTO1 =V(BINSRCH),DMCB,XMTNTRY,AXMTTBL,,XMTTBLQ,XMTKEYQ,0              
         SAM24                                                                  
*                                                                               
         LA    R9,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R9)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(9),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
         ICM   R4,15,DMCB          A(RECORD IN TABLE)                           
         TMH   R4,X'8000'          WAS RECORD FOUND?                            
         BNO   CKNBF50             YES                                          
*                                                                               
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(24),=C'XMTTABLE ENTRY NOT FOUND'                            
         GOTO1 =V(PRINTER)                                                      
*                                                                               
*IF THIS IS "SND" MESSAGE, SAVE IT AROUND AND THEN                              
*   WHEN NEXT "ADD" MESSAGE IS PROCESSED, MATCH REF#                            
*    THEN CALL PROMBDE TO PROCESS IT, REPEAT OF N TIMES ONLY                    
*  MAY STORE MORE THAN ONE "SND" MESSAGES, KEEP A TABLE.                        
*  BUT FOR NOW, JUST DO THIS FOR 4 MESSAGES                                     
*                                                                               
         CLC   EVTYPE,=C'SND'                                                   
         BNE   CKNBF30                                                          
         LAY   RE,MQBUFF                                                        
         OC    SVBDESND,SVBDESND                                                
         BNZ   CKNBF20A                                                         
         MVC   SVBDESND,0(RE)      SAVE TO 1                                    
         MVC   P+30(16),=CL16'SAVE TO SVBDESND'                                 
         GOTO1 =V(PRINTER)                                                      
         B     CKNBFX              IGNORE THIS ONE                              
*                                                                               
CKNBF20A OC    SVBDESD2,SVBDESD2                                                
         BNZ   CKNBF20B                                                         
         MVC   SVBDESD2,0(RE)      SAVE TO 2                                    
         MVC   P+30(16),=CL16'SAVE TO SVBDESD2'                                 
         GOTO1 =V(PRINTER)                                                      
         B     CKNBFX              IGNORE THIS ONE                              
*                                                                               
CKNBF20B OC    SVBDESD3,SVBDESD3                                                
         BNZ   CKNBF20C                                                         
         MVC   SVBDESD3,0(RE)      SAVE TO 3                                    
         MVC   P+30(16),=CL16'SAVE TO SVBDESD3'                                 
         GOTO1 =V(PRINTER)                                                      
         B     CKNBFX              IGNORE THIS ONE                              
*                                                                               
CKNBF20C EQU   *                                                                
         MVC   P+30(16),=CL16'SAVE TO SVBDESD4'                                 
         OC    SVBDESD4,SVBDESD4                                                
         BZ    *+10                                                             
         MVC   P+30(17),=CL17'OVERRIDE SVBDESD4'                                
         MVC   SVBDESD4,0(RE)      ALWAYS SAVE TO 4 IF 1&2&3 ARE USED           
         GOTO1 =V(PRINTER)                                                      
         B     CKNBFX              IGNORE THIS ONE                              
*                                                                               
CKNBF30  EQU   *                                                                
         CLC   EVTYPE,=C'RCV'      RCV/DEL MSG?                                 
         BE    *+14                                                             
         CLC   EVTYPE,=C'DEL'                                                   
         BNE   CKNBFX              NO - IGNORE THIS ONE                         
         LAY   RE,MQBUFF                                                        
         OC    SVBDERCV,SVBDERCV                                                
         BNZ   CKNBF30A                                                         
         MVC   SVBDERCV,0(RE)      SAVE TO 1                                    
         MVC   P+30(16),=CL16'SAVE TO SVBDERCV'                                 
         GOTO1 =V(PRINTER)                                                      
         B     CKNBFX              IGNORE THIS ONE                              
*                                                                               
CKNBF30A OC    SVBDERV2,SVBDERV2                                                
         BNZ   CKNBF30B                                                         
         MVC   SVBDERV2,0(RE)      SAVE TO 2                                    
         MVC   P+30(16),=CL16'SAVE TO SVBDERV2'                                 
         GOTO1 =V(PRINTER)                                                      
         B     CKNBFX              IGNORE THIS ONE                              
*                                                                               
CKNBF30B OC    SVBDERV3,SVBDERV3                                                
         BNZ   CKNBF30C                                                         
         MVC   SVBDERV3,0(RE)      SAVE TO 3                                    
         MVC   P+30(16),=CL16'SAVE TO SVBDERV3'                                 
         GOTO1 =V(PRINTER)                                                      
         B     CKNBFX              IGNORE THIS ONE                              
*                                                                               
CKNBF30C EQU   *                                                                
         MVC   P+30(16),=CL16'SAVE TO SVBDERV4'                                 
         OC    SVBDERV4,SVBDERV4                                                
         BZ    *+10                                                             
         MVC   P+30(17),=CL17'OVERRIDE SVBDERV4'                                
         MVC   SVBDERV4,0(RE)      ALWAYS SAVE TO 4 IF 1&2&3 ARE USED           
         GOTO1 =V(PRINTER)                                                      
         B     CKNBFX              IGNORE THIS ONE                              
*                                                                               
CKNBF50  ST    R4,AXMTNTRY         STORE A(THIS TABLE ENTRY)                    
         MVC31 XMTNTRY,0(R4)       SAVE THE TABLE ENTRY                         
*                                                                               
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
*                                                                               
         MVC   EXMTTBLE,AXMTTBL                                                 
         MVC   ETBLNTRY,XMTDSKAD                                                
*                                                                               
         CLC   EVTYPE,=C'RCV'                                                   
         BNE   *+12                                                             
         MVI   EACTION,EACTDLVQ                                                 
         B     CKNBF70                                                          
*                                                                               
         CLC   EVTYPE,=C'SND'                                                   
         BNE   *+12                                                             
         MVI   EACTION2,EACTNTFQ                                                
         B     CKNBF70                                                          
*                                                                               
         CLC   EVTYPE,=C'DEL'                                                   
         BNE   *+12                                                             
         MVI   EACTION2,EACTDELQ                                                
         B     CKNBF70                                                          
*                                                                               
CKNBF70  OI    EACTION,EACTFILQ    DISK ADDRESS BEING PASSED                    
         MVC   EMAJORNM,=A(MAJORNAM)                                            
         MVC   EABDEDTA,=A(DOCID)                                               
         MVC   EDAYNUM,EVDAY       DAY                                          
         MVC   ETIME,EVTIME        TIME                                         
         MVC   EADTAMGR,DATAMGR                                                 
         GOTO1 =V(POSTSENT)                                                     
         BNE   CKNBF90             COULD NOT POST EDICT FILE                    
         DROP  R1                                                               
*                                  EDICT FILE DISK ADDRESS                      
         GOTO1 =V(HEXOUT),DMCB,XMTDSKAD,P+30,4,=C'TOG'                          
         PRNT  EDICTFILEPOSTED,PRINT=ALWAYS                                     
         B     CKNBFX                                                           
*                                                                               
CKNBF90  MVC   P+30(13),=C'*** ERROR ***'                                       
         PRNT  CANTPOST,PRINT=ALWAYS                                            
*                                                                               
CKNBFX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB,R2                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
PROMINB  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   OVERSIZE,C'Y'                                                    
         BE    PMI20                                                            
         BRAS  RE,EZINB                                                         
         B     PMIX                                                             
*                                                                               
PMI20    MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(40),=C'CAN''T PROCESS THIS INB MESSAGE, TOO BIG!'           
         GOTO1 =V(PRINTER)                                                      
*                                  EMAIL YYUN ALSO, 4/21/06                     
         BRAS  RE,CHKOPM                                                        
         BNE   PMIX                                                             
         GOTO1 DATAMGR,DMCB,=C'OPMSG',('OPMSG4Q',OPMSG4)                        
*                                                                               
PMIX     XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
EZINB    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* THIS ROUTINE READS AN EASYLINK INBOUND MESSAGE AND WRITES IT                  
* TO THE RECEIVER'S PRINT QUEUE.                                                
*                                                                               
         PRNT  EASYLINKINBOUND,PRINT=ALWAYS                                     
         MVI   ENQPQFLG,C'N'       ASSUME WE WON'T ENQ PRINT QUEUE              
*                                                                               
         XC    R,R                 PRINT LINE FOR PRTQUE CALLS                  
*                                                                               
         L     R3,=A(MQBUFF)                                                    
         AHI   R3,5                BUMP PAST 'INB(CR)(LF)'                      
*                                                                               
         LA    R1,DMCB                                                          
         USING DSTPARMD,R1                                                      
         ST    R3,DSTKEY                                                        
         MVC   DSTCNTRL,MAJORNAM+5                                              
         MVC   DSTTBL,SDSTTBLE                                                  
         MVC   DSTMAJNM,SMAJORNM                                                
         GOTO1 =V(FINDDEST)                                                     
         BNE   *+14                NOT THERE                                    
         MVC   HALF,DESTUIDN-DESTTABD(R1)  USERID                               
         B     EZINB20                                                          
*                                                                               
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(27),=C'MAILBOX XXXXXXXX IS UNKNOWN'                         
         MVC   P+38(8),0(R3)       PUT MAILBOX NO. IN PRINT LINE                
         GOTO1 =V(PRINTER)                                                      
         B     EZINBDEQ            DON'T POST THIS MESSAGE                      
*                                                                               
EZINB20  XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         USING UKRECD,R2                                                        
         MVC   UKSRCID,HALF        USERID                                       
         GOTO1 DATAMGR,DMCB,(0,=C'GFILE'),=C'PRTQUE',WORK,R,A(CIREC)            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PRTQNAME,UKUSRINF   PRINT QUEUE NAME                             
*========                                                                       
         MVC   PRTQNAME,=C'PRTQU'  *** UNTIL DMENQDEQ IS RE-ENTRANT ***         
*========                                                                       
         DROP  R2                                                               
*                                                                               
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,PRTQNAME,E,5)  ENQUEUE THE PRINT QUEUE                 
         MVI   ENQPQFLG,C'Y'       REMEMBER WE ENQUEUED                         
*                                                                               
         LA    RE,R                                                             
         USING PQPLD,RE                                                         
         MVC   QLSRCID,HALF        USERID                                       
         MVI   QLEXTRA,X'FF'       OPEN PRINT QUEUE REPORT                      
         MVC   QLSUBID,=C'WU '                                                  
         MVI   QLCLASS,C'K'                                                     
         MVI   QLLINET,X'C0'                                                    
         MVI   QLLINEW,132                                                      
         MVC   QLRETNL,=H'36'                                                   
         MVC   QLRETND,=H'2'                                                    
         MVC   QLDESC,=C'EASYLINK   '                                           
         DROP  RE                                                               
*                                                                               
         L     RE,=A(CIREC)                                                     
         XCEFL (RE),14336          CLEAR PQ C/I BUFFER                          
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    *+14                                                             
         MVC   P+30(28),=C'COULD NOT OPEN PRTQUE REPORT'                        
         B     EZCLOPUR                                                         
*                                                                               
         MVI   R,X'40'                                                          
         MVC   R+1(L'R-1),R                                                     
         MVI   R,X'89'             FORM FEED                                    
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    *+14                                                             
         MVC   P+30(29),=C'COULD NOT WRITE FIRST PQ LINE'                       
         B     EZCLOPUR            ERROR ON FIRST LINE - PROBABLY DISK          
*                                                                               
         LA    R3,23(R3)           MAILBOX+(CRLF)+LEDGER+(CRLF)                 
         XC    HALF,HALF           CLEAR LINE COUNTER                           
*                                                                               
EZINB30  LH    R6,HALF             CURRENT NUMBER OF LINES                      
         CH    R6,=H'56'                                                        
         BNH   EZINB40                                                          
         MVI   R,X'40'                                                          
         MVC   R+1(L'R-1),R                                                     
         MVI   R,X'89'             EJECT PAGE EVERY 56 LINES                    
         SR    R6,R6                                                            
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    EZINB40                                                          
         MVC   P+30(26),=C'COULD NOT WRITE PAGE EJECT'                          
         B     EZCLOPUR            ERROR ON FIRST LINE - PROBABLY DISK          
*                                                                               
EZINB40  LA    R6,1(R6)                                                         
         STH   R6,HALF             INCREMENT NO. OF LINES                       
         MVI   R,X'40'                                                          
         MVC   R+1(L'R-1),R                                                     
         MVI   R,X'09'                                                          
         ST    R3,FULL             START OF TEXT TO BE PRINTED                  
         LA    R4,R+16             START OF PRINT LINE                          
*                                                                               
EZINB50  TRT   0(256,R3),TRINBND   LOOK FOR NON-PRINTING CHARACTERS             
         LR    R6,R1                                                            
         CLC   0(8,R6),ENDMMMM                                                  
         BE    EZINB100            END OF MESSAGE                               
         CLI   0(R6),ETX                                                        
         BE    EZINB100            END OF MESSAGE                               
         CLI   0(R6),ETB                                                        
         BE    EZINB80             GET THE NEXT BUFFER                          
         CLC   LF,0(R6)            LINE FEED?                                   
         BNE   *+12                                                             
         LA    R3,1(R3)            YES - BUMP PAST IT                           
         B     EZINB60                                                          
*                                                                               
         CLC   CRLF,0(R6)                                                       
         BE    *+16                                                             
         MVI   0(R6),C' '          REPLACE INVALID CHARACTER WITH BLANK         
         LA    R3,1(R6)                                                         
         B     EZINB50                                                          
*                                                                               
         L     R3,FULL                                                          
         SR    R6,R3               LENGTH OF STRING TO BE PRINTED               
         LTR   R6,R6               ANY DATA TO PRINT?                           
         BNZ   *+12                YES                                          
         LA    R3,2(R3)            NO - BUMP PAST CRLF                          
         B     EZINB60                                                          
*                                                                               
         LA    R2,0(R6,R4)                                                      
         LA    R0,R+133                                                         
         CR    R2,R0               LINE IS TOO LONG?                            
         BNH   *+14                                                             
         MVC   P+30(22),=C'PRINT LINE IS TOO LONG'                              
         B     EZCLOPUR            YES                                          
*                                                                               
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)       MOVE STRING INTO PRINT LINE                  
         LA    R3,3(R6,R3)         LENGTH OF STRING + BCTR + CRLF               
*                                                                               
EZINB60  CLC   =C'..NP             ',R+16     PROCESS NEW PAGE REQUEST          
         BNE   EZINB70                                                          
         MVI   R,X'40'                                                          
         MVC   R+1(L'R-1),R                                                     
         XC    HALF,HALF                                                        
         MVI   R,X'89'                                                          
*                                                                               
EZINB70  GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    EZINB30                                                          
         MVC   P+30(27),=C'COULD NOT WRITE PRTQUE LINE'                         
         B     EZCLOPUR                                                         
*                                                                               
EZINB80  L     R3,FULL                                                          
         SR    R6,R3               LENGTH OF STRING TO BE PRINTED               
         LTR   R6,R6               ANYTHING TO SAVE?                            
         BZ    EZINB90             NO                                           
*                                                                               
         LA    R2,0(R6,R4)                                                      
         LA    R0,R+133                                                         
         CR    R2,R0               LINE IS TOO LONG?                            
         BNH   *+14                                                             
         MVC   P+30(22),=C'PRINT LINE IS TOO LONG'                              
         B     EZCLOPUR            YES                                          
*                                                                               
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)       MOVE STRING INTO PRINT LINE                  
         LA    R4,1(R6,R4)         LENGTH OF STRING + BCTR                      
*                                                                               
EZINB90  EQU   *                                                                
         B     EZINB50                                                          
*                                                                               
         XC    R,R                                                              
         MVI   R,X'FE'             ERASE PARTIALLY GENERATED REPORT             
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         MVC   P+30(25),=C'MUST ERASE PARTIAL REPORT'                           
         B     EZCLOPUR                                                         
*                                                                               
EZINB100 L     R3,FULL             WRAP UP                                      
         SR    R6,R3               LENGTH OF FINAL LINE                         
         LTR   R6,R6               ANYTHING TO PRINT?                           
         BZ    EZINB110            NO                                           
*                                                                               
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   R+16(0),0(R3)       MOVE STRING INTO PRINT LINE                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    EZINB110                                                         
         MVC   P+30(27),=C'COULD NOT WRITE PRTQUE LINE'                         
         B     EZCLOPUR                                                         
*                                                                               
EZINB110 PRNT  ENDINBOUND,PRINT=ALWAYS                                          
         MVI   R,X'FF'             CLOSE REPORT                                 
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    EZINBDEQ                                                         
         MVC   P+30(29),=C'COULD NOT CLOSE PRTQUE REPORT'                       
*                                                                               
EZCLOPUR MVC   P+11(13),=C'*** ERROR ***'                                       
         GOTO1 =V(PRINTER)                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'CLO/PUR'),=C'PRTQUE',0,R,A(CIREC)             
*                                                                               
EZINBDEQ CLI   ENQPQFLG,C'Y'       REMEMBER WE ENQUEUED                         
         BNE   EZINBX                                                           
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,PRTQNAME,5)    DEQUEUE THE PRINT QUEUE                 
*                                                                               
EZINBX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
TRINBND  DS    0XL256              CHECK OUT ANY NON-PRINTING CHAR              
*                                                                               
*                0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                               
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 00-0F                        
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 10-1F                        
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 20-2F                        
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 30-3F                        
         DC    X'00FFFFFFFFFFFFFFFFFF000000000000' 40-4F                        
         DC    X'00FFFFFFFFFFFFFFFFFF000000000000' 50-5F                        
         DC    X'0000FFFFFFFFFFFFFFFF000000000000' 60-6F                        
         DC    X'FFFFFFFFFFFFFFFFFFFF000000000000' 70-7F                        
         DC    X'FF000000000000000000FFFFFFFFFFFF' 80-8F                        
         DC    X'FF000000000000000000FFFFFFFFFFFF' 90-9F                        
         DC    X'FFFF0000000000000000FFFFFFFFFFFF' A0-AF                        
         DC    X'00000000000000000000FFFFFFFFFFFF' B0-BF                        
         DC    X'FF000000000000000000FFFFFFFFFFFF' C0-CF                        
         DC    X'FF000000000000000000FFFFFFFFFFFF' D0-DF                        
         DC    X'FFFF0000000000000000FFFFFFFFFFFF' E0-EF                        
         DC    X'00000000000000000000FFFFFFFFFFFF' F0-FF                        
         EJECT                                                                  
         DROP  RB                                                               
*                                                                               
PROMADD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    MSG_ACT,MSG_ACT     ACTION                                       
         XC    MSG_REF#,MSG_REF#   REFERENCE NUMBER                             
         XC    MSG_METH,MSG_METH   TRANSMIT METHOD                              
         XC    MSG_ACC#,MSG_ACC#   EZLINK/ECN ACCOUNT NUMBER                    
         XC    MSG_BFN,MSG_BFN     BDE FILE NAME                                
         XC    MSG_DTT,MSG_DTT     DESTINATION TYPE (R/F)                       
         XC    MSG_HDR,MSG_HDR     HEADER                                       
         XC    MSG_TRN,MSG_TRN     TRN CARD                                     
         XC    MSG_MQN,MSG_MQN     MQ QUEUE ID # (CTL/MQDEF)                    
         XC    MSG_PQS,MSG_PQS     FINISHED PQ CLASS/STATUS                     
         XC    MSG_DST,MSG_DST     DESTINATION NAME                             
         XC    MSG_FDST,MSG_FDST   FORMATTED DESTINATION NAME                   
         XC    MSG_PQRT,MSG_PQRT   PQ REPORT TYPE                               
         XC    MSG_BSID,MSG_BSID   BDE SENDER ID                                
*                                                                               
         L     R3,=A(MQBUFF)                                                    
         MVC   MSG_ACT,0(R3)       ACTION                                       
*                                                                               
         AHI   R3,L'MSG_ACT        JUMP TO NEXT FIELD                           
         CLC   CRLF,0(R3)                                                       
         BE    *+12                                                             
         AHI   R3,1                                                             
         B     *-14                                                             
         AHI   R3,2                                                             
*                                                                               
         MVC   MSG_REF#,0(R3)      REFERENCE NUMBER                             
*                                                                               
         CLI   MSG_REF#,C'5'       IGNORE ENCODE MQ MESSAGE                     
         BNE   PMADD10                                                          
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(33),=C'INVALID CUSTOMER REFERENCE NUMBER'                   
         GOTO1 =V(PRINTER)                                                      
         B     PMADDX                                                           
PMADD10  EQU   *                                                                
*                                                                               
         AHI   R3,L'MSG_REF#       JUMP TO NEXT FIELD                           
         CLC   CRLF,0(R3)                                                       
         BE    *+12                                                             
         AHI   R3,1                                                             
         B     *-14                                                             
         AHI   R3,2                                                             
*                                                                               
         MVC   MSG_METH,0(R3)      TRANSMIT METHOD                              
*                                                                               
         AHI   R3,L'MSG_METH       JUMP TO NEXT FIELD                           
         CLC   CRLF,0(R3)                                                       
         BE    *+12                                                             
         AHI   R3,1                                                             
         B     *-14                                                             
         AHI   R3,2                                                             
*                                                                               
         CLI   MSG_METH,C'P'       METH=BDF?                                    
         BE    PMADD12                                                          
         MVC   MSG_ACC#,0(R3)      EZLINK/ECN ACCOUNT #                         
         AHI   R3,L'MSG_ACC#       JUMP TO NEXT FIELD                           
         B     PMADD15                                                          
*                                                                               
PMADD12  EQU   *                                                                
         MVC   MSG_BFN,0(R3)       BDE FILE NAME                                
         AHI   R3,L'MSG_BFN        JUMP TO NEXT FIELD                           
*                                                                               
PMADD15  EQU   *                                                                
         CLC   CRLF,0(R3)                                                       
         BE    *+12                                                             
         AHI   R3,1                                                             
         B     *-14                                                             
         AHI   R3,2                                                             
*                                                                               
         MVC   MSG_DTT,0(R3)       DESTINATION TYPE                             
*                                                                               
         AHI   R3,L'MSG_DTT        JUMP TO NEXT FIELD                           
         CLC   CRLF,0(R3)                                                       
         BE    *+12                                                             
         AHI   R3,1                                                             
         B     *-14                                                             
         AHI   R3,2                                                             
*                                                                               
         MVC   MSG_HDR,0(R3)       HEADER                                       
*                                                                               
         AHI   R3,L'MSG_HDR        JUMP TO NEXT FIELD                           
         CLC   CRLF,0(R3)                                                       
         BE    *+12                                                             
         AHI   R3,1                                                             
         B     *-14                                                             
         AHI   R3,2                                                             
*                                                                               
         MVC   MSG_TRN,0(R3)       TRN CARD                                     
*                                                                               
         AHI   R3,L'MSG_TRN        JUMP TO NEXT FIELD                           
         CLC   CRLF,0(R3)                                                       
         BE    *+12                                                             
         AHI   R3,1                                                             
         B     *-14                                                             
         AHI   R3,2                                                             
*                                                                               
         CLC   CRLF,0(R3)          THIS FIELD IS OPTIONAL                       
         BE    PMADD20             NOTHING - SKIP IT                            
         MVC   MSG_MQN,0(R3)       MQ QUEUE ID #                                
*                                                                               
         AHI   R3,L'MSG_MQN        JUMP TO NEXT FIELD                           
         CLC   CRLF,0(R3)                                                       
         BE    *+12                                                             
         AHI   R3,1                                                             
         B     *-14                                                             
PMADD20  AHI   R3,2                                                             
*                                                                               
         CLC   CRLF,0(R3)          THIS FIELD IS OPTIONAL                       
         BE    PMADD30             NOTHING - SKIP IT                            
         MVC   MSG_PQS,0(R3)       FINISHED PQ CLASS/STATUS                     
*                                                                               
         AHI   R3,L'MSG_PQS        JUMP TO NEXT FIELD                           
         CLC   CRLF,0(R3)                                                       
         BE    *+12                                                             
         AHI   R3,1                                                             
         B     *-14                                                             
PMADD30  AHI   R3,2                                                             
*                                                                               
         MVC   MSG_DST,0(R3)       DESTINATION NAME                             
*                                                                               
         AHI   R3,L'MSG_DST        JUMP TO NEXT FIELD                           
         CLC   CRLF,0(R3)                                                       
         BE    *+12                                                             
         AHI   R3,1                                                             
         B     *-14                                                             
         AHI   R3,2                                                             
*                                                                               
         MVC   MSG_FDST,0(R3)      FORMATTED DESTINATION NAME                   
*                                                                               
         AHI   R3,L'MSG_FDST       JUMP TO NEXT FIELD                           
         CLC   CRLF,0(R3)                                                       
         BE    *+12                                                             
         AHI   R3,1                                                             
         B     *-14                                                             
         AHI   R3,2                                                             
*                                                                               
         MVC   MSG_PQRT,0(R3)      PQ REPORT TYPE                               
*                                                                               
         AHI   R3,L'MSG_PQRT       JUMP TO NEXT FIELD                           
         CLC   CRLF,0(R3)                                                       
         BE    *+12                                                             
         AHI   R3,1                                                             
         B     *-14                                                             
         AHI   R3,2                                                             
*                                                                               
         MVC   MSG_BSID,0(R3)      BDE SENDER ID                                
*                                                                               
         AHI   R3,L'MSG_BSID       JUMP TO NEXT FIELD                           
         CLC   CRLF,0(R3)                                                       
         BE    *+12                                                             
         AHI   R3,1                                                             
         B     *-14                                                             
         AHI   R3,2                                                             
*                                                                               
         MVC   P(L'MSG_ACT),MSG_ACT                                             
         GOTO1 =V(PRINTER)                                                      
         MVC   P(L'MSG_REF#),MSG_REF#                                           
         GOTO1 =V(PRINTER)                                                      
         MVC   P(L'MSG_METH),MSG_METH                                           
         GOTO1 =V(PRINTER)                                                      
         MVC   P(L'MSG_ACC#),MSG_ACC#                                           
         MVC   P+L'MSG_ACC#(1),=C'/'                                            
         MVC   P+L'MSG_ACC#+3(L'MSG_BFN),MSG_BFN                                
         GOTO1 =V(PRINTER)                                                      
         MVC   P(L'MSG_DTT),MSG_DTT                                             
         GOTO1 =V(PRINTER)                                                      
         MVC   P(L'MSG_HDR),MSG_HDR                                             
         GOTO1 =V(PRINTER)                                                      
         MVC   P(L'MSG_TRN),MSG_TRN                                             
         GOTO1 =V(PRINTER)                                                      
         MVC   P(L'MSG_MQN),MSG_MQN                                             
         GOTO1 =V(PRINTER)                                                      
         MVC   P(L'MSG_PQS),MSG_PQS                                             
         GOTO1 =V(PRINTER)                                                      
         MVC   P(L'MSG_DST),MSG_DST                                             
         GOTO1 =V(PRINTER)                                                      
         MVC   P(L'MSG_FDST),MSG_FDST                                           
         GOTO1 =V(PRINTER)                                                      
         MVC   P(L'MSG_PQRT),MSG_PQRT                                           
         GOTO1 =V(PRINTER)                                                      
         MVC   P(L'MSG_BSID),MSG_BSID                                           
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   MSG_REF#,C'D'       GENERATED DURING DR TEST?                    
         BNE   PMADD50             NO -- CONTINUE TO BUILD RECORD               
         CLI   DRTEST,C'Y'         IS THIS THE DR TEST?                         
         BE    PMADD50             YES -- ADD IT                                
*                                  NO  -- SKIP IT                               
         MVC   P+11(14),=C'***SKIP_MSG***'                                      
         MVC   P+30(L'MSG_REF#),MSG_REF#                                        
         GOTO1 =V(PRINTER)                                                      
*                                  EMAIL YYUN FOR NOW, 4/10/06                  
         MVC   OPMSG3C,MSG_REF#                                                 
         BRAS  RE,CHKOPM                                                        
         BNE   PMADDX                                                           
         GOTO1 DATAMGR,DMCB,=C'OPMSG',('OPMSG3Q',OPMSG3)                        
         B     PMADDX                                                           
*                                                                               
PMADD50  BRAS  RE,BLDREC                                                        
*                                                                               
         CLC   MSG_REF#,SVBDESND+3    MATCH REF#?                               
         BNE   *+16                   NO                                        
         LA    R1,SVBDESND                                                      
         BRAS  RE,PROMBSRM            REPROCESS THE LEFTOVER SND MSG            
         B     PMADD60                                                          
*                                                                               
         CLC   MSG_REF#,SVBDESD2+3    MATCH REF#?                               
         BNE   *+16                   NO                                        
         LA    R1,SVBDESD2                                                      
         BRAS  RE,PROMBSRM            REPROCESS THE LEFTOVER SND MSG            
         B     PMADD60                                                          
*                                                                               
         CLC   MSG_REF#,SVBDESD3+3    MATCH REF#?                               
         BNE   *+16                   NO                                        
         LA    R1,SVBDESD3                                                      
         BRAS  RE,PROMBSRM            REPROCESS THE LEFTOVER SND MSG            
         B     PMADD60                                                          
*                                                                               
         CLC   MSG_REF#,SVBDESD4+3    MATCH REF#?                               
         BNE   PMADD60                NO                                        
         LA    R1,SVBDESD4                                                      
         BRAS  RE,PROMBSRM            REPROCESS THE LEFTOVER SND MSG            
*                                                                               
PMADD60  EQU   *                                                                
         CLC   MSG_REF#,SVBDERCV+3    MATCH REF#?                               
         BNE   *+16                   NO                                        
         LA    R1,SVBDERCV                                                      
         BRAS  RE,PROMBSRM            REPROCESS THE LEFTOVER RCV MSG            
         B     PMADD70                                                          
*                                                                               
         CLC   MSG_REF#,SVBDERV2+3    MATCH REF#?                               
         BNE   *+16                   NO                                        
         LA    R1,SVBDERV2                                                      
         BRAS  RE,PROMBSRM            REPROCESS THE LEFTOVER RCV MSG            
         B     PMADD70                                                          
*                                                                               
         CLC   MSG_REF#,SVBDERV3+3    MATCH REF#?                               
         BNE   *+16                   NO                                        
         LA    R1,SVBDERV3                                                      
         BRAS  RE,PROMBSRM            REPROCESS THE LEFTOVER RCV MSG            
         B     PMADD70                                                          
*                                                                               
         CLC   MSG_REF#,SVBDERV4+3    MATCH REF#?                               
         BNE   PMADD70                NO                                        
         LA    R1,SVBDERV4                                                      
         BRAS  RE,PROMBSRM            REPROCESS THE LEFTOVER RCV MSG            
*                                                                               
PMADD70  EQU   *                                                                
PMADDX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
BLDREC   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,=A(MQBUFF)                                                    
         L     RF,=A(MQBUFFL)      CLEAR BUFFER                                 
         XCEFL                                                                  
*                                                                               
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(3,TODAY3)                                 
*                                                                               
*        OC    MSG_METH,MSG_METH                                                
*        BNZ   *+8                                                              
*        MVI   MSG_METH,C'E'       DEFAULT TO EASYLINK METH                     
*                                                                               
         LA    R4,EDICTREC         BUILD AN EDICT RECORD                        
         USING EDFILD,R4                                                        
         XC    EDICTREC,EDICTREC   CLEAR THE RECORD                             
*                                                                               
         MVC   EDFMON,TODAY3+1     MONTH                                        
         MVC   EDFMETH,MSG_METH    LOGICAL REPORT TRANSMISSION METHOD           
*                                                                               
         MVC   EDFSYS,MSG_TRN+6    SYSTEM                                       
         MVC   EDFTYPE,MSG_TRN+8   RECORD TYPE                                  
         MVC   EDFAPPL,MSG_TRN+15  APPLICATION AREA                             
*                                                                               
*                                  PQ USERID#, SUBID, REPORT REF#               
         GOTO1 =V(HEXIN),DMCB,MSG_REF#+02,EDFPQUID,4,0                          
         MVC   EDFPQSUB,MSG_REF#+6                                              
         GOTO1 =V(HEXIN),DMCB,MSG_REF#+09,EDFPQREF,4,0                          
*                                                                               
*                                  REP CREATE DATE (COMPRSD) & TIME             
         GOTO1 =V(HEXIN),DMCB,MSG_REF#+13,EDFPQDAT,4,0                          
         GOTO1 =V(HEXIN),DMCB,MSG_REF#+17,EDFPQTIM,4,0                          
*                                                                               
         CLI   MSG_HDR+67,C'D'     IS IT A DARE REPORT (FOR EASYLINK)?          
         BNE   *+8                                                              
         OI    EDFFLAGS,EDFDAREZ   YES                                          
*                                                                               
*                                  LOGICAL REP SEQ# AND DEST SEQ#               
         GOTO1 =V(HEXIN),DMCB,MSG_REF#+21,EDFPQSEQ+0,4,0                        
         GOTO1 =V(HEXIN),DMCB,MSG_REF#+25,EDFPQDSS+1,2,0                        
*                                                                               
         MVC   EDFDEST,MSG_DST     START WITH UNFORMATTED DESTINATION           
         CLI   MSG_FDST,C' '                                                    
         BNH   *+10                                                             
         MVC   EDFDEST,MSG_FDST    WE WERE GIVEN THE FORMATTED DEST.            
*                                                                               
         GOTO1 =V(HEXIN),DMCB,MSG_PQRT,EDFPQTYP,2,0                             
*                                                                               
         CLI   MSG_BSID,C' '                                                    
         BNH   *+10                                                             
         MVC   EDFBDESC,MSG_BSID                                                
*                                                                               
         TM    EDFPQDSS+1,X'80'    LAST DESTINATION?                            
         BNO   *+8                                                              
         OI    EDFSTAT,EDFSTLST    MARK LAST DEST. IN A LOGICAL REPORT          
         NI    EDFPQDSS+1,X'FF'-X'80'   TURN OFF THIS BIT                       
*                                                                               
         MVC   EDFDSTTY,MSG_DTT    DESTINATION TYPE                             
*                                                                               
         MVC   EDFCPQCL,MSG_PQS+15 PQ CLASS AFTER SENT                          
         CLC   EDFCPQCL,SPACES     ANY PQ CLASS FLAG?                           
         BNH   BREC20              NO                                           
*                                  PQ STATUS AFTER SENT                         
         MVI   EDFCPQST,X'80'      PQ STATUS = ACTIVE AS DEFAULT                
         CLI   MSG_PQS+16,C' '                                                  
         BNH   BREC20              NO STATUS GIVEN, ASSUME ACTIVE               
         GOTO1 =V(HEXIN),DMCB,MSG_PQS+16,EDFCPQST,2                             
*                                                                               
BREC20   MVC   EDFMQAPP,MSG_MQN+15 MQDEF APPL NAME(FACPAK NAME, CL8)            
*                                                                               
         CLC   EDFMQAPP,SPACES     ANY MQ QUEUE INFO GIVEN?                     
         BNH   BREC35              NO                                           
*                                                                               
         LAY   RE,FACIDTAB                                                      
BREC30   CLC   EDFMQAPP(4),0(RE)                                                
         BNE   *+14                                                             
         MVC   EDFMQAPP(4),4(RE)   USE THE 4 CHAR FACPAK ID                     
         B     BREC35                                                           
*                                                                               
         AHI   RE,L'FACIDTAB                                                    
         CLI   0(RE),X'FF'                                                      
         BNE   BREC30                                                           
*                                                                               
         MVC   P+11(33),=C'*** ERROR *** UNKNOWN FACPAK NAME'                   
         GOTO1 =V(PRINTER)                                                      
BREC35   EQU   *                                                                
*                                                                               
         MVC   EDFMQLAB,MSG_MQN+24 MQ MESSAGE LABEL FOR FACPAK, CL6             
*                                                                               
         CLI   MSG_MQN+31,C' '     ANY GIVEN MQ QUEUE #?                        
         BNE   *+14                YES                                          
         MVC   EDFMQID,=XL2'0001'  NO - SET TO 1                                
         B     BREC40                                                           
*                                                                               
         GOTO1 =V(NUMVAL),DMCB,MSG_MQN+31,(2,0)                                 
         CLI   DMCB,0                                                           
         BNE   BREC40              INVALID, DON'T SAVE MQ QUEUE #               
         L     R1,DMCB+4                                                        
         STCM  R1,3,EDFMQID                                                     
BREC40   EQU   *                                                                
*                                                                               
         MVC   HALF,EDFPQUID                                                    
         BRAS  RE,GETUID                                                        
*                                                                               
         CLI   MSG_METH,C'E'       EASYLINK                                     
         BE    BREC50                                                           
         CLI   MSG_METH,C'I'       ECN                                          
         BE    BREC60                                                           
         CLI   MSG_METH,C'P'       BDF                                          
         BE    BREC65                                                           
         B     BREC70                                                           
*                                  GET THE EASYLINK MAILBOX NUMBER              
BREC50   MVC   DUB(6),MSG_ACC#+2   MAILBOX NO. (WITHOUT '62')                   
         PACK  FULL,DUB(6)                                                      
         L     R0,FULL                                                          
         SRL   R0,4                SHIFT OUT SIGN                               
         STCM  R0,7,EDFEZBOX       SAVE MAILBOX NO. (WITHOUT '62')              
         B     BREC70                                                           
*                                  GET THE ECN ACCOUNT #                        
BREC60   MVC   EDFECNID,MSG_ACC#                                                
         B     BREC70                                                           
*                                                                               
BREC65   MVC   EDFBDFIL,MSG_BFN    SAVE BDF FILE NAME                           
*                                                                               
BREC70   EQU   *                                                                
         OI    EDFSTAT,EDFSTSNT+EDFSTPRT    REPORT MARKED SENT & PRTD           
*                                                                               
*                                  GET SENT DAY AND TIME                        
*        GOTO1 =V(HEXIN),DMCB,MSGDESC_PUTDATE+6,EDFSNTDY,2,0                    
*        GOTO1 =V(HEXIN),DMCB,MSGDESC_PUTTIME,EDFSNTIM,4,0                      
*                                                                               
         XC    TIMEWRK,TIMEWRK                                                  
         PACK  TIMEWRK(3),MSGDESC_PUTTIME(5)                                    
         MVI   TIMEWRK+2,X'00'     CLEAR WHATEVER X'?F' AFTER PACK              
         PACK  TIMEWRK+8(5),MSGDESC_PUTDATE(9)                                  
         MVI   TIMEWRK+12,X'00'    CLEAR WHATEVER X'?F' AFTER PACK              
*ADJUST THE TIME                                                                
         CONVTOD  CONVVAL=TIMEWRK,TODVAL=DUB,                          +        
               TIMETYPE=DEC,DATETYPE=YYYYMMDD,OFFSET=TIMEADJ                    
         STCKCONV STCKVAL=DUB,CONVVAL=TIMEWRK,                         +        
               TIMETYPE=DEC,DATETYPE=YYYYMMDD                                   
*                                                                               
         MVC   EDFSNTDY,TIMEWRK+8+3      DD                                     
         MVC   EDFSNTIM,TIMEWRK          HHMM                                   
*                                                                               
         CLC   =C'JNK',MSG_ACT                                                  
         BNE   BREC80                                                           
         OI    EDFSTAT,EDFSTJNK    REPORT HAS BEEN MARKED UNSENDABLE            
         NI    EDFSTAT,X'FF'-EDFSTSNT         NOT SENT                          
         DROP  R4                                                               
*                                                                               
BREC80   EQU   *                                                                
         L     RE,=A(MQBUFF)                                                    
         MVC   0(L'MSG_ACT,RE),MSG_ACT                                          
         AHI   RE,L'MSG_ACT                                                     
         MVC   0(2,RE),CRLF                                                     
         AHI   RE,2                                                             
         MVC   0(L'MSG_REF#,RE),MSG_REF#                                        
         AHI   RE,L'MSG_REF#                                                    
         MVC   0(2,RE),CRLF                                                     
         AHI   RE,2                                                             
         MVC   0(4,RE),=X'00000000'      FOR ADD/JNK, DISK ADDRESS=0            
         AHI   RE,4                                                             
         MVC   0(2,RE),CRLF                                                     
         AHI   RE,2                                                             
         MVC   0(L'EDICTREC,RE),EDICTREC                                        
         AHI   RE,L'EDICTREC                                                    
         MVC   0(2,RE),CRLF                                                     
         AHI   RE,2                                                             
*                                                                               
         CLC   MSG_REF#+1(1),MAJORNAM+5  MATCH ON EDICT (ADV/REP)?              
         BE    BREC90              YES -- OKAY                                  
*                                  NO  -- DLN CAME IN OVER WRONG LINE           
         MVC   P+11(13),=CL13'*** ERROR ***'                                    
         MVC   P+30(25),=CL25'INVALID REFERENCE NUMBER'                         
         GOTO1 =V(PRINTER)                                                      
         PRNT  SKIP_THIS_MSG,PRINT=ALWAYS                                       
         B     BRECX                                                            
*                                                                               
BREC90   EQU   *                                                                
*                                                                               
* CHECK IF THIS REPORT ALREADY IN XMTTABLE, YES, SKIP THIS ONE.                 
         LA    R4,EDICTREC                                                      
         USING EDFILD,R4                                                        
         XC    XMTNTRY,XMTNTRY                                                  
         LA    RE,XMTNTRY                                                       
         USING XMTTABLD,RE                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,EDFPQUID       PQ REPORT SENDING USERID                     
         STCM  R1,3,XMTUSRID                                                    
         D     R0,NUMPQS                                                        
         AHI   R0,1                                                             
         STC   R0,XMTPRTQ          PRINT QUEUE NUMBER                           
         MVI   XMTSOURC,XMTSRCPQ   PRINT QUEUE REPORT                           
         MVC   XMTSUBID,EDFPQSUB   PQ REPORT SUB-ID                             
         MVC   XMTREFNO,EDFPQREF   PQ REPORT REFERENCE NUMBER                   
         MVC   XMTCRDAT,EDFPQDAT   PQ REPORT CREATION DATE - CMPRSD             
         MVC   XMTCRTIM,EDFPQTIM   PQ REPORT CREATION TIME                      
         MVC   XMTLOGNO,EDFPQSEQ   PQ REPORT LOGICAL REPORT NUMBER              
         MVC   XMTDSTNO,EDFPQDSS   PQ REPORT LOGICAL REPORT DEST. NUM.          
         DROP  RE,R4                                                            
*                                                                               
         PRNT  BEFORE_ADD_XMT,PRINT=ALWAYS                                      
*                                                                               
         LA    R9,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R9)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(9),E,8)  ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         SAM31                                                                  
         L     RF,AXMTTBL                                                       
         AHI   RF,-8               RF = A(ENTRIES#)                             
         MVC   DMCB+8(4),0(RF)     NUMBER OF ENTRIES IN TABLE                   
         AHI   RF,4                MAX # TABLE ENTRY                            
         MVC   DMCB+20(4),0(RF)    SET THE 6TH PARM                             
         GOTO1 =V(BINSRCH),DMCB,XMTNTRY,AXMTTBL,,XMTTBLQ,XMTKEYQ,0              
         SAM24                                                                  
*                                                                               
         LA    R9,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R9)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(9),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
         TM    DMCB,X'80'          WAS THIS REPORT ALREADY IN TABLE?            
         BO    BREC100             NO - OK, ADD TO EDICT FILE+XMTTABLE          
*                                                                               
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(20),=C'DUPLICATED PQ REPORT'                                
         GOTO1 =V(PRINTER)                                                      
         PRNT  SKIP_THIS_MSG,PRINT=ALWAYS                                       
         B     BRECX                                                            
*                                                                               
*                                                                               
BREC100  EQU   *                                                                
         PRNT  ADDEDICTRECORD,PRINT=ALWAYS                                      
         GOTO1 =V(PRNTBL),DMCB,0,EDICTREC,C'DUMP',256,=C'1D'                    
         GOTO1 EDCTADD,DMCB,MAJORNAM,EDICTREC,0                                 
         MVC   FULL,DMCB+8         RETURNED DISK ADDRESS                        
         GOTO1 =V(HEXOUT),DMCB,FULL,P+30,4,=C'TOG'                              
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         PRNT  EDICTRECORDADDED,PRINT=ALWAYS                                    
*                                                                               
         LA    R4,EDICTREC                                                      
         USING EDFILD,R4                                                        
         XC    XMTNTRY,XMTNTRY                                                  
         LA    RE,XMTNTRY                                                       
         USING XMTTABLD,RE                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,EDFPQUID       PQ REPORT SENDING USERID                     
         STCM  R1,3,XMTUSRID                                                    
         D     R0,NUMPQS                                                        
         AHI   R0,1                                                             
         STC   R0,XMTPRTQ          PRINT QUEUE NUMBER                           
         MVI   XMTSOURC,XMTSRCPQ   PRINT QUEUE REPORT                           
         MVC   XMTSUBID,EDFPQSUB   PQ REPORT SUB-ID                             
         MVC   XMTREFNO,EDFPQREF   PQ REPORT REFERENCE NUMBER                   
         MVC   XMTCRDAT,EDFPQDAT   PQ REPORT CREATION DATE - CMPRSD             
         MVC   XMTCRTIM,EDFPQTIM   PQ REPORT CREATION TIME                      
         MVC   XMTLOGNO,EDFPQSEQ   PQ REPORT LOGICAL REPORT NUMBER              
         MVC   XMTDSTNO,EDFPQDSS   PQ REPORT LOGICAL REPORT DEST. NUM.          
         MVC   XMTSTAT,EDFSTAT     REPORT STATUS                                
         MVC   XMTMETH,EDFMETH     METHOD OF TRANSMISSION                       
         MVC   XMTSYS,EDFSYS       SYSTEM                                       
         MVC   XMTTYPE,EDFTYPE     RECORD TYPE                                  
         MVC   XMTDSKAD(3),FULL    TTTTBB                                       
         MVC   XMTDSKAD+3(1),FULL+3 LOGICAL RECORD NUMBER                       
*                                                                               
         OC    EDFCPQCL,EDFCPQCL                                                
         BZ    *+8                                                              
         OI    XMTFLAGS,XMTCPQSQ                                                
         OC    EDFCPQST,EDFCPQST                                                
         BZ    *+8                                                              
         OI    XMTFLAGS,XMTCPQSQ                                                
         DROP  RE                                                               
*                                                                               
         PRNT  ADD_TO_XMTTABLE,PRINT=ALWAYS                                     
         LHI   R3,L'XMTNTRY                                                     
         GOTO1 =V(PRNTBL),DMCB,0,XMTNTRY,C'DUMP',(R3),=C'1D'                    
*                                                                               
         LA    R9,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R9)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(9),E,8)  ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         SAM31                                                                  
         L     RF,AXMTTBL                                                       
         AHI   RF,-8               RF = A(ENTRIES#)                             
         MVC   DMCB+8(4),0(RF)     NUMBER OF ENTRIES IN TABLE                   
         AHI   RF,4                MAX # TABLE ENTRY                            
         MVC   DMCB+20(4),0(RF)    SET THE 6TH PARM                             
         GOTO1 =V(BINSRCH),DMCB,XMTNTRY,AXMTTBL,,(1,XMTTBLQ),XMTKEYQ            
         L     RF,AXMTTBL                                                       
         AHI   RF,-8               NUMBER OF ENTRIES IN TABLE                   
         MVC   0(4,RF),DMCB+8      UPDATE TABLE SIZE                            
         SAM24                                                                  
*                                                                               
         TM    DMCB,X'80'          WAS THIS REPORT ALREADY IN TABLE?            
         BO    *+6                                                              
         DC    H'0'                RECORD IS DUPLICATED IN EDICT FILE           
         NI    DMCB,X'7F'          TURN OFF HIGH-ORDER BIT, THEN. . .           
         OC    DMCB(4),DMCB        . . . MAKE SURE THE RECORD WENT IN           
         BNZ   *+6                                                              
         DC    H'0'                XMITTBL IS NOT LARGE ENOUGH                  
*                                                                               
         LA    R9,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R9)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(9),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
BRECX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
GETUID   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    USERID,USERID                                                    
*                                                                               
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         LA    RF,KEY                                                           
         USING CTIREC,RF                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,HALF                                                     
         DROP  RF                                                               
*                                                                               
         LA    R9,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R9)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(9),E,6)  ENQUEUE THE CONTROL FILE                     
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,A(IO),0               
*                                                                               
         LA    R9,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R9)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(9),6)    DEQUEUE THE CONTROL FILE                     
*                                                                               
         CLI   DMCB+8,0                                                         
         BNE   GUIDX               USERID RECORD IS GONE                        
*                                                                               
         L     R1,=A(IO)                                                        
         MVI   ELCODE,CTDSCELQ     PICK UP ALPHA USERID                         
         BAS   RE,GETEL                                                         
         BNE   GUIDX               USERID NAME ELEMENT IS MISSING               
         MVC   USERID,CTDSC-CTDSCD(R1)                                          
*                                                                               
GUIDX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
ERRDFAX  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,=A(EDICTREC)                                                  
         USING EDFILD,R4                                                        
         TM    EDFFLAGS,EDFDAREZ   WAS THIS A DARE REPORT?                      
         BZ    ERRDXX                                                           
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(3,TODAY3)                                 
*                                                                               
         MVC   HALF,EDFPQUID                                                    
         BRAS  RE,GETUID           GET USERID                                   
*                                                                               
         XC    NEWEDREC,NEWEDREC   CLEAR THE RECORD                             
         L     R3,=A(NEWEDREC)                                                  
         MVI   EDFSYS-EDFILD(R3),EDFDAREQ  MARK AS A DARE RECORD                
         MVC   EDFMON-EDFILD(1,R3),TODAY3+1  MONTH                              
*                                                                               
         AHI   R3,EDFDARE-EDFILD                                                
         USING RDLNFAXD,R3                                                      
         MVC   RDFXTID,=C'ERRFAX'  BUILD FAX DELIVERY NOTIFICATION              
         MVC   RDFXORDR,EDFAPPL+23 ORDER NUMBER                                 
         MVC   RDFXFRID,SPACES                                                  
         MVC   RDFXFRID(4),EDFDEST FAX DESTINATION                              
         MVC   RDFXTOID,SPACES                                                  
         MVC   RDFXTOID(L'USERID),USERID    ORIGINATING AGENCY USERID           
         MVC   RDFXRTRN,EDFAPPL+31 'RETURN TO SENDER' DATA                      
*                                                                               
*        MVC   RDFXDATE,MSGDESC_PUTDATE+2   DATE OF FAX ERROR (YYMMDD)          
*        MVC   RDFXTIME,MSGDESC_PUTTIME     C'HHMM'                             
*                                                                               
         XC    TIMEWRK,TIMEWRK                                                  
         PACK  TIMEWRK(3),MSGDESC_PUTTIME(5)                                    
         MVI   TIMEWRK+2,X'00'     CLEAR WHATEVER X'?F' AFTER PACK              
         PACK  TIMEWRK+8(5),MSGDESC_PUTDATE(9)                                  
         MVI   TIMEWRK+12,X'00'    CLEAR WHATEVER X'?F' AFTER PACK              
*ADJUST THE TIME                                                                
         CONVTOD  CONVVAL=TIMEWRK,TODVAL=DUB,                          +        
               TIMETYPE=DEC,DATETYPE=YYYYMMDD,OFFSET=TIMEADJ                    
         STCKCONV STCKVAL=DUB,CONVVAL=TIMEWRK,                         +        
               TIMETYPE=DEC,DATETYPE=YYYYMMDD                                   
*                                                                               
         UNPK  DUB(5),TIMEWRK(3)                                                
         MVC   RDFXTIME,DUB                 C'HHMM'                             
         UNPK  CARD(9),TIMEWRK+8(5)                                             
         MVC   RDFXDATE,CARD+2              DATE OF FAX ERROR (YYMMDD)          
*                                                                               
         MVC   RDFXTDTE,RDFXDATE   DATE OF FAX ERROR (YYMMDD)                   
         MVC   RDFXTTIM,RDFXTIME   C'HHMM'                                      
*                                                                               
         MVC   RDFXERR,=CL3'501'   NO FAX RECORD ERROR                          
         DROP  R3,R4                                                            
*                                                                               
         L     RE,=A(MQBUFF)                                                    
         MVC   0(L'MSG_ACT,RE),MSG_ACT       JNK, DAREFAX ERROR                 
         AHI   RE,L'MSG_ACT                                                     
         MVC   0(2,RE),CRLF                                                     
         AHI   RE,2                                                             
         MVC   0(L'MSG_REF#,RE),MSG_REF#     SAME REF# AS JNK MESSAGE           
         AHI   RE,L'MSG_REF#                                                    
         MVC   0(2,RE),CRLF                                                     
         AHI   RE,2                                                             
         MVC   0(4,RE),=X'00000000'      FOR JNK, DISK ADDRESS=0                
         AHI   RE,4                                                             
         MVC   0(2,RE),CRLF                                                     
         AHI   RE,2                                                             
         MVC   0(L'NEWEDREC,RE),NEWEDREC                                        
         AHI   RE,L'NEWEDREC                                                    
         MVC   0(2,RE),CRLF                                                     
         AHI   RE,2                                                             
*                                                                               
         PRNT  ADDEDICTRECORD,PRINT=ALWAYS                                      
         GOTO1 =V(PRNTBL),DMCB,0,NEWEDREC,C'DUMP',256,=C'1D'                    
         GOTO1 EDCTADD,DMCB,MAJORNAM,NEWEDREC,0                                 
         MVC   FULL,DMCB+8         RETURNED DISK ADDRESS                        
         GOTO1 =V(HEXOUT),DMCB,FULL,P+30,4,=C'TOG'                              
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         PRNT  EDICTRECORDADDED,PRINT=ALWAYS                                    
*                                                                               
ERRDXX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
EZDLNCAN NTR1  BASE=*,LABEL=*                                                   
         LR    R5,RB                                                            
         AHI   R5,4096                                                          
         USING EZDLNCAN+4096,R5                                                 
*                                                                               
* THIS ROUTINE HANDLES EASYLINK DELIVERY AND CANCELLATION NOTIFICATIONS         
* FIELD 'DELIVERY' INDICATES WHICH ONE IT IS                                    
*                                                                               
         XC    MSG_REF#,MSG_REF#                                                
         XC    MSG_ACT,MSG_ACT                                                  
*                                                                               
         L     R3,=A(MQBUFF)                                                    
         MVC   MSG_ACT,0(R3)       ACTION                                       
         MVC   DELIVERY,MSG_ACT                                                 
*                                                                               
         CLC   MSG_ACT,=CL3'REA'   ECN REATTEMPT MESSAGE?                       
         BNE   *+8                                                              
         MVI   DELIVERY,RETRIED                                                 
*                                                                               
         AHI   R3,L'MSG_ACT        JUMP TO NEXT FIELD                           
         CLC   CRLF,0(R3)                                                       
         BE    *+12                                                             
         AHI   R3,1                                                             
         B     *-14                                                             
         AHI   R3,2                                                             
*                                                                               
         MVC   MSG_REF#,0(R3)      REFERENCE NUMBER                             
         CLI   MSG_REF#,C'0'       BAD REF#?                                    
         BH    *+8                 NO - CONTINUE                                
         BRAS  RE,SCANREFN         SCAN FOR THE REF#                            
*                                  GO TO NEXT FIELD                             
         CLC   CRLF,0(R3)                                                       
         BE    *+12                                                             
         AHI   R3,1                                                             
         B     *-14                                                             
         AHI   R3,2                                                             
*                                                                               
EZDLN20  EQU   *                   R3 NOW POINTS TO DATE                        
         CLI   1(R3),C'0'          COULD BE A ONE-DIGIT DATE                    
         BL    *+18                                                             
         MVC   HALF,0(R3)          NO, IT'S TWO DIGITS                          
         LA    R3,2(R3)            BUMP PAST THE DIGITS                         
         B     *+18                                                             
*                                                                               
         MVI   HALF,C'0'           1 DIGIT, SO FORCE 1ST DIGIT TO ZERO          
         MVC   HALF+1(1),0(R3)                                                  
         LA    R3,1(R3)            BUMP PAST THE SINGLE DIGIT                   
*                                                                               
         GOTO1 =V(HEXIN),DMCB,HALF,DELDAY,2                                     
         CLC   =F'1',DMCB+12                                                    
         BE    EZDLN30                                                          
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(12),=C'INVALID DATE'                                        
         MVC   P+50(8),HALF                                                     
         GOTO1 =V(PRINTER)                                                      
         B     EZDLN190            IGNORE THIS ONE                              
*                                                                               
EZDLN30  XC    WORK,WORK                                                        
         MVC   WORK(3),0(R3)       BUILD U.S.-STYLE DATE: MONTH                 
         MVC   WORK+3(2),HALF                             DAY                   
         MVI   WORK+5,C'/'                                DELIMITER             
         MVC   WORK+6(2),3(R3)                            YEAR                  
         GOTO1 =V(DATVAL),DMCB,WORK,DELDATE                                     
         CLC   =F'8',DMCB                                                       
         BE    EZDLN35             DATE IS NOW IN YYMMDD FORMAT                 
*                                                                               
*                                                                               
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(12),=C'INVALID DATE'                                        
         MVC   P+50(8),WORK                                                     
         GOTO1 =V(PRINTER)                                                      
         B     EZDLN190            IGNORE THIS ONE                              
*                                                                               
EZDLN35  MVC   WORK(2),6(R3)       HH                                           
         MVC   WORK+2(2),9(R3)     MM (SKIP OVER COLON)                         
         GOTO1 =V(HEXIN),DMCB,WORK,DELTIME,4                                    
         CLC   =F'2',DMCB+12                                                    
         BE    EZDLN60                                                          
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(12),=C'INVALID TIME'                                        
         MVC   P+50(4),WORK                                                     
         GOTO1 =V(PRINTER)                                                      
         B     EZDLN190            IGNORE THIS ONE                              
*                                                                               
EZDLN60  TBIN  DDSTIME=YES         R0 = DDS TIME DIFFERENCE (IN SECS.)          
         SRDL  R0,32               PREPARE FOR DIVIDE                           
         D     R0,=F'3600'         R1 = DDS START TIME IN HOURS                 
         BCTR  R1,0                MINUS ONE HOUR                               
         CLM   R1,1,DELTIME        IS DELIV/CAN TIME BEFORE DDS START?          
         BNH   EZDLN65             NO                                           
         GOTO1 =V(ADDAY),DMCB,DELDATE,WORK,F'-1'                                
         MVC   DELDATE,WORK        CORRECTED DELIVERY/CANCELLATION DATE         
         GOTO1 =V(HEXIN),DMCB,WORK+4,DELDAY,2                                   
         CLC   =F'1',DMCB+12                                                    
         BE    *+6                 DAY NUMBER IS DDS DAY                        
         DC    H'0'                                                             
*                                                                               
EZDLN65  MVC   EZLEDGER,13(R3)     EZ LEDGER # OR ECN TRACKING NUMBER           
*                                                                               
         XC    RLFAXNUM,RLFAXNUM   RESET "SENT TO" FAX NUMBER                   
         XC    ERRCODE,ERRCODE     RESET CANCELLATION ERROR CODE                
*                                                                               
         CLI   DELIVERY,REJECTED   REJECTION NOTIFICATION RECEIVED?             
         BE    EZDLN67                                                          
         CLI   DELIVERY,CANCELLD   CANCELLATION NOTIFICATION RECEIVED?          
         BE    EZDLN67                                                          
         CLI   DELIVERY,DELIVERD   DELIVERY NOTIFICATION RECEIVED?              
         BNE   EZDLN73                                                          
*                                                                               
EZDLN67  LA    R3,21(R3)           POINT THE END OF LEDGER/TRACKING NUM         
         CLC   CRLF,0(R3)          READ PASS THE FIELD SEPARATOR                
         BE    *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-14                KEEP READ FOR THE FIELD SEPARATOR            
*                                                                               
         CLI   DELIVERY,DELIVERD   FOR DELIVERY                                 
         BNE   EZDLN69                                                          
         CLI   2(R3),C'0'          NUMBERS?                                     
         BNH   EZDLN73             NO, DON'T SAVE FAX #                         
         LA    RE,2(R3)                                                         
         CLI   2(R3),C'1'          SKIP THE LEADING '1'                         
         BNE   *+8                                                              
         AHI   RE,1                                                             
         MVC   RLFAXNUM,0(RE)      DLN, SAVE "SENT TO" FAX NUMBER               
         B     EZDLN73                                                          
*                                                                               
EZDLN69  MVC   ERRCODE,2(R3)       CAN/REJ, SAVE ERROR CODE                     
         CLC   CRLF,5(R3)          IF ERROR CODE IS ONLY 3 CHAR                 
         BNE   *+8                 NO                                           
         MVI   ERRCODE+3,X'40'     YES - PUT SPACE FOR 4TH CHAR                 
*                                                                               
EZDLN73  CLC   MSG_REF#+1(1),MAJORNAM+5  MATCH ON EDICT (ADV/REP)?              
         BNE   EZDLN80             NO -- DLN CAME IN OVER WRONG LINE            
*                                                                               
         CLI   MSG_REF#,C'4'       VERSION 4 CUSTOMER REF. NUMBER?              
         BE    *+12                                                             
         CLI   MSG_REF#,C'1'       VERSION 1 CUSTOMER REF. NUMBER?              
         BNE   *+16                                                             
         CLI   DRTEST,C'Y'         IS THIS THE DR TEST?                         
         BE    EZDLN80             YES -- THROW AWAY LIVE TRAFFIC               
         B     EZDLN75             NO -- TAKE IT                                
*                                                                               
         CLI   MSG_REF#,C'D'       GENERATED DURING DR TEST?                    
         BNE   EZDLN80             NO -- OBSOLETE                               
         CLI   DRTEST,C'Y'         IS THIS THE DR TEST?                         
         BNE   EZDLN80             NO -- OBSOLETE                               
*                                                                               
EZDLN75  EQU   *                                                                
         CLI   MSG_REF#,C'4'       VERSION 4 CUSTOMER REF. NUMBER?              
         BNE   EZDLN90                                                          
*                                                                               
         CLC   TODAY8,MSG_REF#+10  NEED TO RESET TODAY'S DATE?                  
         BNL   EZDLN76             NO                                           
         GOTO1 =V(DATCON),DMCB,(5,0),(20,TODAY8)                                
         MVC   DELPERD+9(8),TODAY8 FOR PERVAL                                   
*                                                                               
EZDLN76  MVC   DELPERD(8),MSG_REF#+10  DATE OF ORIGINAL SEND (YYYYMMDD)         
         GOTO1 =V(PERVAL),DMCB,(17,DELPERD),WORK                                
         CLI   DMCB+4,0                                                         
         BNE   EZDLN80             MAINFRAME DATE MUST BE SCREWED UP!           
         LA    RF,WORK                                                          
         CLC   PVALNDYS-PERVALD(2,RF),=AL2(28)                                  
         BH    EZDLN80             DLN IS MORE THAN 28 DAYS OLD                 
*                                                                               
         LA    R1,DMCB                                                          
         USING CONVDSKD,R1                                                      
         MVC   CONVDMGR,DATAMGR    A(DATAMGR)                                   
         MVI   CONVACTN,CONVDSKO   CONVERT REFERENCE NUMBER TO DSKADDR          
         MVC   CONVOFF,=A(MSG_REF#+2)                                           
         GOTO1 =V(CONVDSKA)                                                     
         MVC   FULL,CONVDSK                                                     
         DROP  R1                                                               
         B     EZDLN100                                                         
*                                                                               
EZDLN80  MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(33),=C'INVALID CUSTOMER REFERENCE NUMBER'                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLC   MSG_REF#+1(1),MAJORNAM+5  MATCH ON EDICT (ADV/REP)?              
         BE    EZDLN190                                                         
         MVC   OPMSG2A,DELIVERY                                                 
         MVC   OPMSG2B,EZLEDGER                                                 
         MVC   OPMSG2C,MSG_REF#+2                                               
         BRAS  RE,CHKOPM                                                        
         BNE   EZDLN190                                                         
         GOTO1 DATAMGR,DMCB,=C'OPMSG',('OPMSG2Q',OPMSG2)                        
         B     EZDLN190            IGNORE THIS ONE                              
*                                                                               
EZDLN90  EQU   *                                                                
*                                                                               
         XC    AXMTNTRY,AXMTNTRY                                                
         XC    XMTNTRY,XMTNTRY                                                  
*                                                                               
         USING XMTTABLD,R2                                                      
         LA    R2,XMTNTRY                                                       
*                                                                               
         MVC   XMTSUBID,MSG_REF#+06                                             
         GOTO1 =V(HEXIN),DMCB,MSG_REF#+02,XMTUSRID,4,0                          
         GOTO1 =V(HEXIN),DMCB,MSG_REF#+09,XMTREFNO,4,0                          
         GOTO1 =V(HEXIN),DMCB,MSG_REF#+13,XMTCRDAT,4,0                          
         GOTO1 =V(HEXIN),DMCB,MSG_REF#+17,XMTCRTIM,4,0                          
         MVI   XMTLOGNO,0                                                       
         GOTO1 =V(HEXIN),DMCB,MSG_REF#+21,XMTLOGNO+0,4,0                        
         MVI   XMTDSTNO,0                                                       
         GOTO1 =V(HEXIN),DMCB,MSG_REF#+25,XMTDSTNO+1,2,0                        
         NI    XMTDSTNO+1,X'FF'-X'80'   TURN OFF THIS BIT                       
*                                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,XMTUSRID       PQ REPORT SENDING USERID                     
         D     R0,NUMPQS                                                        
         AHI   R0,1                                                             
         STC   R0,XMTPRTQ          PRINT QUEUE NUMBER                           
         MVI   XMTSOURC,XMTSRCPQ   PRINT QUEUE REPORT                           
*                                                                               
         MVC   P+11(13),=C'***MSG_REF#**'                                       
         MVC   P+30(L'MSG_REF#),MSG_REF#                                        
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P+11(13),=C'*** XMTNTRY**'                                       
         MVC   P+30(L'XMTNTRY),XMTNTRY                                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R9,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R9)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(9),E,8)  ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         SAM31                                                                  
         L     RF,AXMTTBL                                                       
         AHI   RF,-8               RF = A(ENTRIES#)                             
         MVC   DMCB+8(4),0(RF)     NUMBER OF ENTRIES IN TABLE                   
         GOTO1 =V(BINSRCH),DMCB,XMTNTRY,AXMTTBL,,XMTTBLQ,XMTKEYQ,0              
         SAM24                                                                  
*                                                                               
         LA    R9,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R9)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(9),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
         ICM   R4,15,DMCB          A(RECORD IN TABLE)                           
         TMH   R4,X'8000'          WAS RECORD FOUND?                            
         BNO   EZDLN95             YES                                          
*                                                                               
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(24),=C'XMTTABLE ENTRY NOT FOUND'                            
         GOTO1 =V(PRINTER)                                                      
         B     EZDLN190            IGNORE THIS ONE                              
*                                                                               
EZDLN95  ST    R4,AXMTNTRY         STORE A(THIS TABLE ENTRY)                    
         MVC31 XMTNTRY,0(R4)       SAVE THE TABLE ENTRY                         
*                                                                               
EZDLN100 EQU   *                                                                
         CLI   XMTMETH,C'E'        EASYLINK TRANSACTION?                        
         BNE   EZDLN110                                                         
         CLI   DSTFLAG,C'Y'        IS IT DAYLIGHT SAVING TIME?                  
         BNE   EZDLN110                                                         
*                                                                               
         XC    HALF,HALF                                                        
         ZIC   R1,DELTIME          PWOS HOUR                                    
         SLL   R1,4                                                             
         O     R1,=X'0000000C'                                                  
         STH   R1,HALF             PACKED DECIMAL HOUR                          
         AP    HALF,=P'1'          ADD 1 HOUR ADJUSTMENT                        
         CP    HALF,=P'24'         DID WE GO PAST MIDNIGHT?                     
         BNE   EZDLN105                                                         
         ZAP   HALF,=P'0'          YES -- MILITARY HOUR = 0, NOT 24             
         GOTO1 =V(ADDAY),DMCB,DELDATE,WORK,F'1'                                 
         MVC   DELDATE,WORK        CORRECTED DELIVERY/CANCELLATION DATE         
         GOTO1 =V(HEXIN),DMCB,WORK+4,DELDAY,2                                   
         CLC   =F'1',DMCB+12                                                    
         BE    *+6                 DAY NUMBER IS DDS DAY                        
         DC    H'0'                                                             
*                                                                               
EZDLN105 LH    R1,HALF             ADJUSTED PACKED DECIMAL HOUR                 
         SRL   R1,4                                                             
         STC   R1,DELTIME          PWOS HOUR                                    
*                                                                               
EZDLN110 EQU   *                                                                
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
*                                                                               
         MVI   EACTION2,0          RESET POSTSENT ACTIONS                       
         MVI   EACTION,0                                                        
         MVC   EXMTTBLE,AXMTTBL                                                 
         MVC   ETBLNTRY,XMTDSKAD                                                
         CLI   MSG_REF#,C'4'       V4, OLD STYLE?                               
         BNE   *+10                NO                                           
         MVC   ETBLNTRY,FULL       YES - GET DISKADR FROM "FULL"                
*                                                                               
         CLI   DELIVERY,DELIVERD   DELIVERY NOTIFICATION RECEIVED?              
         BNE   *+12                                                             
         MVI   EACTION,EACTDLVQ                                                 
         B     EZDLN115                                                         
*                                                                               
         CLI   DELIVERY,CANCELLD   CANCELLATION NOTIFICATION RECEIVED?          
         BNE   *+12                                                             
         MVI   EACTION,EACTCANQ                                                 
         B     EZDLN115                                                         
*                                                                               
         CLI   DELIVERY,ACCEPTED   ACCEPTANCE NOTIFICATION RECEIVED?            
         BNE   *+12                                                             
         MVI   EACTION2,EACTACCQ                                                
         B     EZDLN115                                                         
*                                                                               
         CLI   DELIVERY,REJECTED   REJECTION NOTIFICATION RECEIVED?             
         BNE   *+12                                                             
         MVI   EACTION2,EACTREJQ                                                
         B     EZDLN115                                                         
         MVI   EACTION2,EACTREAQ   OTHERWISE, REATTEMPT NOTIFICATION            
*                                                                               
EZDLN115 OI    EACTION,EACTFILQ    DISK ADDRESS BEING PASSED                    
         MVC   EEZCANER,ERRCODE    CANCELLATION ERROR CODE                      
         MVC   EMAJORNM,=A(MAJORNAM)                                            
         MVC   EDAYNUM,DELDAY      DAY DELIVERED/CANCELLED                      
         MVC   ETIME,DELTIME       TIME DELIVERED/CANCELLED                     
         MVC   EAEZDTA,=A(EDFDATA)                                              
         MVC   EADTAMGR,DATAMGR                                                 
         MVC   EFILREC,=A(EDICTREC)                                             
         GOTO1 =V(POSTSENT)                                                     
         BNE   EZDLN190            COULD NOT POST EDICT FILE                    
         DROP  R1                                                               
*                                                                               
         MVC   P+30(L'MSG_REF#),MSG_REF#    CUSTOMER REFERENCE NUMBER           
         CLI   DELIVERY,DELIVERD   DELIVERY NOTIFICATION RECEIVED?              
         BNE   *+14                NO                                           
         MVC   P+60(9),=C'DELIVERED'                                            
         B     EZDLN118                                                         
*                                                                               
         MVC   P+70(4),ERRCODE     CANCELLATION ERROR CODE                      
         CLI   DELIVERY,CANCELLD   CANCELLATION NOTIFICATION RECEIVED?          
         BNE   *+14                NO                                           
         MVC   P+60(9),=C'CANCELLED'                                            
         B     EZDLN118                                                         
*                                                                               
         CLI   DELIVERY,ACCEPTED   ACCEPTATION NOTIFICATION RECEIVED?           
         BNE   *+14                NO                                           
         MVC   P+60(8),=C'ACCEPTED'                                             
         B     EZDLN118                                                         
*                                                                               
         CLI   DELIVERY,REJECTED   REJECTION NOTIFICATION RECEIVED?             
         BNE   *+14                NO                                           
         MVC   P+60(8),=C'REJECTED'                                             
         B     EZDLN118                                                         
*                                                                               
         CLI   DELIVERY,RETRIED    REATTEMPT NOTIFICATION RECEIVED?             
         BE    *+6                 YES                                          
         DC    H'0'                UNKNOWN NOTIFICATION                         
         MVC   P+60(7),=C'RETRIED'                                              
         B     EZDLN118                                                         
*                                                                               
EZDLN118 PRNT  NOTIFY,PRINT=ALWAYS                                              
*                                                                               
         LA    R2,EDICTREC                                                      
         USING EDFILD,R2                                                        
         TM    EDFFLAGS,EDFDAREZ   WAS THIS A DARE REPORT?                      
         BZ    EZDLN130                                                         
*                                                                               
         MVC   HALF,EDFPQUID                                                    
         BRAS  RE,GETUID           READ ID RECORD FROM CTFILE                   
*                                                                               
         XC    NEWEDREC,NEWEDREC   BUILD EDICT FILE RECORD                      
         LA    R3,NEWEDREC                                                      
         MVI   EDFSYS-EDFILD(R3),EDFDAREQ MARK AS A DARE RECORD                 
         GOTO1 =V(DATCON),DMCB,(5,0),(3,DUB2)                                   
         MVC   EDFMON-EDFILD(,R3),DUB2+1 MONTH NUMBER IN BINARY                 
         LA    R3,EDFDARE-EDFILD(,R3)                                           
         USING RDLNFAXD,R3                                                      
         MVC   RDFXTID,=C'DLNFAX'  BUILD FAX DELIVERY NOTIFICATION              
         CLI   DELIVERY,ACCEPTED   ACCEPTATION NOTIFICATION RECEIVED?           
         BE    EZDLN190            DON'T NOTHING                                
         CLI   DELIVERY,DELIVERD   DELIVERY NOTIFICATION RECEIVED?              
         BE    *+10                YES                                          
         MVC   RDFXTID,=C'CANFAX'  BUILD FAX CANCELLATION NOTIFICATION          
         MVC   RDFXORDR,EDFAPPL+23 ORDER NUMBER                                 
         MVC   RDFXFRID,SPACES                                                  
         MVC   RDFXFRID(7),EDFDARFX FAX RECORD KEY                              
         MVC   RDFXTOID,USERID     ORIGINATING AGENCY USERID                    
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),TODAY                                      
         MVC   WORK(6),TODAY       YYMMDD                                       
         THMS  DDSTIME=YES                                                      
         ST    R0,PACKOF4B                                                      
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
         CP    PACKOF4B,=P'240000' PAST MIDNIGHT?                               
         BL    EZDLN120                                                         
         SP    PACKOF4B,=P'240000' YES, BUMP TO NEXT DAY AND ADJUST             
         GOTO1 =V(ADDAY),DMCB,WORK,WORK,F'1'                                    
*                                                                               
EZDLN120 MVC   RDFXDATE,WORK       ADJUSTED DATE (YYMMDD)                       
         EDIT  PACKOF4B,DUB2       C'00HHMMSS'                                  
         MVC   RDFXTIME,DUB2+2     C'HHMM'                                      
         MVC   RDFXTDTE,DELDATE    DATE OF FAX RECEIVE/CANCEL (YYMMDD)          
         GOTO1 =V(HEXOUT),DMCB,DELTIME,RDFXTTIM,2,=C'TOG'                       
         CLC   =F'4',DMCB+16       TIME OF FAX RECEIVE/CANCEL (HHMM)            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   RDFXRTRN,EDFAPPL+31 'RETURN TO SENDER' DATA                      
*                                                                               
         PRNT  ADDEDICTRECORD,PRINT=ALWAYS                                      
         GOTO1 =V(PRNTBL),DMCB,0,NEWEDREC,C'DUMP',256,=C'1D'                    
         GOTO1 EDCTADD,DMCB,MAJORNAM,NEWEDREC,0                                 
         MVC   FULL,DMCB+8         RETURNED DISK ADDRESS                        
         GOTO1 =V(HEXOUT),DMCB,FULL,P+30,4,=C'TOG'                              
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         PRNT  EDICTRECORDADDED,PRINT=ALWAYS                                    
         B     EZDLN190                                                         
         DROP  R3                                                               
*                                                                               
EZDLN130 EQU   *                                                                
         CLI   DELIVERY,DELIVERD   DELIVERY NOTIFICATION RECEIVED?              
         BE    EZDLN135            YES                                          
         CLI   DELIVERY,CANCELLD   CANCELLATION NOTIFICATION RECEIVED?          
         BE    EZDLN135            NO                                           
         CLI   DELIVERY,REJECTED   REJECTION NOTIFICATION RECEIVED?             
         BNE   EZDLN190            NO                                           
EZDLN135 OC    EDFMQAPP,EDFMQAPP   NEED MQ MSG?                                 
         BZ    EZDLN190            NO                                           
*                                                                               
EZDLN170 EQU   *                                                                
*                                                                               
         MVC   HALF,EDFPQUID                                                    
         BRAS  RE,GETUID           READ ID RECORD FROM CTFILE                   
*                                                                               
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         LA    R3,WORK                                                          
         USING EZMQMSGD,R3                                                      
         MVC   EZMBKLAB,=CL16'FACPAK=????*****'                                 
         MVC   EZMBKLAB+7(4),EDFMQAPP                                           
         MVC   EZMLABEL,EDFMQLAB                                                
         MVC   EZMDEST,EDFDEST                                                  
         MVC   EZMUSER,USERID                                                   
         MVC   EZMSTAT,DELIVERY                                                 
         CLI   DELIVERY,REJECTED   REJECTION NOTIFICATION RECEIVED?             
         BNE   *+8                                                              
         MVI   EZMSTAT,CANCELLD    TREAT REJ LIKE CANCELLATION MSG              
         GOTO1 =V(DATCON),DMCB,(0,DELDATE),(7,EZMDATE)                          
         GOTO1 =V(HEXOUT),DMCB,DELTIME,EZMTIME,2,=C'TOG'                        
         MVC   EZMAPPL,EDFAPPL                                                  
         DROP  R2                                                               
*                                                                               
         CLI   DELIVERY,DELIVERD   DELIVERED?                                   
         BE    EZDLN175            YES                                          
*                                                                               
         L     RF,=A(EZMSGTAB)     EASYLINK ERROR TABLE                         
EZDLN172 CLI   0(RF),X'FF'                                                      
         BNE   *+20                                                             
         MVC   EZMEMSG(14),=C'EZ FAX ERROR #'  NOT IN TABLE                     
         MVC   EZMEMSG+14(4),ERRCODE           SO DISPLAY NUMBER                
         B     EZDLN175                                                         
*                                                                               
         CLC   ERRCODE,0(RF)       MATCH ON CODE                                
         BE    EZDLN174                                                         
         LA    RF,28(RF)                                                        
         B     EZDLN172                                                         
EZDLN174 MVC   EZMEMSG,4(RF)      DISPLAY TEXT                                  
         DROP  R3                                                               
*                                                                               
EZDLN175 EQU   *                                                                
         PRNT  MQ_MESSAGE,PRINT=ALWAYS                                          
         GOTO1 =V(PRNTBL),DMCB,0,WORK,C'DUMP',EZMQMSGQ,=C'1D'                   
*                                                                               
         L     RE,=A(MQBUFF)                                                    
         L     RF,=A(MQBUFFL)      CLEAR BUFFER                                 
         XCEFL                                                                  
         L     RE,=A(MQBUFF)                                                    
         MVC   0(EZMQMSGQ,RE),WORK                                              
*                                                                               
         LHI   RE,EZMQMSGQ                                                      
         ST    RE,DATALENGTH                                                    
*                                                                               
         MVC   OBJDESC2_OBJECTTYPE,=A(MQOT_Q)  OBJECT IS A QUEUE                
         LA    RF,MQPMO_NO_SYNCPOINT                                            
         ICM   RE,15,=AL4(MQPMO_FAIL_IF_QUIESCING)                              
         AR    RF,RE                                                            
         ST    RF,PUTMSGOPTS_OPTIONS                                            
         MVC   MSGDESC2_PERSISTENCE,=A(MQPER_PERSISTENT)                        
*                                                                               
         LA    R2,CSQBPUT1_STRUCTURE                                            
         BRAS  RE,CALLMQ           PUT THE MESSAGE TO THE MQ QUEUE              
         BE    EZDLN190                                                         
         DC    H'0'                                                             
*                                                                               
EZDLN190 EQU   *                                                                
*                                                                               
EZDLNX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB,R5                                                            
         EJECT                                                                  
*                                                                               
SCANREFN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* UPON ENTRY,  R3 POINTS TO BUFFER WHERE IT WILL SCAN FORWARD 400 BYTES         
*                                                                               
         MVC   P+11(19),=C'***SCAN FOR REF#***'                                 
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    RE,400                                                           
SREF10   CLC   =C'ORIGINAL MESSAGE:',0(R3)                                      
         BE    SREF20              TRY SCAN FORWARD FOR 1ST REF#                
         AHI   R3,1                                                             
         BCT   RE,SREF10                                                        
         B     SREFX               CAN'T DO ANYTHING, JUST EXIT                 
*                                                                               
SREF20   EQU   *                   FIND THE NEXT SEMICOLON                      
         LA    RE,100                                                           
SREF30   CLI   0(R3),X'5E'         SEMICOLON?                                   
         BE    SREF40              YES                                          
         AHI   R3,1                                                             
         BCT   RE,SREF30                                                        
         B     SREFX               CAN'T DO ANYTHING, JUST EXIT                 
*                                                                               
SREF40   EQU   *                                                                
         MVC   MSG_REF#,1(R3)      REF# SHOULD BE RIGHT AFTER SEMICOLON         
*                                                                               
SREFX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
CHKOPM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TIME  BIN                                                              
         LR    R1,R0                                                            
         S     R1,OPMTBIN          MINUS LAST TIME OPMSG SENT                   
         BNP   CHKOP10             RESET TIMER                                  
*                                                                               
         CHI   R1,1000             LESS THAN 10 SECONDS                         
         BNH   CHKOPMNS                                                         
CHKOP10  ST    R0,OPMTBIN          SEND OPMSG, SAVE TIME SENT                   
         B     CHKOPMOK                                                         
*                                                                               
CHKOPMNS LTR   RB,RB               SET CC NOT EQUAL                             
         B     CHKOPMX                                                          
*                                                                               
CHKOPMOK CR    RB,RB               SET CC EQUAL                                 
CHKOPMX  XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* UPON ENTRY,  R2 POINTS TO PARAMETER STRUCTURE                                 
* UPON RETURN, COMPCODE CONTAINS THE MQ COMPLETION CODE                         
*              REASON CONTAINS THE MQ REASON CODE                               
**********************************************************************          
CALLMQ   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   DOMQCALL,C'Y'       DISABLED ALL MQ CALLS?                       
         BNE   CMQOK               YES -- DON'T DO ANY MQ CALLS                 
*                                                                               
         SAM31                                                                  
         L     RF,0(R2)            RF = A(MQ ROUTINE)                           
         LA    R3,24(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
         SAM24                                                                  
*                                                                               
         L     RF,ASTOPECB                                                      
         TM    0(RF),X'40'         STOP?                                        
         BZ    *+14                NO                                           
         XC    0(4,RF),0(RF)                                                    
         MVI   OPERSTOP,C'Y'                                                    
*                                                                               
         MVI   P,C'+'              '+' MEANS IT'S AN MQ CALL                    
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODES          
         MVC   P+30(12),=C'COMPLETED OK'                                        
         CLC   COMPCODE,=A(MQCC_OK)                                             
         BNE   CMQ05                                                            
         MVC   P+50(L'OBJDESC_OBJECTNAME),OBJDESC_OBJECTNAME                    
         CLC   =CL16'MQ_PUT1',4(R2)                                             
         BNE   *+10                                                             
         MVC   P+50(L'OBJDESC2_OBJECTNAME),OBJDESC2_OBJECTNAME                  
         PRNT  PRINT=ALWAYS                                                     
         B     CMQOK                                                            
*                                                                               
CMQ05    MVC   P+30(12),=C'WARNING     '                                        
         CLC   COMPCODE,=A(MQCC_WARNING)                                        
         BE    CMQ10                                                            
         MVC   P+30(12),=C'** FAILED **'                                        
         CLC   COMPCODE,=A(MQCC_FAILED)                                         
         BE    CMQ10                                                            
         DC    H'0'                UNKNOWN COMPLETION CODE                      
*                                                                               
CMQ10    MVC   P+50(9),=C'REASON = '                                            
         EDIT  REASON,(5,P+59),ALIGN=LEFT,ZERO=NOBLANK                          
         MVC   P+66(13),=C'*** ERROR ***'                                       
         PRNT  PRINT=ALWAYS                                                     
*                                                                               
         LTR   RB,RB               NO GOOD -- SET CC NOT EQUAL                  
         B     CMQX                                                             
*                                                                               
CMQOK    CR    RB,RB               SET CC EQUAL                                 
*                                                                               
CMQX     XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* COMMON WORK AREA                                                              
**********************************************************************          
COMMWORK DS    0D                  COMMON STORAGE AREA                          
         GETEL    R1,28,ELCODE                                                  
*                                                                               
DMWORK   DS    12D                                                              
DMCB     DS    10F                                                              
PACKOF4B DS    PL4                                                              
DUB      DS    D                                                                
DUB2     DS    D                                                                
PRNTDUB  DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
*                                                                               
*DIFFERENCE B/W UTC AND LOCAL TIME IN PACK DEC                                  
TIMEADJ  DS    CL4                 000HHMMS (S=F FOR +, S=D FOR -)              
         DS    0F                                                               
TIMEWRK  DS    CL16                INPUT PARM FOR CONVTOD ON WORD BNTRY         
*                                                                               
*****************************************************************               
NUMPQS   DC    F'16'       ASSUME # OF PQS IS 16 FOR NOW.                       
*****************************************************************               
PRNTTIME DS    CL9                                                              
DATAMGR  DS    A                   A(DATAMGR)                                   
EDCTADD  DS    A                   A(ROUTINE TO ADD EDICT RECORDS)              
AXMTTBL  DS    A                   A(XMIT REPORTS TABLE)                        
AXMTNTRY DS    F                                                                
XMTNTRY  DS    XL(XMTTBLQ)         TEMP STORAGE FOR 1 XIMTABLE ENTRY            
TABLNTRY DS    XL(XMTTBLQ)         SAVED XMIT TABLE KEY                         
USERID   DS    CL10                ALPHA USERID                                 
EDICTREC DS    XL256               FOR BUILDING EDICT FILE RECORD               
NEWEDREC DS    XL256               FOR BUILDING EDICT FILE RECORD               
SVBDESND DS    XL256               SAVED BDE "SND" MESSAGE 1                    
SVBDESD2 DS    XL256               SAVED BDE "SND" MESSAGE 2                    
SVBDESD3 DS    XL256               SAVED BDE "SND" MESSAGE 3                    
SVBDESD4 DS    XL256               SAVED BDE "SND" MESSAGE 4                    
SVBDERCV DS    XL256               SAVED BDE "RCV" MESSAGE 1                    
SVBDERV2 DS    XL256               SAVED BDE "RCV" MESSAGE 2                    
SVBDERV3 DS    XL256               SAVED BDE "RCV" MESSAGE 3                    
SVBDERV4 DS    XL256               SAVED BDE "RCV" MESSAGE 4                    
WORK     DS    CL256                                                            
         DS    XL4                 PQ RECORD LENGTH                             
R        DS    CL256               PQ RECORD DATA                               
PRTQNAME DS    CL5                 EBCDIC PQ NAME (E.G., C'PRTQ5')              
         SPACE 2                                                                
DELPERD  DC    C'YYYYMMDD-YYYYMMDD'  FOR PERVAL: SEND DATE - TODAY              
TODAY8   DS    CL8                 EBCDIC DATE TODAY (YYYYMMDD)                 
TODAY3   DS    XL3                 TODAY -- BINARY YMD                          
TODAY    DS    CL6                 EBCDIC DATE TODAY (YYMMDD)                   
         SPACE 2                                                                
MSG_ACT  DS    CL3                 ACTION                                       
MSG_REF# DS    CL27                REFERENCE NUMBER                             
MSG_METH DS    C                   METHOD                                       
MSG_ACC# DS    CL8                 ACCOUNT #                                    
MSG_BFN  DS    CL9                 BDF FILE NAME                                
MSG_DTT  DS    C                   DESTINATION TYPE (R/F)                       
MSG_HDR  DS    CL80                HEADER                                       
MSG_TRN  DS    CL80                TRN CARD                                     
MSG_MQN  DS    CL80                MQ QUEUE ID # (CTL/MQDEF)                    
MSG_PQS  DS    CL80                FINISHED PQ CLASS/STATUS                     
MSG_DST  DS    CL25                DESTINATION NAME                             
MSG_FDST DS    CL16                FORMATTED DESTINATION NAME                   
MSG_PQRT DS    CL2                 PQ REPORT TYPE                               
MSG_BSID DS    CL1                 BDE SENDER ID                                
         SPACE 2                                                                
*                                                                               
         DC    C'***FIELD***'      BDE NOTIFICATION INFO                        
FIELD    DS    CL50                1ST IS LEN, REST IS DATA                     
VCNUM    DS    CL2                 VERSION CONTROL NUMBER                       
NEWCREF  DS    CL27                                                             
CUSTREF  DS    CL18                VEDDNNNNNNYYYYMMDD                           
EVTYPE   DS    CL3                 EVENT TYPE (SND, RCV, DEL)                   
EVDAY    DS    X                   EVENT DAY -- PWOS                            
EVTIME   DS    XL2                 EVENT TIME (HM) -- PWOS                      
DOCID    DS    CL9                 DOCUMENT ID NUMBER                           
*                                                                               
         SPACE 2                                                                
ELCODE   DS    X                                                                
MAJORNAM DS    CL8                 MAJOR RESOURCE NAME                          
KEY      DS    XL25                CTFILE KEY                                   
         SPACE 2                                                                
ECBLST   DS    0F                                                               
         DC    X'00',AL3(INPQECB)  A(INPUT QUEUE ECB)                           
ASTOPECB DC    X'80',AL3(0)        A(STOPECB)                                   
INPQECB  DC    F'0'                ECB FOR MQ GET FROM INPUT QUEUE              
         SPACE 2                                                                
CRLF     DS    0XL2                                                             
CR       DC    X'0D'                                                            
LF       DC    X'25'                                                            
ETB      EQU   X'26'                                                            
ETX      EQU   X'03'                                                            
ENDMMMM  DC    X'0D25',C'MMMM',X'0D25'     END OF THE MESSAGE                   
         SPACE 2                                                                
DELIVERY DS    C                   DELIVERY/CANCELLATION FLAG                   
DELIVERD EQU   C'D'                 DELIVERY NOTIFICATION RECEIVED              
CANCELLD EQU   C'C'                 CANCELLATION NOTIFICATION RECEIVED          
ACCEPTED EQU   C'A'                 ACCEPT NOTIFICATION RECEIVED                
REJECTED EQU   C'R'                 REJECT NOTIFICATION RECEIVED                
RETRIED  EQU   C'N'                 RETRY NOTIFICATION RECEIVED                 
DELDAY   DS    X                   MESSAGE DELIVERY DAY -- PWOS                 
DELTIME  DS    XL2                 MESSAGE DELIVERY TIME (HM) -- PWOS           
DELDATE  DS    CL6                 DELIVERY DATE (YYMMDD)                       
ERRCODE  DS    CL4                 EAZYLINK CANCELLATION ERROR CODE             
*                                                                               
EDFDATA  DS    0CL21               DATA PASSED FOR UPDATING EDICT REC           
EZLEDGER DS    CL11                EASYLINK LEDGER NUMBER                       
         ORG   EZLEDGER                                                         
ECNTRKNM DS    CL8                 ECN TRACKING NUMBER                          
         ORG                                                                    
RLFAXNUM DS    CL10                EAZYLINK DLN REDILIST FAX NUMBER             
*                                                                               
*                                                                               
OVERSIZE DC    C'N'                MESSAGE OVERSIZE FLAG                        
*                                                                               
OPMCNT   DC    F'0'                OPMSG COUNTER                                
OPMTBIN  DC    F'0'                OPMSG LAST SENT AT THIS TIME                 
*                                                                               
OPMSG2   DS    0C                                                               
         DC    C'AUTONOTE*US-MF_FAC_NOTIFY:EZ ERROR, '                          
OPMSG2A  DC    C'?'                                                             
         DC    C'#'                                                             
OPMSG2B  DC    C'XXXXXXXX'                                                      
         DC    C','                                                             
         DC    C'REF#'                                                          
OPMSG2C  DC    C'NNNNNNNN'                                                      
OPMSG2Q  EQU   *-OPMSG2                                                         
*                                                                               
OPMSG3   DS    0C                                                               
         DC    C'AUTONOTE*US-MF_FAC_NOTIFY:CANNOT ADD THIS ENTRY '              
         DC    C'REF#'                                                          
OPMSG3C  DS    CL27                                                             
OPMSG3Q  EQU   *-OPMSG3                                                         
*                                                                               
OPMSG4   DS    0C                                                               
         DC    C'AUTONOTE*US-MF_FAC_NOTIFY:OVERSIZE INB MESSAGE'                
OPMSG4Q  EQU   *-OPMSG4                                                         
*                                                                               
OPMSG5   DS    0C                                                               
         DC    C'AUTONOTE*US-MF_FAC_NOTIFY:UNKNOWN MESSAGE-'                    
OPMSG5A  DS    CL50                                                             
OPMSG5Q  EQU   *-OPMSG5                                                         
*                                                                               
         SPACE 2                                                                
OPERSTOP DC    C'N'                'Y' IF OPERATOR WANTS TO STOP                
DSTFLAG  DC    C'N'                'Y' = DAYLIGHT SAVING TIME                   
DRTEST   DS    C                   'Y' = DISASTER RECOVERY TEST MODE            
TRACEFLG DS    C                   'Y' IF DETAILED TRACE WANTED                 
ENQPQFLG DS    C                   'Y' = WE ENQUEUED THE PRINT QUEUE            
DOMQCALL DC    C'Y'                'Y' IF DO ALL MQ CALLS                       
CARD     DS    CL80                FOR CONTROL CARDS AND EDICTFIL RECS          
         EJECT                                                                  
         CMQA LIST=YES,EQUONLY=NO                                               
* MQSERIES CALL PARAMETERS                                                      
*                                                                               
QMGRNAME       DS      CL48      MQ QUEUE MANAGER NAME                          
HCONN          DS      F         MQ QMGR CONNECTION HANDLE                      
HOBJ           DS      F         OBJECT HANDLE                                  
OPEN_OPTIONS   DS      F         MQOPEN OPTIONS                                 
CLOSE_OPTIONS  DS      F         MQCLOSE OPTIONS                                
COMPCODE       DS      F         COMPLETION CODE                                
REASON         DS      F         QUALIFIES COMPLETION CODE                      
MQBUFFLENGTH   DS      F         LENGTH OF MQ BUFFER AREA                       
DATALENGTH     DS      F         LENGTH OF THE MESSAGE                          
OBJDESC        CMQODA  LIST=YES  OBJECT DESCRIPTOR                              
MSGDESC        CMQMDA  LIST=YES  MESSAGE DESCRIPTOR                             
OBJDESC2       CMQODA  LIST=YES  OBJECT DESCRIPTOR                              
MSGDESC2       CMQMDA  LIST=YES  MESSAGE DESCRIPTOR                             
GETMSGOPTS     CMQGMOA LIST=YES  GET MESSAGE OPTIONS                            
PUTMSGOPTS     CMQPMOA DSECT=NO,LIST=YES     PUT MESSAGE OPTIONS                
         SPACE 2                                                                
*                                                                               
ENTRYPTS DS    0F                                                               
         DC    Y((ENTRYLSQ-ENTRYSTQ)/60)   NUMBER OF TABLE ENTRIES              
         DC    H'60'                       MUST REMAIN AS 60                    
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
CSQBPUT1 DC    CL8'CSQBPUT1'                                                    
         DC    XL52'00'                                                         
ENTRYLSQ EQU   *                                                                
         SPACE 3                                                                
CSQBCONN_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_CONNECT'                                
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(QMGRNAME)                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(COMPCODE)                             
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBDISC_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_DISCONNECT'                             
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(COMPCODE)                             
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBOPEN_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_OPEN'                                   
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(OBJDESC)                              
                          DC    X'00',AL3(OPEN_OPTIONS)                         
                          DC    X'00',AL3(HOBJ)                                 
                          DC    X'00',AL3(COMPCODE)                             
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBGET_STRUCTURE         DS    A                                               
                          DC    CL16'MQ_GET'                                    
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ)                                 
                          DC    X'00',AL3(MSGDESC)                              
                          DC    X'00',AL3(GETMSGOPTS)                           
                          DC    X'00',AL3(MQBUFFLENGTH)                         
                          DC    X'00',AL3(MQBUFF)                               
                          DC    X'00',AL3(DATALENGTH)                           
                          DC    X'00',AL3(COMPCODE)                             
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCOMM_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_COMMIT'                                 
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(COMPCODE)                             
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCLOS_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_CLOSE'                                  
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ)                                 
                          DC    X'00',AL3(CLOSE_OPTIONS)                        
                          DC    X'00',AL3(COMPCODE)                             
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBPUT1_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_PUT1'                                   
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(OBJDESC2)                             
                          DC    X'00',AL3(MSGDESC2)                             
                          DC    X'00',AL3(PUTMSGOPTS)                           
                          DC    X'00',AL3(DATALENGTH)                           
                          DC    X'00',AL3(MQBUFF)                               
                          DC    X'00',AL3(COMPCODE)                             
                          DC    X'80',AL3(REASON)                               
         EJECT                                                                  
FACIDTAB DS    0CL8                                                             
         DC    C'ADV1',C'ADV1'                                                  
         DC    C'ADV2',C'ADV2'                                                  
         DC    C'ADV3',C'ADV3'                                                  
         DC    C'ADV4',C'ADV4'                                                  
         DC    C'ADV5',C'ADV5'                                                  
         DC    C'ADV6',C'ADV6'                                                  
         DC    C'ADV7',C'ADV7'                                                  
         DC    C'ADV8',C'ADV8'                                                  
         DC    C'AD1 ',C'ADV1'                                                  
         DC    C'AD2 ',C'ADV2'                                                  
         DC    C'AD3 ',C'ADV3'                                                  
         DC    C'AD4 ',C'ADV4'                                                  
         DC    C'AD5 ',C'ADV5'                                                  
         DC    C'AD6 ',C'ADV6'                                                  
         DC    C'AD7 ',C'ADV7'                                                  
         DC    C'AD8 ',C'ADV8'                                                  
         DC    C'REPA',C'REPA'                                                  
         DC    C'REPB',C'REPB'                                                  
         DC    C'REPC',C'REPC'                                                  
         DC    C'TST ',C'TST '                                                  
         DC    C'MEL ',C'MEL '                                                  
         DC    C'FQA ',C'FQA '                                                  
         DC    C'CSC ',C'CSC '                                                  
         DC    4X'FF'                                                           
*                                                                               
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*CIREC**'                                                      
CIREC    DC    14336X'00'          PRINT QUEUE INDEX BUFFER                     
*                                                                               
         DS    0D                                                               
         DC    C'**I/O***'                                                      
IO       DC    2000X'00'           CTFILE/GENFILE I/O BUFFER                    
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
*                                                                               
MQBUFF   DS    (MQBUFFL)X                                                       
MQBUFFL  EQU   64000               MQ BUFFER LENGTH                             
*                                                                               
       ++INCLUDE DDEDIEZERR                                                     
         EJECT                                                                  
       ++INCLUDE DDEDICTWRK                                                     
         EJECT                                                                  
* DDDPRINT                                                                      
* DDEDICTFIL                                                                    
* SPDARDARED                                                                    
* DMPRTQL                                                                       
* DMPRTQK                                                                       
* DDPERVALD                                                                     
* CTGENFILE                                                                     
* CTGENMQDEF                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DDEDICTFIL                                                     
         EJECT                                                                  
       ++INCLUDE SPDARDARED                                                     
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE DMPRTQK                                                        
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE CTGENMQDEF                                                     
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057DDEDINFX  03/09/16'                                      
         END                                                                    
