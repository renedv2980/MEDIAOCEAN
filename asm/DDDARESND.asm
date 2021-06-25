*          DATA SET DDDARESND  AT LEVEL 048 AS OF 02/28/19                      
*PHASE DARESNDA                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE SMFOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        DDDARESND -- TRANSMIT DARE REPORTS VIA APPC/MVS      *         
*                                                                     *         
*  COMMENTS:     ATTACHED BY DARE DISPATCHER (DDDARE)                 *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- PARAMETERS FROM DARE                           *         
*                R6 -- 2ND COMMON AREA BASE REGISTER                  *         
*                R7 -- 3RD PROGRAM BASE                               *         
*                R8 -- 3RD COMMON AREA BASE REGISTER                  *         
*                R9 -- PROGRAM SECOND BASE                            *         
*                RA -- CPRINT                                         *         
*                RB -- PROGRAM BASE                                   *         
*                RC -- COMMON STORAGE AREA                            *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'DDDARESND - TRANSMIT DARE REPORTS VIA APPC/MVS'                 
                                                                                
***********************************************************************         
* PRNT MACRO                                                                    
***********************************************************************         
         MACRO                                                                  
&NAME    PRNT  &A                                                               
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
         GOTO1 =V(PRINTER)                                                      
         MEND                                                                   
                                                                                
***********************************************************************         
* DARE SENDER                                                                   
***********************************************************************         
DARESND  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,DARESND,=A(R13CHAIN),R9,R7                                     
         ENTRY SSB                                                              
         ENTRY UTL                                                              
*                                                                               
         LR    R5,RC                                                            
         L     RC,=A(COMMWORK)     COMMON STORAGE AREA                          
         LR    R6,RC                                                            
         AHI   R6,4096                                                          
         LR    R8,R6                                                            
         AHI   R8,4096                                                          
         USING COMMWORK,RC,R6,R8                                                
*                                                                               
         AHI   R5,-4                                                            
         L     R5,0(R5)            A(R1 PARAMETERS FROM ATTACH)                 
         LR    R4,R5               SAVE THIS PARAMETER                          
         STCM  R5,8,BYTE           HOB IS SHORT SUBTASK NAME                    
         STCM  R5,7,ATSKTAB+1      A(SUBTASK TABLE)                             
         USING TSKTABD,R5                                                       
FINDNTRY CLI   TSKSNAME,X'FF'                                                   
         BNE   *+6                                                              
         DC    H'0'                PARAMETER TO SUBTASK IS BAD                  
         CLC   BYTE,TSKSNAME                                                    
         BE    *+12                R5 NOW POINTS TO OUR TABLE ENTRY             
         LA    R5,TSKTABLQ(,R5)                                                 
         B     FINDNTRY                                                         
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE(15),=C'DARE SENDER TO '                                    
         MVC   TITLE+15(3),TSKNAME                                              
*                                                                               
         ICM   RF,15,VDDSIO                                                     
         ICM   RE,15,TSKDDSIO                                                   
         MVC   0(8,RF),0(RE)                                                    
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(L'SSODSPAC,RF),TSKDSPC                           
*                                                                               
         BAS   RE,INITIAL          INITIALIZE                                   
*                                                                               
         XC    TSKECBSR,TSKECBSR   CLEAR RECEIVER'S STOPPING ECB                
         LA    R2,TSKRCVR          A(RECEIVING MODULE)                          
         LR    R1,R4               SAME PARAMETER THE SENDER GOT                
         ATTACH EPLOC=(R2),SZERO=NO,ETXR=RCVRXIT,ALCOPY=YES                     
         ST    R1,RCVRTCB                                                       
         OC    RCVRTCB,RCVRTCB                                                  
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
         PRNT  ReceiverAttached                                                 
*                                                                               
         L     RF,TSKLGMGR         MQ QUEUE MANAGER NAME                        
         MVC   QMGRNAME,0(RF)                                                   
         LA    R2,CSQBCONN_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     CONNECT TO MQ QUEUE MANAGER                  
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   OPEN_OPTIONS,=A(MQOO_INPUT_AS_Q_DEF)                             
         MVC   OBJDESC_WRKI_OBJECTTYPE,=A(MQOT_Q)    OBJECT IS A QUEUE          
         MVC   OBJDESC_WRKI_OBJECTNAME,SPACES                                   
         LA    R1,OBJDESC_WRKI_OBJECTNAME    'DARE.SSSS.TTT.WORK.QUEUE'         
         MVC   0(5,R1),=CL5'DARE.'                                              
         AHI   R1,5                                                             
*                                                                               
         CLI   TSKDSPC,C'T'                                                     
         BNE   INPQN01                                                          
         MVC   0(5,R1),=CL5'TEST.'                                              
         AHI   R1,5                                                             
         B     INPQN20                                                          
INPQN01  CLI   TSKDSPC,C'Q'                                                     
         BNE   INPQN02                                                          
         MVC   0(4,R1),=CL4'FQA.'                                               
         AHI   R1,4                                                             
         B     INPQN20                                                          
INPQN02  EQU   *                                                                
         MVC   0(5,R1),=CL5'PROD.'                                              
         AHI   R1,5                                                             
INPQN20  MVC   0(L'TSKNAME,R1),TSKNAME                                          
         MVI   L'TSKNAME(R1),C'.'                                               
         AHI   R1,L'TSKNAME+1                                                   
         MVC   0(10,R1),=C'WORK.QUEUE'                                          
*                                                                               
         MVC   P+30(48),OBJDESC_WRKI_OBJECTNAME                                 
         PRNT  AboutToOpenQueue                                                 
*                                                                               
         LA    R2,CSQBOPEN_WRKI_STRUCTURE                                       
         GOTO1 =A(CALLMQ),(RC)     OPEN WORK QUEUE                              
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   LOGFLAG,C'Y'        LOG QUEUE                                    
         BNE   OPNMQLGB                                                         
*                                                                               
         MVC   OBJDESC_LOG_OBJECTTYPE,=A(MQOT_Q)  OBJECT IS A QUEUE             
         L     RF,TSKLOGQ                                                       
         MVC   OBJDESC_LOG_OBJECTNAME,0(RF)       LOG QUEUE NAME                
         MVC   OPEN_OPTIONS,=A(MQOO_OUTPUT+MQOO_SET_IDENTITY_CONTEXT)           
*                                                                               
         LA    R2,CSQBOPEN_LOG_STRUCTURE                                        
         GOTO1 =A(CALLMQ),(RC)     OPEN LOG QUEUE                               
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         JNE   *+2                                                              
*                                                                               
         CLI   LOGFLAG2,C'Y'       SECONDARY LOG QUEUE                          
         BNE   OPNMQLGB                                                         
*                                                                               
         MVC   OBJDESC_LOG2_OBJECTTYPE,=A(MQOT_Q) OBJECT IS A QUEUE             
         L     RF,TSKLOGQ2                                                      
         MVC   OBJDESC_LOG2_OBJECTNAME,0(RF)      LOG QUEUE NAME                
         MVC   OPEN_OPTIONS,=A(MQOO_OUTPUT+MQOO_SET_IDENTITY_CONTEXT)           
*                                                                               
         LA    R2,CSQBOPEN_LOG2_STRUCTURE                                       
         GOTO1 =A(CALLMQ),(RC)     OPEN LOG QUEUE                               
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         JNE   *+2                                                              
*                                                                               
OPNMQLGB CLI   LOGFLAGB,C'Y'       BLOCKCHAIN QUEUE                             
         BNE   OPNMQLGX                                                         
*                                                                               
         MVC   OBJDESC_LOGB_OBJECTTYPE,=A(MQOT_Q) OBJECT IS A QUEUE             
         L     RF,TSKLOGQB                                                      
         MVC   OBJDESC_LOGB_OBJECTNAME,0(RF)      OUTPUT QUEUE NAME             
         MVC   OPEN_OPTIONS,=A(MQOO_OUTPUT+MQOO_SET_IDENTITY_CONTEXT)           
*                                                                               
         LA    R2,CSQBOPEN_LOGB_STRUCTURE                                       
         GOTO1 =A(CALLMQ),(RC)     OPEN BLOCKCHAIN QUEUE                        
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         JNE   *+2                                                              
*                                                                               
OPNMQLGX LA    R1,TSKECBSS                                                      
         STCM  R1,7,ASTPECB2+1     SET A(STOPPING ECB)                          
*                                                                               
         CLI   TSKMETH,METHAPPC    APPC/MVS?                                    
         BNE   MQINIT              NO, MQ SERIES                                
*                                                                               
REGALLOC MVC   TP_NAME_LENGTH,=F'8'                                             
         MVC   TP_NAME(7),TSKTPNAM                                              
         MVI   TP_NAME+7,C'S'      'S' FOR SENDER                               
         MVC   LOCAL_LU_NAME,TSKLUID                                            
*                                                                               
         LA    R2,ATBRFA2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   REGISTER FOR ALLOCATES                       
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR WANTS TO STOP?                      
         BE    STOPRCVR                                                         
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    RCVALLOC            WE ARE REGISTERED FOR ALLOCATES              
         CLC   RETURN_CODE,ATBCTS_REQUEST_UNSUCCESSFUL                          
         BNE   CANTREG                                                          
         CLC   REASON_CODE,ATBCTS_INVAL_LOCAL_LU                                
         BNE   CANTREG             WE CAN'T REGISTER                            
*                                                                               
         MVC   OPMSG5+30(8),LOCAL_LU_NAME                                       
         MVC   OPMSG5+48(3),TSKNAME                                             
NOTQUITE GOTO1 =V(LOGIO),DMCB,1,(L'OPMSG5,OPMSG5)                               
         GOTO1 =V(LOGIO),DMCB,0,(6,WORK)                                        
         CLI   WORK,C'R'           DOES OPERATOR WANT TO RETRY?                 
         BE    REGALLOC                                                         
         CLI   WORK,C'C'           DOES OPERATOR WANT TO CANCEL?                
         BNE   NOTQUITE                                                         
         ABEND 705,DUMP                                                         
*                                                                               
CANTREG  MVC   P+30(14),=C'REASON CODE = '                                      
         EDIT  REASON_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         PRNT  CouldNotRegister                                                 
         B     STOPRCVR                                                         
*                                                                               
RCVALLOC MVC   RECEIVE_ALLOCATE_TYPE,ATBCTS_WAIT    ASSUME NO TIMER             
         LHI   R1,60               60 SECONDS/MINUTE. . .                       
         MH    R1,TSKTIMER         . . . TIMES NUMBER OF MINUTES                
         LTR   R1,R1               . . . EQUALS NUMBER OF SECONDS               
         BZ    *+14                NO TIMEOUT VALUE FOR THIS PARTNER            
         MVC   RECEIVE_ALLOCATE_TYPE,ATBCTS_TIMED                               
         ST    R1,TIME_OUT_VALUE                                                
*                                                                               
         LA    R2,ATBRAL2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   RECEIVE ALLOCATE                             
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR WANTS TO STOP?                      
         BE    STOPRCVR                                                         
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    INCONV              WE HAVE A CONVERSATION                       
         CLC   RETURN_CODE,ATBCTS_REQUEST_UNSUCCESSFUL                          
         BNE   NOCONV                                                           
         CLC   REASON_CODE,ATBCTS_NO_ALLOC_TO_RECEIVE                           
         BNE   NOCONV                                                           
*                                                                               
         MVC   OPMSG6+23(3),TSKNAME  TIMEOUT: DISPLAY CONSOLE WARNING           
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMSG6,OPMSG6)                     
         B     RCVALLOC            TRY TO RECEIVE ALLOCATE AGAIN                
*                                                                               
NOCONV   PRNT  NoConversation                                                   
         B     STOPRCVR                                                         
*                                                                               
INCONV   PRNT  OpenDareSender                                                   
*                                                                               
         MVC   TSKSCONV,CONVERSATION_ID                                         
         GOTO1 =A(QUERYALQ),(RC)   QUERY ALLOCATE QUEUE                         
         GOTO1 =A(PRNTATTB),(RC)   PRINT CONVERSATION ATTRIBUTES                
*                                                                               
         CLC   CONVERSATION_TYPE,ATB_BASIC_CONVERSATION                         
         BNE   NOTBASIC                                                         
         LA    R1,BUFFER_LENGTH    BASIC STARTS BUFFER WITH RECORD LEN          
         STCM  R1,7,ATBRCVW_BUFFER_PARAMETER+1                                  
         STCM  R1,7,ATBSEND_BUFFER_PARAMETER+1                                  
*                                                                               
NOTBASIC MVC   FILL,ATB_FILL_LL                                                 
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
*                                                                               
         LA    R2,ATBRCVW_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   RECEIVE AND WAIT                             
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    STOP                                                             
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BNZ   BADSHAKE            BAD RETURN CODE                              
*                                                                               
         CLC   STATUS_RECEIVED,ATB_CONFIRM_RECEIVED                             
         BNE   NOCONFRM                                                         
*                                                                               
         LA    R2,ATBCFMD_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   ISSUE CONFIRM                                
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    STOP                                                             
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BNZ   BADSHAKE            BAD RETURN CODE                              
         B     NOTBASIC            RECEIVE AGAIN                                
*                                                                               
NOCONFRM CLC   STATUS_RECEIVED,ATB_SEND_RECEIVED                                
         BE    SNDSHAKE                                                         
*                                                                               
         PRNT  ReceiveAgain                                                     
         MVC   FILL,ATB_FILL_LL                                                 
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
*                                                                               
         LA    R2,ATBRCVW_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   RECEIVE AND WAIT                             
*                                                                               
         CLC   STATUS_RECEIVED,ATB_SEND_RECEIVED                                
         BNE   BADSHAKE            WHY WON'T THEY LET US SEND?                  
*                                                                               
SNDSHAKE PRNT  SwitchingToSend                                                  
         CLC   CONVERSATION_TYPE,ATB_MAPPED_CONVERSATION                        
         BNE   *+12                                                             
         ICM   R2,15,RECEIVE_LENGTH LENGTH OF RETURNED DATA (MAPPED)            
         B     *+12                                                             
         LH    R2,BUFFER_LENGTH    LENGTH OF RETURNED DATA                      
         AHI   R2,-2               MINUS 2 BYTES FOR LENGTH ITSELF              
         BNP   BADSHAKE                                                         
*                                                                               
         LA    R1,BUFFER           ASSUME MAPPED CONVERSATION                   
         CLC   CONVERSATION_TYPE,ATB_BASIC_CONVERSATION                         
         BNE   *+12                                                             
         LA    R1,BUFFER_LENGTH                                                 
         AHI   R2,2                R2 = TOTAL AREA LENGTH (FOR PRNTBL)          
         ST    R1,DMCB+4                                                        
         GOTO1 =V(PRNTBL),DMCB,0,,C'DUMP',(R2),=C'1D'                           
*                                                                               
         CLC   APPCM1(APPCM1Q),BUFFER                                           
         BNE   BADSHAKE            PARTNER DID NOT RESPOND READY                
         CLC   APPCM1A(APPCM1AQ),BUFFER+19                                      
         BNE   BADSHAKE            WRONG MODE OR VERSION NUMBER                 
*                                                                               
         PRNT  AboutToRespond                                                   
         XC    BUFFER,BUFFER                                                    
         LHI   R3,APPCM2AQ                                                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   BUFFER(0),APPCM2    INITIAL SIGNON                               
         GOTO1 =A(DATETIME),(RC)   PUT DATE/TIME INTO MESSAGE                   
         MVC   BUFFER+47(8),YYYYMMDD                                            
         MVC   BUFFER+61(6),HHMMSS                                              
         AHI   R3,1                R3 = L'DATA                                  
         MVC   SEND_TYPE,ATB_SEND_AND_CONFIRM                                   
         GOTO1 =A(DRXMTLAP),(RC)                                                
         BNE   BADSHAKE                                                         
         PRNT  ConfirmReceived                                                  
         B     DONTSTOP                                                         
*                                                                               
BADSHAKE PRNT  BadHandshake                                                     
         MVC   OPMSG1+34(3),TSKNAME                                             
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMSG1,OPMSG1)                     
         B     STOP                HANDSHAKE FAILED                             
*                                                                               
DONTSTOP MVC   OPMSG4+29(3),TSKNAME                                             
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMSG4,OPMSG4)                     
         EJECT                                                                  
MQINIT   EQU   *                                                                
         BRAS  RE,SENDRPTM         ANY MESSAGES READY TO SEND                   
         EJECT                                                                  
FINDMORE EQU   *                                                                
*                                                                               
WAIT     PRNT  WaitForSomething                                                 
         WAIT  1,ECBLIST=ECBLST    WAIT FOR SOMETHING EXCITING                  
         PRNT  HeresSomething                                                   
*                                                                               
         TM    RCVRECB,X'40'       IS DARE RECEIVER STILL RUNNING?              
         BZ    CHKRPT              YES                                          
         CLI   TSKMETH,METHAPPC    NO -- APPC/MVS?                              
         BNE   STOPRCVR            NO, MQ SERIES                                
         MVC   DEALLOCATE_TYPE,ATB_DEALLOCATE_ABEND                             
         B     DEALLOC                                                          
*                                                                               
CHKRPT   EQU   *                                                                
         TM    WORKQECB,X'40'      DID AN MQ REPORT MESSAGE ARRIVE?             
         BZ    CHKSTOP             NO                                           
         BRAS  RE,SENDRPTM         GET THE INCOMING MESSAGE(S)                  
         B     WAIT                WAIT FOR SOMETHING ELSE TO HAPPEN            
*                                                                               
CHKSTOP  TM    TSKECBSS,X'40'      OPERATOR REQUESTED STOP?                     
         BO    *+6                                                              
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
STOP     CLI   TSKMETH,METHAPPC    APPC/MVS?                                    
         BNE   STOPRCVR            NO, MQ SERIES                                
*                                                                               
         MVC   DEALLOCATE_TYPE,ATB_DEALLOCATE_SYNC_LEVEL                        
DEALLOC  LA    R2,ATBDEAL_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   DEALLOCATE THE CONVERSATION                  
*                                                                               
         PRNT  DareSenderClosed                                                 
*                                                                               
STOPRCVR PRNT  StoppingDareRCV                                                  
         POST  TSKECBSR            TELL DARERCV TO STOP                         
*                                                                               
         WAIT  ECB=RCVRECB                                                      
         TM    RCVRECB,X'40'                                                    
         BO    *+6                                                              
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
         OC    RCVRCOMP,RCVRCOMP                                                
         BZ    UNREGSTR            RECEIVER ENDED CLEANLY                       
         MVC   OPMSG3+20(3),TSKNAME                                             
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMSG3,OPMSG3)                     
*                                                                               
UNREGSTR CLI   TSKMETH,METHAPPC    APPC/MVS?                                    
         BNE   MQWRAPUP            NO, MQ SERIES                                
*                                                                               
         OC    ALLOCATE_QUEUE_TOKEN,ALLOCATE_QUEUE_TOKEN                        
         BZ    CLOSEMQL            WE NEVER REGISTERED FOR ALLOCATES            
*                                                                               
         LA    R2,ATBURA2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   UNREGISTER FOR ALLOCATES                     
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    CLOSEMQL            RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
MQWRAPUP EQU   *                                                                
*                                                                               
CLOSEMQL MVC   CLOSE_OPTIONS,=A(MQCO_NONE)                                      
         LA    R2,CSQBCLOS_WRKI_STRUCTURE                                       
         GOTO1 =A(CALLMQ),(RC)     CLOSE WORK INPUT QUEUE                       
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   LOGFLAG,C'Y'        LOG QUEUE                                    
         BNE   CLOMQLGB                                                         
*                                                                               
         MVC   CLOSE_OPTIONS,=A(MQCO_NONE)                                      
         LA    R2,CSQBCLOS_LOG_STRUCTURE                                        
         GOTO1 =A(CALLMQ),(RC)     CLOSE LOG QUEUE                              
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         JNE   *+2                                                              
*                                                                               
         CLI   LOGFLAG2,C'Y'       SECONDARY LOG QUEUE                          
         BNE   CLOMQLGB                                                         
*                                                                               
         MVC   CLOSE_OPTIONS,=A(MQCO_NONE)                                      
         LA    R2,CSQBCLOS_LOG2_STRUCTURE                                       
         GOTO1 =A(CALLMQ),(RC)     CLOSE LOG QUEUE 2                            
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         JNE   *+2                                                              
*                                                                               
CLOMQLGB CLI   LOGFLAGB,C'Y'       BLOCKCHAIN QUEUE                             
         BNE   CLOMQLGX                                                         
*                                                                               
         MVC   CLOSE_OPTIONS,=A(MQCO_NONE)                                      
         LA    R2,CSQBCLOS_LOGB_STRUCTURE                                       
         GOTO1 =A(CALLMQ),(RC)     CLOSE BLOCKCHAIN QUEUE                       
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         JNE   *+2                                                              
*                                                                               
CLOMQLGX EQU   *                                                                
*                                                                               
         LA    R2,CSQBDISC_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     DISCONNECT FROM MQ QUEUE MANAGER             
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         JNE   *+2                                                              
*                                                                               
GOODBYE  PRNT  *****************                                                
         PRNT  *****Exiting*****                                                
         PRNT  *****************                                                
*                                                                               
         XBASE                                                                  
                                                                                
***********************************************************************         
* INITIALIZE                                                                    
***********************************************************************         
INITIAL  NTR1                                                                   
         L     RF,=V(PRNTER)       A(SYSPRINT DCB)                              
         USING IHADCB,RF                                                        
         MVC   DCBDDNAM(3),TSKNAME                                              
         MVC   DCBDDNAM+3(5),=C'SEND '                                          
         MVC   TXTDD+6(8),DCBDDNAM                                              
         DROP  RF                                                               
         LA    R1,ARBLK                                                         
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         BZ    *+16                                                             
         CLC   =X'0410',RBLK+4                                                  
         BE    *+6                 APPEND TO PREVIOUS LOG                       
         DC    H'0'                                                             
*                                                                               
         PRNT  *****************                                                
         PRNT  ***Initialize****                                                
         PRNT  *****************                                                
*                                                                               
         L     R2,PSATOLD-PSA(,0)  TCB ADDRESS                                  
         GOTO1 =V(HEXOUT),DMCB,(R2),P+30,4,=C'TOG'                              
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         PRNT  TCBAddress                                                       
*                                                                               
         LA    R1,TSKECBSS         COMPLETE THE ECBLIST                         
         STCM  R1,7,ASTOPECB+1                                                  
*                                                                               
         L     R4,=A(ENTRYPTS)     ENTRYPTS ADDRESS                             
         USING ENTRYPTS,R4                                                      
*                                                                               
         BLDL  0,ENTRYPTS          BUILD LIST OF APPC/MVS ENTRY PTS             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                BAD RETURN FROM BLDL MACRO                   
*                                                                               
         CLI   TSKMETH,METHAPPC    APPC/MVS?                                    
         BNE   INIT10              NO, MUST BE MQ SERIES                        
         LOAD  DE=ATBDEAL                                                       
         ST    R0,ATBDEAL_STRUCTURE                                             
         LOAD  DE=ATBCFMD                                                       
         ST    R0,ATBCFMD_STRUCTURE                                             
         LOAD  DE=ATBEES3                                                       
         ST    R0,ATBEES3_STRUCTURE                                             
         LOAD  DE=ATBRAL2                                                       
         ST    R0,ATBRAL2_STRUCTURE                                             
         LOAD  DE=ATBRFA2                                                       
         ST    R0,ATBRFA2_STRUCTURE                                             
         LOAD  DE=ATBGTA2                                                       
         ST    R0,ATBGTA2_STRUCTURE                                             
         LOAD  DE=ATBRCVW                                                       
         ST    R0,ATBRCVW_STRUCTURE                                             
         LOAD  DE=ATBSEND                                                       
         ST    R0,ATBSEND_STRUCTURE                                             
         LOAD  DE=ATBQAQ2                                                       
         ST    R0,ATBQAQ2_STRUCTURE                                             
         LOAD  DE=ATBURA2                                                       
         ST    R0,ATBURA2_STRUCTURE                                             
*                                  FOR MQ LOGGING                               
         LOAD  DE=CSQBCONN                                                      
         ST    R0,CSQBCONN_STRUCTURE                                            
         LOAD  DE=CSQBOPEN                                                      
         ST    R0,CSQBOPEN_WRKI_STRUCTURE                                       
         ST    R0,CSQBOPEN_WRKO_STRUCTURE                                       
         ST    R0,CSQBOPEN_LOG_STRUCTURE                                        
         ST    R0,CSQBOPEN_LOG2_STRUCTURE                                       
         ST    R0,CSQBOPEN_LOGB_STRUCTURE                                       
         LOAD  DE=CSQBGET                                                       
         ST    R0,CSQBGET_WRKI_STRUCTURE                                        
         LOAD  DE=CSQBPUT                                                       
         ST    R0,CSQBPUT_WRKO_STRUCTURE                                        
         ST    R0,CSQBPUT_LOG_STRUCTURE                                         
         ST    R0,CSQBPUT_LOG2_STRUCTURE                                        
         ST    R0,CSQBPUT_LOGB_STRUCTURE                                        
         LOAD  DE=CSQBCOMM                                                      
         ST    R0,CSQBCOMM_STRUCTURE                                            
         LOAD  DE=CSQBCLOS                                                      
         ST    R0,CSQBCLOS_STRUCTURE                                            
         ST    R0,CSQBCLOS_WRKI_STRUCTURE                                       
         ST    R0,CSQBCLOS_WRKO_STRUCTURE                                       
         ST    R0,CSQBCLOS_LOG_STRUCTURE                                        
         ST    R0,CSQBCLOS_LOG2_STRUCTURE                                       
         ST    R0,CSQBCLOS_LOGB_STRUCTURE                                       
         LOAD  DE=CSQBDISC                                                      
         ST    R0,CSQBDISC_STRUCTURE                                            
*                                                                               
         SAM31                                                                  
         L     R0,=A(1*MQBUFLQ)          GET 1 MESSAGE BUFFERS                  
         GETMAIN RU,LV=(0),LOC=(ANY,ANY),BNDRY=PAGE                             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,AMQWRKXA                                                      
         ST    R1,A_GET_WRKI_BUFFER                                             
         MVC   BUFFLEN_WRKI,=A(MQBUFLQ)                                         
         A     R1,=A(MQBUFLQ)                                                   
         ST    R1,AMQWRKXX                                                      
         SAM24                                                                  
         B     INIT80                                                           
*                                                                               
INIT10   CLI   TSKMETH,METHMQ      MQ SERIES?                                   
         BE    *+6                                                              
         DC    H'0'                IF IT AIN'T APPC OR MQ, WE'RE DEAD           
         LOAD  DE=CSQBCONN                                                      
         ST    R0,CSQBCONN_STRUCTURE                                            
         LOAD  DE=CSQBOPEN                                                      
         ST    R0,CSQBOPEN_STRUCTURE                                            
         ST    R0,CSQBOPEN_WRKI_STRUCTURE                                       
         ST    R0,CSQBOPEN_WRKO_STRUCTURE                                       
         ST    R0,CSQBOPEN_LOG_STRUCTURE                                        
         ST    R0,CSQBOPEN_LOG2_STRUCTURE                                       
         ST    R0,CSQBOPEN_LOGB_STRUCTURE                                       
         LOAD  DE=CSQBGET                                                       
         ST    R0,CSQBGET_WRKI_STRUCTURE                                        
         LOAD  DE=CSQBPUT                                                       
         ST    R0,CSQBPUT_STRUCTURE                                             
         ST    R0,CSQBPUT_WRKO_STRUCTURE                                        
         ST    R0,CSQBPUT_LOG_STRUCTURE                                         
         ST    R0,CSQBPUT_LOG2_STRUCTURE                                        
         ST    R0,CSQBPUT_LOGB_STRUCTURE                                        
         LOAD  DE=CSQBCOMM                                                      
         ST    R0,CSQBCOMM_STRUCTURE                                            
         LOAD  DE=CSQBCLOS                                                      
         ST    R0,CSQBCLOS_STRUCTURE                                            
         ST    R0,CSQBCLOS_WRKI_STRUCTURE                                       
         ST    R0,CSQBCLOS_WRKO_STRUCTURE                                       
         ST    R0,CSQBCLOS_LOG_STRUCTURE                                        
         ST    R0,CSQBCLOS_LOG2_STRUCTURE                                       
         ST    R0,CSQBCLOS_LOGB_STRUCTURE                                       
         LOAD  DE=CSQBDISC                                                      
         ST    R0,CSQBDISC_STRUCTURE                                            
*                                                                               
         SAM31                                                                  
         L     R0,=A(2*MQBUFLQ)          GET 2 MESSAGE BUFFERS                  
         GETMAIN RU,LV=(0),LOC=(ANY,ANY),BNDRY=PAGE                             
         LTR   RF,RF                                                            
         BZ    INIT20                                                           
         DC    H'0'                                                             
INIT20   ST    R1,AMQBUFXA                                                      
         A     R1,=A(MQBUFLQ)                                                   
         ST    R1,AMQBUFXX                                                      
*                                                                               
         ST    R1,AMQWRKXA                                                      
         ST    R1,A_GET_WRKI_BUFFER                                             
         MVC   BUFFLEN_WRKI,=A(MQBUFLQ)                                         
         A     R1,=A(MQBUFLQ)                                                   
         ST    R1,AMQWRKXX                                                      
         SAM24                                                                  
*                                                                               
INIT80   MVI   LOGFLAG,C'Y'                                                     
         L     RE,TSKLOGQ                                                       
         CLC   0(L'TSKLOGQ,RE),SPACES                                           
         BH    INIT85                                                           
         MVI   LOGFLAG,C'N'                                                     
         B     INIT90                                                           
*                                                                               
INIT85   MVI   LOGFLAG2,C'Y'                                                    
         L     RE,TSKLOGQ2                                                      
         CLC   0(L'TSKLOGQ2,RE),SPACES                                          
         BH    INIT90                                                           
         MVI   LOGFLAG2,C'N'                                                    
*                                                                               
INIT90   MVI   LOGFLAGB,C'Y'                                                    
         L     RE,TSKLOGQB                                                      
         CLC   0(L'TSKLOGQB,RE),SPACES                                          
         BH    INITX                                                            
         MVI   LOGFLAGB,C'N'                                                    
*                                                                               
INITX    XIT1                                                                   
         LTORG                                                                  
         DROP  R4                                                               
                                                                                
***********************************************************************         
* THIS IS THE RECEIVING SUBTASK'S EXIT ROUTINE.                                 
***********************************************************************         
RCVRXIT  SAVE  (14,12),,*                                                       
         LR    RB,RF                                                            
         USING RCVRXIT,RB                                                       
*                                                                               
         LR    R2,R1               A(RECEIVER TCB)                              
         MVC   RCVRCOMP,TCBCMPC-TCB(R2)                                         
         DETACH RCVRTCB                                                         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
*                                                                               
         POST  RCVRECB             TELLS SENDER THAT RECEIVER ENDED             
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,7,ASTOPECB+1                                                  
         POST  (3)                 TELLS SENDER TO STOP                         
*                                                                               
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
         SPACE 2                                                                
RCVRTCB  DS    F                   TCB OF ATTACHED SUBTASK DARERCV              
RCVRECB  DC    F'0'                                                             
RCVRCOMP DS    XL3                 RECEIVER'S COMPLETION CODE                   
         SPACE 3                                                                
ECBLST   DS    0F                                                               
         DC    X'00',AL3(RCVRECB)  A(ECB: RECEIVER ENDED)                       
         DC    X'00',AL3(WORKQECB) A(ECB: MQ WORK MESSAGE HAS ARRIVED)          
ASTOPECB DC    X'80',AL3(0)        A(STOPECB)                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
DRXMTLAP NMOD1 0,DRXMTLAP                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* TRANSMIT ONE RECORD VIA APPC/MVS.                                             
* ON ENTRY, R3 = LENGTH OF DATA                                                 
*                                                                               
         LR    R2,R3                                                            
         AHI   R2,2                ADD 2 FOR CRLF                               
         LA    RE,BUFFER           ASSUME MAPPED                                
         CLC   CONVERSATION_TYPE,ATB_BASIC_CONVERSATION                         
         BNE   *+12                                                             
         LA    RE,BUFFER_LENGTH                                                 
         AHI   R2,2                ADD 2 MORE FOR LENGTH                        
         STH   R2,BUFFER_LENGTH                                                 
         ST    R2,SEND_LENGTH                                                   
         LA    RF,BUFFER(R3)                                                    
         MVC   0(2,RF),CRLF        TERMINATE RECORD WITH CRLF                   
*                                                                               
         ST    RE,DMCB+4                                                        
         LA    RE,=C'BUFFER DATA'                                               
         ST    RE,DMCB                                                          
         MVI   DMCB,11                                                          
         CLC   SEND_TYPE,ATB_BUFFER_DATA                                        
         BE    DRXMTA10                                                         
         LA    RE,=C'SEND AND CONFIRM'                                          
         ST    RE,DMCB                                                          
         MVI   DMCB,16                                                          
         CLC   SEND_TYPE,ATB_SEND_AND_CONFIRM                                   
         BE    DRXMTA10                                                         
         DC    H'0'                UNKNOWN SEND TYPE                            
DRXMTA10 GOTO1 =V(PRNTBL),DMCB,,,C'DUMP',(R2),=C'1D'                            
*                                                                               
         LA    R2,ATBSEND_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   SEND DATA                                    
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+10                                                             
         LTR   RB,RB               NO GOOD -- SET CC NOT EQUAL                  
         B     *+6                                                              
         CR    RB,RB               SET CC EQUAL                                 
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
DRXMTLMQ NMOD1 0,DRXMTLMQ                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PUT ONE RECORD TO AN MQ QUEUE.                                                
* ON ENTRY, R3 = LENGTH OF DATA                                                 
* IF BYTE = C'Y', THEN ASK FOR COA (OTHERWISE, DON'T)                           
*                                                                               
         CLI   MQOUTVER,C'2'       NEW OUTPUT VERSION?                          
         BNE   DRXMTM10            NO                                           
         GOTO1 =A(DRXTLMQ2),(RC)   YES                                          
         BE    DRXMTMOK                                                         
         DC    H'0'                                                             
*                                                                               
DRXMTM10 GOTO1 =V(PRNTBL),DMCB,=C'MQ_PUT',BUFFER,C'DUMP',(R3),=C'1D'            
*                                                                               
         MVC   PUTMSGOPTS_OPTIONS,=A(MQPMO_FAIL_IF_QUIESCING)                   
         MVC   MSGDESC_REPORT,=A(MQRO_NONE)                                     
         MVC   MSGDESC_REPLYTOQ,SPACES                                          
         MVC   MSGDESC_PERSISTENCE,=A(MQPER_PERSISTENT)                         
         ST    R3,BUFFERLENGTH                                                  
*                                                                               
         MVC   MSGDESC_CORRELID,SPACES                                          
         MVC   MSGDESC_CORRELID(L'COA####),COA####                              
*                                                                               
         CLI   BYTE,C'Y'                                                        
         BNE   DRXMTM30                                                         
*                                                                               
         L     RF,=A(MQRO_EXCEPTION)                                            
         LA    RE,MQRO_COA                                                      
         AR    RF,RE                                                            
*        LA    RE,MQRO_COD                                                      
*        AR    RF,RE                                                            
         LA    RE,MQRO_PASS_CORREL_ID                                           
         AR    RF,RE                                                            
         ST    RF,MSGDESC_REPORT                                                
*                                                                               
         L     RF,TSKRPLYQ                                                      
         MVC   MSGDESC_REPLYTOQ,0(RF)                                           
*                                                                               
         MVC   P+30(L'MSGDESC_CORRELID),MSGDESC_CORRELID                        
         PRNT  CorrelID                                                         
*                                                                               
DRXMTM30 LA    R1,BUFFER                                                        
         ST    R1,A_PUT_BUFF                                                    
         LA    R2,CSQBPUT_STRUCTURE                                             
         GOTO1 =A(CALLMQ),(RC)     PUT ONE RECORD TO QUEUE                      
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    *+10                                                             
         LTR   RB,RB               NO GOOD -- SET CC NOT EQUAL                  
         B     *+6                                                              
DRXMTMOK CR    RB,RB               SET CC EQUAL                                 
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
DRXTLMQ2 NMOD1 0,DRXTLMQ2                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* BUFFER ONE RECORD INTO MQ BUFFER UNTIL WE GET LAST RECORD                     
* ON ENTRY, R3 = LENGTH OF DATA                                                 
* IF BYTE = C'Y', LAST RECORD, MQPUT THE MESSAGE WITH COA                       
*                                                                               
*                                  CHECK IF RECORD CAN FIT                      
         L     RE,AMQBUFPT         CURRENT BUFFER POINT                         
         AR    RE,R3               NEXT RECORD LENGTH                           
         AHI   RE,2                + CRLF                                       
         C     RE,AMQBUFXX                                                      
         BNH   *+6                                                              
         DC    H'0'                NEED TO INCREASE BUFFER SIZE                 
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'MQ_BUFF',BUFFER,C'DUMP',(R3),=C'1D'           
*                                                                               
         SAM31                                                                  
         L     RE,AMQBUFPT         MQ BUFFER CURRENT POINTER                    
         BCTR  R3,0                LENGTH-1                                     
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),BUFFER      COPY RECORD FROM BUFFER AREA                 
         AR    RE,R3                                                            
         MVC   1(L'CRLF,RE),CRLF   APPEND CRLF AT THE END                       
         AHI   RE,L'CRLF+1                                                      
         ST    RE,AMQBUFPT                                                      
         SAM24                                                                  
*                                                                               
         CLI   BYTE,C'Y'           LAST RECORD?                                 
         BNE   DRXTM2OK            NO - EXIT                                    
*                                                                               
         L     R3,AMQBUFPT         GET LENGTH OF ENTIRE MESSAGE IN R3           
         L     RE,AMQBUFXA                                                      
         SR    R3,RE                                                            
         ST    R3,BUFFERLENGTH                                                  
*                                                                               
         MVC   PUTMSGOPTS_OPTIONS,=A(MQPMO_FAIL_IF_QUIESCING)                   
         MVC   MSGDESC_REPORT,=A(MQRO_NONE)                                     
         MVC   MSGDESC_REPLYTOQ,SPACES                                          
         MVC   MSGDESC_PERSISTENCE,=A(MQPER_PERSISTENT)                         
*                                                                               
         MVC   MSGDESC_CORRELID,SPACES                                          
         MVC   MSGDESC_CORRELID(L'COA####),COA####                              
*                                                                               
         L     RF,=A(MQRO_EXCEPTION)                                            
         LA    RE,MQRO_COA                                                      
         AR    RF,RE                                                            
*        LA    RE,MQRO_COD                                                      
*        AR    RF,RE                                                            
         LA    RE,MQRO_PASS_CORREL_ID                                           
         AR    RF,RE                                                            
         ST    RF,MSGDESC_REPORT                                                
*                                                                               
         L     RF,TSKRPLYQ                                                      
         MVC   MSGDESC_REPLYTOQ,0(RF)                                           
*                                                                               
*********SPECIAL CODE WO ONLY********************************                   
         CLC   =C'QM_WOX',OBJDESC_OBJECTNAME                                    
         BNE   NOTWO2                                                           
         CLI   TSKDSPC,C'T'                                                     
         BNE   *+14                                                             
         MVC   MSGDESC_REPLYTOQ,=CL48'DARE.TEST.WO.REPLY.QUEUE'                 
         B     NOTWO2                                                           
         CLI   TSKDSPC,C'A'                                                     
         BNE   *+14                                                             
         MVC   MSGDESC_REPLYTOQ,=CL48'DARE.PROD.WO.REPLY.QUEUE'                 
         B     NOTWO2                                                           
NOTWO2   EQU   *                                                                
*************************************************************                   
*                                                                               
*                                                                               
         MVC   P+30(L'MSGDESC_CORRELID),MSGDESC_CORRELID                        
         PRNT  CorrelID                                                         
*                                                                               
         L     R1,AMQBUFXA                                                      
         ST    R1,A_PUT_BUFF                                                    
         LA    R2,CSQBPUT_STRUCTURE                                             
         GOTO1 =A(CALLMQ),(RC)     PUT ONE RECORD TO QUEUE                      
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    *+10                                                             
DRXTM2NO LTR   RB,RB               NO GOOD -- SET CC NOT EQUAL                  
         B     *+6                                                              
DRXTM2OK CR    RB,RB               SET CC EQUAL                                 
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
FINDOUTQ NMOD1 0,FINDOUTQ                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* A MESSAGE IS TO BE SENT VIA MQSERIES. FIND THE ASSOCIATED OUTPUT              
* QUEUE NAME FOR THIS RECEIVER, AND PLACE THE ADDRESS OF THE QUEUE              
* NAME IN FIELD 'QUEUENAME'.                                                    
* ON INPUT, R2 = RECEIVER'S NUMERIC USERID.                                     
*                                                                               
         LA    R4,=C'UIDTABL'                                                   
         MVC   P+30(8),DARE                                                     
         MVC   P+40(7),0(R4)                                                    
         PRNT  Enqueue                                                          
         ENQ   (DARE,(4),E,7)      ENQUEUE THE USERID TABLE                     
*                                                                               
         L     R3,TSKUIDTB         TABLE OF USERIDS/PARTNERS                    
         USING UIDTABD,R3                                                       
FO10     CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                USERID HAS VANISHED                          
         CLM   R2,3,UIDNUM                                                      
         BE    *+12                                                             
         LA    R3,UIDTBLQ(,R3)     BUMP TO NEXT ENTRY                           
         B     FO10                                                             
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,3,UIDMQID        MQ QUEUE ID NUMBER FOR THIS USERID           
*                                                                               
         MVC   P+30(8),DARE                                                     
         MVC   P+40(7),0(R4)                                                    
         PRNT  Dequeue                                                          
         DEQ   (DARE,(4),7)        DEQUEUE THE USERID TABLE                     
*                                                                               
         LA    R4,=C'MQTABLE'                                                   
         MVC   P+30(8),DARE                                                     
         MVC   P+40(7),0(R4)                                                    
         PRNT  Enqueue                                                          
         ENQ   (DARE,(4),E,7)      ENQUEUE THE MQ CONTROL TABLE                 
*                                                                               
         L     R3,TSKMQTAB         A(MQ CONTROL TABLE)                          
FO20     CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                MISSING TABLE ENTRY                          
         USING MQTABLED,R3                                                      
         CH    R2,MQQIDNUM                                                      
         BE    *+12                GOT MATCH ON QUEUE ID NUMBER                 
         LA    R3,MQTABLEQ(,R3)    BUMP TO NEXT ENTRY                           
         B     FO20                                                             
         MVC   QUEUENAME,MQQNAME                                                
         MVC   MQOUTVER,MQQOUTVR   GET OUTPUT FORMAT VERSION                    
         DROP  R3                                                               
*                                                                               
         MVC   P+30(8),DARE                                                     
         MVC   P+40(7),0(R4)                                                    
         PRNT  Dequeue                                                          
         DEQ   (DARE,(4),7)        DEQUEUE THE MQ CONTROL TABLE                 
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
                                                                                
***********************************************************************         
* PUT SMF RECORD                                                                
* ON ENTRY R2 = L'RECORD                                                        
*          R3 = A(RECORD TO PUT TO SMF)                                         
***********************************************************************         
PUTSMF   NMOD1 0,PUTSMF                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING SMFRECD,R4                                                       
         AHI   R2,SMFOVLQ          RECORD LENGTH PLUS OVERHEAD                  
         STCM  R2,3,SMFRECLN       SMF RECORD LENGTH (WITHOUT LENGTH)           
         TIME  DEC                                                              
         SRL   R0,4                SHIFT OUT HUNDRETHS                          
         O     R0,=X'0000000F'     TIME IS NOW 0HHMMSSF                         
         STCM  R0,15,SMFTIME       TIME                                         
         STCM  R1,15,SMFDATE       DATE (0CYYDDDF)                              
         MVI   SMFVER,X'01'        VERSION NUMBER                               
         CLI   TSKTSMF,C'Y'        FLAG THESE AS TEST RECORDS?                  
         BNE   *+8                                                              
         OI    SMFFLAGS,SMFTEST    YES                                          
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   SMFDATA(0),0(R3)    DARE RECORD                                  
*                                  SMFOUT TYPE 6 FOR DARE                       
         GOTO1 =V(SMFOUT),DMCB,6,WORK                                           
         DROP  R4                                                               
*                                                                               
* STOP PRINTING SMF RECORD                                                      
*        AHI   R2,1                RESTORE RECORD LENGTH                        
*        AHI   R2,2                ADD TWO FOR LENGTH ITSELF                    
*        GOTO1 =V(PRNTBL),DMCB,=C'SMF RECORD',(R4),C'DUMP',(R2),=C'1D'          
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
                                                                                
***********************************************************************         
DATETIME NMOD1 0,DATETIME                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         TIME  DEC                                                              
         ST    R0,PRNTDUB                                                       
         MVI   PRNTDUB+4,X'0F'                                                  
         UNPK  PRNTTIME,PRNTDUB(5)                                              
         MVC   HHMMSS,PRNTTIME                                                  
         GOTO1 =V(DATCON),DMCB,(5,0),(20,YYYYMMDD)                              
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
                                                                                
***********************************************************************         
* PRINT DETAILS ABOUT THE CURRENT STATE OF THE LU6.2 CONVERSATION               
***********************************************************************         
PRNTATTB NMOD1 0,PRNTATTB                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         PRNT  *****************                                                
         PRNT  GetAttributes                                                    
         PRNT  *****************                                                
*                                                                               
         LA    R2,ATBGTA2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   GET CONVERSATION ATTRIBUTES                  
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,CONVERSATION_ID,P+30,8,=C'TOG'                   
         PRNT  ConversationID                                                   
         MVC   P+30(17),PARTNER_LU_NAME                                         
         PRNT  PartnerLUName                                                    
         MVC   P+30(8),MODE_NAME                                                
         PRNT  ModeName                                                         
         MVC   P+30(4),=C'NONE'                                                 
         CLC   SYNC_LEVEL,ATB_NONE                                              
         BE    *+22                                                             
         MVC   P+30(7),=C'CONFIRM'                                              
         CLC   SYNC_LEVEL,ATB_CONFIRM                                           
         BE    *+6                                                              
         DC    H'0'                UNKNOWN SYNC LEVEL                           
         PRNT  SyncLevel                                                        
         EDIT  TP_NAME_LENGTH,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK                  
         PRNT  TPNameLength                                                     
         MVC   P+30(64),TP_NAME                                                 
         PRNT  TPName                                                           
         MVC   P+30(8),LOCAL_LU_NAME                                            
         PRNT  LocalLUName                                                      
         MVC   P+30(5),=C'BASIC'                                                
         CLC   CONVERSATION_TYPE,ATB_BASIC_CONVERSATION                         
         BE    *+22                                                             
         MVC   P+30(6),=C'MAPPED'                                               
         CLC   CONVERSATION_TYPE,ATB_MAPPED_CONVERSATION                        
         BE    *+6                                                              
         DC    H'0'                UNKNOWN CONVERSATION TYPE                    
         PRNT  ConversationType                                                 
         MVC   P+30(10),USER_ID                                                 
         PRNT  UserID                                                           
         MVC   P+30(10),PROFILE                                                 
         PRNT  Profile                                                          
         MVC   P+30(80),USER_TOKEN                                              
         PRNT  UserToken                                                        
         MVC   P+30(4),=C'SEND'                                                 
         CLC   CONVERSATION_STATE,ATB_SEND_STATE                                
         BE    PRATTB10                                                         
         MVC   P+30(7),=C'RECEIVE'                                              
         CLC   CONVERSATION_STATE,ATB_RECEIVE_STATE                             
         BE    PRATTB10                                                         
         MVC   P+30(7),=C'CONFIRM'                                              
         CLC   CONVERSATION_STATE,ATB_CONFIRM_STATE                             
         BE    PRATTB10                                                         
         MVC   P+30(10),=C'INITIALIZE'                                          
         CLC   CONVERSATION_STATE,ATB_INITIALIZE_STATE                          
         BE    PRATTB10                                                         
         MVC   P+30(12),=C'SEND PENDING'                                        
         CLC   CONVERSATION_STATE,ATB_SEND_PENDING_STATE                        
         BE    PRATTB10                                                         
         MVC   P+30(12),=C'CONFIRM SEND'                                        
         CLC   CONVERSATION_STATE,ATB_CONFIRM_SEND_STATE                        
         BE    PRATTB10                                                         
         MVC   P+30(18),=C'CONFIRM DEALLOCATE'                                  
         CLC   CONVERSATION_STATE,ATB_CONFIRM_DEALLOCATE_STATE                  
         BE    PRATTB10                                                         
         DC    H'0'                UNKNOWN CONVERSATION STATE                   
PRATTB10 PRNT  ConversationState                                                
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
                                                                                
***********************************************************************         
* PRINT DETAILS ABOUT THE CURRENT STATE OF THE ALLOCATE QUEUE                   
***********************************************************************         
QUERYALQ NMOD1 0,QUERYALQ                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         PRNT  *****************                                                
         PRNT  AllocateQueue                                                    
         PRNT  *****************                                                
*                                                                               
         LA    R2,ATBQAQ2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   QUERY ALLOCATE QUEUE                         
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,ALLOCATE_QUEUE_TOKEN,P+30,8,=C'TOG'              
         PRNT  AllocateQToken                                                   
         EDIT  TP_NAME_LENGTH,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK                  
         PRNT  TPNameLength                                                     
         MVC   P+30(64),TP_NAME                                                 
         PRNT  TPName                                                           
         MVC   P+30(8),LOCAL_LU_NAME                                            
         PRNT  LocalLUName                                                      
         EDIT  ALLOCATE_QUEUE_SIZE,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK             
         PRNT  AllocateQSize                                                    
         EDIT  ALLOCATE_QUEUE_OLDEST,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK           
         PRNT  AllocateQAge                                                     
         OC    LAST_REC_ALLOC_ISSUED,LAST_REC_ALLOC_ISSUED                      
         BNZ   *+14                                                             
         MVC   P+30(4),=C'NONE'                                                 
         B     QUALC10                                                          
         GOTO1 =V(HEXOUT),DMCB,LAST_REC_ALLOC_ISSUED,P+30,8,=C'TOG'             
QUALC10  PRNT  LastAllocIssue                                                   
         OC    LAST_REC_ALLOC_RETURNED,LAST_REC_ALLOC_RETURNED                  
         BNZ   *+14                                                             
         MVC   P+30(4),=C'NONE'                                                 
         B     QUALC20                                                          
         GOTO1 =V(HEXOUT),DMCB,LAST_REC_ALLOC_RETURNED,P+30,8,=C'TOG'           
QUALC20  PRNT  LastAllocReturn                                                  
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
                                                                                
***********************************************************************         
* CALL APPC                                                                     
* UPON ENTRY,  R2 POINTS TO PARAMETER STRUCTURE                                 
* UPON RETURN, RETURN_CODE CONTAINS THE APPC/MVS RETURN CODE                    
***********************************************************************         
CALLAPPC NMOD1 0,CALLAPPC                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_ECB                                  
         TM    20(R2),X'80'                                                     
         BZ    *+10                                                             
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         L     RF,0(R2)            RF = A(APPC/MVS ROUTINE)                     
         LA    R3,24(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         TM    20(R2),X'80'        ASYNCHRONOUS CALL?                           
         BZ    CALL10              YES, SO WE MUST WAIT                         
*                                                                               
         L     RF,ASTPECB2         SYNCHRONOUS CALL, SO IT'S DONE               
         TM    0(RF),X'40'         STOP?                                        
         BZ    CALL30              NO                                           
         XC    0(4,RF),0(RF)                                                    
         MVI   OPERSTOP,C'Y'                                                    
         B     CALL30                                                           
*                                                                               
CALL10   CLC   RETURN_CODE,ATB_OK                                               
         BE    CALL20              APPC/MVS CALL WAS ACCEPTED                   
         MVC   P+30(16),4(R2)      PRINT NAME OF APPC/MVS ROUTINE               
         MVC   P+50(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+64),ALIGN=LEFT                                  
         PRNT  ***BadAPPCCall***                                                
         B     CALL30              APPC/MVS CALL NOT ACCEPTED                   
*                                                                               
CALL20   WAIT  1,ECBLIST=ECBLST2   WAIT FOR COMPLETION OR OPERATOR              
*                                                                               
         L     RF,ASTPECB2                                                      
         TM    0(RF),X'40'         STOP?                                        
         BZ    *+18                NO                                           
         XC    0(4,RF),0(RF)                                                    
         MVI   OPERSTOP,C'Y'                                                    
         B     CALL30                                                           
*                                                                               
         TM    APPCECB,X'40'                                                    
         BO    *+6                                                              
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
         MVC   RETURN_CODE,APPCECB                                              
         MVI   RETURN_CODE,0       CLEAR HIGH-ORDER BYTE                        
         XC    APPCECB,APPCECB                                                  
*                                                                               
CALL30   MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODE           
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         PRNT                                                                   
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    CALLX                                                            
         TM    20(R2),X'40'        CAN CALL ERROR_EXTRACT NOW?                  
         BZ    CALLX                                                            
         GOTO1 =A(APPCERR),(RC)    YES - DO IT                                  
*                                                                               
CALLX    XIT1                                                                   
         LTORG                                                                  
                                                                                
***********************************************************************         
* CALL MQ                                                                       
* UPON ENTRY,  R2 POINTS TO PARAMETER STRUCTURE                                 
* UPON RETURN, MQ_COMPCODE CONTAINS THE MQ COMPLETION CODE                      
*              MQ_REASON CONTAINS THE MQ REASON CODE                            
***********************************************************************         
CALLMQ   NMOD1 0,CALLMQ                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         L     RF,0(R2)            RF = A(MQ ROUTINE)                           
         LA    R3,32(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         L     R3,24(R2)           A(A(COMPCODE))                               
         L     R3,0(R3)            A(COMPCODE)                                  
         L     R3,0(R3)            COMPCODE                                     
         L     R4,28(R2)           A(A(REASON))                                 
         L     R4,0(R4)            A(REASON)                                    
         L     R4,0(R4)            REASON                                       
*                                                                               
         MVI   P,C'+'              '+' MEANS IT'S AN MQ CALL                    
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODES          
         MVC   P+30(8),=C'COMP. OK'                                             
         C     R3,=A(MQCC_OK)                                                   
         BE    CALLMQ30                                                         
         MVC   P+30(8),=C'WARNING!'                                             
         C     R3,=A(MQCC_WARNING)                                              
         BE    CALLMQ10                                                         
         MVC   P+30(8),=C'*FAILED!'                                             
         C     R3,=A(MQCC_FAILED)                                               
         BE    CALLMQ10                                                         
         DC    H'0'                UNKNOWN COMPLETION CODE                      
*                                                                               
CALLMQ10 MVC   P+110(9),=C'*** ERROR'                                           
         MVC   P+39(7),=C'REASON='                                              
         EDIT  (R4),(5,P+47),ZERO=NOBLANK                                       
         MVI   P+52,C':'                                                        
*                                                                               
         L     RF,=A(MQ_REASON_CODE_TABLE)                                      
CALLMQ20 CLI   0(RF),X'FF'         END OF TABLE?                                
         BNE   *+14                NO                                           
         MVC   P+54(22),=C'*** UNKNOWN REASON ***'                              
         B     CALLMQ25            REASON CODE NOT IN TABLE                     
*                                                                               
         C     R4,0(RF)            FIND MATCH ON REASON CODE                    
         BE    *+12                GOT IT                                       
         LA    RF,28(RF)           BUMP TO NEXT TABLE ENTRY                     
         B     CALLMQ20                                                         
*                                                                               
         MVC   P+54(24),4(RF)                                                   
*                                                                               
CALLMQ25 C     R3,=A(MQCC_WARNING) CLARIFY MESSAGE IF MESSAGE NOT FOUND         
         BNE   CALLMQ30                                                         
         C     R4,=A(MQRC_SIGNAL_REQUEST_ACCEPTED)                              
         BNE   CALLMQ30                                                         
         MVC   WORK(60),P+39                                                    
         MVC   P+30(90),SPACES                                                  
         MVC   P+30(20),=CL20'NO MESSAGE AVAILABLE'                             
         MVC   P+55(60),WORK                                                    
*                                                                               
CALLMQ30 ST    R3,MQ_COMPCODE      RETURN COMPCODE                              
         ST    R4,MQ_REASON        RETURN REASON                                
*                                                                               
         PRNT                                                                   
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
                                                                                
***********************************************************************         
* PERFORM APPC EXTRACT_ERROR FUNCTION.                                          
***********************************************************************         
APPCERR  NMOD1 0,APPCERR                                                        
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         LA    R2,ATBEES3_STRUCTURE                                             
         L     RF,0(R2)            RF = A(APPC/MVS ROUTINE)                     
         LA    R3,24(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODE           
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE_ERROR_EXTRACT,(5,P+44),ALIGN=LEFT,          +        
               ZERO=NOBLANK                                                     
         PRNT                                                                   
*                                                                               
         OC    RETURN_CODE_ERROR_EXTRACT,RETURN_CODE_ERROR_EXTRACT              
         BNZ   APPCEX              BAD RETURN CODE, SO NO ERROR INFO            
*                                                                               
         MVC   P+30(37),=C'APPC SERVICE XXXXXXXX, REASON_CODE = '               
         MVC   P+43(8),SERVICE_NAME                                             
         EDIT  SERVICE_REASON_CODE,(5,P+67),ALIGN=LEFT,ZERO=NOBLANK             
         PRNT  ErrorExtract                                                     
*                                                                               
         ICM   R3,15,MESSAGE_TEXT_LENGTH                                        
         BZ    APPCE20             NOTHING IN FIELD                             
         C     R3,=F'132'          MESSAGE > 132 CHARACTERS?                    
         BH    APPCE10             YES, BREAK MESSAGE UP INTO TWO LINES         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),MESSAGE_TEXT                                                
         GOTO1 =V(PRINTER)                                                      
         B     APPCE20                                                          
*                                                                               
APPCE10  S     R3,=F'132'                                                       
         MVC   P,MESSAGE_TEXT                                                   
         GOTO1 =V(PRINTER)                                                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),MESSAGE_TEXT+132                                            
         GOTO1 =V(PRINTER)                                                      
*                                                                               
APPCE20  ICM   R3,15,ERROR_LOG_INFORMATION_LENGTH                               
         BZ    APPCEX              NOTHING IN FIELD                             
         C     R3,=F'132'          MESSAGE > 132 CHARACTERS?                    
         BH    APPCE30             YES, BREAK MESSAGE UP                        
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),ERROR_LOG_INFORMATION                                       
         GOTO1 =V(PRINTER)                                                      
         B     APPCEX                                                           
*                                                                               
APPCE30  C     R3,=F'264'          WILL MESSAGE FIT ON TWO LINES?               
         BH    APPCE40                                                          
         S     R3,=F'132'          YES                                          
         MVC   P,ERROR_LOG_INFORMATION                                          
         GOTO1 =V(PRINTER)                                                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),ERROR_LOG_INFORMATION+132                                   
         GOTO1 =V(PRINTER)                                                      
         B     APPCEX                                                           
*                                                                               
APPCE40  C     R3,=F'396'          WILL MESSAGE FIT ON THREE LINES?             
         BH    APPCE50                                                          
         S     R3,=F'264'          YES                                          
         MVC   P,ERROR_LOG_INFORMATION                                          
         GOTO1 =V(PRINTER)                                                      
         MVC   P,ERROR_LOG_INFORMATION+132                                      
         GOTO1 =V(PRINTER)                                                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),ERROR_LOG_INFORMATION+264                                   
         GOTO1 =V(PRINTER)                                                      
         B     APPCEX                                                           
*                                                                               
APPCE50  S     R3,=F'396'          SPLIT INTO FOUR LINES                        
         MVC   P,ERROR_LOG_INFORMATION                                          
         GOTO1 =V(PRINTER)                                                      
         MVC   P,ERROR_LOG_INFORMATION+132                                      
         GOTO1 =V(PRINTER)                                                      
         MVC   P,ERROR_LOG_INFORMATION+264                                      
         GOTO1 =V(PRINTER)                                                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),ERROR_LOG_INFORMATION+396                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
APPCEX   XIT1                                                                   
         LTORG                                                                  
                                                                                
***********************************************************************         
* SEND MQ REPORT MESSAGE                                                        
***********************************************************************         
SENDRPTM NTR1  BASE=*,LABEL=*                                                   
*                                                                               
SRM000   MVC   MSGDESC_WRKI_CORRELID,MQCI_NONE                                  
         MVC   MSGDESC_WRKI_MSGID,MQMI_NONE                                     
*                                                                               
         MVC   GETMSGOPTS_OPTIONS,=A(MQGMO_SET_SIGNAL)                          
         XC    WORKQECB,WORKQECB                                                
         MVC   GETMSGOPTS_SIGNAL1,=A(WORKQECB)                                  
         MVC   GETMSGOPTS_WAITINTERVAL,=A(MQWI_UNLIMITED)                       
*                                                                               
         LA    R2,CSQBGET_WRKI_STRUCTURE                                        
         GOTO1 =A(CALLMQ),(RC)     TRY TO GET A MESSAGE                         
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    SRM010              GOT A MESSAGE                                
         CLC   MQ_COMPCODE,=A(MQCC_WARNING)                                     
         BNE   *+14                                                             
         CLC   MQ_REASON,=A(MQRC_SIGNAL_REQUEST_ACCEPTED)                       
         BE    SRMX                                                             
         DC    H'0'                NO OTHER WARNING IS ACCEPTABLE               
*                                                                               
SRM010   MVC   AMQWRKPT,AMQWRKXA                                                
*                                                                               
         LA    R1,P+30                                                          
         MVC   0(4,R1),=C'None'                                                 
         CLI   MSGDESC_WRKI_USERIDENTIFIER,C' '                                 
         BNH   SFM010A                                                          
         MVC   0(7,R1),=C'UserID='                                              
         MVC   7(12,R1),MSGDESC_WRKI_USERIDENTIFIER                             
         LA    R1,19(,R1)                                                       
         BRAS  RE,BACKSPC                                                       
SFM010A  CLI   MSGDESC_WRKI_REPLYTOQMGR,C' '                                    
         BNH   SFM010B                                                          
         MVC   0(10,R1),=C'ReplyQMGR='                                          
         MVC   10(30,R1),MSGDESC_WRKI_REPLYTOQMGR                               
         LA    R1,40(,R1)                                                       
         BRAS  RE,BACKSPC                                                       
SFM010B  CLI   MSGDESC_WRKI_REPLYTOQ,C' '                                       
         BNH   SFM010C                                                          
         MVC   0(11,R1),=C'ReplyQueue='                                         
         MVC   11(30,R1),MSGDESC_WRKI_REPLYTOQ                                  
         LA    R1,41(,R1)                                                       
         BRAS  RE,BACKSPC                                                       
SFM010C  CLI   MSGDESC_WRKI_PUTAPPLNAME,C' '                                    
         BNH   SFM010D                                                          
         MVC   0(7,R1),=C'PutApp='                                              
         MVC   7(28,R1),MSGDESC_WRKI_PUTAPPLNAME                                
SFM010D  PRNT  SenderInfo                                                       
         B     SRM020                                                           
*                                                                               
BACKSPC  CLI   0(R1),C' ' <------.  BUMP BACK THROUGH WHITE SPACE               
         BH    *+12 >-------.    |                                              
         AHI   R1,-1        |    |                                              
         B     *-12 >------------'                                              
         MVI   1(R1),C',' <-'       ADD A COMMA                                 
         LA    R1,3(,R1)            AND CONTINUE                                
         BR    RE                                                               
*                                                                               
SRM020   XC    BUFFER,BUFFER                                                    
         LA    R1,BUFFER                                                        
         BRAS  RE,GETLINE                                                       
         BE    *+6                                                              
         DC    H'0'                ALL RPTS MUST HAVE...                        
         CLC   APPCM3(13),BUFFER    '+++DARE SEND ' HEADER                      
         BNE   SRM020                                                           
*                                                                               
         CLI   TSKMETH,METHAPPC    APPC/MVS?                                    
         BE    SRM040              YES -- NO QUEUE TO OPEN                      
*                                                                               
         PACK  DUB,MSGDESC_WRKI_MSGID+4(5)   USERID#                            
         CVB   R2,DUB              R2 NEEDED IF WE CALL FINDOUTQ (MQ)           
         GOTO1 =A(FINDOUTQ),(RC)   FIND OUTPUT QUEUE NAME (INPUT IS R2)         
*                                                                               
SRM030   MVC   OBJDESC_OBJECTTYPE,=A(MQOT_Q)  OBJECT IS A QUEUE                 
         MVC   OBJDESC_OBJECTNAME,QUEUENAME   RETURNED BY FINDOUTQ              
         MVC   OPEN_OPTIONS,=A(MQOO_OUTPUT)                                     
*                                                                               
         LA    R2,CSQBOPEN_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     OPEN QUEUE                                   
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    SRM035                                                           
         MVC   P+30(48),QUEUENAME                                               
         PRNT  CannotOpenQueue     CAN'T OPEN THIS QUEUE                        
*                                                                               
         CLC   MQ_REASON,=A(MQRC_UNKNOWN_OBJECT_NAME)                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   OPMSG7A,QUEUENAME   EMAIL WARNING!                               
         GOTO1 DATAMGR,DMCB,=C'OPMSG',('OPMSG7Q',OPMSG7)                        
*                                                                               
         L     RF,TSKDEADQ                                                      
         CLC   QUEUENAME,0(RF)     QNAME = DEADQ ALREADY?                       
         BNE   *+6                 NO                                           
         DC    H'0'                YES - MISSING DEADQ! STOP LOOPING            
         MVC   QUEUENAME,0(RF)                                                  
         MVI   MQOUTVER,C'2'       SET OUTPUT VERSION=2 FOR 1 MSG               
         MVC   P+30(48),QUEUENAME                                               
         PRNT  PutToDeadQueue                                                   
         B     SRM030              PUT TO DEADQ                                 
*                                                                               
SRM035   MVC   P+30(48),QUEUENAME                                               
         MVC   P+78(L'MQOUTVER),MQOUTVER                                        
         PRNT  OutputQueueOpened                                                
*                                                                               
SRM040   EQU   *                                                                
         LHI   R3,APPCM3Q                                                       
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   BUFFER(0),APPCM3    THIS IS THE '+++DARE SEND '                  
         GOTO1 =A(DATETIME),(RC)   GET TIME FOR MESSAGE HEADER                  
         MVC   BUFFER+18(6),HHMMSS                                              
         AHI   R3,1                RESTORE RECORD LENGTH                        
*                                                                               
         AP    COA#PL8,=P'1'                                                    
         MVC   COA#DATE,YYYYMMDD+4     MMDD                                     
         MVC   COA#TIM,HHMMSS          HHMMSS                                   
         ZAP   DUB,COA#PL8                                                      
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  COA#SEQ,DUB             NNNNNNNNNN                               
*                                                                               
         CLI   TSKMETH,METHAPPC    APPC/MVS?                                    
         BE    SRM60                                                            
*                                                                               
         L     RE,AMQBUFXA                                                      
         ST    RE,AMQBUFPT         RESET MQ BUFFER CURRENT POINTER              
         SAM31                                                                  
         XCEFL (RE),'MQBUFLQ'      CLEAR MQ BUFFER                              
         SAM24                                                                  
*                                                                               
         MVI   BYTE,C'N'           NO COD FOR THIS RECORD                       
         GOTO1 =A(DRXMTLMQ),(RC)   NO, MQSERIES                                 
         BE    SRM80                                                            
         DC    H'0'                                                             
*                                                                               
SRM60    MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         GOTO1 =A(DRXMTLAP),(RC)                                                
         BE    SRM80                                                            
         MVC   P+30(14),=C'BAD DARE WRITE'                                      
         B     SRM500                                                           
*                                                                               
SRM80    LHI   R2,1                ONE RECORD SENT SO FAR                       
         XC    BUFFER,BUFFER                                                    
         LA    R1,BUFFER                                                        
         BRAS  RE,GETLINE                                                       
         BE    *+6                                                              
         DC    H'0'                THERE MUST BE MORE HERE                      
*                                                                               
         XC    LOGDATA,LOGDATA                                                  
         CLC   APPCM5(16),BUFFER   +++DARE LOGGING=                             
         BNE   SRM120                                                           
*                                                                               
         MVC   LOGDATA,BUFFER+16   SAVE THE LOG DATA (EDICT FILE D/A)           
         CLI   TSKDDS,C'Y'                                                      
         BNE   SRM100              ONLY SEND TO DDS PARTNERS (EDICT)            
         LHI   R3,APPCM5Q                                                       
*                                                                               
         CLI   TSKMETH,METHAPPC    APPC/MVS?                                    
         BE    SRM90                                                            
         MVI   BYTE,C'N'           NO COD FOR THIS RECORD                       
         GOTO1 =A(DRXMTLMQ),(RC)   NO, MQSERIES                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         AHI   R2,1                ANOTHER RECORD SENT                          
         B     SRM100                                                           
*                                                                               
SRM90    MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         GOTO1 =A(DRXMTLAP),(RC)                                                
         BE    *+14                                                             
         MVC   P+30(14),=C'BAD DARE WRITE'                                      
         B     SRM500                                                           
         AHI   R2,1                ANOTHER RECORD SENT                          
*                                                                               
SRM100   EQU   *                                                                
         XC    BUFFER,BUFFER                                                    
         LA    R1,BUFFER                                                        
         BRAS  RE,GETLINE                                                       
         BE    *+6                                                              
         DC    H'0'                WHERE'S THE FIRST DATA RECORD?               
*                                                                               
SRM120   CLI   TSKMETH,METHAPPC    APPC/MVS?                                    
         BE    SRM125                                                           
         L     RF,TSKDEADQ                                                      
         CLC   QUEUENAME,0(RF)     QNAME = DEADQ?                               
         BNE   *+12                NO                                           
         MVI   NEEDDLN,C'N'        YES - NO DLNNOT FOR DEAD MSG                 
         B     SRM180                                                           
*                                                                               
SRM125   CLC   =C'ERRNOT',BUFFER                                                
         BNE   *+12                                                             
         MVI   NEEDDLN,C'N'        NO DLNNOT FOR AN ERRNOT                      
         B     SRM180                                                           
*                                                                               
         CLC   =C'DLNNOT',BUFFER                                                
         BNE   *+12                                                             
         MVI   NEEDDLN,C'N'        NO DLNNOT FOR A DLNNOT                       
         B     SRM180                                                           
*                                                                               
         LA    R1,BUFFER                                                        
         BRAS  RE,XTRACT           EXTRACT DATA FROM OTHER DARE RECORDS         
         MVI   NEEDDLN,C'Y'        DLNNOT WILL BE REQUIRED                      
*                                                                               
         LA    R4,=C'UIDTABL'                                                   
         MVC   P+30(8),DARE                                                     
         MVC   P+40(7),0(R4)                                                    
         PRNT  Enqueue                                                          
         ENQ   (DARE,(4),E,7)      ENQUEUE THE USERID TABLE                     
*                                                                               
         L     R3,TSKUIDTB         TABLE OF USERIDS/PARTNERS                    
         USING UIDTABD,R3                                                       
SRM140   CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                USERID HAS VANISHED                          
         CLC   SENDER,UIDNAME                                                   
         BE    *+12                                                             
         LA    R3,UIDTBLQ(,R3)     BUMP TO NEXT ENTRY                           
         B     SRM140                                                           
*                                                                               
         MVC   USERID,UIDNUM       SENDING USERID                               
         L     RF,ATSKTAB          TABLE OF DARE SUBTASKS                       
SRM160   CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                MISSING TABLE ENTRY                          
         CLC   UIDPART,TSKSNAME-TSKTABD(RF)                                     
         BE    *+12                GOT MATCH ON SHORT SUBTASK NAME              
         LA    RF,TSKTABLQ(,RF)    BUMP TO NEXT ENTRY                           
         B     SRM160                                                           
         MVC   SUBTSKID,TSKNAME-TSKTABD(RF)                                     
         DROP  R3                                                               
*                                                                               
         MVC   P+30(8),DARE                                                     
         MVC   P+40(7),0(R4)                                                    
         PRNT  Dequeue                                                          
         DEQ   (DARE,(4),7)        DEQUEUE THE USERID TABLE                     
*                                                                               
SRM180   EQU   *                                                                
         LA    R1,BUFFER                                                        
         BRAS  RE,CHKDDS           REMOVE "-DS" IN TO/FROM USER FIELD           
*                                                                               
         LHI   R3,L'BUFFER-1       FIND END OF TEXT                             
         LA    RF,BUFFER(R3)                                                    
         CLI   0(RF),C' '                                                       
         BNE   *+12                FOUND IT                                     
         BCT   R3,*-12                                                          
         B     *+8                                                              
         AHI   R3,1                                                             
*                                                                               
         LTR   R3,R3               ANY REAL DATA TO SEND?                       
         BZ    SRM210              NO -- SKIP IT                                
*                                                                               
         CLI   TSKMETH,METHAPPC    APPC/MVS?                                    
         BE    SRM200                                                           
         MVI   BYTE,C'N'           NO COD FOR THIS RECORD                       
         GOTO1 =A(DRXMTLMQ),(RC)   NO, MQSERIES                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         AHI   R2,1                INCREMENT RECORD COUNTER                     
         B     SRM210                                                           
*                                                                               
SRM200   MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         GOTO1 =A(DRXMTLAP),(RC)                                                
         BE    *+14                                                             
         MVC   P+30(14),=C'BAD DARE WRITE'                                      
         B     SRM500                                                           
         AHI   R2,1                INCREMENT RECORD COUNTER                     
*                                                                               
SRM210   EQU   *                                                                
         XC    BUFFER,BUFFER                                                    
         LA    R1,BUFFER                                                        
         BRAS  RE,GETLINE                                                       
         BE    *+6                                                              
         DC    H'0'                WHERE WAS THE '+++DARE END '?                
*                                                                               
         CLC   =C'+++DARE END ',BUFFER                                          
         BNE   SRM180              NO MORE RECORDS IN REPORT                    
*                                                                               
         MVC   BUFFER(APPCM4Q),APPCM4                                           
         GOTO1 =A(DATETIME),(RC)   PUT TIME INTO MESSAGE                        
         MVC   BUFFER+17(6),HHMMSS                                              
         AHI   R2,1                INCREMENT RECORD COUNTER (FOR LAST)          
         CVD   R2,DUB                                                           
         UNPK  BUFFER+30(6),DUB                                                 
         OI    BUFFER+35,X'F0'                                                  
*                                                                               
         LHI   R3,APPCM4Q                                                       
*                                                                               
         CLI   TSKMETH,METHAPPC    APPC/MVS?                                    
         BE    SRM230                                                           
         MVI   BYTE,C'Y'           COD NEEDED FOR LAST RECORD                   
         GOTO1 =A(DRXMTLMQ),(RC)   NO, MQSERIES                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         AHI   R2,1                INCREMENT RECORD COUNTER                     
         B     SRM240                                                           
*                                                                               
SRM230   MVC   SEND_TYPE,ATB_SEND_AND_CONFIRM                                   
         GOTO1 =A(DRXMTLAP),(RC)                                                
         BE    *+14                                                             
         MVC   P+30(14),=C'BAD DARE WRITE'                                      
         B     SRM500                                                           
*                                                                               
SRM240   PRNT  ReportSent                                                       
         CLI   NEEDDLN,C'Y'        SHOULD WE GENERATE A DLNNOT?                 
         BNE   SRM244                                                           
         BRAS  RE,BUILDDLN         YES                                          
         B     SRM245                                                           
SRM244   EQU   *                                                                
         BRAS  RE,BUILDNOT         NO - BUILD NOTNOT (DUMMY DLNNOT)             
SRM245   EQU   *                                                                
*                                                                               
         CLI   TSKMETH,METHAPPC    APPC/MVS?                                    
         BE    SRM260              YES -- NO QUEUE TO CLOSE                     
*                                                                               
         LA    R2,CSQBCOMM_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     COMMIT                                       
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CLOSE_OPTIONS,=A(MQCO_NONE)                                      
         LA    R2,CSQBCLOS_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     CLOSE QUEUE                                  
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SRM260   EQU   *                                                                
         B     SRM000              TRY TO GET NEXT MESSAGE                      
*                                                                               
SRM500   PRNT  CannotTransmit                                                   
         MVC   OPMSG2+23(3),TSKNAME                                             
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMSG2,OPMSG2)                     
         MVC   OPMSG8A(3),TSKNAME                                               
         GOTO1 DATAMGR,DMCB,=C'OPMSG',('OPMSG8Q',OPMSG8)                        
         DC    H'0'                ABEND, SO MQ MSG RESTORED                    
*                                                                               
SRMX     XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
***********************************************************************         
* GEN NEXT LINE                                                                 
***********************************************************************         
GETLINE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R1,AMQWRKPT                                                      
         MVC31 WORK,0(R1)                                                       
*                                                                               
         LA    RE,WORK                                                          
GLN20    CLC   0(2,RE),CRLF                                                     
         BE    GLN30                                                            
         AHI   RE,1                                                             
         B     GLN20                                                            
*                                                                               
GLN30    LA    RF,WORK                                                          
         SR    RE,RF               RE=LENGTH OF THIS LINE                       
         AR    R1,RE                                                            
         AHI   R1,2                +2 FOR CRLF                                  
         ST    R1,AMQWRKPT         UPDATE MQ BUFFER POINTER                     
*                                                                               
         C     R1,AMQWRKXX                                                      
         BH    GLNXBAD                                                          
*                                                                               
         MVI   BUFFER,C' '                                                      
         MVC   BUFFER+1(L'BUFFER-1),BUFFER                                      
*                                                                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BUFFER(0),WORK                                                   
*                                                                               
*        LA    R3,L'BUFFER                                                      
*        GOTO1 =V(PRNTBL),DMCB,=C'GET-LINE',BUFFER,C'DUMP',(R3),=C'1D'          
         B     GLNXOK                                                           
*                                                                               
GLNXBAD  LTR   RB,RB               NO GOOD -- SET CC NOT EQUAL                  
         B     *+6                                                              
GLNXOK   CR    RB,RB               SET CC EQUAL                                 
         XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
***********************************************************************         
* THIS ROUTINE EXTRACTS DATA FROM DARE RECORDS.                                 
***********************************************************************         
XTRACT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   DARETYPE,0(R1)                                                   
*                                                                               
         CLC   =C'AGYHDR',0(R1)                                                 
         BE    *+14                                                             
         CLC   =C'VARHDR',0(R1)                                                 
         BNE   ORDAPP                                                           
         USING PAGYHDRD,R1                                                      
         MVC   SENDER,PAHDFRID                                                  
         MVC   RECEIVER,PAHDTOID                                                
         MVC   ORDNUM,PAHDORDR                                                  
         MVC   CONTRACT,PAHDRPCN                                                
         MVC   RETURN,PAHDRTRN                                                  
         MVC   ORIGDATE,PAHDDATE                                                
         MVC   ORIGTIME,PAHDTIME                                                
         XC    OFFERID,OFFERID                                                  
         XC    IDSEQNUM,IDSEQNUM                                                
         DROP  R1                                                               
         B     XTRACTX                                                          
*                                                                               
ORDAPP   CLC   =C'ORDAPP',0(R1)                                                 
         BNE   ORDREJ                                                           
         USING RORDAPPD,R1                                                      
         MVC   SENDER,ROAPFRID                                                  
         MVC   RECEIVER,ROAPTOID                                                
         MVC   ORDNUM,ROAPORDR                                                  
         MVC   CONTRACT,ROAPRPCN                                                
         MVC   RETURN,ROAPRTRN                                                  
         MVC   ORIGDATE,ROAPDATE                                                
         MVC   ORIGTIME,ROAPTIME                                                
         XC    OFFERID,OFFERID                                                  
         XC    IDSEQNUM,IDSEQNUM                                                
         DROP  R1                                                               
         B     XTRACTX                                                          
*                                                                               
ORDREJ   CLC   =C'ORDREJ',0(R1)                                                 
         BNE   ORDCFM                                                           
         USING RORDREJD,R1                                                      
         MVC   SENDER,RORJFRID                                                  
         MVC   RECEIVER,RORJTOID                                                
         MVC   ORDNUM,RORJORDR                                                  
         MVC   CONTRACT,RORJRPCN                                                
         MVC   RETURN,RORJRTRN                                                  
         MVC   ORIGDATE,RORJDATE                                                
         MVC   ORIGTIME,RORJTIME                                                
         XC    OFFERID,OFFERID                                                  
         XC    IDSEQNUM,IDSEQNUM                                                
         DROP  R1                                                               
         B     XTRACTX                                                          
*                                                                               
ORDCFM   CLC   =C'ORDCFM',0(R1)                                                 
         BNE   ORDRCL                                                           
         USING RORDCFMD,R1                                                      
         MVC   SENDER,ROCFFRID                                                  
         MVC   RECEIVER,ROCFTOID                                                
         MVC   ORDNUM,ROCFORDR                                                  
         MVC   CONTRACT,ROCFRPCN                                                
         MVC   RETURN,ROCFRTRN                                                  
         MVC   ORIGDATE,ROCFDATE                                                
         MVC   ORIGTIME,ROCFTIME                                                
         XC    OFFERID,OFFERID                                                  
         XC    IDSEQNUM,IDSEQNUM                                                
         DROP  R1                                                               
         B     XTRACTX                                                          
*                                                                               
ORDRCL   CLC   =C'ORDRCL',0(R1)                                                 
         BNE   AGYRCL                                                           
         USING RORDRCLD,R1                                                      
         MVC   SENDER,RORCFRID                                                  
         MVC   RECEIVER,RORCTOID                                                
         MVC   ORDNUM,RORCORDR                                                  
         MVC   CONTRACT,RORCRPCN                                                
         MVC   RETURN,RORCRTRN                                                  
         MVC   ORIGDATE,RORCDATE                                                
         MVC   ORIGTIME,RORCTIME                                                
         XC    OFFERID,OFFERID                                                  
         XC    IDSEQNUM,IDSEQNUM                                                
         DROP  R1                                                               
         B     XTRACTX                                                          
*                                                                               
AGYRCL   CLC   =C'AGYRCL',0(R1)                                                 
         BNE   AGYCAN                                                           
         USING PAGYRCLD,R1                                                      
         MVC   SENDER,PARCFRID                                                  
         MVC   RECEIVER,PARCTOID                                                
         MVC   ORDNUM,PARCORDR                                                  
         MVC   CONTRACT,PARCRPCN                                                
         MVC   RETURN,PARCRTRN                                                  
         MVC   ORIGDATE,PARCDATE                                                
         MVC   ORIGTIME,PARCTIME                                                
         XC    OFFERID,OFFERID                                                  
         XC    IDSEQNUM,IDSEQNUM                                                
         DROP  R1                                                               
         B     XTRACTX                                                          
*                                                                               
AGYCAN   CLC   =C'AGYCAN',0(R1)                                                 
         BNE   MKGHDR                                                           
         USING PAGYCAND,R1                                                      
         MVC   SENDER,PACNFRID                                                  
         MVC   RECEIVER,PACNTOID                                                
         MVC   ORDNUM,PACNORDR                                                  
         MVC   CONTRACT,PACNRPCN                                                
         MVC   RETURN,PACNRTRN                                                  
         MVC   ORIGDATE,PACNDATE                                                
         MVC   ORIGTIME,PACNTIME                                                
         XC    OFFERID,OFFERID                                                  
         XC    IDSEQNUM,IDSEQNUM                                                
         DROP  R1                                                               
         B     XTRACTX                                                          
*                                                                               
MKGHDR   CLC   =C'MKGHDR',0(R1)                                                 
         BNE   MKGAPP                                                           
         USING MOFRHDRD,R1                                                      
         MVC   SENDER,MOHDFRID                                                  
         MVC   RECEIVER,MOHDTOID                                                
         MVC   ORDNUM,MOHDORDR                                                  
         MVC   CONTRACT,MOHDRPCN                                                
         MVC   RETURN,MOHDRTNS                                                  
         MVC   ORIGDATE,MOHDDATE                                                
         MVC   ORIGTIME,MOHDTIME                                                
         MVC   OFFERID,MOHDOFRI                                                 
         MVC   IDSEQNUM,MOHDSEQN                                                
         DROP  R1                                                               
         B     XTRACTX                                                          
*                                                                               
MKGAPP   CLC   =C'MKGAPP',0(R1)                                                 
         BNE   MKGREJ                                                           
         USING MOFRAPPD,R1                                                      
         MVC   SENDER,MOAPFRID                                                  
         MVC   RECEIVER,MOAPTOID                                                
         MVC   ORDNUM,MOAPORDR                                                  
         MVC   CONTRACT,MOAPRPCN                                                
         MVC   RETURN,MOAPRTNS                                                  
         MVC   ORIGDATE,MOAPDATE                                                
         MVC   ORIGTIME,MOAPTIME                                                
         MVC   OFFERID,MOAPOFRI                                                 
         MVC   IDSEQNUM,MOAPSEQN                                                
         DROP  R1                                                               
         B     XTRACTX                                                          
*                                                                               
MKGREJ   CLC   =C'MKGREJ',0(R1)                                                 
         BNE   MKGROK                                                           
         USING MOFRREJD,R1                                                      
         MVC   SENDER,MORJFRID                                                  
         MVC   RECEIVER,MORJTOID                                                
         MVC   ORDNUM,MORJORDR                                                  
         MVC   CONTRACT,MORJRPCN                                                
         MVC   RETURN,MORJRTNS                                                  
         MVC   ORIGDATE,MORJDATE                                                
         MVC   ORIGTIME,MORJTIME                                                
         MVC   OFFERID,MORJOFRI                                                 
         MVC   IDSEQNUM,MORJSEQN                                                
         DROP  R1                                                               
         B     XTRACTX                                                          
*                                                                               
MKGROK   CLC   =C'MKGROK',0(R1)                                                 
         BNE   MKGCAN                                                           
         USING MOFRCFMD,R1                                                      
         MVC   SENDER,MOCFFRID                                                  
         MVC   RECEIVER,MOCFTOID                                                
         MVC   ORDNUM,MOCFORDR                                                  
         MVC   CONTRACT,MOCFRPCN                                                
         MVC   RETURN,MOCFRTNS                                                  
         MVC   ORIGDATE,MOCFDATE                                                
         MVC   ORIGTIME,MOCFTIME                                                
         MVC   OFFERID,MOCFOFRI                                                 
         MVC   IDSEQNUM,MOCFSEQN                                                
         DROP  R1                                                               
         B     XTRACTX                                                          
*                                                                               
MKGCAN   CLC   =C'MKGCAN',0(R1)                                                 
         BNE   XTRACTX                                                          
         USING MOFRCAND,R1                                                      
         MVC   SENDER,MOCNFRID                                                  
         MVC   RECEIVER,MOCNTOID                                                
         MVC   ORDNUM,MOCNORDR                                                  
         MVC   CONTRACT,MOCNRPCN                                                
         MVC   RETURN,MOCNRTNS                                                  
         MVC   ORIGDATE,MOCNDATE                                                
         MVC   ORIGTIME,MOCNTIME                                                
         MVC   OFFERID,MOCNOFRI                                                 
         MVC   IDSEQNUM,MOCNSEQN                                                
         DROP  R1                                                               
*                                                                               
XTRACTX  XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
***********************************************************************         
* THIS ROUTINE GENERATES DELIVERY NOTIFICATIONS.                                
***********************************************************************         
BUILDDLN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,=A(DNOTBUF)      R4 = A(DELNOT BUFFER)                        
         LR    RE,R4               CLEAR DELNOT BUFFER                          
         L     RF,=A(DNOTBUFL)                                                  
         XCEFL                                                                  
*                                                                               
         MVC   0(APPCM3Q,R4),APPCM3      +++DARE SEND TIME=HHMMSS               
         GOTO1 =A(DATETIME),(RC)   PUT TIME INTO MESSAGE                        
         MVC   18(6,R4),HHMMSS                                                  
         MVC   APPCM3Q(L'CRLF,R4),CRLF                                          
         AHI   R4,APPCM3Q+L'CRLF                                                
*                                                                               
         MVC   RECCOUNT,=F'1'      ONE RECORD SO FAR                            
*                                                                               
         OC    LOGDATA,LOGDATA     WAS THERE ANY LOG DATA TO SEND?              
         BZ    BD10                                                             
*                                                                               
         MVC   0(APPCM5Q,R4),APPCM5      +++DARE LOGGING=VTTTTBBRRF             
         MVC   16(L'LOGDATA,R4),LOGDATA                                         
         MVC   APPCM5Q(L'CRLF,R4),CRLF                                          
         AHI   R4,APPCM5Q+L'CRLF                                                
*                                                                               
         L     RE,RECCOUNT                                                      
         AHI   RE,1                                                             
         ST    RE,RECCOUNT                                                      
*                                                                               
BD10     EQU   *                                                                
         USING MDLNNOTD,R4                                                      
         MVC   MDNTTID,=C'DLNNOT'  DELIVERY NOTIFICATION                        
         MVC   MDNTORDR,ORDNUM     ORDER NUMBER                                 
         MVC   MDNTFRID,RECEIVER   RECEIVER USERID                              
         CLI   RCVR_DS,C'Y'        DID I REMOVE "-DS"?                          
         BNE   *+10                NO                                           
         MVC   MDNTFRID+5(3),=C'-DS'   PUT "-DS" BACK                           
         MVC   MDNTTOID,SENDER     SENDER USERID                                
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),WORK                                       
         THMS  DDSTIME=YES                                                      
         ST    R1,FULL             0HHMMSS+ (DDS TIME)                          
         STCM  R0,15,PACKOF4B      0060000+ (DDS CLOCK TIME DIFFERENCE)         
         AP    PACKOF4B,FULL       NOW CONTAINS ACTUAL TIME                     
         CP    PACKOF4B,=P'240000' PAST MIDNIGHT?                               
         BL    BD20                                                             
         SP    PACKOF4B,=P'240000' YES, ADJUST TIME AND BUMP DAY                
         GOTO1 =V(ADDAY),DMCB,WORK,WORK+6,F'1'                                  
         MVC   WORK(6),WORK+6                                                   
BD20     ICM   R0,15,PACKOF4B                                                   
         SRL   R0,12                                                            
         STCM  R0,3,HALF                                                        
*                                                                               
         GOTO1 =V(DATCON),DMCB,WORK,(X'20',MDNTDATE)   DELIVERY DATE            
         GOTO1 =V(HEXOUT),DMCB,HALF,MDNTTIME,2,=C'TOG' DELIVERY TIME            
*                                                                               
         MVC   MDNTTDTE,ORIGDATE   DATE OF ORIGINATION                          
         MVC   MDNTTTIM,ORIGTIME   TIME OR ORIGINATION                          
         MVC   MDNTRPCN,CONTRACT   REP CONTRACT                                 
         MVC   MDNTRTNS,RETURN     'RETURN TO SENDER' DATA                      
         MVC   MDNTEFLG,SPACES     NO ERROR                                     
*                                                                               
         LHI   R2,RDLNNOTL         L'RECORD                                     
         OC    OFFERID,OFFERID     IS THIS A DLN FOR A MAKEGOOD                 
         BZ    BD30                                                             
         LHI   R2,MDLNNOTL         YES                                          
         MVC   MDNTOFRI,OFFERID                                                 
         MVC   MDNTSEQN,IDSEQNUM                                                
         MVC   MDNTLINM,SPACES     NO ERROR LINE NUMBER                         
BD30     EQU   *                                                                
         LR    R3,R4                                                            
         GOTO1 =A(PUTSMF),(RC)     PUT DLNNOT RECORD TO SMF                     
         DROP  R4                                                               
*                                                                               
         AR    R4,R2                                                            
         MVC   0(L'CRLF,R4),CRLF                                                
         AHI   R4,L'CRLF                                                        
*                                                                               
         L     RE,RECCOUNT                                                      
         AHI   RE,1                                                             
         ST    RE,RECCOUNT                                                      
*                                +++DARE END TIME=HHMMSS COUNT=NNNNNN           
         MVC   0(APPCM4Q,R4),APPCM4                                             
         GOTO1 =A(DATETIME),(RC)   PUT TIME INTO MESSAGE                        
         MVC   17(6,R4),HHMMSS                                                  
         L     RE,RECCOUNT                                                      
         AHI   RE,1                ONE MORE FOR FINAL RECORD                    
         CVD   RE,DUB                                                           
         UNPK  30(6,R4),DUB                                                     
         OI    35(R4),X'F0'        TOTAL NUMBER OF RECORDS                      
*                                                                               
         MVC   APPCM4Q(L'CRLF,R4),CRLF                                          
         AHI   R4,APPCM4Q+L'CRLF                                                
*                                                                               
         LAY   R1,DNOTBUF          R1 = A(MESSAGE BUFFER)                       
         LR    R2,R4               R4 = A(END OF MESSAGE)                       
         SR    R2,R1               R2 = MESSAGE LENGTH                          
         LA    R3,SENDER           R3 = A(SENDER NAME)                          
         MVC   HALF,USERID         SENDING USERID#                              
         BRAS  RE,PUTWRKOQ         PUT THIS MQ MESSAGE TO WORK OUTPUT Q         
*                                                                               
         LAY   R1,DNOTBUF          R1 = A(MESSAGE BUFFER)                       
         LR    R2,R4               R4 = A(END OF MESSAGE)                       
         SR    R2,R1               R2 = MESSAGE LENGTH                          
         BRAS  RE,LOGMSGQ          LOG THIS MQ MESSAGE                          
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
***********************************************************************         
* ROUTINE GENERATES DUMMY DELIVERY NOTIFICATIONS ONLY FOR MQ PARTNERS           
***********************************************************************         
BUILDNOT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   TSKMETH,METHAPPC    APPC/MVS?                                    
         BNE   BNOT20              NO  - PUT OUT NOTNOT                         
*                                  YES - DON'T PUT OUT ANY MQ MESSAGE           
         LA    R2,CSQBCOMM_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     JUST COMMIT INPUT WORKQ ONLY                 
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    BNOTX                                                            
         DC    H'0'                                                             
*                                                                               
BNOT20   L     R4,=A(DNOTBUF)      R4 = A(DELNOT BUFFER)                        
         LR    RE,R4               CLEAR DELNOT BUFFER                          
         L     RF,=A(DNOTBUFL)                                                  
         XCEFL                                                                  
*                                                                               
         MVC   0(APPCM3Q,R4),APPCM3      +++DARE SEND TIME=HHMMSS               
         GOTO1 =A(DATETIME),(RC)   PUT TIME INTO MESSAGE                        
         MVC   18(6,R4),HHMMSS                                                  
         MVC   APPCM3Q(L'CRLF,R4),CRLF                                          
         AHI   R4,APPCM3Q+L'CRLF                                                
*                                                                               
         MVC   RECCOUNT,=F'1'      ONE RECORD SO FAR                            
*                                                                               
         USING MDLNNOTD,R4                                                      
         MVC   MDNTTID,=C'NOTNOT'  DELIVERY NOTIFICATION                        
         LHI   R2,RDLNNOTL         L'RECORD                                     
         DROP  R4                                                               
*                                                                               
         AR    R4,R2                                                            
         MVC   0(L'CRLF,R4),CRLF                                                
         AHI   R4,L'CRLF                                                        
*                                                                               
         L     RE,RECCOUNT                                                      
         AHI   RE,1                                                             
         ST    RE,RECCOUNT                                                      
*                                +++DARE END TIME=HHMMSS COUNT=NNNNNN           
         MVC   0(APPCM4Q,R4),APPCM4                                             
         GOTO1 =A(DATETIME),(RC)   PUT TIME INTO MESSAGE                        
         MVC   17(6,R4),HHMMSS                                                  
         L     RE,RECCOUNT                                                      
         AHI   RE,1                ONE MORE FOR FINAL RECORD                    
         CVD   RE,DUB                                                           
         UNPK  30(6,R4),DUB                                                     
         OI    35(R4),X'F0'        TOTAL NUMBER OF RECORDS                      
*                                                                               
         MVC   APPCM4Q(L'CRLF,R4),CRLF                                          
         AHI   R4,APPCM4Q+L'CRLF                                                
*                                                                               
         LAY   R1,DNOTBUF          R1 = A(MESSAGE BUFFER)                       
         LR    R2,R4               R4 = A(END OF MESSAGE)                       
         SR    R2,R1               R2 = MESSAGE LENGTH                          
         BRAS  RE,PUTWRKNQ         PUT THIS NOTNOT TO WORKQ                     
*                                                                               
BNOTX    XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
***********************************************************************         
* OUTPUT LOG MESSAGES                                                           
* ON INPUT: R1 = A(MESSAGE BUFFER)                                              
*           R2 = MESSAGE LENGTH                                                 
***********************************************************************         
LOGMSGQ  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ST    R1,A_PUTLOG_BUFF                                                 
         ST    R1,A_PUTLOG2_BUFF                                                
         ST    R1,A_PUTLOGB_BUFF                                                
         ST    R2,PUTLOG_BUFLEN                                                 
*                                                                               
         CLI   LOGFLAG,C'Y'                                                     
         BE    LOGQ030                                                          
         CLI   LOGFLAGB,C'Y'                                                    
         BE    LOGQ030                                                          
         PRNT  MessageNotLogged                                                 
         B     LOGQX                                                            
*                                                                               
LOGQ030  EQU   *                                                                
*                                  OVERRIDE '+++DARE SEND TIME=HHMMSS'          
         MVC   13(4,R1),=C'LOG '     WITH   '+++DARE SEND LOG =HHMMSS'          
         AHI   R1,APPCM3Q          PASS +++DARE SEND RECORD                     
LOGQ033  CLC   CRLF,0(R1)          SEARCH FORWARD FOR CRLF                      
         BNE   LOGQ040               NOT FOUND, BUMP NEXT CHAR                  
         CLI   L'CRLF(R1),C'+'       FOUND - PASS CRLF                          
         BE    LOGQ040                ANOTHER +++DARE RECORD                    
         AHI   R1,L'CRLF             FOUND DARE RECORD                          
         B     LOGQ050                                                          
LOGQ040  AHI   R1,1                                                             
         B     LOGQ033                                                          
*                                                                               
LOGQ050  EQU   *                                                                
*                                  UPDATE MSGID=CL3(TSKNAME)+S+DARETYPE         
         MVC   MSGDESC_LOG_MSGID(3),TSKNAME                                     
         MVI   MSGDESC_LOG_MSGID+3,C'S'     S FOR SENDER                        
         MVC   MSGDESC_LOG_MSGID+4(L'MSGDESC_LOG_MSGID-4),0(R1)                 
*                                  USE USERID FIELD FOR PARNTER CODE            
         MVC   MSGDESC_LOG_USERIDENTIFIER(4),MSGDESC_LOG_MSGID                  
*                                                                               
         CLI   LOGFLAG,C'Y'                                                     
         BNE   LOGQ070                                                          
*                                                                               
         LA    R2,CSQBPUT_LOG_STRUCTURE                                         
         GOTO1 =A(CALLMQ),(RC)     PUT ONE RECORD TO QUEUE 1                    
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         JNE   *+2                                                              
         PRNT  MessageLogQ1                                                     
*                                                                               
         CLI   LOGFLAG2,C'Y'                                                    
         BNE   LOGQ070                                                          
*                                                                               
         LA    R2,CSQBPUT_LOG2_STRUCTURE                                        
         GOTO1 =A(CALLMQ),(RC)     PUT ONE RECORD TO QUEUE 2                    
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         JNE   *+2                                                              
         PRNT  MessageLogQ2                                                     
*                                                                               
LOGQ070  CLI   LOGFLAGB,C'Y'       BLOCKCHAIN QUEUE                             
         BNE   LOGQ080                                                          
*                                                                               
         L     R2,=A(SSB)                                                       
         LAM   AR2,AR2,SSOTBLET-SSOOFF(R2)                                      
         XR    R2,R2                                                            
         SAM31                                                                  
         SAC   512                                                              
         ICM   R2,15,TABSCOMM-TABSADDR(R2)                                      
         USING TCOMMOND,R2                                                      
         CLI   TCOBCMQ,C'N'        BLOCKCHAIN OUTPUT STOPPED?                   
         DROP  R2                                                               
         SAM24                                                                  
         SAC   0                                                                
         LAM   AR0,ARF,=16F'0'                                                  
         BE    LOGQ080             DO NOT OUTPUT TO BLOCKCHAIN QUEUE            
*                                                                               
         LA    R2,CSQBPUT_LOGB_STRUCTURE                                        
         GOTO1 =A(CALLMQ),(RC)     PUT ONE RECORD TO BLOCKCHAIN QUEUE           
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         JNE   *+2                                                              
         PRNT  MessageLogBC                                                     
*                                                                               
LOGQ080  LA    R2,CSQBCOMM_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     COMMIT                                       
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         JNE   *+2                                                              
*                                                                               
         PRNT  MessageLogCommit                                                 
*                                                                               
LOGQX    XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
***********************************************************************         
* PUT MESSAGE TO WORK QUEUE                                                     
* INPUT: R1 = A(MESSAGE BUFFER)                                                 
*        R2 = MESSAGE LENGTH                                                    
*        R3 = A(USERID NAME)                                                    
*        HALF = USERID#                                                         
***********************************************************************         
PUTWRKOQ NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ST    R1,A_PUTWRKO_BUFF                                                
         ST    R2,PUTWRKO_BUFLEN                                                
*                                  BUILD MSGID                                  
         MVC   MSGDESC_WRKO_MSGID(3),TSKNAME                                    
         MVI   MSGDESC_WRKO_MSGID+3,C'S'     S FOR SENDER                       
         SR    R0,R0                                                            
         ICM   R0,3,HALF           USERID #                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MSGDESC_WRKO_MSGID+4(5),DUB                                      
         MVC   MSGDESC_WRKO_MSGID+9(10),0(R3)   USERID NAME                     
         MVC   MSGDESC_WRKO_MSGID+19(3),SUBTSKID   SUBTASK TO SEND              
*                                                                               
*                                  COPY CORRELID LAST SENT                      
         MVC   MSGDESC_WRKO_CORRELID,MSGDESC_CORRELID                           
*                                                                               
         MVC   OBJDESC_WRKO_OBJECTTYPE,=A(MQOT_Q)  OBJECT IS A QUEUE            
         MVC   OPEN_OPTIONS,=A(MQOO_OUTPUT)                                     
*                                                                               
         MVC   OBJDESC_WRKO_OBJECTNAME,SPACES                                   
         LA    R1,OBJDESC_WRKO_OBJECTNAME    'DARE.SSSS.TTT.WORK.QUEUE'         
         MVC   0(5,R1),=CL5'DARE.'                                              
         AHI   R1,5                                                             
*                                                                               
         CLI   TSKDSPC,C'T'                                                     
         BNE   PWRKO21                                                          
         MVC   0(5,R1),=CL5'TEST.'                                              
         AHI   R1,5                                                             
         B     PWRKO40                                                          
PWRKO21  CLI   TSKDSPC,C'Q'                                                     
         BNE   PWRKO22                                                          
         MVC   0(4,R1),=CL4'FQA.'                                               
         AHI   R1,4                                                             
         B     PWRKO40                                                          
PWRKO22  EQU   *                                                                
         MVC   0(5,R1),=CL5'PROD.'                                              
         AHI   R1,5                                                             
PWRKO40  EQU   *                                                                
         CLI   TSKMETH,METHAPPC    APPC/MVS?                                    
         BNE   PWRKO50                                                          
         MVC   0(L'SUBTSKID,R1),SUBTSKID                                        
         MVI   L'SUBTSKID(R1),C'.'                                              
         AHI   R1,L'SUBTSKID+1           APPC, PUT TO TARGET WORKQ              
         MVC   0(10,R1),=C'WORK.QUEUE'                                          
         B     PWRKO70                                                          
PWRKO50  EQU   *                         MQ, PUT TO DELNOT Q                    
*        **********SPECIAL CODE WO ONLY********************************         
         CLC   =C'QM_WOX',OBJDESC_OBJECTNAME                                    
         BNE   NOTWO3                                                           
         MVC   0(3,R1),=CL3'WO.'                                                
         AHI   R1,3                                                             
NOTWO3   EQU   *                                                                
*        **************************************************************         
         MVC   0(14,R1),=CL14'DNT.WORK.QUEUE'                                   
*                                                                               
PWRKO70  EQU   *                                                                
         LA    R2,CSQBOPEN_WRKO_STRUCTURE                                       
         GOTO1 =A(CALLMQ),(RC)     OPEN WORK QUEUE                              
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,CSQBPUT_WRKO_STRUCTURE                                        
         GOTO1 =A(CALLMQ),(RC)     PUT ONE MESSAGE TO QUEUE                     
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,CSQBCOMM_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     COMMIT WORKQ                                 
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CLOSE_OPTIONS,=A(MQCO_NONE)                                      
         LA    R2,CSQBCLOS_WRKO_STRUCTURE                                       
         GOTO1 =A(CALLMQ),(RC)     CLOSE WORKQ                                  
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P+30(L'OBJDESC_WRKO_OBJECTNAME),OBJDESC_WRKO_OBJECTNAME          
         MVC   P+80(6),=C'MSGID='                                               
         MVC   P+86(L'MSGDESC_WRKO_MSGID),MSGDESC_WRKO_MSGID                    
         PRNT  PutToWorkQueue                                                   
         MVC   P+80(9),=C'CORRELID='                                            
         MVC   P+89(L'MSGDESC_WRKO_CORRELID),MSGDESC_WRKO_CORRELID              
         PRNT  PutToWorkQueue                                                   
         XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
***********************************************************************         
*INPUT:  R1 = A(MESSAGE BUFFER)                                                 
*        R2 = MESSAGE LENGTH                                                    
***********************************************************************         
PUTWRKNQ NTR1  BASE=*,LABEL=*                                                   
*                                  PUT 'NOTNOT' IN MSGID                        
         ST    R1,A_PUTWRKO_BUFF                                                
         ST    R2,PUTWRKO_BUFLEN                                                
*                                  PUT 'NOTNOT' IN MSGID                        
         MVC   MSGDESC_WRKO_MSGID(6),=CL6'NOTNOT'                               
*                                  COPY CORRELID LAST SENT                      
         MVC   MSGDESC_WRKO_CORRELID,MSGDESC_CORRELID                           
*                                                                               
         MVC   OBJDESC_WRKO_OBJECTTYPE,=A(MQOT_Q)  OBJECT IS A QUEUE            
         MVC   OPEN_OPTIONS,=A(MQOO_OUTPUT)                                     
*                                                                               
         MVC   OBJDESC_WRKO_OBJECTNAME,SPACES                                   
         LA    R1,OBJDESC_WRKO_OBJECTNAME    'DARE.SSSS.TTT.WORK.QUEUE'         
         MVC   0(5,R1),=CL5'DARE.'                                              
         AHI   R1,5                                                             
*                                                                               
         CLI   TSKDSPC,C'T'                                                     
         BNE   PWRKN21                                                          
         MVC   0(5,R1),=CL5'TEST.'                                              
         AHI   R1,5                                                             
         B     PWRKN40                                                          
PWRKN21  CLI   TSKDSPC,C'Q'                                                     
         BNE   PWRKN22                                                          
         MVC   0(4,R1),=CL4'FQA.'                                               
         AHI   R1,4                                                             
         B     PWRKN40                                                          
PWRKN22  EQU   *                                                                
         MVC   0(5,R1),=CL5'PROD.'                                              
         AHI   R1,5                                                             
PWRKN40  EQU   *                         PUT TO DELNOT Q                        
*        **********SPECIAL CODE WO ONLY********************************         
         CLC   =C'QM_WOX',OBJDESC_OBJECTNAME                                    
         BNE   NOTWO4                                                           
         MVC   0(3,R1),=CL3'WO.'                                                
         AHI   R1,3                                                             
NOTWO4   EQU   *                                                                
*        **************************************************************         
         MVC   0(14,R1),=CL14'DNT.WORK.QUEUE'                                   
*                                                                               
         LA    R2,CSQBOPEN_WRKO_STRUCTURE                                       
         GOTO1 =A(CALLMQ),(RC)     OPEN WORK QUEUE                              
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,CSQBPUT_WRKO_STRUCTURE                                        
         GOTO1 =A(CALLMQ),(RC)     PUT ONE MESSAGE TO QUEUE                     
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,CSQBCOMM_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     COMMIT WORKQ                                 
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CLOSE_OPTIONS,=A(MQCO_NONE)                                      
         LA    R2,CSQBCLOS_WRKO_STRUCTURE                                       
         GOTO1 =A(CALLMQ),(RC)     CLOSE WORKQ                                  
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P+30(L'OBJDESC_WRKO_OBJECTNAME),OBJDESC_WRKO_OBJECTNAME          
         MVC   P+80(6),=C'MSGID='                                               
         MVC   P+86(L'MSGDESC_WRKO_MSGID),MSGDESC_WRKO_MSGID                    
         PRNT  PutToWorkQueue                                                   
         MVC   P+80(9),=C'CORRELID='                                            
         MVC   P+89(L'MSGDESC_WRKO_CORRELID),MSGDESC_WRKO_CORRELID              
         PRNT  PutToWorkQueue                                                   
         XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
***********************************************************************         
* INPUT: R1 = A(BUFFER)                                                         
* THIS ROUTINE REMOVE "-DS" IN TO/FROM USERID FIELD                             
***********************************************************************         
CHKDDS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   =C'AGYHDR',0(R1)                                                 
         BE    *+14                                                             
         CLC   =C'VARHDR',0(R1)                                                 
         BNE   CORDAPP                                                          
         USING PAGYHDRD,R1                                                      
         LA    R2,PAHDFRID                                                      
         LA    R3,PAHDTOID                                                      
         DROP  R1                                                               
         B     CHKDDSX                                                          
*                                                                               
CORDAPP  CLC   =C'ORDAPP',0(R1)                                                 
         BNE   CORDREJ                                                          
         USING RORDAPPD,R1                                                      
         LA    R2,ROAPFRID                                                      
         LA    R3,ROAPTOID                                                      
         DROP  R1                                                               
         B     CHKDDSX                                                          
*                                                                               
CORDREJ  CLC   =C'ORDREJ',0(R1)                                                 
         BNE   CORDCFM                                                          
         USING RORDREJD,R1                                                      
         LA    R2,RORJFRID                                                      
         LA    R3,RORJTOID                                                      
         DROP  R1                                                               
         B     CHKDDSX                                                          
*                                                                               
CORDCFM  CLC   =C'ORDCFM',0(R1)                                                 
         BNE   CORDRCL                                                          
         USING RORDCFMD,R1                                                      
         LA    R2,ROCFFRID                                                      
         LA    R3,ROCFTOID                                                      
         DROP  R1                                                               
         B     CHKDDSX                                                          
*                                                                               
CORDRCL  CLC   =C'ORDRCL',0(R1)                                                 
         BNE   CAGYRCL                                                          
         USING RORDRCLD,R1                                                      
         LA    R2,RORCFRID                                                      
         LA    R3,RORCTOID                                                      
         DROP  R1                                                               
         B     CHKDDSX                                                          
*                                                                               
CAGYRCL  CLC   =C'AGYRCL',0(R1)                                                 
         BNE   CAGYCAN                                                          
         USING PAGYRCLD,R1                                                      
         LA    R2,PARCFRID                                                      
         LA    R3,PARCTOID                                                      
         DROP  R1                                                               
         B     CHKDDSX                                                          
*                                                                               
CAGYCAN  CLC   =C'AGYCAN',0(R1)                                                 
         BNE   CMKGHDR                                                          
         USING PAGYCAND,R1                                                      
         LA    R2,PACNFRID                                                      
         LA    R3,PACNTOID                                                      
         DROP  R1                                                               
         B     CHKDDSX                                                          
*                                                                               
CMKGHDR  CLC   =C'MKGHDR',0(R1)                                                 
         BNE   CMKGAPP                                                          
         USING MOFRHDRD,R1                                                      
         LA    R2,MOHDFRID                                                      
         LA    R3,MOHDTOID                                                      
         DROP  R1                                                               
         B     CHKDDSX                                                          
*                                                                               
CMKGAPP  CLC   =C'MKGAPP',0(R1)                                                 
         BNE   CMKGREJ                                                          
         USING MOFRAPPD,R1                                                      
         LA    R2,MOAPFRID                                                      
         LA    R3,MOAPTOID                                                      
         DROP  R1                                                               
         B     CHKDDSX                                                          
*                                                                               
CMKGREJ  CLC   =C'MKGREJ',0(R1)                                                 
         BNE   CMKGROK                                                          
         USING MOFRREJD,R1                                                      
         LA    R2,MORJFRID                                                      
         LA    R3,MORJTOID                                                      
         DROP  R1                                                               
         B     CHKDDSX                                                          
*                                                                               
CMKGROK  CLC   =C'MKGROK',0(R1)                                                 
         BNE   CMKGCAN                                                          
         USING MOFRCFMD,R1                                                      
         LA    R2,MOCFFRID                                                      
         LA    R3,MOCFTOID                                                      
         DROP  R1                                                               
         B     CHKDDSX                                                          
*                                                                               
CMKGCAN  CLC   =C'MKGCAN',0(R1)                                                 
         BNE   CDLNNOT                                                          
         USING MOFRCAND,R1                                                      
         LA    R2,MOCNFRID                                                      
         LA    R3,MOCNTOID                                                      
         DROP  R1                                                               
         B     CHKDDSX                                                          
*                                                                               
CDLNNOT  CLC   =C'DLNNOT',0(R1)                                                 
         BE    *+14                                                             
         CLC   =C'ERRNOT',0(R1)                                                 
         BNE   CHKDDSXX                                                         
         USING MDLNNOTD,R1                                                      
         LA    R2,MDNTTOID         SENDER USERID                                
         LA    R3,MDNTFRID         RECEIVER USERID                              
         DROP  R1                                                               
*                                                                               
CHKDDSX  DS    0H                                                               
*                                                                               
         CLI   TSKMETH,METHMQ      MQ SERIES?                                   
         BNE   CHKDDSX5            NO - GO ON TO REMOVE "-DS"                   
*                                                                               
* DON'T REMOVE "-DS" FOR DATATEK'S MQ QUEUE, YYUN, 7/31/03                      
         CLC   =C'DTALICE.QREMOTE',QUEUENAME                                    
         BE    CHKDDSXX                                                         
         CLC   =C'DTPUBLICIS2.QREMOTE',QUEUENAME                                
         BE    CHKDDSXX                                                         
         CLC   =C'DTNORTON.QREMOTE',QUEUENAME                                   
         BE    CHKDDSXX                                                         
*                                                                               
* DON'T REMOVE "-DS" FOR DATATRAK'S MQ QUEUE, SKUI 8/22/03                      
         CLC   =C'CJDSCME.QREMOTE',QUEUENAME                                    
         BE    CHKDDSXX                                                         
         CLC   =C'CJDSDSI.QREMOTE',QUEUENAME                                    
         BE    CHKDDSXX                                                         
         CLC   =C'CJDSFLN.QREMOTE',QUEUENAME                                    
         BE    CHKDDSXX                                                         
         CLC   =C'ENCHWT.QREMOTE',QUEUENAME                                     
         BE    CHKDDSXX                                                         
*                                                                               
CHKDDSX5 DS    0H                                                               
         CLC   =C'-DS',5(R2)       REMOVE "-DS" FOR SENDER USERID               
         BNE   *+10                                                             
         MVC   5(3,R2),SPACES                                                   
*                                                                               
         MVI   RCVR_DS,C'N'                                                     
         CLC   =C'-DS',5(R3)       REMOVE "-DS" FOR RECEIVER USERID             
         BNE   CHKDDSXX                                                         
         MVC   5(3,R3),SPACES                                                   
         MVI   RCVR_DS,C'Y'        REMEMBER THAT WE REMOVE "-DS"                
*                                                                               
CHKDDSXX XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
*                                                                               
COMMWORK DS    0D                  COMMON STORAGE AREA                          
*                                                                               
         DS    0D                                                               
         DC    CL16'SSB+SSB+SSB+SSB+'                                           
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         DC    CL16'SSB-SSB-SSB-SSB-'                                           
*                                                                               
         DS    0D                                                               
         DC    CL16'UTL+UTL+UTL+UTL+'                                           
UTL      DC    F'0',X'0A',251X'00'                                              
         DC    CL16'UTL-UTL-UTL-UTL-'                                           
*                                                                               
         ATBSERV                                                                
         ATBCTASM                                                               
         CMQA LIST=YES,EQUONLY=NO                                               
*                                                                               
* APPC/MVS CALL PARAMETERS                                                      
*                                                                               
CONVERSATION_TYPE              DS    F                                          
SYM_DEST_NAME                  DC    CL8' '                                     
PARTNER_LU_NAME                DC    CL17' '                                    
LOCAL_LU_NAME                  DC    CL8' '                                     
MODE_NAME                      DS    CL8                                        
TP_NAME_LENGTH                 DS    F                                          
TP_NAME                        DC    CL64' '                                    
RECEIVE_ALLOCATE_TYPE          DS    F                                          
TIME_OUT_VALUE                 DC    F'0'                                       
RETURN_CONTROL                 DS    F                                          
SYNC_LEVEL                     DS    F                                          
SECURITY_TYPE                  DS    F                                          
CONVERSATION_CORRELATOR        DS    CL8                                        
USER_ID                        DC    CL10' '                                    
PROFILE                        DC    CL10' '                                    
USER_TOKEN                     DC    XL80'00'                                   
CONVERSATION_ID                DS    CL8                                        
ALLOCATE_QUEUE_TOKEN           DC    XL8'00'                                    
LUW_ID                         DS    XL26                                       
ALLOCATE_QUEUE_SIZE            DS    F                                          
ALLOCATE_QUEUE_OLDEST          DS    F                                          
LAST_REC_ALLOC_ISSUED          DS    CL8                                        
LAST_REC_ALLOC_RETURNED        DS    CL8                                        
CONVERSATION_STATE             DS    F                                          
NOTIFY_TYPE                    DS    F                                          
                               DC    A(APPCECB) MUST FOLLOW NOTIFY_TYPE         
TPID                           DC    XL8'00'                                    
RETURN_CODE                    DS    F                                          
REASON_CODE                    DS    F                                          
FILL                           DS    F                                          
RECEIVE_LENGTH                 DS    F                                          
RECEIVE_ACCESS_TOKEN           DC    F'0'                                       
STATUS_RECEIVED                DS    F                                          
DATA_RECEIVED                  DS    F                                          
REQUEST_TO_SEND_RECEIVED       DS    F                                          
REQUEST_TO_SEND_VALUE          DS    F                                          
PREP_TO_RECEIVE_TYPE           DS    F                                          
DEALLOCATE_TYPE                DS    F                                          
SEND_TYPE                      DS    F                                          
SEND_LENGTH                    DS    F                                          
SEND_ACCESS_TOKEN              DC    F'0'                                       
BUFFER_LENGTH                  DS    H      (BUFFER MUST FOLLOW THIS)           
BUFFER                         DS    CL256                                      
SERVICE_NAME                   DS    CL8                                        
SERVICE_REASON_CODE            DS    F                                          
MESSAGE_TEXT_LENGTH            DS    F                                          
MESSAGE_TEXT                   DS    CL256                                      
ERROR_LOG_PRODUCT_SET_ID_LENGTH DS   F                                          
ERROR_LOG_PRODUCT_SET_ID       DS    CL256                                      
ERROR_LOG_INFORMATION_LENGTH   DS    F                                          
ERROR_LOG_INFORMATION          DS    CL512                                      
REASON_CODE_ERROR_EXTRACT      DS    F                                          
RETURN_CODE_ERROR_EXTRACT      DS    F                                          
                                                                                
***********************************************************************         
* MQSERIES CALL PARAMETERS                                                      
***********************************************************************         
                     DS      0D                                                 
QMGRNAME             DS      CL48      MQ QUEUE MANAGER NAME                    
QUEUENAME            DS      CL48      MQ QUEUE NAME                            
HCONN                DS      F         MQ QMGR CONNECTION HANDLE                
HOBJ                 DS      F         OBJECT HANDLE FOR OUTPUT QUEUES          
HOBJ_WRKI            DS      F         OBJECT HANDLE FOR WRKI QUEUE             
HOBJ_WRKO            DS      F         OBJECT HANDLE FOR WRKO QUEUE             
HOBJ_LOG             DS      F         OBJECT HANDLE FOR LOG QUEUE              
HOBJ_LOG2            DS      F         OBJECT HANDLE FOR LOG QUEUE              
HOBJ_LOGB            DS      F         OBJECT HANDLE FOR LOG QUEUE              
OPEN_OPTIONS         DS      F         MQOPEN OPTIONS                           
CLOSE_OPTIONS        DS      F         MQCLOSE OPTIONS                          
COMPCODE             DS      F         COMPLETION CODE                          
REASON               DS      F         QUALIFIES COMPLETION CODE                
MQ_COMPCODE          DS      F         COMPLETION CODE                          
MQ_REASON            DS      F         QUALIFIES COMPLETION CODE                
BUFFERLENGTH         DS      F         LENGTH OF BUFFER AREA                    
BUFFLEN_WRKI         DS      F         LENGTH OF BUFFER AREA                    
PUTLOG_BUFLEN        DS      F         LENGTH OF BUFFER AREA FOR MQPUT          
PUTWRKO_BUFLEN       DS      F         LENGTH OF BUFFER AREA FOR MQPUT          
AMQWRKXA             DS      A         ADDRESS OF MQ BUFFER AREA IN XA          
AMQWRKXX             DS      A         A(END OF MQ BUFFER)                      
AMQWRKPT             DS      A         A(MQ BUFFER AVAILABLE)                   
AMQBUFXA             DS      A         ADDRESS OF MQ BUFFER AREA IN XA          
AMQBUFPT             DS      A         A(MQ BUFFER AVAILABLE)                   
AMQBUFXX             DS      A         A(END OF MQ BUFFER)                      
MQBUFLQ              EQU     2*1024*1024    MAX MQ BUFFER SIZE (2M)             
*MQBUFLQ              EQU     65535     MAX MQ BUFFER SIZE                      
DATALENGTH           DS      F         LENGTH OF THE MESSAGE                    
OBJDESC              CMQODA  LIST=YES  OBJECT DESCRIPTOR                        
MSGDESC              CMQMDA  LIST=YES  MESSAGE DESCRIPTOR                       
         ORG   MSGDESC_FORMAT                                                   
         DC    CL8'MQSTR   '           MQFMT_STRING  FOR DATA FORMAT            
         ORG                                                                    
GETMSGOPTS           CMQGMOA LIST=YES  GET MESSAGE OPTIONS                      
PUTMSGOPTS           CMQPMOA LIST=YES  PUT MESSAGE OPTIONS                      
*                                                                               
OBJDESC_WRKI   CMQODA  LIST=YES  OBJECT DESCRIPTOR                              
MSGDESC_WRKI   CMQMDA  LIST=YES  MESSAGE DESCRIPTOR                             
*                                                                               
OBJDESC_WRKO   CMQODA  LIST=YES  OBJECT DESCRIPTOR                              
MSGDESC_WRKO   CMQMDA  LIST=YES  MESSAGE DESCRIPTOR                             
         ORG   MSGDESC_WRKO_FORMAT                                              
         DC    CL8'MQSTR   '   MQFMT_STRING  FOR DATA FORMAT                    
         ORG                                                                    
PUTMSGOPTS_WRKO CMQPMOA LIST=YES  PUT MESSAGE OPTIONS                           
*                                                                               
OBJDESC_LOG    CMQODA  LIST=YES  OBJECT DESCRIPTOR                              
OBJDESC_LOG2   CMQODA  LIST=YES  OBJECT DESCRIPTOR                              
OBJDESC_LOGB   CMQODA  LIST=YES  OBJECT DESCRIPTOR                              
MSGDESC_LOG    CMQMDA  LIST=YES  MESSAGE DESCRIPTOR                             
         ORG   MSGDESC_LOG_FORMAT                                               
         DC    CL8'MQSTR   '   MQFMT_STRING  FOR DATA FORMAT                    
         ORG                                                                    
PUTMSGOPTS_LOG CMQPMOA LIST=YES  PUT MESSAGE OPTIONS                            
         ORG   PUTMSGOPTS_LOG_OPTIONS                                           
         DC    A(MQPMO_SET_IDENTITY_CONTEXT)                                    
         ORG                                                                    
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
CRLF     DS    0XL2                                                             
CR       DC    X'0D'                                                            
LF       DC    X'25'                                                            
         SPACE 5                                                                
APPCM1   DC    C'+++DARE SENDER='                                               
APPCM1Q  EQU   *-APPCM1                                                         
         DC    C'XXX '                                                          
APPCM1A  DC    C'CONV=RECEIVE VERSION=1 '                                       
APPCM1AQ EQU   *-APPCM1A                                                        
         DC    C'DATE=YYYYMMDD TIME=HHMMSS'                                     
         SPACE 2                                                                
APPCM2   DC    C'+++DARE SENDER=DDS VERSION=1 STATUS=READY '                    
APPCM2A  DC    C'DATE=YYYYMMDD TIME=HHMMSS'                                     
APPCM2AQ EQU   *-APPCM2                                                         
         SPACE 2                                                                
APPCM3   DC    C'+++DARE SEND TIME=HHMMSS'                                      
APPCM3Q  EQU   *-APPCM3                                                         
         SPACE 2                                                                
APPCM4   DC    C'+++DARE END TIME=HHMMSS COUNT=NNNNNN'                          
APPCM4Q  EQU   *-APPCM4                                                         
         SPACE 2                                                                
APPCM5   DC    C'+++DARE LOGGING=VTTTTBBRRF'                                    
APPCM5Q  EQU   *-APPCM5                                                         
         SPACE 5                                                                
* FOR DYNAMIC ALLOCATION OF SYSPRINT LOGS                                       
         DS    0F                                                               
ARBLK    DC    X'80',AL3(RBLK)     R1 POINTS TO THIS BEFORE DYNALLOC            
         SPACE                                                                  
RBLK     DC    X'1401000000000000',A(ATXT),X'0000000018000000'                  
         SPACE                                                                  
ATXT     DC    X'00',AL3(TXTDD)    DDNAME                                       
         DC    X'80',AL3(TXTSYSOU) SYSOUT                                       
         SPACE                                                                  
TXTDD    DC    AL2(DALDDNAM),X'00010008',C'        '  DDNAME=                   
TXTSYSOU DC    AL2(DALSYSOU),X'00010001',C'X'         SYSOUT=X                  
         SPACE                                                                  
ECBLST2  DS    0F                                                               
AAPPCECB DC    X'00',AL3(APPCECB)  A(APPCECB)                                   
ASTPECB2 DC    X'80',AL3(0)        A(STOPECB)                                   
         EJECT                                                                  
DMWORK   DS    12D                                                              
DMCB     DS    10F                                                              
DUB      DS    D                                                                
DUB2     DS    D                                                                
PRNTDUB  DS    D                                                                
FULL     DS    F                                                                
PACKOF4B DS    PL4                                                              
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
PRNTTIME DS    CL9                                                              
YYYYMMDD DS    CL8                 DATE                                         
HHMMSS   DS    CL6                 TIME                                         
WORK     DS    CL256                                                            
         SPACE 2                                                                
VDDSIO   DC    V(DDSIO)                                                         
DATAMGR  DC    V(DATAMGR)                                                       
APPCECB  DC    F'0'                ECB FOR APPC/MVS CALLS                       
WORKQECB DC    F'0'                ECB FOR MQ SERIES REPLY QUEUE                
ATSKTAB  DC    A(0)                A(SUBTASK TABLE)                             
RECCOUNT DS    F                   RECORD COUNT IN MESSAGE                      
THREESEC DC    F'300'              FOR WAITS BETWEEN PRINT QUEUE WRITES         
STIMER1  DS    XL4                 FOR WAITS BETWEEN PRINT QUEUE WRITES         
NUMREPS  DS    A                   NUMBER OF REPORTS FOUND                      
MQOUTVER DS    C                   MQ OUTPUT FORMAT VERSION NUMBER              
PRTQNINT DS    X                   INTERNAL HEX PRINT QUEUE NUMBER              
PRTQNEXT DS    X                   EXTERNAL HEX PRINT QUEUE NUMBER              
PRTQNAME DS    CL5                 EBCDIC PRINT QUEUE NAME                      
DARE     DC    C'DARE    '         MAJOR NAME FOR ENQ                           
SUBTSKID DS    CL3                 TASK ID (E.G., 'EDA' = EDICTA, ETC)          
USERID   DS    XL2                 DDS USERID (NUMERIC)                         
DARETYPE DS    CL6                 DARE ORDER TYPE                              
SENDER   DS    CL10                USERID OF SENDER (EBCDIC)                    
RECEIVER DS    CL10                USERID OF RECEIVER (EBCDIC)                  
ORDNUM   DS    CL8                 ORDER NUMBER                                 
CONTRACT DS    CL8                 REP CONTRACT                                 
RETURN   DS    CL16                'RETURN TO SENDER' DATA                      
ORIGDATE DS    CL6                 DATE                                         
ORIGTIME DS    CL4                 TIME                                         
OFFERID  DS    CL3                 MAKEGOOD OFFER GROUP ID                      
IDSEQNUM DS    CL2                 VERSION NUMBER WITHIN OFFER ID               
LOGDATA  DS    CL10                FROM LOGGING= RECORD                         
*                                                                               
COA#PL8  DC    PL8'0'              UNIQUE COA SEQ# IN PL8                       
COA####  DS    0CL24                                                            
COA#VER  DC    CL1'1'              UNIQUE COA VERSION#                          
COA#DATE DC    CL4' '              UNIQUE COA DATE (MMDD)                       
COA#TIM  DC    CL6' '              UNIQUE COA TIME (HHMMSS)                     
COA#SEQ  DC    CL10'0'             UNIQUE COA SEQ#                              
COA#END  DC    CL3' '              UNIQUE COA SPARE                             
*                                                                               
OPERSTOP DC    C'N'                'Y' IF WE MUST STOP                          
SWTOSEND DS    C                   REQUEST TO SWITCH TO SEND MODE               
NEEDDLN  DS    C                   'Y' = WE MUST RETURN A DLNNOT                
RCVR_DS  DS    C                   'Y' = WE REMOVED "-DS" IN RECEIVER           
*                                                                               
LOGFLAG  DS    C                   'Y' = LOG ALL MSG TO MQ LOG QUEUE            
LOGFLAG2 DS    C                   'Y' = LOG ALL MSG TO MQ LOG QUEUE            
LOGFLAGB DS    C                   'Y' = PUT ALL MSG TO BLOCKCHAIN Q            
*                                                                               
OPMSG1   DC    C'*DARE ERROR DURING HANDSHAKE WITH XXX'                         
OPMSG2   DC    C'*DARE ERROR SENDING TO XXX. TRANSMISSIONS SUSPENDED.'          
OPMSG3   DC    C'*DARE RECEIVER FROM XXX DIED*'                                 
OPMSG4   DC    C'HANDSHAKE WITH SENDER   TO   XXX OK'                           
OPMSG5   DC    C'*DARE: UNDEFINED LOCAL LUNAME XXXXXXXX, PARTNER XXX, R+        
               EPLY RETRY OR CANCEL'                                            
OPMSG6   DC    C'*DARE WARNING: PARTNER XXX HAS NOT YET ALLOCATED ITS R+        
               ECEIVER'                                                         
*                                                                               
OPMSG7   DC    C'AUTONOTE*US-MF_FAC_NOTIFY:'                                    
         DC    C'QUEUE IS NOT DEFINED, '                                        
OPMSG7A  DC    CL(L'OBJDESC_OBJECTNAME)' '                                      
OPMSG7Q  EQU   *-OPMSG7                                                         
*                                                                               
OPMSG8   DC    C'AUTONOTE*US-DARE_NOTIFY:'                                      
         DC    C'*DARE ERROR SENDING TO '                                       
OPMSG8A  DC    CL3'XXX'                                                         
OPMSG8Q  EQU   *-OPMSG8                                                         
*                                                                               
* PARAMETER LISTS FOR APPC/MVS AND MQ SERIES CALLS                              
*                                                                               
*  F    A(ROUTINE)                                                              
*  CL16 EBCDIC ROUTINE NAME                                                     
*  XL1  FLAGS                                                                   
*       X'80': ROUTINE IS SYNCHRONOUS ONLY                                      
*       X'40': ROUTINE CAN BE FOLLOWED BY EXTRACT_ERROR CALL                    
*  XL3  SPARE                                                                   
*  PARAMETERS (STANDARD IBM FORMAT)                                             
*                                                                               
ATBRAL2_STRUCTURE         DS    A                                               
                          DC    CL16'ReceiveAllocate'                           
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'00',AL3(ALLOCATE_QUEUE_TOKEN)                 
                          DC    X'00',AL3(RECEIVE_ALLOCATE_TYPE)                
                          DC    X'00',AL3(TIME_OUT_VALUE)                       
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(CONVERSATION_TYPE)                    
                          DC    X'00',AL3(PARTNER_LU_NAME)                      
                          DC    X'00',AL3(MODE_NAME)                            
                          DC    X'00',AL3(SYNC_LEVEL)                           
                          DC    X'00',AL3(USER_ID)                              
                          DC    X'00',AL3(PROFILE)                              
                          DC    X'00',AL3(REASON_CODE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBRFA2_STRUCTURE         DS    A                                               
                          DC    CL16'Register4Allocs'                           
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'00',AL3(SYM_DEST_NAME)                        
                          DC    X'00',AL3(TP_NAME_LENGTH)                       
                          DC    X'00',AL3(TP_NAME)                              
                          DC    X'00',AL3(LOCAL_LU_NAME)                        
                          DC    X'00',AL3(PARTNER_LU_NAME)                      
                          DC    X'00',AL3(USER_ID)                              
                          DC    X'00',AL3(PROFILE)                              
                          DC    X'00',AL3(ALLOCATE_QUEUE_TOKEN)                 
                          DC    X'00',AL3(REASON_CODE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBURA2_STRUCTURE         DS    A                                               
                          DC    CL16'Unregister4Alloc'                          
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'00',AL3(ALLOCATE_QUEUE_TOKEN)                 
                          DC    X'00',AL3(REASON_CODE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBCFMD_STRUCTURE         DS    A                                               
                          DC    CL16'Confirmed'                                 
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBDEAL_STRUCTURE         DS    A                                               
                          DC    CL16'Deallocate'                                
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(DEALLOCATE_TYPE)                      
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBRCVW_STRUCTURE         DS    A                                               
                          DC    CL16'ReceiveAndWait'                            
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(FILL)                                 
                          DC    X'00',AL3(RECEIVE_LENGTH)                       
                          DC    X'00',AL3(RECEIVE_ACCESS_TOKEN)                 
ATBRCVW_BUFFER_PARAMETER  DC    X'00',AL3(BUFFER)                               
                          DC    X'00',AL3(STATUS_RECEIVED)                      
                          DC    X'00',AL3(DATA_RECEIVED)                        
                          DC    X'00',AL3(REQUEST_TO_SEND_RECEIVED)             
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBSEND_STRUCTURE         DS    A                                               
                          DC    CL16'SendData'                                  
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(SEND_TYPE)                            
                          DC    X'00',AL3(SEND_LENGTH)                          
                          DC    X'00',AL3(SEND_ACCESS_TOKEN)                    
ATBSEND_BUFFER_PARAMETER  DC    X'00',AL3(BUFFER)                               
                          DC    X'00',AL3(REQUEST_TO_SEND_VALUE)                
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBGTA2_STRUCTURE         DS    A                                               
                          DC    CL16'GetAttributes'                             
                          DC    X'C0'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(PARTNER_LU_NAME)                      
                          DC    X'00',AL3(MODE_NAME)                            
                          DC    X'00',AL3(SYNC_LEVEL)                           
                          DC    X'00',AL3(CONVERSATION_CORRELATOR)              
                          DC    X'00',AL3(LUW_ID)                               
                          DC    X'00',AL3(TP_NAME_LENGTH)                       
                          DC    X'00',AL3(TP_NAME)                              
                          DC    X'00',AL3(LOCAL_LU_NAME)                        
                          DC    X'00',AL3(CONVERSATION_TYPE)                    
                          DC    X'00',AL3(USER_ID)                              
                          DC    X'00',AL3(PROFILE)                              
                          DC    X'00',AL3(USER_TOKEN)                           
                          DC    X'00',AL3(CONVERSATION_STATE)                   
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBQAQ2_STRUCTURE         DS    A                                               
                          DC    CL16'QueryAllocateQ'                            
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'00',AL3(ALLOCATE_QUEUE_TOKEN)                 
                          DC    X'00',AL3(TP_NAME_LENGTH)                       
                          DC    X'00',AL3(TP_NAME)                              
                          DC    X'00',AL3(LOCAL_LU_NAME)                        
                          DC    X'00',AL3(ALLOCATE_QUEUE_SIZE)                  
                          DC    X'00',AL3(ALLOCATE_QUEUE_OLDEST)                
                          DC    X'00',AL3(LAST_REC_ALLOC_ISSUED)                
                          DC    X'00',AL3(LAST_REC_ALLOC_RETURNED)              
                          DC    X'00',AL3(REASON_CODE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBEES3_STRUCTURE         DS    A                                               
                          DC    CL16'ErrorExtract'                              
                          DC    X'80'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(SERVICE_NAME)                         
                          DC    X'00',AL3(SERVICE_REASON_CODE)                  
                          DC    X'00',AL3(MESSAGE_TEXT_LENGTH)                  
                          DC    X'00',AL3(MESSAGE_TEXT)                         
                          DC X'00',AL3(ERROR_LOG_PRODUCT_SET_ID_LENGTH)         
                          DC    X'00',AL3(ERROR_LOG_PRODUCT_SET_ID)             
                          DC    X'00',AL3(ERROR_LOG_INFORMATION_LENGTH)         
                          DC    X'00',AL3(ERROR_LOG_INFORMATION)                
                          DC    X'00',AL3(REASON_CODE_ERROR_EXTRACT)            
                          DC    X'80',AL3(RETURN_CODE_ERROR_EXTRACT)            
*                                                                               
CSQBCONN_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_Connect'                                
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_CONN_COMPCODE)                              
                          DC    A(A_CONN_REASON)                                
                          DC    X'00',AL3(QMGRNAME)                             
                          DC    X'00',AL3(HCONN)                                
A_CONN_COMPCODE           DC    X'00',AL3(COMPCODE)                             
A_CONN_REASON             DC    X'80',AL3(REASON)                               
*                                                                               
CSQBOPEN_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_Open'                                   
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_OPEN_COMPCODE)                              
                          DC    A(A_OPEN_REASON)                                
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(OBJDESC)                              
                          DC    X'00',AL3(OPEN_OPTIONS)                         
                          DC    X'00',AL3(HOBJ)                                 
A_OPEN_COMPCODE           DC    X'00',AL3(COMPCODE)                             
A_OPEN_REASON             DC    X'80',AL3(REASON)                               
*                                                                               
CSQBOPEN_WRKI_STRUCTURE   DS    A                                               
                          DC    CL16'MQ_OpenWorkIn'                             
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_OPEN_WRKI_COMPCODE)                         
                          DC    A(A_OPEN_WRKI_REASON)                           
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(OBJDESC_WRKI)                         
                          DC    X'00',AL3(OPEN_OPTIONS)                         
                          DC    X'00',AL3(HOBJ_WRKI)                            
A_OPEN_WRKI_COMPCODE      DC    X'00',AL3(COMPCODE)                             
A_OPEN_WRKI_REASON        DC    X'80',AL3(REASON)                               
*                                                                               
CSQBOPEN_WRKO_STRUCTURE   DS    A                                               
                          DC    CL16'MQ_OpenWorkOut'                            
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_OPEN_WRKO_COMPCODE)                         
                          DC    A(A_OPEN_WRKO_REASON)                           
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(OBJDESC_WRKO)                         
                          DC    X'00',AL3(OPEN_OPTIONS)                         
                          DC    X'00',AL3(HOBJ_WRKO)                            
A_OPEN_WRKO_COMPCODE      DC    X'00',AL3(COMPCODE)                             
A_OPEN_WRKO_REASON        DC    X'80',AL3(REASON)                               
*                                                                               
CSQBOPEN_LOG_STRUCTURE    DS    A                                               
                          DC    CL16'MQ_OpenLog'                                
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_OPEN_LOG_COMPCODE)                          
                          DC    A(A_OPEN_LOG_REASON)                            
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(OBJDESC_LOG)                          
                          DC    X'00',AL3(OPEN_OPTIONS)                         
                          DC    X'00',AL3(HOBJ_LOG)                             
A_OPEN_LOG_COMPCODE       DC    X'00',AL3(COMPCODE)                             
A_OPEN_LOG_REASON         DC    X'80',AL3(REASON)                               
*                                                                               
CSQBOPEN_LOG2_STRUCTURE   DS    A                                               
                          DC    CL16'MQ_OpenLog2'                               
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_OPEN_LOG2_COMPCODE)                         
                          DC    A(A_OPEN_LOG2_REASON)                           
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(OBJDESC_LOG2)                         
                          DC    X'00',AL3(OPEN_OPTIONS)                         
                          DC    X'00',AL3(HOBJ_LOG2)                            
A_OPEN_LOG2_COMPCODE      DC    X'00',AL3(COMPCODE)                             
A_OPEN_LOG2_REASON        DC    X'80',AL3(REASON)                               
*                                                                               
CSQBOPEN_LOGB_STRUCTURE   DS    A                                               
                          DC    CL16'MQ_OpenLogB'                               
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_OPEN_LOGB_COMPCODE)                         
                          DC    A(A_OPEN_LOGB_REASON)                           
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(OBJDESC_LOGB)                         
                          DC    X'00',AL3(OPEN_OPTIONS)                         
                          DC    X'00',AL3(HOBJ_LOGB)                            
A_OPEN_LOGB_COMPCODE      DC    X'00',AL3(COMPCODE)                             
A_OPEN_LOGB_REASON        DC    X'80',AL3(REASON)                               
*                                                                               
CSQBGET_WRKI_STRUCTURE    DS    A                                               
                          DC    CL16'MQ_Get'                                    
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_GET_WRKI_COMPCODE)                          
                          DC    A(A_GET_WRKI_REASON)                            
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ_WRKI)                            
                          DC    X'00',AL3(MSGDESC_WRKI)                         
                          DC    X'00',AL3(GETMSGOPTS)                           
                          DC    X'00',AL3(BUFFLEN_WRKI)                         
A_GET_WRKI_BUFFER         DC    XL4'00'                                         
                          DC    X'00',AL3(DATALENGTH)                           
A_GET_WRKI_COMPCODE       DC    X'00',AL3(COMPCODE)                             
A_GET_WRKI_REASON         DC    X'80',AL3(REASON)                               
*                                                                               
CSQBPUT_STRUCTURE         DS    A                                               
                          DC    CL16'MQ_Put'                                    
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_PUT_COMPCODE)                               
                          DC    A(A_PUT_REASON)                                 
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ)                                 
                          DC    X'00',AL3(MSGDESC)                              
                          DC    X'00',AL3(PUTMSGOPTS)                           
                          DC    X'00',AL3(BUFFERLENGTH)                         
A_PUT_BUFF                DC    X'00',AL3(0)                                    
A_PUT_COMPCODE            DC    X'00',AL3(COMPCODE)                             
A_PUT_REASON              DC    X'80',AL3(REASON)                               
*                                                                               
CSQBPUT_WRKO_STRUCTURE    DS    A                                               
                          DC    CL16'MQ_PutWorkOut'                             
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_PUTWRKO_COMPCODE)                           
                          DC    A(A_PUTWRKO_REASON)                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ_WRKO)                            
                          DC    X'00',AL3(MSGDESC_WRKO)                         
                          DC    X'00',AL3(PUTMSGOPTS_WRKO)                      
                          DC    X'00',AL3(PUTWRKO_BUFLEN)                       
A_PUTWRKO_BUFF            DC    X'00',AL3(0)                                    
A_PUTWRKO_COMPCODE        DC    X'00',AL3(COMPCODE)                             
A_PUTWRKO_REASON          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBPUT_LOG_STRUCTURE     DS    A                                               
                          DC    CL16'MQ_PutLog'                                 
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_PUTLOG_COMPCODE)                            
                          DC    A(A_PUTLOG_REASON)                              
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ_LOG)                             
                          DC    X'00',AL3(MSGDESC_LOG)                          
                          DC    X'00',AL3(PUTMSGOPTS_LOG)                       
                          DC    X'00',AL3(PUTLOG_BUFLEN)                        
A_PUTLOG_BUFF             DC    X'00',AL3(0)                                    
A_PUTLOG_COMPCODE         DC    X'00',AL3(COMPCODE)                             
A_PUTLOG_REASON           DC    X'80',AL3(REASON)                               
*                                                                               
CSQBPUT_LOG2_STRUCTURE    DS    A                                               
                          DC    CL16'MQ_PutLog2'                                
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_PUTLOG2_COMPCODE)                           
                          DC    A(A_PUTLOG2_REASON)                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ_LOG2)                            
                          DC    X'00',AL3(MSGDESC_LOG)                          
                          DC    X'00',AL3(PUTMSGOPTS_LOG)                       
                          DC    X'00',AL3(PUTLOG_BUFLEN)                        
A_PUTLOG2_BUFF            DC    X'00',AL3(0)                                    
A_PUTLOG2_COMPCODE        DC    X'00',AL3(COMPCODE)                             
A_PUTLOG2_REASON          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBPUT_LOGB_STRUCTURE    DS    A                                               
                          DC    CL16'MQ_PutLogB'                                
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_PUTLOGB_COMPCODE)                           
                          DC    A(A_PUTLOGB_REASON)                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ_LOGB)                            
                          DC    X'00',AL3(MSGDESC_LOG)                          
                          DC    X'00',AL3(PUTMSGOPTS_LOG)                       
                          DC    X'00',AL3(PUTLOG_BUFLEN)                        
A_PUTLOGB_BUFF            DC    X'00',AL3(0)                                    
A_PUTLOGB_COMPCODE        DC    X'00',AL3(COMPCODE)                             
A_PUTLOGB_REASON          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCOMM_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_Commit'                                 
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_COMM_COMPCODE)                              
                          DC    A(A_COMM_REASON)                                
                          DC    X'00',AL3(HCONN)                                
A_COMM_COMPCODE           DC    X'00',AL3(COMPCODE)                             
A_COMM_REASON             DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCLOS_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_Close'                                  
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_CLOSE_COMPCODE)                             
                          DC    A(A_CLOSE_REASON)                               
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ)                                 
                          DC    X'00',AL3(CLOSE_OPTIONS)                        
A_CLOSE_COMPCODE          DC    X'00',AL3(COMPCODE)                             
A_CLOSE_REASON            DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCLOS_WRKI_STRUCTURE   DS     A                                              
                          DC    CL16'MQ_CloseWorkIn'                            
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_CLOSE_WRKI_COMPCODE)                        
                          DC    A(A_CLOSE_WRKI_REASON)                          
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ_WRKI)                            
                          DC    X'00',AL3(CLOSE_OPTIONS)                        
A_CLOSE_WRKI_COMPCODE     DC    X'00',AL3(COMPCODE)                             
A_CLOSE_WRKI_REASON       DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCLOS_WRKO_STRUCTURE   DS     A                                              
                          DC    CL16'MQ_CloseWorkOut'                           
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_CLOSE_WRKO_COMPCODE)                        
                          DC    A(A_CLOSE_WRKO_REASON)                          
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ_WRKO)                            
                          DC    X'00',AL3(CLOSE_OPTIONS)                        
A_CLOSE_WRKO_COMPCODE     DC    X'00',AL3(COMPCODE)                             
A_CLOSE_WRKO_REASON       DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCLOS_LOG_STRUCTURE    DS    A                                               
                          DC    CL16'MQ_CloseLog'                               
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_CLOSE_LOG_COMPCODE)                         
                          DC    A(A_CLOSE_LOG_REASON)                           
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ_LOG)                             
                          DC    X'00',AL3(CLOSE_OPTIONS)                        
A_CLOSE_LOG_COMPCODE      DC    X'00',AL3(COMPCODE)                             
A_CLOSE_LOG_REASON        DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCLOS_LOG2_STRUCTURE   DS    A                                               
                          DC    CL16'MQ_CLoseLog2'                              
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_CLOSE_LOG2_COMPCODE)                        
                          DC    A(A_CLOSE_LOG2_REASON)                          
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ_LOG2)                            
                          DC    X'00',AL3(CLOSE_OPTIONS)                        
A_CLOSE_LOG2_COMPCODE     DC    X'00',AL3(COMPCODE)                             
A_CLOSE_LOG2_REASON       DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCLOS_LOGB_STRUCTURE   DS    A                                               
                          DC    CL16'MQ_CloseLogB'                              
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_CLOSE_LOGB_COMPCODE)                        
                          DC    A(A_CLOSE_LOGB_REASON)                          
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ_LOGB)                            
                          DC    X'00',AL3(CLOSE_OPTIONS)                        
A_CLOSE_LOGB_COMPCODE     DC    X'00',AL3(COMPCODE)                             
A_CLOSE_LOGB_REASON       DC    X'80',AL3(REASON)                               
*                                                                               
CSQBDISC_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_Disconnect'                             
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_DISC_COMPCODE)                              
                          DC    A(A_DISC_REASON)                                
                          DC    X'00',AL3(HCONN)                                
A_DISC_COMPCODE           DC    X'00',AL3(COMPCODE)                             
A_DISC_REASON             DC    X'80',AL3(REASON)                               
*                                                                               
ENTRYPTS DS    0F                                                               
         DC    Y((ENTRYLSQ-ENTRYSTQ)/60)   NUMBER OF TABLE ENTRIES              
         DC    H'60'                       MUST REMAIN AS 60                    
ENTRYSTQ EQU   *                                                                
*                                                                               
ATBCFMD  DC    CL8'ATBCFMD'                                                     
         DC    XL52'00'                                                         
ATBDEAL  DC    CL8'ATBDEAL'                                                     
         DC    XL52'00'                                                         
ATBEES3  DC    CL8'ATBEES3'                                                     
         DC    XL52'00'                                                         
ATBGTA2  DC    CL8'ATBGTA2'                                                     
         DC    XL52'00'                                                         
ATBQAQ2  DC    CL8'ATBQAQ2'                                                     
         DC    XL52'00'                                                         
ATBRAL2  DC    CL8'ATBRAL2'                                                     
         DC    XL52'00'                                                         
ATBRCVW  DC    CL8'ATBRCVW'                                                     
         DC    XL52'00'                                                         
ATBRFA2  DC    CL8'ATBRFA2'                                                     
         DC    XL52'00'                                                         
ATBSEND  DC    CL8'ATBSEND'                                                     
         DC    XL52'00'                                                         
ATBURA2  DC    CL8'ATBURA2'                                                     
         DC    XL52'00'                                                         
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
         DC    C'*DNOTBUF'                                                      
DNOTBUF  DS    (DNOTBUFL)X         DELNOT BUFFER                                
DNOTBUFX EQU   *                                                                
DNOTBUFL EQU   APPCM3Q+APPCM4Q+MDLNNOTL+APPCM5Q+4*L'CRLF                        
         DC    CL20'***END OF DNOTBUF***'                                       
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
*                                                                               
       ++INCLUDE DDMQREASON                                                     
       ++INCLUDE DDDAREWRKD                                                     
       ++INCLUDE SPDARDARED                                                     
       ++INCLUDE SPDARMKGDD                                                     
*                                                                               
* DDDPRINT                                                                      
* CTGENFILE                                                                     
* FATABSD                                                                       
* FATABSCOM                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FATABSD                                                        
       ++INCLUDE FATABSCOM                                                      
*                                                                               
         DCBD  DSORG=PS,DEVD=DA                                                 
*                                                                               
         IEFZB4D2                                                               
*                                                                               
         PUSH  ACONTROL                                                         
         ACONTROL COMPAT(NOCASE)                                                
         IHAPSA                                                                 
         POP   ACONTROL                                                         
*                                                                               
         PUSH  ACONTROL                                                         
         ACONTROL COMPAT(NOCASE)                                                
         IKJTCB                                                                 
         POP   ACONTROL                                                         
*                                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048DDDARESND 02/28/19'                                      
         END                                                                    
