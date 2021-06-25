*          DATA SET DDMQPR     AT LEVEL 001 AS OF 12/18/14                      
*PHASE MQPRA                                                                    
*INCLUDE CARDS                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE ADDAY                                                                  
*                                                                               
         TITLE 'MQ MSG PROCESSING TOOL'                                         
*                                                                               
* PROGRAM TO BROWSE, PURGE, COPY AND MOVE MESSAGES                              
*                                                                               
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
*                                                                               
*                                                                               
*                                                                               
MQTR     CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**MQTR**,=A(R13CHAIN)                                          
*                                                                               
         L     RC,=A(COMMWORK)     COMMON STORAGE AREA                          
         LR    R9,RC                                                            
         AHI   R9,4096                                                          
         USING COMMWORK,RC,R9                                                   
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         BRAS  RE,INITIAL          INITIALIZE                                   
*                                                                               
* CONNECT TO "FROM" QUEUE MANAGER                                               
         LA    R2,CSQBCONN_STRUCTURE_FROM                                       
         BRAS  RE,CALLMQ           CONNECT TO MQ QUEUE MANAGER (FROM)           
*                                                                               
* IF COPYING OR MOVING, CONNECT TO THE "TO" QUEUE MANAGER                       
         TM    MODE,MODE_MOVE+MODE_COPY                                         
         BZ    G1M002                                                           
*                                                                               
* SAME QUEUE MANAGER?  USE SAME CONNECTION HANDLE                               
         CLC   QMGRNAME_FROM,QMGRNAME_TO                                        
         BNE   *+14                                                             
         MVC   HCONN_TO,HCONN_FROM                                              
         B     G1M002         DON'T CONNECT TO SAME QUEUE MANAGER TWICE         
*                                                                               
         LA    R2,CSQBCONN_STRUCTURE_TO                                         
         BRAS  RE,CALLMQ           CONNECT TO MQ QUEUE MANAGER (TO)             
*                                                                               
G1M002   DS    0H                                                               
* OPEN THE "FROM" QUEUE                                                         
         MVC   OBJDESC_FROM_OBJECTTYPE,=A(MQOT_Q)                               
         MVC   OBJDESC_FROM_OBJECTQMGRNAME,QMGRNAME_FROM                        
         MVC   OBJDESC_FROM_OBJECTNAME,QNAME_FROM                               
         MVC   OPEN_OPTIONS_FROM,=A(MQOO_INPUT_AS_Q_DEF+MQOO_BROWSE)            
         LA    R2,CSQBOPEN_STRUCTURE_FROM                                       
         BRAS  RE,CALLMQ           OPEN QUEUE (FROM)                            
*                                                                               
* IF COPYING OR MOVING, CONNECT AND OPEN "TO" QUEUE                             
         TM    MODE,MODE_MOVE+MODE_COPY                                         
         BZ    G1M010                                                           
*                                                                               
* OPEN CURRENT "TO" QUEUE                                                       
         MVC   OBJDESC_TO_OBJECTTYPE,=A(MQOT_Q)                                 
         MVC   OBJDESC_TO_OBJECTQMGRNAME,QMGRNAME_TO                            
         MVC   OBJDESC_TO_OBJECTNAME,QNAME_TO                                   
         MVC   OPEN_OPTIONS_TO,=A(MQOO_OUTPUT)                                  
         LA    R2,CSQBOPEN_STRUCTURE_TO                                         
         BRAS  RE,CALLMQ           OPEN QUEUE (TO)                              
*                                                                               
G1M010   DS    0H                                                               
         MVC   MSGDESC_CORRELID,MQCI_NONE                                       
         MVC   MSGDESC_MSGID,MQMI_NONE                                          
         MVC   GETMSGOPTS_MATCHOPTIONS,=AL4(MQMO_NONE)                          
         MVC   GETMSGOPTS_OPTIONS,=A(MQGMO_BROWSE_NEXT)                         
*                                                                               
         OC    QMSGID,QMSGID                                                    
         BZ    *+16                                                             
         MVC   MSGDESC_MSGID,QMSGID                                             
         OC    GETMSGOPTS_MATCHOPTIONS,=AL4(MQMO_MATCH_MSG_ID)                  
*                                                                               
         OC    QCORID,QCORID                                                    
         BZ    *+16                                                             
         MVC   MSGDESC_CORRELID,QCORID                                          
         OC    GETMSGOPTS_MATCHOPTIONS,=AL4(MQMO_MATCH_CORREL_ID)               
*                                                                               
         LA    R2,CSQBGET_STRUCTURE_FROM                                        
         BRAS  RE,CALLMQ                                                        
         CLC   COMPCODE,=A(MQCC_OK)                                             
         BE    G1M020                                                           
*                                                                               
         CLC   REASON,=A(MQRC_NO_MSG_AVAILABLE)                                 
         JE    G1M090              END OF THE QUEUE                             
         DC    H'0'                NO OTHER WARNING IS ACCEPTABLE               
*                                                                               
G1M020   DS    0H                                                               
         AP    CURRMSG,=P'1'                                                    
*                                                                               
         BRAS  RE,FILTER                                                        
         JE    G1M030              PASSED FILTERS, ALL OK                       
         PRNT  SKIPPING_MESSAGE                                                 
         J     G1M010              DIDN'T PASS FILTERS - GET NEXT MSG           
*                                                                               
* PROCESS THE MESSAGE HERE                                                      
G1M030   DS    0H                                                               
*                                                                               
         AP    PROCMSG,=P'1'       NUMBER OF PROCESSED MESSAGES                 
*                                                                               
* PRINT MESSAGE DETAILS                                                         
         MVC   P+45(20),=CL20'MSGDESC_MSGID'                                    
         MVC   P+65(L'MSGDESC_MSGID),MSGDESC_MSGID                              
         PRNT  PROCESSING_MSG                                                   
         MVC   P+45(20),=CL20'MSGDESC_CORRELID'                                 
         MVC   P+65(L'MSGDESC_CORRELID),MSGDESC_CORRELID                        
         PRNT                                                                   
         MVC   P+45(20),=CL20'MSGDESC_PUTDATE'                                  
         MVC   P+65(L'MSGDESC_PUTDATE),MSGDESC_PUTDATE                          
         PRNT                                                                   
         MVC   P+45(20),=CL20'MSGDESC_PUTTIME'                                  
         MVC   P+65(L'MSGDESC_PUTTIME),MSGDESC_PUTTIME                          
         PRNT                                                                   
*                                                                               
* IF MOVE OR PURGE, GET THE MESSAGE OFF THE "FROM" QUEUE                        
*                                                                               
         CLI   WRITE,C'Y'                                                       
         BNE   G1M040              SKIP GET/PUT                                 
*                                                                               
         TM    MODE,MODE_MOVE+MODE_PURGE                                        
         BZ    G1M035                                                           
*                                                                               
         MVC   GETMSGOPTS_OPTIONS,=A(MQGMO_MSG_UNDER_CURSOR)                    
         LA    R2,CSQBGET_STRUCTURE_FROM                                        
         BRAS  RE,CALLMQ                                                        
         CLC   COMPCODE,=A(MQCC_OK)                                             
         JNE   *+2                                                              
*                                                                               
G1M035   DS    0H                                                               
* IF COPYING/MOVING - PUT THE MESSAGE TO THE "TO" QUEUE                         
         TM    MODE,MODE_MOVE+MODE_COPY                                         
         BZ    G1M040                                                           
*                                                                               
         LA    R2,CSQBPUT_STRUCTURE_TO                                          
         BRAS  RE,CALLMQ           PUT THE MESSAGE TO THE MQ QUEUE              
*                                                                               
G1M040   DS    0H                                                               
         B     G1M010              BROWSE NEXT MESSAGE                          
*                                                                               
* END OF QUEUE                                                                  
*                                                                               
G1M090   DS    0H                                                               
         CP    PROCMSG,=P'0'                                                    
         BE    G1M110              DON'T COMMIT IF NOTHING PROCESSED            
*                                                                               
* COMMIT ANY CHANGES                                                            
* IF MOVING/PURGING - COMMIT TO THE "FROM" QUEUE                                
         TM    MODE,MODE_MOVE+MODE_PURGE                                        
         BZ    G1M100                                                           
*                                                                               
         LA    R2,CSQBCOMM_STRUCTURE_FROM                                       
         BRAS  RE,CALLMQ           COMMIT (FROM)                                
*                                                                               
G1M100   DS    0H                                                               
* IF MOVING/COPYING - COMMIT TO THE "TO" QUEUE                                  
         TM    MODE,MODE_MOVE+MODE_COPY                                         
         BZ    G1M110                                                           
*                                                                               
         LA    R2,CSQBCOMM_STRUCTURE_TO                                         
         BRAS  RE,CALLMQ           COMMIT (TO)                                  
*                                                                               
G1M110   DS    0H                                                               
         LA    R2,CSQBCLOS_STRUCTURE_FROM                                       
         BRAS  RE,CALLMQ           CLOSE QUEUE (FROM)                           
*                                                                               
* IF MOVING/COPYING - CLOSE THE "TO" QUEUE                                      
         TM    MODE,MODE_MOVE+MODE_COPY                                         
         BZ    G1M120                                                           
*                                                                               
         LA    R2,CSQBCLOS_STRUCTURE_TO                                         
         BRAS  RE,CALLMQ           CLOSE QUEUE (TO)                             
*                                                                               
G1M120   DS    0H                                                               
         LA    R2,CSQBDISC_STRUCTURE_FROM                                       
         BRAS  RE,CALLMQ           DISCONNECT FROM MQ MANAGER(FROM)             
*                                                                               
         TM    MODE,MODE_MOVE+MODE_COPY                                         
         BZ    G1M140                                                           
         CLC   QMGRNAME_FROM,QMGRNAME_TO                                        
         BE    G1M140                                                           
*                                                                               
         LA    R2,CSQBDISC_STRUCTURE_TO                                         
         BRAS  RE,CALLMQ           DISCONNECT FROM MQ MANAGER(TO)               
*                                                                               
G1M140   DS    0H                                                               
*                                                                               
         PRNT  EXITING,PRINT=ALWAYS                                             
*                                                                               
         XBASE                                                                  
*                                                                               
*                                                                               
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
*                                                                               
*                                                                               
* INITIALIZE                                                                    
*                                                                               
INITIAL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         PRNT  INITIALIZE,PRINT=ALWAYS                                          
*                                                                               
         L     R0,MAXMSGLN                                                      
         AHI   R0,16               FOR START & END EYE CATCHER                  
         GETMAIN RU,LV=(0),LOC=(ANY,ANY),BNDRY=PAGE                             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SAM31                                                                  
         MVC   0(8,R1),=CL8'*MSGSTR*'                                           
         AHI   R1,8                                                             
         ST    R1,AMSGBUFF                                                      
         A     R1,MAXMSGLN                                                      
         AHI   R1,-16                                                           
         MVC   0(8,R1),=CL8'*MSGEND*'                                           
         SAM24                                                                  
*                                                                               
         PRNT  GOT_MSG_BUFFER,PRINT=ALWAYS                                      
*                                                                               
         MVC   GETBUFF,AMSGBUFF                                                 
         MVC   PUTBUFF,AMSGBUFF                                                 
*                                                                               
         BLDL  0,ENTRYPTS            BUILD LIST OF APPC/MVS ENTRY PTS           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                  BAD RETURN FROM BLDL MACRO                 
*                                                                               
         LOAD  DE=CSQBCONN                                                      
         ST    R0,CSQBCONN_STRUCTURE_FROM                                       
         ST    R0,CSQBCONN_STRUCTURE_TO                                         
         LOAD  DE=CSQBOPEN                                                      
         ST    R0,CSQBOPEN_STRUCTURE_FROM                                       
         ST    R0,CSQBOPEN_STRUCTURE_TO                                         
         LOAD  DE=CSQBGET                                                       
         ST    R0,CSQBGET_STRUCTURE_FROM                                        
         LOAD  DE=CSQBPUT                                                       
         ST    R0,CSQBPUT_STRUCTURE_TO                                          
         LOAD  DE=CSQBCOMM                                                      
         ST    R0,CSQBCOMM_STRUCTURE_FROM                                       
         ST    R0,CSQBCOMM_STRUCTURE_TO                                         
         LOAD  DE=CSQBCLOS                                                      
         ST    R0,CSQBCLOS_STRUCTURE_FROM                                       
         ST    R0,CSQBCLOS_STRUCTURE_TO                                         
         LOAD  DE=CSQBDISC                                                      
         ST    R0,CSQBDISC_STRUCTURE_FROM                                       
         ST    R0,CSQBDISC_STRUCTURE_TO                                         
*                                                                               
* READ THE Q MANAGER (MQQMGRNAME=) AND Q NAMES (MQQUEUENAME=).                  
*                                                                               
RC20     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BE    RCX                                                              
*                                                                               
         CLI   CARD,C'*'                                                        
         BE    RC20                                                             
*                                                                               
         CLC   =C'MQQMGRNAME_FROM=',CARD                                        
         BNE   *+14                                                             
         MVC   QMGRNAME_FROM,CARD+16                                            
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQUEUENAME_FROM=',CARD                                       
         BNE   *+14                                                             
         MVC   QNAME_FROM,CARD+17                                               
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQMGRNAME_TO=',CARD                                          
         BNE   *+14                                                             
         MVC   QMGRNAME_TO,CARD+14                                              
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQUEUENAME_TO=',CARD                                         
         BNE   *+14                                                             
         MVC   QNAME_TO,CARD+15                                                 
         B     RC20                                                             
*                                                                               
         CLC   =C'CORRELID=',CARD                                               
         BNE   RC30                                                             
         MVC   QCORID,CARD+9                                                    
         B     RC20                                                             
*                                                                               
RC30     CLC   =C'MSGID=',CARD                                                  
         BNE   RC40                                                             
         MVC   QMSGID,CARD+6                                                    
         B     RC20                                                             
*                                                                               
RC40     CLC   =C'MSG#=',CARD                                                   
         BNE   RC42                                                             
*                                                                               
         GOTO1 =V(NUMVAL),DMCB,CARD+5,(2,0)                                     
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                NOT NUMERIC                                  
         L     R1,DMCB+4                                                        
         CVD   R1,DUB                                                           
         ZAP   MSGNUMS,DUB                                                      
         ZAP   MSGNUMX,DUB                                                      
         B     RC20                                                             
*                                                                               
RC42     CLC   =C'MSG#_START=',CARD                                             
         BNE   RC45                                                             
*                                                                               
         GOTO1 =V(NUMVAL),DMCB,CARD+11,(2,0)                                    
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                NOT NUMERIC                                  
         L     R1,DMCB+4                                                        
         CVD   R1,DUB                                                           
         ZAP   MSGNUMS,DUB                                                      
         B     RC20                                                             
*                                                                               
RC45     CLC   =C'MSG#_END=',CARD                                               
         BNE   RC50                                                             
*                                                                               
         GOTO1 =V(NUMVAL),DMCB,CARD+9,(2,0)                                     
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                NOT NUMERIC                                  
         L     R1,DMCB+4                                                        
         CVD   R1,DUB                                                           
         ZAP   MSGNUMX,DUB                                                      
         B     RC20                                                             
*                                                                               
RC50     CLC   =C'WRITE=',CARD                                                  
         BNE   RC70                                                             
*                                                                               
         MVC   WRITE,CARD+6                                                     
         B     RC20                                                             
*                                                                               
RC70     CLC   =C'DATE=',CARD                                                   
         BNE   RC75                                                             
*                                                                               
         OC    DAYSBACK,DAYSBACK                                                
         JNZ   *+2                                                              
*                                                                               
         LA    R2,CARD+5                                                        
         GOTO1 =V(DATVAL),DMCB,(0,0(R2)),WORK+100                               
         ICM   R3,15,DMCB          DATE STRING LENGTH                           
         JZ    *+2                 INVALID DATE                                 
*                                                                               
         GOTO1 =V(DATCON),DMCB,(0,WORK+100),(20,DATES)                          
*                                                                               
         AR    R2,R3               ADVANCE R2 TO NEXT CHAR AFTER DATE1          
         CLI   0(R2),C'-'          ONLY 1 DATE ENTERED?                         
         JNE   RC72                YES                                          
         LA    R2,1(R2)            ADVANCE R2 TO START OF DATE2                 
*                                                                               
         GOTO1 =V(DATVAL),DMCB,(0,(R2)),WORK+100                                
         OC    DMCB,DMCB                                                        
         JZ    *+2                                                              
*                                                                               
         GOTO1 =V(DATCON),DMCB,(0,WORK+100),(20,DATEX)                          
         B     RC20                                                             
*                                                                               
RC72     MVC   DATEX,DATES                                                      
         B     RC20                                                             
*                                                                               
RC75     DS    0H                                                               
         CLC   =C'TIME=',CARD                                                   
         BNE   RC80                                                             
*                                                                               
         LA    R2,CARD+5                                                        
         MVC   TIMES,0(R2)                                                      
         LA    R2,8(R2)            ADVANCE R2 TO NEXT CHAR AFTER TIME1          
         CLI   0(R2),C'-'          ONLY 1 TIME ENTERED?                         
         JNE   RC76                YES                                          
         LA    R2,1(R2)            ADVANCE R2 TO START OF DATE2                 
         MVC   TIMEX,0(R2)                                                      
         B     RC20                                                             
*                                                                               
RC76     MVC   TIMEX,TIMES                                                      
         B     RC20                                                             
*                                                                               
RC80     CLC   =C'MODE=',CARD                                                   
         BNE   RC90                                                             
*                                                                               
         CLC   =C'COPY',CARD+5                                                  
         BNE   *+12                                                             
         OI    MODE,MODE_COPY                                                   
         B     RC20                                                             
*                                                                               
         CLC   =C'MOVE',CARD+5                                                  
         BNE   *+12                                                             
         OI    MODE,MODE_MOVE                                                   
         B     RC20                                                             
*                                                                               
         CLC   =C'PURGE',CARD+5                                                 
         BNE   *+12                                                             
         OI    MODE,MODE_PURGE                                                  
         B     RC20                                                             
*                                                                               
         DC    H'0'                                                             
*                                                                               
RC90     DS    0H                                                               
         CLC   =C'TRACE=',CARD                                                  
         JNE   RC100                                                            
         MVC   TRACEFLG,CARD+6                                                  
         J     RC20                                                             
*                                                                               
RC100    DS    0H                                                               
         CLC   =C'DAYS_BACK=',CARD                                              
         JNE   *+2                 UNKNOWN CARD                                 
*                                                                               
         CLC   DATES,=C'00000000'                                               
         JNE   *+2                                                              
*                                                                               
         GOTO1 =V(NUMVAL),DMCB,CARD+10,(2,0)                                    
         CLI   DMCB,0                                                           
         JNE   *+2                                                              
         MVC   DAYSBACK,DMCB+4                                                  
         J     RC20                                                             
*                                                                               
RCX      DS    0H                                                               
         OC    DAYSBACK,DAYSBACK                                                
         BZ    RCXX                                                             
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(X'20',WORK)                               
*                                                                               
         L     R0,DAYSBACK                                                      
         LNR   R0,R0                                                            
         GOTO1 =V(ADDAY),DMCB,(C'D',WORK),WORK+6,(R0)                           
         GOTO1 =V(DATCON),DMCB,(0,WORK+6),(20,DATEX)                            
*                                                                               
RCXX     XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
*                                                                               
*                                                                               
CALLMQ   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* UPON ENTRY,  R2 POINTS TO PARAMETER STRUCTURE                                 
* UPON RETURN, COMPCODE CONTAINS THE MQ COMPLETION CODE                         
*              REASON CONTAINS THE MQ REASON CODE                               
*                                                                               
         SAM31                                                                  
         L     RF,0(R2)            RF = A(MQ ROUTINE)                           
         LA    R3,24(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
         SAM24                                                                  
*                                                                               
         MVI   P,C'+'              '+' MEANS IT'S AN MQ CALL                    
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODES          
         MVC   P+30(8),=C'COMP. OK'                                             
         CLC   COMPCODE,=A(MQCC_OK)                                             
         BNE   CALLMQ05                                                         
*                                                                               
         C     R2,=A(CSQBCONN_STRUCTURE_FROM)                                   
         BNE   CALLMQ_A                                                         
         L     RE,24(R2)                                                        
         MVC   P+45(10),0(RE)      QMGR NAME                                    
         B     CALLMQ30                                                         
CALLMQ_A EQU  *                                                                 
*                                                                               
         C     R2,=A(CSQBCONN_STRUCTURE_TO)                                     
         BNE   CALLMQ_B                                                         
         L     RE,24(R2)                                                        
         MVC   P+45(10),0(RE)      QMGR NAME                                    
         B     CALLMQ30                                                         
CALLMQ_B EQU  *                                                                 
*                                                                               
         C     R2,=A(CSQBOPEN_STRUCTURE_FROM)                                   
         BNE   CALLMQ_C                                                         
         L     RE,28(R2)                                                        
         MVC   P+45(48),OBJDESC_FROM_OBJECTNAME-OBJDESC_FROM(RE)                
         B     CALLMQ30                                                         
CALLMQ_C EQU  *                                                                 
*                                                                               
         C     R2,=A(CSQBOPEN_STRUCTURE_TO)                                     
         BNE   CALLMQ_D                                                         
         L     RE,28(R2)                                                        
         MVC   P+45(48),OBJDESC_TO_OBJECTNAME-OBJDESC_TO(RE)                    
         B     CALLMQ30                                                         
CALLMQ_D EQU  *                                                                 
*                                                                               
         C     R2,=A(CSQBGET_STRUCTURE_FROM)                                    
         BNE   CALLMQ_E                                                         
         L     RE,GETBUFF                                                       
         SAM31                                                                  
         MVC   P+45(60),0(RE)                            MSG                    
         SAM24                                                                  
         B     CALLMQ30                                                         
CALLMQ_E EQU  *                                                                 
*                                                                               
         C     R2,=A(CSQBPUT_STRUCTURE_TO)                                      
         BNE   CALLMQ_F                                                         
         L     RE,PUTBUFF                                                       
         SAM31                                                                  
         MVC   P+45(60),0(RE)                            MSG                    
         SAM24                                                                  
         B     CALLMQ30                                                         
*                                                                               
CALLMQ_F EQU  *                                                                 
         B     CALLMQ30                                                         
*                                                                               
CALLMQ05 MVC   P+30(8),=C'WARNING!'                                             
         CLC   COMPCODE,=A(MQCC_WARNING)                                        
         BE    CALLMQ10                                                         
         MVC   P+30(8),=C'*FAILED!'                                             
         CLC   COMPCODE,=A(MQCC_FAILED)                                         
         BE    CALLMQ10                                                         
*                                                                               
         DC    H'0'                UNKNOWN COMPLETION CODE                      
*                                                                               
CALLMQ10 MVC   P+110(9),=C'*** ERROR'                                           
         MVC   P+39(7),=C'REASON='                                              
         EDIT  REASON,(5,P+47),ZERO=NOBLANK                                     
         MVI   P+52,C':'                                                        
*                                                                               
         L     RF,=A(MQ_REASON_CODE_TABLE)                                      
CALLMQ20 CLI   0(RF),X'FF'         END OF TABLE?                                
         BNE   *+14                NO                                           
         MVC   P+54(22),=C'*** UNKNOWN REASON ***'                              
         B     CALLMQ30            REASON CODE NOT IN TABLE                     
*                                                                               
         CLC   REASON,0(RF)        FIND MATCH ON REASON CODE                    
         BE    *+12                GOT IT                                       
         LA    RF,28(RF)           BUMP TO NEXT TABLE ENTRY                     
         B     CALLMQ20                                                         
*                                                                               
         MVC   P+54(24),4(RF)                                                   
*                                                                               
CALLMQ30 PRNT                                                                   
*                                                                               
         CLC   COMPCODE,=A(MQCC_OK)                                             
         BE    CALLMQX                                                          
         CLC   COMPCODE,=A(MQCC_FAILED)                                         
         BNE   *+16                                                             
         CLC   REASON,=A(MQRC_NO_MSG_AVAILABLE)                                 
         BE    CALLMQX                                                          
         DC    H'0'                NO OTHER FAILURE IS ACCEPTABLE               
         CLC   COMPCODE,=A(MQCC_WARNING)                                        
         BNE   *+16                                                             
         CLC   REASON,=A(MQRC_SIGNAL_REQUEST_ACCEPTED)                          
         BE    *+6                                                              
         DC    H'0'                NO OTHER WARNING IS ACCEPTABLE               
*                                                                               
CALLMQX  XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
*                                                                               
*                                                                               
FILTER   NTR1  BASE=*,LABEL=*                                                   
         CLC   DATES,MSGDESC_PUTDATE                                            
         JH    NEQXIT                                                           
         CLC   DATEX,MSGDESC_PUTDATE                                            
         JL    NEQXIT                                                           
*                                                                               
         CLC   TIMES,MSGDESC_PUTTIME                                            
         JH    NEQXIT                                                           
         CLC   TIMEX,MSGDESC_PUTTIME                                            
         JL    NEQXIT                                                           
*                                                                               
         CP    MSGNUMS,=P'0'                                                    
         JE    EQXIT                                                            
         CP    CURRMSG,MSGNUMS                                                  
         JL    NEQXIT                                                           
         CP    CURRMSG,MSGNUMX                                                  
         JH    NEQXIT                                                           
         J     EQXIT                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
*                                                                               
*                                                                               
COMMWORK DS    0D                  COMMON STORAGE AREA                          
*                                                                               
WRITE    DC    C'N'                                                             
*                                                                               
         PRINT GEN                                                              
         CMQA LIST=YES,EQUONLY=NO                                               
         PRINT NOGEN                                                            
*                                                                               
* MQSERIES CALL PARAMETERS                                                      
*                                                                               
QNAME_LENGTHQ  EQU     48                                                       
*                                                                               
QMGRNAME_FROM  DC      CL(QNAME_LENGTHQ)' '                                     
QNAME_FROM     DC      CL(QNAME_LENGTHQ)' '                                     
QMGRNAME_TO    DC      CL(QNAME_LENGTHQ)' '                                     
QNAME_TO       DC      CL(QNAME_LENGTHQ)' '                                     
*                                                                               
QCORID         DS      XL(L'MSGDESC_CORRELID)                                   
QMSGID         DS      XL(L'MSGDESC_MSGID)                                      
*                                                                               
DAYSBACK       DC      F'0'                                                     
*                                                                               
HCONN_FROM     DS      F         MQ QMGR CONNECTION HANDLE                      
HCONN_TO       DS      F         MQ QMGR CONNECTION HANDLE                      
*                                                                               
HOBJ           DS      F         OBJECT HANDLE                                  
HOBJ2          DS      F         OBJECT HANDLE                                  
*                                                                               
OPEN_OPTIONS_FROM DS   F         MQOPEN OPTIONS                                 
OPEN_OPTIONS_TO   DS   F         MQOPEN OPTIONS                                 
*                                                                               
CLOSE_OPTIONS  DS      F         MQCLOSE OPTIONS                                
CLOSE_OPTIONS2 DS      F         MQCLOSE OPTIONS                                
*                                                                               
COMPCODE       DS      F         COMPLETION CODE                                
REASON         DS      F         QUALIFIES COMPLETION CODE                      
DATALENGTH     DS      F         LENGTH OF THE MESSAGE                          
*                                                                               
               DC    CL8'MSGBUFF'                                               
AMSGBUFF       DC    A(0)               A(MESSAGE BUFFER)                       
MAXMSGLN       DC    A(4*1024*1024)     MAX MESSAGE LENGTH(4M)                  
*                                                                               
         PRINT GEN                                                              
OBJDESC_FROM   CMQODA  LIST=YES  OBJECT DESCRIPTOR                              
OBJDESC_TO     CMQODA  LIST=YES  OBJECT DESCRIPTOR                              
*                                                                               
MSGDESC        CMQMDA  LIST=YES  MESSAGE DESCRIPTOR                             
*                                                                               
GETMSGOPTS   CMQGMOA LIST=YES  GET MESSAGE OPTIONS                              
PUTMSGOPTS   CMQPMOA LIST=YES  PUT MESSAGE OPTIONS                              
         PRINT NOGEN                                                            
*                                                                               
DMWORK   DS    12D                                                              
DMCB     DS    10F                                                              
DUB      DS    D                                                                
DUB2     DS    D                                                                
PRNTDUB  DS    D                                                                
CARD     DS    CL80                                                             
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
PRNTTIME DS    CL9                                                              
WORK     DS    CL256                                                            
ELCODE   DS    X                                                                
TRACEFLG DC    C'N'                'Y' TO PRINT DETAILED TRACE                  
ONEMSG   DC    C'N'                ONLY RUN THIS ONCE TO GET ONE MSG            
CONTINUE DC    C'N'                ONLY RUN THIS JOB ONCE                       
XQUEUES  DC    C'N'                MULTIPLE QUEUE SET                           
MODE     DC    X'00'               DEFAULT=BROWSE, NO UPDATES                   
MODE_PURGE EQU  X'01'                                                           
MODE_COPY  EQU  X'02'                                                           
MODE_MOVE  EQU  X'04'                                                           
DATES    DC    CL8'00000000'                                                    
DATEX    DC    CL8'99999999'                                                    
TIMES    DC    CL8'00000000'                                                    
TIMEX    DC    CL8'99999999'                                                    
MSGNUMS  DC    PL8'0'                                                           
MSGNUMX  DC    PL8'0'                                                           
CURRMSG  DC    PL8'0'                                                           
PROCMSG  DC    PL8'0'                                                           
WAITTIME DC    A(5*60*100)         DEFAULT 5 MIN WAIT                           
*                                                                               
*                                                                               
* PARAMETER LISTS FOR MQSERIES CALLS                                            
*  F    A(ROUTINE)                                                              
*  CL16 EBCDIC ROUTINE NAME                                                     
*  XL1  FLAGS                                                                   
*  XL3  SPARE                                                                   
*  PARAMETERS (STANDARD IBM FORMAT)                                             
*                                                                               
CSQBCONN_STRUCTURE_FROM   DS    A                                               
                          DC    CL16'MQ_CONNECT_FR'                             
                          DC    A(0)                                            
                          DC    A(QMGRNAME_FROM)                                
                          DC    A(HCONN_FROM)                                   
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBOPEN_STRUCTURE_FROM   DS    A                                               
                          DC    CL16'MQ_OPEN_FR'                                
                          DC    A(0)                                            
                          DC    A(HCONN_FROM)                                   
                          DC    A(OBJDESC_FROM)                                 
                          DC    A(OPEN_OPTIONS_FROM)                            
                          DC    A(HOBJ)                                         
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBGET_STRUCTURE_FROM    DS    A                                               
                          DC    CL16'MQ_GET_FR'                                 
                          DC    A(0)                                            
                          DC    A(HCONN_FROM)                                   
                          DC    A(HOBJ)                                         
                          DC    A(MSGDESC)                                      
                          DC    A(GETMSGOPTS)                                   
                          DC    A(MAXMSGLN)                                     
GETBUFF                   DC    A(0)                                            
                          DC    A(DATALENGTH)                                   
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCOMM_STRUCTURE_FROM   DS    A                                               
                          DC    CL16'MQ_COMMIT_FR'                              
                          DC    A(0)                                            
                          DC    A(HCONN_FROM)                                   
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCLOS_STRUCTURE_FROM   DS    A                                               
                          DC    CL16'MQ_CLOSE_FR'                               
                          DC    A(0)                                            
                          DC    A(HCONN_FROM)                                   
                          DC    A(HOBJ)                                         
                          DC    A(CLOSE_OPTIONS)                                
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBDISC_STRUCTURE_FROM   DS    A                                               
                          DC    CL16'MQ_DISCONNECT_FR'                          
                          DC    A(0)                                            
                          DC    A(HCONN_FROM)                                   
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCONN_STRUCTURE_TO     DS    A                                               
                          DC    CL16'MQ_CONNECT_TO'                             
                          DC    A(0)                                            
                          DC    A(QMGRNAME_TO)                                  
                          DC    A(HCONN_TO)                                     
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBOPEN_STRUCTURE_TO     DS    A                                               
                          DC    CL16'MQ_OPEN_TO'                                
                          DC    A(0)                                            
                          DC    A(HCONN_TO)                                     
                          DC    A(OBJDESC_TO)                                   
                          DC    A(OPEN_OPTIONS_TO)                              
                          DC    A(HOBJ2)                                        
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBPUT_STRUCTURE_TO      DS    A                                               
                          DC    CL16'MQ_PUT_TO'                                 
                          DC    A(0)                                            
                          DC    A(HCONN_TO)                                     
                          DC    A(HOBJ2)                                        
                          DC    A(MSGDESC)                                      
                          DC    A(PUTMSGOPTS)                                   
                          DC    A(DATALENGTH)                                   
PUTBUFF                   DC    AL4(0)                                          
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCOMM_STRUCTURE_TO     DS    A                                               
                          DC    CL16'MQ_COMMIT_TO'                              
                          DC    A(0)                                            
                          DC    A(HCONN_TO)                                     
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCLOS_STRUCTURE_TO     DS    A                                               
                          DC    CL16'MQ_CLOSE_TO'                               
                          DC    A(0)                                            
                          DC    A(HCONN_TO)                                     
                          DC    A(HOBJ2)                                        
                          DC    A(CLOSE_OPTIONS2)                               
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBDISC_STRUCTURE_TO     DS    A                                               
                          DC    CL16'MQ_DISCONNECT_TO'                          
                          DC    A(0)                                            
                          DC    A(HCONN_TO)                                     
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
ENTRYPTS DS    0F                                                               
         DC    Y((ENTRYLSQ-ENTRYSTQ)/60)   NUMBER OF TABLE ENTRIES              
         DC    H'60'                       MUST REMAIN AS 60                    
ENTRYSTQ DS    0H                                                               
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
CSQBOPEN DC    CL8'CSQBOPEN'                                                    
         DC    XL52'00'                                                         
CSQBPUT  DC    CL8'CSQBPUT'                                                     
         DC    XL52'00'                                                         
ENTRYLSQ DS    0H                                                               
*                                                                               
* END OF COMMWORK COMMON STORAGE AREA                                           
*                                                                               
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
*                                                                               
       ++INCLUDE DDMQREASON                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DDPARSNIPD                                                     
*                                                                               
         DCBD  DSORG=PS,DEVD=DA                                                 
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DDMQPR    12/18/14'                                      
         END                                                                    
