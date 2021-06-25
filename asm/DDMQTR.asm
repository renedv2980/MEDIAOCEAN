*          DATA SET DDMQTR     AT LEVEL 135 AS OF 07/29/10                      
*PHASE MQTRA                                                                    
*INCLUDE CARDS                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'TRANSFER MQ MSG B/W QUEUES IN DIFFERENT QMGR'                   
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
YYUNMQT  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**YMQTR*,=A(R13CHAIN)                                          
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
RESTART  LA    R6,QNAME_1                                                       
         LA    R7,QNAME2_1                                                      
         MVC   QNAME,0(R6)                                                      
         MVC   QNAME2,0(R7)                                                     
*                                                                               
NEXTSET  DS    0H                                                               
*                                                                               
         LA    R2,CSQBCONN_STRUCTURE_FROM                                       
         BRAS  RE,CALLMQ           CONNECT TO MQ QUEUE MANAGER (FROM)           
         CLC   QMGRNAME,QMGRNAME2                                               
         BNE   CONNQ2                                                           
         MVC   HCONN2,HCONN        SAME QMGR, COPY HCONN                        
         B     OPENQ                                                            
CONNQ2   LA    R2,CSQBCONN_STRUCTURE_TO                                         
         BRAS  RE,CALLMQ           CONNECT TO MQ QUEUE MANAGER (TO)             
*                                                                               
OPENQ    DS    0H                                                               
         MVC   OBJDESC_OBJECTTYPE,=A(MQOT_Q)  OBJECT IS A QUEUE                 
         MVC   OBJDESC_OBJECTNAME,QNAME       INPUT QUEUE NAME  (FROM)          
         MVC   OPEN_OPTIONS,=A(MQOO_INPUT_AS_Q_DEF+MQOO_BROWSE)                 
*                                                                               
         MVC   OBJDESC2_OBJECTTYPE,=A(MQOT_Q) OBJECT IS A QUEUE                 
         MVC   OBJDESC2_OBJECTNAME,QNAME2     OUTPUT QUEUE NAME (TO)            
         LA    RF,MQOO_OUTPUT                                                   
         ST    RF,OPEN_OPTIONS2                                                 
*                                                                               
         LA    R2,CSQBOPEN_STRUCTURE_FROM                                       
         BRAS  RE,CALLMQ           OPEN QUEUE (FROM)                            
         LA    R2,CSQBOPEN_STRUCTURE_TO                                         
         BRAS  RE,CALLMQ           OPEN QUEUE (TO)                              
*                                                                               
*                                                                               
         CLI   ONEMSG,C'Y'         ONLY DO ONCE?                                
         BNE   NEXTMSG             NO - GET NEXT MSG                            
         BRAS  RE,GET1MSG          GET ONE SPECIFIC MESSAGE                     
         BNE   GOODBYE2            NO MSG, CLOSE QUEUES W/O COMMIT              
*                                                                               
* +MSGDESC_PUTDATE  DC CL8''       DATE   YYYYMMDD                              
* +MSGDESC_PUTTIME  DC CL8''       TIME   HHMMSSTT                              
*                                                                               
         CLC   DATETIMS,MSGDESC_PUTDATE                                         
         BH    GOODBYE2                                                         
         CLC   DATETIMX,MSGDESC_PUTDATE                                         
         BL    GOODBYE2                                                         
         MVC   P+35(16),MSGDESC_PUTDATE                                         
         PRNT  PUTTIME,PRINT=ALWAYS                                             
*                                                                               
         LA    RF,MQPMO_SYNCPOINT                                               
         ICM   RE,15,=AL4(MQPMO_FAIL_IF_QUIESCING)                              
         AR    RF,RE                                                            
         ST    RF,PUTMSGOPTS_OPTIONS                                            
*                                                                               
         BRAS  RE,FILTER           CHECK FOR ANY MORE FILTERS                   
         BNE   GOODBYE             NOT MATCH, DONE                              
*                                                                               
         LA    R2,CSQBPUT_STRUCTURE_TO                                          
         BRAS  RE,CALLMQ           PUT THE MESSAGE TO THE MQ QUEUE              
         B     GOODBYE             CLOSE QUEUES WITH COMMIT                     
*                                                                               
*                                                                               
NEXTMSG  BRAS  RE,WAIT             WAIT FOR INCOMING MESSAGE                    
         BNE   GOODBYE             SHUTDOWN BY OPERATOR                         
*                                                                               
* +MSGDESC_PUTDATE  DC CL8''       DATE   YYYYMMDD                              
* +MSGDESC_PUTTIME  DC CL8''       TIME   HHMMSSTT                              
*                                                                               
         CLC   DATETIMS,MSGDESC_PUTDATE                                         
         BH    NEXT_CMT                                                         
         CLC   DATETIMX,MSGDESC_PUTDATE                                         
         BL    NEXT_CMT                                                         
         MVC   P+35(16),MSGDESC_PUTDATE                                         
         PRNT  PUTTIME,PRINT=ALWAYS                                             
*                                                                               
         LA    RF,MQPMO_SYNCPOINT                                               
         ICM   RE,15,=AL4(MQPMO_FAIL_IF_QUIESCING)                              
         AR    RF,RE                                                            
         ST    RF,PUTMSGOPTS_OPTIONS                                            
*                                                                               
         BRAS  RE,FILTER           CHECK FOR ANY MORE FILTERS                   
         BNE   NEXT_CMT            NOT MATCH, SKIP THIS                         
*                                                                               
         LA    R2,CSQBPUT_STRUCTURE_TO                                          
         BRAS  RE,CALLMQ           PUT THE MESSAGE TO THE MQ QUEUE              
*                                                                               
NEXT_CMT LA    R2,CSQBCOMM_STRUCTURE_FROM                                       
         BRAS  RE,CALLMQ           COMMIT (FROM)                                
         LA    R2,CSQBCOMM_STRUCTURE_TO                                         
         BRAS  RE,CALLMQ           COMMIT (TO)                                  
         B     NEXTMSG                                                          
*                                                                               
GOODBYE  DS    0H                                                               
         LA    R2,CSQBCOMM_STRUCTURE_FROM                                       
         BRAS  RE,CALLMQ           COMMIT (FROM)                                
         LA    R2,CSQBCOMM_STRUCTURE_TO                                         
         BRAS  RE,CALLMQ           COMMIT (TO)                                  
*                                                                               
GOODBYE2 DS    0H                                                               
         LA    RF,MQCO_NONE                                                     
         ST    RF,CLOSE_OPTIONS                                                 
         ST    RF,CLOSE_OPTIONS2                                                
*                                                                               
         LA    R2,CSQBCLOS_STRUCTURE_FROM                                       
         BRAS  RE,CALLMQ           CLOSE QUEUE (FROM)                           
         LA    R2,CSQBCLOS_STRUCTURE_TO                                         
         BRAS  RE,CALLMQ           CLOSE QUEUE (TO)                             
*                                                                               
         LA    R2,CSQBDISC_STRUCTURE_FROM                                       
         BRAS  RE,CALLMQ           DISCONNECT FROM MQ MANAGER(FROM)             
         CLC   QMGRNAME,QMGRNAME2                                               
         BE    DISCX                                                            
         LA    R2,CSQBDISC_STRUCTURE_TO                                         
         BRAS  RE,CALLMQ           DISCONNECT FROM MQ MANAGER(TO)               
DISCX    DS    0H                                                               
*                                                                               
         CLI   XQUEUES,C'Y'                                                     
         BNE   DONE                                                             
*                                                                               
         AHI   R6,L'QNAME_1        BUMP TO NEXT SET                             
         AHI   R7,L'QNAME2_1                                                    
         C     R6,=A(QNAME_9)      PASS LAST SET                                
         BNH   *+8                 NO -CONTINUE                                 
         B     PAUSE               YES-RESTART FROM 1ST SET OF Q'S              
*                                                                               
         CLI   0(R6),C' '                                                       
         BNH   PAUSE               NULL FROM QNAME RESTART FORM 1ST             
         CLI   0(R7),C' '                                                       
         BNH   PAUSE               NULL TO QNAME RESTART FORM 1ST               
                                                                                
         MVC   QNAME,0(R6)                                                      
         MVC   QNAME2,0(R7)                                                     
         B     NEXTSET             ANOTHER SET OF Q'S                           
*                                                                               
PAUSE    DS    0H                                                               
         CLI   CONTINUE,C'Y'       CONTINUE?                                    
         BNE   DONE                NO -EXIT                                     
*                                  YES-PAUSE AND THEN RESTART                   
         STIMER  WAIT,BINTVL=WAITTIME                                           
         B     RESTART                                                          
*                                                                               
DONE     PRNT  EXITING,PRINT=ALWAYS                                             
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
INITIAL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* INITIALIZE                                                                    
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
         BRAS  RE,SETOPS                                                        
*                                                                               
* READ THE Q MANAGER (MQQMGRNAME=) AND Q NAMES (MQQUEUENAME=).                  
*                                                                               
RC20     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BE    RCX                                                              
*                                                                               
         CLC   =C'MQQMGRNAME_FROM=',CARD                                        
         BNE   *+14                                                             
         MVC   QMGRNAME,CARD+16                                                 
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQUEUENAME_FROM=',CARD                                       
         BNE   *+14                                                             
         MVC   QNAME_1,CARD+17                                                  
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQUEUENAME_FROM2=',CARD                                      
         BNE   *+18                                                             
         MVC   QNAME_2,CARD+18                                                  
         MVI   XQUEUES,C'Y'                                                     
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQUEUENAME_FROM3=',CARD                                      
         BNE   *+18                                                             
         MVC   QNAME_3,CARD+18                                                  
         MVI   XQUEUES,C'Y'                                                     
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQUEUENAME_FROM4=',CARD                                      
         BNE   *+18                                                             
         MVC   QNAME_4,CARD+18                                                  
         MVI   XQUEUES,C'Y'                                                     
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQUEUENAME_FROM5=',CARD                                      
         BNE   *+18                                                             
         MVC   QNAME_5,CARD+18                                                  
         MVI   XQUEUES,C'Y'                                                     
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQUEUENAME_FROM6=',CARD                                      
         BNE   *+18                                                             
         MVC   QNAME_6,CARD+18                                                  
         MVI   XQUEUES,C'Y'                                                     
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQUEUENAME_FROM7=',CARD                                      
         BNE   *+18                                                             
         MVC   QNAME_7,CARD+18                                                  
         MVI   XQUEUES,C'Y'                                                     
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQUEUENAME_FROM8=',CARD                                      
         BNE   *+18                                                             
         MVC   QNAME_8,CARD+18                                                  
         MVI   XQUEUES,C'Y'                                                     
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQUEUENAME_FROM9=',CARD                                      
         BNE   *+18                                                             
         MVC   QNAME_9,CARD+18                                                  
         MVI   XQUEUES,C'Y'                                                     
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQMGRNAME_TO=',CARD                                          
         BNE   *+14                                                             
         MVC   QMGRNAME2,CARD+14                                                
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQUEUENAME_TO=',CARD                                         
         BNE   *+14                                                             
         MVC   QNAME2_1,CARD+15                                                 
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQUEUENAME_TO2=',CARD                                        
         BNE   *+18                                                             
         MVC   QNAME2_2,CARD+16                                                 
         MVI   XQUEUES,C'Y'                                                     
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQUEUENAME_TO3=',CARD                                        
         BNE   *+18                                                             
         MVC   QNAME2_3,CARD+16                                                 
         MVI   XQUEUES,C'Y'                                                     
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQUEUENAME_TO4=',CARD                                        
         BNE   *+18                                                             
         MVC   QNAME2_4,CARD+16                                                 
         MVI   XQUEUES,C'Y'                                                     
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQUEUENAME_TO5=',CARD                                        
         BNE   *+18                                                             
         MVC   QNAME2_5,CARD+16                                                 
         MVI   XQUEUES,C'Y'                                                     
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQUEUENAME_TO6=',CARD                                        
         BNE   *+18                                                             
         MVC   QNAME2_6,CARD+16                                                 
         MVI   XQUEUES,C'Y'                                                     
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQUEUENAME_TO7=',CARD                                        
         BNE   *+18                                                             
         MVC   QNAME2_7,CARD+16                                                 
         MVI   XQUEUES,C'Y'                                                     
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQUEUENAME_TO8=',CARD                                        
         BNE   *+18                                                             
         MVC   QNAME2_8,CARD+16                                                 
         MVI   XQUEUES,C'Y'                                                     
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQUEUENAME_TO9=',CARD                                        
         BNE   *+18                                                             
         MVC   QNAME2_9,CARD+16                                                 
         MVI   XQUEUES,C'Y'                                                     
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
         BNE   RC50                                                             
         GOTO1 =V(NUMVAL),DMCB,CARD+5,(2,0)                                     
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                NOT NUMERIC                                  
         L     R1,DMCB+4                                                        
         ST    R1,MSGNUM                                                        
         MVI   ONEMSG,C'Y'                                                      
         B     RC20                                                             
*                                                                               
RC50     CLC   =C'WAITMINS=',CARD                                               
         BNE   RC60                                                             
         GOTO1 =V(NUMVAL),DMCB,CARD+9,(2,0)                                     
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                NOT NUMERIC                                  
         L     R1,DMCB+4                                                        
         MHI   R1,100*60           IN MINUTES                                   
         ST    R1,WAITTIME                                                      
         B     RC20                                                             
*                                                                               
RC60     CLC   =C'CONTINUE=',CARD  CONTINUE=Y/N                                 
         BNE   RC70                                                             
         MVC   CONTINUE,CARD+9                                                  
         B     RC20                                                             
*                                                                               
* DATETIME FILTER                                                               
* DATETIME IS THE FORMAT OF YYYYMMDDHHMMSSTT EST                                
*                                                                               
RC70     CLC   =C'START_DATETIME=',CARD                                         
         BNE   RC75                                                             
         MVC   DATETIMS,CARD+15                                                 
         B     RC20                                                             
RC75     CLC   =C'END_DATETIME=',CARD                                           
         BNE   RC80                                                             
         MVC   DATETIMX,CARD+13                                                 
         B     RC20                                                             
*                                                                               
RC80     CLC   =C'MODE=',CARD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'COPY',CARD+5                                                  
         BE    *+6                                                              
         DC    H'0'                NO OTHER MODE ACCEPTABLE                     
         MVC   MODE,=CL8'COPY'                                                  
         B     RC20                                                             
*                                                                               
RCX      DS    0H                                                               
RCXX     XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO WAIT UNTIL POSTED OR A MESSAGE ARRIVES                   *         
***********************************************************************         
WAIT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
WAIT020  XC    TIMERECB,TIMERECB   CLEAR TIMER ECB                              
         XC    GETECB,GETECB       CLEAR SIGNAL ECB                             
         MVC   MSGDESC_CORRELID,MQCI_NONE                                       
         MVC   MSGDESC_MSGID,MQMI_NONE                                          
         LHI   RF,MQMO_NONE                                                     
         ST    RF,GETMSGOPTS_MATCHOPTIONS                                       
*                                                                               
*FOR 1) MULTIPLE IN/OUT Q'S, OR 2) CONTINUE=N                                   
*       BRANCH OVER TO GET MSG, SKIP SETTING SIGNAL.                            
         CLI   XQUEUES,C'Y'                                                     
         BE    WAIT025                                                          
         CLI   CONTINUE,C'N'                                                    
         BE    WAIT025                                                          
*                      ONLY SET SIGNAL FOR CONTINOUS AND 1 PAIR Q'S             
         LHI   RF,MQGMO_SET_SIGNAL                                              
         ST    RF,GETMSGOPTS_OPTIONS                                            
         LA    RF,GETECB           TELL IT TO SIGNAL AND SET ECB                
         ST    RF,GETMSGOPTS_SIGNAL1                                            
         LHI   RF,MQWI_UNLIMITED                                                
         ST    RF,GETMSGOPTS_WAITINTERVAL                                       
*                                                                               
WAIT025  OC    QCORID,QCORID                                                    
         BZ    WAIT030                                                          
         MVC   MSGDESC_CORRELID,QCORID                                          
         L     RF,GETMSGOPTS_MATCHOPTIONS                                       
         LHI   RF,MQMO_MATCH_CORREL_ID                                          
         ST    RF,GETMSGOPTS_MATCHOPTIONS                                       
*                                                                               
WAIT030  OC    QMSGID,QMSGID                                                    
         BZ    WAIT040                                                          
         MVC   MSGDESC_MSGID,QMSGID                                             
         L     RF,GETMSGOPTS_MATCHOPTIONS                                       
         LHI   RF,MQMO_MATCH_MSG_ID                                             
         ST    RF,GETMSGOPTS_MATCHOPTIONS                                       
*                                                                               
WAIT040  DS    0H                                                               
         CLC   MODE,=CL8'COPY'                                                  
         BNE   *+10                                                             
         MVC   GETMSGOPTS_OPTIONS,=A(MQGMO_BROWSE_NEXT)                         
*                                                                               
         LA    R2,CSQBGET_STRUCTURE_FROM                                        
         BRAS  RE,CALLMQ           CALL MQ TO FLAG WHEN MESSAGE ARRIVES         
         CLC   COMPCODE,=A(MQCC_OK)                                             
         BE    EXITOK              GOT A MESSAGE                                
         CLC   REASON,=A(MQRC_NO_MSG_AVAILABLE)                                 
         BE    EXITL               GET OUT                                      
         CLC   COMPCODE,=A(MQCC_WARNING)                                        
         BNE   WAIT050                                                          
         CLC   REASON,=A(MQRC_SIGNAL_REQUEST_ACCEPTED)                          
         BE    WAIT050                                                          
         DC    H'0'                NO OTHER WARNING IS ACCEPTABLE               
*                                                                               
WAIT050  BRAS  RE,SETTIMER                                                      
*                                                                               
         WAIT  1,ECBLIST=ECBLST                                                 
*                                                                               
         L     RF,AOPERECB                                                      
         TM    0(RF),X'40'         OPERATOR INTERRUPT                           
         BZ    WAIT060                                                          
         BRAS  RE,CHKOPER                                                       
         CLI   OPERSTOP,C'Y'       OPERATOR STOP REQUESTED?                     
         BE    EXITL               YES                                          
*                                                                               
WAIT060  TM    GETECB,X'40'        MQ SIGNALS MESSAGE ARRIVED                   
         BO    WAIT020             YES - GO GET IT                              
*                                                                               
         TM    TIMERECB,X'40'                                                   
         BO    WAIT020             TIMER POPPED, TRY GET AGAIN                  
         B     WAIT050             GO BACK TO WAIT                              
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ROUTINE TO GET A SPECIFIC MESSAGE                                   *         
***********************************************************************         
GET1MSG  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,MSGNUM                                                        
         MVC   MSGDESC_CORRELID,MQCI_NONE                                       
         MVC   MSGDESC_MSGID,MQMI_NONE                                          
         LHI   RF,MQMO_NONE                                                     
         ST    RF,GETMSGOPTS_MATCHOPTIONS                                       
*                                                                               
G1M010   MVC   GETMSGOPTS_OPTIONS,=A(MQGMO_BROWSE_NEXT)                         
         LA    R2,CSQBGET_STRUCTURE_FROM                                        
         BRAS  RE,CALLMQ                                                        
         CLC   COMPCODE,=A(MQCC_OK)                                             
         BE    G1M020              GOT A MESSAGE                                
         CLC   REASON,=A(MQRC_NO_MSG_AVAILABLE)                                 
         BE    EXITL               END OF THE QUEUE                             
         DC    H'0'                NO OTHER WARNING IS ACCEPTABLE               
*                                                                               
G1M020   DS    0H                                                               
         MVC   MSGDESC_CORRELID,MQCI_NONE                                       
         MVC   MSGDESC_MSGID,MQMI_NONE                                          
         LHI   RF,MQMO_NONE                                                     
         ST    RF,GETMSGOPTS_MATCHOPTIONS                                       
         BCT   R3,G1M010           BROWSE NEXT MESSAGE                          
*                                                                               
         MVC   GETMSGOPTS_OPTIONS,=A(MQGMO_BROWSE_MSG_UNDER_CURSOR)             
*                                                                               
         CLC   MODE,=CL8'COPY'                                                  
         BE    *+10                IF COPY, DO GET, INSTEAD OF BROWSE           
         MVC   GETMSGOPTS_OPTIONS,=A(MQGMO_MSG_UNDER_CURSOR)                    
*                                                                               
         LA    R2,CSQBGET_STRUCTURE_FROM                                        
         BRAS  RE,CALLMQ                                                        
         CLC   COMPCODE,=A(MQCC_OK)                                             
         BE    EXITOK                                                           
         DC    H'0'                HOW CAN I NOT GET THIS MSG!                  
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* SET UP OPERATOR COMMUNICATIONS                                      *         
***********************************************************************         
SETOPS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         EXTRACT ACOMM,FIELDS=COMM                                              
         L     RF,ACOMM            SET UP OPERATOR COMMUNICATIONS               
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
         MVC   ECBLST,AOPERECB     A(ECB)                                       
         L     R2,COMCIBPT         GET A(CIB)                                   
         LA    R3,COMCIBPT         GET A(A(CIB))                                
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTART    WAS EDICT BROUGHT UP WITH 'START'?           
         BNE   NOSTART                                                          
         DROP  R2                                                               
         QEDIT ORIGIN=(R3),BLOCK=(R2)  YES -- FREE THE CIB                      
NOSTART  QEDIT ORIGIN=(R3),CIBCTR=1    NOW ALLOW MODIFIES                       
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* THE OPERATOR HAS INTERRUPTED WITH EITHER A 'STOP' OR 'MODIFY'       *         
* COMMAND.  EXAMINE THE COMMAND AND TAKE THE APPROPRIATE ACTION.      *         
***********************************************************************         
CHKOPER  NTR1  BASE=*,LABEL=*                                                   
         PRNT  CHECK_OPERATOR,PRINT=ALWAYS                                      
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
         MVI   OPERSTOP,C'Y'       YES -- SET STOP FLAG                         
         PRNT  OPERTOR_STOP,PRINT=ALWAYS                                        
         B     CHX                                                              
*                                                                               
CH02     CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER 'MODIFY'?                 
         BE    *+6                 YES                                          
         DC    H'0'                WHAT DID THE OPERATOR DO?                    
*                                                                               
         B     CHX                 NO MODIFY COMMAND YET, JUST EXIT             
*                                                                               
CHX      L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         LA    R3,COMCIBPT         A(A(CIB))                                    
         QEDIT ORIGIN=(R3),BLOCK=(R2)  FREE THE CIB                             
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
***********************************************************************         
*SET TIMER                                                            *         
***********************************************************************         
SETTIMER NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         PRNT  SET_TIMER,PRINT=ALWAYS                                           
*                                                                               
         OC    STIMER1,STIMER1                                                  
         BZ    ST010                                                            
         STIMERM CANCEL,ID=STIMER1                                              
         LTR   RF,RF               CANCEL TIMER                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ST010    STIMERM SET,ID=STIMER1,BINTVL=WAITSECS,EXIT=TIMERXIT                   
         LTR   RF,RF               WAIT 3600 SEC                                
         BZ    *+6                                                              
         DC    H'0'                                                             
STIMERX  XIT1                                                                   
*                                                                               
WAITSECS DC    A(3600*100)         WAIT 3600 SEC (1 HOUR)                       
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
         DROP  RB                                                               
         EJECT                                                                  
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
         MVC   P+45(48),OBJDESC_OBJECTNAME-OBJDESC(RE)   QUEUE NAME             
         B     CALLMQ30                                                         
CALLMQ_C EQU  *                                                                 
*                                                                               
         C     R2,=A(CSQBOPEN_STRUCTURE_TO)                                     
         BNE   CALLMQ_D                                                         
         L     RE,28(R2)                                                        
         MVC   P+45(48),OBJDESC_OBJECTNAME-OBJDESC(RE)   QUEUE NAME             
         B     CALLMQ30                                                         
CALLMQ_D EQU  *                                                                 
*                                                                               
         C     R2,=A(CSQBGET_STRUCTURE_FROM)                                    
         BNE   CALLMQ_E                                                         
         L     RE,GETBUFF                                                       
         SAM31                                                                  
*CHECK THE LEN AND EX                                                           
         MVC   P+45(60),0(RE)                            MSG                    
         SAM24                                                                  
         B     CALLMQ30                                                         
CALLMQ_E EQU  *                                                                 
*                                                                               
         C     R2,=A(CSQBPUT_STRUCTURE_TO)                                      
         BNE   CALLMQ_F                                                         
         L     RE,PUTBUFF                                                       
         SAM31                                                                  
*CHECK THE LEN AND EX                                                           
         MVC   P+45(60),0(RE)                            MSG                    
         SAM24                                                                  
         B     CALLMQ30                                                         
CALLMQ_F EQU  *                                                                 
         B     CALLMQ30                                                         
*                                                                               
CALLMQ05 MVC   P+30(8),=C'WARNING!'                                             
         CLC   COMPCODE,=A(MQCC_WARNING)                                        
         BE    CALLMQ10                                                         
         MVC   P+30(8),=C'*FAILED!'                                             
         CLC   COMPCODE,=A(MQCC_FAILED)                                         
         BE    CALLMQ10                                                         
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
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
FILTER   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* DO CUSTOM FILTERING HERE                                                      
*                                                                               
*        CLC   MSGDESC_MSGID(4),=CL4'MQ1R'    ONLY FOR MQ1 RECEIVER             
*        BE    FILGOOD                                                          
*        B     FILBAD                                                           
*                                                                               
*        CLC   MSGDESC_MSGID+4(18),=CL18'AGYHDR002181900DTPMV'                  
*        BE    FILGOOD                                                          
*        B     FILBAD                                                           
*                                                                               
*                                                                               
FILGOOD  B     EXITOK                                                           
FILBAD   B     EXITL                                                            
*                                                                               
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
COMMWORK DS    0D                  COMMON STORAGE AREA                          
*                                                                               
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
EXIT     XIT1  ,                                                                
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
OPERSTOP DC    C'N'                'Y' = OPERATOR ENTERED 'STOP'                
                                                                                
*                                                                               
         CMQA LIST=YES,EQUONLY=NO                                               
         EJECT                                                                  
* MQSERIES CALL PARAMETERS                                                      
*                                                                               
QMGRNAME       DC      CL48' '                                                  
QMGRNAME2      DC      CL48' '                                                  
QNAME          DC      CL48' '                                                  
QNAME_1        DC      CL48' '                                                  
QNAME_2        DC      CL48' '                                                  
QNAME_3        DC      CL48' '                                                  
QNAME_4        DC      CL48' '                                                  
QNAME_5        DC      CL48' '                                                  
QNAME_6        DC      CL48' '                                                  
QNAME_7        DC      CL48' '                                                  
QNAME_8        DC      CL48' '                                                  
QNAME_9        DC      CL48' '                                                  
QNAME2         DC      CL48' '                                                  
QNAME2_1       DC      CL48' '                                                  
QNAME2_2       DC      CL48' '                                                  
QNAME2_3       DC      CL48' '                                                  
QNAME2_4       DC      CL48' '                                                  
QNAME2_5       DC      CL48' '                                                  
QNAME2_6       DC      CL48' '                                                  
QNAME2_7       DC      CL48' '                                                  
QNAME2_8       DC      CL48' '                                                  
QNAME2_9       DC      CL48' '                                                  
QCORID         DS      XL(L'MSGDESC_CORRELID)                                   
QMSGID         DS      XL(L'MSGDESC_MSGID)                                      
*                                                                               
HCONN          DS      F         MQ QMGR CONNECTION HANDLE                      
HCONN2         DS      F         MQ QMGR CONNECTION HANDLE                      
HOBJ           DS      F         OBJECT HANDLE                                  
HOBJ2          DS      F         OBJECT HANDLE                                  
OPEN_OPTIONS   DS      F         MQOPEN OPTIONS                                 
OPEN_OPTIONS2  DS      F         MQOPEN OPTIONS                                 
CLOSE_OPTIONS  DS      F         MQCLOSE OPTIONS                                
CLOSE_OPTIONS2 DS      F         MQCLOSE OPTIONS                                
COMPCODE       DS      F         COMPLETION CODE                                
REASON         DS      F         QUALIFIES COMPLETION CODE                      
DATALENGTH     DS      F         LENGTH OF THE MESSAGE                          
*                                                                               
               DC    CL8'MSGBUFF'                                               
AMSGBUFF       DC    A(0)               A(MESSAGE BUFFER)                       
MAXMSGLN       DC    A(4*1024*1024)     MAX MESSAGE LENGTH(4M)                  
*                                                                               
OBJDESC        CMQODA  LIST=YES  OBJECT DESCRIPTOR                              
OBJDESC2       CMQODA  LIST=YES  OBJECT DESCRIPTOR                              
MSGDESC        CMQMDA  LIST=YES  MESSAGE DESCRIPTOR                             
GETMSGOPTS   CMQGMOA LIST=YES  GET MESSAGE OPTIONS                              
PUTMSGOPTS   CMQPMOA LIST=YES  PUT MESSAGE OPTIONS                              
         EJECT                                                                  
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
TRACEFLG DC    C'Y'                'Y' TO PRINT DETAILED TRACE                  
ONEMSG   DC    C'N'                ONLY RUN THIS ONCE TO GET ONE MSG            
CONTINUE DC    C'N'                ONLY RUN THIS JOB ONCE                       
XQUEUES  DC    C'N'                MULTIPLE QUEUE SET                           
MODE     DC    CL8' '              MODE                                         
DATETIMS DC    CL16'0000000000000000'   START DATETIME YYYYMMDDHHMMSSTT         
DATETIMX DC    CL16'9999999999999999'   END DATETIME   YYYYMMDDHHMMSSTT         
MSGNUM   DS    F                                                                
WAITTIME DC    A(5*60*100)         DEFAULT 5 MIN WAIT                           
         EJECT                                                                  
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
                          DC    A(QMGRNAME)                                     
                          DC    A(HCONN)                                        
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBOPEN_STRUCTURE_FROM   DS    A                                               
                          DC    CL16'MQ_OPEN_FR'                                
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(OBJDESC)                                      
                          DC    A(OPEN_OPTIONS)                                 
                          DC    A(HOBJ)                                         
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBGET_STRUCTURE_FROM    DS    A                                               
                          DC    CL16'MQ_GET_FR'                                 
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
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
                          DC    A(HCONN)                                        
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCLOS_STRUCTURE_FROM   DS    A                                               
                          DC    CL16'MQ_CLOSE_FR'                               
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(HOBJ)                                         
                          DC    A(CLOSE_OPTIONS)                                
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBDISC_STRUCTURE_FROM   DS    A                                               
                          DC    CL16'MQ_DISCONNECT_FR'                          
                          DC    A(0)                                            
                          DC    A(HCONN)                                        
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
         SPACE 3                                                                
*                                                                               
CSQBCONN_STRUCTURE_TO     DS    A                                               
                          DC    CL16'MQ_CONNECT_TO'                             
                          DC    A(0)                                            
                          DC    A(QMGRNAME2)                                    
                          DC    A(HCONN2)                                       
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBOPEN_STRUCTURE_TO     DS    A                                               
                          DC    CL16'MQ_OPEN_TO'                                
                          DC    A(0)                                            
                          DC    A(HCONN2)                                       
                          DC    A(OBJDESC2)                                     
                          DC    A(OPEN_OPTIONS2)                                
                          DC    A(HOBJ2)                                        
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBPUT_STRUCTURE_TO      DS    A                                               
                          DC    CL16'MQ_PUT_TO'                                 
                          DC    A(0)                                            
                          DC    A(HCONN2)                                       
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
                          DC    A(HCONN2)                                       
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCLOS_STRUCTURE_TO     DS    A                                               
                          DC    CL16'MQ_CLOSE_TO'                               
                          DC    A(0)                                            
                          DC    A(HCONN2)                                       
                          DC    A(HOBJ2)                                        
                          DC    A(CLOSE_OPTIONS2)                               
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBDISC_STRUCTURE_TO     DS    A                                               
                          DC    CL16'MQ_DISCONNECT_TO'                          
                          DC    A(0)                                            
                          DC    A(HCONN2)                                       
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
         SPACE 3                                                                
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
*                                                                               
ENTRYLSQ DS    0H                                                               
         EJECT                                                                  
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
         EJECT                                                                  
       ++INCLUDE DDMQREASON                                                     
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
* IEZCIB                                                                        
* IEZCOM                                                                        
         PRINT OFF                                                              
         DSECT                                                                  
         IEZCIB                                                                 
         IEZCOM                                                                 
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'135DDMQTR    07/29/10'                                      
         END                                                                    
