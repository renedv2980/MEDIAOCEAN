*          DATA SET DDDARERMQ  AT LEVEL 064 AS OF 12/16/20                      
*PHASE DARERMQB                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE LOADER                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE HEXIN                                                                  
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
*  TITLE:        DDDARERCV -- RECEIVE DARE TRANSMISSIONS (APPC/MVS)   *         
*                                                                     *         
*  COMMENTS:     ATTACHED BY DDDARESND                                *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- PARAMETERS FROM DARESND (VIA R1)               *         
*                R6 -- WORK                                           *         
*                R7 -- S P A R E                                      *         
*                R8 -- 2ND COMMON AREA BASE REGISTER                  *         
*                R9 -- 3RD COMMON AREA BASE REGISTER                  *         
*                RA -- CPRINT                                         *         
*                RB -- PROGRAM BASE                                   *         
*                RC -- COMMON STORAGE AREA                            *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'DDDARERCV -- RECEIVE DARE TRANSMISSIONS VIA APPC/MVS'           
                                                                                
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
* DARE MQ RECEIVER                                                              
***********************************************************************         
DARERCV  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,DARERCV,=A(R13CHAIN)                                           
         ENTRY SSB                                                              
         ENTRY UTL                                                              
*                                                                               
         LR    R5,RC                                                            
         L     RC,=A(COMMWORK)     COMMON STORAGE AREA                          
         LR    R8,RC                                                            
         AHI   R8,4096                                                          
         LR    R9,R8                                                            
         AHI   R9,4096                                                          
         USING COMMWORK,RC,R8,R9                                                
*                                                                               
         AHI   R5,-4                                                            
         L     R5,0(R5)            A(R1 PARAMETERS FROM ATTACH)                 
         STCM  R5,8,BYTE           HOB IS SHORT SUBTASK NAME                    
         STCM  R5,7,ATSKTAB+1      A(SUBTASK TABLE)                             
         L     R5,ATSKTAB          THIS WILL CLEAR HOB WHEN IN 31 BIT           
         USING TSKTABD,R5                                                       
FINDNTRY CLI   TSKSNAME,X'FF'                                                   
         JE    *+2                 DIE-PARAMETER TO SUBTASK IS BAD              
*                                                                               
         CLC   BYTE,TSKSNAME                                                    
         BE    *+12                R5 NOW POINTS TO OUR TABLE ENTRY             
         LA    R5,TSKTABLQ(,R5)                                                 
         B     FINDNTRY                                                         
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE(19),=C'DARE RECEIVER FROM '                                
         MVC   TITLE+19(3),TSKNAME                                              
*                                                                               
         ICM   RF,15,VDDSIO                                                     
         ICM   RE,15,TSKDDSIO                                                   
         MVC   0(8,RF),0(RE)                                                    
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(L'SSODSPAC,RF),TSKDSPC                           
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL',                +        
               =C'NCTFILE NGENDIR NGENFIL X',A(IO1),0                           
*                                                                               
         GOTO1 =A(INITIAL),(RC)                                                 
*                                                                               
         LA    R1,TSKECBSR                                                      
         STCM  R1,7,ASTOPECB+1     SET A(STOPPING ECB) IN APPC ECBLIST          
         STCM  R1,7,ASTPECB2+1     SET A(STOPPING ECB) IN MQ ECBLIST            
*                                                                               
         CLI   TSKATRCR,C'N'                                                    
         BNE   CONNECT                                                          
         PRNT  WaitToStop                                                       
         WAIT  1,ECBLIST=ASTOPECB  WAIT ONLY FOR STOP FOR STOP                  
         B     GOODBYE                                                          
*                                                                               
CONNECT  EQU   *                                                                
*                                                                               
         L     RF,TSKMQMGR         MQ SERIES QUEUE MANAGER NAME                 
         MVC   QMGRNAME,0(RF)                                                   
*                                                                               
         MVC   BUFFERLENGTH,=A(DAREBUFL)   FOR MQGET CALLS                      
*                                                                               
         LA    R2,CSQBCONN_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     CONNECT TO MQ QUEUE MANAGER                  
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         JNE   *+2                                                              
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
OPNMQLGX DS    0H                                                               
*                                                                               
NEXTMSG  XC    BYTECNTR,BYTECNTR   CLEAR MESSAGE BYTE COUNTER                   
*                                                                               
GETMQBUF GOTO1 =A(MQBUFF),(RC)     FILL UP AN INPUT BUFFER VIA MQ               
         BNE   BADRCV              BAD RETURN CODE FROM LU62 SUBROUTINE         
*                                                                               
GOTBUF   CLI   OPERSTOP,C'Y'       DO WE STOP NOW?                              
         BE    CLOSCOMM            YES -- CLOSE COMMUNICATIONS                  
*                                                                               
         XC    USERID,USERID                                                    
         MVC   SENDER,SPACES                                                    
         MVC   RECEIVER,SPACES                                                  
         MVC   RCVPWRCD,SPACES                                                  
         MVC   ORDNUM,SPACES                                                    
         MVC   ROUTCODE,SPACES                                                  
         MVC   CONTRACT,SPACES                                                  
         MVC   RETURN,SPACES                                                    
         MVC   ORIGDATE,SPACES                                                  
         MVC   ORIGTIME,SPACES                                                  
         MVC   OFFERID,SPACES                                                   
         MVC   IDSEQNUM,SPACES                                                  
         MVC   ERRCODE,=C'000'     ASSUME NO ERRNOT NEEDED                      
         MVI   PUTMSGFL,C'N'       NO MESSAGE PUT TO WORKQ YET                  
*                                                                               
         SAM31                                                                  
         L     R3,ADAREBUF                                                      
         CLC   =C'+++DARE SEND ',0(R3)                                          
         BNE   SEVERE              SEVERE PROTOCOL ERROR                        
*                                                                               
         AHI   R3,APPCM3Q                                                       
         LA    R0,256-APPCM3Q      SCAN FORWARD 256 BYTES MAX                   
GOTBUF10 CLC   0(L'CRLF,R3),CRLF                                                
         BE    GOTBUF20                                                         
         AHI   R3,1                                                             
         BCT   R0,GOTBUF10                                                      
         BNE   SEVERE              SEVERE PROTOCOL/FORMAT ERROR                 
GOTBUF20 AHI   R3,L'CRLF                                                        
*                                                                               
         XC    LOGDATA,LOGDATA                                                  
         CLC   APPCM5(16),0(R3)    +++DARE LOGGING=                             
         BNE   CHKKPALV            NO 'DARE LOGGING' RECORD                     
*                                                                               
         MVC   LOGDATA,16(R3)      SAVE THE LOG DATA (EDICT FILE D/A)           
         CLI   LOGDATA,C'1'                                                     
         BNE   *+8                                                              
         MVI   LOGDATA+9,C' '      VERSION ONE DIDN'T HAVE FILE LETTER          
         LA    R3,1(R3)            BUMP PAST LOGGING INFO                       
         CLC   CRLF,0(R3)                                                       
         BNE   *-10                                                             
         LA    R3,2(R3)            BUMP PAST CRLF                               
         B     CHKMSG                                                           
*                                                                               
CHKKPALV CLC   APPCM6,0(R3)        +++DARE KEEPALIVE                            
         BNE   CHKMSG              NOT A 'KEEPALIVE' MESSAGE                    
*                                                                               
         LA    R3,APPCM6Q(,R3)     BUMP PAST KEEPALIVE RECORD                   
         CLC   CRLF,0(R3)                                                       
         BNE   SEVERE              SEVERE PROTOCOL ERROR                        
         LA    R3,2(R3)            BUMP PAST CRLF                               
         B     CONFIRM                                                          
*                                                                               
CHKMSG   EQU   *                                                                
         LA    R1,CKTRNTBL                                                      
CHKMSG10 CLI   0(R1),X'FF'         EOT?                                         
         BE    UNKNOWN             YES, UNKNOWN MESSAGE TYPE                    
         CLC   0(6,R1),0(R3)       NO, DO WE HAVE A MATCH ON MESSAGE?           
         BE    CHKMSG50                YES, GO RUN ITS CODE                     
         LA    R1,CKTRNTB2-CKTRNTBL(R1)                                         
         B     CHKMSG10                                                         
*                                                                               
UNKNOWN  MVC   ERRCODE,=C'003'     HEADER MISSING                               
         B     CONFIRM             CHECK FOR CONFIRMATION                       
*                                                                               
CHKMSG50 L     RF,6(R1)                                                         
         GOTO1 (RF),(RC)                                                        
         BNE   BADRCV              COMMS FAILURE DURING MESSAGE RECEIVE         
         B     CONFIRM             CHECK FOR CONFIRMATION                       
*                                                                               
CKTRNTBL DC    C'AGYHDR',AL4(AGYHDR)                                            
CKTRNTB2 DC    C'VARHDR',AL4(AGYHDR)                                            
         DC    C'DLNNOT',AL4(DLNNOT)                                            
         DC    C'ERRNOT',AL4(ERRNOT)                                            
         DC    C'ORDAPP',AL4(ORDAPP)                                            
         DC    C'ORDREJ',AL4(ORDREJ)                                            
         DC    C'ORDCFM',AL4(ORDCFM)                                            
         DC    C'ORDRCL',AL4(ORDRCL)                                            
         DC    C'AGYRCL',AL4(AGYRCL)                                            
         DC    C'AGYCAN',AL4(AGYCAN)                                            
         DC    C'MKGHDR',AL4(MKGHDR)                                            
         DC    C'MKGAPP',AL4(MKGAPP)                                            
         DC    C'MKGREJ',AL4(MKGREJ)                                            
         DC    C'MKGROK',AL4(MKGROK)                                            
         DC    C'MKGCAN',AL4(MKGCAN)                                            
         DC    X'FF'                                                            
*                                                                               
SEVERE   SAM24                                                                  
         MVC   OPMSG6+63(3),TSKNAME                                             
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMSG6,OPMSG6)                     
*                                  EMAIL AHYD,WHOA,HWON                         
         GOTO1 DATAMGR,DMCB,=C'OPMSG',=C'AUTONOTE*AHYD,WHOA,HWON:DARE O+        
               NLINE SEVERE COMMUNICATIONS PROTOCOL ERROR!'                     
*                                                                               
CONFIRM  EQU   *                                                                
COMMIT   LA    R2,CSQBCOMM_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     COMMIT BOTH INPUTQ AND WORKQ                 
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         JNE   *+2                                                              
*                                                                               
         CLI   PUTMSGFL,C'Y'       IS MESSAGE PUT TO WORKQ?                     
         BNE   *+8                 NO                                           
         BRAS  RE,CLOWRKQ          YES - CLOSE THE WORKQ                        
*                                                                               
         L     R1,ADAREBUF         R1 = A(MESSAGE BUFFER)                       
         L     R2,BUFFERLENGTH2    R2 = MESSAGE LENGTH                          
         BRAS  RE,LOGMSGQ          LOG THIS MQ MESSAGE                          
*                                                                               
CHKERR   CLC   ERRCODE,=C'000'     BUILD AN ERRNOT?                             
         BE    CHKSTOP                                                          
         GOTO1 =A(BUILDERR),(RC)   YES                                          
         EJECT                                                                  
CHKSTOP  CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    CLOSCOMM            YES -- CLOSE COMMUNICATIONS                  
         B     NEXTMSG                                                          
*                                                                               
BADRCV   PRNT  BadReceive                                                       
         SAM24                                                                  
         MVC   OPMSG2+27(3),TSKNAME                                             
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMSG2,OPMSG2)                     
*                                                                               
CLOSCOMM EQU   *                                                                
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
GOODBYE  EQU   *                                                                
*                                                                               
* CLOSE REP FILES OPENED FOR XML ORDER TRAIL OUT                                
*                                                                               
         PRNT  CloseREPFiles                                                    
         MVI   OCFLAG,C'C'                                                      
         BRAS  RE,OPCLREPF                                                      
*                                                                               
         PRNT  *****************                                                
         PRNT  *****Exiting*****                                                
         PRNT  *****************                                                
*                                                                               
         XBASE                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE                                                                    
***********************************************************************         
INITIAL  NMOD1 0,INITIAL                                                        
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         L     RF,=V(PRNTER)       A(SYSPRINT DCB)                              
         USING IHADCB,RF                                                        
         MVC   DCBDDNAM(3),TSKNAME                                              
         MVC   DCBDDNAM+3(5),=C'RCVR '                                          
         MVC   TXTDD+6(8),DCBDDNAM                                              
         DROP  RF                                                               
         LA    R1,ARBLK                                                         
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         JZ    *+14                                                             
         CLC   =X'0410',RBLK+4                                                  
         JNE   *+2                 APPEND TO PREVIOUS LOG                       
*                                                                               
         PRNT  *****************                                                
         PRNT  ***Initialize****                                                
         PRNT  *****************                                                
*                                                                               
         L     R2,PSATOLD-PSA(,0)  TCB ADDRESS                                  
         GOTO1 =V(HEXOUT),DMCB,(R2),P+30,4,=C'TOG'                              
         CLC   =F'8',DMCB+16                                                    
         JNE   *+2                                                              
         PRNT  TCBAddress                                                       
*                                                                               
         BLDL  0,ENTRYPTS          BUILD LIST OF EXTERNAL ENTRY PTS             
         LTR   RF,RF                                                            
         JNZ   *+2                 BAD RETURN FROM BLDL MACRO                   
*                                                                               
         CLI   TSKMETH,METHMQ      MQ SERIES?                                   
         JNE   *+2                 IF IT AIN'T APPC OR MQ, WE'RE DEAD           
*                                                                               
         LOAD  DE=CSQBCONN                                                      
         ST    R0,CSQBCONN_STRUCTURE                                            
         LOAD  DE=CSQBOPEN                                                      
         ST    R0,CSQBOPEN_STRUCTURE                                            
         ST    R0,CSQBOPEN_COA_STRUCTURE                                        
         ST    R0,CSQBOPEN_LOG_STRUCTURE                                        
         ST    R0,CSQBOPEN_LOG2_STRUCTURE                                       
         ST    R0,CSQBOPEN_LOGB_STRUCTURE                                       
         ST    R0,CSQBOPEN_WORK_STRUCTURE                                       
         LOAD  DE=CSQBGET                                                       
         ST    R0,CSQBGET_STRUCTURE                                             
         ST    R0,CSQBGET_COA_STRUCTURE                                         
         LOAD  DE=CSQBBACK                                                      
         ST    R0,CSQBBACK_STRUCTURE                                            
         LOAD  DE=CSQBPUT                                                       
         ST    R0,CSQBPUT_LOG_STRUCTURE                                         
         ST    R0,CSQBPUT_LOG2_STRUCTURE                                        
         ST    R0,CSQBPUT_LOGB_STRUCTURE                                        
         ST    R0,CSQBPUT_WORK_STRUCTURE                                        
         LOAD  DE=CSQBCOMM                                                      
         ST    R0,CSQBCOMM_STRUCTURE                                            
         LOAD  DE=CSQBCLOS                                                      
         ST    R0,CSQBCLOS_STRUCTURE                                            
         ST    R0,CSQBCLOS_COA_STRUCTURE                                        
         ST    R0,CSQBCLOS_LOG_STRUCTURE                                        
         ST    R0,CSQBCLOS_LOG2_STRUCTURE                                       
         ST    R0,CSQBCLOS_LOGB_STRUCTURE                                       
         ST    R0,CSQBCLOS_WORK_STRUCTURE                                       
         LOAD  DE=CSQBDISC                                                      
         ST    R0,CSQBDISC_STRUCTURE                                            
*                                                                               
         SAM31                                                                  
         L     R0,=A(DAREBUFL)     MQBUFF AND DAREBUF ARE SEPARATE              
         GETMAIN RU,LV=(0),LOC=(ANY,ANY),BNDRY=PAGE    4MB AREAS IN XA          
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
INIT15   ST    R1,AMQBUFXA                                                      
         ST    R1,A_GET_BUFF                                                    
*                                                                               
         L     R0,=A(DAREBUFL)                                                  
         GETMAIN RU,LV=(0),LOC=(ANY,ANY),BNDRY=PAGE                             
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,ADAREBUF                                                      
         SAM24                                                                  
*                                                                               
         MVI   LOGFLAG,C'Y'                                                     
         L     RE,TSKLOGQ                                                       
         CLC   0(L'TSKLOGQ,RE),SPACES                                           
         BH    INIT20                                                           
         MVI   LOGFLAG,C'N'                                                     
         B     INIT25              DON'T LOG Q2 IF Q1 IS NOT THREE              
*                                                                               
INIT20   MVI   LOGFLAG2,C'Y'                                                    
         L     RE,TSKLOGQ2                                                      
         CLC   0(L'TSKLOGQ2,RE),SPACES                                          
         BH    INIT25                                                           
         MVI   LOGFLAG2,C'N'                                                    
*                                                                               
INIT25   MVI   LOGFLAGB,C'Y'                                                    
         L     RE,TSKLOGQB         CHECK FOR BLOCKCHAIN QUEUE                   
         CLC   0(L'TSKLOGQB,RE),SPACES                                          
         BH    INIT30                                                           
         MVI   LOGFLAGB,C'N'                                                    
*                                                                               
INIT30   EQU   *          OPEN REP FILES TO SUPPORT XML ORDER TRAIL OUT         
         PRNT  OpenREPFiles                                                     
         MVI   OCFLAG,C'O'                                                      
         BRAS  RE,OPCLREPF                                                      
*                                                                               
          MVC   WORK(8),=CL8'T00A3E'                                            
*****     CLI   TSKDSPC,C'T'                  IF TST?                           
*****     BE    *+8                                                             
*****     CLI   TSKDSPC,C'Q'                  OR FQA?                           
*****     BNE   *+10                                                            
*****     MVC   WORK(8),=CL8'T00A3EC'         YES, LOAD C VERSION OF            
          GOTO1 =V(LOADER),DMCB,WORK,0,0      GETDARE         -HWON             
          ICM   RF,15,4(R1)                                                     
          JZ    *+2                                                             
          ST    RF,AGETDARE                                                     
*                                                                               
INITX    XIT1                                                                   
         LTORG                                                                  
                                                                                
***********************************************************************         
* PROCESS AN ORDER.  LOOK UP THE SENDING ID IN TABLE, SO WE KNOW TO             
* WHICH MQ WORKQ TO WRITE THE REPORT. ALSO, DEDUCE THE RECEIVER AND             
* PUT HIS ID IN THE APPROPRIATE SLOT.                                           
*                                                                               
* UPON ENTRY, R3 = A(FIRST RECORD)                                              
***********************************************************************         
AGYHDR   NMOD1 0,AGYHDR                                                         
         SAM31                                                                  
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         MVC   ARECTAB,=A(ORDRECS) VALID RECORD TYPES FOR AN ORDER              
*                                                                               
         USING PAGYHDRD,R3                                                      
         MVC   ORDNUM,PAHDORDR                                                  
         MVC   ROUTCODE,PAHDROUT                                                
         MVC   CONTRACT,PAHDRPCN                                                
         MVC   RETURN,PAHDRTRN                                                  
         MVC   ORIGDATE,PAHDDATE                                                
         MVC   ORIGTIME,PAHDTIME                                                
         MVC   RECEIVER,PAHDTOID                                                
         MVC   SENDER,PAHDFRID     DDS USERID                                   
******                                                                          
         MVC   FRSTEDDT,PAHDFSNT   SAVE OFF FIRST EDI SEND DATE                 
         CLC   PAHDFSNT,SPACES     DO WE HAVE THIS?                             
         BH    *+10                YES                                          
         MVC   FRSTEDDT,PAHDDATE   OTHERWISE SET IT TO TODAY                    
*                                                                               
***********************************************************************         
**TEMP CODE TO REDIRECT HAWORTH ORDER BACK TO DDS-REP, YYUN, 12/11/03**         
***********************************************************************         
         MVI   RCVIDFLG,C'N'       ASSUME RECEIVER ID IS NOT NULL               
         CLC   RECEIVER,SPACES                                                  
         BH    *+8                                                              
         MVI   RCVIDFLG,C'Y'       RECEIVER ID IS NULL                          
***********************************************************************         
***********************************************************************         
*                                                                               
         GOTO1 =A(CHKSNDR),(RC)    ADD "-DS" FOR SPECIAL DDS REPS               
         MVC   PAHDFRID,SENDER     OVERRIDE THE SENDER                          
*                                                                               
         GOTO1 =A(GETUID),(RC)     LOOK UP NUMERIC                              
         OC    USERID,USERID       VALID USERID?                                
         BZ    AGHDE007            NO, ERROR UNKNOWN AGENCY                     
*                                                                               
         MVC   MEDIA,PAHDQMED      MEDIA                                        
         MVC   STATION,PAHDQSTA    STATION                                      
*                                                                               
         GOTO1 =A(FINDSTN),(RC)    RETURNS CALL LETTERS IN 'STATION'            
         CLC   STATION,SPACES      WAS STATION FOUND IN TABLE?                  
         BNH   AGHDE006            NO, ERROR UNKNOWN STATION                    
*                                                                               
         MVC   PAHDQSTA(5),STATION PUT CORRECT CALL LETTERS INTO AGYHDR         
         CLC   OSTATION,SPACES     WAS STATION FOUND IN TABLE?                  
         BE    *+10                YES                                          
         MVC   PAHDOLDS(5),OSTATION PUT OLD CALL LETTERS INTO AGYHDR            
*                                                                               
***************                                                                 
* SPECIAL CODE -MO / -DS RECEIVING ID                                           
***************                                                                 
         CLC   PAHDTOID+5(3),=CL3'-MO'  SPECIAL -MO RECEIVING ID?               
         BE    *+14                        OR                                   
         CLC   PAHDTOID+5(3),=CL3'-DS'          -DS RECEIVING ID?               
         BNE   AGYHDR10            NO, FIND RECEIVER ID                         
         MVC   RECEIVER,PAHDTOID                                                
         B     AGYHDR70            NO NEED TO FIND RECEIVER ID                  
***************                                                                 
* SPECIAL CODE FOR MEDIAVEST (KIM LEFEMINE)                                     
***************                                                                 
AGYHDR10 CLC   PAHDTOID(4),=C'EJOR'   TESTING FOR EJOR (VSG)                    
         BNE   AGYHDR20                                                         
         CLC   PAHDTOID+4(3),=C'-VS'  KEEPING IT THAT WAY?                      
         BNE   AGYHDR20                                                         
         MVC   RECEIVER,PAHDTOID    YES, SPECIAL CODE FOR VSG                   
         MVC   RECEIVER+4(3),SPACES  NOW GET RID OF THE  -VS                    
         B     AGYHDR70             STAY WITH VSG FOREVER                       
*                                                                               
AGYHDR20 CLC   PAHDTOID(5),=C'SB3VS'  ALREADY FILLED IN WITH VSG                
         BNE   AGYHDR25                                                         
         CLC   PAHDTOID+7(3),=C'-VS'  KEEPING IT THAT WAY?                      
         BNE   AGYHDR25                                                         
         MVC   RECEIVER,PAHDTOID    YES, SPECIAL CODE FOR VSG                   
         MVC   RECEIVER+7(3),SPACES  NOW GET RID OF THE  -VS                    
         B     AGYHDR70             STAY WITH VSG FOREVER                       
*                                                                               
AGYHDR25 CLI   PAHDVERS,PAHDV9     -ONLY- DATATECH FOR MEDIAVEST                
         BNE   AGYHDR30            NO, FOLLOW OLD ROUTE DETERMINATION           
*&&DO                                                                           
*****                                                                           
* WE DO NOT NEED THE HOME MARKET CODE AS SB3VSNY IS THE ONLY OFFICE             
*   TO RECEIVE AND WCBS IS THE ONLY VSG STATION THAT HAS NY AS HOME MKT         
*   SB3VSNY IS SUPPOSE TO RECEIVE THE WCBS ORDER IS VSG ESTIMATE                
*****                                                                           
         TM    FNRCVRFL,FNRCVRHM   X'01' - HOME MKT RECEIVING ID USED?          
         BNZ   AGYHDR30            YES, CAN'T ALWAYS GO TO SB3VS                
*&&                                                                             
         MVC   RECEIVER(5),=C'SB3VS'                                            
         MVC   RECEIVER+5(2),ROUTCODE+3    PUT THE OFFICE IN                    
         MVC   RECEIVER+7(3),SPACES  NOW GET RID OF ANY NULLS                   
         B     AGYHDR65                                                         
*                                                                               
AGYHDR30 CLI   PAHDVERS,PAHDV6     C'6' - SEND DIRECT TO 2ND REP?               
         BNE   AGYHDR40            NO, FOLLOW OLD ROUTE DETERMINATION           
         CLC   PAHDTOID,SPACES     RECEIVER IS FILLED IN?                       
         BE    AGHDE015            NO, ERROR INCORRECT REP                      
         BRAS  RE,GET2NDRP         VALIDATE 2ND REP                             
         CLC   ERRCODE,=C'000'     ERROR CODE SET?                              
         BNE   AGYHDROK            YES, EXIT AND SEND ERROR                     
         B     AGYHDR65                                                         
*                                                                               
AGYHDR40 CLI   PAHDVERS,PAHDV1     C'1' SBTK? ONLY WAY THIS IS SET              
         BNE   AGYHDR50            NO, FOLLOW OLD ROUTE DETERMINATION           
         CLC   PAHDTOID,SPACES     WAS THE RECEIVING ID FILLED IN?              
         BH    AGYHDR70            YES, THEN WE'RE GOING TO USE IT              
*                                                                               
AGYHDR50 GOTO1 =A(FINDRCVR),(RC)   RETURNS RECEIVING ID IN 'RECEIVER'           
         BNE   AGYHDROK            SEND ERROR                                   
*                                                                               
         CLI   PAHDVERS,PAHDV0     C'0' DEFAULT DARE ROUTING?                   
         BE    AGYHDR60             YES                                         
         CLI   PAHDVERS,PAHDV1     C'1' RECEIVING ID FILLED IN                  
         BNE   AGYHDR65             NO                                          
AGYHDR60 GOTO1 =A(CHKMOTAB),(RC)   SEE IF WE HAVE TO MODIFY WITH -MO            
         GOTO1 =A(CHKWOTAB),(RC)   SEE IF WE HAVE TO MODIFY WITH -WO            
*                                                                               
AGYHDR65 MVC   PAHDTOID,RECEIVER                                                
*                                                                               
AGYHDR70 CLI   MEDIA,C'T'          IF THIS IS A TV ORDER                        
         BNE   AGYHDR75                                                         
         MVC   PAHDFSNT,SPACES     ONLY DARE DISPATCHER CARES                   
         MVC   PAHDFSTA,SPACES                                                  
*                                                                               
AGYHDR75 GOTO1 =A(CHKRCVR),(RC)    MASSAGE RECEIVER IF NECESSARY                
*                                                                               
         BRAS  RE,FINDTSK          RETURNS SUBTASK ID IN 'SNDTSKID'             
         OC    SNDTSKID,SNDTSKID   COULD BE FROM HOME MKT W/O RCVNG ID          
         BZ    AGHDE005            NONE, ERROR UNKNOWN DESTINATION ID           
*                                                                               
         CLC   PAHDTOID,SPACES     DID AGENCY FILL IN RECEIVING ID?             
         BNH   AGYHDR80            NO, USE THE REP WE FOUND                     
         CLC   CONTRACT,=8X'FF'    YES, REP WE FOUND DOESN'T MATCH?             
         BNE   AGYHDR80                 NO, REP WE FOUND IS OKAY                
*                                                                               
         MVC   CONTRACT,PAHDRPCN   YES, LEAVE CONTRACT NUMBER ALONE             
         CLC   =C'MQ1',SNDTSKID    IF SENDING TO A MQ? HOPE ITS MO!?!           
         BE    AGYHDR80                                                         
         CLC   =C'EDR',SNDTSKID    IF SENDING TO A REPPAK?                      
         BE    AGYHDR80                                                         
         MVC   CONTRACT,=8C'0'     NO, ZERO OUT                                 
*                                                                               
AGYHDR80 MVC   PAHDTOID,RECEIVER   PUT RECEIVING ID                             
         CLC   =C'EDR',SNDTSKID    SENDING TO A DDS REP?                        
         BE    *+10                                                             
         MVC   PAHDRPCN,CONTRACT     AND CONTRACT NUMBER INTO AGYHDR            
*                                                                               
         BRAS  RE,CHKSERR                                                       
         BNE   AGYHDROK                                                         
*                                                                               
         LHI   R2,PAGYHDRL         L'RECORD                                     
         GOTO1 =A(PUTSMF),(RC)                                                  
*                                                                               
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE MQ WORKQ                
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'AGYHDR'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
AGYHDROK CR    RB,RB               SET CC EQUAL                                 
AGYHDRX  XIT1                                                                   
*                                                                               
AGHDE005 MVC   ERRCODE,=C'005'     UNKNOWN DESTINATION ID                       
         B     AGYHDROK                                                         
*                                                                               
AGHDE006 MVC   ERRCODE,=C'006'     UNKNOWN STATION                              
         B     AGYHDROK                                                         
*                                                                               
AGHDE007 MVC   ERRCODE,=C'007'     UNKNOWN AGENCY                               
         B     AGYHDROK                                                         
*                                                                               
AGHDE015 MVC   ERRCODE,=C'015'     INCORRECT REP                                
         B     AGYHDROK                                                         
*                                                                               
AGHDE017 MVC   ERRCODE,=C'017'     DSTA RECORD ISSUE                            
         B     AGYHDROK                                                         
*                                                                               
AGHDE018 MVC   ERRCODE,=C'018'     NO 2ND REP IN DSTA                           
         B     AGYHDROK                                                         
*                                                                               
AGHDE019 MVC   ERRCODE,=C'019'     DOES NOT MATCH 2ND REP                       
         B     AGYHDROK                                                         
*                                                                               
AGHDE020 MVC   ERRCODE,=C'020'     ISSUE COMING BACK FROM GETDARE               
         B     AGYHDROK                                                         
*                                                                               
* MUST READ DSTA RECORD BECAUSE TSKSTAT TABLE                                   
* NOT HAVE 2ND REP DATA                                                         
*                                                                               
GET2NDRP NTR1  LABEL=*                                                          
         LAY   R4,KEY                                                           
         USING STAKEYD,R4                                                       
         XC    STAKEY,STAKEY                                                    
         MVI   STAKSYS,STAKSYSQ    SYSTEM                                       
         MVI   STAKTYP,STAKTYPQ    RECORD TYPE                                  
         MVC   STAKMEDA,MEDIA                                                   
         MVC   STAKSTIN,STATION                                                 
*                                                                               
         CLC   PAHDFSNT,SPACES     ANY EDI FIRST SENT DATE?                     
         BNH   G2R010              NONE, USE MOST RECENT FOR THIS DSTA          
*                                                                               
*                                  PAHDFSNT IN HICORE SO DATCON WON'T           
         MVC   WORK(L'PAHDFSNT),PAHDFSNT        WORK SO WELL                    
         GOTO1 =V(DATCON),DMCB,(0,WORK),(3,DUB)  AS DONE IN FR020               
         XR    RE,RE                                                            
         ICM   RE,7,DUB                                                         
         LNR   RE,RE                                                            
         STCM  RE,7,DUB                                                         
         STCM  RE,7,STAKEFDA      SET NEGATED EFF DATE IN THE DSTA KEY          
*                                                                               
G2R010   LAY   RE,KEYSAVE          COPY KEY INTO KEYSAVE                        
         MVC   0(L'KEYSAVE,RE),0(R4)                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'GENDIR',A(KEY),A(KEY),0           
         CLI   DMCB+8,0                                                         
         BNE   AGHDE017                                                         
         LAY   RE,KEYSAVE          COMPARE KEYS                                 
         CLC   0(STAKEFDA-STAKEY,RE),0(R4) SAME DSTA UPTO EFF DATE              
         BNE   AGHDE015           ANY ERROR RETURN "INCORRECT REP"              
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'GENFIL',A(KEY+36),       +        
               A(IO2),A(DMWORK)                                                 
         CLI   8(R1),0                                                          
         BNE   AGHDE017                                                         
*                                                                               
         LAY   R4,IO2                                                           
         LA    R4,STAFSTEL        R4 = A(1ST ELEM IN DSTA REC)                  
G2R020   CLI   0(R4),0            EOR?                                          
         BE    AGHDE018           YES, NO 2ND REP                               
         CLI   0(R4),STARP2CQ     X'11' - 2ND REP ELEM                          
         BE    G2R030             YES, WE HAVE IT                               
         LLC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     G2R020                                                           
*                                                                               
         USING STAREPD,R4                                                       
G2R030   CLC   STAREPCR,PAHDTOID  MATCH ON 2ND REP CODE?                        
         BE    G2R050              YES                                          
*                                                                               
         CLI   STAREPLN,STARP2LQ  HAVE MULTIPLE 2ND REPS?                       
         BNE   AGHDE019            NO, SEND ERROR                               
*                                                                               
         LHI   RF,STAREPSN                                                      
G2R040   OC    STAREPS,STAREPS    EOL?                                          
         BZ    AGHDE019           YES, NO MATCH FOUND SEND ERROR                
         CLC   STAREPS,PAHDTOID   MATCH ON 2ND REP CODE?                        
         BE    G2R050             YES                                           
         LA    R4,L'STAREPS(R4)   BUMP TO NEXT 2ND REP                          
         BCT   RF,G2R040                                                        
         B     AGHDE019           NO MATCH FOUND SEND ERROR                     
*                                                                               
G2R050   MVC   FULL,PAHDTOID       CANNOT PASS GETDARE A 31BIT ADDR             
         GOTO1 AGETDARE,DMCB,(C'R',0),FULL,A(IO1),IO1LQ,DATAMGR                 
         BNE   AGHDE020            ERROR, ISSUE WITH GETDARE                    
*                                                                               
         LAY   R4,IO1                                                           
         USING DAREPTD,R4                                                       
*                                                                               
         TM    DAPTFLG2,DPF2EOC    END OF CONTRACT?                             
         BO    AGHDE005            YES, UNKNOWN DESTINATION ID                  
*                                                                               
         MVC   RECEIVER,SPACES                                                  
         LLC   RE,DAPTLPFX         L'USERID PREFIX                              
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   RECEIVER(0),DAPTRPPF                                             
*                                                                               
         TM    DAPTFLG1,DPF1NOOF   DO NOT APPEND OFFICE?                        
         BNZ   GET2NDRPX           YES, WE'RE DONE                              
         DROP  R4                                                               
*                                                                               
         LA    R4,RECEIVER                                                      
         LA    R4,1(RE,R4)         A(OFFICE AFTER USERID PREFIX)                
         MVC   0(2,R4),ROUTCODE+3  PUT THE OFFICE IN                            
******                                                                          
* CHECK DAGYROUT TABLE                                                          
         L     RF,TSKOEXTB         TABLE OF OFFICE EXCEPTIONS                   
         USING OFEXTABD,RF                                                      
*                                                                               
G2R060   CLI   0(RF),X'FF'                                                      
         BE    GET2NDRPX           NO EXCEPTION -- USE ROUTING CODE             
         CLC   MEDIA,OFEXMED                                                    
         BNE   G2R070                                                           
         CLC   ROUTCODE,OFEXROUT   MATCH ON AGENCY ROUTING CODE?                
         BNE   G2R070                                                           
         CLC   OFEXREP,PAHDTOID                                                 
         BE    G2R080              GOT IT                                       
G2R070   LA    RF,OFEXTBLQ(,RF)    BUMP TO NEXT ENTRY                           
         B     G2R060                                                           
*                                                                               
G2R080   MVC   0(2,R4),OFEXOFF     PUT OFFICE OVERRIDE IN RECEIVING ID          
GET2NDRPX B    AGYHDRX                                                          
                                                                                
         DROP  RF                                                               
*                                                                               
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
DLNNOT   NMOD1 0,DLNNOT                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* CHECK TO SEE COA QUEUE IS UP TO SPEED.                                        
* IF NOT, WAIT SOME TIME AND EMAIL WARNING, AND THEN TRY AGAIN.                 
*                                                                               
         L     RF,TSKRPLYQ         USE DEFAULT REPLYQ                           
*                                                                               
*************************************************************                   
* CHECK IF MESSAGE FROM WO, IF SO, SWITCH TO WO'S COA QUEUE                     
*                                                                               
         CLC   =C'QM_WOX',MSGDESC_REPLYTOQMGR   CHECK IF WO QMGR                
         BNE   NOTWO                                                            
         CLI   TSKDSPC,C'A'                                                     
         BNE   *+12                                                             
         LA    RF,=CL48'DARE.PROD.WO.REPLY.QUEUE'                               
         B     NOTWO                                                            
         CLI   TSKDSPC,C'T'                                                     
         BNE   *+12                                                             
         LA    RF,=CL48'DARE.TEST.WO.REPLY.QUEUE'                               
         B     NOTWO                                                            
NOTWO    EQU   *                                                                
*************************************************************                   
*                                                                               
         MVC   OBJDESC_COA_OBJECTNAME,0(RF)                                     
*                                                                               
         MVC   OBJDESC_COA_OBJECTTYPE,=A(MQOT_Q)                                
         MVC   OPEN_OPTIONS,=A((MQOO_INPUT_AS_Q_DEF+MQOO_BROWSE))               
*                                                                               
         MVC   P+30(L'OBJDESC_COA_OBJECTNAME),OBJDESC_COA_OBJECTNAME            
         PRNT  ToOpenCOAQueue                                                   
*                                                                               
         LA    R2,CSQBOPEN_COA_STRUCTURE                                        
         GOTO1 =A(CALLMQ),(RC)     OPEN QUEUE                                   
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         JNE   *+2                                                              
*                                                                               
         LA    R3,10               LOOP COUNTER                                 
         LA    R4,100              WAIT SEC B/W EMAILS (1 SEC UP)               
*                                                                               
DN020    DS    0H                                                               
         MVC   MSGDESC_COA_CORRELID,MQCI_NONE                                   
         MVC   MSGDESC_COA_MSGID,MQMI_NONE                                      
*                                  ONLY BROWSE 1ST MESSAGE                      
         LHI   RF,MQGMO_ACCEPT_TRUNCATED_MSG                                    
         AHI   RF,MQGMO_BROWSE_FIRST                                            
         ST    RF,GETMSGOPTS_COA_OPTIONS                                        
*                                                                               
         LA    R2,CSQBGET_COA_STRUCTURE                                         
         GOTO1 =A(CALLMQ),(RC)                                                  
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    DN040               GOT IT                                       
         CLC   MQ_REASON,=A(MQRC_TRUNCATED_MSG_ACCEPTED)                        
         BE    DN040               READ TRUNCATED MSG ALSO                      
         CLC   MQ_REASON,=A(MQRC_NO_MSG_AVAILABLE)                              
         BE    DN090               GOOD - NO MSG, CLOSE AND EXIT                
         DC    H'0'                NO OTHER ERROR IS ACCEPTABLE                 
*                                                                               
DN040    DS    0H                                                               
         CLC   MSGDESC_PUTDATE(16),MSGDESC_COA_PUTDATE                          
         BL    DN090               GOOD - COA PROCESSED UP TO SPEED             
*                                                                               
*                                  EMAIL WARNING                                
         GOTO1 DATAMGR,DMCB,=C'OPMSG',=C'AUTONOTE*WHOAN,AHYDN,AWILN:WAR+        
               NING! DARE COA NOT UP TO SPEED!'                                 
*                                                                               
         ST    R4,FULL                                                          
         STIMER   WAIT,BINTVL=FULL                                              
         AHI   R4,100              INCREASE NEXT WAIT TIME BY 1 SEC             
*                                                                               
         BCT   R3,DN020            CHECK AGAIN                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'OPMSG',=C'AUTONOTE*WHOAN,AHYDN,AWILN:   +        
               DARE COA NOT UP TO SPEED! **ABEND**'                             
         DC    H'0'                PROBLEM WITH DARECOA/COW JOBS                
*                                                                               
DN090    DS    0H                                                               
         MVC   CLOSE_OPTIONS,=A(MQCO_NONE)                                      
         LA    R2,CSQBCLOS_COA_STRUCTURE                                        
         GOTO1 =A(CALLMQ),(RC)     CLOSE COA QUEUE                              
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         JNE   *+2                                                              
*                                                                               
DLNNOTOK CR    RB,RB               SET CC EQUAL                                 
DLNNOTX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
ERRNOT   NMOD1 0,ERRNOT                                                         
         SAM31                                                                  
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS AN ERROR NOTIFICATION.  LOOK UP THE SENDING ID IN TABLE, SO           
* WE KNOW TO WHICH MQ WORKQ TO WRITE THE REPORT.                                
*                                                                               
* UPON ENTRY, R3 = A(FIRST RECORD)                                              
*                                                                               
         MVC   ARECTAB,=A(ERRRECS) VALID RECORD TYPES FOR AN ERRNOT             
*                                                                               
         USING MDLNNOTD,R3                                                      
*                                                                               
         MVC   ORDNUM,MDNTORDR                                                  
         MVC   CONTRACT,MDNTRPCN                                                
         MVC   RETURN,MDNTRTNS                                                  
         MVC   ORIGDATE,MDNTDATE                                                
         MVC   ORIGTIME,MDNTTIME                                                
         MVC   SENDER,MDNTFRID     DDS USERID                                   
         MVC   RECEIVER,MDNTTOID   RECEIVING ID                                 
         MVC   OFFERID,MDNTOFRI                                                 
         MVC   IDSEQNUM,MDNTSEQN                                                
         MVI   MEDIA,C' '          BECAUSE STATION ISN'T IN RECORD              
*                                                                               
         GOTO1 =A(CHKSNDR),(RC)    ADD "-DS" FOR SPECIAL DDS REPS               
         MVC   MDNTFRID,SENDER     OVERRIDE THE SENDER                          
*                                                                               
         GOTO1 =A(GETUID),(RC)     LOOK UP NUMERIC                              
         OC    USERID,USERID                                                    
         BNZ   EN10                                                             
         MVC   P+30(36),=C'BAD SENDER USERID IN INCOMING ERRNOT'                
         MVC   P+11(13),=C'*** ERROR ***'                                       
         PRNT  BadERRNOT                                                        
         B     ERRNOTOK            DON'T WRITE THE ERRNOT TO WORKQ              
*                                                                               
EN10     GOTO1 =A(CHKRCVR),(RC)    MASSAGE RECEIVER IF NECESSARY                
         MVC   MDNTTOID,RECEIVER   RECEIVING ID                                 
         BRAS  RE,FINDTSK          RETURNS SUBTASK ID IN 'SNDTSKID'             
         OC    SNDTSKID,SNDTSKID                                                
         BNZ   EN20                                                             
         MVC   P+30(38),=C'BAD RECEIVER USERID IN INCOMING ERRNOT'              
         MVC   P+11(13),=C'*** ERROR ***'                                       
         PRNT  BadERRNOT                                                        
         B     ERRNOTOK            DON'T WRITE THE ERRNOT TO WORKQ              
*                                                                               
EN20     BRAS  RE,CHKSERR                                                       
         BNE   ERRNOTOK                                                         
*                                                                               
         LHI   R2,RDLNNOTL         L'RECORD                                     
         CLC   OFFERID,SPACES      IS THIS AN ERRNOT FOR A MAKEGOOD             
         BE    *+8                                                              
         LHI   R2,MDLNNOTL         YES                                          
         GOTO1 =A(PUTSMF),(RC)                                                  
*                                                                               
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE MQ WORKQ                
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'ERRNOT'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
ERRNOTOK CR    RB,RB               SET CC EQUAL                                 
ERRNOTX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
ORDAPP   NMOD1 0,ORDAPP                                                         
         SAM31                                                                  
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS AN ORDER APPROVAL.  LOOK UP THE SENDING ID IN TABLE, SO               
* WE KNOW TO WHICH MQ WORKQ TO WRITE THE REPORT.                                
*                                                                               
* UPON ENTRY, R3 = A(FIRST RECORD)                                              
*                                                                               
         MVC   ARECTAB,=A(APPRECS) VALID RECORD TYPES FOR AN APPROVAL           
*                                                                               
         USING RORDAPPD,R3                                                      
*                                                                               
         MVC   ORDNUM,ROAPORDR                                                  
         MVC   CONTRACT,ROAPRPCN                                                
         MVC   RETURN,ROAPRTRN                                                  
         MVC   ORIGDATE,ROAPDATE                                                
         MVC   ORIGTIME,ROAPTIME                                                
         MVC   SENDER,ROAPFRID     DDS USERID                                   
         MVC   RECEIVER,ROAPTOID                                                
         MVC   STATION,ROAPQSTA    STATION                                      
*                                                                               
         GOTO1 =A(FINDMED),(RC)    DERIVE THE MEDIA                             
         CLI   MEDIA,C' '                                                       
         BNE   *+14                                                             
         MVC   ERRCODE,=C'006'     UNKNOWN STATION                              
         B     ORDAPPOK                                                         
*                                                                               
         GOTO1 =A(CHKSNDR),(RC)    ADD "-DS" FOR SPECIAL DDS REPS               
         MVC   ROAPFRID,SENDER     OVERRIDE THE SENDER                          
*                                                                               
         GOTO1 =A(GETUID),(RC)     LOOK UP NUMERIC                              
         OC    USERID,USERID                                                    
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'007'     UNKNOWN AGENCY                               
         B     ORDAPPOK                                                         
*                                                                               
         GOTO1 =A(FINDSTN),(RC)    RETURNS CALL LETTERS IN 'STATION'            
         CLC   STATION,SPACES      WAS STATION FOUND IN TABLE?                  
         BNE   *+14                YES                                          
         MVC   ERRCODE,=C'006'     UNKNOWN STATION                              
         B     ORDAPPOK                                                         
*                                                                               
         MVC   ROAPQSTA(5),STATION PUT CORRECT CALL LETTERS INTO ORDAPP         
         CLC   OSTATION,SPACES     WAS STATION FOUND IN TABLE?                  
         BE    *+10                YES                                          
         MVC   ROAPOLDS(5),OSTATION PUT OLD CALL LETTERS INTO ORDAPP            
*                                                                               
         GOTO1 =A(CHKRCVR),(RC)    MASSAGE RECEIVER IF NECESSARY                
         MVC   ROAPTOID,RECEIVER                                                
*                                                                               
         BRAS  RE,FINDTSK          RETURNS SUBTASK ID IN 'SNDTSKID'             
         OC    SNDTSKID,SNDTSKID                                                
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'007'     UNKNOWN AGENCY                               
         B     ORDAPPOK                                                         
*                                                                               
         BRAS  RE,CHKSERR                                                       
         BNE   ORDAPPOK                                                         
*                                                                               
         LHI   R2,RORDAPPL         L'RECORD                                     
         GOTO1 =A(PUTSMF),(RC)                                                  
*                                                                               
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE MQ WORKQ                
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'ORDAPP'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
ORDAPPOK CR    RB,RB               SET CC EQUAL                                 
ORDAPPX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
ORDREJ   NMOD1 0,ORDREJ                                                         
         SAM31                                                                  
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS AN ORDER REJECT.  LOOK UP THE SENDING ID IN TABLE, SO                 
* WE KNOW TO WHICH MQ WORKQ TO WRITE THE REPORT.                                
*                                                                               
* UPON ENTRY, R3 = A(FIRST RECORD)                                              
*                                                                               
         MVC   ARECTAB,=A(REJRECS) VALID RECORD TYPES FOR A REJECT              
*                                                                               
         USING RORDREJD,R3                                                      
*                                                                               
         MVC   ORDNUM,RORJORDR                                                  
         MVC   CONTRACT,RORJRPCN                                                
         MVC   RETURN,RORJRTRN                                                  
         MVC   ORIGDATE,RORJDATE                                                
         MVC   ORIGTIME,RORJTIME                                                
         MVC   SENDER,RORJFRID     DDS USERID                                   
         MVC   RECEIVER,RORJTOID                                                
         MVC   STATION,RORJQSTA    STATION                                      
*                                                                               
         GOTO1 =A(FINDMED),(RC)    DERIVE THE MEDIA                             
         CLI   MEDIA,C' '                                                       
         BNE   *+14                                                             
         MVC   ERRCODE,=C'006'     UNKNOWN STATION                              
         B     ORDREJOK                                                         
*                                                                               
         GOTO1 =A(CHKSNDR),(RC)    ADD "-DS" FOR SPECIAL DDS REPS               
         MVC   RORJFRID,SENDER     OVERRIDE THE SENDER                          
*                                                                               
         GOTO1 =A(GETUID),(RC)     LOOK UP NUMERIC                              
         OC    USERID,USERID                                                    
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'007'     UNKNOWN AGENCY                               
         B     ORDREJOK                                                         
*                                                                               
         GOTO1 =A(FINDSTN),(RC)    RETURNS CALL LETTERS IN 'STATION'            
         CLC   STATION,SPACES      WAS STATION FOUND IN TABLE?                  
         BNE   *+14                YES                                          
         MVC   ERRCODE,=C'006'     UNKNOWN STATION                              
         B     ORDREJOK                                                         
*                                                                               
         MVC   RORJQSTA(5),STATION PUT CORRECT CALL LETTERS INTO ORDREJ         
         CLC   OSTATION,SPACES     WAS STATION FOUND IN TABLE?                  
         BE    *+10                YES                                          
         MVC   RORJOLDS(5),OSTATION PUT OLD CALL LETTERS INTO ORDREJ            
*                                                                               
         GOTO1 =A(CHKRCVR),(RC)    MASSAGE RECEIVER IF NECESSARY                
         MVC   RORJTOID,RECEIVER                                                
*                                                                               
         BRAS  RE,FINDTSK          RETURNS SUBTASK ID IN 'SNDTSKID'             
         OC    SNDTSKID,SNDTSKID                                                
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'007'     UNKNOWN AGENCY                               
         B     ORDREJOK                                                         
*                                                                               
         BRAS  RE,CHKSERR                                                       
         BNE   ORDREJOK                                                         
*                                                                               
         LHI   R2,RORDREJL         L'RECORD                                     
         GOTO1 =A(PUTSMF),(RC)                                                  
*                                                                               
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE MQ WORKQ                
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'ORDREJ'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
ORDREJOK CR    RB,RB               SET CC EQUAL                                 
ORDREJX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
ORDCFM   NMOD1 0,ORDCFM                                                         
         SAM31                                                                  
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS AN ORDER CONFIRM.  LOOK UP THE SENDING ID IN TABLE, SO                
* WE KNOW TO WHICH MQ WORKQ TO WRITE THE REPORT.                                
*                                                                               
* UPON ENTRY, R3 = A(FIRST RECORD)                                              
*                                                                               
         MVC   ARECTAB,=A(CFMRECS) VALID RECORD TYPES FOR A CONFIRM             
*                                                                               
         USING RORDCFMD,R3                                                      
*                                                                               
         MVC   SENDER,ROCFFRID     DDS USERID                                   
         MVC   ORDNUM,ROCFORDR                                                  
         MVC   CONTRACT,ROCFRPCN                                                
         MVC   RETURN,ROCFRTRN                                                  
         MVC   ORIGDATE,ROCFDATE                                                
         MVC   ORIGTIME,ROCFTIME                                                
         MVC   RECEIVER,ROCFTOID                                                
         MVC   STATION,ROCFQSTA    STATION                                      
*                                                                               
         GOTO1 =A(FINDMED),(RC)    DERIVE THE MEDIA                             
         CLI   MEDIA,C' '                                                       
         BNE   *+14                                                             
         MVC   ERRCODE,=C'006'     UNKNOWN STATION                              
         B     ORDCFMOK                                                         
*                                                                               
         GOTO1 =A(CHKSNDR),(RC)    ADD "-DS" FOR SPECIAL DDS REPS               
         MVC   ROCFFRID,SENDER     OVERRIDE THE SENDER                          
*                                                                               
         GOTO1 =A(GETUID),(RC)     LOOK UP NUMERIC                              
         OC    USERID,USERID                                                    
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'007'     UNKNOWN AGENCY                               
         B     ORDCFMOK                                                         
*                                                                               
         GOTO1 =A(FINDSTN),(RC)    RETURNS CALL LETTERS IN 'STATION'            
         CLC   STATION,SPACES      WAS STATION FOUND IN TABLE?                  
         BNE   *+14                YES                                          
         MVC   ERRCODE,=C'006'     UNKNOWN STATION                              
         B     ORDCFMOK                                                         
*                                                                               
         MVC   ROCFQSTA(5),STATION PUT CORRECT CALL LETTERS INTO ORDCFM         
         CLC   OSTATION,SPACES     WAS STATION FOUND IN TABLE?                  
         BE    *+10                YES                                          
         MVC   ROCFOLDS(5),OSTATION PUT OLD CALL LETTERS INTO ORDCFM            
*                                                                               
         GOTO1 =A(CHKRCVR),(RC)    MASSAGE RECEIVER IF NECESSARY                
         MVC   ROCFTOID,RECEIVER                                                
*                                                                               
         BRAS  RE,FINDTSK          RETURNS SUBTASK ID IN 'SNDTSKID'             
         OC    SNDTSKID,SNDTSKID                                                
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'007'     UNKNOWN AGENCY                               
         B     ORDCFMOK                                                         
*                                                                               
         BRAS  RE,CHKSERR                                                       
         BNE   ORDCFMOK                                                         
*                                                                               
         LHI   R2,RORDCFML         L'RECORD                                     
         GOTO1 =A(PUTSMF),(RC)                                                  
*                                                                               
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE MQ WORKQ                
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'ORDCFM'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
ORDCFMOK CR    RB,RB               SET CC EQUAL                                 
ORDCFMX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
ORDRCL   NMOD1 0,ORDRCL                                                         
         SAM31                                                                  
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS AN ORDER RECALL.  LOOK UP THE SENDING ID IN TABLE, SO                 
* WE KNOW TO WHICH MQ WORKQ TO WRITE THE REPORT.                                
*                                                                               
* UPON ENTRY, R3 = A(FIRST RECORD)                                              
*                                                                               
         MVC   ARECTAB,=A(ORCRECS) VALID RECORD TYPES FOR A RECALL              
*                                                                               
         USING RORDRCLD,R3                                                      
*                                                                               
         MVC   SENDER,RORCFRID     DDS USERID                                   
         MVC   ORDNUM,RORCORDR                                                  
         MVC   CONTRACT,RORCRPCN                                                
         MVC   RETURN,RORCRTRN                                                  
         MVC   ORIGDATE,RORCDATE                                                
         MVC   ORIGTIME,RORCTIME                                                
         MVC   RECEIVER,RORCTOID                                                
         MVC   STATION,RORCQSTA    STATION                                      
*                                                                               
         GOTO1 =A(FINDMED),(RC)    DERIVE THE MEDIA                             
         CLI   MEDIA,C' '                                                       
         BNE   *+14                                                             
         MVC   ERRCODE,=C'006'     UNKNOWN STATION                              
         B     ORDRCLOK                                                         
*                                                                               
         GOTO1 =A(CHKSNDR),(RC)    ADD "-DS" FOR SPECIAL DDS REPS               
         MVC   RORCFRID,SENDER     OVERRIDE THE SENDER                          
*                                                                               
         GOTO1 =A(GETUID),(RC)     LOOK UP NUMERIC                              
         OC    USERID,USERID                                                    
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'007'     UNKNOWN AGENCY                               
         B     ORDRCLOK                                                         
*                                                                               
         GOTO1 =A(FINDSTN),(RC)    RETURNS CALL LETTERS IN 'STATION'            
         CLC   STATION,SPACES      WAS STATION FOUND IN TABLE?                  
         BNE   *+14                YES                                          
         MVC   ERRCODE,=C'006'     UNKNOWN STATION                              
         B     ORDRCLOK                                                         
*                                                                               
         MVC   RORCQSTA(5),STATION PUT CORRECT CALL LETTERS INTO ORDRCL         
         CLC   OSTATION,SPACES     WAS STATION FOUND IN TABLE?                  
         BE    *+10                YES                                          
         MVC   RORCOLDS(5),OSTATION PUT OLD CALL LETTERS INTO ORDRCL            
*                                                                               
         GOTO1 =A(CHKRCVR),(RC)    MASSAGE RECEIVER IF NECESSARY                
         MVC   RORCTOID,RECEIVER                                                
*                                                                               
         BRAS  RE,FINDTSK          RETURNS SUBTASK ID IN 'SNDTSKID'             
         OC    SNDTSKID,SNDTSKID                                                
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'007'     UNKNOWN AGENCY                               
         B     ORDRCLOK                                                         
*                                                                               
         BRAS  RE,CHKSERR                                                       
         BNE   ORDRCLOK                                                         
*                                                                               
         LHI   R2,RORCAPPL         L'RECORD                                     
         GOTO1 =A(PUTSMF),(RC)                                                  
*                                                                               
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE MQ WORKQ                
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'ORDRCL'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
ORDRCLOK CR    RB,RB               SET CC EQUAL                                 
ORDRCLX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
AGYRCL   NMOD1 0,AGYRCL                                                         
         SAM31                                                                  
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS AN AGENCY RECALL.  LOOK UP THE SENDING ID IN TABLE, SO                
* WE KNOW TO WHICH MQ WORKQ TO WRITE THE REPORT.                                
*                                                                               
* UPON ENTRY, R3 = A(FIRST RECORD)                                              
*                                                                               
         MVC   ARECTAB,=A(ARCRECS) VALID RECORD TYPES FOR A RECALL              
*                                                                               
         USING PAGYRCLD,R3                                                      
*                                                                               
         MVC   SENDER,PARCFRID     DDS USERID                                   
         MVC   ORDNUM,PARCORDR                                                  
         MVC   CONTRACT,PARCRPCN                                                
         MVC   RETURN,PARCRTRN                                                  
         MVC   ORIGDATE,PARCDATE                                                
         MVC   ORIGTIME,PARCTIME                                                
         MVC   ROUTCODE,PARCROUT                                                
         MVC   RECEIVER,PARCTOID                                                
         MVC   STATION,PARCQSTA    STATION                                      
*                                                                               
         GOTO1 =A(FINDMED),(RC)    DERIVE THE MEDIA                             
         CLI   MEDIA,C' '                                                       
         BNE   *+14                                                             
         MVC   ERRCODE,=C'006'     UNKNOWN STATION                              
         B     AGYRCLOK                                                         
*                                                                               
         GOTO1 =A(CHKSNDR),(RC)    ADD "-DS" FOR SPECIAL DDS REPS               
         MVC   PARCFRID,SENDER     OVERRIDE THE SENDER                          
*                                                                               
         GOTO1 =A(GETUID),(RC)     LOOK UP NUMERIC                              
         OC    USERID,USERID                                                    
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'007'     UNKNOWN AGENCY                               
         B     AGYRCLOK                                                         
*                                                                               
         GOTO1 =A(FINDSTN),(RC)    RETURNS CALL LETTERS IN 'STATION'            
         CLC   STATION,SPACES      WAS STATION FOUND IN TABLE?                  
         BNE   *+14                YES                                          
         MVC   ERRCODE,=C'006'     UNKNOWN STATION                              
         B     AGYRCLOK                                                         
*                                                                               
         MVC   PARCQSTA(5),STATION PUT CORRECT CALL LETTERS INTO AGYRCL         
         CLC   OSTATION,SPACES     WAS STATION FOUND IN TABLE?                  
         BE    *+10                YES                                          
         MVC   PARCOLDS(5),OSTATION PUT OLD CALL LETTERS INTO AGYHDR            
*                                                                               
         GOTO1 =A(CHKMOTAB),(RC)   IF WE HAVE TO MODIFY RCVR FOR -MO            
         GOTO1 =A(CHKWOTAB),(RC)   SEE IF WE HAVE TO MODIFY WITH -WO            
         MVC   PARCTOID,RECEIVER                                                
*                                                                               
         GOTO1 =A(CHKRCVR),(RC)    MASSAGE RECEIVER IF NECESSARY                
         MVC   PARCTOID,RECEIVER                                                
*                                                                               
         CLI   MEDIA,C'T'          IF THIS IS A TV ORDER                        
         BNE   *+16                                                             
         MVC   PARCFSNT,SPACES     ONLY DARE DISPATCHER CARES                   
         MVC   PARCFSTA,SPACES                                                  
*                                                                               
         BRAS  RE,FINDTSK          RETURNS SUBTASK ID IN 'SNDTSKID'             
         OC    SNDTSKID,SNDTSKID                                                
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'005'     UNKNOWN DESTINATION                          
         B     AGYRCLOK                                                         
*                                                                               
         BRAS  RE,CHKSERR                                                       
         BNE   AGYRCLOK                                                         
*                                                                               
         LHI   R2,PAGYRCLL         L'RECORD                                     
         GOTO1 =A(PUTSMF),(RC)                                                  
*                                                                               
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE MQ WORKQ                
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'AGYRCL'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
AGYRCLOK CR    RB,RB               SET CC EQUAL                                 
AGYRCLX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
AGYCAN   NMOD1 0,AGYCAN                                                         
         SAM31                                                                  
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS AN AGENCY CANCELLATION.  LOOK UP THE SENDING ID IN TABLE, SO          
* WE KNOW TO WHICH MQ WORKQ TO WRITE THE REPORT.                                
*                                                                               
* UPON ENTRY, R3 = A(FIRST RECORD)                                              
*                                                                               
         MVC   ARECTAB,=A(ACNRECS) VALID RECORD TYPES FOR AGENCY CANCEL         
*                                                                               
         USING PAGYCAND,R3                                                      
*                                                                               
         MVC   ORDNUM,PACNORDR                                                  
         MVC   CONTRACT,PACNRPCN                                                
         MVC   RETURN,PACNRTRN                                                  
         MVC   ORIGDATE,PACNDATE                                                
         MVC   ORIGTIME,PACNTIME                                                
         MVC   ROUTCODE,PACNROUT                                                
         MVC   SENDER,PACNFRID     DDS USERID                                   
         MVC   RECEIVER,PACNTOID                                                
         MVC   STATION,PACNQSTA    STATION                                      
*                                                                               
         GOTO1 =A(FINDMED),(RC)    DERIVE THE MEDIA                             
         CLI   MEDIA,C' '                                                       
         BNE   *+14                                                             
         MVC   ERRCODE,=C'006'     UNKNOWN STATION                              
         B     AGYCANOK                                                         
*                                                                               
         GOTO1 =A(CHKSNDR),(RC)    ADD "-DS" FOR SPECIAL DDS REPS               
         MVC   PACNFRID,SENDER     OVERRIDE THE SENDER                          
*                                                                               
         GOTO1 =A(GETUID),(RC)     LOOK UP NUMERIC                              
         OC    USERID,USERID                                                    
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'007'     UNKNOWN AGENCY                               
         B     AGYCANOK                                                         
*                                                                               
         GOTO1 =A(FINDSTN),(RC)    RETURNS CALL LETTERS IN 'STATION'            
         CLC   STATION,SPACES      WAS STATION FOUND IN TABLE?                  
         BNE   *+14                YES                                          
         MVC   ERRCODE,=C'006'     UNKNOWN STATION                              
         B     AGYCANOK                                                         
*                                                                               
         MVC   PACNQSTA(5),STATION PUT CORRECT CALL LETTERS INTO AGYCAN         
         CLC   OSTATION,SPACES     WAS STATION FOUND IN TABLE?                  
         BE    *+10                YES                                          
         MVC   PACNOLDS(5),OSTATION PUT OLD CALL LETTERS INTO AGYHDR            
*                                                                               
         GOTO1 =A(CHKRCVR),(RC)    MASSAGE RECEIVER IF NECESSARY                
         MVC   PACNTOID,RECEIVER                                                
*                                                                               
         CLI   MEDIA,C'T'          IF THIS IS A TV ORDER                        
         BNE   *+16                                                             
         MVC   PACNFSNT,SPACES     ONLY DARE DISPATCHER CARES                   
         MVC   PACNFSTA,SPACES                                                  
*                                                                               
         BRAS  RE,FINDTSK          RETURNS SUBTASK ID IN 'SNDTSKID'             
         OC    SNDTSKID,SNDTSKID                                                
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'005'     UNKNOWN DESTINATION                          
         B     AGYCANOK                                                         
*                                                                               
         BRAS  RE,CHKSERR                                                       
         BNE   AGYCANOK                                                         
*                                                                               
         LHI   R2,PAGYCALN         L'RECORD                                     
         GOTO1 =A(PUTSMF),(RC)                                                  
*                                                                               
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE MQ WORKQ                
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'AGYCAN'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
AGYCANOK CR    RB,RB               SET CC EQUAL                                 
AGYCANX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
MKGHDR   NMOD1 0,MKGHDR                                                         
         SAM31                                                                  
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS A MAKEGOOD HEADER.  LOOK UP THE SENDING ID IN TABLE, SO               
* WE KNOW TO WHICH MQ WORKQ TO WRITE THE REPORT.                                
*                                                                               
* UPON ENTRY, R3 = A(FIRST RECORD)                                              
*                                                                               
         MVC   ARECTAB,=A(MKGRECS) VALID RECORD TYPES FOR MAKEGOOD              
*                                                                               
         USING MOFRHDRD,R3                                                      
*                                                                               
         MVC   ORDNUM,MOHDORDR                                                  
         MVC   CONTRACT,MOHDRPCN                                                
         MVC   RETURN,MOHDRTNS                                                  
         MVC   ORIGDATE,MOHDDATE                                                
         MVC   ORIGTIME,MOHDTIME                                                
         MVC   SENDER,MOHDFRID     DDS USERID                                   
         MVC   RECEIVER,MOHDTOID                                                
         MVC   OFFERID,MOHDOFRI                                                 
         MVC   IDSEQNUM,MOHDSEQN                                                
*                                                                               
         GOTO1 =A(CHKSNDR),(RC)    ADD "-DS" FOR SPECIAL DDS REPS               
         MVC   MOHDFRID,SENDER     OVERRIDE THE SENDER                          
*                                                                               
         GOTO1 =A(GETUID),(RC)     LOOK UP NUMERIC                              
         OC    USERID,USERID                                                    
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'007'     UNKNOWN AGENCY                               
         B     MKGHDROK                                                         
*                                                                               
         GOTO1 =A(CHKRCVR),(RC)    MASSAGE RECEIVER IF NECESSARY                
*                                                                               
         BRAS  RE,FINDTSK          RETURNS SUBTASK ID IN 'SNDTSKID'             
         OC    SNDTSKID,SNDTSKID                                                
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'007'     UNKNOWN AGENCY                               
         B     MKGHDROK                                                         
*                                                                               
         BRAS  RE,CHKSERR                                                       
         BNE   MKGHDROK                                                         
*                                                                               
         LHI   R2,MOFRHDRL         L'RECORD                                     
         GOTO1 =A(PUTSMF),(RC)                                                  
*                                                                               
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE MQ WORKQ                
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'MKGHDR'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
MKGHDROK CR    RB,RB               SET CC EQUAL                                 
MKGHDRX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
MKGAPP   NMOD1 0,MKGAPP                                                         
         SAM31                                                                  
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS A MAKEGOOD APPROVAL.  LOOK UP THE SENDING ID IN TABLE, SO             
* WE KNOW TO WHICH MQ WORKQ TO WRITE THE REPORT.                                
*                                                                               
* UPON ENTRY, R3 = A(FIRST RECORD)                                              
*                                                                               
         MVC   ARECTAB,=A(MAPRECS) VALID RECORD TYPES FOR MAKEGOOD APP.         
*                                                                               
         USING MOFRAPPD,R3                                                      
*                                                                               
         MVC   ORDNUM,MOAPORDR                                                  
         MVC   CONTRACT,MOAPRPCN                                                
         MVC   RETURN,MOAPRTNS                                                  
         MVC   ORIGDATE,MOAPDATE                                                
         MVC   ORIGTIME,MOAPTIME                                                
         MVC   SENDER,MOAPFRID     DDS USERID                                   
         MVC   RECEIVER,MOAPTOID                                                
         MVC   OFFERID,MOAPOFRI                                                 
         MVC   IDSEQNUM,MOAPSEQN                                                
         MVC   STATION,MOAPQSTA    STATION                                      
*                                                                               
         GOTO1 =A(FINDMED),(RC)    DERIVE THE MEDIA                             
         CLI   MEDIA,C' '                                                       
         BNE   *+14                                                             
         MVC   ERRCODE,=C'006'     UNKNOWN STATION                              
         B     MKGAPPOK                                                         
*                                                                               
         GOTO1 =A(CHKSNDR),(RC)    ADD "-DS" FOR SPECIAL DDS REPS               
         MVC   MOAPFRID,SENDER     OVERRIDE THE SENDER                          
*                                                                               
         GOTO1 =A(GETUID),(RC)     LOOK UP NUMERIC                              
         OC    USERID,USERID                                                    
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'007'     UNKNOWN AGENCY                               
         B     MKGAPPOK                                                         
*                                                                               
         GOTO1 =A(FINDSTN),(RC)    RETURNS CALL LETTERS IN 'STATION'            
         CLC   STATION,SPACES      WAS STATION FOUND IN TABLE?                  
         BNE   *+14                YES                                          
         MVC   ERRCODE,=C'006'     UNKNOWN STATION                              
         B     MKGAPPOK                                                         
*                                                                               
         MVC   MOAPQSTA(5),STATION PUT CORRECT CALL LETTERS INTO MKGAPP         
         CLC   OSTATION,SPACES     WAS STATION FOUND IN TABLE?                  
         BE    *+10                YES                                          
         MVC   MOAPOLDS(5),OSTATION PUT OLD CALL LETTERS INTO AGYHDR            
*                                                                               
         GOTO1 =A(CHKRCVR),(RC)    MASSAGE RECEIVER IF NECESSARY                
         MVC   MOAPTOID,RECEIVER                                                
*                                                                               
         BRAS  RE,FINDTSK          RETURNS SUBTASK ID IN 'SNDTSKID'             
         OC    SNDTSKID,SNDTSKID                                                
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'007'     UNKNOWN AGENCY                               
         B     MKGAPPOK                                                         
*                                                                               
         BRAS  RE,CHKSERR                                                       
         BNE   MKGAPPOK                                                         
*                                                                               
         LHI   R2,MOFRAPPL         L'RECORD                                     
         GOTO1 =A(PUTSMF),(RC)                                                  
*                                                                               
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE MQ WORKQ                
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'MKGAPP'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
MKGAPPOK CR    RB,RB               SET CC EQUAL                                 
MKGAPPX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
MKGREJ   NMOD1 0,MKGREJ                                                         
         SAM31                                                                  
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS A MAKEGOOD REJECTION.  LOOK UP THE SENDING ID IN TABLE, SO            
* WE KNOW TO WHICH MQ WORKQ TO WRITE THE REPORT.                                
*                                                                               
* UPON ENTRY, R3 = A(FIRST RECORD)                                              
*                                                                               
         MVC   ARECTAB,=A(MRJRECS) VALID RECORD TYPES FOR MAKEGOOD REJ.         
*                                                                               
         USING MOFRREJD,R3                                                      
*                                                                               
         MVC   ORDNUM,MORJORDR                                                  
         MVC   CONTRACT,MORJRPCN                                                
         MVC   RETURN,MORJRTNS                                                  
         MVC   ORIGDATE,MORJDATE                                                
         MVC   ORIGTIME,MORJTIME                                                
         MVC   SENDER,MORJFRID     DDS USERID                                   
         MVC   RECEIVER,MORJTOID                                                
         MVC   OFFERID,MORJOFRI                                                 
         MVC   IDSEQNUM,MORJSEQN                                                
         MVC   STATION,MORJQSTA    STATION                                      
*                                                                               
         GOTO1 =A(FINDMED),(RC)    DERIVE THE MEDIA                             
         CLI   MEDIA,C' '                                                       
         BNE   *+14                                                             
         MVC   ERRCODE,=C'006'     UNKNOWN STATION                              
         B     MKGREJOK                                                         
*                                                                               
         GOTO1 =A(CHKSNDR),(RC)    ADD "-DS" FOR SPECIAL DDS REPS               
         MVC   MORJFRID,SENDER     OVERRIDE THE SENDER                          
*                                                                               
         GOTO1 =A(GETUID),(RC)     LOOK UP NUMERIC                              
         OC    USERID,USERID                                                    
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'007'     UNKNOWN AGENCY                               
         B     MKGREJOK                                                         
*                                                                               
         GOTO1 =A(FINDSTN),(RC)    RETURNS CALL LETTERS IN 'STATION'            
         CLC   STATION,SPACES      WAS STATION FOUND IN TABLE?                  
         BNE   *+14                YES                                          
         MVC   ERRCODE,=C'006'     UNKNOWN STATION                              
         B     MKGREJOK                                                         
*                                                                               
         MVC   MORJQSTA(5),STATION PUT CORRECT CALL LETTERS INTO MKGREJ         
         CLC   OSTATION,SPACES     WAS STATION FOUND IN TABLE?                  
         BE    *+10                YES                                          
         MVC   MORJOLDS(5),OSTATION PUT OLD CALL LETTERS INTO AGYHDR            
*                                                                               
         GOTO1 =A(CHKRCVR),(RC)    MASSAGE RECEIVER IF NECESSARY                
         MVC   MORJTOID,RECEIVER                                                
*                                                                               
         BRAS  RE,FINDTSK          RETURNS SUBTASK ID IN 'SNDTSKID'             
         OC    SNDTSKID,SNDTSKID                                                
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'007'     UNKNOWN AGENCY                               
         B     MKGREJOK                                                         
*                                                                               
         BRAS  RE,CHKSERR                                                       
         BNE   MKGREJOK                                                         
*                                                                               
         LHI   R2,MOFRREJL         L'RECORD                                     
         GOTO1 =A(PUTSMF),(RC)                                                  
*                                                                               
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE MQ WORKQ                
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'MKGREJ'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
MKGREJOK CR    RB,RB               SET CC EQUAL                                 
MKGREJX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
MKGROK   NMOD1 0,MKGROK                                                         
         SAM31                                                                  
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS A MAKEGOOD OK.  LOOK UP THE SENDING ID IN TABLE, SO                   
* WE KNOW TO WHICH MQ WORKQ TO WRITE THE REPORT.                                
*                                                                               
* UPON ENTRY, R3 = A(FIRST RECORD)                                              
*                                                                               
         MVC   ARECTAB,=A(MOKRECS) VALID RECORD TYPES FOR MAKEGOOD OK           
*                                                                               
         USING MOFRCFMD,R3                                                      
*                                                                               
         MVC   ORDNUM,MOCFORDR                                                  
         MVC   CONTRACT,MOCFRPCN                                                
         MVC   RETURN,MOCFRTNS                                                  
         MVC   ORIGDATE,MOCFDATE                                                
         MVC   ORIGTIME,MOCFTIME                                                
         MVC   SENDER,MOCFFRID     DDS USERID                                   
         MVC   RECEIVER,MOCFTOID                                                
         MVC   OFFERID,MOCFOFRI                                                 
         MVC   IDSEQNUM,MOCFSEQN                                                
         MVC   STATION,MOCFQSTA    STATION                                      
*                                                                               
         GOTO1 =A(FINDMED),(RC)    DERIVE THE MEDIA                             
         CLI   MEDIA,C' '                                                       
         BNE   *+14                                                             
         MVC   ERRCODE,=C'006'     UNKNOWN STATION                              
         B     MKGROKOK                                                         
*                                                                               
         GOTO1 =A(CHKSNDR),(RC)    ADD "-DS" FOR SPECIAL DDS REPS               
         MVC   MOCFFRID,SENDER     OVERRIDE THE SENDER                          
*                                                                               
         GOTO1 =A(GETUID),(RC)     LOOK UP NUMERIC                              
         OC    USERID,USERID                                                    
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'007'     UNKNOWN AGENCY                               
         B     MKGROKOK                                                         
*                                                                               
         GOTO1 =A(FINDSTN),(RC)    RETURNS CALL LETTERS IN 'STATION'            
         CLC   STATION,SPACES      WAS STATION FOUND IN TABLE?                  
         BNE   *+14                YES                                          
         MVC   ERRCODE,=C'006'     UNKNOWN STATION                              
         B     MKGROKOK                                                         
*                                                                               
         MVC   MOCFQSTA(5),STATION PUT CORRECT CALL LETTERS INTO MKGROK         
         CLC   OSTATION,SPACES     WAS STATION FOUND IN TABLE?                  
         BE    *+10                YES                                          
         MVC   MOCFOLDS(5),OSTATION PUT OLD CALL LETTERS INTO MKGROK            
*                                                                               
         GOTO1 =A(CHKRCVR),(RC)    MASSAGE RECEIVER IF NECESSARY                
         MVC   MOCFTOID,RECEIVER                                                
*                                                                               
         BRAS  RE,FINDTSK          RETURNS SUBTASK ID IN 'SNDTSKID'             
         OC    SNDTSKID,SNDTSKID                                                
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'007'     UNKNOWN AGENCY                               
         B     MKGROKOK                                                         
*                                                                               
         BRAS  RE,CHKSERR                                                       
         BNE   MKGROKOK                                                         
*                                                                               
         LHI   R2,MOFRCFML         L'RECORD                                     
         GOTO1 =A(PUTSMF),(RC)                                                  
*                                                                               
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE MQ WORKQ                
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'MKGROK'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
MKGROKOK CR    RB,RB               SET CC EQUAL                                 
MKGROKX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
MKGCAN   NMOD1 0,MKGCAN                                                         
         SAM31                                                                  
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS A MAKEGOOD CANCELLATION.  LOOK UP THE SENDING ID IN TABLE, SO         
* WE KNOW TO WHICH MQ WORKQ TO WRITE THE REPORT.                                
*                                                                               
* UPON ENTRY, R3 = A(FIRST RECORD)                                              
*                                                                               
         MVC   ARECTAB,=A(MCNRECS) VALID RECORD TYPES FOR MAKEGOOD CAN          
*                                                                               
         USING MOFRCAND,R3                                                      
*                                                                               
         MVC   ORDNUM,MOCNORDR                                                  
         MVC   CONTRACT,MOCNRPCN                                                
         MVC   RETURN,MOCNRTNS                                                  
         MVC   ORIGDATE,MOCNDATE                                                
         MVC   ORIGTIME,MOCNTIME                                                
         MVC   SENDER,MOCNFRID     DDS USERID                                   
         MVC   RECEIVER,MOCNTOID                                                
         MVC   OFFERID,MOCNOFRI                                                 
         MVC   IDSEQNUM,MOCNSEQN                                                
         MVC   STATION,MOCNQSTA    STATION                                      
*                                                                               
         GOTO1 =A(FINDMED),(RC)    DERIVE THE MEDIA                             
         CLI   MEDIA,C' '                                                       
         BNE   *+14                                                             
         MVC   ERRCODE,=C'006'     UNKNOWN STATION                              
         B     MKGCANOK                                                         
*                                                                               
         GOTO1 =A(CHKSNDR),(RC)    ADD "-DS" FOR SPECIAL DDS REPS               
         MVC   MOCNFRID,SENDER     OVERRIDE THE SENDER                          
*                                                                               
         GOTO1 =A(GETUID),(RC)     LOOK UP NUMERIC                              
         OC    USERID,USERID                                                    
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'007'     UNKNOWN AGENCY                               
         B     MKGCANOK                                                         
*                                                                               
         GOTO1 =A(FINDSTN),(RC)    RETURNS CALL LETTERS IN 'STATION'            
         CLC   STATION,SPACES      WAS STATION FOUND IN TABLE?                  
         BNE   *+14                YES                                          
         MVC   ERRCODE,=C'006'     UNKNOWN STATION                              
         B     MKGCANOK                                                         
*                                                                               
         MVC   MOCNQSTA(5),STATION PUT CORRECT CALL LETTERS INTO MKGCAN         
         CLC   OSTATION,SPACES     WAS STATION FOUND IN TABLE?                  
         BE    *+10                YES                                          
         MVC   MOCNOLDS(5),OSTATION PUT OLD CALL LETTERS INTO MKGCAN            
*                                                                               
         GOTO1 =A(CHKRCVR),(RC)    MASSAGE RECEIVER IF NECESSARY                
         MVC   MOCNTOID,RECEIVER                                                
*                                                                               
         BRAS  RE,FINDTSK          RETURNS SUBTASK ID IN 'SNDTSKID'             
         OC    SNDTSKID,SNDTSKID                                                
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'007'     UNKNOWN AGENCY                               
         B     MKGCANOK                                                         
*                                                                               
         BRAS  RE,CHKSERR                                                       
         BNE   MKGCANOK                                                         
*                                                                               
         LHI   R2,MOFRCANL         L'RECORD                                     
         GOTO1 =A(PUTSMF),(RC)                                                  
*                                                                               
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE MQ WORKQ                
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'MKGCAN'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
MKGCANOK CR    RB,RB               SET CC EQUAL                                 
MKGCANX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
GETUID   NMOD1 0,GETUID                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* GIVEN THE EBCDIC DDS USERID IN FIELD SENDER, PUT THE NUMERIC                  
* USERID IN FIELD USERID.  IF USERID NOT FOUND, RETURN NULLS IN USERID.         
*                                                                               
         XC    USERID,USERID                                                    
*                                                                               
         LA    R3,=C'UIDTABL'                                                   
         MVC   P+30(8),DARE                                                     
         MVC   P+40(7),0(R3)                                                    
         PRNT  Enqueue                                                          
*                                                                               
         SAM24                                                                  
         ENQ   (DARE,(3),E,7)      ENQUEUE THE USERID TABLE                     
*                                                                               
         L     RE,TSKUIDTB         TABLE OF USERIDS/PARTNERS                    
         USING UIDTABD,RE                                                       
GETUID10 CLI   0(RE),X'FF'                                                      
         BE    GETUIDX                                                          
         CLC   SENDER,UIDNAME                                                   
         BE    *+12                                                             
         LA    RE,UIDTBLQ(,RE)     BUMP TO NEXT ENTRY                           
         B     GETUID10                                                         
*                                                                               
         MVC   USERID,UIDNUM       SENDING USERID                               
         DROP  RE                                                               
*                                                                               
GETUIDX  MVC   P+30(8),DARE                                                     
         MVC   P+40(7),0(R3)                                                    
         PRNT  Dequeue                                                          
         DEQ   (DARE,(3),7)        DEQUEUE THE USERID TABLE                     
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WRITES THE MESSAGE BUFFER TO THE MQ WORKQ                        
* IT ALSO VALIDATES THE RECORDS IN THE MESSAGE                                  
*                                                                               
* ON ENTRY:    RECEIVER            SET TO USERID OF THE RECEIVER OF MSG         
*              ARECTAB             SET OF 6.5 RECORDS THAT COMPOSES             
*                                    THIS MESSAGE                               
*                                                                               
* NOTE:  DLNNOT *NEVER* CALLS DRINB *NOR* DOES IT SET ARECTAB                   
***********************************************************************         
DRINB    NMOD1 0,DRINB                                                          
         SAM31                                                                  
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         MVI   BITFLGS1,0          TURN OFF THE BITFLAGS                        
         XC    DS2DMS,DS2DMS                                                    
         MVI   PRIMDEMO,C'N'       ASSUME PRIMARY DEMO IS NIELSEN               
*                                                                               
         L     R3,ARECTAB          PROCESSING AN ERROR?                         
         CLC   =C'ERRNOT',0(R3)    THIS COULD BE TO BUYER OR TO SELLER          
         JE    DRTOS10             NO MODIFICATIONS                             
*                                                                               
         LAY   R1,B2SMSGS                                                       
DRINB05  CLI   0(R1),0             IS MESSAGE A BUYER TO SELLER TYPE?           
         JE    DRINB10                                                          
         CLC   0(L'B2SMSGS,R1),0(R3)                                            
         JE    DRINBTOS            YES, TO A SELLER                             
         LA    R1,L'B2SMSGS(R1)                                                 
         J     DRINB05                                                          
*                                                                               
DRINB10  LAY   R1,S2BMSGS                                                       
DRINB15  CLI   0(R1),0             IS MESSAGE A SELLER TO BUYER TYPE?           
         JE    DRINBUNK            HOW CAN THIS BE NEITHER? UNKNOWN             
         CLC   0(L'S2BMSGS,R1),0(R3)                                            
         JE    DRINBTOB            YES, TO A BUYER                              
         LA    R1,L'S2BMSGS(R1)                                                 
         J     DRINB15                                                          
*                                                                               
DRINBTOS DS    0H                  MSG TO A SELLER                              
         GOTO1 AGETDARE,DMCB,(C'U',0),RECEIVER,A(IO1),IO1LQ,DATAMGR             
         JE    DRINB20             SELLER IS A REP                              
         CLI   3(R1),10            MISSING VENDOR FIELD IN IDI RECORD           
         BE    DRTOS10             SELLER ONLY SUPPORTS DARE 6.5                
*                                                                               
         GOTO1 (RF),DMCB,(C'L',0),RECEIVER,A(IO1),IO1LQ,DATAMGR                 
         JE    DRINB20             SHOULD BE A LOCAL STATION THEN               
*                                                                               
DRTOS10  LAY   R4,IO1                                                           
         USING DAREPTD,R4          R4 = A(GETDARE STORAGE)                      
         XC    DAPTNUMF,DAPTNUMF   PARTNER HAS *NO* DARE 7.0 FEATURES           
         J     DRINB20             SHOULD BE A LOCAL STATION THEN               
*                                                                               
DRINBTOB DS    0H                  MSG TO A BUYER                               
         GOTO1 AGETDARE,DMCB,(C'A',0),RECEIVER,A(IO1),IO1LQ,DATAMGR             
         JNE   DRTOS10             UNKNOWN BUYER, DEFAULT TO 6.5                
*****                                                                           
DRINB20  L     R4,=A(IO1)                                                       
         USING DAREPTD,R4          R4 = A(GETDARE STORAGE)                      
         L     R6,=A(IO2)          IO2 HAS MSGS VALID FOR RECEIVER              
         LR    RE,R6               CLEAR IO2, SO NO GARBAGE                     
         LA    RF,IO2LQ                                                         
         XCEFL                                                                  
*                                                                               
         LA    R1,BPRMDRCS         SETTING ALL 6 PARMS FOR BINSRCH              
         LAY   R2,DR65RECS                                                      
         ST    R2,4(R1)            BINSRCH PARM 2 IS A(BINSRCH TABLE)           
         LA    R3,L'DR65RECS                                                    
         ST    R3,12(R1)           BINSRCH PARM 4 IS L'RECORD                   
         ST    R3,16(R1)           BINSRCH PARM 5 IS D'KEY AND L'KEY            
         LAY   RF,DR65RECX         CALCULATE # OF ENTRIES                       
         SR    RF,R2                                                            
         XR    RE,RE                                                            
         DR    RE,R3                                                            
         ST    RF,8(R1)            BINSRCH PARM 3 IS # OF ENTRIES               
         LA    RF,1(RF)            SO WE DON'T GET "TABLE IS FULL"              
         ST    RF,20(R1)           BINSRCH PARM 6 IS MAX # OF ENTRIES           
*                                                                               
         L     R3,ARECTAB          R3=A(SUPERSET OF MSGS FOR TRANS)             
         LR    R2,R3               R2 WILL BE USED FOR CONTEXT CHECK            
*                                                                               
DRINB23  CLI   0(R3),X'FF'         GONE THRU ALL LINES IN ARECTAB?              
         JE    DRINB30             YES, DONE WITH 6.5 MSGS                      
*                                                                               
         ST    R3,BPRMDRCS         BINSRCH PARM 1 IS A(KEY) TO LOOK FOR         
         L     RF,=V(BINSRCH)                                                   
         GOTO1 (RF),BPRMDRCS       ENTRY IN ARECTAB MEMBER OF DARE 6.5?         
         CLI   0(R1),X'01'                                                      
         JE    DRINB26             NOT A MEMBER OF DARE 6.5 SET                 
*                                                                               
         MVC   0(L'DR65RECS,R6),0(R3)  RECEIVER CAN HANDLE THIS MSG             
         LA    R6,L'DR65RECS(R6)                                                
*                                                                               
DRINB26  LA    R3,L'DR65RECS(R3)   NEXT ENTRY IN ARECTAB                        
         J     DRINB23                                                          
*****                                                                           
* BASICALLY IT'LL MOVE ALL MSG LINES INTO IO2 FOR FEATURES THAT MATCH           
* OUR CONTEXT THAT THE PARTNER SUPPORTS                                         
*****                                                                           
DRINB30  OC    DAPTNUMF,DAPTNUMF   PARTNER HAS ANY DARE 7.0 FEATURES?           
         JZ    DRINB40             NONE, ONLY SUPPORTS DARE 6.5                 
*                                                                               
         XR    RF,RF               RF USED TO LOOP THROUGH FEATURES             
         ICM   RF,3,DAPTNUMF                                                    
*                                                                               
         LA    R4,DAPTFEAT                                                      
         USING DAPTFEAT,R4                                                      
DRINB31  XR    RE,RE               RE USED TO LOOP THRU MESSAGES                
         ICM   RE,3,DAPTNUMM          FOR EACH FEATURE                          
         LA    R3,DAPTTEXT                                                      
DRINB33  OC    0(L'DAPTTEXT,R3),0(R3)  ANY MESSAGE HERE?                        
         JZ    DRINB40                 NO, WE'RE DONE                           
         CLC   DAPTCTXT,0(R2)          CONTEXT IS SAME AS ARECTAB?              
         JE    DRINB39                    YES, SAME AS LINE?                    
DRINB36  LA    R3,DAPTMLNQ(R3)     NEXT MSG THAT PARTNER SUPPORTS               
         JCT   RE,DRINB33                                                       
*                                                                               
         LR    R4,R3               AFTER LAST MESSAGE IS ANOTHER FEATR          
         JCT   RF,DRINB31          UNTIL ALL FEATURES ARE PROCESSED             
         J     DRINB40                                                          
*                                                                               
DRINB39  MVC   0(L'DAPTTEXT,R6),0(R3)  MESSAGE BELONGS TO THE CONTEXT           
         LA    R6,L'DR65RECS(R6)                                                
         J     DRINB36              NEXT MSG ENTRY PARTNER SUPPORTS             
*****                                                                           
DRINB40  MVI   0(R6),X'FF'          DONE BUILDING IO2                           
         DROP  R4                                                               
*                                                                               
DRINB45  XC    TLRCOUNT,TLRCOUNT   NO TRAILER RECORD COUNT YET                  
         XC    MSGCOUNT,MSGCOUNT   NO TOTAL RECORD COUNT YET                    
         XC    RECCOUNT,RECCOUNT   NO RECORD COUNT YET                          
         XC    ADJCOUNT,ADJCOUNT   NO ADJUSTED RECORD COUNT YET                 
         XC    DS2DMS,DS2DMS       TO AVOID ANY PREVIOUSLY SAVED DEMO           
*** THESE ARE USED FOR XML AGYHDR                                               
         MVI   ORDRFLG1,0          RESET ORDER FLAGS                            
         ZAP   ATLRTDOL,=P'0'      CLEAR OUT TOTAL DOLLARS                      
         ZAP   ATLRTSPT,=P'0'      CLEAR OUT TOTAL SPOTS                        
*                                                                               
         L     R3,ADAREBUF         BEGINNING OF MESSAGE                         
DRINB50  LR    R4,R3               R4 = A(THIS RECORD)                          
DRINB53  TRT   0(256,R3),TRINBND   LOOK FOR NON-PRINTING CHARACTERS             
         LR    R2,R1                                                            
         CLI   0(R2),X'FF'         END OF MESSAGE?                              
         JNE   DRINB54                                                          
         CLC   =C'+++DARE END ',0(R4)   R4 = A(THIS RECORD)                     
         JNE   DRINB54                                                          
         MVI   2(R2),X'FF'                                                      
         MVC   0(L'CRLF,R2),CRLF                                                
*                                                                               
DRINB54  CLC   CRLF,0(R2)          DID WE HIT A CRLF?                           
         JE    DRINB56             YES                                          
         MVI   0(R2),C' '          REPLACE INVALID CHARS `{}º W/BLANK          
         LA    R3,1(R2)                                                         
         J     DRINB53                                                          
*                                                                               
DRINB56  LA    R3,2(R2)            R3 = A(NEXT RECORD)                          
*                                                                               
         L     RE,MSGCOUNT                                                      
         AHI   RE,1                INCREMENT RECORD COUNT                       
         ST    RE,MSGCOUNT                                                      
*                                                                               
         CLC   =C'TLR',3(R4)       WAS THIS A TRAILER RECORD?                   
         JE    DRINB80             YES, GET COUNT FROM WITHIN TRAILER           
*                                                                               
DRINB59  CLC   =C'+++DARE END ',0(R4)  WAS THIS THE LAST LINE?                  
         JE    DRINB85             YES                                          
*                                                                               
         CLC   =C'+++DARE SEND ',0(R4) WAS THIS THE FIRST LINE?                 
         JE    DRINB50             YES, WE CAN IGNORE                           
         CLC   =C'+++DARE LOGGING=',0(R4)                                       
         JE    DRINB50             YES, WE CAN IGNORE                           
*                                                                               
         L     RF,ARECTAB          TBL OF VALID RECORDS FOR THE MESSAGE         
DRINB60  CLI   0(RF),X'FF'                                                      
         JE    DRINBUNK            UNKNOWN RECORD TYPE                          
         CLC   0(6,R4),0(RF)       MATCH ON RECORD TYPE?                        
         JE    DRINB62                                                          
         LA    RF,6(RF)            BUMP TO NEXT ENTRY                           
         J     DRINB60                                                          
*                                                                               
DRINB62  L     RE,RECCOUNT                                                      
         AHI   RE,1                INCREMENT RECORD COUNT                       
         ST    RE,RECCOUNT                                                      
*                                                                               
         CLC   =C'AGYHDR',0(R4)    CURRENT MESSAGE LINE IS AGYHDR?              
         JE    DRINB63X            YES                                          
*                                                                               
         TM    ORDRFLG1,ORDF1XML   X'80' - AN XML AGYHDR?                       
         JZ    DRINB63Z            NO, DON'T WORRY ABOUT TOTALS                 
         CLC   =C'BUYHDR',0(R4)                                                 
         JE    DRINB62H                LOOK AT BUYLINE LEVEL COST               
         CLC   =C'BUYDTL',0(R4)                                                 
         JE    DRINB62D                LOOK AT BUY DETAIL LINE                  
         CLC   =C'AGYTLR',0(R4)                                                 
         JNE   DRINB63Z                                                         
*                                                                               
* PROCESSING AGYTLR                                                             
         OI    ATLRTSPT+L'ATLRTSPT-1,X'0F'                                      
         UNPK  PATLSPTS-PAGYTLRD(L'PATLSPTS,R4),ATLRTSPT                        
         OI    ATLRTDOL+L'ATLRTDOL-1,X'0F'                                      
         UNPK  PATLTOTL-PAGYTLRD(L'PATLTOTL,R4),ATLRTDOL                        
         J     DRINB63Z                                                         
*                                                                               
* PROCESSING BUYDTL                                                             
DRINB62D CLC   =C'00',PBDTNOWK-PBUYDTLD(R4)    0 WEEKS?                         
         JE    DRINB63Z                                                         
         CLC   =C'00',PBDTSPTS-PBUYDTLD(R4)       -OR-  0 SPOTS?                
         JE    DRINB63Z                        YES, NO $ FOR THESE WKS          
*                                                                               
         CLC   SPACES(L'PBDTCOST),PBDTCOST-PBUYDTLD(R4)  ANY RATE?              
         JE    DRINB62E                              NO, USE BUYHDR'S           
         PACK  DUB,PBDTCOST-PBUYDTLD(L'PBDTCOST,R4)                             
         CVB   RE,DUB                                                           
         ST    RE,BDTLRATE         SAVE OFF THE BUYDTL RATE                     
         J     DRINB62F                                                         
*                                                                               
DRINB62E MVC   BDTLRATE,BHDRRATE   WHEN NO BUYDTL RATE, USE BUYHDR'S            
*                                                                               
DRINB62F PACK  DUB,PBDTNOWK-PBUYDTLD(L'PBDTNOWK,R4)                             
         CVB   R0,DUB              R0 = # OF WEEKS                              
         PACK  DUB,PBDTSPTS-PBUYDTLD(L'PBDTSPTS,R4)                             
         CVB   R1,DUB              R1 = # OF SPOTS/WEEK                         
         XR    RE,RE                                                            
         LR    RF,R0                                                            
         MR    RE,R1               RF = TOTAL # OF SPOTS IN BUYDTL              
         LR    R1,RF               MAKE A COPY IN R1                            
         CVD   RF,DUB                                                           
         AP    ATLRTSPT,DUB        ADD TO THE RUNNING TOTAL OF SPOTS            
*                                                                               
         XR    RE,RE               CALC THE TOTAL $ FOR THIS BUYDTL             
         L     RF,BDTLRATE         RATE PER SPOT                                
         MR    RE,R1               TIMES NUMBER OF SPOTS                        
         CVD   RF,DUB                                                           
         AP    ATLRTDOL,DUB        ADD TO THE RUNNING TOTAL OF DOLLARS          
         J     DRINB63Z                                                         
*                                                                               
* PROCESSING BUYHDR                                                             
DRINB62H PACK  DUB,PBHDCOST-PBUYHDRD(L'PBHDCOST,R4)                             
         CVB   RE,DUB                                                           
         ST    RE,BHDRRATE         SAVE OFF THE BUYHDR RATE                     
         J     DRINB63Z                                                         
*                                                                               
DRINB63X CLI   PAHDREVN-PAGYHDRD(R4),C'+'   XML ORDER?                          
         JNE   DRINB63Z                     NO                                  
         OI    ORDRFLG1,ORDF1XML            YES, SET THE FLAG FOR LATER         
******                                                                          
* NOW CHECK AGAINST LIST OF MESSAGES RECEIVER IS SUBSCRIBED TO                  
******                                                                          
DRINB63Z L     RF,=A(IO2)          TBL OF VALID RECORDS FOR RECEIVER            
DRINB64  CLI   0(RF),X'FF'                                                      
         JE    DRINB70             UNKNOWN TO RECEIVER, NEED TO STRIP           
         CLC   0(6,R4),0(RF)       MATCH ON RECORD TYPE?                        
         JE    DRINB66                                                          
         LA    RF,6(RF)            BUMP TO NEXT ENTRY                           
         J     DRINB64                                                          
*                                                                               
DRINB66  L     RE,ADJCOUNT         THIS MESSAGE LINE IS VALID FOR RCVR          
         AHI   RE,1                INCREMENT RECORD COUNT                       
         ST    RE,ADJCOUNT                                                      
***************                                                                 
* SUBSCRIBED MESSAGE POST-PROCESSING                                            
***************                                                                 
         BRAS  RE,MSGPOSTP         DO SOME MESSAGE POST PROCESSING              
         JE    DRINB50             EVERYTHING IS FINE                           
         CLC   =C'MKGDEM',0(R4)    NOT SO GOOD, MAKEGOOD DEMO VALUES?           
         JE    BADCNVDM            BAD DEMO CONVERSION                          
         J     DRINB50                                                          
***********************************                                             
* MESSAGE LINE MIGHT NEED TO BE STRIPPED AS IT IS NOT AVAILABLE FOR THE         
* RECEIVER OF THIS MESSAGE CONTEXT                                              
***********************************                                             
DRINB70  CLC   =C'AGYCDC',0(R4)    STRIP COMSCORE DEMO CATEGORY LINE?           
         JNE   DRINB72                                                          
         CLI   PRIMDEMO,C'N'                                                    
         JE    *+8                                                              
         MVI   PRIMDEMO,0          CLEAR PRIMARY DEMO SOURCE SO BUYCOMS         
         NI    BITFLGS1,X'FF'-BF1ACDC  NO LONGER HAVE A AGYCDC                  
         J     DRINB75             WITH COMSCORE WILL APPEAR                    
                                                                                
DRINB72  CLC   =C'MKGDC2',0(R4)    WE HAVE A MKGDC2 TO CONVERT/STRIP?           
         JE    DRINB70M            YES, IT MIGHT NOT BE STRIPPED                
*                                                                               
         CLC   =C'BUYDV2',0(R4)    WE HAVE A BUYDV2 TO CONVERT?                 
         JE    DRINB70A                                                         
         CLC   =C'BUYDM2',0(R4)        OR  A BUYDM2 TO CONVERT?                 
         JE    DRINB70A            YES, IT SHALL NOT BE STRIPPED                
********                                                                        
* LINE IS TO BE STRIPPED FROM MESSAGE                                           
********                                                                        
DRINB75  BRAS  RE,STRIPLN                                                       
         J     DRINB50                                                          
***************                                                                 
* WE HAVE A TRADITIONAL DEMO CATEGORY AT THIS POINT                             
***************                                                                 
DRINB70A TM    BITFLGS1,BF1SBATDM   RECEIVER SUBSCRIBED TO AGYTDM?              
         JNZ   DRINB70B             YES, OKAY TO DOWN-CONVERT                   
         CLI   PRIMDEMO,C'N'        PRIMARY DEMO IS NIELSEN?                    
         JNE   DRINB75              NO, NO NEED TO DOWN-CONVERT, STRIP          
*                                                                               
DRINB70B CLC   =C'BUYDV2',0(R4)    WE HAVE A BUYDV2 TO CONVERT?                 
         JE    DRINB70G            YES, IT SHALL NOT BE STRIPPED                
*                                                                               
         CLC   =C'BUYDM2',0(R4)    WE HAVE A BUYDM2 TO CONVERT?                 
         JNE   DRINB75             YES, IT SHALL NOT BE STRIPPED                
DRINB70C MVC   P+11(20),=C'!! DOWN CONVERTED !!'                                
         GOTO1 =V(PRINTER)                                                      
         MVC   P(PBUYDM2L),0(R4)                                                
         MVC   P+PBUYDM2L(5),=C' ==> '                                          
*                                                                               
         GOTO1 CNVDMVLS,DMCB,(0,=C'BUYDEM')                                     
         JNE   BADCNVDM            BAD DEMO CONVERSION                          
         L     R1,DMCB+4           PARAM 2 IS SET TO L'CONVERTED MSG            
         MVC   P+PBUYDM2L+5(PBUYDEML),0(R4)                                     
         GOTO1 =V(PRINTER)                                                      
         LA    R4,2(R1,R4)         R4 = A(BYTE AFTER CRLF)                      
*                                                                               
DRIB70CX LR    R0,R4               SET ADDRESS AND LENGTH OF DEST               
         L     R1,ADAREBUF           FOR THE MVCL                               
         SR    R0,R1               R1 = # OF BYTES FROM START OF TRANS          
         L     R1,BUFFERLENGTH2                                                 
         SR    R1,R0               R1 = # OF BYTES TO END OF TRANS              
         LR    R0,R4               RESET ADDRESS OF DEST                        
*                                                                               
         LR    RE,R3               SET ADDRESS AND LENGTH OF SOURCE             
         L     RF,ADAREBUF           FOR THE MVCL                               
         SR    RE,RF               RF = # OF BYTES FROM START OF TRANS          
         L     RF,BUFFERLENGTH2                                                 
         SR    RF,RE               RF = # OF BYTES TO END OF TRANS              
         LR    RE,R3               RESET ADDRESS OF SOURCE                      
         MVCL  R0,RE               DAREBUF NO LONGER HAS BUYDM2                 
*                                                                               
         L     RE,ADJCOUNT         THIS LINE COUNTS AS A VALID LINE NOW         
         AHI   RE,1                INCREMENT RECORD COUNT                       
         ST    RE,ADJCOUNT                                                      
*                                                                               
         LR    R3,R4               R3 IS NOW THE NEXT LINE AFTER BUYDM2         
         J     DRINB50                                                          
*                                                                               
BADCNVDM DS    0H                  UNEXPECTED BAD DEMO CONVERSION               
         J     DRINB50             LEAVE MSG ALONE, IT'LL ERROR OUT             
***************                                                                 
* BUYDV2 NEEDS TO BE DOWN CONVERTED TO EITHER BUYDM2 OR BUYDEM                  
***************                                                                 
DRINB70G MVC   P+11(20),=C'!! DOWN CONVERTED !!'                                
         GOTO1 =V(PRINTER)                                                      
         MVC   P(PBUYDV2L),0(R4)                                                
         MVC   P+PBUYDV2L(5),=C' ==> '                                          
*                                                                               
         L     RF,=A(IO2)          TBL OF VALID RECORDS FOR RECEIVER            
DRIB70GL CLI   0(RF),X'FF'         DO WE HAVE BUYDM2?                           
         JE    DRIB70G1            NO, ALL VALUES TO 1 DEC (BUYDEM)             
         CLC   =C'BUYDM2',0(RF)                                                 
         JE    DRIB70GR            YES, 2 DEC RATINGS, 1 DEC IMPS               
         LA    RF,6(RF)                                                         
         J     DRIB70GL                                                         
*********                                                                       
* CONVERTING BUYDV2 TO BUYDEM, ALL DEMO VALUES FROM 2 DECIMALS TO 1 DEC         
*********                                                                       
DRIB70G1 GOTO1 CNVDMVLS,DMCB,(0,=C'BUYDEM')                                     
         JNE   BADCNVDM            BAD DEMO CONVERSION                          
         L     R1,DMCB+4           PARAM 2 IS SET TO L'CONVERTED MSG            
         MVC   P+PBUYDV2L+5(PBUYDEML),0(R4)                                     
         GOTO1 =V(PRINTER)                                                      
         LA    R4,2(R1,R4)         R4 = A(BYTE AFTER CRLF)                      
         J     DRIB70CX                                                         
*********                                                                       
* CONVERTING BUYDV2 TO BUYDM2, ALL RATINGS REMAIN AS 2 DECIMALS BUT             
*    IMPRESSIONS ARE TO 1 DECIMAL                                               
*********                                                                       
DRIB70GR GOTO1 CNVDMVLS,DMCB,(0,=C'BUYDM2')                                     
         JNE   BADCNVDM            BAD DEMO CONVERSION                          
         L     R1,DMCB+4           PARAM 2 IS SET TO L'CONVERTED MSG            
         MVC   P+PBUYDV2L+5(PBUYDM2L),0(R4)                                     
         GOTO1 =V(PRINTER)                                                      
         LA    R4,2(R1,R4)         R4 = A(BYTE AFTER CRLF)                      
         J     DRIB70CX                                                         
***************                                                                 
* MKGDC2 NEEDS TO BE EITHER DOWN CONVERTED TO MKGDS2 OR STRIPPED                
***************                                                                 
DRINB70M L     RF,=A(IO2)          TBL OF VALID RECORDS FOR RECEIVER            
DRIB70ML CLI   0(RF),X'FF'         DO WE HAVE MKGDS2?                           
         JE    DRINB75             NO, LINE NEEDS TO BE STRIPPED                
         CLC   =C'MKGDS2',0(RF)                                                 
         JE    DRIB70MC            YES, 2 DEC RATINGS, 1 DEC IMPS               
         LA    RF,6(RF)                                                         
         J     DRIB70ML                                                         
*                                                                               
DRIB70MC MVC   P+11(20),=C'!! DOWN CONVERTED !!'                                
         GOTO1 =V(PRINTER)                                                      
         MVC   P(MOFRDC2L),0(R4)                                                
         MVC   P+MOFRDC2L(5),=C' ==> '                                          
*                                                                               
         BRAS  RE,CNVRTMDC2        R4 = A(MKGDC2 MSG LINE)                      
         MVC   P+MOFRDC2L+5(MOFRDS2L),0(R4)                                     
         GOTO1 =V(PRINTER)                                                      
         LA    R4,MOFRDS2L+2(R4)   R4 = A(BYTE AFTER CRLF)                      
         J     DRIB70CX                                                         
***************                                                                 
* WE ENCOUNTERED A ???TLR                                                       
***************                                                                 
DRINB80  PACK  DUB,14(6,R4)        GET COUNT IN TRAILER RECORD                  
         CVB   R0,DUB                                                           
         ST    R0,TLRCOUNT                                                      
         J     DRINB59             NOW RECCOUNT AND ADJCOUNT                    
***************                                                                 
* WE'RE AT THE  +++DARE END                                                     
***************                                                                 
DRINB85  L     RF,ARECTAB                                                       
         LAY   R1,MSGNOTLR         A MESSAGE TYPE WITHOUT A TRAILER?            
DRINB90  CLI   0(R1),0                                                          
         JE    DRINB94             NOT IN THIS LIST, SO NEED A TLR              
         CLC   0(L'MSGNOTLR,R1),0(RF)                                           
         JE    DRINB96             YES - NO TRAILER NEEDED                      
         LA    R1,L'MSGNOTLR(R1)                                                
         J     DRINB90                                                          
*                                                                               
DRINB92  LAY   R1,MSGSINGL         CAN MESSAGE THAT NORMALLY HAS TLR            
DRINB92A CLI   0(R1),0               ALSO BE ALLOWED AS A SINGLE LINE?          
         JE    DRINBMTL            NOT THIS MESSAGE, MISSING TLR REC            
         CLC   0(L'MSGNOTLR,R1),0(RF)                                           
         JE    DRINB96             YES - NO TLR NEEDED, SPECIAL                 
         LA    R1,L'MSGSINGL(R1)                                                
         J     DRINB92A                                                         
*                                                                               
DRINB94  OC    TLRCOUNT,TLRCOUNT   WAS A TRAILER RECORD SEEN?                   
         JZ    DRINB92             NO, WE MIGHT BE MISSING A TLR REC            
*                                                                               
         CLC   TLRCOUNT,RECCOUNT                                                
         JNE   DRINBTCN            TRAILER COUNT MISMATCH                       
*                                                                               
DRINB96  PACK  DUB,30(6,R4)        RECORD COUNT IN '+++DARE END'                
         CVB   R0,DUB                                                           
         C     R0,MSGCOUNT                                                      
         JNE   DRINBECN            ENVELOPE RECORD COUNT MISMATCH               
*                                                                               
         CLC   RECCOUNT,ADJCOUNT   ANY ADJUSTMENTS?                             
         JE    DRINB97             NONE TO MAKE                                 
         L     R0,ADJCOUNT                                                      
         L     R1,RECCOUNT                                                      
         SR    R0,R1                                                            
         L     R1,MSGCOUNT                                                      
         AR    R0,R1               THIS IS TOTAL MSG COUNT AFTER ADJ            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  30(6,R4),DUB        MODIFY THE DARE END COUNT                    
*                                                                               
DRINB97  PRNT  EndInbound                                                       
*                                                                               
         L     R1,ADAREBUF         R1 = A(MESSAGE BUFFER)                       
         L     R2,BUFFERLENGTH2    R2 = MESSAGE LENGTH                          
         LA    R3,RECEIVER         R3 = A(RECEIVER NAME)                        
         MVC   HALF,RCVIDNUM       HALF = RECEIVER USERID#                      
         SAM24                                                                  
         BRAS  RE,PUTWRKQ          PUT THIS MQ MESSAGE TO WORKQ                 
         MVI   PUTMSGFL,C'Y'       THIS MESSAGE IS PUT TO WORKQ!                
         J     DRINBX                                                           
*                                                                               
DRINBDEM MVC   P+30(19),=C'+++DARE END MISSING'                                 
         J     DRINBAD                                                          
*                                                                               
DRINBUNK MVC   ERRCODE,=C'002'     UNKNOWN RECORD TYPE                          
         MVC   P+30(19),=C'UNKNOWN RECORD TYPE'                                 
****     DC    H'0'       **********DEBUGGING****** SHOULDN'T HAPPEN            
         J     DRINBAD                                                          
*                                                                               
DRINBMTL MVC   ERRCODE,=C'004'     MISSING TRAILER RECORD                       
         MVC   P+30(22),=C'MISSING TRAILER RECORD'                              
         J     DRINBAD                                                          
*                                                                               
DRINBTCN MVC   ERRCODE,=C'001'     RECORD COUNT MISMATCH                        
         MVC   P+30(21),=C'RECORD COUNT MISMATCH'                               
         J     DRINBAD                                                          
*                                                                               
DRINBECN MVC   OPMSG6+63(3),TSKNAME                                             
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMSG6,OPMSG6)                     
         MVC   P+30(30),=C'ENVELOPE RECORD COUNT MISMATCH'                      
         J     DRINBAD             MISMATCH ON ENVELOPE RECORD COUNT            
*                                                                               
DRINBAD  MVC   P+11(13),=C'*** ERROR ***'                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
DRINBX   XIT1                                                                   
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* IN PLACE REPLACEMENT OF THE MKGDC2 MESSAGE LINE TO MKGDS2                     
*                                                                               
* ON ENTRY:    (R4)                A(BUYDC2 MESSAGE LINE)                       
***********************************************************************         
CNVRTMDC2 NTR1                     CONVERT 2 DECIMAL RATINGS DOWN               
CMDC2C2D USING MOFRDC2D,R4                                                      
CMDC2DSD USING MOFRDS2D,R4                                                      
         OI    BITFLGS1,BF1DC2DS2    SO WE KNOW TO MODIFY 'MKGDEM'              
*                                                                               
         MVC   CMDC2DSD.MOD2TID,=C'MKGDS2'  MKGDC2 --> MKGDS2                   
         MVC   CMDC2DSD.MOD2ORDR,CMDC2C2D.MODC2ORDR                             
         MVC   WORK(L'MODC2TDEM*4),CMDC2C2D.MODC2TDEM                           
         MVC   CMDC2DSD.MOD2TDEM(L'MODC2TDEM*4),WORK                            
         MVI   CMDC2DSD.MOD2TWOD,C'Y'                                           
         LA    R3,CMDC2DSD.MOD2TDM4+L'MOD2TDM4                                  
         MVC   0(2,R3),=X'0D25'      CRLF                                       
*                                                                               
         USING MOFRDS2D,R4         SAVE OFF THE DEMO CATEGORY TYPES             
CMDC2D10 LA    R1,DS2DMS           5TH BYTE (X'00') IS FOR END OF LIST          
         LA    RE,CMDC2DSD.MOD2TDEM  1ST DEMO CATEGORY                          
         LA    RF,CMDC2DSD.MOD2TDM4  4TH AND LAST DEMO CATEGORY                 
CMDC2D20 CR    RE,RF               PASS THE 4TH AND LAST CATEGORY?              
         BH    CMDC2X              YES                                          
         MVC   0(1,R1),0(RE)       SAVE OFF THIS DEMO CATEGORY TYPE             
         LA    R1,1(R1)            LOOP THROUGH ALL 4 CATEGORIES                
         LA    RE,L'MOD2TDEM(RE)   WE'LL EVEN SAVE OFF SPACE IF NO              
         J     CMDC2D20              CATEGORY                                   
*                                                                               
CMDC2X   J     DRINBX                                                           
         DROP  CMDC2C2D,CMDC2DSD                                                
                                                                                
***********************************************************************         
* CONVERTS A DEMO VALUE MESSAGE LINE TO AN OLDER DEMO VALUE MESSAGE             
* MKGDEM --> MKGDEM (2 DEC IMPS -> 1 DEC IMPS)                                  
* BUYDV2 --> BUYDM2  -OR-  BUYDEM                                               
* BUYDM2 --> BUYDEM                                                             
*                                                                               
* ON ENTRY:    (R4)                A(CURRENT DEMO MESSAGE LINE)                 
*              PARAM 1             A(OLDER MESSAGE TYPE)                        
* ON EXIT:     PARAM 2             L(OLDER MESSAGE TYPE)                        
***********************************************************************         
CNVDMVLS NTR1                                                                   
*----------------------------------------------------------------------         
* WHAT WE ARE CONVERTING FROM                                                   
*----------------------------------------------------------------------         
         CLC   =C'MKGDEM',0(R4)    MKGD DEMO 2 DEC IMPS?                        
         JE    CDMV100             YES, CHANGE THEM TO 1 DEC IMPS               
*                                                                               
         CLC   =C'BUYDV2',0(R4)    ARE ALL DEMO VALUES 2 DECIMALS?              
         JE    CDMV50              YES, BUYDV2                                  
*                                                                               
         CLC   =C'BUYDM2',0(R4)    NO, IS IT 2 DEC RTGS 1 DEC IMPS?             
         JNE   CDMVNO              LEAVE THE MESSAGE ALONE                      
*----------------------------------------------------------------------         
* WHAT WE ARE CONVERTING TO                                                     
*----------------------------------------------------------------------         
         L     RF,0(R1)            FIND OUT WHAT WE'RE CONVERTING TO            
         CLC   =C'BUYDEM',0(RF)    WILL IT BE  BUYDM2 -> BUYDEM?                
         JNE   CDMVNO                 NO, LEAVE THE MESSAGE ALONE               
*----------------------------------------------------------------------         
* BUYDM2 -> BUYDEM  (ALL DEMO VALUES ARE 1 DEC)                                 
* BUT ALSO 8 DIGIT VALUES TO 7 DIGIT VALUES                                     
*----------------------------------------------------------------------         
CDMVD1   USING PBUYDEMD,R4          1 DEC RTGS AND 1 DEC IMPS                   
CDMVD2   USING PBUYDM2D,R4          2 DEC RTGS AND 1 DEC IMPS                   
*                                                                               
         MVC   CDMVD1.PBDMTID,=C'BUYDEM'    BUYDM2 --> BUYDEM                   
         LA    R2,DS2DMS           DEMO CAT TYPES FROM AGYDS2 LINE              
         LA    R3,CDMVD1.PBDMVAL                                                
         LA    R6,CDMVD2.PBD2VAL                                                
*                                                                               
CDMV10   CLI   0(R2),0             DONE WITH ALL 4 DEMO CATEGORIES?             
         JE    CDMVYES             YES                                          
         CLI   0(R2),C' '          NO CATEGORY?                                 
         JE    CDMVYES             YES                                          
*                                                                               
         LR    RF,R6               HAVE A CATEGORY, SO POINT TO VALUE           
         LA    RE,L'PBD2VAL        IT SHOULD HAVE THIS MANY DIGITS              
CDMV15   CLI   0(RF),C'0'                                                       
         JL    CDMVSP              NOT A VALID DEMO VALUE, SPACE FILL           
         LA    RF,1(RF)                                                         
         JCT   RE,CDMV15                                                        
*                                                                               
         CLI   0(R2),C'I'          AN IMPRESSION VALUE?                         
         JE    CDMVIM                                                           
         CLI   0(R2),C'R'          A RATING VALUE?                              
         JE    CDMVRT                                                           
         CLI   0(R2),C'E'          AN EXTENDED RATING VALUE?                    
         JE    CDMVRT                                                           
*                                                                               
CDMVIM   MVC   0(L'PBDMVAL,R3),1(R6)  FOR IMPRESSION, JUST REMOVE THE           
         J     CDMV20                   LEADING ZERO                            
*                                                                               
CDMVRT   PACK  DUB,0(L'PBD2VAL,R6)  GET THE RATING VALUE                        
         SRP   DUB,64-1,5           DIVIDE BY 10 AND ROUND IF NECESSARY         
         OI    DUB+7,X'0F'                                                      
         UNPK  0(L'PBDMVAL,R3),DUB                                              
         J     CDMV20                                                           
*                                                                               
CDMVSP   MVC   0(L'PBDMVAL,R3),SPACES NO DEMO CATEGORY, SO SPACE FILL           
*                                                                               
CDMV20   LA    R2,1(R2)              BUMP TO NEXT CATEGORY TYPE                 
         LA    R3,L'PBDMVAL(R3)      BUMP TO NEXT 1 DEC DEMO VALUE              
         LA    R6,L'PBD2VAL(R6)      BUMP TO NEXT 2 DEC DEMO VALUE              
         J     CDMV10                CONVERT THE NEXT DEMO VALUE                
         DROP  CDMVD1,CDMVD2                                                    
*----------------------------------------------------------------------         
* INCOMING IS BUYDV2                                                            
*----------------------------------------------------------------------         
CDMV50   L     RF,0(R1)            FIND OUT WHAT WE'RE CONVERTING TO            
         CLC   =C'BUYDM2',0(RF)      BUYDV2 -> BUYDM2?                          
         JE    CDMV70                  2 DEC RTGS AND 1 DEC IMPS                
         CLC   =C'BUYDEM',0(RF)      BUYDV2 -> BUYDEM?                          
         JNE   CDMVNO                NO, EXIT AS WE DON'T KNOW                  
*----------------------------------------------------------------------         
* BUYDV2 -> BUYDEM  (ALL DEMO VALUES ARE 1 DEC)                                 
* BUT ALSO 8 DIGIT VALUES TO 7 DIGIT VALUES                                     
*----------------------------------------------------------------------         
CDMVAD1  USING PBUYDEMD,R4          1 DEC RTGS AND 1 DEC IMPS                   
CDMVAD2  USING PBUYDV2D,R4          2 DEC RTGS AND 2 DEC IMPS                   
*                                                                               
         MVC   CDMVAD1.PBDMTID,=C'BUYDEM'   BUYDV2 --> BUYDEM                   
         LA    R2,DS2DMS           DEMO CAT TYPES FROM AGYDS2 LINE              
         LA    R3,CDMVAD1.PBDMVAL                                               
         LA    R6,CDMVAD2.PBDV2VAL                                              
*                                                                               
CDMV53   CLI   0(R2),0             DONE WITH ALL 4 DEMO CATEGORIES?             
         JE    CDMVYES             YES                                          
         CLI   0(R2),C' '          NO CATEGORY?                                 
         JE    CDMV59              NONE, THEN SPACES                            
*                                                                               
         LR    RF,R6               HAVE A CATEGORY, SO POINT TO VALUE           
         LA    RE,L'PBDV2VAL       IT SHOULD HAVE THIS MANY DIGITS              
CDMV55   CLI   0(RF),C'0'                                                       
         JL    CDMV59              NOT A VALID DEMO VALUE, SPACE FILL           
         LA    RF,1(RF)                                                         
         JCT   RE,CDMV55                                                        
*----------------------------------------------------------------------         
* 8 DIGIT VALUES --> 7 DIGIT VALUES                                             
*----------------------------------------------------------------------         
         PACK  DUB,0(L'PBD2VAL,R6) GET THE VALUE                                
         SRP   DUB,64-1,5          DIVIDE BY 10 AND ROUND IF NECESSARY          
         OI    DUB+7,X'0F'                                                      
         UNPK  0(L'PBDMVAL,R3),DUB                                              
         J     CDMV60                                                           
*                                                                               
CDMV59   MVC   0(L'PBDMVAL,R3),SPACES NO DEMO CATEGORY, SO SPACE FILL           
*                                                                               
CDMV60   LA    R2,1(R2)              BUMP TO NEXT CATEGORY TYPE                 
         LA    R3,L'PBDMVAL(R3)      BUMP TO NEXT 1 DEC DEMO VALUE              
         LA    R6,L'PBDV2VAL(R6)     BUMP TO NEXT 2 DEC DEMO VALUE              
         J     CDMV53                CONVERT THE NEXT DEMO VALUE                
         DROP  CDMVAD1,CDMVAD2                                                  
*----------------------------------------------------------------------         
* BUYDV2 -> BUYDM2                                                              
* 8 DIGIT VALUES TO 8 DIGIT VALUES                                              
*----------------------------------------------------------------------         
CDMVRD1  USING PBUYDM2D,R4          2 DEC RTGS AND 1 DEC IMPS                   
CDMVRD2  USING PBUYDV2D,R4          2 DEC RTGS AND 2 DEC IMPS                   
CDMV70   DS    0H                                                               
         MVC   CDMVRD1.PBD2TID,=C'BUYDM2'   BUYDV2 --> BUYDM2                   
         LA    R2,DS2DMS           DEMO CAT TYPES FROM AGYDS2 LINE              
         LA    R3,CDMVRD1.PBD2VAL                                               
         LA    R6,CDMVRD2.PBDV2VAL                                              
*                                                                               
CDMV73   CLI   0(R2),0             DONE WITH ALL 4 DEMO CATEGORIES?             
         JE    CDMVYES             YES                                          
         CLI   0(R2),C' '          NO CATEGORY?                                 
         JE    CDMV79              NONE, THEN SPACES                            
*                                                                               
         LR    RF,R3               HAVE A CATEGORY, SO POINT TO VALUE           
         LA    RE,L'PBDV2VAL       IT SHOULD HAVE THIS MANY DIGITS              
CDMV74   CLI   0(RF),C'0'                                                       
         JL    CDMV79              NOT A VALID DEMO VALUE, SPACE FILL           
         LA    RF,1(RF)                                                         
         JCT   RE,CDMV74                                                        
*                                                                               
         CLI   0(R2),C'I'          AN IMPRESSION VALUE?                         
         JE    CDMV76              YES                                          
         MVC   0(L'PBD2VAL,R3),0(R6) NO, FOR RATINGS-STRAIGHT COPY              
         J     CDMV80                                                           
*                                                                               
CDMV76   PACK  DUB,0(L'PBDV2VAL,R6) GET THE VALUE                               
         SRP   DUB,64-1,5           DIVIDE BY 10 AND ROUND IF NECESSARY         
         OI    DUB+7,X'0F'                                                      
         UNPK  0(L'PBD2VAL,R3),DUB                                              
         J     CDMV80                                                           
*                                                                               
CDMV79   MVC   0(L'PBD2VAL,R3),SPACES NO DEMO CATEGORY, SO SPACE FILL           
*                                                                               
CDMV80   LA    R2,1(R2)              BUMP TO NEXT CATEGORY TYPE                 
         LA    R3,L'PBD2VAL(R3)      BUMP TO NEXT 1 DEC DEMO VALUE              
         LA    R6,L'PBDV2VAL(R6)     BUMP TO NEXT 2 DEC DEMO VALUE              
         J     CDMV73                CONVERT THE NEXT DEMO VALUE                
         DROP  CDMVRD1,CDMVRD2                                                  
*----------------------------------------------------------------------         
* INCOMING IS MKGDEM, BUT MESSAGE LINE WILL STILL BE MKGDEM                     
*   THIS IS THE CASE WHERE WE HAVE 2 DEC IMPS --> 1 DEC IMPS                    
*   WE LEAVE THE RATING VALUES ALONE                                            
*----------------------------------------------------------------------         
         USING MOFRDEMD,R4                                                      
CDMV100  LA    R2,DS2DMS           DEMO CAT TYPES FROM AGYDS2 LINE              
         LA    R3,MODMVAL                                                       
*                                                                               
CDMV110  CLI   0(R2),0             DONE WITH ALL 4 DEMO CATEGORIES?             
         JE    CDMVYES             YES                                          
         CLI   0(R2),C' '          NO CATEGORY?                                 
         JE    CDMV125             NONE, THEN SPACES                            
*                                                                               
         LR    RF,R3               HAVE A CATEGORY, SO POINT TO VALUE           
         LA    RE,L'MODMVAL        IT SHOULD HAVE THIS MANY DIGITS              
CDMV115  CLI   0(RF),C'0'                                                       
         JL    CDMV125             NOT A VALID DEMO VALUE, SPACE FILL           
         LA    RF,1(RF)                                                         
         JCT   RE,CDMV115                                                       
*                                                                               
         CLI   0(R2),C'I'          AN IMPRESSION VALUE?                         
         JNE   CDMV120             NO, LEAVE RATINGS VALUE ALONE                
*                                                                               
         PACK  DUB,0(L'MODMVAL,R3) GET THE VALUE                                
         SRP   DUB,64-1,5          DIVIDE BY 10 AND ROUND IF NECESSARY          
         OI    DUB+7,X'0F'                                                      
         UNPK  0(L'MODMVAL,R3),DUB                                              
*                                                                               
CDMV120  LA    R2,1(R2)            BUMP TO NEXT CATEGORY TYPE                   
         LA    R3,L'MODMVAL(R3)    BUMP TO NEXT 2 DEC DEMO VALUE                
         J     CDMV110             MIGHT NEED TO CONVERT NEXT VALUE             
*                                                                               
CDMV125  MVC   0(L'PBD2VAL,R3),SPACES   NO DEMO CATEGORY, SO SPACE FILL         
         J     CDMV120                                                          
*                                                                               
CDMVYES  MVC   0(2,R3),=X'0D25'    CRLF                                         
         SR    R3,R4               L'CONVERTED MESSAGE W/O CRLF                 
         ST    R3,DMCB+4           SET IN PARAM 2 FOR RETURN                    
         CR    RB,RB               YES, SET CC EQUAL                            
         J     CDMVX                                                            
*                                                                               
CDMVNO   LTR   RB,RB               NO, SET CC NOT EQUAL                         
CDMVX    XIT1                                                                   
         LTORG                                                                  
                                                                                
***********************************************************************         
* MESSAGE POST PROCESSING FOR MESSAGES THAT THE RECEIVER IS SUBSCRIBED          
* TO                                                                            
*                                                                               
* ON ENTRY:    (R4)                A(MESSAGE LINE)                              
*              (R3)                A(NEXT MESSAGE LINE)                         
***********************************************************************         
MSGPOSTP NTR1                                                                   
         CLC   =C'AGYDS2',0(R4)    AGYDS2 WITH DEMO CATEGORIES?                 
         JE    MSGPP10                                                          
         CLC   =C'AGYCDC',0(R4)    AGYCDC WITH COMSCORE DEMO CATS?              
         JE    MSGPP20                                                          
         CLC   =C'BUYDV2',0(R4)    BUY DEMOS, ALL DEMO VALUES AT 2 DEC?         
         JE    MSGPP30                                                          
         CLC   =C'BUYDM2',0(R4)    BUY DEMOS, RTG 2 DEC, IMPS 1 DEC             
         JE    MSGPP30                                                          
         CLC   =C'BUYDEM',0(R4)    BUY DEMOS, ALL DEMO VALUES AT 1 DEC          
         JE    MSGPP30                                                          
         CLC   =C'BUYCDV',0(R4)    COMSCORE DEMOS?                              
         JE    MSGPP35                                                          
         CLC   =C'BUYCOM',0(R4)    BUY COMMENTS?                                
         JE    MSGPP40                                                          
         CLC   =C'MKGDEM',0(R4)    MAKEGOOD DEMO VALUES?                        
         JE    MSGPP70                                                          
***********************************                                             
* FOR TRAILERS                                                                  
***********************************                                             
         CLC   =C'TLR',3(R4)       WAS THIS A TRAILER RECORD?                   
         JNE   MSGPSTPY            NO                                           
         CLC   RECCOUNT,ADJCOUNT   NO ADJUSTMENTS?                              
         JE    MSGPSTPY            NONE                                         
         L     RE,ADJCOUNT                                                      
         CVD   RE,DUB              TLR COUNT NEEDS TO BE MODIFIED               
         OI    DUB+7,X'0F'                                                      
         UNPK  14(6,R4),DUB          WITH THE ADJUSTED COUNT                    
         J     MSGPSTPY            YES - RECORD IS OK                           
***********************************                                             
* FOR AGYDS2                                                                    
***********************************                                             
         USING PAGYDS2D,R4         SAVE OFF THE DEMO CATEGORY TYPES             
MSGPP10  LA    R1,DS2DMS           5TH BYTE (X'00') IS FOR END OF LIST          
         LA    RE,PAD2TDEM         1ST DEMO CATEGORY                            
         LA    RF,PAD2TDM4         4TH AND LAST DEMO CATEGORY                   
MSGPP10A CR    RE,RF               PASS THE 4TH AND LAST CATEGORY?              
         BH    MSGPP12             YES, PROCEED TO NEXT MESSAGE LINE            
*                                                                               
         CLI   0(RE),C' '          DO WE HAVE SOMETHING HERE?                   
         JNH   *+10                NO, SKIP IT                                  
         MVC   0(1,R1),0(RE)       SAVE OFF THIS DEMO CATEGORY TYPE             
*                                                                               
         LA    R1,1(R1)            LOOP THROUGH ALL 4 CATEGORIES                
         LA    RE,L'PAD2TDEM(RE)   WE'LL EVEN SAVE OFF SPACE IF NO              
         J     MSGPP10A              CATEGORY                                   
*                                                                               
MSGPP12  LR    RE,R3               REMEMBER WHERE NEXT MESSAGE IS               
         CLC   =C'AGYCDC',0(R3)    DO WE HAVE A COMSCORE DEMO LINE?             
         JNE   MSGPSTPY            NO, NOTHING LEFT TO DO                       
         OI    BITFLGS1,BF1ACDC      WE HAVE A COMSCORE DEMO LINE               
* THE ABOVE 2 LINES WON'T BE SUFFICIENT IF WE HAVE MORE DEMO SOURCES            
*                                                                               
********                                                                        
* LET SEE IF THE RECEIVER SUPPORTS BOTH DEMO SOURCES                            
*      IF AGYTDM IS IN THE SUBSCRIBED LIST OF MESSAGE RECORDS                   
********                                                                        
         L     RF,=A(IO2)          TBL OF VALID RECORDS FOR RECEIVER            
MSGPP12A CLI   0(RF),X'FF'         DEFAULT IS THAT AGYDTM                       
         JE    MSGPP12C               IS NOT IN SUBSCRIBED LIST                 
         CLC   =C'AGYTDM',0(RF)    UNLESS AGYTDM IS IN THE SUBSCRIBED           
         JE    MSGPP12B              LIST AS RETURNED FROM GETDARE              
         LA    RF,6(RF)                                                         
         J     MSGPP12A                                                         
*                                                                               
MSGPP12B OI    BITFLGS1,BF1SBATDM  RECEIVER IS SUBSCRIBED TO AGYTDM             
         J     MSGPSTPY            RECEIVER WANTS BOTH SOURCES                  
*********                                                                       
* WE HAVE COMSCORE DEMOS IN THE DARE MESSAGE                                    
*  WE KNOW THEY DON'T WANT BOTH SOURCES BECAUSE OF AGYTDM JUST ABOVE            
* WE NEED TO SEE WHICH SOURCE IS THE PRIMARY DEMO AND ONLY THAT                 
*   DEMO SOURCE WILL BE SENT. THE OTHER SOURCE(S) WILL BE STRIPPED              
*********                                                                       
MSGPP12C TRT   0(256,R3),TRINBND   LET'S FIND THE AGYTDM                        
         CLI   0(R1),X'FF'         END OF ENTIRE DARE MESSAGE?                  
         JE    MSGPSTPY            SHOULDN'T HAPPEN, NIELSEN IS DEFAULT         
         CLC   CRLF,0(R1)          DID WE HIT THE CRLF FOR AGYCDC?              
         JE    MSGPP12E                                                         
         MVI   0(R1),C' '          REPLACE INVALID CHARS W/ A BLANK             
         LA    R3,1(R1)                                                         
         J     MSGPP12A            WE WANT THE CRLF FOR AGYCDC                  
*                                                                               
MSGPP12E LA    R3,2(R1)            R3 = A(NEXT RECORD)                          
         CLC   =C'AGYTDM',0(R3)    DO WE HAVE AN 'AGYTDM'?                      
         JNE   MSGPSTPY            SHOULDN'T HAPPEN,  NIELSEN DEFAULT           
         USING PAGYTDMD,R3                                                      
********                                                                        
         CLC   =C'D=Y',RETURN+RTNDDS-RTN2SNDR  DDS ORDER?                       
         JNE   MSGPP12H            NO, ASSUME OX, BUT COULD BE HARRIS!          
         J     MSGPP12J            YES, LOOK AT AGYTDM                          
*                                                                               
MSGPP12H CLC   FRSTEDDT,=C'170825' ARE WE AFTER AUG25/17?                       
         JH    MSGPP12J            YES, LOOK AT AGYTDM                          
         MVI   PRIMDEMO,C'N'       MAKE NIELSEN IS PRIMARY SOURCE               
         J     MSGPSTPY                                                         
********                                                                        
MSGPP12J CLC   =C'AGYDS2',PTDMDEM1 IS THE PRIMARY DEMO NIELSEN?                 
         JE    MSGPSTPY            YES, NOTHING LEFT TO DO                      
         DROP  R3                                                               
* C'AGYCDC' CAN BE CHECKED HERE IF WE HAVE YET ANOTHER RATING SOURCE            
*                                                                               
         MVI   PRIMDEMO,C'C'       WE HAVE A COMSCORE DEMO                      
         XC    DS2DMS,DS2DMS       NO MORE DEMOS IN  AGYDS2                     
         MVC   PAD2TDEM(L'PAD2TDEM*4),SPACES                                    
         J     MSGPSTPY            STRIP OUT DEMOS FROM DARE MESSAGE            
***********************************                                             
* FOR AGYCDC                                                                    
***********************************                                             
         USING PAGYCDCD,R4                                                      
MSGPP20  DS    0H                                                               
         OI    BITFLGS1,BF1ACDC    WE HAVE A COMSCORE DEMO LINE                 
*                                                                               
         OC    DS2DMS,DS2DMS       WE HAVE DEMOS IN AGYDS2?                     
         JNZ   MSGPP22             YES, CHECK IF AGYCDC LINE STAYS              
         MVI   PRIMDEMO,C'C'       NONE, PRIMARY DEMO IS COMSCORE               
         J     MSGPSTPY               AND NOTHING LEFT TO DO                    
*                                                                               
MSGPP22  CLI   PRIMDEMO,C'C'       PRIMARY DEMO IS COMSCORE?                    
         JE    MSGPSTPY                 LEAVE THIS LINE ALONE                   
         TM    BITFLGS1,BF1SBATDM  RECEIVER SUBSCRIBED TO AGYTDM?               
         JNZ   MSGPSTPY                 LEAVE THIS LINE ALONE                   
*                                                                               
         NI    BITFLGS1,X'FF'-BF1ACDC    NO LONGER HAVE AGYCDC                  
         J     MSGPSTRP            STRIP OUT AGYCDC FROM DARE MESSAGE           
***********************************                                             
* FOR TRADITIONAL BUY DEMOS                                                     
***********************************                                             
MSGPP30  TM    BITFLGS1,BF1SBATDM  RECEIVER SUBSCRIBED TO AGYTDM?               
         JNZ   MSGPSTPY               NOTHING LEFT TO DO                        
         CLI   PRIMDEMO,C'N'       PRIMARY DEMO IS NIELSEN?                     
         JE    MSGPSTPY                                                         
         J     MSGPSTRP            NO, STRIP TRADITIONAL DEMO VALUES            
***********************************                                             
* FOR COMSCORE BUY DEMOS                                                        
***********************************                                             
MSGPP35  TM    BITFLGS1,BF1SBATDM  RECEIVER SUBSCRIBED TO AGYTDM?               
         JNZ   MSGPSTPY               NOTHING LEFT TO DO                        
         CLI   PRIMDEMO,C'C'       PRIMARY DEMO IS COMSCORE                     
         JE    MSGPSTPY                                                         
         J     MSGPSTRP            NO, STRIP THE COMSCORE DEMO VALUES           
***********************************                                             
* FOR BUYCOM WITH COMSCORE DEMOS                                                
***********************************                                             
         USING PBUYCOMD,R4                                                      
MSGPP40  DS    0H                                                               
         LA    R2,PBCMTEXT+14      A BUYCOM WITH COMSCORE DEMOS?                
         CLC   =C'COMSCORE DEMO ',PBCMTEXT    DS USES THIS FORMAT               
         JE    MSGPP42                                                          
         LA    R2,PBCMTEXT+13                                                   
         CLC   =C'Rentrak Demo ',PBCMTEXT     OX USES THIS FORMAT               
         JNE   MSGPSTPY               AND NOTHING LEFT TO DO                    
*********                                                                       
* ONE ADDITIONAL CHECK AS BUYER COULD HAVE ENTERED A COMMENT THAT LOOKS         
* LIKE THE ABOVE FORMAT, SO LOOK FOR THE X IN THE DEMO CATEGORY                 
*********                                                                       
MSGPP42  CLI   0(R2),C'X'          COMSCORE IMPRESSION CATEGORY?                
         JE    MSGPP42A                                                         
         CLC   =C'IX',0(R2)        COMSCORE IMPRESSION CATEGORY?                
         JE    MSGPP42A                                                         
         CLC   =C'RX',0(R2)        COMSCORE RATING CATEGORY?                    
         JNE   MSGPSTPY              NO, DOESN'T MATCH THE FORMAT               
*                                                                               
MSGPP42A TM    BITFLGS1,BF1ACDC    DO WE HAVE AN 'AGYCDC' LINE?                 
         JNZ   MSGPP44             YES, STRIP OUT BUYCOM WITH COMSCORE          
         CLI   PRIMDEMO,C'C'       PRIMARY DEMO IS A COMSCORE DEMO?             
         JE    MSGPP44             YES, STRIP THESE BUYCOMS                     
         OC    DS2DMS,DS2DMS       DO WE HAVE TRADITIONAL DEMOS?                
         JZ    MSGPSTPY            NO, KEEP THE BUYCOM WITH COMSCORE            
*                                                                               
MSGPP44  CLI   PBCMCONT,C'*'       WE HAVE A CONTINUATION?                      
         JE    MSGPSTRP            YES, OKAY TO STRIP                           
         MVC   PBCMTEXT,SPACES     OTHERWISE REPLACE WITH SPACES                
         J     MSGPSTPY                                                         
***********************************                                             
* FOR MKGDEM                                                                    
***********************************                                             
         USING MOFRDEMD,R4                                                      
MSGPP70  DS    0H                                                               
         TM    BITFLGS1,BF1DC2DS2   2 DEC IMPS --> 1 DEC IMPS?                  
         JZ    MSGPSTPY             NO, NO NEED TO TOUCH MKGDEM LINE            
         MVC   P+11(40),=C'!! MKGDEM - 2 DEC IMPS --> 1 DEC IMPS !!'            
         GOTO1 =V(PRINTER)                                                      
         MVC   P(MOFRDEML),0(R4)                                                
         MVC   P+MOFRDEML(5),=C' ==> '                                          
         GOTO1 CNVDMVLS,DMCB,(0,=C'MKGDEM')                                     
         JNE   MSGPSTPN            BAD DEMO CONVERSION                          
         MVC   P+MOFRDEML+5(MOFRDEML),0(R4)                                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
MSGPSTPY SR    RC,RC                                                            
MSGPSTPN LTR   RC,RC                                                            
         XIT1                                                                   
*                                                                               
MSGPSTRP BRAS  RE,STRIPLN                                                       
         L     RE,ADJCOUNT         DECREMENT THE COUNT                          
         SHI   RE,1                                                             
         ST    RE,ADJCOUNT                                                      
*                                                                               
         SR    RC,RC               ALSO RETURN WITH A YES                       
         LTR   RC,RC                                                            
         XIT1  REGS=(R3)           R3 NEEDS TO BE RETURNED                      
         DROP  R4                                                               
*********                                                                       
* LINE IS TO BE STRIPPED FROM MESSAGE                                           
*********                                                                       
STRIPLN  NTR1                                                                   
         MVC   P+11(19),=C'!! STRIPPED LINE !!'                                 
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LR    RE,R3               R3 IS THE A(NEXT MESSAGE)                    
         SR    RE,R4               RE = L(MESSAGE LINE) TO STRIP                
         CHI   RE,L'P              DON'T TRASH WHAT IS AFTER P                  
         BNH   *+8                                                              
         LA    RE,L'P                                                           
         BCTR  RE,0                                                             
         EX    RE,SHWSTRPD                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LR    R0,R4               SET ADDRESS AND LENGTH OF DEST               
         L     R1,ADAREBUF           FOR THE MVCL                               
         SR    R0,R1               R1 = # OF BYTES FROM START OF TRANS          
         L     R1,BUFFERLENGTH2                                                 
         SR    R1,R0               R1 = # OF BYTES TO END OF TRANS              
         LR    R0,R4               RESET ADDRESS OF DEST                        
*                                                                               
         LR    RE,R3               SET ADDRESS AND LENGTH OF SOURCE             
         L     RF,ADAREBUF           FOR THE MVCL                               
         SR    RE,RF               RF = # OF BYTES FROM START OF TRANS          
         L     RF,BUFFERLENGTH2                                                 
         SR    RF,RE               RF = # OF BYTES TO END OF TRANS              
         LR    RE,R3               RESET ADDRESS OF SOURCE                      
*                                                                               
         MVCL  R0,RE               DAREBUF NO LONGER HAS UNSUPPORTED            
         LR    R3,R4               R3 IS NOW THE NEXT LINE AFTER STRIP          
*                                                                               
         XIT1  REGS=(R3)           RETURN R3 TO CALLER                          
*                                                                               
SHWSTRPD MVC   P(0),0(R4)          EXECUTED MOVE                                
         LTORG                                                                  
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
***********************************************************************         
* THIS ROUTINE GENERATES ERROR NOTIFICATIONS.                                   
***********************************************************************         
BUILDERR NMOD1 0,BUILDERR                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
         BRAS  RE,FINDTSKS         FIND THE TASK TO SEND THIS MSG               
         OC    SNDTSKID,SNDTSKID   DID WE KNOW WHERE TO SEND?                   
         BNZ   BE05                YES                                          
         MVC   P+30(11),=C'ERROR CODE='                                         
         MVC   P+42(3),ERRCODE                                                  
         MVC   P+60(15),=C'UNKNOWN USERID='                                     
         MVC   P+75(10),SENDER                                                  
         PRNT  OnlineERRNOT        DON'T CREATE ERRNOT TO MQ WORKQ              
*                                                                               
         AHI   R3,-6               GET ROOM FOR *ERROR LABEL                    
         MVC   0(6,R3),=C'*ERROR'                                               
         L     R2,BYTECNTR         L'RECORD                                     
         AHI   R2,6                +6 FOR *ERROR LABEL                          
         CHI   R2,200                                                           
         BNH   *+8                                                              
         LHI   R2,200                                                           
         GOTO1 =A(PUTSMF),(RC)                                                  
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'*ERROR'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
*CERTAINLY, I CAN PUT MESSAGES (DAREBUFF) DIRECTLY TO NOTES MQ QUEUE            
*BUT, FOR NOW, JUST KEEP IT SIMPLE, 2/17/04                                     
*                                  EMAIL SKUI,WHOA,HWON                         
         MVC   ERRMSG1A,ERRCODE      ERROR CODE                                 
         MVC   ERRMSG1B,SENDER       SENDER                                     
         MVC   ERRMSG1C,ORDNUM       ORDER NUMBER                               
         GOTO1 DATAMGR,DMCB,=C'OPMSG',('ERRMSG1Q',ERRMSG1)                      
         B     BEX                                                              
*                                                                               
BE05     EQU   *                                                                
         L     R4,=A(ENOTBUF)      R4 = A(ERRNOT MQ LOG BUFFER)                 
         LR    RE,R4               CLEAR ERRNOT MQ LOG BUFFER                   
         L     RF,=A(ENOTBUFL)                                                  
         XCEFL                                                                  
*                                                                               
         MVC   0(APPCM3Q,R4),APPCM3 +++DARE SEND                                
         GOTO1 =A(DATETIME),(RC)   PUT TIME INTO MESSAGE                        
         MVC   18(L'HHMMSS,R4),HHMMSS                                           
         MVC   APPCM3Q(L'CRLF,R4),CRLF                                          
         AHI   R4,APPCM3Q+L'CRLF                                                
         MVC   ERRRCCNT,=F'1'      ONE RECORD SO FAR                            
*                                                                               
         OC    LOGDATA,LOGDATA     WAS THERE ANY LOG DATA TO SEND?              
         BZ    BE10                                                             
         MVC   0(APPCM5Q,R4),APPCM5 +++DARE LOGGING=                            
         MVC   16(L'LOGDATA,R4),LOGDATA                                         
         MVC   APPCM5Q(L'CRLF,R4),CRLF                                          
         AHI   R4,APPCM5Q+L'CRLF                                                
         L     RE,ERRRCCNT                                                      
         AHI   RE,1                                                             
         ST    RE,ERRRCCNT                                                      
*                                                                               
BE10     EQU   *                                                                
*                                                                               
         USING MDLNNOTD,R4                                                      
         MVC   MDNTTID,=C'ERRNOT'  ERROR NOTIFICATION                           
*                                                                               
         OC    SNDTSKID,SNDTSKID   DID WE KNOW WHERE TO SEND?                   
         BNZ   *+10                YES                                          
         MVC   MDNTTID,=C'DARERR'  NO - SPECIAL DARE ERROR                      
*                                                                               
         MVC   MDNTORDR,ORDNUM     ORDER NUMBER                                 
         MVC   MDNTFRID,RECEIVER   RECEIVER USERID                              
         CLC   MDNTFRID,SPACES     DON'T LET SENDING ID BE NULLS                
         BH    *+10                                                             
         MVC   MDNTFRID,SPACES     AS THAT BE INTERPRETED AS EOL                
*                                                                               
         MVC   MDNTTOID,SENDER     SENDER USERID                                
         CLC   MDNTTOID,SPACES     DON'T LET RECEIVING ID BE NULLS              
         BH    *+10                                                             
         MVC   MDNTTOID,SPACES     AS THAT BE INTERPRETED AS EOL                
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),WORK                                       
         THMS  DDSTIME=YES                                                      
         ST    R1,FULL             0HHMMSS+ (DDS TIME)                          
         STCM  R0,15,PACKOF4B      0060000+ (DDS CLOCK TIME DIFFERENCE)         
         AP    PACKOF4B,FULL       NOW CONTAINS ACTUAL TIME                     
         CP    PACKOF4B,=P'240000' PAST MIDNIGHT?                               
         BL    BE20                                                             
         SP    PACKOF4B,=P'240000' YES, ADJUST TIME AND BUMP DAY                
         GOTO1 =V(ADDAY),DMCB,WORK,WORK+6,F'1'                                  
         MVC   WORK(6),WORK+6                                                   
BE20     ICM   R0,15,PACKOF4B                                                   
         SRL   R0,12                                                            
         STCM  R0,3,HALF                                                        
*                                                                               
         GOTO1 =V(DATCON),DMCB,WORK,(X'20',MDNTDATE)   DELIVERY DATE            
         GOTO1 =V(HEXOUT),DMCB,HALF,MDNTTIME,2,=C'TOG' DELIVERY TIME            
         CLC   =F'4',DMCB+16                                                    
         JNE   *+2                                                              
*                                                                               
         MVC   MDNTTDTE,ORIGDATE   DATE OF ORIGINATION                          
         MVC   MDNTTTIM,ORIGTIME   TIME OR ORIGINATION                          
         MVC   MDNTRPCN,CONTRACT   REP CONTRACT                                 
         MVC   MDNTRTNS,RETURN     'RETURN TO SENDER' DATA                      
         MVC   MDNTOFRI,OFFERID    MAKEGOOD OFFER GROUP                         
         MVC   MDNTSEQN,IDSEQNUM   ID NUMBER WITHIN GROUP                       
         MVC   MDNTEFLG,ERRCODE    ERROR CODE                                   
         DROP  R4                                                               
*                                                                               
         MVC   MDLNNOTL(L'CRLF,R4),CRLF                                         
         AHI   R4,MDLNNOTL+L'CRLF                                               
         L     RE,ERRRCCNT                                                      
         AHI   RE,1                                                             
         ST    RE,ERRRCCNT                                                      
*                                                                               
         MVC   0(APPCM4Q,R4),APPCM4 +++DARE END                                 
         GOTO1 =A(DATETIME),(RC)   PUT TIME INTO MESSAGE                        
         MVC   17(L'HHMMSS,R4),HHMMSS                                           
         L     RE,ERRRCCNT                                                      
         AHI   RE,1                ONE MORE FOR FINAL RECORD                    
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  30(5,R4),DUB         TOTAL NUMBER OF RECORDS                     
         MVC   APPCM4Q(L'CRLF,R4),CRLF                                          
         AHI   R4,APPCM4Q+L'CRLF                                                
*                                                                               
         OC    SNDTSKID,SNDTSKID   DID WE KNOW WHERE TO SEND?                   
         BZ    BE40                NO - JUST LOG THIS MSG                       
*                                                                               
         LAY   R1,ENOTBUF          R1 = A(MESSAGE BUFFER)                       
         LR    R2,R4                                                            
         SR    R2,R1               R2 = MESSAGE LENGTH                          
         LA    R3,SENDER           R3 = A(SENDER NAME)                          
         MVC   HALF,USERID         HALF = SENDER USERID#                        
         SAM24                                                                  
         BRAS  RE,PUTWRKQ          PUT THIS MQ MESSAGE TO WORKQ                 
         BRAS  RE,CMTWRKQ          COMMIT THE MESSAGE                           
         BRAS  RE,CLOWRKQ          CLOSE THE WORKQ                              
*                                                                               
BE40     LAY   R1,ENOTBUF          R1 = A(MESSAGE BUFFER)                       
         LR    R2,R4               R4 = A(END OF MESSAGE)                       
         SR    R2,R1               R2 = MESSAGE LENGTH                          
         BRAS  RE,LOGMSGQ          LOG THIS MQ MESSAGE                          
*                                                                               
BEX      XIT1                                                                   
         LTORG                                                                  
                                                                                
***********************************************************************         
* RECEIVE RECORDS FROM DARE, AND PUT ENTIRE MESSAGE IN DAREBUF                  
***********************************************************************         
MQBUFF   NMOD1 0,MQBUFF                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         MVI   TOOLONG,C'N'        (NO RECORDS RECEIVED YET)                    
         MVI   NULLREC,C'N'        (NO RECORDS RECEIVED YET)                    
*                                  CLEAR SVMCIDTB                               
         SAM31                                                                  
         SR    R4,R4               CLEAR RECORD COUNTER                         
         L     R3,ADAREBUF                                                      
         LR    RE,R3                                                            
         L     RF,=A(DAREBUFL)     CLEAR BUFFER                                 
         XCEFL                                                                  
         SAM24                                                                  
*                                  CLEAR SVMCIDTB                               
         LA    RE,SVMCIDTB                                                      
         LA    RF,SKIPMAXQ                                                      
         XC    0(L'SVMCIDTB,RE),0(RE)                                           
         AHI   RE,L'SVMCIDTB                                                    
         BCT   RF,*-10                                                          
*                                                                               
         MVC   OBJDESC_OBJECTTYPE,=A(MQOT_Q)  OBJECT IS A QUEUE                 
         L     RF,TSKINPTQ                                                      
         MVC   OBJDESC_OBJECTNAME,0(RF)  INPUT QUEUE NAME                       
         MVC   OPEN_OPTIONS,=A((MQOO_INPUT_AS_Q_DEF+MQOO_BROWSE))               
*                                                                               
         MVC   P+30(L'OBJDESC_OBJECTNAME),OBJDESC_OBJECTNAME                    
         PRNT  AboutToOpenQueue                                                 
*                                                                               
         LA    R2,CSQBOPEN_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     OPEN QUEUE                                   
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         JNE   *+2                                                              
*                                                                               
         MVC   MSGDESC_CORRELID,MQCI_NONE                                       
         MVC   MSGDESC_MSGID,MQMI_NONE                                          
*                                                                               
MQBUF05  LHI   RF,MQGMO_SET_SIGNAL                                              
         ST    RF,GETMSGOPTS_OPTIONS                                            
         MVC   GETMSGOPTS_MATCHOPTIONS,=A((MQMO_MATCH_MSG_ID+MQMO_MATCHX        
               _CORREL_ID))                                                     
         MVC   GETMSGOPTS_SIGNAL1,=A(INPQECB)                                   
         MVC   GETMSGOPTS_WAITINTERVAL,=A(MQWI_UNLIMITED)                       
*                                                                               
MQBUF10  OC    MSGDESC_MSGID,MSGDESC_MSGID  DON'T PRINT IF NO ID                
         BZ    MQBUF12                                                          
         MVC   P+30(24),MSGDESC_MSGID                                           
         PRNT  MsgID                                                            
*                                                                               
         MVC   P+70(21),=CL21'MSGDESC_PUTDATE+TIME'                             
         MVI   P+92,C'='                                                        
         MVC   P+93(16),MSGDESC_PUTDATE                                         
*                                                                               
         MVC   P+30(24),MSGDESC_CORRELID                                        
         PRNT  CorrelID                                                         
*                                                                               
MQBUF12  XC    INPQECB,INPQECB                                                  
         LA    R2,CSQBGET_STRUCTURE                                             
         GOTO1 =A(CALLMQ),(RC)     TRY TO GET A MESSAGE                         
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    MQBUF40             GOT A MESSAGE                                
         CLC   MQ_COMPCODE,=A(MQCC_WARNING)                                     
         JNE   *+2                                                              
         CLC   MQ_REASON,=A(MQRC_SIGNAL_REQUEST_ACCEPTED)                       
         JNE   *+2                                                              
*                                                                               
         PRNT  AboutToWait                                                      
*                                                                               
         WAIT  1,ECBLIST=ECBLSTMQ  WAIT FOR COMPLETION OR OPERATOR              
*                                                                               
         PRNT  WaitComplete                                                     
*                                                                               
         L     RF,ASTOPECB                                                      
         TM    0(RF),X'40'         STOP?                                        
         BNZ   MQBUF20             NO                                           
*                                                                               
         TM    INPQECB,X'40'                                                    
         JNO   *+2                 HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
         L     RF,INPQECB          BITS 2-31 OF ECB CONTAIN COMP. CODE          
         SLL   RF,2                                                             
         SRL   RF,2                                                             
         C     RF,=A(MQEC_WAIT_INTERVAL_EXPIRED)                                
         BE    MQBUF23                                                          
         C     RF,=A(MQEC_MSG_ARRIVED)                                          
         JNE   *+2                 NO OTHER RESULT ACCEPTABLE (FOR NOW)         
         XC    INPQECB,INPQECB     CLEAR ECB BEFORE MQGET CALL                  
         B     MQBUF85             A MESSAGE IS WAITING: GET IT                 
*                                                                               
MQBUF20  XC    0(4,RF),0(RF)                                                    
         MVI   OPERSTOP,C'Y'                                                    
*                                                                               
MQBUF23  DS    0H                  UNIT OF WORK IS NOT COMPLETED YET            
         LA    R2,CSQBBACK_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     BACK                                         
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         JNE   *+2                                                              
*                                                                               
         CLI   OPERSTOP,C'Y'                                                    
         BE    MQBUFCLO                                                         
*                                                                               
         LA    RE,SVMCIDTB                                                      
         USING SVMCIDD,RE                                                       
         LA    RF,SKIPMAXQ                                                      
MQBUF24  OC    0(L'SVMCIDTB,RE),0(RE)                                           
         BZ    MQBUF25                                                          
*                                                                               
         CLC   SVMCIDC,MSGDESC_CORRELID                                         
         BNE   *+14                                                             
         CLC   SVMCIDM,MSGDESC_MSGID                                            
         BE    MQBUF27                                                          
*                                                                               
         AHI   RE,L'SVMCIDTB       NEXT SVMCIDTB ENTRY                          
         BCT   RF,MQBUF24                                                       
         DC    H'0'                        SVMCIDTB TABLE FULL!                 
*                                                                               
MQBUF25  MVC   SVMCIDC,MSGDESC_CORRELID    SAVE CORRLID                         
         MVC   SVMCIDM,MSGDESC_MSGID       SAVE MSGID                           
MQBUF27  AHI   RE,L'SVMCIDTB       CLEAR NEXT SVMCIDTB ENTRY                    
         XC    0(L'SVMCIDTB,RE),0(RE)    TO MARK END OF LIST                    
         DROP  RE                                                               
*                                  EMAIL AHYD,AWIL,HWON,WHOA                    
         GOTO1 DATAMGR,DMCB,=C'OPMSG',=C'AUTONOTE*AHYD,AWIL,HWON,WHOA: +        
               DARE MQ1RCVR MQGET TIME OUT!'                                    
*                                                                               
         MVC   P+30(24),MSGDESC_MSGID                                           
         PRNT  SkipMsgID                                                        
         MVC   P+30(24),MSGDESC_CORRELID                                        
         PRNT  SkipCorrelID                                                     
*                                                                               
         MVC   GETMSGOPTS_OPTIONS,=A(MQGMO_BROWSE_FIRST)                        
*                                                                               
MQBUF28  MVC   MSGDESC_CORRELID,MQCI_NONE                                       
         MVC   MSGDESC_MSGID,MQMI_NONE                                          
         XC    INPQECB,INPQECB                                                  
         LA    R2,CSQBGET_STRUCTURE                                             
         GOTO1 =A(CALLMQ),(RC)     TRY TO BROWSE A MESSAGE                      
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    MQBUF30             GOT A MESSAGE                                
         CLC   MQ_REASON,=A(MQRC_NO_MSG_AVAILABLE)                              
         JNE   *+2                 NO ERROR IS ACCEPTABLE (FOR NOW)             
*                                                                               
* END OF QUEUE, NO OTHER UNIT OF WORK FOUND, BACK TO THE SKIP ONE.              
         MVC   GETMSGOPTS_OPTIONS,=A(MQGMO_BROWSE_FIRST)                        
         XC    INPQECB,INPQECB                                                  
         LA    R2,CSQBGET_STRUCTURE                                             
         GOTO1 =A(CALLMQ),(RC)     POINT THE CURSOR BACK TO 1ST MSG             
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         JNE   *+2                 CAN'T FAIL NOW                               
*                                                                               
         MVC   MSGDESC_CORRELID,MQCI_NONE                                       
         MVC   MSGDESC_MSGID,MQMI_NONE                                          
         B     MQBUF38                                                          
*                                                                               
MQBUF30  LA    RE,SVMCIDTB                                                      
         USING SVMCIDD,RE                                                       
         LA    RF,SKIPMAXQ+1                                                    
MQBUF32  OC    0(L'SVMCIDTB,RE),0(RE)                                           
         BZ    MQBUF38             NO MATCH                                     
*                                                                               
         CLC   MSGDESC_CORRELID,SVMCIDC                                         
         BNE   *+14                                                             
         CLC   MSGDESC_MSGID,SVMCIDM                                            
         BE    MQBUF35             MATCH, SKIP                                  
         AHI   RE,L'SVMCIDTB       NEXT SVMCIDTB ENTRY                          
         BCT   RF,MQBUF32                                                       
*                                                                               
*                                  SKIP MSG W/ SAME CORRLID+MSGID               
MQBUF35  PRNT  NowBrowseNext                                                    
         MVC   GETMSGOPTS_OPTIONS,=A(MQGMO_BROWSE_NEXT)                         
         B     MQBUF28                                                          
*                                                                               
MQBUF38  MVI   TOOLONG,C'N'        (NO RECORDS RECEIVED YET)                    
         MVI   NULLREC,C'N'        (NO RECORDS RECEIVED YET)                    
*                                                                               
         SAM31                                                                  
         SR    R4,R4               CLEAR RECORD COUNTER                         
         L     R3,ADAREBUF                                                      
         LR    RE,R3                                                            
         L     RF,=A(DAREBUFL)     CLEAR BUFFER                                 
         XCEFL                                                                  
         SAM24                                                                  
         B     MQBUF05                                                          
*                                                                               
MQBUF40  CLC   MSGDESC_MSGTYPE,=A(MQMT_DATAGRAM)                                
         JNE   *+2                                                              
*                                                                               
*&&DO                                                                           
         MVC   P+11(10),=CL10'STRUCID: '                                        
         MVC   P+25(L'MSGDESC_STRUCID),MSGDESC_STRUCID                          
         GOTO1 =V(PRINTER)                                                      
         MVC   P+11(10),=CL10'VERSION: '                                        
         GOTO1 =V(HEXOUT),DMCB,MSGDESC_VERSION,P+25,L'MSGDESC_VERSION           
         GOTO1 =V(PRINTER)                                                      
         MVC   P+11(10),=CL10'REPORT:  '                                        
         GOTO1 =V(HEXOUT),DMCB,MSGDESC_REPORT,P+25,L'MSGDESC_REPORT             
         GOTO1 =V(PRINTER)                                                      
         MVC   P+11(10),=CL10'MSGTYPE: '                                        
         GOTO1 =V(HEXOUT),DMCB,MSGDESC_MSGTYPE,P+25,L'MSGDESC_MSGTYPE           
         GOTO1 =V(PRINTER)                                                      
         MVC   P+11(10),=CL10'EXPIRY:  '                                        
         GOTO1 =V(HEXOUT),DMCB,MSGDESC_EXPIRY,P+25,L'MSGDESC_EXPIRY             
         GOTO1 =V(PRINTER)                                                      
         MVC   P+11(10),=CL10'FEEDBACK:'                                        
         GOTO1 =V(HEXOUT),DMCB,MSGDESC_FEEDBACK,P+25,L'MSGDESC_FEEDBACK         
         GOTO1 =V(PRINTER)                                                      
         MVC   P+11(10),=CL10'ENCODING:'                                        
         GOTO1 =V(HEXOUT),DMCB,MSGDESC_ENCODING,P+25,L'MSGDESC_ENCODING         
         GOTO1 =V(PRINTER)                                                      
         MVC   P+11(10),=CL10'CHARSET: '                                        
         GOTO1 =V(HEXOUT),DMCB,MSGDESC_CODEDCHARSETID,P+25,            +        
               L'MSGDESC_CODEDCHARSETID                                         
         GOTO1 =V(PRINTER)                                                      
         MVC   P+11(10),=CL10'FORMAT:  '                                        
         MVC   P+25(L'MSGDESC_FORMAT),MSGDESC_FORMAT                            
         GOTO1 =V(PRINTER)                                                      
         MVC   P+11(10),=CL10'PRIORITY:'                                        
         GOTO1 =V(HEXOUT),DMCB,MSGDESC_PRIORITY,P+25,L'MSGDESC_PRIORITY         
         GOTO1 =V(PRINTER)                                                      
         MVC   P+11(12),=CL12'PERSISTENCE:'                                     
         GOTO1 =V(HEXOUT),DMCB,MSGDESC_PERSISTENCE,P+25,               +        
               L'MSGDESC_PERSISTENCE                                            
         GOTO1 =V(PRINTER)                                                      
         MVC   P+11(10),=CL10'MSGID: '                                          
         MVC   P+25(L'MSGDESC_MSGID),MSGDESC_MSGID                              
         GOTO1 =V(PRINTER)                                                      
         MVC   P+11(10),=CL10'CORRELID:'                                        
         MVC   P+25(L'MSGDESC_CORRELID),MSGDESC_CORRELID                        
         GOTO1 =V(PRINTER)                                                      
         MVC   P+11(10),=CL10'LENGTH:'                                          
         GOTO1 =V(HEXOUT),DMCB,MSGDESC_LENGTH,P+25,4                            
         GOTO1 =V(PRINTER)                                                      
         MVC   P+11(10),=CL10'DATALEN:'                                         
         GOTO1 =V(HEXOUT),DMCB,DATALENGTH,P+25,4                                
         GOTO1 =V(PRINTER)                                                      
*&&                                                                             
         ICM   R6,15,DATALENGTH    R6 = LENGTH OF DATA IN MESSAGE               
         BZ    MQBUF45                                                          
*                                  COPY 1ST RECORD FROM MQ BUFFER               
         SAM31                                                                  
         L     R1,AMQBUFXA                                                      
         MVC   BUFFER,0(R1)                                                     
         SAM24                                                                  
*                                                                               
         LA    R1,P+30                                                          
         MVC   0(4,R1),=C'None'                                                 
         CLI   MSGDESC_USERIDENTIFIER,C' '                                      
         BNH   MQBUF40A                                                         
         MVC   0(7,R1),=C'UserID='                                              
         MVC   7(12,R1),MSGDESC_USERIDENTIFIER                                  
         LA    R1,19(,R1)                                                       
         BRAS  RE,BACKSPC                                                       
MQBUF40A CLI   MSGDESC_REPLYTOQMGR,C' '                                         
         BNH   MQBUF40B                                                         
         MVC   0(10,R1),=C'ReplyQMGR='                                          
         MVC   10(30,R1),MSGDESC_REPLYTOQMGR                                    
         LA    R1,40(,R1)                                                       
         BRAS  RE,BACKSPC                                                       
MQBUF40B CLI   MSGDESC_REPLYTOQ,C' '                                            
         BNH   MQBUF40C                                                         
         MVC   0(11,R1),=C'ReplyQueue='                                         
         MVC   11(30,R1),MSGDESC_REPLYTOQ                                       
         LA    R1,41(,R1)                                                       
         BRAS  RE,BACKSPC                                                       
MQBUF40C CLI   MSGDESC_PUTAPPLNAME,C' '                                         
         BNH   MQBUF40D                                                         
         MVC   0(7,R1),=C'PutApp='                                              
         MVC   7(28,R1),MSGDESC_PUTAPPLNAME                                     
MQBUF40D PRNT  SenderInfo                                                       
         B     MQBUF41                                                          
*                                                                               
BACKSPC  CLI   0(R1),C' ' <------.  BUMP BACK THROUGH WHITE SPACE               
         BH    *+12 >---------.  |                                              
         AHI   R1,-1          |  |                                              
         B     *-12 >------------'                                              
         MVI   1(R1),C',' <-'       ADD A COMMA                                 
         LA    R1,3(,R1)            AND CONTINUE                                
         BR    RE                                                               
*                                                                               
MQBUF41  CLC   =C'LOG ',BUFFER+13       REPLAY LOGGED MESSAGE?                  
         BE    MQBUF42                  YES- MOVE MSG TO DAREBUF                
         CLC   =C'VERSION=2',BUFFER+13  VERSION 2?                              
         BNE   MQBUF46                  NO - PROCESS AS VERSION=1               
MQBUF42  BRAS  RE,MVTODARF              YES- MOVE MSG TO DAREBUF                
         B     MQBUFCLO            CLOSE QUEUE AND EXIT                         
*                                                                               
MQBUF45  MVC   P+11(37),=C'*** ERROR *** INCOMING RECORD IS NULL'               
         GOTO1 =V(PRINTER)                                                      
         MVI   NULLREC,C'Y'        RECORD IS NULL                               
         B     MQBUF10             GET NEXT MESSAGE                             
*                                                                               
MQBUF46  EQU   *                                                                
         GOTO1 =V(PRNTBL),DMCB,=C'MQ_GET',BUFFER,C'DUMP',(R6),=C'1D'            
*                                                                               
         AHI   R4,1                INCREMENT RECORD COUNTER                     
         L     RE,BYTECNTR                                                      
         AR    RE,R6                                                            
         ST    RE,BYTECNTR         ADD BYTE COUNT TO MESSAGE TOTAL              
*                                                                               
         LA    RE,BUFFER(R6)       RE = A(END OF DATA)                          
         MVC   0(2,RE),CRLF        (FOR BACKWARD COMPATIBILITY)                 
*                                                                               
         CLC   =C'+++DARE SEND',BUFFER  IS THIS THE FIRST LINE?                 
         BNE   MQBUF48                                                          
         MVI   VERFLAG,C'Y'        ASSUME VERSION FLAG IS OKAY                  
         CLC   =C'VERSION=1',BUFFER+13  YES -- IS IT THE RIGHT VERSION?         
         BE    *+8                                                              
         MVI   VERFLAG,C'N'        MISSING VERSION FLAG                         
*                                                                               
         SAM31                                                                  
         MVC   0(APPCM3Q,R3),APPCM3   CREATE FAKE +++DARE SEND RECORD           
         GOTO1 =A(DATETIME),(RC)   PUT TIME INTO MESSAGE                        
         MVC   18(6,R3),HHMMSS                                                  
         MVC   24(2,R3),CRLF       MOVE IN CRLF                                 
         LA    R3,APPCM3Q+2(,R3)   BUMP BUFFER POINTER                          
         SAM24                                                                  
         B     MQBUF85             GET NEXT RECORD                              
*                                                                               
MQBUF48  CLC   =C'+++DARE END',BUFFER   IS THIS THE LAST LINE?                  
         BNE   MQBUF50                                                          
         SAM31                                                                  
         MVC   0(APPCM4Q,R3),APPCM4   CREATE FAKE +++DARE END RECORD            
         GOTO1 =A(DATETIME),(RC)   PUT TIME INTO MESSAGE                        
         MVC   17(6,R3),HHMMSS                                                  
         CVD   R4,DUB              RECORD COUNTER                               
         UNPK  30(6,R3),DUB                                                     
         OI    35(R3),X'F0'        TOTAL NUMBER OF RECORDS                      
         MVC   36(2,R3),CRLF       MOVE IN CRLF                                 
         LA    R3,APPCM4Q+2(,R3)   BUMP BUFFER POINTER                          
         B     MQBUF90             SHOULD BE NO MORE MESSAGES                   
*                                                                               
MQBUF50  XC    MYKEY,MYKEY         TABLE OF KNOWN DARE RECORDS IN SPEC          
         MVC   MYKEY(6),BUFFER                                                  
*                                                                               
         LA    R1,BPRMDRCS         SETTING ALL 6 PARMS FOR BINSRCH              
         LAY   R2,DRALLRCS                                                      
         LAY   RE,DRALLRCX         CALCULATE # OF ENTRIES                       
         SR    RE,R2                                                            
         SRL   RE,3                DIVIDE BY 8 (RECORD LENGTH)                  
         ST    RE,8(R1)            BINSRCH PARM3 IS # OF ENTRIES                
         LA    RE,1(RE)            SO WE WON'T GET "TABLE IS FULL"              
         ST    RE,20(R1)           BINSRCH PARM6 IS MAX # OF ENTRIES            
*                                                                               
         L     RF,=V(BINSRCH)                                                   
         GOTO1 (RF),BPRMDRCS,(0,MYKEY),(R2),,8,6                                
         CLI   0(R1),X'01'         RECORD NOT FOUND?                            
         BE    MQBUF80             RECORD NOT IN TABLE -- THAT'S OK             
*                                                                               
         L     R2,0(R1)            R2 = A(ENTRY IN DARERECS)                    
         LH    R2,6(R2)            R2 = LENGTH OF RECORD IN SPEC                
         CR    R6,R2                                                            
         BNH   MQBUF70                                                          
*                                                                               
         MVC   P+11(41),=C'*** ERROR *** INCOMING RECORD IS TOO LONG'           
         GOTO1 =V(PRINTER)                                                      
         LR    R0,R6               RECORD IS LONGER THAN IT SHOULD BE           
         AHI   R0,2                ADD TWO FOR CRLF                             
         GOTO1 =V(PRNTBL),DMCB,=C'TOOLONG',A(BUFFER),                  +        
               C'DUMP',(R0),=C'1D'                                              
         MVI   TOOLONG,C'Y'        RECORD IS TOO LONG                           
*                                                                               
MQBUF70  SAM31                                                                  
         MVI   0(R3),C' '                                                       
         BCTR  R2,0                FOR MVC TRICK                                
         BCTR  R2,0                FOR EX                                       
         EX    R2,MQBUFSPC                                                      
         BCTR  R6,0                                                             
         EX    R6,MQBUFDTA                                                      
         LA    R3,2(R2,R3)         POINT TO END OF DATA IN BUFFER               
         MVC   0(2,R3),CRLF        MOVE IN CRLF                                 
         LA    R3,2(R3)                                                         
         SAM24                                                                  
         B     MQBUF85                                                          
*                                                                               
MQBUFSPC MVC   1(0,R3),0(R3)       PRE-FILL BUFFER WITH SPACES                  
MQBUFDTA MVC   0(0,R3),BUFFER      PLACE DATA IN BUFFER                         
*                                                                               
MQBUF80  L     R6,DATALENGTH                                                    
         SAM31                                                                  
         AHI   R6,2                FOR CRLF                                     
         BCTR  R6,0                                                             
         EX    R6,MQBUFDTA         PLACE DATA IN BUFFER                         
         LA    R3,1(R6,R3)         POINT TO NEXT POSITION IN BUFFER             
         SAM24                                                                  
*                                  WAIT NO MORE THAN 1 MIN FOR NEXT MSG         
MQBUF85  MVC   GETMSGOPTS_WAITINTERVAL,=A(20000)    20 SEC                      
         B     MQBUF10                                                          
*                                                                               
MQBUF90  SAM31                                                                  
         MVI   0(R3),X'FF'         END OF MESSAGE MARKER                        
*                                                                               
MQBUFXOK SAM31                                                                  
         GOTO1 =A(PRTDARBF),(RC)                                                
*                                                                               
         L     RE,ADAREBUF                                                      
         A     RE,=A(DAREBUFL)                                                  
         CR    R3,RE               HAVE WE GONE PAST BUFFER END?                
         JH    *+2                 YES -- MUST INCREASE DAREBUFL                
*                                                                               
         L     RE,ADAREBUF                                                      
         SR    R3,RE                                                            
         ST    R3,BUFFERLENGTH2    SAVE BYTE COUNT IN BUFFERLENGTH2             
*                                                                               
MQBUFCLO MVC   CLOSE_OPTIONS,=A(MQCO_NONE)                                      
         LA    R2,CSQBCLOS_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     CLOSE QUEUE                                  
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         JNE   *+2                                                              
*                                                                               
         CR    RB,RB               SET CC EQUAL                                 
         B     MQBUFXX                                                          
*                                                                               
MQBUFXBD LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
MQBUFXX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
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
*                                                                               
         CLI   TSKTSMF,C'Y'        FLAG THESE AS TEST RECORDS?                  
         BNE   *+8                                                              
         OI    SMFFLAGS,SMFTEST    YES                                          
*                                                                               
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   SMFDATA(0),0(R3)    DATA PORTION OF RECORD                       
*                                  SMFOUT TYPE 6 FOR DARE                       
         GOTO1 =V(SMFOUT),DMCB,6,WORK                                           
         DROP  R4                                                               
*                                                                               
* NO LONGER PRINTING SMF RECORDS TO SYSOUT                                      
*                                                                               
*        AHI   R2,1                RESTORE RECORD LENGTH                        
*        AHI   R2,2                ADD TWO FOR LENGTH ITSELF                    
*        SAM24                                                                  
*        GOTO1 =V(PRNTBL),DMCB,=C'SMF RECORD',(R4),C'DUMP',(R2),=C'1D'          
*        SAM31                                                                  
*                                                                               
*        AHI   R2,-SMFOVLQ                                                      
*        CHI   R2,L'P                                                           
*        BNL   PS30                                                             
*        BCTR  R2,0                                                             
*        EX    R2,*+8                                                           
*        B     *+10                                                             
*        MVC   P(0),0(R3)          DATA PORTION OF RECORD                       
*        GOTO1 =V(PRINTER)                                                      
*        B     PSX                                                              
*                                                                               
*S30     MVC   P,0(R3)             PRINT OUT 1ST DATA                           
*        GOTO1 =V(PRINTER)                                                      
*                                                                               
PSX      XIT1                                                                   
         LTORG                                                                  
                                                                                
***********************************************************************         
* ROUTINE THAT GETS CALLED BY AGYHDR TO DEDUCE THE RECEIVING ID                 
* OF THE ORDER BASED ON THE STATION'S REP AND AGENCY'S OFFICE                   
***********************************************************************         
FINDRCVR NMOD1 0,FINDRCVR                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
         MVI   FNRCVRFL,0          INIT FLAGS                                   
*                                                                               
         LR    R6,R3               THIS ADDRESS IS IN HIGH CORE!!!              
         USING PAGYHDRD,R6                                                      
*                                                                               
         SAM24                                                                  
         LA    R3,=C'STATTABL'                                                  
         MVC   P+30(8),DARE                                                     
         MVC   P+40(8),0(R3)                                                    
         PRNT  Enqueue                                                          
         ENQ   (DARE,(3),E,8)      ENQUEUE THE STATION TABLE                    
*                                                                               
         LA    R3,=C'OFEXTABL'                                                  
         MVC   P+30(8),DARE                                                     
         MVC   P+40(8),0(R3)                                                    
         PRNT  Enqueue                                                          
         ENQ   (DARE,(3),E,8)      ENQUEUE THE OFFICE EXCEPTION TABLE           
         SAM31                                                                  
*                                                                               
         OI    FNRCVRFL,FNRCVROP   TABLES HAVE BEEN ENQ'D                       
*                                                                               
         CLC   PAHDFSTA,SPACES     FIRST EDI SEND STATION SET?                  
         BNH   *+10                NO, WE'LL USE WHAT WE GOT                    
         MVC   STATION,PAHDFSTA    YES, USE FIRST EDI SEND STATION              
*                                                                               
* FOUND FIRST RECORD FOR THAT DSTA, LET'S SEE IF EFFECTIVE                      
         CLC   PAHDFSNT(6),SPACES   ANY FIRST EDI SEND DATE?                    
         BH    FR020                YES, USE THIS DATE LOOKING FOR DSTA         
         GOTO1 =V(DATCON),DMCB,(5,0),(3,DUB)  NO, PROCESS AS IF NEW             
         B     FR030                                                            
*                                                                               
FR020    MVC   WORK,PAHDFSNT       BECAUSE PAHDFSNT IS NOW 31 BIT               
*                                  PARAM TYPES WIPES HOB OF 31BIT ADDR          
         GOTO1 =V(DATCON),DMCB,(0,WORK),(3,DUB)                                 
FR030    SR    RE,RE               EFFECTIVE DATE WAS NEGATED NOT FF'D          
         ICM   RE,7,DUB                                                         
         LNR   RE,RE                                                            
         STCM  RE,7,DUB                                                         
*                                                                               
         MVC   MYKEY(1),MEDIA                                                   
         MVC   MYKEY+1(6),STATION                                               
         MVC   MYKEY+7(3),DUB                                                   
*                                                                               
         L     R2,TSKSTATB         R2 = A(ENTRY IN TBL OF DSTAS/REPS)           
         L     R0,4(R2)            RECORD COUNT                                 
         XC    DMCB,DMCB                                                        
         L     RF,=V(BINSRCH)                                                   
         GOTO1 (RF),DMCB,(X'02',MYKEY),8(R2),(R0),STATTBLQ,(0,10),(R0)          
*                                                                               
         L     R2,0(R1)            A(FOUND RECORD) AFTER BINSRCH                
         USING STATTABD,R2                                                      
FR055    CLC   MEDIA,STATMED       MATCH ON MEDIA?                              
         BNE   FRERROR                                                          
         CLC   STATION,STATSTN     MATCH ON STATION?                            
         BNE   FRERROR                                                          
*                                                                               
FR060    CLC   STATEFDT,DUB        THIS DSTA RECORD IS APPLICABLE?              
         BNL   FR070               YES, OLDER THAN 1ST EDI SEND                 
         LR    R3,R2               R3 = COPY THIS IN CASE                       
         AHI   R2,STATTBLQ                                                      
         B     FR055                                                            
*                                                                               
FR070    CLC   STATNSTN,=5X'FF'    DO WE HAVE A NEW STATION CALLS?              
         BE    FR090               NO                                           
         CLC   STATNSTD,=5X'FF'      IS IT AN EFFECTIVE DATE?                   
         BE    FR080                 NO, OLD CALLS                              
         GOTO1 =V(DATCON),DMCB,(6,STATNSTD),(3,DUB)                             
         MVC   STATION,STATNSTN    WE'RE USING THIS STATION AND EFF NOW         
         MVI   STATION+5,C' '                                                   
         MVC   OSTATION,STATSTN    STATION WE WERE LOOKING FOR IS OLD           
         B     FR030                                                            
*                                                                               
FR080    MVC   OSTATION,STATNSTN   NEW CALLS WITH FFFF EFF DATE IS OLD          
*                                                                               
FR090    BRAS  RE,CKGRAY           CHECK IF GRAY STATION SITUATION              
         JNE   FRERROR             TREAT AS AN ERROR CONDITION                  
*                                                                               
         MVC   PAHDQSTA,STATION    OVERWRITE PAGYHDR WITH WHAT WE HAVE          
         CLC   OSTATION,SPACES                                                  
         BNH   *+10                                                             
         MVC   PAHDOLDS(5),OSTATION                                             
*                                                                               
FR100    GOTO1 AGETDARE,DMCB,(C'R',0),STATREP,A(IO1),IO1LQ,DATAMGR              
         JE    FR105                                                            
         CLI   3(R1),10            MISSING VENDOR FIELD IN IDI RECORD           
         JNE   FRERROR             NO, SOME OTHER ERROR                         
*                                                                               
* SPECIAL CODE FOR CASHPLUS (CE-MN). IF STATION IS KNTV, ROUTE THE              
* ORDER TO KNTV INSTEAD OF NBC NSO                                              
*                                                                               
FR105    LAY   R3,IO1                                                           
         USING DAREPTD,R3                                                       
*                                                                               
         TM    DAPTFLG2,DPF2EOC    END OF CONTRACT?                             
         BNZ   FRERR005            YES , ERROR UNKNOWN DESTINATION ID           
*                                                                               
         TM    DAPTFLG1,DPF1MORP   X'10' - AM I A MEDIA OCEAN REP?              
         BZ    FR110                                                            
         CLC   =C'NBC',DAPTRPPF    NBC REP?                                     
         BNE   FR110                                                            
         CLC   =C'KNTV',PAHDQSTA   STATION IS KNTV?                             
         BNE   FR110                                                            
         CLC   =C'CE MN',PAHDROUT  AGY ROUTE IS CE-MN?                          
         BNE   FR110                                                            
         MVC   RECEIVER,SPACES     SET RECEIVER TO KNTV                         
         MVC   RECEIVER(4),=C'KNTV'                                             
         B     FR230                                                            
*                                                                               
* FOR INCOMING AGENCY XML ORDERS, ONLY DDS REPPED STATIONS ARE ALLOWED          
* TO PASS THROUGH THE DISPATCHER                                                
* !!!! THIS CHECK REMOVED ON 7/1/14 !!!! ESPERANTO WILL FILTER                  
*                                                                               
* ALLOW MO REPPED STATIONS TO RECEIVE XML ORDERS AS WELL                        
*                                                                               
FR110    DS    0H                                                               
         MVI   AMIXML,C'N'                                                      
         CLI   PAHDREVN,C'+'       XML ORDER?                                   
         BNE   FRV2                                                             
         MVI   AMIXML,C'Y'         YES, I AM A XML ORDER                        
*                                                                               
FRV2     DS    0H                                                               
         CLI   PAHDVERS,PAHDV2     C'2'-ESPERANTO FOR NATIONAL (REP)            
         BNE   FRV3                                                             
*                                                                               
         ZIC   R4,DAPTLPFX         SEE IF THE RECEIVER IS THE SAME              
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   PAHDTOID(0),DAPTRPPF                                             
         BNE   FRERR015            WRONG REP, SEND ERROR                        
*                                                                               
         TM    DAPTFLG1,DPF1NODS   X'01' - NOT A DDS REP?                       
         BNZ   FRERR016            NO, ERROR NOT A DDS REP                      
         MVC   RECEIVER,PAHDTOID   WE'RE DONE WITH ESPERANTO                    
         B     FR230                                                            
*                                                                               
FRV3     CLI   PAHDVERS,PAHDV3     C'3'-ESPERANTO FOR LOCAL (STA)               
         BNE   FRV8                                                             
         MVC   RECEIVER,STATUID    SEE IF RECEIVER IS RECEIVING ID              
         B     FR230                                                            
*                                                                               
FRV8     CLI   PAHDVERS,PAHDV8     C'8'- SEND DIRECT TO STATION?                
         BNE   FR160               NO                                           
         CLC   STATUID,SPACES      ANY RECEIVING ID FOR THIS DSTA?              
         BNH   FRERR005                                                         
         MVC   RECEIVER,STATUID    YES, WE'RE USING IT                          
         B     FR230                                                            
         DROP  R6                                                               
*                                                                               
FRERR015 MVC   ERRCODE,=C'015'     WRONG REP                                    
         B     FRERROR               RETURN WITH THIS ERROR                     
*                                                                               
FRERR016 MVC   ERRCODE,=C'016'     NO, NOT A DDS REP                            
         B     FRERROR               RETURN WITH THIS ERROR                     
*                                                                               
FRERR005 MVC   ERRCODE,=C'005'     NONE, INVALID DESTINATION ID                 
*                                                                               
FRERROR  OI    FNRCVRFL,FNRCVRBD   DID NOT FIND RECEIVING ID                    
         B     FR230                                                            
*                                                                               
FR160    CLC   RECEIVER,SPACES     RECEIVER FILLED IN FROM AGYHDR?              
         BNH   FR175                                                            
         CLC   CONTRACT,=8C'0'     ANYTHING TO ZERO OUT?                        
         BNH   FR180               NO, LEAVE ALONE (COULD BE NOTDARE)           
*                                                                               
         ZIC   R4,DAPTLPFX                                                      
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   RECEIVER(0),DAPTRPPF                                             
         BE    FR180                                                            
*   REP ID IS NOW DIFFERENT                                                     
         OC    STATCITY,STATCITY   ANY HOME MARKET CITY CODE?                   
         BZ    FR170                                                            
         CLC   =C'**',STATCITY     APPLIES TO ALL CITIES?                       
         BE    FR165                                                            
         CLC   STATCITY,ROUTCODE+3 YES -- MATCH ON AGENCY ROUTING CODE?         
         BNE   FR170                                                            
FR165    CLC   RECEIVER,STATUID    YES -- USE STATION USERID, NOT REP           
         BE    FR180                                                            
FR170    MVC   CONTRACT,=8X'FF'    FF TO SIGNIFY LEAVE CONTRACT ALONE           
         B     FR180                                                            
FR175    MVC   CONTRACT,=8C'0'                                                  
         TM    STATFLG1,SF1H2UID                                                
         BO    FR190                                                            
*                                                                               
FR180    CLC   RECEIVER,STATUID    RECEIVER MATCHS STATION UID?                 
         BE    FR230               DON'T NEED HOME MARKET THEN                  
         OC    STATCITY,STATCITY   ANY HOME MARKET CITY CODE?                   
         BZ    FR190                                                            
         CLC   =C'**',STATCITY     APPLIES TO ALL CITIES?                       
         BE    FR185                                                            
         CLC   STATCITY,ROUTCODE+3 YES -- MATCH ON AGENCY ROUTING CODE?         
         BNE   FR190                                                            
FR185    TM    STATFLG1,SF1H2UID                                                
         BO    FR190                                                            
         MVC   RECEIVER,STATUID    YES -- USE STATION USERID, NOT REP           
         OI    FNRCVRFL,FNRCVRHM   X'01' - HOME MKT RECEIVING ID USED           
         B     FR230                                                            
*                                                                               
FR190    MVC   RECEIVER,SPACES                                                  
         ZIC   R4,DAPTLPFX         CONSTRUCT RECEIVING ID                       
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   RECEIVER(0),DAPTRPPF REP-ID                                      
         TM    DAPTFLG1,DPF1NOOF   X'80' - APPEND OFFICE CODE?                  
         BO    FR230               NO                                           
*                                                                               
         LA    R4,RECEIVER+1(R4)   POINT BEYOND REP-ID                          
         MVC   0(2,R4),ROUTCODE+3  OFFICE                                       
         DROP  R3                                                               
*                                                                               
         L     R3,TSKOEXTB         TABLE OF OFFICE EXCEPTIONS                   
         USING OFEXTABD,R3                                                      
FR200    CLI   0(R3),X'FF'                                                      
         BE    FR230               NO EXCEPTION -- USE ROUTING CODE             
         CLC   MEDIA,OFEXMED                                                    
         BNE   FR210                                                            
         CLC   ROUTCODE,OFEXROUT                                                
         BNE   FR210                                                            
         CLC   STATREP,OFEXREP                                                  
         BE    FR220               GOT IT                                       
         DROP  R2                                                               
*                                                                               
FR210    LA    R3,OFEXTBLQ(,R3)    BUMP TO NEXT ENTRY                           
         B     FR200                                                            
*                                                                               
FR220    MVC   0(2,R4),OFEXOFF     PUT OFFICE OVERRIDE IN RECEIVING ID          
         DROP  R3                                                               
*                                                                               
FR230    TM    FNRCVRFL,FNRCVROP   WERE TABLES OPENED?                          
         BZ    FR240                NO                                          
         LA    R3,=C'STATTABL'                                                  
         MVC   P+30(8),DARE                                                     
         MVC   P+40(8),0(R3)                                                    
         PRNT  Dequeue                                                          
         SAM24                                                                  
         DEQ   (DARE,(3),8)        DEQUEUE THE STATION TABLE                    
*                                                                               
         LA    R3,=C'OFEXTABL'                                                  
         MVC   P+30(8),DARE                                                     
         MVC   P+40(8),0(R3)                                                    
         PRNT  Dequeue                                                          
         DEQ   (DARE,(3),8)        DEQUEUE THE OFFICE EXCEPTION TABLE           
*                                                                               
FR240    TM    FNRCVRFL,FNRCVRBD   WERE WE SUCCESSFUL?                          
         BZ    FRXYES              NO                                           
         CLC   ERRCODE,=C'000'     ERROR CODE SET?                              
         BNE   FRXNO               YES                                          
         MVC   ERRCODE,=C'006'     NO, SET UNKNOWN STATION                      
         B     FRXNO                                                            
*                                                                               
FRXYES   SR    RE,RE                                                            
FRXNO    LTR   RE,RE                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* CHECK IF WE HAVE A GRAY STATION SITUATION                                     
*                                                                               
* ON ENTRY:    (R2)                A(CURRENT EFF DSTA IN STATTABL)              
***********************************************************************         
CKGRAY   NTR1                                                                   
         USING STATTABD,R2                                                      
         CLC   =C'NOR',STATREP     IS THE CURRENT REP NOR?                      
         JNE   CKGRAYY                                                          
*                                                                               
         LA    R3,GRAYSTAS                                                      
         LA    R0,GRAYSTAX                                                      
CKGRAY10 CR    R3,R0                                                            
         JNL   CKGRAYY             NOT ONE OF THE GRAY STATIONS                 
         CLC   STATION,0(R3)                                                    
         JE    CKGRAY20                                                         
         LA    R3,L'STATION(R3)                                                 
         J     CKGRAY10            CHECK NEXT STATION IN MY LIST                
*                                                                               
CKGRAY20 CLC   RECEIVER,SPACES     RECEIVING ID FILLED IN?                      
         JNH   CKGRAYY             NONE, IT IS A BRAND NEW ORDER                
*                                                                               
         CLC   RECEIVER,STATUID    SAME AS HOME MKT RCVR IN CURR DSTA?          
         JE    CKGRAYY             YES, WE'RE OKAY                              
*                                                                               
         LA    R2,STATTBLQ(R2)     CHECK NEXT DSTA                              
         CLC   STATION,STATSTN     SAME STATION STILL?                          
         JNE   CKGRAYN                                                          
*                                                                               
CKGRAYY  SR    RC,RC                                                            
CKGRAYN  LTR   RC,RC                                                            
         XIT1  REGS=(R2)           RETURN WITH ENTRY IN STATTABL                
         DROP  R2                                                               
         LTORG                                                                  
GRAYSTAS DS    0CL6                LIST OF GRAY STATIONS                        
         DC    C'EAGMT '                                                        
         DC    C'EAGTT '                                                        
         DC    C'EAHUT '                                                        
         DC    C'EBKOT '                                                        
         DC    C'ECTVT '                                                        
         DC    C'EDBJT '                                                        
         DC    C'EECPT '                                                        
         DC    C'EIBWT '                                                        
         DC    C'EIFRT '                                                        
         DC    C'EITNT '                                                        
         DC    C'EIYET '                                                        
         DC    C'EJHGT '                                                        
         DC    C'EJRTT '                                                        
         DC    C'EKYTT '                                                        
         DC    C'ELUCT '                                                        
         DC    C'ENDUT '                                                        
         DC    C'EOWTT '                                                        
         DC    C'ERDWT '                                                        
         DC    C'ESAWT '                                                        
         DC    C'ESAZT '                                                        
         DC    C'ESVFT '                                                        
         DC    C'ESWGT '                                                        
         DC    C'ETOKT '                                                        
         DC    C'ETVGT '                                                        
         DC    C'ETVYT '                                                        
         DC    C'EVLTT '                                                        
         DC    C'EYMTT '                                                        
         DC    C'EZAWT '                                                        
         DC    C'FXIIT '                                                        
         DC    C'GBKOT '                                                        
         DC    C'GEAUT '                                                        
         DC    C'GILXT '                                                        
         DC    C'GITNT '                                                        
         DC    C'GJRTT '                                                        
         DC    C'GLUCT '                                                        
         DC    C'GMTVT '                                                        
         DC    C'GSWGT '                                                        
         DC    C'GTOKT '                                                        
         DC    C'GTVGT '                                                        
         DC    C'GTVYT '                                                        
         DC    C'HHSVT '                                                        
         DC    C'KALBT '                                                        
         DC    C'KBTXT '                                                        
         DC    C'KCHWT '                                                        
         DC    C'KCRGT '                                                        
         DC    C'KCWYT '                                                        
         DC    C'KDCUT '                                                        
         DC    C'KEVNT '                                                        
         DC    C'KFYRT '                                                        
         DC    C'KGNST '                                                        
         DC    C'KGWNT '                                                        
         DC    C'KIITT '                                                        
         DC    C'KJCTT '                                                        
         DC    C'KKCOT '                                                        
         DC    C'KKTVT '                                                        
         DC    C'KMOTT '                                                        
         DC    C'KMVTT '                                                        
         DC    C'KNDXT '                                                        
         DC    C'KNOET '                                                        
         DC    C'KNOPT '                                                        
         DC    C'KNPLT '                                                        
         DC    C'KOLNT '                                                        
         DC    C'KOLOT '                                                        
         DC    C'KOSAT '                                                        
         DC    C'KOTAT '                                                        
         DC    C'KQCDT '                                                        
         DC    C'KSCWT '                                                        
         DC    C'KSFYT '                                                        
         DC    C'KSNBT '                                                        
         DC    C'KSPRT '                                                        
         DC    C'KSTFT '                                                        
         DC    C'KSVTT '                                                        
         DC    C'KTUUT '                                                        
         DC    C'KUMVT '                                                        
         DC    C'KVLYT '                                                        
         DC    C'KWCHT '                                                        
         DC    C'KWTXT '                                                        
         DC    C'KXIIT '                                                        
         DC    C'KXIPT '                                                        
         DC    C'KXNDT '                                                        
         DC    C'KYEST '                                                        
         DC    C'KYLXT '                                                        
         DC    C'KYTVT '                                                        
         DC    C'MNOET '                                                        
         DC    C'MVLYT '                                                        
         DC    C'NALBT '                                                        
         DC    C'NBTXT '                                                        
         DC    C'NCRGT '                                                        
         DC    C'NCWYT '                                                        
         DC    C'NGNST '                                                        
         DC    C'NGWNT '                                                        
         DC    C'NKCOT '                                                        
         DC    C'NKTVT '                                                        
         DC    C'NMVTT '                                                        
         DC    C'NNOET '                                                        
         DC    C'NOSAT '                                                        
         DC    C'NQCDT '                                                        
         DC    C'NSCWT '                                                        
         DC    C'NSFYT '                                                        
         DC    C'NSGWT '                                                        
         DC    C'NSNBT '                                                        
         DC    C'NTUUT '                                                        
         DC    C'NUMVT '                                                        
         DC    C'NVLYT '                                                        
         DC    C'NWCHT '                                                        
         DC    C'NWTXT '                                                        
         DC    C'NYEST '                                                        
         DC    C'NYFRT '                                                        
         DC    C'NYLXT '                                                        
         DC    C'NYTVT '                                                        
         DC    C'OCRGT '                                                        
         DC    C'ODBJT '                                                        
         DC    C'OGNST '                                                        
         DC    C'OJCTT '                                                        
         DC    C'OKCOT '                                                        
         DC    C'OSFYT '                                                        
         DC    C'OSGWT '                                                        
         DC    C'OSPRT '                                                        
         DC    C'OYTVT '                                                        
         DC    C'UXIIT '                                                        
         DC    C'WAGMT '                                                        
         DC    C'WAGTT '                                                        
         DC    C'WAHUT '                                                        
         DC    C'WBKOT '                                                        
         DC    C'WBXXT '                                                        
         DC    C'WCAVT '                                                        
         DC    C'WCTVT '                                                        
         DC    C'WDBJT '                                                        
         DC    C'WEAUT '                                                        
         DC    C'WECPT '                                                        
         DC    C'WHSVT '                                                        
         DC    C'WIBWT '                                                        
         DC    C'WIFRT '                                                        
         DC    C'WILXT '                                                        
         DC    C'WITNT '                                                        
         DC    C'WIYET '                                                        
         DC    C'WJHGT '                                                        
         DC    C'WJRTT '                                                        
         DC    C'WKYTT '                                                        
         DC    C'WLUCT '                                                        
         DC    C'WMTVT '                                                        
         DC    C'WNDUT '                                                        
         DC    C'WOVAT '                                                        
         DC    C'WOWTT '                                                        
         DC    C'WQCWT '                                                        
         DC    C'WRDWT '                                                        
         DC    C'WRGXT '                                                        
         DC    C'WSAWT '                                                        
         DC    C'WSAZT '                                                        
         DC    C'WSVFT '                                                        
         DC    C'WSWGT '                                                        
         DC    C'WTAPT '                                                        
         DC    C'WTOKT '                                                        
         DC    C'WTVGT '                                                        
         DC    C'WTVYT '                                                        
         DC    C'WVAWT '                                                        
         DC    C'WVLTT '                                                        
         DC    C'WYMTT '                                                        
         DC    C'WZAWT '                                                        
GRAYSTAX DC    X'00'                                                            
         EJECT                                                                  
CHKRCVR  NMOD1 0,CHKRCVR                                                        
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* EXAMINE THE RECEIVING USERID IN 'RECEIVER', AND MODIFY IT IF                  
* NECESSARY. THIS SHOULD ONLY BE NEEDED IN UNUSUAL CASES.                       
*                                                                               
***********************************************************************         
**TEMP CODE TO REDIRECT HAWORTH ORDER BACK TO DDS-REP, YYUN, 12/11/03**         
***********************************************************************         
*        CLI   RCVIDFLG,C'Y'       RECEIVER ID IS NULL?                         
*        BNE   CHKRCV05            NO - SKIP                                    
*                                                                               
***********************************************************************         
**TEMP CODE TO REDIRECT ALL NBC XML ORDERS TO DDS-REP, SKUI  03/11/05**         
**COMMENTED OUT,NBC XML ORDERS WILL GO TO MO AGAIN,    HQIU  08/24/05**         
***********************************************************************         
*        CLI   AMIXML,C'Y'         ALL NBC XML ORDERS GO TO NBC AT DDS          
*        BNE   CHKRCV01                                                         
*        MVI   AMIXML,C'N'         RESET                                        
*        B     CHKRCV02                                                         
*                                                                               
* ALL HAWORTH (HMEMN) ORDERS WILL NOW GO TO MEDIAOCEAN                          
*                                                                               
*&&DO                                                                           
CHKRCV01 DS    0H                                                               
         CLC   =C'MQ1',TSKNAME                                                  
         BNE   CHKRCV05                                                         
         CLC   =C'HMMMN',SENDER    SENDER=HMMMN - HAWORTH?                      
         BE    CHKRCV02                                                         
         CLC   =C'HMMLA',SENDER    SENDER=HMMLA - HAWORTH?                      
         BE    CHKRCV02                                                         
         CLC   =C'HMEMN',SENDER    SENDER=HMEMN - HAWORTH?                      
         BNE   CHKRCV05                                                         
*                                                                               
CHKRCV02 DS    0H                                                               
         CLC   =C'NBC',RECEIVER    NBC?                                         
         BNE   CHKRCV05                                                         
         MVC   RECEIVER+5(3),=C'-DS'                                            
         MVI   RCVIDFLG,C'N'       SET IT BACK TO N                             
         B     CHKRCVOK                                                         
*&&                                                                             
* STRATA STATIONS ARE 2ND REPPED BY VIDEA NOW                                   
*&&DO                                                                           
CHKRCV05 BRAS  RE,STRATARP         STRATA REPPED?                               
*&&                                                                             
CHKRCV20 CLC   =C'KNA',RECEIVER    KATZ NATIONAL IS NO LONGER VALID             
         BE    CKRCVKAM                                                         
*                                                                               
         CLC   =C'PETPT',RECEIVER  CHECK FOR OBSOLETE PETRY USERID              
         BE    CKRCVPET                                                         
*                                                                               
         CLC   =C'KTZ',RECEIVER    CHECK FOR OBSOLETE KATZ USERID               
         BNE   CHKRCVOK                                                         
*                                                                               
         CLI   MEDIA,C' '                                                       
         BE    CHKRCVOK            NO STATION, SO CAN'T LOOK IT UP              
*                                                                               
         LA    R3,=C'STATTABL'                                                  
         MVC   P+30(8),DARE                                                     
         MVC   P+40(8),0(R3)                                                    
         PRNT  Enqueue                                                          
         ENQ   (DARE,(3),E,8)      ENQUEUE THE STATION TABLE                    
*                                                                               
         XC    MYKEY,MYKEY                                                      
         MVC   MYKEY(1),MEDIA                                                   
         MVC   MYKEY+1(6),STATION                                               
*                                                                               
         L     R2,TSKSTATB         TABLE OF STATIONS/REPS                       
         L     R0,4(R2)            RECORD COUNT                                 
         XC    DMCB,DMCB                                                        
         L     RF,=V(BINSRCH)                                                   
         GOTO1 (RF),DMCB,(X'02',MYKEY),8(R2),(R0),STATTBLQ,(0,10),(R0)          
*                                                                               
         L     R2,0(R1)                                                         
         USING STATTABD,R2                                                      
         CLC   MEDIA,STATMED                                                    
         JNE   *+2                                                              
         CLC   STATION,STATSTN                                                  
         JNE   *+2                 STATION HAS DISAPPEARED                      
*                                                                               
         MVC   RECEIVER(3),STATREP CHANGE KTZ WITH NEW KATZ REP                 
*                                                                               
         LA    R3,=C'STATTABL'                                                  
         MVC   P+30(8),DARE                                                     
         MVC   P+40(8),0(R3)                                                    
         PRNT  Dequeue                                                          
         DEQ   (DARE,(3),8)        DEQUEUE THE STATION TABLE                    
*                                                                               
CHKRCVOK CR    RB,RB               SET CC EQUAL                                 
         B     CHKRCVX                                                          
*                                                                               
CKRCVKAM MVC   RECEIVER(3),=C'KAM' PREFIX 'KNA' BECOMES 'KAM'                   
         B     CHKRCVOK                                                         
*                                                                               
CKRCVPET MVC   RECEIVER+3(5),RECEIVER+5  "PETNYXXXXX" BECOMES. . .              
         MVC   RECEIVER+8(2),SPACES      "PETXXXXX  "                           
         B     CHKRCVOK                                                         
*                                                                               
CHKRCVNO LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
CHKRCVX  XIT1                                                                   
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* STRATA REPPED STATION?                                                        
***********************************************************************         
STRATARP NTR1                                                                   
         LA    RE,STRATAST                                                      
         USING STRASTAD,RE                                                      
STRARP10 CLI   0(RE),0             EOT?                                         
         JE    STRARPX             YES, NOT STRATA REPPED                       
*                                                                               
         CLC   STRASTA,STATION     MATCH ON STATION?                            
         JE    STRARP20            YES                                          
STRARP15 LA    RE,STRASTAL(RE)     BUMP TO NEXT ENTRY                           
         J     STRARP10                                                         
*                                                                               
STRARP20 CLC   STRAOFF,=C'**'      FOR ALL OFFICES?                             
         JE    STRARP30            YES                                          
*                                                                               
         CLC   STRAOFF,ROUTCODE+3  MATCH ON OFFICE?                             
         JNE   STRARP15            NO                                           
*                                                                               
STRARP30 MVC   RECEIVER,SPACES                                                  
         LLC   RF,STRAUPLN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   RECEIVER(0),STRAUIDP YES, OVERRIDE THE RECEIVER                  
         LA    R1,RECEIVER                                                      
         LA    R1,1(R1,RF)                                                      
         MVC   0(2,R1),STRAOFF      APPEND THE OFFICE                           
*                                                                               
STRARPX  XIT1                                                                   
         DROP  RE                                                               
*                                                                               
* TABLE OF STATIONS THAT WILL BE REPPED BY                                      
*                                                                               
STRATAST DS    0CL(STRASTAL)                                                    
* STRATA REP'D STATIONS                                                         
*****    DC    C'YYYET',C'NY',AL1(3),CL8'ETS'  TESTING ON TST                   
* FOR STRATA UAT                                                                
         DC    C'YYRBT',C'CH',AL1(3),CL8'TTS'  TESTING ON ADV                   
         DC    C'YYRBT',C'AT',AL1(3),CL8'TTS'  TESTING ON ADV                   
         DC    C'YYCGT',C'CH',AL1(3),CL8'TTS'  TESTING ON ADV                   
         DC    C'YYCGT',C'AT',AL1(3),CL8'TTS'  TESTING ON ADV                   
* FOR STRATA PRODUCTION                                                         
         DC    C'YYKET',C'NY',AL1(3),CL8'ETS'  TESTING ON ADV                   
* FOR STRATA PILOT                                                              
         DC    C'WKBWT',C'NY',AL1(3),CL8'KCS'  PILOT ON ADV                     
         DC    C'WPNTT',C'NY',AL1(3),CL8'MSS'  PILOT ON ADV                     
         DC    C'WPGHT',C'NY',AL1(3),CL8'MSS'  PILOT ON ADV                     
         DC    C'WPMYT',C'NY',AL1(3),CL8'MSS'  CALL SWITCH TO WPNT              
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
STRASTAD DSECT                                                                  
STRASTA  DS    CL5                 STATION                                      
STRAOFF  DS    CL2                 OFFICE                                       
STRAUPLN DS    XL1                 USER-ID PREFIX LENGTH                        
STRAUIDP DS    CL8                 USER-ID PREFIX                               
STRASTAL EQU   *-STRASTAD                                                       
*                                                                               
DARERCV  CSECT                                                                  
         EJECT                                                                  
*&&                                                                             
CHKMOTAB NMOD1 0,CHKMOTAB                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
***********************************************************************         
* FOR NBC NATIONAL OR LOCAL ORDERS:                                             
* SKIP ALL CHECKS FOR NBC XML ORDERS AND USE DEFAULT ROUTING                    
* CHECK THE LIST OF NON-DDS AGENCIES CURRENTLY SENDING TO MO                    
* THIS TABLE WILL ROUTE ORDERS TO WIDE ORBIT BASED ON ROUTING CODE,             
* CUTOVER DARE ORDER NUMBER, AND RECEVING ID                                    
***********************************************************************         
*                                                                               
         LA    R2,GOWOTAB                                                       
CKNBC10  CLC   ROUTCODE,0(R2)      MATCH ON ROUTING CODE?                       
         BE    CKNBC30                                                          
CKNBC20  LA    R2,L'GOWOTAB(R2)                                                 
         CLI   0(R2),X'FF'                                                      
         BE    CKNBC90             NO MATCH                                     
         B     CKNBC10                                                          
*                                                                               
* CHECK IF NATIONAL ORDER GOING TO NBC??                                        
*                                                                               
CKNBC30  DS    0H                                                               
         CLC   =C'MON',RECEIVER    ALL OFFICES, SO CHECK ONLY PREFIX            
         BE    CKNBC35                                                          
         CLC   =C'NBC',RECEIVER    ALL OFFICES, SO CHECK ONLY PREFIX            
         BNE   CKNBC40                                                          
CKNBC35  CLI   AMIXML,C'Y'         XML ORDER ?                                  
         BE    CKMOTBX                                                          
         CLC   ORDNUM,5(R2)        YES, CHECK IF ORDER NUMBER GREATER           
         BNH   CKNBC90             IF NOT, LEAVE IT                             
         MVC   RECEIVER+5(3),=C'-WO'                                            
         B     CKMOTBX             OTHERWISE, SEND TO WIDE ORBIT                
*                                                                               
* CHECK IF LOCAL ORDER GOING TO NBC LOCAL HOME MARKET                           
*                                                                               
CKNBC40  DS    0H                                                               
         LA    RF,NBCLCTAB                                                      
CKNBC50  CLC   RECEIVER,0(RF)                                                   
         BE    CKNBC70                                                          
CKNBC60  LA    RF,L'NBCLCTAB(RF)                                                
         CLI   0(RF),X'FF'                                                      
         BE    CKNBC90             NO MATCH                                     
         B     CKNBC50                                                          
*                                                                               
CKNBC70  DS    0H                                                               
         CLI   AMIXML,C'Y'         XML ORDER ?                                  
         BE    CKMOTBX                                                          
         CLC   ORDNUM,5(R2)        YES, CHECK IF ORDER NUMBER GREATER           
         BNH   CKNBC90             IF NOT, KEEP GOING                           
*                                                                               
         LLC   R1,10(RF)                                                        
         LA    RE,RECEIVER                                                      
         AR    RE,R1                                                            
*                                                                               
         MVC   0(3,RE),=C'-WO'                                                  
         B     CKMOTBX             OTHERWISE, SEND TO WIDE ORBIT                
*                                                                               
CKNBC90  DS    0H                                                               
***********************************************************************         
* **WIDEORBIT CODE**                                                            
* EXAMINE THE RECEIVING USERID IN 'RECEIVER'                                    
* APPEND "-WO" IF ON OR AFTER THE DATE                                          
***********************************************************************         
***********************************                                             
* REP - DATE                                                                    
* LOCAL RECEIVING IDS - DATE                                                    
* **NOTE**                                                                      
* IF NOT ON OR AFTER DATE, ORDER STAYS ON MEDIAOCEAN  (DON'T USE -WO)           
***********************************                                             
CKWDOR10 CLI   AMIXML,C'Y'         XML ORDER ?                                  
         BE    CKWDOR90            YES, LEAVE THEM ALONE                        
         L     R1,=A(WORPDTAB)                                                  
         LA    R2,RECEIVER                                                      
CKWDOR20 CLI   0(R1),X'FF'         EOT?                                         
         BE    CKWDOR90            YES                                          
         LLC   R4,4(R1)            L(REP) ENTRY                                 
         BCTR  R4,0                MINUS 1 FOR EX!                              
         EX    R4,*+8              MATCH ON REP/RECEIVER?                       
         BE    CKWDOR30            YES, CHECK DATE                              
         CLC   5(0,R1),0(R2)                                                    
         LLC   R4,4(R1)            NO, BUMP TO NEXT REP                         
         LA    R1,5(R1,R4)                                                      
         B     CKWDOR20                                                         
*                                                                               
***  POINT TO DATE FOR COMPARE                                                  
CKWDOR30 LLC   R4,4(R1)                                                         
         LA    R6,0(R2)                                                         
*                                                                               
         CLC   =C'NBC',0(R2)       IS IT A REP?                                 
         BE    CKWDOR35                                                         
         CLC   =C'MON',0(R2)                                                    
         BNE   *+12                                                             
CKWDOR35 LA    R6,2(R4,R6)         R6=A(AFTER OFFICE CODE IN EDICT)             
         B     CKWDOR39                                                         
*                                                                               
         BCTR  R4,0                TO ACCOUNT FOR SPACE IN ENTRY                
         LA    R6,0(R4,R6)         R6=A(AFTER LOCAL STATION IN EDICT)           
*                                                                               
CKWDOR39 MVC   FULL,0(R1)          SAVE OF CONVERSION DATE                      
*********                                                                       
* IS THE ROUTING CODE A NON-DDS BUYER?                                          
*********                                                                       
         LA    RE,NODDSTAB                                                      
CKWDOR40 CLC   ROUTCODE,0(RE)                                                   
         BE    CKWDOR90            NON-DDS BUYER, SONNY HANDLED THIS            
         LA    RE,L'NODDSTAB(RE)     WITH GOWOTAB ABOVE                         
         CLI   0(RE),X'FF'                                                      
         BE    CKWDOR50                                                         
         B     CKWDOR40                                                         
*                                                                               
* CHECK TODAY AGAINST DATE IN TABLE                                             
* IF CONVERSION DATE IS ON OR BEFORE TODAY, CHECK IF ORDER                      
* EXISTS IN REPPAK                                                              
* NOW WE CHECK THE DATE IF -WO OR LEAVE ALONE                                   
*                                                                               
CKWDOR50 DS    0H                                                               
         CLI   ORDNUM+1,C'4'       NEW ORDER#?                                  
         BNL   CKWDOR55                                                         
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(15,WORK)  TODAY'S DATE IN JULIAN          
         MVC   WORK+4(4),WORK        MAKE A COPY OF TODAY'S DATE                
         NC    WORK(4),=X'FFF0000F'  TO GET CURRENT DECADE                      
         GOTO1 =V(HEXIN),DMCB,ORDNUM,WORK+8,6                                   
         OI    WORK+8+2,X'0F'                                                   
         SRP   WORK+8(3),63,0                                                   
         OC    WORK+1(3),WORK+8       MAKE ORDER # FOR CURRENT DECADE           
*                                                                               
         CLC   WORK(2),WORK+4         IS ORDER'S YEAR > TODAY'S YEAR?           
         BNH   *+10                   NO, THIS DECADE IS GOOD                   
         SP    WORK(4),=P'0010000'    YES, ORDER FROM LAST DECADE               
         SRP   WORK(4),1,0            WORK(3) IS NOW PWOS JUL                   
*                                                                               
         CLC   WORK(3),FULL           CHECK ORDER'S DATE AGAINST DATE           
         BL    CKWDOR90                  IN WORPDTAB                            
CKWDOR55 MVC   0(3,R6),=C'-WO'        FORCE TO A WIDEORBIT ID                   
         B     CKMOTBX                DONE WITH RECEIVER                        
*                                                                               
CKWDOR90 DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* FOR UNIVISION OR LOCAL ORDERS:                                                
* SKIP ALL CHECKS FOR UNI XML ORDERS AND USE DEFAULT ROUTING                    
* CHECK THE LIST OF NON-DDS AGENCIES CURRENTLY SENDING TO MO                    
* THIS TABLE WILL ROUTE ORDERS TO REPPAK BASED ON ROUTING CODE,                 
* CUTOVER DARE ORDER NUMBER, AND RECEVING ID                                    
***********************************************************************         
         LA    R2,GOUNTAB                                                       
CKUNI10  CLC   ROUTCODE,0(R2)      MATCH ON ROUTING CODE INCL OFFICE?           
         BE    CKUNI30                                                          
CKUNI20  LA    R2,L'GOUNTAB(R2)                                                 
         CLI   0(R2),X'FF'                                                      
         BE    CKUNI90             NO MATCH                                     
         B     CKUNI10                                                          
*                                                                               
* CHECK IF NATIONAL ORDER GOING TO UNIVISION                                    
*                                                                               
CKUNI30  DS    0H                                                               
         CLC   =C'ROB',RECEIVER    REP ROB                                      
         BE    CKUNI35                                                          
         CLC   =C'UN',RECEIVER     OR  UNI                                      
         BNE   CKUNI40                                                          
CKUNI35  CLI   AMIXML,C'Y'         XML ORDER ?                                  
         BE    CKMOTBX                                                          
         CLC   ORDNUM,5(R2)        YES, CHECK IF ORDER NUMBER GREATER           
         BNL   CKMOTBX             IT IS, GOING TO REPPAK, WE'RE DONE           
*                                                                               
         LA    RF,RECEIVER+4       TYPICALLY UNIVISION (4 CHAR USERID)          
         CLC   =C'UN',RECEIVER     ALL OFFICES, SO CHECK ONLY PREFIX            
         BE    *+8                                                              
         LA    RF,RECEIVER+5       FOR TEST REP  ROB, 5 CHAR USERID             
*                                                                               
* (SKUI 9/25/13) SKIP ADDING -MO AS ALL UNVISION ORDERS SHOULD BE               
* PROCESSED IN REPPAK NOW                                                       
*                                                                               
*        MVC   0(3,RF),=C'-MO'                                                  
         B     CKMOTBX             WE'RE DONE                                   
*                                                                               
* CHECK IF LOCAL ORDER GOING TO UNI LOCAL HOME MARKET                           
*                                                                               
CKUNI40  DS    0H                                                               
         LA    RF,UNILCTAB                                                      
CKUNI50  CLC   RECEIVER,0(RF)                                                   
         BE    CKUNI70                                                          
CKUNI60  LA    RF,L'UNILCTAB(RF)                                                
         CLI   0(RF),X'FF'                                                      
         BE    CKUNI90             NO MATCH                                     
         B     CKUNI50                                                          
*                                                                               
CKUNI70  DS    0H                                                               
         CLI   AMIXML,C'Y'         XML ORDER ?                                  
         BE    CKMOTBX                                                          
         CLC   ORDNUM,5(R2)        YES, CHECK IF ORDER NUMBER GREATER           
         BNH   CKMOTBX             IF NOT, KEEP GOING TO MEDIAOCEAN             
*                                                                               
         LLC   R1,10(RF)                                                        
         LA    RE,RECEIVER                                                      
         AR    RE,R1                                                            
*                                                                               
         MVC   0(3,RE),=C'-DS'                                                  
         B     CKMOTBX             OTHERWISE, SEND TO REPPAK                    
*                                                                               
CKUNI90  DS    0H                                                               
***********************************************************************         
* **UNIVISION ROLLBACK**                                                        
* EXAMINE THE RECEIVING USERID IN 'RECEIVER'                                    
* APPEND "-MO" BY DEFAULT, BUT NOT ON OR AFTER THE DATE                         
***********************************************************************         
CKUNVN10 CLI   AMIXML,C'Y'         XML ORDER ?                                  
         BE    CKUNVN90            YES, LEAVE THEM ALONE                        
         L     R1,=A(MORODTAB)                                                  
         LA    R2,RECEIVER                                                      
CKUNVN15 CLI   0(R1),X'FF'         EOT?                                         
         BE    CKUNVN90            YES                                          
         LLC   R4,3(R1)            L(REP-OFFICE) ENTRY                          
         BCTR  R4,0                MINUS 1 FOR EX!                              
         EX    R4,*+8              MATCH ON REP-OFFICE?                         
         BE    CKUNVN20            YES, CHECK DATE                              
         CLC   4(0,R1),0(R2)                                                    
         LLC   R4,3(R1)            NO, BUMP TO NEXT REP                         
         LA    R1,4(R1,R4)                                                      
         B     CKUNVN15                                                         
*                                                                               
***  POINT TO DATE FOR COMPARE                                                  
CKUNVN20 LLC   R4,3(R1)                                                         
         LA    R6,0(R2)                                                         
         LA    R6,0(R4,R6)         R6=A(AFTER OFFICE CODE IN EDICT)             
*                                                                               
         MVC   FULL(3),0(R1)       SAVE OF CONVERSION DATE                      
*********                                                                       
* IS THE ROUTING CODE A NON-DDS BUYER?                                          
*********                                                                       
         LA    RE,NODDSTAB                                                      
CKUNVN40 CLC   ROUTCODE,0(RE)                                                   
         BE    CKUNVN90            NON-DDS BUYER, SONNY HANDLED THIS            
         LA    RE,L'NODDSTAB(RE)     WITH GOUNTAB ABOVE                         
         CLI   0(RE),X'FF'                                                      
         BE    CKUNVN50                                                         
         B     CKUNVN40                                                         
*                                                                               
* CHECK TODAY AGAINST DATE IN TABLE                                             
* IF CONVERSION DATE IS ON OR BEFORE TODAY, CHECK IF ORDER                      
* EXISTS IN REPPAK                                                              
* NOW WE CHECK THE DATE IF -WO OR LEAVE ALONE                                   
*                                                                               
CKUNVN50 DS    0H                                                               
         CLI   ORDNUM+1,C'4'         NEW ORDER#?                                
         BNL   CKMOTBX                                                          
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(15,WORK)  TODAY'S DATE IN JULIAN          
         MVC   WORK+4(4),WORK        MAKE A COPY OF TODAY'S DATE                
         NC    WORK(4),=X'FFF0000F'  TO GET CURRENT DECADE                      
         GOTO1 =V(HEXIN),DMCB,ORDNUM,WORK+8,6                                   
         OI    WORK+8+2,X'0F'                                                   
         SRP   WORK+8(3),63,0                                                   
         OC    WORK+1(3),WORK+8       MAKE ORDER # FOR CURRENT DECADE           
*                                                                               
         CLC   WORK(2),WORK+4         IS ORDER'S YEAR > TODAY'S YEAR?           
         BNH   *+10                   NO, THIS DECADE IS GOOD                   
         SP    WORK(4),=P'0010000'    YES, ORDER FROM LAST DECADE               
         SRP   WORK(4),1,0            WORK(3) IS NOW PWOS JUL                   
*                                                                               
         CLC   WORK(3),FULL           CHECK ORDER'S DATE AGAINST DATE           
         BNL   CKMOTBX                   IN WORPDTAB                            
*                                                                               
* (SKUI 9/25/13) SKIP ADDING -MO AS ALL UNVISION ORDERS SHOULD BE               
* PROCESSED IN REPPAK NOW                                                       
*                                                                               
*        MVC   0(3,R6),=C'-MO'        FORCE TO A WIDEORBIT ID                   
         B     CKMOTBX                DONE WITH RECEIVER                        
*                                                                               
CKUNVN90 DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK THE LIST OF NON-DDS AGENCIES CURRENTLY SENDING TO MO                    
* THIS TABLE WILL ROUTE ORDERS BACK TO REPPAK BASED ON ROUTING CODE,            
* REP AND CUTOVER DARE ORDER NUMBER                                             
***********************************************************************         
         LA    RE,GORPKTAB                                                      
CKMOTB02 CLC   ROUTCODE,0(RE)      MATCH ON ROUTING CODE?                       
         BE    CKMOTB04                                                         
CKMOTB03 LA    RE,L'GORPKTAB(RE)                                                
         CLI   0(RE),X'FF'                                                      
         BE    CKMOTB05            NO MATCH                                     
         B     CKMOTB02                                                         
*                                                                               
CKMOTB04 DS    0H                                                               
         ZIC   R1,5(RE)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         BNE   CKMOTB03            MATCH ON REP RECEIVER?                       
         CLC   6(0,RE),RECEIVER                                                 
*                                                                               
         CLC   ORDNUM,9(RE)        YES, CHECK IF ORDER NUMBER GREATER           
         BH    CKMOTBX             IF SO, SEND TO REPPAK                        
         B     CKMOTB10                                                         
*                                                                               
* FOR AM/FB/UV, THE REMAING CHECK IS FOR NON-DDS AGENCIES THAT NEVER            
* SENT AN ORDER TO MO. IF AGENCY IS ON THIS LIST, JUST SEND TO REPPAK           
*                                                                               
* NOTE: NODDSTAB CONTAINS ALL NON-DDS AGENCIES. THE ONES THAT                   
*       CURRENTLY SEND ORDERS TO MO HAVE BEEN PROCESSED ABOVE WITH              
*       THE GORPKTAB TABLE.                                                     
*                                                                               
*                                                                               
CKMOTB05 DS    0H                                                               
         CLC   =C'ETV',RECEIVER    FOR EAGLE                                    
         BE    CKMOTB06                                                         
         CLC   =C'FSS',RECEIVER    FOX                                          
         BE    CKMOTB06                                                         
         CLC   =C'UN',RECEIVER     NOW UNIVISION ROLLBACK TO REPPAK             
         BNE   CKMOTB10                                                         
*                                                                               
CKMOTB06 DS    0H                                                               
         LA    RE,NODDSTAB                                                      
CKMOTB08 CLC   ROUTCODE,0(RE)                                                   
         BE    CKMOTBX                                                          
         LA    RE,L'NODDSTAB(RE)                                                
         CLI   0(RE),X'FF'                                                      
         BNE   CKMOTB08                                                         
*                                                                               
CKMOTB10 DS    0H                                                               
***********************************************************************         
* EXAMINE THE RECEIVING USERID IN 'RECEIVER' AND THE STATION                    
* APPEND "-MO" IF THE AS OF DATE IS NULLS                                       
***********************************************************************         
* WE HAVE OTHER REP-OFFICE THAT ARE GOING TO MEDIAOCEAN BY STATION/DATE         
* **NOTE**                                                                      
* IF WE MATCH ON REP-OFFICE BUT NOT STATION/DATE, ORDER STAYS ON REPPAK         
***********************************                                             
CKMOTB50 DS    0H                                                               
*                                                                               
* IF XML AND EAGLE/FSS/UN, JUST SEND TO REPPAK                                  
*                                                                               
         CLI   AMIXML,C'Y'         XML ORDER ?                                  
         BNE   CKMOTB52                                                         
         CLC   =C'ETV',RECEIVER    ALL EAGLE XML ORDERS GO BACK TO              
         BE    CKMOTBX             REPPAK                                       
         CLC   =C'FSS',RECEIVER    ALL FSS XML ORDERS GO BACK TO                
         BE    CKMOTBX             REPPAK                                       
         CLC   =C'UN',RECEIVER     ALL UN XML ORDERS GO BACK TO                 
         BE    CKMOTBX             REPPAK                                       
*                                                                               
CKMOTB52 DS    0H                                                               
         L     R1,=A(MOOFSTTB)                                                  
         LA    R2,RECEIVER                                                      
CKMOTB55 CLI   0(R1),X'FF'         EOT?                                         
         BE    CKMOTBX             YES                                          
         LLC   R4,2(R1)            L(REP-OFFICE) ENTRY                          
         BCTR  R4,0                MINUS 1 FOR EX!                              
         EX    R4,*+8              MATCH ON REP-OFFICE?                         
         BE    CKMOTB60            YES, CHECK STATION AND DATE                  
         CLC   3(0,R1),0(R2)                                                    
         SR    R4,R4               NO, BUMP TO NEXT REP                         
         ICM   R4,3,0(R1)                                                       
         AR    R1,R4                                                            
         B     CKMOTB55                                                         
*                                                                               
***  POINT TO STATION/DATE FOR COMPARE                                          
CKMOTB60 LLC   R4,2(R1)                                                         
         LA    R6,0(R2)                                                         
         LA    R6,0(R4,R6)         R6=A(AFTER OFFICE CODE IN EDICT)             
         LA    R1,3(R4,R1)         R1=A(OFFICE CODE IN TABLE)                   
*                                                                               
CKMOTB63 CLI   0(R1),0             END OF STATION/DATE LIST?                    
         BE    CKMOTBX             THEN STAYS ON REPPAK                         
*                                                                               
         CLC   STATION(5),0(R1)    MATCHES THIS STATION?                        
         BE    CKMOTB66                                                         
         LA    R1,14(R1)           NO, NEXT STATION/DATE FOR REP-OFFICE         
         B     CKMOTB63                                                         
*                                                                               
CKMOTB66 OC    5(3,R1),5(R1)       WAS THE STATION ALREADY CONVERTED?           
         BNZ   CKMOTB70            NO, LEAVE RECEIVER ALONE                     
*                                                                               
* (SKUI 9/25/13) SKIP ADDING -MO AS ALL UNVISION ORDERS SHOULD BE               
* PROCESSED IN REPPAK NOW                                                       
*                                                                               
*        MVC   0(3,R6),=C'-MO'     YES, GOTO A FORCED MEDIAOCEAN ID             
         B     CKMOTBX                                                          
*                                                                               
CKMOTB68 MVC   FULL(3),5(R1)       SAVE OFF CONVERSION DATE (PWOS JUL)          
*                                                                               
* IF XML, CHECK IF ORDER EXISTS IN REPPAK, IF SO, ROUTE IT TO                   
* REPPAK, ELSE ROUTE IT TO MO                                                   
*                                                                               
CKMOTB70 DS    0H                                                               
         CLI   AMIXML,C'Y'         XML ORDER ?                                  
         BE    CKMOTB76                                                         
*                                                                               
* CHECK IF NON-DDS AGENCY                                                       
*                                                                               
         LA    RE,NODDSTAB                                                      
CKMOTB74 CLC   ROUTCODE,0(RE)                                                   
         BE    CKMOTB76                                                         
         LA    RE,L'NODDSTAB(RE)                                                
         CLI   0(RE),X'FF'                                                      
         BE    CKMOTBX                                                          
         B     CKMOTB74                                                         
*                                                                               
* CHECK TODAY AGAINST DATE IN TABLE                                             
* IF CONVERSION DATE IS ON OR BEFORE TODAY, CHECK IF ORDER                      
* EXISTS IN REPPAK                                                              
*                                                                               
CKMOTB76 DS    0H                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(19,WORK)  GET TODAY IN PWOS JUL           
         CLC   WORK(3),FULL                                                     
         BL    CKMOTBX                                                          
*                                                                               
* CHECK IF ORDER EXISTS IN REPPAK                                               
*                                                                               
CKMOTB77 DS    0H                                                               
         XC    WORK,WORK                                                        
DARKEYD  USING RDARKEY,WORK                                                     
DARSKEYD USING RDARKEY,WORK+32                                                  
*                                                                               
         L     RE,TSKUIDTB         TABLE OF USERIDS/PARTNERS                    
         USING UIDTABD,RE                                                       
CKMOTB78 CLI   0(RE),X'FF'                                                      
         BNE   CKMOTB80                                                         
*                                                                               
* (SKUI 9/25/13) SKIP ADDING -MO AS ALL UNVISION ORDERS SHOULD BE               
* PROCESSED IN REPPAK NOW                                                       
*                                                                               
*        MVC   0(3,R6),=C'-MO'                                                  
         B     CKMOTBX                                                          
CKMOTB80 CLC   RECEIVER,UIDNAME                                                 
         BE    CKMOTB85                                                         
         LA    RE,UIDTBLQ(,RE)     BUMP TO NEXT ENTRY                           
         B     CKMOTB78                                                         
*                                                                               
CKMOTB85 DS    0H                                                               
         CLI   UIDREPFN,X'FF'      NOT REPPAK USER                              
         BNE   CKMOTB90            ROUTE TO MO                                  
*                                                                               
* (SKUI 9/25/13) SKIP ADDING -MO AS ALL UNVISION ORDERS SHOULD BE               
* PROCESSED IN REPPAK NOW                                                       
*                                                                               
*        MVC   0(3,R6),=C'-MO'                                                  
         B     CKMOTBX                                                          
*                                                                               
CKMOTB90 DS    0H                                                               
         MVC   RCVPWRCD,UIDREPCD                                                
         MVC   RCVSENUM,UIDREPFN                                                
         DROP  RE                                                               
*                                                                               
         PRNT  ReadREPFile                                                      
*                                                                               
         L     RF,=A(UTL)                                                       
         MVC   SVUTL,4(RF)         SAVE OFF ORIG UTL+4                          
         MVC   4(1,RF),RCVSENUM    SWITCH TO CORRECT REP FILE                   
***      MVC   SVUTL,UTL+4         SAVE OFF ORIG UTL+4                          
***      MVC   UTL+4(1),RCVSENUM   SWITCH TO CORRECT REP FILE                   
*                                                                               
         MVI   DARKEYD.RDARKTYP,X'41'                                           
         MVC   DARKEYD.RDARKREP,RCVPWRCD                                        
         MVC   DARKEYD.RDARKSTA,STATION                                         
         MVI   DARKEYD.RDARKSTA+5,C' '                                          
         MVC   DARKEYD.RDARKAGY(5),ROUTCODE                                     
         GOTO1 =V(HEXIN),DMCB,ORDNUM,DARKEYD.RDARKORD,8,0                       
*                                                                               
         MVC   DARSKEYD.RDARKEY(27),DARKEYD.RDARKEY                             
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',DARSKEYD.RDARKEY,    +        
               DARKEYD.RDARKEY                                                  
*                                                                               
         CLC   DARKEYD.RDARKEY(24),DARSKEYD.RDARKEY                             
         BE    CKMOTB95                                                         
*                                                                               
* CHECK IF XML ORDER WAS CONFIRMED                                              
*                                                                               
         MVI   DARSKEYD.RDARKEY,X'51'                                           
         MVC   DARKEYD.RDARKEY,DARSKEYD.RDARKEY                                 
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',DARSKEYD.RDARKEY,    +        
               DARKEYD.RDARKEY                                                  
*                                                                               
         CLC   DARKEYD.RDARKEY(24),DARSKEYD.RDARKEY                             
         BE    CKMOTB95                                                         
*                                                                               
*                                                                               
* (SKUI 9/25/13) SKIP ADDING -MO AS ALL UNVISION ORDERS SHOULD BE               
* PROCESSED IN REPPAK NOW                                                       
*                                                                               
*        MVC   0(3,R6),=C'-MO'     NO, ROUTE XML TO MO                          
*                                                                               
CKMOTB95 DS    0H                                                               
         L     RF,=A(UTL)                                                       
         MVC   4(1,RF),SVUTL       RESTORE SAVED UTL+4                          
***      MVC   UTL+4(1),SVUTL      RESTORE SAVED UTL+4                          
*                                                                               
CKMOTBX  DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
       ++INCLUDE DDNODDSTAB                                                     
       ++INCLUDE DDGORPKTAB                                                     
       ++INCLUDE DDGOWOTAB                                                      
       ++INCLUDE DDGOUNTAB                                                      
         EJECT                                                                  
CHKWOTAB NMOD1 0,CHKWOTAB                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
***********************************************************************         
* FOR ETV NATIONAL ORDERS:                                                      
* THIS TABLE WILL ROUTE ORDERS TO WIDE ORBIT BASED ON STATION (HEARST)          
* AND IF ORDER WAS NEVER SENT TO REPPAK                                         
* OR IF SENT TO REPPAK AND ORDER WAS NEVER LINKED TO A CONTRACT                 
*                                                                               
*                                                                               
* 3/9/11 - WBAL/EBAL ROUTING BACK TO REPPAK                                     
*                                                                               
***********************************************************************         
*                                                                               
* CHECK IF NATIONAL ORDER GOING TO ETV??                                        
*                                                                               
*        CLC   =C'ETV',RECEIVER    ALL OFFICES, SO CHECK ONLY PREFIX            
*        BE    CHKWO10                                                          
*                                                                               
         CLI   AMIXML,C'Y'         SKIP IF XML ORDER                            
         BE    CHKWOX                                                           
*                                                                               
         CLC   =C'ETVNY',RECEIVER  START WITH NY OFFICE                         
         BE    CHKWO10                                                          
         CLC   =C'MOQNY',RECEIVER    TEST REP                                   
         BNE   CHKWOX                                                           
*                                                                               
* CHECK IF ORDER EXISTS IN REPPAK                                               
*                                                                               
CHKWO10  DS    0H                                                               
         CLC   =C'-WO',RECEIVER+5  SKIP IF ALREADY GOING TO WO                  
         BE    CHKWOX                                                           
*                                                                               
         L     R1,=A(WOHSTTAB)     CHECK IF HEARST STATION                      
CHKWO13  CLI   0(R1),X'FF'         EOT?                                         
         BE    CHKWOX              YES                                          
         CLC   0(5,R1),STATION                                                  
         BE    CHKWO14                                                          
         LA    R1,5(R1)                                                         
         B     CHKWO13                                                          
*                                                                               
* FOR NON-DDS, ONLY MEDIAVEST (DMVNY) HAS SENT ORDERS TO WO                     
* USE LAST ORDER SENT TO WO AS THE CUTOVER MARK                                 
*                                                                               
* FOR TESTING:                                                                  
*                                                                               
CHKWO14  DS    0H                                                               
         CLC   =C'SJ NY',ROUTCODE                                               
         BNE   CHKWO15                                                          
         CLC   ORDNUM,=C'10680000'                                              
         BH    CHKWOX                                                           
         B     CHKWO18                                                          
*                                                                               
* IF NOT MEDIAVEST, ALL NON-DDS ORDERS SHOULD GO TO REPPAK SINCE NO             
* OTHER NON-DDS AGENCIES HAD SENT TO WO                                         
*                                                                               
CHKWO15  DS    0H                                                               
         CLC   =C'DMVNY',ROUTCODE                                               
         BNE   CHKWOX                                                           
         CLC   ORDNUM,=C'00253703'                                              
         BH    CHKWOX                                                           
*                                                                               
CHKWO18  DS    0H                                                               
         XC    WORK,WORK                                                        
DARKEYD  USING RDARKEY,WORK                                                     
DARSKEYD USING RDARKEY,WORK+32                                                  
*                                                                               
         L     RE,TSKUIDTB         TABLE OF USERIDS/PARTNERS                    
         USING UIDTABD,RE                                                       
CHKWO20  CLI   0(RE),X'FF'                                                      
         BE    CHKWOX                                                           
*                                                                               
* USER ID NOT IN REPPAK, EXIT                                                   
*                                                                               
CHKWO30  CLC   RECEIVER,UIDNAME                                                 
         BE    CHKWO40                                                          
         LA    RE,UIDTBLQ(,RE)     BUMP TO NEXT ENTRY                           
         B     CHKWO20                                                          
*                                                                               
CHKWO40  DS    0H                                                               
         CLI   UIDREPFN,X'FF'      NOT REPPAK USER                              
         BE    CHKWOX              *** SHOULD NEVER HAPPEN                      
*                                                                               
CHKWO50  DS    0H                                                               
         MVC   RCVPWRCD,UIDREPCD                                                
         MVC   RCVSENUM,UIDREPFN                                                
         DROP  RE                                                               
*                                                                               
         PRNT  ReadREPFile                                                      
*                                                                               
         L     RF,=A(UTL)                                                       
         MVC   SVUTL,4(RF)         SAVE OFF ORIG UTL+4                          
         MVC   4(1,RF),RCVSENUM    SWITCH TO CORRECT REP FILE                   
***      MVC   SVUTL,UTL+4         SAVE OFF ORIG UTL+4                          
***      MVC   UTL+4(1),RCVSENUM   SWITCH TO CORRECT REP FILE                   
*                                                                               
         MVI   DARKEYD.RDARKTYP,X'41'                                           
         MVC   DARKEYD.RDARKREP,RCVPWRCD                                        
         MVC   DARKEYD.RDARKSTA,STATION                                         
         MVI   DARKEYD.RDARKSTA+5,C' '                                          
         MVC   DARKEYD.RDARKAGY(5),ROUTCODE                                     
         GOTO1 =V(HEXIN),DMCB,ORDNUM,DARKEYD.RDARKORD,8,0                       
*                                                                               
         MVC   DARSKEYD.RDARKEY(27),DARKEYD.RDARKEY                             
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',DARSKEYD.RDARKEY,    +        
               DARKEYD.RDARKEY                                                  
*                                                                               
         CLC   DARKEYD.RDARKEY(24),DARSKEYD.RDARKEY                             
         BNE   CHKWO55                                                          
*                                                                               
* FOUND IN REPPAK, BUT WAS THE CONTRACT NUMBER EVER SENT TO AGENCY?             
*                                                                               
         CLC   CONTRACT,=8C'0'                                                  
         BNH   CHKWO58             NOT LINKED, GO TO WO                         
         B     CHKWO60             LINKED, STAY IN REPPAK                       
*                                                                               
* CHECK IF ORDER WAS CONFIRMED                                                  
*                                                                               
CHKWO55  DS    0H                                                               
         MVI   DARSKEYD.RDARKEY,X'51'                                           
         MVC   DARKEYD.RDARKEY,DARSKEYD.RDARKEY                                 
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',DARSKEYD.RDARKEY,    +        
               DARKEYD.RDARKEY                                                  
*                                                                               
         CLC   DARKEYD.RDARKEY(24),DARSKEYD.RDARKEY                             
         BE    CHKWO60                                                          
*                                                                               
CHKWO58  DS    0H                  NO, ROUTE TO WO                              
         MVC   RECEIVER+5(3),=C'-WO'                                            
*                                                                               
CHKWO60  DS    0H                                                               
         L     RF,=A(UTL)                                                       
         MVC   4(1,RF),SVUTL       RESTORE SAVED UTL+4                          
***      MVC   UTL+4(1),SVUTL      RESTORE SAVED UTL+4                          
         B     CHKWOX                                                           
*                                                                               
CHKWO100 DS    0H                                                               
*                                                                               
CHKWOX   DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
       ++INCLUDE DDWOHSTTAB                                                     
         LTORG                                                                  
         EJECT                                                                  
CHKSNDR  NMOD1 0,CHKSNDR                                                        
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* EXAMINE THE SENDING USERID IN 'SENDER', ATTACH "-DS" TO THE END OF            
* THE SENDERUSERIDFOR SOME SPECIAL CASES.  ONLY FOR "EDR" TASKID.               
*                                                                               
         CLC   =C'EDR',TSKNAME                                                  
         BNE   CHKSND40                                                         
         CLC   =C'MON',SENDER      SENDER=MON??                                 
         BE    CHKSND20                                                         
         CLC   =C'NBC',SENDER      SENDER=NBC??                                 
         BNE   CHKSNDX                                                          
CHKSND20 MVC   SENDER+5(3),=C'-DS'                                              
         B     CHKSNDX                                                          
*                                                                               
* EXAMINE THE SENDING USERID IN 'SENDER', ATTACH "-MO" TO THE END OF            
* THE SENDERUSERIDFOR SOME SPECIAL CASES.  ONLY FOR "MQ1" TASKID.               
*                                                                               
CHKSND40 CLC   =C'MQ1',TSKNAME                                                  
         BNE   CHKSNDX                                                          
         CLC   =C'ROBCH',SENDER                                                 
         BNE   CHKSNDX                                                          
         MVC   SENDER+5(3),=C'-MO'                                              
*                                                                               
CHKSNDX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* ENSURES THAT A CALL LETTER CHANGE BECOMES EFFECTIVE                           
*                                                                               
* MEDIA USING UNIQUE ID'S WON'T HAVE A CALL LETTER CHANGE THIS WAY              
*                                                                               
* ADD SUPPORT FOR FIRST EDI SEND STATION AND DATE, TO THE AGYHDR,               
* AGYRCL, & AGYCAN MESSAGES HEADER                     -HWON 04/13/2017         
***********************************************************************         
FINDSTN  NMOD1 0,FINDSTN                                                        
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         LR    R6,R3               THIS ADDRESS IS IN HIGH CORE!!!              
*                                                                               
         MVC   OSTATION,SPACES     CLEAR OUT OLD STATION CALL LETTERS           
         LA    R3,=C'STATTABL'                                                  
         MVC   P+30(8),DARE                                                     
         MVC   P+40(8),0(R3)                                                    
         PRNT  Enqueue                                                          
         ENQ   (DARE,(3),E,8)      ENQUEUE THE STATION TABLE                    
*                                                                               
         USING PAGYHDRD,R6                                                      
FSAGYHDR CLC   PAHDTID,=C'AGYHDR'  AGYHDR MESSAGE?                              
         BNE   FSAGYRCL             NO, CHECK IF AGYRCL OR AGYCAN               
         CLC   PAHDFSNT,SPACES     ANY FIRST EDI SEND DATE?                     
         BNH   FS20                 NO, GO USE TODAY'S DATE                     
         MVC   STATION,PAHDFSTA     YES, USE THIS 1ST EDI SEND STATION          
         MVC   WORK,PAHDFSNT        AND USE THIS DATE FOR DSTA LKUP             
         B     FS10                                                             
         DROP  R6                                                               
*                                                                               
         USING PAGYRCLD,R6                                                      
FSAGYRCL CLC   PARCTID,=C'AGYRCL'  AGYRCL MESSAGE?                              
         BNE   FSAGYCAN             NO, CHECK IF AGYCAN                         
         CLC   PARCFSNT,SPACES     ANY FIRST EDI SEND DATE?                     
         BNH   FS20                 NO, GO USE TODAY'S DATE                     
         MVC   STATION,PARCFSTA     YES, USE THIS 1ST EDI SEND STATION          
         MVC   WORK,PARCFSNT        AND USE THIS DATE FOR DSTA LKUP             
         B     FS10                                                             
         DROP  R6                                                               
*                                                                               
         USING PAGYCAND,R6                                                      
FSAGYCAN CLC   PACNTID,=C'AGYCAN'  AGYCAN MESSAGE?                              
         BNE   FS20                 NO, GO USE TODAY'S DATE                     
         CLC   PACNFSNT,SPACES     ANY FIRST EDI SEND DATE?                     
         BNH   FS20                 NO, GO USE TODAY'S DATE                     
         MVC   STATION,PACNFSTA     YES, USE THIS 1ST EDI SEND STATION          
         MVC   WORK,PACNFSNT        AND USE THIS DATE FOR DSTA LKUP             
         DROP  R6                                                               
*                                                                               
* IF WE GET HERE WE ARE USING 1ST EDI SEND DATE FOR LOOKUP                      
*      ***BE CAREFUL*** R6 IS 31BIT, PARAMTYPES WIPES HOB, MOVE TO WORK         
FS10     GOTO1 =V(DATCON),DMCB,(0,WORK),(3,DUB)                                 
         B     FS30               TODAYS DATE AND SEE IF DSTA EFFECTIVE         
*                                                                               
* IF WE GET HERE, USING TODAY'S DATE FOR LOOKUP                                 
FS20     GOTO1 =V(DATCON),DMCB,(5,0),(3,DUB)                                    
FS30     SR    RE,RE               EFFECTIVE DATE WAS NEGATED NOT FF'D          
         ICM   RE,7,DUB                                                         
         LNR   RE,RE                                                            
         STCM  RE,7,DUB                                                         
*                                                                               
* LAST BYTE OF THE 2 BYTE BAND IS NOT REQUIRED                                  
         MVI   STATION+5,C' '      TO MATCH STATTABL FROM DDDARE                
*                                                                               
         XC    MYKEY,MYKEY                                                      
MK       USING STATTABD,MYKEY                                                   
         MVC   MK.STATMED,MEDIA                                                 
         MVC   MK.STATSTN,STATION                                               
         MVC   MK.STATEFDT,DUB                                                  
         DROP  MK                                                               
*                                                                               
         L     R2,TSKSTATB         TABLE OF STATIONS/REPS                       
         L     R0,4(R2)            RECORD COUNT                                 
         XC    DMCB,DMCB                                                        
         L     RF,=V(BINSRCH)                                                   
         GOTO1 (RF),DMCB,(X'02',MYKEY),8(R2),(R0),STATTBLQ,(0,10),(R0)          
*                                                                               
         L     R2,0(R1)                                                         
         USING STATTABD,R2                                                      
         CLC   MEDIA,STATMED       SAME MEDIA?                                  
         BNE   FSNOSTN                                                          
         CLC   STATION,STATSTN     SAME STATION?                                
         BNE   FSNOSTN                                                          
*                                                                               
         CLC   STATEFDT,DUB        DSTA RECORD APPLICABLE?                      
         BNL   FS40                 YES, ON OR OLDER THAN 1ST EDI SEND          
         AHI   R2,STATTBLQ          NO, CHECK NEXT STATION                      
         CLC   STATION,STATSTN     SAME STATION STILL?                          
         BNE   FSNOSTN              NO, NOT GOOD                                
*                                                                               
FS40     CLC   STATNSTN,=5X'FF'    DO WE HAVE A NEW STATION CALLS?              
         BE    FSX                  NO                                          
         CLC   STATNSTD,=5X'FF'    IS IT OLD? OR NEW STATION?                   
         BE    FS50                 YES, FFS MEANS IT'S OLD                     
*                                   CHECK IF NEW STA IS IN EFFECT               
         GOTO1 =V(DATCON),DMCB,(5,0),(15,FULL)  TODAY: 0CYYDDDF                 
         CLC   STATNSTD,FULL       HAVE WE REACHED EFFECTIVE DATE?              
         BH    FSX                  NO                                          
*                                   YES, UPDATE STATIONS                        
         GOTO1 =V(DATCON),DMCB,(6,STATNSTD),(3,DUB)                             
         MVC   STATION,STATNSTN    WE'RE USING THIS STATION AND EFF NOW         
         MVI   STATION+5,C' '                                                   
         MVC   OSTATION,STATSTN    STATION WE WERE LOOKING FOR IS OLD           
         B     FS30                                                             
*                                                                               
FS50     MVC   OSTATION,STATNSTN   NEW CALLS WITH FFFF EFF DATE IS OLD          
         B     FSX                                                              
         DROP  R2                                                               
*                                                                               
FSNOSTN  DS    0H                                                               
         MVC   STATION,SPACES      STATION NOT FOUND                            
*                                                                               
FSX      LA    R3,=C'STATTABL'                                                  
         MVC   P+30(8),DARE                                                     
         MVC   P+40(8),0(R3)                                                    
         PRNT  Dequeue                                                          
         DEQ   (DARE,(3),8)        DEQUEUE THE STATION TABLE                    
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* GIVEN THE STATION CALL LETTERS IN FIELD 'STATION', DEDUCE THE MEDIA,          
* AND RETURN IT IN THE FIELD 'MEDIA'.                                           
**********************************************************************          
FINDMED  NMOD1 0,FINDMED                                                        
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         CLI   STATION+4,C'T'      T MEANS TV                                   
         BE    FNDMEDTV                                                         
*                                                                               
         CLI   STATION+4,C'L'      L MEANS TV (LOW-POWER)                       
         BE    FNDMEDTV                                                         
*                                                                               
         CLI   STATION+4,C'D'      DIGITAL VIDEO                                
         BE    FNDMEDTV                                                         
*                                                                               
         CLI   STATION+4,C' '      SO DOES A BLANK                              
         BE    FNDMEDTV                                                         
*                                                                               
         CLI   STATION+4,C'A'      AM RADIO                                     
         BE    FNDMEDRD                                                         
*                                                                               
         CLI   STATION+4,C'F'      FM RADIO                                     
         BE    FNDMEDRD                                                         
*                                                                               
         CLI   STATION+4,C'S'      STREAMING MEDIA                              
         BE    FNDMEDRD                                                         
*********                                                                       
* LET'S SEE IF THE STATION IS UNIQUE ID                                         
*********                                                                       
         CLI   STATION+5,C' '                                                   
         BE    FNDMEDNO                                                         
         CLC   STATION+4(2),=C'00'                                              
         BL    FNDMEDNO                                                         
*                                                                               
* DEDUCE THE MEDIA FROM THE RETURN TO SENDER AGY/MEDIA COMBO                    
*                                                                               
         LA    R1,RETURN            LOOK IN RETURN TO SENDER INFO               
         USING RTN2SNDR,R1                                                      
         CLI   RTNAGYMD+1,C'1'      AGY'S BAGYMED IS ?1  (TV)                   
         BE    FNDMEDTV                                                         
         CLI   RTNAGYMD+1,C'2'      AGY'S BAGYMED IS ?2  (RADIO)                
         BNE   FNDMEDNO                                                         
FNDMEDRD MVI   MEDIA,C'R'           YES,                                        
         B     FINDMEDX                                                         
         DROP  R1                                                               
*****                                                                           
FNDMEDTV MVI   MEDIA,C'T'                                                       
         B     FINDMEDX                                                         
*                                                                               
FNDMEDNO MVI   MEDIA,C' '          UNKNOWN STATION                              
*                                                                               
FINDMEDX XIT1                                                                   
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
         SAM31                     SWITCH TO 31-BIT MODE                        
         L     RF,0(R2)            RF = A(MQ ROUTINE)                           
         LA    R3,32(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
         SAM24                     SWITCH BACK TO 24-BIT MODE                   
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
         JNE   *+2                                                              
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
         PRNT                                                                   
         XIT1                                                                   
         LTORG                                                                  
                                                                                
***********************************************************************         
* FIND THE PARTNER FOR THE RECEIVER OF THE MESSAGE.                             
***********************************************************************         
FINDTSK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    SNDTSKID,SNDTSKID                                                
*                                                                               
         LA    R3,=C'UIDTABL'                                                   
         MVC   P+30(8),DARE                                                     
         MVC   P+40(7),0(R3)                                                    
         PRNT  Enqueue                                                          
*                                                                               
         SAM24                                                                  
         ENQ   (DARE,(3),E,7)      ENQUEUE THE USERID TABLE                     
*                                                                               
         L     RE,TSKUIDTB         TABLE OF USERIDS/PARTNERS                    
         USING UIDTABD,RE                                                       
FT10     CLI   0(RE),X'FF'                                                      
         BE    FTX                                                              
         CLC   RECEIVER,UIDNAME                                                 
         BE    *+12                                                             
         LA    RE,UIDTBLQ(,RE)     BUMP TO NEXT ENTRY                           
         B     FT10                                                             
*                                                                               
         MVC   RCVIDNUM,UIDNUM     SAVE RECEIVER'S NUMERIC USERID               
*                                                                               
         L     RF,ATSKTAB          TABLE OF DARE SUBTASKS                       
FT20     CLI   0(RF),X'FF'                                                      
*****    BNE   *+6                                                              
*****    DC    H'0'                MISSING TABLE ENTRY                          
         BE    FTX                 MISSING TASK ENTRY SO NO SNDTSKID            
         CLC   UIDPART,TSKSNAME-TSKTABD(RF)                                     
         BE    *+12                GOT MATCH ON SHORT SUBTASK NAME              
         LA    RF,TSKTABLQ(,RF)    BUMP TO NEXT ENTRY                           
         B     FT20                                                             
         DROP  RE                                                               
*                                                                               
         MVC   SNDTSKID,TSKNAME-TSKTABD(RF)  RETURN SUBTASK ID                  
*                                                                               
FTX      MVC   P+30(8),DARE                                                     
         MVC   P+40(7),0(R3)                                                    
         PRNT  Dequeue                                                          
         DEQ   (DARE,(3),7)        DEQUEUE THE USERID TABLE                     
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
FINDTSKS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* FIND THE PARTNER FOR THE SENDER OF THE MESSAGE.                               
*                                                                               
         XC    SNDTSKID,SNDTSKID                                                
*                                                                               
         LA    R3,=C'UIDTABL'                                                   
         MVC   P+30(8),DARE                                                     
         MVC   P+40(7),0(R3)                                                    
         PRNT  Enqueue                                                          
*                                                                               
         SAM24                                                                  
         ENQ   (DARE,(3),E,7)      ENQUEUE THE USERID TABLE                     
*                                                                               
         L     RE,TSKUIDTB         TABLE OF USERIDS/PARTNERS                    
         USING UIDTABD,RE                                                       
FTS10    CLI   0(RE),X'FF'                                                      
         BE    FTSX                                                             
         CLC   SENDER,UIDNAME                                                   
         BE    *+12                                                             
         LA    RE,UIDTBLQ(,RE)     BUMP TO NEXT ENTRY                           
         B     FTS10                                                            
*                                                                               
         MVC   USERID,UIDNUM       SENDING USERID#                              
*                                                                               
         L     RF,ATSKTAB          TABLE OF DARE SUBTASKS                       
FTS20    CLI   0(RF),X'FF'                                                      
*****    BNE   *+6                                                              
*****    DC    H'0'                MISSING TABLE ENTRY                          
         BE    FTSX                MISSING TASK ENTRY SO NO SNDTSKID            
         CLC   UIDPART,TSKSNAME-TSKTABD(RF)                                     
         BE    *+12                GOT MATCH ON SHORT SUBTASK NAME              
         LA    RF,TSKTABLQ(,RF)    BUMP TO NEXT ENTRY                           
         B     FTS20                                                            
         DROP  RE                                                               
*                                                                               
         MVC   SNDTSKID,TSKNAME-TSKTABD(RF)  RETURN SUBTASK ID                  
*                                                                               
FTSX     MVC   P+30(8),DARE                                                     
         MVC   P+40(7),0(R3)                                                    
         PRNT  Dequeue                                                          
         DEQ   (DARE,(3),7)        DEQUEUE THE USERID TABLE                     
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
CHKSERR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* THIS ROUTINE CHECKS FOR SPECIAL ERROR WITH THE RECEIVED MESSAGES.             
*                                                                               
         CLI   NULLREC,C'Y'        NULL RECORD RECEIVED?                        
         BNE   *+14                                                             
         MVC   ERRCODE,=C'010'                                                  
         B     CXERXBAD            NULL RECORD                                  
*                                                                               
         CLI   VERFLAG,C'N'        MISSING VERSION FLAG?                        
         BNE   *+14                                                             
         MVC   ERRCODE,=C'011'     MISSING VERSION FLAG ERROR                   
         B     CXERXBAD                                                         
*                                                                               
         CLI   TOOLONG,C'Y'        WAS A 'TOO LONG' RECORD RECEIVED?            
         BNE   *+14                                                             
         MVC   ERRCODE,=C'012'                                                  
         B     CXERXBAD            RECORD TOO LONG                              
*                                                                               
CSERXOK  CR    RB,RB               SET CC EQUAL                                 
         B     CSERX                                                            
CXERXBAD LTR   RB,RB               SET CC NOT EQUAL                             
CSERX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
MVTODARF NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,DATALENGTH       MSG MUST NOT EXCEED DAREBUFL                 
         C     RE,=AL4(DAREBUFL-1)                                              
         JH    *+2                 YES -- MUST INCREASE DAREBUFL                
*                                                                               
         MVI   VERFLAG,C'Y'        SET VERSION FLAG IS OKAY                     
         LA    R4,1                RESET RECORD COUNTER                         
         GOTO1 =A(DATETIME),(RC)   PUT CURRENT TIME INTO MESSAGE                
*                                                                               
         SAM31                                                                  
         L     R6,AMQBUFXA                                                      
         L     R3,ADAREBUF                                                      
*                                                                               
*MESSAGE  COME IN AS    '+++DARE SEND VERSION=2  ' CRLF                         
*                 OR    '+++DARE SEND VERSION=2' CRLF                           
*         REPLACE IT AS '+++DARE SEND TIME=HHMMSS' CRLF                         
*                                                                               
         MVC   0(APPCM3Q,R3),APPCM3   REPLACE +++DARE SEND RECORD               
         MVC   18(6,R3),HHMMSS                                                  
*                                                                               
         AHI   R3,APPCM3Q                                                       
         AHI   R6,APPCM3Q-2        ASSUME "VERSION=2"                           
         CLC   0(L'CRLF,R6),CRLF                                                
         BE    MTDF30              YES - NEXT LINE                              
         AHI   R6,2                "VERSION=2  " WITH 2 SPACES                  
*                                                                               
         LA    R0,256-APPCM3Q      SCAN FORWARD 256 BYTES MAX                   
MTDF20   CLC   0(L'CRLF,R6),CRLF                                                
         BE    MTDF30                                                           
         MVC   0(1,R3),0(R6)                                                    
         AHI   R3,1                                                             
         AHI   R6,1                                                             
         BCT   R0,MTDF20                                                        
         MVI   TOOLONG,C'Y'        RECORD IS TOO LONG                           
         MVC   P(80),0(R6)         SAVE THIS FOR PRINT LATER                    
*                                                                               
MTDF30   EQU   *                                                                
         MVC   0(L'CRLF,R3),0(R6)                                               
         AHI   R3,L'CRLF                                                        
         AHI   R6,L'CRLF                                                        
         AHI   R4,1                +1 FOR RECORD COUNTER                        
         CLC   =C'+++DARE END',0(R6)  IS THIS THE LAST LINE?                    
         BE    MTDF70                                                           
*                                  GET LENGTH OF THIS DARE RECORD               
         LR    RE,R6                                                            
         LA    R0,256                 (SCAN FORWARD 256 BYTES MAX)              
MTDF35   CLC   0(L'CRLF,RE),CRLF                                                
         BE    *+12                                                             
         AHI   RE,1                                                             
         BCT   R0,MTDF35                                                        
         SR    RE,R6               RE=LENGTH                                    
*                                                                               
         LAY   R2,DRALLRCS         TABLE OF KNOWN DARE RECORDS IN SPEC          
MTDF40   CLI   0(R2),X'FF'                                                      
         BE    MTDF50              RECORD NOT IN TABLE -- THAT'S OK             
         CLC   0(6,R2),0(R6)                                                    
         BE    *+12                                                             
         LA    R2,8(R2)            BUMP TO NEXT ENTRY                           
         B     MTDF40                                                           
*                                                                               
         LH    R2,6(R2)            R2 = LENGTH OF RECORD IN SPEC                
         CR    RE,R2               RCV'D REC > SPEC                             
         BNH   MTDF45                                                           
         MVI   TOOLONG,C'Y'        RECORD IS TOO LONG                           
         MVC   P(80),0(R6)         SAVE THIS FOR PRINT LATER                    
*                                                                               
MTDF45   LR    R1,R2               MAX LENGTH OF RECORD                         
         MVI   0(R3),C' '                                                       
         BCTR  R1,0                FOR MVC TRICK                                
         BCTR  R1,0                FOR EX                                       
         EX    R1,MTDEXPFL         PRE-FILL BUFFER WITH SPACES                  
*                                                                               
         LR    R1,RE               RCV'D LENGTH OF RECORD                       
         BCTR  R1,0                                                             
         EX    R1,MTDEXMVC         PLACE DATA IN BUFFER                         
*                                                                               
         AR    R3,R2               BUMP TO EOR (+ MAX REC LEN)                  
         AR    R6,RE               BUMP TO EOR (+ RCV'D REC LEN)                
         B     MTDF30                                                           
*                                                                               
MTDF50   LR    R1,RE               RCV'D LENGTH OF UNKNOWN RECORD               
         BCTR  R1,0                                                             
         EX    R1,MTDEXMVC         PLACE DATA IN BUFFER                         
*                                                                               
         AR    R3,RE               BUMP TO EOR (+ RCV'D REC LEN)                
         AR    R6,RE               BUMP TO EOR (+ RCV'D REC LEN)                
         B     MTDF30                                                           
*                                                                               
MTDEXPFL MVC   1(0,R3),0(R3)       PRE-FILL BUFFER WITH SPACES                  
MTDEXMVC MVC   0(0,R3),0(R6)       PLACE DATA IN BUFFER                         
*                                                                               
MTDF70   EQU   *                                                                
*MESSAGE  COME IN AS    '+++DARE END                         ' CRLF             
*                 OR    '+++DARE END' CRLF                                      
*         REPLACE IT AS '+++DARE END TIME=HHMMSS COUNT=NNNNNN' CRLF             
*                                                                               
         MVC   0(APPCM4Q,R3),APPCM4   REPLACE +++DARE END RECORD                
         MVC   17(6,R3),HHMMSS                                                  
         CVD   R4,DUB              RECORD COUNTER                               
         UNPK  30(6,R3),DUB                                                     
         OI    35(R3),X'F0'        TOTAL NUMBER OF RECORDS                      
*                                                                               
         AHI   R3,APPCM4Q                                                       
         AHI   R6,11               PASS C'+++DARE END'                          
*                                                                               
         CLC   0(L'CRLF,R6),CRLF   SHORT C'+++DARE END', NO TIME/CNT            
         BE    MTDF80              YES                                          
         AHI   R6,APPCM4Q-11       NO - EXPECT FULL LENGTH APPCM4Q              
*                                                                               
         LA    R0,256-APPCM4Q      SCAN FORWARD 256 BYTES MAX                   
MTDF75   CLC   0(L'CRLF,R6),CRLF                                                
         BE    MTDF80                                                           
         MVC   0(1,R3),0(R6)                                                    
         AHI   R3,1                                                             
         AHI   R6,1                                                             
         BCT   R0,MTDF75                                                        
         MVI   TOOLGEND,C'Y'       END RECORD IS TOO LONG (IGNORED)             
         MVC   P(20),=CL20'END RECORD TOO LONG'      FOR PRINT LATER            
*                                                                               
MTDF80   EQU   *                                                                
         MVC   0(L'CRLF,R3),0(R6)                                               
         AHI   R3,L'CRLF                                                        
         AHI   R6,L'CRLF                                                        
         MVI   0(R3),X'FF'         END OF MESSAGE MARKER                        
*                                                                               
MTDF90   EQU   *                                                                
         SAM24                                                                  
         CLI   TOOLONG,C'Y'        RECORD IS TOO LONG?                          
         BNE   MTDF95              NO                                           
         GOTO1 =V(PRINTER)                                                      
         MVC   P(24),=C'!!DARE RECORD TOO LONG!!'                               
         GOTO1 =V(PRINTER)         YES-PRINT THIS RECORD                        
*                                                                               
MTDF95   DS    0H                                                               
         SAM31                                                                  
         GOTO1 =A(PRTDARBF),(RC)                                                
         L     RE,ADAREBUF                                                      
         SR    R3,RE                                                            
         ST    R3,BYTECNTR         SAVE BYTE COUNT IN BYTECNTR                  
         ST    R3,BUFFERLENGTH2    SAVE BYTE COUNT IN BUFFERLENGTH2             
*                                                                               
         C     R3,=A(DAREBUFL)     HAVE WE GONE PAST BUFFER END?                
         JH    *+2                 YES -- MUST INCREASE DAREBUFL                
*                                                                               
MTDFX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* DOES A PRNTBL OF WHAT IS IN ADAREBUF USING THE 64K TEMPBUF                    
*                                                                               
* ON ENTRY:    R1                  A(COMMON STORAGE AREA)                       
*              ADAREBUF            A(10MB DARE BUFFER)                          
*              BUFFERLENGTH2       LENGTH OF MESSAGE IN DARE BUFFER             
***********************************************************************         
PRTDARBF NMOD1 0,PRTDARBF                                                       
         LR    RC,R1                                                            
*                                                                               
         PRNT  -----------------                                                
*                                                                               
         L     R6,ADAREBUF                                                      
         L     R4,BUFFERLENGTH2    R4 = A(BYTE AFTER MESSAGE)                   
         AR    R4,R6                                                            
*                                                                               
PRTDBF10 LAY   RE,TEMPBUF                                                       
         L     RF,=A(TEMPBUFL)     PRINT THE FIRST 64K BYTES SINCE              
         LR    R0,R6                PRNTBL IS NOT 31-BIT COMPATIBLE             
         LR    R1,RF                AND ONLY EVER PRINTED OUT 64K               
         MVCL  RE,R0                                                            
*                                                                               
         L     R2,=A(TEMPBUFL)     PRINT ENTIRE MESSAGE BUFFER                  
         SAM24                                                                  
         GOTO1 =V(PRNTBL),DMCB,=C'DARE INPUT BUFFER',A(TEMPBUF),       +        
               C'DUMP',(R2),=C'1D'                                              
         SAM31                                                                  
*                                                                               
         AR    R6,R2               BUMP TO NEXT 64K IN DARE BUFFER              
         CR    R6,R4               ARE WE PAST THE MSG IN DARE BUFFER?          
         BL    PRTDBF10            NO, CONTINUE PRINTING                        
*                                                                               
         PRNT  -----------------                                                
*                                                                               
PRTDBFX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* OPEN/CLOSE ALL REP FILES FOR XML ORDER ROUTING VERIFICATION                   
***********************************************************************         
OPCLREPF NTR1  BASE=*,LABEL=*                                                   
         L     RF,=A(UTL)                                                       
         MVC   SVUTL,4(RF)         SAVE OFF CURRENT UTL+4                       
         L     R4,TSKFILES         LIST OF REP FILES TO OPEN                    
*                                                                               
OCREPF10 DS    0H                                                               
         CLI   0(R4),X'FF'                                                      
         BE    OCREPFX                                                          
         L     RF,=A(UTL)                                                       
         MVC   4(1,RF),0(R4)                                                    
*                                                                               
         CLI   OCFLAG,C'C'         OPEN OR CLOSE REQUEST?                       
         BE    OCREPF20                                                         
         CLI   OCFLAG,C'O'                                                      
         BNE   OCREPFX                                                          
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'REP',                    +        
               =C'NREPFILENREPDIR X',A(IO1),0                                   
         B     OCREPF30                                                         
*                                                                               
OCREPF20 DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMCLSE'),=C'REP',0,A(IO1),0                   
*                                                                               
OCREPF30 DS    0H                                                               
         LA    R4,1(R4)                                                         
         B     OCREPF10                                                         
*                                                                               
OCREPFX  DS    0H                                                               
         L     RF,=A(UTL)                                                       
         MVC   4(1,RF),SVUTL       RESTORE UTL+4                                
         XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
***********************************************************************         
* OUTPUT LOG MESSAGES                                                           
* ON INPUT: R1 = A(MESSAGE BUFFER)                                              
*           R2 = MESSAGE LENGTH                                                 
***********************************************************************         
LOGMSGQ  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SAM31                                                                  
         ST    R1,A_PUT_BUFF                                                    
         ST    R1,A_PUT2_BUFF                                                   
         ST    R1,A_PUTB_BUFF                                                   
         ST    R2,PUT_BUFLEN                                                    
*                                                                               
         CLI   LOGFLAG,C'Y'                                                     
         BE    LOGQ030                                                          
         CLI   LOGFLAGB,C'Y'                                                    
         BE    LOGQ030                                                          
         PRNT  MessageNotLogged                                                 
         B     LOGQX                                                            
*                                  OVERRIDE '+++DARE SEND TIME=HHMMSS'          
LOGQ030  MVC   13(4,R1),=C'LOG '   WITH     '+++DARE SEND LOG =HHMMSS'          
         AHI   R1,APPCM3Q          PASS +++DARE SEND RECORD                     
LOGQ033  CLC   CRLF,0(R1)          SEARCH FORWARD FOR CRLF                      
         BNE   LOGQ040             NOT FOUND, BUMP NEXT CHAR                    
         CLI   L'CRLF(R1),C'+'     FOUND - PASS CRLF                            
         BE    LOGQ040             ANOTHER +++DARE RECORD                       
         AHI   R1,L'CRLF           FOUND DARE RECORD                            
         B     LOGQ050                                                          
LOGQ040  AHI   R1,1                                                             
         B     LOGQ033                                                          
*                                  UPDATE MSGID=CL3(TSKNAME)+R+DARETYPE         
LOGQ050  MVC   MSGDESC_LOG_MSGID(3),TSKNAME                                     
         MVI   MSGDESC_LOG_MSGID+3,C'R'     R FOR RECEIVER                      
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
         PRNT  MessageLOGCommit                                                 
*                                                                               
LOGQX    XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
***********************************************************************         
*INPUT:  R1 = A(MESSAGE BUFFER)                                                 
*        R2 = MESSAGE LENGTH                                                    
*        R3 = A(USERID NAME)                                                    
*        HALF = USERID#                                                         
***********************************************************************         
PUTWRKQ  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ST    R1,A_PUTW_BUFF                                                   
         ST    R2,PUTW_BUFLEN                                                   
*                                  BUILD MSGID                                  
         MVC   MSGDESC_WORK_MSGID(3),TSKNAME                                    
         MVI   MSGDESC_WORK_MSGID+3,C'R'     R FOR RECEIVER                     
         SR    R0,R0                                                            
         ICM   R0,3,HALF           USERID #                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MSGDESC_WORK_MSGID+4(5),DUB                                      
         MVC   MSGDESC_WORK_MSGID+9(10),0(R3)   USERID NAME                     
         MVC   MSGDESC_WORK_MSGID+19(3),SNDTSKID   SUBTASK TO SEND              
*                                                                               
         MVC   OBJDESC_WORK_OBJECTTYPE,=A(MQOT_Q)  OBJECT IS A QUEUE            
         MVC   OPEN_OPTIONS,=A(MQOO_OUTPUT)                                     
*                                                                               
         MVC   OBJDESC_WORK_OBJECTNAME,SPACES                                   
         LA    R1,OBJDESC_WORK_OBJECTNAME    'DARE.SSSS.TTT.WORK.QUEUE'         
         MVC   0(5,R1),=CL5'DARE.'                                              
         AHI   R1,5                                                             
*                                                                               
         CLI   TSKDSPC,C'T'                                                     
         BNE   PWRK021                                                          
         MVC   0(5,R1),=CL5'TEST.'                                              
         AHI   R1,5                                                             
         B     PWRK040                                                          
PWRK021  CLI   TSKDSPC,C'Q'                                                     
         BNE   PWRK022                                                          
         MVC   0(4,R1),=CL4'FQA.'                                               
         AHI   R1,4                                                             
         B     PWRK040                                                          
PWRK022  EQU   *                                                                
         MVC   0(5,R1),=CL5'PROD.'                                              
         AHI   R1,5                                                             
PWRK040  EQU   *                                                                
         MVC   0(L'SNDTSKID,R1),SNDTSKID                                        
         MVI   L'SNDTSKID(R1),C'.'                                              
         AHI   R1,L'SNDTSKID+1                                                  
         MVC   0(10,R1),=C'WORK.QUEUE'                                          
*                                                                               
         LA    R2,CSQBOPEN_WORK_STRUCTURE                                       
         GOTO1 =A(CALLMQ),(RC)     OPEN WORK QUEUE                              
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         JNE   *+2                                                              
*                                                                               
         LA    R2,CSQBPUT_WORK_STRUCTURE                                        
         GOTO1 =A(CALLMQ),(RC)     PUT ONE MESSAGE TO QUEUE                     
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         JNE   *+2                                                              
*                                                                               
         MVC   P+30(L'OBJDESC_WORK_OBJECTNAME),OBJDESC_WORK_OBJECTNAME          
         MVC   P+80(6),=C'MSGID='                                               
         MVC   P+86(L'MSGDESC_WORK_MSGID),MSGDESC_WORK_MSGID                    
         PRNT  PutToWorkQueue                                                   
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* COMMIT WORK                                                                   
***********************************************************************         
CMTWRKQ  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,CSQBCOMM_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     COMMIT WORKQ                                 
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         JNE   *+2                                                              
*                                                                               
         PRNT  CommitWorkQueue                                                  
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
***********************************************************************         
* CLOSE WORK                                                                    
***********************************************************************         
CLOWRKQ  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   CLOSE_OPTIONS,=A(MQCO_NONE)                                      
         LA    R2,CSQBCLOS_WORK_STRUCTURE                                       
         GOTO1 =A(CALLMQ),(RC)     CLOSE LOG QUEUE                              
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         JNE   *+2                                                              
*                                                                               
         PRNT  CloseWorkQueue                                                   
         XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
***********************************************************************         
* COMMON STORAGE AREA COVERED BY RC,R8                                          
***********************************************************************         
COMMWORK DS    0D                                                               
*                                                                               
         CMQA LIST=YES,EQUONLY=NO                                               
*                                                                               
*----------------------------------------------------------------------         
* MQSERIES CALL PARAMETERS                                                      
*----------------------------------------------------------------------         
BUFFER       DS      CL256       WORK BUFFER                                    
*                                                                               
QMGRNAME       DS      CL48      MQ QUEUE MANAGER NAME                          
HCONN          DS      F         MQ QMGR CONNECTION HANDLE                      
HOBJ           DS      F         OBJECT HANDLE                                  
HOBJ_LOG       DS      F         OBJECT HANDLE                                  
HOBJ_LOG2      DS      F         OBJECT HANDLE                                  
HOBJ_LOGB      DS      F         OBJECT HANDLE                                  
HOBJ_WORK      DS      F         OBJECT HANDLE                                  
OPEN_OPTIONS   DS      F         MQOPEN OPTIONS                                 
CLOSE_OPTIONS  DS      F         MQCLOSE OPTIONS                                
MQ_COMPCODE    DS      F         COMPLETION CODE                                
MQ_REASON      DS      F         QUALIFIES COMPLETION CODE                      
BUFFERLENGTH   DS      F         LENGTH OF MQ BUFFER AREA FOR MQGET             
BUFFERLENGTH2  DS      F         LENGTH OF MQ BUFFER AREA FOR MQPUT             
PUT_BUFLEN     DS      F         LENGTH OF MQ BUFFER AREA FOR MQPUT             
PUTW_BUFLEN    DS      F         LENGTH OF MQ BUFFER AREA FOR MQPUT             
AMQBUFXA       DS      A         ADDRESS OF MQ BUFFER AREA IN XA                
DATALENGTH     DS      F         LENGTH OF THE MESSAGE                          
*                                                                               
HOBJ_COA       DS      F         OBJECT HANDLE                                  
GET_COA_BUFLEN DS      F         LENGTH OF MQ BUFFER AREA FOR MQGET             
DATALENGTH_COA DS      F         LENGTH OF THE MESSAGE                          
*                                                                               
OBJDESC        CMQODA  LIST=YES  OBJECT DESCRIPTOR                              
MSGDESC        CMQMDA  LIST=YES  MESSAGE DESCRIPTOR                             
         ORG   MSGDESC_FORMAT                                                   
         DC    CL8'MQSTR   '   MQFMT_STRING  FOR DATA FORMAT                    
         ORG                                                                    
OBJDESC_LOG    CMQODA  LIST=YES  OBJECT DESCRIPTOR                              
OBJDESC_LOG2   CMQODA  LIST=YES  OBJECT DESCRIPTOR                              
OBJDESC_LOGB   CMQODA  LIST=YES  OBJECT DESCRIPTOR                              
MSGDESC_LOG    CMQMDA  LIST=YES  MESSAGE DESCRIPTOR                             
         ORG   MSGDESC_LOG_FORMAT                                               
         DC    CL8'MQSTR   '   MQFMT_STRING  FOR DATA FORMAT                    
         ORG                                                                    
OBJDESC_WORK   CMQODA  LIST=YES  OBJECT DESCRIPTOR                              
MSGDESC_WORK   CMQMDA  LIST=YES  MESSAGE DESCRIPTOR                             
         ORG   MSGDESC_LOG_FORMAT                                               
         DC    CL8'MQSTR   '   MQFMT_STRING  FOR DATA FORMAT                    
         ORG                                                                    
OBJDESC_COA    CMQODA  LIST=YES  OBJECT DESCRIPTOR                              
MSGDESC_COA    CMQMDA  LIST=YES  MESSAGE DESCRIPTOR                             
         ORG   MSGDESC_LOG_FORMAT                                               
         DC    CL8'MQSTR   '   MQFMT_STRING  FOR DATA FORMAT                    
         ORG                                                                    
GETMSGOPTS      CMQGMOA LIST=YES  GET MESSAGE OPTIONS                           
GETMSGOPTS_COA  CMQGMOA LIST=YES  GET MESSAGE OPTIONS                           
PUTMSGOPTS_LOG  CMQPMOA LIST=YES  PUT MESSAGE OPTIONS                           
         ORG   PUTMSGOPTS_LOG_OPTIONS                                           
         DC    A(MQPMO_SET_IDENTITY_CONTEXT)                                    
         ORG                                                                    
PUTMSGOPTS_WORK CMQPMOA LIST=YES  PUT MESSAGE OPTIONS                           
         EJECT                                                                  
CRLF     DS    0XL2                                                             
CR       DC    X'0D'                                                            
LF       DC    X'25'                                                            
         SPACE 5                                                                
APPCM1   DC    C'+++DARE SENDER='                                               
APPCM1Q  EQU   *-APPCM1                                                         
         DC    C'XXX '                                                          
APPCM1A  DC    C'CONV=SEND VERSION=1 '                                          
APPCM1AQ EQU   *-APPCM1A                                                        
         DC    C'DATE=YYYYMMDD TIME=HHMMSS'                                     
         SPACE 2                                                                
APPCM2   DC    C'+++DARE SENDER=DDS VERSION=1 STATUS=READY '                    
         DC    C'DATE=YYYYMMDD TIME=HHMMSS'                                     
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
         SPACE 2                                                                
APPCM6   DC    C'+++DARE KEEPALIVE'                                             
APPCM6Q  EQU   *-APPCM6                                                         
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
         SPACE 3                                                                
ECBLSTAP DS    0F                                                               
         DC    X'00',AL3(APPCECB)  A(APPCECB)                                   
ASTOPECB DC    X'80',AL3(0)        A(STOPECB)                                   
         SPACE 3                                                                
ECBLSTMQ DS    0F                                                               
         DC    X'00',AL3(INPQECB)  A(INPUT QUEUE ECB)                           
ASTPECB2 DC    X'80',AL3(0)        A(STOPECB)                                   
         EJECT                                                                  
DMCB     DS    10F                                                              
BPRMDRCS DS    6F                                                               
DUB      DS    D                                                                
PRNTDUB  DS    D                   FOR PRNT MACRO                               
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
*                                                                               
BITFLGS1 DS    XL1                 VARIOUS BIT FLAGS                            
BF1DC2DS2 EQU   X'80'               - MKGDEM 2 DEC IMPS --> 1 DEC IMPS          
BF1ACDC   EQU   X'40'               - GOT A AGYCSC MESSAGE LINE                 
BF1SBATDM EQU   X'20'               - SUBSCRIBED TO AGYTDM                      
*                                                                               
PRIMDEMO DS    CL1                 'N' - NIELSEN                                
*                                  'C' - COMSCORE                               
THREE    DS    XL3                                                              
DS2DMS   DS    XL5                 AGYDS2 DEMO CATEGORY TYPES AND X'00'         
PRNTTIME DS    CL9                 FOR PRNT MACRO                               
YYYYMMDD DS    CL8                 DATE                                         
HHMMSS   DS    CL6                 TIME                                         
WORK     DS    CL256                                                            
PACKOF4B DS    PL4                                                              
MYKEY    DS    XL10                                                             
*                                                                               
ARECTAB  DS    A                   A(VALID RECORD TYPES FOR MESSAGE)            
ATSKTAB  DC    A(0)                A(SUBTASK TABLE)                             
VDDSIO   DC    V(DDSIO)                                                         
DATAMGR  DC    V(DATAMGR)                                                       
ADAREBUF DS    A                   DARE BUFFER IN XA (31 BIT)                   
AGETDARE DS    A                                                                
*                                                                               
APPCECB  DC    F'0'                ECB FOR APPC/MVS CALLS                       
INPQECB  DC    F'0'                ECB FOR MQ GET FROM INPUT QUEUE              
TLRCOUNT DS    F                   TRAILER RECORD COUNT                         
MSGCOUNT DS    F                   TOTAL INCOMING RECORD COUNT                  
RECCOUNT DS    F                   RECORD COUNT (W/OUT CONTROL RECORDS)         
ADJCOUNT DS    F                   ADJSTD COUNT (W/OUT CONTROL RECORDS)         
ERRRCCNT DS    F                   RECORD COUNT IN ERRNOT                       
BYTECNTR DS    F                   MESSAGE BYTE COUNTER                         
*                                                                               
BHDRRATE DS    F                   CL9 - COST ON THE BUYHDR                     
BDTLRATE DS    F                   CL9 - COST ON THE BUYDTL                     
*                                                                               
ORDRFLG1 DS    XL1                 ORDER FLAGS SET #1                           
ORDF1XML EQU   X'80'               - AGYHDR IS FOR AN XML ORDER                 
*                                                                               
ATLRTDOL DS    PL6                 TOTAL DOLLARS IN PENNIES - CL10              
ATLRTSPT DS    PL4                 TOTAL SPOTS IN THE ORDER - CL6               
*                                                                               
DARE     DC    C'DARE    '         DARE MAJOR ENQ NAME                          
THREESEC DC    F'300'              FOR WAITS BETWEEN PRINT QUEUE WRITES         
STIMER1  DS    XL4                 FOR WAITS BETWEEN PRINT QUEUE WRITES         
USERID   DS    XL2                 SENDER'S DDS USERID (NUMERIC)                
SENDER   DS    CL10                SENDER'S DDS USERID (EBCDIC)                 
RCVIDNUM DS    XL2                 RECEIVER'S DDS USERID (NUMERIC)              
RECEIVER DS    CL10                RECEIVER'S DDS USERID (EBCDIC)               
RCVPWRCD DS    CL2                 RECEIVER'S ALPHA/POWER CODE                  
RCVSENUM DS    X                   RECEIVER'S FILE SE NUMBER                    
SVUTL    DS    X                   SAVED UTL+4                                  
OCFLAG   DS    C                                                                
* REPFILES 1, 2, 4, 5, 7, 8, Y                                                  
*EPFILES DC    X'0838687898A8C8FF'                                              
*                                                                               
MEDIA    DS    C                   MEDIA                                        
STATION  DS    CL6                 STATION                                      
OSTATION DS    CL5                 OLD STATION IF CALL LETTER CHANGE            
ORDNUM   DS    CL8                 ORDER NUMBER                                 
ROUTCODE DS    CL5                 ROUTING CODE                                 
CONTRACT DS    CL8                 REP CONTRACT                                 
RETURN   DS    CL16                'RETURN TO SENDER' DATA                      
ORIGDATE DS    CL6                 DATE OF ORIGINATION                          
ORIGTIME DS    CL4                 TIME OF ORIGINATION                          
OFFERID  DS    CL3                 MAKEGOOD OFFER GROUP ID                      
IDSEQNUM DS    CL2                 VERSION NUMBER WITHIN OFFER ID               
LOGDATA  DS    CL10                FROM LOGGING= RECORD                         
VERFLAG  DC    C'Y'                'N' = MISSING VERSION=1 FLAG                 
ERRCODE  DS    CL3                 ERROR CODE FOR ERRNOT                        
FRSTEDDT DS    CL6                 1ST EDI SEND DATE                            
***WHEN A NEW ERROR CODE #, PLEASE ALSO UPDATE BOOK=SPDARERROR                  
SNDTSKID DS    CL3                 SUBTASK OF WHAT WILL BECOME SENDER           
OPERSTOP DC    C'N'                'Y' = OPERATOR ENTERED 'STOP'                
FIRSTRCV DC    C'N'                'Y' = VERY FIRST MESSAGE RECEIVE             
SWTOSEND DS    C                   'Y' = WE SHOULD SWITCH TO SEND MODE          
TOOLONG  DS    C                   'Y' = AN INBOUND RECORD WAS TOO LONG         
TOOLGEND DS    C                   'Y' = AN END RECORD WAS TOO LONG             
NULLREC  DS    C                   'Y' = AN INBOUND RECORD WAS NULL             
*                                                                               
PUTMSGFL DS    C                   'Y' = JUST PUT ONE MESSAGE TO WORKQ          
LOGFLAG  DS    C                   'Y' = LOG ALL MSG TO MQ LOG QUEUE            
LOGFLAG2 DS    C                   'Y' = LOG ALL MSG TO MQ LOG QUEUE            
LOGFLAGB DS    C                   'Y' = PUT ALL MSG TO BLOCKCHAIN Q            
*                                                                               
FNRCVRFL DS    X                   FLAG FOR FINDRCVR ROUTINE                    
FNRCVROP EQU   X'80'               -OPEN TABLES                                 
FNRCVRBD EQU   X'40'               -RECEIVER NOT GOOD                           
FNRCVRHM EQU   X'01'               X'01' - HOME MKT RECEIVING ID USED           
***********************************************************************         
**TEMP CODE TO REDIRECT HAWORTH ORDER BACK TO DDS-REP, YYUN, 12/11/03**         
***********************************************************************         
RCVIDFLG DS    C                   'Y' = NO RECIVER ID                          
*          DATA SET DDDARERCV  AT LEVEL 033 AS OF 03/18/05                      
***********************************************************************         
**TEMP CODE TO REDIRECT XML ORDERS BACK TO DDS-REP, SKUI, 02/11/05**            
***********************************************************************         
AMIXML   DS    C                   'Y' = XML ORDER                              
***********************************************************************         
***********************************************************************         
BCNTSMF  DC    C'BCOUNTXXXXXX',X'00000000'                                      
BCNTSMFL EQU   *-BCNTSMF                                                        
OPMSG1   DC    C'*DARE ERROR DURING HANDSHAKE WITH XXX'                         
OPMSG2   DC    C'*DARE ERROR RECEIVING FROM XXX. TRANSMISSIONS ENDED.'          
OPMSG3   DC    C'HANDSHAKE WITH RECEIVER FROM XXX OK'                           
OPMSG4   DC    C'*DARE: UNDEFINED LOCAL LUNAME XXXXXXXX, PARTNER XXX, R+        
               EPLY RETRY OR CANCEL'                                            
OPMSG5   DC    C'*DARE WARNING: PARTNER XXX HAS NOT YET ALLOCATED ITS S+        
               ENDER'                                                           
OPMSG6   DC    C'*DARE ERROR: SEVERE COMMUNICATIONS PROTOCOL ERROR WITH+        
                PARTNER XXX'                                                    
*                                                                               
ERRMSG1  DS    0C                                                               
         DC    C'AUTONOTE*SKUI,WHOA,HWON:'                                      
         DC    C'DARE ONLINE ERROR-'                                            
         DC    C'ERRCODE='                                                      
ERRMSG1A DS    CL(L'ERRCODE)                                                    
         DC    C',SENDER='                                                      
ERRMSG1B DS    CL(L'SENDER)                                                     
         DC    C',ORDER#='                                                      
ERRMSG1C DS    CL(L'ORDNUM)                                                     
ERRMSG1Q EQU   *-ERRMSG1                                                        
*                                                                               
SVMCIDTB DS    (SKIPMAXQ)CL(L'MSGDESC_MSGID+L'MSGDESC_CORRELID)                 
         DS    XL(L'MSGDESC_MSGID+L'MSGDESC_CORRELID)                           
SKIPMAXQ EQU   4                                                                
         EJECT                                                                  
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
CSQBCONN_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_Connect'                                
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_CONN_COMPCODE)                              
                          DC    A(A_CONN_REASON)                                
                          DC    X'00',AL3(QMGRNAME)                             
                          DC    X'00',AL3(HCONN)                                
A_CONN_COMPCODE           DC    X'00',AL3(MQ_COMPCODE)                          
A_CONN_REASON             DC    X'80',AL3(MQ_REASON)                            
*                                                                               
CSQBOPEN_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_OpenInput'                              
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_OPEN_COMPCODE)                              
                          DC    A(A_OPEN_REASON)                                
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(OBJDESC)                              
                          DC    X'00',AL3(OPEN_OPTIONS)                         
                          DC    X'00',AL3(HOBJ)                                 
A_OPEN_COMPCODE           DC    X'00',AL3(MQ_COMPCODE)                          
A_OPEN_REASON             DC    X'80',AL3(MQ_REASON)                            
*                                                                               
CSQBOPEN_COA_STRUCTURE    DS    A                                               
                          DC    CL16'MQ_OpenCOA'                                
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_OPEN_LOG_COMPCODE)                          
                          DC    A(A_OPEN_LOG_REASON)                            
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(OBJDESC_COA)                          
                          DC    X'00',AL3(OPEN_OPTIONS)                         
                          DC    X'00',AL3(HOBJ_COA)                             
A_OPEN_COA_COMPCODE       DC    X'00',AL3(MQ_COMPCODE)                          
A_OPEN_COA_REASON         DC    X'80',AL3(MQ_REASON)                            
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
A_OPEN_LOG_COMPCODE       DC    X'00',AL3(MQ_COMPCODE)                          
A_OPEN_LOG_REASON         DC    X'80',AL3(MQ_REASON)                            
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
A_OPEN_LOG2_COMPCODE      DC    X'00',AL3(MQ_COMPCODE)                          
A_OPEN_LOG2_REASON        DC    X'80',AL3(MQ_REASON)                            
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
A_OPEN_LOGB_COMPCODE      DC    X'00',AL3(MQ_COMPCODE)                          
A_OPEN_LOGB_REASON        DC    X'80',AL3(MQ_REASON)                            
*                                                                               
CSQBOPEN_WORK_STRUCTURE   DS    A                                               
                          DC    CL16'MQ_OpenWork'                               
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_OPEN_WORK_COMPCODE)                         
                          DC    A(A_OPEN_WORK_REASON)                           
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(OBJDESC_WORK)                         
                          DC    X'00',AL3(OPEN_OPTIONS)                         
                          DC    X'00',AL3(HOBJ_WORK)                            
A_OPEN_WORK_COMPCODE      DC    X'00',AL3(MQ_COMPCODE)                          
A_OPEN_WORK_REASON        DC    X'80',AL3(MQ_REASON)                            
*                                                                               
CSQBGET_STRUCTURE         DS    A                                               
                          DC    CL16'MQ_Get'                                    
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_GET_COMPCODE)                               
                          DC    A(A_GET_REASON)                                 
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ)                                 
                          DC    X'00',AL3(MSGDESC)                              
                          DC    X'00',AL3(GETMSGOPTS)                           
                          DC    X'00',AL3(BUFFERLENGTH)                         
A_GET_BUFF                DC    X'00',AL3(0)                                    
                          DC    X'00',AL3(DATALENGTH)                           
A_GET_COMPCODE            DC    X'00',AL3(MQ_COMPCODE)                          
A_GET_REASON              DC    X'80',AL3(MQ_REASON)                            
*                                                                               
CSQBGET_COA_STRUCTURE     DS    A                                               
                          DC    CL16'MQ_GetCOA'                                 
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_GET_COA_COMPCODE)                           
                          DC    A(A_GET_COA_REASON)                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ_COA)                             
                          DC    X'00',AL3(MSGDESC_COA)                          
                          DC    X'00',AL3(GETMSGOPTS_COA)                       
                          DC    X'00',AL3(GET_COA_BUFLEN)                       
                          DC    X'00',AL3(COABUF)                               
                          DC    X'00',AL3(DATALENGTH_COA)                       
A_GET_COA_COMPCODE        DC    X'00',AL3(MQ_COMPCODE)                          
A_GET_COA_REASON          DC    X'80',AL3(MQ_REASON)                            
*                                                                               
CSQBBACK_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_Back'                                   
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_BACK_COMPCODE)                              
                          DC    A(A_BACK_REASON)                                
                          DC    X'00',AL3(HCONN)                                
A_BACK_COMPCODE           DC    X'00',AL3(MQ_COMPCODE)                          
A_BACK_REASON             DC    X'80',AL3(MQ_REASON)                            
*                                                                               
CSQBPUT_LOG_STRUCTURE     DS    A                                               
                          DC    CL16'MQ_PutLog'                                 
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_PUT_COMPCODE)                               
                          DC    A(A_PUT_REASON)                                 
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ_LOG)                             
                          DC    X'00',AL3(MSGDESC_LOG)                          
                          DC    X'00',AL3(PUTMSGOPTS_LOG)                       
                          DC    X'00',AL3(PUT_BUFLEN)                           
A_PUT_BUFF                DC    X'00',AL3(0)                                    
A_PUT_COMPCODE            DC    X'00',AL3(MQ_COMPCODE)                          
A_PUT_REASON              DC    X'80',AL3(MQ_REASON)                            
*                                                                               
CSQBPUT_LOG2_STRUCTURE    DS    A                                               
                          DC    CL16'MQ_PutLog2'                                
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_PUT2_COMPCODE)                              
                          DC    A(A_PUT2_REASON)                                
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ_LOG2)                            
                          DC    X'00',AL3(MSGDESC_LOG)                          
                          DC    X'00',AL3(PUTMSGOPTS_LOG)                       
                          DC    X'00',AL3(PUT_BUFLEN)                           
A_PUT2_BUFF               DC    X'00',AL3(0)                                    
A_PUT2_COMPCODE           DC    X'00',AL3(MQ_COMPCODE)                          
A_PUT2_REASON             DC    X'80',AL3(MQ_REASON)                            
*                                                                               
CSQBPUT_LOGB_STRUCTURE    DS    A                                               
                          DC    CL16'MQ_PutLogB'                                
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_PUTB_COMPCODE)                              
                          DC    A(A_PUTB_REASON)                                
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ_LOGB)                            
                          DC    X'00',AL3(MSGDESC_LOG)                          
                          DC    X'00',AL3(PUTMSGOPTS_LOG)                       
                          DC    X'00',AL3(PUT_BUFLEN)                           
A_PUTB_BUFF               DC    X'00',AL3(0)                                    
A_PUTB_COMPCODE           DC    X'00',AL3(MQ_COMPCODE)                          
A_PUTB_REASON             DC    X'80',AL3(MQ_REASON)                            
*                                                                               
CSQBPUT_WORK_STRUCTURE    DS    A                                               
                          DC    CL16'MQ_PutWork'                                
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_PUTW_COMPCODE)                              
                          DC    A(A_PUTW_REASON)                                
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ_WORK)                            
                          DC    X'00',AL3(MSGDESC_WORK)                         
                          DC    X'00',AL3(PUTMSGOPTS_WORK)                      
                          DC    X'00',AL3(PUTW_BUFLEN)                          
A_PUTW_BUFF               DC    X'00',AL3(0)                                    
A_PUTW_COMPCODE           DC    X'00',AL3(MQ_COMPCODE)                          
A_PUTW_REASON             DC    X'80',AL3(MQ_REASON)                            
*                                                                               
CSQBCOMM_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_Commit'                                 
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_COMM_COMPCODE)                              
                          DC    A(A_COMM_REASON)                                
                          DC    X'00',AL3(HCONN)                                
A_COMM_COMPCODE           DC    X'00',AL3(MQ_COMPCODE)                          
A_COMM_REASON             DC    X'80',AL3(MQ_REASON)                            
*                                                                               
CSQBCLOS_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_CloseInput'                             
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_CLOSE_COMPCODE)                             
                          DC    A(A_CLOSE_REASON)                               
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ)                                 
                          DC    X'00',AL3(CLOSE_OPTIONS)                        
A_CLOSE_COMPCODE          DC    X'00',AL3(MQ_COMPCODE)                          
A_CLOSE_REASON            DC    X'80',AL3(MQ_REASON)                            
*                                                                               
CSQBCLOS_COA_STRUCTURE    DS    A                                               
                          DC    CL16'MQ_CloseCOA'                               
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_CLOSE_COA_COMPCODE)                         
                          DC    A(A_CLOSE_COA_REASON)                           
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ_COA)                             
                          DC    X'00',AL3(CLOSE_OPTIONS)                        
A_CLOSE_COA_COMPCODE      DC    X'00',AL3(MQ_COMPCODE)                          
A_CLOSE_COA_REASON        DC    X'80',AL3(MQ_REASON)                            
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
A_CLOSE_LOG_COMPCODE      DC    X'00',AL3(MQ_COMPCODE)                          
A_CLOSE_LOG_REASON        DC    X'80',AL3(MQ_REASON)                            
*                                                                               
CSQBCLOS_LOG2_STRUCTURE   DS    A                                               
                          DC    CL16'MQ_CloseLog2'                              
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_CLOSE_LOG2_COMPCODE)                        
                          DC    A(A_CLOSE_LOG2_REASON)                          
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ_LOG2)                            
                          DC    X'00',AL3(CLOSE_OPTIONS)                        
A_CLOSE_LOG2_COMPCODE     DC    X'00',AL3(MQ_COMPCODE)                          
A_CLOSE_LOG2_REASON       DC    X'80',AL3(MQ_REASON)                            
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
A_CLOSE_LOGB_COMPCODE     DC    X'00',AL3(MQ_COMPCODE)                          
A_CLOSE_LOGB_REASON       DC    X'80',AL3(MQ_REASON)                            
*                                                                               
CSQBCLOS_WORK_STRUCTURE   DS    A                                               
                          DC    CL16'MQ_CloseWork'                              
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_CLOSE_WORK_COMPCODE)                        
                          DC    A(A_CLOSE_WORK_REASON)                          
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ_WORK)                            
                          DC    X'00',AL3(CLOSE_OPTIONS)                        
A_CLOSE_WORK_COMPCODE     DC    X'00',AL3(MQ_COMPCODE)                          
A_CLOSE_WORK_REASON       DC    X'80',AL3(MQ_REASON)                            
*                                                                               
CSQBDISC_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_Disconnect'                             
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_DISC_COMPCODE)                              
                          DC    A(A_DISC_REASON)                                
                          DC    X'00',AL3(HCONN)                                
A_DISC_COMPCODE           DC    X'00',AL3(MQ_COMPCODE)                          
A_DISC_REASON             DC    X'80',AL3(MQ_REASON)                            
         SPACE 3                                                                
ENTRYPTS DS    0F                                                               
         DC    Y((ENTRYLSQ-ENTRYSTQ)/60)   NUMBER OF TABLE ENTRIES              
         DC    H'60'                       MUST REMAIN AS 60                    
ENTRYSTQ EQU   *                                                                
*                                                                               
CSQBBACK DC    CL8'CSQBBACK'                                                    
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
*                                                                               
ENTRYLSQ EQU   *                                                                
         EJECT                                                                  
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
         SPACE 2                                                                
***********************************************************************         
* THIS IS THE MASTER LIST OF ALL RECORD TYPES                                   
*                                                                               
* NOTE: KEEP ENTRIES IN SORTED ORDER!!!                                         
* ENTRIES ARE SORTED BY RECORD TEXT FOR BINSRCH                                 
***********************************************************************         
DRALLRCS DS    0CL8                VALID DARE RECORDS AND LENGTHS               
         DC    C'AGYCAN',Y(PAGYCALN)                                            
         DC    C'AGYCDC',Y(PAGYCDCL)  ORDER'S COMSCORE DEMO CATEGORIES          
         DC    C'AGYCOM',Y(PAGYCOML)                                            
         DC    C'AGYDS1',Y(PAGYDS1L)                                            
         DC    C'AGYDS2',Y(PAGYDS2L)                                            
         DC    C'AGYDS3',Y(PAGYDS3L)                                            
         DC    C'AGYDS4',Y(PAGYDS4L)                                            
         DC    C'AGYDS5',Y(PAGYDS5L)                                            
         DC    C'AGYHDR',Y(PAGYHDRL)                                            
         DC    C'AGYHIA',Y(PAGYHIAL)                                            
         DC    C'AGYRCL',Y(PAGYRCLL)                                            
         DC    C'AGYSAL',Y(PAGYSALL)                                            
         DC    C'AGYSTD',Y(PAGYSTDL)                                            
         DC    C'AGYTDM',Y(PAGYTDML)  TARGET DEMO CATEGORIES                    
         DC    C'AGYTLR',Y(PAGYTLRL)                                            
         DC    C'AGYTST',Y(PAGYTSTL)                                            
         DC    C'BUYCDV',Y(PBUYCDVL)  ORDER'S COMSCORE DEMO VALUES              
         DC    C'BUYCOM',Y(PBUYCOML)                                            
         DC    C'BUYDEM',Y(PBUYDEML)                                            
         DC    C'BUYDM2',Y(PBUYDM2L)                                            
         DC    C'BUYAAU',Y(PBUYAAUL)  BUYLINE'S AUTOMATED AVAIL UUID            
         DC    C'BUYDTL',Y(PBUYDTLL)                                            
         DC    C'BUYDV2',Y(PBUYDV2L)                                            
         DC    C'BUYHDR',Y(PBUYHDRL)                                            
         DC    C'BUYNWK',Y(PBUYNWKL)                                            
         DC    C'BUYORB',Y(PBUYORBL)                                            
         DC    C'BUYTST',Y(PBUYTSTL)                                            
         DC    C'DLNNOT',Y(MDLNNOTL)                                            
         DC    C'ERRNOT',Y(MDLNNOTL)                                            
         DC    C'MKGACM',Y(MOFRACML)                                            
         DC    C'MKGAPP',Y(MOFRAPPL)                                            
         DC    C'MKGBUY',Y(MOFRBUYL)                                            
         DC    C'MKGAAU',Y(MOFRAAUL)  MISSED/OFFERED LINE'S UUID                
         DC    C'MKGCAN',Y(MOFRCANL)                                            
         DC    C'MKGCDC',Y(MOFRCDCL)  MKGD'S COMSCORE DEMO CATEGORIES           
         DC    C'MKGCDV',Y(MOFRCDVL)  MKGD'S COMSCORE DEMO VALUES               
         DC    C'MKGCOM',Y(MOFRBCML)                                            
         DC    C'MKGDC2',Y(MOFRDC2L)                                            
         DC    C'MKGDEM',Y(MOFRDEML)                                            
         DC    C'MKGDS1',Y(MOFRDS1L)                                            
         DC    C'MKGDS2',Y(MOFRDS2L)                                            
         DC    C'MKGDPT',Y(MOFRDPTL)  offered line's daypart code               
         DC    C'MKGDTL',Y(MOFRBDTL)                                            
         DC    C'MKGHDR',Y(MOFRHDRL)                                            
         DC    C'MKGMNW',Y(MOFRMNWL)                                            
         DC    C'MKGMSS',Y(MOFRMISL)                                            
         DC    C'MKGONW',Y(MOFRONWL)                                            
         DC    C'MKGORB',Y(MORDORBL)                                            
         DC    C'MKGRCM',Y(MOFRCOML)                                            
         DC    C'MKGREJ',Y(MOFRREJL)                                            
         DC    C'MKGROK',Y(MOFRCFML)                                            
         DC    C'MKGTLR',Y(MOFRTLRL)                                            
         DC    C'MKGTST',Y(MOFRTSTL)                                            
         DC    C'MK2TLR',Y(MOF2TLRL)                                            
         DC    C'ORDAPP',Y(RORDAPPL)                                            
         DC    C'ORDCFM',Y(RORDCFML)                                            
         DC    C'ORDCOM',Y(RORDCOML)                                            
         DC    C'ORDLIN',Y(RORDLINL)                                            
         DC    C'ORDMO1',Y(RORDMO1L)                                            
         DC    C'ORDRCL',Y(RORCAPPL)                                            
         DC    C'ORDREJ',Y(RORDREJL)                                            
         DC    C'ORDSAL',Y(RORDSALL)                                            
         DC    C'ORDTLR',Y(RORDTLRL)                                            
         DC    C'ORDURL',Y(RORDURLL)                                            
         DC    C'VARCOM',Y(PAGYCOML)                                            
         DC    C'VARDS1',Y(PAGYDS1L)                                            
         DC    C'VARDS2',Y(PAGYDS2L)                                            
         DC    C'VARDS3',Y(PAGYDS3L)                                            
         DC    C'VARDS4',Y(PAGYDS4L)                                            
         DC    C'VARDS5',Y(PAGYDS5L)                                            
         DC    C'VARHDR',Y(PAGYHDRL)                                            
         DC    C'VARPRD',Y(PAGYPROL)                                            
         DC    C'VARSTD',Y(PAGYSTDL)                                            
         DC    C'VARTLR',Y(PAGYTLRL)                                            
DRALLRCX DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
* THIS IS THE LIST OF ALL DARE 6.5 RECORD TYPES                                 
*                                                                               
* NOTE: KEEP ENTRIES IN SORTED ORDER!!!                                         
* ENTRIES ARE SORTED BY RECORD TEXT FOR BINSRCH                                 
***********************************************************************         
DR65RECS DS    0CL6                VALID DARE RECORDS AND LENGTHS               
         DC    C'AGYCAN'                                                        
         DC    C'AGYCOM'                                                        
         DC    C'AGYDS1'                                                        
         DC    C'AGYDS2'                                                        
         DC    C'AGYDS3'                                                        
         DC    C'AGYDS4'                                                        
         DC    C'AGYDS5'                                                        
         DC    C'AGYHDR'                                                        
         DC    C'AGYHIA'                                                        
         DC    C'AGYRCL'                                                        
         DC    C'AGYSTD'                                                        
         DC    C'AGYTLR'                                                        
         DC    C'BUYCOM'                                                        
         DC    C'BUYDEM'                                                        
         DC    C'BUYDTL'                                                        
         DC    C'BUYHDR'                                                        
         DC    C'BUYORB'                                                        
         DC    C'DLNNOT'                                                        
         DC    C'ERRNOT'                                                        
         DC    C'MKGAPP'                                                        
         DC    C'MKGBUY'                                                        
         DC    C'MKGCAN'                                                        
         DC    C'MKGCOM'                                                        
         DC    C'MKGDS1'                                                        
         DC    C'MKGDTL'                                                        
         DC    C'MKGHDR'                                                        
         DC    C'MKGMSS'                                                        
         DC    C'MKGORB'                                                        
         DC    C'MKGRCM'                                                        
         DC    C'MKGREJ'                                                        
         DC    C'MKGROK'                                                        
         DC    C'MKGTLR'                                                        
         DC    C'ORDAPP'                                                        
         DC    C'ORDCFM'                                                        
         DC    C'ORDCOM'                                                        
         DC    C'ORDLIN'                                                        
         DC    C'ORDMO1'                                                        
         DC    C'ORDRCL'                                                        
         DC    C'ORDREJ'                                                        
         DC    C'ORDSAL'                                                        
         DC    C'ORDTLR'                                                        
         DC    C'ORDURL'                                                        
         DC    C'VARCOM'                                                        
         DC    C'VARDS1'                                                        
         DC    C'VARDS2'                                                        
         DC    C'VARDS3'                                                        
         DC    C'VARDS4'                                                        
         DC    C'VARDS5'                                                        
         DC    C'VARHDR'                                                        
         DC    C'VARPRD'                                                        
         DC    C'VARSTD'                                                        
         DC    C'VARTLR'                                                        
DR65RECX DS    0X                                                               
         EJECT                                                                  
B2SMSGS  DS    0CL6                LIST OF MESSAGES BUYER -> SELLER             
         DC    CL6'AGYCAN'           EXCL DLNNOT AND ERRNOT                     
         DC    CL6'AGYHDR'                                                      
         DC    CL6'AGYRCL'                                                      
         DC    CL6'MKGAPP'                                                      
         DC    CL6'MKGREJ'                                                      
         DC    CL6'VARHDR'                                                      
         DC    X'00'                                                            
*                                                                               
S2BMSGS  DS    0CL6                LIST OF MESSAGES SELLER -> BUYER             
         DC    CL6'MKGCAN'           EXCL DLNNOT AND ERRNOT                     
         DC    CL6'MKGHDR'                                                      
         DC    CL6'MKGROK'                                                      
         DC    CL6'ORDAPP'                                                      
         DC    CL6'ORDCFM'                                                      
         DC    CL6'ORDRCL'                                                      
         DC    CL6'ORDREJ'                                                      
         DC    X'00'                                                            
*                                                                               
MSGNOTLR DS    0CL6                LIST OF MESSAGES WITHOUT TRAILERS            
         DC    CL6'AGYCAN'                                                      
         DC    CL6'DLNNOT'                                                      
         DC    CL6'ERRNOT'                                                      
         DC    CL6'MKGAPP'                                                      
         DC    CL6'MKGROK'                                                      
         DC    CL6'MKGCAN'                                                      
         DC    CL6'AGYRCL'                                                      
         DC    CL6'ORDAPP'                                                      
         DC    CL6'ORDRCL'                                                      
         DC    X'00'                                                            
*                                                                               
MSGSINGL DS    0CL6                LIST OF MESSAGES THAT CAN BE SINGLE          
         DC    CL6'AGYCAN'           LINE TRANSMISSION                          
         DC    CL6'DLNNOT'                                                      
         DC    CL6'ERRNOT'                                                      
         DC    CL6'MKGAPP'                                                      
         DC    CL6'MKGROK'                                                      
         DC    CL6'MKGCAN'                                                      
         DC    CL6'AGYRCL'                                                      
         DC    CL6'ORDAPP'                                                      
         DC    CL6'ORDRCL'                                                      
         DC    X'00'                                                            
*                                                                               
ORDRECS  DS    0CL6                VALID RECORD TYPES IN AN ORDER               
         DC    C'AGYHDR'                                                        
         DC    C'AGYDS1'                                                        
         DC    C'AGYDS2'                                                        
         DC    C'AGYCDC'           ORDER'S COMSCORE DEMO CATEGORIES             
         DC    C'AGYTDM'           TARGET DEMO CATEGORIES                       
         DC    C'AGYDS3'                                                        
         DC    C'AGYDS4'                                                        
         DC    C'AGYDS5'                                                        
         DC    C'AGYSAL'                                                        
         DC    C'AGYSTD'                                                        
         DC    C'AGYTST'                                                        
         DC    C'AGYCOM'                                                        
         DC    C'AGYHIA'                                                        
         DC    C'BUYNWK'                                                        
         DC    C'BUYHDR'                                                        
         DC    C'BUYDEM'                                                        
         DC    C'BUYDM2'                                                        
         DC    C'BUYDV2'           ALL DEMO VALUES AT 2 DECIMALS                
         DC    C'BUYCDV'           ORDER'S COMSCORE DEMO VALUES                 
         DC    C'BUYORB'                                                        
         DC    C'BUYCOM'                                                        
         DC    C'BUYTST'                                                        
         DC    C'BUYAAU'           BUYLINE'S AUTOMATED AVAIL UUID               
         DC    C'BUYDTL'                                                        
         DC    C'AGYTLR'                                                        
         DC    C'VARHDR'                                                        
         DC    C'VARDS1'                                                        
         DC    C'VARDS2'                                                        
         DC    C'VARDS3'                                                        
         DC    C'VARDS4'                                                        
         DC    C'VARDS5'                                                        
         DC    C'VARSTD'                                                        
         DC    C'VARCOM'                                                        
         DC    C'VARPRD'                                                        
         DC    C'VARTLR'                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
ERRRECS  DS    0CL6                VALID RECORD TYPES IN AN ERRNOT              
         DC    C'ERRNOT'                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
APPRECS  DS    0CL6                VALID RECORD TYPES IN AN APPROVAL            
         DC    C'ORDAPP'                                                        
         DC    C'ORDSAL'                                                        
         DC    C'ORDMO1'                                                        
         DC    C'ORDTLR'                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
REJRECS  DS    0CL6                VALID RECORD TYPES IN A REJECTION            
         DC    C'ORDREJ'                                                        
         DC    C'ORDSAL'                                                        
         DC    C'ORDURL'                                                        
         DC    C'ORDCOM'                                                        
         DC    C'ORDLIN'                                                        
         DC    C'ORDTLR'                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
CFMRECS  DS    0CL6                VALID RECORD TYPES IN A CONFIRM              
         DC    C'ORDCFM'                                                        
         DC    C'ORDSAL'                                                        
         DC    C'ORDURL'                                                        
         DC    C'ORDCOM'                                                        
         DC    C'ORDLIN'                                                        
         DC    C'ORDTLR'                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
ACNRECS  DS    0CL6                VALID RECORD TYPES IN AN AGYCAN              
         DC    C'AGYCAN'                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
ORCRECS  DS    0CL6                VALID RECORD TYPES IN AN ORDRCL              
         DC    C'ORDRCL'                                                        
         DC    C'ORDSAL'                                                        
         DC    C'ORDTLR'                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
ARCRECS  DS    0CL6                VALID RECORD TYPES IN AN AGYRCL              
         DC    C'AGYRCL'                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
MKGRECS  DS    0CL6                VALID RECORD TYPES IN A MAKEGOOD             
         DC    C'MKGHDR'                                                        
         DC    C'MKGDS1'                                                        
         DC    C'MKGDS2'                                                        
         DC    C'MKGCDC'           MKGD'S COMSCORE DEMO CATEGORIES              
         DC    C'MKGDC2'           ALL DEMO VALUES AT 2 DECIMALS                
         DC    C'MKGTST'                                                        
         DC    C'MKGMNW'                                                        
         DC    C'MKGMSS'                                                        
         DC    C'MKGAAU'           MISSED BUYLINE AUTO-AVAIL UUID               
         DC    C'MKGONW'                                                        
         DC    C'MKGBUY'                                                        
         DC    C'MKGAAU'           OFFERED BUYLINE AUTO-AVAIL UUID              
         DC    C'MKGDEM'                                                        
         DC    C'MKGCDV'           MKGD'S COMSCORE DEMO VALUES                  
         DC    C'MKGORB'                                                        
         DC    C'MKGCOM'                                                        
         DC    C'MKGDPT'           OFFERED LINE'S DAYPART CODE                  
         DC    C'MKGDTL'                                                        
         DC    C'MKGTLR'                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
MAPRECS  DS    0CL6                VALID RECORD TYPES IN MKGD APPROVAL          
         DC    C'MKGAPP'                                                        
         DC    C'MKGACM'           WHEN WE'RE READY FOR THIS                    
         DC    C'MK2TLR'           NEW TLR TYPE, DIFF FROM THE 6.5 ONE          
         DC    X'FF'                                                            
         SPACE 2                                                                
MRJRECS  DS    0CL6                VALID RECORD TYPES IN MKGD REJECTION         
         DC    C'MKGREJ'                                                        
         DC    C'MKGRCM'                                                        
         DC    C'MKGTLR'                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
MOKRECS  DS    0CL6                VALID RECORD TYPES IN MKGD OK                
         DC    C'MKGROK'                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
MCNRECS  DS    0CL6                VALID RECORD TYPES IN MKGD CANCEL            
         DC    C'MKGCAN'                                                        
         DC    X'FF'                                                            
         EJECT                                                                  
****   ++INCLUDE DDDARETAB                                                      
       ++INCLUDE DDMOREPTAB                                                     
         EJECT                                                                  
         DS    0D                                                               
DMWORK   DS    12D                                                              
KEY      DS    XL40                                                             
KEYSAVE  DS    XL40                                                             
         DC    C'***IO***'                                                      
IO1      DC    (IO1LQ)X'00'        CTFILE/GENFIL I/O AREA                       
IO2      DC    (IO2LQ)X'00'        A 2ND IO AREA                                
*                                                                               
IO1LQ    EQU   2000                                                             
IO2LQ    EQU   2000                                                             
*                                                                               
         DS    0D                                                               
         DC    C'*ENOTBUF'                                                      
ENOTBUF  DS    (ENOTBUFL)X         ERRNOT BUFFER                                
ENOTBUFX EQU   *                                                                
ENOTBUFL EQU   APPCM3Q+APPCM4Q+MDLNNOTL+APPCM5Q+4*L'CRLF                        
         DC    CL20'***END OF ENOTBUF***'                                       
*                                                                               
         DS    0D                                                               
         DC    C'*COABUF*'                                                      
COABUF   DS    (COABUFL)X          COA BUFFER                                   
COABUFX  EQU   *                                                                
COABUFL  EQU   100                 COA HAS NO DATA                              
         DC    CL20'***END OF COABUF***'                                        
*                                                                               
         DS    0D                                                               
         DC    C'*TEMPBUF'                                                      
TEMPBUF  DS    (TEMPBUFL)X         TEMP MESSAGE BUFFER                          
TEMPBUFX EQU   *                                                                
TEMPBUFL EQU   65535               TEMP MESSAGE BUFFER LENGTH                   
         DS    CL20                                                             
         DC    X'0D25'             CRLF                                         
*                                                                               
DAREBUFL EQU   4*1024*1024         ABOVE THE LINE STORAGE - 4MB                 
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
         SPACE 3                                                                
       ++INCLUDE DDMQREASON                                                     
         SPACE 3                                                                
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
SVMCIDD  DSECT                                                                  
SVMCIDM  DS    CL(L'MSGDESC_MSGID)                                              
SVMCIDC  DS    CL(L'MSGDESC_CORRELID)                                           
         EJECT                                                                  
       ++INCLUDE DDDAREWRKD                                                     
         EJECT                                                                  
       ++INCLUDE SPDARDARED                                                     
         EJECT                                                                  
       ++INCLUDE SPDARMKGDD                                                     
         EJECT                                                                  
       ++INCLUDE REGENDAR                                                       
         EJECT                                                                  
       ++INCLUDE DDGETDARED                                                     
         EJECT                                                                  
* DMGREQUS                                                                      
* DDDPRINT                                                                      
* CTGENFILE                                                                     
       ++INCLUDE DMGREQUS                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE CTGENSTAD                                                      
         EJECT                                                                  
       ++INCLUDE FATABSD                                                        
       ++INCLUDE FATABSCOM                                                      
         EJECT                                                                  
         IEFZB4D2                                                               
         EJECT                                                                  
         IHAPSA                                                                 
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064DDDARERMQ 12/16/20'                                      
         END                                                                    
