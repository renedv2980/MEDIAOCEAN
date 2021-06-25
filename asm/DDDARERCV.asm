*          DATA SET DDDARERCV  AT LEVEL 080 AS OF 06/28/10                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 035890.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*                                                                               
*THIS IS NOW SPLITTED INTO 2 PROGRAM:DDDARERAP AND DDDARERMQ. YYUN,6/09         
*                                                                               
*PHASE DARERCVA                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
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
*                R9 -- S P A R E                                      *         
*                RA -- CPRINT                                         *         
*                RB -- PROGRAM BASE                                   *         
*                RC -- COMMON STORAGE AREA                            *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'DDDARERCV -- RECEIVE DARE TRANSMISSIONS VIA APPC/MVS'           
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
         EJECT                                                                  
DARERCV  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,DARERCV,=A(R13CHAIN)                                           
*                                                                               
         LR    R5,RC                                                            
         L     RC,=A(COMMWORK)     COMMON STORAGE AREA                          
         LR    R8,RC                                                            
         AHI   R8,4096                                                          
         USING COMMWORK,RC,R8                                                   
*                                                                               
         AHI   R5,-4                                                            
         L     R5,0(R5)            A(R1 PARAMETERS FROM ATTACH)                 
         STCM  R5,8,BYTE           HOB IS SHORT SUBTASK NAME                    
         STCM  R5,7,ATSKTAB+1      A(SUBTASK TABLE)                             
         USING TSKTABD,R5                                                       
FINDNTRY CLI   TSKSNAME,X'FF'                                                   
         BNE   *+6                                                              
         DC    H'0'                PARAMETER TO SUBTASK IS FUCKED               
         CLC   BYTE,TSKSNAME                                                    
         BE    *+12                R5 NOW POINTS TO OUR TABLE ENTRY             
         LA    R5,TSKTABLQ(,R5)                                                 
         B     FINDNTRY                                                         
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE(19),=C'DARE RECEIVER FROM '                                
         MVC   TITLE+19(3),TSKNAME                                              
         MVC   DATAMGR,TSKDMGR     A(DATAMGR) PASSED FROM SENDING TASK          
*                                                                               
         GOTO1 =A(INITIAL),(RC)                                                 
*                                                                               
         LA    R1,TSKECBSR                                                      
         STCM  R1,7,ASTOPECB+1     SET A(STOPPING ECB) IN APPC ECBLIST          
         STCM  R1,7,ASTPECB2+1     SET A(STOPPING ECB) IN MQ ECBLIST            
*                                                                               
         CLI   TSKMETH,METHAPPC    APPC/MVS?                                    
         BNE   MQINIT              NO, MQ SERIES                                
*                                                                               
REGALLOC MVC   TP_NAME_LENGTH,=F'8'                                             
         MVC   TP_NAME(7),TSKTPNAM                                              
         MVI   TP_NAME+7,C'R'      'R' FOR RECEIVER                             
         MVC   LOCAL_LU_NAME,TSKLUID                                            
*                                                                               
         LA    R2,ATBRFA2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   REGISTER FOR ALLOCATES                       
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    UNREGSTR                                                         
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    RCVALLOC            WE ARE REGISTERED FOR ALLOCATES              
         CLC   RETURN_CODE,ATBCTS_REQUEST_UNSUCCESSFUL                          
         BNE   CANTREG                                                          
         CLC   REASON_CODE,ATBCTS_INVAL_LOCAL_LU                                
         BNE   CANTREG             WE CAN'T REGISTER                            
*                                                                               
         MVC   OPMSG4+30(8),LOCAL_LU_NAME                                       
         MVC   OPMSG4+48(3),TSKNAME                                             
NOTQUITE GOTO1 =V(LOGIO),DMCB,1,(L'OPMSG4,OPMSG4)                               
         GOTO1 =V(LOGIO),DMCB,0,(6,WORK)                                        
         CLI   WORK,C'R'           DOES OPERATOR WANT TO RETRY?                 
         BE    REGALLOC                                                         
         CLI   WORK,C'C'           DOES OPERATOR WANT TO CANCEL?                
         BNE   NOTQUITE                                                         
         ABEND 705,DUMP                                                         
*                                                                               
CANTREG  MVC   P+30(14),=C'REASON CODE = '                                      
         EDIT  REASON_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         PRNT  COULDNT_REGISTER                                                 
         B     UNREGSTR                                                         
*                                                                               
RCVALLOC MVC   RECEIVE_ALLOCATE_TYPE,ATBCTS_WAIT    ASSUME NO TIMER             
         LHI   R1,60               60 SECONDS/MINUTE. . .                       
         MH    R1,TSKTIMER         . . . TIMES NUMBER OF MINUTES                
         LTR   R1,R1               . . . EQUALS NUMBER OF SECONDS               
         BZ    *+14                NO TIMOUT VALUE FOR THIS PARTNER             
         MVC   RECEIVE_ALLOCATE_TYPE,ATBCTS_TIMED                               
         ST    R1,TIME_OUT_VALUE                                                
*                                                                               
         LA    R2,ATBRAL2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   RECEIVE ALLOCATE                             
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    UNREGSTR                                                         
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    INCONV              WE HAVE A CONVERSATION                       
         CLC   RETURN_CODE,ATBCTS_REQUEST_UNSUCCESSFUL                          
         BNE   NOCONV                                                           
         CLC   REASON_CODE,ATBCTS_NO_ALLOC_TO_RECEIVE                           
         BNE   NOCONV                                                           
*                                                                               
         MVC   OPMSG5+23(3),TSKNAME  TIMEOUT: DISPLAY CONSOLE WARNING           
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMSG5,OPMSG5)                     
         B     RCVALLOC            TRY TO RECEIVE ALLOCATE AGAIN                
*                                                                               
NOCONV   PRNT  NO_CONVERSATION                                                  
         B     UNREGSTR                                                         
*                                                                               
INCONV   PRNT  OPENDARERECEIVER                                                 
*                                                                               
         MVC   TSKRCONV,CONVERSATION_ID                                         
         GOTO1 =A(QUERYALQ),(RC)   QUERY ALLOCATE QUEUE                         
         GOTO1 =A(PRNTATTB),(RC)   PRINT CONVERSATION ATTRIBUTES                
*                                                                               
         CLC   CONVERSATION_TYPE,ATB_BASIC_CONVERSATION                         
         BNE   NOTBASIC                                                         
         LA    R1,BUFFER_LENGTH    BASIC REQUIRES RECORD LENGTH                 
         STCM  R1,7,ATBRCVW_BUFFER_PARAMETER+1                                  
         STCM  R1,7,ATBSEND_BUFFER_PARAMETER+1                                  
*                                                                               
NOTBASIC GOTO1 =A(APPCBUFF),(RC)   FILL UP AN INPUT BUFFER                      
         BNE   BADSHAKE                                                         
*                                                                               
         CLI   RQSTCONF,C'Y'       CONFIRM REQUESTED?                           
         BNE   NOCONFRM                                                         
*                                                                               
         LA    R2,ATBCFMD_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   SEND CONFIRMATION                            
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    UNREGSTR                                                         
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    NOTBASIC            RETURN CODE WAS OK -- RECEIVE AGAIN          
         B     BADSHAKE                                                         
*                                                                               
NOCONFRM CLI   SWTOSEND,C'Y'       SWITCH TO SEND MODE?                         
         BNE   BADSHAKE            NO -- HOW DID WE GET HERE?                   
         CLC   APPCM1(APPCM1Q),BUFFER                                           
         BNE   BADSHAKE            DARE DID NOT RESPOND READY                   
         CLC   APPCM1A(APPCM1AQ),BUFFER+19                                      
         BNE   BADSHAKE            WRONG MODE OR VERSION NUMBER                 
*                                                                               
         PRNT  ABOUTTORESPOND                                                   
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
         LR    RF,R3                                                            
         AHI   RF,2                ADD 2 FOR CRLF                               
         LA    RE,BUFFER           ASSUME MAPPED                                
         CLC   CONVERSATION_TYPE,ATB_BASIC_CONVERSATION                         
         BNE   *+12                                                             
         LA    RE,BUFFER_LENGTH                                                 
         AHI   RF,2                ADD 2 MORE FOR LENGTH                        
         ST    RE,DMCB+4           FOR PRNTBL                                   
         STH   RF,BUFFER_LENGTH                                                 
         ST    RF,SEND_LENGTH                                                   
         LR    R2,RF               SAVE LENGTH (FOR PRNTBL)                     
         LA    RF,BUFFER(R3)                                                    
         MVC   0(2,RF),CRLF        TERMINATE RECORD WITH CRLF                   
         GOTO1 =V(PRNTBL),DMCB,=C'BUFFER DATA',,C'DUMP',(R2),=C'1D'             
*                                                                               
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
*                                                                               
         LA    R2,ATBSEND_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   SEND DATA                                    
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   OPMSG3+29(3),TSKNAME                                             
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMSG3,OPMSG3)                     
*                                                                               
         MVC   PREP_TO_RECEIVE_TYPE,ATB_PREP_TO_RECEIVE_FLUSH                   
*                                                                               
         LA    R2,ATBPTR_STRUCTURE                                              
         GOTO1 =A(CALLAPPC),(RC)   PREPARE TO RECEIVE                           
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    UNREGSTR            YES                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         MVC   FILL,ATB_FILL_LL                                                 
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
*                                                                               
         LA    R2,ATBRCVW_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   RECEIVE AND WAIT                             
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    UNREGSTR            YES                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         PRNT  FIRSTRECRECEIVED                                                 
*                                                                               
         MVI   FIRSTRCV,C'Y'       FIRST RECORD IS IN BUFFER ALREADY            
         B     NEXTMSG                                                          
         EJECT                                                                  
MQINIT   L     RF,TSKMQMGR         MQ SERIES QUEUE MANAGER NAME                 
         MVC   QMGRNAME,0(RF)                                                   
*                                                                               
         MVC   BUFFERLENGTH,=A(L'BUFFER)   FOR MQGET CALLS                      
*                                                                               
         LA    R2,CSQBCONN_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     CONNECT TO MQ QUEUE MANAGER                  
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
NEXTMSG  XC    BYTECNTR,BYTECNTR   CLEAR MESSAGE BYTE COUNTER                   
*                                                                               
         CLI   TSKMETH,METHAPPC    APPC/MVS?                                    
         BNE   GETMQBUF            NO, MQ SERIES                                
         GOTO1 =A(APPCBUFF),(RC)   FILL UP AN INPUT BUFFER VIA APPC/MVS         
         BNE   BADRCV              BAD RETURN CODE FROM LU62 SUBROUTINE         
         B     GOTBUF                                                           
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
         MVI   INBDRPQ,C'N'        ASSUME NO DARE PQ REPORT READY YET           
         MVC   ERRCODE,=C'000'     ASSUME NO ERRNOT NEEDED                      
*                                                                               
         L     R3,=A(DAREBUF)                                                   
         CLC   =C'+++DARE SEND ',0(R3)                                          
         BNE   SEVERE              SEVERE PROTOCOL ERROR                        
         CLC   CRLF,24(R3)                                                      
         BNE   SEVERE              SEVERE PROTOCOL ERROR                        
         LA    R3,26(R3)           BUMP PAST 'DARE SEND' RECORD                 
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
CHKMSG   MVC   DESCRIP,0(R3)       MESSAGE TYPE WILL BECOME PQ DESCRIP.         
*                                                                               
         CLC   =C'AGYHDR',0(R3)    IS THIS AN ORDER?                            
         BE    *+14                                                             
         CLC   =C'VARHDR',0(R3)            OR A VAR ORDER?                      
         BNE   CHKERN                                                           
         GOTO1 =A(AGYHDR),(RC)     YES -- WRITE IT TO PRTQUE                    
         BNE   BADRCV              COMMS FAILURE DURING MESSAGE RECEIVE         
         B     CONFIRM             CHECK FOR CONFIRMATION                       
*                                                                               
CHKERN   CLC   =C'ERRNOT',0(R3)    IS THIS AN ERROR NOTIFICATION?               
         BNE   CHKOAP                                                           
         GOTO1 =A(ERRNOT),(RC)     YES -- WRITE IT TO PRTQUE                    
         BNE   BADRCV              COMMS FAILURE DURING MESSAGE RECEIVE         
         B     CONFIRM             CHECK FOR CONFIRMATION                       
*                                                                               
CHKOAP   CLC   =C'ORDAPP',0(R3)    IS THIS AN ORDER APPROVAL?                   
         BNE   CHKORJ                                                           
         GOTO1 =A(ORDAPP),(RC)     YES -- WRITE IT TO PRTQUE                    
         BNE   BADRCV              COMMS FAILURE DURING MESSAGE RECEIVE         
         B     CONFIRM             CHECK FOR CONFIRMATION                       
*                                                                               
CHKORJ   CLC   =C'ORDREJ',0(R3)    IS THIS AN ORDER REJECT?                     
         BNE   CHKOCF                                                           
         GOTO1 =A(ORDREJ),(RC)     YES -- WRITE IT TO PRTQUE                    
         BNE   BADRCV              COMMS FAILURE DURING MESSAGE RECEIVE         
         B     CONFIRM             CHECK FOR CONFIRMATION                       
*                                                                               
CHKOCF   CLC   =C'ORDCFM',0(R3)    IS THIS AN ORDER CONFIRM?                    
         BNE   CHKORC                                                           
         GOTO1 =A(ORDCFM),(RC)     YES -- WRITE IT TO PRTQUE                    
         BNE   BADRCV              COMMS FAILURE DURING MESSAGE RECEIVE         
         B     CONFIRM             CHECK FOR CONFIRMATION                       
*                                                                               
CHKORC   CLC   =C'ORDRCL',0(R3)    IS THIS AN ORDER RECALL?                     
         BNE   CHKARC                                                           
         GOTO1 =A(ORDRCL),(RC)     YES -- WRITE IT TO PRTQUE                    
         BNE   BADRCV              COMMS FAILURE DURING MESSAGE RECEIVE         
         B     CONFIRM             CHECK FOR CONFIRMATION                       
*                                                                               
CHKARC   CLC   =C'AGYRCL',0(R3)    IS THIS AN AGYRCL RECALL?                    
         BNE   CHKAGC                                                           
         GOTO1 =A(AGYRCL),(RC)     YES -- WRITE IT TO PRTQUE                    
         BNE   BADRCV              COMMS FAILURE DURING MESSAGE RECEIVE         
         B     CONFIRM             CHECK FOR CONFIRMATION                       
*                                                                               
CHKAGC   CLC   =C'AGYCAN',0(R3)    IS THIS AN AGENCY CANCELLATION?              
         BNE   CHKMHD                                                           
         GOTO1 =A(AGYCAN),(RC)     YES -- WRITE IT TO PRTQUE                    
         BNE   BADRCV              COMMS FAILURE DURING MESSAGE RECEIVE         
         B     CONFIRM             CHECK FOR CONFIRMATION                       
*                                                                               
CHKMHD   CLC   =C'MKGHDR',0(R3)    IS THIS A MAKEGOOD HEADER?                   
         BNE   CHKMAP                                                           
         GOTO1 =A(MKGHDR),(RC)     YES -- WRITE IT TO PRTQUE                    
         BNE   BADRCV              COMMS FAILURE DURING MESSAGE RECEIVE         
         B     CONFIRM             CHECK FOR CONFIRMATION                       
*                                                                               
CHKMAP   CLC   =C'MKGAPP',0(R3)    IS THIS A MAKEGOOD APPROVAL?                 
         BNE   CHKMRJ                                                           
         GOTO1 =A(MKGAPP),(RC)     YES -- WRITE IT TO PRTQUE                    
         BNE   BADRCV              COMMS FAILURE DURING MESSAGE RECEIVE         
         B     CONFIRM             CHECK FOR CONFIRMATION                       
*                                                                               
CHKMRJ   CLC   =C'MKGREJ',0(R3)    IS THIS A MAKEGOOD REJECTION?                
         BNE   CHKMOK                                                           
         GOTO1 =A(MKGREJ),(RC)     YES -- WRITE IT TO PRTQUE                    
         BNE   BADRCV              COMMS FAILURE DURING MESSAGE RECEIVE         
         B     CONFIRM             CHECK FOR CONFIRMATION                       
*                                                                               
CHKMOK   CLC   =C'MKGROK',0(R3)    IS THIS A MAKEGOOD OK?                       
         BNE   CHKMCN                                                           
         GOTO1 =A(MKGROK),(RC)     YES -- WRITE IT TO PRTQUE                    
         BNE   BADRCV              COMMS FAILURE DURING MESSAGE RECEIVE         
         B     CONFIRM             CHECK FOR CONFIRMATION                       
*                                                                               
CHKMCN   CLC   =C'MKGCAN',0(R3)    IS THIS A MAKEGOOD CANCELLATION?             
         BNE   CHKDLN                                                           
         GOTO1 =A(MKGCAN),(RC)     YES -- WRITE IT TO PRTQUE                    
         BNE   BADRCV              COMMS FAILURE DURING MESSAGE RECEIVE         
         B     CONFIRM             CHECK FOR CONFIRMATION                       
*                                                                               
CHKDLN   CLC   =C'DLNNOT',0(R3)    IS THIS A DELIVERY NOTIFICATION?             
         BE    CONFIRM             DON'T DO ANYTHING, CHK FOR COMFIRM           
*                                                                               
UNKNOWN  MVC   ERRCODE,=C'003'     HEADER MISSING                               
         B     CONFIRM             CHECK FOR CONFIRMATION                       
*                                                                               
SEVERE   MVC   OPMSG6+63(3),TSKNAME                                             
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMSG6,OPMSG6)                     
*                                  EMAIL YYUN,WHOA                              
         GOTO1 DATAMGR,DMCB,=C'OPMSG',=C'AUTONOTE*YYUN,WHOA:DARE ONLINE+        
                SEVERE COMMUNICATIONS PROTOCOL ERROR!'                          
*                                                                               
CONFIRM  CLI   TSKMETH,METHAPPC    APPC/MVS?                                    
         BNE   COMMIT              NO, MQ SERIES                                
*                                                                               
         LA    R2,ATBCFMD_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   SEND CONFIRMATION                            
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    CHKDRPQ             RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
COMMIT   LA    R2,CSQBCOMM_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     COMMIT                                       
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHKDRPQ  CLI   INBDRPQ,C'Y'        DARE PQ REPORT READY TO BE SENT?             
         BNE   CHKERR              NO, CHECK ERROR                              
*                                                                               
         MVC   P+30(8),DARE                                                     
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  ENQUEUE                                                          
         ENQ   (DARE,PRTQNAME,E,5) ENQUEUE THE PRINT QUEUE                      
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'ACTIVATE'),=C'PRTQUE',R,R,A(CIREC)            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         PRNT  REPORT_ACTIVATED                                                 
*                                                                               
         MVC   P+30(8),DARE                                                     
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  DEQUEUE                                                          
         DEQ   (DARE,PRTQNAME,5)   DEQUEUE THE PRINT QUEUE                      
*                                                                               
         MVC   THREE,SNDTSKID      TELL SENDER TO LOOK FOR THE REPORT           
         GOTO1 =A(TELLSNDR),(RC)                                                
*                                                                               
CHKERR   CLC   ERRCODE,=C'000'     BUILD AN ERRNOT?                             
         BE    CHKSTOP                                                          
         GOTO1 =A(BUILDERR),(RC)   YES                                          
         EJECT                                                                  
CHKSTOP  CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    CLOSCOMM            YES -- CLOSE COMMUNICATIONS                  
*                                                                               
         B     NEXTMSG                                                          
*                                                                               
BADSHAKE PRNT  BADHANDSHAKE                                                     
         MVC   OPMSG1+34(3),TSKNAME                                             
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMSG1,OPMSG1)                     
         B     UNREGSTR                                                         
*                                                                               
BADRCV   PRNT  BADRECEIVE                                                       
         MVC   OPMSG2+27(3),TSKNAME                                             
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMSG2,OPMSG2)                     
*                                                                               
CLOSCOMM CLI   TSKMETH,METHAPPC    APPC/MVS?                                    
         BNE   MQDISC              NO, MQ SERIES                                
*                                                                               
UNREGSTR OC    ALLOCATE_QUEUE_TOKEN,ALLOCATE_QUEUE_TOKEN                        
         BZ    GOODBYE             WE NEVER REGISTERED FOR ALLOCATES            
*                                                                               
         LA    R2,ATBURA2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   UNREGISTER FOR ALLOCATES                     
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BE    GOODBYE                                                          
         DC    H'0'                                                             
*                                                                               
MQDISC   LA    R2,CSQBDISC_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     DISCONNECT FROM MQ QUEUE MANAGER             
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GOODBYE  PRNT  *****************                                                
         PRNT  *****************                                                
         PRNT  *****EXITING*****                                                
         PRNT  *****************                                                
         PRNT  *****************                                                
*                                                                               
         XBASE                                                                  
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
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
         BZ    *+16                                                             
         CLC   =X'0410',RBLK+4                                                  
         BE    *+6                 APPEND TO PREVIOUS LOG                       
         DC    H'0'                                                             
*                                                                               
         PRNT  *****************                                                
         PRNT  *****************                                                
         PRNT  ***INITIALIZE****                                                
         PRNT  *****************                                                
         PRNT  *****************                                                
*                                                                               
         L     R2,PSATOLD-PSA(,0)  TCB ADDRESS                                  
         GOTO1 =V(HEXOUT),DMCB,(R2),P+30,4,=C'TOG'                              
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         PRNT  TCBADDRESS                                                       
*                                                                               
         BLDL  0,ENTRYPTS          BUILD LIST OF EXTERNAL ENTRY PTS             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                BAD RETURN FROM BLDL MACRO                   
*                                                                               
         CLI   TSKMETH,METHAPPC    APPC/MVS?                                    
         BNE   INIT10              NO, MUST BE MQ SERIES                        
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
         LOAD  DE=ATBPTR                                                        
         ST    R0,ATBPTR_STRUCTURE                                              
         LOAD  DE=ATBRCVW                                                       
         ST    R0,ATBRCVW_STRUCTURE                                             
         LOAD  DE=ATBSEND                                                       
         ST    R0,ATBSEND_STRUCTURE                                             
         LOAD  DE=ATBQAQ2                                                       
         ST    R0,ATBQAQ2_STRUCTURE                                             
         LOAD  DE=ATBURA2                                                       
         ST    R0,ATBURA2_STRUCTURE                                             
         B     INITX                                                            
*                                                                               
INIT10   CLI   TSKMETH,METHMQ      MQ SERIES?                                   
         BE    *+6                                                              
         DC    H'0'                IF IT AIN'T APPC OR MQ, WE'RE DEAD           
         LOAD  DE=CSQBCONN                                                      
         ST    R0,CSQBCONN_STRUCTURE                                            
         LOAD  DE=CSQBOPEN                                                      
         ST    R0,CSQBOPEN_STRUCTURE                                            
         LOAD  DE=CSQBGET                                                       
         ST    R0,CSQBGET_STRUCTURE                                             
         LOAD  DE=CSQBBACK                                                      
         ST    R0,CSQBBACK_STRUCTURE                                            
         LOAD  DE=CSQBCOMM                                                      
         ST    R0,CSQBCOMM_STRUCTURE                                            
         LOAD  DE=CSQBCLOS                                                      
         ST    R0,CSQBCLOS_STRUCTURE                                            
         LOAD  DE=CSQBDISC                                                      
         ST    R0,CSQBDISC_STRUCTURE                                            
*                                                                               
INITX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
AGYHDR   NMOD1 0,AGYHDR                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS AN ORDER.  LOOK UP THE SENDING ID IN TABLE, SO WE KNOW TO             
* WHICH DARE PQ TO WRITE THE REPORT.  ALSO, DEDUCE THE RECEIVER AND             
* PUT HIS ID IN THE APPROPRIATE SLOT.                                           
*                                                                               
* UPON ENTRY, R3 = A(FIRST RECORD)                                              
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
         OC    USERID,USERID                                                    
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'007'     UNKNOWN AGENCY                               
         B     AGYHDROK                                                         
*                                                                               
         MVC   MEDIA,PAHDQMED      MEDIA                                        
         MVC   STATION,PAHDQSTA    STATION                                      
*                                                                               
         GOTO1 =A(FINDSTN),(RC)    RETURNS CALL LETTERS IN 'STATION'            
         CLC   STATION,SPACES      WAS STATION FOUND IN TABLE?                  
         BNE   *+14                YES                                          
         MVC   ERRCODE,=C'006'     UNKNOWN STATION                              
         B     AGYHDROK                                                         
*                                                                               
         MVC   PAHDQSTA(5),STATION PUT CORRECT CALL LETTERS INTO AGYHDR         
         CLC   OSTATION,SPACES     WAS STATION FOUND IN TABLE?                  
         BE    *+10                YES                                          
         MVC   PAHDOLDS(5),OSTATION PUT OLD CALL LETTERS INTO AGYHDR            
*                                                                               
         CLC   PAHDTOID+5(3),=CL3'-MO'  SPECIAL -MO RECEIVING ID?               
         BE    *+14                        OR                                   
         CLC   PAHDTOID+5(3),=CL3'-DS'          -DS RECEIVING ID?               
         BNE   AGYHDR10            NO, FIND RECEIVER ID                         
         MVC   RECEIVER,PAHDTOID                                                
         CLI   MEDIA,C'T'          IF THIS IS A TV ORDER                        
         BNE   AGYHDR70                                                         
         MVC   PAHDFSNT,SPACES     ONLY DARE DISPATCHER CARES                   
         MVC   PAHDFSTA,SPACES                                                  
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
AGYHDR25 CLI   PAHDVERS,C'9'       -ONLY- DATATECH FOR MEDIAVEST                
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
AGYHDR30 CLC   PAHDFSTA,SPACES     FIRST EDI SEND STATION SET?                  
         BNH   *+10                NO, WE'LL USE WHAT WE GOT                    
         MVC   STATION,PAHDFSTA    YES, USE FIRST EDI SEND STATION              
*                                                                               
         CLI   MEDIA,C'T'          IF THIS IS A TV ORDER                        
         BNE   AGYHDR40                                                         
         MVC   PAHDFSNT,SPACES     ENOUGH TO JUST SEND SPACES?                  
         MVC   PAHDFSTA,SPACES                                                  
*                                                                               
AGYHDR40 CLI   PAHDVERS,C'1'       OM DESKTOP?  ONLY WAY THIS IS SET            
         BNE   *+14                NO, FOLLOW OLD ROUTE DETERMINATION           
         CLC   PAHDTOID,SPACES     WAS THE RECEIVING ID FILLED IN?              
         BH    AGYHDR70            YES, THEN WE'RE GOING TO USE IT              
*                                                                               
         GOTO1 =A(FINDRCVR),(RC)   RETURNS RECEIVING ID IN 'RECEIVER'           
         BE    AGYHDR60            CC EQUAL MEANS WE'RE OK                      
         MVC   ERRCODE,=C'006'     UNKNOWN STATION                              
         B     AGYHDROK                                                         
*                                                                               
AGYHDR60 GOTO1 =A(CHKMOTAB),(RC)   SEE IF WE HAVE TO MODIFY WITH -MO            
AGYHDR65 MVC   PAHDTOID,RECEIVER                                                
*                                                                               
AGYHDR70 GOTO1 =A(CHKRCVR),(RC)    MASSAGE RECEIVER IF NECESSARY                
*                                                                               
         GOTO1 =A(FINDTSK),(RC)    RETURNS SUBTASK ID IN 'SNDTSKID'             
         OC    SNDTSKID,SNDTSKID                                                
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'005'     UNKNOWN DESTINATION ID                       
         B     AGYHDROK                                                         
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
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE PRINT QUEUES            
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'AGYHDR'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
         CLI   INBRCVOK,C'Y'       WAS MESSAGE RECEIVED OK?                     
         BE    AGYHDROK            YES                                          
*                                                                               
         LTR   RB,RB               SET CC NOT EQUAL                             
         B     AGYHDRX                                                          
*                                                                               
AGYHDROK CR    RB,RB               SET CC EQUAL                                 
*                                                                               
AGYHDRX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
ERRNOT   NMOD1 0,ERRNOT                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS AN ERROR NOTIFICATION.  LOOK UP THE SENDING ID IN TABLE, SO           
* WE KNOW TO WHICH DARE PQ TO WRITE THE REPORT.                                 
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
         PRNT  BADERRNOT                                                        
         B     ERRNOTOK            DON'T WRITE THE ERRNOT TO PRTQS              
*                                                                               
EN10     GOTO1 =A(CHKRCVR),(RC)    MASSAGE RECEIVER IF NECESSARY                
         MVC   MDNTTOID,RECEIVER   RECEIVING ID                                 
         GOTO1 =A(FINDTSK),(RC)    RETURNS SUBTASK ID IN 'SNDTSKID'             
         OC    SNDTSKID,SNDTSKID                                                
         BNZ   EN20                                                             
         MVC   P+30(38),=C'BAD RECEIVER USERID IN INCOMING ERRNOT'              
         MVC   P+11(13),=C'*** ERROR ***'                                       
         PRNT  BADERRNOT                                                        
         B     ERRNOTOK            DON'T WRITE THE ERRNOT TO PRTQS              
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
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE PRINT QUEUES            
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'ERRNOT'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
         CLI   INBRCVOK,C'Y'       WAS MESSAGE RECEIVED OK?                     
         BE    ERRNOTOK            YES                                          
*                                                                               
         LTR   RB,RB               SET CC NOT EQUAL                             
         B     ERRNOTX                                                          
*                                                                               
ERRNOTOK CR    RB,RB               SET CC EQUAL                                 
*                                                                               
ERRNOTX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
ORDAPP   NMOD1 0,ORDAPP                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS AN ORDER APPROVAL.  LOOK UP THE SENDING ID IN TABLE, SO               
* WE KNOW TO WHICH DARE PQ TO WRITE THE REPORT.                                 
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
         GOTO1 =A(FINDTSK),(RC)    RETURNS SUBTASK ID IN 'SNDTSKID'             
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
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE PRINT QUEUES            
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'ORDAPP'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
         CLI   INBRCVOK,C'Y'       WAS MESSAGE RECEIVED OK?                     
         BE    ORDAPPOK            YES                                          
*                                                                               
         LTR   RB,RB               SET CC NOT EQUAL                             
         B     ORDAPPX                                                          
*                                                                               
ORDAPPOK CR    RB,RB               SET CC EQUAL                                 
*                                                                               
ORDAPPX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
ORDREJ   NMOD1 0,ORDREJ                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS AN ORDER REJECT.  LOOK UP THE SENDING ID IN TABLE, SO                 
* WE KNOW TO WHICH DARE PQ TO WRITE THE REPORT.                                 
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
         GOTO1 =A(FINDTSK),(RC)    RETURNS SUBTASK ID IN 'SNDTSKID'             
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
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE PRINT QUEUES            
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'ORDREJ'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
         CLI   INBRCVOK,C'Y'       WAS MESSAGE RECEIVED OK?                     
         BE    ORDREJOK            YES                                          
*                                                                               
         LTR   RB,RB               SET CC NOT EQUAL                             
         B     ORDREJX                                                          
*                                                                               
ORDREJOK CR    RB,RB               SET CC EQUAL                                 
*                                                                               
ORDREJX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
ORDCFM   NMOD1 0,ORDCFM                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS AN ORDER CONFIRM.  LOOK UP THE SENDING ID IN TABLE, SO                
* WE KNOW TO WHICH DARE PQ TO WRITE THE REPORT.                                 
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
         GOTO1 =A(FINDTSK),(RC)    RETURNS SUBTASK ID IN 'SNDTSKID'             
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
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE PRINT QUEUES            
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'ORDCFM'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
         CLI   INBRCVOK,C'Y'       WAS MESSAGE RECEIVED OK?                     
         BE    ORDCFMOK            YES                                          
*                                                                               
         LTR   RB,RB               SET CC NOT EQUAL                             
         B     ORDCFMX                                                          
*                                                                               
ORDCFMOK CR    RB,RB               SET CC EQUAL                                 
*                                                                               
ORDCFMX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
ORDRCL   NMOD1 0,ORDRCL                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS AN ORDER RECALL.  LOOK UP THE SENDING ID IN TABLE, SO                 
* WE KNOW TO WHICH DARE PQ TO WRITE THE REPORT.                                 
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
         GOTO1 =A(FINDTSK),(RC)    RETURNS SUBTASK ID IN 'SNDTSKID'             
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
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE PRINT QUEUES            
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'ORDRCL'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
         CLI   INBRCVOK,C'Y'       WAS MESSAGE RECEIVED OK?                     
         BE    ORDRCLOK            YES                                          
*                                                                               
         LTR   RB,RB               SET CC NOT EQUAL                             
         B     ORDRCLX                                                          
*                                                                               
ORDRCLOK CR    RB,RB               SET CC EQUAL                                 
*                                                                               
ORDRCLX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
AGYRCL   NMOD1 0,AGYRCL                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS AN AGENCY RECALL.  LOOK UP THE SENDING ID IN TABLE, SO                
* WE KNOW TO WHICH DARE PQ TO WRITE THE REPORT.                                 
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
         MVC   PARCTOID,RECEIVER                                                
*                                                                               
         GOTO1 =A(CHKRCVR),(RC)    MASSAGE RECEIVER IF NECESSARY                
         MVC   PARCTOID,RECEIVER                                                
*                                                                               
         GOTO1 =A(FINDTSK),(RC)    RETURNS SUBTASK ID IN 'SNDTSKID'             
         OC    SNDTSKID,SNDTSKID                                                
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'005'     UNKNOWN DESTINATION                          
         B     AGYRCLOK                                                         
*                                                                               
         BRAS  RE,CHKSERR                                                       
         BNE   AGYRCLOK                                                         
*                                                                               
         LHI   R2,PARCAPPL         L'RECORD                                     
         GOTO1 =A(PUTSMF),(RC)                                                  
*                                                                               
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE PRINT QUEUES            
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'AGYRCL'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
         CLI   INBRCVOK,C'Y'       WAS MESSAGE RECEIVED OK?                     
         BE    AGYRCLOK            YES                                          
*                                                                               
         LTR   RB,RB               SET CC NOT EQUAL                             
         B     AGYRCLX                                                          
*                                                                               
AGYRCLOK CR    RB,RB               SET CC EQUAL                                 
*                                                                               
AGYRCLX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
AGYCAN   NMOD1 0,AGYCAN                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS AN AGENCY CANCELLATION.  LOOK UP THE SENDING ID IN TABLE, SO          
* WE KNOW TO WHICH DARE PQ TO WRITE THE REPORT.                                 
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
         GOTO1 =A(FINDTSK),(RC)    RETURNS SUBTASK ID IN 'SNDTSKID'             
         OC    SNDTSKID,SNDTSKID                                                
         BNZ   *+14                                                             
         MVC   ERRCODE,=C'005'     UNKNOWN DESTINATION                          
         B     AGYCANOK                                                         
*                                                                               
         BRAS  RE,CHKSERR                                                       
         BNE   AGYCANOK                                                         
*                                                                               
         LHI   R2,PAGYCANL         L'RECORD                                     
         GOTO1 =A(PUTSMF),(RC)                                                  
*                                                                               
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE PRINT QUEUES            
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'AGYCAN'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
         CLI   INBRCVOK,C'Y'       WAS MESSAGE RECEIVED OK?                     
         BE    AGYCANOK            YES                                          
*                                                                               
         LTR   RB,RB               SET CC NOT EQUAL                             
         B     AGYCANX                                                          
*                                                                               
AGYCANOK CR    RB,RB               SET CC EQUAL                                 
*                                                                               
AGYCANX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
MKGHDR   NMOD1 0,MKGHDR                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS A MAKEGOOD HEADER.  LOOK UP THE SENDING ID IN TABLE, SO               
* WE KNOW TO WHICH DARE PQ TO WRITE THE REPORT.                                 
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
         GOTO1 =A(FINDTSK),(RC)    RETURNS SUBTASK ID IN 'SNDTSKID'             
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
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE PRINT QUEUES            
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'MKGHDR'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
         CLI   INBRCVOK,C'Y'       WAS MESSAGE RECEIVED OK?                     
         BE    MKGHDROK            YES                                          
*                                                                               
         LTR   RB,RB               SET CC NOT EQUAL                             
         B     MKGHDRX                                                          
*                                                                               
MKGHDROK CR    RB,RB               SET CC EQUAL                                 
*                                                                               
MKGHDRX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
MKGAPP   NMOD1 0,MKGAPP                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS A MAKEGOOD APPROVAL.  LOOK UP THE SENDING ID IN TABLE, SO             
* WE KNOW TO WHICH DARE PQ TO WRITE THE REPORT.                                 
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
         GOTO1 =A(FINDTSK),(RC)    RETURNS SUBTASK ID IN 'SNDTSKID'             
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
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE PRINT QUEUES            
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'MKGAPP'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
         CLI   INBRCVOK,C'Y'       WAS MESSAGE RECEIVED OK?                     
         BE    MKGAPPOK            YES                                          
*                                                                               
         LTR   RB,RB               SET CC NOT EQUAL                             
         B     MKGAPPX                                                          
*                                                                               
MKGAPPOK CR    RB,RB               SET CC EQUAL                                 
*                                                                               
MKGAPPX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
MKGREJ   NMOD1 0,MKGREJ                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS A MAKEGOOD REJECTION.  LOOK UP THE SENDING ID IN TABLE, SO            
* WE KNOW TO WHICH DARE PQ TO WRITE THE REPORT.                                 
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
         GOTO1 =A(FINDTSK),(RC)    RETURNS SUBTASK ID IN 'SNDTSKID'             
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
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE PRINT QUEUES            
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'MKGREJ'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
         CLI   INBRCVOK,C'Y'       WAS MESSAGE RECEIVED OK?                     
         BE    MKGREJOK            YES                                          
*                                                                               
         LTR   RB,RB               SET CC NOT EQUAL                             
         B     MKGREJX                                                          
*                                                                               
MKGREJOK CR    RB,RB               SET CC EQUAL                                 
*                                                                               
MKGREJX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
MKGROK   NMOD1 0,MKGROK                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS A MAKEGOOD OK.  LOOK UP THE SENDING ID IN TABLE, SO                   
* WE KNOW TO WHICH DARE PQ TO WRITE THE REPORT.                                 
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
         GOTO1 =A(FINDTSK),(RC)    RETURNS SUBTASK ID IN 'SNDTSKID'             
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
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE PRINT QUEUES            
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'MKGROK'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
         CLI   INBRCVOK,C'Y'       WAS MESSAGE RECEIVED OK?                     
         BE    MKGROKOK            YES                                          
*                                                                               
         LTR   RB,RB               SET CC NOT EQUAL                             
         B     MKGROKX                                                          
*                                                                               
MKGROKOK CR    RB,RB               SET CC EQUAL                                 
*                                                                               
MKGROKX  XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
MKGCAN   NMOD1 0,MKGCAN                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PROCESS A MAKEGOOD CANCELLATION.  LOOK UP THE SENDING ID IN TABLE, SO         
* WE KNOW TO WHICH DARE PQ TO WRITE THE REPORT.                                 
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
         GOTO1 =A(FINDTSK),(RC)    RETURNS SUBTASK ID IN 'SNDTSKID'             
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
         GOTO1 =A(DRINB),(RC)      WRITE REPORT TO DARE PRINT QUEUES            
*                                                                               
         LHI   R2,BCNTSMFL         L'SMF DATA FOR MESSAGE COUNT                 
         LA    R3,BCNTSMF                                                       
         MVC   BCNTSMF+6(6),=C'MKGCAN'                                          
         MVC   BCNTSMF+12(4),BYTECNTR                                           
         GOTO1 =A(PUTSMF),(RC)     PUT BYTE COUNT RECORD TO SMF                 
*                                                                               
         CLI   INBRCVOK,C'Y'       WAS MESSAGE RECEIVED OK?                     
         BE    MKGCANOK            YES                                          
*                                                                               
         LTR   RB,RB               SET CC NOT EQUAL                             
         B     MKGCANX                                                          
*                                                                               
MKGCANOK CR    RB,RB               SET CC EQUAL                                 
*                                                                               
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
         PRNT  ENQUEUE                                                          
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
         PRNT  DEQUEUE                                                          
         DEQ   (DARE,(3),7)        DEQUEUE THE USERID TABLE                     
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
DRINB    NMOD1 0,DRINB                                                          
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* THIS ROUTINE WRITES THE MESSAGE BUFFER TO THE DARE PRINT QUEUES.              
*                                                                               
         MVC   P+30(8),DARE                                                     
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  ENQUEUE                                                          
         ENQ   (DARE,PRTQNAME,E,5) ENQUEUE THE PRINT QUEUE                      
*                                                                               
         L     R3,=A(DAREBUF)      BEGINNING OF MESSAGE                         
         XC    TLRCOUNT,TLRCOUNT   NO TRAILER RECORD COUNT YET                  
         XC    MSGCOUNT,MSGCOUNT   NO TOTAL RECORD COUNT YET                    
         XC    RECCOUNT,RECCOUNT   NO RECORD COUNT YET                          
*                                                                               
         XC    R,R                 PRINT LINE FOR PRTQUE CALLS                  
         LA    R2,R                                                             
         USING PQPLD,R2                                                         
         MVI   QLEXTRA,X'FF'       OPEN PRINT QUEUE REPORT                      
         MVC   QLSRCID,USERID      SENDING USERID                               
         MVC   QLSUBID,SNDTSKID    TASK ID (EDR = EDICTR, MQ1, ETC.)            
         MVI   QLSTAT,QLSTHO       CREATE REPORT AS "HOLD"                      
         MVI   QLCLASS,C'N'                                                     
         MVI   QLLINET,X'C0'                                                    
         MVI   QLLINEW,164                                                      
         MVC   QLRETNL,=H'48'                                                   
         MVC   QLRETND,=H'2'                                                    
         MVC   QLDESC(6),DESCRIP   TYPE OF MESSAGE RECEIVED                     
         SR    R0,R0                                                            
         ICM   R0,3,RCVIDNUM       RECEIVER'S NUMERIC USERID                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QLDESC+6(5),DUB                                                  
*                                                                               
         OI    QLATTB,QLATUSR                                                   
         MVI   QLUSRINF,C'D'       USER DATA TYPE FOR "DARE"                    
* EXTRACT SSTH FROM LAST "PRNT" CALL (PRNTDUB IN HHMMSSTH)                      
* SO, THIS FIELD BECOMES PART OF SORT KEY TO ENSURE FIFO IN THE DARESND         
         MVC   QLUSRINF+3(2),PRNTDUB+2                                          
*                                                                               
         L     RE,=A(CIREC)                                                     
         XCEFL (RE),14336          CLEAR PQ C/I BUFFER                          
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    *+14                                                             
         MVC   P+30(28),=C'COULD NOT OPEN PRTQUE REPORT'                        
         B     DRCLOPUR                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,QLSRCID                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+30(5),DUB                                                      
         MVI   P+35,C','                                                        
         MVC   P+36(3),QLSUBID                                                  
         MVI   P+39,C','                                                        
         SR    R0,R0                                                            
         ICM   R0,3,QLREPRNO                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+40(5),DUB                                                      
         PRNT  DAREINBOUND                                                      
         DROP  R2                                                               
*                                                                               
DRINB10  MVI   R,X'09'                                                          
         MVI   R+1,C' '                                                         
         MVC   R+2(L'R-2),R+1                                                   
         ST    R3,FULL             START OF TEXT TO BE PRINTED                  
         LA    R4,R+1              START OF PRINT LINE                          
*                                                                               
DRINB20  TRT   0(256,R3),TRINBND   LOOK FOR NON-PRINTING CHARACTERS             
         LR    R2,R1                                                            
         CLI   0(R2),X'FF'         END OF MESSAGE?                              
         BNE   *+14                                                             
         MVC   P+30(19),=C'+++DARE END MISSING'                                 
         B     DRCLOPUR                                                         
*                                                                               
         CLC   CRLF,0(R2)                                                       
         BE    *+16                                                             
         MVI   0(R2),C' '          REPLACE INVALID CHARACTER WITH BLANK         
         LA    R3,1(R2)                                                         
         B     DRINB20                                                          
*                                                                               
         L     R3,FULL                                                          
         SR    R2,R3               LENGTH OF STRING TO BE PRINTED               
         LTR   R2,R2               ANY DATA TO PRINT?                           
         BNZ   *+12                YES                                          
         LA    R3,2(R3)            NO - BUMP PAST CRLF                          
         B     DRINB10                                                          
*                                                                               
         LA    R1,0(R2,R4)                                                      
         LA    R0,R                                                             
         AHI   R0,164                                                           
         CR    R1,R0               LINE IS TOO LONG?                            
         BNH   *+14                                                             
         MVC   P+30(22),=C'PRINT LINE IS TOO LONG'                              
         B     DRCLOPUR            YES                                          
*                                                                               
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)       MOVE STRING INTO PRINT LINE                  
         LA    R3,3(R2,R3)         LENGTH OF STRING + BCTR + CRLF               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    *+14                                                             
         MVC   P+30(27),=C'COULD NOT WRITE PRTQUE LINE'                         
         B     DRCLOPUR                                                         
*                                                                               
         L     RE,MSGCOUNT                                                      
         AHI   RE,1                INCREMENT RECORD COUNT                       
         ST    RE,MSGCOUNT                                                      
*                                                                               
         CLC   =C'TLR',3(R4)       WAS THIS A TRAILER RECORD?                   
         BNE   DRINB30                                                          
         PACK  DUB,14(6,R4)        YES - GET COUNT IN TRAILER RECORD            
         CVB   R0,DUB                                                           
         ST    R0,TLRCOUNT                                                      
*                                                                               
DRINB30  CLC   =C'+++DARE END ',0(R4)  WAS THIS THE LAST LINE?                  
         BE    DRINB50             YES                                          
*                                                                               
         CLC   =C'+++DARE SEND ',0(R4) WAS THIS THE FIRST LINE?                 
         BE    DRINB10             YES                                          
         CLC   =C'+++DARE LOGGING=',0(R4)                                       
         BE    DRINB10                                                          
*                                                                               
         L     RF,ARECTAB          TABLE OF VALID RECORD TYPES                  
DRINB40  CLC   0(6,R4),0(RF)       MATCH ON RECORD TYPE?                        
         BNE   DRINB43                                                          
         L     RE,RECCOUNT                                                      
         AHI   RE,1                INCREMENT RECORD COUNT                       
         ST    RE,RECCOUNT                                                      
         B     DRINB10             YES - RECORD IS OK                           
*                                                                               
DRINB43  CLI   0(RF),X'FF'                                                      
         BNE   DRINB46                                                          
         MVC   ERRCODE,=C'002'     UNKNOWN RECORD TYPE                          
         MVC   P+30(19),=C'UNKNOWN RECORD TYPE'                                 
         B     DRCLOPUR                                                         
*                                                                               
DRINB46  LA    RF,6(RF)            BUMP TO NEXT ENTRY                           
         B     DRINB40                                                          
*                                                                               
DRINB50  L     RF,ARECTAB                                                       
         CLC   =C'AGYCAN',0(RF)    WAS THIS AN AGENCY CANCELLATION?             
         BE    DRINB60             YES - NO TRAILER NEEDED                      
         CLC   =C'ERRNOT',0(RF)    WAS THIS AN ERROR NOTIFICATION?              
         BE    DRINB60             YES - NO TRAILER NEEDED                      
         CLC   =C'MKGAPP',0(RF)    WAS THIS A MAKEGOOD APPROVAL?                
         BE    DRINB60             YES - NO TRAILER NEEDED                      
         CLC   =C'MKGROK',0(RF)    WAS THIS A MAKEGOOD OK?                      
         BE    DRINB60             YES - NO TRAILER NEEDED                      
         CLC   =C'MKGCAN',0(RF)    WAS THIS A MAKEGOOD CANCELLATION?            
         BE    DRINB60             YES - NO TRAILER NEEDED                      
         CLC   =C'AGYRCL',0(RF)    WAS THIS AN AGENCY RECALL?                   
         BE    DRINB60             YES - NO TRAILER NEEDED                      
*                                                                               
         CLC   =C'ORDAPP',0(RF)    WAS THIS AN APPROVAL?                        
         BE    DRINB52                                                          
         CLC   =C'ORDRCL',0(RF)         OR  AN ORDER RECALL?                    
         BNE   DRINB54             NO, A TRALER IS REQUIRED                     
*                                                                               
*   ORDAPP/ORDRCL USING UNIQUE ID IN STATION?                                   
DRINB52  CLI   STATION+5,C'0'                                                   
         BNH   DRINB60             DOESN'T ALWAYS NEED A TRAILER                
*                                                                               
DRINB54  OC    TLRCOUNT,TLRCOUNT   WAS A TRAILER RECORD SEEN?                   
         BNZ   DRINB56                                                          
         MVC   ERRCODE,=C'004'     MISSING TRAILER RECORD                       
         MVC   P+30(22),=C'MISSING TRAILER RECORD'                              
         B     DRCLOPUR                                                         
*                                                                               
DRINB56  CLC   TLRCOUNT,RECCOUNT                                                
         BE    DRINB60                                                          
         MVC   ERRCODE,=C'001'     RECORD COUNT MISMATCH                        
         MVC   P+30(21),=C'RECORD COUNT MISMATCH'                               
         B     DRCLOPUR                                                         
*                                                                               
DRINB60  PACK  DUB,30(6,R4)        RECORD COUNT IN '+++DARE END'                
         CVB   R0,DUB                                                           
         C     R0,MSGCOUNT                                                      
         BE    DRINB70                                                          
*                                                                               
         MVC   OPMSG6+63(3),TSKNAME                                             
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMSG6,OPMSG6)                     
         MVC   P+30(30),=C'ENVELOPE RECORD COUNT MISMATCH'                      
         B     DRCLOPUR            MISMATCH ON ENVELOPE RECORD COUNT            
*                                                                               
DRINB70  PRNT  ENDINBOUND                                                       
         MVI   R,X'FF'             CLOSE REPORT                                 
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    *+14                                                             
         MVC   P+30(34),=C'COULD NOT CLOSE PRINT QUEUE REPORT'                  
         B     DRCLOPUR                                                         
*                                                                               
         MVI   INBDRPQ,C'Y'        DARE PQ REPORT READY TO BE SENT              
         B     DRINBDEQ                                                         
*                                                                               
DRCLOPUR MVC   P+11(13),=C'*** ERROR ***'                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'CLO/PUR'),=C'PRTQUE',0,R,A(CIREC)             
*                                                                               
         CLI   TSKMETH,METHAPPC    APPC/MVS?                                    
         BNE   DRINBDEQ                                                         
*                                                                               
         CLI   RQSTCONF,C'Y'       DID WE GET THE WHOLE MESSAGE?                
         BE    *+6                                                              
         DC    H'0'                NO -- IMPOSSIBLE                             
*                                                                               
DRINBDEQ MVC   P+30(8),DARE                                                     
         MVC   P+40(5),PRTQNAME                                                 
         PRNT  DEQUEUE                                                          
         DEQ   (DARE,PRTQNAME,5)   DEQUEUE THE PRINT QUEUE                      
         MVI   INBRCVOK,C'Y'       INBOUND MESSAGE RECEIVED OK                  
         B     DRINBX                                                           
*                                                                               
DRINBBAD MVI   INBRCVOK,C'N'       INBOUND MSG COMMUNICATIONS FAILURE           
*                                                                               
DRINBX   XIT1                                                                   
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
BUILDERR NMOD1 0,BUILDERR                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* THIS ROUTINE GENERATES ERROR NOTIFICATIONS.                                   
*                                                                               
         OC    USERID,USERID       DID WE EVER FIND A VALID USERID?             
         BNZ   BE05                                                             
         MVC   P+30(11),=C'ERROR CODE='                                         
         MVC   P+42(3),ERRCODE                                                  
         MVC   P+60(15),=C'UNKNOWN USERID='                                     
         MVC   P+75(10),SENDER                                                  
         PRNT  ONLINE_ERRNOT       DON'T CREATE ERRNOT TO PQ                    
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
*                                  EMAIL SKUI,WHOA                              
         GOTO1 DATAMGR,DMCB,=C'OPMSG',=C'AUTONOTE*SKUI,WHOA,HWON,HQIU:D+        
               ARE ONLINE ERROR!'                                               
*                                                                               
         B     BEX                                                              
*                                                                               
BE05     MVC   P+30(8),DARE                                                     
         MVC   P+40(5),PRTQU                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (DARE,PRTQU,E,5)    ENQUEUE THE PRINT QUEUE                      
*                                                                               
         XC    R,R                                                              
         LA    R2,R                                                             
         USING PQPLD,R2                                                         
         MVI   QLEXTRA,X'FF'       OPEN PRINT QUEUE REPORT                      
         MVC   QLSRCID,USERID      SENDING USERID (OR SJR, IF NONE)             
         MVC   QLSUBID,TSKNAME     MY OWN TASK ID (RETURN TO SENDER)            
         MVI   QLCLASS,C'N'                                                     
         MVI   QLLINET,X'C0'                                                    
         MVI   QLLINEW,132                                                      
         MVC   QLRETNL,=H'48'                                                   
         MVC   QLRETND,=H'2'                                                    
         MVC   QLDESC(6),=C'ERRNOT'                                             
         SR    R0,R0                                                            
         ICM   R0,3,USERID         SENDER'S NUMERIC USERID                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QLDESC+6(5),DUB                                                  
*                                                                               
         OI    QLATTB,QLATUSR                                                   
         MVI   QLUSRINF,C'D'       USER DATA TYPE FOR "DARE"                    
* EXTRACT SSTH FROM LAST "PRNT" CALL (PRNTDUB IN HHMMSSTH)                      
* SO, THIS FIELD BECOMES PART OF SORT KEY TO ENSURE FIFO IN THE DARESND         
         MVC   QLUSRINF+3(2),PRNTDUB+2                                          
*                                                                               
         L     RE,=A(CIREC)                                                     
         XCEFL (RE),14336          CLEAR PQ BUFFER                              
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                COULD NOT OPEN PRINT QUEUE REPORT            
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,QLSRCID                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+30(5),DUB                                                      
         MVI   P+35,C','                                                        
         MVC   P+36(3),QLSUBID                                                  
         MVI   P+39,C','                                                        
         SR    R0,R0                                                            
         ICM   R0,3,QLREPRNO                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+40(5),DUB                                                      
         PRNT  CREATINGERRNOT                                                   
         DROP  R2                                                               
*                                                                               
         XC    R,R                                                              
         MVI   R,X'09'                                                          
         MVC   R+1(APPCM3Q),APPCM3 +++DARE SEND                                 
         GOTO1 =A(DATETIME),(RC)   PUT TIME INTO MESSAGE                        
         MVC   R+19(6),HHMMSS                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                COULD NOT WRITE PQ REPORT LINE               
         MVC   ERRRCCNT,=F'1'      ONE RECORD SO FAR                            
*                                                                               
         OC    LOGDATA,LOGDATA     WAS THERE ANY LOG DATA TO SEND?              
         BZ    BE10                                                             
         XC    R,R                                                              
         MVI   R,X'09'                                                          
         MVC   R+1(APPCM5Q),APPCM5 +++DARE LOGGING=                             
         MVC   R+17(L'LOGDATA),LOGDATA                                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                COULD NOT WRITE PQ REPORT LINE               
         L     RE,ERRRCCNT                                                      
         AHI   RE,1                                                             
         ST    RE,ERRRCCNT                                                      
*                                                                               
BE10     XC    R,R                                                              
         MVI   R,X'09'                                                          
         LA    R2,R+1                                                           
         USING MDLNNOTD,R2                                                      
         MVC   MDNTTID,=C'ERRNOT'  ERROR NOTIFICATION                           
         MVC   MDNTORDR,ORDNUM     ORDER NUMBER                                 
*                                                                               
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
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   MDNTTDTE,ORIGDATE   DATE OF ORIGINATION                          
         MVC   MDNTTTIM,ORIGTIME   TIME OR ORIGINATION                          
         MVC   MDNTRPCN,CONTRACT   REP CONTRACT                                 
         MVC   MDNTRTNS,RETURN     'RETURN TO SENDER' DATA                      
         MVC   MDNTOFRI,OFFERID    MAKEGOOD OFFER GROUP                         
         MVC   MDNTSEQN,IDSEQNUM   ID NUMBER WITHIN GROUP                       
         MVC   MDNTEFLG,ERRCODE    ERROR CODE                                   
         DROP  R2                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                COULD NOT WRITE PQ REPORT LINE               
         L     RE,ERRRCCNT                                                      
         AHI   RE,1                                                             
         ST    RE,ERRRCCNT                                                      
*                                                                               
         XC    R,R                                                              
         MVI   R,X'09'                                                          
         MVC   R+1(APPCM4Q),APPCM4 +++DARE END                                  
         GOTO1 =A(DATETIME),(RC)   PUT TIME INTO MESSAGE                        
         MVC   R+18(6),HHMMSS                                                   
         L     RE,ERRRCCNT                                                      
         AHI   RE,1                ONE MORE FOR FINAL RECORD                    
         CVD   RE,DUB                                                           
         UNPK  R+31(6),DUB                                                      
         OI    R+36,X'F0'          TOTAL NUMBER OF RECORDS                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                COULD NOT WRITE PQ REPORT LINE               
*                                                                               
         XC    R,R                                                              
         MVI   R,X'FF'             CLOSE REPORT                                 
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                COULD NOT CLOSE REPORT                       
*                                                                               
         MVC   P+30(8),DARE                                                     
         MVC   P+40(5),PRTQU                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (DARE,PRTQU,5)      DEQUEUE THE PRINT QUEUE                      
*                                                                               
         MVC   THREE,TSKNAME       TELL SENDER TO LOOK FOR THE REPORT           
         GOTO1 =A(TELLSNDR),(RC)                                                
*                                                                               
BEX      XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
APPCBUFF NMOD1 0,APPCBUFF                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* RECEIVE RECORDS FROM DARE, AND PUT ENTIRE MESSAGE IN DAREBUF                  
*                                                                               
         MVI   RQSTCONF,C'N'       NO REQUEST FOR CONFIRMATION YET              
         MVI   SWTOSEND,C'N'       NO REQUEST TO SWITCH TO SEND MODE            
         MVI   TOOLONG,C'N'        (NO RECORDS RECEIVED YET)                    
         L     R3,=A(DAREBUF)                                                   
         LR    RE,R3                                                            
         L     RF,=A(DAREBUFL)     CLEAR BUFFER                                 
         XCEFL                                                                  
*                                                                               
         CLI   FIRSTRCV,C'Y'       IS FIRST RECORD ALREADY RECEIVED?            
         BNE   APPCB10                                                          
         MVI   FIRSTRCV,C'N'       YES                                          
         B     APPCB20                                                          
*                                                                               
APPCB10  MVC   FILL,ATB_FILL_LL                                                 
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
*                                                                               
         LA    R2,ATBRCVW_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   RECEIVE AND WAIT                             
*                                                                               
APPCB20  CLI   OPERSTOP,C'Y'       STOP NOW?                                    
         BE    APPCBXOK                                                         
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+10                APPC ERROR ON RECEIVE AND WAIT CALL          
         LTR   RB,RB               SET CC NOT EQUAL                             
         B     APPCBXX             DON'T TRY TO RECEIVE ANY MORE                
*                                                                               
         CLC   STATUS_RECEIVED,ATB_CONFIRM_DEALLOC_RECEIVED                     
         BNE   APPCB30                                                          
*                                                                               
         PRNT  GOTSTOPANDCONFIRM                                                
*                                                                               
         LA    R2,ATBCFMD_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   SEND CONFIRMATION                            
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    APPCBXOK                                                         
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
         MVI   OPERSTOP,C'Y'                                                    
         B     APPCBXOK                                                         
*                                                                               
APPCB30  CLC   STATUS_RECEIVED,ATB_CONFIRM_RECEIVED                             
         BNE   APPCB40                                                          
         MVI   RQSTCONF,C'Y'                                                    
         PRNT  CONFIRMREQUESTED                                                 
*                                                                               
APPCB40  CLC   STATUS_RECEIVED,ATB_SEND_RECEIVED                                
         BNE   APPCB50                                                          
         MVI   SWTOSEND,C'Y'       SWITCHING TO SEND MODE                       
         PRNT  SWITCHINGTOSEND                                                  
*                                                                               
APPCB50  CLC   DATA_RECEIVED,ATB_NO_DATA_RECEIVED                               
         BE    APPCB90                                                          
         CLC   CONVERSATION_TYPE,ATB_MAPPED_CONVERSATION                        
         BNE   *+12                                                             
         ICM   R6,15,RECEIVE_LENGTH LENGTH OF RETURNED DATA (MAPPED)            
         B     *+12                                                             
         LH    R6,BUFFER_LENGTH    LENGTH OF RETURNED DATA                      
         AHI   R6,-2               MINUS 2 BYTES FOR LENGTH ITSELF              
         LTR   RF,R6                                                            
         BP    *+6                                                              
         DC    H'0'                WE TESTED FOR NO DATA JUST ABOVE!            
*                                                                               
         L     RE,BYTECNTR                                                      
         AR    RE,R6                                                            
         ST    RE,BYTECNTR         ADD BYTE COUNT TO MESSAGE TOTAL              
*                                                                               
         AHI   R6,-2               MINUS 2 BYTES FOR CRLF                       
         LA    RE,BUFFER(R6)       RE = A(END OF DATA)                          
         CLC   CRLF,0(RE)                                                       
         BE    *+6                 RECORD MUST END WITH CRLF                    
         DC    H'0'                                                             
*                                                                               
         LA    R2,DARERECS         TABLE OF KNOWN DARE RECORDS IN SPEC          
APPCB60  CLI   0(R2),X'FF'                                                      
         BE    APPCB80             RECORD NOT IN TABLE -- THAT'S OK             
         CLC   0(6,R2),BUFFER                                                   
         BE    *+12                                                             
         LA    R2,8(R2)            BUMP TO NEXT ENTRY                           
         B     APPCB60                                                          
*                                                                               
         LH    R2,6(R2)            R2 = LENGTH OF RECORD IN SPEC                
         CR    R6,R2                                                            
         BNH   APPCB70                                                          
*                                                                               
         MVC   P+11(41),=C'*** ERROR *** INCOMING RECORD IS TOO LONG'           
         GOTO1 =V(PRINTER)                                                      
         LR    R0,R6               RECORD IS LONGER THAN IT SHOULD BE           
         AHI   R0,2                ADD TWO FOR CRLF                             
         LA    RE,BUFFER                                                        
         CLC   CONVERSATION_TYPE,ATB_BASIC_CONVERSATION                         
         BNE   *+12                                                             
         LA    RE,BUFFER_LENGTH                                                 
         AHI   R0,2                ADD TWO FOR LENGTH                           
         ST    RE,DMCB+4                                                        
         GOTO1 =V(PRNTBL),DMCB,=C'TOOLONG',,C'DUMP',(R0),=C'1D'                 
         MVI   TOOLONG,C'Y'        RECORD IS TOO LONG                           
*                                                                               
APPCB70  MVI   0(R3),C' '                                                       
         BCTR  R2,0                FOR MVC TRICK                                
         BCTR  R2,0                FOR EX                                       
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R3),0(R3)       PRE-FILL BUFFER WITH SPACES                  
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),BUFFER      PLACE DATA IN BUFFER                         
         LA    R3,2(R2,R3)         POINT TO END OF DATA IN BUFFER               
         MVC   0(2,R3),CRLF        MOVE IN CRLF                                 
         LA    R3,2(R3)                                                         
         B     APPCB90                                                          
*                                                                               
APPCB80  BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),BUFFER      PLACE DATA IN BUFFER                         
         LA    R3,1(RF,R3)         POINT TO NEXT POSITION IN BUFFER             
*                                                                               
APPCB90  CLI   RQSTCONF,C'Y'       CONFIRMATION REQUESTED?                      
         BNE   *+12                                                             
         MVI   0(R3),X'FF'         END OF MESSAGE MARKER                        
         B     APPCBXOK            NO MORE TO RECEIVE                           
*                                                                               
         CLI   SWTOSEND,C'Y'       SWITCH TO SEND REQUESTED?                    
         BNE   APPCB10             NO -- GET NEXT RECORD                        
         MVI   0(R3),X'FF'         END OF MESSAGE MARKER                        
*                                                                               
APPCBXOK L     R2,=A(DAREBUFL)     PRINT ENTIRE MESSAGE BUFFER                  
         GOTO1 =V(PRNTBL),DMCB,=C'DARE INPUT BUFFER',A(DAREBUF),       +        
               C'DUMP',(R2),=C'2D'                                              
*                                                                               
         C     R3,=A(DAREBUFX)     HAVE WE GONE PAST BUFFER END?                
         BNH   *+6                                                              
         DC    H'0'                YES -- MUST INCREASE DAREBUFL                
*                                                                               
         CR    RB,RB               SET CC EQUAL                                 
*                                                                               
APPCBXX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
MQBUFF   NMOD1 0,MQBUFF                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* RECEIVE RECORDS FROM DARE, AND PUT ENTIRE MESSAGE IN DAREBUF                  
*                                                                               
         MVI   TOOLONG,C'N'        (NO RECORDS RECEIVED YET)                    
         MVI   NULLREC,C'N'        (NO RECORDS RECEIVED YET)                    
         SR    R4,R4               CLEAR RECORD COUNTER                         
         L     R3,=A(DAREBUF)                                                   
         LR    RE,R3                                                            
         L     RF,=A(DAREBUFL)     CLEAR BUFFER                                 
         XCEFL                                                                  
*                                  CLEAR SVMCIDTB                               
         LA    RE,SVMCIDTB                                                      
         LA    RF,SKIPMAXQ                                                      
         XC    0(L'SVMCIDTB,RE),0(RE)                                           
         AHI   RE,L'SVMCIDTB                                                    
         BCT   RF,*-10                                                          
                                                                                
         MVC   OBJDESC_OBJECTTYPE,=A(MQOT_Q)  OBJECT IS A QUEUE                 
         L     RF,TSKINPTQ                                                      
         MVC   OBJDESC_OBJECTNAME,0(RF)  INPUT QUEUE NAME                       
         MVC   OPEN_OPTIONS,=A((MQOO_INPUT_AS_Q_DEF+MQOO_BROWSE))               
*                                                                               
         MVC   P+30(L'OBJDESC_OBJECTNAME),OBJDESC_OBJECTNAME                    
         PRNT  ABOUT_TO_OPEN_Q                                                  
*                                                                               
         LA    R2,CSQBOPEN_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     OPEN QUEUE                                   
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   MSGDESC_CORRELID,MQCI_NONE                                       
         MVC   MSGDESC_MSGID,MQMI_NONE                                          
*                                                                               
MQBUF05  MVC   GETMSGOPTS_OPTIONS,=A(MQGMO_SET_SIGNAL)                          
         MVC   GETMSGOPTS_MATCHOPTIONS,=A((MQMO_MATCH_MSG_ID+MQMO_MATCHX        
               _CORREL_ID))                                                     
         MVC   GETMSGOPTS_SIGNAL1,=A(INPQECB)                                   
         MVC   GETMSGOPTS_WAITINTERVAL,=A(MQWI_UNLIMITED)                       
*                                                                               
MQBUF10  MVC   P+30(24),MSGDESC_MSGID                                           
         PRNT  MSGID                                                            
         MVC   P+30(24),MSGDESC_CORRELID                                        
         PRNT  CORRELID                                                         
*                                                                               
         XC    INPQECB,INPQECB                                                  
         LA    R2,CSQBGET_STRUCTURE                                             
         GOTO1 =A(CALLMQ),(RC)     TRY TO GET A MESSAGE                         
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    MQBUF40             GOT A MESSAGE                                
         CLC   MQ_COMPCODE,=A(MQCC_WARNING)                                     
         BNE   *+16                                                             
         CLC   MQ_REASON,=A(MQRC_SIGNAL_REQUEST_ACCEPTED)                       
         BE    MQBUF20                                                          
         DC    H'0'                NO OTHER WARNING IS ACCEPTABLE               
*********CLC   MQ_REASON,=A(MQRC_NO_MSG_AVAILABLE)                              
*********BE    MQBUF90                                                          
         DC    H'0'                NO ERROR IS ACCEPTABLE (FOR NOW)             
*                                                                               
MQBUF20  PRNT  ABOUTTOWAIT                                                      
*                                                                               
         WAIT  1,ECBLIST=ECBLSTMQ  WAIT FOR COMPLETION OR OPERATOR              
*                                                                               
         PRNT  WAITCOMPLETE                                                     
*                                                                               
         L     RF,ASTOPECB                                                      
         TM    0(RF),X'40'         STOP?                                        
         BZ    *+18                NO                                           
         XC    0(4,RF),0(RF)                                                    
         MVI   OPERSTOP,C'Y'                                                    
         B     MQBUF23                                                          
*                                                                               
         TM    INPQECB,X'40'                                                    
         BO    *+6                                                              
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
         L     RF,INPQECB          BITS 2-31 OF ECB CONTAIN COMP. CODE          
         SLL   RF,2                                                             
         SRL   RF,2                                                             
         C     RF,=A(MQEC_WAIT_INTERVAL_EXPIRED)                                
         BE    MQBUF23                                                          
         C     RF,=A(MQEC_MSG_ARRIVED)                                          
         BE    *+6                                                              
         DC    H'0'                NO OTHER RESULT ACCEPTABLE (FOR NOW)         
         XC    INPQECB,INPQECB     CLEAR ECB BEFORE MQGET CALL                  
         B     MQBUF85             A MESSAGE IS WAITING: GET IT                 
*                                                                               
MQBUF23  DS    0H                  UNIT OF WORK IS NOT COMPLETED YET            
         LA    R2,CSQBBACK_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     BACK                                         
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    *+6                                                              
         DC    H'0'                                                             
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
*                                  EMAIL YYUN                                   
         GOTO1 DATAMGR,DMCB,=C'OPMSG',=C'AUTONOTE*YYUN,YYUN:DARE MQ1RCV+        
               R MQGET TIME OUT!'                                               
*                                                                               
         MVC   P+30(24),MSGDESC_MSGID                                           
         PRNT  SKIP_MSGID                                                       
         MVC   P+30(24),MSGDESC_CORRELID                                        
         PRNT  SKIP_CORRELID                                                    
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
         BE    *+6                                                              
         DC    H'0'                NO ERROR IS ACCEPTABLE (FOR NOW)             
*                                                                               
* END OF QUEUE, NO OTHER UNIT OF WORK FOUND, BACK TO THE SKIP ONE.              
         MVC   GETMSGOPTS_OPTIONS,=A(MQGMO_BROWSE_FIRST)                        
         XC    INPQECB,INPQECB                                                  
         LA    R2,CSQBGET_STRUCTURE                                             
         GOTO1 =A(CALLMQ),(RC)     POINT THE CURSOR BACK TO 1ST MSG             
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    *+6                                                              
         DC    H'0'                CANT FAIL NOW                                
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
MQBUF35  PRNT  NOW_BROWSE_NEXT                                                  
         MVC   GETMSGOPTS_OPTIONS,=A(MQGMO_BROWSE_NEXT)                         
         B     MQBUF28                                                          
*                                                                               
MQBUF38  MVI   TOOLONG,C'N'        (NO RECORDS RECEIVED YET)                    
         MVI   NULLREC,C'N'        (NO RECORDS RECEIVED YET)                    
         SR    R4,R4               CLEAR RECORD COUNTER                         
         L     R3,=A(DAREBUF)                                                   
         LR    RE,R3                                                            
         L     RF,=A(DAREBUFL)     CLEAR BUFFER                                 
         XCEFL                                                                  
         B     MQBUF05                                                          
*                                                                               
MQBUF40  CLC   MSGDESC_MSGTYPE,=A(MQMT_DATAGRAM)                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   R6,15,DATALENGTH    R6 = LENGTH OF DATA IN MESSAGE               
         BNZ   MQBUF42                                                          
*                                                                               
         MVC   P+11(37),=C'*** ERROR *** INCOMING RECORD IS NULL'               
         GOTO1 =V(PRINTER)                                                      
         MVI   NULLREC,C'Y'        RECORD IS NULL                               
         B     MQBUF10             GET NEXT MESSAGE                             
*                                                                               
MQBUF42  EQU   *                                                                
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
         BNE   MQBUF45                                                          
         MVI   VERFLAG,C'Y'        ASSUME VERSION FLAG IS OKAY                  
         CLC   =C'VERSION=1',BUFFER+13  YES -- IS IT THE RIGHT VERSION?         
         BE    *+8                                                              
         MVI   VERFLAG,C'N'        MISSING VERSION FLAG                         
*                                                                               
         MVC   0(APPCM3Q,R3),APPCM3   CREATE FAKE +++DARE SEND RECORD           
         GOTO1 =A(DATETIME),(RC)   PUT TIME INTO MESSAGE                        
         MVC   18(6,R3),HHMMSS                                                  
         MVC   24(2,R3),CRLF       MOVE IN CRLF                                 
         LA    R3,APPCM3Q+2(,R3)   BUMP BUFFER POINTER                          
         B     MQBUF85             GET NEXT RECORD                              
*                                                                               
MQBUF45  CLC   =C'+++DARE END',BUFFER   IS THIS THE LAST LINE?                  
         BNE   MQBUF50                                                          
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
MQBUF50  LA    R2,DARERECS         TABLE OF KNOWN DARE RECORDS IN SPEC          
MQBUF60  CLI   0(R2),X'FF'                                                      
         BE    MQBUF80             RECORD NOT IN TABLE -- THAT'S OK             
         CLC   0(6,R2),BUFFER                                                   
         BE    *+12                                                             
         LA    R2,8(R2)            BUMP TO NEXT ENTRY                           
         B     MQBUF60                                                          
*                                                                               
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
MQBUF70  MVI   0(R3),C' '                                                       
         BCTR  R2,0                FOR MVC TRICK                                
         BCTR  R2,0                FOR EX                                       
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R3),0(R3)       PRE-FILL BUFFER WITH SPACES                  
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),BUFFER      PLACE DATA IN BUFFER                         
         LA    R3,2(R2,R3)         POINT TO END OF DATA IN BUFFER               
         MVC   0(2,R3),CRLF        MOVE IN CRLF                                 
         LA    R3,2(R3)                                                         
         B     MQBUF85                                                          
*                                                                               
MQBUF80  L     R6,DATALENGTH                                                    
         AHI   R6,2                FOR CRLF                                     
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),BUFFER      PLACE DATA IN BUFFER                         
         LA    R3,1(R6,R3)         POINT TO NEXT POSITION IN BUFFER             
*                                                                               
*                                  WAIT NO MORE THAN 1 MIN FOR NEXT MSG         
MQBUF85  MVC   GETMSGOPTS_WAITINTERVAL,=A(20000)    20 SEC                      
*MQBUF85  MVC   GETMSGOPTS_WAITINTERVAL,=A(60000)                               
         B     MQBUF10                                                          
*                                                                               
MQBUF90  MVI   0(R3),X'FF'         END OF MESSAGE MARKER                        
*                                                                               
MQBUFXOK L     R2,=A(DAREBUFL)     PRINT ENTIRE MESSAGE BUFFER                  
         GOTO1 =V(PRNTBL),DMCB,=C'DARE INPUT BUFFER',A(DAREBUF),       +        
               C'DUMP',(R2),=C'2D'                                              
*                                                                               
         C     R3,=A(DAREBUFX)     HAVE WE GONE PAST BUFFER END?                
         BNH   *+6                                                              
         DC    H'0'                YES -- MUST INCREASE DAREBUFL                
*                                                                               
MQBUFCLO MVC   CLOSE_OPTIONS,=A(MQCO_NONE)                                      
         LA    R2,CSQBCLOS_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     CLOSE QUEUE                                  
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BE    *+6                                                              
         DC    H'0'                                                             
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
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
PUTSMF   NMOD1 0,PUTSMF                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* R2 = L'RECORD                                                                 
* R3 = A(RECORD TO PUT TO SMF)                                                  
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
         MVC   SMFDATA(0),0(R3)    DATA PORTION OF RECORD                       
         LR    R1,R4               R1 = A(PARAMETERS TO SVC)                    
         LHI   R0,22                                                            
         LNR   R0,R0               R0 = -22                                     
         SVC   247                 FOR STATISTICS                               
*                                                                               
         AHI   R2,1                RESTORE RECORD LENGTH                        
         AHI   R2,2                ADD TWO FOR LENGTH ITSELF                    
         GOTO1 =V(PRNTBL),DMCB,=C'SMF RECORD',(R4),C'DUMP',(R2),=C'1D'          
         DROP  R4                                                               
*                                                                               
         AHI   R2,-SMFOVLQ                                                      
         CHI   R2,L'P                                                           
         BNL   PS30                                                             
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R3)          DATA PORTION OF RECORD                       
         GOTO1 =V(PRINTER)                                                      
         B     PSX                                                              
*                                                                               
PS30     MVC   P,0(R3)             PRINT OUT 1ST DATA                           
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PSX      XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
TELLSNDR NMOD1 0,TELLSNDR                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* A MESSAGE HAS BEEN WRITTEN TO THE PRINT QUEUES, SO WE MUST TELL THE           
* APPROPRIATE SENDER TO LOOK FOR IT.  UPON ENTRY, FIELD 'THREE'                 
* CONTAINS THE TASK-ID OF THE SENDER.                                           
*                                                                               
         L     RF,ATSKTAB          TABLE OF SENDER'S ECBS                       
TS10     CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                SERIOUS BUG SOMEWHERE                        
         CLC   THREE,TSKNAME-TSKTABD(RF)  FIND MATCH ON SENDER'S ID             
         BE    *+12                GOT IT                                       
         LA    RF,TSKTABLQ(,RF)    BUMP TO NEXT TABLE ENTRY                     
         B     TS10                                                             
*                                                                               
         LA    R2,TSKECBPQ-TSKTABD(,RF)    A(ECB TO SCAN PQS)                   
         POST  (2)                                                              
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE THAT GETS CALLED BY AGYHDR TO DEDUCE THE RECEIVING ID                 
* OF THE ORDER BASED ON THE STATION'S REP AND AGENCY'S OFFICE                   
***********************************************************************         
FINDRCVR NMOD1 0,FINDRCVR                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         LR    R6,R3                                                            
         USING PAGYHDRD,R6                                                      
*                                                                               
         MVI   FNRCVRFL,FNRCVRGD   X'80' ASSUME NO TROUBLE HERE                 
*                                                                               
         LA    R3,=C'STATTABL'                                                  
         MVC   P+30(8),DARE                                                     
         MVC   P+40(8),0(R3)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (DARE,(3),E,8)      ENQUEUE THE STATION TABLE                    
*                                                                               
         LA    R3,=C'OFEXTABL'                                                  
         MVC   P+30(8),DARE                                                     
         MVC   P+40(8),0(R3)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (DARE,(3),E,8)      ENQUEUE THE OFFICE EXCEPTION TABLE           
*                                                                               
* FOUND FIRST RECORD FOR THAT DSTA, LET'S SEE IF EFFECTIVE                      
         CLC   PAHDFSNT(6),SPACES   ANY FIRST EDI SEND DATE?                    
         BH    FR10                 YES, USE THIS DATE LOOKING FOR DSTA         
         GOTO1 =V(DATCON),DMCB,(5,0),(3,DUB)  NO, PROCESS AS IF NEW             
         B     FR20                                                             
*                                                                               
FR10     GOTO1 =V(DATCON),DMCB,(0,PAHDFSNT),(3,DUB)                             
FR20     XR    RE,RE               EFFECTIVE DATE WAS NEGATED NOT FF'D          
         ICM   RE,7,DUB                                                         
         LNR   RE,RE                                                            
         STCM  RE,7,DUB                                                         
*                                                                               
FR30     L     R2,TSKSTATB         R2 = A(ENTRY IN TBL OF DSTAS/REPS)           
         USING STATTABD,R2                                                      
FR40     CLI   0(R2),X'FF'                                                      
         BNE   *+12                                                             
FR50     NI    FNRCVRFL,X'FF'-FNRCVRGD   STATION HAS DISAPPEARED                
         B     FRX                                                              
*                                                                               
         CLC   MEDIA,STATMED                                                    
         BNE   *+14                                                             
         CLC   STATION,STATSTN                                                  
         BE    FR60                GOT IT                                       
         LA    R2,STATTBLQ(,R2)    BUMP TO NEXT ENTRY                           
         B     FR40                                                             
*                                                                               
FR60     CLC   STATEFDT,DUB        THIS DSTA RECORD IS APPLICABLE?              
         BNL   FR70                YES, OLDER THAN 1ST EDI SEND                 
         LR    R3,R2               R3 = COPY THIS IN CASE                       
         AHI   R2,STATTBLQ                                                      
         CLC   STATION,STATSTN     SAME STATION STILL?                          
         BNE   FR50                NO, NOT GOOD                                 
         B     FR60                YES, CHECK 1ST EDI SEND DATE                 
*                                                                               
FR70     CLC   STATNSTN,=5X'FF'    DO WE HAVE A NEW STATION CALLS?              
         BE    FR90                NO                                           
         CLC   STATNSTD,=5X'FF'      IS IT AN EFFECTIVE DATE?                   
         BE    FR80                  NO, OLD CALLS                              
         GOTO1 =V(DATCON),DMCB,(6,STATNSTD),(3,DUB)                             
         MVC   STATION,STATNSTN    WE'RE USING THIS STATION AND EFF NOW         
         MVI   STATION+5,C' '                                                   
         MVC   OSTATION,STATSTN    STATION WE WERE LOOKING FOR IS OLD           
         B     FR20                                                             
*                                                                               
FR80     MVC   OSTATION,STATNSTN   NEW CALLS WITH FFFF EFF DATE IS OLD          
*                                                                               
FR90     DS    0H                                                               
         MVC   PAHDQSTA,STATION    OVERWRITE PAGYHDR WITH WHAT WE HAVE          
         CLC   OSTATION,SPACES                                                  
         BNH   *+10                                                             
         MVC   PAHDOLDS(5),OSTATION                                             
*                                                                               
         L     R3,=A(REPIDS)       TABLE OF RECEIVING REPS                      
FR100    CLI   0(R3),X'FF'                                                      
         BNE   *+12                                                             
         NI    FNRCVRFL,X'FF'-FNRCVRGD   MISSING TABLE ENTRY                    
         B     FRX                                                              
*                                                                               
         CLC   STATREP,0(R3)                                                    
         BE    *+12                GOT IT                                       
         LA    R3,L'REPIDS(R3)     SKIP TO NEXT ENTRY                           
         B     FR100                                                            
*                                                                               
* SPECIAL CODE FOR CASHPLUS (CE-MN). IF STATION IS KNTV, ROUTE THE              
* ORDER TO KNTV INSTEAD OF NBC NSO                                              
*                                                                               
         TM    13(R3),X'10'        AM I A MEDIA OCEAN REP?                      
         BZ    FR110                                                            
         CLC   =C'NBC',15(R3)      NBC REP?                                     
         BNE   FR110                                                            
         CLC   =C'KNTV',PAHDQSTA   STATION IS KNTV?                             
         BNE   FR110                                                            
         CLC   =C'CE MN',PAHDROUT  AGY ROUTE IS CE-MN?                          
         BNE   FR110                                                            
         MVC   RECEIVER,SPACES     SET RECEIVER TO KNTV                         
         MVC   RECEIVER(4),=C'KNTV'                                             
         B     FRX                                                              
*                                                                               
* FOR INCOMING AGENCY XML ORDERS, ONLY DDS REPPED STATIONS ARE ALLOWED          
* TO PASS THROUGH THE DISPATCHER                                                
*                                                                               
* ALLOW MO REPPED STATIONS TO RECEIVE XML ORDERS AS WELL                        
*                                                                               
FR110    DS    0H                                                               
         MVI   AMIXML,C'N'                                                      
         CLI   PAHDREVN,C'+'       XML ORDER?                                   
         BNE   FR150                                                            
         MVI   AMIXML,C'Y'         YES, I AM A XML ORDER                        
*                                                                               
         TM    13(R3),X'10'        AM I A MEDIA OCEAN REP?                      
         BZ    FR130                                                            
         CLC   =C'NBC',15(R3)      NBC REP?                                     
         BNE   FR130                                                            
         CLC   =C'NY',PAHDROUT+3   AGENCY IS NY OFFICE?                         
         BNE   FR120                                                            
         CLC   =C'WNBC',PAHDQSTA   STATION IS WNBC?                             
         BNE   FR130                                                            
         MVC   RECEIVER,SPACES     SET RECEIVER TO WNBC                         
         MVC   RECEIVER(4),=C'WNBC'                                             
         B     FRX                                                              
*                                                                               
FR120    DS    0H                                                               
         CLC   =C'CH',PAHDROUT+3   AGENCY IS CH OFFICE?                         
         BNE   FR130                                                            
         CLC   =C'WMAQ',PAHDQSTA   STATION IS WMAQ?                             
         BNE   FR130                                                            
         MVC   RECEIVER,SPACES     SET RECEIVER TO WMAQ                         
         MVC   RECEIVER(4),=C'WMAQ'                                             
         B     FRX                                                              
*                                                                               
FR130    DS    0H                                                               
         TM    13(R3),X'01'        NOT A DDS REP?                               
         BZ    FR150                                                            
*                                                                               
*        TM    13(R3),X'10'        AM I A MEDIA OCEAN REP?                      
*        BZ    FR150                                                            
*        CLC   =C'NBC',15(R3)      ERROR UNLESS REP IS NBC                      
*        BE    FR150               WE WILL DIRECT NBCXX TO NBCXX-DS             
*                                                                               
FR140    DS    0H                                                               
         NI    FNRCVRFL,X'FF'-FNRCVRGD                                          
         B     FRX                                                              
         DROP  R6                                                               
*                                                                               
FR150    DS    0H                                                               
         CLC   RECEIVER,SPACES     RECEIVER FILLED IN FROM AGYHDR?              
         BNH   FR170                                                            
         CLC   CONTRACT,=8C'0'     ANYTHING TO ZERO OUT?                        
         BNH   FR180               NO, LEAVE ALONE (COULD BE NOTDARE)           
*                                                                               
         ZIC   R4,14(R3)                                                        
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   RECEIVER(0),15(R3)                                               
         BE    FR180                                                            
*   REP ID IS NOW DIFFERENT                                                     
         OC    STATCITY,STATCITY   ANY HOME MARKET CITY CODE?                   
         BZ    FR160                                                            
         CLC   STATCITY,ROUTCODE+3 YES -- MATCH ON AGENCY ROUTING CODE?         
         BNE   FR160                                                            
         CLC   RECEIVER,STATUID    YES -- USE STATION USERID, NOT REP           
         BE    FR180                                                            
FR160    MVC   CONTRACT,=8X'FF'    FF TO SIGNIFY LEAVE CONTRACT ALONE           
         B     FR180                                                            
FR170    MVC   CONTRACT,=8C'0'                                                  
         TM    STATFLG1,SF1H2UID                                                
         BO    FR190                                                            
*                                                                               
FR180    CLC   RECEIVER,STATUID    RECEIVER MATCHS STATION UID?                 
         BE    FRX                 DON'T NEED HOME MARKET THEN                  
         OC    STATCITY,STATCITY   ANY HOME MARKET CITY CODE?                   
         BZ    FR190                                                            
         CLC   STATCITY,ROUTCODE+3 YES -- MATCH ON AGENCY ROUTING CODE?         
         BNE   FR190                                                            
         TM    STATFLG1,SF1H2UID                                                
         BO    FR190                                                            
         MVC   RECEIVER,STATUID    YES -- USE STATION USERID, NOT REP           
         OI    FNRCVRFL,FNRCVRHM   X'01' - HOME MKT RECEIVING ID USED           
         B     FRX                                                              
*                                                                               
FR190    MVC   RECEIVER,SPACES                                                  
         ZIC   R4,14(R3)           CONSTRUCT RECEIVING ID                       
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   RECEIVER(0),15(R3)  REP-ID                                       
         TM    13(R3),X'80'        APPEND OFFICE CODE?                          
         BO    FRX                 NO                                           
*                                                                               
         LA    R4,RECEIVER+1(R4)   POINT BEYOND REP-ID                          
         MVC   0(2,R4),ROUTCODE+3  OFFICE                                       
*                                                                               
         L     R3,TSKOEXTB         TABLE OF OFFICE EXCEPTIONS                   
         USING OFEXTABD,R3                                                      
FR200    CLI   0(R3),X'FF'                                                      
         BE    FRX                 NO EXCEPTION -- USE ROUTING CODE             
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
FRX      LA    R3,=C'STATTABL'                                                  
         MVC   P+30(8),DARE                                                     
         MVC   P+40(8),0(R3)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (DARE,(3),8)        DEQUEUE THE STATION TABLE                    
*                                                                               
         LA    R3,=C'OFEXTABL'                                                  
         MVC   P+30(8),DARE                                                     
         MVC   P+40(8),0(R3)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (DARE,(3),8)        DEQUEUE THE OFFICE EXCEPTION TABLE           
*                                                                               
         TM    FNRCVRFL,FNRCVRGD   WERE WE SUCCESSFUL?                          
         BZ    *+10                                                             
         CR    RB,RB               YES, SET CC EQUAL                            
         B     *+6                                                              
         LTR   RB,RB               NO, SET CC NOT EQUAL                         
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
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
*                                                                               
CHKRCV05 DS    0H                                                               
***********************************************************************         
***********************************************************************         
*                                                                               
         CLC   =C'KNA',RECEIVER    KATZ NATIONAL IS NO LONGER VALID             
         BNE   *+14                                                             
         MVC   RECEIVER(3),=C'KAM' PREFIX 'KNA' BECOMES 'KAM'                   
         B     CHKRCVOK                                                         
*                                                                               
         CLC   =C'PETPT',RECEIVER  CHECK FOR OBSOLETE PETRY USERID              
         BNE   CHKRCV10                                                         
         MVC   RECEIVER+3(5),RECEIVER+5  "PETNYXXXXX" BECOMES. . .              
         MVC   RECEIVER+8(2),SPACES      "PETXXXXX  "                           
         B     CHKRCVOK                                                         
*                                                                               
CHKRCV10 CLC   =C'KTZ',RECEIVER    CHECK FOR OBSOLETE KATZ USERID               
         BNE   CHKRCVOK                                                         
*                                                                               
         CLI   MEDIA,C' '                                                       
         BE    CHKRCVOK            NO STATION, SO CAN'T LOOK IT UP              
*                                                                               
         LA    R3,=C'STATTABL'                                                  
         MVC   P+30(8),DARE                                                     
         MVC   P+40(8),0(R3)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (DARE,(3),E,8)      ENQUEUE THE STATION TABLE                    
*                                                                               
         L     R2,TSKSTATB         TABLE OF STATIONS/REPS                       
         USING STATTABD,R2                                                      
CHKRCV20 CLI   0(R2),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                STATION HAS DISAPPEARED                      
         CLC   MEDIA,STATMED                                                    
         BNE   *+14                                                             
         CLC   STATION,STATSTN                                                  
         BE    *+12                GOT IT                                       
         LA    R2,STATTBLQ(,R2)    BUMP TO NEXT ENTRY                           
         B     CHKRCV20                                                         
*                                                                               
         MVC   RECEIVER(3),STATREP CHANGE KTZ WITH NEW KATZ REP                 
*                                                                               
         LA    R3,=C'STATTABL'                                                  
         MVC   P+30(8),DARE                                                     
         MVC   P+40(8),0(R3)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (DARE,(3),8)        DEQUEUE THE STATION TABLE                    
*                                                                               
CHKRCVOK CR    RB,RB               SET CC EQUAL                                 
         B     CHKRCVX                                                          
*                                                                               
CHKRCVNO LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
CHKRCVX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
CHKMOTAB NMOD1 0,CHKMOTAB                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
***********************************************************************         
* FOR WJLA XML AND NON-DDS AGENCY ORDERS, ALWAYS SEND TO REPPAK                 
***********************************************************************         
***********************************************************************         
* SPECIAL VERSION FOR YI TESTING (SKUI)                                         
***********************************************************************         
         CLC   =C'DKUI',STATION    IS STATION QAXN? FOR TESTING                 
         BNE   CKMOTB02                                                         
         CLI   AMIXML,C'Y'         XML ORDER ?                                  
         BNE   CKMOTB02            YES, DON'T INSERT -MO, JUST EXIT             
         CLC   =X'2221234',ORDNUM                                               
         BE    CKMOTB77                                                         
*                                                                               
CKMOTB02 DS    0H                                                               
         CLC   =C'QAXN',STATION    IS STATION QAXN? FOR TESTING                 
         BE    CKMOTB03                                                         
         CLC   =C'WJLA',STATION    IS STATION WJLA?                             
         BNE   CKMOTB10                                                         
CKMOTB03 CLI   AMIXML,C'Y'         XML ORDER ?                                  
         BE    CKMOTBX             YES, DON'T INSERT -MO, JUST EXIT             
*                                                                               
* CHECK IF NON-DDS AGENCY                                                       
*                                                                               
         LA    RE,NODDSTAB                                                      
CKMOTB05 CLC   ROUTCODE,0(RE)      YES, DON'T INSERT -MO, JUST EXIT             
         BE    CKMOTBX                                                          
         LA    RE,L'NODDSTAB(RE)                                                
         CLI   0(RE),X'FF'                                                      
         BE    CKMOTB10                                                         
         B     CKMOTB05                                                         
*                                                                               
***********************************************************************         
* EXAMINE THE RECEIVING USERID IN 'RECEIVER' AND THE STATION                    
* APPEND "-MO" IF THE AS OF DATE IS NULLS                                       
***********************************************************************         
***********************************                                             
* REP-OFFICE-DATE                                                               
*                                                                               
* WE HAVE OTHER REP-OFFICE THAT ARE GOING TO MEDIAOCEAN BY DATE                 
* **NOTE**                                                                      
* IF WE MATCH ON REP-OFFICE BUT NOT DATE, ORDER STAYS ON REPPAK                 
***********************************                                             
CKMOTB10 L     R1,=A(MORODTAB)                                                  
         LA    R2,RECEIVER                                                      
CKMOTB20 CLI   0(R1),X'FF'         EOT?                                         
         BE    CKMOTB50            YES                                          
         LLC   R4,4(R1)            L(REP-OFFICE) ENTRY                          
         BCTR  R4,0                MINUS 1 FOR EX!                              
         EX    R4,*+8              MATCH ON REP-OFFICE?                         
         BE    CKMOTB30            YES, CHECK DATE                              
         CLC   5(0,R1),0(R2)                                                    
         LLC   R4,4(R1)            NO, BUMP TO NEXT REP                         
         LA    R1,5(R1,R4)                                                      
         B     CKMOTB20                                                         
*                                                                               
***  POINT TO DATE FOR COMPARE                                                  
CKMOTB30 LLC   R4,4(R1)                                                         
         LA    R6,0(R2)                                                         
         LA    R6,0(R4,R6)         R6=A(AFTER OFFICE CODE IN EDICT)             
*                                                                               
         MVC   FULL,0(R1)          SAVE OF CONVERSION DATE                      
         B     CKMOTB70            NOW WE CHECK IF -MO OR REPPAK                
***********************************                                             
* WE HAVE OTHER REP-OFFICE THAT ARE GOING TO MEDIAOCEAN BY STATION/DATE         
* **NOTE**                                                                      
* IF WE MATCH ON REP-OFFICE BUT NOT STATION/DATE, ORDER STAYS ON REPPAK         
***********************************                                             
CKMOTB50 L     R1,=A(MOOFSTTB)                                                  
         LA    R2,RECEIVER                                                      
CKMOTB55 CLI   0(R1),X'FF'         EOT?                                         
         BE    CKMOTBX             YES                                          
         LLC   R4,2(R1)            L(REP-OFFICE) ENTRY                          
         BCTR  R4,0                MINUS 1 FOR EX!                              
         EX    R4,*+8              MATCH ON REP-OFFICE?                         
         BE    CKMOTB60            YES, CHECK STATION AND DATE                  
         CLC   3(0,R1),0(R2)                                                    
         XR    R4,R4               NO, BUMP TO NEXT REP                         
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
         LA    R1,17(R1)           NO, NEXT STATION/DATE FOR REP-OFFICE         
         B     CKMOTB63                                                         
*                                                                               
CKMOTB66 OC    5(4,R1),5(R1)       WAS THE STATION ALREADY CONVERTED?           
         BNZ   CKMOTB70            NO, LEAVE RECEIVER ALONE                     
         MVC   0(3,R6),=C'-MO'     YES, GOTO A FORCED MEDIAOCEAN ID             
         B     CKMOTBX                                                          
*                                                                               
CKMOTB68 MVC   FULL,5(R1)          SAVE OF CONVERSION DATE                      
*                                                                               
* IF XML, CHECK IF ORDER EXISTS IN REPPAK, IF SO, ROUTE IT TO                   
* REPPAK, ELSE ROUTE IT TO MO                                                   
*                                                                               
CKMOTB70 DS    0H                                                               
         CLI   AMIXML,C'Y'         XML ORDER ?                                  
         BE    CKMOTB76                                                         
*                                                                               
* CHECK IF HARRIS AGENCY                                                        
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
         GOTO1 =V(DATCON),DMCB,(5,0),(19,WORK)                                  
         GOTO1 =V(HEXOUT),DMCB,WORK+1,WORK+10,2                                 
         CLC   WORK+10(4),FULL                                                  
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
* USER ID NOT IN REPPAK, ROUTE ORDER TO MO                                      
*                                                                               
         MVC   0(3,R6),=C'-MO'                                                  
         B     CKMOTBX                                                          
CKMOTB80 CLC   RECEIVER,UIDNAME                                                 
         BE    CKMOTB85                                                         
         LA    RE,UIDTBLQ(,RE)     BUMP TO NEXT ENTRY                           
         B     CKMOTB78                                                         
*                                                                               
CKMOTB85 DS    0H                                                               
         CLI   UIDREPFN,X'FF'      NOT REPPAK USER                              
         BNE   CKMOTB90            ROUTE TO MO                                  
         MVC   0(3,R6),=C'-MO'                                                  
         B     CKMOTBX                                                          
*                                                                               
CKMOTB90 DS    0H                                                               
         MVC   RCVPWRCD,UIDREPCD                                                
         MVC   RCVSENUM,UIDREPFN                                                
         DROP  RE                                                               
*                                                                               
         L     RE,TSKUTL           GET A(UTL) FROM DDDARE                       
         MVC   SVUTL,4(RE)         SAVE OFF ORIG UTL+4                          
         MVC   4(1,RE),RCVSENUM    SWITCH TO CORRECT REP FILE                   
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
         MVC   0(3,R6),=C'-MO'     NO, ROUTE XML TO MO                          
*                                                                               
CKMOTB95 DS    0H                                                               
         L     RE,TSKUTL           GET A(UTL) FROM DDDARE                       
         MVC   4(1,RE),SVUTL       RESTORE SAVED UTL+4                          
*                                                                               
CKMOTBX  DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
       ++INCLUDE DDNODDSTAB                                                     
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
***********************************************************************         
FINDSTN  NMOD1 0,FINDSTN                                                        
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* LAST BYTE OF THE 2 BYTE BAND IS NOT REQUIRED                                  
         MVI   STATION+5,C' '      TO MATCH STATTABL FROM DDDARE                
*                                                                               
         MVC   OSTATION,SPACES     CLEAR OUT OLD STATION CALL LETTERS           
         LA    R3,=C'STATTABL'                                                  
         MVC   P+30(8),DARE                                                     
         MVC   P+40(8),0(R3)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (DARE,(3),E,8)      ENQUEUE THE STATION TABLE                    
*                                                                               
         L     R2,TSKSTATB         TABLE OF STATIONS/REPS                       
         USING STATTABD,R2                                                      
FS10     CLI   0(R2),X'FF'                                                      
         BNE   *+14                                                             
         MVC   STATION,SPACES      STATION NOT FOUND                            
         B     FSX                                                              
*                                                                               
         CLC   MEDIA,STATMED                                                    
         BNE   *+14                                                             
         CLC   STATION,STATSTN                                                  
         BE    *+12                GOT IT                                       
         LA    R2,STATTBLQ(,R2)    BUMP TO NEXT ENTRY                           
         B     FS10                                                             
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(15,FULL)  TODAY: 0CYYDDDF                 
         CLC   STATNSTD,FULL       HAVE WE REACHED EFFECTIVE DATE?              
         BH    FS20                NO -- SEE IF THERE'S A NEW STATION           
         MVC   STATION(5),STATNSTN NEW STATION CALLS (STATNSTN IS 5)            
         MVI   STATION+5,C' '                                                   
         MVC   OSTATION,STATSTN    OLD STATION CALL LETTERS                     
         B     FSX                                                              
*                                                                               
FS20     CLC   STATNSTN,=5X'FF'    IS THERE A NEW STATION PRESENT?              
         BE    FSX                                                              
         MVC   OSTATION,STATNSTN   YES -- PASS IT ALONG AS OLD STATION          
         DROP  R2                                                               
*                                                                               
FSX      LA    R3,=C'STATTABL'                                                  
         MVC   P+30(8),DARE                                                     
         MVC   P+40(8),0(R3)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (DARE,(3),8)        DEQUEUE THE STATION TABLE                    
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
FINDTSK  NMOD1 0,FINDTSK                                                        
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* FIND THE LU6.2 PARTNER FOR THE RECEIVER OF THE MESSAGE.                       
*                                                                               
         XC    SNDTSKID,SNDTSKID                                                
*                                                                               
         LA    R3,=C'UIDTABL'                                                   
         MVC   P+30(8),DARE                                                     
         MVC   P+40(7),0(R3)                                                    
         PRNT  ENQUEUE                                                          
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
         PRNT  DEQUEUE                                                          
         DEQ   (DARE,(3),7)        DEQUEUE THE USERID TABLE                     
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
FINDMED  NMOD1 0,FINDMED                                                        
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* GIVEN THE STATION CALL LETTERS IN FIELD 'STATION', DEDUCE THE MEDIA,          
* AND RETURN IT IN THE FIELD 'MEDIA'.                                           
*                                                                               
         CLI   STATION+4,C'T'      T MEANS TV                                   
         BE    FNDMEDTV                                                         
*                                                                               
         CLI   STATION+4,C'L'      L MEANS TV (LOW-POWER)                       
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
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
PRNTATTB NMOD1 0,PRNTATTB                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PRINT DETAILS ABOUT THE CURRENT STATE OF THE LU6.2 CONVERSATION               
*                                                                               
         PRNT  *************                                                    
         PRNT  GETATTRIBUTES                                                    
         PRNT  *************                                                    
*                                                                               
         LA    R2,ATBGTA2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   GET CONVERSATION ATTRIBUTES                  
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,CONVERSATION_ID,P+30,8,=C'TOG'                   
         PRNT  CONVERSATION_ID                                                  
         MVC   P+30(17),PARTNER_LU_NAME                                         
         PRNT  PARTNER_LU_NAME                                                  
         MVC   P+30(8),MODE_NAME                                                
         PRNT  MODE_NAME                                                        
         MVC   P+30(4),=C'NONE'                                                 
         CLC   SYNC_LEVEL,ATB_NONE                                              
         BE    *+22                                                             
         MVC   P+30(7),=C'CONFIRM'                                              
         CLC   SYNC_LEVEL,ATB_CONFIRM                                           
         BE    *+6                                                              
         DC    H'0'                UNKNOWN SYNC LEVEL                           
         PRNT  SYNC_LEVEL                                                       
         EDIT  TP_NAME_LENGTH,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK                  
         PRNT  TP_NAME_LENGTH                                                   
         MVC   P+30(64),TP_NAME                                                 
         PRNT  TP_NAME                                                          
         MVC   P+30(8),LOCAL_LU_NAME                                            
         PRNT  LOCAL_LU_NAME                                                    
         MVC   P+30(5),=C'BASIC'                                                
         CLC   CONVERSATION_TYPE,ATB_BASIC_CONVERSATION                         
         BE    *+22                                                             
         MVC   P+30(6),=C'MAPPED'                                               
         CLC   CONVERSATION_TYPE,ATB_MAPPED_CONVERSATION                        
         BE    *+6                                                              
         DC    H'0'                UNKNOWN CONVERSATION TYPE                    
         PRNT  CONVERSATION_TYPE                                                
         MVC   P+30(10),USER_ID                                                 
         PRNT  USER_ID                                                          
         MVC   P+30(10),PROFILE                                                 
         PRNT  PROFILE                                                          
         MVC   P+30(80),USER_TOKEN                                              
         PRNT  USER_TOKEN                                                       
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
PRATTB10 PRNT  CONVERSATION_STATE                                               
*                                                                               
         PRNT  *************                                                    
         PRNT  *************                                                    
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
QUERYALQ NMOD1 0,QUERYALQ                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PRINT DETAILS ABOUT THE CURRENT STATE OF THE ALLOCATE QUEUE                   
*                                                                               
         PRNT  **************                                                   
         PRNT  ALLOCATE_QUEUE                                                   
         PRNT  **************                                                   
*                                                                               
         LA    R2,ATBQAQ2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   QUERY ALLOCATE QUEUE                         
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,ALLOCATE_QUEUE_TOKEN,P+30,8,=C'TOG'              
         PRNT  ALLOCATE_Q_TOKEN                                                 
         EDIT  TP_NAME_LENGTH,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK                  
         PRNT  TP_NAME_LENGTH                                                   
         MVC   P+30(64),TP_NAME                                                 
         PRNT  TP_NAME                                                          
         MVC   P+30(8),LOCAL_LU_NAME                                            
         PRNT  LOCAL_LU_NAME                                                    
         EDIT  ALLOCATE_QUEUE_SIZE,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK             
         PRNT  ALLOCATE_Q_SIZE                                                  
         EDIT  ALLOCATE_QUEUE_OLDEST,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK           
         PRNT  ALLOCATE_Q_AGE                                                   
         OC    LAST_REC_ALLOC_ISSUED,LAST_REC_ALLOC_ISSUED                      
         BNZ   *+14                                                             
         MVC   P+30(4),=C'NONE'                                                 
         B     QUALC10                                                          
         GOTO1 =V(HEXOUT),DMCB,LAST_REC_ALLOC_ISSUED,P+30,8,=C'TOG'             
QUALC10  PRNT  LAST_ALLOC_ISSUED                                                
         OC    LAST_REC_ALLOC_RETURNED,LAST_REC_ALLOC_RETURNED                  
         BNZ   *+14                                                             
         MVC   P+30(4),=C'NONE'                                                 
         B     QUALC20                                                          
         GOTO1 =V(HEXOUT),DMCB,LAST_REC_ALLOC_RETURNED,P+30,8,=C'TOG'           
QUALC20  PRNT  LAST_ALLOC_RETURN                                                
*                                                                               
         PRNT  *************                                                    
         PRNT  *************                                                    
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
CALLAPPC NMOD1 0,CALLAPPC                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* UPON ENTRY,  R2 POINTS TO PARAMETER STRUCTURE                                 
* UPON RETURN, RETURN_CODE CONTAINS THE APPC/MVS RETURN CODE                    
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
         L     RF,ASTOPECB         SYNCHRONOUS CALL, SO IT'S DONE               
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
         PRNT  **BAD_APPC_CALL**                                                
         DC    H'0'                APPC/MVS CALL NOT ACCEPTED                   
*                                                                               
CALL20   WAIT  1,ECBLIST=ECBLSTAP  WAIT FOR COMPLETION OR OPERATOR              
*                                                                               
         L     RF,ASTOPECB                                                      
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
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
CALLMQ   NMOD1 0,CALLMQ                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* UPON ENTRY,  R2 POINTS TO PARAMETER STRUCTURE                                 
* UPON RETURN, MQ_COMPCODE CONTAINS THE MQ COMPLETION CODE                      
*              MQ_REASON CONTAINS THE MQ REASON CODE                            
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
         B     CALLMQ30            REASON CODE NOT IN TABLE                     
*                                                                               
         C     R4,0(RF)            FIND MATCH ON REASON CODE                    
         BE    *+12                GOT IT                                       
         LA    RF,28(RF)           BUMP TO NEXT TABLE ENTRY                     
         B     CALLMQ20                                                         
*                                                                               
         MVC   P+54(24),4(RF)                                                   
*                                                                               
CALLMQ30 ST    R3,MQ_COMPCODE      RETURN COMPCODE                              
         ST    R4,MQ_REASON        RETURN REASON                                
*                                                                               
         PRNT                                                                   
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
APPCERR  NMOD1 0,APPCERR                                                        
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PERFORM APPC EXTRACT_ERROR FUNCTION.                                          
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
         PRNT  ERROR_EXTRACT                                                    
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
         SPACE 2                                                                
         LTORG                                                                  
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
COMMWORK DS    0D                  COMMON STORAGE AREA                          
         SPACE 2                                                                
         ATBSERV                                                                
         SPACE 2                                                                
         ATBCTASM                                                               
         SPACE 2                                                                
         CMQA LIST=YES,EQUONLY=NO                                               
         SPACE 2                                                                
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
LUW_ID                         DS    XL26                                       
ALLOCATE_QUEUE_SIZE            DS    F                                          
ALLOCATE_QUEUE_OLDEST          DS    F                                          
LAST_REC_ALLOC_ISSUED          DS    CL8                                        
LAST_REC_ALLOC_RETURNED        DS    CL8                                        
CONVERSATION_ID                DS    CL8                                        
ALLOCATE_QUEUE_TOKEN           DC    XL8'00'                                    
NOTIFY_TYPE                    DS    F                                          
                               DC    A(APPCECB) MUST FOLLOW NOTIFY_TYPE         
TPID                           DC    XL8'00'                                    
RETURN_CODE                    DS    F                                          
REASON_CODE                    DS    F                                          
FILL                           DS    F                                          
LOCKS                          DS    F                                          
CONVERSATION_STATE             DS    F                                          
RECEIVE_LENGTH                 DS    F                                          
RECEIVE_ACCESS_TOKEN           DC    F'0'                                       
STATUS_RECEIVED                DS    F                                          
DATA_RECEIVED                  DS    F                                          
REQUEST_TO_SEND_RECEIVED       DS    F                                          
REQUEST_TO_SEND_VALUE          DS    F                                          
PREP_TO_RECEIVE_TYPE           DS    F                                          
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
         EJECT                                                                  
* MQSERIES CALL PARAMETERS                                                      
*                                                                               
QMGRNAME     DS      CL48      MQ QUEUE MANAGER NAME                            
HCONN        DS      F         MQ QMGR CONNECTION HANDLE                        
HOBJ         DS      F         OBJECT HANDLE                                    
OPEN_OPTIONS DS      F         MQOPEN OPTIONS                                   
CLOSE_OPTIONS DS     F         MQCLOSE OPTIONS                                  
MQ_COMPCODE  DS      F         COMPLETION CODE                                  
MQ_REASON    DS      F         QUALIFIES COMPLETION CODE                        
BUFFERLENGTH DS      F         LENGTH OF MQ BUFFER AREA                         
DATALENGTH   DS      F         LENGTH OF THE MESSAGE                            
OBJDESC      CMQODA  LIST=YES  OBJECT DESCRIPTOR                                
MSGDESC      CMQMDA  LIST=YES  MESSAGE DESCRIPTOR                               
         ORG   MSGDESC_FORMAT                                                   
         DC    CL8'MQSTR   '   MQFMT_STRING  FOR DATA FORMAT                    
         ORG                                                                    
GETMSGOPTS   CMQGMOA LIST=YES  GET MESSAGE OPTIONS                              
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
DUB      DS    D                                                                
PRNTDUB  DS    D                   FOR PRNT MACRO                               
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
THREE    DS    XL3                                                              
PRNTTIME DS    CL9                 FOR PRNT MACRO                               
YYYYMMDD DS    CL8                 DATE                                         
HHMMSS   DS    CL6                 TIME                                         
WORK     DS    CL256                                                            
PACKOF4B DS    PL4                                                              
         SPACE 2                                                                
ARECTAB  DS    A                   A(VALID RECORD TYPES FOR MESSAGE)            
ATSKTAB  DC    A(0)                A(SUBTASK TABLE)                             
DATAMGR  DS    A                   A(DATAMGR)                                   
APPCECB  DC    F'0'                ECB FOR APPC/MVS CALLS                       
INPQECB  DC    F'0'                ECB FOR MQ GET FROM INPUT QUEUE              
TLRCOUNT DS    F                   TRAILER RECORD COUNT                         
MSGCOUNT DS    F                   TOTAL INCOMING RECORD COUNT                  
RECCOUNT DS    F                   RECORD COUNT (W/OUT CONTROL RECORDS)         
ERRRCCNT DS    F                   RECORD COUNT IN ERRNOT                       
BYTECNTR DS    F                   MESSAGE BYTE COUNTER                         
DARE     DC    C'DARE    '         DARE MAJOR ENQ NAME                          
PRTQU    DC    C'PRTQU'            DARE MINOR ENQ NAME                          
THREESEC DC    F'300'              FOR WAITS BETWEEN PRINT QUEUE WRITES         
STIMER1  DS    XL4                 FOR WAITS BETWEEN PRINT QUEUE WRITES         
USERID   DS    XL2                 SENDER'S DDS USERID (NUMERIC)                
SENDER   DS    CL10                SENDER'S DDS USERID (EBCDIC)                 
RCVIDNUM DS    XL2                 RECEIVER'S DDS USERID (NUMERIC)              
RECEIVER DS    CL10                RECEIVER'S DDS USERID (EBCDIC)               
RCVPWRCD DS    CL2                 RECEIVER'S ALPHA/POWER CODE                  
RCVSENUM DS    X                   RECEIVER'S FILE SE NUMBER                    
SVUTL    DS    X                   SAVED UTL+4                                  
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
***WHEN A NEW ERROR CODE #, PLEASE ALSO UPDATE BOOK=SPDARERROR                  
SNDTSKID DS    CL3                 SUBTASK OF WHAT WILL BECOME SENDER           
DESCRIP  DS    CL11                PRINT QUEUE REPORT DESCRIPTION               
PRTQNAME DC    C'PRTQU'                                                         
OPERSTOP DC    C'N'                'Y' = OPERATOR ENTERED 'STOP'                
FIRSTRCV DC    C'N'                'Y' = VERY FIRST MESSAGE RECEIVE             
ENQPQFLG DS    C                   'Y' = WE ENQUEUED THE PRINT QUEUE            
RQSTCONF DS    C                   'Y' = WE GOT REQUEST FOR CONFIRM             
SWTOSEND DS    C                   'Y' = WE SHOULD SWITCH TO SEND MODE          
INBRCVOK DS    C                   'Y' = WE RECEIVED A MESSAGE OK               
INBDRPQ  DS    C                   'Y' = PQ REPORT READY TO BE SENT             
TOOLONG  DS    C                   'Y' = AN INBOUND RECORD WAS TOO LONG         
NULLREC  DS    C                   'Y' = AN INBOUND RECORD WAS NULL             
*                                                                               
FNRCVRFL DS    X                   FLAG FOR FINDRCVR ROUTINE                    
FNRCVRGD EQU   X'80'               X'80' - GOOD, OTHERWISE BAD                  
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
         DS    XL4                 PQ RECORD LENGTH                             
R        DS    CL210               PQ RECORD DATA                               
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
ATBRAL2_STRUCTURE         DS    A                                               
                          DC    CL16'RECEIVE_ALLOCATE'                          
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
                          DC    CL16'REGISTR_4_ALLOCS'                          
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
                          DC    CL16'UNREGISTR_4_ALLC'                          
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'00',AL3(ALLOCATE_QUEUE_TOKEN)                 
                          DC    X'00',AL3(REASON_CODE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBRCVW_STRUCTURE         DS    A                                               
                          DC    CL16'RECEIVE_AND_WAIT'                          
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
                          DC    CL16'SEND_DATA'                                 
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
ATBPTR_STRUCTURE          DS    A                                               
                          DC    CL16'PREP_TO_RECEIVE'                           
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(PREP_TO_RECEIVE_TYPE)                 
                          DC    X'00',AL3(LOCKS)                                
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBCFMD_STRUCTURE         DS    A                                               
                          DC    CL16'ISSUE_CONFIRM'                             
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBGTA2_STRUCTURE         DS    A                                               
                          DC    CL16'GET_ATTRIBUTES'                            
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
                          DC    CL16'QUERY_ALLOCATE_Q'                          
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
                          DC    CL16'ERROR_EXTRACT'                             
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
                          DC    CL16'MQ_CONNECT'                                
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
                          DC    CL16'MQ_OPEN'                                   
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
CSQBGET_STRUCTURE         DS    A                                               
                          DC    CL16'MQ_GET'                                    
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_GET_COMPCODE)                               
                          DC    A(A_GET_REASON)                                 
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ)                                 
                          DC    X'00',AL3(MSGDESC)                              
                          DC    X'00',AL3(GETMSGOPTS)                           
                          DC    X'00',AL3(BUFFERLENGTH)                         
                          DC    X'00',AL3(BUFFER)                               
                          DC    X'00',AL3(DATALENGTH)                           
A_GET_COMPCODE            DC    X'00',AL3(MQ_COMPCODE)                          
A_GET_REASON              DC    X'80',AL3(MQ_REASON)                            
*                                                                               
CSQBBACK_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_BACK'                                   
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_BACK_COMPCODE)                              
                          DC    A(A_BACK_REASON)                                
                          DC    X'00',AL3(HCONN)                                
A_BACK_COMPCODE           DC    X'00',AL3(MQ_COMPCODE)                          
A_BACK_REASON             DC    X'80',AL3(MQ_REASON)                            
*                                                                               
CSQBCOMM_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_COMMIT'                                 
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(A_COMM_COMPCODE)                              
                          DC    A(A_COMM_REASON)                                
                          DC    X'00',AL3(HCONN)                                
A_COMM_COMPCODE           DC    X'00',AL3(MQ_COMPCODE)                          
A_COMM_REASON             DC    X'80',AL3(MQ_REASON)                            
*                                                                               
CSQBCLOS_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_CLOSE'                                  
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
CSQBDISC_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_DISCONNECT'                             
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
ATBCFMD  DC    CL8'ATBCFMD'                                                     
         DC    XL52'00'                                                         
ATBEES3  DC    CL8'ATBEES3'                                                     
         DC    XL52'00'                                                         
ATBGTA2  DC    CL8'ATBGTA2'                                                     
         DC    XL52'00'                                                         
ATBPTR   DC    CL8'ATBPTR'                                                      
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
*                                                                               
ENTRYLSQ EQU   *                                                                
         EJECT                                                                  
DARERECS DS    0D                  VALID DARE RECORDS AND LENGTHS               
         DC    C'AGYHDR',Y(PAGYHDRL)                                            
         DC    C'AGYDS1',Y(PAGYDS1L)                                            
         DC    C'AGYDS2',Y(PAGYDS2L)                                            
         DC    C'AGYDS3',Y(PAGYDS3L)                                            
         DC    C'AGYDS4',Y(PAGYDS4L)                                            
         DC    C'AGYDS5',Y(PAGYDS5L)                                            
         DC    C'AGYSTD',Y(PAGYSTDL)                                            
         DC    C'AGYCOM',Y(PAGYCOML)                                            
         DC    C'VARHDR',Y(PAGYHDRL)                                            
         DC    C'VARDS1',Y(PAGYDS1L)                                            
         DC    C'VARDS2',Y(PAGYDS2L)                                            
         DC    C'VARDS3',Y(PAGYDS3L)                                            
         DC    C'VARDS4',Y(PAGYDS4L)                                            
         DC    C'VARDS5',Y(PAGYDS5L)                                            
         DC    C'VARSTD',Y(PAGYSTDL)                                            
         DC    C'VARCOM',Y(PAGYCOML)                                            
         DC    C'VARPRD',Y(PAGYPROL)                                            
         DC    C'AGYHIA',Y(PAGYHIAL)                                            
         DC    C'AGYCAN',Y(PAGYCANL)                                            
         DC    C'BUYHDR',Y(PBUYHDRL)                                            
         DC    C'BUYDEM',Y(PBUYDEML)                                            
         DC    C'BUYORB',Y(PBUYORBL)                                            
         DC    C'BUYCOM',Y(PBUYCOML)                                            
         DC    C'BUYDTL',Y(PBUYDTLL)                                            
         DC    C'AGYTLR',Y(PAGYTLRL)                                            
         DC    C'VARTLR',Y(PAGYTLRL)                                            
         DC    C'AGYRCL',Y(PARCAPPL)                                            
         DC    C'ERRNOT',Y(MDLNNOTL)                                            
         DC    C'DLNNOT',Y(MDLNNOTL)                                            
         DC    C'ORDAPP',Y(RORDAPPL)                                            
         DC    C'ORDREJ',Y(RORDREJL)                                            
         DC    C'ORDCOM',Y(RORDCOML)                                            
         DC    C'ORDTLR',Y(RORDTLRL)                                            
         DC    C'ORDCFM',Y(RORDCFML)                                            
         DC    C'ORDLIN',Y(RORDLINL)                                            
         DC    C'ORDRCL',Y(RORCAPPL)                                            
         DC    C'MKGHDR',Y(MOFRHDRL)                                            
         DC    C'MKGDS1',Y(MOFRDS1L)                                            
         DC    C'MKGMSS',Y(MOFRMISL)                                            
         DC    C'MKGBUY',Y(MOFRBUYL)                                            
         DC    C'MKGORB',Y(MORDORBL)                                            
         DC    C'MKGCOM',Y(MOFRBCML)                                            
         DC    C'MKGDTL',Y(MOFRBDTL)                                            
         DC    C'MKGTLR',Y(MOFRTLRL)                                            
         DC    C'MKGAPP',Y(MOFRAPPL)                                            
         DC    C'MKGREJ',Y(MOFRREJL)                                            
         DC    C'MKGRCM',Y(MOFRCOML)                                            
         DC    C'MKGROK',Y(MOFRCFML)                                            
         DC    C'MKGCAN',Y(MOFRCANL)                                            
         DC    C'ORDSAL',Y(RORDSALL)                                            
         DC    C'ORDURL',Y(RORDURLL)                                            
         DC    C'ORDMO1',Y(RORDMO1L)                                            
         DC    X'FF'                                                            
         EJECT                                                                  
ORDRECS  DS    0C                  VALID RECORD TYPES IN AN ORDER               
         DC    C'AGYHDR'                                                        
         DC    C'AGYDS1'                                                        
         DC    C'AGYDS2'                                                        
         DC    C'AGYDS3'                                                        
         DC    C'AGYDS4'                                                        
         DC    C'AGYDS5'                                                        
         DC    C'AGYSTD'                                                        
         DC    C'AGYCOM'                                                        
         DC    C'VARHDR'                                                        
         DC    C'VARDS1'                                                        
         DC    C'VARDS2'                                                        
         DC    C'VARDS3'                                                        
         DC    C'VARDS4'                                                        
         DC    C'VARDS5'                                                        
         DC    C'VARSTD'                                                        
         DC    C'VARCOM'                                                        
         DC    C'VARPRD'                                                        
         DC    C'AGYHIA'                                                        
         DC    C'BUYHDR'                                                        
         DC    C'BUYDEM'                                                        
         DC    C'BUYORB'                                                        
         DC    C'BUYCOM'                                                        
         DC    C'BUYDTL'                                                        
         DC    C'AGYTLR'                                                        
         DC    C'VARTLR'                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
ERRRECS  DS    0C                  VALID RECORD TYPES IN AN ERRNOT              
         DC    C'ERRNOT'                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
APPRECS  DS    0C                  VALID RECORD TYPES IN AN APPROVAL            
         DC    C'ORDAPP'                                                        
         DC    C'ORDSAL'                                                        
         DC    C'ORDMO1'                                                        
         DC    C'ORDTLR'                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
REJRECS  DS    0C                  VALID RECORD TYPES IN A REJECTION            
         DC    C'ORDREJ'                                                        
         DC    C'ORDSAL'                                                        
         DC    C'ORDURL'                                                        
         DC    C'ORDCOM'                                                        
         DC    C'ORDLIN'                                                        
         DC    C'ORDTLR'                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
CFMRECS  DS    0C                  VALID RECORD TYPES IN A CONFIRM              
         DC    C'ORDCFM'                                                        
         DC    C'ORDSAL'                                                        
         DC    C'ORDURL'                                                        
         DC    C'ORDCOM'                                                        
         DC    C'ORDLIN'                                                        
         DC    C'ORDTLR'                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
ACNRECS  DS    0C                  VALID RECORD TYPES IN AN AGYCAN              
         DC    C'AGYCAN'                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
ORCRECS  DS    0C                  VALID RECORD TYPES IN AN ORDRCL              
         DC    C'ORDRCL'                                                        
         DC    C'ORDSAL'                                                        
         DC    C'ORDTLR'                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
ARCRECS  DS    0C                  VALID RECORD TYPES IN AN AGYRCL              
         DC    C'AGYRCL'                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
MKGRECS  DS    0C                  VALID RECORD TYPES IN A MAKEGOOD             
         DC    C'MKGHDR'                                                        
         DC    C'MKGDS1'                                                        
         DC    C'MKGMSS'                                                        
         DC    C'MKGBUY'                                                        
         DC    C'MKGORB'                                                        
         DC    C'MKGCOM'                                                        
         DC    C'MKGDTL'                                                        
         DC    C'MKGTLR'                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
MAPRECS  DS    0C                  VALID RECORD TYPES IN MKGD APPROVAL          
         DC    C'MKGAPP'                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
MRJRECS  DS    0C                  VALID RECORD TYPES IN MKGD REJECTION         
         DC    C'MKGREJ'                                                        
         DC    C'MKGRCM'                                                        
         DC    C'MKGTLR'                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
MOKRECS  DS    0C                  VALID RECORD TYPES IN MKGD OK                
         DC    C'MKGROK'                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
MCNRECS  DS    0C                  VALID RECORD TYPES IN MKGD CANCEL            
         DC    C'MKGCAN'                                                        
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE DDDARETAB                                                      
       ++INCLUDE DDMOREPTAB                                                     
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*DAREBUF'                                                      
DAREBUF  DS    (DAREBUFL)X         DARE MESSAGE BUFFER                          
DAREBUFX EQU   *                                                                
DAREBUFL EQU   65535               DARE MESSAGE BUFFER LENGTH                   
*                                                                               
*&&DO                                                                           
*** WHY IS THIS HERE??????  DAREBF2 IS NEVER REFERENCED ANYWHERE                
         DS    0D                                                               
         DC    C'*DAREBF2'                                                      
DAREBF2  DS    (DAREBF2L)X         DARE MESSAGE BUFFER                          
DAREBF2X EQU   *                                                                
DAREBF2L EQU   65535               DARE MESSAGE BUFFER LENGTH                   
*&&                                                                             
         DS    0D                                                               
         DC    C'*CIREC**'                                                      
CIREC    DC    14336X'00'          PRINT QUEUE C/I BUFFER                       
*                                                                               
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
* DMGREQUS                                                                      
* DMPRTQL                                                                       
* DDDPRINT                                                                      
* CTGENFILE                                                                     
       ++INCLUDE DMGREQUS                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
         IEFZB4D2                                                               
         EJECT                                                                  
         IHAPSA                                                                 
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'080DDDARERCV 06/28/10'                                      
         END                                                                    
