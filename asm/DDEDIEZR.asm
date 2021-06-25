*          DATA SET DDEDIEZR   AT LEVEL 105 AS OF 10/10/05                      
*PHASE EDIEZRA                                                                  
*INCLUDE ADDAY                                                                  
*INCLUDE BINSR31                                                                
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE EDISUB                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PERVAL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*                                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        EDIEZR -- RECEIVE AT&T EASYLINK TRANSMISSIONS        *         
*                          VIA APPC/MVS                               *         
*                                                                     *         
*  CALLED BY:    DDEDIEZS (EDICT SENDER TO EASYLINK)                  *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- WORK                                           *         
*                R7 -- PARAMETERS FROM EDIEZS (VIA R1)                *         
*                R8 -- COMMON STORAGE AREA (2ND BASE)                 *         
*                R9 -- S P A R E                                      *         
*                RA -- CPRINT                                         *         
*                RB -- PROGRAM BASE                                   *         
*                RC -- COMMON STORAGE AREA                            *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'DDEDIEZR -- RECEIVE AT&&T EASYLINK XMISSIONS APPC/MVS'          
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
EDIEZR   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*EDIEZR*,=A(R13CHAIN)                                          
*                                                                               
         LR    R7,RC                                                            
         L     RC,=A(COMMWORK)     COMMON STORAGE AREA                          
         LR    R8,RC                                                            
         AH    R8,=H'4096'                                                      
         USING COMMWORK,RC,R8                                                   
*                                                                               
         SH    R7,=H'4'                                                         
         L     R7,0(R7)                                                         
         L     R7,0(R7)            A(R1 PARAMETERS FROM ATTACH)                 
         USING EZRPARMD,R7                                                      
*                                                                               
         MVC   DATAMGR,EZRADMGR    A(DATAMGR)                                   
         MVC   TRACEFLG,EZRTRACE   'Y' = PRINT DETAILED TRACE                   
         ENTRY TRACEFLG                                                         
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         L     RF,=V(PRNTER)       A(SYSPRINT DCB)                              
         MVC   DCBDDNAM-IHADCB(8,RF),=C'EZRTRACE'  DDNAME=EZRTRACE              
*                                                                               
         MVC   TITLE(38),=C'EDICT: RECEIVING SUBTASK FROM EASYLINK'             
         PRNT  START_SUBTASK,PRINT=ALWAYS                                       
*                                                                               
         GOTO1 =A(INITIAL),(RC)    INITIALIZE                                   
*                                                                               
         MVC   TP_NAME_LENGTH,=F'6'                                             
         L     RF,EZRTPNAM                                                      
         MVC   TP_NAME(6),0(RF)                                                 
         L     RF,EZRLUID                                                       
         MVC   LOCAL_LU_NAME,0(RF)                                              
*                                                                               
         LA    R2,ATBRFA2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   REGISTER FOR ALLOCATES                       
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    UNREGSTR            YES                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    RCVALLOC            WE ARE REGISTERED FOR ALLOCATES              
         CLC   RETURN_CODE,ATBCTS_WARNING                                       
         BNE   *+18                                                             
         CLC   REASON_CODE,ATBCTS_ALREADY_REGISTERED                            
         BE    RCVALLOC            WE WERE ALREADY REGISTERED, SO OK            
         B     CANTREG                                                          
         CLC   RETURN_CODE,ATBCTS_REQUEST_UNSUCCESSFUL                          
         BNE   CANTREG                                                          
         CLC   REASON_CODE,ATBCTS_INVAL_LOCAL_LU                                
         BNE   CANTREG             WE CAN'T REGISTER                            
*                                                                               
         MVC   OPMSG1+50(8),LOCAL_LU_NAME                                       
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMSG1,OPMSG1)                     
         ABEND 705,DUMP                                                         
*                                                                               
CANTREG  MVC   P+30(14),=C'REASON CODE = '                                      
         EDIT  REASON_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         PRNT  COULDNT_REGISTER,PRINT=ALWAYS                                    
         DC    H'0'                                                             
*                                                                               
RCVALLOC GOTO1 =A(QUERYALQ),(RC)   QUERY ALLOCATE QUEUE                         
*                                                                               
         MVC   RECEIVE_ALLOCATE_TYPE,ATBCTS_WAIT                                
         LA    R2,ATBRAL2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   RECEIVE ALLOCATE                             
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    UNREGSTR                                                         
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    INCONV              WE HAVE A CONVERSATION                       
         PRNT  NO_CONVERSATION,PRINT=ALWAYS                                     
         ABEND 701,DUMP                                                         
         EJECT                                                                  
INCONV   PRNT  OPENEZRECEIVE,PRINT=ALWAYS                                       
*                                                                               
         GOTO1 =A(QUERYALQ),(RC)   QUERY ALLOCATE QUEUE                         
         GOTO1 =A(PRNTATTB),(RC)   PRINT CONVERSATION ATTRIBUTES                
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),TODAY                                      
*                                                                               
         MVC   WORK(2),TODAY       EBCDIC YEAR                                  
         MVC   WORK+2(4),=C'0331'  START LOOKING AT MARCH 31                    
*                                                                               
DAYLITE5 GOTO1 =V(ADDAY),DMCB,WORK,WORK,F'1'                                    
         GOTO1 =V(GETDAY),DMCB,WORK,WORK+6                                      
         CLI   DMCB,7              IS IT SUNDAY?                                
         BNE   DAYLITE5            NO                                           
*                                                                               
         CLC   TODAY,WORK                                                       
         BL    NEXTMSG             TODAY IS PRIOR TO START OF DST               
*                                                                               
         MVC   WORK(2),TODAY       EBCDIC YEAR                                  
         MVC   WORK+2(4),=C'1101'  START LOOKING AT NOVEMBER 1                  
*                                                                               
DAYLITE7 GOTO1 =V(ADDAY),DMCB,WORK,WORK,F'-1'                                   
         GOTO1 =V(GETDAY),DMCB,WORK,WORK+6                                      
         CLI   DMCB,7              IS IT SUNDAY?                                
         BNE   DAYLITE7            NO                                           
         GOTO1 =V(ADDAY),DMCB,WORK,WORK,F'-1' SATURDAY IS END OF DST            
*                                                                               
         CLC   TODAY,WORK                                                       
         BH    NEXTMSG             TODAY IS AFTER THE END OF DST                
*                                                                               
         MVI   DSTFLAG,C'Y'        IT'S DAYLIGHT SAVING TIME!                   
         PRNT  DAYLIGHTSAVING,PRINT=ALWAYS                                      
         EJECT                                                                  
NEXTMSG  GOTO1 =A(EZLINRCV),(RC)   FILL UP AN INPUT BUFFER                      
         BE    CHKSTOP                                                          
         ABEND 701,DUMP            BAD RETURN CODE FROM SUBROUTINE              
*                                                                               
CHKSTOP  CLI   OPERSTOP,C'Y'       DO WE STOP NOW?                              
         BE    DEALLOC             YES                                          
*                                                                               
         LA    R3,EASYBUF                                                       
*                                                                               
         CLC   =C'ACC',0(R3)       ACCEPT NOTIFICATION?                         
         BNE   CHKREJ              NO                                           
         MVI   DELIVERY,ACCEPTED   YES -- POST MESSAGE ACCEPTED                 
         BRAS  RE,EZDLNCAN                                                      
         B     CONFIRM                                                          
*                                                                               
CHKREJ   CLC   =C'REJ',0(R3)       REJECT NOTIFICATION?                         
         BNE   CHKDLN              NO                                           
         MVI   DELIVERY,REJECTED   YES -- POST MESSAGE REJECTED                 
         BRAS  RE,EZDLNCAN                                                      
         B     CONFIRM                                                          
*                                                                               
CHKDLN   CLC   =C'DLN',0(R3)       DELIVERY NOTIFICATION?                       
         BNE   CHKCAN              NO                                           
         MVI   DELIVERY,DELIVERD   YES -- POST MESSAGE DELIVERED                
         BRAS  RE,EZDLNCAN                                                      
         B     CONFIRM                                                          
*                                                                               
CHKCAN   CLC   =C'CAN',0(R3)       CANCELLATION NOTIFICATION?                   
         BNE   CHKINB              NO                                           
         MVI   DELIVERY,CANCELLD   YES -- POST MESSAGE CANCELLED                
         BRAS  RE,EZDLNCAN                                                      
         B     CONFIRM                                                          
*                                                                               
CHKINB   CLC   =C'INB',0(R3)       INBOUND MESSAGE?                             
         BNE   UNKNOWN             NO -- IGNORE THIS MESSAGE                    
         GOTO1 =A(EZINB),(RC)      YES -- WRITE IT TO PRTQUE                    
         B     CONFIRM             NO MORE TO RECEIVE                           
*                                                                               
UNKNOWN  MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(34),=C'UNKNOWN MESSAGE TYPE FROM EASYLINK'                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
CHKCONF  CLI   RQSTCONF,C'Y'       DID WE GET THE WHOLE MESSAGE?                
         BE    CONFIRM                                                          
         GOTO1 =A(EZLINRCV),(RC)   NO -- RECEIVE IT (AND IGNORE IT!)            
         BE    CHKCONF                                                          
         ABEND 701,DUMP            BAD RETURN CODE FROM SUBROUTINE              
*                                                                               
CONFIRM  CLI   RQSTCONF,C'Y'       CONFIRMATION REQUESTED?                      
         BE    *+6                                                              
         DC    H'0'                NO -- HOW DID WE GET HERE?                   
*                                                                               
         LA    R2,ATBCFMD_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   ISSUE CONFIRMATION                           
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    DEALLOC                                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    NEXTMSG             RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
DEALLOC  GOTO1 =A(CALLDEAL),(RC)   DEALLOCATE/ABEND                             
*                                                                               
UNREGSTR OC    ALLOCATE_QUEUE_TOKEN,ALLOCATE_QUEUE_TOKEN                        
         BZ    GOODBYE             WE NEVER REGISTERED FOR ALLOCATES            
*                                                                               
         LA    R2,ATBURA2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   UNREGISTER FOR ALLOCATES                     
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    GOODBYE                                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GOODBYE  EQU   *                                                                
         LA    R2,CSQBDISC_STRUCTURE                                            
         BRAS  RE,CALLMQ           DISCONNECT FROM MQ QUEUE MANAGER             
         PRNT  EZRECEIVERCLOSED,PRINT=ALWAYS                                    
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
INITIAL  NMOD1 0,INITIAL                                                        
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* INITIALIZE                                                                    
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(20,TODAY8)                                
         MVC   DELPERD+9(8),TODAY8 FOR PERVAL                                   
*                                                                               
         LA    R1,EZRSTOP                                                       
         STCM  R1,7,ASTOPECB+1     SET A(STOPPING ECB)                          
*                                                                               
         MVC   DRTEST,EZRDRTST     DISASTER RECOVERY TEST MODE                  
*                                                                               
         L     RF,EZRMAJNM         A(MAJOR RESOURCE NAME)                       
         MVC   MAJORNAM,0(RF)      MAJOR RESOURCE NAME                          
*                                                                               
         BLDL  0,ENTRYPTS          BUILD LIST OF APPC/MVS ENTRY PTS             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                BAD RETURN FROM BLDL MACRO                   
         LOAD  DE=ATBDEAL                                                       
         ST    R0,ATBDEAL_STRUCTURE                                             
         LOAD  DE=ATBRAL2                                                       
         ST    R0,ATBRAL2_STRUCTURE                                             
         LOAD  DE=ATBRFA2                                                       
         ST    R0,ATBRFA2_STRUCTURE                                             
         LOAD  DE=ATBQAQ2                                                       
         ST    R0,ATBQAQ2_STRUCTURE                                             
         LOAD  DE=ATBGTA2                                                       
         ST    R0,ATBGTA2_STRUCTURE                                             
         LOAD  DE=ATBRCVW                                                       
         ST    R0,ATBRCVW_STRUCTURE                                             
         LOAD  DE=ATBCFMD                                                       
         ST    R0,ATBCFMD_STRUCTURE                                             
         LOAD  DE=ATBURA2                                                       
         ST    R0,ATBURA2_STRUCTURE                                             
         LOAD  DE=ATBEES3                                                       
         ST    R0,ATBEES3_STRUCTURE                                             
*                                                                               
         LOAD  DE=CSQBCONN                                                      
         ST    R0,CSQBCONN_STRUCTURE                                            
         LOAD  DE=CSQBDISC                                                      
         ST    R0,CSQBDISC_STRUCTURE                                            
         LOAD  DE=CSQBPUT1                                                      
         ST    R0,CSQBPUT1_STRUCTURE                                            
*                                                                               
*                                                                               
         LA    R2,CSQBCONN_STRUCTURE                                            
         BRAS  RE,CALLMQ           CONNECT TO MQ QUEUE MANAGER                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  SET UP SOME PUT1 OPTIONS                     
         MVC   OBJDESC_OBJECTTYPE,=A(MQOT_Q)  OBJECT IS A QUEUE                 
         LA    RF,MQPMO_NO_SYNCPOINT                                            
         ICM   RE,15,=AL4(MQPMO_FAIL_IF_QUIESCING)                              
         AR    RF,RE                                                            
         ST    RF,PUTMSGOPTS_OPTIONS                                            
         MVC   MSGDESC_PERSISTENCE,=A(MQPER_PERSISTENT)                         
*                                                                               
         MVC   OBJDESC_OBJECTNAME,=CL48'DDS.BROKER.LOCALQ'                      
         CLI   MAJORNAM+5,C'T'     TEST EDICT?                                  
         BNE   *+10                NO                                           
         MVC   OBJDESC_OBJECTNAME,=CL48'DDS.BROKER.TEST.LOCALQ'                 
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
GETUID   NMOD1 0,GETUID                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* THIS ROUTINE READS AN ID RECORD FROM CTFILE.                                  
*  FIELD 'HALF' CONTAINS THE USERID.                                            
*  ALPHA USERID IS RETURNED IN 'USERID'.                                        
*                                                                               
         PRNT  GETUSERIDRECORD,PRINT=ALWAYS                                     
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         LA    RF,KEY                                                           
         USING CTIREC,RF                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,HALF                                                     
         DROP  RF                                                               
*                                                                               
         LA    R6,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R6)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(6),E,6)  ENQUEUE THE CONTROL FILE                     
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,A(CIREC),0            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                USERID RECORD IS GONE                        
*                                                                               
         LA    R6,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R6)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(6),6)    DEQUEUE THE CONTROL FILE                     
*                                                                               
         L     R4,=A(CIREC)                                                     
         MVI   ELCODE,CTDSCELQ     PICK UP ALPHA USERID                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   USERID,CTDSC-CTDSCD(R4)                                          
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
EZINB    NMOD1 0,EZINB                                                          
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* THIS ROUTINE READS AN EASYLINK INBOUND MESSAGE AND WRITES IT                  
* TO THE RECEIVER'S PRINT QUEUE.                                                
*                                                                               
         PRNT  EASYLINKINBOUND,PRINT=ALWAYS                                     
         MVI   ENQPQFLG,C'N'       ASSUME WE WON'T ENQ PRINT QUEUE              
*                                                                               
         XC    R,R                 PRINT LINE FOR PRTQUE CALLS                  
         LA    R5,R                                                             
         USING PQPLD,R5                                                         
*                                                                               
         LA    R3,5(R3)            BUMP PAST 'INB(CR)(LF)'                      
*                                                                               
         LA    R1,DMCB                                                          
         USING DSTPARMD,R1                                                      
         ST    R3,DSTKEY                                                        
         MVC   DSTCNTRL,MAJORNAM+5                                              
         MVC   DSTTBL,EZRDSTTB                                                  
         MVC   DSTMAJNM,EZRMAJNM                                                
         GOTO1 =V(FINDDEST)                                                     
         BNE   *+14                NOT THERE                                    
         MVC   HALF,DESTUIDN-DESTTABD(R1)  USERID                               
         B     EZINB20                                                          
*                                                                               
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(27),=C'MAILBOX XXXXXXXX IS UNKNOWN'                         
         MVC   P+38(8),0(R3)       PUT MAILBOX NO. IN PRINT LINE                
         GOTO1 =V(PRINTER)                                                      
         B     EZRCVALL            DON'T POST THIS MESSAGE                      
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
         MVC   QLSRCID,HALF        USERID                                       
         MVI   QLEXTRA,X'FF'       OPEN PRINT QUEUE REPORT                      
         MVC   QLSUBID,=C'WU '                                                  
         MVI   QLCLASS,C'K'                                                     
         MVI   QLLINET,X'C0'                                                    
         MVI   QLLINEW,132                                                      
         MVC   QLRETNL,=H'36'                                                   
         MVC   QLRETND,=H'2'                                                    
         MVC   QLDESC,=C'EASYLINK   '                                           
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
         MVI   R,X'89'             FORM FEED                                    
         MVC   R+1(L'R-1),MYSPACES                                              
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
         MVI   R,X'89'             EJECT PAGE EVERY 56 LINES                    
         SR    R6,R6                                                            
         MVC   R+1(L'R-1),MYSPACES                                              
         GOTO1 DATAMGR,DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)             
         CLI   DMCB+8,0                                                         
         BE    EZINB40                                                          
         MVC   P+30(26),=C'COULD NOT WRITE PAGE EJECT'                          
         B     EZCLOPUR            ERROR ON FIRST LINE - PROBABLY DISK          
*                                                                               
EZINB40  LA    R6,1(R6)                                                         
         STH   R6,HALF             INCREMENT NO. OF LINES                       
         MVI   R,X'09'                                                          
         MVC   R+1(L'R-1),MYSPACES                                              
         ST    R3,FULL             START OF TEXT TO BE PRINTED                  
         LA    R4,R+16             START OF PRINT LINE                          
*                                                                               
EZINB50  TRT   0(256,R3),TRINBND   LOOK FOR NON-PRINTING CHARACTERS             
         LR    R6,R1                                                            
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
         MVC   R+1(L'R-1),MYSPACES                                              
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
EZINB90  GOTO1 =A(EZLINRCV),(RC)   GET NEXT BUFFER                              
         BNE   *+16                ERROR                                        
         LA    R3,EASYBUF          CONTINUATION OF MESSAGE                      
         ST    R3,FULL                                                          
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
         DROP  R5                                                               
*                                                                               
EZCLOPUR MVC   P+11(13),=C'*** ERROR ***'                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'CLO/PUR'),=C'PRTQUE',0,R,A(CIREC)             
*                                                                               
EZRCVALL CLI   RQSTCONF,C'Y'       DID WE GET THE WHOLE MESSAGE?                
         BE    EZINBDEQ                                                         
         GOTO1 =A(EZLINRCV),(RC)   NO -- RECEIVE IT (AND IGNORE IT!)            
         BE    EZRCVALL                                                         
         ABEND 701,DUMP            BAD RETURN CODE FROM SUBROUTINE              
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
EZLINRCV NMOD1 0,EZLINRCV                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* RECEIVE RECORDS FROM EASYLINK, AND FILL UP A BUFFER                           
*                                                                               
         MVI   RQSTCONF,C'N'       NO REQUEST FOR CONFIRMATION YET              
         LA    R3,EASYBUF                                                       
         LR    RE,R3                                                            
         LA    RF,EASYBUFL         CLEAR BUFFER                                 
         XCEFL                                                                  
         LA    R5,10               10 RECORDS PER BUFFER MAXIMUM                
*                                                                               
EZRCV10  MVC   FILL,ATB_FILL_LL                                                 
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
*                                                                               
         LA    R2,ATBRCVW_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   RECEIVE AND WAIT                             
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    EZLINXOK            YES                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BNZ   EZLINBAD            BAD RETURN CODE                              
*                                                                               
         XC    EZRSTOP,EZRSTOP                                                  
*                                                                               
         CLC   STATUS_RECEIVED,ATB_CONFIRM_RECEIVED                             
         BNE   EZRCV20                                                          
         MVI   RQSTCONF,C'Y'                                                    
         PRNT  CONFIRMREQUESTED,PRINT=ALWAYS                                    
*                                                                               
EZRCV20  LH    R4,BUFFER_LENGTH                                                 
         SH    R4,=H'2'            MINUS 2 BYTES FOR LENGTH ITSELF              
         LTR   R1,R4                                                            
         BZ    EZRCV30             NO DATA RETURNED                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),BUFFER      PLACE IN MY BUFFER                           
         AR    R3,R4               POINT TO NEXT POSITION IN BUFFER             
         LA    R4,2(R4)            ADD LENGTH (FOR PRNTBL)                      
         CLI   TRACEFLG,C'Y'                                                    
         BNE   EZRCV30                                                          
         GOTO1 =V(PRNTBL),DMCB,0,BUFFER_LENGTH,C'DUMP',(R4),=C'1D'              
                                                                                
EZRCV30  CLI   RQSTCONF,C'Y'       CONFIRMATION REQUESTED?                      
         BNE   *+12                NO                                           
         MVI   0(R3),ETX           END OF MESSAGE MARKER                        
         B     EZLINXOK                                                         
*                                                                               
         BCT   R5,EZRCV10          GET NEXT LINE                                
*                                                                               
         MVI   0(R3),ETB           END OF BUFFER MARKER                         
*                                                                               
EZLINXOK GOTO1 =V(PRNTBL),DMCB,=C'EASYLINK INPUT BUFFER',A(EASYBUF),   +        
               C'DUMP',2000,=C'2D'                                              
*                                                                               
         CR    RB,RB               SET CC EQUAL                                 
         B     EZLINXX                                                          
*                                                                               
EZLINBAD LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
EZLINXX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
PRNTATTB NMOD1 0,PRNTATTB                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PRINT DETAILS ABOUT THE CURRENT STATE OF THE LU6.2 CONVERSATION               
*                                                                               
         PRNT  *************,PRINT=ALWAYS                                       
         PRNT  GETATTRIBUTES,PRINT=ALWAYS                                       
         PRNT  *************,PRINT=ALWAYS                                       
*                                                                               
         LA    R2,ATBGTA2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   GET CONVERSATION ATTRIBUTES                  
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,CONVERSATION_ID,P+30,4,=C'TOG'                   
         PRNT  CONVERSATION_ID,PRINT=ALWAYS                                     
         MVC   P+30(17),PARTNER_LU_NAME                                         
         PRNT  PARTNER_LU_NAME,PRINT=ALWAYS                                     
         MVC   P+30(8),MODE_NAME                                                
         PRNT  MODE_NAME,PRINT=ALWAYS                                           
         MVC   P+30(4),=C'NONE'                                                 
         CLC   SYNC_LEVEL,ATB_NONE                                              
         BE    *+22                                                             
         MVC   P+30(7),=C'CONFIRM'                                              
         CLC   SYNC_LEVEL,ATB_CONFIRM                                           
         BE    *+6                                                              
         DC    H'0'                UNKNOWN SYNC LEVEL                           
         PRNT  SYNC_LEVEL,PRINT=ALWAYS                                          
         EDIT  TP_NAME_LENGTH,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK                  
         PRNT  TP_NAME_LENGTH,PRINT=ALWAYS                                      
         MVC   P+30(64),TP_NAME                                                 
         PRNT  TP_NAME,PRINT=ALWAYS                                             
         MVC   P+30(8),LOCAL_LU_NAME                                            
         PRNT  LOCAL_LU_NAME,PRINT=ALWAYS                                       
         MVC   P+30(5),=C'BASIC'                                                
         CLC   CONVERSATION_TYPE,ATB_BASIC_CONVERSATION                         
         BE    *+22                                                             
         MVC   P+30(6),=C'MAPPED'                                               
         CLC   CONVERSATION_TYPE,ATB_MAPPED_CONVERSATION                        
         BE    *+6                                                              
         DC    H'0'                UNKNOWN CONVERSATION TYPE                    
         PRNT  CONVERSATION_TYPE,PRINT=ALWAYS                                   
         MVC   P+30(10),USER_ID                                                 
         PRNT  USER_ID,PRINT=ALWAYS                                             
         MVC   P+30(10),PROFILE                                                 
         PRNT  PROFILE,PRINT=ALWAYS                                             
         MVC   P+30(80),USER_TOKEN                                              
         PRNT  USER_TOKEN,PRINT=ALWAYS                                          
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
PRATTB10 PRNT  CONVERSATION_STATE,PRINT=ALWAYS                                  
*                                                                               
         PRNT  *************,PRINT=ALWAYS                                       
         PRNT  *************,PRINT=ALWAYS                                       
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
         PRNT  **************,PRINT=ALWAYS                                      
         PRNT  ALLOCATE_QUEUE,PRINT=ALWAYS                                      
         PRNT  **************,PRINT=ALWAYS                                      
*                                                                               
         LA    R2,ATBQAQ2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   QUERY ALLOCATE QUEUE                         
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,ALLOCATE_QUEUE_TOKEN,P+30,8,=C'TOG'              
         PRNT  ALLOCATE_Q_TOKEN,PRINT=ALWAYS                                    
         EDIT  TP_NAME_LENGTH,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK                  
         PRNT  TP_NAME_LENGTH,PRINT=ALWAYS                                      
         MVC   P+30(64),TP_NAME                                                 
         PRNT  TP_NAME,PRINT=ALWAYS                                             
         MVC   P+30(8),LOCAL_LU_NAME                                            
         PRNT  LOCAL_LU_NAME,PRINT=ALWAYS                                       
         EDIT  ALLOCATE_QUEUE_SIZE,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK             
         PRNT  ALLOCATE_Q_SIZE,PRINT=ALWAYS                                     
         EDIT  ALLOCATE_QUEUE_OLDEST,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK           
         PRNT  ALLOCATE_Q_AGE,PRINT=ALWAYS                                      
         OC    LAST_REC_ALLOC_ISSUED,LAST_REC_ALLOC_ISSUED                      
         BNZ   *+14                                                             
         MVC   P+30(4),=C'NONE'                                                 
         B     QUALC10                                                          
         GOTO1 =V(HEXOUT),DMCB,LAST_REC_ALLOC_ISSUED,P+30,8,=C'TOG'             
QUALC10  PRNT  LAST_ALLOC_ISSUED,PRINT=ALWAYS                                   
         OC    LAST_REC_ALLOC_RETURNED,LAST_REC_ALLOC_RETURNED                  
         BNZ   *+14                                                             
         MVC   P+30(4),=C'NONE'                                                 
         B     QUALC20                                                          
         GOTO1 =V(HEXOUT),DMCB,LAST_REC_ALLOC_RETURNED,P+30,8,=C'TOG'           
QUALC20  PRNT  LAST_ALLOC_RETURN,PRINT=ALWAYS                                   
*                                                                               
         PRNT  *************,PRINT=ALWAYS                                       
         PRNT  *************,PRINT=ALWAYS                                       
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
         PRNT  **BAD_APPC_CALL**,PRINT=ALWAYS                                   
         DC    H'0'                APPC/MVS CALL NOT ACCEPTED                   
*                                                                               
CALL20   WAIT  1,ECBLIST=ECBLST    WAIT FOR COMPLETION OR OPERATOR              
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
         PRNT  PRINT=ALWAYS                                                     
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
CALLDEAL NMOD1 0,CALLDEAL                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         LA    R2,ATBDEAL_STRUCTURE                                             
*                                                                               
         MVC   DEALLOCATE_TYPE,ATB_DEALLOCATE_ABEND                             
         MVC   NOTIFY_TYPE_DEALLOCATE,ATB_NOTIFY_TYPE_ECB                       
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
         CLC   RETURN_CODE_DEALLOCATE,ATB_OK                                    
         BE    DEAL10              APPC/MVS CALL WAS ACCEPTED                   
         MVC   P+30(16),4(R2)      PRINT NAME OF APPC/MVS ROUTINE               
         MVC   P+50(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE_DEALLOCATE,(5,P+64),ALIGN=LEFT                       
         PRNT  **BAD_APPC_CALL**,PRINT=ALWAYS                                   
         DC    H'0'                APPC/MVS CALL NOT ACCEPTED                   
*                                                                               
DEAL10   WAIT  ECB=APPCECB_D       WAIT FOR DEALLOCATE/ABEND COMPLETION         
*                                                                               
         MVC   RETURN_CODE_DEALLOCATE,APPCECB_D                                 
         MVI   RETURN_CODE_DEALLOCATE,0       CLEAR HIGH-ORDER BYTE             
         XC    APPCECB_D,APPCECB_D                                              
*                                                                               
         MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODE           
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE_DEALLOCATE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK          
         PRNT  PRINT=ALWAYS                                                     
*                                                                               
         OC    RETURN_CODE_DEALLOCATE,RETURN_CODE_DEALLOCATE                    
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         WAIT  ECB=APPCECB         WAIT FOR COMPLETION OF PREVIOUS CALL         
*                                                                               
         MVC   RETURN_CODE,APPCECB                                              
         MVI   RETURN_CODE,0       CLEAR HIGH-ORDER BYTE                        
         XC    APPCECB,APPCECB                                                  
*                                                                               
         MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
         MVC   P+1(16),=C'OUTSTANDING CALL'                                     
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         PRNT  PRINT=ALWAYS                                                     
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
         PRNT  PRINT=ALWAYS                                                     
*                                                                               
         OC    RETURN_CODE_ERROR_EXTRACT,RETURN_CODE_ERROR_EXTRACT              
         BNZ   APPCEX              BAD RETURN CODE, SO NO ERROR INFO            
*                                                                               
         MVC   P+30(37),=C'APPC SERVICE XXXXXXXX, REASON_CODE = '               
         MVC   P+43(8),SERVICE_NAME                                             
         EDIT  SERVICE_REASON_CODE,(5,P+67),ALIGN=LEFT,ZERO=NOBLANK             
         PRNT  ERROR_EXTRACT,PRINT=ALWAYS                                       
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
EZDLNCAN NTR1  BASE=*,LABEL=*                                                   
         LR    R5,RB                                                            
         AHI   R5,4096                                                          
         USING EZDLNCAN+4096,R5                                                 
*                                                                               
* THIS ROUTINE HANDLES EASYLINK DELIVERY AND CANCELLATION NOTIFICATIONS         
* FIELD 'DELIVERY' INDICATES WHICH ONE IT IS                                    
*                                                                               
         MVC   CUSTREF,MYSPACES                                                 
         LA    R3,5(R3)            BUMP PAST 'DLN(CR)(LF)' TO CUSTREF           
*                                                                               
         LA    RF,CUSTREF                                                       
         SR    R1,R1               COUNT CHARACTERS IN CUSTOMER REF NO.         
EZDLN10  CLC   CRLF,0(R3)                                                       
         BE    EZDLN20                                                          
         MVC   0(1,RF),0(R3)                                                    
         LA    R3,1(R3)                                                         
         LA    RF,1(RF)                                                         
         LA    R1,1(R1)                                                         
         CH    R1,=Y(L'CUSTREF)                                                 
         BH    EZDLN80             REFERENCE NUMBER IS TOO LONG                 
         B     EZDLN10                                                          
*                                                                               
EZDLN20  LA    R3,2(R3)            R3 NOW POINTS TO DATE                        
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
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(12),=C'INVALID DATE'                                        
         GOTO1 =V(PRINTER)                                                      
         B     EZDLN190            IGNORE THIS ONE                              
*                                                                               
EZDLN35  MVC   WORK(2),6(R3)       HH                                           
         MVC   WORK+2(2),9(R3)     MM (SKIP OVER COLON)                         
         GOTO1 =V(HEXIN),DMCB,WORK,DELTIME,4                                    
         CLC   =F'2',DMCB+12                                                    
         BE    EZDLN40                                                          
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(12),=C'INVALID TIME'                                        
         GOTO1 =V(PRINTER)                                                      
         B     EZDLN190            IGNORE THIS ONE                              
*                                                                               
EZDLN40  CLI   DSTFLAG,C'Y'        IS IT DAYLIGHT SAVING TIME?                  
         BNE   EZDLN60                                                          
*                                                                               
         XC    HALF,HALF                                                        
         ZIC   R1,DELTIME          PWOS HOUR                                    
         SLL   R1,4                                                             
         O     R1,=X'0000000C'                                                  
         STH   R1,HALF             PACKED DECIMAL HOUR                          
         AP    HALF,=P'1'          ADD 1 HOUR ADJUSTMENT                        
         CP    HALF,=P'24'         DID WE GO PAST MIDNIGHT?                     
         BNE   EZDLN50                                                          
         ZAP   HALF,=P'0'          YES -- MILITARY HOUR = 0, NOT 24             
         GOTO1 =V(ADDAY),DMCB,DELDATE,WORK,F'1'                                 
         MVC   DELDATE,WORK        CORRECTED DELIVERY/CANCELLATION DATE         
         GOTO1 =V(HEXIN),DMCB,WORK+4,DELDAY,2                                   
         CLC   =F'1',DMCB+12                                                    
         BE    *+6                 DAY NUMBER IS DDS DAY                        
         DC    H'0'                                                             
*                                                                               
EZDLN50  LH    R1,HALF             ADJUSTED PACKED DECIMAL HOUR                 
         SRL   R1,4                                                             
         STC   R1,DELTIME          PWOS HOUR                                    
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
EZDLN65  MVC   EZLEDGER,13(R3)                                                  
*                                                                               
         XC    RLFAXNUM,RLFAXNUM   RESET "SENT TO" FAX NUMBER                   
         XC    ERRCODE,ERRCODE     RESET CANCELLATION ERROR CODE                
         CLI   DELIVERY,CANCELLD   CANCELLATION NOTIFICATION RECEIVED?          
         BE    *+12                                                             
         CLI   DELIVERY,DELIVERD   DELIVERY NOTIFICATION RECEIVED?              
         BNE   EZDLN70                                                          
         LA    R3,24(R3)           POINT THE END OF LEDGER NUMBER               
         CLC   =X'0D25',0(R3)      READ PASS THE FIELD SEPARATOR                
         BE    *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-14                KEEP READ FOR THE FIELD SEPARATOR            
         CLI   DELIVERY,CANCELLD   FOR CANCELLATION                             
         BNE   *+14                                                             
         MVC   ERRCODE,2(R3)       SAVE CANCELLATION ERROR CODE                 
         B     EZDLN73                                                          
         CLI   2(R3),C'0'          NUMBERS?                                     
         BNH   EZDLN73             NO, DON'T SAVE FAX #                         
         MVC   RLFAXNUM,2(R3)      DLN, SAVE "SENT TO" FAX NUMBER               
         B     EZDLN73                                                          
*                                                                               
EZDLN70  CLI   DELIVERY,REJECTED   REJECTION NOTIFICATION RECEIVED?             
         BNE   EZDLN73                                                          
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(31),=C'REJECTION NOTIFICATION RECEIVED'                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P+11(13),=C'*** ERROR ***'                                       
         LA    R3,26(R3)           POINT TO THE REJECTION MESSAGE               
         LA    RF,P+30                                                          
         LA    RE,60               MESSAGE CANNOT EXCESS 60 BYTES               
*                                                                               
EZDLN71  CLC   =X'0D25',0(R3)      END OF THE MESSAGE                           
         BE    EZDLN72                                                          
         MVI   0(RF),R3                                                         
         LA    R3,1(R3)                                                         
         LA    RF,1(RF)                                                         
         BCT   RE,EZDLN71                                                       
*                                                                               
EZDLN72  GOTO1 =V(PRINTER)                                                      
*                                                                               
EZDLN73  CLC   CUSTREF+1(1),MAJORNAM+5  MATCH ON EDICT (ADV/REP)?               
         BNE   EZDLN80             NO -- DLN CAME IN OVER WRONG LINE            
*                                                                               
         CLI   CUSTREF,C'4'        VERSION 4 CUSTOMER REF. NUMBER?              
         BNE   *+16                                                             
         CLI   DRTEST,C'Y'         IS THIS THE DR TEST?                         
         BE    EZDLN80             YES -- THROW AWAY LIVE TRAFFIC               
         B     EZDLN75             NO -- TAKE IT                                
         CLI   CUSTREF,C'D'        GENERATED DURING DR TEST?                    
         BNE   EZDLN80             NO -- OBSOLETE                               
         CLI   DRTEST,C'Y'         IS THIS THE DR TEST?                         
         BNE   EZDLN80             NO -- OBSOLETE                               
*                                                                               
EZDLN75  EQU   *                                                                
         CLC   TODAY8,CUSTREF+10   NEED TO RESET TODAY'S DATE?                  
         BNL   EZDLN76             NO                                           
         GOTO1 =V(DATCON),DMCB,(5,0),(20,TODAY8)                                
         MVC   DELPERD+9(8),TODAY8 FOR PERVAL                                   
*                                                                               
EZDLN76  MVC   DELPERD(8),CUSTREF+10   DATE OF ORIGINAL SEND (YYYYMMDD)         
         GOTO1 =V(PERVAL),DMCB,(17,DELPERD),WORK                                
         CLI   DMCB+4,0                                                         
         BNE   EZDLN80             MAINFRAME DATE MUST BE SCREWED UP!           
         LA    RF,WORK                                                          
         CLC   PVALNDYS-PERVALD(2,RF),=AL2(28)                                  
         BNH   EZDLN90             DLN ISN'T MORE THAN 28 DAYS OLD              
*                                                                               
EZDLN80  MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(33),=C'INVALID CUSTOMER REFERENCE NUMBER'                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLC   CUSTREF+1(1),MAJORNAM+5  MATCH ON EDICT (ADV/REP)?               
         BE    EZDLN190                                                         
         MVC   OPMSG2A,DELIVERY                                                 
         MVC   OPMSG2B,EZLEDGER                                                 
         MVC   OPMSG2C,CUSTREF+2                                                
         GOTO1 DATAMGR,DMCB,=C'OPMSG',('OPMSG2Q',OPMSG2)                        
*                                                                               
         B     EZDLN190            IGNORE THIS ONE                              
*                                                                               
EZDLN90  LA    R1,DMCB                                                          
         USING CONVDSKD,R1                                                      
         MVC   CONVDMGR,DATAMGR    A(DATAMGR)                                   
         MVI   CONVACTN,CONVDSKO   CONVERT REFERENCE NUMBER TO DSKADDR          
         MVC   CONVOFF,=A(CUSTREF+2)                                            
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
         L     R6,EZRXMTTB         A(TRANSMIT TABLE)                            
         USING XMTTABLD,R3                                                      
         LA    R3,XMTENTRY                                                      
*                                                                               
EZDLN100 MVC31 XMTENTRY,0(R6)                                                   
         CLI   0(R3),0             END OF TABLE?                                
         BNE   *+16                                                             
         L     R2,FULL             YES -- USE EDICT FILE DISK ADDRESS           
         MVI   BYTE,C'F'           'F' FOR FILE                                 
         B     EZDLN110                                                         
         CLI   XMTSOURC,XMTSRCPQ   PRINT QUEUE ENTRY?                           
         BNE   *+14                                                             
         CLC   FULL,XMTDSKAD       FIND MATCH ON DISK ADDRESS                   
         BE    *+12                                                             
         AHI   R6,XMTTBLQ          BUMP TO NEXT ENTRY                           
         B     EZDLN100                                                         
         MVC   TABLNTRY,0(R3)      SAVE XMIT TABLE ENTRY                        
         DROP  R3                                                               
*                                                                               
EZDLN110 LA    R6,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R6)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(6),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
*                                                                               
         MVI   EACTION2,0          RESET POSTSENT ACTIONS                       
         MVI   EACTION,0                                                        
         MVC   EXMTTBLE,EZRXMTTB                                                
         ST    R2,ETBLNTRY                                                      
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
         MVI   EACTION2,EACTREJQ   OTHERWISE, REJECTION NOTIFICATION            
*                                                                               
EZDLN115 CLI   BYTE,C'F'           DISK ADDRESS BEING PASSED?                   
         BNE   *+8                                                              
         OI    EACTION,EACTFILQ    YES                                          
         MVC   EEZCANER,ERRCODE    CANCELLATION ERROR CODE                      
         MVC   EMAJORNM,EZRMAJNM                                                
         MVC   EDAYNUM,DELDAY      DAY DELIVERED/CANCELLED                      
         MVC   ETIME,DELTIME       TIME DELIVERED/CANCELLED                     
         MVC   EAEZDTA,=A(EDFDATA)                                              
         MVC   EADTAMGR,DATAMGR                                                 
         MVC   EFILREC,=A(EDICTREC)                                             
         GOTO1 =V(POSTSENT)                                                     
         BNE   EZDLN190            COULD NOT POST EDICT FILE                    
         DROP  R1                                                               
*                                                                               
         MVC   P+30(L'CUSTREF),CUSTREF    CUSTOMER REFERENCE NUMBER             
         CLI   DELIVERY,DELIVERD   DELIVERY NOTIFICATION RECEIVED?              
         BNE   *+14                NO                                           
         MVC   P+60(9),=C'DELIVERED'                                            
         B     EZDLN118                                                         
*                                                                               
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
         BE    *+16                YES                                          
         MVC   P+60(8),=C'REJECTED'                                             
         B     EZDLN118                                                         
         DC    H'0'                UNKNOWN NOTIFICATION                         
*                                                                               
EZDLN118 PRNT  EASYLINK_NOTIFY,PRINT=ALWAYS                                     
*                                                                               
         LA    R2,EDICTREC                                                      
         USING EDFILD,R2                                                        
         TM    EDFFLAGS,EDFDAREZ   WAS THIS A DARE REPORT?                      
         BZ    EZDLN130                                                         
*                                                                               
         MVC   HALF,EDFPQUID                                                    
         GOTO1 =A(GETUID),(RC)     READ ID RECORD FROM CTFILE                   
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
         MVC   RDFXFRID,MYSPACES                                                
         MVC   RDFXFRID(7),EDFDARFX FAX RECORD KEY                              
         MVC   RDFXTOID,USERID     ORIGINATING AGENCY USERID                    
*                                                                               
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
         GOTO1 EZREDADD,DMCB,EZRMAJNM,NEWEDREC,0                                
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
         BE    *+12                YES                                          
         CLI   DELIVERY,CANCELLD   CANCELLATION NOTIFICATION RECEIVED?          
         BNE   EZDLN190            NO                                           
         OC    EDFMQAPP,EDFMQAPP   NEED MQ MSG?                                 
         BZ    EZDLN190            NO                                           
*                                                                               
EZDLN170 EQU   *                                                                
*                                                                               
         MVC   HALF,EDFPQUID                                                    
         GOTO1 =A(GETUID),(RC)     READ ID RECORD FROM CTFILE                   
*                                                                               
         MVI   BUFFER,C' '                                                      
         MVC   BUFFER+1(L'BUFFER-1),BUFFER                                      
         LA    R3,BUFFER                                                        
         USING EZMQMSGD,R3                                                      
         MVC   EZMBKLAB,=CL16'FACPAK=????*****'                                 
         MVC   EZMBKLAB+7(4),EDFMQAPP                                           
         MVC   EZMLABEL,EDFMQLAB                                                
         MVC   EZMDEST,EDFDEST                                                  
         MVC   EZMUSER,USERID                                                   
         MVC   EZMSTAT,DELIVERY                                                 
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
EZDLN174 MVC   EZMEMSG,4(RF)       DISPLAY TEXT                                 
         DROP  R3                                                               
*                                                                               
EZDLN175 EQU   *                                                                
         PRNT  MQ_MESSAGE,PRINT=ALWAYS                                          
         GOTO1 =V(PRNTBL),DMCB,0,BUFFER,C'DUMP',EZMQMSGQ,=C'1D'                 
*                                                                               
         LHI   RE,EZMQMSGQ                                                      
         ST    RE,BUFFERLENGTH                                                  
*                                                                               
         LA    R2,CSQBPUT1_STRUCTURE                                            
         BRAS  RE,CALLMQ           PUT THE MESSAGE TO THE MQ QUEUE              
         BE    EZDLN190                                                         
*                                                                               
         CLC   REASON,=A(MQRC_CONNECTION_BROKEN)                                
         BE    *+6                                                              
         DC    H'0'                MQ CALL FAILED!                              
*                                                                               
         LA    R2,CSQBCONN_STRUCTURE                                            
         BRAS  RE,CALLMQ           RECONNECT TO MQ QUEUE MANAGER                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,CSQBPUT1_STRUCTURE                                            
         BRAS  RE,CALLMQ           PUT THE MESSAGE TO THE MQ QUEUE              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
EZDLN190 CLI   RQSTCONF,C'Y'       DID WE GET THE WHOLE MESSAGE?                
         BE    EZDLNX                                                           
         GOTO1 =A(EZLINRCV),(RC)   NO -- RECEIVE IT (AND IGNORE IT!)            
         BE    EZDLN190                                                         
         ABEND 701,DUMP            BAD RETURN CODE FROM SUBROUTINE              
*                                                                               
EZDLNX   XIT1                                                                   
         DROP  R5                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
CALLMQ   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* UPON ENTRY,  R2 POINTS TO PARAMETER STRUCTURE                                 
* UPON RETURN, COMPCODE CONTAINS THE MQ COMPLETION CODE                         
*              REASON CONTAINS THE MQ REASON CODE                               
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         L     RF,0(R2)            RF = A(MQ ROUTINE)                           
         LA    R3,24(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         MVC   P,SPACES                                                         
         MVI   P,C'+'              '+' MEANS IT'S AN MQ CALL                    
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODES          
         MVC   P+30(12),=C'COMPLETED OK'                                        
         MVC   P+50(L'OBJDESC_OBJECTNAME),OBJDESC_OBJECTNAME                    
         CLC   COMPCODE,=A(MQCC_OK)                                             
         BNE   CMQ05                                                            
*        DC    H'0'                                                             
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
CMQOK    CR    RB,RB               SET CC EQUAL                                 
CMQX     XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
COMMWORK DS    0D                  COMMON STORAGE AREA                          
         SPACE 2                                                                
         GETEL R4,28,ELCODE                                                     
         SPACE 2                                                                
         ATBSERV                                                                
         SPACE 2                                                                
         ATBCTASM                                                               
         SPACE 3                                                                
* APPC/MVS CALL PARAMETERS                                                      
*                                                                               
CONVERSATION_TYPE              DS    F                                          
SYM_DEST_NAME                  DC    CL8' '                                     
PARTNER_LU_NAME                DC    CL17' '                                    
LOCAL_LU_NAME                  DC    CL8' '                                     
MODE_NAME                      DS    CL8                                        
SYNC_LEVEL                     DS    F                                          
CONVERSATION_ID                DS    CL8                                        
CONVERSATION_CORRELATOR        DS    CL8                                        
ALLOCATE_QUEUE_TOKEN           DC    XL8'00'                                    
USER_ID                        DC    CL10' '                                    
PROFILE                        DC    CL10' '                                    
LUW_ID                         DS    XL26                                       
ALLOCATE_QUEUE_SIZE            DS    F                                          
ALLOCATE_QUEUE_OLDEST          DS    F                                          
LAST_REC_ALLOC_ISSUED          DS    CL8                                        
LAST_REC_ALLOC_RETURNED        DS    CL8                                        
USER_TOKEN                     DS    CL80                                       
TP_NAME_LENGTH                 DS    F                                          
TP_NAME                        DC    CL64' '                                    
RECEIVE_ALLOCATE_TYPE          DS    F                                          
TIME_OUT_VALUE                 DC    F'0'                                       
CONVERSATION_STATE             DS    F                                          
NOTIFY_TYPE                    DS    F                                          
                               DC    A(APPCECB) MUST FOLLOW NOTIFY_TYPE         
NOTIFY_TYPE_DEALLOCATE         DS    F                                          
                               DC    A(APPCECB_D)                               
TPID                           DC    XL8'00'                                    
RETURN_CODE                    DS    F                                          
RETURN_CODE_DEALLOCATE         DS    F                                          
REASON_CODE                    DS    F                                          
STATUS_RECEIVED                DS    F                                          
DATA_RECEIVED                  DS    F                                          
FILL                           DS    F                                          
REQUEST_TO_SEND_RECEIVED       DS    F                                          
RECEIVE_LENGTH                 DS    F                                          
RECEIVE_ACCESS_TOKEN           DC    F'0'                                       
BUFFER_LENGTH                  DS    H      (BUFFER MUST FOLLOW THIS)           
BUFFER                         DS    CL256                                      
DEALLOCATE_TYPE                DS    F                                          
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
         CMQA LIST=YES,EQUONLY=NO                                               
* MQSERIES CALL PARAMETERS                                                      
*                                                                               
QMGRNAME     DC      CL48' '               NAME OF QUEUE MANAGER                
HCONN        DS      F                     CONNECTION HANDLE                    
HOBJ         DS      F                     OBJECT HANDLE                        
COMPCODE     DS      F                     COMPLETION CODE                      
REASON       DS      F                     QUALIFIES COMPLETION CODE            
BUFFERLENGTH DS      F                     LENGTH OF DATA IN MQBUFFER           
OBJDESC      CMQODA  DSECT=NO,LIST=YES     OBJECT DESCRIPTOR                    
MSGDESC      CMQMDA  DSECT=NO,LIST=YES     MESSAGE DESCRIPTOR                   
PUTMSGOPTS   CMQPMOA DSECT=NO,LIST=YES     PUT MESSAGE OPTIONS                  
*                                                                               
         EJECT                                                                  
OPMSG1   DC    C'EDICT01 *EASYLINK RECEIVER UNDEFINED LOCAL LUNAME XXXX+        
               XXXX'                                                            
OPMSG2   DS    0C                                                               
         DC    C'AUTONOTE*LCON,RBAD:EZ ERROR, '                                 
OPMSG2A  DC    C'?'                                                             
         DC    C'#'                                                             
OPMSG2B  DC    C'XXXXXXXX'                                                      
         DC    C','                                                             
         DC    C'REF#'                                                          
OPMSG2C  DC    C'NNNNNNNN'                                                      
OPMSG2Q  EQU   *-OPMSG2                                                         
*OPMSG2   DC    C'ERROR! EZ WRONG LINE:  XXXXXXXXXXX'                           
         SPACE 2                                                                
CRLF     DS    0XL2                                                             
CR       DC    X'0D'                                                            
LF       DC    X'25'                                                            
ETB      EQU   X'26'                                                            
ETX      EQU   X'03'                                                            
         SPACE 2                                                                
ECBLST   DS    0F                                                               
ASTOPECB DC    X'00',AL3(0)        A(STOPECB)                                   
AAPPCECB DC    X'80',AL3(APPCECB)  A(APPCECB)                                   
         SPACE 2                                                                
DMWORK   DS    12D                                                              
DMCB     DS    10F                                                              
DUB      DS    D                                                                
DUB2     DS    D                                                                
PRNTDUB  DS    D                   FOR PRNT MACRO                               
FULL     DS    F                                                                
PACKOF4B DS    PL4                                                              
HALF     DS    H                                                                
BYTE     DS    X                                                                
ELCODE   DS    X                                                                
PRNTTIME DS    CL9                 FOR PRNT MACRO                               
WORK     DS    CL256                                                            
MYSPACES DC    256C' '                                                          
EDICTREC DS    CL256                                                            
NEWEDREC DS    CL256                                                            
KEY      DS    XL25                FOR CTFILE READS                             
KEY2     DS    XL48                FOR GENDIR/FILE READS                        
         SPACE 2                                                                
DATAMGR  DS    A                   A(DATAMGR)                                   
APPCECB  DC    F'0'                ECB FOR APPC/MVS CALLS                       
APPCECB_D  DC  F'0'                ECB FOR APPC/MVS DEALLOCATE/ABEND            
TABLNTRY DS    XL(XMTTBLQ)         SAVED XMIT TABLE ENTRY                       
XMTENTRY DS    XL(XMTTBLQ)         TEMP STORAGE FOR XMIT TABLE ENTRY            
MAJORNAM DS    CL8                 MAJOR RESOURCE NAME                          
TODAY    DS    CL6                 EBCDIC DATE TODAY (YYMMDD)                   
TODAY8   DS    CL8                 EBCDIC DATE TODAY (YYYYMMDD)                 
PRTQNAME DS    CL5                 EBCDIC PQ NAME (E.G., C'PRTQ5')              
OPERSTOP DC    C'N'                'Y' = OPERATOR ENTERED 'STOP'                
DSTFLAG  DC    C'N'                'Y' = DAYLIGHT SAVING TIME                   
DRTEST   DS    C                   'Y' = DISASTER RECOVERY TEST MODE            
DELIVERY DS    C                   DELIVERY/CANCELLATION FLAG                   
DELIVERD EQU   C'D'                 DELIVERY NOTIFICATION RECEIVED              
CANCELLD EQU   C'C'                 CANCELLATION NOTIFICATION RECEIVED          
ACCEPTED EQU   C'A'                 ACCEPT NOTIFICATION RECEIVED                
REJECTED EQU   C'R'                 REJECT NOTIFICATION RECEIVED                
ERRCODE  DS    CL4                 EAZYLINK CANCELLATION ERROR CODE             
TRACEFLG DS    C                   'Y' = PRINT DETAILED TRACE                   
ENQPQFLG DS    C                   'Y' = WE ENQUEUED THE PRINT QUEUE            
CUSTREF  DS    CL18                VEDDNNNNNNYYYYMMDD                           
*                                                                               
EDFDATA  DS    0CL21               DATA PASSED FOR UPDATING EDICT REC           
EZLEDGER DS    CL11                EASYLINK LEDGER NUMBER                       
RLFAXNUM DS    CL10                EAZYLINK DLN REDILIST FAX NUMBER             
*                                                                               
USERID   DS    CL10                EBCDIC USERID                                
DELDAY   DS    X                   MESSAGE DELIVERY DAY -- PWOS                 
DELTIME  DS    XL2                 MESSAGE DELIVERY TIME (HM) -- PWOS           
DELPERD  DC    C'YYYYMMDD-YYYYMMDD'  FOR PERVAL: SEND DATE - TODAY              
RQSTCONF DS    C                   'Y' = WE GOT REQUEST FOR CONFIRM             
DELDATE  DS    CL6                 DELIVERY DATE (YYMMDD)                       
         DS    XL4                 PQ RECORD LENGTH                             
R        DS    CL256               PQ RECORD DATA                               
         EJECT                                                                  
* PARAMETER LISTS FOR APPC/MVS CALLS                                            
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
ATBRCVW_STRUCTURE         DS    A                                               
                          DC    CL16'RECEIVE_AND_WAIT'                          
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(FILL)                                 
                          DC    X'00',AL3(RECEIVE_LENGTH)                       
                          DC    X'00',AL3(RECEIVE_ACCESS_TOKEN)                 
                          DC    X'00',AL3(BUFFER_LENGTH)                        
                          DC    X'00',AL3(STATUS_RECEIVED)                      
                          DC    X'00',AL3(DATA_RECEIVED)                        
                          DC    X'00',AL3(REQUEST_TO_SEND_RECEIVED)             
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
ATBDEAL_STRUCTURE         DS    A                                               
                          DC    CL16'DEALLOCATE_ABEND'                          
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(DEALLOCATE_TYPE)                      
                          DC    X'00',AL3(NOTIFY_TYPE_DEALLOCATE)               
                          DC    X'80',AL3(RETURN_CODE_DEALLOCATE)               
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
         EJECT                                                                  
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
CSQBPUT1_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_PUT1'                                   
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(OBJDESC)                              
                          DC    X'00',AL3(MSGDESC)                              
                          DC    X'00',AL3(PUTMSGOPTS)                           
                          DC    X'00',AL3(BUFFERLENGTH)                         
                          DC    X'00',AL3(BUFFER)                               
                          DC    X'00',AL3(COMPCODE)                             
                          DC    X'80',AL3(REASON)                               
         EJECT                                                                  
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
ATBURA2  DC    CL8'ATBURA2'                                                     
         DC    XL52'00'                                                         
*                                                                               
CSQBCONN DC    CL8'CSQBCONN'                                                    
         DC    XL52'00'                                                         
CSQBDISC DC    CL8'CSQBDISC'                                                    
         DC    XL52'00'                                                         
CSQBPUT1 DC    CL8'CSQBPUT1'                                                    
         DC    XL52'00'                                                         
*                                                                               
ENTRYLSQ EQU   *                                                                
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*EASYBUF'                                                      
EASYBUF  DS    (EASYBUFL)X'00'     EASYLINK INPUT BUFFER                        
EASYBUFL EQU   3000                                                             
*                                                                               
         DS    0D                                                               
         DC    C'*CIREC**'                                                      
CIREC    DC    14336X'00'          PRINT QUEUE C/I BUFFER                       
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
         SPACE 3                                                                
       ++INCLUDE DDEDIEZERR                                                     
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
       ++INCLUDE DDEDICTWRK                                                     
         EJECT                                                                  
       ++INCLUDE DDEDICTFIL                                                     
         EJECT                                                                  
       ++INCLUDE SPDARDARED                                                     
         EJECT                                                                  
* DMGREQUS                                                                      
* DMPRTQL                                                                       
* DMPRTQK                                                                       
* DDDPRINT                                                                      
* DDPERVALD                                                                     
* CTGENFILE                                                                     
* CTGENMQDEF                                                                    
       ++INCLUDE DMGREQUS                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE DMPRTQK                                                        
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE CTGENMQDEF                                                     
         EJECT                                                                  
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'105DDEDIEZR  10/10/05'                                      
         END                                                                    
