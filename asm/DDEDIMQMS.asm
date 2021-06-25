*          DATA SET DDEDIMQMS  AT LEVEL 027 AS OF 09/19/08                      
*PHASE EDIMQMSA                                                                 
*INCLUDE BINSR31                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE EDISUB                                                                 
*INCLUDE NUMVAL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE XSORT                                                                  
*                                                                               
* THIS IS A COPY OF EDIMQMS TO SUPPORT THE NEW MVS TO LOTUS FORMAT              
* NOTES2                                                                        
* ^SENDTO^...RECIPIENTS LIST...^/SENDTO^                                        
* ^COPYTO^...CARBON COPY LIST...^/COPYTO^                                       
* ^BLINDCOPYTO^...BLIND COPY LIST...^/BLINDCOPYTO^                              
* ^SUBJECT^...BLIND COPY LIST...^/SUBJECT^                                      
* ^PRINCIPAL^...SENDER'S EMAIL...^/PRINCIPAL^                                   
* ^REPLYTO^...REPLYTOEMAIL...^/REPLYTO^                                         
* ^EMAILTYPE^...EMAILTYPE...^/EMAILTYPE^                                        
* ^BODY^...CONTENT OF EMAIL...^/BODY^                                           
*                                                                               
* ALSO THIS ONE TAKES OUT THE REF# AFTER EACH EMAIL ADDRESS                     
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        EDIMQMS -- TRANSMIT PQ REPORTS VIA LOTUS E-MAIL      *         
*                                                                     *         
*  COMMENTS:     CALLED BY EDICT                                      *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- PARAMETERS FROM EDICT                          *         
*                R6 -- WORK                                           *         
*                R7 -- COMMON STORAGE AREA (2ND BASE)                 *         
*                R8 -- WORK                                           *         
*                R9 -- WORK                                           *         
*                RA -- CPRINT                                         *         
*                RB -- PROGRAM BASE                                   *         
*                RC -- COMMON STORAGE AREA BASE                       *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'DDEDIMQMS -- PERFORM E-MAIL TRANSMISSIONS'                      
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
EDIMQMS  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*EDIMQMS,=A(R13CHAIN)                                          
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
         MVC   TRACEFLG,STRACEON   'Y' = PRINT DETAILED TRACE                   
         ENTRY TRACEFLG                                                         
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         L     RF,=V(PRNTER)       A(SYSPRINT DCB)                              
         MVC   DCBDDNAM-IHADCB(8,RF),=C'MQSTRACE'  DDNAME=MQSTRACE              
*                                                                               
         MVC   TITLE(34),=C'EDICT: SENDING SUBTASK BY E-MAIL'                   
         PRNT  START_SUBTASK,PRINT=ALWAYS                                       
*                                                                               
         L     RF,SMAJORNM                                                      
         MVC   MAJORNAM,0(RF)      MAJOR RESOURCE NAME                          
*                                                                               
         GOTO1 =A(READCRDS)        READ PARAMETER CARDS                         
*                                                                               
         GOTO1 =A(INITIAL)         INITIALIZE                                   
*                                                                               
         LA    R2,CSQBCONN_STRUCTURE                                            
         GOTO1 =A(CALLMQ)          CONNECT TO MQ QUEUE MANAGER                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* RECEIVER CURRENTLY IS NOT USED                                                
*                                                                               
         EJECT                                                                  
LOOP     CLI   OPERSTOP,C'Y'       DOES OPERATOR WANT TO STOP?                  
         BE    STOP                                                             
*                                                                               
         PRNT  ABOUTTOWAIT,PRINT=ALWAYS                                         
*                                                                               
         WAIT  1,ECBLIST=ECBLST    WAIT FOR POST FROM MAIN PROGRAM              
*                                                                               
         L     RF,ASTOPECB                                                      
         TM    0(RF),X'40'         STOP?                                        
         BZ    DONTSTOP            NO                                           
         XC    0(4,RF),0(RF)                                                    
*                                                                               
STOP     LA    R2,CSQBDISC_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     DISCONNECT FROM MQ QUEUE MANAGER             
         PRNT  EXITING,PRINT=ALWAYS                                             
*                                                                               
         XBASE                                                                  
*                                                                               
DONTSTOP L     RF,ALOOKECB                                                      
         TM    0(RF),X'40'         MAIN TASK POSTED READY?                      
         BO    *+6                 YES                                          
         DC    H'0'                                                             
         XC    0(4,RF),0(RF)                                                    
*                                                                               
         GOTO1 =A(XFERREPS)        TRANSMIT ALL E-MAIL REPORTS IN TABLE         
*                                                                               
         B     LOOP                WAIT                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
INITIAL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* INITIALIZE                                                                    
*                                                                               
         PRNT  INITIALIZE,PRINT=ALWAYS                                          
*                                                                               
         L     R1,SLOOKECB         BUILD ECBLIST                                
         STCM  R1,7,ALOOKECB+1                                                  
         L     R1,SSTOPECB                                                      
         STCM  R1,7,ASTOPECB+1                                                  
*                                                                               
         BLDL  0,ENTRYPTS            BUILD LIST OF MQ CALL ENTRY PTS            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                  BAD RETURN FROM BLDL MACRO                 
         LOAD  DE=CSQBCONN                                                      
         ST    R0,CSQBCONN_STRUCTURE                                            
         LOAD  DE=CSQBDISC                                                      
         ST    R0,CSQBDISC_STRUCTURE                                            
         LOAD  DE=CSQBPUT1                                                      
         ST    R0,CSQBPUT1_STRUCTURE                                            
*                                                                               
* ALLOCATE THE SPACE FOR A TABLE THAT WILL CONTAINS THE MULTI                   
* RECIPIENTS OF A REPORT AND THEIR CORRESPONDING EDICT FILE ENTRY               
* ADDRESS.                                                                      
*                                                                               
         LH    R3,SMAXDSTS         MAX # RCP/REP (INCLUDE HDR DST)              
*        BCTR  R3,0                MAX # RCT/REP (EXCLUDE HDR DST)              
*        LA    R3,1(R3)            + 1 REC FOR THE SUBJECT REC                  
         MHI   R3,MAILTABQ         X SIZE OF EACH TABLE ENTRY                   
         LA    R3,9(R3)            ROOM FOR EYE-CATCHER + EOT MARKER            
*                                                                               
         STORAGE OBTAIN,LENGTH=(3) ... RCP'S + A(EDFIL_ENTRY) TABLE             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL STORAGE OBTAIN                  
         MVC   0(8,R1),=C'*MAILTAB'                                             
         LA    R1,8(R1)            BUMP PAST LABEL                              
         ST    R1,AMAILTAB         A(RECIPIENTS TABLE)                          
*                                                                               
* OBTAIN STORAGE FOR E-MAIL BUFFER                                              
*                                                                               
         L     R3,=F'4194304'      MAX MESSAGE SIZE                             
         AHI   R3,8                ROOM FOR EYE-CATCHER                         
*                                                                               
         STORAGE OBTAIN,LENGTH=(3)                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL STORAGE OBTAIN                  
         MVC   0(8,R1),=C'*MQMAIL*'                                             
         LA    R1,8(R1)            BUMP PAST LABEL                              
         ST    R1,AMQMAIL          A(MQ MAIL BUFFER)                            
         A     R1,=F'4194304'                                                   
         ST    R1,AEOMBUFF         A(END OF MQ MAIL BUFFER)                     
*                                                                               
         LA    R1,CSQBPUT1_STRUCTURE                                            
         MVC   44(4,R1),AMQMAIL                                                 
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
READCRDS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* READ PARAMETER CARDS BETWEEN "++MQM" AND "++MQMEND" CARDS                     
*                                                                               
*   *......          "*" IN COLUMN ONE IS A COMMENT CARD                        
*   TRACE=YES        OVERRIDE TRACE FLAG                                        
*   SUBTASK=CCCCCCCC RECEIVING SUBTASK NAME (DEFAULT=EDIMQMR)                   
*   MQQMGRNAME=CL48      MQ QUEUE MANAGER NAME (DEFAULT=' ')                    
*   MQQUEUENAME=CL48     MQ QUEUE NAME (DEFAULT=' ')                            
*   MQCALL=NO        DO ALL MQSERISE CALLS (DEFAULT=YES)                        
*                                                                               
*   USERMAXXMIT=N    MAXIMUM RPTS XMITTED IN A ROW PER USERID (DEF.=50)         
*   USERID=UUUUUUUU  TRANSFER REPORTS FROM THIS USERID ONLY                     
*                                                                               
RC10     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++MQM',CARD      LOOK FOR START OF PARAMETERS                 
         BNE   RC10                                                             
*                                                                               
RC20     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++MQMEND',CARD   LOOK FOR END OF PARAMETERS                   
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
         CLC   =C'SUBTASK=',CARD   SUBTASK=                                     
         BNE   *+14                                                             
         MVC   SUBTASK,CARD+8                                                   
         B     RC20                                                             
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
RC40     CLC   =C'USERMAXXMIT=',CARD  USERMAXXMIT=                              
         BNE   RC50                                                             
         GOTO1 =V(NUMVAL),DMCB,CARD+12,(2,0)                                    
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                INVALID VALUE AFTER EQUALS SIGN              
         MVC   MAXURPTS,DMCB+4                                                  
         B     RC20                                                             
*                                                                               
RC50     CLC   =C'USERID=',CARD    USERID=XXX                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         LA    RF,KEY                                                           
         USING CTIREC,RF                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,CARD+7                                                    
         DROP  RF                                                               
*                                                                               
         LA    R3,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R3)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(3),E,6)  ENQUEUE THE CONTROL FILE                     
*                                                                               
         GOTO1 SADTAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,A(IO),0              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                NO ID RECORD                                 
*                                                                               
         LA    R3,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R3)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(3),6)    DEQUEUE THE CONTROL FILE                     
*                                                                               
         L     R4,=A(IO)                                                        
         MVI   ELCODE,CTDSCELQ     PICK UP HEX USERID                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   USERFILT,CTDSC-CTDSCD(R4)                                        
         B     RC20                                                             
*                                                                               
RCX      GOTO1 =V(PRINTER)         SKIP A LINE                                  
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
XFERREPS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    LSTPQKEY,LSTPQKEY   LAST PRINT QUEUE REPORT SENT                 
         MVI   SKIPLAST,C'N'       DON'T SKIP LAST USERID FOUND (YET)           
*                                                                               
XFER10   LA    R8,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R8)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(8),E,8)  ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         LA    R3,XMTENTRY                                                      
         USING XMTTABLD,R3                                                      
*                                                                               
XFER15   L     R8,SXMTTBLE         R8 = A(REPORT TABLE)                         
         LA    R4,WORK                                                          
         XC    WORK,WORK           WORK = EARLIEST ENTRY --> FIFO               
*                                                                               
XFER20   MVC31 XMTENTRY,0(R8)                                                   
         CLI   XMTTABLD,0          END OF TABLE?                                
         BE    XFER40              YES, NO MORE                                 
         CLI   XMTSOURC,XMTSRCPQ   PRINT QUEUE ENTRY?                           
         BNE   XFER30                                                           
         CLI   XMTMETH,C'M'        YES -- E-MAIL METHOD?                        
         BNE   XFER30                                                           
         TM    XMTSTAT,EDFSTWTG    YES -- WAITING TO BE SENT?                   
         BZ    XFER30                                                           
         TM    XMTSTAT,EDFSTLST    YES -- LAST DEST IN LOGICAL REPORT?          
         BZ    XFER30                                                           
         OC    USERFILT,USERFILT   YES -- IS THERE A USERID FILTER?             
         BZ    *+14                                                             
         CLC   XMTUSRID,USERFILT   YES - DOES THIS REPORT MATCH FILTER?         
         BNE   XFER30                                                           
         CLI   SKIPLAST,C'Y'       YES -- DID WE HIT THE MAX EARLIER?           
         BNE   *+14                                                             
         CLC   XMTPQKEY,LSTPQKEY   YES -- IS THIS THE SAME USERID?              
         BE    XFER30                                                           
*                                                                               
         OC    WORK,WORK           IS THIS THE FIRST ONE WE'VE CHECKED?         
         BZ    *+14                YES, SO SAVE THE DAY/TIME                    
         CLC   XMTCRDTM-XMTTABLD(,R4),XMTCRDTM                                  
         BNH   XFER30                                                           
         MVC   WORK(XMTTBLQ),XMTENTRY   EARLIEST ENTRY FOUND SO FAR             
*                                                                               
XFER30   AHI   R8,XMTTBLQ          BUMP TO NEXT REPORT                          
         B     XFER20                                                           
         DROP  R3                                                               
*                                                                               
XFER40   OC    WORK,WORK           ANYTHING FOUND TO SEND?                      
         BNZ   XFER50              YES                                          
         CLI   SKIPLAST,C'Y'       DID WE MAX OUT ON A USERID?                  
         BNE   XFER140             NO, SO THERE REALLY ARE NO MORE              
*                                                                               
         XC    LSTPQKEY,LSTPQKEY   GET SOME OF THE ONES WE SKIPPED              
         MVI   SKIPLAST,C'N'                                                    
         B     XFER15                                                           
*                                                                               
XFER50   CLC   LSTPQKEY,XMTPQKEY-XMTTABLD(R4)                                   
         BE    *+22                                                             
         MVC   LSTPQKEY,XMTPQKEY-XMTTABLD(R4)                                   
         LA    R6,1                FIRST ONE FOUND                              
         MVI   SKIPLAST,C'N'                                                    
         B     XFER60              SEND IT                                      
*                                                                               
         C     R6,MAXURPTS         ARE WE ABOUT TO EXCEED THE MAXIMUM?          
         BNL   *+12                                                             
         LA    R6,1(R6)            NO -- SEND THE REPORT                        
         B     XFER60                                                           
         MVI   SKIPLAST,C'Y'       YES -- NO MORE FOR THIS USERID               
*                                                                               
XFER60   MVC   TABLNTRY,0(R4)      SAVE XMIT TABLE KEY                          
*                                                                               
         LA    R8,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R8)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(8),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
         LA    R1,DMCB                                                          
         USING PQPARMD,R1                                                       
         MVC   ATBLNTRY,=A(TABLNTRY)                                            
         MVC   APQBUFF,=A(CXREC)                                                
         MVC   APQHDR,=A(PQRPTHDR)                                              
         MVC   AEDHDR,=A(EDICTHDR)                                              
         MVC   APQLINE,=A(R)                                                    
         MVC   ACITABLE,SCITABLE                                                
         MVC   ADTAMGR,SADTAMGR                                                 
         GOTO1 =V(INITPQRD)                                                     
         BE    XFER90                                                           
         DROP  R1                                                               
*                                                                               
         PRNT  CANT_SEND_REPORT,PRINT=ALWAYS                                    
         LA    R2,TABLNTRY         THIS IS THE LAST RECIPIENT ENTRY             
         USING XMTTABLD,R2                                                      
         ZICM  R4,XMTDSTNO,2       LAST RECIPIENT #                             
*                                  MARK ALL RECIPIENT UNSENDABLE                
XFER70   STCM  R4,3,XMTDSTNO                                                    
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         MVC   ETBLNTRY,=A(TABLNTRY)                                            
         MVI   EACTION,EACTJNKQ    MARK REPORT UNSENDABLE                       
         MVI   EERRORCD,EDFERNPQ   REPORT NOT FOUND ON PRINT QUEUE              
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   XFER80                                                           
         DROP  R1,R2                                                            
*                                                                               
         MVC   P+30(14),=C'RECIPIENT NO ='                                      
         EDIT  (R4),(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                            
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
XFER80   BCT   R4,XFER70                                                        
         B     XFER130                                                          
*                                                                               
         USING PQPLD,R4                                                         
XFER90   LA    R4,PQRPTHDR                                                      
         MVC   NUMPAGES,QLPAGES    PQ REPORT PAGES NUMBER                       
*                                                                               
         MVI   BYTE,EDFERIDQ       ASSUME BAD ID/IDI RECORD ERROR               
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         LA    RF,KEY                                                           
         USING CTIREC,RF                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,XMTUSRID-XMTTABLD+TABLNTRY                               
         DROP  RF                                                               
*                                                                               
         LA    R8,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R8)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(8),E,6)  ENQUEUE THE CONTROL FILE                     
*                                                                               
         GOTO1 SADTAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,A(IO),0              
*                                                                               
         LA    R8,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R8)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(8),6)    DEQUEUE THE CONTROL FILE                     
*                                                                               
         CLI   DMCB+8,0                                                         
         BNE   XFER95              USERID RECORD IS GONE                        
*                                                                               
         L     R4,=A(IO)                                                        
         MVI   ELCODE,CTDSCELQ     PICK UP ALPHA USERID                         
         BAS   RE,GETEL                                                         
         BNE   XFER95                                                           
         MVC   USERID,CTDSC-CTDSCD(R4)                                          
*                                                                               
         LA    R1,DMCB                                                          
         USING DSTPARMD,R1                                                      
         MVC   DSTKEY,=A(USERID)                                                
         MVC   DSTTBL,SDSTTBLE                                                  
         MVC   DSTMAJNM,SMAJORNM                                                
         GOTO1 =V(FINDDEST)                                                     
         BE    XFER120                                                          
         DROP  R1                                                               
         MVI   BYTE,EDFERNED       EDICT RECORD WAS DELETED ERROR               
*                                                                               
XFER95   PRNT  CANT_SEND_REPORT,PRINT=ALWAYS                                    
         LA    R2,TABLNTRY         THIS IS THE LAST RECIPIENT ENTRY             
         USING XMTTABLD,R2                                                      
         ZICM  R4,XMTDSTNO,2       LAST RECIPIENT #                             
*                                  MARK ALL RECIPIENT UNSENDABLE                
XFER100  STCM  R4,3,XMTDSTNO                                                    
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         MVC   ETBLNTRY,=A(TABLNTRY)                                            
         MVI   EACTION,EACTJNKQ    MARK REPORT UNSENDABLE                       
         MVC   EERRORCD,BYTE                                                    
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   XFER110                                                          
         DROP  R1,R2                                                            
*                                                                               
         MVC   P+30(14),=C'RECIPIENT NO ='                                      
         EDIT  (R4),(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                            
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
XFER110  BCT   R4,XFER100                                                       
         B     XFER130                                                          
*                                                                               
         USING DESTTABD,R1                                                      
XFER120  LA    R2,TABLNTRY                                                      
         USING XMTTABLD,R2                                                      
         MVC   P+30(8),DESTNAME    PRINT DESTINATION                            
         MVC   P+40(3),XMTSUBID    PRINT PQ SUBID                               
         EDIT  XMTREFNO,(5,P+45),ALIGN=LEFT                                     
         GOTO1 =V(HEXOUT),DMCB,XMTDSKAD,P+53,4,=C'TOG'                          
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         PRNT  XFERONEREPORT,PRINT=ALWAYS                                       
         DROP  R1,R2                                                            
*                                                                               
         GOTO1 =A(SENDMAIL)                                                     
*                                                                               
XFER130  L     RF,ASTOPECB                                                      
         TM    0(RF),X'40'         OPERATOR WANTS TO STOP?                      
         BZ    XFER10              NO                                           
*                                                                               
         XC    0(4,RF),0(RF)                                                    
         MVI   OPERSTOP,C'Y'                                                    
         B     XFERX                                                            
*                                                                               
XFER140  LA    R8,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R8)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(8),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
XFERX    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
SENDMAIL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,TABLNTRY         BUILD TO/BCC/CC RECIPIENTS TABLE             
         USING XMTTABLD,R2                                                      
         MVC   NUMRCPS,XMTDSTNO                                                 
         DROP  R2                                                               
*                                                                               
         L     R3,AMAILTAB                                                      
         USING MAILTABD,R3                                                      
         XC    SUBJECT,SUBJECT     CLEAR THE SUBJECT OF THE E-MAIL              
         XC    REPLYTO,REPLYTO     CLEAR THE REPLY TO E-MAIL                    
         XC    FROM,FROM           CLEAR THE SENDER'S E-MAIL                    
         XC    SNDRNAME,SNDRNAME   CLEAR THE SENDER'S NAME                      
         XC    EMLTYPE,EMLTYPE     CLEAR THE EMAILTYPE                          
         LH    R4,NUMRCPS          NUMBER OF RECIPIENTS                         
         AHI   R4,1                FOR THE SUB                                  
*                                                                               
SNDM10   LA    R1,DMCB                                                          
         USING PQPARMD,R1                                                       
         MVC   ATBLNTRY,=A(TABLNTRY)                                            
         MVC   APQBUFF,=A(CXREC)                                                
         MVC   APQHDR,=A(PQRPTHDR)                                              
         MVC   AEDHDR,=A(EDICTHDR)                                              
         MVC   APQLINE,=A(R)                                                    
         MVC   ACITABLE,SCITABLE                                                
         MVC   ADTAMGR,SADTAMGR                                                 
         DROP  R1                                                               
         GOTO1 =V(PQGETLIN)                                                     
         BE    *+6                 MORE REPORT LINES TO BE READ                 
         DC    H'0'                HOW CAN WE GET HERE?                         
*                                                                               
         CLC   =C'++DDS',R+1       IS THIS LINE A CONTROL CARD?                 
         BNE   SNDM10              NO -- NEXT LINE                              
         CLC   =C'RCP',R+12        IS THIS RECIPIENT CONTROL CARD?              
         BE    SNDM20                                                           
         CLC   =C'CCR',R+12        IS THIS CC CONTROL CARD?                     
         BE    SNDM20                                                           
         CLC   =C'BCC',R+12        IS THIS BCC CONTROL CARD?                    
         BE    SNDM20                                                           
         CLC   =C'RPY',R+12        IS THIS REPLYTO CONTROL CARD?                
         BNE   *+14                                                             
         MVC   REPLYTO,R+16                                                     
         BE    SNDM10                                                           
         CLC   =C'FRM',R+12        IS THIS SENDER'S EMAIL CONTROL CARD?         
         BNE   *+14                                                             
         MVC   FROM,R+16                                                        
         B     SNDM10                                                           
         CLC   =C'SDN',R+12        IS THIS SENDER'S NAME CONTROL CARD?          
         BNE   *+14                                                             
         MVC   SNDRNAME,R+16                                                    
         B     SNDM10                                                           
         CLC   =C'EMT',R+12        IS THIS EMAILTYPE CONTROL CARD?              
         BNE   *+14                NO -- NEXT LINE                              
         MVC   EMLTYPE,R+16        EMAILTYPE OF E-MAIL                          
         B     SNDM10                                                           
         CLC   =C'SUB',R+12        IS THIS SUBJECT CONTROL CARD?                
         BNE   SNDM10              NO -- NEXT LINE                              
         MVC   SUBJECT,R+16        SUBJECT OF E-MAIL                            
         B     SNDM25              CONTINUE TO SEARCH NEXT RECIPIENT            
*                                                                               
SNDM20   MVC   MAILRTYP,R+12       RECIPIENT TYPE (R/C/B)                       
         MVC   MAILRCP,R+16        E-MAIL RECIPIENT                             
         LA    R3,MAILTABQ(R3)                                                  
SNDM25   BCT   R4,SNDM10           CONTINUE TO SEARCH NEXT RECIPIENT            
*                                                                               
         MVI   0(R3),X'FF'         MARK END OF THE MAIL RECIPIENT TABLE         
*                                                                               
* FIND A(EDICTFILE_ENTRY) FOR EACH RECIPIENT IN THE RECIPIENT TABLE             
*                                                                               
         L     R3,AMAILTAB         A(RECIPIENT TABLE)                           
         LH    R4,NUMRCPS          NUMBER OF RECIPIENTS                         
*                                                                               
         USING XMTTABLD,R9                                                      
         LA    R9,XMTENTRY                                                      
*                                                                               
         MVC   XMTENTRY,TABLNTRY   BUILD XMIT KEY FOR 1ST RECIPIENT             
         MVC   XMTDSTNO,=H'1'      PQ RPT LOGICAL REPORT 1ST RECIPIENT          
*                                                                               
         LA    R8,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R8)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(8),E,8)  ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         L     RF,SXMTTBLE                                                      
         AHI   RF,-8                                                            
         L     RF,0(RF)            NUMBER OF ENTRIES IN TABLE                   
         ST    RF,DMCB+8                                                        
         GOTO1 =V(BINSRCH),DMCB,XMTENTRY,SXMTTBLE,,XMTTBLQ,XMTKEYQ,0            
*                                                                               
         ICM   R2,15,DMCB          A(RECORD IN TABLE)                           
         TMH   R2,X'8000'          WAS RECORD FOUND?                            
         BNO   *+6                 YES                                          
         DC    H'0'                NO WAY THAT WILL FAIL!                       
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
SNDM30   MVC31 XMTENTRY,0(R2)                                                   
         MVC   MAILAED,XMTDSKAD    SAVE THE EDICT FILE ENTRY ADDRESS            
         AHI   R2,XMTTBLQ          NEXT RCP (THEY MUST BE TOGETHER)             
         LA    R3,MAILTABQ(R3)     NEXT TABLE ENTRY                             
         BCT   R4,SNDM30                                                        
         DROP  R9                                                               
*                                                                               
         LA    R8,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R8)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(8),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
* SORT RECIPIENT TABLE BY RECIPIENTS IN DESENDING ORDER                         
*                                                                               
         LH    R4,NUMRCPS          NUMBER OF RECIPIENTS                         
         GOTO1 =V(XSORT),DMCB,(1,AMAILTAB),(R4),MAILTABQ,L'MAILRTYP,   +        
               MAILRTYP-MAILTABD                                                
*                                                                               
         L     R3,AMAILTAB         A(RECIPIENT TABLE)                           
         LA    R9,MTYPELST         E-MAIL TYPE LIST                             
         L     R8,AMQMAIL                                                       
         MVC   0(14,R8),=C'NOTES2^SENDTO^'  ALL NOTES BEGIN WITH THIS           
         LA    R8,14(R8)                                                        
*                                                                               
SNDM40   CLC   MAILRTYP,0(R9)      SAME E-MAIL TYPE?                            
         BNE   SNDM50                                                           
*                                                                               
         BCTR  R8,0                                                             
         CLI   0(R8),C'^'          1ST E-MAIL ADDRESS IN THIS FIELD             
         BE    *+12                                                             
         LA    R8,1(R8)                                                         
         MVI   0(R8),C','          E-MAIL ADDRESS DELIMITER                     
         LA    R8,1(R8)                                                         
*                                                                               
         MVC   0(L'MAILRCP,R8),MAILRCP    RECIPIENT E-MAIL ADDRESS              
         LA    R8,L'MAILRCP-1(R8)                                               
*                                  BACKUP TO LAST NON-BLANK CHAR                
         CLI   0(R8),C' '                                                       
         BNE   *+10                                                             
         BCTR  R8,0                                                             
         B     *-10                                                             
         LA    R8,1(R8)                                                         
*                                                                               
         LA    R3,MAILTABQ(R3)     NEXT TABLE ENTRY                             
         B     SNDM40                                                           
*                                                                               
*                                  CLOSE & OPEN TABS                            
SNDM50   CLI   0(R9),C'R'                                                       
         BNE   *+18                                                             
         MVC   0(17,R8),=C'^/SENDTO^^COPYTO^'                                   
         LA    R8,17(R8)                                                        
         B     SNDM52                                                           
         CLI   0(R9),C'C'                                                       
         BNE   *+18                                                             
         MVC   0(22,R8),=C'^/COPYTO^^BLINDCOPYTO^'                              
         LA    R8,22(R8)                                                        
         B     SNDM52                                                           
         CLI   0(R9),C'B'                                                       
         BNE   *+18                                                             
         MVC   0(14,R8),=C'^/BLINDCOPYTO^'                                      
         LA    R8,14(R8)                                                        
         B     SNDM52                                                           
*                                                                               
SNDM52   LA    R9,1(R9)            NEXT E-MAIL TYPE                             
         CLI   0(R9),X'FF'         END OF E-MAIL TYPE LIST?                     
         BNE   SNDM40                                                           
         DROP  R3                                                               
*                                                                               
         OC    SUBJECT,SUBJECT     ANY SUBJECT?                                 
         BZ    SNDM53                                                           
*                                                                               
         MVC   0(9,R8),=C'^SUBJECT^'   REPLY TO OPEN TAG                        
         LA    R8,9(R8)                                                         
*                                  TR CHARS THAT MAY CAUSE PROBLEM              
         L     RF,=A(TRTABLE)                                                   
         TR    SUBJECT,0(RF)                                                    
*                                                                               
         MVC   0(L'SUBJECT,R8),SUBJECT                                          
         LA    R8,L'SUBJECT-1(R8)                                               
*                                                                               
*                                  BACKUP TO LAST NON-BLANK CHAR                
         CLI   0(R8),C' '                                                       
         BNE   *+10                                                             
         BCTR  R8,0                                                             
         B     *-10                                                             
         LA    R8,1(R8)                                                         
*                                                                               
         MVC   0(10,R8),=C'^/SUBJECT^'  SUB CLOSE TAB                           
         LA    R8,10(R8)                                                        
*                                                                               
SNDM53   OC    EMLTYPE,EMLTYPE     ANY EMAILTYPE?                               
         BZ    SNDM55                                                           
*                                                                               
         MVC   0(11,R8),=C'^EMAILTYPE^'   OPEN TAG                              
         AHI   R8,11                                                            
*                                                                               
         MVC   0(L'EMLTYPE,R8),EMLTYPE                                          
         AHI   R8,L'EMLTYPE                                                     
*                                                                               
         MVC   0(12,R8),=C'^/EMAILTYPE^'    CLOSE TAB                           
         AHI   R8,12                                                            
*                                                                               
SNDM55   OC    FROM,FROM           ANY SENDER'S E-MAIL GIVEN?                   
         BZ    SNDM59                                                           
*                                                                               
         MVC   0(11,R8),=C'^PRINCIPAL^'       OPEN TAG                          
         LA    R8,11(R8)                                                        
*                                                                               
         OC    SNDRNAME,SNDRNAME   ANY SENDER'S NAME GIVEN?                     
         BZ    SNDM55J                                                          
*                                                                               
         L     RF,=A(TRTABLE)                                                   
         TR    SNDRNAME,0(RF)                                                   
*                                                                               
         MVI   0(R8),C'"'                                                       
         LA    R8,1(R8)                                                         
*                                                                               
         MVC   0(L'SNDRNAME,R8),SNDRNAME                                        
         LA    R8,L'SNDRNAME-1(R8)                                              
*                                  BACKUP TO LAST NON-BLANK CHAR                
         CLI   0(R8),C' '                                                       
         BNE   *+10                                                             
         BCTR  R8,0                                                             
         B     *-10                                                             
         LA    R8,1(R8)                                                         
*                                                                               
         MVI   0(R8),C'"'                                                       
         LA    R8,1(R8)                                                         
*                                                                               
SNDM55J  MVI   0(R8),C'<'                                                       
         LA    R8,1(R8)                                                         
*                                  TR CHARS THAT MAY CAUSE PROBLEM              
         L     RF,=A(TRTABLE)                                                   
         TR    FROM,0(RF)                                                       
*                                                                               
         MVC   0(L'FROM,R8),FROM                                                
         LA    R8,L'FROM-1(R8)                                                  
*                                  BACKUP TO LAST NON-BLANK CHAR                
         CLI   0(R8),C' '                                                       
         BNE   *+10                                                             
         BCTR  R8,0                                                             
         B     *-10                                                             
         LA    R8,1(R8)                                                         
*                                                                               
         MVC   0(17,R8),=C'@DDS>^/PRINCIPAL^'     CLOSE TAG                     
         LA    R8,17(R8)                                                        
*                                                                               
SNDM59   OC    REPLYTO,REPLYTO     ANY REPLY TO E-MAIL GIVEN?                   
         BZ    SNDM60                                                           
*                                                                               
         MVC   0(9,R8),=C'^REPLYTO^'   REPLY TO OPEN TAG                        
         LA    R8,9(R8)                                                         
*                                                                               
         MVC   0(L'REPLYTO,R8),REPLYTO                                          
         LA    R8,L'REPLYTO-1(R8)                                               
*                                  BACKUP TO LAST NON-BLANK CHAR                
         CLI   0(R8),C' '                                                       
         BNE   *+10                                                             
         BCTR  R8,0                                                             
         B     *-10                                                             
         LA    R8,1(R8)                                                         
*                                                                               
         MVC   0(10,R8),=C'^/REPLYTO^'   REPLY TO CLOSE TAG                     
         LA    R8,10(R8)                                                        
*                                                                               
SNDM60   MVC   0(6,R8),=C'^BODY^'   BODY OPEN TAG                               
         LA    R8,6(R8)                                                         
*                                                                               
         L     R4,AMQMAIL                                                       
         LR    R3,R8                                                            
         SR    R3,R4                                                            
         ST    R3,FULL             LENGTH OF HEADER                             
*                                                                               
         PRNT  MAIL_HEADER,PRINT=ALWAYS                                         
*                                                                               
         SR    R2,R2               PREPARE FOR DIVIDE                           
         LA    RE,80               NUMBER OF CHAR/LINE                          
         DR    R2,RE                                                            
*                                  R2-REMAINDER, R3-QUOTIENT                    
         LTR   R3,R3                                                            
         BZ    SNDM63                                                           
*                                                                               
SNDM62   MVC   P+2(80),0(R4)                                                    
         GOTO1 =V(PRINTER)                                                      
         LA    R4,80(R4)                                                        
         BCT   R3,SNDM62                                                        
*                                                                               
SNDM63   LTR   R2,R2               ANY MORE TEXT TO PRINT?                      
         BZ    SNDM65              NO - SKIP OVER                               
*                                                                               
         BCTR  R2,0                MOVE IN THE LAST LINE                        
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   P+2(0),0(R4)                                                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
SNDM65   PRNT  END_MAIL_HDR,PRINT=ALWAYS                                        
*                                                                               
         LA    R2,79               (MAX 80 CHARACTERS)                          
         CLI   EDICTHDR+35,C'W'    DO WE WANT A WIDE TRANSMISSION?              
         BNE   *+8                                                              
         LA    R2,131              YES (MAX 132 CHARACTERS)                     
*                                                                               
SNDM68   LA    R1,DMCB                                                          
         USING PQPARMD,R1                                                       
         MVC   ATBLNTRY,=A(TABLNTRY)                                            
         MVC   APQBUFF,=A(CXREC)                                                
         MVC   APQHDR,=A(PQRPTHDR)                                              
         MVC   AEDHDR,=A(EDICTHDR)                                              
         MVC   APQLINE,=A(R)                                                    
         MVC   ACITABLE,SCITABLE                                                
         MVC   ADTAMGR,SADTAMGR                                                 
         DROP  R1                                                               
         GOTO1 =V(PQGETLIN)                                                     
         BNE   SNDM70              MORE REPORT LINES TO BE READ                 
*                                                                               
         EX    R2,*+8              MOVE IN LINE                                 
         B     *+10                (FAIL S0C4 MEANS MSG TOO BIG)                
         MVC   0(0,R8),R+1                                                      
         LA    R8,1(R2,R8)                                                      
*                                                                               
         BCTR  R8,0                OMIT THE TAILING BLANKS                      
         CLI   0(R8),C' '                                                       
         BNE   *+8                                                              
         B     *-10                                                             
         LA    R8,1(R8)                                                         
*                                                                               
         MVI   0(R8),X'25'         LINE BREAK                                   
         LA    R8,1(R8)                                                         
*                                                                               
         C     R8,AEOMBUFF                                                      
         BNH   *+6                                                              
         DC    H'0'                EXCESS THE LIMIT OF MAIL BUFFER              
         B     SNDM68                                                           
*                                                                               
SNDM70   MVC   0(7,R8),=C'^/BODY^'   BODY CLOSE TAG                             
         LA    R8,7(R8)                                                         
         C     R8,AEOMBUFF                                                      
         BNH   *+6                                                              
         DC    H'0'                EXCESS THE LIMIT OF MAIL BUFFER              
*                                                                               
         L     R4,AMQMAIL                                                       
         SR    R8,R4                                                            
         ST    R8,BUFFERLENGTH                                                  
*                                                                               
         CLI   TRACEFLG,C'Y'       DETAILED TRACE?                              
         BNE   SNDM78                                                           
*                                                                               
         PRNT  BEGIN_MAIL_DATA                                                  
*                                                                               
         A     R4,FULL             POINT TO THE BODY OF MAIL, NOT HDR           
*                                                                               
         SR    R2,R2               PREPARE FOR DIVIDE                           
         LR    R3,R8               BUFFER LENGTH                                
         S     R3,FULL             BUFFER LENGTH - HDR LENGTH                   
         LA    RE,81               NUMBER OF CHAR/LINE                          
         DR    R2,RE                                                            
*                                  R2-REMAINDER, R3-QUOTIENT                    
         LTR   R3,R3                                                            
         BZ    SNDM73                                                           
*                                                                               
SNDM72   MVC   P+2(81),0(R4)                                                    
         GOTO1 =V(PRINTER)                                                      
         LA    R4,81(R4)                                                        
         BCT   R3,SNDM72                                                        
*                                                                               
SNDM73   LTR   R2,R2               ANY MORE TEXT TO PRINT?                      
         BZ    SNDM75              NO - SKIP OVER                               
*                                                                               
         BCTR  R2,0                MOVE IN THE LAST LINE                        
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   P+2(0),0(R4)                                                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
SNDM75   PRNT  END_MAIL_DATA                                                    
*                                                                               
SNDM78   MVC   OBJDESC_OBJECTTYPE,=A(MQOT_Q)  OBJECT IS A QUEUE                 
*                                                                               
         LA    RF,MQPMO_NO_SYNCPOINT                                            
         ICM   RE,15,=AL4(MQPMO_FAIL_IF_QUIESCING)                              
         AR    RF,RE                                                            
         ST    RF,PUTMSGOPTS_OPTIONS                                            
         MVC   MSGDESC_PERSISTENCE,=A(MQPER_PERSISTENT)                         
*                                                                               
         LA    R2,CSQBPUT1_STRUCTURE                                            
         GOTO1 =A(CALLMQ)          PUT THE MESSAGE TO THE MQ QUEUE              
         BE    SNDM90                                                           
*                                                                               
         CLC   REASON,=A(MQRC_CONNECTION_BROKEN)                                
         BE    *+6                                                              
         DC    H'0'                MQ CALL FAILED!                              
*                                                                               
         LA    R2,CSQBCONN_STRUCTURE                                            
         GOTO1 =A(CALLMQ)          RECONNECT TO MQ QUEUE MANAGER                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,CSQBPUT1_STRUCTURE                                            
         GOTO1 =A(CALLMQ)          PUT THE MESSAGE TO THE MQ QUEUE              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                  MARK ALL DESTINATIONS SENT                   
SNDM90   LA    R2,TABLNTRY         THIS IS THE LAST RECIPIENT ENTRY             
         USING XMTTABLD,R2                                                      
         ZICM  R4,XMTDSTNO,2       LAST RECIPIENT #                             
*                                                                               
SNDM92   STCM  R4,3,XMTDSTNO                                                    
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         MVC   ETBLNTRY,=A(TABLNTRY)                                            
         MVI   EACTION,EACTSNTQ+EACTDLVQ   MARK SENT AND DELIVERED              
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,SADTAMGR                                                
         MVC   ENUMPAGE,NUMPAGES                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   SNDM95                                                           
*                                                                               
         MVC   P+30(14),=C'RECIPIENT NO ='                                      
         EDIT  (R4),(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                            
         PRNT  RPTMARKEDSENT,PRINT=ALWAYS                                       
SNDM95   BCT   R4,SNDM92                                                        
         DROP  R1,R2                                                            
*                                                                               
SNDMX    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
CALLMQ   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* UPON ENTRY,  R2 POINTS TO PARAMETER STRUCTURE                                 
* UPON RETURN, COMPCODE CONTAINS THE MQ COMPLETION CODE                         
*              REASON CONTAINS THE MQ REASON CODE                               
*                                                                               
         CLI   DOMQCALL,C'Y'       DISABLED ALL MQ CALLS?                       
         BNE   CMQOK               YES -- DON'T DO ANY MQ CALLS                 
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
* MORE WORK SHOULD NEED TO LOOK INTO THE REASON CODE,                           
* TO DECIDE WHETHER (1) RESEND OR (2) MARK UNSENDABLE OR (3) DIE.               
*                                                                               
* FOR THE TIME BEING, THE PROGRAM WILL DIE FOR ALL ERRORS.                      
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
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
COMMWORK DS    0D                  COMMON STORAGE AREA                          
         GETEL    R4,28,ELCODE                                                  
         SPACE 2                                                                
DMWORK   DS    12D                                                              
DMCB     DS    10F                                                              
DUB      DS    D                                                                
DUB2     DS    D                                                                
PRNTDUB  DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
PRNTTIME DS    CL9                                                              
WORK     DS    CL256                                                            
         SPACE 2                                                                
ELCODE   DS    X                                                                
AEOMBUFF DS    A                   A(END OF MAIL BUFFER)                        
AMQMAIL  DS    A                   A(MAIL BUFFER)                               
MTYPELST DC    C'RCB',X'FF'        DESENDING ORDER OF E-MAIL TYPE LIST          
USERFILT DC    H'0'                ONLY XMIT REPORTS FOR THIS USERID            
NUMPAGES DS    H                   NUMBER OF PAGES OF THIS PQ REPORT            
NUMRCPS  DS    H                   NUMBER OF RECIPIENT                          
MAXURPTS DC    F'50'               MAX REPORTS PER USERID IN A ROW              
XMTENTRY DS    XL(XMTTBLQ)         TEMP STORAGE FOR 1 XIMTABLE ENTRY            
LSTPQKEY DS    XL(L'XMTPQKEY)      LAST PRINT QUEUE REPORT SENT                 
SKIPLAST DS    C                   'Y' IF WE MAXED OUT ON A USERID              
USERID   DS    CL10                ALPHA USERID                                 
MAJORNAM DS    CL8                 MAJOR RESOURCE NAME                          
SUBJECT  DS    CL60                SUBJECT OF THE E-MAIL                        
EMLTYPE  DS    CL1                 EMAILTYPE                                    
REPLYTO  DS    CL60                REPLY TO E-MAIL                              
FROM     DS    CL60                SENDER'S E-MAIL                              
SNDRNAME DS    CL60                SENDER'S NAME                                
KEY      DS    XL25                CTFILE KEY                                   
         SPACE 2                                                                
ECBLST   DS    0F                                                               
ALOOKECB DC    X'00',AL3(0)        A(LOOKECB)                                   
ASTOPECB DC    X'80',AL3(0)        A(STOPECB)                                   
         SPACE 2                                                                
AMAILTAB DS    A                   A(RECIPIENT TABLE)                           
TABLNTRY DS    XL(XMTTBLQ)         SAVED XMIT TABLE KEY                         
OPERSTOP DC    C'N'                'Y' IF OPERATOR WANTS TO STOP                
TRACEFLG DS    C                   'Y' IF DETAILED TRACE WANTED                 
DOMQCALL DC    C'Y'                'Y' IF DO ALL MQ CALLS                       
SUBTASK  DC    C'EDIMQMR '         RECEIVING SUBTASK NAME                       
CUSTREF  DS    CL18                VEDDNNNNNNYYYYMMDD                           
CARD     DS    CL80                FOR CONTROL CARDS AND EDICTFIL RECS          
         DS    XL4                 PQ RECORD LENGTH                             
R        DS    CL256               PQ RECORD DATA                               
         DS    XL4                                                              
EDICTHDR DS    CL256               EDICT HDR* RECORD                            
         DS    XL4                                                              
PQRPTHDR DS    CL256               PQ REPORT HEADER                             
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
ENTRYPTS DS    0F                                                               
         DC    Y((ENTRYLSQ-ENTRYSTQ)/60)   NUMBER OF TABLE ENTRIES              
         DC    H'60'                       MUST REMAIN AS 60                    
ENTRYSTQ EQU   *                                                                
CSQBCONN DC    CL8'CSQBCONN'                                                    
         DC    XL52'00'                                                         
CSQBDISC DC    CL8'CSQBDISC'                                                    
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
CSQBPUT1_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_PUT1'                                   
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(OBJDESC)                              
                          DC    X'00',AL3(MSGDESC)                              
                          DC    X'00',AL3(PUTMSGOPTS)                           
                          DC    X'00',AL3(BUFFERLENGTH)                         
                          DS    AL4                                             
                          DC    X'00',AL3(COMPCODE)                             
                          DC    X'80',AL3(REASON)                               
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*CXREC**'                                                      
CXREC    DC    14336X'00'          PRINT QUEUE INDEX BUFFER                     
*                                                                               
         DS    0D                                                               
         DC    C'**I/O***'                                                      
IO       DC    2000X'00'           CTFILE/GENFILE I/O BUFFER                    
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
*                                                                               
TRTABLE  DS    0XL256                                                           
*                                                                               
*                0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                               
         DC    X'40404040404040404040404040404040' 00-0F                        
         DC    X'40404040404040404040404040404040' 10-1F                        
         DC    X'40404040404040404040404040404040' 20-2F                        
         DC    X'40404040404040404040404040404040' 30-3F                        
         DC    X'404040404040404040404A4B404D4E4F' 40-4F                        
         DC    X'504040404040404040405A5B5C5D5E40' 50-5F                        
         DC    X'606140404040404040406A406C6D406F' 60-6F                        
         DC    X'404040404040404040797A7B7C7D7E7F' 70-7F                        
         DC    X'40818283848586878889404040404040' 80-8F                        
         DC    X'40919293949596979899404040404040' 90-9F                        
         DC    X'40A1A2A3A4A5A6A7A8A9404040404040' A0-AF                        
         DC    X'40404040404040404040404040404040' B0-BF                        
         DC    X'C0C1C2C3C4C5C6C7C8C9404040404040' C0-CF                        
         DC    X'D0D1D2D3D4D5D6D7D8D9404040404040' D0-DF                        
         DC    X'E040E2E3E4E5E6E7E8E9404040404040' E0-EF                        
         DC    X'F0F1F2F3F4F5F6F7F8F9404040404040' F0-FF                        
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
MAILTABD DSECT                                                                  
MAILRTYP DS    C                   RECIPIENT TYPE (R/C/B - RCP/CC/BCC)          
MAILRCP  DS    CL(MAILADRQ)        E-MAIL ADDRESS                               
MAILAED  DS    XL4                 A(EDICT FILE ENTRY) OF RECIPIENT             
MAILTABQ EQU   *-MAILTABD                                                       
MAILADRQ EQU   60                  ASSUMED MAX LENGTH OF E-MAIL ADDRESS         
         EJECT                                                                  
       ++INCLUDE DDEDICTWRK                                                     
         EJECT                                                                  
* DDDPRINT                                                                      
* DDEDICTFIL                                                                    
* DMPRTQL                                                                       
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DDEDICTFIL                                                     
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027DDEDIMQMS 09/19/08'                                      
         END                                                                    
