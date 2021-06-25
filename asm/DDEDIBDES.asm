*          DATA SET DDEDIBDES  AT LEVEL 016 AS OF 11/20/06                      
*PHASE EDIBDESA                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE BINSR31                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE EDISUB                                                                 
*INCLUDE NUMVAL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE DYNALLOC                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        EDIBDES -- TRANSMIT PQ REPORTS VIA BDE FILE TRANSFER *         
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
         TITLE 'DDEDIBDES -- PERFORM BDE FILE TRANSFER'                         
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
EDIBDES  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*EDIBDES,=A(R13CHAIN)                                          
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
         MVC   DCBDDNAM-IHADCB(8,RF),=C'BDETRACE'  DDNAME=BDETRACE              
*                                                                               
         MVC   TITLE(43),=C'EDICT: SENDING SUBTASK BY BDE FILE TRANSFER+        
               '                                                                
         PRNT  START_SUBTASK,PRINT=ALWAYS                                       
*                                                                               
         L     RF,SMAJORNM                                                      
         MVC   MAJORNAM,0(RF)      MAJOR RESOURCE NAME                          
         MVC   EDICTTYP,SEDCTTYP   EDICT TYPE A/R                               
*                                                                               
         BRAS  RE,READCRDS         READ PARAMETER CARDS                         
*                                                                               
         BRAS  RE,INITIAL          INITIALIZE                                   
*                                                                               
         CLI   ATCHRCVR,C'Y'       ATTACH RECEIVER?                             
         BNE   LOOP                NO                                           
*                                                                               
         LA    R2,SUBTASK                                                       
         ATTACH EPLOC=(R2),ECB=BDERECB,PARAM=BDERPARM,SZERO=NO                  
         ST    R1,BDERTCB                                                       
         OC    BDERTCB,BDERTCB                                                  
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
*                                                                               
         EJECT                                                                  
LOOP     CLI   OPERSTOP,C'Y'       DOES OPERATOR WANT TO STOP?                  
         BE    STOP                                                             
*                                                                               
         PRNT  ABOUTTOWAIT,PRINT=ALWAYS                                         
         WAIT  1,ECBLIST=ECBLST    WAIT FOR POST FROM MAIN PROGRAM              
*                                                                               
         L     RF,ASTOPECB                                                      
         TM    0(RF),X'40'         STOP?                                        
         BZ    DONTSTOP            NO                                           
         XC    0(4,RF),0(RF)                                                    
*                                                                               
STOP     CLI   ATCHRCVR,C'Y'       DID WE ATTACH RECEIVER?                      
         BNE   EXIT                NO                                           
*                                                                               
         PRNT  STOPPINGBDER,PRINT=ALWAYS                                        
         LA    R2,BDERPARM         PARAMETERS TO RECEIVER                       
         USING BDRPARMD,R2                                                      
         POST  BDRSTOP             TELL EDIBDER TO STOP                         
         DROP  R2                                                               
*                                                                               
         WAIT  ECB=BDERECB                                                      
         TM    BDERECB,X'40'                                                    
         BO    *+6                                                              
         DC    H'0'                                                             
         DETACH BDERTCB                                                         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
*                                                                               
EXIT     PRNT  EXITING,PRINT=ALWAYS                                             
         XBASE                                                                  
*                                                                               
DONTSTOP L     RF,ALOOKECB                                                      
         TM    0(RF),X'40'         MAIN TASK POSTED READY?                      
         BO    *+6                 YES                                          
         DC    H'0'                                                             
         XC    0(4,RF),0(RF)                                                    
*                                                                               
         BRAS  RE,XFERREPS         TRANSMIT ALL E-MAIL REPORTS IN TABLE         
         B     LOOP                WAIT                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
INITIAL  NTR1  BASE=*,LABEL=*                                                   
         PRNT  INITIALIZE,PRINT=ALWAYS                                          
*                                                                               
         L     R1,SLOOKECB           BUILD ECBLIST                              
         STCM  R1,7,ALOOKECB+1                                                  
         L     R1,SSTOPECB                                                      
         STCM  R1,7,ASTOPECB+1                                                  
*                                                                               
         LA    R2,BDERPARM         PARAMETERS TO RECEIVER                       
         USING BDRPARMD,R2                                                      
         MVC   BDRXMTTB,SXMTTBLE                                                
         MVC   BDRMAJNM,SMAJORNM                                                
         MVC   BDRADMGR,SADTAMGR                                                
         MVC   BDRQMGNM,=A(RQMGRNAM)                                            
         MVC   BDRMQQNM,=A(RQUENAME)                                            
         MVC   BDRTRACE,TRACEFLG                                                
         MVC   BDREDADD,SEDCTADD                                                
         DROP  R2                                                               
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         L     R1,16           CVT - COMMON VECTOR TABLE                        
         L     R1,544(R1)      CSRTABLE                                         
         L     R1,24(R1)       CSR SLOT                                         
         MVC   BPX1OPN,BPX1OPNQ(R1)     ADDRESS OF THE SERVICE BPX1OPN          
         MVC   BPX1CLO,BPX1CLOQ(R1)     ADDRESS OF THE SERVICE BPX1CLO          
         MVC   BPX1WRT,BPX1WRTQ(R1)     ADDRESS OF THE SERVICE BPX1WRT          
         MVC   BPX1RED,BPX1REDQ(R1)     ADDRESS OF THE SERVICE BPX1RED          
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         BLDL  0,ENTRYPTS                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LOAD  DE=LDTABLE                                                       
         ST    R0,VTABLE                                                        
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
READCRDS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* READ PARAMETER CARDS BETWEEN "++BDE" AND "++BDEEND" CARDS                     
*                                                                               
*   *......          "*" IN COLUMN ONE IS A COMMENT CARD                        
*   TRACE=YES        OVERRIDE TRACE FLAG                                        
*   SUBTASK=CCCCCCCC RECEIVING SUBTASK NAME (DEFAULT=EDIBDER)                   
*   RCVR_MQQMGRNAME=CL48  RECEIVER MQ QUEUE MANAGER NAME (DEFAULT=' ')          
*   RCVR_MQQUEUENAME=CL48 RECEIVER MQ QUEUE NAME (DEFAULT=' ')                  
*   PATH=Cl60        FILE PATH (u/bde1/edicta, u/bde1/edictr)                   
*                                                                               
*   ATTACHRCVR=NO    ATTACH RECEIVING SUBTASK (DEFAULT=YES)                     
*   USERMAXXMIT=N    MAXIMUM RPTS XMITTED IN A ROW PER USERID (DEF.=50)         
*   USERID=UUUUUUUU  TRANSFER REPORTS FROM THIS USERID ONLY                     
*                                                                               
RC10     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++BDE',CARD      LOOK FOR START OF PARAMETERS                 
         BNE   RC10                                                             
*                                                                               
RC20     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++BDEEND',CARD   LOOK FOR END OF PARAMETERS                   
         BE    RC199                                                            
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
         CLC   =C'RCVR_MQQMGRNAME=',CARD  RECEIVER MQQMGRNAME                   
         BNE   *+14                                                             
         MVC   RQMGRNAM,CARD+16                                                 
         B     RC20                                                             
*                                                                               
         CLC   =C'RCVR_MQQUEUENAME=',CARD RECEIVER MQQUEUENAME                  
         BNE   RC25                                                             
         MVC   RQUENAME,CARD+17                                                 
         B     RC20                                                             
*                                                                               
RC25     CLC   =C'TRACE=',CARD     TRACE=                                       
         BNE   RC30                                                             
         CLC   =C'NO',CARD+6                                                    
         BE    RC20                DON'T OVERRIDE                               
         CLC   =C'YES',CARD+6                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRACEFLG,C'Y'       TRACE=YES                                    
         B     RC20                                                             
*                                                                               
RC30     CLC   =C'ATTACHRCVR=',CARD  ATTACH BDE MQ RECEIVER?                    
         BNE   RC40                                                             
         CLC   =C'YES',CARD+11                                                  
         BE    RC20                ATTACH                                       
         CLC   =C'NO',CARD+11                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ATCHRCVR,C'N'       DON'T ATTACH                                 
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
         BNE   RC60                                                             
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
         MVC   DATADISP,=H'28'     FOR CTFILE                                   
         L     R4,=A(IO)                                                        
         MVI   ELCODE,CTDSCELQ     PICK UP HEX USERID                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   USERFILT,CTDSC-CTDSCD(R4)                                        
         B     RC20                                                             
*                                                                               
RC60     CLC   =C'PATH=',CARD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   PATH,CARD+5                                                      
*                                                                               
         LA    RF,PATH                                                          
         LA    RE,L'PATH(RF)                                                    
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         B     *-10                                                             
         LA    RE,1(RE)                                                         
         SR    RE,RF                                                            
         ST    RE,PHLEN                                                         
         B     RC20                                                             
*                                                                               
RC199    OC    PATH,PATH                                                        
         BNZ   *+6                                                              
         DC    H'0'                MUST HAVE PATH NAME                          
*                                                                               
RCX      GOTO1 =V(PRINTER)         SKIP A LINE                                  
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
XFERREPS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    LSTPQKEY,LSTPQKEY   LAST PRINT QUEUE REPORT SENT                 
         MVI   SKIPLAST,C'N'       DON'T SKIP LAST USERID FOUND YET             
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
         CLI   XMTMETH,C'T'        YES -- BDE METHOD?                           
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
         BNE   XFER130             CAN'T POST EDICT FILE                        
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
         B     XFER130                                                          
         DROP  R1                                                               
*                                                                               
         USING PQPLD,R4                                                         
XFER90   LA    R4,PQRPTHDR                                                      
         MVC   NUMPAGES,QLPAGES    PQ REPORT PAGES NUMBER                       
         DROP  R4                                                               
*                                                                               
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
         BNE   XFER110             USERID RECORD IS GONE                        
*                                                                               
         MVC   DATADISP,=H'28'     FOR CTFILE                                   
         L     R4,=A(IO)                                                        
         MVI   ELCODE,CTDSCELQ     PICK UP ALPHA USERID                         
         BAS   RE,GETEL                                                         
         BNE   XFER110                                                          
         MVC   USERID,CTDSC-CTDSCD(R4)                                          
*                                                                               
         L     R4,=A(IO)                                                        
         MVI   ELCODE,CTAGYELQ     GET AGENCY POWERCODE                         
         BAS   RE,GETEL                                                         
         BNE   XFER110                                                          
         MVC   AGYPOWER,CTAGYID-CTAGYD(R4)                                      
         MVC   SVAGYPOW,AGYPOWER                                                
*                                                                               
         L     R4,=A(IO)                                                        
         MVI   ELCODE,CTORGELQ     GET ORIGIN DETAILS                           
         BAS   RE,GETEL                                                         
         BNE   XFER110                                                          
         MVC   AGYNAME,CTORGNAM-CTORGD(R4)                                      
         MVC   AGYADDR,CTORGADD-CTORGD(R4)                                      
*                                                                               
         XC    ROUTCODE,ROUTCODE                                                
         L     R4,=A(IO)                                                        
         MVI   ELCODE,CTUSAELQ     GET US AGENCY EXTRA INFO                     
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   ROUTCODE,CTUSADRC-CTUSAD(R4) DARE ROUTING CODE                   
         B     XFER120                                                          
*                                                                               
XFER110  PRNT  CANT_SEND_REPORT,PRINT=ALWAYS                                    
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         MVC   ETBLNTRY,=A(TABLNTRY)                                            
         MVI   EACTION,EACTJNKQ    MARK REPORT UNSENDABLE                       
         MVI   EERRORCD,EDFERIDQ   BAD ID/IDI RECORD ERROR                      
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   XFER130             CAN'T POST EDICT FILE                        
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
         B     XFER130                                                          
         DROP  R1                                                               
*                                                                               
XFER120  LA    R2,TABLNTRY                                                      
         USING XMTTABLD,R2                                                      
         MVC   P+30(8),USERID      PRINT USER ID                                
         MVC   P+40(3),XMTSUBID    PRINT PQ SUBID                               
         EDIT  XMTREFNO,(5,P+45),ALIGN=LEFT                                     
         MVC   MSG01(20),P+30                                                   
         GOTO1 =V(HEXOUT),DMCB,XMTDSKAD,P+53,4,=C'TOG'                          
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         PRNT  XFERONEREPORT,PRINT=ALWAYS                                       
         DROP  R2                                                               
*                                                                               
         BRAS  RE,SENDMAIL                                                      
         CLI   OPERSTOP,C'Y'                                                    
         BE    XFERX                                                            
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
*                                                                               
SENDMAIL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    DSN,DSN                                                          
         XC    REQUESTR,REQUESTR                                                
         MVC   FLEXT,SPACES                                                     
         MVC   FLEXT(3),=C'htm'                                                 
         MVC   FLNAME,SPACES                                                    
         MVC   FLNAME(4),=CL4'BDE_'                                             
         MVC   FLNAME+4(1),MAJORNAM+5   EdictA/R/T                              
         MVC   FLNAME+5(1),EDICTTYP     EDICT TYPE A/R                          
*                                  FIND THE 1ST ++DDS CARD                      
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
         B     SNDM30                                                           
*                                                                               
SNDM20   LA    R1,DMCB                                                          
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
         BNE   SNDM40              NO                                           
*                                                                               
SNDM30   CLC   =C'RCP',R+12        IS THIS RECIPIENT CONTROL CARD?              
         BNE   SNDM32                                                           
         MVC   EMAILADR,R+16                                                    
         B     SNDM20                                                           
*                                                                               
SNDM32   CLC   =C'SUB',R+12        IS THIS SUBJECT CONTROL CARD?                
         BNE   SNDM34              NO -- NEXT LINE                              
         MVC   SUBJECT,R+16        SUBJECT OF E-MAIL                            
         B     SNDM20              NEXT LINE                                    
*                                                                               
SNDM34   CLC   =C'FIL',R+12        IS THIS FILE NAME CARD?                      
         BNE   SNDM35              NO -- NEXT LINE                              
         MVC   FLNAME,R+16         FILE NAME                                    
         B     SNDM20              NEXT LINE                                    
*                                                                               
SNDM35   CLC   =C'EXT',R+12        IS THIS FILE TYPE EXTENSION CARD?            
         BNE   SNDM37              NO -- NEXT LINE                              
         MVC   FLEXT,R+16          FILE TYPE EXTENSION                          
         B     SNDM20              NEXT LINE                                    
*                                                                               
SNDM37   CLC   =C'DSN',R+12        IS DATA SET NAME?                            
         BNE   SNDM38              NO -- NEXT LINE                              
         MVC   DSN,R+16                                                         
         B     SNDM20              NEXT LINE                                    
*                                                                               
SNDM38   CLC   =C'TRN',R+12                                                     
         BNE   SNDM39              NO -- NEXT LINE                              
         MVC   REQUESTR,R+36                                                    
         B     SNDM20              NEXT LINE                                    
*                                                                               
SNDM39   CLC   =C'DXC',R+12                                                     
         BNE   SNDM20              NO -- NEXT LINE                              
         MVC   FAXSUBC,R+16                                                     
         B     SNDM20              NEXT LINE                                    
*                                                                               
SNDM40   BRAS  RE,BLDDATF          BUILD UNCONVERTED DATA FILE                  
         BRAS  RE,BLDREF           BUILD CUSTOMER REF #                         
         BRAS  RE,BLDFILN          BUILD FILENAME WITH EXTENSION                
*                                                                               
* BUILD COMMAND FILE AND INVOKE USS TO TRANSFER DATA FILE TO BDE SERVER         
*                                                                               
         BRAS  RE,BLDCMDF                                                       
         BRAS  RE,BATCH                                                         
         BRAS  RE,CHKRC                                                         
*                                                                               
         CLC   =C'RC=START',WORK                                                
         BNE   SNDM100                                                          
         GOTO1 SADTAMGR,DMCB,=C'OPMSG',=C'AUTONOTE*YYUN,FROE:BDE JAVA S+        
               CRIPT FAILED, PLEASE CHECK SYSOUT FOR ERROR'                     
         GOTO1 =V(LOGIO),DMCB,1,=C'BDE CUI unable to communicate succes+        
               sfully with the BDE server.  Call LAN dept.'                     
         GOTO1 =V(LOGIO),DMCB,0,(1,BYTE)          WTOR MESSAGE                  
*                                                                               
         BRAS  RE,PSTDOUT                                                       
         MVI   OPERSTOP,C'Y'                                                    
         B     SNDMX                                                            
*                                                                               
*        CLI   BYTE,C'D'           DUMP?                                        
*        BNE   SNDMX               RETRY AGAIN                                  
*        DC    H'0'                                                             
*                                                                               
SNDM100  CLC   =C'RC=COMPLETED',WORK                                            
         BNE   SNDM150                                                          
*                                                                               
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         MVC   ETBLNTRY,=A(TABLNTRY)                                            
         MVI   EACTION,EACTSNTQ            MARK SENT                            
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,SADTAMGR                                                
         MVC   ENUMPAGE,NUMPAGES                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   SNDMX                                                            
         MVC   P+30(32),WORK                                                    
         PRNT  RPTMARKEDSENT,PRINT=ALWAYS                                       
         DROP  R1                                                               
         B     SNDMX                                                            
*                                                                               
SNDM150  MVC   P+30(32),WORK                                                    
         PRNT  CANT_SEND_REPORT,PRINT=ALWAYS                                    
         MVI   ERRCODE,EDFERNUQ    UNKNOWN BDE USER ERROR                       
         CLC   =C'RC=ERROR',WORK                                                
         BE    SNDM160                                                          
*                                  "RC=FAILED"                                  
         MVI   ERRCODE,EDFERNDQ    NO DOC ERROR/ICONV ERROR                     
         MVC   WORK2,SPACES                                                     
         MVC   WORK2(12),=CL12'BDE REPORT: '                                    
         MVC   WORK2+12(L'MSG01),MSG01                                          
         MVC   WORK2+12+L'MSG01+1(61),=CL61'CAN''T BE SENT!  REPLY: (R)+        
               TO RETRY, (S) TO SKIP, (P) TO STOP'                              
         GOTO1 =V(LOGIO),DMCB,1,(100,WORK2)                                     
         GOTO1 =V(LOGIO),DMCB,0,(1,BYTE)          WTOR MESSAGE                  
*                                                                               
         CLI   BYTE,C'S'           SKIP?                                        
         BE    SNDM160                                                          
*                                                                               
         CLI   BYTE,C'P'           STOP?                                        
         BNE   *+12                                                             
         MVI   OPERSTOP,C'Y'                                                    
         B     SNDMX                                                            
*                                                                               
         CLI   BYTE,C'R'           RETRY?                                       
         BE    SNDMX                                                            
         B     SNDMX               ASSUME RETRY WHEN INVALID REPLY              
*                                                                               
SNDM160  LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EERRORCD,ERRCODE                                                 
         MVC   EXMTTBLE,SXMTTBLE                                                
         MVC   ETBLNTRY,=A(TABLNTRY)                                            
         MVI   EACTION,EACTJNKQ    MARK REPORT UNSENDABLE                       
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EFILREC,=A(EDICTREC)     GET EDICT REC IN RETURN                 
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   SNDMX               CAN'T POST EDICT FILE                        
         MVC   P+30(32),WORK                                                    
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
         DROP  R1                                                               
*                                                                               
         BRAS  RE,ERRDFAX          ADD ERRFAX IF DAREFAX                        
*                                                                               
SNDMX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
BLDDATF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   EDICTHDR+38,C'D'    IS A DSN SUPPLIED?                           
         BNE   BDAT20                                                           
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(X'FF',=CL8'INFILE'),DSN                       
         MVC   INFILE(DCBLNGQS),NULLFILE                                        
         OPEN  (INFILE,INPUT)                                                   
*                                                                               
BDAT20   MVC   DATAFILE(40),=C'/tmp/BDE_??/unconverted_EBCDIC_data_BDE_+        
               '                                                                
         MVC   DATAFILE+9(1),MAJORNAM+5        EdictA/R/T                       
         MVC   DATAFILE+10(1),EDICTTYP         EDICT TYPE A/R                   
         MVC   DATAFILE+40(1),MAJORNAM+5       EdictA/R/T                       
         MVC   DATAFILE+41(1),EDICTTYP         EDICT TYPE A/R                   
         LA    RE,42                                                            
         ST    RE,DFLEN                                                         
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         XC    S_MODE,S_MODE                                                    
         MVI   S_MODE2,S_IRUSR       USER READ/WRITE, GROUP READ,               
         MVI   S_MODE3,S_IWUSR+S_IRGRP+S_IROTH           OTHER READ             
         XC    O_FLAGS(OPNF#LENGTH),O_FLAGS                                     
         MVI   O_FLAGS4,O_CREAT+O_RDWR+O_TRUNC                                  
         L     RF,BPX1OPN                                                       
         CALL  (15),                 OPEN A FILE  BPX1OPN              X        
               (DFLEN,               INPUT: PATHNAME LENGTH            X        
               DATAFILE,             INPUT: PATHNAME                   X        
               O_FLAGS,              INPUT: ACCESS            BPXYOPNF X        
               S_MODE,               INPUT: MODE    BPXYMODE, BPXYFTYP X        
               RETVAL,               RETURN VALUE:-1 OR FILE DESCRIPTO X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL   TEST RETVAL           BL    PSEUD            
         BNL   *+6                                                              
         DC    H'0'                                                             
         ST    RF,FILEDESC         SAVE FILE DESCRIPTOR                         
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         MVI   BYTE,X'FF'          FOR NEW DOC BEGINNING HTML CODE              
         BRAS  RE,CHKCC                                                         
*                                                                               
         LA    R2,79               (MAX 80 CHARACTERS)                          
         CLI   EDICTHDR+35,C'W'    DO WE WANT A WIDE TRANSMISSION?              
         BNE   *+8                                                              
         LA    R2,131              YES (MAX 132 CHARACTERS)                     
*                                                                               
         BRAS  RE,COVERPAG                                                      
*                                                                               
         CLI   EDICTHDR+38,C'D'    IS A DSN SUPPLIED?                           
         BNE   BDAT50              ALREADY GOT REC FOR 1ST ROUND                
         B     BDAT40                                                           
*                                                                               
BDAT30   LA    R1,DMCB                                                          
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
         BNE   BDAT90                                                           
         B     BDAT50                                                           
*                                                                               
BDAT40   GET   INFILE,R+1                                                       
         MVI   R,X'09'                                                          
*                                                                               
BDAT50   MVC   BYTE,R                                                           
         BRAS  RE,CHKCC                                                         
*                                                                               
         LA    R8,WORK                                                          
         EX    R2,*+8              MOVE IN LINE                                 
         B     *+10                                                             
         MVC   0(0,R8),R+1                                                      
*                                                                               
         L     RF,=A(VALOCHRS)                                                  
         EX    R2,*+8                                                           
         B     *+10                                                             
         TR    0(0,R8),0(RF)       CONVERT BOX CHAR FOR TRANSMITION             
*                                                                               
         LA    R8,1(R2,R8)                                                      
         BCTR  R8,0                OMIT THE TAILING BLANKS                      
         CLI   0(R8),C' '                                                       
         BNE   *+8                                                              
         B     *-10                                                             
         LA    R8,1(R8)                                                         
*                                                                               
         MVI   0(R8),X'0D'         CARRIAGE RETURN                              
         MVI   1(R8),X'15'         NEW LINE                                     
         LA    R8,2(R8)                                                         
*                                                                               
         S     R8,=A(WORK)                                                      
         ST    R8,BUFLEN                                                        
         MVC   BUFADDR,=A(WORK)                                                 
         MVC   ALET,=F'0'                                                       
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         L     RF,BPX1WRT                                                       
         CALL  (15),                 WRITE TO A FILE                   X        
               (FILEDESC,            INPUT: FILE DESCRIPTOR            X        
               BUFADDR,              INPUT: ->BUFFER                   X        
               ALET,                 INPUT: BUFFER ALET                X        
               BUFLEN,               INPUT: NUMBER OF BYTES TO WRIT    X        
               RETVAL,               RETURN VALUE: -1 OR BYTES WRIT    X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL   TEST RETVAL           BL    PSEUD            
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         CLI   EDICTHDR+38,C'D'    IS A DSN SUPPLIED?                           
         BE    BDAT40                                                           
         B     BDAT30                                                           
*                                                                               
*                                                                               
BDAT90   CLI   EDICTHDR+38,C'D'    IS A DSN SUPPLIED?                           
         BNE   BDAT100                                                          
         CLOSE (INFILE)                                                         
*                                                                               
BDAT100  LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         L     RF,BPX1CLO                                                       
         CALL  (15),                 CLOSE A FILE                      X        
               (FILEDESC,            INPUT: FILE DESCRIPTOR            X        
               RETVAL,               RETURN VALUE: 0 OR -1             X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL   TEST RETVAL           BL    PSEUD            
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
BDATX    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*CHECK THE PRINT LINE CONTROL CHARACTER, LOOK UP THE TABLE AND                  
*PRINT OUT THE ACCORDING HTML CODE.                                             
*NTRY: BYTE = CONTROL CHAR                                                      
CHKCC    NTR1  BASE=*,LABEL=*                                                   
         L     R3,VTABLE                                                        
         L     R3,0(R3)            GET FIRST TABLE ADDRESS                      
*                                                                               
         USING TABLED,R3                                                        
CHKCC10  CLC   TABCC,BYTE                                                       
         BE    CHKCC20                                                          
         CLI   TABCC,X'FF'                                                      
         BE    CHKCCX              NOT FOUND, DO NOTHING                        
         AHI   R3,TABLENQ                                                       
         B     CHKCC10                                                          
*                                                                               
CHKCC20  ICM   RE,15,TABPTR                                                     
         TM    TABFLG,TABFP2                                                    
         BZ    CHKCC40             NO 2ND ADDR FOR WIDE REPORT                  
         CLI   EDICTHDR+35,C'W'    IS THIS WIDE REPORT                          
         BNE   CHKCC40                                                          
         ICM   RE,15,TABPTR2                                                    
         DROP  R3                                                               
*                                                                               
CHKCC40  LH    RF,0(RE)                                                         
         CHI   RF,(L'WORK+L'WORKX)                                              
         BNH   *+6                                                              
         DC    H'0'                TABLE ENTRY TOO BIG                          
*                                                                               
         LA    RE,2(RE)                                                         
         LA    R8,WORK                                                          
         LR    R9,RF                                                            
         MVCL  R8,RE                                                            
*                                                                               
         SHI   RF,2                                                             
         AR    R8,RF                                                            
*                                                                               
         S     R8,=A(WORK)                                                      
         ST    R8,BUFLEN                                                        
         MVC   BUFADDR,=A(WORK)                                                 
         MVC   ALET,=F'0'                                                       
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         L     RF,BPX1WRT                                                       
         CALL  (15),                 WRITE TO A FILE                   X        
               (FILEDESC,            INPUT: FILE DESCRIPTOR            X        
               BUFADDR,              INPUT: ->BUFFER                   X        
               ALET,                 INPUT: BUFFER ALET                X        
               BUFLEN,               INPUT: NUMBER OF BYTES TO WRIT    X        
               RETVAL,               RETURN VALUE: -1 OR BYTES WRIT    X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL   TEST RETVAL           BL    PSEUD            
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
CHKCCX   XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*BUILD A COVERPAGE                                                              
*                                                                               
COVERPAG NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   EDICTHDR+68,C'D'    DARE FAX?                                    
         BNE   CPX                                                              
*                                  ONLY DARE FAX TYPE GET COVERPAGE             
         CLI   EDICTHDR+71,C'N'    NO COVER PAGE?                               
         BE    CPX                                                              
*                                                                               
         MVI   DESTTYPE,EDFDSTXQ   FAX RECORD KEY                               
         XC    WORK,WORK           RECEIVING ID WILL BE BUILT HERE              
         XC    COVPGLNS,COVPGLNS                                                
*                                                                               
         MVC   DESTINAT,EDICTHDR+39                                             
         MVI   DESTINAT+4,C'T'         SET MEDIA TO TV                          
*                                                                               
         XC    KEY,KEY             BUILD DARE STATION RECORD KEY                
         LA    R4,KEY                                                           
         USING STAKEYD,R4                                                       
         MVI   STAKSYS,STAKSYSQ                                                 
         MVI   STAKTYP,STAKTYPQ                                                 
         MVC   STAKMEDA,DESTINAT+4                                              
         MVC   STAKSTIN,DESTINAT                                                
         DROP  R4                                                               
*                                                                               
         GOTO1 SADTAMGR,DMCB,(0,=C'DMREAD'),=C'GENDIR',KEY,KEY                  
         CLI   DMCB+8,0                                                         
         BNE   CPX                                                              
*                                                                               
         GOTO1 SADTAMGR,DMCB,(0,=C'GETREC'),=C'GENFIL',KEY+36,A(IO),   +        
               DMWORK                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   DATADISP,=H'42'     FOR GENFIL                                   
         L     R4,=A(IO)                                                        
         MVI   ELCODE,STAHOMCQ     HOME MARKET ELEM?                            
         BAS   RE,GETEL                                                         
         BNE   CP010               NO HOME MKT ELEM, FIND THE REP               
         USING STAHOMD,R4                                                       
         CLC   STAHOMCT,ROUTCODE+3 MATCHES OUR CITY?                            
         BNE   CP010               NO                                           
         OC    STAHOMIB,STAHOMIB   HAVE A CITY BUT NO RECEIVING ID?             
         BNZ   CP010                 STRANGE...USE DEFAULT INSTEAD              
         MVC   WORK(L'DESTINAT),DESTINAT  USE STATION AS RECEIVER               
         MVI   DESTTYPE,EDFDSTHQ   HOME MARKET W/O ID                           
         B     CP100                                                            
*                                                                               
CP010    L     R4,=A(IO)                                                        
         MVI   ELCODE,STAREPCQ     REP ELEMENT                                  
         BAS   RE,GETEL                                                         
         BNE   CP100               NO REP ELEMENT (SHOULDN'T HAPPEN)            
*                                                                               
         USING STAREPD,R4                                                       
         MVC   REPCODE,STAREPCR    CURRENT REPCODE                              
         GOTO1 =V(DATCON),DMCB,(5,0),(15,FULL)                                  
         CLC   STAREPED,FULL                                                    
         BNH   *+10                                                             
         MVC   REPCODE,STAREPPR    PREVIOUS REPCODE                             
         DROP  R4                                                               
*                                                                               
         L     R3,=A(REPIDS)       TABLE OF RECEIVING REPS                      
CP020    CLI   0(R3),X'FF'                                                      
         BE    CP100               MISSING ENTRY -- SHOULD NEVER HAPPEN         
         CLC   REPCODE,0(R3)                                                    
         BE    *+12                GOT IT                                       
         LA    R3,L'REPIDS(R3)     SKIP TO NEXT ENTRY                           
         B     CP020                                                            
*                                                                               
         MVC   WORK,SPACES                                                      
         ZIC   R4,14(R3)           CONSTRUCT RECEIVING ID IN WORK               
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),15(R3)      REP-ID                                       
         TM    13(R3),X'80'        APPEND OFFICE CODE?                          
         BO    CP100               NO                                           
*                                                                               
         LA    R3,WORK+1(R4)       POINT BEYOND REP-ID                          
         MVC   0(2,R3),ROUTCODE+3  OFFICE                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING AGRKEYD,R4                                                       
         MVI   AGRKSYS,AGRKSYSQ                                                 
         MVI   AGRKTYP,AGRKTYPQ                                                 
         MVC   AGRKMEDA,DESTINAT+4                                              
         MVC   AGRKAGRT,ROUTCODE                                                
         DROP  R4                                                               
*                                                                               
         GOTO1 SADTAMGR,DMCB,(0,=C'DMREAD'),=C'GENDIR',KEY,KEY                  
         TM    DMCB+8,X'FF'-X'10'                                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         TM    DMCB+8,X'10'                                                     
         BO    CP100               NO OFFICE EXCEPTION RECORD                   
*                                                                               
         GOTO1 SADTAMGR,DMCB,(0,=C'GETREC'),=C'GENFIL',KEY+36,A(IO),   +        
               DMWORK                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   DATADISP,=H'42'     FOR GENFIL                                   
         L     R4,=A(IO)                                                        
         MVI   ELCODE,AGROVRCQ     OVERRIDE ELEMENT CODE                        
         BAS   RE,GETEL                                                         
         BNE   CP100                                                            
*                                                                               
         USING AGROVRD,R4                                                       
CP030    CLC   REPCODE,AGROVRCR    IS THERE AN OVERRIDE FOR THIS REP?           
         BE    *+16                YES                                          
         BAS   RE,NEXTEL                                                        
         BE    CP030                                                            
         B     CP100               NO                                           
*                                                                               
         MVC   0(2,R3),AGROVROF    PUT OFFICE OVERRIDE IN RECEIVING ID          
         DROP  R4                                                               
*                                                                               
CP100    MVC   FAXKEY,WORK                                                      
*                                                                               
         CLC   =C'NOREP',FAXKEY                                                 
         BNE   COVRPG03                                                         
         CLI   DESTINAT+4,C'T'     MEDIA T?                                     
         BNE   COVRPG03            **SHOULD CHECK RADIO AND OTHER LATER         
*                                                                               
         MVC   AGYPOWER,SVAGYPOW   USE AGENCY POWER CODE, NOT "DARADM"          
         MVC   FAXKEY(4),DESTINAT  USE STATION CALL LETTERS (4)                 
         MVC   FAXKEY+4(3),SPACES                                               
         B     COVRPG04                                                         
*                                                                               
COVRPG03 CLI   DESTTYPE,EDFDSTHQ   HOME MARKET W/O ID?                          
         BE    *+10                                                             
         MVC   AGYPOWER,=C'D7'     NO, ALWAYS USE USERID 'DARADM'               
*                                                                               
COVRPG04 L     R2,=A(COVERBUF)     BUILD COVER PAGE HERE                        
         LR    RF,R2                                                            
         LA    R0,COVPGMAX         MAXIMUM NUMBER OF LINES                      
COVRPG05 MVC   0(132,RF),SPACES    CLEAR TABLE                                  
         LA    RF,132(RF)                                                       
         BCT   R0,COVRPG05                                                      
*                                                                               
         MVC   DATADISP,=H'28'     FOR CTFILE                                   
*                                                                               
         XC    KEY,KEY             READ FAX RECORD                              
         LA    R4,KEY                                                           
         USING CTFXREC,R4                                                       
         MVI   CTFXKTYP,CTFXEQU                                                 
         MVC   CTFXAGY,AGYPOWER                                                 
         MVC   CTFXCODE,FAXKEY                                                  
*                                                                               
         OC    FAXSUBC(3),FAXSUBC  1ST SUB CODE IS THERE?                       
         BZ    COVRPG09            NO - TRY 2ND SUB CODE                        
         MVC   CTFXSUBC,SPACES                                                  
         MVC   CTFXSUBC(3),FAXSUBC                                              
         DROP  R4                                                               
*                                                                               
         MVC   P+30(25),KEY                                                     
         PRNT  DAREFAX_KEY                                                      
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
         L     RF,=A(IO)                                                        
         MVC   P+30(25),0(RF)                                                   
         PRNT  DAREFAX_IO                                                       
*                                                                               
         TM    DMCB+8,X'FF'-X'10'                                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         TM    DMCB+8,X'10'        1ST FAX CODE FOUND?                          
         BNZ   *+12                NO                                           
         MVI   DESTTYPE,EDFDSTXQ                                                
         B     COVRPG14                                                         
*                                                                               
COVRPG09 OC    FAXSUBC+3(3),FAXSUBC+3   2ND SUB CODE IS THERE?                  
         BZ    COVRPG11                 NO - TRY WITHOUT SUB CODE               
*                                                                               
         XC    KEY,KEY             READ FAX RECORD                              
         LA    R4,KEY                                                           
         USING CTFXREC,R4                                                       
         MVI   CTFXKTYP,CTFXEQU                                                 
         MVC   CTFXAGY,AGYPOWER                                                 
         MVC   CTFXCODE,FAXKEY                                                  
         MVC   CTFXSUBC,SPACES                                                  
         MVC   CTFXSUBC(3),FAXSUBC+3                                            
         DROP  R4                                                               
*                                                                               
         MVC   P+30(25),KEY                                                     
         PRNT  DAREFAX_KEY                                                      
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
         L     RF,=A(IO)                                                        
         MVC   P+30(25),0(RF)                                                   
         PRNT  DAREFAX_IO                                                       
*                                                                               
         TM    DMCB+8,X'FF'-X'10'                                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         TM    DMCB+8,X'10'        2ND FAX CODE FOUND?                          
         BNZ   *+12                NO                                           
         MVI   DESTTYPE,EDFDSTXQ                                                
         B     COVRPG14                                                         
*                                                                               
COVRPG11 XC    KEY,KEY             READ FAX RECORD                              
         LA    R4,KEY                                                           
         USING CTFXREC,R4                                                       
         MVI   CTFXKTYP,CTFXEQU                                                 
         MVC   CTFXAGY,AGYPOWER                                                 
         MVC   CTFXCODE,FAXKEY                                                  
         DROP  R4                                                               
*                                                                               
         MVC   P+30(25),KEY                                                     
         PRNT  DAREFAX_KEY                                                      
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
         L     RF,=A(IO)                                                        
         MVC   P+30(25),0(RF)                                                   
         PRNT  DAREFAX_IO                                                       
*                                                                               
         TM    DMCB+8,X'FF'-X'10'                                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         TM    DMCB+8,X'10'                                                     
         BO    COVRPG15            NO FOUND                                     
*                                                                               
COVRPG14 CLI   DESTTYPE,EDFDSTHQ   FAX NUMBER (HOME MARKET W/O ID)?             
         BNE   COVRPG18                                                         
         MVI   DESTTYPE,EDFDSTXQ   YES, RESET TO FAX RECORD KEY                 
         B     COVRPG18                                                         
*                                                                               
COVRPG15 CLI   DESTTYPE,EDFDSTHQ   FAX NUMBER (HOME MARKET W/O ID)?             
         BNE   COVRPG40            "SENT TO" FAX REC NOT FOUND                  
*                                                                               
COVRPG18 MVC   34(10,R2),=C'FAX HEADER'                                         
         LA    R2,132(R2)                                                       
         MVC   34(10,R2),=C'----------'                                         
         LA    R2,132(R2)                                                       
         LA    R2,132(R2)                                                       
         MVC   21(8,R2),=C'SENT TO:'                                            
*                                                                               
         CLI   DESTTYPE,EDFDSTHQ   FAX NUMBER (HOME MARKET W/O ID)?             
         BE    COVRPG20                                                         
*                                                                               
         L     R4,=A(IO)                                                        
         MVI   ELCODE,CTFX2ELQ     ATTENTION ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   COVRPG20                                                         
         USING CTFXATT,R4                                                       
*                                                                               
         ZIC   R1,CTFX2LEN                                                      
         SH    R1,=H'3'            2 FOR OVERHEAD, ONE FOR EX                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   34(0,R2),CTFX2ATT                                                
         LA    R2,132(R2)                                                       
         DROP  R4                                                               
*                                                                               
COVRPG20 MVC   34(4,R2),=C'FAX='                                                
*                                                                               
         CLI   DESTTYPE,EDFDSTHQ   FAX NUMBER (HOME MARKET W/O ID)?             
         BE    COVRPG25                                                         
*                                                                               
         L     R4,=A(IO)                                                        
         USING CTFXREC,R4                                                       
*                                                                               
         ZIC   R1,CTFX1LEN         FAX NUMBER                                   
         SH    R1,=H'3'            2 FOR OVERHEAD, ONE FOR EX                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   38(0,R2),CTFX1NUM                                                
         LA    R1,38(R2)                                                        
         BAS   RE,EDITTEL                                                       
*                                                                               
COVRPG25 LA    R2,132(R2)                                                       
         LA    R2,132(R2)                                                       
         LR    R6,R2                                                            
         DROP  R4                                                               
*                                                                               
         CLI   DESTTYPE,EDFDSTHQ   FAX NUMBER (HOME MARKET W/O ID)?             
         BE    COVRPG40                                                         
*                                                                               
         MVI   ELCODE,CTFX3ELQ     MESSAGE ELEMENTS                             
         BAS   RE,GETEL                                                         
         BNE   COVRPG40                                                         
         USING CTFXMSG,R4                                                       
*                                                                               
COVRPG30 ZIC   R3,CTFX3LIN                                                      
         BCTR  R3,0                                                             
         MH    R3,=H'132'                                                       
         LR    R2,R6                                                            
         AR    R2,R3               POINT TO CORRECT LINE FOR MESSAGE            
*                                                                               
         ZIC   R1,CTFX3LEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   34(0,R2),CTFX3MSG                                                
         LA    R2,132(R2)                                                       
         BAS   RE,NEXTEL                                                        
         BE    COVRPG30                                                         
         DROP  R4                                                               
*                                                                               
COVRPG40 LA    R2,132(R2)                                                       
         MVC   21(10,R2),=C'SENT FROM:'                                         
*                                                                               
         MVI   BYTE2,C'N'                                                       
         OC    REQUESTR,REQUESTR                                                
         BZ    COVRPG50                                                         
         XC    KEY,KEY             READ FAX RECORD FOR REQUESTOR                
         LA    R4,KEY                                                           
         USING CTFXREC,R4                                                       
         MVI   CTFXKTYP,CTFXEQU                                                 
         MVC   CTFXAGY,SVAGYPOW                                                 
         MVC   CTFXCODE(2),=C'**'  LOOK FOR **RRR (REQUESTOR)                   
         MVC   CTFXCODE+2(3),REQUESTR                                           
         MVC   CTFXCODE+5(2),SPACES                                             
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
         TM    DMCB+8,X'FF'-X'10'                                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         TM    DMCB+8,X'10'                                                     
         BO    COVRPG50            REQUESTOR DOESN'T HAVE A FAX ELEMENT         
*                                                                               
         MVI   BYTE2,C'Y'                                                       
         L     R4,=A(IO)                                                        
         MVI   ELCODE,CTFX2ELQ     ATTENTION ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   COVRPG20                                                         
         USING CTFXATT,R4                                                       
*                                                                               
         ZIC   R1,CTFX2LEN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   34(0,R2),CTFX2ATT                                                
         LA    R2,132(R2)                                                       
         DROP  R4                                                               
*                                                                               
COVRPG50 MVC   34(33,R2),AGYNAME                                                
         LA    R2,132(R2)                                                       
         MVC   34(33,R2),AGYADDR                                                
         LA    R2,132(R2)                                                       
         MVC   34(3,R2),=C'ID='                                                 
         MVC   37(10,R2),USERID                                                 
         LA    R2,132(R2)                                                       
*                                                                               
         OC    REQUESTR,REQUESTR                                                
         BZ    COVRPG80                                                         
         MVC   34(10,R2),=C'REQUESTOR='                                         
         MVC   44(3,R2),REQUESTR                                                
         LA    R2,132(R2)                                                       
*                                                                               
         CLI   BYTE2,C'Y'          DO WE HAVE REQUESTOR FAX DATA?               
         BNE   COVRPG80                                                         
         L     R4,=A(IO)                                                        
         MVI   ELCODE,CTFX4ELQ     RETURN NUMBER ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   COVRPG60                                                         
         USING CTFXTEL,R4                                                       
*                                                                               
         MVC   34(10,R2),=C'TELEPHONE='                                         
         ZIC   R1,CTFX4LEN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   44(0,R2),CTFX4TEL                                                
         LA    R1,44(R2)                                                        
         BAS   RE,EDITTEL                                                       
         LA    R2,132(R2)                                                       
         DROP  R4                                                               
*                                                                               
COVRPG60 L     R4,=A(IO)                                                        
         USING CTFXREC,R4                                                       
*                                                                               
         MVC   34(4,R2),=C'FAX='   FAX NUMBER                                   
         ZIC   R1,CTFX1LEN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   38(0,R2),CTFX1NUM                                                
         LA    R1,38(R2)                                                        
         BAS   RE,EDITTEL                                                       
*                                                                               
         LA    R2,132(R2)                                                       
         LA    R2,132(R2)                                                       
         LR    R6,R2                                                            
*                                                                               
         MVI   ELCODE,CTFX3ELQ     MESSAGE ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   COVRPG80                                                         
         USING CTFXMSG,R4                                                       
*                                                                               
COVRPG70 ZIC   R3,CTFX3LIN                                                      
         BCTR  R3,0                                                             
         MH    R3,=H'132'                                                       
         LR    R2,R6                                                            
         AR    R2,R3               POINT TO CORRECT LINE FOR MESSAGE            
*                                                                               
         ZIC   R1,CTFX3LEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   34(0,R2),CTFX3MSG                                                
         LA    R2,132(R2)                                                       
         BAS   RE,NEXTEL                                                        
         BE    COVRPG70                                                         
         DROP  R4                                                               
*                                                                               
COVRPG80 LA    R2,132(R2)                                                       
         MVC   21(12,R2),=C'FAX SENT ON:'             DATE                      
         GOTO1 =V(DATCON),DMCB,(5,0),(8,34(R2))                                 
         LA    R2,132(R2)                                                       
         MVC   21(10,R2),=C'TIME SENT:'                                         
         THMS  DDSTIME=YES                                                      
         ST    R1,FULL             0HHMMSS+ (DDS TIME)                          
         ST    R0,PACKOF4B         0060000+ (DDS CLOCK TIME DIFFERENCE)         
         AP    FULL,PACKOF4B       FULL = ACTUAL TIME                           
         XC    DUB,DUB                                                          
         MVC   DUB+5(3),FULL                                                    
         OI    DUB+7,X'0F'                                                      
         CVB   R1,DUB                                                           
         C     R1,=F'2400'                                                      
         BL    *+8                                                              
         S     R1,=F'2400'                                                      
         EDIT  (R1),(5,34(R2)),2                                                
*                                                                               
COVRPG90 S     R2,=A(COVERBUF)                                                  
         BZ    CPX                 COVER PAGE IS EMPTY                          
         SRDL  R2,32               PREPARE FOR DIVIDE                           
         D     R2,=F'132'                                                       
         LA    R3,1(R3)            R3 = NUMBER OF LINES IN COVER PAGE           
         ST    R3,COVPGLNS                                                      
*                                                                               
COVRPGX  DS    0H                                                               
*                                                                               
         ICM   R6,15,COVPGLNS                                                   
         BZ    CPX                                                              
         CHI   R6,COVPGMAX                                                      
         BNH   *+6                                                              
         DC    H'0'                TOO MANY LINE IN THE COVER PAGE!             
*                                                                               
         L     R2,=A(COVERBUF)                                                  
         LA    R8,132              LENGTH OF EACH LINE                          
*                                                                               
         MVI   BYTE,X'89'          FOR NEW DOC BEGINNING HTML CODE              
         B     *+8                                                              
CP300    MVI   BYTE,X'09'          NEW LINE                                     
         BRAS  RE,CHKCC                                                         
         BAS   R9,CPPRTLN          WRITE A LINE TO HFS FILE                     
*************                                                                   
         MVC   P(100),0(R2)                                                     
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
*************                                                                   
         LA    R2,132(R2)                                                       
         BCT   R6,CP300                                                         
*                                                                               
CPX      XIT1                                                                   
*                                                                               
CPPRTLN  ST    R8,BUFLEN                                                        
         ST    R2,BUFADDR                                                       
         MVC   ALET,=F'0'                                                       
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         L     RF,BPX1WRT                                                       
         CALL  (15),                 WRITE TO A FILE                   X        
               (FILEDESC,            INPUT: FILE DESCRIPTOR            X        
               BUFADDR,              INPUT: ->BUFFER                   X        
               ALET,                 INPUT: BUFFER ALET                X        
               BUFLEN,               INPUT: NUMBER OF BYTES TO WRIT    X        
               RETVAL,               RETURN VALUE: -1 OR BYTES WRIT    X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL   TEST RETVAL           BL    PSEUD            
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
CPPRTLNX BR    R9                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                       EDIT TELEPHONE NUMBER                         *         
*        PUT IN () & -                                                *         
*        R1=A(CL25 TELEPHONE)                                         *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
EDITTEL  NTR1                                                                   
*                                                                               
         MVC   WORK(25),0(R1)                                                   
         MVC   0(25,R1),SPACES                                                  
         LA    R2,WORK                                                          
         CLC   WORK+7(4),SPACES    IF > 7 CHARACTERS                            
         BE    EDITEL20                                                         
         CLI   WORK,C'1'                                                        
         BH    EDITEL10                                                         
         MVC   0(2,R1),=C'1-'                                                   
         LA    R1,2(R1)                                                         
         LA    R2,1(R2)                                                         
*                                                                               
EDITEL10 MVI   0(R1),C'('          ASSUME AREA CODE TO START                    
         MVC   1(3,R1),0(R2)                                                    
         MVI   4(R1),C')'                                                       
         LA    R2,3(R2)                                                         
         LA    R1,6(R1)                                                         
*                                                                               
EDITEL20 MVC   0(3,R1),0(R2)                                                    
         MVI   3(R1),C'-'                                                       
         MVC   4(10,R1),3(R2)                                                   
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
ERRDFAX  NTR1  BASE=*,LABEL=*                                                   
         L     R6,=A(EDICTREC)                                                  
         USING EDFILD,R6                                                        
         TM    EDFFLAGS,EDFDAREZ   WAS THIS A DARE REPORT?                      
         BZ    ERRDXX                                                           
*                                                                               
         L     R3,=A(NEWEDREC)                                                  
         XC    0(L'NEWEDREC,R3),0(R3)  BUILD EDICT FILE RECORD                  
         MVI   EDFSYS-EDFILD(R3),EDFDAREQ MARK AS A DARE RECORD                 
         GOTO1 =V(DATCON),DMCB,(5,0),(3,DUB2)                                   
         MVC   EDFMON-EDFILD(,R3),DUB2+1 MONTH NUMBER IN BINARY                 
         LA    R3,EDFDARE-EDFILD(,R3)                                           
         USING RDLNFAXD,R3                                                      
         MVC   RDFXTID,=C'ERRFAX'  BUILD FAX DELIVERY NOTIFICATION              
         MVC   RDFXORDR,EDFAPPL+23 ORDER NUMBER                                 
         MVC   RDFXFRID,SPACES                                                  
         MVC   RDFXFRID(4),EDFAPPL+15    DEST STATION CALL LETTER               
         MVC   RDFXTOID,SPACES                                                  
         MVC   RDFXTOID(8),MSG01   ORIGINATING AGENCY USERID                    
         MVC   RDFXRTRN,EDFAPPL+31 'RETURN TO SENDER' DATA                      
         DROP  R6                                                               
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(0,WORK)     (YYMMDD)                      
         THMS  DDSTIME=YES                                                      
         ST    R0,PACKOF4B                                                      
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
         CP    PACKOF4B,=P'240000' PAST MIDNIGHT?                               
         BL    ERRDX20                                                          
         SP    PACKOF4B,=P'240000' YES, BUMP TO NEXT DAY AND ADJUST             
         GOTO1 =V(ADDAY),DMCB,WORK,WORK,F'1'                                    
*                                                                               
ERRDX20  MVC   RDFXDATE,WORK       ADJUSTED DATE (YYMMDD)                       
         EDIT  PACKOF4B,DUB2       C'00HHMMSS'                                  
         MVC   RDFXTIME,DUB2+2     C'HHMM'                                      
*                                                                               
         MVC   RDFXTDTE,RDFXDATE   DATE OF FAX ERROR (YYMMDD)                   
         MVC   RDFXTTIM,RDFXTIME   C'HHMM'                                      
*                                                                               
         CLI   ERRCODE,EDFERNUQ    UNKNOWN BDE USER ERROR                       
         BNE   *+10                                                             
         MVC   RDFXERR,=CL3'502'   UNKNOWN USER                                 
*                                                                               
         CLI   ERRCODE,EDFERNDQ    NO DOC ERROR/ICONV ERROR                     
         BNE   *+10                                                             
         MVC   RDFXERR,=CL3'503'   UNABLE TO SEND                               
*                                                                               
         PRNT  ADDEDICTRECORD,PRINT=ALWAYS                                      
         MVC   DMCB+4,=A(NEWEDREC)                                              
         GOTO1 =V(PRNTBL),DMCB,0,,C'DUMP',256,=C'1D'                            
         MVC   DMCB+4,=A(NEWEDREC)                                              
         GOTO1 SEDCTADD,DMCB,SMAJORNM,,0                                        
         MVC   FULL,DMCB+8         RETURNED DISK ADDRESS                        
         GOTO1 =V(HEXOUT),DMCB,FULL,P+30,4,=C'TOG'                              
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         PRNT  EDICTRECORDADDED,PRINT=ALWAYS                                    
         DROP  R3                                                               
ERRDXX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
BLDFILN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   FILENAME,SPACES                                                  
         MVC   FILENAME(L'FLNAME),FLNAME                                        
         LA    RE,FILENAME+L'FLNAME+1                                           
*                                                                               
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         B     *-10                                                             
*                                                                               
         MVI   1(RE),C'.'                                                       
         MVC   2(L'FLEXT,RE),FLEXT                                              
*                                                                               
         AHI   RE,L'FLEXT+2                                                     
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         B     *-10                                                             
*                                                                               
         AHI   RE,1                                                             
         S     RE,=A(FILENAME)                                                  
         ST    RE,FNLEN                                                         
*                                  TR CHARS THAT MAY CAUSE PROBLEM              
         BCTR  RE,0                                                             
         L     RF,=A(TRTABLE)                                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         TR    FILENAME(0),0(RF)                                                
*                                                                               
BFNX     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
BLDREF   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   CUSTREF,C'4'        VERSION 4                                    
         CLI   SDRTEST,C'Y'        DISASTER RECOVERY TEST MODE?                 
         BNE   *+8                                                              
         MVI   CUSTREF,C'D'        YES -- VERSION 'D' (FOR 'D'ISASTER)          
         MVC   CUSTREF+1(1),MAJORNAM+5  'A' FOR ADV, 'R' FOR REP                
         GOTO1 =V(DATCON),DMCB,(5,0),(20,CUSTREF+10)                            
*                                                                               
         LA    R2,TABLNTRY                                                      
         USING XMTTABLD,R2                                                      
         LA    R1,DMCB                                                          
         USING CONVDSKD,R1                                                      
         MVC   CONVDMGR,SADTAMGR   A(DATAMGR)                                   
         MVI   CONVACTN,CONVDSKF   CONVERT DSKADDR TO REFERENCE NUMBER          
         MVC   CONVDSK,XMTDSKAD    EDICT FILE DISK ADDRESS                      
         MVC   CONVOFF,=A(CUSTREF+2)  RESULT GOES INTO CUSTREF                  
         GOTO1 =V(CONVDSKA)                                                     
         DROP  R1,R2                                                            
*                                                                               
BREFX    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
BLDCMDF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   CM00P1,EMAILADR                                                  
         MVC   CM01P1,FILENAME                                                  
*                                                                               
         MVC   CM02P1(9),=C'/tmp/BDE_'                                          
         MVC   CM02P1+9(1),MAJORNAM+5        EdictA/R/T                         
         MVC   CM02P1+10(1),EDICTTYP         EDICT TYPE A/R                     
*                                                                               
         MVC   CM04P1,SPACES                                                    
         CLI   MAJORNAM+5,C'T'     IS THIS TST EDICT?                           
         BNE   *+10                                                             
         MVC   CM04P1(20),=CL20'DOCEXPHOURS 720'     30 DAYS EXP                
*                                                                               
         MVC   CM05P1,SUBJECT                                                   
         MVC   CM05P2,CUSTREF                                                   
*                                                                               
         L     RE,PHLEN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CM10P1(0),PATH                                                   
*                                                                               
         LA    RE,CM10P1+1(RE)                                                  
         MVC   0(14,RE),=C'/function1.rex'                                      
         L     RE,PHLEN                                                         
         AHI   RE,14                                                            
         CHI   RE,L'CM10P1                                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CM14P1,CM10P1                                                    
         MVC   CM18P1,CM10P1                                                    
*                                                                               
         L     RE,PHLEN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CMDFILE(0),PATH                                                  
*                                                                               
         LA    RE,CMDFILE+1(RE)                                                 
         MVC   0(13,RE),=C'/command_file'                                       
         L     RE,PHLEN                                                         
         AHI   RE,13                                                            
         ST    RE,CFLEN                                                         
         CHI   RE,L'CMDFILE                                                     
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         XC    S_MODE,S_MODE                                                    
         MVI   S_MODE2,S_IRUSR       USER READ/WRITE, GROUP READ,               
         MVI   S_MODE3,S_IWUSR+S_IRGRP+S_IROTH           OTHER READ             
         XC    O_FLAGS(OPNF#LENGTH),O_FLAGS                                     
         MVI   O_FLAGS4,O_CREAT+O_RDWR+O_TRUNC                                  
         L     RF,BPX1OPN                                                       
         CALL  (15),                 OPEN A FILE  BPX1OPN              X        
               (CFLEN,               INPUT: PATHNAME LENGTH            X        
               CMDFILE,              INPUT: PATHNAME                   X        
               O_FLAGS,              INPUT: ACCESS            BPXYOPNF X        
               S_MODE,               INPUT: MODE    BPXYMODE, BPXYFTYP X        
               RETVAL,               RETURN VALUE:-1 OR FILE DESCRIPTO X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL   TEST RETVAL           BL    PSEUD            
         BNL   *+6                                                              
         DC    H'0'                                                             
         ST    RF,FILEDESC         SAVE FILE DESCRIPTOR                         
*                                                                               
         MVC   BUFLEN,=A(L'P)                                                   
         MVC   ALET,=F'0'                                                       
         LA    RE,P                                                             
         ST    RE,BUFADDR                                                       
         LA    R3,CM00                                                          
*                                                                               
BCF30    CLI   0(R3),X'FF'                                                      
         BE    BCF50                                                            
*                                                                               
         MVC   P,SPACES                                                         
         ZIC   R2,0(R3)                                                         
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   P(0),1(R3)                                                       
         MVI   P+L'P-1,X'15'                                                    
         LA    R3,2(R2,R3)                                                      
*                                                                               
         L     RF,BPX1WRT                                                       
         CALL  (15),                 WRITE TO A FILE                   X        
               (FILEDESC,            INPUT: FILE DESCRIPTOR            X        
               BUFADDR,              INPUT: ->BUFFER                   X        
               ALET,                 INPUT: BUFFER ALET                X        
               BUFLEN,               INPUT: NUMBER OF BYTES TO WRIT    X        
               RETVAL,               RETURN VALUE: -1 OR BYTES WRIT    X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL   TEST RETVAL           BL    PSEUD            
         BNL   *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(PRINTER)                                                      
         B     BCF30                                                            
*                                                                               
BCF50    L     RF,BPX1CLO                                                       
         CALL  (15),                 CLOSE A FILE                      X        
               (FILEDESC,            INPUT: FILE DESCRIPTOR            X        
               RETVAL,               RETURN VALUE: 0 OR -1             X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL   TEST RETVAL           BL    PSEUD            
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
BCFX     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
CHKRC    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,PHLEN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   RCFILE(0),PATH                                                   
*                                                                               
         LA    RE,RCFILE+1(RE)                                                  
         MVC   0(12,RE),=C'/return_code'                                        
         L     RE,PHLEN                                                         
         AHI   RE,12                                                            
         ST    RE,RCFLEN                                                        
         CHI   RE,L'RCFILE                                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         XC    S_MODE,S_MODE                                                    
         MVI   S_MODE2,S_IRUSR       USER READ/WRITE, GROUP READ,               
         MVI   S_MODE3,S_IWUSR+S_IRGRP+S_IROTH        OTHER READ                
         XC    O_FLAGS(OPNF#LENGTH),O_FLAGS                                     
         MVI   O_FLAGS4,O_RDONLY                                                
         L     RF,BPX1OPN                                                       
         CALL  (15),                 OPEN A FILE  BPX1OPN              X        
               (RCFLEN,              INPUT: PATHNAME LENGTH            X        
               RCFILE,               INPUT: PATHNAME                   X        
               O_FLAGS,              INPUT: ACCESS            BPXYOPNF X        
               S_MODE,               INPUT: MODE    BPXYMODE, BPXYFTYP X        
               RETVAL,               RETURN VALUE:-1 OR FILE DESCRIPTO X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL     TEST RETVAL           BL    PSEUD          
         BNL   *+6                                                              
         DC    H'0'                                                             
         ST    RF,FILEDESC           SAVE FILE DESCRIPTOR                       
*                                                                               
         XC    WORK,WORK                                                        
         MVC   BUFLEN,=A(L'WORK)     READ BUFFER LENGTH                         
         MVC   BUFADDR,=A(WORK)                                                 
         L     RF,BPX1RED                                                       
         CALL  (15),                 OPEN A FILE  BPX1RED              X        
               (FILEDESC,            INPUT: FILE DESCRIPTOR            X        
               BUFADDR,              ->BUFFER TO READ INTO             X        
               BUFFALET,             INPUT: BUFFER ALET                X        
               BUFLEN,               INPUT: NUMBER OF BYTES TO READ    X        
               RETVAL,               RETURN VALUE: 0, -1, OR CHAR COUN X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL     TEST RETVAL           BL    PSEUD          
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,BPX1CLO                                                       
         CALL  (15),                 CLOSE A FILE                      X        
               (FILEDESC,            INPUT: FILE DESCRIPTOR            X        
               RETVAL,               RETURN VALUE: 0 OR -1             X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL     TEST RETVAL           BL    PSEUD          
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
CRCX     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
PSTDOUT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,PHLEN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SOFILE(0),PATH                                                   
*                                                                               
         LA    RE,SOFILE+1(RE)                                                  
         MVC   0(7,RE),=C'/stdout'                                              
         L     RE,PHLEN                                                         
         AHI   RE,7                                                             
         ST    RE,SOFLEN                                                        
         CHI   RE,L'SOFILE                                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         XC    S_MODE,S_MODE                                                    
         MVI   S_MODE2,S_IRUSR       USER READ/WRITE, GROUP READ,               
         MVI   S_MODE3,S_IWUSR+S_IRGRP+S_IROTH        OTHER READ                
         XC    O_FLAGS(OPNF#LENGTH),O_FLAGS                                     
         MVI   O_FLAGS4,O_RDONLY                                                
         L     RF,BPX1OPN                                                       
         CALL  (15),                 OPEN A FILE  BPX1OPN              X        
               (SOFLEN,              INPUT: PATHNAME LENGTH            X        
               SOFILE,               INPUT: PATHNAME                   X        
               O_FLAGS,              INPUT: ACCESS            BPXYOPNF X        
               S_MODE,               INPUT: MODE    BPXYMODE, BPXYFTYP X        
               RETVAL,               RETURN VALUE:-1 OR FILE DESCRIPTO X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL     TEST RETVAL           BL    PSEUD          
         BNL   *+6                                                              
         DC    H'0'                                                             
         ST    RF,FILEDESC           SAVE FILE DESCRIPTOR                       
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         SR    R3,R3                 LEFTOVER LENGTH                            
         XC    WORK,WORK                                                        
PSO20    XC    P,P                                                              
         MVC   BUFLEN,=A(L'P)        READ BUFFER LENGTH                         
         LA    RE,P                                                             
         ST    RE,BUFADDR                                                       
         L     RF,BPX1RED                                                       
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         CALL  (15),                 OPEN A FILE  BPX1RED              X        
               (FILEDESC,            INPUT: FILE DESCRIPTOR            X        
               BUFADDR,              ->BUFFER TO READ INTO             X        
               BUFFALET,             INPUT: BUFFER ALET                X        
               BUFLEN,               INPUT: NUMBER OF BYTES TO READ    X        
               RETVAL,               RETURN VALUE: 0, -1, OR CHAR COUN X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL     TEST RETVAL           BL    PSEUD          
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
*                                  MERGE THE LEFTOVER WITH NEW LINE             
         LA    RE,WORK(R3)                                                      
         MVC   0(L'P,RE),P                                                      
         MVI   L'P(RE),X'FF'       END OF STRING MARKER                         
         AHI   R3,L'P              LENGTH OF STRING (EXCLUDE END X'FF')         
         MVC   P,SPACES                                                         
*                                                                               
PSO40    LA    RE,WORK                                                          
         LR    RF,RE                                                            
         LA    R1,L'P                                                           
PSO50    CLI   0(RE),X'FF'         END OF STRING?                               
         BE    PSO90                                                            
         CLI   0(RE),X'15'         FIND X'15' EOL CHAR                          
         BE    *+16                                                             
         AHI   RE,1                                                             
         BCT   R1,PSO50                                                         
         B     PSO80                                                            
*                                                                               
         SR    R4,R4                                                            
         SR    RE,RF                                                            
         BZ    PSO70               SKIP A LINE                                  
*                                                                               
         LR    R4,RE                                                            
         SR    R3,RE               REDUCE THE STRING LENGTH                     
*                                                                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   P(0),WORK           MOVE IN A PRINT LINE                         
PSO70    GOTO1 =V(PRINTER)                                                      
*                                                                               
         BCTR  R3,0                -1 FOR X'15' CHAR                            
         LA    R4,WORK(R4)         PT TO ADDR OF X'15' CHAR                     
*                                                                               
         LR    RE,R3                                                            
*        BCTR  RE,0                DON'T -1 TO INCL X'FF' END OF STRING         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),1(R4)       SHIFT UP THE LEFTOVER STRING                 
         B     PSO40                                                            
*                                                                               
PSO80    CHI   R3,L'P                                                           
         BL    PSO90                                                            
*                                                                               
         MVC   P,WORK              MOVE IN A PRINT LINE                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         SHI   R3,L'P                                                           
         LA    R4,WORK+L'P                                                      
         LR    RE,R3                                                            
*        BCTR  RE,0                DON'T -1 TO INCL X'FF' END OF STRING         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R4)       SHIFT UP THE LEFTOVER STRING                 
         B     PSO40                                                            
*                                                                               
PSO90    CLC   RETVAL,=A(L'P)                                                   
         BE    PSO20                                                            
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         L     RF,BPX1CLO                                                       
         CALL  (15),                 CLOSE A FILE                      X        
               (FILEDESC,            INPUT: FILE DESCRIPTOR            X        
               RETVAL,               RETURN VALUE: 0 OR -1             X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL     TEST RETVAL           BL    PSEUD          
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
PSOX     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
BATCH    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   PARM+2(3),=C'SH '                                                
         L     RE,PHLEN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PARM+5(0),PATH                                                   
*                                                                               
         LA    RE,PARM+6(RE)                                                    
         MVC   0(11,RE),=C'/main.rex "'                                         
         AHI   RE,11                                                            
*                                                                               
         L     RF,FNLEN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),FILENAME                                                 
*                                                                               
         LA    RE,1(RE,RF)                                                      
         MVI   0(RE),C'"'                                                       
         AHI   RE,1                                                             
*                                                                               
         S     RE,=A(PARM)                                                      
         SHI   RE,2                -2 BYTES FOR PARM LEN                        
         STH   RE,PARM             STORE THE LEN OF PARM                        
*                                                                               
         LINKX EP=BPXBATCH,PARAM=(PARM),VL=1,ERRET=ERRET                        
         XIT1                                                                   
ERRET    DC    H'0'                                                             
PARM     DS    CL255                                                            
         LTORG                                                                  
         EJECT                                                                  
COMMWORK DS    0D                  COMMON STORAGE AREA                          
*                                                                               
INFILE   DCB   DDNAME=INFILE,DSORG=PS,MACRF=GM,EODAD=BDAT90                     
NULLFILE DCB   DDNAME=INFILE,DSORG=PS,MACRF=GM,EODAD=BDAT90                     
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
DMWORK   DS    12D                                                              
DMCB     DS    10F                                                              
DUB      DS    D                                                                
DUB2     DS    D                                                                
PRNTDUB  DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
DATADISP DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
PRNTTIME DS    CL9                                                              
WORK     DS    CL256                                                            
WORKX    DS    CL250               OVERFLOW FOR WORK IN BLDDATF                 
WORK2    DS    CL100                                                            
MSG01    DS    CL20                                                             
ERRCODE  DS    X                                                                
         SPACE 2                                                                
ELCODE   DS    X                                                                
USERFILT DC    H'0'                ONLY XMIT REPORTS FOR THIS USERID            
NUMPAGES DS    H                   NUMBER OF PAGES OF THIS PQ REPORT            
COVPGLNS DS    F                   NUMBER OF LINES IN COVER PAGE                
PACKOF4B DS    PL4                                                              
MAXURPTS DC    F'50'               MAX REPORTS PER USERID IN A ROW              
XMTENTRY DS    XL(XMTTBLQ)         TEMP STORAGE FOR 1 XIMTABLE ENTRY            
LSTPQKEY DS    XL(L'XMTPQKEY)      LAST PRINT QUEUE REPORT SENT                 
SKIPLAST DS    C                   'Y' IF WE MAXED OUT ON A USERID              
DESTTYPE DS    C                   DESTINATION TYPE                             
DESTINAT DS    CL5                 DESTINATION                                  
AGYNAME  DS    CL33                                                             
AGYADDR  DS    CL33                                                             
FAXKEY   DS    CL7                 KEY OF FAX RECORD                            
ROUTCODE DS    CL5                 DARE ROUTING CODE                            
REPCODE  DS    CL3                 DARE REP CODE                                
USERID   DS    CL10                ALPHA USERID                                 
SVAGYPOW DS    CL2                 SAVED AGENCY POWER CODE                      
AGYPOWER DS    CL2                 AGENCY POWER CODE                            
REQUESTR DS    CL3                 REQUESTOR                                    
FAXSUBC  DS    CL6                 FAX RECORD SUBCODE                           
MAJORNAM DS    CL8                 MAJOR RESOURCE NAME                          
EDICTTYP DS    CL1                 EDICT TYPE A/R (USEFUL FOR EDICTT)           
KEY      DS    XL40                CTFILE KEY/GENDIR,GENFIL KEY                 
         SPACE 2                                                                
BPX1OPNQ EQU   156                                                              
BPX1CLOQ EQU   72                                                               
BPX1REDQ EQU   176                                                              
BPX1WRTQ EQU   220                                                              
BPX1OPN  DS    A                                                                
BPX1CLO  DS    A                                                                
BPX1WRT  DS    A                                                                
BPX1RED  DS    A                                                                
*                                                                               
CMDFILE  DS    CL60                                                             
DATAFILE DS    CL60                                                             
PATH     DS    CL60                                                             
RCFILE   DS    CL60                RETURN CODE FILE NAME                        
SOFILE   DS    CL60                STDOUT FILE NAME                             
BUFFALET DC    F'0'                                                             
CFLEN    DS    F                                                                
DFLEN    DS    F                                                                
PHLEN    DS    F                                                                
RCFLEN   DS    F                                                                
SOFLEN   DS    F                                                                
BUFADDR  DS    A                                                                
BUFLEN   DS    F                                                                
ALET     DS    F                                                                
PLIST    DS    13A              CALL PARMLIST WORK AREA                         
RETCODE  DS    F                RETURN CODE (ERRNO)                             
RSNCODE  DS    F                REASON CODE                                     
RETVAL   DS    F                RETURN VALUE (0, -1 OR OTHER)                   
FILEDESC DS    F                FILE DESCRIPTOR                                 
         BPXYFTYP DSECT=NO                                                      
         BPXYOPNF DSECT=NO                                                      
         BPXYMODE DSECT=NO                                                      
         EJECT                                                                  
ECBLST   DS    0F                                                               
ALOOKECB DC    X'00',AL3(0)        A(LOOKECB)                                   
ASTOPECB DC    X'80',AL3(0)        A(STOPECB)                                   
         SPACE 2                                                                
TABLNTRY DS    XL(XMTTBLQ)         SAVED XMIT TABLE KEY                         
OPERSTOP DC    C'N'                'Y' IF OPERATOR WANTS TO STOP                
TRACEFLG DS    C                   'Y' IF DETAILED TRACE WANTED                 
ATCHRCVR DC    C'Y'                'Y' TO ATTACH RECEIVER                       
*                                                                               
FLNAME   DS    CL60                                                             
FLEXT    DS    CL10                                                             
FNLEN    DS    F                                                                
FILENAME DS    CL71                                                             
SUBJECT  DS    CL60                                                             
CUSTREF  DS    CL18                VEDDNNNNNNYYYYMMDD                           
EMAILADR DS    CL60                                                             
DSN      DS    CL30                DSN TO BE SENT INSTEAD OF PQ REPORT          
*                                                                               
BDERTCB  DS    F                   TCB OF ATTACHED SUBTASK EDIBDER              
BDERECB  DC    F'0'                ECB OF ATTACHED SUBTASK EDIBDER              
BDERPARM DC    XL(BDRPRMLQ)'00'    PARAMETERS TO EDIBDER (VIA R1)               
SUBTASK  DC    C'EDIBDER '         RECEIVING SUBTASK NAME                       
RQMGRNAM DS    CL48                RECEIVING SUBTASK MQ Q MANAGER NAME          
RQUENAME DS    CL48                RECEIVING SUBTASK QUEUE NAME                 
CARD     DS    CL80                FOR CONTROL CARDS AND EDICTFIL RECS          
         DS    XL4                 PQ RECORD LENGTH                             
R        DS    CL256               PQ RECORD DATA                               
         DS    XL4                                                              
EDICTHDR DS    CL256               EDICT HDR* RECORD                            
         DS    XL4                                                              
PQRPTHDR DS    CL256               PQ REPORT HEADER                             
EDICTREC DS    CL256                                                            
NEWEDREC DS    CL256                                                            
*                                                                               
*                                                                               
         DC    C'*CMDMAIL*'                                                     
CM00     DC    X'00',C'THE_USER=SHO USR EMAIL "'                                
CM00P1   DC    CL60' ',C'";'                       EMAIL PARM                   
CM01     DC    X'00',C'FILE_TO_SEND=FILE  "'                                    
*CM01P1   DC    CL73' ',C'"'                        FILENAME PARM               
CM01P1   DC    CL64' ',C'"'                        FILENAME PARM                
CM02     DC    X'00',C'PATH "'                                                  
CM02P1   DC    CL60' ',C'";'                       PATH PARM                    
CM03     DC    X'00',C'IF (THE_USER IS NOT EMPTY) THEN'                         
CM04     DC    X'00',C'  DOCLIST=SND TO THE_USER DOC FILE_TO_SEND '             
CM04P1   DC    CL20' '                                                          
CM05     DC    X'00',C'  SBJ "#'                                                
CM05P1   DC    CL60' ',4C' '                       SUBJECT PARM                 
CM05P2   DC    CL18' ',C'";'                       REFERENCE # PARM             
CM06     DC    X'00',C'  COMPLETED_LIST=DOCLIST{DOC COMPLETED};'                
CM07     DC    X'00',C'  IF (COMPLETED_LIST IS NOT EMPTY) THEN'                 
CM08     DC    X'00',C'    display COMPLETED_LIST;'                             
CM09     DC    X'00',C' '                                                       
*CM09     DC    X'00',C'    sho snd recipient "*" last 1 completed;'            
CM10     DC    X'00',C'    exec program "'                                      
CM10P1   DC    CL60' ',C' C";'                     PATH/FUNCTION1 PARM          
CM11     DC    X'00',C'  ELSE'                                                  
CM12     DC    X'00',C'    FAILED_LIST=DOCLIST{DOC FAILED};'                    
CM13     DC    X'00',C'    display FAILED_LIST;'                                
CM14     DC    X'00',C'    exec program "'                                      
CM14P1   DC    CL60' ',C' F";'                     PATH/FUNCTION1 PARM          
CM15     DC    X'00',C'  ENDIF;'                                                
CM16     DC    X'00',C'ELSE'                                                    
CM17     DC    X'00',C'  exec program "echo user NOT IN BDE ";'                 
CM18     DC    X'00',C'    exec program "'                                      
CM18P1   DC    CL60' ',C' E";'                     PATH/FUNCTION1 PARM          
CM19     DC    X'00',C'ENDIF;'                                                  
CM20     EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
         ORG   CM00                                                             
         DC    AL1(CM01-*-1)                                                    
         ORG   CM01                                                             
         DC    AL1(CM02-*-1)                                                    
         ORG   CM02                                                             
         DC    AL1(CM03-*-1)                                                    
         ORG   CM03                                                             
         DC    AL1(CM04-*-1)                                                    
         ORG   CM04                                                             
         DC    AL1(CM05-*-1)                                                    
         ORG   CM05                                                             
         DC    AL1(CM06-*-1)                                                    
         ORG   CM06                                                             
         DC    AL1(CM07-*-1)                                                    
         ORG   CM07                                                             
         DC    AL1(CM08-*-1)                                                    
         ORG   CM08                                                             
         DC    AL1(CM09-*-1)                                                    
         ORG   CM09                                                             
         DC    AL1(CM10-*-1)                                                    
         ORG   CM10                                                             
         DC    AL1(CM11-*-1)                                                    
         ORG   CM11                                                             
         DC    AL1(CM12-*-1)                                                    
         ORG   CM12                                                             
         DC    AL1(CM13-*-1)                                                    
         ORG   CM13                                                             
         DC    AL1(CM14-*-1)                                                    
         ORG   CM14                                                             
         DC    AL1(CM15-*-1)                                                    
         ORG   CM15                                                             
         DC    AL1(CM16-*-1)                                                    
         ORG   CM16                                                             
         DC    AL1(CM17-*-1)                                                    
         ORG   CM17                                                             
         DC    AL1(CM18-*-1)                                                    
         ORG   CM18                                                             
         DC    AL1(CM19-*-1)                                                    
         ORG   CM19                                                             
         DC    AL1(CM20-*-1)                                                    
         ORG                                                                    
         EJECT                                                                  
VTABLE   DS    V                                                                
ENTRYPTS DS    0F                                                               
         DC    Y((ENTRYLSQ-ENTRYSTQ)/60) NUMBER OF TABLE ENTRIES                
         DC    H'60'                     MUST REMAIN AS 60                      
ENTRYSTQ EQU   *                                                                
*                                                                               
LDTABLE  DC    CL8'EDIBHTM '                                                    
         DC    XL52'00'                                                         
ENTRYLSQ EQU   *                                                                
         EJECT                                                                  
                                                                                
TRTABLE  DS    0XL256                                                           
*                                                                               
*                0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                               
         DC    X'000102030405060708090A0B0C0D0E0F' 00-0F                        
         DC    X'101112131415161718191A1B1C1D1E1F' 10-1F                        
         DC    X'202122232425262728292A2B2C2D2E2F' 20-2F                        
         DC    X'303132333435363738393A3B3C3D3E3F' 30-3F                        
         DC    X'6D4142434445464748494A4B6CAD4E6C' 40-4F                        
         DC    X'D55152535455565758595A5B6CBD5E5F' 50-5F                        
         DC    X'606C62636465666768696C6B6C6D6C6C' 60-6F                        
         DC    X'707172737475767778796C6C7C6D7E6C' 70-7F                        
         DC    X'808182838485868788898A8B8C8D8E8F' 80-8F                        
         DC    X'909192939495969798999A9B9C9D9E9F' 90-9F                        
         DC    X'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF' A0-AF                        
         DC    X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF' B0-BF                        
         DC    X'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF' C0-CF                        
         DC    X'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF' D0-DF                        
         DC    X'6CE1E2E3E4E5E6E7E8E9EAEBECEDEEEF' E0-EF                        
         DC    X'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF' F0-FF                        
         EJECT                                                                  
*                                                                               
*          DATA SET SRPQU03    AT LEVEL 003 AS OF 07/02/02                      
         SPACE 2                                                                
VALOCHRS DC    XL16'404E40404E40404E40404E40404E4040'  00-0F **TEMP**           
         DC    XL16'4E40404E40404E40404E40406040407A'  10-1F **TEMP**           
         DC    XL16'40404040404040404040404040404040'  20-2F                    
         DC    XL16'40404040404040404040404040404040'  30-3F                    
         DC    XL16'404040404040404040404A4B4C4D4E4F'  40-4F                    
         DC    XL16'504040404040404040405A5B5C5D5E5F'  50-5F                    
         DC    XL16'606140404040404040406A6B6C6D6E6F'  60-6F                    
         DC    XL16'404040404040404040797A7B7C7D7E7F'  70-7F                    
         DC    XL16'4081828384858687888940404040404E'  80-8F                    
         DC    XL16'40919293949596979899404040404040'  90-9F                    
         DC    XL16'40A1A2A3A4A5A6A7A8A9404E4E404040'  A0-AF                    
         DC    XL16'40404040404040404040404E4E404060'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C9404E4E404040'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D9404040404040'  D0-D1                    
         DC    XL16'E040E2E3E4E5E6E7E8E9404E4E404040'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F97A4040404040'  F0-FF                    
         SPACE 1                                                                
*              EXTRA CHRACTERS ADDED FOR GERMANY                                
*        A1    DOUBLE S SYMBOL                                                  
*        C0    SMALL A UMLAUT                                                   
*        D0    SMALL U UMLAUT                                                   
*        E0    CAPITAL O UMLAUT                                                 
         EJECT                                                                  
* DDDARETAB                                                                     
       ++INCLUDE DDDARETAB                                                      
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
         DC    C'*COVPAG*'                                                      
COVERBUF DS    (COVPGMAX)CL132     BUILD COVER PAGE HERE IF NECESSARY           
COVPGMAX EQU   33                                                               
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDEDICTWRK                                                     
         EJECT                                                                  
* DDDPRINT                                                                      
* DDEDICTFIL                                                                    
* SPDARDARED                                                                    
* DMPRTQL                                                                       
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DDEDICTFIL                                                     
         EJECT                                                                  
       ++INCLUDE SPDARDARED                                                     
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE CTGENSTAD                                                      
         EJECT                                                                  
       ++INCLUDE CTGENAGRD                                                      
         EJECT                                                                  
         PRINT ON                                                               
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
TABLED   DSECT                                                                  
TABCC    DS    X                   PRINT LINE CONTROL CHARACTER                 
TABFLG   DS    X                   FLAG                                         
TABFP2   EQU   X'80'               USED 2ND ADDRESS FOR WIDE REPORT             
TABPTR   DS    AL4                 1ST ADDRESS                                  
TABPTR2  DS    AL4                 2ND ADDRESS FOR WIDE REPORT                  
TABLENQ  EQU   *-TABLED                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016DDEDIBDES 11/20/06'                                      
         END                                                                    
