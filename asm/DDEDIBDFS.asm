*          DATA SET DDEDIBDFS  AT LEVEL 050 AS OF 04/25/14                      
*PHASE EDIBDFSA                                                                 
*INCLUDE BINSR31                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE EDISUB                                                                 
*INCLUDE NUMVAL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE BDESND                                                                 
*INCLUDE CPUINFO                                                                
*                                                                               
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
*                R9 -- COMMON STORAGE AREA (3RD BASE)                 *         
*                RA -- CPRINT                                         *         
*                RB -- PROGRAM BASE                                   *         
*                RC -- COMMON STORAGE AREA BASE                       *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'DDEDIBDFS -- PERFORM BDE FILE TRANSFER'                         
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
         LR    R9,R7                                                            
         AHI   R9,4096                                                          
         USING COMMWORK,RC,R7,R9                                                
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
         MVC   DCBDDNAM-IHADCB(8,RF),=C'BDFTRACE'  DDNAME=BDFTRACE              
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
*                                  CLEAR TASK ENQUEUES                          
         GOTO1 SADTAMGR,DMCB,(0,=CL7'ISGENQ'),C'#07K',0                         
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
         PRNT  STOPPINGBDFR,PRINT=ALWAYS                                        
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
*                                  CLEAR TASK ENQUEUES                          
         GOTO1 SADTAMGR,DMCB,(0,=CL7'ISGENQ'),C'#07K',0                         
*                                                                               
         XBASE                                                                  
*                                                                               
DONTSTOP L     RF,ALOOKECB                                                      
         TM    0(RF),X'40'         MAIN TASK POSTED READY?                      
         BO    *+6                 YES                                          
         DC    H'0'                                                             
         XC    0(4,RF),0(RF)                                                    
*                                                                               
         BRAS  RE,XFERREPS         TRANSMIT ALL BDE REPORTS IN TABLE            
         B     LOOP                WAIT                                         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* Initialize                                                                    
***********************************************************************         
INITIAL  NTR1  BASE=*,LABEL=*                                                   
         PRNT  INITIALIZE,PRINT=ALWAYS                                          
*                                  SET TASK # AND CLEAR TASK ENQUEUES           
         GOTO1 SADTAMGR,DMCB,(0,=CL7'ISGENQ'),C'#07K',0                         
*                                                                               
         L     R1,SLOOKECB         BUILD ECBLIST                                
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
         DROP  R2                                                               
*                                                                               
         SAM31                                                                  
         L     R1,16(,0)           CVT - COMMON VECTOR TABLE                    
         L     R1,544(R1)          CSRTABLE                                     
         L     R1,24(R1)           CSR SLOT                                     
         MVC   BPX1OPN,BPX1OPNQ(R1)     ADDRESS OF THE SERVICE BPX1OPN          
         MVC   BPX1CLO,BPX1CLOQ(R1)     ADDRESS OF THE SERVICE BPX1CLO          
         MVC   BPX1WRT,BPX1WRTQ(R1)     ADDRESS OF THE SERVICE BPX1WRT          
         MVC   BPX1RED,BPX1REDQ(R1)     ADDRESS OF THE SERVICE BPX1RED          
         SAM24                                                                  
*                                                                               
         BRAS  RE,BLDDFILN         BUILD THE DEFAULT FILE NAME                  
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*BUILD DEFAULT FILE NAME                                                        
***********************************************************************         
BLDDFILN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   DFFEDX,MAJORNAM+5   EDICTA/R/T                                   
         GOTO1 =V(DATCON),DMCB,(5,0),(11,DFFDATE)                               
         MVI   DFFDATE+5,C'-'                                                   
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READ PARAMETER CARDS BETWEEN "++BDF" AND "++BDFEND" CARDS                     
*                                                                               
*   *......          "*" IN COLUMN ONE IS A COMMENT CARD                        
*   TRACE=YES        OVERRIDE TRACE FLAG                                        
*   SUBTASK=CCCCCCCC RECEIVING SUBTASK NAME (DEFAULT=EDIBDFR)                   
*   BDE_MQQMGRNAME=CL48  BDE SENDER SERVICES MQ QUEUE MANAGER NAME              
*   BDE_MQQUEUENAME=CL48 BDE SENDER SERVICES MQ QUEUE NAME                      
*   RCVR_MQQMGRNAME=CL48  RECEIVER MQ QUEUE MANAGER NAME (DEFAULT=' ')          
*   RCVR_MQQUEUENAME=CL48 RECEIVER MQ QUEUE NAME (DEFAULT=' ')                  
*   PATH=Cl60        FILE PATH (u/bde1/edicta-ftp, u/bde1/edictr-ftp)           
*                                                                               
*   ATTACHRCVR=NO    ATTACH RECEIVING SUBTASK (DEFAULT=YES)                     
*   USERMAXXMIT=N    MAXIMUM RPTS XMITTED IN A ROW PER USERID (DEF.=50)         
*   USERID=UUUUUUUU  TRANSFER REPORTS FROM THIS USERID ONLY (MAX 50)            
*   EXUSERID=UUUUUUUU EXCLUDE REPORTS FROM THIS USERID ONLY (MAX 50)            
*    (CAN HAVE EITHER USERID OR EXUSERID, BUT NOT BOTH)                         
*                                                                               
***********************************************************************         
READCRDS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
RC10     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++BDF',CARD      LOOK FOR START OF PARAMETERS                 
         BNE   RC10                                                             
*                                                                               
RC20     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++BDFEND',CARD   LOOK FOR END OF PARAMETERS                   
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
         CLC   =C'BDE_MQQMGRNAME=',CARD   BDE SNDR SRVR MQQMGRNAME              
         BNE   *+14                                                             
         MVC   BQMGRNAM,CARD+15                                                 
         B     RC20                                                             
*                                                                               
         CLC   =C'BDE_MQQUEUENAME=',CARD  BDE SNDR SRVR QUEUE NAME              
         BNE   *+14                                                             
         MVC   BQUENAME,CARD+16                                                 
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
         L     R4,=A(IO)                                                        
         MVI   ELCODE,CTDSCELQ     PICK UP HEX USERID                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RF,USERFILT                                                      
         LA    RE,USRFMAXQ                                                      
         OC    0(L'USERFILT,RF),0(RF)     ANY MORE EMPTY ENTRY?                 
         BZ    *+14                       YES - SAVE THE FILTER                 
         AHI   RF,L'USERFILT                                                    
         BCT   RE,*-14                    CHECK FOR NEXT EMPTY ENTRY            
         DC    H'0'                EXCESS THE MAX # OF USERID FILTERS           
         MVC   0(L'USERFILT,RF),CTDSC-CTDSCD(R4)  HEX USERID FILTER             
         B     RC20                                                             
*                                                                               
RC60     CLC   =C'EXUSERID=',CARD  EXUSERID=XXX                                 
         BNE   RC80                                                             
*                                                                               
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         LA    RF,KEY                                                           
         USING CTIREC,RF                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,CARD+9                                                    
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
*                                                                               
         LA    RF,EXUFILT                                                       
         LA    RE,EXUFMAXQ                                                      
         OC    0(L'EXUFILT,RF),0(RF)      ANY MORE EMPTY ENTRY?                 
         BZ    *+14                       YES - SAVE THE FILTER                 
         AHI   RF,L'EXUFILT                                                     
         BCT   RE,*-14                    CHECK FOR NEXT EMPTY ENTRY            
         DC    H'0'                EXCESS THE MAX # OF USERID FILTERS           
         MVC   0(L'EXUFILT,RF),CTDSC-CTDSCD(R4)   HEX USERID NEG FILTER         
         B     RC20                                                             
*                                                                               
RC80     DS    0H                                                               
*                                                                               
RC100    CLC   =C'PATH=',CARD                                                   
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
         CLI   XMTMETH,C'P'        YES -- BDE-FTP METHOD?                       
         BNE   XFER30                                                           
         TM    XMTSTAT,EDFSTWTG    YES -- WAITING TO BE SENT?                   
         BZ    XFER30                                                           
         TM    XMTSTAT,EDFSTLST    YES -- LAST DEST IN LOGICAL REPORT?          
         BZ    XFER30                                                           
*                                                                               
         OC    USERFILT,USERFILT   YES -- IS THERE A USERID FILTER?             
         BZ    XFER21X             NO                                           
         LA    RF,USERFILT                                                      
         LA    RE,USRFMAXQ                                                      
XFER21   OC    0(L'USERFILT,RF),0(RF)    ANY MORE USERID FILTER?                
         BZ    XFER30              NO - NOT A MATCH - NEXT REPORT               
         CLC   XMTUSRID,0(RF)      YES - DOES THIS REPORT MATCH FILTER?         
         BE    XFER28              YES - GOT A MATCH                            
         AHI   RF,L'USERFILT                                                    
         BCT   RE,XFER21           TRY NEXT FILTER                              
         B     XFER30              NOT A MATCH - NEXT REPORT                    
XFER21X  EQU   *                                                                
*                                                                               
         OC    EXUFILT,EXUFILT     IS THERE A NEGATIVE USERID FILTER?           
         BZ    XFER28              NO                                           
         LA    RF,EXUFILT                                                       
         LA    RE,EXUFMAXQ                                                      
XFER22   OC    0(L'EXUFILT,RF),0(RF)    ANY MORE NEG USERID FILTER?             
         BZ    XFER28              NO - NOT A MATCH - OKAY                      
         CLC   XMTUSRID,0(RF)      YES - DOES THIS REPORT MATCH FILTER?         
         BE    XFER30              YES - SKIP THIS REPORT                       
         AHI   RF,L'EXUFILT                                                     
         BCT   RE,XFER22           TRY NEXT FILTER                              
*                                                                               
XFER28   CLI   SKIPLAST,C'Y'       YES -- DID WE HIT THE MAX EARLIER?           
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
         L     R4,=A(IO)                                                        
         MVI   ELCODE,CTDSCELQ     PICK UP ALPHA USERID                         
         BAS   RE,GETEL                                                         
         BNE   XFER110                                                          
         MVC   USERID,CTDSC-CTDSCD(R4)                                          
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
*                                                                               
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
         MVC   SUBJECT,SPACES                                                   
         MVI   SUBJECT,C'%'        MUST HAVE SOMETHING IN SUBJECT FIELD         
         MVC   FLEXT,SPACES                                                     
         MVC   FLEXT(3),=C'htm'                                                 
         MVI   HFSSPCOK,C'Y'       ASSUME HFS SPACE IS OKAY                     
*                                                                               
         BRAS  RE,BLDREF           BUILD CUSTOMER REF #                         
         MVC   DFFREFN,CUSTREF+2                                                
         MVC   DFFEDKEY,EDICTHDR+16                                             
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,XMTUSRID-XMTTABLD+TABLNTRY   USERID #                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DFFUIDN,DUB                                                      
*                                                                               
         MVC   FLNAME,DFFILN                                                    
*                                                                               
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
         BE    SNDM29              MORE REPORT LINES TO BE READ                 
*                                                                               
         CLC   =C'++DDS',R+1       IS THIS LINE A CONTROL CARD?                 
         BNE   SNDM40              NO - BUT ONLY ONE LINE OF DATA               
         B     SNDM160             NO DATA TO SEND, MARK REPORT JUNK            
*                                                                               
SNDM29   CLC   =C'++DDS',R+1       IS THIS LINE A CONTROL CARD?                 
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
         LA    RE,L'SUBJECT                                                     
SNDM32B  CLI   SUBJECT,C'#'                                                     
         BE    SNDM32E                                                          
         CLI   SUBJECT,C' '                                                     
         BNE   SNDM20              NEXT LINE                                    
         MVC   SUBJECT(L'SUBJECT-1),SUBJECT+1                                   
         MVI   SUBJECT+L'SUBJECT-1,C' '                                         
         BCT   RE,SNDM32B                                                       
SNDM32E  MVI   SUBJECT,C'%'                                                     
         B     SNDM20              NEXT LINE                                    
*                                                                               
SNDM34   CLC   =C'FIL',R+12        IS THIS FILE NAME CARD?                      
         BNE   SNDM35              NO -- NEXT LINE                              
         MVC   FLNAME,R+16         FILE NAME                                    
         B     SNDM20              NEXT LINE                                    
*                                                                               
SNDM35   CLC   =C'EXT',R+12        IS THIS FILE TYPE EXTENSION CARD?            
         BNE   SNDM36              NO -- NEXT LINE                              
         MVC   FLEXT,R+16          FILE TYPE EXTENSION                          
         B     SNDM20              NEXT LINE                                    
*                                                                               
SNDM36   CLC   =C'TRN',R+12        IS THIS TRN CARD?                            
         BNE   SNDM37              NO -- NEXT LINE                              
         CLI   R+9,C'#'                                                         
         BE    SNDM36B                                                          
         CLI   R+9,C'$'                                                         
         BE    SNDM36B                                                          
         CLI   R+9,C'+'                                                         
         BE    SNDM36B                                                          
         CLI   R+9,C'A'                                                         
         BL    SNDM20                                                           
SNDM36B  CLI   R+10,C'#'                                                        
         BE    SNDM36D                                                          
         CLI   R+10,C'$'                                                        
         BE    SNDM36D                                                          
         CLI   R+10,C'+'                                                        
         BE    SNDM36D                                                          
         CLI   R+10,C'A'                                                        
         BL    SNDM20                                                           
SNDM36D  CLI   R+11,C'#'                                                        
         BE    SNDM36G                                                          
         CLI   R+11,C'$'                                                        
         BE    SNDM36G                                                          
         CLI   R+11,C'+'                                                        
         BE    SNDM36G                                                          
         CLI   R+11,C'A'                                                        
         BL    SNDM20                                                           
SNDM36G  MVC   FLEXT(3),R+9        SAVE IT FOR DEFAULT FILE EXT                 
         B     SNDM20              NEXT LINE                                    
*                                                                               
SNDM37   CLC   =C'DSN',R+12        IS DATA SET NAME?                            
         BNE   SNDM38              NO -- NEXT LINE                              
         MVC   DSN,R+16                                                         
         B     SNDM20              NEXT LINE                                    
*                                                                               
SNDM38   DS    0H                                                               
         B     SNDM20              NEXT LINE                                    
*&&DO                                                                           
*'DIR', NO LONGER SUPPORTED                                                     
         CLC   =C'DIR',R+12        IS HFS DIRECTORY?                            
         BNE   SNDM20              NO -- NEXT LINE                              
         MVC   DIR,R+16                                                         
         B     SNDM20              NEXT LINE                                    
*&&                                                                             
*                                                                               
SNDM40   BRAS  RE,BLDFILN          BUILD FILENAME WITH EXTENSION                
         BRAS  RE,GETBDEP          RESOLVE THE BDE SENDING OPTIONS              
         BNE   SNDM200             CAN'T PROCESS FILE                           
*                                                                               
         CLI   EDICTHDR+38,C'R'    IS A DIR SUPPLIED?                           
         BNE   SNDM50                                                           
         GOTO1 SADTAMGR,DMCB,=C'OPMSG',=C'AUTONOTE*US-MF_FAC_NOTIFY:BDF+        
               --DIR option is no longer supported.       '                     
         B     SNDM160             MAKE REPORT UNSENDABLE                       
*                                                                               
SNDM50   CLI   EDICTHDR+38,C'D'    IS A DSN SUPPLIED?                           
         BE    SNDM75              -ALL DSN FILE GOES THE NEW WAY!              
         CLI   EDICTHDR+38,C'H'    IS A HFS FILE SUPPLIED?                      
         BE    SNDM75              -ALL HFS FILE GOES THE NEW WAY!              
*                                  -ALL THE REST TOO!!!!!!!!!!!!!               
*                                                                               
         BRAS  RE,BLDDATF2         NEW BDE XML REQUEST VIA MQ                   
         BE    SNDM145                                                          
         B     SNDM200                                                          
*                                                                               
SNDM75   EQU   *                                                                
         BRAS  RE,INTRC            SET RC=START IN RCFILE                       
         BRAS  RE,BATCH2           NEW BDE XML REQUEST VIA MQ                   
*                                                                               
         CLC   =C'RC=START',WORK                                                
         BNE   SNDM100                                                          
         GOTO1 SADTAMGR,DMCB,=C'OPMSG',=C'AUTONOTE*US-MF_FAC_NOTIFY:BDF+        
               REXX EXEC FAILED, PLEASE CHECK MYSTDOUT FOR ERROR'               
         DC    H'0'                ABEND FOR NOW                                
         MVI   OPERSTOP,C'Y'                                                    
         B     SNDMX                                                            
*                                                                               
SNDM100  DS    0H                                                               
         CLC   =C'RC=COMPLETED',WORK                                            
         BNE   SNDM150                                                          
*                                                                               
SNDM145  EQU   *                                                                
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
         MVI   ERRCODE,EDFERDNQ    DSN ERROR                                    
         CLC   =C'RC=DSN ERROR',WORK                                            
         BE    SNDM155             ALRET COMP ROOM ANYWAY                       
*                                                                               
*                                  "RC=ICONV Error'                             
         MVI   ERRCODE,EDFERNDQ    NO DOC ERROR/ICONV ERROR                     
*                                                                               
SNDM155  DS    0H                                                               
         CLC   =C'GRPMAIX',EDICTHDR+16                                          
         BNE   SNDM157                                                          
         GOTO1 =V(LOGIO),DMCB,1,=C'Cannot skip GRPMAIX report. Try agai+        
               n later.        '                                                
         GOTO1 =V(LOGIO),DMCB,0,(1,BYTE)          WTOR MESSAGE                  
         MVI   OPERSTOP,C'Y'                                                    
         GOTO1 SADTAMGR,DMCB,=C'OPMSG',=C'AUTONOTE*US-MF_FAC_NOTIFY,JSH+        
               AN:Cannot send GroupM report! Please check.  '                   
         B     SNDMX                                                            
*                                                                               
SNDM157  DS    0H                                                               
         CLC   =C'KRGNY     SMO',MSG01                                          
         BNE   SNDM158                                                          
         MVC   WORK2,SPACES                                                     
         MVC   WORK2(45),=CL45'CANNOT OPEN DATATSET, CHECK IF IT IS IN +        
               USE: '                                                           
         MVC   WORK2+45(55),DSN                                                 
         GOTO1 =V(LOGIO),DMCB,1,(100,WORK2)                                     
         GOTO1 =V(LOGIO),DMCB,0,(1,BYTE)          WTOR MESSAGE                  
*                                                                               
SNDM158  MVC   WORK2,SPACES                                                     
         MVC   WORK2(12),=CL12'BDF REPORT: '                                    
         MVC   WORK2+12(L'MSG01),MSG01                                          
         MVC   WORK2+12+L'MSG01+1(70),=CL70'CAN''T BE SENT!  REPLY: (R)+        
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
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   SNDMX               CAN'T POST EDICT FILE                        
         MVC   P+30(32),WORK                                                    
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
         DROP  R1                                                               
*                                  BUILD FAILURE NOTIFICATION MESSAGE           
         CLC   BDEFN,SPACES                                                     
         BL    SNDMX               NO EMAIL ADDRESS                             
*                                                                               
         MVC   WORK2,SPACES                                                     
         MVC   WORK2(9),=CL9'AUTONOTE*'                                         
         MVC   WORK2+9(L'BDEFN),BDEFN                                           
*                                                                               
         LA    RE,WORK2+9+L'BDEFN         PT TO THE LAST CHAR                   
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         B     *-10                       RE = A(LAST NON-BLANK CHAR)           
*                                                                               
         MVC   1(21,RE),=CL21':BDF REPORT FAILED - '                            
         MVC   22(L'MSG01,RE),MSG01       REPORT USERID, SUBID & #              
*                                                                               
*&&DO                                                                           
****************DON'T PRINT RC=XXXXX CODE FOR NOW*********                      
         MVC   1(20,RE),=CL20':BDF REPORT FAILED ('                             
         MVC   21(32,RE),WORK                                                   
*                                                                               
         LA    RE,21+32(RE)               PT TO THE LAST CHAR                   
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         B     *-10                       RE = A(LAST NON-BLANK CHAR)           
         MVI   1(RE),C')'                                                       
*                                                                               
         MVI   2(RE),C' '                                                       
         MVC   3(L'MSG01,RE),MSG01        REPORT USERID, SUBID & #              
**********************************************************                      
*&&                                                                             
*                                                                               
         GOTO1 SADTAMGR,DMCB,=C'OPMSG',(L'WORK2,WORK2)                          
*                                                                               
SNDM200  DS    0H                                                               
         CLI   HFSSPCOK,C'Y'       HFS SPACE PROBLEM?                           
         BE    SNDMX               NO                                           
         GOTO1 =V(LOGIO),DMCB,1,=C'No space left in the HFS device.  BD+        
               F subtask will shutdown.  Try again later.'                      
         GOTO1 =V(LOGIO),DMCB,0,(1,BYTE)          WTOR MESSAGE                  
         MVI   OPERSTOP,C'Y'                                                    
*                                                                               
SNDMX    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
BLDDATF2 NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    WORK,WORK                                                        
         CLI   MAJORNAM+5,C'A'                                                  
         BE    BDA220                                                           
         CLI   MAJORNAM+5,C'R'                                                  
         BE    BDA220                                                           
         MVC   WORK(11),=CL11'/u/bdeftpt/'     TEST                             
         LA    RE,11                                                            
         LA    R1,WORK+11                                                       
         B     BDA225                                                           
BDA220   MVC   WORK(10),=CL10'/u/bdeftp/'      LIVE                             
         LA    RE,10                                                            
         LA    R1,WORK+10                                                       
*                                                                               
BDA225   AHI   RE,L'CUSTREF                                                     
         ST    RE,DFLEN                                                         
         MVC   0(L'CUSTREF,R1),CUSTREF                                          
*                                                                               
         SAM31                                                                  
         XC    S_MODE,S_MODE                                                    
         MVI   S_MODE2,S_IRUSR       USER R/W, GROUP R/W, OTHER R/W             
         MVI   S_MODE3,S_IWUSR+S_IRGRP+S_IWGRP+S_IROTH+S_IWOTH                  
         XC    O_FLAGS(OPNF#LENGTH),O_FLAGS                                     
         MVI   O_FLAGS4,O_CREAT+O_RDWR+O_TRUNC                                  
         L     RF,BPX1OPN                                                       
         CALL  (15),                 OPEN A FILE  BPX1OPN              X        
               (DFLEN,               INPUT: PATHNAME LENGTH            X        
               WORK,                 INPUT: PATHNAME                   X        
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
         SAM24                                                                  
*                                                                               
*                                                                               
         LA    R2,79               (MAX 80 CHARACTERS)                          
         CLI   EDICTHDR+35,C'W'    DO WE WANT A WIDE TRANSMISSION?              
         BNE   *+8                                                              
         LA    R2,131              YES (MAX 132 CHARACTERS)                     
*                                                                               
         TM    QLTYPE-PQPLD+PQRPTHDR,QLTYDL  DOWNLOADABLE REPORT?               
         BZ    *+8                                                              
         LA    R2,998              YES (MAX 999 CHARACTERS)                     
         B     BDA250                                                           
*                                                                               
BDA230   LA    R1,DMCB                                                          
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
         BNE   BDA2100                                                          
*                                                                               
BDA250   L     R8,=A(R+1)                                                       
         AR    R8,R2               PT TO THE LAST CHAR IN THIS RECORD           
         LR    RE,R2                                                            
*                                  OMIT THE TAILING BLANKS                      
         CLI   0(R8),C' '                                                       
         BH    *+10                                                             
         BCTR  R8,0                                                             
         BCT   RE,*-10                                                          
         LA    R8,1(R8)                                                         
*                                                                               
         CLI   BDEOP,DESTBUIXQ     UNIX OS?                                     
         BNE   BDA260              NO CARRIAGE RETURN FOR UNIX OS               
         MVI   0(R8),X'15'         NEW LINE                                     
         LA    R8,1(R8)                                                         
         B     BDA270                                                           
*                                                                               
BDA260   MVI   0(R8),X'0D'         CARRIAGE RETURN                              
         MVI   1(R8),X'15'         NEW LINE                                     
         LA    R8,2(R8)                                                         
*                                                                               
BDA270   S     R8,=A(R+1)          IGNORE THE CC CHAR                           
         ST    R8,BUFLEN                                                        
         MVC   BUFADDR,=A(R+1)                                                  
         MVC   ALET,=F'0'                                                       
*                                                                               
         CLI   BDECA,C'Y'          CONVERT TO ASCII?                            
         BNE   BDA280              DON'T DO THE TRANSLATION                     
*                                                                               
         LHI   R0,X'FF'         PREPARE FOR TRE(SET TEST VALUE=X'FF')           
         L     RE,=A(R+1)          TARGET                                       
         LR    RF,R8               LENGTH                                       
         L     R1,=A(ASCIITBL)     EBCDIC TO ASCII TABLE                        
         TRE   RE,R1               TRANSLATE FROM EBCDIC TO ASCII               
         BO    *-4                                                              
*                                                                               
BDA280   EQU   *                                                                
*                                                                               
         SAM31                                                                  
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
         BNL   BDA290                                                           
*                                                                               
         CLC   RETCODE,=XL4'00000085'  NO SPACE LEFT ON THE DEVICE              
         BE    *+6                                                              
         DC    H'0'                CAN'T WRITE TO HFS FILE!                     
         MVI   HFSSPCOK,C'N'       HFS SPACE PROBLEM                            
*                                                                               
BDA290   SAM24                                                                  
         CLI   HFSSPCOK,C'N'       HFS SPACE OKAY?                              
         BE    BDA2CER             NO, EXIT                                     
         B     BDA230                                                           
*                                                                               
BDA2100  DS    0H                                                               
         SAM31                                                                  
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
         SAM24                                                                  
*                                                                               
         BRAS  RE,CALLBSND                                                      
         B     BDA2YES                                                          
*                                                                               
BDA2CER  DS    0H                                                               
         SAM31                                                                  
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
         SAM24                                                                  
         B     BDA2NO                                                           
*                                                                               
BDA2YES  CR    RB,RB               SET CC EQUAL                                 
         B     BDA2X                                                            
BDA2NO   LTR   RB,RB               SET CC NOT EQUAL                             
BDA2X    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
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
*                                  **********************************           
         LA    R1,1(,RE)           SAVE FILENAME LENGTH W/O EXT IN R1           
         LAY   RF,FILENAME              FOR LATER USE                           
         SR    R1,RF               **********************************           
*                                                                               
         CLC   =C'.NONE',FLEXT                                                  
         BE    BFN40               SKIP ANY FILE EXT.                           
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
BFN40    AHI   RE,1                                                             
         S     RE,=A(FILENAME)                                                  
         ST    RE,FNLEN                                                         
*                                  **********************************           
         CLC   DFFILN(12),FILENAME ONLY TR FILENAME, NOT INCLUDE EXT            
         BNE   BFN50                   IF DEFAULT FILENAME IS USED.             
         LR    RE,R1               USE LENGTH W/O EXT                           
*                                  **********************************           
*                                                                               
*                                  TR CHARS THAT MAY CAUSE PROBLEM              
BFN50    BCTR  RE,0                                                             
         L     RF,=A(TRTABLE)                                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         TR    FILENAME(0),0(RF)                                                
*                                                                               
BFNX     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
CALLBSND NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LAY   RE,SUBJECT          REPLACE & IN SUBJECT WITH +                  
         LA    RF,L'SUBJECT-1                                                   
CBSND20  CLI   0(RE),C'&&'                                                      
         JNE   CBSND20N                                                         
         MVI   0(RE),C'+'                                                       
CBSND20N AHI   RE,1                                                             
         BCT   RF,CBSND20                                                       
*                                                                               
         LAY   R3,BDECBLK                                                       
         USING BDESNDD,R3                                                       
         MVC   BSNCNM,BDECN         BDE COMMAN NAME  (CL40)                     
         MVC   BSNENC,BDEEN         BDE ENCRYPTION   (CL1)                      
         MVC   BSNCMP,BDECM         BDE COMPRESS Y/N (CL1)                      
         MVC   BSNQMGR,BQMGRNAM     QMGR BDE SENDER  (CL48)                     
         MVC   BSNQNAM,BQUENAME     QUEUE BDE SENDER (CL48)                     
         MVC   BSNSUB,SPACES                                                    
         OC    SUBJECT,SPACES                                                   
         MVC   BSNSUB(L'SUBJECT),SUBJECT     SUBJECT (CL60)                     
         MVC   BSNRID,SPACES                                                    
         MVC   BSNRID(L'CUSTREF),CUSTREF     REQUEST ID (CL18)                  
         MVC   BSNAPN,SPACES                                                    
         MVC   BSNAPN(10),=CL10'EDICT-BDF'   APPLICATION NAME (CL10)            
         MVC   BSNFIL,SPACES                                                    
         MVC   BSNFIL(L'FILENAME),FILENAME   FILENAME(CL71)                     
         MVI   BSNINFS,C'S'         FILE ALREADY in /u/bdeftp(t)                
         MVC   BSNINFN(L'CUSTREF),CUSTREF    CUSTREF AS FILENAME(CL18)          
         DROP  R3                                                               
*                                                                               
         GOTO1 =V(BDESND),DMCB,=C'OPEN',BDECBLK,BSNDDQ,0,0,0                    
         TM    DMCB+12,X'80'                                                    
         JO    *+2                                                              
         GOTO1 =V(BDESND),DMCB,=C'CLOSE',0,0,0,0,0                              
         TM    DMCB+12,X'80'                                                    
         JO    *+2                                                              
*                                                                               
CBSNDX   XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
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
*                                                                               
BREFX    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
GETBDEP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,DMCB                                                          
         USING DSTPARMD,R1                                                      
         LA    RF,EDICTHDR+16      FIND MATCH ON EDICT= KEY                     
         ST    RF,DSTKEY                                                        
         MVC   DSTTBL,SDSTTBLE                                                  
         MVC   DSTMAJNM,SMAJORNM                                                
         GOTO1 =V(FINDDEST)                                                     
         BE    GBP10                                                            
         DROP  R1                                                               
*                                                                               
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         MVC   ETBLNTRY,=A(TABLNTRY)                                            
         MVI   EACTION,EACTJNKQ    MARK UNSENDABLE                              
         MVI   EERRORCD,EDFERNED   EDICT RECORD WAS DELETED                     
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   GBPNO               CAN'T POST EDICT FILE                        
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
         B     GBPNO                                                            
         DROP  R1                                                               
*                                                                               
*                                                                               
         USING DESTTABD,R1                                                      
GBP10    MVC   BDECN,DESTBDECN      BDE COMMAN NAME                             
         MVC   BDEOP,DESTBDEOP      BDE RECEIVER'S OPERATING SYSTEM             
         MVC   BDEEN,DESTBDEEN      BDE ENCRYPTION (NONE,BLOWFISH,3DES)         
         MVC   BDECM,DESTBDECM      BDE COMPRESS (Y/N)                          
         MVC   BDESF,DESTBDESF      BDE DELETE SENT FILE (Y/N)                  
         MVC   BDECA,DESTBDECA      BDE CONVERT TO ASCII (Y/N)                  
         MVC   BDECP,DESTBDECP      BDE CODE PAGE                               
         MVC   BDEFN,DESTBDEFN      BDE FAILURE NOTIFICATION EMAIL ADDR         
         MVC   BDEBI,DESTBDEBI      BDE BINARY DATA TRANSFER (Y/N)              
         DROP  R1                                                               
*                                                                               
***************FOR TRACTING*******************************************          
         MVC   CR00P1,BDECN                                                     
         MVC   CR01P1,SUBJECT                                                   
         MVC   CR01P2,CUSTREF                                                   
         MVC   CR02P1(L'DSN),DSN                                                
         MVC   CR03P1,FILENAME                                                  
         MVC   CR04P1(1),BDECM                                                  
         MVC   CR05P1(1),BDESF                                                  
*                                                                               
         CLI   BDEEN,DESTBNOQ                                                   
         BNE   *+14                                                             
         MVC   CR06P1(10),=CL10'NO_CRYPT'                                       
         B     GBP10X                                                           
         CLI   BDEEN,DESTBBFQ                                                   
         BNE   *+14                                                             
         MVC   CR06P1(10),=CL10'BLOWFISH'                                       
         B     GBP10X                                                           
         CLI   BDEEN,DESTB3DQ                                                   
         BNE   *+10                                                             
         MVC   CR06P1(10),=CL10'TRIPLE_DES'                                     
GBP10X   DS    0H                                                               
*                                                                               
         LA    R3,CR00                                                          
GBP30    CLI   0(R3),X'FF'                                                      
         BE    GBP30X                                                           
         MVC   P,SPACES                                                         
         ZIC   R2,0(R3)                                                         
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   P(0),1(R3)                                                       
         LA    R3,2(R2,R3)                                                      
         GOTO1 =V(PRINTER)                                                      
         B     GBP30                                                            
GBP30X   DS    0H                                                               
***************FOR TRACTING*******************************************          
*                                                                               
*                                                                               
GBPYES   CR    RB,RB               SET CC EQUAL                                 
         B     GBPX                                                             
GBPNO    LTR   RB,RB               SET CC NOT EQUAL                             
GBPX     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
INTRC    NTR1  BASE=*,LABEL=*                                                   
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
         SAM31                                                                  
         XC    S_MODE,S_MODE                                                    
         MVI   S_MODE2,S_IRUSR       USER READ/WRITE, GROUP READ,               
         MVI   S_MODE3,S_IWUSR+S_IRGRP+S_IROTH        OTHER READ                
         XC    O_FLAGS(OPNF#LENGTH),O_FLAGS                                     
         MVI   O_FLAGS4,O_CREAT+O_RDWR+O_TRUNC                                  
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
         MVC   WORK(8),=CL8'RC=START'                                           
         MVC   BUFLEN,=F'8'          READ BUFFER LENGTH                         
         MVC   BUFADDR,=A(WORK)                                                 
         L     RF,BPX1WRT                                                       
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
         SAM24                                                                  
*                                                                               
IRCX     XIT1                                                                   
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
         SAM31                                                                  
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
         SAM24                                                                  
*                                                                               
CRCX     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
BATCH2   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   PARM2,C' '                                                       
         MVC   PARM2+1(200),PARM2                                               
         MVC   PARM2+200(200),PARM2                                             
*                                                                               
         MVC   PARM2+2(3),=C'SH '                                               
         L     RE,PHLEN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PARM2+5(0),PATH                                                  
*                                                                               
         LA    RE,PARM2+6(RE)                                                   
         MVC   0(15,RE),=C'/newmain2.rex '''                                    
         AHI   RE,15                                                            
*                                                                               
         L     RF,FNLEN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),FILENAME                                                 
*                                                                               
         LA    RE,1(RE,RF)                                                      
         MVC   0(2,RE),=C''' '                                                  
         AHI   RE,2                                                             
*                                                                               
         OC    BDECA,BDECA                                                      
         BNZ   *+8                                                              
         MVI   BDECA,C'N'                                                       
         MVC   0(L'BDECA,RE),BDECA                                              
         AHI   RE,L'BDECA+1                                                     
*                                                                               
         OC    BDECP,BDECP                                                      
         BNZ   *+10                                                             
         MVC   BDECP,=CL10'ISO8859-1'   USE DEFAULT CODE PAGE                   
         MVC   0(L'BDECP,RE),BDECP                                              
         AHI   RE,L'BDECP+1                                                     
*                                                                               
         OC    BDEOP,BDEOP                                                      
         BNZ   *+8                                                              
         MVI   BDEOP,C'?'                                                       
         MVC   0(L'BDEOP,RE),BDEOP                                              
         AHI   RE,L'BDEOP+1                                                     
*                                                                               
         CLI   EDICTHDR+38,C'D'    IS A DSN SUPPLIED?                           
         BE    BAT2_40                                                          
         CLI   EDICTHDR+38,C'H'    IS A HFS FILE SUPPLIED?                      
         BE    BAT2_40                                                          
*                                                                               
         MVC   0(3,RE),=CL3'X X'   NO DSNFLAG AND DSN                           
         AHI   RE,4                                                             
         B     BAT2_50                                                          
*                                                                               
BAT2_40  MVC   0(1,RE),EDICTHDR+38                                              
         MVC   2(L'DSN,RE),DSN                                                  
         AHI   RE,L'DSN+3                                                       
*                                                                               
BAT2_50  OC    BDEBI,BDEBI                                                      
         BNZ   *+8                                                              
         MVI   BDEBI,C'N'                                                       
         MVC   0(L'BDEBI,RE),BDEBI    BINARY FLAG                               
         AHI   RE,L'BDEBI+1                                                     
*                                                                               
         MVC   0(L'CUSTREF,RE),CUSTREF  USE CUSTOM REF# AS FILENAME             
         AHI   RE,L'CUSTREF+1                                                   
*                                                                               
         OC    BDESF,BDESF                                                      
         BNZ   *+8                                                              
         MVI   BDESF,C'N'                                                       
         MVC   0(L'BDESF,RE),BDESF    DELETE_AFTER_SENT FLAG                    
         AHI   RE,L'BDESF+1                                                     
*                                                                               
         MVC   0(2,RE),=CL2'>>'                                                 
         AHI   RE,2                                                             
*                                                                               
         L     R1,PHLEN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),PATH                                                     
         A     RE,PHLEN                                                         
*                                                                               
         MVC   0(9,RE),=CL9'/mystdout'                                          
         AHI   RE,9                                                             
*                                                                               
         S     RE,=A(PARM2)                                                     
         SHI   RE,2                -2 BYTES FOR PARM2 LEN                       
         STH   RE,PARM2            STORE THE LEN OF PARM2                       
*                                                                               
         LINKX EP=BPXBATCH,PARAM=(PARM2),VL=1,ERRET=ERRET                       
         ST    RF,FULL                                                          
*                                                                               
*&&DO                                                                           
         XC    BPXECB2,BPXECB2        clear ecb                                 
         ATTACH EP=ASMREXX,ECB=BPXECB2,PARAM=PARM2,SZERO=NO                     
         LTR   RF,RF                                                            
         BZ    BAT2_71                                                          
         DC    H'0'                                                             
BAT2_71  ST    R1,TCBADDR2                                                      
         WAIT  ECB=BPXECB2                                                      
         DETACH TCBADDR2                                                        
*                                                                               
         MVC   P(35),=CL35'Return code from BPXBATCH:'                          
         GOTO1 =V(HEXOUT),DMCB,BPXECB2,P+35,4,=C'TOG'                           
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,BPXECB2+1     IGNORE 1ST BYTE                               
         BZ    BAT2_90           NO ERRORS                                      
*                                                                               
         CHI   RF,X'0100'                                                       
         BL    BAT2_80                                                          
         CHI   RF,X'0F00'                                                       
         BNH   BAT2_X            X'100' - X'F00' REXX ERROR, CHK RCFILE         
BAT2_80  EQU   *                                                                
         MVC   P(35),=CL35'BPXBATCH ERROR'                                      
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
*&&                                                                             
*                                                                               
BAT2_90  DS    0H                                                               
         BRAS  RE,CHKRC            CHECK RC FILE                                
         CLC   =C'RC=COMPLETED',WORK                                            
         BNE   BAT2_X                                                           
         BRAS  RE,CALLBSND         DON'T SEND MQ MSG IF ANY ERROR               
*                                                                               
BAT2_X   XIT1                                                                   
*                                                                               
BPXECB2  DC    F'0'                                                             
TCBADDR2 DS    A                                                                
ERRET    DC    H'0'                                                             
PARM2    DS    CL400                                                            
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
COMMWORK DS    0D                  COMMON STORAGE AREA                          
*                                                                               
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
WORK2    DS    CL100                                                            
MSG01    DS    CL20                                                             
         SPACE 2                                                                
ELCODE   DS    X                                                                
ERRCODE  DS    X                                                                
*                                                                               
USERFILT DC    (USRFMAXQ)H'0'      OPTIONAL HEX USERID FILTERS                  
USRFMAXQ EQU   51                  MAX NUMBER OF USERID FILTERS                 
EXUFILT  DC    (EXUFMAXQ)H'0'      OPTIONAL HEX NEGATIVE USERID FILTERS         
EXUFMAXQ EQU   51                  MAX NUMBER OF NEG USERID FILTERS             
*                                                                               
NUMPAGES DS    H                   NUMBER OF PAGES OF THIS PQ REPORT            
MAXURPTS DC    F'50'               MAX REPORTS PER USERID IN A ROW              
XMTENTRY DS    XL(XMTTBLQ)         TEMP STORAGE FOR 1 XIMTABLE ENTRY            
LSTPQKEY DS    XL(L'XMTPQKEY)      LAST PRINT QUEUE REPORT SENT                 
SKIPLAST DS    C                   'Y' IF WE MAXED OUT ON A USERID              
USERID   DS    CL10                ALPHA USERID                                 
MAJORNAM DS    CL8                 MAJOR RESOURCE NAME                          
EDICTTYP DS    CL1                 EDICT TYPE A/R (USEFUL FOR EDICTT)           
KEY      DS    XL25                CTFILE KEY                                   
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
BDECN    DS    CL(L'DESTBDECN)     BDE COMMAN NAME                              
BDEOP    DS    CL(L'DESTBDEOP)     BDE RECEIVER'S OPERATING SYSTEM              
BDEEN    DS    CL(L'DESTBDEEN)     BDE ENCRYPTION (NONE,BLOWFISH,3DES)          
BDECM    DS    CL(L'DESTBDECM)     BDE COMPRESS (Y/N)                           
BDESF    DS    CL(L'DESTBDESF)     BDE DELETE SENT FILE (Y/N)                   
BDECA    DS    CL(L'DESTBDECA)     BDE CONVERT TO ASCII (Y/N)                   
BDECP    DS    CL(L'DESTBDECP)     BDE CODE PAGE                                
BDEFN    DS    CL(L'DESTBDEFN)     BDE FAILURE NOTIFICATION EMAIL ADDR          
BDEBI    DS    CL(L'DESTBDEBI)     BDE BINARY DATA TRANSFER (Y/N)               
*                                                                               
*                                                                               
DATAFILE DS    CL60                                                             
PATH     DS    CL60                                                             
RCFILE   DS    CL60                RETURN CODE FILE NAME                        
BUFFALET DC    F'0'                                                             
DFLEN    DS    F                                                                
PHLEN    DS    F                                                                
RCFLEN   DS    F                                                                
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
HFSSPCOK DC    C'Y'                'Y' IF HFS SPACE IS OKAY                     
TRACEFLG DS    C                   'Y' IF DETAILED TRACE WANTED                 
ATCHRCVR DC    C'N'                'Y' TO ATTACH RECEIVER                       
*                                                                               
*        DDSFTP-EDICTX-EDICTKEY-UID00000-REFNUM##-MMMDD-YY                      
DFFILN   DS    0CL60               DEFAULT FILENAME W/O EXT                     
         DC    CL6'DDSFTP'                                                      
         DC    C'-'                                                             
         DC    CL5'EDICT'                                                       
DFFEDX   DC    C'X'                                                             
         DC    C'-'                                                             
DFFEDKEY DC    CL8'EDICTKEY'                                                    
         DC    C'-'                                                             
         DC    CL3'UID'                                                         
DFFUIDN  DC    CL5'00000'                                                       
         DC    C'-'                                                             
DFFREFN  DC    CL8'REFERNUM'                                                    
         DC    C'-'                                                             
DFFDATE  DC    CL8'MMMDD-YY'                                                    
         DC    (*-DFFILN)C' '                                                   
*                                                                               
FLNAME   DS    CL60                                                             
FLEXT    DS    CL10                                                             
FNLEN    DS    F                                                                
FILENAME DS    CL71                                                             
SUBJECT  DS    CL60                                                             
CUSTREF  DS    CL18                VEDDNNNNNNYYYYMMDD                           
EMAILADR DS    CL60                                                             
DSN      DS    CL60                DSN TO BE SENT INSTEAD OF PQ REPORT          
*                                                                               
BDERTCB  DS    F                   TCB OF ATTACHED SUBTASK EDIBDER              
BDERECB  DC    F'0'                ECB OF ATTACHED SUBTASK EDIBDER              
BDERPARM DC    XL(BDRPRMLQ)'00'    PARAMETERS TO EDIBDER (VIA R1)               
SUBTASK  DC    C'EDIBDFR '         RECEIVING SUBTASK NAME                       
BQMGRNAM DS    CL48                BDE SNDR SERVICE MQ Q MANAGER NAME           
BQUENAME DS    CL48                BDE SNDR SERVICE QUEUE NAME                  
RQMGRNAM DS    CL48                RECEIVING SUBTASK MQ Q MANAGER NAME          
RQUENAME DS    CL48                RECEIVING SUBTASK QUEUE NAME                 
CARD     DS    CL80                FOR CONTROL CARDS AND EDICTFIL RECS          
         DS    XL4                 PQ RECORD LENGTH                             
R        DS    CL1000              PQ RECORD DATA                               
         DS    XL4                 ROOM FOR CR AND LF                           
EDICTHDR DS    CL256               EDICT HDR* RECORD                            
         DS    XL4                                                              
PQRPTHDR DS    CL256               PQ REPORT HEADER                             
*                                                                               
BDECBLK  DC    (BSNDDQ)X'00'                                                    
*                                                                               
         DC    C'*BDEPARM*'                                                     
CR00     DC    X'00',C'COMMONNAME='                                             
CR00P1   DC    CL60' '                             COMMAN NAME PARM             
CR01     DC    X'00',C'SUBJECT='                                                
CR01P1   DC    CL60' ',C' '                        SUBJECT PARM                 
CR01P2   DC    CL18' '                             REFERENCE # PARM             
CR02     DC    X'00',C'INPUT FILE='                                             
CR02P1   DC    CL60' '                             INPUT FILE                   
CR03     DC    X'00',C'FILENAME='                                               
CR03P1   DC    CL60' '                             FILENAME PARM                
CR04     DC    X'00',C'PRECOMPRESS='                                            
CR04P1   DC    CL60' '                             PRECOMPRESS PARM             
CR05     DC    X'00',C'DELSENTFILES='                                           
CR05P1   DC    CL60' '                             DELSENTFILES PARM            
CR06     DC    X'00',C'ENCRYPTION='                                             
CR06P1   DC    CL60' '                             ENCRYPTION  PARM             
CR07     EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
         ORG   CR00                                                             
         DC    AL1(CR01-*-1)                                                    
         ORG   CR01                                                             
         DC    AL1(CR02-*-1)                                                    
         ORG   CR02                                                             
         DC    AL1(CR03-*-1)                                                    
         ORG   CR03                                                             
         DC    AL1(CR04-*-1)                                                    
         ORG   CR04                                                             
         DC    AL1(CR05-*-1)                                                    
         ORG   CR05                                                             
         DC    AL1(CR06-*-1)                                                    
         ORG   CR06                                                             
         DC    AL1(CR07-*-1)                                                    
         ORG                                                                    
         EJECT                                                                  
TRTABLE  DS    0XL256                                                           
*                                                                               
*                0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                               
         DC    X'000102030405060708090A0B0C0D0E0F' 00-0F                        
         DC    X'101112131440161718191A1B1C1D1E1F' 10-1F                        
         DC    X'202122232425262728292A2B2C2D2E2F' 20-2F                        
         DC    X'303132333435363738393A3B3C3D3E3F' 30-3F                        
         DC    X'6D4142434445464748494A4B4C4D4E4F' 40-4F                        
         DC    X'505152535455565758595A5B5C5D5E5F' 50-5F                        
         DC    X'606D62636465666768696A6B6C6D6E6F' 60-6F                        
         DC    X'707172737475767778797A6C7C6D7E7F' 70-7F                        
         DC    X'808182838485868788898A8B8C8D8E8F' 80-8F                        
         DC    X'909192939495969798999A9B9C9D9E9F' 90-9F                        
         DC    X'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF' A0-AF                        
         DC    X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF' B0-BF                        
         DC    X'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF' C0-CF                        
         DC    X'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF' D0-DF                        
         DC    X'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF' E0-EF                        
         DC    X'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF' F0-FF                        
         EJECT                                                                  
ASCIITBL DS    0XL256      EBCDIC TO ASCII TABLE (CODE PAGE 1252)               
*                          12/28/04 GIVEN FROM DAVID SCALESE                    
*                                                                               
*                0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                               
         DC    X'00010203CF09D37FD4D5C30B0C0D0E0F' 00-0F                        
         DC    X'10111213C70A08C91819CCCD831DD21F' 10-1F                        
         DC    X'81821C84860A171B89919295A2050607' 20-2F                        
         DC    X'E0EE16E5D01EEA048AF6C6C21415C11A' 30-3F                        
         DC    X'2020E180EB909FE2AB8BA22E3C282B7C' 40-4F                        
         DC    X'26A95E9CDBA599E3A89E21242A293BAC' 50-5F                        
         DC    X'2D2FDFDC9ADDDE989DACA62C255F3E3F' 60-6F                        
         DC    X'D78894B0B1B2FCD6FB603A2340273D22' 70-7F                        
         DC    X'F861626364656667686996A4F3AFAEC5' 80-8F                        
         DC    X'8C6A6B6C6D6E6F7071729787CE93F180' 90-9F                        
         DC    X'C87E737475767778797AEFC0DA5BF2F9' A0-AF                        
         DC    X'B5B6FDB7B8B9E6BBBCBD5BD9BF5DA8C4' B0-BF                        
         DC    X'7B414243444546474849A8A8BEA8A8A8' C0-CF                        
         DC    X'7D4A4B4C4D4E4F505152A1AD81A8A38F' D0-DF                        
         DC    X'5C20535455565758595AA0858EA8A8D1' E0-EF                        
         DC    X'30313233343536373839B3A89AA8A7A8' F0-FF                        
         EJECT                                                                  
*                                                                               
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
         EJECT                                                                  
       ++INCLUDE DDEDICTWRK                                                     
         EJECT                                                                  
* DDDPRINT                                                                      
* DDEDICTFIL                                                                    
* DMPRTQL                                                                       
* CTGENFILE                                                                     
* DDBDESNDD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DDEDICTFIL                                                     
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE DDBDESNDD                                                      
         EJECT                                                                  
         PRINT ON                                                               
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050DDEDIBDFS 04/25/14'                                      
         END                                                                    
