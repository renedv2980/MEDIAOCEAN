*          DATA SET DDEDIQ2QS  AT LEVEL 024 AS OF 05/01/02                      
*PHASE EDIQ2QSA                                                                 
*INCLUDE BINSR31                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE EDISUB                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        EDIQ2QS -- TRANSMIT PQ REPORTS VIA Q2Q               *         
*                                                                     *         
*  COMMENTS:     CALLED BY EDICT                                      *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- PARAMETERS FROM EDICT                          *         
*                R6 -- PARAMETERS TO EDIQ2R                           *         
*                R7 -- PROGRAM 2ND BASE                               *         
*                R8 -- WORK                                           *         
*                R9 -- WORK                                           *         
*                RA -- CPRINT                                         *         
*                RB -- PROGRAM BASE                                   *         
*                RC -- WORK                                           *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'DDEDIQ2QS -- PERFORM Q2Q TRANSMISSIONS'                         
         MACRO                                                                  
&NAME    PRNT  &A,&PRINT=                                                       
&NAME    MVC   P(17),=CL17'&A'                                                  
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
EDIQ2QS  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*EDIQ2QS,=A(R13CHAIN),R7                                       
*                                                                               
         LR    R5,RC                                                            
         SH    R5,=H'4'                                                         
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
         MVC   DCBDDNAM-IHADCB(8,RF),=C'Q2QTRACE'  DDNAME=Q2QTRACE              
*                                                                               
         MVC   TITLE(34),=C'EDICT: SENDING SUBTASK TO PQ-TO-PQ'                 
         PRNT  START_SUBTASK,PRINT=ALWAYS                                       
*                                                                               
         LA    R6,Q2RPARMS                                                      
         USING Q2QPARMD,R6                                                      
         LA    RF,QSUBECB                                                       
         STCM  RF,7,ECBLSTQ+1      A(QSUBECB)                                   
*                                                                               
         BAS   RE,READCRDS         READ PARAMETER CARDS                         
*                                                                               
         BAS   RE,INITIAL          INITIALIZE                                   
*                                                                               
         LA    R2,SUBTASK          A(RECEIVING SUBTASK NAME)                    
         ATTACH EPLOC=(R2),ECB=Q2RECB,PARAM=Q2RPARMS,SZERO=NO                   
         ST    R1,Q2RTCB                                                        
         OC    Q2RTCB,Q2RTCB                                                    
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
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
STOP     PRNT  STOPPINGQ2R,PRINT=ALWAYS                                         
         POST  QSTOPECB            TELL EDIQ2QR TO STOP                         
         WAIT  ECB=Q2RECB                                                       
         TM    Q2RECB,X'40'                                                     
         BO    *+6                                                              
         DC    H'0'                                                             
         DETACH Q2RTCB                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
*                                                                               
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
         BAS   RE,XFERREPS         TRANSMIT ALL Q2Q REPORTS IN TABLE            
*                                                                               
         B     LOOP                WAIT                                         
         EJECT                                                                  
INITIAL  NTR1                                                                   
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
XIT      XIT1                                                                   
         EJECT                                                                  
READCRDS NTR1                                                                   
*                                                                               
* READ PARAMETER CARDS BETWEEN "++Q2Q" AND "++Q2QEND" CARDS                     
*                                                                               
*   *......          "*" IN COLUMN ONE IS A COMMENT CARD                        
*   TRACE=YES        OVERRIDE TRACE FLAG                                        
*   SUBTASK=CCCCCCCC RECEIVING SUBTASK NAME (DEFAULT=EDIQ2QR)                   
*                                                                               
RC10     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++Q2Q',CARD      LOOK FOR START OF PARAMETERS                 
         BNE   RC10                                                             
*                                                                               
RC20     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++Q2QEND',CARD   LOOK FOR END OF PARAMETERS                   
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
         CLC   =C'TRACE=',CARD     TRACE=                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'NO',CARD+6                                                    
         BE    RC20                DON'T OVERRIDE                               
         CLC   =C'YES',CARD+6                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRACEFLG,C'Y'       TRACE=YES                                    
         B     RC20                                                             
*                                                                               
RCX      GOTO1 =V(PRINTER)         SKIP A LINE                                  
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
XFERREPS NTR1                                                                   
*                                                                               
         L     R8,SMAJORNM         MAJOR RESOURCE NAME                          
         LA    R9,=C'XMTTABLE'     MINOR RESOURCE NAME                          
*                                                                               
XFER10   MVC   P+30(8),0(R8)                                                    
         MVC   P+40(8),0(R9)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   ((8),(9),E,8)       ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         L     R2,SXMTTBLE         R2 = A(REPORT TABLE)                         
         USING XMTTABLD,R3                                                      
         LA    R3,XMTENTRY                                                      
         LA    R4,WORK                                                          
         XC    WORK,WORK           WORK = EARLIEST ENTRY --> FIFO               
*                                                                               
XFER20   MVC31 XMTENTRY,0(R2)                                                   
         CLI   0(R3),0             END OF TABLE?                                
         BE    XFER40              YES, NO MORE                                 
         CLI   XMTSOURC,XMTSRCPQ   PRINT QUEUE ENTRY?                           
         BNE   XFER30                                                           
         CLI   XMTMETH,C'Q'        YES -- Q2Q METHOD?                           
         BNE   XFER30                                                           
         TM    XMTSTAT,EDFSTWTG    YES -- WAITING TO BE SENT?                   
         BZ    XFER30                                                           
*                                                                               
         OC    WORK,WORK           IS THIS THE FIRST ONE WE'VE CHECKED?         
         BZ    *+14                YES, SO SAVE THE DAY/TIME                    
         CLC   XMTCRDTM-XMTTABLD(,R4),XMTCRDTM                                  
         BNH   XFER30                                                           
         MVC   WORK(XMTTBLQ),XMTENTRY   EARLIEST ENTRY FOUND SO FAR             
*                                                                               
XFER30   AHI   R2,XMTTBLQ          BUMP TO NEXT REPORT                          
         B     XFER20                                                           
         DROP  R3                                                               
*                                                                               
XFER40   OC    WORK,WORK           ANYTHING FOUND TO SEND?                      
         BZ    XFER60              NO                                           
         MVC   TABLNTRY,0(R4)      SAVE XMIT TABLE KEY                          
*                                                                               
         MVC   P+30(8),0(R8)                                                    
         MVC   P+40(8),0(R9)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   ((8),(9),8)         DEQUEUE THE TRANSMIT TABLE                   
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
         BE    XFER45                                                           
         DROP  R1                                                               
*                                                                               
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         MVC   ETBLNTRY,=A(TABLNTRY)                                            
         MVI   EACTION,EACTJNKQ    MARK REPORT UNSENDABLE                       
         MVI   EERRORCD,EDFERNPQ   PRINT QUEUE REPORT NOT FOUND                 
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   XFER50                                                           
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
         B     XFER50                                                           
         DROP  R1                                                               
*                                                                               
XFER45   LA    R1,DMCB                                                          
         USING DSTPARMD,R1                                                      
         LA    RF,EDICTHDR+16      FIND MATCH ON EDICT= KEY                     
         ST    RF,DSTKEY                                                        
         MVC   DSTTBL,SDSTTBLE                                                  
         MVC   DSTMAJNM,SMAJORNM                                                
         GOTO1 =V(FINDDEST)                                                     
         BE    XFER48                                                           
         DROP  R1                                                               
*                                                                               
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         MVC   ETBLNTRY,=A(TABLNTRY)                                            
         MVI   EACTION,EACTJNKQ    MARK REPORT UNSENDABLE                       
         MVI   EERRORCD,EDFERNED   EDICT RECORD WAS DELETED                     
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   XFER50                                                           
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
         B     XFER50                                                           
         DROP  R1                                                               
*                                                                               
         USING DESTTABD,R1                                                      
XFER48   MVC   DSTNAME,DESTNAME    DESTINATION                                  
         MVC   DSTUIDN,DESTUIDN                                                 
         DROP  R1                                                               
*                                                                               
         LA    R3,TABLNTRY                                                      
         USING XMTTABLD,R3                                                      
         MVC   P+30(8),DSTNAME     PRINT DESTINATION                            
         MVC   P+40(3),XMTSUBID    PRINT PQ SUBID                               
         EDIT  XMTREFNO,(5,P+45),ALIGN=LEFT                                     
         GOTO1 =V(HEXOUT),DMCB,XMTDSKAD,P+53,4,=C'TOG'                          
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
         PRNT  XFERONEREPORT,PRINT=ALWAYS                                       
*                                                                               
         BAS   RE,SENDQ2Q                                                       
*                                                                               
XFER50   L     RF,ASTOPECB                                                      
         TM    0(RF),X'40'         OPERATOR WANTS TO STOP?                      
         BZ    XFER10              NO                                           
*                                                                               
         XC    0(4,RF),0(RF)                                                    
         MVI   OPERSTOP,C'Y'                                                    
         B     XFERX                                                            
*                                                                               
XFER60   MVC   P+30(8),0(R8)                                                    
         MVC   P+40(8),0(R9)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   ((8),(9),8)         DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
XFERX    B     XIT                                                              
         EJECT                                                                  
SENDQ2Q  NTR1                                                                   
*                                                                               
* TRANSFER A REPORT VIA Q2Q.                                                    
*                                                                               
         OPEN  (Q2QRPT,OUTPUT)     OPEN Q2Q TEMPORARY FILE                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SNDQ2Q10 LA    R1,DMCB                                                          
         USING PQPARMD,R1                                                       
         MVC   ATBLNTRY,=A(TABLNTRY)                                            
         MVC   APQBUFF,=A(CXREC)                                                
         MVC   APQHDR,=A(PQRPTHDR)                                              
         MVC   AEDHDR,=A(EDICTHDR)                                              
         MVC   APQLINE,=A(R)                                                    
         MVC   ACITABLE,SCITABLE                                                
         MVC   ADTAMGR,SADTAMGR                                                 
         GOTO1 =V(PQGETLIN)                                                     
         BNE   SNDQ2Q20            NO MORE REPORT LINES                         
         DROP  R1                                                               
*                                                                               
         CLC   =C'++DDS',R+1       HEADING INFORMATION?                         
         BE    SNDQ2Q10                                                         
*                                                                               
         PUT   Q2QRPT,R            PUT CARD TO PQ2Q OUTPUT FILE                 
         B     SNDQ2Q10                                                         
*                                                                               
SNDQ2Q20 CLOSE (Q2QRPT)                                                         
         LTR   RF,RF                                                            
         BE    *+6                                                              
         DC    H'0'                COULD NOT OPEN PQ REPORT                     
*                                                                               
         LA    RF,PQRPTHDR                                                      
         USING PQPLD,RF                                                         
         MVC   QUSERID,DSTUIDN     PQ INFO FOR EDIQ2QR                          
         MVC   QSUBID,QLSUBID                                                   
         MVC   QDESCRIP,QLDESC                                                  
         DROP  RF                                                               
*                                                                               
         POST  QMAINECB            TELL EDIQ2QR WE ARE READY                    
         WAIT  1,ECBLIST=ECBLSTQ   WAIT FOR EDIQ2QR POST                        
*                                                                               
         TM    Q2RECB,X'40'        DID EDIQ2QR TERMINATE?                       
         BZ    *+6                                                              
         DC    H'0'                YES -- THEREFORE IT ABENDED                  
*                                                                               
         TM    QSUBECB,X'40'       DID REPORT GET COPIED?                       
         BO    *+6                 YES                                          
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
         XC    QSUBECB,QSUBECB                                                  
*                                                                               
         PRNT  SENTVIAQ2Q,PRINT=ALWAYS                                          
*                                                                               
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         MVC   ETBLNTRY,=A(TABLNTRY)                                            
         MVI   EACTION,EACTSNTQ+EACTDLVQ   MARK SENT AND DELIVERED              
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   SNDQ2QX                                                          
         DROP  R1                                                               
         PRNT  RPTMARKEDSENTDLVD,PRINT=ALWAYS                                   
*                                                                               
SNDQ2QX  B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
Q2QRPT   DCB   DDNAME=Q2QRPT,MACRF=PM,DSORG=PS,RECFM=FBM,LRECL=133              
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
ECBLST   DS    0F                                                               
ALOOKECB DC    X'00',AL3(0)        A(LOOKECB)                                   
ASTOPECB DC    X'80',AL3(0)        A(STOPECB)                                   
         SPACE 2                                                                
Q2RPARMS DS    XL(Q2QPARLQ)        PARAMETERS TO EDIQ2QR (VIA R1)               
Q2RTCB   DS    F                   TCB OF ATTACHED SUBTASK EDIQ2QR              
Q2RECB   DC    F'0'                ECB OF ATTACHED SUBTASK EDIQ2QR              
*                                                                               
ECBLSTQ  DC    X'00',AL3(0)        A(QSUBECB IS STORED HERE)                    
         DC    X'80',AL3(Q2RECB)   A(EDIQ2QR SUBTASK ECB)                       
         SPACE 2                                                                
TABLNTRY DS    XL(XMTTBLQ)         SAVED XMIT TABLE KEY                         
XMTENTRY DS    XL(XMTTBLQ)         TEMP STORAGE FOR XMIT TABLE ENTRY            
OPERSTOP DC    C'N'                'Y' IF OPERATOR WANTS TO STOP                
TRACEFLG DS    C                   'Y' IF DETAILED TRACE WANTED                 
SUBTASK  DC    C'EDIQ2QR '         RECEIVING SUBTASK NAME                       
DSTNAME  DS    CL8                 DESTINATION                                  
DSTUIDN  DS    XL2                 DESTINATION USERID                           
CARD     DS    CL80                FOR CONTROL CARDS AND EDICTFIL RECS          
         DS    XL4                 PQ RECORD LENGTH                             
R        DS    CL256               PQ RECORD DATA                               
         DS    XL4                                                              
EDICTHDR DS    CL256               EDICT HDR* RECORD                            
         DS    XL4                                                              
PQRPTHDR DS    CL256               PQ REPORT HEADER                             
         SPACE 3                                                                
         DS    0D                                                               
         DC    C'*CXREC**'                                                      
CXREC    DC    14336X'00'          PRINT QUEUE INDEX BUFFER                     
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
         EJECT                                                                  
       ++INCLUDE DDEDICTWRK                                                     
         EJECT                                                                  
* DDDPRINT                                                                      
* DDEDICTFIL                                                                    
* DMPRTQL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DDEDICTFIL                                                     
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024DDEDIQ2QS 05/01/02'                                      
         END                                                                    
