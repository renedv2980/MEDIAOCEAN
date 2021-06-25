*          DATA SET DDEDISMTP  AT LEVEL 077 AS OF 11/11/13                      
*PHASE EDISMTPA                                                                 
*INCLUDE BINSR31                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE EDISUB                                                                 
*INCLUDE NUMVAL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE XSORT                                                                  
*INCLUDE SMTP                                                                   
*                                                                               
         TITLE 'DDEDISMTP -- PERFORM SMTP E-MAIL TRANSMISSIONS'                 
*                                                                               
* EMAIL VIA JES SMTP TO REPLACE EDIMQMS, 1/20/2012, YYUN                        
***********************************************************************         
*                                                                     *         
*  TITLE:        EDISMTP -- TRANSMIT PQ REPORTS VIA JES SMTP EMAIL    *         
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
***********************************************************************         
                                                                                
EDISMTP  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*EDISMTP,=A(R13CHAIN)                                          
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
         MVC   DCBDDNAM-IHADCB(8,RF),=C'SMTPTRCE'  DDNAME=SMTPTRCE              
*                                                                               
         MVC   TITLE(40),=CL40'EDICT: SENDING SUBTASK FOR SMTP E-MAIL'          
         PRNT  START_SUBTASK,PRINT=ALWAYS                                       
*                                                                               
         L     RF,SMAJORNM                                                      
         MVC   MAJORNAM,0(RF)      MAJOR RESOURCE NAME                          
*                                                                               
         GOTO1 =A(READCRDS)        READ PARAMETER CARDS                         
*                                                                               
         GOTO1 =A(INITIAL)         INITIALIZE                                   
*                                                                               
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
STOP     DS    0H                                                               
         PRNT  EXITING,PRINT=ALWAYS                                             
*                                                                               
         XBASE                                                                  
                                                                                
DONTSTOP L     RF,ALOOKECB                                                      
         TM    0(RF),X'40'         MAIN TASK POSTED READY?                      
         BO    *+6                 YES                                          
         DC    H'0'                                                             
         XC    0(4,RF),0(RF)                                                    
*                                                                               
         GOTO1 =A(XFERREPS)        TRANSMIT ALL E-MAIL REPORTS IN TABLE         
*                                                                               
         GOTO1 SADTAMGR,DMCB,=CL8'ISGENQ',C'#03K'                               
*                                                                               
         B     LOOP                WAIT                                         
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* INITIALIZE                                                                    
***********************************************************************         
INITIAL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         PRNT  INITIALIZE,PRINT=ALWAYS                                          
*                                                                               
         L     R1,SLOOKECB         BUILD ECBLIST                                
         STCM  R1,7,ALOOKECB+1                                                  
         L     R1,SSTOPECB                                                      
         STCM  R1,7,ASTOPECB+1                                                  
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
         GOTO1 SADTAMGR,DMCB,=CL7'ISGENQ',C'#03K'  SET SUBTASK #03              
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* READ PARAMETER CARDS BETWEEN "++SMTP" AND "++SMTPEND" CARDS                   
*                                                                               
*   *......          "*" IN COLUMN ONE IS A COMMENT CARD                        
*   TRACE=YES        OVERRIDE TRACE FLAG                                        
*   USERMAXXMIT=N    MAXIMUM RPTS XMITTED IN A ROW PER USERID (DEF.=50)         
*   USERID=UUUUUUUU  TRANSFER REPORTS FROM THIS USERID ONLY                     
*   SUBID=XXX        TRANSFER REPORTS FROM THIS SUBID ONLY                      
***********************************************************************         
READCRDS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
RC10     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++SMTP',CARD     LOOK FOR START OF PARAMETERS                 
         BNE   RC10                                                             
*                                                                               
RC20     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++SMTPEND',CARD  LOOK FOR END OF PARAMETERS                   
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
RC30     CLC   =C'USERMAXXMIT=',CARD  USERMAXXMIT=                              
         BNE   RC40                                                             
         GOTO1 =V(NUMVAL),DMCB,CARD+12,(2,0)                                    
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                INVALID VALUE AFTER EQUALS SIGN              
         MVC   MAXURPTS,DMCB+4                                                  
         B     RC20                                                             
*                                                                               
RC40     CLC   =C'SUBID=',CARD     SUBID=XXX                                    
         BNE   RC50                                                             
         MVC   SUBFILT,CARD+6                                                   
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
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* TRANSFER REPORTS                                                              
***********************************************************************         
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
         CLI   SUBFILT,C' '        YES -- IS THERE A SUBID FILTER?              
         BE    *+14                                                             
         CLC   XMTSUBID,SUBFILT    YES - DOES THIS REPORT MATCH FILTER?         
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
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* SEND MAIL                                                                     
***********************************************************************         
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
         BNE   SNDM10D                                                          
         MVC   FROM,R+16                                                        
         L     RF,=A(TRTABLE)                                                   
         TR    FROM,0(RF)                                                       
         B     SNDM10                                                           
*                                                                               
SNDM10D  CLC   =C'SDN',R+12        IS THIS SENDER'S NAME CONTROL CARD?          
         BNE   SNDM10E                                                          
         MVC   SNDRNAME,R+16                                                    
         L     RF,=A(TRTABLE)                                                   
         TR    SNDRNAME,0(RF)                                                   
         B     SNDM10                                                           
*                                                                               
SNDM10E  CLC   =C'EMT',R+12        IS THIS EMAILTYPE CONTROL CARD?              
         BNE   *+14                NO -- NEXT LINE                              
         MVC   EMLTYPE,R+16        EMAILTYPE OF E-MAIL                          
         B     SNDM10                                                           
*                                                                               
         CLC   =C'SUB',R+12        IS THIS SUBJECT CONTROL CARD?                
         BNE   SNDM10              NO -- NEXT LINE                              
         MVC   SUBJECT,R+16        SUBJECT OF E-MAIL                            
         L     RF,=A(TRTABLE)                                                   
         TR    SUBJECT,0(RF)                                                    
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
         BRAS  RE,FMTSMTP          FORMAT AND SEND VIA SMTP                     
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
*                                                                               
*                                                                               
FMTSMTP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    SENDTO,SENDTO                                                    
         XC    SENDCC,SENDCC                                                    
         XC    SENDBCC,SENDBCC                                                  
         LA    R8,RCPTLIST                                                      
*                                                                               
         L     R3,AMAILTAB         A(RECIPIENT TABLE)                           
         USING MAILTABD,R3                                                      
         CLI   MAILRTYP,C'R'                                                    
         BNE   FS020                                                            
         ST    R8,SENDTO                                                        
*                                                                               
FS010    CLI   0(R3),X'FF'                                                      
         BE    FS018                                                            
         CLI   MAILRTYP,C'R'                                                    
         BNE   FS018                                                            
         MVC   0(L'MAILRCP,R8),MAILRCP    RECIPIENT E-MAIL ADDRESS              
         LA    R8,L'MAILRCP-1(R8)                                               
*                                                                               
         CLI   0(R8),C' '          BACKUP TO LAST NON-BLANK CHAR                
         BNE   *+10                                                             
         BCTR  R8,0                                                             
         B     *-10                                                             
*                                                                               
         CLI   0(R8),C','          IF LAST CHAR IS ","                          
         BNE   FS015                                                            
         LA    R8,1(,R8)           DON'T ADD ANOTHER ","                        
         B     FS016                                                            
FS015    MVI   1(R8),C','                                                       
         LA    R8,2(,R8)                                                        
*                                                                               
FS016    LA    R3,MAILTABQ(R3)     NEXT TABLE ENTRY                             
         B     FS010                                                            
*                                                                               
FS018    BCTR  R8,0                                                             
         MVI   0(R8),C':'                                                       
         AHI   R8,1                                                             
*                                                                               
FS020    CLI   MAILRTYP,C'C'                                                    
         BNE   FS030                                                            
         ST    R8,SENDCC                                                        
*                                                                               
FS022    CLI   0(R3),X'FF'                                                      
         BE    FS028                                                            
         CLI   MAILRTYP,C'C'                                                    
         BNE   FS028                                                            
         MVC   0(L'MAILRCP,R8),MAILRCP    RECIPIENT E-MAIL ADDRESS              
         LA    R8,L'MAILRCP-1(R8)                                               
*                                                                               
         CLI   0(R8),C' '          BACKUP TO LAST NON-BLANK CHAR                
         BNE   *+10                                                             
         BCTR  R8,0                                                             
         B     *-10                                                             
*                                                                               
         MVI   1(R8),C','                                                       
         LA    R8,2(,R8)                                                        
*                                                                               
         LA    R3,MAILTABQ(R3)     NEXT TABLE ENTRY                             
         B     FS022                                                            
*                                                                               
FS028    BCTR  R8,0                                                             
         MVI   0(R8),C':'                                                       
         AHI   R8,1                                                             
*                                                                               
FS030    CLI   MAILRTYP,C'B'                                                    
         BNE   FS040                                                            
         ST    R8,SENDBCC                                                       
*                                                                               
FS032    CLI   0(R3),X'FF'                                                      
         BE    FS038                                                            
         CLI   MAILRTYP,C'B'                                                    
         BNE   FS038                                                            
         MVC   0(L'MAILRCP,R8),MAILRCP    RECIPIENT E-MAIL ADDRESS              
         LA    R8,L'MAILRCP-1(R8)                                               
*                                                                               
         CLI   0(R8),C' '          BACKUP TO LAST NON-BLANK CHAR                
         BNE   *+10                                                             
         BCTR  R8,0                                                             
         B     *-10                                                             
*                                                                               
         MVI   1(R8),C','                                                       
         LA    R8,2(,R8)                                                        
*                                                                               
         LA    R3,MAILTABQ(R3)     NEXT TABLE ENTRY                             
         B     FS032                                                            
*                                                                               
FS038    BCTR  R8,0                                                             
         MVI   0(R8),C':'                                                       
         AHI   R8,1                                                             
         ST    R8,SENDXXX                                                       
*                                                                               
FS040    DS    0H                                                               
         LAY   R9,RCPTLSTX                                                      
         CR    R8,R9                                                            
         BL    *+6                                                              
         DC    H'0'                OVERFLOW THE RCPTLIST                        
*                                                                               
         MVC   FROMWHO,SPACESS                                                  
         LAY   R8,FROMWHO                                                       
*                                                                               
         OC    SNDRNAME,SNDRNAME   ANY SENDER'S NAME GIVEN?                     
         BZ    FS050                                                            
*                                  "SNDR"                                       
         MVI   0(R8),C'"'                                                       
         MVC   1(L'SNDRNAME,R8),SNDRNAME                                        
         LA    R8,L'SNDRNAME(R8)                                                
*                                  BACKUP TO LAST NON-BLANK CHAR                
         CLI   0(R8),C' '                                                       
         BNE   *+10                                                             
         BCTR  R8,0                                                             
         B     *-10                                                             
*                                                                               
         MVI   1(R8),C'"'                                                       
         LA    R8,2(R8)                                                         
*                                                                               
FS050    DS    0H                  <FROM_EMAIL>                                 
         CLC   FROM,SPACESS                                                     
         BNH   FS060                                                            
         MVI   0(R8),C'<'                                                       
         MVC   1(L'FROM,R8),FROM                                                
         LA    R8,L'FROM(R8)                                                    
*                                  BACKUP TO LAST NON-BLANK CHAR                
         CLI   0(R8),C' '                                                       
         BNE   *+10                                                             
         BCTR  R8,0                                                             
         B     *-10                                                             
*                                                                               
         MVI   1(R8),C'>'                                                       
         LA    R8,2(R8)                                                         
*                                                                               
         LAY   R9,FROMWHOX                                                      
         CR    R8,R9                                                            
         BL    *+6                                                              
         DC    H'0'                OVERFLOW THE FROMWHO FIELD                   
*                                                                               
FS060    DS    0H                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAINI',0)                                     
         OC    EMLTYPE,EMLTYPE     ANY EMAILTYPE?                               
         BZ    FS065                                                            
         CLI   EMLTYPE,C'1'        SEND AS ATTACHMENT?                          
         BNE   FS065               NO - SKIP THIS                               
         GOTO1 =V(SMTP),DMCB,('SMTPAATT',0)                                     
FS065    DS    0H                                                               
         CLC   FROMWHO,SPACESS                                                  
         BE    FS070                                                            
         GOTO1 =V(SMTP),DMCB,('SMTPAFR2',0),(L'FROMWHO,FROMWHO)                 
FS070    DS    0H                                                               
         OC    REPLYTO,REPLYTO     ANY REPLY TO E-MAIL GIVEN?                   
         BZ    FS080                                                            
         GOTO1 =V(SMTP),DMCB,('SMTPARPY',0),(L'REPLYTO,REPLYTO)                 
FS080    DS    0H                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAMXF',0)      TURN WARNING OFF               
         L     R2,SENDTO                                                        
         L     R3,SENDCC                                                        
         L     R4,SENDBCC                                                       
         OC    SUBJECT,SUBJECT     ANY SUBJECT?                                 
         BNZ   FS090                                                            
         GOTO1 =V(SMTP),DMCB,('SMTPATCS',(R2)),0,(R3),(R4)                      
         B     FS100                                                            
FS090    DS    0H                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPATCS',(R2)),(L'SUBJECT,SUBJECT),    +        
               (R3),(R4)                                                        
FS100    DS    0H                                                               
*                                                                               
         CLI   EDICTHDR+35,C'W'    DO WE WANT A WIDE TRANSMISSION?              
         BNE   FS110                                                            
         GOTO1 =V(SMTP),DMCB,('SMTPASLL',0)                                     
FS110    DS    0H                                                               
*                                                                               
         CLI   TRACEFLG,C'Y'       DETAILED TRACE?                              
         BNE   FS115                                                            
         PRNT  MAIL_HEADER                                                      
         BRAS  RE,PRNTHDR                                                       
         PRNT  END_MAIL_HDR                                                     
FS115    DS    0H                                                               
         PRNT  BEGIN_MAIL_DATA                                                  
*                                                                               
         LA    R2,79               (MAX 80 CHARACTERS)                          
         CLI   EDICTHDR+35,C'W'    DO WE WANT A WIDE TRANSMISSION?              
         BNE   *+8                                                              
         LA    R2,131              YES (MAX 132 CHARACTERS)                     
*                                                                               
FS120    LA    R1,DMCB                                                          
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
         BNE   FS130               MORE REPORT LINES TO BE READ                 
*                                                                               
         MVC   TEXTLINE,SPACESS                                                 
         EX    R2,*+8              MOVE IN LINE                                 
         B     *+10                (FAIL S0C4 MEANS MSG TOO BIG)                
         MVC   TEXTLINE(0),R+1                                                  
*                                                                               
         CLI   TRACEFLG,C'Y'       DETAILED TRACE?                              
         BNE   FS125                                                            
         MVC   P,TEXTLINE                                                       
         GOTO1 =V(PRINTER)                                                      
FS125    DS    0H                                                               
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',TEXTLINE)                              
         B     FS120                                                            
*                                                                               
FS130    DS    0H                                                               
         PRNT  BEGIN_MAIL_DATA                                                  
*                                                                               
FS140    DS    0H                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPASND',0)                                     
         GOTO1 =V(SMTP),DMCB,('SMTPAEND',0)                                     
*                                                                               
FSX      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
PRNTHDR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    EMLTYPE,EMLTYPE     ANY EMAILTYPE?                               
         BZ    PRTH030                                                          
         CLI   EMLTYPE,C'1'        SEND AS ATTACHMENT?                          
         BNE   PRTH030             NO - SKIP THIS                               
         MVC   P(15),=CL15'ATTACHMENT=YES'                                      
         GOTO1 =V(PRINTER)                                                      
PRTH030  DS    0H                                                               
*                                                                               
         CLC   FROMWHO,SPACESS     ANY SENDER E-MAIL ADDRESS?                   
         BE    PRTH040                                                          
         MVC   P(10),=CL10'FROM:'                                               
         MVC   P+10(L'FROMWHO),FROMWHO                                          
         GOTO1 =V(PRINTER)                                                      
PRTH040  DS    0H                                                               
*                                                                               
         OC    REPLYTO,REPLYTO     ANY REPLY TO E-MAIL GIVEN?                   
         BZ    PRTH050                                                          
         MVC   P(10),=CL10'REPLYTO:'                                            
         MVC   P+10(L'REPLYTO),REPLYTO                                          
         GOTO1 =V(PRINTER)                                                      
PRTH050  DS    0H                                                               
*                                                                               
         OC    SUBJECT,SUBJECT     ANY SUBJECT?                                 
         BZ    PRTH060                                                          
         MVC   P(10),=CL10'SUBJECT:'                                            
         MVC   P+10(L'SUBJECT),SUBJECT                                          
         GOTO1 =V(PRINTER)                                                      
PRTH060  DS    0H                                                               
*                                                                               
         L     R3,AMAILTAB         A(RECIPIENT TABLE)                           
         USING MAILTABD,R3                                                      
*                                                                               
PRTH080  CLI   0(R3),X'FF'                                                      
         BE    PRTH090                                                          
         MVC   P(10),=CL10'TO:'                                                 
         CLI   MAILRTYP,C'R'                                                    
         BE    PRTH085                                                          
         MVC   P(10),=CL10'CC:'                                                 
         CLI   MAILRTYP,C'C'                                                    
         BE    PRTH085                                                          
         MVC   P(10),=CL10'BCC:'                                                
         CLI   MAILRTYP,C'B'                                                    
         BE    PRTH085                                                          
*                                                                               
PRTH085  MVC   P+10(L'MAILRCP),MAILRCP    E-MAIL ADDRESS                        
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R3,MAILTABQ(R3)     NEXT TABLE ENTRY                             
         B     PRTH080                                                          
         DROP  R3                                                               
PRTH090  DS    0H                                                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
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
*                                                                               
MTYPELST DC    C'RCB',X'FF'        DESENDING ORDER OF E-MAIL TYPE LIST          
SUBFILT  DC    CL3' '              PQ SUBID FILTER                              
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
CUSTREF  DS    CL18                VEDDNNNNNNYYYYMMDD                           
CARD     DS    CL80                FOR CONTROL CARDS AND EDICTFIL RECS          
         EJECT                                                                  
*                                                                               
SPACESS  DC    CL256' '            PQ RECORD DATA                               
         DS    XL4                 PQ RECORD LENGTH                             
R        DS    CL256               PQ RECORD DATA                               
         DS    XL4                                                              
EDICTHDR DS    CL256               EDICT HDR* RECORD                            
         DS    XL4                                                              
PQRPTHDR DS    CL256               PQ REPORT HEADER                             
         DS    XL4                                                              
FROMWHO  DS    CL120               FROM EMAIL ADDRESS MAY INCL SNDRNAME         
FROMWHOX EQU   *                                                                
TEXTLINE DS    CL160                                                            
SENDTO   DS    F                   A(SENDTO LIST)                               
SENDCC   DS    F                   A(SENDCC LIST)                               
SENDBCC  DS    F                   A(SENDBCC LIST)                              
SENDXXX  DS    F                   A(SENDBCC LIST END)                          
RCPTLIST DS    CL1000                                                           
RCPTLSTX EQU   *                                                                
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
* SMTPD                                                                         
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DDEDICTFIL                                                     
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE DDSMTPD                                                        
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'077DDEDISMTP 11/11/13'                                      
         END                                                                    
