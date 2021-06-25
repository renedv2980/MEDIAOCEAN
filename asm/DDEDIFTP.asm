*          DATA SET DDEDIFTP   AT LEVEL 085 AS OF 03/09/16                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 046562.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
***********************************************************************         
*                                                                     *         
*                THIS BOOK IS NO LONGER USED                          *         
*                     03/09/2016 TZIH                                 *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
***********************************************************************         
* LEVEL 82 FOR SENDING MO EC VIA MQ, YYUN                             *         
***********************************************************************         
*PHASE EDIFTPA                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE BINSR31                                                                
*INCLUDE DATCON                                                                 
*INCLUDE EDISUB                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE NUMVAL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        EDIFTP -- TRANSMIT PQ REPORTS VIA FTP                *         
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
*                R7 -- WORK                                           *         
*                R8 -- COMMON WORK AREA BASE 2                        *         
*                R9 -- SECOND PROGRAM BASE                            *         
*                RA -- CPRINT                                         *         
*                RB -- PROGRAM BASE                                   *         
*                RC -- COMMON WORK AREA BASE 1                        *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'DDEDIFTP -- PERFORM FTP TRANSMISSIONS'                          
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
EDIFTP   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*EDIFTP*,=A(R13CHAIN),R9                                       
*                                                                               
         LR    R5,RC                                                            
         L     RC,=A(COMMWORK)     COMMON STORAGE AREA                          
         LR    R8,RC                                                            
         AHI   R8,4096                                                          
         USING COMMWORK,RC,R8                                                   
*                                                                               
         AHI   R5,-4                                                            
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
         MVC   DCBDDNAM-IHADCB(,RF),=C'FTPTRACE'  DDNAME=FTPTRACE               
*                                                                               
         MVC   TITLE(43),=C'EDICT: SENDING SUBTASK TO FTP QUEUE HANDLER+        
               '                                                                
         PRNT  START_SUBTASK,PRINT=ALWAYS                                       
*                                                                               
         BAS   RE,READCRDS         READ PARAMETER CARDS                         
*                                                                               
         BAS   RE,INITIAL          INITIALIZE                                   
*                                                                               
         LA    R2,CSQBCONN_STRUCTURE                                            
         BRAS  RE,CALLMQ           CONNECT TO MQ QUEUE MANAGER                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,QUERY            QUERY ONCE BEFORE ANYTHING ELSE              
         EJECT                                                                  
LOOP     CLI   OPERSTOP,C'Y'       DOES OPERATOR WANT TO STOP?                  
         BE    STOP                                                             
*                                                                               
         PRNT  ABOUTTOWAIT,PRINT=ALWAYS                                         
*                                                                               
         XC    TIMERECB,TIMERECB                                                
         STIMERM SET,ID=STIMERID,BINTVL=WAITSECS,EXIT=TIMERXIT                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         WAIT  1,ECBLIST=ECBLST    WAIT FOR POST FROM MAIN OR TIMER POP         
*                                                                               
         TM    TIMERECB,X'40'                                                   
         BO    QUERYNOW            TIMER POPPED                                 
*                                                                               
         STIMERM CANCEL,ID=STIMERID                                             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,ASTOPECB                                                      
         TM    0(RF),X'40'         STOP?                                        
         BZ    DONTSTOP            NO                                           
         XC    0(4,RF),0(RF)                                                    
*                                                                               
STOP     LA    R2,CSQBDISC_STRUCTURE                                            
         BRAS  RE,CALLMQ           DISCONNECT FROM MQ QUEUE MANAGER             
         BE    *+6                                                              
         DC    H'0'                                                             
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
         BAS   RE,XFERREPS         TRANSMIT ALL FTP REPORTS IN TABLE            
*                                                                               
QUERYNOW BAS   RE,QUERY            QUERY AFTER SET OF TRANSMISSIONS             
*                                                                               
         B     LOOP                WAIT                                         
         EJECT                                                                  
INITIAL  NTR1                                                                   
*                                                                               
* INITIALIZE                                                                    
*                                                                               
         PRNT  INITIALIZE,PRINT=ALWAYS                                          
*                                                                               
         L     RF,SMAJORNM         A(MAJOR RESOURCE NAME)                       
         MVC   MAJORNAM,0(RF)      MAJOR RESOURCE NAME                          
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
*                                                                               
         LOAD  DE=CSQBCONN                                                      
         ST    R0,CSQBCONN_STRUCTURE                                            
         LOAD  DE=CSQBDISC                                                      
         ST    R0,CSQBDISC_STRUCTURE                                            
         LOAD  DE=CSQBPUT1                                                      
         ST    R0,CSQBPUT1_STRUCTURE                                            
*                                                                               
         MVI   MQSLAB+10,C'T'        ASSUME TST EDICT                           
         CLI   MAJORNAM+5,C'R'       REP EDICT?                                 
         BNE   *+8                                                              
         MVI   MQSLAB+10,C'R'                                                   
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
XFERREPS NTR1                                                                   
*                                                                               
XFER10   LA    R3,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R3)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(3),E,8)  ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         L     R7,SXMTTBLE         R7 = A(REPORT TABLE)                         
         USING XMTTABLD,R3                                                      
         LA    R3,XMTENTRY                                                      
         LA    R4,WORK                                                          
         XC    WORK,WORK           WORK = EARLIEST ENTRY --> FIFO               
*                                                                               
XFER20   MVC31 XMTENTRY,0(R7)                                                   
         CLI   0(R3),0             END OF TABLE?                                
         BE    XFER40              YES, NO MORE                                 
         CLI   XMTSOURC,XMTSRCPQ   PRINT QUEUE ENTRY?                           
         BNE   XFER30                                                           
         CLI   XMTMETH,C'F'        YES -- FTP METHOD?                           
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
XFER30   AHI   R7,XMTTBLQ          BUMP TO NEXT REPORT                          
         B     XFER20                                                           
         DROP  R3                                                               
*                                                                               
XFER40   OC    WORK,WORK           ANYTHING FOUND TO SEND?                      
         BZ    XFER60              NO                                           
         MVC   TABLNTRY,0(R4)      SAVE XMIT TABLE KEY                          
*                                                                               
         LA    R3,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R3)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(3),8)    DEQUEUE THE TRANSMIT TABLE                   
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
         MVI   EACTION,EACTJNKQ    MARK UNSENDABLE                              
         MVI   EERRORCD,EDFERNPQ   REPORT NOT FOUND ON PRINT QUEUE              
         MVI   SVACTION,C'C'                                                    
         MVC   SVERROR,EERRORCD                                                 
         MVC   EAFTPDTA,=A(FTPDATA)                                             
         MVC   EFILREC,=A(WORK)                                                 
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   XFER43              CAN'T POST EDICT FILE                        
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
         DROP  R1                                                               
XFER43   EQU   *                   BUILD MQ STATUS MESSAGE                      
         BRAS  RE,BLDMQS                                                        
         B     XFER50                                                           
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
         MVI   EACTION,EACTJNKQ    MARK UNSENDABLE                              
         MVI   EERRORCD,EDFERNED   EDICT RECORD WAS DELETED                     
         MVI   SVACTION,C'C'                                                    
         MVC   SVERROR,EERRORCD                                                 
         MVC   EAFTPDTA,=A(FTPDATA)                                             
         MVC   EFILREC,=A(WORK)                                                 
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   XFER46              CAN'T POST EDICT FILE                        
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
         DROP  R1                                                               
XFER46   EQU   *                   BUILD MQ STATUS MESSAGE                      
         BRAS  RE,BLDMQS                                                        
         B     XFER50                                                           
*                                                                               
         USING DESTTABD,R1                                                      
XFER48   MVC   DSTNAME,DESTNAME    DESTINATION                                  
         MVC   FTPOPSYS,DESTFTPO   FTP OPERATING SYSTEM                         
         MVC   FTPLUID,DESTFTPL    FTP REMOTE LUNAME                            
         MVC   FTPUID,DESTFTPU     FTP APPC USERID                              
         MVC   FTPPASS,DESTFTPP    FTP APPC PASSWORD                            
         MVC   FTPCLASS,DESTFTPS   FTP APPC SERVER CLASS                        
         MVC   FTPFLAGS,DESTFTPF   FTP FLAGS                                    
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
         BAS   RE,SENDFTP                                                       
*                                                                               
XFER50   L     RF,ASTOPECB                                                      
         TM    0(RF),X'40'         OPERATOR WANTS TO STOP?                      
         BZ    XFER10              NO                                           
*                                                                               
         XC    0(4,RF),0(RF)                                                    
         MVI   OPERSTOP,C'Y'                                                    
         B     XFERX                                                            
*                                                                               
XFER60   LA    R3,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R3)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(3),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
XFERX    J     XIT                                                              
         EJECT                                                                  
SENDFTP  NTR1                                                                   
*                                                                               
* TRANSFER A REPORT VIA FTP.                                                    
*                                                                               
         XC    FTPDATA,FTPDATA                                                  
         XC    RECCOUNT,RECCOUNT                                                
         MVC   FTPRCVLU,FTPLUID                                                 
         MVC   FTPSUBTP,=X'FFFF'                                                
         MVI   ERRCODE,0                                                        
         MVI   DDSTRN,C'N'         WE'VE HAVEN'T SEEN ++DDS TRN YET             
         MVI   DYNFLAG,C'N'        WE HAVEN'T DONE DYNALLOC YET                 
*                                                                               
         LA    R2,FTPFILE          A(MVS DATASET DCB)                           
         USING IHADCB,R2                                                        
         XC    DCBBLKSI,DCBBLKSI   CLEAR BLKSIZE                                
         MVC   DCBLRECL,=H'255'    MAKE LRECL 255 IF DOWNLOADABLE               
         LA    RF,PQRPTHDR         PRINT QUEUE REPORT HEADER                    
         USING PQPLD,RF                                                         
         TM    QLTYPE,QLTYDL                                                    
         BO    SNDFTP5                                                          
         MVC   DCBLRECL,=Y(LRECLDEF) DEFAULT LRECL FOR NON-DOWNLOADABLE         
         SR    RE,RE               PICK UP REPORT WIDTH                         
         IC    RE,QLLINEW                                                       
         CHI   RE,LRECLDEF         IF REPORT WIDER THAN DEFAULT. . .            
         BNH   SNDFTP5                                                          
         STH   RE,DCBLRECL         . . .USE REPORT WIDTH AS LRECL               
*                                                                               
SNDFTP5  LH    RE,DCBLRECL                                                      
         LR    R1,RE                                                            
         MHI   R1,100              ASSUME 100 LOGICAL RECORDS PER BLOCK         
         STCM  R1,7,TXTBLKLN+6     ASSUMED BLOCK LENGTH (RECORD LENGTH)         
         DROP  R2                                                               
*                                                                               
         SR    R2,R2               PREPARE FOR MULTIPLY                         
         SR    R3,R3                                                            
         ICM   R3,7,QLLINES        NUMBER OF LINES IN REPORT                    
         DROP  RF                                                               
         MVC   P+30(6),=C'LINES='                                               
         EDIT  (R3),(8,P+36),ALIGN=LEFT,ZERO=NOBLANK                            
         MR    R2,RE               LINES * LRECL = BYTE COUNT                   
         MVC   P+46(6),=C'BYTES='                                               
         EDIT  (R3),(8,P+52),ALIGN=LEFT,ZERO=NOBLANK                            
         DR    R2,R1               BYTE COUNT / BLKLEN = BLOCKS NEEDED          
         LA    R3,1(R3)            ROUND UP TO INCLUDE REMAINDER                
         MVC   P+62(7),=C'BLOCKS='                                              
         EDIT  (R3),(8,P+69),ALIGN=LEFT,ZERO=NOBLANK                            
         STCM  R3,7,TXTPRI+6       PRIMARY SPACE ALLOCATION                     
         SRL   R3,3                DIVIDE BY 8 (SEMI-ARBITRARY)                 
         LA    R3,1(R3)            ADD ONE (SO THERE'S AT LEAST ONE)            
         STCM  R3,7,TXTSEC+6       SECONDARY SPACE ALLOCATION                   
         PRNT  DATASET_SIZE,PRINT=ALWAYS                                        
*                                                                               
         MVC   DYNAMDSN+12(1),MAJORNAM+5  'A' FOR ADV, 'R' FOR REP              
         MVC   DYNAMDSN+14(8),FTPLUID REMOTE LU NAME                            
*                                                                               
         LA    R3,TABLNTRY                                                      
         USING XMTTABLD,R3                                                      
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,XMTUSRID       USERID NUMBER                                
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DYNAMDSN+26(5),DUB                                               
*                                                                               
         LA    R1,DMCB                                                          
         USING CONVDSKD,R1                                                      
         MVC   CONVDMGR,SADTAMGR   A(DATAMGR)                                   
         MVI   CONVACTN,CONVDSKF   CONVERT DSKADDR TO REFERENCE NUMBER          
         MVC   CONVDSK,XMTDSKAD    EDICT FILE DISK ADDRESS                      
         MVC   CONVOFF,=A(DYNAMDSN+32)  RESULT GOES INTO DSN                    
         GOTO1 =V(CONVDSKA)                                                     
         DROP  R1                                                               
         DROP  R3                                                               
*                                                                               
         MVC   FTPRQNAM,DYNAMDSN+32 REFERENCE NUMBER = FTP REQUEST NAME         
*                                                                               
         LA    RF,DSNMAP                                                        
SNDFTP10 CLI   0(RF),X'FF'         END OF LIST?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   DYNAMDSN+32(1),0(RF) FIND FIRST DIGIT OF REF. NUMBER             
         BE    *+12                                                             
         LA    RF,2(RF)                                                         
         B     SNDFTP10                                                         
         MVC   DYNAMDSN+32(1),1(RF) QUALIFIER NOW BEGINS WITH ALPHA             
*                                                                               
SNDFTP20 LA    R1,DMCB                                                          
         USING PQPARMD,R1                                                       
         MVC   ATBLNTRY,=A(TABLNTRY)                                            
         MVC   APQBUFF,=A(CXREC)                                                
         MVC   APQHDR,=A(PQRPTHDR)                                              
         MVC   AEDHDR,=A(EDICTHDR)                                              
         MVC   APQLINE,=A(R)                                                    
         MVC   ACITABLE,SCITABLE                                                
         MVC   ADTAMGR,SADTAMGR                                                 
         GOTO1 =V(PQGETLIN)                                                     
         BNE   SNDFTP50            NO MORE REPORT LINES                         
         DROP  R1                                                               
*                                                                               
         CLC   =X'FFFF',R          CHECK IF REPORT LINE EXCEED 255?             
         BNE   SNDFTP21                                                         
         MVI   ERRCODE,9          ERROR CODE FOR TOO LONG RECORD                
         MVC   P+30(24),=C'RECORD LENGTH EXCEED 255'                            
         MVC   P+11(13),=C'*** ERROR ***'                                       
         GOTO1 =V(PRINTER)                                                      
         B     SNDFTP50            NO -- SOMETHING'S WRONG WITH REPORT          
*                                                                               
SNDFTP21 CLC   =C'++DDS',R+1       HEADING INFORMATION?                         
         BNE   SNDFTP24            NO                                           
         CLC   =C'DSN',R+12        IS THIS A DATASET NAME CARD?                 
         BNE   *+14                                                             
         MVC   FTPDSN,R+16         YES -- PICK UP THE DSN                       
         B     SNDFTP20                                                         
*                                                                               
         CLC   =C'FTP',R+12        IS THIS FTP QUALIFIER OVERRIDE?              
         BNE   SNDFTP22            NO                                           
*                                                                               
         L     RF,=A(DSNCHAR1)     TRT TABLE FOR FIRST QUALIFIER CHAR           
         TRT   R+16(1),0(RF)                                                    
         BZ    *+14                                                             
         MVC   DYNAMDSN+23(8),=C'ERROR###'  INVALID QUALIFIER                   
         B     SNDFTP20                                                         
*                                                                               
         LA    R1,7                PAD TRAILING BLANKS W/ #'S                   
         LA    RF,R+17                                                          
         CLI   0(RF),C' '                                                       
         BNE   *+8                                                              
         MVI   0(RF),C'#'                                                       
         AHI   RF,1                                                             
         BCT   R1,*-16                                                          
*                                                                               
         L     RF,=A(DSNCHARN)     TRT TABLE FOR REMAINING CHARACTERS           
         TRT   R+17(7),0(RF)                                                    
         BZ    *+14                                                             
         MVC   DYNAMDSN+23(8),=C'ERROR###'  INVALID QUALIFIER                   
         B     SNDFTP20                                                         
*                                                                               
         MVC   DYNAMDSN+23(8),R+16 DSN QUALIFIER IS VALID: USE IT               
         B     SNDFTP20                                                         
*                                                                               
SNDFTP22 CLC   =C'TRN',R+12        IS THIS THE TRANSACTION DATA CARD?           
         BNE   SNDFTP20            NO                                           
         MVI   DDSTRN,C'Y'         WE'VE SEEN ++DDS TRN                         
*                                                                               
         MVC   DYNAMDSN+41(3),=C'#ER'  ASSUME QUALIFIER WILL BE INVALID         
*                                                                               
         L     RF,=A(DSNCHAR1)     TRT TABLE FOR FIRST QUALIFIER CHAR           
         TRT   R+9(1),0(RF)                                                     
         BNZ   SNDFTP20            IT IS INVALID                                
*                                                                               
         L     RF,=A(DSNCHARN)     TRT TABLE FOR REMAINING CHARACTERS           
         TRT   R+10(2),0(RF)                                                    
         BNZ   SNDFTP20            INVALID                                      
*                                                                               
         MVC   DYNAMDSN+41(3),R+9  DSN QUALIFIER IS VALID: USE IT               
         B     SNDFTP20                                                         
*                                                                               
SNDFTP24 CLI   DYNFLAG,C'Y'        ALREADY DYNALLOC                             
         BE    SNDFTP40                                                         
*                                                                               
         MVC   TXTDSN+6(44),DYNAMDSN                                            
*                                                                               
         CLI   EDICTHDR+38,C'D'    DID CALLER SUPPLY A DSN?                     
         BE    SNDFTP40            YES -- NO DYNALLOC NEEDED                    
*                                                                               
SNDFTP25 MVC   P+30(44),DYNAMDSN   PRINT DESTINATION                            
         PRNT  DYNALLOC_DSN,PRINT=ALWAYS                                        
*                                                                               
         MVC   FTPSUBTP,R+10                                                    
         MVC   FTPDSN,DYNAMDSN     USE DYNAMICALLY ALLOCATED DSN                
         LA    R1,TXTSTATN         FOR 'NEW' STATUS                             
         STCM  R1,7,FILESTAT+1                                                  
         LA    R1,TXTNDISP         FOR 'CATLG' DISPOSITION                      
         STCM  R1,7,FILEDISP+1                                                  
         LA    R1,ARBLKCTA         DYNAMIC ALLOCATION PARAMETER BLOCK           
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         BZ    SNDFTP30            DATASET WAS CREATED OK                       
*                                                                               
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(22),=C'DYNALLOC ERROR CODE = '                              
         GOTO1 =V(HEXOUT),DMCB,RBLKCATA+4,P+52,2,=C'TOG'                        
         CLC   =F'4',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P+56(15),=C',  INFO CODE = '                                     
         GOTO1 =V(HEXOUT),DMCB,RBLKCATA+6,P+71,2,=C'TOG'                        
         CLC   =F'4',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R1,TXTSTATO         FOR 'OLD' STATUS                             
         STCM  R1,7,FILESTAT+1                                                  
         LA    R1,TXTNDISK         FOR 'KEEP' DISPOSITION                       
         STCM  R1,7,FILEDISP+1                                                  
         LA    R1,ARBLKCTA         DYNAMIC ALLOCATION PARAMETER BLOCK           
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         BZ    SNDFTP30            DATASET WAS FOUND OK                         
*                                                                               
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(22),=C'DYNALLOC ERROR CODE = '                              
         GOTO1 =V(HEXOUT),DMCB,RBLKCATA+4,P+52,2,=C'TOG'                        
         CLC   =F'4',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P+56(15),=C',  INFO CODE = '                                     
         GOTO1 =V(HEXOUT),DMCB,RBLKCATA+6,P+71,2,=C'TOG'                        
         CLC   =F'4',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
*                                                                               
SNDFTP30 OPEN  (FTPFILE,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   DYNFLAG,C'Y'        WE'VE DONE THE DYNALLOC                      
*                                                                               
SNDFTP40 CLI   DDSTRN,C'Y'         HAVE WE SEEN ++DDS TRN CARD?                 
         BE    SNDFTP45                                                         
         MVI   ERRCODE,3           ERROR CODE                                   
         MVC   P+30(31),=C'NO ++DDS TRN CARD ON FTP REPORT'                     
         MVC   P+11(13),=C'*** ERROR ***'                                       
         GOTO1 =V(PRINTER)                                                      
         B     SNDFTP50            NO -- SOMETHING'S WRONG WITH REPORT          
*                                                                               
SNDFTP45 CLI   EDICTHDR+38,C'D'    DID CALLER SUPPLY A DSN?                     
         BE    SNDFTP20            YES -- NO DYNALLOC NEEDED                    
*                                                                               
         L     R1,RECCOUNT                                                      
         LA    R1,1(R1)            INCREMENT RECORD COUNT                       
         ST    R1,RECCOUNT                                                      
         PUT   FTPFILE,R+1         PUT CARD TO FTPFILE (W/OUT CARR CTL)         
         B     SNDFTP20                                                         
*                                                                               
SNDFTP50 CLI   EDICTHDR+38,C'D'    DID CALLER SUPPLY A DSN?                     
         BE    SNDFTP60            YES -- NO DYNALLOC NEEDED                    
*                                                                               
         CLI   DYNFLAG,C'Y'        ALREADY DYNALLOC                             
         BE    SNDFTP55                                                         
         MVI   ERRCODE,EDFEREFR    NO, EMPTY DOWNLOADABLE REPORT                
         B     SNDFTBAD                                                         
*                                                                               
SNDFTP55 CLOSE (FTPFILE)                                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         FREEPOOL FTPFILE                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,ARBLKUNA         DYNAMIC UNALLOCATION PARAMETER BLOCK         
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         BZ    SNDFTP60                                                         
*                                                                               
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(22),=C'DYNALLOC ERROR CODE = '                              
         GOTO1 =V(HEXOUT),DMCB,RBLKUNA+4,P+52,2,=C'TOG'                         
         CLC   =F'4',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P+56(15),=C',  INFO CODE = '                                     
         GOTO1 =V(HEXOUT),DMCB,RBLKUNA+6,P+71,2,=C'TOG'                         
         CLC   =F'4',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
*                                                                               
SNDFTP60 CLI   ERRCODE,0           ANY ERROR FOUND?                             
         BNE   SNDFTP67            YES                                          
*                                                                               
         BAS   RE,APLINITL         INITIALIZE FTP BLOCKS                        
*                                                                               
         L     R4,=A(APLACCR)      A(APL)                                       
         USING APLACCR,R4                                                       
*                                                                               
         MVI   APLVBC,APLADDRE     FUNCTION ADD                                 
         MVC   APLRQNAM,FTPRQNAM   REQUEST NAME                                 
         MVC   APLCLASS,FTPCLASS   SERVER CLASS=1                               
         MVI   APLPRTY,C'1'        PRTY=1                                       
*                                                                               
         MVC   APLAPUID,FTPUID     APPC CONV USER-ID                            
         MVC   APLAPPWD,FTPPASS    APPC PASSWORD                                
*                                                                               
* TRANSFER PARAMETERS FOLLOW                                                    
*                                                                               
         MVC   APLRLUNM,FTPLUID    REMOTE LU NAME                               
         MVI   APLXMODE,APLTO      WE ARE A SENDER                              
         MVI   APLSSMOD,APLCONT    CONTINUOUS RUNNING SERVERS                   
         MVI   APLREST,APLYES      RESTART FROM CHECKPOINT                      
         MVI   APLREQUE,APLYES     AUTOMATIC TRANSFER RESTART                   
         MVI   APLRRCHK,APLNO      INDICATE REMOTE CHECK NOT NEEDED             
*                                                                               
* FILE PARAMETERS                                                               
*                                                                               
         MVC   APLSDYFD,FTPDSN     SENDING DSN                                  
*                                                                               
* SECURITY PARAMETERS                                                           
* FILE PARAMETERS FOR OS/400 SYSTEM                                             
*                                                                               
* OS/400 SIZE GROUP                                                             
*                                                                               
         MVC   APLR4MRC,=F'-1'     MAX RECORDS                                  
         TM    FTPFLAGS,DESTFTRC   PASS TRUE RECORD COUNT?                      
         BZ    *+10                                                             
         MVC   APLR4NRC,RECCOUNT   YES                                          
*                                                                               
         MVI   APLRSOPT,APLMNE     FILE STATUS OPTION = MUST NOT EXIST          
         MVI   APLSACCO,APLEXC     SENDING FILE ACCESS OPTION = EXCLU.          
         MVI   APLSFORG,APLPS      DSORG = PS                                   
         MVI   APLREOPS,APLKEEP    KEEP IF SUCCESSFUL (RECEIVER)                
         MVI   APLSEOPS,APLDEL     DELETE IF SUCCESSFUL (SENDER)                
*                                                                               
         CLI   FTPOPSYS,DESTOS2Q   ARE WE SENDING TO AN OS/2 MACHINE?           
         BNE   SNDFTP65            NO                                           
         MVI   APLRFHMD,APLRECO    FILE HANDLING MODE = RECORD                  
         MVI   APLREOPU,APLDEL     DELETE IF UNSUCCESSFUL (RECEIVER)            
         MVI   APLCONVT,C'Y'       ASSUME WE SHOULD CONVERT CHARSET             
         TM    FTPFLAGS,DESTFTCO   CONVERT CHARACTER SET?                       
         BO    SNDFTP65                                                         
         MVI   APLCONVT,C'N'       NO                                           
*                                                                               
SNDFTP65 PRNT  DVGCALL_ADD         ADD REQUEST TO QUEUE HANDLER                 
         DVGCALL PARM=ADVGAPLP     SETS UP API                                  
         ST    RF,RETCODE          SAVE RETURN CODE                             
         PRNT  DVGCALL_RETURN                                                   
         OC    RETCODE,RETCODE     CHECK API RETURN CODE                        
         BZ    SNDFTP90            OK                                           
*                                                                               
SNDFTP67 CLI   EDICTHDR+38,C'D'    DID CALLER SUPPLY A DSN?                     
         BE    SNDFTP80            YES -- NO DYNALLOC NEEDED                    
*                                                                               
         MVC   TXTDSN+6(44),DYNAMDSN PURGE THE DATASET WE JUST ADDED            
         LA    R1,ARBLKDLA         DYNAMIC ALLOCATION PARAMETER BLOCK           
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         BZ    SNDFTP70                                                         
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(22),=C'DYNALLOC ERROR CODE = '                              
         GOTO1 =V(HEXOUT),DMCB,RBLKDELA+4,P+52,2,=C'TOG'                        
         CLC   =F'4',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P+56(15),=C',  INFO CODE = '                                     
         GOTO1 =V(HEXOUT),DMCB,RBLKDELA+6,P+71,2,=C'TOG'                        
         CLC   =F'4',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
*                                                                               
SNDFTP70 LA    R1,ARBLKUN2         DYNAMIC UNALLOCATION PARAMETER BLOCK         
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         BZ    SNDFTP80            THE DATASET IS NOW PURGED                    
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(22),=C'DYNALLOC ERROR CODE = '                              
         GOTO1 =V(HEXOUT),DMCB,RBLKUNA2+4,P+52,2,=C'TOG'                        
         CLC   =F'4',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P+56(15),=C',  INFO CODE = '                                     
         GOTO1 =V(HEXOUT),DMCB,RBLKUNA2+6,P+71,2,=C'TOG'                        
         CLC   =F'4',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
SNDFTP80 CLI   ERRCODE,0           PROBLEM WITH REPORT?                         
         BNE   SNDFTBAD            YES                                          
*                                                                               
         GOTO1 =A(APIERROR),(RC)                                                
         B     SNDFTPX                                                          
*                                                                               
SNDFTP90 MVC   FTPREFNO,APLRQNUM   SAVE REQUEST NUMBER                          
         DROP  R4                                                               
*                                                                               
         PRNT  SENTVIAFTP,PRINT=ALWAYS                                          
*                                                                               
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         MVC   ETBLNTRY,=A(TABLNTRY)                                            
         MVI   EACTION,EACTSNTQ    MARK SENT                                    
         MVI   SVACTION,C'S'                                                    
         MVC   EAFTPDTA,=A(FTPDATA)                                             
         MVC   EFILREC,=A(WORK)                                                 
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   SNDFTP93            CAN'T POST EDICT FILE                        
         DROP  R1                                                               
         PRNT  RPTMARKEDSENT,PRINT=ALWAYS                                       
SNDFTP93 EQU   *                   BUILD MQ STATUS MESSAGE                      
         BRAS  RE,BLDMQS                                                        
         B     SNDFTPX                                                          
*                                                                               
SNDFTBAD LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         MVC   ETBLNTRY,=A(TABLNTRY)                                            
         MVI   EACTION,EACTJNKQ    MARK UNSENDABLE                              
         MVC   EERRORCD,ERRCODE                                                 
         MVI   SVACTION,C'C'                                                    
         MVC   SVERROR,EERRORCD                                                 
         MVC   EAFTPDTA,=A(FTPDATA)                                             
         MVC   EFILREC,=A(WORK)                                                 
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   SNDFTP99            CAN'T POST EDICT FILE                        
         DROP  R1                                                               
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
SNDFTP99 EQU   *                   BUILD MQ STATUS MESSAGE                      
         BRAS  RE,BLDMQS                                                        
         B     SNDFTPX                                                          
*                                                                               
SNDFTPX  J     XIT                                                              
         EJECT                                                                  
APLINITL NTR1                                                                   
*                                                                               
* INITIALIZE THE FTP PARAMETER BLOCKS.                                          
*                                                                               
         L     R4,=A(APLACCR)      A(APL)                                       
         USING APLACCR,R4                                                       
*                                                                               
         MVC   APLID,APLACCR       INSERT EYE-CATCHER                           
         MVC   APLLNGTH,=Y(APLLEN) STORE LENGTH IN APL                          
         MVI   APLVBC,APLCLEAR     'CLEAR APL' VERB CODE                        
         PRNT  DVGCALL_CLEAR       CLEAR DVG PARAMETER AREAS                    
         DVGCALL PARM=ADVGAPLP     SETS UP APL                                  
         ST    RF,RETCODE          SAVE RETURN CODE                             
         PRNT  DVGCALL_RETURN                                                   
         OC    RETCODE,RETCODE     CHECK API RETURN CODE                        
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   APLVID,APLVIDC      INSERT APL VERSION                           
         L     R1,=A(MSGBUFF)      MSG AREA                                     
         ST    R1,APLMAPTR         A(MSG AREA)                                  
         MVC   APLMSGLN,=F'1024'   SET LENGTH OF MSG AREA                       
         LA    R1,FBASPACE         QRA FEEDBACK AREA                            
         ST    R1,APLFBAP                                                       
         MVC   APLFBALN,=F'133'    SET LENGTH OF FEEDBACK AREA                  
         DROP  R4                                                               
*                                                                               
         J     XIT                                                              
         EJECT                                                                  
APIERROR NTR1                                                                   
*                                                                               
* REPORT AN API ERROR.                                                          
*                                                                               
         L     R4,=A(APLACCR)      A(APL)                                       
         USING APLACCR,R4                                                       
*                                                                               
         MVC   P+11(28),=C'*** ERROR FROM API INTERFACE'                        
         GOTO1 =V(PRINTER)                                                      
         OC    APLMACT,APLMACT     ANY MESSAGES IN MESSAGE AREA?                
         BZ    APIERR20            NO                                           
*                                                                               
         MVC   P(14),=C'ACTUAL MSGS = '                                         
         EDIT  (B4,APLMACT),(10,P+14)                                           
         MVC   P+24(16),=C',  TOTAL MSGS = '                                    
         EDIT  (B4,APLMTOT),(10,P+40)                                           
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         L     R2,APLMACT          NUMBER OF MESSAGES IN APLMAPTR               
         L     R3,APLMAPTR         A(MESSAGE AREA)                              
APIERR10 ZIC   R1,3(R3)            LENGTH OF ONE MESSAGE                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P(0),4(R3)          MOVE MESSAGE INTO PRINT LINE                 
         GOTO1 =V(PRINTER)                                                      
         LA    R1,1(R1)            RESTORE MESSAGE LENGTH                       
         LA    R3,4(R1,R3)         BUMP TO NEXT MESSAGE LENGTH                  
         BCT   R2,APIERR10         PRINT ALL MESSAGES                           
*                                                                               
APIERR20 MVC   P(12),=C'VERB CODE = '                                           
         MVC   P+12(1),APLVBC                                                   
         MVC   P+13(17),=C',  RETURN CODE = '                                   
         EDIT  (B4,APLRC),(10,P+30),ZERO=NOBLANK                                
         MVC   P+40(17),=C',  REASON CODE = '                                   
         EDIT  (B4,APLRSN),(10,P+57),ZERO=NOBLANK                               
         GOTO1 =V(PRINTER)                                                      
         DROP  R4                                                               
*                                                                               
         J     XIT                                                              
         EJECT                                                                  
QUERY    NTR1                                                                   
*                                                                               
* QUERY ON FTP TRANSMISSIONS.                                                   
*                                                                               
         CLI   QUERYFLG,C'N'       SUPPRESS QUERY?                              
         BE    QUERYX              YES                                          
*                                                                               
         L     R4,=A(APLACCR)      A(APL)                                       
         USING APLACCR,R4                                                       
*                                                                               
         MVI   QUERY30+1,0         SELF-MODIFYING CODE                          
         XC    REFCOUNT,REFCOUNT                                                
*                                                                               
QUERY10  GOTO1 =A(APLINITL),(RC)   INITIALIZE FTP BLOCKS                        
*                                                                               
         MVI   APLVBC,APLQRYAR     'QUERY ALL' VERB CODE                        
         MVC   APLRQNUM,REFCOUNT   STARTING REQUEST NUMBER                      
*                                                                               
         PRNT  DVGCALL_QUERYALL    ISSUE 'QUERY ALL'                            
         DVGCALL PARM=ADVGAPLP     SETS UP API                                  
         ST    RF,RETCODE          SAVE RETURN CODE                             
         PRNT  DVGCALL_RETURN                                                   
         OC    RETCODE,RETCODE     CHECK API RETURN CODE                        
         BZ    QUERY20             OK                                           
*                                                                               
         CLC   APLRC,=F'4'         IF ERROR IS THAT QRA IS TOO SMALL...         
         BNE   *+14                                                             
         CLC   APLRSN,=AL4(196)                                                 
         BE    QUERY20             ... THEN IT'S OK                             
*                                                                               
         GOTO1 =A(APIERROR),(RC)                                                
         B     QUERYX                                                           
*                                                                               
QUERY20  OC    APLQACNT,APLQACNT   ANYTHING FOUND?                              
         BZ    QUERYX              NO                                           
*                                                                               
QUERY30  BC    0,QUERY40           PRINT HEADLINES ONLY ONCE                    
         MVI   QUERY30+1,X'F0'     SELF-MODIFYING CODE                          
         L     RF,=A(QSRHD1)                                                    
         MVC   P(L'QSRHD1),0(RF)                                                
         GOTO1 =V(PRINTER)                                                      
         L     RF,=A(QSRHD2)                                                    
         MVC   P(L'QSRHD2),0(RF)                                                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
QUERY40  L     R6,APLFBAP          A(QRA FEEDBACK AREA)                         
         USING DVGQSR,R6                                                        
         MVC   P(QSRLEN),QSR                                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XC    REFCOUNT,REFCOUNT                                                
         CLC   APLQTCNT,APLQACNT   ANY MORE TO QUERY?                           
         BE    *+10                NO                                           
         MVC   REFCOUNT,APLRQNUM   SAVE NEXT REQUEST NUMBER TO QUERY            
*                                                                               
         CLI   QSRSTA,QSRWAIT      REQUEST STILL WAITING?                       
         BE    QUERY90                                                          
         CLI   QSRSTA,QSRACTI      REQUEST ACTIVE RIGHT NOW?                    
         BE    QUERY90                                                          
         CLI   QSRSTA,QSRHOLD      REQUEST ON HOLD?                             
         BE    QUERY90                                                          
*                                                                               
         CLI   QSRSTA,QSRFINI      TRANSFER FINISHED?                           
         BNE   QUERY90             UNKNOWN STATUS                               
         CLC   QSRTRC,=C'00000'    RETURN CODE                                  
         BNE   QUERY90                                                          
         CLC   QSRTRS,=C'0000'     REASON CODE                                  
         BNE   QUERY90                                                          
*                                                                               
         MVC   WORK(8),QSRRQNAM    REQUEST NAME IS REFERENCE NUMBER             
         LA    R1,DMCB                                                          
         USING CONVDSKD,R1                                                      
         MVC   CONVDMGR,SADTAMGR   A(DATAMGR)                                   
         MVI   CONVACTN,CONVDSKO   CONVERT REFERENCE NUMBER TO DSKADDR          
         MVC   CONVOFF,=A(WORK)                                                 
         GOTO1 =V(CONVDSKA)                                                     
         MVC   FULL,CONVDSK        RETURN EDICT FILE DSKADDR IN FULL            
         DROP  R1                                                               
*                                                                               
         LA    R3,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R3)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(3),E,8)  ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         LA    R2,TABLNTRY         ASSUME WE'LL FIND REPORT IN TABLE            
         MVI   BYTE,C'T'           'T' FOR TABLE                                
*                                                                               
         L     R7,SXMTTBLE         A(TRANSMIT TABLE)                            
         USING XMTTABLD,R3                                                      
         LA    R3,XMTENTRY                                                      
*                                                                               
QUERY60  MVC31 XMTENTRY,0(R7)                                                   
         CLI   0(R3),0             END OF TABLE?                                
         BNE   *+16                                                             
         L     R2,FULL             YES -- USE EDICT FILE DISK ADDRESS           
         MVI   BYTE,C'F'           'F' FOR FILE                                 
         B     QUERY80                                                          
*                                                                               
         CLI   XMTSOURC,XMTSRCPQ   PRINT QUEUE ENTRY?                           
         BNE   *+14                                                             
         CLC   FULL,XMTDSKAD       FIND MATCH ON DISK ADDRESS                   
         BE    *+12                                                             
         AHI   R7,XMTTBLQ          BUMP TO NEXT ENTRY                           
         B     QUERY60                                                          
*                                                                               
         MVC   TABLNTRY,0(R3)      SAVE XMIT TABLE KEY                          
         DROP  R3                                                               
*                                                                               
QUERY80  LA    R3,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R3)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(3),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         ST    R2,ETBLNTRY                                                      
         MVI   EACTION,EACTDLVQ    MARK DELIVERED                               
         MVI   SVACTION,C'D'                                                    
         CLI   BYTE,C'F'           DISK ADDRESS BEING PASSED?                   
         BNE   *+8                                                              
         OI    EACTION,EACTFILQ    YES                                          
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EFILREC,=A(WORK)                                                 
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BE    QUERY85             EDICT FILE UPDATE OKAY                       
         BRAS  RE,BLDMQS           BUILD MQ STATUS MESSAGE                      
         B     QUERY90             CAN'T POST EDICT FILE                        
         DROP  R1                                                               
QUERY85  EQU   *                                                                
         BRAS  RE,BLDMQS           BUILD MQ STATUS MESSAGE                      
*                                                                               
         LA    R3,WORK                                                          
         USING EDFILD,R3                                                        
         PACK  DUB,QSRURI          REQUEST NUMBER FROM QRA. . .                 
         CVB   R0,DUB                                                           
         C     R0,EDFFTPRN         . . . MUST MATCH NO. IN EDICT FILE           
         BE    QUERY87                                                          
*                                  IT DOESN'T, SEND WARNING EMAIL               
         WTO   TEXT=OPMSG1H                                                     
         DROP  R3                                                               
**********************************************************************          
*                                  DON'T DELETE IT FOR NOW, 5/8/09   *          
*                                  NOOP THIS/USE NFTP ADM TO PURGE IT*          
         B     QUERY90                                               *          
**********************************************************************          
*                                                                               
QUERY87  EQU   *                                                                
         GOTO1 =A(APLINITL),(RC)   INITIALIZE FTP BLOCKS                        
         MVI   APLVBC,APLDELSR     'DELETE' VERB CODE                           
         PACK  DUB,QSRURI          REQUEST NUMBER FROM QRA. . .                 
         CVB   R0,DUB                                                           
         ST    R0,APLRQNUM                                                      
         PRNT  DVGCALL_DELETE      DELETE FROM QUEUE HANDLER                    
         DVGCALL PARM=ADVGAPLP     SETS UP API                                  
         ST    RF,RETCODE          SAVE RETURN CODE                             
         PRNT  DVGCALL_RETURN                                                   
         OC    RETCODE,RETCODE     CHECK API RETURN CODE                        
         BZ    QUERY90             OK                                           
         DROP  R6                                                               
*                                                                               
         GOTO1 =A(APIERROR),(RC)                                                
         B     QUERYX                                                           
*                                                                               
QUERY90  OC    REFCOUNT,REFCOUNT   ANY MORE TO QUERY?                           
         BNZ   QUERY10             YES                                          
         DROP  R4                                                               
*                                                                               
QUERYX   J     XIT                                                              
         EJECT                                                                  
TIMERXIT SAVE  (14,12),,*                                                       
         LR    R4,RF                                                            
         USING TIMERXIT,R4                                                      
         POST  TIMERECB                                                         
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
         SPACE 2                                                                
TIMERECB DS    F                                                                
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
READCRDS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* READ PARAMETER CARDS BETWEEN "++FTP" AND "++FTPEND" CARDS                     
*                                                                               
*   *......          "*" IN COLUMN ONE IS A COMMENT CARD                        
*   WAITSECS=N       WAIT N SECONDS BETWEEN FTP QUEUE HANDLER QUERIES           
*   QUERY=YES/NO     PURGE COMPLETED QUEUE ENTRIES, DATASETS, MARK DLVD         
*                     (DEFAULT=YES)                                             
*   TRACE=YES        OVERRIDE TRACE FLAG                                        
*   MQQMGRNAME=CL48  MQ Q MANAGER NAME                                          
*   MQQUEUENAME=CL48 MQ Q NAME                                                  
*                                                                               
RC10     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++FTP',CARD      LOOK FOR START OF PARAMETERS                 
         BNE   RC10                                                             
*                                                                               
RC20     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++FTPEND',CARD   LOOK FOR END OF PARAMETERS                   
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
         CLC   =C'WAITSECS=',CARD  WAITSECS=N                                   
         BNE   RC30                                                             
         GOTO1 =V(NUMVAL),DMCB,CARD+9,(2,0)                                     
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                NOT NUMERIC                                  
         L     R1,DMCB+4                                                        
         MHI   R1,100              SCALE THE TIME INTERVAL FOR STIMER           
         ST    R1,WAITSECS                                                      
         B     RC20                                                             
*                                                                               
RC30     CLC   =C'TRACE=',CARD     TRACE=                                       
         BNE   RC40                                                             
         CLC   =C'NO',CARD+6                                                    
         BE    RC20                DON'T OVERRIDE                               
         CLC   =C'YES',CARD+6                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRACEFLG,C'Y'       TRACE=YES                                    
         B     RC20                                                             
*                                                                               
RC40     CLC   =C'QUERY=',CARD     QUERY=                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'YES',CARD+6                                                   
         BE    RC20                DON'T OVERRIDE                               
         CLC   =C'NO',CARD+6                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   QUERYFLG,C'N'       QUERY=NO                                     
         B     RC20                                                             
*                                                                               
RCX      GOTO1 =V(PRINTER)         SKIP A LINE                                  
*                                                                               
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
BLDMQS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*UPON ENTRY, WORK = THE EDICT FILE ENTRY                                        
*                                                                               
         LA    RE,MQBUFF                                                        
         LHI   RF,MQBUFFL                                                       
         XCEFL                                                                  
*                                                                               
         LA    R1,WORK                                                          
         USING EDFILD,R1                                                        
*                                                                               
         CLC   =C'MOEC',EDFMQAPP                                                
         BNE   BMQSX                                                            
*                                                                               
         LA    R3,MQBUFF                                                        
         USING ECMQMSGD,R3                                                      
         MVC   ECMBKLAB,MQSLAB                                                  
         MVC   ECMLABEL,=CL10'NFTP'                                             
         MVC   ECMDEST,EDFDEST                                                  
         MVC   ECMAPPL,EDFAPPL                                                  
         GOTO1 =V(DATCON),DMCB,(5,0),(7,ECMDATE)                                
*                                                                               
         TIME  DEC                                                              
         ST    R0,DUB                                                           
         MVI   DUB+3,X'0C'                                                      
*                                  CONVERT DDS HR TO REAL 24-HR                 
         AP    DUB(4),=P'600000'       + 6 HR                                   
         CP    DUB(4),=P'2400000'      > 24 HR ?                                
         BL    *+10                                                             
         SP    DUB(4),=P'2400000'      - 24 HR                                  
         UNPK  PRNTTIME(7),DUB(4)                                               
         MVC   ECMTIME,PRNTTIME                                                 
*                                                                               
         MVC   ECMSTAT,SVACTION                                                 
*                                                                               
         CLI   SVACTION,C'C'                                                    
         BNE   BMQS20                                                           
         MVC   ECMEMSG(8),=CL8'ERROR # '                                        
         SR    R0,R0                                                            
         IC    R0,SVERROR                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ECMEMSG+8(5),DUB                                                 
*                                                                               
         DROP  R1,R3                                                            
*                                                                               
BMQS20   LHI   RE,ECMQMSGQ                                                      
         ST    RE,DATALENGTH                                                    
*                                                                               
         MVC   OBJDESC_OBJECTTYPE,=A(MQOT_Q)  OBJECT IS A QUEUE                 
         LA    RF,MQPMO_NO_SYNCPOINT                                            
         ICM   RE,15,=AL4(MQPMO_FAIL_IF_QUIESCING)                              
         AR    RF,RE                                                            
         ST    RF,PUTMSGOPTS_OPTIONS                                            
         MVC   MSGDESC_PERSISTENCE,=A(MQPER_PERSISTENT)                         
*                                                                               
         LA    R2,CSQBPUT1_STRUCTURE                                            
         BRAS  RE,CALLMQ           PUT THE MESSAGE TO THE MQ QUEUE              
*                                                                               
BMQSX    J     XIT                                                              
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
         MVC   P+50(L'OBJDESC_OBJECTNAME),OBJDESC_OBJECTNAME                    
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
*                                                                               
CMQOK    CR    RB,RB               SET CC EQUAL                                 
*                                                                               
CMQX     XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
COMMWORK DS    0D                  COMMON STORAGE AREA                          
DMWORK   DS    12D                                                              
DMCB     DS    10F                                                              
DUB      DS    D                                                                
PRNTDUB  DS    D                   FOR PRNT MACRO                               
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
SVACTION DS    X                                                                
SVERROR  DS    X                                                                
*                                                                               
OPMSG1H  DS    0C                                                               
         DC    AL2(OPMSG1Q)                                                     
OPMSG1   DC    C'AUTONOTE*YYUNN,AWILN,AHYDN:NFTP REQUEST# NOT MATCH'            
         DC    C'  '                                                            
OPMSG1Q  EQU   *-OPMSG1                                                         
*                                                                               
WORK     DS    CL256                                                            
PRNTTIME DS    CL9                 FOR PRNT MACRO                               
OPERSTOP DC    C'N'                'Y' IF OPERATOR WANTS TO STOP                
DYNFLAG  DS    C                   'Y' IF WE'VE DONE DYNALLOC                   
DDSTRN   DS    C                   'Y' IF WE'VE SEEN ++DDS TRN RECORD           
ERRCODE  DS    X                   ERROR REASON CODE                            
TRACEFLG DS    C                   'Y' = PRINT DETAILED TRACE                   
QUERYFLG DC    C'Y'                'N' = DON'T DO 'QUERY ALL'                   
         SPACE 2                                                                
ECBLST   DS    0F                                                               
ALOOKECB DC    X'00',AL3(0)        A(LOOKECB)                                   
ATIMRECB DC    X'00',AL3(TIMERECB) A(TIMERECB)                                  
ASTOPECB DC    X'80',AL3(0)        A(STOPECB)                                   
         SPACE 2                                                                
REFCOUNT DS    F                   QUEUE HANDLER REFERENCE # COUNTER            
RETCODE  DS    F                   RETURN CODE FROM DVGCALL                     
WAITSECS DC    F'60000'            WAIT TEN MINUTES BETWEEN QUERIES             
STIMERID DS    XL4                 FOR TIMER BETWEEN QUERIES                    
DSTNAME  DS    CL8                 DESTINATION                                  
RECCOUNT DS    F                   RECORD COUNT IN FTP DATASET                  
FTPOPSYS DS    C                   FTP OPERATING SYSTEM                         
FTPLUID  DS    CL8                 FTP REMOTE LUNAME                            
FTPUID   DS    CL10                FTP APPC USERID                              
FTPPASS  DS    CL10                FTP APPC PASSWORD                            
FTPCLASS DS    C                   FTP APPC SERVER CLASS                        
FTPFLAGS DS    X                   FTP FLAGS                                    
         SPACE 2                                                                
* FIRST CHARACTER OF DSN QUALIFER MUST BE A LETTER, SO MAP THE LEFTMOST         
* DIGIT OF THE REFERENCE NUMBER TO A SEMI-ARBITRARY LETTER                      
*                                                                               
DSNMAP   DC    C'0O1P2Q3R4S5T6U7V8W9XAABBCCDDEEFF',X'FF'                        
         EJECT                                                                  
FTPFILE  DCB   DDNAME=FTPFILE,MACRF=PM,DSORG=PS,RECFM=FB                        
         SPACE 2                                                                
LRECLDEF EQU   165                 DEFAULT LRECL OF MVS DATASET                 
         EJECT                                                                  
* CREATE FTPFILE DATASETS -- DYNAMIC ALLOCATION                                 
*                                                                               
         DS    0F                                                               
ARBLKCTA DC    X'80',AL3(RBLKCATA) R1 POINTS TO THIS BEFORE DYNALLOC            
*                                                                               
RBLKCATA DC    X'1401000000000000',A(ACATALLT),X'0000000000000000'              
*                                                                               
ACATALLT DC    X'00',AL3(TXTDD)                                                 
         DC    X'00',AL3(TXTDSN)                                                
FILESTAT DC    X'00',AL3(0)                                                     
FILEDISP DC    X'00',AL3(0)                                                     
         DC    X'00',AL3(TXTBLKLN)                                              
         DC    X'00',AL3(TXTPRI)                                                
         DC    X'00',AL3(TXTSEC)                                                
         DC    X'00',AL3(TXTRLSE)                                               
         DC    X'00',AL3(TXTAVGR)                                               
         DC    X'80',AL3(TXTUNIT)                                               
         SPACE 2                                                                
* FTPFILE DATASETS -- DYNAMIC UNALLOCATION BY DDNAME                            
*                                                                               
         DS    0F                                                               
ARBLKUNA DC    X'80',AL3(RBLKUNA)  R1 POINTS TO THIS BEFORE DYNALLOC            
*                                                                               
RBLKUNA  DC    X'1402000000000000',A(ACATUNT),X'0000000000000000'               
*                                                                               
ACATUNT  DC    X'80',AL3(TXTUNDD)                                               
         SPACE 2                                                                
* DELETE FTPFILE DATASETS -- DYNAMIC ALLOCATION                                 
*                                                                               
         DS    0F                                                               
ARBLKDLA DC    X'80',AL3(RBLKDELA) R1 POINTS TO THIS BEFORE DYNALLOC            
*                                                                               
RBLKDELA DC    X'1401000000000000',A(ADELALLT),X'0000000000000000'              
*                                                                               
ADELALLT DC    X'00',AL3(TXTDSN)                                                
         DC    X'00',AL3(TXTSTATO)                                              
         DC    X'80',AL3(TXTNDISD)                                              
         SPACE 2                                                                
* FTPFILE DATASETS -- DYNAMIC UNALLOCATION BY DSN                               
*                                                                               
         DS    0F                                                               
ARBLKUN2 DC    X'80',AL3(RBLKUNA2) R1 POINTS TO THIS BEFORE DYNALLOC            
*                                                                               
RBLKUNA2 DC    X'1402000000000000',A(ACATUNT2),X'0000000000000000'              
*                                                                               
ACATUNT2 DC    X'80',AL3(TXTDSN)                                                
         SPACE 3                                                                
TXTDD    DC    AL2(DALDDNAM),X'00010007',C'FTPFILE'    DDNAME=FTPFILE           
TXTUNDD  DC    AL2(DUNDDNAM),X'00010007',C'FTPFILE'    DDNAME=FTPFILE           
TXTDSN   DC    AL2(DALDSNAM),X'0001002C',CL44' '       DSN=............         
TXTSTATN DC    AL2(DALSTATS),X'0001000104'             DISP=(NEW,.....)         
TXTNDISP DC    AL2(DALNDISP),X'0001000102'                 =(...,CATLG)         
TXTSTATO DC    AL2(DALSTATS),X'0001000101'             DISP=(OLD,...)           
TXTNDISD DC    AL2(DALNDISP),X'0001000104'                 =(...,DEL)           
TXTNDISK DC    AL2(DALNDISP),X'0001000108'                 =(...,KEEP)          
TXTBLKLN DC    AL2(DALBLKLN),X'00010003',AL3(0)        SPACE=(NNN,.....         
TXTPRI   DC    AL2(DALPRIME),X'00010003',AL3(0)             =(.(NN,....         
TXTSEC   DC    AL2(DALSECND),X'00010003',AL3(0)             =(....,NN),         
TXTRLSE  DC    AL2(DALRLSE),X'0000'                         =(...,RLSE)         
TXTUNIT  DC    AL2(DALUNIT),X'00010005',C'SYSDA'       UNIT=SYSDA               
TXTAVGR  DC    AL2(DALAVGR),X'0001000180'              AVGREC=U                 
         EJECT                                                                  
ADVGAPLP DC    A(DVGAPL)           PARAMETER FOR DVGCALL                        
         SPACE 2                                                                
TABLNTRY DS    XL(XMTTBLQ)         SAVED XMIT TABLE ENTRY                       
XMTENTRY DS    XL(XMTTBLQ)         TEMP STORAGE FOR XMIT TABLE ENTRY            
MAJORNAM DS    CL8                 MAJOR RESOURCE NAME                          
FTPDATA  DS    0XL21                                                            
FTPREFNO DS    XL4                 FTP REFERENCE NUMBER                         
FTPRCVLU DS    CL8                 FTP RECEIVING LUID                           
FTPSUBTP DS    CL2                 SUBTYPE                                      
         DS    XL7                 SPARE                                        
DYNAMDSN DC    C'DDSFTP.EDICTX.RCVGLUID.UIDNNNNN.ADNNNNNN.TUU'                  
FTPDSN   DS    CL44                DSN TO BE SENT VIA FTP                       
FTPRQNAM DS    CL8                 FTP REQUEST NAME                             
         SPACE 2                                                                
         DS    0D                                                               
FBASPACE DS    CL133               FTP QUERY RESPONSE AREA                      
         SPACE 2                                                                
CARD     DS    CL80                FOR CONTROL CARDS AND EDICTFIL RECS          
         DS    XL4                 PQ RECORD LENGTH                             
R        DS    CL256               PQ RECORD DATA                               
         DS    XL4                                                              
EDICTHDR DS    CL256               EDICT HDR* RECORD                            
         DS    XL4                                                              
PQRPTHDR DS    CL256               PQ REPORT HEADER                             
         SPACE 2                                                                
         DS    0D                                                               
MSGBUFF  DS    1024C               MESSAGE AREA FOR FTP                         
         EJECT                                                                  
* VALID CHARACTERS FOR DSN: FIRST CHARACTER OF A QUALIFER                       
*  (A..Z)  $  @  #                                                              
*                   0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                            
DSNCHAR1 DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 00-0F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 10-1F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 20-2F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 30-3F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 40-4F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFF' 50-5F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 60-6F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFF0000FFFFFF' 70-7F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 80-8F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 90-9F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' A0-AF                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' B0-BF                     
         DC    XL16'FF000000000000000000FFFFFFFFFFFF' C0-CF                     
         DC    XL16'FF000000000000000000FFFFFFFFFFFF' D0-DF                     
         DC    XL16'FFFF0000000000000000FFFFFFFFFFFF' E0-EF                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' F0-FF                     
         SPACE 2                                                                
* VALID CHARACTERS FOR DSN: REMAINING CHARACTERS OF A QUALIFIER                 
*  (A..Z) (0..9)  $  @  #  -  {                                                 
*                   0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                            
DSNCHARN DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 00-0F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 10-1F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 20-2F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 30-3F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 40-4F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFF' 50-5F                     
         DC    XL16'00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 60-6F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFF0000FFFFFF' 70-7F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 80-8F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 90-9F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' A0-AF                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' B0-BF                     
         DC    XL16'00000000000000000000FFFFFFFFFFFF' C0-CF                     
         DC    XL16'FF000000000000000000FFFFFFFFFFFF' D0-DF                     
         DC    XL16'FFFF0000000000000000FFFFFFFFFFFF' E0-EF                     
         DC    XL16'00000000000000000000FFFFFFFFFFFF' F0-FF                     
         EJECT                                                                  
         CMQA LIST=YES,EQUONLY=NO                                               
* MQSERIES CALL PARAMETERS                                                      
*                                                                               
QMGRNAME       DS      CL48      MQ QUEUE MANAGER NAME                          
HCONN          DS      F         MQ QMGR CONNECTION HANDLE                      
HOBJ           DS      F         OBJECT HANDLE                                  
COMPCODE       DS      F         COMPLETION CODE                                
REASON         DS      F         QUALIFIES COMPLETION CODE                      
MQBUFFLENGTH   DS      F         LENGTH OF MQ BUFFER AREA                       
DATALENGTH     DS      F         LENGTH OF THE MESSAGE                          
OBJDESC        CMQODA  LIST=YES  OBJECT DESCRIPTOR                              
MSGDESC        CMQMDA  LIST=YES  MESSAGE DESCRIPTOR                             
         ORG   MSGDESC_FORMAT                                                   
         DC    CL8'MQSTR   '           MQFMT_STRING  FOR DATA FORMAT            
         ORG                                                                    
PUTMSGOPTS     CMQPMOA DSECT=NO,LIST=YES     PUT MESSAGE OPTIONS                
         SPACE 2                                                                
MQBUFF   DS    (MQBUFFL)X                                                       
MQBUFFL  EQU   256                 MQ BUFFER LENGTH                             
*                                                                               
MQSLAB   DC    CL16'MO_ECSTAT_?*****'                                           
*                                                                               
         SPACE 2                                                                
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
                          DC    X'00',AL3(DATALENGTH)                           
                          DC    X'00',AL3(MQBUFF)                               
                          DC    X'00',AL3(COMPCODE)                             
                          DC    X'80',AL3(REASON)                               
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
         DC    C'*CXREC**'                                                      
CXREC    DC    14336X'00'          PRINT QUEUE INDEX BUFFER                     
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
         SPACE 3                                                                
         DS    0D                                                               
         DVGAPL                                                                 
         EJECT                                                                  
       ++INCLUDE DDEDICTWRK                                                     
* DDDPRINT                                                                      
* DDEDICTFIL                                                                    
* DMPRTQL                                                                       
* MACRO:  DCBD  DSORG=PS,DEVD=DA                                                
* MACRO:  IEFZB4D2                                                              
* MACRO:  DVGQSR DSECT=YES                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DDEDICTFIL                                                     
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
         IEFZB4D2                                                               
         EJECT                                                                  
EDIFTP   CSECT                                                                  
         SPACE 2                                                                
         DVGQSR DSECT=YES                                                       
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'085DDEDIFTP  03/09/16'                                      
         END                                                                    
