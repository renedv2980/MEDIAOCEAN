*          DATA SET DDEDIADVN  AT LEVEL 055 AS OF 05/01/02                      
***********************************************************************         
*                                                                               
* !THIS PROGRAM DOESN'T SUPPORT 31-BIT MODE ADDRESSING!                         
*                                                                               
* 5/21/99, YI, CURRENTLY THE XMTTABLE IS BROUGHT ABOUT THE 16MB LINE            
* THE ADDRESS MODE WILL BE NEEDED TO SET THE 31-BIT MODE WHEN ACCESSING         
* THE XMTTABLE.                                                                 
*                                                                               
***********************************************************************         
*PHASE EDIADVNA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE DATCON                                                                 
*INCLUDE EDISUB                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        EDIADVN -- TRANSMIT PQ REPORTS VIA ADVANTIS          *         
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
*                R7 -- S P A R E                                      *         
*                R8 -- S P A R E                                      *         
*                R9 -- SECOND PROGRAM BASE                            *         
*                RA -- CPRINT                                         *         
*                RB -- PROGRAM BASE                                   *         
*                RC -- WORK                                           *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'DDEDIADVN -- PERFORM ADVANTIS TRANSMISSIONS'                    
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
EDIADVN  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*EDIADVN,=A(R13CHAIN),R9                                       
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
         MVC   DCBDDNAM-IHADCB(,RF),=C'ADNTRACE'  DDNAME=ADNTRACE               
*                                                                               
         MVC   TITLE(23),=C'EDICT: ADVANTIS SUBTASK'                            
         PRNT  START_SUBTASK,PRINT=ALWAYS                                       
*                                                                               
         BAS   RE,READCRDS         READ PARAMETER CARDS                         
*                                                                               
         BAS   RE,INITIAL          INITIALIZE                                   
         EJECT                                                                  
LOOP     CLI   OPERSTOP,C'Y'       DOES OPERATOR WANT TO STOP?                  
         BE    STOP                                                             
*                                                                               
         PRNT  ABOUTTOWAIT,PRINT=ALWAYS                                         
*                                                                               
         WAIT  1,ECBLIST=ECBLST    WAIT FOR POST FROM MAIN                      
*                                                                               
         L     RF,ASTOPECB                                                      
         TM    0(RF),X'40'         STOP?                                        
         BZ    DONTSTOP            NO                                           
         XC    0(4,RF),0(RF)                                                    
*                                                                               
STOP     PRNT  EXITING,PRINT=ALWAYS                                             
*                                                                               
         XBASE                                                                  
*                                                                               
DONTSTOP L     RF,ALOOKECB                                                      
         TM    0(RF),X'40'         MAIN TASK POSTED READY?                      
         BO    *+6                 YES                                          
         DC    H'0'                                                             
         XC    0(4,RF),0(RF)                                                    
*                                                                               
         BAS   RE,XFERREPS         TRANSMIT ALL ADVANTIS REPORTS                
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
* READ PARAMETER CARDS BETWEEN "++ADVANTIS" AND "++ADVANTISEND" CARDS           
*                                                                               
*   *......          "*" IN COLUMN ONE IS A COMMENT CARD                        
*   TRACE=YES        OVERRIDE TRACE FLAG                                        
*                                                                               
RC10     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++ADVANTIS',CARD LOOK FOR START OF PARAMETERS                 
         BNE   RC10                                                             
*                                                                               
RC20     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++ADVANTISEND',CARD   LOOK FOR END OF PARAMETERS              
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
XFER10   L     R2,SMAJORNM         MAJOR RESOURCE NAME                          
         LA    R3,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),0(R2)                                                    
         MVC   P+40(8),0(R3)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   ((2),(3),E,8)       ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         L     R3,SXMTTBLE         R3 = A(REPORT TABLE)                         
         USING XMTTABLD,R3                                                      
         SR    R4,R4               R4 = A(EARLIEST ENTRY) --> FIFO              
*                                                                               
XFER20   CLI   0(R3),0             END OF TABLE?                                
         BE    XFER40              YES, NO MORE                                 
         CLI   XMTSOURC,XMTSRCPQ   PRINT QUEUE ENTRY?                           
         BNE   XFER30                                                           
         CLI   XMTMETH,C'A'        YES -- ADVANTIS METHOD?                      
         BNE   XFER30                                                           
         TM    XMTSTAT,EDFSTWTG    YES -- WAITING TO BE SENT?                   
         BZ    XFER30                                                           
*                                                                               
         LTR   R4,R4               IS THIS THE FIRST ONE WE'VE CHECKED?         
         BZ    *+14                YES, SO SAVE THE DAY/TIME                    
         CLC   XMTCRDTM-XMTTABLD(,R4),XMTCRDTM                                  
         BNH   XFER30                                                           
         LR    R4,R3               R4 = EARLIEST ENTRY FOUND SO FAR             
*                                                                               
XFER30   LA    R3,XMTTBLQ(,R3)     BUMP TO NEXT REPORT                          
         B     XFER20                                                           
         DROP  R3                                                               
*                                                                               
XFER40   LTR   R4,R4               ANYTHING FOUND TO SEND?                      
         BZ    XFER60              NO                                           
         MVC   TABLNTRY,0(R4)      SAVE XMIT TABLE KEY                          
*                                                                               
         L     R2,SMAJORNM         MAJOR RESOURCE NAME                          
         LA    R3,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),0(R2)                                                    
         MVC   P+40(8),0(R3)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   ((2),(3),8)         DEQUEUE THE TRANSMIT TABLE                   
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
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   XFER50              COULD NOT UPDATE EDICT FILE                  
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
         MVI   EACTION,EACTJNKQ    MARK UNSENDABLE                              
         MVI   EERRORCD,EDFERNED   EDICT RECORD WAS DELETED                     
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   XFER50              COULD NOT UPDATE EDICT FILE                  
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
         B     XFER50                                                           
         DROP  R1                                                               
*                                                                               
         USING DESTTABD,R1                                                      
XFER48   MVC   DSTNAME,DESTNAME    DESTINATION                                  
         MVC   ADVNACCT,DESTADNA   ADVANTIS ACCOUNT                             
         MVC   ADVNUID,DESTADNU    ADVANTIS USERID                              
         MVC   ADVNCLS,DESTADNC    ADVANTIS CLASS                               
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
         BAS   RE,SENDADVN                                                      
*                                                                               
XFER50   L     RF,ASTOPECB                                                      
         TM    0(RF),X'40'         OPERATOR WANTS TO STOP?                      
         BZ    XFER10              NO                                           
*                                                                               
         XC    0(4,RF),0(RF)                                                    
         MVI   OPERSTOP,C'Y'                                                    
         B     XFERX                                                            
*                                                                               
XFER60   L     R2,SMAJORNM         MAJOR RESOURCE NAME                          
         LA    R3,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),0(R2)                                                    
         MVC   P+40(8),0(R3)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   ((2),(3),8)         DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
XFERX    B     XIT                                                              
         EJECT                                                                  
SENDADVN NTR1                                                                   
*                                                                               
* TRANSFER A REPORT VIA ADVANTIS.                                               
*                                                                               
         MVI   ERRCODE,0                                                        
*                                                                               
         LA    RF,PQRPTHDR         PRINT QUEUE REPORT HEADER                    
         ZIC   RE,QLLINEW-PQPLD(,RF)  PICK UP REPORT WIDTH                      
         LA    RF,ADVNFILE         A(MVS DATASET DCB)                           
         USING IHADCB,RF                                                        
         STH   RE,DCBLRECL         SET LRECL IN DCB                             
         XC    DCBBLKSI,DCBBLKSI   CLEAR BLKSIZE                                
         DROP  RF                                                               
         MH    RE,=H'100'                                                       
         STCM  RE,7,TXTBLKLN+6     SET BLKSIZE IN DYNALLOC BLOCK                
*                                                                               
         L     RF,SMAJORNM                                                      
         MVC   ADVNDSN+18(1),5(RF)    'A' FOR ADV, 'R' FOR REP                  
*                                                                               
         LA    R3,TABLNTRY                                                      
         USING XMTTABLD,R3                                                      
*                                                                               
         LA    R1,DMCB                                                          
         USING CONVDSKD,R1                                                      
         MVC   CONVDMGR,SADTAMGR   A(DATAMGR)                                   
         MVI   CONVACTN,CONVDSKF   CONVERT DSKADDR TO REFERENCE NUMBER          
         MVC   CONVDSK,XMTDSKAD    EDICT FILE DISK ADDRESS                      
         MVC   CONVOFF,=A(ADVNDSN+20)  RESULT GOES INTO DSN                     
         GOTO1 =V(CONVDSKA)                                                     
         DROP  R1                                                               
         DROP  R3                                                               
*                                                                               
         LA    RF,DSNMAP                                                        
SNDAD10  CLI   0(RF),X'FF'         END OF LIST?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   ADVNDSN+20(1),0(RF) FIND 1ST DIGIT OF REF. NUMBER                
         BE    *+12                                                             
         LA    RF,2(RF)                                                         
         B     SNDAD10                                                          
         MVC   ADVNDSN+20(1),1(RF) QUALIFIER NOW BEGINS WITH ALPHA              
         MVI   BYTE,C'C'           SET FLAG -- ADVNFILE IS CLOSED               
*                                                                               
SNDAD20  LA    R1,DMCB                                                          
         USING PQPARMD,R1                                                       
         MVC   ATBLNTRY,=A(TABLNTRY)                                            
         MVC   APQBUFF,=A(CXREC)                                                
         MVC   APQHDR,=A(PQRPTHDR)                                              
         MVC   AEDHDR,=A(EDICTHDR)                                              
         MVC   APQLINE,=A(R)                                                    
         MVC   ACITABLE,SCITABLE                                                
         MVC   ADTAMGR,SADTAMGR                                                 
         GOTO1 =V(PQGETLIN)                                                     
         BNE   SNDAD50             NO MORE REPORT LINES                         
         DROP  R1                                                               
*                                                                               
         CLC   =C'++DDS',R+1       HEADING INFORMATION?                         
         BNE   SNDAD40             NO                                           
         CLC   =C'TRN',R+12        IS THIS THE TRANSACTION DATA CARD?           
         BNE   SNDAD20             NO                                           
         MVC   TXTDSN+6(44),ADVNDSN                                             
*                                                                               
         LA    R1,TXTSTATN         FOR 'NEW' STATUS                             
         STCM  R1,7,FILESTAT+1                                                  
         LA    R1,TXTNDISP         FOR 'CATLG' DISPOSITION                      
         STCM  R1,7,FILEDISP+1                                                  
         LA    R1,ARBLKCTA         DYNAMIC ALLOCATION PARAMETER BLOCK           
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         BZ    SNDAD30             DATASET WAS CREATED OK                       
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
         BZ    SNDAD30             DATASET WAS FOUND OK                         
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
SNDAD30  OPEN  (ADVNFILE,OUTPUT)                                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   BYTE,C'O'           SET FLAG -- ADVNFILE IS OPEN                 
         B     SNDAD20                                                          
*                                                                               
SNDAD40  CLI   BYTE,C'O'           IS ADVNFILE OPEN?                            
         BE    *+18                                                             
         MVC   P+30(36),=C'NO ++DDS TRN CARD ON ADVANTIS REPORT'                
         MVI   ERRCODE,3           ERROR CODE                                   
         B     SNDADBAD            NO -- SOMETHING'S WRONG WITH REPORT          
*                                                                               
         PUT   ADVNFILE,R+1        PUT CARD TO ADVNFILE (W/OUT CRG CTL)         
         B     SNDAD20                                                          
*                                                                               
SNDAD50  CLOSE (ADVNFILE)                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         FREEPOOL ADVNFILE                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (INMSG,OUTPUT)                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RF,INMSGACC+14      BUILD ACCOUNT PARAMETER CARD                 
         LA    RE,ADVNACCT                                                      
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
         MVI   0(RF),C')'                                                       
*                                                                               
         LA    RF,INMSGUID+13      BUILD USERID PARAMETER CARD                  
         LA    RE,ADVNUID                                                       
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
         MVI   0(RF),C')'                                                       
*                                                                               
         LA    RF,INMSGCLS+12      BUILD CLASS PARAMETER CARD                   
         LA    RE,ADVNCLS                                                       
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
         MVI   0(RF),C')'                                                       
*                                                                               
         LA    R2,INMSGDTA         PUT ALL CARDS TO INPUT DATASET               
SNDAD60  PUT   INMSG,(2)                                                        
         LA    R2,80(R2)                                                        
         C     R2,=A(INMSGDTX)     ANY MORE DATA CARDS?                         
         BNE   SNDAD60                                                          
*                                                                               
         CLOSE (INMSG)                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R1,R1                                                            
         LINK  EP=IEBASE                                                        
         LTR   RF,RF               CHECKS RETURN CODE                           
         BZ    SNDAD70             OK                                           
         PRNT  ***ERROR_SENDING                                                 
         DC    H'0'                *** TEMPORARY *** DIE FOR NOW                
         B     SNDADX                                                           
*                                                                               
SNDAD70  MVC   TXTDSN+6(44),ADVNDSN PURGE THE DATASET WE JUST ADDED             
         LA    R1,ARBLKDLA         DYNAMIC ALLOCATION PARAMETER BLOCK           
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         BZ    SNDAD80                                                          
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
SNDAD80  LA    R1,ARBLKUN2         DYNAMIC UNALLOCATION PARAMETER BLOCK         
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         BZ    SNDAD90             THE DATASET IS NOW PURGED                    
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
         DC    H'0'                                                             
*                                                                               
SNDAD90  PRNT  SENTVIAADVANTIS,PRINT=ALWAYS                                     
*                                                                               
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         MVC   ETBLNTRY,=A(TABLNTRY)                                            
         MVI   EACTION,EACTSNTQ+EACTDLVQ  MARK SENT AND DELIVERED               
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   SNDADX              COULD NOT UPDATE EDICT FILE                  
         DROP  R1                                                               
         PRNT  RPTMARKEDSENT,PRINT=ALWAYS                                       
         B     SNDADX                                                           
*                                                                               
SNDADBAD MVC   P+11(13),=C'*** ERROR ***'                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         MVC   ETBLNTRY,=A(TABLNTRY)                                            
         MVI   EACTION,EACTJNKQ    MARK UNSENDABLE                              
         MVC   EERRORCD,ERRCODE                                                 
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   SNDADX              COULD NOT UPDATE EDICT FILE                  
         DROP  R1                                                               
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
*                                                                               
SNDADX   B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
DMWORK   DS    12D                                                              
DMCB     DS    10F                                                              
DUB      DS    D                                                                
PRNTDUB  DS    D                   FOR PRNT MACRO                               
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
WORK     DS    CL256                                                            
PRNTTIME DS    CL9                 FOR PRNT MACRO                               
OPERSTOP DC    C'N'                'Y' IF OPERATOR WANTS TO STOP                
ERRCODE  DS    X                   ERROR REASON CODE                            
TRACEFLG DS    C                   'Y' = PRINT DETAILED TRACE                   
         SPACE 2                                                                
ECBLST   DS    0F                                                               
ALOOKECB DC    X'00',AL3(0)        A(LOOKECB)                                   
ASTOPECB DC    X'80',AL3(0)        A(STOPECB)                                   
         SPACE 2                                                                
DSTNAME  DS    CL8                 DESTINATION                                  
ADVNACCT DS    CL4                 ADVANTIS ACCOUNT                             
         DC    C' '                MUST FOLLOW ADVNACCT                         
ADVNUID  DS    CL8                 ADVANTIS USERID                              
         DC    C' '                MUST FOLLOW ADVNUID                          
ADVNCLS  DS    CL8                 ADVANTIS CLASS                               
         DC    C' '                MUST FOLLOW ADVNCLS                          
         SPACE 2                                                                
* FIRST CHARACTER OF DSN QUALIFER MUST BE A LETTER, SO MAP THE LEFTMOST         
* DIGIT OF THE REFERENCE NUMBER TO A SEMI-ARBITRARY LETTER                      
*                                                                               
DSNMAP   DC    C'0O1P2Q3R4S5T6U7V8W9XAABBCCDDEEFF',X'FF'                        
         SPACE 2                                                                
INMSGDTA EQU   *                                                                
INMSGSND DC    CL80' SEND'                                                      
INMSGACC DC    CL80'      ACCOUNT('                                             
INMSGUID DC    CL80'      USERID('                                              
INMSGFID DC    CL80'      FILEID(DD:ADVNFILE) FORMAT(N)'                        
INMSGCLS DC    CL80'      CLASS('                                               
INMSGMOD DC    CL80'      MODE( )'                                              
INMSGCHG DC    CL27'      CHARGE(1) DATATYPE(E)'                                
         DC    X'5E'                      (SEMICOLON)                           
         DC    CL52' '                                                          
INMSGDTX EQU   *                                                                
         EJECT                                                                  
ADVNFILE DCB   DDNAME=ADVNFILE,MACRF=PM,DSORG=PS,RECFM=FB                       
         SPACE 2                                                                
INMSG    DCB   DDNAME=INMSG,MACRF=PM,DSORG=PS,RECFM=FB,LRECL=80                 
         SPACE 2                                                                
* CREATE ADVNFILE DATASETS -- DYNAMIC ALLOCATION                                
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
         DC    X'80',AL3(TXTUNIT)                                               
         SPACE 2                                                                
* DELETE ADVNFILE DATASETS -- DYNAMIC ALLOCATION                                
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
* ADVNFILE DATASETS -- DYNAMIC UNALLOCATION BY DSN                              
*                                                                               
         DS    0F                                                               
ARBLKUN2 DC    X'80',AL3(RBLKUNA2) R1 POINTS TO THIS BEFORE DYNALLOC            
*                                                                               
RBLKUNA2 DC    X'1402000000000000',A(ACATUNT2),X'0000000000000000'              
*                                                                               
ACATUNT2 DC    X'80',AL3(TXTDSN)                                                
         SPACE 3                                                                
TXTDD    DC    AL2(DALDDNAM),X'00010008',C'ADVNFILE'   DDNAME=ADVNFILE          
TXTUNDD  DC    AL2(DUNDDNAM),X'00010008',C'ADVNFILE'   DDNAME=ADVNFILE          
TXTDSN   DC    AL2(DALDSNAM),X'0001002C',CL44' '       DSN=............         
TXTSTATN DC    AL2(DALSTATS),X'0001000104'             DISP=(NEW,.....)         
TXTNDISP DC    AL2(DALNDISP),X'0001000102'                 =(...,CATLG)         
TXTSTATO DC    AL2(DALSTATS),X'0001000101'             DISP=(OLD,...)           
TXTNDISD DC    AL2(DALNDISP),X'0001000104'                 =(...,DEL)           
TXTNDISK DC    AL2(DALNDISP),X'0001000108'                 =(...,KEEP)          
TXTBLKLN DC    AL2(DALBLKLN),X'00010003',AL3(0)        SPACE=(BLKSIZE..         
TXTPRI   DC    AL2(DALPRIME),X'00010003',AL3(25)            =(..(25,...         
TXTSEC   DC    AL2(DALSECND),X'00010003',AL3(40)            =(...,40),.         
TXTRLSE  DC    AL2(DALRLSE),X'0000'                         =(...,RLSE)         
TXTUNIT  DC    AL2(DALUNIT),X'00010005',C'SYSDA'       UNIT=SYSDA               
         EJECT                                                                  
TABLNTRY DS    XL(XMTTBLQ)         SAVED XMIT TABLE ENTRY                       
ADVNDSN  DC    CL44'DDS.ADVANTIS.EDICTX.ADNNNNNN'                               
         SPACE 2                                                                
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
* DDDPRINT                                                                      
* DDEDICTFIL                                                                    
* DMPRTQL                                                                       
* MACRO:  DCBD  DSORG=PS,DEVD=DA                                                
* MACRO:  IEFZB4D2                                                              
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
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'055DDEDIADVN 05/01/02'                                      
         END                                                                    
