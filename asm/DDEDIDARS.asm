*          DATA SET DDEDIDARS  AT LEVEL 076 AS OF 08/08/13                      
*PHASE EDIDARSA                                                                 
*INCLUDE BINSR31                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE EDISUB                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE MQRPT                                                                  
*                                                                               
         TITLE 'DDEDIDARS -- PERFORM DARE TRANSMISSIONS USING APPC/MVS'         
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        EDIDARS -- TRANSMIT PQ REPORTS VIA DARE (APPC/MVS)   *         
*                                                                     *         
*  COMMENTS:     CALLED BY EDICT                                      *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- WORK                                           *         
*                R7 -- S P A R E                                      *         
*                R8 -- COMMON STORAGE AREA (2ND BASE)                 *         
*                R9 -- PARAMETERS FROM EDICT                          *         
*                RA -- CPRINT                                         *         
*                RB -- PROGRAM BASE                                   *         
*                RC -- COMMON STORAGE AREA                            *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
                                                                                
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
                                                                                
EDIDARS  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,EDIDARS*,=A(R13CHAIN)                                          
*                                                                               
         LR    R9,RC                                                            
         L     RC,=A(COMMWORK)     COMMON STORAGE AREA                          
         LR    R8,RC                                                            
         AH    R8,=H'4096'                                                      
         USING COMMWORK,RC,R8                                                   
*                                                                               
         SH    R9,=H'4'                                                         
         L     R9,0(R9)                                                         
         L     R9,0(R9)            A(R1 PARAMETERS FROM ATTACH)                 
         USING SUBPARMD,R9                                                      
*                                                                               
         MVC   TRACEFLG,STRACEON   'Y' = PRINT DETAILED TRACE                   
         ENTRY TRACEFLG            FOR EDISUB MODULE                            
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         L     RF,=V(PRNTER)       A(SYSPRINT DCB)                              
         MVC   DCBDDNAM-IHADCB(8,RF),=C'DRTRACE '  DDNAME=DRTRACE               
*                                                                               
         MVC   TITLE(30),=C'EDICT: SENDING SUBTASK TO DARE'                     
         PRNT  START_SUBTASK,PRINT=ALWAYS                                       
*                                                                               
         GOTO1 =A(READCRDS),(RC)   READ PARAMETER CARDS                         
*                                                                               
         GOTO1 =A(INITIAL),(RC)    INITIALIZE                                   
*                                                                               
ALLOCATE MVC   CONVERSATION_TYPE,ATB_MAPPED_CONVERSATION                        
         MVC   PARTNER_LU_NAME(L'DARELUID),DARELUID                             
         MVC   TP_NAME_LENGTH,=F'8'                                             
         MVC   RETURN_CONTROL,ATB_WHEN_SESSION_ALLOCATED                        
         MVC   SYNC_LEVEL,ATB_CONFIRM                                           
         MVC   SECURITY_TYPE,ATB_SECURITY_NONE                                  
         CLC   USER_ID,=CL10' '    WAS A USERID PROVIDED?                       
         BE    *+10                NO, SO NO SECURITY                           
         MVC   SECURITY_TYPE,ATB_SECURITY_PROGRAM                               
*                                                                               
         LA    R2,ATBALC2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   ALLOCATE THE CONVERSATION                    
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    GOODBYE             YES                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    DRALLCOK            ALLOCATE WAS OK                              
*                                                                               
         CLC   RETURN_CODE,ATB_ALLOCATE_FAILURE_NO_RETRY                        
         BE    ASKOPER             DARE ISN'T AVAILABLE                         
         CLC   RETURN_CODE,ATB_PARAMETER_ERROR                                  
         BNE   CANTALLC            CAN'T ALLOCATE                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'EDICT01 *DARE OPEN PARAMET+        
               ER ERROR. PROBABLE CAUSE: UNDEFINED LOCAL LUNAME'                
         ABEND 705,DUMP                                                         
*                                                                               
ASKOPER  MVC   ALLCMSG+35(8),PARTNER_LU_NAME                                    
NOTQUITE GOTO1 =V(LOGIO),DMCB,1,('ALLCMSGQ',ALLCMSG)                            
         GOTO1 =V(LOGIO),DMCB,0,(6,WORK)                                        
         CLI   WORK,C'R'           DOES OPERATOR WANT TO RETRY?                 
         BE    ALLOCATE                                                         
         CLI   WORK,C'C'           DOES OPERATOR WANT TO CANCEL?                
         BNE   NOTQUITE                                                         
         ABEND 109,DUMP                                                         
*                                                                               
CANTALLC PRNT  ALLOCATEFAILED,PRINT=ALWAYS                                      
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'EDICT01 *DARE OPEN/SEND FA+        
               ILED'                                                            
         ABEND 701,DUMP                                                         
*                                                                               
DRALLCOK LA    R2,ATBCFM_STRUCTURE                                              
         GOTO1 =A(CALLAPPC),(RC)   ASK FOR A CONFIRMATION                       
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    GOODBYE             YES                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    GOTCONF             CONFIRM WAS OK                               
*                                                                               
         CLC   RETURN_CODE,ATB_TP_NOT_AVAILABLE_NO_RETRY                        
         BE    ASKOPER             DARE ISN'T UP                                
         DC    H'0'                WE DON'T HANDLE OTHER ERRORS                 
*                                                                               
GOTCONF  PRNT  ATTACHSENDER,PRINT=ALWAYS                                        
*                                                                               
         GOTO1 =A(PRNTATTB),(RC)   PRINT CONVERSATION ATTRIBUTES                
*                                                                               
         LA    R2,SUBTASK          A(RECEIVING SUBTASK NAME)                    
         ATTACH EPLOC=(R2),ECB=DRRECB,PARAM=DRRPARMS,SZERO=NO,         +        
               ALCOPY=YES                                                       
         ST    R1,DRRTCB                                                        
         OC    DRRTCB,DRRTCB                                                    
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
*                                                                               
         XC    BUFFER,BUFFER                                                    
         LH    R5,=Y(APPCM1LQ)                                                  
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   BUFFER(0),APPCM1        INITIAL SIGNON                           
         L     RF,SMAJORNM             MAJOR ENQ NAME                           
         MVC   BUFFER+17(1),5(RF)      'A' FOR EDICTA, ETC.                     
         GOTO1 =A(DATETIME),(RC)       PUT DATE/TIME INTO MESSAGE               
         MVC   BUFFER+44(8),YYYYMMDD                                            
         MVC   BUFFER+58(6),HHMMSS                                              
         LA    R5,1(R5)                R5 = L'DATA                              
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         GOTO1 =A(DRLINXMT),(RC)                                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   PREP_TO_RECEIVE_TYPE,ATB_PREP_TO_RECEIVE_FLUSH                   
         LA    R2,ATBPTR_STRUCTURE                                              
         GOTO1 =A(CALLAPPC),(RC)   PREPARE TO RECEIVE                           
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    GOODBYE             YES                                          
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
         BE    GOODBYE             YES                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         L     R2,RECEIVE_LENGTH                                                
         GOTO1 =V(PRNTBL),DMCB,=C'HANDSHAKE RECEIVED',BUFFER,          +        
               C'DUMP',(R2),=C'1D'                                              
*                                                                               
         CLC   APPCM2(APPCM2LQ),BUFFER                                          
         BE    *+6                                                              
         DC    H'0'                DARE DID NOT RESPOND READY                   
*                                                                               
         CLC   STATUS_RECEIVED,ATB_SEND_RECEIVED                                
         BNE   TRYAGAIN                                                         
         PRNT  SWITCHINGTOSEND                                                  
         B     LOOP                                                             
*                                                                               
TRYAGAIN PRNT  RECEIVE_AGAIN                                                    
         MVC   FILL,ATB_FILL_LL                                                 
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
*                                                                               
         LA    R2,ATBRCVW_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   RECEIVE AND WAIT                             
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    GOODBYE             YES                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         CLC   STATUS_RECEIVED,ATB_SEND_RECEIVED                                
         BE    *+6                                                              
         DC    H'0'                WHY WON'T PARTNER LET US SEND?               
         PRNT  SWITCHINGTOSEND                                                  
         EJECT                                                                  
LOOP     CLI   OPERSTOP,C'Y'       DOES OPERATOR WANT TO STOP?                  
         BE    STOP                YES                                          
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
STOP     PRNT  STOPPINGDARERRCV,PRINT=ALWAYS                                    
         LA    R2,DRRPARMS         PARAMETERS TO RECEIVER                       
         USING DRRPARMD,R2                                                      
         POST  DRRSTOP             TELL EDIDARR TO STOP                         
         DROP  R2                                                               
*                                                                               
         MVC   DEALLOCATE_TYPE,ATB_DEALLOCATE_SYNC_LEVEL                        
         LA    R2,ATBDEAL_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   DEALLOCATE THE CONVERSATION                  
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         PRNT  DEALLOCATEDOK,PRINT=ALWAYS                                       
*                                                                               
         WAIT  ECB=DRRECB                                                       
         TM    DRRECB,X'40'                                                     
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DETACH   DETACH DRRTCB                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
*                                                                               
GOODBYE  PRNT  EXITING,PRINT=ALWAYS                                             
*                                                                               
         XBASE                                                                  
*                                                                               
DONTSTOP L     RF,ALOOKECB                                                      
         TM    0(RF),X'40'         MAIN TASK POSTED READY?                      
         BZ    CHKRCVR             NO                                           
         XC    0(4,RF),0(RF)                                                    
*                                                                               
         GOTO1 =A(XFERREPS),(RC)   TRANSMIT ALL DARE REPORTS IN TABLE           
*                                                                               
         GOTO1 SADTAMGR,DMCB,=CL8'ISGENQ',C'#01K' RELEASE TASK ENQS             
*                                                                               
         B     LOOP                YES                                          
*                                                                               
CHKRCVR  TM    DRRECB,X'40'        IS DARE RECEIVER STILL RUNNING?              
         BO    *+6                                                              
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
         OC    DRRECB+1(3),DRRECB+1                                             
         BZ    DETACH              RECEIVER ENDED CLEANLY                       
*                                                                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'EDICT01 *DARE RECEIVER DIE+        
               D'                                                               
         ABEND 701,DUMP                                                         
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* INITIALIZE                                                                    
***********************************************************************         
INITIAL  NMOD1 0,INITIAL                                                        
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         PRNT  INITIALIZE,PRINT=ALWAYS                                          
*                                                                               
         L     R1,SLOOKECB         BUILD ECBLISTS                               
         STCM  R1,7,ALOOKECB+1                                                  
         L     R1,SSTOPECB                                                      
         STCM  R1,7,ASTOPECB+1                                                  
         STCM  R1,7,ASTPECB2+1                                                  
*                                                                               
         LA    R2,DRRPARMS         PARAMETERS TO RECEIVER                       
         USING DRRPARMD,R2                                                      
         MVC   DRRDLUID,=A(DARELUID) DARE'S LUID                                
         MVC   DRRVMODE,=A(MODE_NAME) VTAM MODE                                 
         MVC   DRRTPNAM,=A(TP_NAME)  TRANSACTION PROGRAM NAME                   
         MVC   DRRUSRID,=A(USER_ID)  APPC USER_ID                               
         MVC   DRRPASWD,=A(PASSWORD) APPC USER_ID                               
         MVC   DRRXMTTB,SXMTTBLE     A(REPORT TABLE)                            
         MVC   DRRMAJNM,SMAJORNM     A(MAJOR NAME FOR ENQ)                      
         MVC   DRRADMGR,SADTAMGR     A(DATAMGR)                                 
         MVC   DRREDADD,SEDCTADD     A(ROUTINE TO ADD EDICT RECORDS)            
         MVC   DRRTRACE,TRACEFLG     C'Y' = PRODUCE DETAILED TRACE              
         MVC   DRRLOGID,SLOGID       FOR DARE LOGGING INFO                      
         MVC   DRRMAIL,MAILFLAG      C'Y' = SEND DAREMAIL VIA MQSERIES          
         MVC   DRRMQMO,MQEDIMO       C'Y' = SEND EDI COPY TO MO                 
         MVC   DRREDTYP,SEDCTTYP     C'A' OR C'R' FOR EDICTA/EDICTR             
         MVC   DRRQMGNM,=A(QMGRNAME) MQSERIES QUEUE MANAGER NAME                
         MVC   DRRMQQNM,=A(CTLQNAME) MQSERIES FACPAK CONTROL QUEUE NAME         
         MVC   DRRDNATB,SDNATAB      A(DARE NOTIFICATION ASSIGNMENTS)           
         MVC   DRRATSAR,STSAROFF     A(TSAROFF)                                 
         DROP  R2                                                               
*                                                                               
         BLDL  0,ENTRYPTS            BUILD LIST OF APPC/MVS ENTRY PTS           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                  BAD RETURN FROM BLDL MACRO                 
         LOAD  DE=ATBALC2                                                       
         ST    R0,ATBALC2_STRUCTURE                                             
         LOAD  DE=ATBCFM                                                        
         ST    R0,ATBCFM_STRUCTURE                                              
         LOAD  DE=ATBDEAL                                                       
         ST    R0,ATBDEAL_STRUCTURE                                             
         LOAD  DE=ATBEES3                                                       
         ST    R0,ATBEES3_STRUCTURE                                             
         LOAD  DE=ATBGTA2                                                       
         ST    R0,ATBGTA2_STRUCTURE                                             
         LOAD  DE=ATBPTR                                                        
         ST    R0,ATBPTR_STRUCTURE                                              
         LOAD  DE=ATBRCVW                                                       
         ST    R0,ATBRCVW_STRUCTURE                                             
         LOAD  DE=ATBSEND                                                       
         ST    R0,ATBSEND_STRUCTURE                                             
*                                                                               
         GOTO1 SADTAMGR,DMCB,=CL8'ISGENQ',C'#01K'  SET SUBTASK #01              
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* READ PARAMETER CARDS BETWEEN "++DARE" AND "++DAREEND" CARDS                   
*                                                                               
*   *......          "*" IN COLUMN ONE IS A COMMENT CARD                        
*   DARELUID=CCCCCCCC LUID OF DARE                                              
*   VTAMMODE=CCCCCCCC VTAM MODE NAME                                            
*   TPNAME=CCCCCCCC  TRANSACTION PROGRAM NAME                                   
*   USERID=CCCCCCCCCC   APPC USERID (FOR SECURITY)                              
*   PASSWORD=CCCCCCCCCC APPC PASSWORD (FOR SECURITY)                            
*   TRACE=YES        OVERRIDE TRACE FLAG                                        
*   SUBTASK=CCCCCCCC RECEIVING SUBTASK NAME (DEFAULT = EDIDARR)                 
*   MQDAREMAIL=YES   SEND DAREMAIL VIA MQSERIES                                 
*   MQQMGRNAME=      MQSERIES QUEUE MANAGER NAME                                
*   MQQUEUENAME=     MQSERIES FACPAK CONTROL QUEUE NAME                         
*   MQEDIMO=YES      SEND EDI COPY TO MO VIA MQ                                 
***********************************************************************         
READCRDS NMOD1 0,READCRDS                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
RC10     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++DARE',CARD     LOOK FOR START OF PARAMETERS                 
         BNE   RC10                                                             
*                                                                               
RC20     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++DAREEND',CARD  LOOK FOR END OF PARAMETERS                   
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
         CLC   =C'DARELUID=',CARD  DARELUID=                                    
         BNE   *+14                                                             
         MVC   DARELUID,CARD+9                                                  
         B     RC20                                                             
*                                                                               
         CLC   =C'VTAMMODE=',CARD  VTAMMODE=                                    
         BNE   *+14                                                             
         MVC   MODE_NAME,CARD+9                                                 
         B     RC20                                                             
*                                                                               
         CLC   =C'USERID=',CARD    APPC USERID (FOR SECURITY)                   
         BNE   *+14                                                             
         MVC   USER_ID,CARD+7                                                   
         B     RC20                                                             
*                                                                               
         CLC   =C'PASSWORD=',CARD  APPC PASSWORD (FOR SECURITY)                 
         BNE   *+14                                                             
         MVC   PASSWORD,CARD+9                                                  
         B     RC20                                                             
*                                                                               
         CLC   =C'TPNAME=',CARD    TPNAME=                                      
         BNE   *+18                                                             
         MVC   TP_NAME(7),CARD+7                                                
         MVI   TP_NAME+7,C'R'      TARGET THE RECEIVER WITH SUFFIX 'R'          
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQMGRNAME=',CARD                                             
         BNE   *+14                                                             
         MVC   QMGRNAME,CARD+11                                                 
         B     RC20                                                             
*                                                                               
         CLC   =C'MQQUEUENAME=',CARD                                            
         BNE   *+14                                                             
         MVC   CTLQNAME,CARD+12                                                 
         B     RC20                                                             
*                                                                               
         CLC   =C'MQDAREMAIL=',CARD                                             
         BNE   RC30                                                             
         CLC   =C'YES',CARD+11                                                  
         BE    RC20                DON'T OVERRIDE                               
         CLC   =C'NO',CARD+11                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MAILFLAG,C'N'       DON'T SEND DAREMAIL VIA MQSERIES             
         B     RC20                                                             
*                                                                               
RC30     CLC   =C'MQEDIMO=',CARD                                                
         BNE   RC40                                                             
         CLC   =C'YES',CARD+8                                                   
         BE    RC20                DON'T OVERRIDE                               
         CLC   =C'NO',CARD+8                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MQEDIMO,C'N'        DON'T SEND EDI TO MO                         
         B     RC20                                                             
*                                                                               
RC40     CLC   =C'TRACE=',CARD     TRACE=                                       
         BNE   RC50                                                             
         CLC   =C'NO',CARD+6                                                    
         BE    RC20                DON'T OVERRIDE                               
         CLC   =C'YES',CARD+6                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRACEFLG,C'Y'       TRACE=YES                                    
         B     RC20                                                             
*                                                                               
RC50     CLC   =C'SUBTASK=',CARD   SUBTASK=                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SUBTASK,CARD+8                                                   
         B     RC20                                                             
*                                                                               
RCX      GOTO1 =V(PRINTER)         SKIP A LINE                                  
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* TRANSFER REPORTS                                                              
***********************************************************************         
XFERREPS NMOD1 0,XFERREPS                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
XFER10   L     R2,SMAJORNM         MAJOR RESOURCE NAME                          
         LA    R6,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),0(R2)                                                    
         MVC   P+40(8),0(R6)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   ((2),(6),E,8)       ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         L     R6,SXMTTBLE         R6 = A(REPORT TABLE)                         
         USING XMTTABLD,R3                                                      
         LA    R3,XMTENTRY                                                      
         LA    R4,WORK                                                          
         XC    WORK,WORK           WORK = EARLIEST ENTRY --> FIFO               
*                                                                               
XFER20   MVC31 XMTENTRY,0(R6)                                                   
         CLI   0(R3),0             END OF TABLE?                                
         BE    XFER40              YES, NO MORE                                 
         CLI   XMTSOURC,XMTSRCPQ   PRINT QUEUE ENTRY?                           
         BNE   XFER30                                                           
         CLI   XMTMETH,C'D'        YES -- DARE METHOD?                          
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
XFER30   AHI   R6,XMTTBLQ          BUMP TO NEXT REPORT                          
         B     XFER20                                                           
         DROP  R3                                                               
*                                                                               
XFER40   OC    WORK,WORK           ANYTHING FOUND TO SEND?                      
         BZ    XFER60              NO                                           
         MVC   TABLNTRY,0(R4)      SAVE XMIT TABLE KEY                          
*                                                                               
         L     R2,SMAJORNM         MAJOR RESOURCE NAME                          
         LA    R6,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),0(R2)                                                    
         MVC   P+40(8),0(R6)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   ((2),(6),8)         DEQUEUE THE TRANSMIT TABLE                   
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
         LA    R1,DMCB             REPORT WAS NOT FOUND                         
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         MVC   ETBLNTRY,=A(TABLNTRY)                                            
         MVI   EACTION,EACTJNKQ    MARK REPORT UNSENDABLE                       
         MVI   EERRORCD,EDFERNPQ                                                
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   XFER50              CAN'T POST EDICT FILE                        
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
         B     XFER50                                                           
         DROP  R1                                                               
*                                                                               
XFER45   LA    R3,TABLNTRY                                                      
         USING XMTTABLD,R3                                                      
         EDIT  XMTUSRID,(5,P+30),ALIGN=LEFT                                     
         MVC   P+40(3),XMTSUBID    PRINT PQ SUBID                               
         EDIT  XMTREFNO,(5,P+45),ALIGN=LEFT                                     
         GOTO1 =V(HEXOUT),DMCB,XMTDSKAD,EDICTDA,4,=C'TOG'                       
         MVC   P+53(8),EDICTDA                                                  
         PRNT  XFERONEREPORT,PRINT=ALWAYS                                       
         DROP  R3                                                               
*                                                                               
         GOTO1 =A(SENDDARE),(RC)                                                
*                                                                               
XFER50   CLI   OPERSTOP,C'Y'       OPERATOR WANTS TO STOP?                      
         BNE   XFER10              NO                                           
         B     XFERX                                                            
*                                                                               
XFER60   L     R2,SMAJORNM         MAJOR RESOURCE NAME                          
         LA    R6,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),0(R2)                                                    
         MVC   P+40(8),0(R6)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   ((2),(6),8)         DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
XFERX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* TRANSFER A REPORT VIA DARE                                                    
***********************************************************************         
SENDDARE NMOD1 0,SENDDARE                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
         XC    BUFFER,BUFFER           BUILD MESSAGE HEADER                     
         LH    R5,=Y(APPCM3LQ)                                                  
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   BUFFER(0),APPCM3        +++DARE SEND                             
         GOTO1 =A(DATETIME),(RC)       PUT DATE/TIME INTO MESSAGE               
         MVC   BUFFER+18(6),HHMMSS                                              
         LA    R5,1(R5)                R5 = L'DATA                              
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         GOTO1 =A(DRLINXMT),(RC)                                                
         BNE   SNDDR50                                                          
*                                                                               
         XC    BUFFER,BUFFER           BUILD LOGGING INFO                       
         LH    R5,=Y(APPCM5LQ)                                                  
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   BUFFER(0),APPCM5        +++DARE LOGGING=                         
         MVI   BUFFER+16,C'3'          VERSION THREE                            
*                                                                               
         GOTO1 =V(HEXIN),DMCB,EDICTDA,FULL,8  EDICT FILE DSKADDR                
         CLC   =F'4',DMCB+12                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,DMCB                                                          
         USING CONVDSKD,R1                                                      
         MVC   CONVDMGR,SADTAMGR   A(DATAMGR)                                   
         MVI   CONVACTN,CONVDSKF   CONVERT DSKADDR TO REFERENCE NUMBER          
         MVC   CONVDSK,FULL        EDICT FILE DISK ADDRESS                      
         MVC   CONVOFF,=A(BUFFER+17)  RESULT GOES INTO LOGGING DATA             
         GOTO1 =V(CONVDSKA)                                                     
         DROP  R1                                                               
*                                                                               
         MVC   BUFFER+25(1),SLOGID     'A' FOR EDICTA, ETC.                     
         LA    R5,1(R5)                R5 = L'DATA                              
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         GOTO1 =A(DRLINXMT),(RC)                                                
         BNE   SNDDR50                                                          
         LA    R2,2                    TWO RECORDS SENT SO FAR                  
*                                                                               
SNDDR10  LA    R1,DMCB                                                          
         USING PQPARMD,R1                                                       
         MVC   ATBLNTRY,=A(TABLNTRY)                                            
         MVC   APQBUFF,=A(CXREC)                                                
         MVC   APQHDR,=A(PQRPTHDR)                                              
         MVC   AEDHDR,=A(EDICTHDR)                                              
         MVC   APQLINE,=A(R)                                                    
         MVC   ACITABLE,SCITABLE                                                
         MVC   ADTAMGR,SADTAMGR                                                 
         GOTO1 =V(PQGETLIN)                                                     
         BNE   SNDDR40             NO MORE REPORT LINES                         
         DROP  R1                                                               
         CLC   =C'++DDS',R+1       SHOULD THIS LINE BE TRANSMITTED?             
         BE    SNDDR10             NO -- TRANSMIT REMAINDER OF REPORT           
*                                                                               
         CLI   SEDCTTYP,C'R'       FOR EDICTR ONLY                              
         BNE   SNDDR20                                                          
*        CLC   =C'ORDCFM',R+1                                                   
*        BE    SNDDR15                                                          
         CLC   =C'ORDRCL',R+1                                                   
         BE    SNDDR15                                                          
         CLC   =C'ORDREJ',R+1                                                   
         BE    SNDDR15                                                          
*        CLC   =C'ORDAPP',R+1                                                   
*        BE    SNDDR15                                                          
         B     SNDDR20                                                          
SNDDR15  MVC   MQUSERID,ROAPFRID-RORDAPPD+R+1                                   
*        MVI   MQACTION,C'O'                                                    
*        GOTO1 =A(CCMOMQ),(RC)                                                  
*                                                                               
SNDDR20  LA    R4,254              FIND END OF TEXT                             
         LA    RF,R+1(R4)                                                       
         CLI   0(RF),C' '                                                       
         BNE   *+12                FOUND IT                                     
         BCT   R4,*-12                                                          
         B     *+8                                                              
         LA    R4,1(R4)                                                         
*                                                                               
         XC    BUFFER,BUFFER                                                    
         LTR   R4,R4               ANY REAL DATA TO SEND?                       
         BZ    SNDDR30             NO -- SKIP IT                                
         BCTR  R4,0                                                             
         EX    R4,*+8              MOVE IN LINE                                 
         B     *+10                                                             
         MVC   BUFFER(0),R+1                                                    
         LA    R4,1(R4)            RESTORE LENGTH OF DATA                       
         LR    R5,R4               R5 = L'DATA                                  
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         GOTO1 =A(DRLINXMT),(RC)                                                
         BNE   SNDDR50                                                          
         LA    R2,1(R2)            INCREMENT RECORD COUNTER                     
*                                                                               
         LA    R4,BUFFER                                                        
*        MVI   MQACTION,C'P'                                                    
*        GOTO1 =A(CCMOMQ),(RC)                                                  
*                                                                               
SNDDR30  LA    R1,DMCB                                                          
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
         BE    SNDDR20                                                          
*                                                                               
SNDDR40  XC    BUFFER,BUFFER                                                    
         LH    R5,=Y(APPCM4LQ)                                                  
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   BUFFER(0),APPCM4    MESSAGE TRAILER                              
         GOTO1 =A(DATETIME),(RC)   PUT DATE/TIME INTO MESSAGE                   
         MVC   BUFFER+17(6),HHMMSS                                              
         LA    R2,1(R2)            INCREMENT RECORD COUNTER (FOR LAST)          
         CVD   R2,DUB                                                           
         UNPK  BUFFER+30(6),DUB                                                 
         OI    BUFFER+35,X'F0'                                                  
         LA    R5,1(R5)            R5 = L'DATA                                  
         MVC   SEND_TYPE,ATB_SEND_AND_CONFIRM                                   
         GOTO1 =A(DRLINXMT),(RC)                                                
         BE    SNDDR60                                                          
*                                                                               
SNDDR50  MVC   P+30(14),=C'BAD DARE WRITE'                                      
         B     CANTSEND            SKIP IT                                      
*                                                                               
SNDDR60  PRNT  SENTVIADARE,PRINT=ALWAYS                                         
*                                                                               
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         MVC   ETBLNTRY,=A(TABLNTRY)                                            
         MVI   EACTION,EACTSNTQ    MARK REPORT SENT                             
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   SNDDRXIT            CAN'T POST EDICT FILE                        
         DROP  R1                                                               
         PRNT  RPTMARKEDSENT,PRINT=ALWAYS                                       
*                                                                               
*        MVI   MQACTION,C'C'                                                    
*        GOTO1 =A(CCMOMQ),(RC)                                                  
*                                                                               
         B     SNDDRXIT                                                         
*                                                                               
SNDDRBAD MVC   P+11(13),=C'*** ERROR ***'                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         MVC   ETBLNTRY,=A(TABLNTRY)                                            
         MVI   EACTION,EACTJNKQ    MARK REPORT UNSENDABLE                       
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   SNDDRXIT            CAN'T POST EDICT FILE                        
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
         DROP  R1                                                               
*                                                                               
SNDDRXIT XIT1                                                                   
*                                                                               
CANTSEND PRNT  CANTTRANSMIT,PRINT=ALWAYS                                        
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'EDICT01 *ERROR SENDING TO +        
               DARE -- TRANSMISSIONS SUSPENDED'                                 
         ABEND 701,DUMP                                                         
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* ON ENTRY, R5 = LENGTH OF DATA                                                 
***********************************************************************         
DRLINXMT NMOD1 0,DRLINXMT                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         LR    R2,R5                                                            
         LA    R2,2(R2)            L'DATA + 2 BYTES FOR CRLF                    
         ST    R2,SEND_LENGTH                                                   
         LA    R1,BUFFER(R5)                                                    
         MVC   0(2,R1),=X'0D25'    TERMINATE RECORD WITH CRLF                   
         CLI   TRACEFLG,C'Y'                                                    
         BNE   DRLIN20                                                          
*                                                                               
         LA    RE,=C'BUFFER DATA'                                               
         ST    RE,DMCB                                                          
         MVI   DMCB,11                                                          
         CLC   SEND_TYPE,ATB_BUFFER_DATA                                        
         BE    DRLIN10                                                          
         LA    RE,=C'SEND AND CONFIRM'                                          
         ST    RE,DMCB                                                          
         MVI   DMCB,16                                                          
         CLC   SEND_TYPE,ATB_SEND_AND_CONFIRM                                   
         BE    DRLIN10                                                          
         DC    H'0'                UNKNOWN SEND TYPE                            
DRLIN10  GOTO1 =V(PRNTBL),DMCB,,BUFFER,C'DUMP',(R2),=C'1D'                      
*                                                                               
DRLIN20  LA    R2,ATBSEND_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   SEND DATA                                    
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    DRLINXOK            RETURN CODE WAS OK                           
*                                                                               
         LTR   RB,RB               NO GOOD -- SET CC NOT EQUAL                  
         B     DRLINXX                                                          
*                                                                               
DRLINXOK CR    RB,RB               SET CC EQUAL                                 
*                                                                               
DRLINXX  XIT1                                                                   
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
         GOTO1 =V(HEXOUT),DMCB,CONVERSATION_ID,P+30,8,=C'TOG'                   
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
         L     RF,ASTPECB2         SYNCHRONOUS CALL, SO IT'S DONE               
         TM    0(RF),X'40'         STOP?                                        
         BZ    CALL40              NO                                           
         XC    0(4,RF),0(RF)                                                    
         MVI   OPERSTOP,C'Y'                                                    
         B     CALL40                                                           
*                                                                               
CALL10   CLC   RETURN_CODE,ATB_OK                                               
         BE    CALL20              APPC/MVS CALL WAS ACCEPTED                   
         MVC   P+30(16),4(R2)      PRINT NAME OF APPC/MVS ROUTINE               
         MVC   P+50(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+64),ALIGN=LEFT                                  
         PRNT  **BAD_APPC_CALL**,PRINT=ALWAYS                                   
         TM    20(R2),X'40'        CAN CALL ERROR_EXTRACT NOW?                  
         BZ    CALL15                                                           
         GOTO1 =A(APPCERR),(RC)    YES - DO IT                                  
CALL15   DC    H'0'                APPC/MVS CALL NOT ACCEPTED                   
*                                                                               
CALL20   WAIT  1,ECBLIST=ECBLST2   WAIT FOR COMPLETION OR OPERATOR              
*                                                                               
         L     RF,ASTPECB2                                                      
         TM    0(RF),X'40'         STOP?                                        
         BZ    *+18                NO                                           
         XC    0(4,RF),0(RF)                                                    
         MVI   OPERSTOP,C'Y'       REMEMBER OPERATOR WANTS TO STOP              
         B     CALL20              WAIT SOME MORE                               
*                                                                               
         TM    APPCECB,X'40'                                                    
         BO    *+6                                                              
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
         MVC   RETURN_CODE,APPCECB                                              
         MVI   RETURN_CODE,0       CLEAR HIGH-ORDER BYTE                        
         XC    APPCECB,APPCECB                                                  
*                                                                               
CALL40   MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
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
*&&DO                                                                           
* DISABLED (2/6/09, SKUI)                                                       
CCMOMQ   NMOD1 0,CCMOMQ                                                         
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* SPECIAL ROUTINE TO CARBON COPY EDI'S TO MEDIAOCEAN VIA MQ                     
* UPON ENTRY,  PARM1, BYTE 0 HAS FLAG FOR MQ ACTION                             
*                                                                               
         CLI   MQEDIMO,C'Y'                                                     
         BNE   CCMOMQX                                                          
         CLI   MQFLAG,C'Y'                                                      
         BE    CCMOMQ05                                                         
         MVI   MQPUTBYT,X'A0'                                                   
         LA    R2,MOMQNAME                                                      
*        LA    R2,TESTMQ                                                        
         CLC   =C'ROBAT',MQUSERID                                               
         BNE   CCMOMQ02                                                         
         OI    MQPUTBYT,X'01'      USE TEST BROKER                              
         B     CCMOMQ03                                                         
*                                                                               
CCMOMQ02 CLC   =C'ETVNY',MQUSERID                                               
         BNE   CCMOMQX                                                          
CCMOMQ03 MVI   MQFLAG,C'Y'                                                      
*                                                                               
CCMOMQ05 DS    0H                                                               
         CLI   MQACTION,C'O'       OPEN?                                        
         BE    CCMOMQ10                                                         
         CLI   MQACTION,C'P'       PUT ?                                        
         BE    CCMOMQ40                                                         
         CLI   MQACTION,C'C'       CLOSE?                                       
         BE    CCMOMQ80                                                         
         B     CCMOMQX                                                          
*                                                                               
CCMOMQ10 DS    0H                                                               
         GOTO1 =V(MQRPT),DMCB,(0,=C'OPEN'),(0,(R2)),(MQPUTBYT,0),0              
         PRNT  OPENMQTOMO,PRINT=ALWAYS                                          
         B     CCMOMQX                                                          
*                                                                               
CCMOMQ40 DS    0H                                                               
         GOTO1 =V(MQRPT),DMCB,(0,=C'PUT'),(R4),((R5)),0                         
         GOTO1 =V(PRNTBL),DMCB,=C'SENDING TO MQ BUFFER',(R4),          +        
               C'DUMP',(R5),=C'1D'                                              
         B     CCMOMQX                                                          
*                                                                               
CCMOMQ80 DS    0H                                                               
         GOTO1 =V(MQRPT),DMCB,(0,=C'CLOSE'),0,0,0                               
         PRNT  CLOSEMQTOMO,PRINT=ALWAYS                                         
         MVI   MQFLAG,C'N'                                                      
         XC    MQUSERID,MQUSERID                                                
*                                                                               
CCMOMQX  DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
APPCERR  NMOD1 0,APPCERR                                                        
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PERFORM APPC ERROR_EXTRACT FUNCTION.                                          
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
COMMWORK DS    0D                  COMMON STORAGE AREA                          
         SPACE 2                                                                
         ATBSERV                                                                
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
RETURN_CONTROL                 DS    F                                          
SYNC_LEVEL                     DS    F                                          
SECURITY_TYPE                  DS    F                                          
USER_ID                        DC    CL10' '                                    
PASSWORD                       DC    CL10' '                                    
PROFILE                        DC    CL10' '                                    
USER_TOKEN                     DC    XL80'00'                                   
CONVERSATION_ID                DS    CL8                                        
CONVERSATION_CORRELATOR        DS    CL8                                        
NOTIFY_TYPE                    DS    F                                          
                               DC    A(APPCECB) MUST FOLLOW NOTIFY_TYPE         
TPID                           DC    XL8'00'                                    
LUW_ID                         DS    XL26                                       
CONVERSATION_STATE             DS    F                                          
RETURN_CODE                    DS    F                                          
DEALLOCATE_TYPE                DS    F                                          
FILL                           DS    F                                          
RECEIVE_LENGTH                 DS    F                                          
RECEIVE_ACCESS_TOKEN           DC    F'0'                                       
STATUS_RECEIVED                DS    F                                          
DATA_RECEIVED                  DS    F                                          
REQUEST_TO_SEND_RECEIVED       DS    F                                          
REQUEST_TO_SEND_VALUE          DS    F                                          
SEND_TYPE                      DS    F                                          
SEND_LENGTH                    DS    F                                          
SEND_ACCESS_TOKEN              DC    F'0'                                       
PREP_TO_RECEIVE_TYPE           DS    F                                          
LOCKS                          DS    F                                          
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
APPCM1   DC    C'+++DARE SENDER=EDX CONV=SEND VERSION=1 DATE=YYYYMMDD T+        
               IME=HHMMSS'                                                      
APPCM1LQ EQU   *-APPCM1                                                         
         SPACE 2                                                                
APPCM2   DC    C'+++DARE SENDER=DDS VERSION=1 STATUS=READY '                    
APPCM2LQ EQU   *-APPCM2                                                         
APPCM2A  DC    C'DATE=YYYYMMDD TIME=HHMMSS'                                     
APPCM2L2 EQU   *-APPCM2                                                         
         SPACE 2                                                                
APPCM3   DC    C'+++DARE SEND TIME=HHMMSS'                                      
APPCM3LQ EQU   *-APPCM3                                                         
         SPACE 2                                                                
APPCM4   DC    C'+++DARE END TIME=HHMMSS COUNT=NNNNNN'                          
APPCM4LQ EQU   *-APPCM4                                                         
         SPACE 2                                                                
APPCM5   DC    C'+++DARE LOGGING=VTTTTBBRRF'                                    
APPCM5LQ EQU   *-APPCM5                                                         
         SPACE 5                                                                
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
YYYYMMDD DS    CL8                 DATE                                         
HHMMSS   DS    CL6                 TIME                                         
WORK     DS    CL256                                                            
OPERSTOP DC    C'N'                'Y' IF OPERATOR WANTS TO STOP                
TRACEFLG DS    C                   'Y' FOR DETAILED TRACE                       
MAILFLAG DC    C'Y'                'Y' TO SEND DAREMAIL VIA MQSERIES            
MQUSERID DS    CL10                ALPHA USERID USED FOR CC TO MO               
MQEDIMO  DC    C'Y'                'Y' TO SEND DARE EDI COPY VIA MQ             
MQACTION DS    C                   MO MQ ACTION (OPEN, PUT, CLOSE)              
MQFLAG   DC    C'N'                'Y' TO SEND DARE EDI COPY VIA MQ             
MQPUTBYT DS    X                   P3 B0 OF MQRPT PUT COMMAND                   
*                                                                               
MOMQNAME DC    C'MEDIAOCEANEDI***'                                              
TESTMQ   DC    C'DDSTESTINGQUEUE*'                                              
*                                                                               
*                                  SET ONLY FOR INTERIM OF AN EDI MSG           
QMGRNAME DC    CL48' '             MQSERIES QUEUE MANAGER NAME                  
CTLQNAME DC    CL48' '             MQSERIES FACPAK CONTROL QUEUE NAME           
WAITSECS DC    F'12000'            2 MINS. FOR APPC CALLS TO COMPLETE           
         SPACE 2                                                                
ALLCMSG  DC    C'EDICT01 *ERROR CONTACTING PARTNER (XXXXXXXX). REPLY RE+        
               TRY OR CANCEL'                                                   
ALLCMSGQ EQU   *-ALLCMSG                                                        
         SPACE 2                                                                
ECBLST   DS    0F                                                               
ALOOKECB DC    X'00',AL3(0)        A(LOOKECB)                                   
         DC    X'00',AL3(DRRECB)   A(RECEIVER ECB)                              
ASTOPECB DC    X'80',AL3(0)        A(STOPECB)                                   
         SPACE 2                                                                
ECBLST2  DS    0F                                                               
ASTPECB2 DC    X'00',AL3(0)             A(STOPECB)                              
AAPPCECB DC    X'80',AL3(APPCECB)       A(APPCECB)                              
         SPACE 2                                                                
DRRPARMS DC    XL(DRRPRMLQ)'00'    PARAMETERS TO EDIDARR (VIA R1)               
DRRTCB   DS    F                   TCB OF ATTACHED SUBTASK EDIDARR              
DRRECB   DC    F'0'                ECB OF ATTACHED SUBTASK EDIDARR              
*                                                                               
SUBTASK  DC    C'EDIDARR '         RECEIVING SUBTASK NAME                       
DARELUID DS    CL8                 DARE LUID                                    
APPCECB  DC    F'0'                ECB FOR APPC/MVS CALLS                       
         SPACE 2                                                                
TABLNTRY DS    XL(XMTTBLQ)         SAVED XMIT TABLE ENTRY                       
XMTENTRY DS    XL(XMTTBLQ)         TEMP STORAGE FOR XMIT TABLE ENTRY            
EDICTDA  DS    CL8                 EDICT FILE DISK ADDRESS (EBCDIC)             
CARD     DS    CL80                FOR CONTROL CARDS AND EDICTFIL RECS          
         DS    XL4                 PQ RECORD LENGTH                             
R        DS    CL256               PQ RECORD DATA                               
         DS    XL4                                                              
EDICTHDR DS    CL256               EDICT HDR* RECORD                            
         DS    XL4                                                              
PQRPTHDR DS    CL256               PQ REPORT HEADER                             
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
ATBALC2_STRUCTURE         DS    A                                               
                          DC    CL16'ALLOCATE'                                  
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_TYPE)                    
                          DC    X'00',AL3(SYM_DEST_NAME)                        
                          DC    X'00',AL3(PARTNER_LU_NAME)                      
                          DC    X'00',AL3(MODE_NAME)                            
                          DC    X'00',AL3(TP_NAME_LENGTH)                       
                          DC    X'00',AL3(TP_NAME)                              
                          DC    X'00',AL3(RETURN_CONTROL)                       
                          DC    X'00',AL3(SYNC_LEVEL)                           
                          DC    X'00',AL3(SECURITY_TYPE)                        
                          DC    X'00',AL3(USER_ID)                              
                          DC    X'00',AL3(PASSWORD)                             
                          DC    X'00',AL3(PROFILE)                              
                          DC    X'00',AL3(USER_TOKEN)                           
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'00',AL3(TPID)                                 
                          DC    X'00',AL3(LOCAL_LU_NAME)                        
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
ATBRCVW_STRUCTURE         DS    A                                               
                          DC    CL16'RECEIVE_AND_WAIT'                          
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(FILL)                                 
                          DC    X'00',AL3(RECEIVE_LENGTH)                       
                          DC    X'00',AL3(RECEIVE_ACCESS_TOKEN)                 
                          DC    X'00',AL3(BUFFER)                               
                          DC    X'00',AL3(STATUS_RECEIVED)                      
                          DC    X'00',AL3(DATA_RECEIVED)                        
                          DC    X'00',AL3(REQUEST_TO_SEND_RECEIVED)             
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBDEAL_STRUCTURE         DS    A                                               
                          DC    CL16'DEALLOCATE'                                
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(DEALLOCATE_TYPE)                      
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBCFM_STRUCTURE          DS    A                                               
                          DC    CL16'CONFIRM'                                   
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
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
                          DC    X'00',AL3(BUFFER)                               
                          DC    X'00',AL3(REQUEST_TO_SEND_VALUE)                
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
                          DC    X'00',AL3(WORK)                                 
                          DC    X'00',AL3(TP_NAME)                              
                          DC    X'00',AL3(LOCAL_LU_NAME)                        
                          DC    X'00',AL3(CONVERSATION_TYPE)                    
                          DC    X'00',AL3(WORK)                                 
                          DC    X'00',AL3(WORK)                                 
                          DC    X'00',AL3(WORK)                                 
                          DC    X'00',AL3(CONVERSATION_STATE)                   
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
         SPACE 3                                                                
ENTRYPTS DS    0F                                                               
         DC    Y((ENTRYLSQ-ENTRYSTQ)/60)   NUMBER OF TABLE ENTRIES              
         DC    H'60'                       MUST REMAIN AS 60                    
ENTRYSTQ EQU   *                                                                
*                                                                               
ATBALC2  DC    CL8'ATBALC2'                                                     
         DC    XL52'00'                                                         
ATBCFM   DC    CL8'ATBCFM'                                                      
         DC    XL52'00'                                                         
ATBDEAL  DC    CL8'ATBDEAL'                                                     
         DC    XL52'00'                                                         
ATBEES3  DC    CL8'ATBEES3'                                                     
         DC    XL52'00'                                                         
ATBGTA2  DC    CL8'ATBGTA2'                                                     
         DC    XL52'00'                                                         
ATBPTR   DC    CL8'ATBPTR'                                                      
         DC    XL52'00'                                                         
ATBRCVW  DC    CL8'ATBRCVW'                                                     
         DC    XL52'00'                                                         
ATBSEND  DC    CL8'ATBSEND'                                                     
         DC    XL52'00'                                                         
*                                                                               
ENTRYLSQ EQU   *                                                                
         EJECT                                                                  
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
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DDEDICTFIL                                                     
         EJECT                                                                  
       ++INCLUDE SPDARDARED                                                     
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'076DDEDIDARS 08/08/13'                                      
         END                                                                    
