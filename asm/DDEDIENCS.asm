*          DATA SET DDEDIENCS  AT LEVEL 019 AS OF 11/06/08                      
*PHASE EDIENCSA                                                                 
*INCLUDE BINSR31                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE EDISUB                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:      EDIENCS -- TRANSMIT PQ REPORTS VIA ENCODA EC (APPC/MVS)*         
*                         (SAME PROCESS AS EDIBIAS)                   *         
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
         TITLE 'DDEDIENCS--PERFORM ECONDA TRANSMISSIONS USING APPC/MVS'         
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
EDIENCS  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,EDIENCS*,=A(R13CHAIN)                                          
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
         MVC   DCBDDNAM-IHADCB(8,RF),=C'ENTRACE '  DDNAME=BITRACE               
*                                                                               
         MVC   TITLE(29),=C'EDICT: ENCODA SENDING SUBTASK'                      
         PRNT  START_SUBTASK,PRINT=ALWAYS                                       
*                                                                               
         GOTO1 =A(READCRDS),(RC)   READ PARAMETER CARDS                         
*                                                                               
         GOTO1 =A(INITIAL),(RC)    INITIALIZE                                   
         EJECT                                                                  
         PRNT  ATTACHSENDER,PRINT=ALWAYS                                        
*                                                                               
         LA    R2,SUBTASK          A(RECEIVING SUBTASK NAME)                    
         ATTACH EPLOC=(R2),ECB=ENRECB,PARAM=ENRPARMS,SZERO=NO                   
         ST    R1,ENRTCB                                                        
         OC    ENRTCB,ENRTCB                                                    
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
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
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'EDICT01 *JDS/ENCODA OPEN P+        
               ARAMETER ERROR. PROBABLE CAUSE: UNDEFINED LOCAL LUNAME'          
         ABEND 705,DUMP                                                         
*                                                                               
CANTREG  MVC   P+30(14),=C'REASON CODE = '                                      
         EDIT  REASON_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         PRNT  COULDNT_REGISTER,PRINT=ALWAYS                                    
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'EDICT01 *JDS/ENCODA APPC E+        
               RROR. COULD NOT REGISTER FOR ALLOCATES.'                         
         ABEND 705,DUMP                                                         
*                                                                               
RCVALLOC GOTO1 =A(QUERYALQ),(RC)   QUERY ALLOCATE QUEUE                         
*                                                                               
         LA    R2,ATBRAL2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   RECEIVE ALLOCATE                             
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    UNREGSTR            YES                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    INCONV              WE HAVE A CONVERSATION                       
         CLC   RETURN_CODE,ATBCTS_REQUEST_UNSUCCESSFUL                          
         BNE   NOCONV                                                           
         CLC   REASON_CODE,ATBCTS_NO_ALLOC_TO_RECEIVE                           
         BNE   NOCONV                                                           
*                                                                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'EDICT01 *JDS/ENCODA HAS NO+        
               T YET ALLOCATED ITS RECEIVER'                                    
         B     RCVALLOC            TRY TO RECEIVE ALLOCATE AGAIN                
*                                                                               
NOCONV   PRNT  NO_CONVERSATION,PRINT=ALWAYS                                     
         B     STOP                                                             
*                                                                               
INCONV   GOTO1 =A(QUERYALQ),(RC)   QUERY ALLOCATE QUEUE                         
         GOTO1 =A(PRNTATTB),(RC)   PRINT CONVERSATION ATTRIBUTES                
*                                                                               
RCVAGAIN MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
*                                                                               
         LA    R2,ATBRCVW_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   RECEIVE AND WAIT                             
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    STOP                                                             
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BNZ   BADSHAKE            BAD RETURN CODE                              
*                                                                               
         CLC   STATUS_RECEIVED,ATB_CONFIRM_RECEIVED                             
         BNE   NOCONFRM                                                         
*                                                                               
         LA    R2,ATBCFMD_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   ISSUE CONFIRM                                
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    STOP                                                             
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BNZ   BADSHAKE            BAD RETURN CODE                              
         B     RCVAGAIN            RECEIVE AGAIN                                
*                                                                               
NOCONFRM CLC   STATUS_RECEIVED,ATB_SEND_RECEIVED                                
         BE    SNDSHAKE                                                         
*                                                                               
         PRNT  RECEIVE_AGAIN,PRINT=ALWAYS                                       
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
*                                                                               
         LA    R2,ATBRCVW_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   RECEIVE AND WAIT                             
*                                                                               
         CLC   STATUS_RECEIVED,ATB_SEND_RECEIVED                                
         BNE   BADSHAKE            WHY WON'T THEY LET US SEND?                  
*                                                                               
SNDSHAKE PRNT  SWITCHINGTOSEND,PRINT=ALWAYS                                     
         LH    R2,BUFFER_LENGTH    LENGTH OF RETURNED DATA                      
         SH    R2,=H'2'            MINUS 2 BYTES FOR LENGTH ITSELF              
         BNP   BADSHAKE                                                         
*                                                                               
         LA    R1,BUFFER_LENGTH    BASIC CONVERSATION                           
         LA    R2,2(R2)            R2 = TOTAL AREA LENGTH (FOR PRNTBL)          
         ST    R1,DMCB+4                                                        
         GOTO1 =V(PRNTBL),DMCB,0,,C'DUMP',(R2),=C'1D'                           
*                                                                               
         CLC   APPCM1(APPCM1Q),BUFFER                                           
         BNE   BADSHAKE            PARTNER DID NOT RESPOND READY                
         CLC   APPCM1A(APPCM1AQ),BUFFER+19                                      
         BNE   BADSHAKE            WRONG MODE OR VERSION NUMBER                 
*                                                                               
         PRNT  ABOUTTORESPOND,PRINT=ALWAYS                                      
         XC    BUFFER,BUFFER                                                    
         LH    R5,=Y(APPCM2Q)                                                   
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   BUFFER(0),APPCM2    INITIAL SIGNON                               
         GOTO1 =A(DATETIME),(RC)   PUT DATE/TIME INTO MESSAGE                   
         MVC   BUFFER+47(8),YYYYMMDD                                            
         MVC   BUFFER+61(6),HHMMSS                                              
         LA    R5,1(R5)            R5 = L'DATA                                  
         MVC   SEND_TYPE,ATB_SEND_AND_CONFIRM                                   
         GOTO1 =A(BILINXMT),(RC)                                                
         BNE   BADSHAKE                                                         
         PRNT  CONFIRMRECEIVED,PRINT=ALWAYS                                     
         B     SHAKEOK                                                          
*                                                                               
BADSHAKE PRNT  BADHANDSHAKE,PRINT=ALWAYS                                        
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'EDICT01 *JDS/ENCODA: ERROR+        
                DURING HANDSHAKE WITH SENDER'                                   
         B     STOP                HANDSHAKE FAILED                             
*                                                                               
SHAKEOK  GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'EDICT01 *JDS/ENCODA: HANDS+        
               HAKE WITH SENDER OK'                                             
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
STOP     PRNT  STOPPINGENCODARCV,PRINT=ALWAYS                                   
         LA    R2,ENRPARMS         PARAMETERS TO RECEIVER                       
         USING ENRPARMD,R2                                                      
         POST  ENRSTOP             TELL EDIENCR TO STOP                         
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
         WAIT  ECB=ENRECB                                                       
         TM    ENRECB,X'40'                                                     
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DETACH   DETACH ENRTCB                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
*                                                                               
UNREGSTR OC    ALLOCATE_QUEUE_TOKEN,ALLOCATE_QUEUE_TOKEN                        
         BZ    GOODBYE             WE NEVER REGISTERED FOR ALLOCATES            
*                                                                               
         LA    R2,ATBURA2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   UNREGISTER FOR ALLOCATES                     
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
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
         GOTO1 =A(XFERREPS),(RC)   TRANSMIT ALL ENCODA REPORTS IN TABLE         
         B     LOOP                YES                                          
*                                                                               
CHKRCVR  TM    ENRECB,X'40'        IS ENCODA RECEIVER STILL RUNNING?            
         BO    *+6                                                              
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
         OC    ENRECB+1(3),ENRECB+1                                             
         BZ    DETACH              RECEIVER ENDED CLEANLY                       
*                                                                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'EDICT01 *ENCODA RECEIVER D+        
               IED'                                                             
         ABEND 701,DUMP                                                         
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
INITIAL  NMOD1 0,INITIAL                                                        
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* INITIALIZE                                                                    
*                                                                               
         PRNT  INITIALIZE,PRINT=ALWAYS                                          
*                                                                               
         L     R1,SLOOKECB         BUILD ECBLISTS                               
         STCM  R1,7,ALOOKECB+1                                                  
         L     R1,SSTOPECB                                                      
         STCM  R1,7,ASTOPECB+1                                                  
         STCM  R1,7,ASTPECB2+1                                                  
*                                                                               
         LA    R2,ENRPARMS           PARAMETERS TO RECEIVER                     
         USING ENRPARMD,R2                                                      
         MVC   ENRXMTTB,SXMTTBLE     A(REPORT TABLE)                            
         MVC   ENRMAJNM,SMAJORNM     A(MAJOR NAME FOR ENQ)                      
         MVC   ENRADMGR,SADTAMGR     A(DATAMGR)                                 
         MVC   ENREDADD,SEDCTADD     A(ROUTINE TO ADD EDICT RECORDS)            
         MVC   ENRTRACE,TRACEFLG     C'Y' = PRODUCE DETAILED TRACE              
         MVC   ENRWRITE,WRTSTAMP     C'Y' = WRITE TIMESTAMPS TO EDCTFIL         
         MVC   ENRLUNAM,=A(LOCAL_LU_NAME)   A(LOCAL_LU_NAME)                    
         MVC   ENRTPNAM,=A(TP_NAME)         A(TP_NAME)                          
         DROP  R2                                                               
*                                                                               
         BLDL  0,ENTRYPTS            BUILD LIST OF APPC/MVS ENTRY PTS           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                  BAD RETURN FROM BLDL MACRO                 
         LOAD  DE=ATBCFMD                                                       
         ST    R0,ATBCFMD_STRUCTURE                                             
         LOAD  DE=ATBDEAL                                                       
         ST    R0,ATBDEAL_STRUCTURE                                             
         LOAD  DE=ATBEES3                                                       
         ST    R0,ATBEES3_STRUCTURE                                             
         LOAD  DE=ATBGTA2                                                       
         ST    R0,ATBGTA2_STRUCTURE                                             
         LOAD  DE=ATBRCVW                                                       
         ST    R0,ATBRCVW_STRUCTURE                                             
         LOAD  DE=ATBSEND                                                       
         ST    R0,ATBSEND_STRUCTURE                                             
         LOAD  DE=ATBRAL2                                                       
         ST    R0,ATBRAL2_STRUCTURE                                             
         LOAD  DE=ATBRFA2                                                       
         ST    R0,ATBRFA2_STRUCTURE                                             
         LOAD  DE=ATBQAQ2                                                       
         ST    R0,ATBQAQ2_STRUCTURE                                             
         LOAD  DE=ATBURA2                                                       
         ST    R0,ATBURA2_STRUCTURE                                             
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
READCRDS NMOD1 0,READCRDS                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* READ PARAMETER CARDS BETWEEN "++ENCODA" AND "++ENCODAEND" CARDS               
*                                                                               
*   *......          "*" IN COLUMN ONE IS A COMMENT CARD                        
*   TRACE=YES        OVERRIDE TRACE FLAG                                        
*   TIMESTAMP=YES    WRITE INCOMING TIMESTAMPS TO EDICT FILE (Y/N)              
*   SUBTASK=CCCCCCCC RECEIVING SUBTASK NAME (DEFAULT = EDIENCR)                 
*   LOCALLUNAME=CCCCCCCC  LOCAL LU NAME (DEFAULT = EDCTBIAS)                    
*   TPNAMEPREFIX=CL7      TP NAME PREFIX(DEFAULT = DDSENCO)                     
*                                                                               
RC10     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++ENCODA',CARD   LOOK FOR START OF PARAMETERS                 
         BNE   RC10                                                             
*                                                                               
RC20     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++ENCODAEND',CARD  LOOK FOR END OF PARAMETERS                 
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
RC30     CLC   =C'SUBTASK=',CARD   SUBTASK=                                     
         BNE   *+14                                                             
         MVC   SUBTASK,CARD+8                                                   
         B     RC20                                                             
*                                                                               
         CLC   =C'TIMESTAMP=',CARD TIMESTAMP=                                   
         BNE   RC40                                                             
         CLC   =C'YES',CARD+10                                                  
         BE    RC20                DON'T OVERRIDE                               
         CLC   =C'NO',CARD+10                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   WRTSTAMP,C'N'       TIMESTAMP=NO                                 
         B     RC20                                                             
*                                                                               
RC40     CLC   =C'LOCALLUNAME=',CARD   LOCALLUNAME=                             
         BNE   RC50                                                             
         MVC   LOCAL_LU_NAME,CARD+12                                            
         B     RC20                                                             
*                                                                               
RC50     CLC   =C'TPNAMEPREFIX=',CARD                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TP_NAME(7),CARD+13  TRANSACTION PROGRAM NAME                     
         MVI   TP_NAME+7,C'S'      APPEND 'S'                                   
         B     RC20                                                             
*                                                                               
RCX      GOTO1 =V(PRINTER)         SKIP A LINE                                  
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
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
         CLI   XMTMETH,C'Z'        YES -- ENCODA METHOD?                        
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
         MVC   P+40(3),XMTSUBID    PRINT PQ SUBID                               
         EDIT  XMTREFNO,(5,P+45),ALIGN=LEFT                                     
         GOTO1 =V(HEXOUT),DMCB,XMTDSKAD,EDICTDA,4,=C'TOG'                       
         MVC   P+53(8),EDICTDA                                                  
         PRNT  XFERONEREPORT,PRINT=ALWAYS                                       
         DROP  R3                                                               
*                                                                               
         GOTO1 =A(SENDENC),(RC)                                                 
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
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
SENDENC  NMOD1 0,*SENDENC                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* TRANSFER A REPORT VIA ENCODA                                                  
*                                                                               
         XC    BUFFER,BUFFER           BUILD MESSAGE HEADER                     
         LH    R5,=Y(APPCM3Q)                                                   
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   BUFFER(0),APPCM3        +++BIAS SEND                             
         GOTO1 =A(DATETIME),(RC)       PUT DATE/TIME INTO MESSAGE               
         MVC   BUFFER+18(6),HHMMSS                                              
         BRAS  RE,BLDREF                                                        
         MVC   BUFFER+30(L'CUSTREF),CUSTREF                                     
         LA    R5,1(R5)                R5 = L'DATA                              
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         GOTO1 =A(BILINXMT),(RC)                                                
         BNE   SNDBI50                                                          
         LA    R2,1                    ONE RECORD SENT SO FAR                   
*                                                                               
SNDBI10  LA    R1,DMCB                                                          
         USING PQPARMD,R1                                                       
         MVC   ATBLNTRY,=A(TABLNTRY)                                            
         MVC   APQBUFF,=A(CXREC)                                                
         MVC   APQHDR,=A(PQRPTHDR)                                              
         MVC   AEDHDR,=A(EDICTHDR)                                              
         MVC   APQLINE,=A(R)                                                    
         MVC   ACITABLE,SCITABLE                                                
         MVC   ADTAMGR,SADTAMGR                                                 
         GOTO1 =V(PQGETLIN)                                                     
         BNE   SNDBI40             NO MORE REPORT LINES                         
         DROP  R1                                                               
         CLC   =C'++DDS',R+1       SHOULD THIS LINE BE TRANSMITTED?             
         BE    SNDBI10             NO -- TRANSMIT REMAINDER OF REPORT           
*                                                                               
SNDBI20  LA    R4,254              FIND END OF TEXT                             
         LA    RF,R+1(R4)                                                       
         CLI   0(RF),C' '                                                       
         BNE   *+12                FOUND IT                                     
         BCT   R4,*-12                                                          
         B     *+8                                                              
         LA    R4,1(R4)                                                         
*                                                                               
         MVI   BUFFER,C' '         PRE-FILL BUFFER WITH BLANKS                  
         MVC   BUFFER+1(L'BUFFER-1),BUFFER                                      
         LTR   R4,R4               ANY REAL DATA TO SEND?                       
         BZ    SNDBI30             NO -- SKIP IT                                
         BCTR  R4,0                                                             
         EX    R4,*+8              MOVE IN LINE                                 
         B     *+10                                                             
         MVC   BUFFER(0),R+1                                                    
         LA    R5,110              BIAS WANTS FIXED LENGTH, 110-BYTES           
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         GOTO1 =A(BILINXMT),(RC)                                                
         BNE   SNDBI50                                                          
         LA    R2,1(R2)            INCREMENT RECORD COUNTER                     
*                                                                               
SNDBI30  LA    R1,DMCB                                                          
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
         BE    SNDBI20                                                          
*                                                                               
SNDBI40  XC    BUFFER,BUFFER                                                    
         LH    R5,=Y(APPCM4Q)                                                   
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
         GOTO1 =A(BILINXMT),(RC)                                                
         BE    SNDBI60                                                          
*                                                                               
SNDBI50  MVC   P+30(16),=C'BAD ENCODA WRITE'                                    
         B     CANTSEND            SKIP IT                                      
*                                                                               
SNDBI60  PRNT  SENTVIAENCODA,PRINT=ALWAYS                                       
*                                                                               
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         MVC   ETBLNTRY,=A(TABLNTRY)                                            
         MVI   EACTION,EACTSNTQ            MARK SENT                            
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,SADTAMGR                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   SNDBIXIT            CAN'T POST EDICT FILE                        
         DROP  R1                                                               
         PRNT  RPTMARKEDSENT,PRINT=ALWAYS                                       
*                                                                               
         B     SNDBIXIT                                                         
         SPACE 2                                                                
SNDBIBAD MVC   P+11(13),=C'*** ERROR ***'                                       
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
         BNE   SNDBIXIT            CAN'T POST EDICT FILE                        
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
         DROP  R1                                                               
*                                                                               
SNDBIXIT XIT1                                                                   
         SPACE 2                                                                
CANTSEND PRNT  CANTTRANSMIT,PRINT=ALWAYS                                        
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'EDICT01 *ERROR SENDING TO +        
               ENCODA -- TRANSMISSIONS SUSPENDED'                               
         ABEND 701,DUMP                                                         
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
BLDREF   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   CUSTREF,C'4'        VERSION 4                                    
         CLI   SDRTEST,C'Y'        DISASTER RECOVERY TEST MODE?                 
         BNE   *+8                                                              
         MVI   CUSTREF,C'D'        YES -- VERSION 'D' (FOR 'D'ISASTER)          
         L     RE,SMAJORNM                                                      
         MVC   CUSTREF+1(1),5(RE)  'A' FOR ADV, 'R' FOR REP                     
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
         LTORG                                                                  
         EJECT                                                                  
BILINXMT NMOD1 0,BILINXMT                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* ON ENTRY, R5 = LENGTH OF DATA                                                 
*                                                                               
         LR    R2,R5                                                            
         LA    R2,4(R2)            L'DATA + 2 FOR LENGTH + 2 FOR CRLF           
         STH   R2,BUFFER_LENGTH                                                 
         ST    R2,SEND_LENGTH                                                   
         LA    R1,BUFFER(R5)                                                    
         MVC   0(2,R1),=X'0D25'    TERMINATE RECORD WITH CRLF                   
         CLI   TRACEFLG,C'Y'                                                    
         BNE   BILIN20                                                          
*                                                                               
         LA    RE,=C'BUFFER DATA'                                               
         ST    RE,DMCB                                                          
         MVI   DMCB,11                                                          
         CLC   SEND_TYPE,ATB_BUFFER_DATA                                        
         BE    BILIN10                                                          
         LA    RE,=C'SEND AND CONFIRM'                                          
         ST    RE,DMCB                                                          
         MVI   DMCB,16                                                          
         CLC   SEND_TYPE,ATB_SEND_AND_CONFIRM                                   
         BE    BILIN10                                                          
         DC    H'0'                UNKNOWN SEND TYPE                            
BILIN10  GOTO1 =V(PRNTBL),DMCB,,BUFFER_LENGTH,C'DUMP',(R2),=C'1D'               
*                                                                               
BILIN20  LA    R2,ATBSEND_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   SEND DATA                                    
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    BILINXOK            RETURN CODE WAS OK                           
*                                                                               
         LTR   RB,RB               NO GOOD -- SET CC NOT EQUAL                  
         B     BILINXX                                                          
*                                                                               
BILINXOK CR    RB,RB               SET CC EQUAL                                 
*                                                                               
BILINXX  XIT1                                                                   
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
         DC    H'0'                APPC/MVS CALL NOT ACCEPTED                   
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
COMMWORK DS    0D                  COMMON STORAGE AREA                          
         SPACE 2                                                                
         ATBSERV                                                                
         SPACE 2                                                                
         ATBCTASM                                                               
         SPACE 2                                                                
* APPC/MVS CALL PARAMETERS                                                      
*                                                                               
CONVERSATION_TYPE              DS    F                                          
SYM_DEST_NAME                  DC    CL8' '                                     
PARTNER_LU_NAME                DC    CL17' '                                    
LOCAL_LU_NAME                  DC    CL8'EDCTBIAS'                              
MODE_NAME                      DS    CL8                                        
TP_NAME_LENGTH                 DC    F'8'                                       
TP_NAME                        DC    CL64'DDSENCO'                              
SYNC_LEVEL                     DS    F                                          
USER_ID                        DC    CL10' '                                    
PROFILE                        DC    CL10' '                                    
CONVERSATION_ID                DS    CL8                                        
CONVERSATION_CORRELATOR        DS    CL8                                        
ALLOCATE_QUEUE_TOKEN           DC    CL8'00'                                    
NOTIFY_TYPE                    DS    F                                          
                               DC    A(APPCECB) MUST FOLLOW NOTIFY_TYPE         
LUW_ID                         DS    XL26                                       
ALLOCATE_QUEUE_SIZE            DS    F                                          
ALLOCATE_QUEUE_OLDEST          DS    F                                          
LAST_REC_ALLOC_ISSUED          DS    CL8                                        
LAST_REC_ALLOC_RETURNED        DS    CL8                                        
CONVERSATION_STATE             DS    F                                          
RETURN_CODE                    DS    F                                          
REASON_CODE                    DS    F                                          
DEALLOCATE_TYPE                DS    F                                          
FILL                           DC    F'0'   ATB_FILL_LL                         
RECEIVE_LENGTH                 DS    F                                          
RECEIVE_ACCESS_TOKEN           DC    F'0'                                       
STATUS_RECEIVED                DS    F                                          
DATA_RECEIVED                  DS    F                                          
REQUEST_TO_SEND_RECEIVED       DS    F                                          
REQUEST_TO_SEND_VALUE          DS    F                                          
SEND_TYPE                      DS    F                                          
SEND_LENGTH                    DS    F                                          
SEND_ACCESS_TOKEN              DC    F'0'                                       
RECEIVE_ALLOCATE_TYPE          DC    F'3'   ATBCTS_TIMED                        
TIME_OUT_VALUE                 DC    F'600' TEN MINUTES                         
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
APPCM1   DC    C'+++BIAS SENDER='                                               
APPCM1Q  EQU   *-APPCM1                                                         
         DC    C'DDS '                                                          
APPCM1A  DC    C'CONV=RECEIVE VERSION=1 '                                       
APPCM1AQ EQU   *-APPCM1A                                                        
         DC    C'DATE=YYYYMMDD TIME=HHMMSS'                                     
         SPACE 2                                                                
APPCM2   DC    C'+++BIAS SENDER=DDS VERSION=1 STATUS=READY DATE=YYYYMMD+        
               D TIME=HHMMSS'                                                   
APPCM2Q  EQU   *-APPCM2                                                         
         SPACE 2                                                                
APPCM3   DC    C'+++BIAS SEND TIME=HHMMSS REF#=VEDDNNNNNNYYYYMMDD'              
APPCM3Q  EQU   *-APPCM3                                                         
         SPACE 2                                                                
APPCM4   DC    C'+++BIAS END TIME=HHMMSS COUNT=NNNNNN'                          
APPCM4Q  EQU   *-APPCM4                                                         
         SPACE 5                                                                
DMCB     DS    10F                                                              
DUB      DS    D                                                                
PRNTDUB  DS    D                                                                
PRNTTIME DS    CL9                                                              
YYYYMMDD DS    CL8                 DATE                                         
HHMMSS   DS    CL6                 TIME                                         
CUSTREF  DS    CL18                VEDDNNNNNNYYYYMMDD                           
WORK     DS    CL256                                                            
OPERSTOP DC    C'N'                'Y' IF OPERATOR WANTS TO STOP                
TRACEFLG DS    C                   'Y' FOR DETAILED TRACE                       
WRTSTAMP DC    C'Y'                'N' TO SUPPRESS WRITING TIMESTAMPS           
WAITSECS DC    F'12000'            2 MINS. FOR APPC CALLS TO COMPLETE           
         SPACE 2                                                                
RCVAMSG  DC    C'EDICT01 *JDS/ENCODA: UNDEFINED LOCAL LUNAME XXXXXXXX. +        
               REPLY RETRY OR CANCEL'                                           
RCVAMSGQ EQU   *-RCVAMSG                                                        
         SPACE 2                                                                
ECBLST   DS    0F                                                               
ALOOKECB DC    X'00',AL3(0)        A(LOOKECB)                                   
         DC    X'00',AL3(ENRECB)   A(RECEIVER ECB)                              
ASTOPECB DC    X'80',AL3(0)        A(STOPECB)                                   
         SPACE 2                                                                
ECBLST2  DS    0F                                                               
ASTPECB2 DC    X'00',AL3(0)        A(STOPECB)                                   
         DC    X'80',AL3(APPCECB)  A(APPCECB)                                   
         SPACE 2                                                                
ENRPARMS DC    XL(ENRPRMLQ)'00'    PARAMETERS TO EDIENCR (VIA R1)               
ENRTCB   DS    F                   TCB OF ATTACHED SUBTASK EDIENCR              
ENRECB   DC    F'0'                ECB OF ATTACHED SUBTASK EDIENCR              
*                                                                               
SUBTASK  DC    C'EDIENCR '         RECEIVING SUBTASK NAME                       
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
ATBDEAL_STRUCTURE         DS    A                                               
                          DC    CL16'DEALLOCATE'                                
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(DEALLOCATE_TYPE)                      
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBCFMD_STRUCTURE         DS    A                                               
                          DC    CL16'CONFIRMED'                                 
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
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
                          DC    X'00',AL3(BUFFER_LENGTH)                        
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
ATBSEND  DC    CL8'ATBSEND'                                                     
         DC    XL52'00'                                                         
ATBURA2  DC    CL8'ATBURA2'                                                     
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
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019DDEDIENCS 11/06/08'                                      
         END                                                                    
