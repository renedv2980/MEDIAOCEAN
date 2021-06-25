*          DATA SET DDEDIEZS   AT LEVEL 032 AS OF 11/20/06                      
*PHASE EDIEZSA                                                                  
*INCLUDE ADDAY                                                                  
*INCLUDE BINSR31                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE EDISUB                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        EDIEZS -- TRANSMIT PQ REPORTS VIA AT&T EASYLINK      *         
*                          USING APPC/MVS                             *         
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
*                R7 -- WORK                                           *         
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
         TITLE 'DDEDIEZS -- AT&&T EASYLINK TRANSMISSIONS VIA APPC/MVS'          
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
EDIEZS   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*EDIEZS*,=A(R13CHAIN)                                          
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
         MVC   DATAMGR,SADTAMGR                                                 
         MVC   TRACEFLG,STRACEON   'Y' = PRINT DETAILED TRACE                   
         ENTRY TRACEFLG                                                         
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         L     RF,=V(PRNTER)       A(SYSPRINT DCB)                              
         MVC   DCBDDNAM-IHADCB(8,RF),=C'EZTRACE '  DDNAME=EZTRACE               
*                                                                               
         MVC   TITLE(34),=C'EDICT: SENDING SUBTASK TO EASYLINK'                 
         PRNT  START_SUBTASK,PRINT=ALWAYS                                       
*                                                                               
         L     RF,SMAJORNM                                                      
         MVC   MAJORNAM,0(RF)      MAJOR RESOURCE NAME                          
*                                                                               
         GOTO1 =A(READCRDS),(RC)   READ PARAMETER CARDS                         
*                                                                               
         GOTO1 =A(INITIAL),(RC)    INITIALIZE                                   
*                                                                               
         MVC   CONVERSATION_TYPE,ATB_BASIC_CONVERSATION                         
         MVC   LOCAL_LU_NAME,EZRCLUID                                           
         MVC   PARTNER_LU_NAME(L'EZLUID),EZLUID                                 
         MVC   TP_NAME_LENGTH,=F'6'                                             
         MVC   RETURN_CONTROL,ATB_WHEN_SESSION_ALLOCATED                        
         MVC   SYNC_LEVEL,ATB_CONFIRM                                           
         MVC   SECURITY_TYPE,ATB_SECURITY_NONE                                  
*                                                                               
         L     R2,=A(ATBALC2_STRUCTURE)                                         
         GOTO1 =A(CALLAPPC),(RC)   ALLOCATE THE CONVERSATION                    
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    GOODBYE             YES                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    RQSTCONF            ALLOCATE WAS OK                              
*                                                                               
         CLC   RETURN_CODE,ATB_PARAMETER_ERROR                                  
         BNE   CANTALLC            CAN'T ALLOCATE                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'EDICT01 *EASYLINK SENDER P+        
               ARAMETER ERROR. PROBABLE CAUSE: UNDEFINED LOCAL LUNAME'          
         ABEND 705,DUMP                                                         
*                                                                               
CANTALLC GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'EDICT01 *EASYLINK SENDER O+        
               PEN/SEND FAILED -- CALL COMMUNICATIONS / AT&&T'                  
         ABEND 703,DUMP                                                         
*                                                                               
RQSTCONF L     R2,=A(ATBCFM_STRUCTURE)                                          
         GOTO1 =A(CALLAPPC),(RC)   REQUEST CONFIRMATION                         
*                                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR SAYS STOP?                          
         BE    GOODBYE             YES                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    EZALCOK             CONFIRMATION RECEIVED OK                     
*                                                                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'EDICT01 *EASYLINK SENDER O+        
               PEN/CONFIRM FAILED -- CALL COMMUNICATIONS / AT&&T'               
         ABEND 704,DUMP                                                         
*                                                                               
EZALCOK  PRNT  OPENEZLINKSENDER,PRINT=ALWAYS                                    
*                                                                               
         GOTO1 =A(PRNTATTB),(RC)   PRINT CONVERSATION ATTRIBUTES                
*                                                                               
         CLI   DOCOMMS,C'Y'        ARE WE DOING COMMUNICATIONS?                 
         BNE   LOOP                NO, SO DON'T ATTACH RECEIVER                 
         CLI   ATCHRCVR,C'Y'       ATTACH RECEIVER?                             
         BNE   LOOP                NO                                           
*                                                                               
         LA    R2,SUBTASK                                                       
         ATTACH EPLOC=(R2),ECB=EZRECB,PARAM=EZRPARMS,SZERO=NO                   
         ST    R1,EZRTCB                                                        
         OC    EZRTCB,EZRTCB                                                    
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
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
*                                                                               
         XC    0(4,RF),0(RF)                                                    
*                                                                               
STOP     CLI   ATCHRCVR,C'Y'       DID WE ATTACH RECEIVER?                      
         BNE   DEALLOC             NO                                           
*                                                                               
         PRNT  STOPPINGEZR,PRINT=ALWAYS                                         
         LA    R2,EZRPARMS         PARAMETERS TO RECEIVER                       
         USING EZRPARMD,R2                                                      
         POST  EZRSTOP             TELL EDIEZR TO STOP                          
         DROP  R2                                                               
*                                                                               
DEALLOC  MVC   DEALLOCATE_TYPE,ATB_DEALLOCATE_SYNC_LEVEL                        
         L     R2,=A(ATBDEAL_STRUCTURE)                                         
         GOTO1 =A(CALLAPPC),(RC)   DEALLOCATE THE CONVERSATION                  
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         PRNT  DEALLOCATEDOK,PRINT=ALWAYS                                       
*                                                                               
         CLI   ATCHRCVR,C'Y'       DID WE ATTACH RECEIVER?                      
         BNE   GOODBYE             NO                                           
*                                                                               
         WAIT  ECB=EZRECB                                                       
         TM    EZRECB,X'40'                                                     
         BO    *+6                                                              
         DC    H'0'                                                             
         DETACH EZRTCB                                                          
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
         BO    *+6                 YES                                          
         DC    H'0'                                                             
         XC    0(4,RF),0(RF)                                                    
*                                                                               
         GOTO1 =A(XFERREPS),(RC)   TRANSMIT ALL AT&T REPORTS IN TABLE           
*                                                                               
         TM    EZRECB,X'40'        IS EASYLINK RECEIVER STILL RUNNING?          
         BZ    LOOP                YES                                          
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'EDICT01 *EASYLINK RECEIVER+        
                DIED -- CALL COMMUNICATIONS / AT&&T'                            
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
* ALLOCATE THE SPACE FOR A TABLE THAT WILL CONTAINS THE MULTI                   
* DESTINATIONS OF A REPORT AND THEIR CORRESPONDING EDICT FILE ENTRY             
* ADDRESS.                                                                      
*                                                                               
         LH    R5,SMAXDSTS         MAX DESTS/REPORT                             
         MHI   R5,DSTAEFTQ         X SIZE OF EACH TABLE ENTRY                   
         LA    R5,8(R5)            ADD ROOM FOR EYE-CATCHER                     
*                                                                               
         STORAGE OBTAIN,LENGTH=(5) ... DST+A(EDIFILE_ENTRY) TABLE               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL STORAGE OBTAIN                  
         MVC   0(8,R1),=C'*DSTAEFT'                                             
         LA    R1,8(R1)            BUMP PAST LABEL                              
         ST    R1,ADSTAEFT         A(DST+A(EDIFILE_ENTRY) TABLE)                
*                                                                               
*                                                                               
         L     R1,SLOOKECB         BUILD ECBLIST                                
         STCM  R1,7,ALOOKECB+1                                                  
         L     R1,SSTOPECB                                                      
         STCM  R1,7,ASTOPECB+1                                                  
         STCM  R1,7,ASTPECB2+1                                                  
*                                                                               
         LA    R2,EZRPARMS         PARAMETERS TO RECEIVER                       
         USING EZRPARMD,R2                                                      
         MVC   EZRLUID,=A(EZRCLUID) LUID OF RECEIVER                            
         MVC   EZRXMTTB,SXMTTBLE                                                
         MVC   EZRDSTTB,SDSTTBLE                                                
         MVC   EZRMAJNM,SMAJORNM                                                
         MVC   EZRADMGR,DATAMGR                                                 
         MVC   EZRTRACE,TRACEFLG                                                
         MVC   EZRDRTST,SDRTEST                                                 
         MVC   EZRTPNAM,=A(RECEIVER_TP_NAME)                                    
         MVC   EZREDADD,SEDCTADD     A(ROUTINE TO ADD EDICT RECORDS)            
         DROP  R2                                                               
*                                                                               
         BLDL  0,ENTRYPTS            BUILD LIST OF APPC/MVS ENTRY PTS           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                  BAD RETURN FROM BLDL MACRO                 
         LOAD  DE=ATBALC2                                                       
         ST    R0,ATBALC2_STRUCTURE                                             
         LOAD  DE=ATBDEAL                                                       
         ST    R0,ATBDEAL_STRUCTURE                                             
         LOAD  DE=ATBEES3                                                       
         ST    R0,ATBEES3_STRUCTURE                                             
         LOAD  DE=ATBGTA2                                                       
         ST    R0,ATBGTA2_STRUCTURE                                             
         LOAD  DE=ATBSEND                                                       
         ST    R0,ATBSEND_STRUCTURE                                             
         LOAD  DE=ATBCFM                                                        
         ST    R0,ATBCFM_STRUCTURE                                              
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
READCRDS NMOD1 0,READCRDS                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* READ PARAMETER CARDS BETWEEN "++EASYLINK" AND "++EASYLINKEND" CARDS           
*                                                                               
*   *......          "*" IN COLUMN ONE IS A COMMENT CARD                        
*   EZLUID=CCCCCCCC  LUID OF AT&T EASYLINK RECEIVER                             
*   EZRLUID=CCCCCCCC LUID OF RECEIVER FROM EASYLINK                             
*   VTAMMODE=CCCCCCCC  VTAM MODE NAME                                           
*   EZTPNAME=CCC...  TRANSACTION PROGRAM NAME FOR EASYLINK RECEIVER             
*   MYTPNAME=CCC...  TRANSACTION PROGRAM NAME FOR MY RECEIVER                   
*   USERMAXXMIT=N    MAXIMUM RPTS XMITTED IN A ROW PER USERID (DEF.=50)         
*   TRACE=YES/NO     OVERRIDE TRACE FLAG                                        
*   SUBTASK=CCCCCCCC RECEIVING SUBTASK NAME (DEFAULT = EDIEZR)                  
*   DOCOMMS=Y/N      Y = PERFORM COMMUNICATIONS WITH EASYLINK (DEFAULT)         
*   FAXPAGEWARN=N    WARN CONSOLE EVERY N PAGES FOR LONG FAX (DEF.= 20)         
*   DONTSEND=Y/N     Y = DON'T SEND ANY FAXES TO EASYLINK                       
*   ATTACHRCVR=Y/N   Y = ATTACH RECEIVING SUBTASK (DEFAULT)                     
*   REQUESTDLN=Y/N   Y = WE WANT DELIVERY NOTIFICATIONS (DEFAULT)               
*   USERID=UUUUUUUU  TRANSFER REPORTS FROM THIS USERID ONLY                     
*                                                                               
RC10     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++EASYLINK',CARD LOOK FOR START OF PARAMETERS                 
         BNE   RC10                                                             
*                                                                               
RC20     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++EASYLINKEND',CARD   LOOK FOR END OF PARAMETERS              
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
         CLC   =C'EZLUID=',CARD    EZLUID=                                      
         BNE   *+14                                                             
         MVC   EZLUID,CARD+7                                                    
         B     RC20                                                             
*                                                                               
         CLC   =C'EZRLUID=',CARD   EZRLUID=                                     
         BNE   *+14                                                             
         MVC   EZRCLUID,CARD+8                                                  
         B     RC20                                                             
*                                                                               
         CLC   =C'VTAMMODE=',CARD  VTAMMODE=                                    
         BNE   *+14                                                             
         MVC   MODE_NAME,CARD+9                                                 
         B     RC20                                                             
*                                                                               
         CLC   =C'EZTPNAME=',CARD  EZTPNAME=                                    
         BNE   *+14                                                             
         MVC   TP_NAME(6),CARD+9                                                
         B     RC20                                                             
*                                                                               
         CLC   =C'MYTPNAME=',CARD  MYTPNAME=                                    
         BNE   *+14                                                             
         MVC   RECEIVER_TP_NAME(6),CARD+9                                       
         B     RC20                                                             
*                                                                               
         CLC   =C'DONTSEND=',CARD  TRANSMIT TO EASYLINK?                        
         BNE   RC70                                                             
         CLC   =C'NO',CARD+9                                                    
         BE    RC20                                                             
         CLC   =C'YES',CARD+9                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   DONTSEND,C'Y'       DON'T SEND ANYTHING TO EASYLINK              
         B     RC20                                                             
*                                                                               
RC70     CLC   =C'USERMAXXMIT=',CARD  USERMAXXMIT=                              
         BNE   RC80                                                             
         GOTO1 =V(NUMVAL),DMCB,CARD+12,(2,0)                                    
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                INVALID VALUE AFTER EQUALS SIGN              
         MVC   MAXURPTS,DMCB+4                                                  
         B     RC20                                                             
*                                                                               
RC80     CLC   =C'TRACE=',CARD     TRACE=                                       
         BNE   RC90                                                             
         CLC   =C'NO',CARD+6                                                    
         BNE   *+12                                                             
         MVI   TRACEFLG,C'N'       TRACE=NO                                     
         B     RC20                                                             
         CLC   =C'YES',CARD+6                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRACEFLG,C'Y'       TRACE=YES                                    
         B     RC20                                                             
*                                                                               
RC90     CLC   =C'SUBTASK=',CARD   SUBTASK=                                     
         BNE   *+14                                                             
         MVC   SUBTASK,CARD+8                                                   
         B     RC20                                                             
*                                                                               
         CLC   =C'DOCOMMS=',CARD   DOCOMMS=                                     
         BNE   RC100                                                            
         CLC   =C'YES',CARD+8                                                   
         BE    RC20                DON'T OVERRIDE                               
         CLC   =C'NO',CARD+8                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   DOCOMMS,C'N'        DOCOMMS=NO                                   
         B     RC20                                                             
*                                                                               
RC100    CLC   =C'FAXPAGEWARN=',CARD  FAXPAGEWARN=                              
         BNE   RC110                                                            
         GOTO1 =V(NUMVAL),DMCB,CARD+12,(2,0)                                    
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                INVALID VALUE AFTER EQUALS SIGN              
         MVC   WARNPAGE,DMCB+4                                                  
         B     RC20                                                             
*                                                                               
RC110    CLC   =C'ATTACHRCVR=',CARD   ATTACH EASYLINK RECEIVER?                 
         BNE   RC120                                                            
         CLC   =C'YES',CARD+11                                                  
         BE    RC20                ATTACH                                       
         CLC   =C'NO',CARD+11                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ATCHRCVR,C'N'       DON'T ATTACH                                 
         B     RC20                                                             
*                                                                               
RC120    CLC   =C'REQUESTDLN=',CARD   ASK EASYLINK FOR DLNS?                    
         BNE   RC130                                                            
         CLC   =C'YES',CARD+11                                                  
         BE    RC20                REQUEST DELIVERY NOTIFICATIONS               
         CLC   =C'NO',CARD+11                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   SENDDLNS,C'N'       DON'T SEND DLNS                              
         B     RC20                                                             
*                                                                               
RC130    CLC   =C'USERID=',CARD    USERID=XXX                                   
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
         LA    R7,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R7)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(7),E,6)  ENQUEUE THE CONTROL FILE                     
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,A(IO),0               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                NO ID RECORD                                 
*                                                                               
         LA    R7,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R7)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(7),6)    DEQUEUE THE CONTROL FILE                     
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
RCX      GOTO1 =V(PRINTER)         SKIP A LINE                                  
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
XFERREPS NMOD1 0,XFERREPS                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         CLI   DONTSEND,C'Y'       SEND FAXES TO EASYLINK?                      
         BE    XFERX               NO                                           
*                                                                               
         XC    LSTPQKEY,LSTPQKEY   LAST PRINT QUEUE REPORT SENT                 
         MVI   SKIPLAST,C'N'       DON'T SKIP LAST USERID FOUND (YET)           
*                                                                               
XFER10   LA    R7,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R7)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(7),E,8)  ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         LA    R3,XMTENTRY                                                      
         USING XMTTABLD,R3                                                      
*                                                                               
XFER15   L     R7,SXMTTBLE         R7 = A(REPORT TABLE)                         
         LA    R4,WORK                                                          
         XC    WORK,WORK           WORK = EARLIEST ENTRY --> FIFO               
*                                                                               
XFER20   MVC31 XMTENTRY,0(R7)                                                   
         CLI   0(R3),0             END OF TABLE?                                
         BE    XFER40              YES, NO MORE                                 
         CLI   XMTSOURC,XMTSRCPQ   PRINT QUEUE ENTRY?                           
         BNE   XFER30                                                           
         CLI   XMTMETH,C'E'        YES -- EASYLINK METHOD?                      
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
XFER30   AHI   R7,XMTTBLQ          BUMP TO NEXT REPORT                          
         B     XFER20                                                           
         DROP  R3                                                               
*                                                                               
XFER40   OC    WORK,WORK           ANYTHING FOUND TO SEND?                      
         BNZ   XFER50              YES                                          
         CLI   SKIPLAST,C'Y'       DID WE MAX OUT ON A USERID?                  
         BNE   XFER200             NO, SO THERE REALLY ARE NO MORE              
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
         B     XFER55              SEND IT                                      
*                                                                               
         C     R6,MAXURPTS         ARE WE ABOUT TO EXCEED THE MAXIMUM?          
         BNL   *+12                                                             
         LA    R6,1(R6)            NO -- SEND THE REPORT                        
         B     XFER55                                                           
         MVI   SKIPLAST,C'Y'       YES -- NO MORE FOR THIS USERID               
*                                                                               
XFER55   MVC   TABLNTRY,0(R4)      SAVE XMIT TABLE KEY                          
*                                                                               
         LA    R7,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R7)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(7),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
         LA    R1,DMCB                                                          
         USING PQPARMD,R1                                                       
         MVC   ATBLNTRY,=A(TABLNTRY)                                            
         MVC   APQBUFF,=A(CXREC)                                                
         MVC   APQHDR,=A(PQRPTHDR)                                              
         MVC   AEDHDR,=A(EDICTHDR)                                              
         MVC   APQLINE,=A(R)                                                    
         MVC   ACITABLE,SCITABLE                                                
         MVC   ADTAMGR,DATAMGR                                                  
         GOTO1 =V(INITPQRD)                                                     
         BE    XFER70                                                           
         DROP  R1                                                               
*                                                                               
         PRNT  CANT_SEND_REPORT,PRINT=ALWAYS                                    
         LA    R2,TABLNTRY              THIS IS THE LAST DEST ENTRY             
         USING XMTTABLD,R2                                                      
         ZICM  R4,XMTDSTNO,2            LAST DEST #                             
*                                       MARK ALL DEST UNSENDABLE                
XFER60   STCM  R4,3,XMTDSTNO                                                    
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         MVC   ETBLNTRY,=A(TABLNTRY)                                            
         MVI   EACTION,EACTJNKQ    MARK REPORT UNSENDABLE                       
         MVI   EERRORCD,EDFERNPQ   REPORT NOT FOUND ON PRINT QUEUE              
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,DATAMGR                                                 
         GOTO1 =V(POSTSENT)                                                     
         BNE   XFER65                                                           
         DROP  R1,R2                                                            
*                                                                               
         MVC   P+30(16),=C'DESTINATION NO ='                                    
         EDIT  (R4),(5,P+46),ALIGN=LEFT,ZERO=NOBLANK                            
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
XFER65   BCT   R4,XFER60                                                        
         B     XFER160                                                          
*                                                                               
XFER70   LA    R2,TABLNTRY                                                      
         USING XMTTABLD,R2                                                      
         MVI   CUSTREF,C'4'        VERSION 4                                    
         CLI   SDRTEST,C'Y'        DISASTER RECOVERY TEST MODE?                 
         BNE   *+8                                                              
         MVI   CUSTREF,C'D'        YES -- VERSION 'D' (FOR 'D'ISASTER)          
         MVC   CUSTREF+1(1),MAJORNAM+5  'A' FOR ADV, 'R' FOR REP                
         GOTO1 =V(DATCON),DMCB,(5,0),(20,CUSTREF+10)                            
*                                                                               
         MVI   BYTE,EDFERIDQ       ASSUME BAD ID/IDI RECORD ERROR               
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         LA    RF,KEY                                                           
         USING CTIREC,RF                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,XMTUSRID                                                 
         DROP  RF,R2                                                            
*                                                                               
         LA    R7,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R7)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(7),E,6)  ENQUEUE THE CONTROL FILE                     
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,A(IO),0               
*                                                                               
         LA    R7,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R7)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(7),6)    DEQUEUE THE CONTROL FILE                     
*                                                                               
         CLI   DMCB+8,0                                                         
         BNE   XFER90              USERID RECORD IS GONE                        
*                                                                               
         MVC   DATADISP,=H'28'     FOR CTFILE                                   
         L     R4,=A(IO)                                                        
         MVI   ELCODE,CTDSCELQ     PICK UP ALPHA USERID                         
         BAS   RE,GETEL                                                         
         BNE   XFER90                                                           
         MVC   USERID,CTDSC-CTDSCD(R4)                                          
*                                                                               
         L     R4,=A(IO)                                                        
         MVI   ELCODE,CTAGYELQ     GET AGENCY POWERCODE                         
         BAS   RE,GETEL                                                         
         BNE   XFER90                                                           
         MVC   AGYPOWER,CTAGYID-CTAGYD(R4)                                      
         MVC   SVAGYPOW,AGYPOWER                                                
*                                                                               
         L     R4,=A(IO)                                                        
         MVI   ELCODE,CTORGELQ     GET ORIGIN DETAILS                           
         BAS   RE,GETEL                                                         
         BNE   XFER90                                                           
         MVC   AGYNAME,CTORGNAM-CTORGD(R4)                                      
         MVC   AGYADDR,CTORGADD-CTORGD(R4)                                      
*                                                                               
         XC    ROUTCODE,ROUTCODE                                                
         L     R4,=A(IO)                                                        
         MVI   ELCODE,CTUSAELQ     GET US AGENCY EXTRA INFO                     
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   ROUTCODE,CTUSADRC-CTUSAD(R4) DARE ROUTING CODE                   
*                                                                               
         MVI   DESTTYPE,0          NO DESTINATION TYPE YET                      
         MVC   DESTINAT,EDICTHDR+10  DESTINATION (UP TO 25 CHARACTERS)          
*                                                                               
         LA    R1,DMCB                                                          
         USING DSTPARMD,R1                                                      
         CLC   =C'EDICT=',DESTINAT EDICT RECORD KEY GIVEN?                      
         BNE   *+12                                                             
         LA    RF,EDICTHDR+16      YES -- FIND RECEIVER'S INFO                  
         B     *+8                                                              
         LA    RF,USERID           NO -- FIND SENDER'S INFO                     
         ST    RF,DSTKEY                                                        
         MVC   DSTTBL,SDSTTBLE                                                  
         MVC   DSTMAJNM,SMAJORNM                                                
         GOTO1 =V(FINDDEST)                                                     
         BE    XFER80                                                           
         DROP  R1                                                               
         MVI   BYTE,EDFERNED       EDICT RECORD WAS DELETED ERROR               
         B     XFER90                                                           
*                                                                               
         USING DESTTABD,R1                                                      
XFER80   MVC   DSTNAME,DESTNAME    DESTINATION                                  
         MVC   DSTUIDN,DESTUIDN                                                 
         CLI   MAJORNAM+5,C'R'     REP?                                         
         BE    *+14                                                             
         MVC   MAILBOX,DESTADVN+2  USE ADV MAILBOX NO. (WITHOUT '62')           
         B     *+10                                                             
         MVC   MAILBOX,DESTREPN+2  USE REP MAILBOX NO. (WITHOUT '62')           
         OC    MAILBOX,MAILBOX                                                  
         BNZ   XFER100                                                          
         MVI   BYTE,EDFERNMQ       MAILBOX NUMBER IS MISSING                    
         DROP  R1                                                               
*                                                                               
XFER90   PRNT  CANT_SEND_REPORT,PRINT=ALWAYS                                    
         LA    R2,TABLNTRY              THIS IS THE LAST DEST ENTRY             
         USING XMTTABLD,R2                                                      
         ZICM  R4,XMTDSTNO,2            LAST DEST #                             
*                                       MARK ALL DEST UNSENDABLE                
XFER95   STCM  R4,3,XMTDSTNO                                                    
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         MVC   ETBLNTRY,=A(TABLNTRY)                                            
         MVI   EACTION,EACTJNKQ    MARK REPORT UNSENDABLE                       
         MVC   EERRORCD,BYTE                                                    
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,DATAMGR                                                 
         GOTO1 =V(POSTSENT)                                                     
         BNE   XFER99                                                           
         DROP  R1,R2                                                            
*                                                                               
         MVC   P+30(16),=C'DESTINATION NO ='                                    
         EDIT  (R4),(5,P+46),ALIGN=LEFT,ZERO=NOBLANK                            
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
XFER99   BCT   R4,XFER95                                                        
         B     XFER160                                                          
*                                                                               
XFER100  LA    R2,TABLNTRY                                                      
         USING XMTTABLD,R2                                                      
         MVC   P+30(8),DSTNAME     PRINT DESTINATION                            
         MVC   P+40(3),XMTSUBID    PRINT PQ SUBID                               
         EDIT  XMTREFNO,(5,P+45),ALIGN=LEFT                                     
         GOTO1 =V(HEXOUT),DMCB,XMTDSKAD,P+53,4,=C'TOG'                          
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P+63(25),DESTINAT   PRINT DESTINATION FROM *HDR RECORD           
         PRNT  XFERONEREPORT,PRINT=ALWAYS                                       
         DROP  R2                                                               
*                                                                               
         GOTO1 =A(SENDEZ),(RC)                                                  
*                                                                               
XFER160  CLI   OPERSTOP,C'Y'       OPERATOR WANTS TO STOP?                      
         BNE   XFER10              NO                                           
         B     XFERX                                                            
*                                                                               
XFER200  LA    R7,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R7)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(7),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
XFERX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
SENDEZ   NMOD1 0,SENDEZ,R8         R8 AS SECOND BASE                            
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* TRANSFER A REPORT VIA AT&T EASYLINK                                           
*                                                                               
         XC    REQUESTR,REQUESTR                                                
         XC    FAXSUBC,FAXSUBC                                                  
         XC    COVPGLNS,COVPGLNS   NO COVER PAGE YET                            
         XC    EASYDATA,EASYDATA                                                
         MVC   NUMPAGES,=H'1'      AT LEAST ONE PAGE WILL BE SENT               
         MVI   ENDOFMSG,C'N'       MESSAGE IS NOT YET COMPLETE                  
         XC    LSTSDST,LSTSDST     ASSUME NO REPORT IS SENDABLE                 
         MVC   CTABNTRY,TABLNTRY                                                
*                                                                               
         L     R1,=A(EDICTHDR)                                                  
         CLI   68(R1),C'D'         IS THIS A DARE FAX?                          
         BNE   SNDEZ_0X                                                         
*                                                                               
SNDEZ_0  LA    R1,DMCB                                                          
         USING PQPARMD,R1                                                       
         MVC   ATBLNTRY,=A(TABLNTRY)                                            
         MVC   APQBUFF,=A(CXREC)                                                
         MVC   APQHDR,=A(PQRPTHDR)                                              
         MVC   AEDHDR,=A(EDICTHDR)                                              
         MVC   APQLINE,=A(R)                                                    
         MVC   ACITABLE,SCITABLE                                                
         MVC   ADTAMGR,DATAMGR                                                  
         DROP  R1                                                               
         GOTO1 =V(PQGETLIN)                                                     
         BE    *+8                 MORE REPORT LINES TO BE READ                 
         B     SNDEZ03A            MARK REPORT UNSENDABLE                       
*                                                                               
         OC    REQUESTR,REQUESTR                                                
         BZ    *+14                                                             
         CLC   =C'++DDS',R+1                                                    
         BNE   SNDEZ_0X            NO DXC CARD IS FINE                          
*                                                                               
         CLC   =C'++DDS',R+1       IS THIS LINE A CONTROL CARD?                 
         BNE   SNDEZ_0             NO -- NEXT LINE                              
*                                                                               
         CLC   =C'TRN',R+12        IS THIS TRN CONTROL CARD?                    
         BNE   *+14                NO                                           
         MVC   REQUESTR,R+36                                                    
         B     SNDEZ_0             NEXT LINE                                    
*                                                                               
* FOR MULTIPLE DESTINATIONS, THIS FAX SUBCODE WILL APPLY TO ALL DESTS           
         CLC   =C'DXC',R+12        IS THIS DXC CONTROL CARD?                    
         BNE   *+10                NO -- LINE NEXT                              
         MVC   FAXSUBC,R+16                                                     
SNDEZ_0X  DS   0H                                                               
*                                                                               
* BUILD A DESTINATION TABLE                                                     
*                                                                               
         LA    R2,TABLNTRY                                                      
         USING XMTTABLD,R2                                                      
         MVC   NUMDESTS,XMTDSTNO                                                
         DROP  R2                                                               
*                                                                               
         L     R7,ADSTAEFT                                                      
         USING DSTAEFTD,R7                                                      
         MVC   DSTAEDST,DESTINAT   1ST DESTINATION                              
         LHI   R4,1                DEST COUNTER                                 
*                                                                               
* THE FORMAT OF FAX (LANDSCAPE/WIDE), IS DEPENDENT 'ONLY' ON THE HDR            
* DESTINATION FROM THE CURRENT LOGICAL REPORT IN A PQ ENTRY.                    
*                                                                               
         CLC   =C'FAXW',DSTAEDST   DESTINATION BEGIN W/ FAX/FAXW?               
         BNE   *+8                                                              
         MVI   EDICTHDR+35,C'W'                                                 
         B     SNDEZ00A                                                         
*                                                                               
SNDEZ00  LA    R1,DMCB                                                          
         USING PQPARMD,R1                                                       
         MVC   ATBLNTRY,=A(TABLNTRY)                                            
         MVC   APQBUFF,=A(CXREC)                                                
         MVC   APQHDR,=A(PQRPTHDR)                                              
         MVC   AEDHDR,=A(EDICTHDR)                                              
         MVC   APQLINE,=A(R)                                                    
         MVC   ACITABLE,SCITABLE                                                
         MVC   ADTAMGR,DATAMGR                                                  
         DROP  R1                                                               
         GOTO1 =V(PQGETLIN)                                                     
         BE    *+8                 MORE REPORT LINES TO BE READ                 
         B     SNDEZ03A            MARK REPORT UNSENDABLE                       
*                                                                               
         CLC   =C'++DDS',R+1       IS THIS LINE A CONTROL CARD?                 
         BNE   SNDEZ00             NO -- NEXT LINE                              
*                                                                               
         CLC   =C'DST',R+12        IS THIS DESTINATION CONTROL CARD?            
         BNE   SNDEZ00             NO -- LINE NEXT                              
         MVC   DSTAEDST,R+16       DESTINATION                                  
*                                                                               
* THE COVER PAGE CONTENT IS DEPENDENT 'ONLY' ON THE LAST                        
* SENDABLE DESTINATION THAT CALL THE COVERPAG ROUTINE.                          
*                                                                               
SNDEZ00A CLC   =C'FAX',DSTAEDST    IS DESTINATION A FAX MACHINE?                
         BE    SNDEZ02             A SENDABLE DEST FOUND!                       
*                                                                               
         MVI   LONGFXPG,C'N'       ASSUME FAX PAGES AREN'T "LONG"               
         CLC   =C'FXKEY=',DSTAEDST IS DESTINATION A FAX RECORD KEY?             
         BE    *+18                                                             
         CLC   =C'FXKYL=',DSTAEDST (FOR "LONG" FAX PAGES)                       
         BNE   SNDEZ01                                                          
         MVI   LONGFXPG,C'Y'       EASYLINK MUST GENERATE "LONG" PAGES          
*                                                                               
         MVC   FAXKEY,DSTAEDST+6                                                
         GOTO1 =A(COVERPAG),(RC)   BUILD COVER PAGE                             
         OC    DESTINAT,DESTINAT   WAS A SENDABLE DEST FOUND!                   
         BNZ   SNDEZ02             YES                                          
         B     SNDEZ03             NO - NEXT ONE                                
*                                                                               
SNDEZ01  L     R1,=A(EDICTHDR)                                                  
         CLI   68(R1),C'D'         IS THIS A DARE FAX?                          
         BNE   SNDEZ02             ASSUME THIS IS A REDILIST NAME               
         MVC   DESTINAT,DSTAEDST                                                
         GOTO1 =A(DAREFAX),(RC)                                                 
         MVC   FAXKEY,WORK         RETURNED BY DAREFAX ROUTINE                  
         OC    FAXKEY,FAXKEY       DID WE BUILD A RECEIVING ID?                 
         BZ    SNDEZ03             NO - NEXT ONE                                
         MVC   EASYDATA+14(7),FAXKEY                                            
*                                                                               
         CLI   DESTTYPE,EDFDSTHQ   HOME MARKET W/O ID?                          
         BE    *+10                                                             
         MVC   AGYPOWER,=C'D7'     NO, ALWAYS USE USERID 'DARADM'               
*                                                                               
         MVI   LONGFXPG,C'N'       STANDARD SIZE FAX PAGES                      
         GOTO1 =A(COVERPAG),(RC)   BUILD COVER PAGE                             
         OC    DESTINAT,DESTINAT   WAS THE FAX RECORD FOUND?                    
         BNZ   SNDEZ02             YES                                          
         B     SNDEZ03             NO - NEXT ONE                                
*                                                                               
SNDEZ02  STH   R4,LSTSDST          ASSUME THIS IS THE LAST SENDABLE DST         
*                                                                               
SNDEZ03  LA    R7,DSTAEFTQ(R7)                                                  
         LA    R4,1(R4)                                                         
         CH    R4,NUMDESTS         IS THIS THE LAST DEST                        
         BNH   SNDEZ00             CONTINUE TO SEARCH NEXT DST CARD             
         DROP  R7                                                               
*                                                                               
         OC    LSTSDST,LSTSDST     ALL DESTS ARE UNSENDABLE?                    
         BNZ   SNDEZ07             NO                                           
*                                                                               
SNDEZ03A PRNT  CANT_SEND_REPORT,PRINT=ALWAYS                                    
         LA    R2,CTABNTRY              THIS IS THE LAST DEST ENTRY             
         USING XMTTABLD,R2                                                      
         ZICM  R4,XMTDSTNO,2            LAST DEST #                             
*                                       MARK ALL DEST UNSENDABLE                
SNDEZ04  STCM  R4,3,XMTDSTNO                                                    
*                                                                               
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         MVC   ETBLNTRY,=A(CTABNTRY)                                            
         MVI   EACTION,EACTJNKQ    MARK REPORT UNSENDABLE                       
         MVI   EERRORCD,EDFERNFQ   NO FAX NUMBER                                
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,DATAMGR                                                 
         MVC   EFILREC,=A(EDICTREC)   GET EDICT REC IN RETURN                   
         GOTO1 =V(POSTSENT)                                                     
         BNE   SNDEZ05                                                          
         DROP  R1                                                               
*                                                                               
         BRAS  RE,ERRDFAX                                                       
*                                                                               
         MVC   P+30(16),=C'DESTINATION NO ='                                    
         EDIT  (R4),(5,P+46),ALIGN=LEFT,ZERO=NOBLANK                            
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
SNDEZ05  BCT   R4,SNDEZ04                                                       
         B     SNDEZXIT                                                         
         DROP  R2                                                               
*                                                                               
* FIND A(EDICTFILE_ENTRY) FOR EACH DESTINATION, FOR BUILDING CUSTREF            
*                                                                               
SNDEZ07  L     R7,ADSTAEFT         A(DST+A(EDIFILE_ENTRY) TABLE)                
         USING DSTAEFTD,R7                                                      
         LH    R4,NUMDESTS         NUMBER OF DESTINATIONS                       
*                                                                               
         USING XMTTABLD,R3                                                      
         LA    R3,XMTENTRY                                                      
*                                                                               
         MVC   XMTENTRY,CTABNTRY   BUILD XMIT KEY FOR 1ST DEST                  
         MVC   XMTDSTNO,=H'1'      PQ RPT LOGICAL REPORT 1ST DEST               
*                                                                               
         LA    R5,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R5)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(5),E,8)  ENQUEUE THE TRANSMIT TABLE                   
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
SNDEZ10  MVC31 XMTENTRY,0(R2)                                                   
         MVC   DSTAEADR,XMTDSKAD   SAVE THE EDICT FILE ENTRY ADDRESS            
         AHI   R2,XMTTBLQ          NEXT DEST (THEY MUST BE TOGETHER)            
         LA    R7,DSTAEFTQ(R7)     NEXT TABLE ENTRY                             
         BCT   R4,SNDEZ10          NEXT XMIT TABLE WITH DIFFERENT DEST          
         DROP  R3,R7                                                            
*                                                                               
         LA    R5,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R5)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(5),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
         TIME  BIN                                                              
         ST    R0,STRTTIME         REMEMBER TIME WE START SENDING               
         XC    NUMLINES,NUMLINES   RESET LINE COUNTER                           
*                                                                               
         XC    BUFFER,BUFFER                                                    
         MVC   BUFFER(10),=C'SIGNON DDS'                                        
         MVC   BUFFER+10(6),MAILBOX                                             
         MVI   BUFFER+16,C' '                                                   
         LA    R3,BUFFER+17                                                     
*                                                                               
         LA    R6,USERID           POINT R6 TO SIGNON-ID                        
         LA    R0,L'USERID         MAXIMUM CHARS IN SIGNON-ID                   
SNDEZ20  MVC   0(1,R3),0(R6)       SIGNON-ID                                    
         LA    R3,1(R3)                                                         
         BCTR  R0,0                                                             
         LTR   R0,R0                                                            
         BZ    *+16                USERID LENGTH IS THE MAXIMUM                 
         LA    R6,1(R6)                                                         
         CLI   0(R6),C' '          END OF ID?                                   
         BNE   SNDEZ20             NO                                           
*                                                                               
         MVC   0(4,R3),=C'.DDS'    PASSWORD                                     
         LA    R3,4(R3)            BUMP PAST PASSWORD                           
         S     R3,=A(BUFFER)                                                    
         LR    R5,R3                                                            
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         MVI   TRACEONE,C'Y'                                                    
         GOTO1 =A(EZLINXMT),(RC)                                                
         BNE   SNDEZ240                                                         
*                                                                               
* READ DST+A(EDIFILE_ENTRY) TABLE ONE AT A TIME AND BUILD /NTF AND              
* CUSTOM REF #, TEL#, BILLING CODE...                                           
*                                                                               
         L     R7,ADSTAEFT         A(DST+A(EDIFILE_ENTRY) TABLE)                
         USING DSTAEFTD,R7                                                      
         LH    R4,NUMDESTS         NUMBER OF DESTINATIONS                       
*                                                                               
         LA    R2,CTABNTRY         BUILD XMIT KEY FOR 1ST DEST                  
         USING XMTTABLD,R2                                                      
         MVC   XMTDSTNO,=H'1'      PQ RPT LOGICAL REPORT 1ST DEST               
         MVC   XMTDSKAD,DSTAEADR   EDICT FILE ENTRY DISK ADDRESS                
         DROP  R2                                                               
*                                                                               
SNDEZ30  CLC   =C'FAX',DSTAEDST    IS DESTINATION A FAX MACHINE?                
         BNE   *+12                                                             
         MVI   DESTTYPE,EDFDSTFQ   IT'S A FAX NUMBER                            
         B     SNDEZ70                                                          
*                                                                               
         MVI   LONGFXPG,C'N'       ASSUME FAX PAGES AREN'T "LONG"               
         CLC   =C'FXKEY=',DSTAEDST IS DESTINATION A FAX RECORD KEY?             
         BE    *+18                                                             
         CLC   =C'FXKYL=',DSTAEDST (FOR "LONG" FAX PAGES)                       
         BNE   SNDEZ40                                                          
         MVI   LONGFXPG,C'Y'       EASYLINK MUST GENERATE "LONG" PAGES          
*                                                                               
         MVI   DESTTYPE,EDFDSTXQ   YES                                          
         MVC   FAXKEY,DSTAEDST+6                                                
         GOTO1 =A(COVERPAG),(RC)   BUILD COVER PAGE                             
         MVC   DSTAEDST,DESTINAT                                                
         OC    DSTAEDST,DSTAEDST   WAS THE FAX RECORD FOUND?                    
         BNZ   SNDEZ70             YES                                          
         B     SNDEZ60             NO - ERROR                                   
*                                                                               
SNDEZ40  L     R1,=A(EDICTHDR)                                                  
         CLI   68(R1),C'D'         IS THIS A DARE FAX?                          
         BNE   SNDEZ50                                                          
         MVC   DESTINAT,DSTAEDST                                                
         GOTO1 =A(DAREFAX),(RC)                                                 
         MVC   FAXKEY,WORK         RETURNED BY DAREFAX ROUTINE                  
         OC    FAXKEY,FAXKEY       DID WE BUILD A RECEIVING ID?                 
         BZ    SNDEZ60             NO - ERROR                                   
         MVC   EASYDATA+14(7),FAXKEY                                            
*                                                                               
         CLI   DESTTYPE,EDFDSTHQ   HOME MARKET W/O ID?                          
         BE    *+10                                                             
         MVC   AGYPOWER,=C'D7'     NO, ALWAYS USE USERID 'DARADM'               
*                                                                               
         MVI   LONGFXPG,C'N'       STANDARD SIZE FAX PAGES                      
         GOTO1 =A(COVERPAG),(RC)   BUILD COVER PAGE                             
         MVC   DSTAEDST,DESTINAT                                                
         OC    DSTAEDST,DSTAEDST   WAS THE FAX RECORD FOUND?                    
         BNZ   SNDEZ70             YES                                          
         B     SNDEZ60             NO - ERROR                                   
*                                                                               
SNDEZ50  MVI   DESTTYPE,EDFDSTMQ   ASSUME IT'S A MAILBOX NUMBER                 
         CLI   DSTAEDST,C'9'       IS IT A MAILBOX NUMBER?                      
         BH    *+12                NO - REDILIST NAME                           
         CLI   DSTAEDST,C'0'                                                    
         BNL   SNDEZ70             YES - MAILBOX NUMBER                         
*                                                                               
         MVI   DESTTYPE,EDFDSTRQ   IT'S A REDILIST NAME                         
         CLI   DSTAEDST+3,C' '     STATION CALL LETTERS ARE ASSUMED             
         BH    *+8                 IF THERE ARE ONLY 3 CHARACTERS. . .          
         MVI   DSTAEDST+3,C'9'     . . .THEN INSERT A '9' (EX:  WGN9)           
         CLI   DSTAEDST+4,C' '     MEDIA 'T'?                                   
         BH    *+8                                                              
         MVI   DSTAEDST+4,C'T'     YES -- INSERT THE 'T' (EX:  WGN9T)           
*                                                                               
         CLI   DSTAEDST+3,C'9'     DID I INSERT A '9'?                          
         BNE   SNDEZ70             NO                                           
*                                  YES - CHECK EZLINK COMMAND LIST              
         L     RE,=A(EZRSVCMD)                                                  
SNDEZ53  OC    0(3,RE),0(RE)                                                    
         BZ    SNDEZ70             END OF LIST                                  
         CLC   0(3,RE),DSTAEDST                                                 
         BE    *+12                THIS IS A RESERVED EZ COMMAND                
         AHI   RE,3                                                             
         B     SNDEZ53                                                          
*                                  ADD Z9 IN THE FRONT OF REDILIST NAME         
         MVC   WORK(2),=C'Z9'                                                   
         MVC   WORK+2(L'DSTAEDST-2),DSTAEDST                                    
         MVC   DSTAEDST,WORK                                                    
         B     SNDEZ70                                                          
*                                                                               
SNDEZ60  LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         MVC   ETBLNTRY,=A(CTABNTRY)                                            
         MVI   EACTION,EACTJNKQ    MARK REPORT UNSENDABLE                       
         MVI   EERRORCD,EDFERNFQ   NO FAX NUMBER                                
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,DATAMGR                                                 
         MVC   EFILREC,=A(EDICTREC)   GET EDICT REC IN RETURN                   
         GOTO1 =V(POSTSENT)                                                     
         BNE   SNDEZXIT                                                         
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
         DROP  R1                                                               
*                                                                               
         BRAS  RE,ERRDFAX                                                       
*                                                                               
         B     SNDEZ130            NEXT DEST                                    
*                                                                               
SNDEZ70  CLI   SENDDLNS,C'Y'       DO WE WANT DELIVERY NOTIFICATIONS?           
         BNE   SNDEZ80                                                          
         XC    BUFFER,BUFFER                                                    
         MVC   BUFFER(4),=C'/NTF'  SO WE GET NOTIFICATIONS                      
         LA    R5,4                                                             
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         MVI   TRACEONE,C'Y'                                                    
         GOTO1 =A(EZLINXMT),(RC)                                                
         BNE   SNDEZ240                                                         
*                                                                               
SNDEZ80  XC    BUFFER,BUFFER                                                    
*                                                                               
         CLI   SDRTEST,C'Y'        DISASTER RECOVERY TEST MODE?                 
         BNE   *+18                                                             
         MVC   BUFFER(16),=C'FAXW 18457594610'                                  
         LA    R3,BUFFER+15        YES -- IGNORE APPLIC. FAX NUMBER             
         B     SNDEZ110                                                         
*                                                                               
         L     RF,=A(TSTIDTAB)     TABLE OF DDS TEST USERIDS                    
SNDEZ90  CLC   USERID,0(RF)                                                     
         BNE   SNDEZ100                                                         
         MVC   BUFFER(5),=C'FAXW '                                              
         MVC   BUFFER+5(11),10(RF) DDS USERID: USE OUR FAX NUMBER               
         LA    R3,BUFFER+15        POINT TO LAST DIGIT IN FAX NUMBER            
         B     SNDEZ110                                                         
SNDEZ100 LA    RF,21(RF)                                                        
         CLI   0(RF),X'FF'                                                      
         BNE   SNDEZ90                                                          
SNDEZ102 MVC   BUFFER(25),DSTAEDST DESTINATION                                  
*                                                                               
         LA    R3,BUFFER+25        R3=A(END OF LONGEST POSSIBLE STRING)         
         LA    RF,25                                                            
SNDEZ105 CLI   0(R3),C'.'          REPLACE "."S BY "-"                          
         BNE   *+8                   BECAUSE ATT CAN'T HANDLE IT                
         MVI   0(R3),C'-'                                                       
         BCTR  R3,0                NO -- BACK UP                                
         BCT   RF,SNDEZ105                                                      
*                                                                               
         LA    R3,BUFFER+25        R3=A(END OF LONGEST POSSIBLE STRING)         
         CLI   0(R3),C' '          END OF STRING?                               
         BH    SNDEZ110                                                         
         BCTR  R3,0                NO -- BACK UP                                
         B     *-10                                                             
*                                                                               
SNDEZ110 MVI   1(R3),X'5E'         SEMICOLON (DELIMITER)                        
*                                                                               
         LA    R2,CTABNTRY                                                      
         USING XMTTABLD,R2                                                      
         LA    R1,DMCB                                                          
         USING CONVDSKD,R1                                                      
         MVC   CONVDMGR,DATAMGR    A(DATAMGR)                                   
         MVI   CONVACTN,CONVDSKF   CONVERT DSKADDR TO REFERENCE NUMBER          
         MVC   CONVDSK,XMTDSKAD    EDICT FILE DISK ADDRESS                      
         MVC   CONVOFF,=A(CUSTREF+2)  RESULT GOES INTO CUSTREF                  
         GOTO1 =V(CONVDSKA)                                                     
         DROP  R1,R2                                                            
         MVC   2(18,R3),CUSTREF    CUSTOMER REFERENCE NUMBER                    
         MVI   20(R3),C':'         COLON (DELIMITER)                            
         LA    R3,21(R3)                                                        
*                                                                               
         MVC   0(8,R3),USERID      START BILLCODE WITH USERID (1ST 8)           
         CLI   MAJORNAM+5,C'A'     ADV?                                         
         BE    SNDEZ120            YES -- NO OVERRIDE ALLOWED                   
         CLC   EDICTHDR+1(4),MYSPACES   OVERRIDE ORIGIN GIVEN?                  
         BNH   SNDEZ120                                                         
         MVC   0(4,R3),EDICTHDR+1  YES                                          
         MVC   4(4,R3),MYSPACES                                                 
*                                                                               
SNDEZ120 L     R1,=A(EDICTHDR)                                                  
         MVC   8(13,R3),55(R1)     REMAINDER OF BILLING INFORMATION             
         LA    R3,20(R3)           A(LAST BILLING CHARACTER)                    
         CLI   0(R3),C' '          FIND LAST NON-BLANK CHARACTER                
         BH    *+10                GOT IT                                       
         BCTR  R3,0                BACK UP                                      
         B     *-10                                                             
*                                                                               
         LA    R2,CTABNTRY                                                      
         USING XMTTABLD,R2                                                      
         CLC   XMTDSTNO,LSTSDST    LAST SENDABLE DEST?                          
         BL    *+12                                                             
         MVI   1(R3),C'+'          DELIMITER FOR LAST DEST                      
         B     *+8                                                              
         MVI   1(R3),C','          DELIMITER, BUT MORE DEST TO COME             
         DROP  R2                                                               
*                                                                               
         LA    R3,2(R3)                                                         
         S     R3,=A(BUFFER)                                                    
         LR    R5,R3                                                            
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         MVI   TRACEONE,C'Y'                                                    
         GOTO1 =A(EZLINXMT),(RC)                                                
         BNE   SNDEZ240                                                         
*                                                                               
*                                                                               
SNDEZ130 LA    R2,CTABNTRY         BUILD XMIT KEY FOR NEXT DEST                 
         USING XMTTABLD,R2                                                      
         ZICM  RE,XMTDSTNO,2       INCREMENT DEST #                             
         LA    RE,1(RE)                                                         
         STCM  RE,3,XMTDSTNO                                                    
         LA    R7,DSTAEFTQ(R7)     NEXT DEST AND EDIFILE ENTRY DISK ADR         
         MVC   XMTDSKAD,DSTAEADR   EDICT FILE ENTRY DISK ADDRESS                
         DROP  R2,R7                                                            
         BCT   R4,SNDEZ30          NEXT DEST                                    
*                                                                               
         MVC   CTABNTRY,TABLNTRY   RESTORE THE LAST DEST REP                    
*                                                                               
*                                                                               
         XC    BUFFER,BUFFER                                                    
         LA    R3,BUFFER                                                        
         CLI   EDICTHDR+36,C'P'    REPLACE X'89'S WITH /PAGE?                   
         BNE   *+18                                                             
         MVI   LASTEJCT,C'Y'       LAST LINE WAS A /PAGE                        
         MVC   BUFFER(10),=C'/FORM/PAGE' DON'T PRINT EZLINK HDR                 
         LA    R3,10(R3)                                                        
         CLI   EDICTHDR+35,C'W'    WIDE REPORT?                                 
         BNE   *+14                                                             
         MVC   0(5,R3),=C'/WIDE'   YES                                          
         LA    R3,5(R3)                                                         
         S     R3,=A(BUFFER)                                                    
         LR    R5,R3                                                            
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         MVI   TRACEONE,C'Y'                                                    
         GOTO1 =A(EZLINXMT),(RC)                                                
         BNE   SNDEZ240                                                         
*                                                                               
         CLI   SDRTEST,C'Y'        DISASTER RECOVERY TEST MODE?                 
         BNE   SNDEZ140                                                         
         MVI   BUFFER,C'*'                                                      
         MVC   BUFFER+1(49),BUFFER FILL BUFFER WITH ASTERISKS                   
         LA    R5,50                                                            
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         MVI   TRACEONE,C'Y'                                                    
         GOTO1 =A(EZLINXMT),(RC)                                                
         BNE   SNDEZ240                                                         
         XC    BUFFER,BUFFER                                                    
         SR    R5,R5                                                            
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         MVI   TRACEONE,C'N'                                                    
         GOTO1 =A(EZLINXMT),(RC)                                                
         BNE   SNDEZ240                                                         
         MVC   BUFFER(50),=C'THIS IS A TEST FAX TRANSMISSION - PLEASE D+        
               ISREGARD'                                                        
         LA    R5,50                                                            
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         MVI   TRACEONE,C'Y'                                                    
         GOTO1 =A(EZLINXMT),(RC)                                                
         BNE   SNDEZ240                                                         
         XC    BUFFER,BUFFER                                                    
         SR    R5,R5                                                            
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         MVI   TRACEONE,C'N'                                                    
         GOTO1 =A(EZLINXMT),(RC)                                                
         BNE   SNDEZ240                                                         
         MVI   BUFFER,C'*'                                                      
         MVC   BUFFER+1(49),BUFFER FILL BUFFER WITH ASTERISKS                   
         LA    R5,50                                                            
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         MVI   TRACEONE,C'Y'                                                    
         GOTO1 =A(EZLINXMT),(RC)                                                
         BNE   SNDEZ240                                                         
         XC    BUFFER,BUFFER                                                    
         SR    R5,R5                                                            
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         MVI   TRACEONE,C'N'                                                    
         GOTO1 =A(EZLINXMT),(RC)                                                
         BNE   SNDEZ240                                                         
*                                                                               
SNDEZ140 ICM   R0,15,COVPGLNS                                                   
         BZ    SNDEZ160                                                         
         L     R2,=A(COVERBUF)                                                  
SNDEZ150 XC    BUFFER,BUFFER                                                    
         MVC   BUFFER(132),0(R2)                                                
         LA    R5,132                                                           
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         MVI   TRACEONE,C'N'                                                    
         GOTO1 =A(EZLINXMT),(RC)                                                
         BNE   SNDEZ240                                                         
         LA    R2,132(R2)                                                       
         BCT   R0,SNDEZ150                                                      
*                                                                               
         LH    RF,NUMPAGES         INCREMENT PAGE COUNTER                       
         LA    RF,1(RF)                                                         
         STH   RF,NUMPAGES                                                      
*                                                                               
         XC    BUFFER,BUFFER                                                    
         MVC   BUFFER(5),=C'/PAGE'                                              
         LA    R5,5                                                             
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         MVI   TRACEONE,C'N'                                                    
         GOTO1 =A(EZLINXMT),(RC)                                                
         BNE   SNDEZ240                                                         
*                                                                               
SNDEZ160 LA    R1,DMCB                                                          
         USING PQPARMD,R1                                                       
         MVC   ATBLNTRY,=A(TABLNTRY)                                            
         MVC   APQBUFF,=A(CXREC)                                                
         MVC   APQHDR,=A(PQRPTHDR)                                              
         MVC   AEDHDR,=A(EDICTHDR)                                              
         MVC   APQLINE,=A(R)                                                    
         MVC   ACITABLE,SCITABLE                                                
         MVC   ADTAMGR,DATAMGR                                                  
         GOTO1 =V(PQGETLIN)                                                     
         BNE   SNDEZ230            NO MORE REPORT LINES                         
         DROP  R1                                                               
*                                                                               
         CLC   =C'++DDS',R+1       SHOULD THIS LINE BE TRANSMITTED?             
         BE    SNDEZ160            NO -- TRANSMIT REMAINDER OF REPORT           
*                                                                               
         CLI   EDICTHDR+36,C'P'    REPLACE X'89'S WITH /PAGE?                   
         BNE   SNDEZ180                                                         
         CLI   R,X'89'             PAGE EJECT?                                  
         BNE   *+14                                                             
         CLC   R+1(L'R-1),MYSPACES                                              
         BE    *+12                                                             
         MVI   LASTEJCT,C'N'                                                    
         B     SNDEZ180            THIS IS NOT A PAGE EJECT                     
*                                                                               
         CLI   LASTEJCT,C'Y'                                                    
         BE    SNDEZ160                                                         
         LH    RF,NUMPAGES         INCREMENT PAGE COUNTER                       
         LA    RF,1(RF)                                                         
         STH   RF,NUMPAGES                                                      
         SR    RE,RE               PREPARE FOR DIVIDE                           
         D     RE,WARNPAGE                                                      
         LTR   RE,RE               WARN CONSOLE EVERY N PAGES                   
         BNZ   SNDEZ170                                                         
*                                                                               
         MVC   PAGEMSG+36(8),DSTNAME   PQ USERID                                
         LA    RE,TABLNTRY                                                      
         USING XMTTABLD,RE                                                      
         MVC   PAGEMSG+45(3),XMTSUBID  PQ SUBID                                 
         EDIT  XMTREFNO,(5,PAGEMSG+49),ALIGN=LEFT                               
         DROP  RE                                                               
         EDIT  NUMPAGES,(4,PAGEMSG+73),ALIGN=LEFT                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'PAGEMSG,PAGEMSG)                   
*                                                                               
SNDEZ170 MVI   LASTEJCT,C'Y'       LAST LINE WAS A /PAGE                        
         MVI   R,X'09'                                                          
         MVC   R+1(5),=C'/PAGE'                                                 
*                                                                               
SNDEZ180 LA    R4,79               FIND END OF TEXT (MAX 80 CHARACTERS)         
         CLI   EDICTHDR+35,C'W'    DO WE WANT A WIDE TRANSMISSION?              
         BNE   *+8                                                              
         LA    R4,131              YES (MAX 132 CHARACTERS)                     
         LA    RF,R+1(R4)                                                       
         CLI   0(RF),C' '                                                       
         BNE   *+12                FOUND IT                                     
         BCT   R4,*-12                                                          
         B     *+8                                                              
         LA    R4,1(R4)                                                         
*                                                                               
         XC    BUFFER,BUFFER                                                    
         LTR   R4,R4               ANY REAL DATA TO SEND?                       
         BZ    SNDEZ190            NO -- JUST SEND CRLF                         
         BCTR  R4,0                                                             
         EX    R4,*+8              MOVE IN LINE                                 
         B     *+10                                                             
         MVC   BUFFER(0),R+1                                                    
         LA    R4,1(R4)            RESTORE LENGTH OF DATA                       
*                                                                               
SNDEZ190 LR    R5,R4                                                            
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         MVI   TRACEONE,C'N'                                                    
         GOTO1 =A(EZLINXMT),(RC)                                                
         BNE   SNDEZ240                                                         
*                                                                               
         CLI   R,X'19'             TRIPLE SPACE IF NECESSARY                    
         BNE   SNDEZ200                                                         
         XC    BUFFER,BUFFER                                                    
         MVC   BUFFER(2),=X'0D25'                                               
         LA    R5,2                                                             
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         MVI   TRACEONE,C'N'                                                    
         GOTO1 =A(EZLINXMT),(RC)                                                
         BNE   SNDEZ240                                                         
         B     SNDEZ210                                                         
*                                                                               
SNDEZ200 CLI   R,X'11'             DOUBLE SPACE IF NECESSARY                    
         BNE   SNDEZ210                                                         
         XC    BUFFER,BUFFER                                                    
         SR    R5,R5                                                            
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         MVI   TRACEONE,C'N'                                                    
         GOTO1 =A(EZLINXMT),(RC)                                                
         BNE   SNDEZ240                                                         
*                                                                               
SNDEZ210 LA    R1,DMCB                                                          
         USING PQPARMD,R1                                                       
         MVC   ATBLNTRY,=A(TABLNTRY)                                            
         MVC   APQBUFF,=A(CXREC)                                                
         MVC   APQHDR,=A(PQRPTHDR)                                              
         MVC   AEDHDR,=A(EDICTHDR)                                              
         MVC   APQLINE,=A(R)                                                    
         MVC   ACITABLE,SCITABLE                                                
         MVC   ADTAMGR,DATAMGR                                                  
         GOTO1 =V(PQGETLIN)                                                     
         BNE   SNDEZ230            NO MORE REPORT LINES                         
         DROP  R1                                                               
*                                                                               
         CLI   EDICTHDR+36,C'P'    REPLACE X'89'S WITH /PAGE?                   
         BNE   SNDEZ180                                                         
         CLI   R,X'89'             PAGE EJECT?                                  
         BNE   *+14                                                             
         CLC   R+1(L'R-1),MYSPACES                                              
         BE    *+12                                                             
         MVI   LASTEJCT,C'N'                                                    
         B     SNDEZ180            THIS IS NOT A PAGE EJECT                     
*                                                                               
         CLI   LASTEJCT,C'Y'                                                    
         BE    SNDEZ210                                                         
         LH    RF,NUMPAGES         INCREMENT PAGE COUNTER                       
         LA    RF,1(RF)                                                         
         STH   RF,NUMPAGES                                                      
         SR    RE,RE               PREPARE FOR DIVIDE                           
         D     RE,WARNPAGE                                                      
         LTR   RE,RE               WARN CONSOLE EVERY N PAGES                   
         BNZ   SNDEZ220                                                         
*                                                                               
         MVC   PAGEMSG+36(8),DSTNAME   PQ USERID                                
         LA    RE,TABLNTRY                                                      
         USING XMTTABLD,RE                                                      
         MVC   PAGEMSG+45(3),XMTSUBID  PQ SUBID                                 
         EDIT  XMTREFNO,(5,PAGEMSG+49),ALIGN=LEFT                               
         DROP  RE                                                               
         EDIT  NUMPAGES,(4,PAGEMSG+73),ALIGN=LEFT                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'PAGEMSG,PAGEMSG)                   
*                                                                               
SNDEZ220 MVI   LASTEJCT,C'Y'       LAST LINE WAS A /PAGE                        
         MVI   R,X'09'                                                          
         MVC   R+1(5),=C'/PAGE'                                                 
         B     SNDEZ180                                                         
*                                                                               
SNDEZ230 XC    BUFFER,BUFFER                                                    
         MVC   BUFFER(4),=C'MMMM'                                               
         LA    R5,4                                                             
         MVC   SEND_TYPE,ATB_SEND_AND_CONFIRM                                   
         MVI   TRACEONE,C'Y'                                                    
         MVI   ENDOFMSG,C'Y'       MESSAGE IS COMPLETE                          
         GOTO1 =A(EZLINXMT),(RC)                                                
         BE    SNDEZ250                                                         
*                                                                               
SNDEZ240 MVC   P+30(18),=C'BAD EASYLINK WRITE'                                  
         B     CANTSEND            SKIP IT                                      
*                                                                               
SNDEZ250 PRNT  SENTVIAEASYLINK,PRINT=ALWAYS                                     
*                                                                               
         EDIT  NUMLINES,(6,RATEMSG)  NUMBER OF TRANSMITTED LINES                
         TIME  BIN                                                              
         S     R0,STRTTIME         COMPUTE ELAPSED TIME                         
         BNM   *+8                                                              
         A     R0,=A(24*3600*100)  ADD 24HRS, JUST WENT PAST MIDNIGHT           
*                                                                               
         SRDL  R0,32               CONVERT SEC*100 TO SEC                       
         D     R0,=F'100'          LEAVES SECONDS IN R1                         
         LTR   R1,R1               IF R1 = 0, THEN IT TOOK LT 1 SECOND          
         BNZ   *+8                                                              
         LA    R1,1                ROUND UP TO 1 SECOND                         
         LR    RF,R1               SAVE ELAPSED TIME IN SECONDS                 
         SR    R0,R0               PREPARE FOR DIVIDE                           
         D     R0,=F'3600'         GIVES HOURS IN R1                            
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RATEMSG+16(2),DUB                                                
         MVI   RATEMSG+18,C':'                                                  
         SRDL  R0,32                                                            
         D     R0,=F'60'           GIVES MINUTES IN R1/SECS IN R0               
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RATEMSG+19(2),DUB                                                
         MVI   RATEMSG+21,C':'                                                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RATEMSG+22(2),DUB                                                
*                                                                               
         SR    R0,R0               PREPARE FOR DIVIDE                           
         L     R1,NUMLINES                                                      
         DR    R0,RF               R1 = LINES PER SECOND                        
         EDIT  (R1),(6,RATEMSG+33)                                              
         MVC   P+30(49),RATEMSG                                                 
         PRNT  RATE_STATS,PRINT=ALWAYS                                          
*                                                                               
* MARK ALL DESTINATION SENT                                                     
*                                                                               
         LA    R2,CTABNTRY            THIS IS THE LAST DEST ENTRY               
         USING XMTTABLD,R2                                                      
         ZICM  R4,XMTDSTNO,2          LAST DEST #                               
*                                                                               
SNDEZ260 STCM  R4,3,XMTDSTNO                                                    
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,SXMTTBLE                                                
         MVC   ETBLNTRY,=A(CTABNTRY)                                            
         MVI   EACTION,EACTSNTQ    MARK REPORT SENT                             
         MVC   EMAJORNM,SMAJORNM                                                
         MVC   EADTAMGR,DATAMGR                                                 
         MVC   EAEZDTA,=A(EASYDATA)                                             
         MVC   ENUMPAGE,NUMPAGES                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   SNDEZ270                                                         
         DROP  R1,R2                                                            
*                                                                               
         MVC   P+30(16),=C'DESTINATION NO ='                                    
         EDIT  (R4),(5,P+46),ALIGN=LEFT,ZERO=NOBLANK                            
         PRNT  RPTMARKEDSENT,PRINT=ALWAYS                                       
*                                                                               
SNDEZ270 BCT   R4,SNDEZ260                                                      
*                                                                               
SNDEZXIT XIT1                                                                   
         SPACE 2                                                                
CANTSEND PRNT  CANTTRANSMIT,PRINT=ALWAYS                                        
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'EDICT01 *ERROR SENDING TO +        
               EASYLINK -- TRANSMISSIONS SUSPENDED'                             
         ABEND 701,DUMP                                                         
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
ERRDFAX  NTR1  BASE=*                                                           
         L     R7,=A(EDICTREC)                                                  
         USING EDFILD,R7                                                        
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
         MVC   RDFXFRID,MYSPACES                                                
         MVC   RDFXFRID(4),EDFDEST FAX DESTINATION                              
         MVC   RDFXTOID,MYSPACES                                                
         MVC   RDFXTOID(L'DSTNAME),DSTNAME    ORIGINATING AGENCY USERID         
         MVC   RDFXRTRN,EDFAPPL+31 'RETURN TO SENDER' DATA                      
         DROP  R7                                                               
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
         MVC   RDFXERR,=CL3'501'   NO FAX RECORD ERROR                          
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
EZLINXMT NMOD1 0,EZLINXMT                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* ON ENTRY, R5 = LENGTH OF DATA                                                 
*                                                                               
         L     RF,NUMLINES                                                      
         LA    RF,1(RF)            INCREMENT LINE COUNTER                       
         ST    RF,NUMLINES                                                      
*                                                                               
         CLI   ENDOFMSG,C'Y'       IS MESSAGE COMPLETE?                         
         BE    EZLINX5             YES -- OK TO SEND MMMM                       
         CLC   =C'MMMM',BUFFER                                                  
         BNE   EZLINX5                                                          
         CH    R5,=H'4'                                                         
         BNE   EZLINX5                                                          
         PRNT  PREMATURE_MMMM,PRINT=ALWAYS                                      
         B     EZLINXOK            JUST IGNORE THE RECORD                       
*                                                                               
EZLINX5  LR    R2,R5                                                            
         LA    R2,4(R2)            L'DATA + 2 BYTES FOR CRLF + LENGTH           
         STH   R2,BUFFER_LENGTH                                                 
         ST    R2,SEND_LENGTH                                                   
         LA    R1,BUFFER(R5)                                                    
         MVC   0(2,R1),=X'0D25'    TERMINATE RECORD WITH CRLF                   
*                                                                               
         CLI   TRACEONE,C'Y'                                                    
         BE    *+12                FORCE PRINTING OF LINE                       
         CLI   TRACEFLG,C'Y'                                                    
         BNE   EZLINX20                                                         
*                                                                               
         LA    RE,=C'BUFFER DATA'                                               
         ST    RE,DMCB                                                          
         MVI   DMCB,11                                                          
         CLC   SEND_TYPE,ATB_BUFFER_DATA                                        
         BE    EZLINX10                                                         
         LA    RE,=C'SEND AND CONFIRM'                                          
         ST    RE,DMCB                                                          
         MVI   DMCB,16                                                          
         CLC   SEND_TYPE,ATB_SEND_AND_CONFIRM                                   
         BE    EZLINX10                                                         
         DC    H'0'                UNKNOWN SEND TYPE                            
EZLINX10 GOTO1 =V(PRNTBL),DMCB,,BUFFER_LENGTH,C'DUMP',(R2),=C'1D'               
*                                                                               
EZLINX20 L     R2,=A(ATBSEND_STRUCTURE)                                         
         GOTO1 =A(CALLAPPC),(RC)                                                
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    EZLINXOK            RETURN CODE WAS OK                           
*                                                                               
         LTR   RB,RB               NO GOOD -- SET CC NOT EQUAL                  
         B     EZLINXX                                                          
*                                                                               
EZLINXOK CR    RB,RB               SET CC EQUAL                                 
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
         CLI   DOCOMMS,C'Y'        ARE WE DOING COMMUNICATIONS?                 
         BNE   PRATTBX             NO                                           
*                                                                               
         PRNT  *************,PRINT=ALWAYS                                       
         PRNT  GETATTRIBUTES,PRINT=ALWAYS                                       
         PRNT  *************,PRINT=ALWAYS                                       
*                                                                               
         L     R2,=A(ATBGTA2_STRUCTURE)                                         
         GOTO1 =A(CALLAPPC),(RC)                                                
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
PRATTBX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
CALLAPPC NMOD1 0,CALLAPPC                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* UPON ENTRY,  R2 POINTS TO PARAMETER STRUCTURE                                 
* UPON RETURN, RETURN_CODE CONTAINS THE APPC/MVS RETURN CODE                    
*                                                                               
         MVC   RETURN_CODE,ATB_OK  IN CASE WE DON'T DO COMMS                    
*                                                                               
         CLI   DOCOMMS,C'Y'        ARE WE DOING COMMUNICATIONS?                 
         BNE   CALLX               NO, SO NO APPC/MVS CALLS                     
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
         B     CALL40              APPC/MVS CALL NOT ACCEPTED                   
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
* PERFORM APPC EXTRACT_ERROR FUNCTION.                                          
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         L     R2,=A(ATBEES3_STRUCTURE)                                         
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
DAREFAX  NMOD1 0,DAREFAX*                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         MVI   DESTTYPE,EDFDSTXQ   FAX RECORD KEY                               
         XC    WORK,WORK           RECEIVING ID WILL BE BUILT HERE              
*                                                                               
         MVI   MEDIA,C'T'          DEFAULT MEDIA IS TV                          
*                                                                               
         CLI   DESTINAT+4,C'T'     T MEANS TV                                   
         BE    DAREFX10                                                         
*                                                                               
         CLI   DESTINAT+4,C' '     SO DOES A BLANK                              
         BE    DAREFX10                                                         
*                                                                               
         CLI   DESTINAT+4,C'A'     AM RADIO                                     
         BNE   *+12                                                             
         MVI   MEDIA,C'R'                                                       
         B     DAREFX10                                                         
*                                                                               
         CLI   DESTINAT+4,C'F'     FM RADIO                                     
         BNE   *+8                                                              
         MVI   MEDIA,C'R'                                                       
*                                                                               
DAREFX10 XC    KEY,KEY             BUILD DARE STATION RECORD KEY                
         LA    R4,KEY                                                           
         USING STAKEYD,R4                                                       
         MVI   STAKSYS,STAKSYSQ                                                 
         MVI   STAKTYP,STAKTYPQ                                                 
         MVC   STAKMEDA,MEDIA                                                   
         MVC   STAKSTIN,DESTINAT                                                
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'GENDIR',KEY,KEY                   
         TM    DMCB+8,X'FF'-X'10'                                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         TM    DMCB+8,X'10'                                                     
         BO    DAREFXX             NO DSTATION REC (SHOULDN'T HAPPEN)           
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'GENFIL',KEY+36,A(IO),    +        
               DMWORK                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   DATADISP,=H'42'     FOR GENFIL                                   
         L     R4,=A(IO)                                                        
         MVI   ELCODE,STAHOMCQ     HOME MARKET ELEM?                            
         BAS   RE,GETEL                                                         
         BNE   DAREFX15            NO HOME MKT ELEM, FIND THE REP               
         USING STAHOMD,R4                                                       
         CLC   STAHOMCT,ROUTCODE+3 MATCHES OUR CITY?                            
         BNE   DAREFX15            NO                                           
         OC    STAHOMIB,STAHOMIB   HAVE A CITY BUT NO RECEIVING ID?             
         BNZ   DAREFX15              STRANGE...USE DEFAULT INSTEAD              
         MVC   WORK(L'DESTINAT),DESTINAT  USE STATION AS RECEIVER               
         MVI   DESTTYPE,EDFDSTHQ   HOME MARKET W/O ID                           
         B     DAREFXX                                                          
*                                                                               
DAREFX15 L     R4,=A(IO)                                                        
         MVI   ELCODE,STAREPCQ     REP ELEMENT                                  
         BAS   RE,GETEL                                                         
         BNE   DAREFXX             NO REP ELEMENT (SHOULDN'T HAPPEN)            
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
DAREFX20 CLI   0(R3),X'FF'                                                      
         BE    DAREFXX             MISSING ENTRY -- SHOULD NEVER HAPPEN         
         CLC   REPCODE,0(R3)                                                    
         BE    *+12                GOT IT                                       
         LA    R3,L'REPIDS(R3)     SKIP TO NEXT ENTRY                           
         B     DAREFX20                                                         
*                                                                               
         MVC   WORK,SPACES                                                      
         ZIC   R4,14(R3)           CONSTRUCT RECEIVING ID IN WORK               
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),15(R3)      REP-ID                                       
         TM    13(R3),X'80'        APPEND OFFICE CODE?                          
         BO    DAREFXX             NO                                           
*                                                                               
         LA    R3,WORK+1(R4)       POINT BEYOND REP-ID                          
         MVC   0(2,R3),ROUTCODE+3  OFFICE                                       
*                                                                               
         XC    KEY,KEY             BUILD DARE STATION RECORD KEY                
         LA    R4,KEY                                                           
         USING AGRKEYD,R4                                                       
         MVI   AGRKSYS,AGRKSYSQ                                                 
         MVI   AGRKTYP,AGRKTYPQ                                                 
         MVC   AGRKMEDA,MEDIA                                                   
         MVC   AGRKAGRT,ROUTCODE                                                
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'GENDIR',KEY,KEY                   
         TM    DMCB+8,X'FF'-X'10'                                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         TM    DMCB+8,X'10'                                                     
         BO    DAREFXX             NO OFFICE EXCEPTION RECORD                   
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'GENFIL',KEY+36,A(IO),    +        
               DMWORK                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,=A(IO)                                                        
         MVI   ELCODE,AGROVRCQ     OVERRIDE ELEMENT CODE                        
         BAS   RE,GETEL                                                         
         BNE   DAREFXX                                                          
*                                                                               
         USING AGROVRD,R4                                                       
DAREFX30 CLC   REPCODE,AGROVRCR    IS THERE AN OVERRIDE FOR THIS REP?           
         BE    *+16                YES                                          
         BAS   RE,NEXTEL                                                        
         BE    DAREFX30                                                         
         B     DAREFXX             NO                                           
*                                                                               
         MVC   0(2,R3),AGROVROF    PUT OFFICE OVERRIDE IN RECEIVING ID          
         DROP  R4                                                               
*                                                                               
DAREFXX  XIT1                                                                   
         EJECT                                                                  
COVERPAG NMOD1 0,COVERPAG                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* BUILD A COVER PAGE FROM A FAX RECORD.                                         
*                                                                               
         L     R2,=A(COVERBUF)     BUILD COVER PAGE HERE                        
         LR    RF,R2                                                            
         LA    R0,COVPGMAX         MAXIMUM NUMBER OF LINES                      
COVRPG00 MVC   0(132,RF),MYSPACES  CLEAR TABLE                                  
         LA    RF,132(RF)                                                       
         BCT   R0,COVRPG00                                                      
*                                                                               
         MVC   DATADISP,=H'28'     FOR CTFILE                                   
*                                                                               
         CLC   FAXKEY(4),DESTINAT  READING FAX RECORD FOR THE STATION?          
         BNE   COVRPG03            NO, BETTER NOT RUIN FAX KEY FOR REP          
         CLI   DESTINAT+4,C'T'     MEDIA T?                                     
         BNE   COVRPG03            **SHOULD CHECK RADIO AND OTHER LATER         
         MVC   FAXKEY(4),DESTINAT  USE STATION CALL LETTERS (4)                 
         MVC   FAXKEY+4(3),SPACES                                               
*                                                                               
COVRPG03 CLC   =C'NOREP',FAXKEY                                                 
         BNE   COVRPG05                                                         
         CLI   DESTINAT+4,C'T'     MEDIA T?                                     
         BNE   COVRPG05            **SHOULD CHECK RADIO AND OTHER LATER         
         MVC   AGYPOWER,SVAGYPOW   USE AGENCY POWER CODE, NOT "DARADM"          
*                                                                               
COVRPG05 XC    DESTINAT,DESTINAT   FAX NUMBER WILL GO HERE                      
*                                                                               
         XC    KEY,KEY             READ FAX RECORD                              
         LA    R4,KEY                                                           
         USING CTFXREC,R4                                                       
         MVI   CTFXKTYP,CTFXEQU                                                 
         MVC   CTFXAGY,AGYPOWER                                                 
         MVC   CTFXCODE,FAXKEY                                                  
*                                                                               
         L     R1,=A(EDICTHDR)                                                  
         CLI   68(R1),C'D'         IS THIS A DARE FAX?                          
         BNE   COVRPG07                                                         
         OC    FAXSUBC(3),FAXSUBC  1ST SUB CODE IS THERE?                       
         BZ    COVRPG09            NO - TRY 2ND SUB CODE                        
         MVC   CTFXSUBC,SPACES                                                  
         MVC   CTFXSUBC(3),FAXSUBC                                              
         DROP  R4                                                               
*                                                                               
COVRPG07 MVC   P+30(25),KEY                                                     
         PRNT  DAREFAX_KEY                                                      
*                                                                               
         LA    R7,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R7)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(7),E,6)  ENQUEUE THE CONTROL FILE                     
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,A(IO),0               
*                                                                               
         LA    R7,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R7)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(7),6)    DEQUEUE THE CONTROL FILE                     
*                                                                               
         L     RF,=A(IO)                                                        
         MVC   P+30(25),0(RF)                                                   
         PRNT  DAREFAX_IO                                                       
*                                                                               
         L     R1,=A(EDICTHDR)                                                  
         CLI   68(R1),C'D'         IS THIS A DARE FAX?                          
         BNE   COVRPG12                                                         
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
         LA    R7,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R7)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(7),E,6)  ENQUEUE THE CONTROL FILE                     
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,A(IO),0               
*                                                                               
         LA    R7,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R7)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(7),6)    DEQUEUE THE CONTROL FILE                     
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
         LA    R7,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R7)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(7),E,6)  ENQUEUE THE CONTROL FILE                     
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,A(IO),0               
*                                                                               
         LA    R7,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R7)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(7),6)    DEQUEUE THE CONTROL FILE                     
*                                                                               
         L     RF,=A(IO)                                                        
         MVC   P+30(25),0(RF)                                                   
         PRNT  DAREFAX_IO                                                       
*                                                                               
COVRPG12 TM    DMCB+8,X'FF'-X'10'                                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         TM    DMCB+8,X'10'                                                     
         BO    COVRPGX             NO FOUND                                     
*                                                                               
COVRPG14 CLI   DESTTYPE,EDFDSTHQ   FAX NUMBER (HOME MARKET W/O ID)?             
         BNE   COVRPG18                                                         
         MVI   DESTTYPE,EDFDSTXQ   YES, RESET TO FAX RECORD KEY                 
*        B     COVRPG18                                                         
*                                                                               
*COVRPG15 CLI   DESTTYPE,EDFDSTHQ   FAX NUMBER (HOME MARKET W/O ID)?            
*         BNE   COVRPG90            FAX RECORD NOT FOUND                        
*         MVC   DESTINAT(L'FAXKEY),FAXKEY                                       
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
         MVC   DESTINAT(4),=C'FAX '                                             
         LA    RE,DESTINAT+4                                                    
         CLI   LONGFXPG,C'Y'       GENERATE "LONG" FAX PAGES?                   
         BNE   *+14                                                             
         MVC   DESTINAT(5),=C'FAXL '                                            
         LA    RE,DESTINAT+5                                                    
*                                                                               
         ZIC   R1,CTFX1LEN         FAX NUMBER                                   
         SH    R1,=H'3'            2 FOR OVERHEAD, ONE FOR EX                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),CTFX1NUM    MOVE IN FAX NUMBER                           
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   38(0,R2),CTFX1NUM                                                
         LA    R1,38(R2)                                                        
         BAS   RE,EDITTEL                                                       
*                                                                               
COVRPG25 LA    R2,132(R2)                                                       
         LA    R2,132(R2)                                                       
         LR    R5,R2                                                            
         DROP  R4                                                               
*                                                                               
         L     R1,=A(EDICTHDR)                                                  
         CLI   71(R1),C'N'         NO COVER PAGE?                               
         BNE   *+14                                                             
         XC    COVPGLNS,COVPGLNS                                                
         B     COVRPGX                                                          
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
         LR    R2,R5                                                            
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
         MVC   CTFXCODE+5(2),MYSPACES                                           
*                                                                               
         LA    R7,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R7)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(7),E,6)  ENQUEUE THE CONTROL FILE                     
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,A(IO),0               
*                                                                               
         LA    R7,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R7)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(7),6)    DEQUEUE THE CONTROL FILE                     
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
         LR    R5,R2                                                            
*                                                                               
         MVI   ELCODE,CTFX3ELQ     MESSAGE ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   COVRPG80                                                         
         USING CTFXMSG,R4                                                       
*                                                                               
COVRPG70 ZIC   R3,CTFX3LIN                                                      
         BCTR  R3,0                                                             
         MH    R3,=H'132'                                                       
         LR    R2,R5                                                            
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
         BZ    COVRPGX             COVER PAGE IS EMPTY                          
         SRDL  R2,32               PREPARE FOR DIVIDE                           
         D     R2,=F'132'                                                       
         LA    R3,1(R3)            R3 = NUMBER OF LINES IN COVER PAGE           
         ST    R3,COVPGLNS                                                      
*                                                                               
COVRPGX  XIT1                                                                   
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                       EDIT TELEPHONE NUMBER                         *         
*        PUT IN () & -                                                *         
*        R1=A(CL25 TELEPHONE)                                         *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
EDITTEL  NTR1                                                                   
*                                                                               
         MVC   WORK(25),0(R1)                                                   
         MVC   0(25,R1),MYSPACES                                                
         LA    R2,WORK                                                          
         CLC   WORK+7(4),MYSPACES  IF > 7 CHARACTERS                            
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
RECEIVER_TP_NAME               DC    CL64' '                                    
CONVERSATION_CORRELATOR        DS    CL8                                        
LUW_ID                         DS    XL26                                       
RETURN_CONTROL                 DS    F                                          
SYNC_LEVEL                     DS    F                                          
SECURITY_TYPE                  DS    F                                          
USER_ID                        DC    CL10' '                                    
PASSWORD                       DC    CL10' '                                    
PROFILE                        DC    CL10' '                                    
USER_TOKEN                     DC    XL80'00'                                   
CONVERSATION_ID                DS    CL8                                        
NOTIFY_TYPE                    DS    F                                          
                               DC    A(APPCECB) MUST FOLLOW NOTIFY_TYPE         
TPID                           DC    XL8'00'                                    
RETURN_CODE                    DS    F                                          
CONVERSATION_STATE             DS    F                                          
DEALLOCATE_TYPE                DS    F                                          
REQUEST_TO_SEND_VALUE          DS    F                                          
REQUEST_TO_SEND_RECEIVED       DS    F                                          
SEND_TYPE                      DS    F                                          
SEND_LENGTH                    DS    F                                          
SEND_ACCESS_TOKEN              DC    F'0'                                       
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
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
MYSPACES DC    CL256' '                                                         
PAGEMSG  DC    C'EDICT01 *EASYLINK WARNING: LONG FAX XXXXXXXX,XXX,XXXXX+        
               , NOW SENDING PAGE NNNN'                                         
RATEMSG  DC    C'NNNNNN LINES IN HH:MM:SS, RATE = NNNNNN LINES/SEC'             
         SPACE 2                                                                
         EJECT                                                                  
DMWORK   DS    12D                                                              
DMCB     DS    20F                                                              
DUB      DS    D                                                                
DUB2     DS    D                                                                
PRNTDUB  DS    D                                                                
DATAMGR  DS    A                   A(DATAMGR)                                   
FULL     DS    F                                                                
PACKOF4B DS    PL4                                                              
ADSTAEFT DS    A                   ADDRESS OF DST+A(EDIFIL ENTRY) TABLE         
NUMDESTS DS    H                   NUMBER OF DEST FOR A LOGICAL REPORT          
LSTSDST  DS    H                   LAST SENDABLE DEST # IN A LOG REP            
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
DATADISP DS    H                                                                
PRNTTIME DS    CL9                                                              
WORK     DS    CL256                                                            
ELCODE   DS    X                                                                
OPERSTOP DC    C'N'                'Y' IF OPERATOR WANTS TO STOP                
TRACEFLG DS    C                   'Y' TO PRINT DETAILED TRACE                  
TRACEONE DS    C                   'Y' TO FORCE TRACE OF THIS LINE ONLY         
DOCOMMS  DC    C'Y'                'Y' TO PERFORM COMMUNICATIONS                
ATCHRCVR DC    C'Y'                'Y' TO ATTACH RECEIVER FROM EASYLINK         
DONTSEND DC    C'N'                'Y' TO PREVENT SENDING TO EASYLINK           
SENDDLNS DC    C'Y'                'Y' IF WE WANT DLNS FROM EASYLINK            
ENDOFMSG DS    C                   'Y' WHEN MESSAGE IS COMPLETE                 
MAXURPTS DC    F'50'               MAX REPORTS PER USERID IN A ROW              
COVPGLNS DS    F                   NUMBER OF LINES IN COVER PAGE                
STRTTIME DS    F                   TIME WE STARTED SENDING A REPORT             
NUMLINES DS    F                   NUMBER OF TRANSMITTED LINES/REPORT           
WARNPAGE DC    F'20'               WARN CONSOLE EVERY N PAGES                   
WAITSECS DC    F'12000'            2 MINS. FOR APPC CALLS TO COMPLETE           
NUMPAGES DS    H                   PAGE COUNTER FOR FAX TRANSMISSION            
USERFILT DC    H'0'                ONLY XMIT REPORTS FOR THIS USERID            
LSTPQKEY DS    XL7                 LAST PRINT QUEUE REPORT SENT                 
SKIPLAST DS    C                   'Y' IF WE MAXED OUT ON A USERID              
LONGFXPG DS    C                   'Y' IF WE SHOULD SEND 'FAXL' COMMAND         
         SPACE 2                                                                
ECBLST   DS    0F                                                               
ALOOKECB DC    X'00',AL3(0)        A(LOOKECB)                                   
ASTOPECB DC    X'80',AL3(0)        A(STOPECB)                                   
         SPACE 2                                                                
ECBLST2  DS    0F                                                               
ASTPECB2 DC    X'00',AL3(0)        A(STOPECB)                                   
AAPPCECB DC    X'80',AL3(APPCECB)  A(APPCECB)                                   
         SPACE 2                                                                
EZRPARMS DC    XL(EZRPRMLQ)'00'    PARAMETERS TO EDIEZR (VIA R1)                
EZRTCB   DS    F                   TCB OF ATTACHED SUBTASK EDIEZR               
EZRECB   DC    F'0'                ECB OF ATTACHED SUBTASK EDIEZR               
*                                                                               
SUBTASK  DC    C'EDIEZR  '         RECEIVING SUBTASK NAME                       
EZLUID   DS    CL8                 LUID OF AT&T EASYLINK                        
EZRCLUID DS    CL8                 LUID OF RECEIVER FROM EASYLINK               
APPCECB  DC    F'0'                ECB FOR APPC/MVS CALLS                       
         SPACE 2                                                                
TABLNTRY DS    XL(XMTTBLQ)         SAVED XMIT TABLE ENTRY                       
CTABNTRY DS    XL(XMTTBLQ)         COPIED XMIT TABLE ENTRY                      
XMTENTRY DS    XL(XMTTBLQ)         TEMP STORAGE FOR XMIT TABLE ENTRY            
MAJORNAM DS    CL8                 MAJOR RESOURCE NAME                          
USERID   DS    CL10                ALPHA USERID                                 
AGYPOWER DS    CL2                 AGENCY POWER CODE                            
SVAGYPOW DS    CL2                 SAVED AGENCY POWER CODE                      
AGYNAME  DS    CL33                                                             
AGYADDR  DS    CL33                                                             
REQUESTR DS    CL3                                                              
ROUTCODE DS    CL5                 DARE ROUTING CODE                            
MEDIA    DS    C                   DARE MEDIA                                   
REPCODE  DS    CL3                 DARE REP CODE                                
CUSTREF  DS    CL18                VEDDNNNNNNYYYYMMDD                           
DESTINAT DS    CL25                                                             
MAILBOX  DS    CL6                 EASYLINK MAILBOX NUMBER                      
DSTNAME  DS    CL8                 DESTINATION NAME                             
DSTUIDN  DS    XL2                 USERID                                       
FAXKEY   DS    CL7                 KEY OF FAX RECORD                            
FAXSUBC  DS    CL6                 FAX SUBCODE RECORD                           
DESTTYPE DS    C                   DESTINATION TYPE (FAX OR REDILIST)           
LASTEJCT DS    C                   'Y' = LAST LINE PRINTED WAS /PAGE            
EASYDATA DS    XL21                EASYLINK-SPECIFIC TRANSFER DATA              
KEY      DS    XL40                CTFILE/GENDIR KEY                            
CARD     DS    CL80                FOR CONTROL CARDS AND EDICTFIL RECS          
         DS    XL4                 PQ RECORD LENGTH                             
R        DS    CL256               PQ RECORD DATA                               
         DS    XL4                                                              
EDICTHDR DS    CL256               EDICT HDR* RECORD                            
         DS    XL4                                                              
PQRPTHDR DS    CL256               PQ REPORT HEADER                             
EDICTREC DS    CL256                                                            
NEWEDREC DS    CL256                                                            
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
ATBCFM_STRUCTURE          DS    A                                               
                          DC    CL16'REQUEST_CONFIRM'                           
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
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
ATBSEND  DC    CL8'ATBSEND'                                                     
         DC    XL52'00'                                                         
*                                                                               
ENTRYLSQ EQU   *                                                                
*                                                                               
*EASYLINK 3 CHAR RESERVED COMMAND LIST, YYUN, 7/30/03                           
EZRSVCMD DS    0CL3                                                             
         DC    C'CFM',C'ELN',C'ESL',C'FCH',C'CMG',C'ZIP',C'CLS',C'DLY'          
         DC    C'FON',C'FWD',C'FYI',C'ICS',C'PLS',C'FCR',C'INT',C'MBX'          
         DC    C'MMX',C'MGM',C'PMS',C'GRT',C'ITS',C'ITX',C'NFT',C'NTF'          
         DC    C'NTS',C'NTY',C'PRI',C'RCA',C'TLX',C'TRT',C'TWX',C'WUW'          
         DC    C'FAX',C'FCR',C'EDI',C'FXL',C'WUI',C'EXT',C'COL',C'COM'          
         DC    C'FTC',C'RPT',C'WWU',C'SVC',C'FLW',C'FWL',C'FXW'                 
         DC    XL3'00'                                                          
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDEDITSTID                                                     
         EJECT                                                                  
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
         EJECT                                                                  
DSTAEFTD DSECT                                                                  
DSTAEDST DS    CL(L'DESTINAT)      FAX DESTINATION                              
DSTAEADR DS    XL4                 A(EDICT FILE ENTRY) OF THIS DST              
DSTAEFTQ EQU   *-DSTAEFTD                                                       
         EJECT                                                                  
       ++INCLUDE DDEDICTWRK                                                     
         EJECT                                                                  
* DDDPRINT                                                                      
* DDEDICTFIL                                                                    
* SPDARDARED                                                                    
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DDEDICTFIL                                                     
         EJECT                                                                  
       ++INCLUDE SPDARDARED                                                     
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE CTGENSTAD                                                      
         EJECT                                                                  
       ++INCLUDE CTGENAGRD                                                      
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032DDEDIEZS  11/20/06'                                      
         END                                                                    
