*          DATA SET DDEDIFAX   AT LEVEL 080 AS OF 03/16/14                      
*PHASE EDIFAXA                                                                  
*INCLUDE ADDAY                                                                  
*INCLUDE BINSR31                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE EDISUB                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        EDIFAX - TRANSMIT PQ REPORTS VIA FAXGATE             *         
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
*                R6 -- APPC/MVS CONVERSATION CONTROL TABLE            *         
*                R7 -- WORK                                           *         
*                R8 -- 2ND PROGRAM BASE                               *         
*                R9 -- WORK                                           *         
*                RA -- CPRINT                                         *         
*                RB -- 1ST PROGRAM BASE                               *         
*                RC -- COMMON STORAGE AREA                            *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'DDEDIFAX -- FAXGATE TRANSMISSIONS VIA APPC/MVS'                 
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
EDIFAX   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*EDIFAX*,=A(R13CHAIN),R8                                       
*                                                                               
         LR    R9,RC                                                            
         L     RC,=A(COMMWORK)     COMMON STORAGE AREA                          
         USING COMMWORK,RC                                                      
*                                                                               
         SH    R9,=H'4'                                                         
         L     R9,0(R9)                                                         
         L     R9,0(R9)            A(R1 PARAMETERS FROM ATTACH)                 
         USING SUBPARMD,R9                                                      
*                                                                               
         USING APPCD,R6                                                         
*                                                                               
         MVC   TRACEFLG,STRACEON   'Y' = PRINT DETAILED TRACE                   
         ENTRY TRACEFLG                                                         
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         L     RF,=V(PRNTER)       A(SYSPRINT DCB)                              
         MVC   DCBDDNAM-IHADCB(8,RF),=C'FXTRACE '  DDNAME=FXTRACE               
*                                                                               
         MVC   TITLE(39),=C'EDICT: SEND/RECEIVE TO/FROM FAXGATE PCS'            
         PRNT  START_SUBTASK,PRINT=ALWAYS                                       
*                                                                               
         MVC   DATAMGR,SADTAMGR    A(DATAMGR)                                   
         MVC   CITABLE,SCITABLE    A(CITABLE)                                   
         MVC   STOPECB,SSTOPECB    A(ECB SAYING STOP)                           
         MVC   LOOKECB,SLOOKECB    A(ECB SAYING LOOK)                           
         MVC   XMTTABLE,SXMTTBLE   A(XMIT TABLE)                                
         MVC   DSTTABLE,SDSTTBLE   A(DESTINATION TABLE)                         
         MVC   AAPPCTAB,SAPPCTAB   A(APPC/MVS CONTROL TABLE)                    
         MVC   MAXDESTS,SMAXDSTS   MAX # DESTS/REPORT                           
         L     RF,SMAJORNM                                                      
         MVC   MAJORNAM,0(RF)      MAJOR RESOURCE NAME                          
         MVC   DRTEST,SDRTEST      DISASTER RECOVERY TEST MODE                  
         DROP  R9                                                               
*                                                                               
         GOTO1 =A(READCRDS),(RC)   READ PARAMETER CARDS                         
*                                                                               
         GOTO1 =A(INITIAL),(RC)    INITIALIZE                                   
*                                                                               
WAIT     PRNT  ABOUTTOWAIT,PRINT=ALWAYS                                         
*                                                                               
* SET (#) TASK CODE (NF) & TCB ADDR IN ISGENQ / CLEAR TASK ENQUEUES (K)         
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=CL7'ISGENQ'),C'#08K'                            
*                                                                               
         L     R3,AECBLIST                                                      
         WAIT  1,ECBLIST=(3)       WAIT FOR SOMETHING TO HAPPEN                 
*                                                                               
         L     RF,4(R3)            A(ECB) SAYING STOP                           
         TM    0(RF),X'40'         STOP?                                        
         BZ    DONTSTOP            NO                                           
*                                                                               
         XC    0(4,RF),0(RF)       CLEAR ECB                                    
         PRNT  STOPREQUESTED,PRINT=ALWAYS                                       
         MVI   OPERSTOP,C'Y'       OPERATOR WANTS TO STOP                       
*                                                                               
WINDDOWN L     R6,AAPPCTAB         APPC/MVS CONTROL TABLE                       
ALLFREE  TM    INTERNAL_CONVERSATION_STATUS,OPERATOR_ENABLED                    
         BZ    *+12                OPERATOR HAS DISABLED THIS PARTNER           
         TM    INTERNAL_CONVERSATION_STATUS,WAITING_FOR_CONFIRMED               
         BO    WAIT                NOT EVERYONE IS DONE TALKING YET             
         AH    R6,=Y(APPCDLEN)     BUMP TO NEXT CONVERSATION                    
         CLC   EOTMARK,0(R6)       END OF TABLE?                                
         BNE   ALLFREE                                                          
*                                                                               
         GOTO1 =A(DEALALL),(RC)    DEALLOCATE ALL CONVERSATIONS                 
*                                                                               
         PRNT  EXITING,PRINT=ALWAYS                                             
*                                                                               
* SET (#) TASK CODE (NF) & TCB ADDR IN ISGENQ / CLEAR TASK ENQUEUES (K)         
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=CL7'ISGENQ'),C'#08K'                            
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
*                                                                               
DONTSTOP LA    R3,8(R3)            POINT TO FIRST ECB ADDRESS                   
CHKAPPC  L     RF,0(R3)            RF NOW CONTAINS ECB                          
         TM    0(RF),X'40'         WAS THIS CONVERSATION POSTED?                
         BO    DOAPPC              YES                                          
         TM    0(R3),X'80'         NO - END OF ECBLIST?                         
         BO    CHKLOOK             YES                                          
         LA    R3,4(R3)            BUMP TO NEXT ECB ADDRESS                     
         B     CHKAPPC                                                          
*                                                                               
DOAPPC   LR    R6,RF               APPC CONTROL TABLE ENTRY ADDRESS             
         MVC   RETURN_CODE,APPC_ECB  RETURN CODE IS IN ECB                      
         MVI   RETURN_CODE,0       CLEAR HOB                                    
         XC    APPC_ECB,APPC_ECB   CLEAR ECB                                    
         ICM   RF,15,APPC_COMPLETION_ENTRY_POINT                                
         BNZR  RF                  PERFORM NEXT APPC/MVS FUNCTION               
*                                                                               
         CLI   CONVERSATION_FUNCTION,SENDER                                     
         BNE   *+16                                                             
         CLI   DONTSEND,C'Y'       PREVENT SENDING TO FAXGATE PC?               
         BE    WAIT                YES - NOTHING FOR SENDER TO DO               
         B     ALLOCATE            NO - DO FIRST SENDER FUNCTION                
*                                                                               
         CLI   CONVERSATION_FUNCTION,RECEIVER                                   
         BE    REG4ALLC            FIRST RECEIVER FUNCTION                      
         DC    H'0'                WHAT APPC FUNCTION IS NEXT?                  
*                                                                               
CHKLOOK  CLI   OPERSTOP,C'Y'       OPERATOR WANTS TO STOP?                      
         BE    WAIT                YES - DON'T BOTHER LOOKING FOR MORE          
*                                                                               
         L     RF,AECBLIST                                                      
         L     RF,0(RF)            A(ECB) SAYING LOOK                           
         TM    0(RF),X'40'         MAIN TASK POSTED READY?                      
         BZ    WAIT                NO                                           
*                                                                               
         XC    0(4,RF),0(RF)       CLEAR ECB                                    
*                                                                               
         CLI   DONTSEND,C'Y'       PREVENT SENDING TO FAXGATE PC?               
         BE    WAIT                YES -- NO NEED TO LOOK FOR ANYTHING          
*                                                                               
         GOTO1 =A(FINDREP),(RC)    LOOK FOR A FAXGATE REPORT                    
         B     WAIT                                                             
         EJECT                                                                  
ALLOCATE TM    INTERNAL_CONVERSATION_STATUS,OPERATOR_ENABLED                    
         BZ    WAIT                PARTNER DELIBERATELY DISABLED                
         LA    R2,ATBALC2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   ALLOCATE THE CONVERSATION                    
         MVC   APPC_COMPLETION_ENTRY_POINT,=A(CONFALLC)                         
         B     WAIT                                                             
         SPACE 3                                                                
CONFALLC MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         MVC   P+50(14),=C'AFTER ALLOCATE'                                      
         MVC   P+68(7),=C'MACHINE'                                              
         MVC   P+76(1),PARTNER_MACHINE_ID                                       
         MVI   P+77,C'/'                                                        
         MVC   P+78(1),CONVERSATION_FUNCTION                                    
         PRNT  PRINT=ALWAYS                                                     
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    RQSTCNF             ALLOCATE WAS OK                              
         GOTO1 =A(APPCERR),(RC)    CALL ERROR_EXTRACT ROUTINE                   
         CLC   RETURN_CODE,ATB_ALLOCATE_FAILURE_NO_RETRY                        
         BE    ASKOPER             FAXGATE ISN'T AVAILABLE                      
         CLC   RETURN_CODE,ATB_PARAMETER_ERROR                                  
         BNE   CANTALLC            CAN'T ALLOCATE                               
         MVC   WORK(88),=C'EDICT01 *FAXGATE SENDER PARAMETER ERROR. PRO+        
               BABLE CAUSE: UNDEFINED LOCAL LUNAME XXXXXXXX'                    
         MVC   WORK+80(8),LOCAL_LU_NAME                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(88,WORK)                             
         OI    INTERNAL_CONVERSATION_STATUS,COMMUNICATIONS_ERROR                
         B     WAIT                                                             
         SPACE 3                                                                
ASKOPER  MVC   WORK(85),=C'EDICT01 *CANNOT ALLOCATE WITH FAXGATE XXXXXX+        
               XX -- CHECK PC, REPLY R(ETRY) OR C(ANCEL)'                       
         MVC   WORK+38(8),PARTNER_LU_NAME                                       
         GOTO1 =V(LOGIO),DMCB,1,(85,WORK)                                       
         GOTO1 =V(LOGIO),DMCB,0,(6,WORK)                                        
         CLI   WORK,C'R'           DOES OPERATOR WANT TO RETRY?                 
         BE    ALLOCATE                                                         
         CLI   WORK,C'C'           DOES OPERATOR WANT TO CANCEL?                
         BNE   ASKOPER                                                          
         B     WAIT                                                             
         SPACE 3                                                                
CANTALLC MVC   WORK(63),=C'EDICT01 *SENDER TO FAXGATE XXXXXXXX CANNOT A+        
               LLOCATE -- CHECK PC'                                             
         MVC   WORK+27(8),PARTNER_LU_NAME                                       
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(63,WORK)                             
         OI    INTERNAL_CONVERSATION_STATUS,COMMUNICATIONS_ERROR                
         B     WAIT                                                             
         SPACE 3                                                                
RQSTCNF  LA    R2,ATBCFM_STRUCTURE                                              
         GOTO1 =A(CALLAPPC),(RC)   REQUEST CONFIRMATION                         
         MVC   APPC_COMPLETION_ENTRY_POINT,=A(CHKALLC)                          
         B     WAIT                                                             
         EJECT                                                                  
CHKALLC  MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         MVC   P+50(17),=C'AFTER CONF. ALLOC'                                   
         MVC   P+68(7),=C'MACHINE'                                              
         MVC   P+76(1),PARTNER_MACHINE_ID                                       
         MVI   P+77,C'/'                                                        
         MVC   P+78(1),CONVERSATION_FUNCTION                                    
         PRNT  PRINT=ALWAYS                                                     
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    FXALCOK             CONFIRMATION RECEIVED OK                     
         GOTO1 =A(APPCERR),(RC)    CALL ERROR_EXTRACT ROUTINE                   
         MVC   WORK(63),=C'EDICT01 *FAXGATE XXXXXXXX DOES NOT CONFIRM A+        
               LLOCATE -- CHECK PC'                                             
         MVC   WORK+17(8),PARTNER_LU_NAME                                       
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(63,WORK)                             
         OI    INTERNAL_CONVERSATION_STATUS,COMMUNICATIONS_ERROR                
         B     WAIT                                                             
         SPACE 3                                                                
FXALCOK  PRNT  OPENFAXGATESENDER,PRINT=ALWAYS                                   
*                                                                               
         GOTO1 =A(PRNTATTB),(RC)   PRINT CONVERSATION ATTRIBUTES                
         OI    INTERNAL_CONVERSATION_STATUS,ALLOCATED                           
         L     R2,LOOKECB                                                       
         POST  (2)                 FORCE A LOOKUP FOR REPORTS                   
*                                                                               
         B     WAIT                CONVERSATION IS ALLOCATED, SO. . .           
*                                  . . . WAIT FOR SOME REPORTS TO SEND          
         EJECT                                                                  
CHKCONF  NI    INTERNAL_CONVERSATION_STATUS,X'FF'-WAITING_FOR_CONFIRMED         
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         MVC   P+50(18),=C'AFTER SEND/CONFIRM'                                  
         MVC   P+68(7),=C'MACHINE'                                              
         MVC   P+76(1),PARTNER_MACHINE_ID                                       
         MVI   P+77,C'/'                                                        
         MVC   P+78(1),CONVERSATION_FUNCTION                                    
         PRNT  PRINT=ALWAYS                                                     
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    CHKCON05            CONFIRMATION RECEIVED OK                     
         GOTO1 =A(APPCERR),(RC)    CALL ERROR_EXTRACT ROUTINE                   
         MVC   WORK(55),=C'EDICT01 *FAXGATE XXXXXXXX DOES NOT CONFIRM T+        
               RANSMISSION'                                                     
         MVC   WORK+17(8),PARTNER_LU_NAME                                       
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(55,WORK)                             
         OI    INTERNAL_CONVERSATION_STATUS,COMMUNICATIONS_ERROR                
         B     WAIT                                                             
         SPACE 3                                                                
CHKCON05 PRNT  SENTVIAFAXGATE,PRINT=ALWAYS                                      
*                                                                               
         LA    R2,EDICT_TABLE_ENTRY   THIS IS THE LAST DEST ENTRY               
         USING XMTTABLD,R2                                                      
         ZICM  R4,XMTDSTNO,2          LAST DEST #                               
*                                                                               
CHKCON10 STCM  R4,3,XMTDSTNO                                                    
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,XMTTABLE                                                
         LA    RE,EDICT_TABLE_ENTRY                                             
         ST    RE,ETBLNTRY                                                      
         MVI   EACTION,EACTSNTQ    MARK REPORT SENT                             
         MVC   EMAJORNM,=A(MAJORNAM)                                            
         MVC   EADTAMGR,DATAMGR                                                 
         MVC   ENUMPAGE,NUMPAGES                                                
         GOTO1 =V(POSTSENT)                                                     
         BNE   CHKCON20                                                         
         DROP  R1,R2                                                            
*                                                                               
         MVC   P+30(16),=C'DESTINATION NO ='                                    
         EDIT  (R4),(5,P+46),ALIGN=LEFT,ZERO=NOBLANK                            
         PRNT  RPTMARKEDSENT,PRINT=ALWAYS                                       
*                                                                               
CHKCON20 BCT   R4,CHKCON10                                                      
*                                                                               
NEXTREP  XC    APPC_COMPLETION_ENTRY_POINT,APPC_COMPLETION_ENTRY_POINT          
         XC    EDICT_TABLE_ENTRY,EDICT_TABLE_ENTRY                              
*                                                                               
         CLI   OPERSTOP,C'Y'       DOES OPERATOR WANT TO STOP?                  
         BE    WINDDOWN            YES - DON'T LOOK FOR MORE REPORTS            
*                                                                               
         GOTO1 =A(FINDREP),(RC)    LOOK FOR A FAXGATE REPORT                    
*                                                                               
         B     WAIT                                                             
         EJECT                                                                  
REG4ALLC TM    INTERNAL_CONVERSATION_STATUS,OPERATOR_ENABLED                    
         BZ    WAIT                PARTNER DELIBERATELY DISABLED                
         LA    R2,ATBRFA2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   REGISTER FOR ALLOCATES                       
         MVC   APPC_COMPLETION_ENTRY_POINT,=A(CHKRFA)                           
         B     WAIT                                                             
         SPACE 3                                                                
CHKRFA   OC    RETURN_CODE,RETURN_CODE                                          
         BZ    RCVALLOC            WE ARE REGISTERED FOR ALLOCATES              
         GOTO1 =A(APPCERR),(RC)    CALL ERROR_EXTRACT ROUTINE                   
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
         MVC   WORK(57),=C'EDICT01 *FAXGATE RECEIVER UNDEFINED LOCAL LU+        
               NAME XXXXXXXX'                                                   
         MVC   WORK+49(8),LOCAL_LU_NAME                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(57,WORK)                             
         OI    INTERNAL_CONVERSATION_STATUS,COMMUNICATIONS_ERROR                
         B     WAIT                                                             
         SPACE 3                                                                
CANTREG  MVC   P+30(14),=C'REASON CODE = '                                      
         EDIT  REASON_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         PRNT  COULDNT_REGISTER,PRINT=ALWAYS                                    
         MVC   WORK(72),=C'EDICT01 *RECEIVER FROM FAXGATE XXXXXXXX COUL+        
               D NOT REGISTER FOR ALLOCATES'                                    
         MVC   WORK+31(8),LOCAL_LU_NAME                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(72,WORK)                             
         OI    INTERNAL_CONVERSATION_STATUS,COMMUNICATIONS_ERROR                
         B     WAIT                                                             
         SPACE 3                                                                
RCVALLOC GOTO1 =A(QUERYALQ),(RC)   QUERY ALLOCATE QUEUE                         
*                                                                               
         MVC   RECEIVE_ALLOCATE_TYPE,ATBCTS_WAIT                                
         LA    R2,ATBRAL2_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   RECEIVE ALLOCATE                             
         MVC   APPC_COMPLETION_ENTRY_POINT,=A(CHKRAL)                           
         B     WAIT                                                             
         EJECT                                                                  
CHKRAL   OC    RETURN_CODE,RETURN_CODE                                          
         BZ    RCVCFM              WE HAVE A CONVERSATION                       
         GOTO1 =A(APPCERR),(RC)    CALL ERROR_EXTRACT ROUTINE                   
         PRNT  NO_CONVERSATION,PRINT=ALWAYS                                     
         MVC   WORK(67),=C'EDICT01 *RECEIVER FROM FAXGATE XXXXXXXX DID +        
               NOT RECEIVE AN ALLOCATE'                                         
         MVC   WORK+31(8),LOCAL_LU_NAME                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(67,WORK)                             
         OI    INTERNAL_CONVERSATION_STATUS,COMMUNICATIONS_ERROR                
         B     WAIT                                                             
         SPACE 3                                                                
RCVCFM   MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
         LA    R2,ATBRCVW_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   RECEIVE AND WAIT                             
         MVC   APPC_COMPLETION_ENTRY_POINT,=A(CHKCFMD)                          
         B     WAIT                                                             
         SPACE 3                                                                
CHKCFMD  OC    RETURN_CODE,RETURN_CODE                                          
         BZ    CHKCFMD5            BAD RETURN CODE                              
         GOTO1 =A(APPCERR),(RC)    CALL ERROR_EXTRACT ROUTINE                   
         PRNT  BAD_RECEIVE_CALL,PRINT=ALWAYS                                    
         MVC   WORK(59),=C'EDICT01 *RECEIVER FROM FAXGATE XXXXXXXX -- E+        
               RROR ON RECEIVE'                                                 
         MVC   WORK+31(8),LOCAL_LU_NAME                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(59,WORK)                             
         OI    INTERNAL_CONVERSATION_STATUS,COMMUNICATIONS_ERROR                
         B     WAIT                                                             
         SPACE 3                                                                
CHKCFMD5 CLC   STATUS_RECEIVED,ATB_CONFIRM_RECEIVED                             
         BE    SNDCFM                                                           
         PRNT  NO_CONFIRMREQUEST,PRINT=ALWAYS                                   
         MVC   WORK(85),=C'EDICT01 *RECEIVER FROM FAXGATE XXXXXXXX DID +        
               NOT RECEIVE EXPECTED CONFIRMATION REQUEST'                       
         MVC   WORK+31(8),LOCAL_LU_NAME                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(85,WORK)                             
         OI    INTERNAL_CONVERSATION_STATUS,COMMUNICATIONS_ERROR                
         B     WAIT                                                             
         SPACE 3                                                                
SNDCFM   PRNT  CONFIRMREQUESTED,PRINT=ALWAYS                                    
*                                                                               
         LA    R2,ATBCFMD_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   ISSUE CONFIRMATION                           
         MVC   APPC_COMPLETION_ENTRY_POINT,=A(SNDCFMD)                          
         B     WAIT                                                             
         EJECT                                                                  
SNDCFMD  OC    RETURN_CODE,RETURN_CODE                                          
         BZ    INCONV              RETURN CODE WAS OK                           
         GOTO1 =A(APPCERR),(RC)    CALL ERROR_EXTRACT ROUTINE                   
         PRNT  BAD_CONFIRMEDCALL,PRINT=ALWAYS                                   
         MVC   WORK(66),=C'EDICT01 *RECEIVER FROM FAXGATE XXXXXXXX COUL+        
               D NOT CONFIRM ALLOCATE'                                          
         MVC   WORK+31(8),LOCAL_LU_NAME                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(66,WORK)                             
         OI    INTERNAL_CONVERSATION_STATUS,COMMUNICATIONS_ERROR                
         B     WAIT                                                             
         SPACE 3                                                                
INCONV   PRNT  OPENFXRECEIVE,PRINT=ALWAYS                                       
         OI    INTERNAL_CONVERSATION_STATUS,ALLOCATED                           
*                                                                               
         GOTO1 =A(PRNTATTB),(RC)   PRINT CONVERSATION ATTRIBUTES                
         EJECT                                                                  
RCVNTU   XC    BUFFER,BUFFER                                                    
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
         LA    R2,ATBRCVW_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   RECEIVE AND WAIT                             
         MVC   APPC_COMPLETION_ENTRY_POINT,=A(CHKNTU)                           
         B     WAIT                                                             
         SPACE 3                                                                
CHKNTU   OC    RETURN_CODE,RETURN_CODE                                          
         BZ    NTUOK                                                            
         GOTO1 =A(APPCERR),(RC)    CALL ERROR_EXTRACT ROUTINE                   
         MVC   WORK(59),=C'EDICT01 *RECEIVER FROM FAXGATE XXXXXXXX -- E+        
               RROR ON RECEIVE'                                                 
         MVC   WORK+31(8),LOCAL_LU_NAME                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(59,WORK)                             
         OI    INTERNAL_CONVERSATION_STATUS,COMMUNICATIONS_ERROR                
         B     WAIT                                                             
*                                                                               
NTUOK    CLC   STATUS_RECEIVED,ATB_CONFIRM_DEALLOC_RECEIVED                     
         BNE   NODEALOC                                                         
         OI    INTERNAL_CONVERSATION_STATUS,PARTNER_DEALLOCATED                 
         PRNT  DEALLOCATEREQUESTED,PRINT=ALWAYS                                 
         B     ISSUECNF                                                         
*                                                                               
NODEALOC CLC   STATUS_RECEIVED,ATB_CONFIRM_RECEIVED                             
         BNE   *+6                                                              
         DC    H'0'                WHY DID FAXGATE REQUEST CONFIRM?             
*                                                                               
         CLC   DATA_RECEIVED,ATB_NO_DATA_RECEIVED                               
         BNE   *+6                                                              
         DC    H'0'                FAXGATE SENT EMPTY DATA RECORD               
*                                                                               
         L     R2,RECEIVE_LENGTH                                                
         CLI   TRACEFLG,C'Y'                                                    
         BNE   SKIPA                                                            
         GOTO1 =V(PRNTBL),DMCB,=C'RECEIVED FROM FAXGATE',BUFFER,       +        
               C'DUMP',(R2),=C'1D'                                              
*                                                                               
SKIPA    CLC   =C'#NTU#',BUFFER    THIS MUST BE FIRST RECORD                    
         BE    *+6                                                              
         DC    H'0'                WHAT DID THEY SEND US?                       
*                                                                               
         XC    BUFFER,BUFFER                                                    
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
         LA    R2,ATBRCVW_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   RECEIVE AND WAIT                             
         MVC   APPC_COMPLETION_ENTRY_POINT,=A(RCVLOG)                           
         B     WAIT                                                             
         EJECT                                                                  
RCVLOG   OC    RETURN_CODE,RETURN_CODE                                          
         BZ    FGCICSOK                                                         
         GOTO1 =A(APPCERR),(RC)    CALL ERROR_EXTRACT ROUTINE                   
         MVC   WORK(59),=C'EDICT01 *RECEIVER FROM FAXGATE XXXXXXXX -- E+        
               RROR ON RECEIVE'                                                 
         MVC   WORK+31(8),LOCAL_LU_NAME                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(59,WORK)                             
         OI    INTERNAL_CONVERSATION_STATUS,COMMUNICATIONS_ERROR                
         B     WAIT                                                             
*                                                                               
FGCICSOK CLC   STATUS_RECEIVED,ATB_CONFIRM_DEALLOC_RECEIVED                     
         BNE   NODEALC                                                          
         OI    INTERNAL_CONVERSATION_STATUS,PARTNER_DEALLOCATED                 
         PRNT  DEALLOCATEREQUESTED,PRINT=ALWAYS                                 
         B     ISSUECNF                                                         
*                                                                               
NODEALC  CLC   STATUS_RECEIVED,ATB_CONFIRM_RECEIVED                             
         BE    *+6                                                              
         DC    H'0'                THEY MUST HAVE REQUESTED CONFIRM             
*                                                                               
         CLC   DATA_RECEIVED,ATB_NO_DATA_RECEIVED                               
         BNE   *+6                                                              
         DC    H'0'                FAXGATE SENT EMPTY DATA RECORD               
*                                                                               
         L     R2,RECEIVE_LENGTH                                                
         CLI   TRACEFLG,C'Y'                                                    
         BNE   SKIPB                                                            
         GOTO1 =V(PRNTBL),DMCB,=C'RECEIVED FROM FAXGATE',BUFFER,       +        
               C'DUMP',(R2),=C'1D'                                              
*                                                                               
SKIPB    CLC   =C'FGCICS',BUFFER   THIS MUST BE SECOND RECORD                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,BUFFER                                                        
         USING LOGRECD,R4                                                       
*                                                                               
         CLI   LOGLAST,C'Y'        IS THIS THE FINAL STATUS OF JOB?             
         BNE   ISSUECNF            NO - IGNORE IT                               
*                                                                               
         GOTO1 =V(HEXIN),DMCB,LOGDATE+3,DELDAY,2                                
         CLC   =F'1',DMCB+12                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WORK(2),LOGTIME     HH                                           
         MVC   WORK+2(2),LOGTIME+3 MM (SKIP OVER COLON)                         
         GOTO1 =V(HEXIN),DMCB,WORK,DELTIME,4                                    
         CLC   =F'2',DMCB+12                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TBIN  DDSTIME=YES         R0 = DDS TIME DIFFERENCE (IN SECS.)          
         SRDL  R0,32               PREPARE FOR DIVIDE                           
         D     R0,=F'3600'         R1 = DDS START TIME IN HOURS                 
         BCTR  R1,0                MINUS ONE HOUR                               
         CLM   R1,1,DELTIME        IS DELIVERY TIME BEFORE DDS START?           
         BNH   GM10                NO                                           
         XC    WORK,WORK                                                        
         MVC   WORK(2),LOGDATE+6   YY                                           
         MVC   WORK+2(2),LOGDATE   MM                                           
         MVC   WORK+4(2),LOGDATE+3 DD                                           
         GOTO1 =V(ADDAY),DMCB,WORK,WORK+6,F'-1'                                 
         GOTO1 =V(HEXIN),DMCB,WORK+10,DELDAY,2                                  
         CLC   =F'1',DMCB+12                                                    
         BE    *+6                 DAY NUMBER IS DDS DAY                        
         DC    H'0'                                                             
*                                                                               
GM10     CLI   LOGCID,C'4'         CHECK CUSTOMER REF. NUMBER VERSION           
         BE    *+12                                                             
         CLI   LOGCID,C'D'         GENERATED DURING DR TEST?                    
         BNE   ISSUECNF            NO -- OBSOLETE                               
*                                                                               
         CLC   LOGCID+1(1),MAJORNAM+5  CAME FROM EDICTR (OR EDICTT)?            
         BE    *+6                                                              
         DC    H'0'                NO: ABSOLUTELY IMPOSSIBLE                    
*                                                                               
         MVC   DELPERD(8),LOGCID+10  DATE OF ORIGINAL SEND (YYYYMMDD)           
         GOTO1 =V(PERVAL),DMCB,(17,DELPERD),WORK                                
         CLI   DMCB+4,0                                                         
         BNE   ISSUECNF            MAINFRAME DATE MUST BE SCREWED UP!           
         LA    RF,WORK                                                          
         CLC   PVALNDYS-PERVALD(2,RF),=AL2(28)                                  
         BH    ISSUECNF            LOG RECORD IS MORE THAN 28 DAYS OLD          
*                                                                               
         LA    R1,DMCB                                                          
         USING CONVDSKD,R1                                                      
         MVC   CONVDMGR,DATAMGR    A(DATAMGR)                                   
         MVI   CONVACTN,CONVDSKO   CONVERT REFERENCE NUMBER TO DSKADDR          
         LA    RE,(LOGCID+2-LOGRECD)(R4)                                        
         ST    RE,CONVOFF          A(CUSTOMER ID)                               
         GOTO1 =V(CONVDSKA)                                                     
         MVC   FULL,CONVDSK                                                     
         DROP  R1                                                               
*                                                                               
         LA    R5,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R5)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(5),E,8)  ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         LA    R2,TABLNTRY         ASSUME WE'LL FIND REPORT IN TABLE            
         MVI   BYTE,C'T'           'T' FOR TABLE                                
*                                                                               
         L     R9,XMTTABLE         A(TRANSMIT TABLE)                            
         USING XMTTABLD,R3                                                      
         LA    R3,XMTENTRY                                                      
*                                                                               
GM20     MVC31 XMTENTRY,0(R9)                                                   
         CLI   XMTTABLD,0          END OF TABLE?                                
         BNE   *+16                                                             
         L     R2,FULL             YES -- USE EDICT FILE DISK ADDRESS           
         MVI   BYTE,C'F'           'F' FOR FILE                                 
         B     GM30                                                             
         CLI   XMTSOURC,XMTSRCPQ   PRINT QUEUE ENTRY?                           
         BNE   *+14                                                             
         CLC   FULL,XMTDSKAD       FIND MATCH ON DISK ADDRESS                   
         BE    *+12                                                             
         AHI   R9,XMTTBLQ          BUMP TO NEXT ENTRY                           
         B     GM20                                                             
         MVC   TABLNTRY,0(R3)      SAVE XMIT TABLE ENTRY                        
         DROP  R3                                                               
*                                                                               
GM30     LA    R5,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R5)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(5),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,XMTTABLE                                                
         ST    R2,ETBLNTRY                                                      
         MVI   EACTION,EACTDLVQ    ASSUME DELIVERED SUCCESSFULLY                
         CLC   =C'0007',LOGSTAT    WAS IT?                                      
         BE    *+8                                                              
         MVI   EACTION,EACTCANQ    NO - POST CANCELLED                          
         CLI   BYTE,C'F'           DISK ADDRESS BEING PASSED?                   
         BNE   *+8                                                              
         OI    EACTION,EACTFILQ    YES                                          
         MVC   EMAJORNM,=A(MAJORNAM)                                            
         MVC   EDAYNUM,DELDAY      DAY DELIVERED                                
         MVC   ETIME,DELTIME       TIME DELIVERED                               
         XC    WORK,WORK                                                        
         MVC   WORK(6),LOGJID      JOB ID                                       
         MVC   WORK+6(4),LOGSTAT   FINAL STATUS                                 
         MVC   EAFXGDTA,=A(WORK)                                                
         MVC   EADTAMGR,DATAMGR                                                 
         GOTO1 =V(POSTSENT)                                                     
         BNE   ISSUECNF            COULD NOT POST EDICT FILE                    
         DROP  R1                                                               
*                                                                               
         MVC   P+30(L'CUSTID),LOGCID  CUSTOMER ID                               
         MVC   P+55(7),=C'JOBID: '                                              
         MVC   P+62(6),LOGJID      FAXGATE JOB ID                               
         PRNT  FXDELIVERYNOT,PRINT=ALWAYS                                       
         DROP  R4                                                               
*                                                                               
ISSUECNF LA    R2,ATBCFMD_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)   ISSUE CONFIRMATION                           
         MVC   APPC_COMPLETION_ENTRY_POINT,=A(RCVNTU)                           
         B     WAIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB,R8                                                            
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
         LH    R5,MAXDESTS         MAX DESTS/REPORT                             
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
         GOTO1 =V(DATCON),DMCB,(5,0),(20,TODAY8)                                
         MVC   DELPERD+9(8),TODAY8   FOR PERVAL                                 
*                                                                               
         BLDL  0,ENTRYPTS            BUILD LIST OF APPC/MVS ENTRY PTS           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                  BAD RETURN FROM BLDL MACRO                 
         LOAD  DE=ATBALC2                                                       
         ST    R0,AATBALC2                                                      
         LOAD  DE=ATBDEAL                                                       
         ST    R0,AATBDEAL                                                      
         LOAD  DE=ATBSEND                                                       
         ST    R0,AATBSEND                                                      
         LOAD  DE=ATBRCVW                                                       
         ST    R0,AATBRCVW                                                      
         LOAD  DE=ATBCFM                                                        
         ST    R0,AATBCFM                                                       
         LOAD  DE=ATBCFMD                                                       
         ST    R0,AATBCFMD                                                      
         LOAD  DE=ATBGTA2                                                       
         ST    R0,AATBGTA2                                                      
         LOAD  DE=ATBEES3                                                       
         ST    R0,AATBEES3                                                      
         LOAD  DE=ATBQAQ2                                                       
         ST    R0,AATBQAQ2                                                      
         LOAD  DE=ATBRAL2                                                       
         ST    R0,AATBRAL2                                                      
         LOAD  DE=ATBRFA2                                                       
         ST    R0,AATBRFA2                                                      
         LOAD  DE=ATBURA2                                                       
         ST    R0,AATBURA2                                                      
*                                                                               
         L     R2,NUMPCS           NUMBER OF PARTNER FAXGATE PCS                
         SLL   R2,3                4 BYTES PER ECB, 2 ECBS PER PARTNER          
         LA    R2,16(R2)           EYE-CATCHER, PLUS TWO MORE ECBS              
         STORAGE OBTAIN,LENGTH=(2)                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL STORAGE OBTAIN                  
         MVC   0(8,R1),=C'*ECBLIST'                                             
         LA    R1,8(R1)            BUMP PAST LABEL                              
         ST    R1,AECBLIST         A(START OF ECBLIST)                          
*                                                                               
         L     R3,AECBLIST         BUILD ECBLIST                                
         MVI   0(R3),0                                                          
         L     R1,LOOKECB                                                       
         STCM  R1,7,1(R3)          1ST ECB: EDICT SAYS LOOK                     
         LA    R3,4(R3)                                                         
         MVI   0(R3),0                                                          
         L     R1,STOPECB                                                       
         STCM  R1,7,1(R3)          2ND ECB: EDICT SAYS STOP                     
*                                                                               
         L     R6,AAPPCTAB         APPC/MVS CONTROL TABLE                       
         L     R5,NUMPCS           NUMBER OF FAXGATE PCS                        
         LA    R4,PARTLUS          TABLE OF LUNAMES                             
         USING PARTLUSD,R4                                                      
*                                                                               
INIT10   LA    R2,2                2 CONVERSATIONS PER MACHINE                  
         MVI   BYTE,SENDER         FIRST CONVERSATION IS SENDER                 
*                                                                               
INIT20   LA    R3,4(R3)            BUMP TO NEXT POSITION IN ECBLIST             
         ST    R6,0(R3)            FIRST FIELD IN TABLE IS ECB                  
         XC    CONVERSATION_ID,CONVERSATION_ID                                  
         XC    EDICT_TABLE_ENTRY,EDICT_TABLE_ENTRY                              
         XC    APPC_COMPLETION_ENTRY_POINT,APPC_COMPLETION_ENTRY_POINT          
         MVC   CONVERSATION_FUNCTION,BYTE                                       
         MVI   INTERNAL_CONVERSATION_STATUS,OPERATOR_ENABLED                    
         TM    PARTFLAG,PARTNOOP   WAS PARTNER DISABLED VIA PARAMETERS?         
         BZ    *+8                 NO                                           
         MVI   INTERNAL_CONVERSATION_STATUS,0                                   
         XC    APPC_ECB,APPC_ECB                                                
         POST  APPC_ECB                                                         
         LA    R1,APPC_ECB                                                      
         ST    R1,NOTIFY_TYPE+4                                                 
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_ECB                                  
         XC    APPC_ECB_DEALLOCATE,APPC_ECB_DEALLOCATE                          
         LA    R1,APPC_ECB_DEALLOCATE                                           
         ST    R1,NOTIFY_TYPE_DEALLOCATE+4                                      
         MVC   NOTIFY_TYPE_DEALLOCATE,ATB_NOTIFY_TYPE_ECB                       
         MVC   CONVERSATION_TYPE,ATB_MAPPED_CONVERSATION                        
         MVC   SYM_DEST_NAME,MYSPACES                                           
         MVC   LOCAL_LU_NAME,LOCALLU                                            
         MVC   PARTNER_MACHINE_ID,PARTID                                        
         MVC   PARTNER_LU_NAME,MYSPACES                                         
         MVC   PARTNER_LU_NAME(8),PARTLU                                        
         MVC   MODE_NAME,VTAMMODE                                               
         MVC   TP_NAME_LENGTH,=F'10'                                            
         MVC   TP_NAME,=CL64'DDSFAXGATE'                                        
         MVC   RETURN_CONTROL,ATB_WHEN_SESSION_ALLOCATED                        
         MVC   SYNC_LEVEL,ATB_CONFIRM                                           
         MVC   SECURITY_TYPE,ATB_SECURITY_PROGRAM                               
         MVC   USER_ID,MYSPACES                                                 
         MVC   PASSWORD,MYSPACES                                                
         MVC   PROFILE,MYSPACES                                                 
         MVC   FILL,ATB_FILL_LL                                                 
         XC    USER_TOKEN,USER_TOKEN                                            
         XC    TPID,TPID                                                        
         XC    RECEIVE_ACCESS_TOKEN,RECEIVE_ACCESS_TOKEN                        
         XC    SEND_ACCESS_TOKEN,SEND_ACCESS_TOKEN                              
         XC    ALLOCATE_QUEUE_TOKEN,ALLOCATE_QUEUE_TOKEN                        
         XC    TIME_OUT_VALUE,TIME_OUT_VALUE                                    
*                                                                               
         MVC   ATBALC2_STRUCTURE,AATBALC2                                       
         MVC   ATBALC2_STRUCTURE+4(16),=CL16'ALLOCATE'                          
         MVI   ATBALC2_STRUCTURE+20,X'40'                                       
         LA    R1,CONVERSATION_TYPE                                             
         ST    R1,ATBALC2_STRUCTURE+24                                          
         LA    R1,SYM_DEST_NAME                                                 
         ST    R1,ATBALC2_STRUCTURE+28                                          
         LA    R1,PARTNER_LU_NAME                                               
         ST    R1,ATBALC2_STRUCTURE+32                                          
         LA    R1,MODE_NAME                                                     
         ST    R1,ATBALC2_STRUCTURE+36                                          
         LA    R1,TP_NAME_LENGTH                                                
         ST    R1,ATBALC2_STRUCTURE+40                                          
         LA    R1,TP_NAME                                                       
         ST    R1,ATBALC2_STRUCTURE+44                                          
         LA    R1,RETURN_CONTROL                                                
         ST    R1,ATBALC2_STRUCTURE+48                                          
         LA    R1,SYNC_LEVEL                                                    
         ST    R1,ATBALC2_STRUCTURE+52                                          
         LA    R1,SECURITY_TYPE                                                 
         ST    R1,ATBALC2_STRUCTURE+56                                          
         LA    R1,USER_ID                                                       
         ST    R1,ATBALC2_STRUCTURE+60                                          
         LA    R1,PASSWORD                                                      
         ST    R1,ATBALC2_STRUCTURE+64                                          
         LA    R1,PROFILE                                                       
         ST    R1,ATBALC2_STRUCTURE+68                                          
         LA    R1,USER_TOKEN                                                    
         ST    R1,ATBALC2_STRUCTURE+72                                          
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBALC2_STRUCTURE+76                                          
         LA    R1,NOTIFY_TYPE                                                   
         ST    R1,ATBALC2_STRUCTURE+80                                          
         LA    R1,TPID                                                          
         ST    R1,ATBALC2_STRUCTURE+84                                          
         LA    R1,LOCAL_LU_NAME                                                 
         ST    R1,ATBALC2_STRUCTURE+88                                          
         LA    R1,RETURN_CODE                                                   
         O     R1,=X'80000000'                                                  
         ST    R1,ATBALC2_STRUCTURE+92                                          
*                                                                               
         MVC   ATBDEAL_STRUCTURE,AATBDEAL                                       
         MVC   ATBDEAL_STRUCTURE+4(16),=CL16'DEALLOCATE'                        
         MVI   ATBDEAL_STRUCTURE+20,X'40'                                       
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBDEAL_STRUCTURE+24                                          
         LA    R1,DEALLOCATE_TYPE                                               
         ST    R1,ATBDEAL_STRUCTURE+28                                          
         LA    R1,NOTIFY_TYPE                                                   
         ST    R1,ATBDEAL_STRUCTURE+32                                          
         LA    R1,RETURN_CODE                                                   
         O     R1,=X'80000000'                                                  
         ST    R1,ATBDEAL_STRUCTURE+36                                          
*                                                                               
         MVC   ATBDEAL_ABEND_STRUCTURE,AATBDEAL                                 
         MVC   ATBDEAL_ABEND_STRUCTURE+4(16),=CL16'DEALLOCATE_ABEND'            
         MVI   ATBDEAL_ABEND_STRUCTURE+20,X'40'                                 
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBDEAL_ABEND_STRUCTURE+24                                    
         LA    R1,DEALLOCATE_TYPE                                               
         ST    R1,ATBDEAL_ABEND_STRUCTURE+28                                    
         LA    R1,NOTIFY_TYPE_DEALLOCATE                                        
         ST    R1,ATBDEAL_ABEND_STRUCTURE+32                                    
         LA    R1,RETURN_CODE_DEALLOCATE                                        
         O     R1,=X'80000000'                                                  
         ST    R1,ATBDEAL_ABEND_STRUCTURE+36                                    
*                                                                               
         MVC   ATBSEND_STRUCTURE,AATBSEND                                       
         MVC   ATBSEND_STRUCTURE+4(16),=CL16'SEND_DATA'                         
         MVI   ATBSEND_STRUCTURE+20,X'40'                                       
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBSEND_STRUCTURE+24                                          
         LA    R1,SEND_TYPE                                                     
         ST    R1,ATBSEND_STRUCTURE+28                                          
         LA    R1,SEND_LENGTH                                                   
         ST    R1,ATBSEND_STRUCTURE+32                                          
         LA    R1,SEND_ACCESS_TOKEN                                             
         ST    R1,ATBSEND_STRUCTURE+36                                          
         LA    R1,BUFFER                                                        
         ST    R1,ATBSEND_STRUCTURE+40                                          
         LA    R1,REQUEST_TO_SEND_VALUE                                         
         ST    R1,ATBSEND_STRUCTURE+44                                          
         LA    R1,NOTIFY_TYPE                                                   
         ST    R1,ATBSEND_STRUCTURE+48                                          
         LA    R1,RETURN_CODE                                                   
         O     R1,=X'80000000'                                                  
         ST    R1,ATBSEND_STRUCTURE+52                                          
*                                                                               
         MVC   ATBRCVW_STRUCTURE,AATBRCVW                                       
         MVC   ATBRCVW_STRUCTURE+4(16),=CL16'RECEIVE_AND_WAIT'                  
         MVI   ATBRCVW_STRUCTURE+20,X'40'                                       
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBRCVW_STRUCTURE+24                                          
         LA    R1,FILL                                                          
         ST    R1,ATBRCVW_STRUCTURE+28                                          
         LA    R1,RECEIVE_LENGTH                                                
         ST    R1,ATBRCVW_STRUCTURE+32                                          
         LA    R1,RECEIVE_ACCESS_TOKEN                                          
         ST    R1,ATBRCVW_STRUCTURE+36                                          
         LA    R1,BUFFER                                                        
         ST    R1,ATBRCVW_STRUCTURE+40                                          
         LA    R1,STATUS_RECEIVED                                               
         ST    R1,ATBRCVW_STRUCTURE+44                                          
         LA    R1,DATA_RECEIVED                                                 
         ST    R1,ATBRCVW_STRUCTURE+48                                          
         LA    R1,REQUEST_TO_SEND_RECEIVED                                      
         ST    R1,ATBRCVW_STRUCTURE+52                                          
         LA    R1,NOTIFY_TYPE                                                   
         ST    R1,ATBRCVW_STRUCTURE+56                                          
         LA    R1,RETURN_CODE                                                   
         O     R1,=X'80000000'                                                  
         ST    R1,ATBRCVW_STRUCTURE+60                                          
*                                                                               
         MVC   ATBCFM_STRUCTURE,AATBCFM                                         
         MVC   ATBCFM_STRUCTURE+4(16),=CL16'CONFIRM'                            
         MVI   ATBCFM_STRUCTURE+20,X'40'                                        
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBCFM_STRUCTURE+24                                           
         LA    R1,REQUEST_TO_SEND_RECEIVED                                      
         ST    R1,ATBCFM_STRUCTURE+28                                           
         LA    R1,NOTIFY_TYPE                                                   
         ST    R1,ATBCFM_STRUCTURE+32                                           
         LA    R1,RETURN_CODE                                                   
         O     R1,=X'80000000'                                                  
         ST    R1,ATBCFM_STRUCTURE+36                                           
*                                                                               
         MVC   ATBCFMD_STRUCTURE,AATBCFMD                                       
         MVC   ATBCFMD_STRUCTURE+4(16),=CL16'CONFIRMED'                         
         MVI   ATBCFMD_STRUCTURE+20,X'40'                                       
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBCFMD_STRUCTURE+24                                          
         LA    R1,NOTIFY_TYPE                                                   
         ST    R1,ATBCFMD_STRUCTURE+28                                          
         LA    R1,RETURN_CODE                                                   
         O     R1,=X'80000000'                                                  
         ST    R1,ATBCFMD_STRUCTURE+32                                          
*                                                                               
         MVC   ATBGTA2_STRUCTURE,AATBGTA2                                       
         MVC   ATBGTA2_STRUCTURE+4(16),=CL16'GET_ATTRIBUTES'                    
         MVI   ATBGTA2_STRUCTURE+20,X'40'                                       
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBGTA2_STRUCTURE+24                                          
         LA    R1,PARTNER_LU_NAME                                               
         ST    R1,ATBGTA2_STRUCTURE+28                                          
         LA    R1,MODE_NAME                                                     
         ST    R1,ATBGTA2_STRUCTURE+32                                          
         LA    R1,SYNC_LEVEL                                                    
         ST    R1,ATBGTA2_STRUCTURE+36                                          
         LA    R1,CONVERSATION_CORRELATOR                                       
         ST    R1,ATBGTA2_STRUCTURE+40                                          
         LA    R1,LUW_ID                                                        
         ST    R1,ATBGTA2_STRUCTURE+44                                          
         LA    R1,WORK                                                          
         ST    R1,ATBGTA2_STRUCTURE+48                                          
         LA    R1,TP_NAME                                                       
         ST    R1,ATBGTA2_STRUCTURE+52                                          
         LA    R1,LOCAL_LU_NAME                                                 
         ST    R1,ATBGTA2_STRUCTURE+56                                          
         LA    R1,CONVERSATION_TYPE                                             
         ST    R1,ATBGTA2_STRUCTURE+60                                          
         LA    R1,WORK                                                          
         ST    R1,ATBGTA2_STRUCTURE+64                                          
         LA    R1,WORK                                                          
         ST    R1,ATBGTA2_STRUCTURE+68                                          
         LA    R1,WORK                                                          
         ST    R1,ATBGTA2_STRUCTURE+72                                          
         LA    R1,CONVERSATION_STATE                                            
         ST    R1,ATBGTA2_STRUCTURE+76                                          
         LA    R1,RETURN_CODE                                                   
         O     R1,=X'80000000'                                                  
         ST    R1,ATBGTA2_STRUCTURE+80                                          
*                                                                               
         MVC   ATBEES3_STRUCTURE,AATBEES3                                       
         MVC   ATBEES3_STRUCTURE+4(16),=CL16'ERROR_EXTRACT'                     
         MVI   ATBEES3_STRUCTURE+20,0                                           
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBEES3_STRUCTURE+24                                          
         LA    R1,SERVICE_NAME                                                  
         ST    R1,ATBEES3_STRUCTURE+28                                          
         LA    R1,SERVICE_REASON_CODE                                           
         ST    R1,ATBEES3_STRUCTURE+32                                          
         LA    R1,MESSAGE_TEXT_LENGTH                                           
         ST    R1,ATBEES3_STRUCTURE+36                                          
         LA    R1,MESSAGE_TEXT                                                  
         ST    R1,ATBEES3_STRUCTURE+40                                          
         LA    R1,ERROR_LOG_PRODUCT_SET_ID_LENGTH                               
         ST    R1,ATBEES3_STRUCTURE+44                                          
         LA    R1,ERROR_LOG_PRODUCT_SET_ID                                      
         ST    R1,ATBEES3_STRUCTURE+48                                          
         LA    R1,ERROR_LOG_INFORMATION_LENGTH                                  
         ST    R1,ATBEES3_STRUCTURE+52                                          
         LA    R1,ERROR_LOG_INFORMATION                                         
         ST    R1,ATBEES3_STRUCTURE+56                                          
         LA    R1,REASON_CODE_ERROR_EXTRACT                                     
         ST    R1,ATBEES3_STRUCTURE+60                                          
         LA    R1,RETURN_CODE_ERROR_EXTRACT                                     
         O     R1,=X'80000000'                                                  
         ST    R1,ATBEES3_STRUCTURE+64                                          
*                                                                               
         MVC   ATBQAQ2_STRUCTURE,AATBQAQ2                                       
         MVC   ATBQAQ2_STRUCTURE+4(16),=CL16'QUERY_ALLOCATE_Q'                  
         MVI   ATBQAQ2_STRUCTURE+20,0                                           
         LA    R1,NOTIFY_TYPE                                                   
         ST    R1,ATBQAQ2_STRUCTURE+24                                          
         LA    R1,ALLOCATE_QUEUE_TOKEN                                          
         ST    R1,ATBQAQ2_STRUCTURE+28                                          
         LA    R1,TP_NAME_LENGTH                                                
         ST    R1,ATBQAQ2_STRUCTURE+32                                          
         LA    R1,TP_NAME                                                       
         ST    R1,ATBQAQ2_STRUCTURE+36                                          
         LA    R1,LOCAL_LU_NAME                                                 
         ST    R1,ATBQAQ2_STRUCTURE+40                                          
         LA    R1,ALLOCATE_QUEUE_SIZE                                           
         ST    R1,ATBQAQ2_STRUCTURE+44                                          
         LA    R1,ALLOCATE_QUEUE_OLDEST                                         
         ST    R1,ATBQAQ2_STRUCTURE+48                                          
         LA    R1,LAST_REC_ALLOC_ISSUED                                         
         ST    R1,ATBQAQ2_STRUCTURE+52                                          
         LA    R1,LAST_REC_ALLOC_RETURNED                                       
         ST    R1,ATBQAQ2_STRUCTURE+56                                          
         LA    R1,REASON_CODE                                                   
         ST    R1,ATBQAQ2_STRUCTURE+60                                          
         LA    R1,RETURN_CODE                                                   
         O     R1,=X'80000000'                                                  
         ST    R1,ATBQAQ2_STRUCTURE+64                                          
*                                                                               
         MVC   ATBRAL2_STRUCTURE,AATBRAL2                                       
         MVC   ATBRAL2_STRUCTURE+4(16),=CL16'RECEIVE_ALLOCATE'                  
         MVI   ATBRAL2_STRUCTURE+20,0                                           
         LA    R1,NOTIFY_TYPE                                                   
         ST    R1,ATBRAL2_STRUCTURE+24                                          
         LA    R1,ALLOCATE_QUEUE_TOKEN                                          
         ST    R1,ATBRAL2_STRUCTURE+28                                          
         LA    R1,RECEIVE_ALLOCATE_TYPE                                         
         ST    R1,ATBRAL2_STRUCTURE+32                                          
         LA    R1,TIME_OUT_VALUE                                                
         ST    R1,ATBRAL2_STRUCTURE+36                                          
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBRAL2_STRUCTURE+40                                          
         LA    R1,CONVERSATION_TYPE                                             
         ST    R1,ATBRAL2_STRUCTURE+44                                          
         LA    R1,PARTNER_LU_NAME                                               
         ST    R1,ATBRAL2_STRUCTURE+48                                          
         LA    R1,MODE_NAME                                                     
         ST    R1,ATBRAL2_STRUCTURE+52                                          
         LA    R1,SYNC_LEVEL                                                    
         ST    R1,ATBRAL2_STRUCTURE+56                                          
         LA    R1,USER_ID                                                       
         ST    R1,ATBRAL2_STRUCTURE+60                                          
         LA    R1,PROFILE                                                       
         ST    R1,ATBRAL2_STRUCTURE+64                                          
         LA    R1,REASON_CODE                                                   
         ST    R1,ATBRAL2_STRUCTURE+68                                          
         LA    R1,RETURN_CODE                                                   
         O     R1,=X'80000000'                                                  
         ST    R1,ATBRAL2_STRUCTURE+72                                          
*                                                                               
         MVC   ATBRFA2_STRUCTURE,AATBRFA2                                       
         MVC   ATBRFA2_STRUCTURE+4(16),=CL16'REGISTER_4_ALLOC'                  
         MVI   ATBRFA2_STRUCTURE+20,0                                           
         LA    R1,NOTIFY_TYPE                                                   
         ST    R1,ATBRFA2_STRUCTURE+24                                          
         LA    R1,SYM_DEST_NAME                                                 
         ST    R1,ATBRFA2_STRUCTURE+28                                          
         LA    R1,TP_NAME_LENGTH                                                
         ST    R1,ATBRFA2_STRUCTURE+32                                          
         LA    R1,TP_NAME                                                       
         ST    R1,ATBRFA2_STRUCTURE+36                                          
         LA    R1,LOCAL_LU_NAME                                                 
         ST    R1,ATBRFA2_STRUCTURE+40                                          
         LA    R1,PARTNER_LU_NAME                                               
         ST    R1,ATBRFA2_STRUCTURE+44                                          
         LA    R1,USER_ID                                                       
         ST    R1,ATBRFA2_STRUCTURE+48                                          
         LA    R1,PROFILE                                                       
         ST    R1,ATBRFA2_STRUCTURE+52                                          
         LA    R1,ALLOCATE_QUEUE_TOKEN                                          
         ST    R1,ATBRFA2_STRUCTURE+56                                          
         LA    R1,REASON_CODE                                                   
         ST    R1,ATBRFA2_STRUCTURE+60                                          
         LA    R1,RETURN_CODE                                                   
         O     R1,=X'80000000'                                                  
         ST    R1,ATBRFA2_STRUCTURE+64                                          
*                                                                               
         MVC   ATBURA2_STRUCTURE,AATBURA2                                       
         MVC   ATBURA2_STRUCTURE+4(16),=CL16'UNREGISTER_4_ALC'                  
         MVI   ATBURA2_STRUCTURE+20,0                                           
         LA    R1,NOTIFY_TYPE                                                   
         ST    R1,ATBURA2_STRUCTURE+24                                          
         LA    R1,ALLOCATE_QUEUE_TOKEN                                          
         ST    R1,ATBURA2_STRUCTURE+28                                          
         LA    R1,REASON_CODE                                                   
         ST    R1,ATBURA2_STRUCTURE+32                                          
         LA    R1,RETURN_CODE                                                   
         O     R1,=X'80000000'                                                  
         ST    R1,ATBURA2_STRUCTURE+36                                          
*                                                                               
         AH    R6,=Y(APPCDLEN)     BUMP TO NEXT CONVERSATION                    
         MVI   BYTE,RECEIVER       SECOND CONVERSATION IS RECEIVER              
         BCT   R2,INIT20                                                        
*                                                                               
         LA    R4,PARTLULQ(R4)     BUMP TO NEXT RECEIVING LUNAME                
*                                                                               
         BCT   R5,INIT10                                                        
*                                                                               
         MVC   0(4,R6),EOTMARK     MARK END OF APPC CONTROL TABLE               
         MVI   0(R3),X'80'         MARK END OF ECBLIST                          
         DROP  R4                                                               
*                                                                               
* SET (#) TASK CODE (NF) & TCB ADDR IN ISGENQ / CLEAR TASK ENQUEUES (K)         
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=CL7'ISGENQ'),C'#08K'                            
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* READCRDS                                                                      
***********************************************************************         
* READ PARAMETER CARDS BETWEEN "++FAXGATE" AND "++FAXGATEEND" CARDS             
*                                                                               
*   *......          "*" IN COLUMN ONE IS A COMMENT CARD                        
*   VTAMMODE=CCCCCCCC       VTAM MODE                                           
*   LOCALLUID=CCCCCCCC      EDICT'S LOCAL RECEIVING LUNAME                      
*   FAXGATEPC=N/LLLLLLLL/NOP RECEIVING MACHINE ID NUMBER AND LUNAME             
*    (ONE OF THESE CARDS PER MACHINE)                                           
*    ("/NOP" OPTION MEANS THIS PARTNER IS KNOWN, BUT DISABLED)                  
*   TRACE=YES/NO     OVERRIDE TRACE FLAG                                        
*   DONTSEND=YES     DON'T SEND ANY REPORTS TO THE FAXGATE/2 MACHINES           
*                     (BUT CONTINUE TO RECEIVE LOGS)                            
*   FAXPAGEWARN=N    WARN CONSOLE EVERY N PAGES FOR LONG FAX (DEF.= 20)         
*   USERID=UUUUUUUU  TRANSFER REPORTS FROM THIS USERID ONLY                     
***********************************************************************         
READCRDS NMOD1 0,READCRDS                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         SR    R2,R2               COUNT NUMBER OF FAXGATE PCS                  
         LA    R3,PARTLUS          FOR LUNAME TABLE                             
         USING PARTLUSD,R3                                                      
*                                                                               
RC10     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++FAXGATE',CARD LOOK FOR START OF PARAMETERS                  
         BNE   RC10                                                             
*                                                                               
RC20     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++FAXGATEEND',CARD    LOOK FOR END OF PARAMETERS              
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
         CLC   =C'LOCALLUID=',CARD                                              
         BNE   *+14                                                             
         MVC   LOCALLU,CARD+10     EDICT'S LOCAL LUNAME                         
         B     RC20                                                             
*                                                                               
         CLC   =C'VTAMMODE=',CARD                                               
         BNE   *+14                                                             
         MVC   VTAMMODE,CARD+9     MODE NAME                                    
         B     RC20                                                             
*                                                                               
         CLC   =C'FAXGATEPC=',CARD FAXGATEPC=N/LLLLLLLL/NOP                     
         BNE   RC30                                                             
         MVC   PARTID,CARD+10      FAXGATE PARTNER MACHINE ID                   
         MVC   PARTLU,CARD+12      FAXGATE RECEIVING LUNAME (PARTNER)           
         MVI   PARTFLAG,0          CLEAR ALL FLAGS                              
         CLC   =C'/NOP',CARD+20                                                 
         BNE   *+8                                                              
         OI    PARTFLAG,PARTNOOP   MACHINE IS NON-OPERATIONAL                   
         LA    R3,PARTLULQ(R3)     BUMP TO NEXT ENTRY                           
         LA    R2,1(R2)            INCREMENT NUMBER OF FAXGATE PCS              
         B     RC20                                                             
*                                                                               
RC30     CLC   =C'TRACE=',CARD     TRACE=                                       
         BNE   RC40                                                             
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
RC40     CLC   =C'DONTSEND=',CARD                                               
         BNE   RC50                                                             
         CLC   =C'NO',CARD+9                                                    
         BE    RC20                DON'T OVERRIDE                               
         CLC   =C'YES',CARD+9                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   DONTSEND,C'Y'       DON'T SEND REPORTS TO FAXGATE                
         B     RC20                                                             
*                                                                               
RC50     CLC   =C'FAXPAGEWARN=',CARD  FAXPAGEWARN=                              
         BNE   RC60                                                             
         GOTO1 =V(NUMVAL),DMCB,CARD+12,(2,0)                                    
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                INVALID VALUE AFTER EQUALS SIGN              
         MVC   WARNPAGE,DMCB+4                                                  
         B     RC20                                                             
*                                                                               
RC60     CLC   =C'USERID=',CARD    USERID=XXX                                   
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
         LA    R5,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R5)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(5),E,6)  ENQUEUE THE CONTROL FILE                     
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,A(IO),0               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                NO ID RECORD                                 
*                                                                               
         LA    R5,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R5)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(5),6)    DEQUEUE THE CONTROL FILE                     
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
         ST    R2,NUMPCS           SAVE NUMBER OF PCS FOUND                     
         CH    R2,=Y(MAX_FAXGATE_PARTNERS)                                      
         BNH   *+6                                                              
         DC    H'0'                MAX_FAXGATE_PARTNERS IS TOO SMALL            
*                                   MUST ALSO RELINK DDEDICT                    
         DROP  R3                                                               
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
FINDREP  NMOD1 0,FINDREP                                                        
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
FIND10   LA    R5,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R5)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(5),E,8)  ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         L     R9,XMTTABLE         R9 = A(REPORT TABLE)                         
         USING XMTTABLD,R3                                                      
         LA    R3,XMTENTRY                                                      
         XC    WORK,WORK           WORK = EARLIEST ENTRY --> FIFO               
*                                                                               
FIND20   MVC31 XMTENTRY,0(R9)                                                   
         CLI   XMTTABLD,0          END OF TABLE?                                
         BE    FIND40              YES, NO MORE                                 
         CLI   XMTSOURC,XMTSRCPQ   PRINT QUEUE ENTRY?                           
         BNE   FIND30                                                           
         CLI   XMTMETH,C'X'        YES -- FAXGATE METHOD?                       
         BNE   FIND30                                                           
         TM    XMTSTAT,EDFSTWTG    YES -- WAITING TO BE SENT?                   
         BZ    FIND30                                                           
         TM    XMTSTAT,EDFSTLST    YES -- LAST DEST IN LOGICAL REPORT?          
         BZ    FIND30                                                           
         OC    USERFILT,USERFILT   YES -- IS THERE A USERID FILTER?             
         BZ    *+14                                                             
         CLC   XMTUSRID,USERFILT   YES - DOES THIS REPORT MATCH FILTER?         
         BNE   FIND30                                                           
         TM    XMTFLAGS,XMTTEMPQ   YES -- ALREADY LOOKED AT THIS ONE?           
         BO    FIND30                                                           
*                                                                               
         L     R6,AAPPCTAB         YES -- EXAMINE APPC CONTROL TABLE            
FIND25   LA    RF,EDICT_TABLE_ENTRY                                             
         CLC   XMTDSKAD-XMTTABLD(,RF),XMTDSKAD   IS REPORT IN PROCESS?          
         BE    FIND30              YES -- SKIP IT                               
         AH    R6,=Y(APPCDLEN)     BUMP TO NEXT CONVERSATION                    
         CLC   EOTMARK,0(R6)       END OF TABLE?                                
         BNE   FIND25              NO                                           
*                                                                               
         OC    WORK,WORK           IS THIS THE FIRST ONE WE'VE CHECKED?         
         BZ    *+14                YES, SO SAVE THE DAY/TIME                    
         CLC   XMTCRDTM-XMTTABLD+WORK,XMTCRDTM                                  
         BNH   FIND30                                                           
         MVC   WORK(XMTTBLQ),XMTENTRY   EARLIEST ENTRY FOUND SO FAR             
         LR    R4,R9               SAVE THE ADDRESS OF EARLIEST ENTRY           
*                                                                               
FIND30   AHI   R9,XMTTBLQ          BUMP TO NEXT REPORT                          
         B     FIND20                                                           
*                                                                               
FIND40   OC    WORK,WORK           ANYTHING FOUND TO SEND?                      
         BZ    FIND42              NO                                           
         MVC   TABLNTRY,WORK       SAVE XMIT TABLE KEY                          
*                                                                               
         MVC   XMTENTRY,WORK                                                    
         OI    XMTFLAGS,XMTTEMPQ   WE'RE CONSIDERING IT                         
         MVC31 0(L'XMTENTRY,R4),XMTENTRY   CHANGE FLAG ON THE XMTTABLE          
         DROP  R3                                                               
*                                                                               
FIND42   LA    R5,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R5)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(5),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
         OC    WORK,WORK           ANYTHING FOUND TO SEND?                      
         BZ    FIND90              NO - EXIT                                    
*                                                                               
         LA    R1,DMCB                                                          
         USING PQPARMD,R1                                                       
         MVC   ATBLNTRY,=A(TABLNTRY)                                            
         MVC   APQBUFF,=A(CXREC)                                                
         MVC   APQHDR,=A(PQRPTHDR)                                              
         MVC   AEDHDR,=A(EDICTHDR)                                              
         MVC   APQLINE,=A(R)                                                    
         MVC   ACITABLE,CITABLE                                                 
         MVC   ADTAMGR,DATAMGR                                                  
         GOTO1 =V(INITPQRD)                                                     
         BE    FIND45                                                           
         DROP  R1                                                               
*                                                                               
         PRNT  PQREP_NOT_FOUND,PRINT=ALWAYS                                     
         MVI   ERRCODE,EDFERNPQ    REPORT NOT FOUND ON PRINT QUEUE              
         LA    R2,TABLNTRY         THIS IS THE LAST DEST ENTRY                  
         USING XMTTABLD,R2                                                      
         ZICM  R4,XMTDSTNO,2       LAST DEST #                                  
*                                  MARK ALL DEST UNSENDABLE                     
FIND43   STCM  R4,3,XMTDSTNO                                                    
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,XMTTABLE                                                
         MVC   ETBLNTRY,=A(TABLNTRY)                                            
         MVI   EACTION,EACTJNKQ    MARK REPORT UNSENDABLE                       
         MVC   EERRORCD,ERRCODE                                                 
         MVC   EMAJORNM,=A(MAJORNAM)                                            
         MVC   EADTAMGR,DATAMGR                                                 
         GOTO1 =V(POSTSENT)                                                     
         BNE   FIND44                                                           
         DROP  R1,R2                                                            
*                                                                               
         MVC   P+30(16),=C'DESTINATION NO ='                                    
         EDIT  (R4),(5,P+46),ALIGN=LEFT,ZERO=NOBLANK                            
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
FIND44   BCT   R4,FIND43                                                        
         B     FIND10                                                           
*                                                                               
         USING XMTTABLD,R4                                                      
FIND45   LA    R4,TABLNTRY                                                      
*                                                                               
         MVI   PRIORITY,C'M'       DEFAULT TO MEDIUM PRIORITY                   
*                                                                               
         MVI   BYTE2,C'1'          ONE LUID FOR ALL FAXES                       
         CLI   XMTSYS,C'R'         IS IT A REP CONTRACT REPORT?                 
         BNE   FIND50                                                           
         CLI   XMTTYPE,C'C'                                                     
         BNE   FIND50              NO                                           
         DROP  R4                                                               
*                                                                               
         MVI   PRIORITY,C'H'       CONTRACTS GET HIGH PRIORITY                  
*                                                                               
FIND50   L     R6,AAPPCTAB         APPC/MVS CONTROL TABLE                       
FIND55   CLC   PARTNER_MACHINE_ID,BYTE2                                         
         BNE   FIND60              NOT THE MACHINE WE'RE SENDING TO             
         CLI   CONVERSATION_FUNCTION,SENDER                                     
         BNE   FIND60              NOT A SENDER'S ENTRY                         
         TM    INTERNAL_CONVERSATION_STATUS,OPERATOR_ENABLED                    
         BZ    FIND10              OPERATOR HAS DISABLED THIS PARTNER           
         TM    INTERNAL_CONVERSATION_STATUS,ALLOCATED                           
         BZ    FIND10              CONVERSATION ISN'T YET ALLOCATED             
         TM    INTERNAL_CONVERSATION_STATUS,COMMUNICATIONS_ERROR                
         BO    FIND10              PROBLEM TALKING TO THIS PARTNER              
         TM    INTERNAL_CONVERSATION_STATUS,WAITING_FOR_CONFIRMED               
         BZ    FIND70              CONVERSATION IS FREE FOR SENDING             
         TM    APPC_ECB,X'40'      WAS THIS CONVERSATION POSTED?                
         BO    FIND90              YES - MAYBE A CONFIRM CAME IN                
         B     FIND10              NO - TRY ANOTHER REPORT                      
*                                                                               
FIND60   AH    R6,=Y(APPCDLEN)     BUMP TO NEXT CONVERSATION                    
         CLC   EOTMARK,0(R6)       END OF TABLE?                                
         BNE   FIND55                                                           
         DC    H'0'                                                             
*                                                                               
FIND70   MVC   EDICT_TABLE_ENTRY,TABLNTRY                                       
*                                                                               
         USING XMTTABLD,R2                                                      
         LA    R2,EDICT_TABLE_ENTRY                                             
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         LA    RF,KEY                                                           
         USING CTIREC,RF                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,XMTUSRID                                                 
         DROP  RF                                                               
*                                                                               
         LA    R5,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R5)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(5),E,6)  ENQUEUE THE CONTROL FILE                     
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,A(IO),0               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                USERID RECORD IS GONE                        
*                                                                               
         LA    R5,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R5)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(5),6)    DEQUEUE THE CONTROL FILE                     
*                                                                               
         L     R4,=A(IO)                                                        
         MVI   ELCODE,CTDSCELQ     PASSIVE POINTER ELEMENT                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   USERID,CTDSC-CTDSCD(R4)  ORIGINATING ALPHA USERID                
*                                                                               
         MVC   DESTINAT,EDICTHDR+10  DESTINATION (UP TO 25 CHARACTERS)          
*                                                                               
         LA    R1,DMCB                                                          
         USING DSTPARMD,R1                                                      
         MVC   DSTKEY,=A(USERID)                                                
         MVC   DSTTBL,DSTTABLE                                                  
         MVC   DSTMAJNM,=A(MAJORNAM)                                            
         GOTO1 =V(FINDDEST)                                                     
         BE    FIND80                                                           
         DROP  R1                                                               
*                                                                               
         PRNT  EDIFIL_NOT_FOUND,PRINT=ALWAYS                                    
         ZICM  R4,XMTDSTNO,2            LAST DEST #                             
*                                       MARK ALL DEST UNSENDABLE                
FIND75   STCM  R4,3,XMTDSTNO                                                    
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,XMTTABLE                                                
         LA    RE,EDICT_TABLE_ENTRY                                             
         ST    RE,ETBLNTRY                                                      
         MVI   EACTION,EACTJNKQ    MARK REPORT UNSENDABLE                       
         MVC   EMAJORNM,=A(MAJORNAM)                                            
         MVC   EADTAMGR,DATAMGR                                                 
         GOTO1 =V(POSTSENT)                                                     
         BNE   FIND76                                                           
         DROP  R1                                                               
*                                                                               
         MVC   P+30(16),=C'DESTINATION NO ='                                    
         EDIT  (R4),(5,P+46),ALIGN=LEFT,ZERO=NOBLANK                            
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
FIND76   BCT   R4,FIND75                                                        
         B     FIND10                                                           
*                                                                               
FIND80   MVC   P+30(8),DESTNAME-DESTTABD(R1)  PRINT DESTINATION                 
         MVC   P+40(3),XMTSUBID               PRINT PQ SUBID                    
         EDIT  XMTREFNO,(5,P+45),ALIGN=LEFT                                     
         GOTO1 =V(HEXOUT),DMCB,XMTDSKAD,P+53,4,=C'TOG'                          
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         PRNT  XFERONEREPORT,PRINT=ALWAYS                                       
         DROP  R2                                                               
*                                                                               
         GOTO1 =A(SENDMFAX),(RC)   TRANSMIT MULTI DEST. FAX                     
         BNE   FIND10              PROBLEM SENDING REPORT                       
*                                                                               
FIND87   LA    R2,ATBCFM_STRUCTURE                                              
         GOTO1 =A(CALLAPPC),(RC)   REQUEST CONFIRMATION                         
         MVC   APPC_COMPLETION_ENTRY_POINT,=A(CHKCONF)                          
         OI    INTERNAL_CONVERSATION_STATUS,WAITING_FOR_CONFIRMED               
         B     FIND10                                                           
*                                                                               
FIND90   LA    R2,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R2)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(2),E,8)  ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         L     R9,XMTTABLE         R9 = A(REPORT TABLE)                         
         USING XMTTABLD,R3                                                      
         LA    R3,XMTENTRY                                                      
*                                                                               
FIND100  MVC31 XMTENTRY,0(R9)                                                   
         CLI   0(R3),0             END OF TABLE?                                
         BE    FIND110             YES, NO MORE                                 
         CLI   XMTSOURC,XMTSRCPQ   PRINT QUEUE ENTRY?                           
         BNE   *+16                                                             
         CLI   XMTMETH,C'X'        YES -- FAXGATE METHOD?                       
         BNE   FIND105                                                          
         NI    XMTFLAGS,X'FF'-XMTTEMPQ  WE CAN LOOK AT THIS ONE LATER           
         MVC31 0(L'XMTENTRY,R9),XMTENTRY                                        
FIND105  AHI   R9,XMTTBLQ          BUMP TO NEXT REPORT                          
         B     FIND100                                                          
         DROP  R3                                                               
*                                                                               
FIND110  LA    R2,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R2)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(2),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
SENDMFAX NMOD1 0,SENDMFAX,R8                                                    
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         MVI   ERRCODE,0                                                        
         MVI   LASTEJCT,C'Y'       SO WE DON'T BEGIN WITH A FORM FEED           
         MVC   NUMPAGES,=H'1'      AT LEAST ONE PAGE WILL BE SENT               
         MVI   BYTE2,0             ASSUME NO SENDABLE DEST YET                  
*                                                                               
* BUILD THE RECEIVERS' DESTINATIONS TABLE                                       
*                                                                               
         LA    R2,EDICT_TABLE_ENTRY                                             
         USING XMTTABLD,R2                                                      
         MVC   NUMDESTS,XMTDSTNO                                                
         DROP  R2                                                               
*                                                                               
         L     R3,ADSTAEFT                                                      
         USING DSTAEFTD,R3                                                      
         MVC   DSTAEDST,DESTINAT   1ST DESTINATION                              
         LH    R4,NUMDESTS         NUMBER OF DESTINATIONS                       
         B     SDMFX20                                                          
*                                                                               
SDMFX10  LA    R1,DMCB                                                          
         USING PQPARMD,R1                                                       
         LA    RE,EDICT_TABLE_ENTRY                                             
         ST    RE,ATBLNTRY                                                      
         MVC   APQBUFF,=A(CXREC)                                                
         MVC   APQHDR,=A(PQRPTHDR)                                              
         MVC   AEDHDR,=A(EDICTHDR)                                              
         MVC   APQLINE,=A(R)                                                    
         MVC   ACITABLE,CITABLE                                                 
         MVC   ADTAMGR,DATAMGR                                                  
         DROP  R1                                                               
         GOTO1 =V(PQGETLIN)                                                     
         BE    *+8                 MORE REPORT LINES TO BE READ                 
         B     SDMFX25             MARK REPORT UNSENDABLE                       
*                                                                               
         CLC   =C'++DDS',R+1       IS THIS LINE A CONTROL CARD?                 
         BNE   SDMFX10             NO -- NEXT LINE                              
         CLC   =C'DST',R+12        IS THIS DESTINATION CONTROL CARD?            
         BNE   SDMFX10             NO -- LINE NEXT                              
         MVC   DSTAEDST,R+16       DESTINATION                                  
*                                                                               
         CLI   BYTE2,1             IS THERE ONE SENDABLE DEST ALREADY?          
         BE    *+18                YES - SKIP CHECKING                          
SDMFX20  CLC   =C'FAX',DSTAEDST    IS DESTINATION A FAX MACHINE?                
         BNE   *+8                                                              
         MVI   BYTE2,1                                                          
*                                                                               
         LA    R3,DSTAEFTQ(R3)                                                  
         BCT   R4,SDMFX10          CONTINUE TO SEARCH NEXT DST CARD             
         DROP  R3                                                               
*                                                                               
         CLI   BYTE2,0             ALL DESTS ARE UNSENDABLE?                    
         BNE   SDMFX50             NO                                           
*                                                                               
SDMFX25  MVC   P+30(41),=C'NO FAX NUMBERS GIVEN FOR ALL DESTINATIONS'           
         MVC   P+11(13),=C'*** ERROR ***'                                       
         GOTO1 =V(PRINTER)                                                      
         MVI   ERRCODE,5           ERROR CODE                                   
*                                                                               
         PRNT  CANT_SEND_REPORT,PRINT=ALWAYS                                    
         LA    R2,EDICT_TABLE_ENTRY     THIS IS THE LAST DEST ENTRY             
         USING XMTTABLD,R2                                                      
         ZICM  R4,XMTDSTNO,2            LAST DEST #                             
*                                       MARK ALL DEST UNSENDABLE                
SDMFX30  STCM  R4,3,XMTDSTNO                                                    
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,XMTTABLE                                                
         LA    RE,EDICT_TABLE_ENTRY                                             
         ST    RE,ETBLNTRY                                                      
         MVI   EACTION,EACTJNKQ    MARK REPORT UNSENDABLE                       
         MVC   EERRORCD,ERRCODE                                                 
         MVC   EMAJORNM,=A(MAJORNAM)                                            
         MVC   EADTAMGR,DATAMGR                                                 
         GOTO1 =V(POSTSENT)                                                     
         BNE   SDMFX40                                                          
         DROP  R1,R2                                                            
*                                                                               
         MVC   P+30(16),=C'DESTINATION NO ='                                    
         EDIT  (R4),(5,P+46),ALIGN=LEFT,ZERO=NOBLANK                            
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
SDMFX40  BCT   R4,SDMFX30                                                       
         B     SDMFXBAD                                                         
*                                                                               
* FIND A(EDICTFILE_ENTRY) FOR EACH DESTINATION, NEEDED FOR BUILDING CID         
*                                                                               
SDMFX50  L     R3,ADSTAEFT         A(DST+A(EDIFILE_ENTRY) TABLE)                
         USING DSTAEFTD,R3                                                      
         LH    R4,NUMDESTS         NUMBER OF DESTINATIONS                       
*                                                                               
         MVC   WORK(L'EDICT_TABLE_ENTRY),EDICT_TABLE_ENTRY                      
         LA    R2,WORK             BUILD XMIT KEY FOR 1ST DEST                  
         USING XMTTABLD,R2                                                      
         MVC   XMTDSTNO,=H'1'      PQ RPT LOGICAL REPORT 1ST DEST               
         DROP  R2                                                               
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
         L     RF,XMTTABLE                                                      
         AHI   RF,-8                                                            
         L     RF,0(RF)            NUMBER OF ENTRIES IN TABLE                   
         ST    RF,DMCB+8                                                        
         GOTO1 =V(BINSRCH),DMCB,WORK,XMTTABLE,,XMTTBLQ,XMTKEYQ,0                
*                                                                               
         ICM   R9,15,DMCB          A(RECORD IN TABLE)                           
         TMH   R9,X'8000'          WAS RECORD FOUND?                            
         BNO   *+6                 YES                                          
         DC    H'0'                NO WAY THAT WILL FAIL!                       
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         USING XMTTABLD,R2                                                      
         LA    R2,XMTENTRY                                                      
*                                                                               
SDMFX60  MVC31 XMTENTRY,0(R9)                                                   
         MVC   DSTAEADR,XMTDSKAD   SAVE THE EDICT FILE ENTRY ADDRESS            
         AHI   R9,XMTTBLQ          NEXT DEST (THEY MUST BE TOGETHER)            
         LA    R3,DSTAEFTQ(R3)     NEXT TABLE ENTRY                             
         BCT   R4,SDMFX60          NEXT XMIT TABLE WITH DIFFERENT DEST          
         DROP  R2,R3                                                            
*                                                                               
         LA    R5,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R5)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(5),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
* STARTING SEND THE LEADING EMBEDDED CODES OF THE FAX                           
*                                                                               
         XC    BUFFER,BUFFER                                                    
         MVC   BUFFER(5),=C'#NTU#' SO WE GET NOTIFICATIONS                      
         MVI   BUFFER+5,X'5E'      SKIP OVER FAX DESCRIPTION                    
         MVC   BUFFER+6(8),LOCALLU PARTNER ALIAS (MY RECEIVER LUID)             
         MVI   BUFFER+14,X'5E'     SEMICOLON (DELIMITER)                        
         MVC   BUFFER+15(10),=C'DDSFAXGATE'                                     
         MVI   BUFFER+25,X'5E'     SEMICOLON (DELIMITER)                        
         MVI   BUFFER+26,X'5E'     SKIP OVER USER NAME                          
         MVC   BUFFER+27(4),=C'FORM'                                            
         MVI   TRACEONE,C'Y'                                                    
         LA    R5,31               L'RECORD                                     
         GOTO1 =A(FXLINXMT),(RC)                                                
         BNE   SDMFXRS                                                          
*                                                                               
         XC    BUFFER,BUFFER                                                    
         MVC   BUFFER(5),=C'#NCP#' NO COVER PAGE                                
         MVI   TRACEONE,C'Y'                                                    
         LA    R5,5                L'RECORD                                     
         GOTO1 =A(FXLINXMT),(RC)                                                
         BNE   SDMFXRS                                                          
*                                                                               
         XC    BUFFER,BUFFER                                                    
         MVC   BUFFER(5),=C'#PTY#' FAX PRIORITY                                 
         MVC   BUFFER+5(1),PRIORITY                                             
         MVI   TRACEONE,C'Y'                                                    
         LA    R5,6                L'RECORD                                     
         GOTO1 =A(FXLINXMT),(RC)                                                
         BNE   SDMFXRS                                                          
*                                                                               
         CLI   EDICTHDR+69,C'L'    LANDSCAPE ORIENTATION?                       
         BNE   SDMFX70                                                          
         XC    BUFFER,BUFFER                                                    
         MVC   BUFFER(5),=C'#LND#' YES                                          
         MVI   TRACEONE,C'Y'                                                    
         LA    R5,5                L'RECORD                                     
         GOTO1 =A(FXLINXMT),(RC)                                                
         BNE   SDMFXRS                                                          
*                                                                               
SDMFX70  XC    BUFFER,BUFFER                                                    
         MVC   BUFFER(5),=C'#FCC#' FAX CARBON COPY (BROADCAST FAXING)           
         MVI   TRACEONE,C'Y'                                                    
         LA    R5,5                L'RECORD                                     
         GOTO1 =A(FXLINXMT),(RC)                                                
         BNE   SDMFXRS                                                          
*                                                                               
         LA    R1,DMCB             REINTIALIZE THE EDICT FILE POINTER           
         USING PQPARMD,R1                                                       
         MVC   ATBLNTRY,=A(TABLNTRY)                                            
         MVC   APQBUFF,=A(CXREC)                                                
         MVC   APQHDR,=A(PQRPTHDR)                                              
         MVC   AEDHDR,=A(EDICTHDR)                                              
         MVC   APQLINE,=A(R)                                                    
         MVC   ACITABLE,CITABLE                                                 
         MVC   ADTAMGR,DATAMGR                                                  
         GOTO1 =V(INITPQRD)                                                     
         BE    *+6                                                              
         DC    H'0'                NO WAY IT WILL FAIL                          
         DROP  R1                                                               
*                                                                               
SDMFX80  LA     R1,DMCB                                                         
         USING PQPARMD,R1                                                       
         LA    RE,EDICT_TABLE_ENTRY                                             
         ST    RE,ATBLNTRY                                                      
         MVC   APQBUFF,=A(CXREC)                                                
         MVC   APQHDR,=A(PQRPTHDR)                                              
         MVC   AEDHDR,=A(EDICTHDR)                                              
         MVC   APQLINE,=A(R)                                                    
         MVC   ACITABLE,CITABLE                                                 
         MVC   ADTAMGR,DATAMGR                                                  
         DROP  R1                                                               
         GOTO1 =V(PQGETLIN)                                                     
         BE    *+6                 MORE REPORT LINES TO BE READ                 
         DC    H'0'                HOW CAN WE GET HERE?                         
*                                                                               
         CLC   =C'++DDS',R+1       IS THIS LINE A CONTROL CARD?                 
         BNE   SDMFX90             NO -- NEXT LINE                              
         CLC   =C'FXG',R+12        NO -- IS THIS FAXGATE LOGO DATA?             
         BNE   SDMFX80                                                          
*                                                                               
         CLI   PARTNER_MACHINE_ID,C'1' IS THIS THE ORDERS MACHINE?              
         BE    SDMFX80             YES -- FORMS MERGE NOT AUTHORIZED            
*                                      -- NEXT LINE                             
         XC    BUFFER,BUFFER       YES                                          
         MVC   BUFFER(5),=C'#FMF#' FORMS MERGE (ON EACH FORM FEED)              
         MVI   TRACEONE,C'Y'                                                    
         LA    R5,5                L'RECORD                                     
         GOTO1 =A(FXLINXMT),(RC)                                                
         BNE   SDMFXRS                                                          
*                                                                               
         XC    BUFFER,BUFFER                                                    
         MVC   BUFFER(5),=C'#FMB#' FORMS MERGE BEGIN                            
         MVC   BUFFER+5(12),R+16   FORM (.PCX) FILENAME                         
         MVI   BUFFER+17,X'5E'     SEMICOLON (DELIMITER)                        
         MVC   BUFFER+18(12),R+29  TEMPLATE (.TMP) FILENAME                     
         MVI   TRACEONE,C'Y'                                                    
         LA    R5,30               L'RECORD                                     
         GOTO1 =A(FXLINXMT),(RC)                                                
         BNE   SDMFXRS                                                          
         B     SDMFX80             NEXT LINE                                    
*                                                                               
* READ DST+A(EDIFILE_ENTRY) TABLE ONE AT A TIME AND BUILD #TEL# AND             
* #CID# TAG FOR FAXING.                                                         
*                                                                               
SDMFX90  L     R3,ADSTAEFT         A(DST+A(EDIFILE_ENTRY) TABLE)                
         USING DSTAEFTD,R3                                                      
         LH    R4,NUMDESTS         NUMBER OF DESTINATIONS                       
*                                                                               
         LA    R2,EDICT_TABLE_ENTRY   BUILD XMIT KEY FOR 1ST DEST               
         USING XMTTABLD,R2                                                      
         MVC   XMTDSTNO,=H'1'      PQ RPT LOGICAL REPORT 1ST DEST               
         MVC   XMTDSKAD,DSTAEADR   EDICT FILE ENTRY DISK ADDRESS                
         MVC   DESTINAT,DSTAEDST   DESTINATION STRING                           
         DROP  R2                                                               
*                                                                               
SDMFX100 CLC   =C'FAXW',DESTINAT                                                
         BNE   *+12                                                             
         LA    R2,DESTINAT+4                                                    
         B     SDMFX110                                                         
         CLC   =C'FAX',DESTINAT    IS DESTINATION A FAX MACHINE?                
         BNE   *+12                                                             
         LA    R2,DESTINAT+3                                                    
         B     SDMFX110                                                         
         MVC   P+30(19),=C'NO FAX NUMBER GIVEN'                                 
         MVI   ERRCODE,5           ERROR CODE                                   
*                                                                               
         MVC   P+11(13),=C'*** ERROR ***'                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
         MVC   EXMTTBLE,XMTTABLE                                                
         LA    RE,EDICT_TABLE_ENTRY                                             
         ST    RE,ETBLNTRY                                                      
         MVI   EACTION,EACTJNKQ    MARK REPORT UNSENDABLE                       
         MVC   EERRORCD,ERRCODE                                                 
         MVC   EMAJORNM,=A(MAJORNAM)                                            
         MVC   EADTAMGR,DATAMGR                                                 
         GOTO1 =V(POSTSENT)                                                     
         BNE   SDMFX140            COULD NOT POST EDICT FILE                    
         DROP  R1                                                               
         PRNT  RPTMARKEDJUNK,PRINT=ALWAYS                                       
         B     SDMFX140            GO TO NEXT DESTINATION                       
*                                                                               
SDMFX110 XC    BUFFER,BUFFER                                                    
         MVC   BUFFER(5),=C'#TEL#'                                              
         LA    RE,BUFFER+5         POINT RE WHERE PHONE NUMBER GOES             
*                                                                               
         CLI   DRTEST,C'Y'         DISASTER RECOVERY TEST MODE?                 
         BNE   *+18                IF SO, IGNORE APPLIC. FAX NUMBER             
         MVC   0(10,RE),=C'8457594610'                                          
*        MVC   0(10,RE),=C'2126335515'   2ND FLOOR DDS FAX TEL                  
         LA    R5,15               10-DIGIT NUMBER + 5 FOR #TEL#                
         B     SDMFX130                                                         
*                                                                               
         L     RF,=A(TSTIDTAB)     TABLE OF DDS TEST USERIDS                    
SDMFX120 CLC   USERID,0(RF)        IF USERID IS ONE OF OURS, THEN. . .          
         BNE   *+18                                                             
         MVC   0(10,RE),11(RF)     USE OUR FAX NUM (WITHOUT PREFIX 1)           
         LA    R5,15               10-DIGIT NUMBER + 5 FOR #TEL#                
         B     SDMFX130                                                         
         LA    RF,21(RF)                                                        
         CLI   0(RF),X'FF'                                                      
         BNE   SDMFX120                                                         
*                                                                               
         LA    RF,DESTINAT+L'DESTINAT                                           
         SR    RF,R2                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R2)       DESTINATION                                  
         LA    R5,BUFFER+30        R5=A(END OF LONGEST POSSIBLE STRING)         
         CLI   0(R5),C' '          END OF STRING?                               
         BH    *+10                                                             
         BCTR  R5,0                NO -- BACK UP                                
         B     *-10                                                             
         LA    R5,1(R5)                                                         
         LA    RF,BUFFER                                                        
*                                                                               
         CLC   1(10,RE),=C'8438529041'                                          
         BNE   SDMFX128                                                         
         MVC   0(4,R5),=C',,,,'                                                 
         AHI   R5,4                                                             
*                                                                               
SDMFX128 SR    R5,RF                                                            
*                                                                               
SDMFX130 MVI   TRACEONE,C'Y'                                                    
         GOTO1 =A(FXLINXMT),(RC)                                                
         BNE   SDMFXRS                                                          
*                                                                               
         LA    R2,EDICT_TABLE_ENTRY                                             
         USING XMTTABLD,R2         BUILD CUSTOMER ID                            
         MVI   CUSTID,C'4'         VERSION 4                                    
         CLI   DRTEST,C'Y'         DISASTER RECOVERY TEST MODE?                 
         BNE   *+8                                                              
         MVI   CUSTID,C'D'         YES -- VERSION 'D' (FOR 'D'ISASTER)          
         MVC   CUSTID+1(1),MAJORNAM+5  'A' FOR ADV, 'R' FOR REP                 
         LA    R1,DMCB                                                          
         USING CONVDSKD,R1                                                      
         MVC   CONVDMGR,DATAMGR    A(DATAMGR)                                   
         MVI   CONVACTN,CONVDSKF   CONVERT DSKADDR TO REFERENCE NUMBER          
         MVC   CONVDSK,XMTDSKAD    EDICT FILE DISK ADDRESS                      
         MVC   CONVOFF,=A(CUSTID+2)  RESULT GOES INTO CUSTID                    
         GOTO1 =V(CONVDSKA)                                                     
         DROP  R1,R2                                                            
         MVC   CUSTID+10(8),TODAY8 YYYYMMDD                                     
*                                                                               
         XC    BUFFER,BUFFER                                                    
         MVC   BUFFER(5),=C'#CID#' CUSTOMER ID                                  
         MVC   BUFFER+5(L'CUSTID),CUSTID                                        
         MVI   TRACEONE,C'Y'                                                    
         LA    R5,L'CUSTID+5       L'RECORD                                     
         GOTO1 =A(FXLINXMT),(RC)                                                
         BNE   SDMFXRS                                                          
*                                                                               
SDMFX140 LA    R2,EDICT_TABLE_ENTRY   BUILD XMIT KEY FOR NEXT DEST              
         USING XMTTABLD,R2                                                      
         ZICM  RE,XMTDSTNO,2       INCREMENT DEST #                             
         LA    RE,1(RE)                                                         
         STCM  RE,3,XMTDSTNO                                                    
         LA    R3,DSTAEFTQ(R3)     NEXT DEST AND EDIFILE ENTRY DISK ADR         
         MVC   XMTDSKAD,DSTAEADR   EDICT FILE ENTRY DISK ADDRESS                
         MVC   DESTINAT,DSTAEDST   DESTINATION STRING                           
         DROP  R2,R3                                                            
         BCT   R4,SDMFX100                                                      
*                                                                               
*                                                                               
         CLI   DRTEST,C'Y'         DISASTER RECOVERY TEST MODE?                 
         BNE   SDMFX150                                                         
         MVI   BUFFER,C'*'                                                      
         MVC   BUFFER+1(49),BUFFER FILL BUFFER WITH ASTERISKS                   
         LA    R5,50                                                            
         MVI   TRACEONE,C'Y'                                                    
         GOTO1 =A(FXLINXMT),(RC)                                                
         BNE   SDMFXRS                                                          
         XC    BUFFER,BUFFER                                                    
         SR    R5,R5                                                            
         MVI   TRACEONE,C'N'                                                    
         GOTO1 =A(FXLINXMT),(RC)                                                
         BNE   SDMFXRS                                                          
         MVC   BUFFER(50),=C'THIS IS A TEST FAX TRANSMISSION - PLEASE D+        
               ISREGARD'                                                        
         LA    R5,50                                                            
         MVI   TRACEONE,C'Y'                                                    
         GOTO1 =A(FXLINXMT),(RC)                                                
         BNE   SDMFXRS                                                          
         XC    BUFFER,BUFFER                                                    
         SR    R5,R5                                                            
         MVI   TRACEONE,C'N'                                                    
         GOTO1 =A(FXLINXMT),(RC)                                                
         BNE   SDMFXRS                                                          
         MVI   BUFFER,C'*'                                                      
         MVC   BUFFER+1(49),BUFFER FILL BUFFER WITH ASTERISKS                   
         LA    R5,50                                                            
         MVI   TRACEONE,C'Y'                                                    
         GOTO1 =A(FXLINXMT),(RC)                                                
         BNE   SDMFXRS                                                          
         XC    BUFFER,BUFFER                                                    
         SR    R5,R5                                                            
         MVI   TRACEONE,C'N'                                                    
         GOTO1 =A(FXLINXMT),(RC)                                                
         BNE   SDMFXRS                                                          
*                                                                               
SDMFX150 CLI   R,X'89'             PAGE EJECT?                                  
         BNE   *+14                                                             
         CLC   R+1(L'R-1),MYSPACES                                              
         BE    *+12                                                             
         MVI   LASTEJCT,C'N'                                                    
         B     SDMFX170            THIS IS NOT A PAGE EJECT                     
*                                                                               
         CLI   LASTEJCT,C'Y'                                                    
         BE    SDMFX210                                                         
         LH    RF,NUMPAGES         INCREMENT PAGE COUNTER                       
         LA    RF,1(RF)                                                         
         STH   RF,NUMPAGES                                                      
         SR    RE,RE               PREPARE FOR DIVIDE                           
         D     RE,WARNPAGE                                                      
         LTR   RE,RE               WARN CONSOLE EVERY N PAGES                   
         BNZ   SDMFX160                                                         
*                                                                               
         MVC   PAGEMSG+35(8),USERID    PQ USERID                                
         LA    RE,EDICT_TABLE_ENTRY                                             
         USING XMTTABLD,RE                                                      
         MVC   PAGEMSG+44(3),XMTSUBID  PQ SUBID                                 
         EDIT  XMTREFNO,(5,PAGEMSG+48),ALIGN=LEFT                               
         DROP  RE                                                               
         EDIT  NUMPAGES,(4,PAGEMSG+72),ALIGN=LEFT                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'PAGEMSG,PAGEMSG)                   
*                                                                               
SDMFX160 MVI   LASTEJCT,C'Y'       LAST LINE WAS A #FFP#                        
         MVI   R,X'09'                                                          
         MVC   R+1(5),=C'#FFP#'                                                 
*                                                                               
SDMFX170 LA    R4,79               FIND END OF TEXT (MAX 80 CHARACTERS)         
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
         BNZ   *+16                                                             
         MVI   BUFFER,C' '                                                      
         LA    R4,1                                                             
         B     SDMFX180            NO -- JUST SEND ONE BLANK                    
*                                                                               
         BCTR  R4,0                                                             
         EX    R4,*+8              MOVE IN LINE                                 
         B     *+10                                                             
         MVC   BUFFER(0),R+1                                                    
         LA    R4,1(R4)            RESTORE LENGTH OF DATA                       
*                                                                               
SDMFX180 LR    R5,R4                                                            
         MVI   TRACEONE,C'N'                                                    
         GOTO1 =A(FXLINXMT),(RC)                                                
         BNE   SDMFXRS                                                          
*                                                                               
         CLI   R,X'19'             TRIPLE SPACE IF NECESSARY                    
         BNE   SDMFX190                                                         
         XC    BUFFER,BUFFER                                                    
         MVI   BUFFER,C' '                                                      
         MVI   TRACEONE,C'N'                                                    
         LA    R5,1                                                             
         GOTO1 =A(FXLINXMT),(RC)                                                
         BNE   SDMFXRS                                                          
         B     SDMFX200                                                         
*                                                                               
SDMFX190 CLI   R,X'11'             DOUBLE SPACE IF NECESSARY                    
         BNE   SDMFX210                                                         
SDMFX200 XC    BUFFER,BUFFER                                                    
         MVI   BUFFER,C' '                                                      
         MVI   TRACEONE,C'N'                                                    
         LA    R5,1                                                             
         GOTO1 =A(FXLINXMT),(RC)                                                
         BNE   SDMFXRS                                                          
*                                                                               
SDMFX210 LA    R1,DMCB                                                          
         USING PQPARMD,R1                                                       
         LA    RE,EDICT_TABLE_ENTRY                                             
         ST    RE,ATBLNTRY                                                      
         MVC   APQBUFF,=A(CXREC)                                                
         MVC   APQHDR,=A(PQRPTHDR)                                              
         MVC   AEDHDR,=A(EDICTHDR)                                              
         MVC   APQLINE,=A(R)                                                    
         MVC   ACITABLE,CITABLE                                                 
         MVC   ADTAMGR,DATAMGR                                                  
         GOTO1 =V(PQGETLIN)                                                     
         BNE   SDMFXOK             NO MORE REPORT LINES                         
         DROP  R1                                                               
         B     SDMFX150                                                         
*                                                                               
SDMFXRS  LA    R2,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R2)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(2),E,8)  ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         L     RF,XMTTABLE         SEARCH THE LAST DEST LOGICAL REP             
         AHI   RF,-8                                                            
         L     RF,0(RF)            NUMBER OF ENTRIES IN TABLE                   
         ST    RF,DMCB+8                                                        
         GOTO1 =V(BINSRCH),DMCB,TABLNTRY,XMTTABLE,,XMTTBLQ,XMTKEYQ,0            
*                                                                               
         ICM   R2,15,DMCB          A(RECORD IN TABLE)                           
         TMH   R2,X'8000'          WAS RECORD FOUND?                            
         BNO   *+6                 YES                                          
         DC    H'0'                NO WAY THAT WILL FAIL!                       
*                                                                               
         MVC31 XMTENTRY,0(R2)                                                   
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         LA    R3,XMTENTRY         A(RECORD IN TABLE)                           
         USING XMTTABLD,R3                                                      
         NI    XMTFLAGS,X'FF'-XMTTEMPQ  TRY LOOK AT THIS ONE AGAIN              
         MVC31 0(L'XMTENTRY,R2),XMTENTRY                                        
         DROP  R3                                                               
         XC    EDICT_TABLE_ENTRY,EDICT_TABLE_ENTRY                              
*                                                                               
         LA    R2,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R2)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(2),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
SDMFXBAD LTR   RB,RB               NO GOOD -- SET CC NOT EQUAL                  
         B     SDMFXXIT                                                         
*                                                                               
SDMFXOK  MVC   EDICT_TABLE_ENTRY,TABLNTRY   RESTORE THE LAST DEST REP           
         CR    RB,RB               SET CC EQUAL                                 
*                                                                               
SDMFXXIT XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB,R8                                                            
         EJECT                                                                  
FXLINXMT NMOD1 0,FXLINXMT                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* ON ENTRY, R5 = LENGTH OF DATA                                                 
*                                                                               
         ST    R5,SEND_LENGTH                                                   
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
*                                                                               
         CLI   TRACEONE,C'Y'                                                    
         BE    *+12                FORCE PRINTING OF LINE                       
         CLI   TRACEFLG,C'Y'                                                    
         BNE   FXLINX10                                                         
         GOTO1 =V(PRNTBL),DMCB,=C'BUFFER DATA',BUFFER,C'DUMP',         +        
               (R5),=C'1D'                                                      
*                                                                               
FXLINX10 LA    R2,ATBSEND_STRUCTURE                                             
         GOTO1 =A(CALLAPPC),(RC)                                                
*                                                                               
         WAIT  ECB=APPC_ECB        WAIT FOR COMPLETION                          
*                                                                               
         TM    APPC_ECB,X'40'                                                   
         BO    *+6                                                              
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
         MVC   RETURN_CODE,APPC_ECB  RETURN CODE IS IN ECB                      
         MVI   RETURN_CODE,0       CLEAR HOB                                    
         XC    APPC_ECB,APPC_ECB   CLEAR ECB                                    
*                                                                               
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         PRNT  PRINT=ALWAYS                                                     
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    FXLINXOK            SEND_DATA COMPLETED OK                       
         GOTO1 =A(APPCERR),(RC)    CALL ERROR_EXTRACT ROUTINE                   
         MVC   P+30(16),=C'BAD FAXGATE SEND'                                    
         PRNT  CANTTRANSMIT,PRINT=ALWAYS                                        
         MVC   WORK(66),=C'EDICT01 *CANNOT SEND TO FAXGATE XXXXXXXX - T+        
               RANSMISSIONS SUSPENDED'                                          
         MVC   WORK+32(8),PARTNER_LU_NAME                                       
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(66,WORK)                             
         OI    INTERNAL_CONVERSATION_STATUS,COMMUNICATIONS_ERROR                
*                                                                               
         LTR   RB,RB               NO GOOD -- SET CC NOT EQUAL                  
         B     FXLINXX                                                          
*                                                                               
FXLINXOK CR    RB,RB               SET CC EQUAL                                 
*                                                                               
FXLINXX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
PRNTATTB NMOD1 0,PRNTATTB                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PRINT DETAILS ABOUT THE CURRENT STATE OF THE LU6.2 CONVERSATION               
*                                                                               
         PRNT  *************,PRINT=ALWAYS                                       
         MVC   P+68(7),=C'MACHINE'                                              
         MVC   P+76(1),PARTNER_MACHINE_ID                                       
         MVI   P+77,C'/'                                                        
         MVC   P+78(1),CONVERSATION_FUNCTION                                    
         PRNT  GETATTRIBUTES,PRINT=ALWAYS                                       
         PRNT  *************,PRINT=ALWAYS                                       
*                                                                               
         LA    R2,ATBGTA2_STRUCTURE                                             
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
         MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODE           
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         PRNT  PRINT=ALWAYS                                                     
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    GTA2OK              RETURN CODE WAS OK                           
         GOTO1 =A(APPCERR),(RC)    CALL ERROR_EXTRACT ROUTINE                   
         DC    H'0'                                                             
*                                                                               
GTA2OK   GOTO1 =V(HEXOUT),DMCB,CONVERSATION_ID,P+30,8,=C'TOG'                   
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
         DROP  RB                                                               
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
         WAIT  ECB=APPC_ECB        WAIT FOR COMPLETION                          
*                                                                               
         TM    APPC_ECB,X'40'                                                   
         BO    *+6                                                              
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
         MVC   RETURN_CODE,APPC_ECB  RETURN CODE IS IN ECB                      
         MVI   RETURN_CODE,0       CLEAR HOB                                    
         XC    APPC_ECB,APPC_ECB   CLEAR ECB                                    
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
         DROP  RB                                                               
         EJECT                                                                  
CALLAPPC NMOD1 0,CALLAPPC                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* UPON ENTRY,  R2 POINTS TO PARAMETER STRUCTURE                                 
*                                                                               
         MVC   P(16),=C'ABOUT TO PERFORM'                                       
         MVC   P+30(16),4(R2)      PRINT ROUTINE NAME                           
         GOTO1 =V(HEXOUT),DMCB,CONVERSATION_ID,P+50,8,=C'TOG'                   
         MVC   P+68(7),=C'MACHINE'                                              
         MVC   P+76(1),PARTNER_MACHINE_ID                                       
         MVI   P+77,C'/'                                                        
         MVC   P+78(1),CONVERSATION_FUNCTION                                    
         PRNT  PRINT=ALWAYS                                                     
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
         CLC   RETURN_CODE,ATB_OK                                               
         BE    CALLX               APPC/MVS CALL WAS ACCEPTED                   
*                                                                               
         MVC   P+30(16),4(R2)      PRINT NAME OF APPC/MVS ROUTINE               
         MVC   P+50(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+64),ALIGN=LEFT                                  
         PRNT  **BAD_APPC_CALL**,PRINT=ALWAYS                                   
         TM    20(R2),X'40'        CAN CALL ERROR_EXTRACT NOW?                  
         BO    *+6                                                              
         DC    H'0'                NO -- NOTHING ELSE TO DO                     
         GOTO1 =A(APPCERR),(RC)    YES - DO IT                                  
         DC    H'0'                APPC/MVS CALL NOT ACCEPTED                   
*                                                                               
CALLX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
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
         DROP  RB                                                               
         EJECT                                                                  
DEALALL  NMOD1 0,DEALALL                                                        
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         L     R6,AAPPCTAB         APPC/MVS CONTROL TABLE                       
LOOP     CLI   CONVERSATION_FUNCTION,SENDER                                     
         BNE   DEALRCVR                                                         
*                                                                               
         GOTO1 =A(DEALLOC),(RC)    DEALLOCATE SENDING CONVERSATION              
         B     NEXT                                                             
*                                                                               
DEALRCVR CLI   CONVERSATION_FUNCTION,RECEIVER                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =A(DEALUNRG),(RC)   DEALLOCATE/ABEND AND UNREGISTER              
*                                                                               
NEXT     AH    R6,=Y(APPCDLEN)     BUMP TO NEXT CONVERSATION                    
         CLC   EOTMARK,0(R6)       END OF TABLE?                                
         BNE   LOOP                                                             
*                                                                               
DEALALLX XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
DEALLOC  NMOD1 0,DEALLOC                                                        
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         TM    INTERNAL_CONVERSATION_STATUS,ALLOCATED                           
         BZ    DEALX               CONVERSATION WAS NEVER ALLOCATED             
*                                                                               
         LA    R2,ATBDEAL_STRUCTURE                                             
         MVC   P(16),=C'ABOUT TO PERFORM'                                       
         MVC   P+30(16),4(R2)      PRINT ROUTINE NAME                           
         GOTO1 =V(HEXOUT),DMCB,CONVERSATION_ID,P+50,8,=C'TOG'                   
         MVC   P+68(7),=C'MACHINE'                                              
         MVC   P+76(1),PARTNER_MACHINE_ID                                       
         MVI   P+77,C'/'                                                        
         MVC   P+78(1),CONVERSATION_FUNCTION                                    
         PRNT  PRINT=ALWAYS                                                     
*                                                                               
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_ECB                                  
         MVC   DEALLOCATE_TYPE,ATB_DEALLOCATE_FLUSH                             
*        MVC   DEALLOCATE_TYPE,ATB_DEALLOCATE_SYNC_LEVEL                        
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
         CLC   RETURN_CODE,ATB_OK                                               
         BE    DEAL10              APPC/MVS CALL WAS ACCEPTED                   
         MVC   P+30(16),4(R2)      PRINT NAME OF APPC/MVS ROUTINE               
         MVC   P+50(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+64),ALIGN=LEFT                                  
         PRNT  **BAD_APPC_CALL**,PRINT=ALWAYS                                   
         B     DEAL20              APPC/MVS CALL NOT ACCEPTED                   
*                                                                               
DEAL10   WAIT  ECB=APPC_ECB        WAIT FOR COMPLETION                          
*                                                                               
         TM    APPC_ECB,X'40'                                                   
         BO    *+6                                                              
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
         MVC   RETURN_CODE,APPC_ECB                                             
         MVI   RETURN_CODE,0       CLEAR HIGH-ORDER BYTE                        
         XC    APPC_ECB,APPC_ECB                                                
*                                                                               
DEAL20   MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODE           
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         PRNT  PRINT=ALWAYS                                                     
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
DEALX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
DEALUNRG NMOD1 0,DEALUNRG                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
         TM    INTERNAL_CONVERSATION_STATUS,ALLOCATED                           
         BZ    DU20                CONVERSATION WAS NEVER ALLOCATED             
         TM    INTERNAL_CONVERSATION_STATUS,PARTNER_DEALLOCATED                 
         BO    DU20                YES, AND WE'VE ALREADY CONFIRMED IT          
*                                                                               
         LA    R2,ATBDEAL_ABEND_STRUCTURE                                       
         MVC   P(16),=C'ABOUT TO PERFORM'                                       
         MVC   P+30(16),4(R2)      PRINT ROUTINE NAME                           
         GOTO1 =V(HEXOUT),DMCB,CONVERSATION_ID,P+50,8,=C'TOG'                   
         MVC   P+68(7),=C'MACHINE'                                              
         MVC   P+76(1),PARTNER_MACHINE_ID                                       
         MVI   P+77,C'/'                                                        
         MVC   P+78(1),CONVERSATION_FUNCTION                                    
         PRNT  PRINT=ALWAYS                                                     
*                                                                               
         MVC   NOTIFY_TYPE_DEALLOCATE,ATB_NOTIFY_TYPE_ECB                       
         MVC   DEALLOCATE_TYPE,ATB_DEALLOCATE_ABEND                             
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
         CLC   RETURN_CODE_DEALLOCATE,ATB_OK                                    
         BE    DU10                APPC/MVS CALL WAS ACCEPTED                   
         MVC   P+30(16),4(R2)      PRINT NAME OF APPC/MVS ROUTINE               
         MVC   P+50(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE_DEALLOCATE,(5,P+64),ALIGN=LEFT                       
         PRNT  **BAD_APPC_CALL**,PRINT=ALWAYS                                   
         DC    H'0'                APPC/MVS CALL NOT ACCEPTED                   
*                                                                               
DU10     WAIT  ECB=APPC_ECB_DEALLOCATE   WAIT FOR COMPLETION                    
*                                                                               
         TM    APPC_ECB_DEALLOCATE,X'40'                                        
         BO    *+6                                                              
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
         MVC   RETURN_CODE_DEALLOCATE,APPC_ECB_DEALLOCATE                       
         MVI   RETURN_CODE_DEALLOCATE,0   CLEAR HIGH-ORDER BYTE                 
         XC    APPC_ECB_DEALLOCATE,APPC_ECB_DEALLOCATE                          
*                                                                               
         MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODE           
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE_DEALLOCATE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK          
         PRNT  PRINT=ALWAYS                                                     
*                                                                               
         OC    RETURN_CODE_DEALLOCATE,RETURN_CODE_DEALLOCATE                    
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         WAIT  ECB=APPC_ECB        WAIT FOR COMPLETION OF PREVIOUS CALL         
*                                                                               
         TM    APPC_ECB,X'40'                                                   
         BO    *+6                                                              
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
         MVC   RETURN_CODE,APPC_ECB                                             
         MVI   RETURN_CODE,0       CLEAR HIGH-ORDER BYTE                        
         XC    APPC_ECB,APPC_ECB                                                
*                                                                               
         MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
         MVC   P+1(16),=C'OUTSTANDING CALL'                                     
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         PRNT  PRINT=ALWAYS                                                     
*                                                                               
DU20     OC    ALLOCATE_QUEUE_TOKEN,ALLOCATE_QUEUE_TOKEN                        
         BZ    DUX                 WE NEVER REGISTERED FOR ALLOCATES            
*                                                                               
         LA    R2,ATBURA2_STRUCTURE                                             
         MVC   P(16),=C'ABOUT TO PERFORM'                                       
         MVC   P+30(16),4(R2)      PRINT ROUTINE NAME                           
         GOTO1 =V(HEXOUT),DMCB,CONVERSATION_ID,P+50,8,=C'TOG'                   
         MVC   P+68(7),=C'MACHINE'                                              
         MVC   P+76(1),PARTNER_MACHINE_ID                                       
         MVI   P+77,C'/'                                                        
         MVC   P+78(1),CONVERSATION_FUNCTION                                    
         PRNT  PRINT=ALWAYS                                                     
*                                                                               
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_ECB                                  
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
         CLC   RETURN_CODE,ATB_OK                                               
         BE    DU30                APPC/MVS CALL WAS ACCEPTED                   
         MVC   P+30(16),4(R2)      PRINT NAME OF APPC/MVS ROUTINE               
         MVC   P+50(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+64),ALIGN=LEFT                                  
         PRNT  **BAD_APPC_CALL**,PRINT=ALWAYS                                   
         DC    H'0'                APPC/MVS CALL NOT ACCEPTED                   
*                                                                               
DU30     WAIT  ECB=APPC_ECB        WAIT FOR COMPLETION                          
*                                                                               
         TM    APPC_ECB,X'40'                                                   
         BO    *+6                                                              
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
         MVC   RETURN_CODE,APPC_ECB                                             
         MVI   RETURN_CODE,0       CLEAR HIGH-ORDER BYTE                        
         XC    APPC_ECB,APPC_ECB                                                
*                                                                               
         MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODE           
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         PRNT  PRINT=ALWAYS                                                     
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DUX      XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
COMMWORK DS    0D                  COMMON STORAGE AREA                          
         ATBSERV                                                                
         SPACE 2                                                                
         ATBCTASM                                                               
         SPACE 2                                                                
         GETEL R4,28,ELCODE                                                     
         SPACE 2                                                                
MYSPACES DC    CL256' '                                                         
PAGEMSG  DC    C'EDICT01 *FAXGATE WARNING: LONG FAX XXXXXXXX,XXX,XXXXX,+        
                NOW SENDING PAGE NNNN'                                          
DMWORK   DS    12D                                                              
DMCB     DS    20F                                                              
DUB      DS    D                                                                
PRNTDUB  DS    D                                                                
FULL     DS    F                                                                
NUMDESTS DS    H                   NUMBER OF DEST FOR A LOGICAL REPORT          
MAXDESTS DS    H                   MAXINUM OF DESTINATION/REPORT                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
PRNTTIME DS    CL9                                                              
WORK     DS    CL256                                                            
TODAY8   DS    CL8                 C'YYYYMMDD'                                  
DATAMGR  DS    V                   A(DATAMGR)                                   
CITABLE  DS    V                   A(CITABLE)                                   
STOPECB  DS    A                   A(ECB) TO STOP SUBTASK                       
LOOKECB  DS    A                   A(ECB) TO TELL SUBTASK TO SEARCH             
XMTTABLE DS    A                   A(REPORT TABLE)                              
DSTTABLE DS    A                   A(DESTINATION TABLE)                         
ADSTAEFT DS    A                   A(DST+A(EDIFILE_ENTRY) TABLE)                
ELCODE   DS    X                                                                
OPERSTOP DC    C'N'                'Y' IF OPERATOR WANTS TO STOP                
TRACEFLG DS    C                   'Y' TO PRINT DETAILED TRACE                  
TRACEONE DS    C                   'Y' TO FORCE TRACE OF THIS LINE ONLY         
DONTSEND DC    C'N'                'Y' TO PREVENT SENDING TO FAXGATE PC         
DRTEST   DS    C                   'Y' = DISASTER RECOVERY TEST MODE            
ERRCODE  DS    X                   ERROR REASON CODE                            
NUMPCS   DS    F                   NUMBER OF FAXGATE PCS                        
WARNPAGE DC    F'20'               WARN CONSOLE EVERY N PAGES                   
NUMPAGES DS    H                   PAGE COUNTER FOR FAX TRANSMISSION            
USERFILT DC    H'0'                ONLY XMIT REPORTS FOR THIS USERID            
EOTMARK  DC    X'FFFFFFFF'         END OF APPC/MVS CONTROL TABLE                
LOCALLU  DS    CL8                 LOCAL LUNAME                                 
VTAMMODE DS    CL8                 VTAM MODE NAME                               
XMTENTRY DS    XL(XMTTBLQ)         TEMP STORAGE FOR 1 XIMTABLE ENTRY            
PARTLUS  DS    (MAX_FAXGATE_PARTNERS)CL(PARTLULQ) PARTNER LUNAME TABLE          
*                                                                               
AAPPCTAB DS    A                   A(APPC CONTROL TABLE)                        
AECBLIST DS    A                   A(ECBLIST)                                   
AATBALC2 DS    A                   A(APPC/MVS ALLOCATE)                         
AATBDEAL DS    A                   A(APPC/MVS DEALLOCATE)                       
AATBSEND DS    A                   A(APPC/MVS SEND_DATA)                        
AATBRCVW DS    A                   A(APPC/MVS RECEIVE_AND_WAIT)                 
AATBCFM  DS    A                   A(APPC/MVS CONFIRM)                          
AATBCFMD DS    A                   A(APPC/MVS CONFIRMED)                        
AATBGTA2 DS    A                   A(APPC/MVS GET_ATTRIBUTES)                   
AATBEES3 DS    A                   A(APPC/MVS ERROR_EXTRACT)                    
AATBQAQ2 DS    A                   A(APPC/MVS QUERY_ALLOATE_QUEUE)              
AATBRAL2 DS    A                   A(APPC/MVS RECEIVE_ALLOCATE)                 
AATBRFA2 DS    A                   A(APPC/MVS REGISTER_FOR_ALLOCATES)           
AATBURA2 DS    A                   A(APPC/MVS UNREGISTER_FOR_ALLOCATES)         
         SPACE 2                                                                
TABLNTRY DS    XL(XMTTBLQ)         SAVED XMIT TABLE ENTRY                       
MAJORNAM DS    CL8                 MAJOR RESOURCE NAME                          
USERID   DS    CL10                ALPHA USERID                                 
CUSTID   DS    CL18                VEDDNNNNNNYYYYMMDD                           
DESTINAT DS    CL25                                                             
DELDAY   DS    X                   MESSAGE DELIVERY DAY -- PWOS                 
DELTIME  DS    XL2                 MESSAGE DELIVERY TIME (HM) -- PWOS           
DELPERD  DC    C'YYYYMMDD-YYYYMMDD'  FOR PERVAL: SEND DATE - TODAY              
LASTEJCT DS    C                   'Y' = LAST LINE PRINTED WAS #FFP#            
PRIORITY DS    C                   'H' = HIGH, 'M' = MEDIUM, 'L' = LOW          
KEY      DS    XL25                CTFILE KEY                                   
CARD     DS    CL80                FOR CONTROL CARDS AND EDICTFIL RECS          
         DS    XL4                 PQ RECORD LENGTH                             
R        DS    CL256               PQ RECORD DATA                               
         DS    XL4                                                              
EDICTHDR DS    CL256               EDICT HDR* RECORD                            
         DS    XL4                                                              
PQRPTHDR DS    CL256               PQ REPORT HEADER                             
         EJECT                                                                  
ENTRYPTS DS    0F                                                               
         DC    Y((ENTRYLSQ-ENTRYSTQ)/60)   NUMBER OF TABLE ENTRIES              
         DC    H'60'                       MUST REMAIN AS 60                    
ENTRYSTQ EQU   *                                                                
*                                                                               
ATBALC2  DC    CL8'ATBALC2'                                                     
         DC    XL52'00'                                                         
ATBCFM   DC    CL8'ATBCFM'                                                      
         DC    XL52'00'                                                         
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
         SPACE 3                                                                
         DS    0D                                                               
         DC    C'*CXREC**'                                                      
CXREC    DC    14336X'00'          PRINT QUEUE INDEX BUFFER                     
*                                                                               
         DS    0D                                                               
         DC    C'**I/O***'                                                      
IO       DC    1000X'00'           CTFILE I/O BUFFER                            
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
         EJECT                                                                  
       ++INCLUDE DDEDITSTID                                                     
         EJECT                                                                  
       ++INCLUDE DDEDIFGWRK                                                     
         EJECT                                                                  
DSTAEFTD DSECT                                                                  
DSTAEDST DS    CL(L'DESTINAT)      FAX DESTINATION                              
DSTAEADR DS    XL4                 A(EDICT FILE ENTRY) OF THIS DST              
DSTAEFTQ EQU   *-DSTAEFTD                                                       
PARTLUSD DSECT                                                                  
PARTID   DS    C                   ID NO. (ORDERS=1, CONTRACT=2, ETC)           
PARTLU   DS    CL8                 MACHINE LUNAME                               
PARTFLAG DS    X                                                                
PARTNOOP EQU   X'80'               THIS MACHINE IS NON-OPERATIONAL              
PARTTRAC EQU   X'40'               TRACE ALL ACTIVITY FOR THIS MACHINE          
PARTLULQ EQU   *-PARTLUSD                                                       
         SPACE 5                                                                
LOGRECD  DSECT                                                                  
LOGIDENT DS    CL6                 MUST ALWAYS BE C'FGCICS'                     
LOGVER   DS    CL2                 PACKET VERSION NUMBER (C'01' NOW)            
LOGTYPE  DS    CL2                 PACKET TYPE                                  
LOGUSER  DS    CL30                USER-DEFINED TEXT                            
LOGTMZON DS    CL6                 TIME-ZONE DIFFERENCE FROM GMT                
LOGDATE  DS    CL8                 TRANSMISSION DATE (MM/DD/YY)                 
LOGTIME  DS    CL5                 TRANSMISSION TIME (HH:MM)                    
LOGJID   DS    CL6                 JOB ID                                       
LOGCID   DS    CL25                CUSTOMER IDENTIFICATION                      
LOGSTAT  DS    CL4                 TRANSMISSION STATUS                          
LOGDUR   DS    CL8                 DURATION OF TRANSMISSION                     
LOGPAGES DS    CL3                 NUMBER OF PAGES IN TRANSMISSION              
LOGFORM  DS    CL4                 FORM NAME                                    
LOGLAST  DS    CL1                 'Y' = LAST LOG RECORD FOR THIS JOB           
LOGDEST  DS    CL32                RECIPIENT NAME/ADDRESS (FROM #DST#)          
LOGFAXNO DS    CL32                RECIPIENT FAX NUMBER                         
LOGSENDR DS    CL32                SENDER NAME (FROM #SND#)                     
         EJECT                                                                  
       ++INCLUDE DDEDICTWRK                                                     
         EJECT                                                                  
* DDDPRINT                                                                      
* DDEDICTFIL                                                                    
* DDPERVALD                                                                     
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DDEDICTFIL                                                     
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'080DDEDIFAX  03/16/14'                                      
         END                                                                    
