*          DATA SET DDEDIBDFR  AT LEVEL 007 AS OF 08/12/05                      
*PHASE EDIBDFRA                                                                 
*INCLUDE BINSR31                                                                
*INCLUDE CARDS                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE EDISUB                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE LOGIO                                                                  
*INCLUDE PERVAL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        EDIMQMR -- RECEIVE NOTIFICATIONS FROM BDE VIA MQ     *         
*                                                                     *         
*  COMMENTS:     ATTACHED BY DDEDIBDES                                *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- WORK                                           *         
*                R7 -- COMMON STORAGE AREA (2ND BASE)                 *         
*                R8 -- WORK                                           *         
*                R9 -- S P A R E                                      *         
*                RA -- CPRINT                                         *         
*                RB -- PROGRAM BASE                                   *         
*                RC -- COMMON STORAGE AREA                            *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'DDEDIBDFR -- RECEIVE BDE NOTIFICATIONS VIA MQSERIES'            
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
EDIMQMR  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*EDIBDFR,=A(R13CHAIN)                                          
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
         USING BDRPARMD,R5                                                      
*                                                                               
         LA    RF,BDRSTOP                                                       
         STCM  RF,7,ASTOPECB+1     SET A(STOPPING ECB)                          
         MVC   AXMTTBL,BDRXMTTB    A(TRANSMIT TABLE)                            
         MVC   DATAMGR,BDRADMGR    A(DATAMGR)                                   
         MVC   TRACEFLG,BDRTRACE   'Y' = PRINT DETAILED TRACE                   
         ENTRY TRACEFLG                                                         
*                                                                               
         L     RF,BDRQMGNM         MQ SERIES QUEUE MANAGER NAME                 
         MVC   QMGRNAME,0(RF)                                                   
         L     RF,BDRMQQNM         MQ SERIES QUEUE NAME                         
         MVC   OBJDESC_OBJECTNAME,0(RF)                                         
         L     RF,BDRMAJNM                                                      
         MVC   MAJORNAM,0(RF)      MAJOR RESOURCE NAME                          
         DROP  R5                                                               
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         L     RF,=V(PRNTER)       A(SYSPRINT DCB)                              
         MVC   DCBDDNAM-IHADCB(8,RF),=C'BFRTRACE'  DDNAME=BFRTRACE              
*                                                                               
         MVC   TITLE(33),=C'EDICT: RECEIVING SUBTASK FROM BDF'                  
         PRNT  START_SUBTASK,PRINT=ALWAYS                                       
*                                                                               
         BRAS  RE,INITIAL                                                       
*                                                                               
         LA    R2,CSQBCONN_STRUCTURE                                            
         BRAS  RE,CALLMQ           CONNECT TO MQ QUEUE MANAGER                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETBUFF  BRAS  RE,MQBUFF           GET BDE NOTIFICAITIONS                       
*                                                                               
         CLI   OPERSTOP,C'Y'       DO WE STOP NOW?                              
         BE    MQDISC              YES -- CLOSE COMMUNICATIONS                  
*                                                                               
         BRAS  RE,GETFIELD                                                      
         BNE   GETBUFF                                                          
*                                                                               
         BRAS  RE,CHKBUFF          CHECK BDE NOTIFICATION                       
         B     GETBUFF             GET NEXT MESSAGE                             
*                                                                               
MQDISC   LA    R2,CSQBDISC_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     DISCONNECT FROM MQ QUEUE MANAGER             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GOODBYE  PRNT  EXITING,PRINT=ALWAYS                                             
*                                                                               
         XBASE                                                                  
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
INITIAL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         PRNT  INITIALIZE,PRINT=ALWAYS                                          
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(20,TODAY8)                                
         MVC   DELPERD+9(8),TODAY8 FOR PERVAL                                   
*                                                                               
         BLDL  0,ENTRYPTS          BUILD LIST OF EXTERNAL ENTRY PTS             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                BAD RETURN FROM BLDL MACRO                   
*                                                                               
         LOAD  DE=CSQBCONN                                                      
         ST    R0,CSQBCONN_STRUCTURE                                            
         LOAD  DE=CSQBOPEN                                                      
         ST    R0,CSQBOPEN_STRUCTURE                                            
         LOAD  DE=CSQBGET                                                       
         ST    R0,CSQBGET_STRUCTURE                                             
         LOAD  DE=CSQBCOMM                                                      
         ST    R0,CSQBCOMM_STRUCTURE                                            
         LOAD  DE=CSQBCLOS                                                      
         ST    R0,CSQBCLOS_STRUCTURE                                            
         LOAD  DE=CSQBDISC                                                      
         ST    R0,CSQBDISC_STRUCTURE                                            
*                                                                               
INITX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
MQBUFF   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* GET THE DLN/CAN NOTIFICATION OF LOTUS NOTES EMAIL FROM A MQ QUEUE IN          
* BUFFER                                                                        
*                                                                               
         XC    BUFFER,BUFFER                                                    
*                                                                               
         MVC   OBJDESC_OBJECTTYPE,=A(MQOT_Q)  OBJECT IS A QUEUE                 
         LA    RF,MQOO_INPUT_AS_Q_DEF                                           
         ST    RF,OPEN_OPTIONS                                                  
*                                                                               
         MVC   P+30(L'OBJDESC_OBJECTNAME),OBJDESC_OBJECTNAME                    
         PRNT  ABOUT_TO_OPEN_Q                                                  
*                                                                               
         LA    R2,CSQBOPEN_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     OPEN QUEUE                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   MSGDESC_CORRELID,MQCI_NONE                                       
         MVC   MSGDESC_MSGID,MQMI_NONE                                          
*                                                                               
         MVC   BUFFERLENGTH,=A(L'BUFFER)                                        
         MVC   GETMSGOPTS_OPTIONS,=A(MQGMO_SET_SIGNAL)                          
         XC    INPQECB,INPQECB                                                  
         MVC   GETMSGOPTS_SIGNAL1,=A(INPQECB)                                   
         MVC   GETMSGOPTS_WAITINTERVAL,=A(MQWI_UNLIMITED)                       
*                                                                               
MQBUF10  LA    R2,CSQBGET_STRUCTURE                                             
         GOTO1 =A(CALLMQ),(RC)     TRY TO GET A MESSAGE                         
         BE    MQBUF30             GOT A MESSAGE                                
         CLC   MQ_COMPCODE,=A(MQCC_WARNING)                                     
         BNE   *+16                                                             
         CLC   MQ_REASON,=A(MQRC_SIGNAL_REQUEST_ACCEPTED)                       
         BE    MQBUF20                                                          
         DC    H'0'                NO OTHER WARNING IS ACCEPTABLE               
*                                                                               
MQBUF20  PRNT  ABOUTTOWAIT                                                      
*                                                                               
         WAIT  1,ECBLIST=ECBLST    WAIT FOR COMPLETION OR OPERATOR              
*                                                                               
         PRNT  WAITCOMPLETE                                                     
*                                                                               
         L     RF,ASTOPECB                                                      
         TM    0(RF),X'40'         STOP?                                        
         BZ    *+18                NO                                           
         XC    0(4,RF),0(RF)                                                    
         MVI   OPERSTOP,C'Y'                                                    
         B     MQBUFFX                                                          
*                                                                               
         TM    INPQECB,X'40'                                                    
         BO    *+6                                                              
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
         L     RF,INPQECB          BITS 2-31 OF ECB CONTAIN COMP. CODE          
         SLL   RF,2                                                             
         SRL   RF,2                                                             
         C     RF,=A(MQEC_MSG_ARRIVED)                                          
         BE    *+6                                                              
         DC    H'0'                NO OTHER RESULT ACCEPTABLE (FOR NOW)         
         XC    INPQECB,INPQECB     CLEAR ECB BEFORE MQGET CALL                  
         B     MQBUF10             A MESSAGE IS WAITING: GET IT                 
*                                                                               
MQBUF30  CLC   MSGDESC_MSGTYPE,=A(MQMT_DATAGRAM)                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   R6,15,DATALENGTH    R6 = LENGTH OF DATA IN MESSAGE               
         BNZ   *+6                                                              
         DC    H'0'                NO DATA RECEIVED -- HOW?                     
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'MQ_GET: BDE NOTIFICAITON BUFFER',    +        
               BUFFER,C'DUMP',(R6),=C'1D'                                       
*                                                                               
         LA    R2,CSQBCOMM_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     COMMIT                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CLOSE_OPTIONS,=A(MQCO_NONE)                                      
         LA    R2,CSQBCLOS_STRUCTURE                                            
         GOTO1 =A(CALLMQ),(RC)     CLOSE QUEUE                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MQBUFFX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
GETFIELD NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* VERSIONCONTROLNUM:    CL2                                                     
* DDSREFNUM:            CL18 (MAX CL30)                                         
* SERVERDATE:           CL10 AS MMMDD/YYYY                                      
* SERVERTIME:           CL8  AS HH:MM:SS IN 24-HR FORMAT                        
* EVENTTYPE:            CL3  (SND, RCV, DEL)                                    
* EVENTDATE:            CL10 AS MMMDD/YYYY                                      
* EVENTTIME:            CL8  AS HH:MM:SS IN 24-HR FORMAT                        
* DOCID:                CL9                                                     
* SENDERCN:             CL20                                                    
*                                                                               
         LA    R3,FLDTAB                                                        
         LA    R4,BUFFER                                                        
*                                                                               
GFLD10   MVC   FIELD+1(L'FIELD-1),0(R4)                                         
         LR    R2,R4                                                            
*                                                                               
         LA    RF,L'FIELD                                                       
         CLI   0(R4),C';'                                                       
         BE    GFLD20                                                           
         AHI   R4,1                                                             
         BCT   RF,*-12                                                          
         B     GFLDERR                                                          
*                                                                               
GFLD20   LR    RE,R4                                                            
         SR    RE,R2                                                            
*                                                                               
         STC   RE,FIELD                                                         
         OC    0(L'FLDTAB,R3),0(R3)                                             
         BZ    *+14                                                             
         L     RF,0(R3)                                                         
         BASR  RE,RF                                                            
         BNE   GFLDX                                                            
*                                                                               
         AHI   R4,1                                                             
         AHI   R3,L'FLDTAB                                                      
         CLI   0(R3),X'FF'                                                      
         BNE   GFLD10                                                           
         B     GFLDOK                                                           
*                                                                               
GFLDERR  MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(15),=C'UNKNOWN MESSAGE'                                     
         GOTO1 =V(PRINTER)                                                      
         B     GFLDBAD                                                          
*                                                                               
GFLDOK   CR    RB,RB                                                            
         B     *+6                                                              
GFLDBAD  LTR   RB,RB                                                            
GFLDX    XIT1                                                                   
*                                                                               
FLDTAB   DS    0F                                                               
         DC    A(GVCNUM)                                                        
         DC    A(GCUSTREF)                                                      
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(GEVTYPE)                                                       
         DC    A(GEVDAY)                                                        
         DC    A(GEVTIME)                                                       
         DC    A(GDOCID)                                                        
*        DC    A(0)                                                             
         DC    X'FF'                                                            
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
GVCNUM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZIC   RE,FIELD                                                         
         CHI   RE,L'VCNUM                                                       
         BNE   *+14                                                             
         MVC   VCNUM,FIELD+1                                                    
         B     GVCNOK                                                           
*                                                                               
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(30),=C'INVALID VERSION CONTROL NUMBER'                      
         GOTO1 =V(PRINTER)                                                      
         B     GVCNBAD                                                          
*                                                                               
GVCNOK   CR    RB,RB                                                            
         B     *+6                                                              
GVCNBAD  LTR   RB,RB                                                            
GVCNX    XIT1                                                                   
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
GCUSTREF NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZIC   RE,FIELD                                                         
         CHI   RE,L'CUSTREF                                                     
         BNE   GCR10                                                            
         MVC   CUSTREF,FIELD+1                                                  
*                                                                               
         CLC   CUSTREF+1(1),MAJORNAM+5   MATCH ON EDICT (ADV/REP)?              
         BNE   GCR10               NO -- DLN CAME IN OVER WRONG LINE            
*                                                                               
         CLI   CUSTREF,C'4'        VERSION 4 CUSTOMER REF. NUMBER?              
         BNE   GCR10                                                            
*                                                                               
         CLC   TODAY8,CUSTREF+10   NEED TO RESET TODAY'S DATE?                  
         BNL   GCR05               NO                                           
         GOTO1 =V(DATCON),DMCB,(5,0),(20,TODAY8)                                
         MVC   DELPERD+9(8),TODAY8 FOR PERVAL                                   
*                                                                               
GCR05    MVC   DELPERD(8),CUSTREF+10   DATE OF ORIGINAL SEND (YYYYMMDD)         
         GOTO1 =V(PERVAL),DMCB,(L'DELPERD,DELPERD),WORK                         
         CLI   DMCB+4,0                                                         
         BNE   GCR10               MAINFRAME DATE MUST BE SCREWED UP!           
         LA    RF,WORK                                                          
         CLC   PVALNDYS-PERVALD(2,RF),=AL2(28)                                  
         BNH   GCROK               DLN ISN'T MORE THAN 28 DAYS OLD              
*                                                                               
GCR10    MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(33),=C'INVALID CUSTOMER REFERENCE NUMBER'                   
         MVC   P+70(L'CUSTREF),CUSTREF                                          
         GOTO1 =V(PRINTER)                                                      
         B     GCRBAD              IGNORE THIS ONE                              
*                                                                               
GCROK    CR    RB,RB                                                            
         B     *+6                                                              
GCRBAD   LTR   RB,RB                                                            
GCRNX    XIT1                                                                   
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
GEVTYPE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZIC   RE,FIELD                                                         
         CHI   RE,L'EVTYPE                                                      
         BNE   GETYP20                                                          
         MVC   EVTYPE,FIELD+1                                                   
*                                                                               
         CLC   EVTYPE,=C'SND'                                                   
         BE    GETYPOK                                                          
         CLC   EVTYPE,=C'RCV'                                                   
         BE    GETYPOK                                                          
         CLC   EVTYPE,=C'DEL'                                                   
         BE    GETYPOK                                                          
*                                                                               
GETYP20  MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(17),=C'INVALID EVENT TYPE'                                  
         GOTO1 =V(PRINTER)                                                      
         B     GETYPBAD                                                         
*                                                                               
GETYPOK  CR    RB,RB                                                            
         B     *+6                                                              
GETYPBAD LTR   RB,RB                                                            
GETYPX   XIT1                                                                   
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
GEVDAY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZIC   RE,FIELD                                                         
         CHI   RE,10               CL10, MMMDD/YYYY                             
         BNE   GEDAYBAD                                                         
*                                                                               
         GOTO1 =V(DATVAL),DMCB,(10,FIELD+1),WORK                                
         OC    DMCB,DMCB                                                        
         BNE   GEDAY10                                                          
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(17),=C'INVALID EVENT DATE'                                  
         GOTO1 =V(PRINTER)                                                      
         B     GEDAYBAD            IGNORE THIS ONE                              
*                                                                               
GEDAY10  GOTO1 =V(HEXIN),DMCB,FIELD+4,EVDAY,2                                   
*                                                                               
GEDAYOK  CR    RB,RB                                                            
         B     *+6                                                              
GEDAYBAD LTR   RB,RB                                                            
GEDAYX   XIT1                                                                   
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
GEVTIME  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZIC   RE,FIELD                                                         
         CHI   RE,8                CL8, HH:MM:SS                                
         BNE   GETIMBAD                                                         
*                                                                               
         MVC   FULL(2),FIELD+1                                                  
         MVC   FULL+2(2),FIELD+4                                                
         GOTO1 =V(HEXIN),DMCB,FULL,EVTIME,4                                     
         CLC   =F'2',DMCB+12                                                    
         BE    GETIMOK                                                          
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(17),=C'INVALID EVENT TIME'                                  
         GOTO1 =V(PRINTER)                                                      
         B     GETIMBAD            IGNORE THIS ONE                              
*                                                                               
GETIMOK  CR    RB,RB                                                            
         B     *+6                                                              
GETIMBAD LTR   RB,RB                                                            
GETIMX   XIT1                                                                   
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
GDOCID   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZIC   RE,FIELD                                                         
         CHI   RE,L'DOCID                                                       
         BNE   *+14                                                             
         MVC   DOCID,FIELD+1                                                    
         B     GDIDOK                                                           
*                                                                               
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(26),=C'INVALID DOCUMENT ID NUMBER'                          
         GOTO1 =V(PRINTER)                                                      
         B     GDIDBAD                                                          
*                                                                               
GDIDOK   CR    RB,RB                                                            
         B     *+6                                                              
GDIDBAD  LTR   RB,RB                                                            
GDIDX    XIT1                                                                   
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
CHKBUFF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   RETRY,C'N'          ASSUME NO RETRY YET                          
         LA    R1,DMCB                                                          
         USING CONVDSKD,R1                                                      
         MVC   CONVDMGR,DATAMGR    A(DATAMGR)                                   
         MVI   CONVACTN,CONVDSKO   CONVERT REFERENCE NUMBER TO DSKADDR          
         LA    RE,CUSTREF+2                                                     
         ST    RE,CONVOFF                                                       
         GOTO1 =V(CONVDSKA)                                                     
         MVC   FULL,CONVDSK                                                     
         DROP  R1                                                               
*                                                                               
         LA    R6,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R6)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(6),E,8)  ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         LA    R2,TABLNTRY         ASSUME WE'LL FIND REPORT IN TABLE            
         MVI   BYTE,C'T'           'T' FOR TABLE                                
*                                                                               
         L     R6,AXMTTBL          A(TRANSMIT TABLE)                            
         USING XMTTABLD,R3                                                      
         LA    R3,XMTENTRY                                                      
*                                                                               
CHKB50   MVC31 XMTENTRY,0(R6)                                                   
         CLI   0(R3),0             END OF TABLE?                                
         BNE   *+16                                                             
         L     R2,FULL             YES -- USE EDICT FILE DISK ADDRESS           
         MVI   BYTE,C'F'           'F' FOR FILE                                 
         B     CHKB60                                                           
         CLI   XMTSOURC,XMTSRCPQ   PRINT QUEUE ENTRY?                           
         BNE   *+14                                                             
         CLC   FULL,XMTDSKAD       FIND MATCH ON DISK ADDRESS                   
         BE    *+12                                                             
         AHI   R6,XMTTBLQ          BUMP TO NEXT ENTRY                           
         B     CHKB50                                                           
         MVC   TABLNTRY,0(R3)      SAVE XMIT TABLE ENTRY                        
         DROP  R3                                                               
*                                                                               
CHKB60   LA    R6,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R6)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(6),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
CHKB65   LA    R1,DMCB                                                          
         USING EDFPARMD,R1                                                      
         XC    0(EDFPRMLQ,R1),0(R1)                                             
*                                                                               
         MVC   EXMTTBLE,AXMTTBL                                                 
         ST    R2,ETBLNTRY                                                      
*                                                                               
         CLC   EVTYPE,=C'RCV'                                                   
         BNE   *+12                                                             
         MVI   EACTION,EACTDLVQ                                                 
         B     CHKB70                                                           
*                                                                               
         CLC   EVTYPE,=C'SND'                                                   
         BNE   *+12                                                             
         MVI   EACTION2,EACTNTFQ                                                
         B     CHKB70                                                           
*                                                                               
         CLC   EVTYPE,=C'DEL'                                                   
         BNE   *+12                                                             
         MVI   EACTION2,EACTDELQ                                                
         B     CHKB70                                                           
*                                                                               
CHKB70   CLI   BYTE,C'F'           DISK ADDRESS BEING PASSED?                   
         BNE   *+8                                                              
         OI    EACTION,EACTFILQ    YES                                          
         MVC   EMAJORNM,=A(MAJORNAM)                                            
         MVC   EABDEDTA,=A(DOCID)                                               
         MVC   EDAYNUM,EVDAY       DAY                                          
         MVC   ETIME,EVTIME        TIME                                         
         MVC   EADTAMGR,DATAMGR                                                 
         GOTO1 =V(POSTSENT)                                                     
         BNE   CHKB80              COULD NOT POST EDICT FILE                    
         DROP  R1                                                               
*                                  EDICT FILE DISK ADDRESS                      
         GOTO1 =V(HEXOUT),DMCB,FULL,P+30,4,=C'TOG'                              
         PRNT  EDICTFILEPOSTED,PRINT=ALWAYS                                     
         B     CHKBX                                                            
*                                                                               
CHKB80   CLC   EVTYPE,=C'SND'                                                   
         BE    CHKB90              NO RETRY FOR "SND" MESSAGE                   
*                                                                               
*SOMETIMES, A "RCV"/"DEL" MAY COME BEFORE SENDER MARKS THE REPORT SENT.         
*SO, IN THIS CASE, WE SHOULD WAIT 2 MIN AND RETRY AGAIN.                        
*IT IS OKAY TO WAIT FOR THIS LONG TIME BECASUE SENDER TAKES ABOUT 3 MIN         
*TO SEND A REPORT.                                                              
*                                                                               
         CLI   RETRY,C'Y'          RETRY ALREADY?                               
         BE    CHKB90                                                           
*                                                                               
         PRNT  WAIT_AND_RETRY,PRINT=ALWAYS                                      
         STIMER WAIT,BINTVL=WAITSECS                                            
         MVI   RETRY,C'Y'          RETRY NOW                                    
         B     CHKB65                                                           
*                                                                               
CHKB90   MVC   P+30(13),=C'*** ERROR ***'                                       
         PRNT  CANTPOST,PRINT=ALWAYS                                            
*                                                                               
CHKBX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
WAITSECS DC    F'12000'            DEFAULT WAIT TIME = 2 MINUTES                
RETRY    DS    C                                                                
         EJECT                                                                  
CALLMQ   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* UPON ENTRY,  R2 POINTS TO PARAMETER STRUCTURE                                 
* UPON RETURN, MQ_COMPCODE CONTAINS THE MQ COMPLETION CODE                      
*              MQ_REASON CONTAINS THE MQ REASON CODE                            
*              SET CC=EQ WHEN CALL OKAY, OTHERWISE CC=NEQ                       
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
         MVI   P,C'+'              '+' MEANS IT'S AN MQ CALL                    
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODES          
         MVC   P+30(12),=C'COMPLETED OK'                                        
         CLC   MQ_COMPCODE,=A(MQCC_OK)                                          
         BNE   CMQ10                                                            
         PRNT  PRINT=ALWAYS                                                     
         B     CMQOK                                                            
*                                                                               
CMQ10    MVC   P+30(12),=C'WARNING     '                                        
         CLC   MQ_COMPCODE,=A(MQCC_WARNING)                                     
         BE    CMQ20                                                            
         MVC   P+30(12),=C'** FAILED **'                                        
         CLC   MQ_COMPCODE,=A(MQCC_FAILED)                                      
         BE    CMQ20                                                            
         DC    H'0'                UNKNOWN COMPLETION CODE                      
*                                                                               
CMQ20    MVC   P+50(9),=C'REASON = '                                            
         EDIT  MQ_REASON,(5,P+59),ALIGN=LEFT,ZERO=NOBLANK                       
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
         EJECT                                                                  
COMMWORK DS    0D                  COMMON STORAGE AREA                          
         SPACE 2                                                                
         CMQA LIST=YES,EQUONLY=NO                                               
         SPACE 2                                                                
* MQSERIES CALL PARAMETERS                                                      
*                                                                               
QMGRNAME     DS      CL48      MQ QUEUE MANAGER NAME                            
HCONN        DS      F         MQ QMGR CONNECTION HANDLE                        
HOBJ         DS      F         OBJECT HANDLE                                    
OPEN_OPTIONS DS      F         MQOPEN OPTIONS                                   
CLOSE_OPTIONS DS     F         MQCLOSE OPTIONS                                  
MQ_COMPCODE  DS      F         COMPLETION CODE                                  
MQ_REASON    DS      F         QUALIFIES COMPLETION CODE                        
BUFFERLENGTH DS      F         LENGTH OF MQ BUFFER AREA                         
DATALENGTH   DS      F         LENGTH OF THE MESSAGE                            
OBJDESC      CMQODA  LIST=YES  OBJECT DESCRIPTOR                                
MSGDESC      CMQMDA  LIST=YES  MESSAGE DESCRIPTOR                               
GETMSGOPTS   CMQGMOA LIST=YES  GET MESSAGE OPTIONS                              
BUFFER       DS    CL256                                                        
         SPACE 2                                                                
ECBLST   DS    0F                                                               
         DC    X'00',AL3(INPQECB)  A(INPUT QUEUE ECB)                           
ASTOPECB DC    X'80',AL3(0)        A(STOPECB)                                   
INPQECB  DC    F'0'                ECB FOR MQ GET FROM INPUT QUEUE              
         EJECT                                                                  
DMCB     DS    10F                                                              
DUB      DS    D                                                                
PRNTDUB  DS    D                   FOR PRNT MACRO                               
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
PRNTTIME DS    CL9                 FOR PRNT MACRO                               
TODAY8   DS    CL8                 EBCDIC DATE TODAY (YYYYMMDD)                 
DELPERD  DC    C'YYYYMMDD-YYYYMMDD'  FOR PERVAL: SEND DATE - TODAY              
WORK     DS    CL256                                                            
MAJORNAM DS    CL8                 MAJOR RESOURCE NAME                          
DATAMGR  DS    A                   A(DATAMGR)                                   
AXMTTBL  DS    A                   A(XMIT REPORTS TABLE)                        
XMTENTRY DS    XL(XMTTBLQ)         TEMP STORAGE FOR 1 XIMTABLE ENTRY            
TABLNTRY DS    XL(XMTTBLQ)         SAVED XMIT TABLE KEY                         
OPERSTOP DC    C'N'                'Y' = OPERATOR ENTERED 'STOP'                
TRACEFLG DS    C                   'Y' = PRINT DETAILED TRACE                   
*                                                                               
         DC    C'***FIELD***'                                                   
FIELD    DS    CL50                1ST IS LEN, REST IS DATA                     
VCNUM    DS    CL2                 VERSION CONTROL NUMBER                       
CUSTREF  DS    CL18                VEDDNNNNNNYYYYMMDD                           
EVTYPE   DS    CL3                 EVENT TYPE (SND, RCV, DEL)                   
EVDAY    DS    X                   EVENT DAY -- PWOS                            
EVTIME   DS    XL2                 EVENT TIME (HM) -- PWOS                      
DOCID    DS    CL9                 DOCUMENT ID NUMBER                           
*                                                                               
         EJECT                                                                  
*                                                                               
ENTRYPTS DS    0F                                                               
         DC    Y((ENTRYLSQ-ENTRYSTQ)/60)   NUMBER OF TABLE ENTRIES              
         DC    H'60'                       MUST REMAIN AS 60                    
ENTRYSTQ EQU   *                                                                
CSQBCLOS DC    CL8'CSQBCLOS'                                                    
         DC    XL52'00'                                                         
CSQBCOMM DC    CL8'CSQBCOMM'                                                    
         DC    XL52'00'                                                         
CSQBCONN DC    CL8'CSQBCONN'                                                    
         DC    XL52'00'                                                         
CSQBDISC DC    CL8'CSQBDISC'                                                    
         DC    XL52'00'                                                         
CSQBGET  DC    CL8'CSQBGET'                                                     
         DC    XL52'00'                                                         
CSQBOPEN DC    CL8'CSQBOPEN'                                                    
         DC    XL52'00'                                                         
ENTRYLSQ EQU   *                                                                
*                                                                               
* PARAMETER LISTS FOR MQ SERIES CALLS                                           
*                                                                               
*  F    A(ROUTINE)                                                              
*  CL16 EBCDIC ROUTINE NAME                                                     
*  XL1  FLAGS                                                                   
*       X'80': ROUTINE IS SYNCHRONOUS ONLY                                      
*       X'40': ROUTINE CAN BE FOLLOWED BY EXTRACT_ERROR CALL                    
*  XL3  SPARE                                                                   
*  PARAMETERS (STANDARD IBM FORMAT)                                             
*                                                                               
CSQBCONN_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_CONNECT'                                
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(QMGRNAME)                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(MQ_COMPCODE)                          
                          DC    X'80',AL3(MQ_REASON)                            
*                                                                               
CSQBOPEN_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_OPEN'                                   
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(OBJDESC)                              
                          DC    X'00',AL3(OPEN_OPTIONS)                         
                          DC    X'00',AL3(HOBJ)                                 
                          DC    X'00',AL3(MQ_COMPCODE)                          
                          DC    X'80',AL3(MQ_REASON)                            
*                                                                               
CSQBGET_STRUCTURE         DS    A                                               
                          DC    CL16'MQ_GET'                                    
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ)                                 
                          DC    X'00',AL3(MSGDESC)                              
                          DC    X'00',AL3(GETMSGOPTS)                           
                          DC    X'00',AL3(BUFFERLENGTH)                         
                          DC    X'00',AL3(BUFFER)                               
                          DC    X'00',AL3(DATALENGTH)                           
                          DC    X'00',AL3(MQ_COMPCODE)                          
                          DC    X'80',AL3(MQ_REASON)                            
*                                                                               
CSQBCOMM_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_COMMIT'                                 
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(MQ_COMPCODE)                          
                          DC    X'80',AL3(MQ_REASON)                            
*                                                                               
CSQBCLOS_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_CLOSE'                                  
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(HOBJ)                                 
                          DC    X'00',AL3(CLOSE_OPTIONS)                        
                          DC    X'00',AL3(MQ_COMPCODE)                          
                          DC    X'80',AL3(MQ_REASON)                            
*                                                                               
CSQBDISC_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_DISCONNECT'                             
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(HCONN)                                
                          DC    X'00',AL3(MQ_COMPCODE)                          
                          DC    X'80',AL3(MQ_REASON)                            
         SPACE 3                                                                
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
         SPACE 3                                                                
       ++INCLUDE DDMQREASON                                                     
         SPACE 3                                                                
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
       ++INCLUDE DDEDICTWRK                                                     
         EJECT                                                                  
       ++INCLUDE DDEDICTFIL                                                     
         EJECT                                                                  
* DMGREQUS                                                                      
* DMPRTQL                                                                       
* DMPRTQK                                                                       
* DDDPRINT                                                                      
* CTGENFILE                                                                     
       ++INCLUDE DMGREQUS                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE DMPRTQK                                                        
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
         IEFZB4D2                                                               
         EJECT                                                                  
         IHAPSA                                                                 
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007DDEDIBDFR 08/12/05'                                      
         END                                                                    
