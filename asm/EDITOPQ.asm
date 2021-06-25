*          DATA SET EDITOPQ    AT LEVEL 030 AS OF 06/04/09                      
*PHASE EDITOPQA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*                                                                               
         TITLE 'MQ GET THEN PUT TO A PRINT QUEUE'                               
*                                                                               
MQ2PQ    CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**MQ2PQ*,=A(R13CHAIN),R9,R8                                    
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         BAS   RE,INITIAL                INITIALIZE/CONNECT/OPEN                
*                                                                               
MAIN010  BAS   RE,GETWAIT                GET/WAIT FOR MESSAGE                   
         BNE   STOP                      JOB CANCELED                           
*                                                                               
         BAS   RE,MSGTOPQ                PUT MESSAGE TO PRINT QUEUE             
*                                                                               
         CLI   DOMQCALL,C'D'             SINGLE MQGET THEN ABEND                
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,CSQBCOMM_STRUCTURE     COMMIT MQ                              
         BAS   RE,CALLMQ                                                        
         B     MAIN010                                                          
*                                                                               
STOP     BAS   RE,JOBCAN                 JOB CANCELED, CLOSE/DISCONNECT         
         XBASE                                                                  
         EJECT                                                                  
                                                                                
*======================================================================         
* INITIAL ROUTINE                                                               
*======================================================================         
INITIAL  NTR1                                                                   
*                                                                               
         MVC   TITLE(11),=C'MQSERIES EC'                                        
         GOTOR PRNT,INITIALIZE                                                  
*                                                                               
         BLDL  0,ENTRYPTS                 LIST OF APPC/MVS ENTRY POINTS         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                       BAD RETURN FROM BLDL MACRO            
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
         BAS   RE,SETOPS                 SET OPERATOR STOP ECB                  
         XC    ERRCNT,ERRCNT                                                    
                                                                                
*----------------------------------------                                       
* READ CARDS FROM JCL                                                           
*----------------------------------------                                       
INIT010  GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
*                                                                               
         MVC   P(L'CARD),CARD                                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLC   =C'/*',CARD               END-OF-FILE?                           
         BE    INIT020                                                          
*                                                                               
         CLC   =C'DDSIO=',CARD                                                  
         BNE   *+18                                                             
         L     RF,=V(DDSIO)              OVERRIDE DDSIO NAME                    
         MVC   0(8,RF),CARD+6                                                   
         B     INIT010                                                          
*                                                                               
         L     RF,=A(SSB)                                                       
         CLC   =C'DSPACE=',CARD                                                 
         BNE   *+14                                                             
         MVC   SSODSPAC-SSOOFF(1,RF),CARD+7                                     
         B     INIT010                                                          
*                                                                               
         CLC   =C'MQQMGRNAME=',CARD                                             
         BNE   *+14                                                             
         MVC   QMGRNAME,CARD+11                                                 
         B     INIT010                                                          
*                                                                               
         CLC   =C'MQQUEUENAME=',CARD                                            
         BNE   *+14                                                             
         MVC   QNAME,CARD+12                                                    
         B     INIT010                                                          
*                                                                               
         CLC   =C'MQCALL=',CARD                                                 
         BNE   *+14                                                             
         MVC   DOMQCALL,CARD+7                                                  
         B     INIT010                                                          
*                                                                               
         CLC   =C'LOGDETAIL=',CARD                                              
         BNE   *+14                                                             
         MVC   QLOGDET,CARD+10                                                  
         B     INIT010                                                          
*                                                                               
         CLC   =C'PRINTQUEUE=',CARD                                             
         BNE   *+14                                                             
         MVC   QPRINTQ,CARD+11                                                  
         B     INIT010                                                          
*                                                                               
         CLC   =C'EMAIL=',CARD                                                  
         BNE   INIT010                                                          
         MVC   QEMAIL,CARD+6                                                    
         B     INIT010                                                          
                                                                                
*----------------------------------------                                       
* GET STORAGE FOR MQ MESSAGE BUFFER                                             
*----------------------------------------                                       
INIT020  L     R3,MSGMAX                                                        
         LR    R4,R3                                                            
         AHI   R3,8                                                             
*                                                                               
         SAM31                         * SWITCH TO 31-BIT MODE                  
         GETMAIN RU,LV=(3),LOC=(ANY,ANY),BNDRY=PAGE                             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                      PROBLEM GETTING STORAGE                
*                                                                               
         MVC   0(8,R1),=C'*MQMSGPQ'                                             
         AHI   R1,8                                                             
         ST    R1,AMSGIN                                                        
         AR    R4,R1                                                            
         ST    R4,AMSGINX                                                       
         SAM24                         * SWITCH BACK TO 24-BIT MODE             
                                                                                
*----------------------------------------                                       
* CONNECT TO AND OPEN MQ QUEUE                                                  
*----------------------------------------                                       
         LA    R2,CSQBCONN_STRUCTURE                                            
         BAS   RE,CALLMQ                 CONNECT TO MQ QUEUE MANAGER            
         BE    *+6                                                              
         DC    H'0'                      COULD NOT CONNECT TO QUEUE             
*                                                                               
         MVC   OBJDESC_OBJECTTYPE,=A(MQOT_Q)  OBJECT IS A QUEUE                 
         MVC   OBJDESC_OBJECTNAME,QNAME       INPUT QUEUE NAME                  
         MVC   OPENOPTS,=A(MQOO_INPUT_AS_Q_DEF)                                 
*                                                                               
         LA    R2,CSQBOPEN_STRUCTURE                                            
         BAS   RE,CALLMQ                 OPEN QUEUE                             
         BE    *+6                                                              
         DC    H'0'                      COULD NOT OPEN QUEUE                   
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN  ',=C'CONTROL ',FLIST,AIO             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
                                                                                
*----------------------------------------------------------------------         
* SET UP OPERATOR COMMUNICATIONS                                                
*----------------------------------------------------------------------         
SETOPS   NTR1                                                                   
*                                                                               
         EXTRACT ACOMM,FIELDS=COMM                                              
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
*                                                                               
         L     R2,COMCIBPT               GET A(CIB)                             
         USING CIBNEXT,R2                                                       
         LA    R3,COMCIBPT               SET A(A(CIB))                          
         DROP  RF                                                               
*                                                                               
         CLI   CIBVERB,CIBSTART          TEST FOR 'S JOBNAME'                   
         BNE   SETOP2                    (IE NOT BATCH)                         
         DROP  R2                                                               
         QEDIT ORIGIN=(R3),BLOCK=(R2)    RELEASE THE CIB                        
SETOP2   QEDIT ORIGIN=(R3),CIBCTR=1      ACCEPT MODIFY COMMANDS                 
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
                                                                                
*======================================================================         
* GET / WAIT FOR THE NEXT MQ MESSAGE                                            
*======================================================================         
GETWAIT  NTR1                                                                   
         MVI   PRNTMSG,C'Y'              PRINT MESSAGE TO QUEUE                 
*                                                                               
GW010    MVC   MSGDESC_CORRELID,MQCI_NONE                                       
         MVC   MSGDESC_MSGID,MQMI_NONE                                          
         MVC   BUFFER,AMSGIN                                                    
*                                                                               
         MVC   GMSGOPTS_OPTIONS,=A(MQGMO_SET_SIGNAL+MQGMO_ACCEPT_TRUNCA+        
               TED_MSG)                                                         
         XC    INPQECB,INPQECB                                                  
         MVC   GMSGOPTS_SIGNAL1,=A(INPQECB)                                     
         MVC   GMSGOPTS_WAITINTERVAL,=A(MQWI_UNLIMITED)                         
*                                                                               
         LA    R2,CSQBGET_STRUCTURE                                             
         BAS   RE,CALLMQ                 TRY TO GET A MESSAGE                   
         BE    GWOKX                     GOT A MESSAGE                          
*                                                                               
         CLC   COMPCODE,=A(MQCC_WARNING)                                        
         BNE   GW020                                                            
         CLC   REASON,=A(MQRC_SIGNAL_REQUEST_ACCEPTED)                          
         BE    GW020                                                            
         CLC   REASON,=A(MQRC_TRUNCATED_MSG_ACCEPTED)                           
         BE    *+6                       GOT AN OVERSIZE MESSAGE                
         DC    H'0'                      NO OTHER WARNING IS ACCEPTABLE         
         MVC   P+11(13),=C'** WARNING **'                                       
         MVC   P+30(21),=C'MESSAGE WAS TRUNCATED'                               
         GOTO1 =V(PRINTER)                                                      
         MVI   QLOGDET,C'Y'              PRINT LOG OF MESSAGE                   
         MVI   PRNTMSG,C'T'              MARK MESSAGE WAS TRUNCATED             
         B     GWOKX                                                            
*                                                                               
GW020    GOTOR PRNT,ABOUTTOWAIT                                                 
         WAIT  1,ECBLIST=ECBLST          WAIT FOR COMPLETION/OPERATOR           
         GOTOR PRNT,WAITCOMPLETE                                                
*                                                                               
         L     RF,AOPERECB                                                      
         TM    0(RF),X'40'               STOP?                                  
         BZ    *+18                      . NO                                   
         XC    0(4,RF),0(RF)                                                    
         MVI   OPERSTOP,C'Y'                                                    
         B     GWSTOPX                                                          
*                                                                               
         TM    INPQECB,X'40'                                                    
         BO    *+6                                                              
         DC    H'0'                      HOW DID WE GET OUT OF WAIT?            
*                                                                               
         L     RF,INPQECB                BITS 2-31 OF ECB ARE COMP CODE         
         SLL   RF,2                                                             
         SRL   RF,2                                                             
         C     RF,=A(MQEC_MSG_ARRIVED)                                          
         BE    *+6                                                              
         DC    H'0'                      ANY OTHER RESULT NO GOOD               
         XC    INPQECB,INPQECB           CLEAR ECB BEFORE MQGET CALL            
         B     GW010                     A MESSAGE IS WAITING: GET IT           
*                                                                               
GWSTOPX  LTR   RB,RB                     SET CC NOT EQUAL-JOB STOPPED           
         B     XIT                                                              
GWOKX    CR    RB,RB                     SET CC EQUAL-MESSAGE RECEIVED          
         B     XIT                                                              
         EJECT                                                                  
                                                                                
*======================================================================         
* OUTPUT MESSAGE TO PRINT QUEUE                                                 
*======================================================================         
MSGTOPQ  NTR1                                                                   
*                                                                               
         BRAS  RE,CHKUSR                 CHECK USER-ID                          
*                                                                               
         XC    REC,REC                   PRINT LINE FOR PRTQUE CALLS            
         LA    R2,REC                                                           
         USING PQPLD,R2                                                         
         MVC   QLSRCID,CUSERNO           USERID                                 
         MVI   QLEXTRA,X'FF'             OPEN PRINT QUEUE REPORT                
         MVC   QLSUBID,=C'M2P'                                                  
         MVI   QLCLASS,C'G'              MUST BE CLASS "G" REPORT               
         MVI   QLLINET,X'C0'                                                    
         MVI   QLLINEW,132                                                      
         MVC   QLRETNL,=H'36'                                                   
         MVC   QLRETND,=H'2'                                                    
         MVC   QLDESC,=C'EDI-EC     '                                           
*                                                                               
         CLI   PRNTMSG,C'Y'                                                     
         BE    *+8                                                              
         OI    QLSTAT,QLSTHO             MARK ON HOLD                           
*                                                                               
         L     RE,=A(CIREC)                                                     
         XCEFL (RE),14336                CLEAR PQ C/I BUFFER                    
*                                                                               
         CLI   QPRINTQ,C'N'              JOB PARAMETER                          
         BE    MTP030                                                           
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',=C'PRTQUE',0,REC,A(CIREC)           
         CLI   DMCB+8,0                                                         
         BE    MTP020                                                           
*                                                                               
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(28),=C'COULD NOT OPEN PRTQUE REPORT'                        
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
*                                                                               
MTP020   MVC   P(L'QLSRCID),QLSRCID                                             
         MVC   P+10(L'QLSUBID),QLSUBID                                          
         MVC   P+20(L'QLREPRNO),QLREPRNO                                        
         MVC   P+30(L'QLREPRCI),QLREPRCI                                        
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVI   REC,X'89'                 SET SKIP TO TOP OF PAGE                
         MVC   REC+1(132),SPACES                                                
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',=C'PRTQUE',0,REC,A(CIREC)           
         CLI   DMCB+8,0                                                         
         BNE   MTP050                                                           
*                                                                               
MTP030   BRAS  RE,PQLINES                PUT MESSAGE LINES TO PRTQUE            
         BNE   MTP050                                                           
*                                                                               
         CLI   QPRINTQ,C'N'              DON'T SEND TO PRINT QUEUE?             
         BE    MTPOKX                                                           
*                                                                               
         MVI   REC,X'FF'                 SET END OF REPORT                      
         MVC   REC+1(132),SPACES                                                
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',=C'PRTQUE',0,REC,A(CIREC)           
         CLI   DMCB+8,0                                                         
         BE    MTPOKX                                                           
*                                                                               
MTP050   MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(27),=C'COULD NOT WRITE PRTQUE LINE'                         
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
*                                                                               
MTPERX   LTR   RB,RB                     NO GOOD -- SET CC NOT EQUAL            
         B     XIT                                                              
MTPOKX   CR    RB,RB                     SET CC EQUAL                           
         B     XIT                                                              
         EJECT                                                                  
                                                                                
*----------------------------------------------------------------------         
* CHECK USER-ID IN MESSAGE                                                      
*----------------------------------------------------------------------         
CHKUSR   NTR1                                                                   
*                                                                               
         XC    CUSERID,CUSERID                                                  
         XC    CUSERNO,CUSERNO                                                  
         XC    CUALPHA,CUALPHA                                                  
*                                                                               
         SAM31                         * SWITCH TO 31-BIT MODE                  
         L     R4,AMSGIN                 A(MESSAGE BUFFER)                      
         LR    R5,R4                                                            
         L     R1,DATALEN                MESSAGE LENGTH                         
         AR    R5,R1                     A(END OF MESSAGE)                      
*                                                                               
         MVC   P,0(R4)                   PRINT MESSAGE                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
CU010    CR    R4,R5                     END OF MESSAGE                         
         BNL   CUNHX                                                            
         CLC   HDREDICT,0(R4)            LOOK FOR EDICT HEADER W/USER           
         BE    CU020                                                            
         AHI   R4,1                                                             
         B     CU010                                                            
*                                                                               
CU020    MVC   CUSERID,L'HDREDICT(R4)    CURRENT USER-ID                        
         SAM24                                                                  
*                                                                               
         XC    IOKEY,IOKEY               FIND ID REC                            
         LA    R2,IOKEY                                                         
         USING CTIREC,R2                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,CUSERID                                                   
         OC    CTIKID,SPACES                                                    
         GOTO1 =V(DATAMGR),DMCB,=C'DMREAD ',=C'CTFILE ',IOKEY,AIO               
         CLI   8(R1),0                                                          
         BNE   CUIUX                                                            
*                                                                               
         L     R2,AIO                                                           
         LA    R4,CTIDATA                                                       
CU030    CLI   0(R4),0                                                          
         BE    CU060                                                            
         CLI   0(R4),CTDSCELQ            X'02' - USER ID NUMBER                 
         BE    CU040                                                            
         CLI   0(R4),CTAGYELQ            X'06' - ALPHA ID                       
         BE    CU050                                                            
CU032    LLC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     CU030                                                            
*                                                                               
         USING CTDSCD,R4                                                        
CU040    MVC   CUSERNO,CTDSC             USER ID NUMBER                         
         B     CU032                                                            
         DROP  R4                                                               
*                                                                               
         USING CTAGYD,R4                                                        
CU050    MVC   CUALPHA,CTAGYID           ALPHA ID                               
         B     CU032                                                            
         DROP  R4                                                               
*                                                                               
CU060    OC    CUSERNO,CUSERNO                                                  
         BZ    CUIUX                                                            
*                                                                               
         XC    IOKEY,IOKEY               FIND EDICT RECORD                      
         LA    R2,IOKEY                                                         
         USING EDIKEYD,R2                                                       
         MVI   EDIKSYS,EDIKSYSQ          X'05'                                  
         MVI   EDITYPE,EDITYPEQ          X'07'                                  
         MVC   EDINAME,CUSERID           USER-ID IS KEY OF EDICT REC            
         OC    EDINAME,SPACES                                                   
         GOTO1 =V(DATAMGR),DMCB,=C'DMREAD ',=C'CTFILE ',IOKEY,AIO               
         CLI   8(R1),0                                                          
         BNE   CUIEX                                                            
*                                                                               
         CLI   PRNTMSG,C'T'              CHECK MESSAGE WAS TRUNCATED            
         BE    CUTRX                                                            
*                                                                               
         B     CUOKX                                                            
*                                                                               
CUTRX    MVC   P+30(26),=C'MESSAGE WAS TRUNCATED      '                         
         MVC   P+58(L'CUSERID),CUSERID                                          
         B     CUERX                                                            
CUNHX    MVC   P+30(26),=C'NO MESSAGE HEADER FOUND    '                         
         MVC   P+58(L'HDREDICT),HDREDICT                                        
         B     CUERX                                                            
CUIUX    MVC   P+30(26),=C'USER-ID NOT FOUND          '                         
         MVC   P+58(L'CUSERID),CUSERID                                          
         B     CUERX                                                            
CUIEX    MVC   P+30(26),=C'NO EDICT RECORD FOUND      '                         
         MVC   P+58(L'CUSERID),CUSERID                                          
*                                                                               
CUERX    SAM24                                                                  
*                                                                               
         MVC   P+11(13),=C'*** ERROR ***'                                       
         CLC   QEMAIL,SPACES                                                    
         BNH   *+10                                                             
         MVC   OPMSGT,QEMAIL                                                    
*                                                                               
         LA    R1,OPMSGT+L'OPMSGT-1                                             
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         AHI   R1,-1                                                            
         B     *-12                                                             
         CLI   0(R1),C':'                                                       
         BE    *+12                                                             
         MVI   1(R1),C':'                                                       
         LA    R1,1(R1)                                                         
         LA    R1,1(R1)                                                         
         MVC   0(40,R1),P+30                                                    
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'OPMSG ',('OPMSGLQ',OPMSG)                    
         GOTO1 =V(PRINTER)                                                      
         MVC   CUSERNO,=H'17'            SET DEFAULT TO SJR                     
         MVC   CUSERID,=CL8'SJR'                                                
         MVI   PRNTMSG,C'N'              MESSAGE NOT SUCCESSFUL                 
*                                                                               
         LLC   R1,ERRCNT                 COUNT THE ERRORS                       
         AHI   R1,1                                                             
         STC   R1,ERRCNT                                                        
*                                                                               
         CLI   ERRCNT,20                 MORE THAN 20 ERRORS, NO GOOD           
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LTR   RB,RB                     NO GOOD -- SET CC NOT EQUAL            
         B     XIT                                                              
*                                                                               
CUOKX    CR    RB,RB                     SET CC EQUAL                           
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
                                                                                
*======================================================================         
* PUT MESSAGE TO PRINT QUEUE                                                    
*======================================================================         
PQLINES  NTR1                                                                   
         NI    INDIC,X'FF'-INDCCS                                               
*                                                                               
         SAM31                         * SWITCH TO 31-BIT MODE                  
         L     R4,AMSGIN                 A(MESSAGE BUFFER)                      
         LR    R5,R4                                                            
         L     R1,DATALEN                MESSAGE LENGTH                         
         AR    R5,R1                     A(END OF MESSAGE)                      
*                                                                               
PQL010   XC    REC,REC                   PRINT LINE FOR PRTQUE CALLS            
         MVI   REC,X'09'                                                        
         LA    R2,REC+1                                                         
         LHI   R1,L'P                    132 MAX CHARS IN LINE                  
         CR    R4,R5                     END OF MESSAGE                         
         BNL   PQLOKX                                                           
*                                                                               
PQL020   CR    R4,R5                     END OF MESSAGE?                        
         BH    PQL040                                                           
         CLI   0(R4),X'0D'               CARRIAGE RETURN?                       
         BE    PQL030                                                           
         CLI   0(R4),X'25'               LINEFEED?                              
         BE    PQL040                                                           
         MVC   0(1,R2),0(R4)                                                    
         LA    R2,1(R2)                                                         
PQL030   LA    R4,1(R4)                                                         
         BCT   R1,PQL020                                                        
*                                                                               
         SAM24                                                                  
         MVC   P+11(13),=C'** WARNING **'                                       
         MVC   P+30(25),=C'LINE EXCEEDS PRTQUE LIMIT'                           
         GOTO1 =V(PRINTER)                                                      
         MVC   P,REC                                                            
         GOTO1 =V(PRINTER)                                                      
         B     PQL050                                                           
*                                                                               
PQL040   LA    R4,1(R4)                  BUMP PAST LINEFEED                     
         SAM24                                                                  
*                                                                               
PQL050   CLI   QLOGDET,C'Y'                                                     
         BNE   PQL055                                                           
         MVC   P,REC                                                            
         GOTO1 =V(PRINTER)                                                      
                                                                                
PQL055   CLI   QPRINTQ,C'N'              REQUEST                                
         BE    PQL060                                                           
*                                                                               
         CLC   REC+1(5),=C'++DDS'        CONTROL CARDS                          
         BNE   *+12                                                             
         OI    INDIC,INDCCS                                                     
         B     PQL058                                                           
*                                                                               
         TM    INDIC,INDCCS                                                     
         BZ    PQL058                                                           
         NI    INDIC,X'FF'-INDCCS                                               
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',=C'PRTQUE',0,PGB,A(CIREC)           
         CLI   DMCB+8,0                                                         
         BNE   PQLERX                                                           
*                                                                               
PQL058   GOTO1 =V(DATAMGR),DMCB,=C'DMPRINT',=C'PRTQUE',0,REC,A(CIREC)           
         CLI   DMCB+8,0                                                         
         BNE   PQLERX                                                           
*                                                                               
PQL060   SAM31                                                                  
         B     PQL010                                                           
*                                                                               
PQLERX   LTR   RB,RB                     NO GOOD -- SET CC NOT EQUAL            
         B     XIT                                                              
PQLOKX   CR    RB,RB                     SET CC EQUAL                           
         B     XIT                                                              
         EJECT                                                                  
                                                                                
*======================================================================         
* JOB CANCELED, CLOSE QUEUE, DISCONNECT                                         
*======================================================================         
JOBCAN   NTR1                                                                   
*                                                                               
         LA    RF,MQCO_NONE                                                     
         ST    RF,CLOSOPTS                                                      
*                                                                               
         LA    R2,CSQBCLOS_STRUCTURE                                            
         BAS   RE,CALLMQ                 CLOSE QUEUE                            
*                                                                               
         LA    R2,CSQBDISC_STRUCTURE                                            
         BAS   RE,CALLMQ                 DISCONNECT FROM MQ QUEUE               
*                                        MANAGER                                
         GOTOR PRNT,EXITING                                                     
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMCLSE ',=C'CONTROL ',FLIST,AIO              
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
                                                                                
*======================================================================         
* CALL MQ                                                                       
* UPON ENTRY,  R2 POINTS TO PARAMETER STRUCTURE                                 
* UPON RETURN, COMPCODE CONTAINS THE MQ COMPLETION CODE                         
*              REASON CONTAINS THE MQ REASON CODE                               
*======================================================================         
CALLMQ   NTR1                                                                   
*                                                                               
         CLI   DOMQCALL,C'N'             DISABLED ALL MQ CALLS?                 
         BE    CMQOK                     . YES - DON'T DO ANY MQ CALLS          
*                                                                               
         SAM31                         * SWITCH TO 31-BIT MODE                  
         L     RF,0(R2)                  RF = A(MQ ROUTINE)                     
         LA    R3,24(R2)                 R3 = A(PARAMETER LIST)                 
         CALL  (15),MF=(E,(R3))                                                 
         SAM24                         * SWITCH BACK TO 24-BIT MODE             
*                                                                               
         L     RF,AOPERECB                                                      
         TM    0(RF),X'40'               STOP?                                  
         BZ    *+14                      NO                                     
         XC    0(4,RF),0(RF)                                                    
         MVI   OPERSTOP,C'Y'                                                    
*                                                                               
         MVI   P,C'+'                    "+" MEANS IT'S AN MQ CALL              
         MVC   P+1(16),4(R2)             PRINT NAME AND RETURN CODES            
         MVC   P+30(12),=C'COMPLETED OK'                                        
         CLC   COMPCODE,=A(MQCC_OK)                                             
         BNE   CMQ05                                                            
         MVC   P+50(L'OBJDESC_OBJECTNAME),OBJDESC_OBJECTNAME                    
*                                                                               
         GOTOR PRNT,MESSAGESET                                                  
         B     CMQOK                                                            
*                                                                               
CMQ05    MVC   P+30(12),=C'WARNING     '                                        
         CLC   COMPCODE,=A(MQCC_WARNING)                                        
         BE    CMQ10                                                            
         MVC   P+30(12),=C'** FAILED **'                                        
         CLC   COMPCODE,=A(MQCC_FAILED)                                         
         BE    CMQ10                                                            
         DC    H'0'                      UNKNOWN COMPLETION CODE                
*                                                                               
CMQ10    MVC   P+50(9),=C'REASON = '                                            
         EDIT  REASON,(5,P+59),ALIGN=LEFT,ZERO=NOBLANK                          
         MVC   P+66(13),=C'*** ERROR ***'                                       
         GOTOR PRNT,MESSAGESET                                                  
*                                                                               
         LTR   RB,RB                     NO GOOD -- SET CC NOT EQUAL            
         B     CMQX                                                             
CMQOK    CR    RB,RB                     SET CC EQUAL                           
CMQX     B     XIT                                                              
         EJECT                                                                  
                                                                                
*======================================================================         
* PRINT LOG                                                                     
*    ON ENTRY - R1 = LOG ENTRY NUMBER                                           
*======================================================================         
PRNT     NTR1                                                                   
         CHI   R1,MESSAGESET             MESSAGE MANUALLY SET                   
         BE    PRNT010                                                          
         MHI   R1,L'LOGTAB                                                      
         LA    R2,LOGTAB                                                        
         AR    R2,R1                                                            
         MVC   P(17),0(R2)               PRINT DESCRIPTION                      
*                                                                               
PRNT010  TIME  DEC                       PRINT TIME                             
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
         GOTO1 =V(PRINTER)                                                      
         B     XIT                                                              
         EJECT                                                                  
                                                                                
LOGTAB   DC    CL17'INITIALIZE'                                                 
         DC    CL17'ABOUTTOWAIT'                                                
         DC    CL17'WAITCOMPLETE'                                               
         DC    CL17'EXITING'                                                    
*                                                                               
INITIALIZE   EQU 0                                                              
ABOUTTOWAIT  EQU 1                                                              
WAITCOMPLETE EQU 2                                                              
EXITING      EQU 3                                                              
MESSAGESET   EQU 255                                                            
                                                                                
*======================================================================         
* GETEL / CONSTANTS                                                             
*======================================================================         
         GETEL R1,28,ELCODE                                                     
*                                                                               
HDREDICT DC    C'*HDR*EDICT='                                                   
*                                                                               
PGB      DS    0CL132                                                           
         DC    XL1'89'                                                          
         DC    CL131' '                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
*======================================================================         
* COMMON STORAGE AREA                                                           
*======================================================================         
COMMWORK DS    0D                                                               
AIO      DC    A(IO)                                                            
*                                                                               
DMCB     DS    10F                                                              
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
CARD     DS    CL80                                                             
WORK     DS    CL256                                                            
ELCODE   DS    X                                                                
IOKEY    DS    XL42                                                             
PRNTTIME DS    CL9                                                              
PRNTDUB  DS    D                                                                
PRNTMSG  DC    C'Y'                D=DEFAULT (SJR)                              
ERRCNT   DC    XL1'00'                                                          
INDIC    DS    X                                                                
INDCCS   EQU   X'80'                                                            
*                                                                               
CUSERID  DS    CL8                 USER-ID                                      
CUSERNO  DS    XL2                 USER-ID NUMBER                               
CUALPHA  DS    CL2                 ALPHA ID                                     
*                                                                               
ECBLST   DS    0F                                                               
         DC    X'00',AL3(INPQECB)  A(INPUT QUEUE ECB)                           
AOPERECB DC    X'80',AL3(0)        A(STOPECB)                                   
INPQECB  DC    F'0'                ECB FOR MQ GET FROM INPUT QUEUE              
OPERSTOP DC    C'N'                'Y' IF OPERATOR WANTS TO STOP                
DOMQCALL DS    C'Y'                'N' IF DO NOT DO MQ CALLS                    
*                                                                               
         DC    CL8'RLEN===>'                                                    
RLEN     DS    H                                                                
REC      DS    XL256                                                            
*                                                                               
ACOMM    DC    A(0)                A(COMMUNICATIONS PARAMETER LIST)             
AMSGIN   DC    A(0)                                                             
AMSGINX  DC    A(0)                                                             
*                                                                               
OPMSG    DC    C'AUTONOTE*'                                                     
OPMSGT   DC    CL30'AWIL:'                                                      
OPMSGM   DC    CL50' '                                                          
OPMSGLQ  EQU   *-OPMSG                                                          
                                                                                
*======================================================================         
* MQSERIES CALL PARAMETERS                                                      
*======================================================================         
QMGRNAME DC    CL48' '                                                          
QNAME    DC    CL48' '                                                          
QLOGDET  DS    C' '                                                             
QPRINTQ  DS    C' '                                                             
QEMAIL   DS    CL30' '                                                          
*                                                                               
HCONN    DS    F                   MQ QMGR CONNECTION HANDLE                    
HOBJ     DS    F                   OBJECT HANDLE                                
OPENOPTS DS    F                   MQOPEN OPTIONS                               
CLOSOPTS DS    F                   MQCLOSE OPTIONS                              
COMPCODE DS    F                   COMPLETION CODE                              
REASON   DS    F                   QUALIFIES COMPLETION CODE                    
DATALEN  DS    F                   LENGTH OF THE MESSAGE                        
MSGMAX   DC    A(2*1024*1024)      2M MESSAGE MAXIMUM                           
*                                                                               
         CMQA   LIST=YES,EQUONLY=NO                                             
OBJDESC  CMQODA LIST=YES           OBJECT DESCRIPTOR                            
MSGDESC  CMQMDA LIST=YES           MESSAGE DESCRIPTOR                           
         ORG   MSGDESC_FORMAT                                                   
         DC    CL8'MQSTR   '       MQFMT_STRING  FOR DATA FORMAT                
         ORG                                                                    
GMSGOPTS CMQGMOA LIST=YES          GET MESSAGE OPTIONS                          
         EJECT                                                                  
                                                                                
*======================================================================         
         DC    0D                                                               
         DC    CL16'*FIL*FIL*FIL*FIL'                                           
FLIST    DC    C'NCTFILE '                                                      
         DC    C'X       '                                                      
*                                                                               
         DS    0D                                                               
         DC    CL16'*SSB*SSB*SSB*SSB'                                           
       ++INCLUDE FASSBOFF                                                       
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'                SET EXTENDED OFFLINE SSB                    
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
*                                                                               
         DS    0D                                                               
         DC    CL16'*UTL*UTL*UTL*UTL'                                           
UTL      DC    F'0',X'0A',XL3'00',XL252'00'                                     
*                                                                               
         DS    0D                                                               
         DC    CL16'*IO1*IO1*IO1*IO1'                                           
IO       DC    2000X'00'            FOR DATAMGR (CONTROL SYSTEM)                
         EJECT                                                                  
                                                                                
*======================================================================         
* PARAMETER LISTS FOR MQSERIES CALLS                                            
*  F    A(ROUTINE)                                                              
*  CL16 EBCDIC ROUTINE NAME                                                     
*  XL1  FLAGS                                                                   
*  XL3  SPARE                                                                   
*  PARAMETERS (STANDARD IBM FORMAT)                                             
*======================================================================         
CSQBCONN_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_CONNECT'                                
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(QMGRNAME)                                     
                          DC    A(HCONN)                                        
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBOPEN_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_OPEN'                                   
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(HCONN)                                        
                          DC    A(OBJDESC)                                      
                          DC    A(OPENOPTS)                                     
                          DC    A(HOBJ)                                         
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBGET_STRUCTURE         DS    A                                               
                          DC    CL16'MQ_GET'                                    
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(HCONN)                                        
                          DC    A(HOBJ)                                         
                          DC    A(MSGDESC)                                      
                          DC    A(GMSGOPTS)                                     
                          DC    A(MSGMAX)                                       
BUFFER                    DC    A(0)                                            
                          DC    A(DATALEN)                                      
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCOMM_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_COMMIT'                                 
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(HCONN)                                        
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBCLOS_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_CLOSE'                                  
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(HCONN)                                        
                          DC    A(HOBJ)                                         
                          DC    A(CLOSOPTS)                                     
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
CSQBDISC_STRUCTURE        DS    A                                               
                          DC    CL16'MQ_DISCONNECT'                             
                          DC    X'00'                                           
                          DS    XL3                                             
                          DC    A(HCONN)                                        
                          DC    A(COMPCODE)                                     
                          DC    X'80',AL3(REASON)                               
*                                                                               
ENTRYPTS DS    0F                                                               
         DC    Y((ENTRYLSQ-ENTRYSTQ)/60)   NUMBER OF TABLE ENTRIES              
         DC    H'60'                       MUST REMAIN AS 60                    
ENTRYSTQ EQU   *                                                                
*                                                                               
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
*                                                                               
ENTRYLSQ EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    C'*CIREC**'                                                      
CIREC    DC    14336X'00'          PRINT QUEUE BUFFER                           
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
         EJECT                                                                  
                                                                                
*======================================================================         
       ++INCLUDE DDMQREASON                                                     
         EJECT                                                                  
       ++INCLUDE DMPRTQK                                                        
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE CTGENEDICT                                                     
         EJECT                                                                  
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*                                                                               
* IEZCIB                                                                        
         PRINT OFF                                                              
         DSECT                                                                  
         IEZCIB                                                                 
         PRINT ON                                                               
* IEZCOM                                                                        
         PRINT OFF                                                              
         IEZCOM                                                                 
         PRINT ON                                                               
*                                                                               
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030EDITOPQ   06/04/09'                                      
         END                                                                    
