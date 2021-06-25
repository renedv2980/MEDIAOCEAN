*          DATA SET ACREPP002  AT LEVEL 002 AS OF 09/07/11                      
*PHASE ACP002B                                                                  
*INCLUDE DLFLD                                                                  
         TITLE 'PRODUCTION ORDER LIST REPORT'                                   
***********************************************************************         
* OPTION 1 = Y, TRY TRSDATE FIRST                                     *         
* OPTION 2 = P = PRODUCTION ONLY                                      *         
*            E = EXPENSE ONLY                                         *         
*                                                                     *         
***********************************************************************         
ACP002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACP0**,R9                                                    
         L     RA,0(R1)            RA=A(ACWORKD)                                
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND          RC=A(PROGRAM W/S)                            
         ST    RC,HOOKRC                                                        
         USING ACP0D,RC                                                         
         LA    RF,HOOK                                                          
         ST    RF,HEADHOOK                                                      
*                                                                               
         CLI   MODE,RUNFRST        FIRST FOR RUN                                
         BE    RUNF                                                             
         CLI   MODE,REQFRST        FIRST FOR REQUEST                            
         BE    REQF                                                             
         CLI   MODE,PROCORD        READ AN ORDER RECORD                         
         BE    ORDER                                                            
         CLI   MODE,REQLAST        LAST FOR REQUEST                             
         BE    REQL                                                             
*                                                                               
XIT      XIT1  ,                   PROGRAM EXIT POINT                           
*                                                                               
RUNF     B     XIT                                                              
         EJECT                                                                  
REQF     MVC   PAGE,=H'1'                                                       
         LA    RF,ACCUMS           RF=A(TOTALS ACCUMULATORS)                    
         LA    R0,ACCUNUM          R0=NUMBER OF TOTALS ACCUMULATORS             
         ZAP   0(ACCULQ,RF),=P'0'  ZAP ACCUMULATOR                              
         LA    RF,ACCULQ(RF)       NEXT ACCUMULATOR                             
         BCT   R0,*-10                                                          
*                                                                               
         XC    PSTART,PSTART                                                    
         MVC   PEND,=3X'FF'                                                     
*                                  DTUSED=TODAY IF QOPT3 NEQ R                  
         CLC   QSTART,SPACES       START DATE PRESENT?                          
         BE    REQF10                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(1,PSTART)                                
         GOTO1 DATCON,DMCB,(0,QSTART),(2,TSTART)                                
*                                                                               
REQF10   CLC   QEND,SPACES                                                      
         BE    REQF20                                                           
         GOTO1 DATCON,DMCB,(0,QEND),(1,PEND)                                    
         GOTO1 DATCON,DMCB,(0,QEND),(2,TEND)                                    
*                                                                               
REQF20   BAS   RE,SETSORT                                                       
*                                                                               
         MVI   PRTOPT,C'P'                                                      
         MVI   FORCEHED,C'Y'                                                    
         USING MASTD,R2                                                         
         L     R2,ADMASTC                                                       
         LA    R2,MCREMOTE                                                      
         USING REMOTED,R2                                                       
         CLC   =C'DOWN',REMOTFNO   DOWNLOADING                                  
         BNE   REQFX               NO                                           
         MVI   PRTOPT,C'D'                                                      
         BAS   RE,SETDOWN          SET DLBUFF                                   
*                                                                               
REQFX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* READ AND SORT ORDER RECORDS                                         *         
***********************************************************************         
*                                                                               
ORDER    L     R2,ADACC            PROCORD                                      
         USING ORDRECD,R2                                                       
         CLC   ORDKORD,=C'000000'  SKIP CONTROL RECORD                          
         BE    ORDERX                                                           
*                                                                               
         LA    R0,SORTREC          CLEAR SORT RECORD AREA                       
         LA    R1,SORTRECL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   SRTORD(L'ORDKORD),ORDKORD                                        
         ZAP   SRTORDAM,=P'0'                                                   
*                                                                               
         MVI   SRTSTAT,C'C'                                                     
         TM    ORDRSTAT,ORDCLOSE   IS IT CLOSED?                                
         BO    ORD10               YES                                          
         MVI   SRTSTAT,C'F'                                                     
         TM    ORDRSTAT,ORDSFMCH   IS IT FULLY MATCHED?                         
         BO    ORD10               YES                                          
         MVI   SRTSTAT,C'D'                                                     
         TM    ORDRSTAT,ORDSDEL    IS IT DELETED?                               
         BO    ORD10                                                            
         MVI   SRTSTAT,C'L'                                                     
         TM    ORDRSTAT,ORDSLDEL   IS IT LOGICALLY DELETED?                     
         BO    ORD10                                                            
         MVI   SRTSTAT,C'O'        NO, MUST BE OPEN                             
*                                                                               
ORD10    MVI   ELCODE,ORDELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   ORDERX                                                           
*                                                                               
         CLI   QOPT1,C'Y'          USING TRSDATE?                               
         BE    ORD20               YES                                          
         USING ORDELD,R2                                                        
         CLC   ORDDATE,PSTART      NO, USE ORDER DATE                           
         BL    ORDERX                                                           
         CLC   ORDDATE,PEND                                                     
         BH    ORDERX                                                           
         MVC   SRTDATE,ORDDATE     ORDER DATE                                   
*                                                                               
ORD20    MVC   SRTSUPP,ORDSUP      SUPPLIER                                     
         MVC   SRTJOB,ORDACCA+6    JOB                                          
         MVC   SRTPRO,ORDACCA+3    PRODUCT                                      
         MVC   SRTCLI,ORDACCA      CLIENT                                       
         MVC   SRTUNL,ORDACCU      CLIENT                                       
         MVI   SRTTYP,C'P'         SET AS PRODUCTION                            
         CLC   SRTUNL,PRODUL                                                    
         BE    *+8                                                              
         MVI   SRTTYP,C'E'         IF NOT, SET AS EXPENSE                       
*                                                                               
         CLI   QOPT2,C' '            ANY OR ALL ORDERS?                         
         BE    ORD25                                                            
         CLC   QOPT2,SRTTYP        IF TYPE MATCHES, TAKE IT                     
         BNE   ORDERX                                                           
*                                                                               
ORD25    CLI   QOPT1,C'Y'          LOOK FOR TRSDATE?                            
         BNE   ORD30                                                            
         L     R2,ADACC            PROCORD                                      
         MVI   ELCODE,TRSELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   ORDERX                                                           
         USING TRSELD,R2                                                        
         CLC   TRSDATE,TSTART      APPLY DATE FILTERS TO TRSEL                  
         BL    ORDERX                                                           
         CLC   TRSDATE,TEND                                                     
         BH    ORDERX                                                           
         GOTO1 DATCON,DMCB,(2,TRSDATE),(1,SRTDATE)                              
*                                                                               
         USING OAMELD,R2                                                        
ORD30    L     R2,ADACC                                                         
         MVI   ELCODE,OAMELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   ORDERX                                                           
*                                                                               
ORD40    AP    SRTORDAM,OAMAMNT    ORDER AMOUNT                                 
         MVI   ELCODE,OAMELQ                                                    
         BAS   RE,NEXTEL                                                        
         BE    ORD40                                                            
         BAS   RE,PUTSORT          PUT RECORD TO SORTER                         
*                                                                               
ORDERX   B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
REQL     CLI   SORTACTV,C'Y'       NOTHING PUT TO SORTER - ENDSORT              
         BNE   REQL100                                                          
         XC    PREVSORT,PREVSORT                                                
*                                                                               
REQL10   BAS   RE,GETSORT          ELSE READ SORTED RECORDS                     
*                                                                               
         CLC   SORTKEY(SRTORDKL),PREVSORT    NEW P/O                            
         BE    REQL20                                                           
         BAS   RE,ORDTOTS          PRODUCE ORDER TOTALS                         
*                                                                               
REQL20   OC    SORTKEY,SORTKEY     LAST TIME?                                   
         BZ    REQL100                                                          
         MVC   PREVSORT,SORTREC                                                 
         CLI   SRTISEQ,0           IS THIS THE FIRST INVOICE OF THE PO          
         BNE   REQL50              NO                                           
*                                                                               
         USING PRINTD,R5                                                        
         LA    R5,P                                                             
         MVC   P,SPACES                                                         
         MVC   PRJOB,SRTJOB                                                     
         MVC   PRPRO,SRTPRO                                                     
         MVC   PRCLI,SRTCLI                                                     
         MVC   PRUNL,SRTUNL                                                     
         MVC   PRORD,SRTORD                                                     
         MVC   PROSTAT,SRTSTAT                                                  
         GOTO1 DATCON,DMCB,(1,SRTDATE),(5,PRODATE)                              
         EDIT  (P6,SRTORDAM),(13,PROAMNT),2,MINUS=YES                           
         MVC   PRSUPP,SRTSUPP+1                                                 
         AP    REQORDAM,SRTORDAM                                                
         AP    REQCNT,=P'1'                                                     
*                                                                               
REQL50   BAS   RE,PRINTEM                                                       
         B     REQL10                                                           
*                                                                               
REQL100  BAS   RE,ENDSORT                                                       
         CLI   SORTACTV,C'Y'       NOTHING PUT TO SORTER - QUIT                 
         BNE   REQLX                                                            
         BAS   RE,REPTOTS                                                       
         CLI   PRTOPT,C'D'         DOWNLOADED                                   
         BNE   *+8                                                              
         BAS   RE,ENDDOWN                                                       
*                                                                               
REQLX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SORTER INTERFACE ROUTINES                                           *         
***********************************************************************         
*                                                                               
SETSORT  NTR1  ,                                                                
         LA    R1,SORTKEYL         SORT KEY LENGTH INTO SORTCARD                
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+16(3),DUB                                               
         LA    R1,SORTRECL         SORT RECORD LENGTH INTO RECDCARD             
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECDCARD+22(3),DUB                                               
         GOTO1 ADSORTER,DMCB,SORTCARD,RECDCARD                                  
         MVI   SORTACTV,C'N'                                                    
         B     SORTX                                                            
*                                                                               
PUTSORT  NTR1  ,                                                                
         MVI   SORTACTV,C'Y'                                                    
         GOTO1 ADSORTER,DMCB,=C'PUT',SORTREC                                    
*                                                                               
PUTSX    B     XIT                                                              
*                                                                               
GETSORT  NTR1  ,                                                                
         GOTO1 ADSORTER,DMCB,=C'GET',0                                          
         XC    SORTKEY,SORTKEY                                                  
         L     RE,DMCB+4           RE=A(SORTED RECORD)                          
         LTR   RE,RE                                                            
         BZ    SORTX                                                            
*                                                                               
         LA    R0,SORTREC                                                       
         LA    R1,SORTRECL                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE SORTED RECORD BACK INTO W/S             
         B     SORTX                                                            
*                                                                               
ENDSORT  NTR1  ,                                                                
         GOTO1 ADSORTER,DMCB,=C'END'                                            
*                                                                               
SORTX    B     XIT                                                              
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(01,000,A),FORMAT=BI,WORK=1'                    
RECDCARD DC    CL80'RECORD TYPE=F,LENGTH=(000,,,,)  '                           
*                                                                               
PRODUL   DC    C'SJ'                                                            
         EJECT                                                                  
***********************************************************************         
* DOWNLOADING ROUTINES                                                *         
***********************************************************************         
*                                                                               
SETDOWN  NTR1                                                                   
         L     RF,=V(DLFLD)                                                     
         ST    RF,DOWNLOAD                                                      
         L     RF,=A(DLBUFF)                                                    
         ST    RF,ADLBUFF                                                       
         XC    HEADHOOK,HEADHOOK                                                
         L     R4,ADLBUFF                                                       
         USING DLCBD,R4                                                         
         MVI   DLCBACT,DLCBSOR     DOWN LOAD ACTION IS START                    
         SPACE 1                                                                
         LA    RE,P                                                             
         ST    RE,DLCBAPL                                                       
         SPACE 1                                                                
         LA    RE,DLPRINT          SAVE A(HOOK)                                 
         ST    RE,DLCBAPR                                                       
         GOTO1 DOWNLOAD,(R4)                                                    
*                                  DO HEADERS                                   
         L     R4,ADLBUFF                                                       
         USING DLCBD,R4                                                         
         BAS   RE,DOWNHEAD                                                      
         B     XIT                                                              
*                                                                               
PUTDOWN  NTR1                                                                   
         L     R2,P1               P1 IS TABLE DESCRIBING DATA                  
         SR    R0,R0                                                            
         L     R0,P2               P2 IS NUMBER OF FIELDS TO DOWNLOAD           
*                                                                               
         MVC   DOWNDATA,P          SAVE P                                       
         MVC   P,SPACES                                                         
         USING DLCBD,R4                                                         
         L     R4,ADLBUFF          DOWNLOAD CONTROL BUFFER                      
         LA    R3,DOWNDATA                                                      
*                                                                               
PUTD10   ZIC   R5,2(R2)            OFFSET INTO DOWNDATA                         
         AR    R5,R3                                                            
         IC    R6,0(R2)            LENGTH OF THIS FIELD                         
         MVC   DLCBLEN,0(R2)                                                    
         MVC   DLCBTYP,1(R2)       TYPE OF THIS FIELD                           
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
*                                                                               
         CH    R6,=Y(L'DLCBFLD)    CHECK SIZE TO DOWN LOAD                      
         BNH   *+8                                                              
         LA    R6,L'DLCBFLD                                                     
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(R5)                                                 
*                                                                               
         GOTO1 DOWNLOAD,(R4)                                                    
*                                                                               
         LA    R2,3(R2)                                                         
         BCT   R0,PUTD10                                                        
*                                                                               
         L     R4,ADLBUFF                                                       
         MVI   DLCBACT,DLCBEOL     MARK END-OF-LINE                             
         GOTO1 DOWNLOAD,(R4)                                                    
         B     XIT                                                              
*                                                                               
ENDDOWN  NTR1                                                                   
         USING DLCBD,R4                                                         
         L     R4,ADLBUFF          YES, SO MARK END OF REPORT                   
         MVI   DLCBACT,DLCBEOR     END OF REPORT                                
         GOTO1 DOWNLOAD,(R4)                                                    
         B     XIT                                                              
*                                                                               
*        HOOK FOR DOWNLOAD ROUTINE                                              
DLPRINT  NTR1                                                                   
         MVI   SPACING,1                                                        
         MVI   LINE,1                                                           
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'       KEEP EVERY THING ON CURRENT PAGE             
         MVI   NEWPAGE,C'N'        TURN OFF SKIP TO CHANNEL 1 STUFF             
         GOTO1 ACREPORT                                                         
         XIT1                                                                   
*                                                                               
*        HOOK FOR DOWNLOAD WITH TOF                                             
DLPRHEAD NTR1                                                                   
         MVI   SPACING,1                                                        
         MVI   LINE,1                                                           
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'Y'                                                    
         MVI   NEWPAGE,C'Y'                                                     
         GOTO1 ACREPORT                                                         
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
PRINTEM  EQU   *                                                                
         ST    RE,SAVERE                                                        
         CLI   PRTOPT,C'D'         DOWNLOAD?                                    
         BE    PRINT10             YES                                          
*                                                                               
         GOTO1 ACREPORT                                                         
         B     PRINTX                                                           
*                                                                               
PRINT10  GOTO1 PUTDOWN,DMCB,DOWNTAB,NFIELDS                                     
*                                                                               
PRINTX   L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
DOWNHEAD NTR1                                                                   
         MVI   RCSUBPRG,1          TURN OFF HEADER PRINT                        
         LA    RE,DLPRHEAD         USE HEAD HOOK W/TOP OF FORM                  
         ST    RE,DLCBAPR                                                       
*                                                                               
         MVC   P,SPACES            PUT OUT A BLANK LINE                         
         GOTO1 PUTDOWN,DMCB,HEADTAB,NFIELDS                                     
         LA    RE,DLPRINT          USE HEAD HOOK, NO FORM FEED                  
         ST    RE,DLCBAPR                                                       
*                                                                               
         MVC   P,H1                                                             
         GOTO1 (RF),(R1)                                                        
         MVC   P,H3                                                             
         BAS   RE,HOOK                                                          
*                                                                               
         GOTO1 (RF),(R1)                                                        
         MVC   P,H4                                                             
         GOTO1 (RF),(R1)                                                        
         MVC   P,H5                                                             
         GOTO1 (RF),(R1)                                                        
         B     XIT                                                              
***********************************************************************         
* HOOK ROUTINE                                                        *         
***********************************************************************         
*                                                                               
HOOK     DS    0H                                                               
         L     RC,HOOKRC                                                        
HOOKX    BR    RE                                                               
HOOKRC   DS    A                                                                
         EJECT                                                                  
ORDTOTS  NTR1                                                                   
         LA    R2,PREVSORT                                                      
         CLI   SRTISEQ-SORTKEY(R2),0  DO I NEED TOTAL FOR ORDER                 
         BE    ORDTX                                                            
*                                                                               
         MVC   PROAMNT,=CL13'-------------'                                     
         BAS   RE,PRINTEM                                                       
         MVC   PRODATE(11),=C'TOTAL ORDER'                                      
         LA    R3,SRTORDAM-SORTKEY(R2)                                          
         EDIT  (P6,0(R3)),(13,PROAMNT),2,MINUS=YES                              
         MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
*                                                                               
ORDTX    B     XIT                                                              
*                                                                               
REPTOTS  NTR1                                                                   
         MVC   PROAMNT,=CL13'-------------'                                     
         BAS   RE,PRINTEM                                                       
         MVC   PRODATE(16),=C'TOTAL FOR REPORT'                                 
         EDIT  (P6,REQORDAM),(13,PROAMNT),2,MINUS=YES                           
         BAS   RE,PRINTEM                                                       
         MVC   PRODATE(16),=C'NUMBER OF ORDERS'                                 
         EDIT  (P6,REQCNT),(13,PROAMNT-1)                                       
         BAS   RE,PRINTEM                                                       
         B     XIT                                                              
*                                                                               
         GETEL R2,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
DOWNTAB  DS    0C                                                               
         DC    AL1(2,DLCBTXT,PRUNL-PRINTD)                                      
         DC    AL1(3,DLCBTXT,PRCLI-PRINTD)                                      
         DC    AL1(3,DLCBTXT,PRPRO-PRINTD)                                      
         DC    AL1(6,DLCBTXT,PRJOB-PRINTD)                                      
         DC    AL1(6,DLCBTXT,PRORD-PRINTD)                                      
         DC    AL1(1,DLCBTXT,PROSTAT-PRINTD)                                    
         DC    AL1(8,DLCBTXT,PRODATE-PRINTD)                                    
         DC    AL1(14,DLCBTXT,PRSUPP-PRINTD)                                    
         DC    AL1(13,DLCBNUM,PROAMNT-PRINTD)                                   
NFIELDS  EQU   (*-DOWNTAB)/3                                                    
*                                                                               
*        WHEN DOWNLOADING HEADERS, ALL IS TEXT                                  
*                                                                               
HEADTAB  DS    0C                                                               
         DC    AL1(3,DLCBTXT,PRUNL-PRINTD)                                      
         DC    AL1(4,DLCBTXT,PRCLI-PRINTD)                                      
         DC    AL1(4,DLCBTXT,PRPRO-PRINTD)                                      
         DC    AL1(8,DLCBTXT,PRJOB-PRINTD)                                      
         DC    AL1(6,DLCBTXT,PRORD-PRINTD)                                      
         DC    AL1(1,DLCBTXT,PROSTAT-PRINTD)                                    
         DC    AL1(8,DLCBTXT,PRODATE-PRINTD)                                    
         DC    AL1(12,DLCBTXT,PRSUPP-PRINTD)                                    
         DC    AL1(13,DLCBTXT,PROAMNT-PRINTD)                                   
         EJECT                                                                  
HEADAREA DS    0C                                                               
H1       DC    CL(L'P)' '                                                       
         ORG   H1                                                               
         DS    CL31' '                                                          
         DC    CL28'PRODUCTION ORDERS ISSUED FOR'                               
         ORG   *+L'H1                                                           
*                                                                               
H3       DC    CL(L'P)' '                                                       
         ORG   H3                                                               
         DC    CL1' '                                                           
         DC    C'UL CLI/PRO/JOB   ORDER'                                        
         DC    CL3' '                                                           
         DC    C'ORDER'                                                         
         DC    CL38' '                                                          
         DC    C'ORDER'                                                         
         DC    CL6' '                                                           
         ORG   *+L'H3                                                           
*                                                                               
H4       DC    CL(L'P)' '                                                       
         ORG   H4                                                               
         DC    CL3' '                                                           
         DC    C'              NUMBER'                                          
         DC    CL2' '                                                           
         DC    C'DATE'                                                          
         DC    CL4' '                                                           
         DC    C'VENDOR NAME'                                                   
         DC    CL24' '                                                          
         DC    C'AMOUNT'                                                        
         DC    CL5' '                                                           
         ORG   *+L'H4                                                           
*                                                                               
H5       DC    CL(L'P)' '                                                       
         ORG   H5                                                               
         DC    CL1' '                                                           
         DC    C'-- -------- ---- ------'                                       
         DC    CL1' '                                                           
         DC    C'--------'                                                      
         DC    CL1' '                                                           
         DC    C'-------------------------------'                               
         DC    CL1' '                                                           
         DC    C'-------------'                                                 
         DC    CL1' '                                                           
         ORG   *+L'H5                                                           
         EJECT                                                                  
DLBUFF   DS    (DLCBXL)C           DLFLD INTERFACE BUFFER                       
         EJECT                                                                  
*                                  ********                                     
*                                  LITERALS                                     
*                                  ********                                     
         LTORG                                                                  
         EJECT                                                                  
*                                  ***********************                      
ACP0D    DSECT                     PROGRAM WORKING STORAGE                      
SAVERE   DS    A                                                                
*                                  ***********************                      
ADLBUFF  DS    A                   A(DLFLD BUFFER)                              
DOWNLOAD DS    V                   V(DLFLD CSECT)                               
*                                  ***********************                      
ACCUMS   DS    0C                  TOTALS ACCUMULATORS                          
REQORDAM DS    PL6                 REQUEST TOTAL ORDER AMOUNT                   
REQINVAM DS    PL6                 REQUEST TOTAL INVOICED AMOUNT                
REQCNT   DS    PL6                 NUMBER OF P/O IN REPORT                      
ACCULQ   EQU   6                   L'EACH ACCUMULATOR                           
ACCUNUM  EQU   (*-ACCUMS)/ACCULQ   NUMBER OF ACCUMULATORS                       
*                                                                               
PSTART   DS    CL3                 PACKED START YYMMDD                          
PEND     DS    CL3                 PACKED END YYMMDD                            
TSTART   DS    CL2                 COMPRESSED START YYMMDD                      
TEND     DS    CL2                 COMPRESSED END YYMMDD                        
*                                                                               
SORTACTV DS    C                   SORTER ACTIVE (Y/N)                          
PREVSORT DS    CL(SORTRECL)                                                     
*                                                                               
SORTREC  DS    0CL(SORTRECL)       'SORT' RECORD (FOR SORT OR PRINT)            
SORTKEY  DS    0CL(SORTKEYL)                                                    
SRTUNL   DS    CL2                                                              
SRTCLI   DS    CL3                                                              
SRTPRO   DS    CL3                                                              
SRTJOB   DS    CL6                                                              
SRTORD   DS    CL(L'ORDKORD)       ORDER NUMBER                                 
SRTSTAT  DS    CL1                 ORDER STATUS                                 
SRTDATE  DS    CL3                 ORDER DATE                                   
SRTORDKL EQU   *-SRTCLI                                                         
SRTISEQ  DS    CL1                 INVOICE SEQUENCE NUMBER                      
SORTKEYL EQU   *-SRTCLI            KEY LENGTH                                   
SRTSUPP  DS    CL15                SUPPLIER                                     
SRTORDAM DS    PL6                 ORDER AMOUNT (ESTIMATE)                      
SRTTYP   DS    C                   ORDER TYPE (E OR P)                          
SORTRECL EQU   *-SORTKEY           RECORD LENGTH                                
*                                                                               
ELCODE   DS    CL1                                                              
*                                                                               
PRTOPT   DS    CL1                 D=DOWNLOADING                                
DOWNDATA DS    CL(L'P)                                                          
*                                                                               
IOAREA   DS    2000C                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DSECT FOR A PRINT LINE                                              *         
***********************************************************************         
PRINTD   DSECT                                                                  
         DS    CL1                                                              
PRUNL    DS    CL2                                                              
         DS    CL1                                                              
PRCLI    DS    CL3                                                              
         DS    CL2                                                              
PRPRO    DS    CL3                                                              
         DS    CL2                                                              
PRJOB    DS    CL6                                                              
         DS    CL2                                                              
PRORD    DS    CL6                                                              
         DS    CL2                                                              
PROSTAT  DS    CL1                                                              
         DS    CL2                                                              
PRODATE  DS    CL8                                                              
         DS    CL2                                                              
PRSUPP   DS    CL14                                                             
         DS    CL2                                                              
PROAMNT  DS    CL13                                                             
         EJECT                                                                  
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* DDEBLOCK                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDEBLOCK                                                       
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDBOXEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
         PRINT ON                                                               
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
* DDDLCB                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
* ACGOBLOCK                                                                     
         PRINT OFF                                                              
GOBLOCKD DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPP002 09/07/11'                                      
         END                                                                    
