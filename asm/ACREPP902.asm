*          DATA SET ACREPP902  AT LEVEL 090 AS OF 05/01/02                      
*PHASE ACP902A                                                                  
*INCLUDE DLFLD                                                                  
         TITLE 'PURCHACE ORDER RECORD LIST FOR JW/USMC'                         
ACP902   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACP9**,R9                                                    
         L     RA,0(R1)            RA=A(ACWORKD)                                
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND          RC=A(PROGRAM W/S)                            
         ST    RC,HOOKRC                                                        
         USING ACP9D,RC                                                         
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
         SPACE 2                                                                
XIT      XIT1  ,                   PROGRAM EXIT POINT                           
         EJECT                                                                  
         SPACE 2                                                                
RUNF     B     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
REQF     MVI   FORCECLR,C'Y'       REQFRST                                      
*                                                                               
         MVC   PAGE,=H'1'                                                       
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
*                                                                               
REQF10   CLC   QEND,SPACES                                                      
         BE    REQF20                                                           
         GOTO1 DATCON,DMCB,(0,QEND),(1,PEND)                                    
*                                                                               
REQF20   DS    0H                                                               
*                                                                               
         BAS   RE,SETSORT                                                       
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
*                                                                               
         BAS   RE,SETDOWN          SET DLBUFF                                   
*                                                                               
REQFX    B     XIT                                                              
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* TEST FOR INCLUSION AND IF ORDER HAS BEEN INVOICED, GET INVOICE      *         
* NUMBER/DATE FROM VENDOR ACCOUNT TRANSACTIONS                        *         
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
         ZAP   SRTINVAM,=P'0'                                                   
*                                                                               
         MVI   ELCODE,ORDELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   ORDERX                                                           
*                                                                               
         USING ORDELD,R2                                                        
*                                                                               
         CLC   ORDDATE,PSTART     APPLY DATE FILTERS                            
         BL    ORDERX                                                           
         CLC   ORDDATE,PEND                                                     
         BH    ORDERX                                                           
*                                                                               
         CLC   ORDJOB(6),=C'TSJMC ' USMC PO'S ONLY                              
         BNE   ORDERX                                                           
*                                                                               
         MVC   SRTSUPP,ORDSUP      SUPPLIER                                     
         MVC   SRTDATE,ORDDATE     ORDER DATE                                   
         MVC   SRTJOB,ORDACCA+6    JOB                                          
         MVC   SRTPROD,ORDACCA+3   PRODUCT                                      
*                                                                               
         USING OAMELD,R2                                                        
         L     R2,ADACC                                                         
         MVI   ELCODE,OAMELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   ORDERX                                                           
*                                                                               
         XC    ORDINDS,ORDINDS                                                  
*                                                                               
ORD50    AP    SRTORDAM,OAMAMNT    ORDER AMOUNT                                 
*                                                                               
         CP    OAMINUM,=P'0'       CHECK NUMBER OF INVOICES TO DATE             
         BE    *+8                 NONE                                         
         OI    ORDINDS,NEEDINV     SET TO GET INVOICE DATA                      
*                                                                               
         MVI   ELCODE,OAMELQ                                                    
         BAS   RE,NEXTEL                                                        
         BE    ORD50                                                            
*                                                                               
         BAS   RE,GETINV           GET INVOICE DATA                             
*                                                                               
         TM    ORDINDS,DIDSORT     DID I PUT RECORDS TO SORT IN GETINV          
         BO    *+8                 YES                                          
         BAS   RE,PUTSORT          PUT RECORD TO SORTER                         
*                                                                               
         SPACE 2                                                                
ORDERX   B     XIT                                                              
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
REQL     CLI   SORTACTV,C'Y'       NOTHING PUT TO SORTER - ENDSORT              
         BNE   REQL100                                                          
         XC    PREVSORT,PREVSORT                                                
         XC    PREVRANK,PREVRANK                                                
*                                                                               
REQL10   BAS   RE,GETSORT          ELSE READ SORTED RECORDS                     
         CLC   SRTRANK,PREVRANK    NEW RANK                                     
         BE    *+8                                                              
         BAS   RE,RANKTOTS         PRODUCE RANKING TOTALS                       
*                                                                               
         CLC   SORTKEY(SRTORDKL),PREVSORT    NEW P/O                            
         BE    REQL20                                                           
         BAS   RE,ORDTOTS          PRODUCE ORDER TOTALS                         
*                                                                               
         CLI   SRTRANK,ALL         BUMP INVOICED TOTALS                         
         BNE   *+10                                                             
         AP    REQINVAM,ORINVTOT                                                
*                                                                               
         AP    RANINVAM,ORINVTOT                                                
*                                                                               
REQL20   OC    SORTKEY,SORTKEY     LAST TIME?                                   
         BZ    REQL100                                                          
*                                                                               
         MVC   PREVRANK,SRTRANK                                                 
         MVC   PREVSORT,SORTREC                                                 
*                                                                               
         CLI   SRTISEQ,0           IS THIS THE FIRST INVOICE OF THE PO          
         BNE   REQL50              NO                                           
*                                                                               
         USING PRINTD,R5                                                        
         LA    R5,P                                                             
         MVC   PRJOB,SRTJOB                                                     
         MVC   PRPROD,SRTPROD                                                   
         MVC   PRORD,SRTORD                                                     
         GOTO1 DATCON,DMCB,(1,SRTDATE),(5,PRODATE)                              
         EDIT  (P6,SRTORDAM),(13,PROAMNT),2,FLOAT=-                             
         MVC   PRSUPPN,SRTSUPPN                                                 
         ZAP   ORINVTOT,=P'0'                                                   
*                                                                               
         AP    RANORDAM,SRTORDAM                                                
         AP    RANCNT,=P'1'                                                     
*                                                                               
         CLI   SRTRANK,ALL         BUMP P/O TOTS W/FIRST RECORD                 
         BE    REQL50              BUT USE RANKED RECORDS, NOT "ALL"            
*                                                                               
         AP    REQORDAM,SRTORDAM                                                
         AP    REQCNT,=P'1'                                                     
*                                                                               
REQL50   MVC   PRIREF,SRTINVNO                                                  
         GOTO1 DATCON,DMCB,(1,SRTINVDT),(5,PRIDATE)                             
         EDIT  (P6,SRTINVAM),(13,PRIAMNT),2,FLOAT=-                             
         AP    ORINVTOT,SRTINVAM                                                
         BAS   RE,PRINTEM                                                       
*                                                                               
         B     REQL10                                                           
*                                                                               
REQL100  BAS   RE,ENDSORT                                                       
         CLI   SORTACTV,C'Y'       NOTHING PUT TO SORTER - QUIT                 
         BNE   REQLX                                                            
*                                                                               
         BAS   RE,REPTOTS                                                       
*                                                                               
         CLI   PRTOPT,C'D'         DOWNLOADED                                   
         BNE   *+8                                                              
         BAS   RE,ENDDOWN                                                       
*                                                                               
REQLX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE READS ACCFILE FOR SUPPLIER ACCOUNT, LOOKS FOR 25 ELS TO     *         
* FIND THE INVOICES WHICH WERE CREATED BY THE P/O                     *         
* SAVES INVOICE NUMBER/AMOUNT/DATE                                    *         
* THE SUPPLIER NAME IS ALSO SAVED                                     *         
***********************************************************************         
*                                                                               
GETINV   NTR1  ,                                                                
*        BAS   RE,SAVESEQ          SAVE MONACCS READ SEQ                        
*                                                                               
         MVC   SRTSUPPN,SPACES                                                  
         XC    SRTISEQ,SRTISEQ                                                  
*                                                                               
         MVC   IOAREA(ACLENGTH-ACKEYD),SPACES                                   
         MVC   IOAREA(L'ACKEYACC),SRTSUPP                                       
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',IOAREA,IOAREA                    
*                                                                               
         CLI   DMCB+8,X'FF'-X'02'  OK IF DELETED                                
         BO    GETIX               RECORD NOT FOUND                             
*                                                                               
         USING NAMELD,R2                                                        
         LA    R2,IOAREA                                                        
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         BNM   *+6                                                              
         DC    H'0'                BAD LENGTH FOR NAME                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SRTSUPPN(0),NAMEREC                                              
*                                                                               
         TM    ORDINDS,NEEDINV     INVOICES ON THIS ORDER                       
         BNO   GETIX                                                            
*                                                                               
GETI20   GOTO1 DATAMGR,DMCB,DMRSEQ,=C'ACCOUNT',IOAREA,IOAREA                    
         LA    R2,IOAREA                                                        
         CLC   IOAREA(L'ACKEYACC),SRTSUPP SAME ACCOUNT                          
         BNE   GETIX                                                            
*                                                                               
         CLI   ACCORFST(R2),X'44'  GOT A TRAN                                   
         BNE   GETI20              TRY NEXT                                     
*                                                                               
         USING FFNELD,R2                                                        
         MVI   ELCODE,FFNELQ       LOOK FOR AN ORDER NUMBER                     
         BAS   RE,GETEL                                                         
         BNE   GETI20                                                           
         CLC   SRTORD,FFNONUM                                                   
         BNE   GETI20                                                           
*                                  SAVE INVOICE DATA                            
         LA    R2,IOAREA                                                        
         LA    R2,ACCORFST(R2)                                                  
*                                                                               
         USING TRNELD,R2                                                        
         MVC   SRTINVDT,TRNDATE                                                 
         MVC   SRTINVNO,TRNREF                                                  
         ZAP   SRTINVAM,TRNAMNT                                                 
         OI    ORDINDS,DIDSORT                                                  
         BAS   RE,PUTSORT                                                       
*                                                                               
         ZIC   R1,SRTISEQ                                                       
         LA    R1,1(R1)                                                         
         STC   R1,SRTISEQ                                                       
*                                                                               
         B     GETI20                                                           
*                                                                               
*ETIX    BAS   RE,RESTSEQ          RESTORE MONACCS READ SEQ                     
GETIX    B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        SAVE THE READ SEQUENCE                                                 
*----------------------------------------------------------------------         
***********************************************************************         
*AVESEQ  NTR1                                                         *         
*        L     RE,ADACCFIL         A(ACC DIR)                         *         
*        L     RE,ISPDKEY-ISDTF(RE)                                   *         
*        MVC   DCBKEY,0(RE)                                           *         
*        B     XIT                                                    *         
*                                                                     *         
*----------------------------------------------------------------------         
*        RESTORE THE READ SEQUENCE                                    *         
*----------------------------------------------------------------------         
*ESTSEQ  NTR1                                                         *         
*        LA    R3,DCBKEY                                              *         
*        GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',DCBKEY,IOAREA          *         
*        B     XIT                                                    *         
*                                                                     *         
***********************************************************************         
*                                                                               
*                                                                               
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
         SPACE 2                                                                
PUTSORT  NTR1  ,                                                                
         CLI   PRTOPT,C'D'         DOWNLOADING?                                 
         BE    PUTS10              YES, RANKED REPORTS ONLY                     
*                                                                               
         MVI   SRTRANK,ALL                                                      
         GOTO1 ADSORTER,DMCB,=C'PUT',SORTREC                                    
*                                                                               
PUTS10   MVI   SORTACTV,C'Y'                                                    
         MVI   SRTRANK,ZERO                                                     
         CP    SRTORDAM,=P'1000000'                                             
         BNH   PUTS50                                                           
         MVI   SRTRANK,TENK                                                     
*                                                                               
         CP    SRTORDAM,=P'2500000'                                             
         BNH   PUTS50                                                           
*                                                                               
         MVI   SRTRANK,TWO5K                                                    
         CP    SRTORDAM,=P'10000000'                                            
         BNH   PUTS50                                                           
         MVI   SRTRANK,HUNDREDK                                                 
*                                                                               
PUTS50   GOTO1 ADSORTER,DMCB,=C'PUT',SORTREC                                    
*                                                                               
PUTSX    B     XIT                                                              
         SPACE 2                                                                
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
         SPACE 2                                                                
ENDSORT  NTR1  ,                                                                
         GOTO1 ADSORTER,DMCB,=C'END'                                            
         B     SORTX                                                            
*                                                                               
SORTX    B     XIT                                                              
         SPACE 2                                                                
SORTCARD DC    CL80'SORT FIELDS=(01,000,A),FORMAT=BI,WORK=1'                    
RECDCARD DC    CL80'RECORD TYPE=F,LENGTH=(000,,,,)  '                           
         EJECT                                                                  
***********************************************************************         
* DOWNLOADING ROUTINES                                                *         
***********************************************************************         
*                                                                               
SETDOWN  NTR1                                                                   
         L     RF,=V(DLFLD)                                                     
         ST    RF,DOWNLOAD                                                      
*                                                                               
         L     RF,=A(DLBUFF)                                                    
         ST    RF,ADLBUFF                                                       
*                                                                               
         XC    HEADHOOK,HEADHOOK                                                
*                                                                               
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
*                                                                               
         BAS   RE,DOWNHEAD                                                      
*                                                                               
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
         MVC   P,H2                                                             
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
         L     R3,=A(RANKTAB)                                                   
         LA    R0,NRANKS                                                        
*                                                                               
         LA    R4,HEAD3+52                                                      
         CLI   PRTOPT,C'D'        DOWNLOADING                                   
         BNE   *+8                                                              
         LA    R4,P+32             HEADERS BUILT IN P                           
*                                                                               
HOOK10   CLC   0(L'SRTRANK,R3),SRTRANK                                          
         BNE   HOOK40                                                           
*                                                                               
         MVC   0(32,R4),1(R3)                                                   
         B     HOOKX                                                            
*                                                                               
HOOK40   LA    R3,RANKTBLN(R3)                                                  
         BCT   R0,HOOK10                                                        
HOOKX    BR    RE                                                               
HOOKRC   DS    A                                                                
         EJECT                                                                  
ORDTOTS  NTR1                                                                   
         LA    R2,PREVSORT                                                      
         CLI   SRTISEQ-SORTKEY(R2),0  DO I NEED TOTAL FOR ORDER                 
         BE    ORDTX                                                            
*                                                                               
         MVC   PROAMNT,=CL13'-------------'                                     
         MVC   PRIAMNT,=CL13'-------------'                                     
         BAS   RE,PRINTEM                                                       
         MVC   PRSUPPN+4(11),=C'TOTAL ORDER'                                    
         LA    R3,SRTORDAM-SORTKEY(R2)                                          
         EDIT  (P6,0(R3)),(13,PROAMNT),2,FLOAT=-                                
         EDIT  (P6,ORINVTOT),(13,PRIAMNT),2,FLOAT=-                             
         MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
*                                                                               
ORDTX    B     XIT                                                              
RANKTOTS NTR1                                                                   
*        CLI   PRTOPT,C'D'                                                      
*        BE    RANKX                                                            
         MVC   PROAMNT,=CL13'-------------'                                     
         MVC   PRIAMNT,=CL13'-------------'                                     
         BAS   RE,PRINTEM                                                       
         MVC   PRSUPPN+4(14),=C'TOTAL FOR RANK'                                 
         LA    R2,PREVSORT                                                      
         CLI   SRTRANK-SORTKEY(R2),ALL IS THIS ALL RANKS                        
         BNE   *+10                                                             
         MVC   PRSUPPN+4(20),=C'TOTAL FOR ALL ORDERS'                           
         EDIT  (P6,RANORDAM),(13,PROAMNT),2,FLOAT=-                             
         EDIT  (P6,RANINVAM),(13,PRIAMNT),2,FLOAT=-                             
         BAS   RE,PRINTEM                                                       
         MVC   PRSUPPN+4(14),=C'COUNT FOR RANK'                                 
         CLI   SRTRANK-SORTKEY(R2),ALL IS THIS ALL RANKS                        
         BNE   *+10                                                             
         MVC   PRSUPPN+4(16),=C'NUMBER OF ORDERS'                               
         EDIT  (P6,RANCNT),(13,PROAMNT)                                         
         BAS   RE,PRINTEM                                                       
         ZAP   RANORDAM,=P'0'                                                   
         ZAP   RANCNT,=P'0'                                                     
         ZAP   RANINVAM,=P'0'                                                   
         MVI   FORCEHED,C'Y'                                                    
RANKX    B     XIT                                                              
*                                                                               
REPTOTS  NTR1                                                                   
         MVC   PROAMNT,=CL13'-------------'                                     
         MVC   PRIAMNT,=CL13'-------------'                                     
         BAS   RE,PRINTEM                                                       
         MVC   PRSUPPN+4(16),=C'TOTAL FOR REPORT'                               
         EDIT  (P6,REQORDAM),(13,PROAMNT),2,FLOAT=-                             
         EDIT  (P6,REQINVAM),(13,PRIAMNT),2,FLOAT=-                             
         BAS   RE,PRINTEM                                                       
         MVC   PRSUPPN+4(16),=C'NUMBER OF ORDERS'                               
         EDIT  (P6,REQCNT),(13,PROAMNT)                                         
         BAS   RE,PRINTEM                                                       
         B     XIT                                                              
*                                                                               
         GETEL R2,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
RANKTAB  DS    0C                                                               
         DC    AL1(ALL)                                                         
         DC    CL32' '                                                          
RANKTBLN EQU   *-RANKTAB                                                        
         DC    AL1(ZERO)                                                        
         DC    CL32'     ORDERS UP TO $10,000'                                  
         DC    AL1(TENK)                                                        
         DC    CL32'ORDERS FROM $10,000 TO $25,000'                             
         DC    AL1(TWO5K)                                                       
         DC    CL32'ORDERS FROM $25,000 TO $100,000'                            
         DC    AL1(HUNDREDK)                                                    
         DC    CL32'     ORDERS OVER $100,000'                                  
NRANKS   EQU   (*-RANKTAB)/RANKTBLN                                             
ALL      EQU   0                                                                
ZERO     EQU   1                                                                
TENK     EQU   2                                                                
TWO5K    EQU   3                                                                
HUNDREDK EQU   4                                                                
         EJECT                                                                  
DOWNTAB  DS    0C                                                               
         DC    AL1(6,DLCBTXT,PRJOB-PRINTD)                                      
         DC    AL1(3,DLCBTXT,PRPROD-PRINTD)                                     
         DC    AL1(6,DLCBTXT,PRORD-PRINTD)                                      
         DC    AL1(8,DLCBTXT,PRODATE-PRINTD)                                    
         DC    AL1(31,DLCBTXT,PRSUPPN-PRINTD)                                   
         DC    AL1(13,DLCBNUM,PROAMNT-PRINTD)                                   
         DC    AL1(6,DLCBTXT,PRIREF-PRINTD)                                     
         DC    AL1(8,DLCBTXT,PRIDATE-PRINTD)                                    
         DC    AL1(13,DLCBNUM,PRIAMNT-PRINTD)                                   
NFIELDS  EQU   (*-DOWNTAB)/3                                                    
*                                                                               
*        WHEN DOWNLOADING HEADERS, ALL IS TEXT                                  
*                                                                               
HEADTAB  DS    0C                                                               
         DC    AL1(8,DLCBTXT,PRJOB-PRINTD)                                      
         DC    AL1(4,DLCBTXT,PRPROD-PRINTD)                                     
         DC    AL1(6,DLCBTXT,PRORD-PRINTD)                                      
         DC    AL1(8,DLCBTXT,PRODATE-PRINTD)                                    
         DC    AL1(28,DLCBTXT,PRSUPPN-PRINTD)                                   
         DC    AL1(13,DLCBTXT,PROAMNT-PRINTD)                                   
         DC    AL1(7,DLCBTXT,PRIREF-PRINTD)                                     
         DC    AL1(8,DLCBTXT,PRIDATE-PRINTD)                                    
         DC    AL1(13,DLCBTXT,PRIAMNT-PRINTD)                                   
         EJECT                                                                  
HEADAREA DS    0C                                                               
H1       DC    CL(L'P)' '                                                       
         ORG   H1                                                               
         DS    CL31' '                                                          
         DC    CL28'PRODUCTION ORDERS ISSUED FOR'                               
         ORG   *+L'H1                                                           
*                                                                               
*                                                                               
H2       DC    CL(L'P)' '                                                       
         ORG   H2                                                               
         DS    0CL(L'P)                                                         
         DS    CL30' '                                                          
         DC    CL28' UNITED STATES MARINE CORPS'                                
         ORG   *+L'H2                                                           
*                                                                               
H3       DC    CL(L'P)' '                                                       
         ORG   H3                                                               
         DC    CL1' '                                                           
         DC    C'ESTIMATE PROD ORDER'                                           
         DC    CL3' '                                                           
         DC    C'ORDER'                                                         
         DC    CL38' '                                                          
         DC    C'ORDER'                                                         
         DC    CL6' '                                                           
         DC    C'INVOICE'                                                       
         DC    CL1' '                                                           
         DC    C'INVOICE'                                                       
         DC    CL5' '                                                           
         DC    C'INVOICE'                                                       
         ORG   *+L'H3                                                           
*                                                                               
H4       DC    CL(L'P)' '                                                       
         ORG   H4                                                               
         DC    CL1' '                                                           
         DC    C'NUMBER   CODE NUMBER'                                          
         DC    CL2' '                                                           
         DC    C'DATE'                                                          
         DC    CL4' '                                                           
         DC    C'VENDOR NAME'                                                   
         DC    CL24' '                                                          
         DC    C'AMOUNT'                                                        
         DC    CL5' '                                                           
         DC    C'NUMBER'                                                        
         DC    CL3' '                                                           
         DC    C'DATE'                                                          
         DC    CL7' '                                                           
         DC    C'AMOUNT'                                                        
         ORG   *+L'H4                                                           
*                                                                               
H5       DC    CL(L'P)' '                                                       
         ORG   H5                                                               
         DC    CL1' '                                                           
         DC    C'-------- ---- ------'                                          
         DC    CL1' '                                                           
         DC    C'--------'                                                      
         DC    CL1' '                                                           
         DC    C'-------------------------------'                               
         DC    CL1' '                                                           
         DC    C'-------------'                                                 
         DC    CL1' '                                                           
         DC    C'-------'                                                       
         DC    CL1' '                                                           
         DC    C'-------'                                                       
         DC    CL1' '                                                           
         DC    C'-------------'                                                 
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
ACP9D    DSECT                     PROGRAM WORKING STORAGE                      
*                                  ***********************                      
SAVERE   DS    A                                                                
*                                  ***********************                      
ADLBUFF  DS    A                   A(DLFLD BUFFER)                              
DOWNLOAD DS    V                   V(DLFLD CSECT)                               
*                                  ***********************                      
ACCUMS   DS    0C                  TOTALS ACCUMULATORS                          
REQORDAM DS    PL6                 REQUEST TOTAL ORDER AMOUNT                   
REQINVAM DS    PL6                 REQUEST TOTAL INVOICED AMOUNT                
RANORDAM DS    PL6                 RANK TOTAL ORDER AMOUNT                      
RANINVAM DS    PL6                 RANK TOTAL INVOICED AMOUNT                   
ORINVTOT DS    PL6                 TOT INVOICED ON AN ORDER                     
RANCNT   DS    PL6                 NUMBER OF P/O'S PER RANK                     
REQCNT   DS    PL6                 NUMBER OF P/O IN REPORT                      
ACCULQ   EQU   6                   L'EACH ACCUMULATOR                           
ACCUNUM  EQU   (*-ACCUMS)/ACCULQ   NUMBER OF ACCUMULATORS                       
*                                                                               
PSTART   DS    CL3                 PACKED START YYMMDD                          
PEND     DS    CL3                 PACKED END YYMMDD                            
*                                                                               
SORTACTV DS    C                   SORTER ACTIVE (Y/N)                          
PREVSORT DS    CL(SORTRECL)                                                     
PREVRANK DS    CL1                                                              
*                                                                               
SORTREC  DS    0CL(SORTRECL)       'SORT' RECORD (FOR SORT OR PRINT)            
SORTKEY  DS    0CL(SORTKEYL)                                                    
SRTRANK  DS    CL1                 FOR P/O'S RANKED BY AMOUNT                   
SRTJOB   DS    CL6                                                              
SRTPROD  DS    CL3                                                              
SRTORD   DS    CL(L'ORDKORD)       ORDER NUMBER                                 
SRTDATE  DS    CL3                 ORDER DATE                                   
SRTORDKL EQU   *-SRTRANK                                                        
SRTISEQ  DS    CL1                 INVOICE SEQUENCE NUMBER                      
SORTKEYL EQU   *-SRTRANK           KEY LENGTH                                   
SRTSUPP  DS    CL15                SUPPLIER                                     
SRTSUPPN DS    CL36                SUPPLIER NAME                                
*                                                                               
SRTINVDT DS    CL3                 INVOICED DATE                                
SRTINVNO DS    CL6                 INVOICE NUMBER                               
SRTORDAM DS    PL6                 ORDER AMOUNT (ESTIMATE)                      
SRTINVAM DS    PL6                 INVOICED AMOUNT                              
SORTRECL EQU   *-SORTKEY           RECORD LENGTH                                
*                                                                               
ELCODE   DS    CL1                                                              
ORDINDS  DS    CL1                                                              
DIDSORT  EQU   1                                                                
NEEDINV  EQU   2                                                                
*                                                                               
PRTOPT   DS    CL1                 D=DOWNLOADING                                
DOWNDATA DS    CL(L'P)                                                          
*                                                                               
*CBKEY   DS    CL42                                                             
IOAREA   DS    2000C                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DSECT FOR A PRINT LINE                                              *         
***********************************************************************         
PRINTD   DSECT                                                                  
         DS    CL1                                                              
PRJOB    DS    CL6                                                              
         DS    CL3                                                              
PRPROD   DS    CL3                                                              
         DS    CL2                                                              
PRORD    DS    CL6                                                              
         DS    CL1                                                              
PRODATE  DS    CL8                                                              
         DS    CL1                                                              
PRSUPPN  DS    CL31                                                             
         DS    CL1                                                              
PROAMNT  DS    CL13                                                             
         DS    CL1                                                              
PRIREF   DS    CL6                                                              
         DS    CL2                                                              
PRIDATE  DS    CL8                                                              
         DS    CL1                                                              
PRIAMNT  DS    CL13                                                             
         EJECT                                                                  
*                                  *************                                
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
**PAN#1  DC    CL21'090ACREPP902 05/01/02'                                      
         END                                                                    
