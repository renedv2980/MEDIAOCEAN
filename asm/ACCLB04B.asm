*          DATA SET ACCLB04B   AT LEVEL 181 AS OF 12/21/99                      
*PHASE T62104B                                                                  
CLB04    TITLE '- BILL PROGRAM - FORMAT ACTIVITY INTO BILL RECORD'              
CLB04    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL FWORKL,**CLB4**,R8,R7,CLEAR=YES,RR=RE                            
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         USING FWORKD,RC                                                        
         ST    RE,BORELO                                                        
         LH    RE,=Y(MSTREC-FWORKD)                                             
         AR    RE,RC                                                            
         ST    RE,AMSTREC                                                       
         LA    R5,OSVALS                                                        
         USING OSVALSD,R5                                                       
         L     R6,ALSVALS                                                       
         USING LSVALSD,R6                                                       
         USING PRORATAD,PRATBLK                                                 
         USING TLSTD,LSTLST                                                     
         USING FBLKD,FFMTBLK                                                    
*                                                                               
         SR    RE,RE               SET PREVIOUS SESSION RECORD/ACTION           
         IC    RE,TWASESNL                                                      
         SLL   RE,1                                                             
         LA    RE,TWASESRA-L'TWASESRA(RE)                                       
         MVC   FIRECACT,0(RE)                                                   
*                                                                               
         USING ACTRECD,R2                                                       
INI00    MVC   IODAOVER,BCJOBDA    READ JOB FOR ALLOCATION TOTALS               
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,AGOPBLK          BILLING EXTENSION REQUIRED                   
         MVC   GOABEXT-GOBLOCKD(L'GOABEXT,RE),AJOBBLK                           
         GOTO1 AGETOPT,BODMCB,AIO1                                              
*                                                                               
         ZAP   PREAALLO,BCPZERO    CLEAR PRIOR BILLING TOTALS                   
         ZAP   PREACOMM,BCPZERO                                                 
         ZAP   PREFALLO,BCPZERO                                                 
         ZAP   PREFCOMM,BCPZERO                                                 
         ZAP   PREHRSB,BCPZERO                                                  
         ZAP   PREWUHRS,BCPZERO                                                 
         ZAP   PREWUAMT,BCPZERO                                                 
*                                                                               
         L     R2,AIO1                                                          
         LA    R3,ACTRFST                                                       
         ZAP   FJOBNET,BCPZERO                                                  
         ZAP   FJOBCOM,BCPZERO                                                  
         SR    R0,R0                                                            
         USING SCIEL,R3                                                         
INI10    CLI   SCIEL,0                                                          
         BNE   *+14                NO ALLOC ELEMENT IS ERROR                    
         MVC   FVMSGNO,=AL2(AE$NACOJ)                                           
         B     INIERR                                                           
         CLI   SCIEL,SCIELQ                                                     
         BE    INI30                                                            
INI20    IC    R0,SCILN                                                         
         AR    R3,R0                                                            
         B     INI10                                                            
INI30    CLI   SCITYPE,SCITCBAP                                                 
         BNE   INI20                                                            
         ZAP   FJOBNET,SCIAMNT     NET ALLOCATION                               
         ZAP   FJOBCOM,SCIADMN     COMMISSION ALLOCATED                         
*                                                                               
         LA    R2,IOKEY            READ FORMAT CONTROL RECORD                   
         USING PBCRECD,R2                                                       
         XC    PBCKEY,PBCKEY                                                    
         MVI   PBCKTYP,PBCKTYPQ                                                 
         MVC   PBCKCPY,CUABIN                                                   
         MVI   PBCKSUB,PBCKCONQ                                                 
         MVC   PBCKFMT,CSFORMAT                                                 
*        MVC   PBCKLANG,GOBILLNG   BILL LANGUAGE MAY BE SET IN OPT MNT          
         MVC   PBCKLANG,CSFMLANG                                                
         GOTO1 AIO,IO3+IOACCMST+IORD                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,AMSTREC                                                       
         L     R2,AIO3                                                          
         SR    RF,RF                                                            
         ICM   RF,3,PBCRLEN                                                     
         BCTR  RF,0                                                             
         LR    R3,RE                                                            
         AR    R3,RF               R3=A(SLOT FOR CONTINUATION RECORD)           
         L     R0,AIO3                                                          
*                                                                               
INIT52   LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         LA    R2,IOKEY                                                         
         SR    RE,RE                                                            
         IC    RE,PBCKSEQ                                                       
         LA    RE,1(RE)                                                         
         STC   RE,PBCKSEQ                                                       
         GOTO1 AIO,IO3+IOACCMST+IORD                                            
         BNE   INIT54                                                           
         LR    RE,R3               A(DESTINATION)                               
         L     R2,AIO3                                                          
         SR    RF,RF                                                            
         ICM   RF,3,PBCRLEN                                                     
         SH    RF,=Y(PBCRFST-PBCRECD+1)                                         
         AR    R3,RF               R3=A(SLOT FOR CONTINUATION RECORD)           
         L     R0,AIO3                                                          
         AH    R0,=Y(PBCRFST-PBCRECD)                                           
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         B     INIT52                                                           
*                                                                               
INIT54   MVI   0(R3),0             SET DUMMY E-O-RECORD MARKER                  
         GOTO1 AFMTBLK,BOPARM,('FBGET',FBLKD),AMSTREC                           
         MVI   FNPARDES,C'N'                                                    
         L     RE,AMSTREC                                                       
         LA    RE,PBCRFST-PBCRECD(RE)                                           
         MVI   FPREVB,C'N'         SET NO PREVIOUS BILLING                      
         SR    R0,R0                                                            
         B     *+8                                                              
INI60    IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0             TEST E-O-R                                   
         BE    INI100                                                           
         CLI   0(RE),BOFELQ                                                     
         BNE   INI62                                                            
         TM    BOFINDS2-BOFELD(RE),BOFIPREV                                     
         BZ    INI60                                                            
         MVI   FPREVB,C'Y'                                                      
         B     INI60                                                            
INI62    CLI   0(RE),BLFELQ        TEST DATA FORMAT ELEMENT                     
         BNE   INI60                                                            
         CLI   BLFFLD-BLFELD(RE),BLFFWCNQ                                       
         BNE   INI60                                                            
         MVI   FNPARDES,C'Y'       SET NO PARAGRAPH DESCRIPTION                 
         B     INI60                                                            
INI100   BAS   RE,BLDKLIST         BUILD LISTS OF KEY ITEMS                     
         BAS   RE,SETDEFN                                                       
         L     R2,AMSTREC                                                       
         B     HDR00                                                            
*                                                                               
INIERR   DS    0H                                                               
         B     EXITN                                                            
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
*        ADD A NEW BILL HEADER RECORD                                *          
*                                                                    *          
**********************************************************************          
         SPACE 1                                                                
HDR00    LA    R2,IOKEY            PROD LEDGER FOR NEXT DRAFT BILL              
         USING LDGRECD,R2                                                       
         MVC   LDGKEY,BCSPACES                                                  
         MVC   LDGKCPY,CUABIN      COMPANY                                      
         MVC   LDGKUNT(L'BCCPYPRD),BCCPYPRD PRODUCTION LEDGER                   
         GOTO1 AIO,IO2+IOACCMST+IORDUP                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
         LA    R3,LDGRFST                                                       
         SR    RE,RE                                                            
         USING SCIELD,R3                                                        
HDR20    CLI   SCIEL,0             END OF RECORD                                
         BE    HDR40                                                            
         CLI   SCIEL,SCIELQ                                                     
         BE    HDR50                                                            
HDR30    IC    RE,SCILN                                                         
         AR    R3,RE                                                            
         B     HDR20                                                            
*                                                                               
HDR40    LA    R3,BOELEM           ADD A DRAFT BILL NUMBER ELEMENT              
         XC    SCIEL(SCILN1Q),SCIEL                                             
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCITYPE,SCITDBNO                                                 
         MVI   SCILN,SCILN1Q                                                    
         ZAP   SCIAMNT,=P'0'                                                    
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO2,(R3),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   R3,15,16(R1)        R3=A(NEW ELEMENT)                            
         B     *+12                                                             
*                                                                               
HDR50    CLI   SCITYPE,SCITDBNO    DRAFT BILL NUMBER SEQ TYPE?                  
         BNE   HDR30                                                            
         AP    SCIAMNT,=P'1'       SET NEXT DRAFT BILL NUMBER                   
         UNPK  BCDUB,SCIAMNT                                                    
         MVC   FBILLNO,BCDUB+2                                                  
         OI    FBILLNO+L'FBILLNO-1,X'F0'                                        
*                                                                               
HDR60    LA    R2,IOKEY            READ FOR DRAFT BILL                          
         USING PBRRECD,R2                                                       
         XC    PBRPAS,PBRPAS                                                    
         MVI   PBRPTYP,PBRPTYPQ                                                 
         MVC   PBRPCPY,CUABIN                                                   
         MVI   PBRPSUB,PBRPPASQ    PASSIVE RECORD                               
         MVC   PBRPBLNO,FBILLNO    BILL NUMBER                                  
         MVI   PBRPIND,PBRPIDFT    DRAFT BILL                                   
         GOTO1 AIO,IO1+IOACCDIR+IOHIGH                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PBRPAS(PBRPUSER-PBRPAS),IOKEYSAV                                 
         BNE   HDR150                                                           
         CP    SCIAMNT,=P'999999'  RECORD EXISTS - TRY AGAIN                    
         BNE   HDR50                                                            
         ZAP   SCIAMNT,=P'0'       RESTART DRAFT SEQUENCE                       
         B     HDR50                                                            
*                                  WRITE BACK UPDATED LEDGER RECORD             
HDR150   GOTO1 AIO,IO2+IOACCMST+IOPUTREC                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
HDR160   LA    R2,IOKEY                                                         
         USING PBRRECD,R2                                                       
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ                                                 
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,BCJOBCOD    JOB CODE                                     
         GOTO1 AIO,IO1+IOACCDIR+IOHIGH+IORDEL  GET BILL RECORD                  
         LH    RE,=XL2'FFFF'                                                    
         CLC   PBRKEY(PBRKSEQ-PBRKEY),IOKEYSAV RECORD FOUND?                    
         BNE   HDR170                                                           
         SR    RE,RE                                                            
         ICM   RE,3,PBRKSEQ                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
HDR170   L     R2,AIO1                                                          
         MVC   PBRKEY,IOKEYSAV                                                  
         XC    PBRRSTA,PBRRSTA                                                  
         CLI   CSACT,ACTPRB        CALLED FROM PREVBILL                         
         BNE   *+8                                                              
         OI    PBRRSTA2,PBRSAUTR   SET AUTOREV BILL                             
         STCM  RE,3,PBRKSEQ                                                     
         MVC   FPBRKEY,PBRKEY      SAVE HEADER KEY                              
         LA    RE,PBRRFST                                                       
         MVI   0(RE),0                                                          
         LA    RE,1(RE)                                                         
         SR    RE,R2                                                            
         STCM  RE,3,PBRRLEN                                                     
         LA    R3,BOELEM                                                        
         USING BLHELD,R3                                                        
         XC    BOELEM,BOELEM                                                    
         MVI   BLHEL,BLHELQ                                                     
         MVI   BLHLN,BLHLNQ                                                     
         MVC   BLHJOB,BCJOBCOD                                                  
         MVC   BLHBLNO,FBILLNO                                                  
         MVC   BLHUSER,CUUSER                                                   
         MVC   BLHPERS,CUPASS                                                   
         MVC   BLHCRED,ASCDAT                                                   
         MVC   BLHFORM,CSFORMAT                                                 
         MVC   BLHLANG,CSFMLANG                                                 
         MVC   BLHCUR,CSBILCUR     SET CURRENCY CODE IN BILL                    
         CLC   CSBILCUR,CSCPYCUR   TEST AGENCY PRIMARY CURRENCY                 
         BE    HDR180                                                           
         MVC   BLHRVAL,CSEXCVAL    SET EXCHANGE RATE                            
*&&UK                                                                           
         CLC   BCCPYSEC,CSBILCUR   TEST AGENCY SECONDARY CURRENCY               
         BE    HDR180                                                           
*&&                                                                             
         OI    AFINDS,AFFORBIL     SET FOREIGN CURRENCY BILL                    
HDR180   L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         ZAP   BLHDSC,GODSCPCT-GOBBLOCK(L'GODSCPCT,RF)                          
         ZAP   BLHSCH,GOSRGPCT-GOBBLOCK(L'GOSRGPCT,RF)                          
         GOTO1 VDATCON,BCDMCB,(2,BCTODAYC),(0,BOWORK1)                          
         SR    RF,RF                                                            
         ICM   RF,1,P#RETDAY       GET NON-STANDARD RETENTION DAYS              
         BNZ   *+8                                                              
         LA    RF,7                DEFAULT IS 7                                 
         GOTO1 VADDAY,BCDMCB,BOWORK1,BOWORK1+6,(RF)                             
         GOTO1 VDATCON,BCDMCB,(0,BOWORK1+6),(2,BLHEXPD)                         
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO1,(R3),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,BOELEM                                                        
         USING NDXELD,R3                                                        
         XC    BOELEM,BOELEM                                                    
         MVI   NDXEL,NDXELQ                                                     
         MVI   NDXLN,NDXLNQ                                                     
         MVI   NDXHIGH,1                                                        
         MVI   NDXACTV,1                                                        
         MVI   NDXINDX,1                                                        
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO1,(R3),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IO1+IOACCMST+IOADDREC                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,IOKEY                                                         
         USING PBRRECD,R2                                                       
         XC    PBRPAS,PBRPAS                                                    
         XC    PBRKSTA,PBRKSTA                                                  
         MVI   PBRPTYP,PBRPTYPQ    RECORD TYPE                                  
         MVC   PBRPCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRPSUB,PBRPPASQ    PASSIVE RECORD                               
         MVC   PBRPBLNO,FBILLNO    BILL NUMBER                                  
         MVI   PBRPIND,PBRPIDFT    DRAFT BILL                                   
         MVC   PBRPUSER,CUUSER     USER-ID                                      
         MVC   PBRPJOB,BCJOBCOD    JOB                                          
         MVC   PBRPCRED,ASCDAT     CREATED DATE                                 
         MVC   PBRPFORM,CSFORMAT                                                
         MVC   PBRPPERS,CUPASS     PERSON CODE                                  
         CLI   CSACT,ACTPRB        CALLED FROM PREVBILL                         
         BNE   *+8                                                              
         OI    PBRKSTA2,PBRSAUTR   SET AUTOREV BILL                             
         MVC   PBRKDA,IODA         DISK ADDRESS                                 
         MVC   FHDRDA,IODA         SAVE HEADER DISK ADDRESS                     
         MVC   FPBRPAS,PBRPAS                                                   
         GOTO1 AIO,IO1+IOACCDIR+IOADDREC                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
**********************************************************************          
*        READ JOB TRANSACTIONS AND ADD PARAGRAPH/LINE RECORDS        *          
*                                                                    *          
**********************************************************************          
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
TXT010   MVI   FANYALLC,C'N'       SET NO ALLOCATED ITEMS                       
         MVI   FJOBOVER,C'N'                                                    
         ZAP   FPARNET,=P'0'                                                    
         ZAP   FPARCOM,=P'0'                                                    
         ZAP   FPAROALC,=P'0'                                                   
         CLI   CSACT,ACTINS        ADVANCE SCREEN PASSES LIST OF D/AS           
         BNE   *+14                                                             
         MVC   FSAVTNUM,TLNUM      SAVE CALLER'S TSAR RECORD NUMBER             
         B     TXT026                                                           
         L     RE,AIO5             RE=A(D/A BUFFER)                             
         LA    R4,L'TRNKDA(RE)     R4=A(FIRST D/A)                              
         ST    R4,0(RE)                                                         
         LA    R0,MAXADDRS         R0=MAX COUNT OF SAVED D/A S                  
         XC    0(L'TRNKDA,R4),0(R4)                                             
         LA    R2,IOKEY                                                         
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   TRNKACT,BCJOBCOD                                                 
         LA    R1,IOHI+IOACCDIR+IO1                                             
         B     *+8                                                              
TXT012   LA    R1,IOSQ+IOACCDIR+IO1                                             
         GOTO1 AIO,(R1)                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   TRNKCULA,IOKEYSAV+TRNKCULA-TRNKEY                                
         BNE   TXT024              FINISHED                                     
         CLC   TRNKWORK,=C'99'     TEST BILLING                                 
         BE    TXT024                                                           
         CLC   TRNKREF,BCSPACES                                                 
         BNH   TXT012              DO NOT SAVE BUCKETS                          
         TM    TRNKSTA2,TRNSBILP                                                
         BNO   TXT012                                                           
         TM    TRNKSTAT,TRNSDRFT   TEST DRAFT ITEM                              
         BNO   *+12                                                             
         CLI   TRNKSTYP,99         ONLY ADVANCES IF DRAFT ITEM                  
         BNE   TXT012                                                           
         MVI   FANYALLC,C'Y'       SET ALLOCATED ITEM FOUND                     
*                                                                               
TXT020   MVC   0(L'TRNKDA,R4),TRNKDA                                            
         LA    R4,L'TRNKDA(R4)                                                  
         XC    0(L'TRNKDA,R4),0(R4)                                             
         BCT   R0,TXT012                                                        
         B     TXT026              BUFFER FULL - PROCESS WHAT WE HAVE           
*                                                                               
TXT024   CLI   FANYALLC,C'Y'       TEST ANY ALLOCATED TRANSACTIONS              
         BE    TXT026                                                           
         MVC   FVMSGNO,=AL2(AE$NACOJ)                                           
         OI    CSINDSG1,CSINDUNW   SET UNWIND VIA $ABEND REQUIRED               
         B     EXITN                                                            
*                                                                               
TXT026   L     RE,AIO5             RE=A(D/A BUFFER)                             
         L     RF,0(RE)            RF=A(NEXT D/A)                               
         OC    0(L'TRNKDA,RF),0(RF)                                             
         BZ    TXT050              END OF RECORDS - ADD BILL                    
*                                                                               
         MVC   IODAOVER,0(RF)      READ TRANSACTION RECORD                      
         LA    RF,L'TRNKDA(RF)                                                  
         ST    RF,0(RE)            SAVE A(NEXT D/A)                             
         GOTO1 AIO,IOACCMST+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO1                                                          
         CLC   TRNKCULC,FCTRACC    TEST HAVE CONTRA ACCOUNT NAME                
         BE    TXT036                                                           
         MVC   FCTRACC,TRNKCULC                                                 
         MVC   FCTRNAM,BCSPACES                                                 
K        USING TRNRECD,IOKEY                                                    
         MVC   K.TRNKEY,BCSPACES                                                
         MVC   K.TRNKCULA,FCTRACC                                               
         GOTO1 AIO,IOREAD+IOACCDIR+IO2                                          
         BNE   TXT036                                                           
         GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BNE   TXT036                                                           
         L     R1,AIO2                                                          
         LA    R1,TRNRFST-TRNRECD(R1)                                           
         USING NAMELD,R1                                                        
         XR    RF,RF                                                            
TXT028   CLI   NAMEL,0                                                          
         BE    TXT036                                                           
         IC    RF,NAMLN                                                         
         CLI   NAMEL,NAMELQ                                                     
         BE    *+8                                                              
         BXH   R1,RF,TXT028                                                     
         AHI   RF,-(NAMLN1Q+1)                                                  
         BM    TXT036                                                           
         EX    RF,*+4                                                           
         MVC   FCTRNAM(0),NAMEREC                                               
         DROP  R1,K                                                             
*                                                                               
TXT036   CLI   TRNRFST,TRNELQ                                                   
         BNE   TXT026              MUST BE A STRANGE RECORD                     
*                                                                               
         USING PTAELD,RE                                                        
         LA    RE,TRNRFST          TEST IF THIS ALLOCATION REQUIRED             
         SR    R0,R0                                                            
TXT040   IC    R0,PTALN                                                         
         AR    RE,R0                                                            
         CLI   PTAEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   PTAEL,PTAELQ                                                     
         BNE   TXT040                                                           
         CLI   PTATYPE,PTATRAL     TEST ALLOCATION                              
         BNE   TXT040                                                           
         TM    PTASTAT1,PTASPEND                                                
         BNO   TXT040                                                           
*                                  ALWAYS TAKE ALL ALLOCATION                   
*        LA    RF,X'70'            ASSUME VANILLA BILL - SET BO                 
*        CLI   CSACT,ACTPRB        TEST FORMATTING AUTOREV BILL                 
*        BNE   *+8                                                              
*        LA    RF,X'80'            FORMATTING AUTOREV BILL - SET BZ             
*        TM    PTASTAT2,PTASAUTR   TEST THIS IS AUTOREV ALLOCATION              
*        EX    RF,*+4                                                           
*        NOP   TXT026              NOT WANTED - GET NEXT                        
*                                                                               
TXT044   GOTO1 RDWCODE             ENSURE WORKCODE IN IO4                       
         SR    R0,R0                                                            
         TM    AFINDS,AFFORBIL     TEST FOREIGN CURRENCY BILL                   
         BNO   *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 APRORATA,BODMCB,AIO1,AGOPBLK,ACOM,(R0),PRORATAD                  
         CLI   FPREVB,C'Y'         TEST IF PREVIOUS BILLING IS SHOWN            
         BNE   TXT046                                                           
         AP    PP$AALLO,PA$NETBL   ADD PRIOR BILLING TO PENDING AMTS            
         AP    PP$ACOMM,PA$COMBL                                                
         AP    PP$FALLO,PB$NETBL                                                
         AP    PP$FCOMM,PB$COMBL                                                
         AP    PP$HRSB,PA$HRSB                                                  
*        AP    PP$WUHRS,PA$WUHRS                                                
*        AP    PP$WUAMT,PA$WUAMT                                                
*                                                                               
         AP    PREAALLO,PA$NETBL   SAVE PRIOR BILLING TOTALS                    
         AP    PREACOMM,PA$COMBL                                                
         AP    PREFALLO,PB$NETBL                                                
         AP    PREFCOMM,PB$COMBL                                                
         AP    PREHRSB,PA$HRSB                                                  
*        AP    PREWUHRS,PA$WUHRS                                                
*        AP    PREWUAMT,PA$WUAMT                                                
*                                                                               
TXT046   LA    RE,PP$AALLO                                                      
         TM    AFINDS,AFFORBIL     TEST FOREIGN CURRENCY BILL                   
         BNO   *+8                                                              
         LA    RE,PP$FALLO                                                      
         CP    0(L'PP$AALLO,RE),=P'0'                                           
         BNE   TXT048                                                           
         CP    L'PP$AALLO(L'PP$AALLO,RE),=P'0'                                  
         BE    TXT026              NOTHING PENDING - GET NEXT                   
*                                                                               
TXT048   ZAP   FTRNNET,0(L'PP$AALLO,RE)                                         
         ZAP   FTRNCOM,L'PP$AALLO(L'PP$AALLO,RE)                                
         AP    FTRNCOM,PP$WUAMT    ANY WRITE-UP IS COMMISSION                   
         ZAP   FTRNOALC,BCPZERO                                                 
         TM    PG$STAT4,PG$OALLC   TEST COMM IS OVERALLOC NET                   
         BNO   *+10                NO                                           
         ZAP   FTRNOALC,L'PP$AALLO(L'PP$AALLO,RE)                               
         AP    FTRNOALC,PP$WUAMT   PRINT WRITE-UP AS OVERALLOC NET              
*                                                                               
         GOTO1 BLDKEY              BUILD TSAR RECORD KEY                        
         GOTO1 SETFWT              SET FWT ELEMENTS ON TSAR RECORD              
*        GOTO1 FMTFWT,BODMCB,TLDAFWT,TLDALINE                                   
         GOTO1 FMTDATA             FORMAT NON-NUMERIC DATA TO PRINT             
         BE    TXT026                                                           
         OI    CSINDSG1,CSINDUNW   SET UNWIND VIA $ABEND REQUIRED               
         B     EXITN                                                            
*                                                                               
TXT050   GOTO1 BLDBILL             ADD BILL LINES AND PARAGRAPHS                
         BNE   EXITN                                                            
         CLI   FHIPARA,1           TEST IF ONLY ONE PARAGRAPH                   
         BNE   TXT052              NO - INDEX NEEDED                            
         CLI   CSACT,ACTPRB        TEST AUTOREV BILL                            
         BNE   TXT060              NO - BILL HEADER IS OK                       
TXT052   MVC   IODAOVER,FHDRDA     READ BACK BILL HEADER RECORD                 
         GOTO1 AIO,IOGETRUP+IOACCMST+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   FHIPARA,1           TEST IF ONLY ONE PARAGRAPH                   
         BE    TXT054              YES - INDEX IS OK                            
         GOTO1 VHELLO,BODMCB,(C'D',ACCMST),('NDXELQ',AIO2),0,0                  
         CLI   BODMCB+12,0                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ADDNDXEL,FHIPARA                                                 
*                                                                               
TXT054   CLI   CSACT,ACTPRB        TEST AUTOREV BILL                            
         BNE   TXT056              NO                                           
         PUSH  USING                                                            
         USING FFTELD,BOELEM                                                    
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,(FFTLN1Q+L'CSBILNUM+1)                                     
         MVI   FFTTYPE,FFTTAUTR                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,L'CSBILNUM                                               
         MVC   FFTDATA(L'CSBILNUM),CSBILNUM                                     
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO2,FFTEL,0                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         POP   USING                                                            
*                                                                               
TXT056   GOTO1 AIO,IOACCMST+IOPUTREC+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TXT060   CLI   CSACT,ACTPRB        TEST CALLED FROM PREVBILL                    
         BNE   TXT070                                                           
         MVC   IODAOVER,BCJOBDA    READ BACK JOB RECORD                         
         GOTO1 AIO,IOGETRUP+IOACCMST+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         PUSH  USING                                                            
         USING FFTELD,BOELEM                                                    
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,(FFTLN1Q+L'CSBILNUM+1)                                     
         MVI   FFTTYPE,FFTTAUTR                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,L'CSBILNUM                                               
         MVC   FFTDATA(L'CSBILNUM),CSBILNUM                                     
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO2,FFTEL,0                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         POP   USING                                                            
*                                                                               
         GOTO1 AIO,IOACCMST+IOPUTREC+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
TXT070   MVC   CSBILNUM,FBILLNO                                                 
         MVC   TLNUM,FSAVTNUM      RESTORE CALLERS                              
         OC    TLNUM,TLNUM                                                      
         BZ    EXITY                                                            
         GOTO1 ATSARIO,TSAGET                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXITY                                                            
*                                                                               
EXITN    MVI   FVOMTYP,GTMERR                                                   
         LTR   RB,RB                                                            
         B     EXIT                                                             
EXITY    MVI   FVOMTYP,GTMINF                                                   
         CR    RB,RB                                                            
EXIT     XIT1  ,                                                                
*                                                                               
SUBEDIT  DC    AL1(RECBIL,ACTEDT)                                               
         DC    AL1(1,0,0,0)                                                     
         EJECT                                                                  
**********************************************************************          
*        READ WORKCODE RECORD                                        *          
**********************************************************************          
         SPACE 1                                                                
         USING WCORECD,R4                                                       
RDWCODE  NTR1                                                                   
         L     R2,AIO1                                                          
         L     R4,AIO4                                                          
         CLC   WCOKWRK,TRNKWORK                                                 
         BE    RDWX                WORKCODE RECORD ALREADY THERE                
         MVI   FTRNWGRP,0                                                       
         LA    R4,IOKEY                                                         
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUABIN                                                   
         MVC   WCOKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   WCOKWRK,TRNKWORK                                                 
         LH    R1,=Y(IO4)                                                       
         GOTO1 AIO,IORD+IOACCDIR(R1)                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         LH    R1,=Y(IO4)                                                       
         GOTO1 AIO,IOACCMST+IOGET(R1)                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   FTRNWTYP,BLFTCSTQ   SET COST                                     
         L     RE,AIO4                                                          
         TM    (WCORFST-WCORECD)+(WCOSTAT-WCOELD)(RE),WCOSHCOE                  
         BNO   *+8                                                              
         MVI   FTRNWTYP,BLFTTIMQ   SET TIME                                     
         MVC   FTRNWGRP,(WCORFST-WCORECD)+(WCOGRP-WCOELD)(RE)                   
         LA    RF,FWCNTAB                                                       
         LA    R0,FWCNMAX                                                       
         OC    0(L'TRNKWORK,RF),0(RF)                                           
         BZ    RDWC10                                                           
         LA    RF,L'TRNKWORK+L'NAMEREC(RF)                                      
         BCT   R0,*-14                                                          
         SH    RF,=Y(L'TRNKWORK+L'NAMEREC)                                      
RDWC10   BAS   RE,GETWCN           R1 RETURNS WITH LENGTH AND NAME              
         BNE   RDWX                                                             
         LR    RE,R1                                                            
         SRA   RE,32-8                                                          
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   L'TRNKWORK(0,RF),0(R1)                                           
         MVC   0(L'TRNKWORK,RF),TRNKWORK                                        
*                                                                               
RDWX     B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*        SET FWT ELEMENTS IN TSAR RECORD                             *          
**********************************************************************          
         SPACE 1                                                                
SETFWT   NTR1                                                                   
         LA    RF,PP$AALLO                                                      
         TM    AFINDS,AFFORBIL     TEST FOREIGN CURRENCY BILL                   
         BNO   *+8                                                              
         LA    RF,PP$FALLO                                                      
         AP    TLDANET,0(L'PP$AALLO,RF)                                         
         AP    TLDACOM,PP$ACOMM-PP$AALLO(L'PP$ACOMM,RF)                         
         ZAP   TLDAOAC,BCPZERO                                                  
         LA    RF,PP$ADSCB                                                      
         TM    AFINDS,AFFORBIL     TEST FOREIGN CURRENCY BILL                   
         BNO   *+8                                                              
         LA    RF,PP$FDSCB                                                      
         ZAP   TLDACDSC,0(L'PP$ADSCB,RF)                                        
*                                                                               
         L     R4,AMSTREC          CONTROL RECORD                               
         LA    R4,PBCRFST-PBCRECD(R4)                                           
         MVI   BCBYTE2,0           CLEAR NET/COMM INDICATOR                     
         USING BLFELD,R4                                                        
SFWT10   CLI   BLFEL,0             TEST E-O-R                                   
         BE    SFWT60                                                           
         CLI   BLFEL,BLFELQ                                                     
         BNE   SFWT20                                                           
         CLC   BLFTYPE,TLKATYPE    MATCH ON WORKCODE TYPE                       
         BE    SFWT30                                                           
SFWT20   SR    R0,R0                                                            
         IC    R0,BLFLN                                                         
         AR    R4,R0                                                            
         B     SFWT10                                                           
SFWT30   LA    RE,FWTTAB           SEE IF THIS BLFEL HAS AN FWT ENTRY           
         B     *+8                                                              
SFWT30A  LA    RE,6(RE)                                                         
         CLI   0(RE),0                                                          
         BE    SFWT20                                                           
         CLC   BLFFLD,0(RE)                                                     
         BNE   SFWT30A                                                          
         CLI   TLKATYPE,FF         TEST IF THIS IS PREVIOUS BILLING             
         BNE   *+12                                                             
         TM    1(RE),X'80'         TEST THIS BLFFLD IS REQUIRED                 
         BZ    SFWT20                                                           
         ICM   RF,15,2(RE)                                                      
         A     RF,BORELO                                                        
         BASR  RE,RF               GET AMOUNT IN BODUB1                         
*                                                                               
         SR    R0,R0                                                            
         LA    R3,TLDAFWT          FIND FWTEL SLOT IN TSAR RECORD               
         USING FWTELD,R3                                                        
SFWT40   CLI   FWTEL,0                                                          
         BE    SFWT50                                                           
         CLC   FWTFLD,BLFFLD                                                    
         BNE   SFWT42                                                           
         CLI   FWTFLD,BLFFRATQ     DO NOT ADD INTO RATE                         
         BE    SFWT20                                                           
         CLI   FWTFLD,BLFFCMRQ     DO NOT ADD INTO COMM RATE                    
         BE    SFWT20                                                           
         AP    FWTAMT,BODUB1       ADD INTO EXISTING ELEMENT                    
         B     SFWT20                                                           
SFWT42   IC    R0,FWTLN                                                         
         AR    R3,R0                                                            
         B     SFWT40                                                           
*                                                                               
SFWT50   XC    FWTEL(FWTLNQ),FWTEL ADD NEW ELEMENT                              
         MVI   FWTEL+FWTLNQ,0                                                   
         MVI   FWTEL,FWTELQ                                                     
         MVI   FWTLN,FWTLNQ                                                     
         MVC   FWTFLD,BLFFLD                                                    
         MVC   FWTCOLF,BLFCOLF                                                  
         MVC   FWTCOLN,BLFCOLN                                                  
         MVC   FWTLINE,BLFLINF                                                  
         CLI   BLFFLD,BLFFGRS2                                                  
         BE    *+12                                                             
         CLI   BLFFLD,BLFFNET2                                                  
         BNE   *+8                                                              
         OI    FWTCOLN,X'80'       SET 'TOTAL-ONLY' FLAG                        
         ZAP   FWTAMT,BODUB1                                                    
         B     SFWT20                                                           
*                                                                               
SFWT60   LA    R3,TLDAFWT          CHECK NET AND COMM FWTELS                    
         CLI   BCBYTE2,X'11'       TEST NET/COMM BOTH UPDATED                   
         BE    SFWTX                                                            
         SR    R0,R0                                                            
SFWT62   CLI   FWTEL,0                                                          
         BE    SFWT70                                                           
         CLI   FWTFLD,BLFFNETQ                                                  
         BNE   SFWT64                                                           
         TM    BCBYTE2,X'10'       TEST NET ALREADY UPDATED                     
         BO    SFWT68                                                           
         BAS   RE,SFWTNET          GET NET                                      
         AP    FWTAMT,BODUB1                                                    
         OI    BCBYTE2,X'10'       SET NET UPDATED                              
         B     SFWT68                                                           
SFWT64   CLI   FWTFLD,BLFFCOMQ                                                  
         BNE   SFWT68                                                           
         TM    BCBYTE2,X'01'       TEST COMM ALREADY UPDATED                    
         BO    SFWT68                                                           
         BAS   RE,SFWTCOM          GET COMM                                     
         AP    FWTAMT,BODUB1                                                    
         OI    BCBYTE2,X'01'       SET COMM UPDATED                             
         B     SFWT68                                                           
SFWT68   IC    R0,FWTLN                                                         
         AR    R3,R0                                                            
         B     SFWT62                                                           
*                                                                               
SFWT70   CLI   BCBYTE2,X'11'       TEST BOTH NET AND COMMISSION THERE           
         BE    SFWTX                                                            
         XC    FWTEL(FWTLNQ),FWTEL ADD NEW ELEMENT                              
         MVI   FWTEL,FWTELQ                                                     
         MVI   FWTLN,FWTLNQ                                                     
         MVI   FWTFLD,BLFFNETQ                                                  
         MVI   FWTCOLF,0                                                        
         MVI   FWTCOLN,0                                                        
         TM    BCBYTE2,X'10'       TEST IF NET FWTEL ALREADY DONE               
         BO    SFWT72                                                           
         BAS   RE,SFWTNET          GET NET AMOUNT                               
         ZAP   FWTAMT,BODUB1                                                    
         TM    BCBYTE2,X'01'       TEST IF COMM FWTEL ALREADY DONE              
         BO    SFWTX                                                            
         MVC   FWTLNQ(FWTLNQ,R3),0(R3)                                          
         LA    R3,FWTLNQ(R3)                                                    
*                                                                               
SFWT72   MVI   FWTFLD,BLFFCOMQ                                                  
         BAS   RE,SFWTCOM                                                       
         ZAP   FWTAMT,BODUB1                                                    
         B     SFWTX                                                            
*                                                                               
SFWTNET  ZAP   BODUB1,PP$AALLO     SET AGENCY CURRENCY NET PENDING              
         OI    BCBYTE2,X'10'                                                    
         TM    AFINDS,AFFORBIL     TEST FOREIGN CURRENCY BILL                   
         BNOR  RE                                                               
         ZAP   BODUB1,PP$FALLO     SET FOREIGN CURRENCY NET PENDING             
         BR    RE                                                               
*                                                                               
SFWTCOM  ZAP   BODUB1,PP$ACOMM     SET AGENCY CURRENCY COMM PENDING             
         OI    BCBYTE2,X'01'                                                    
         TM    AFINDS,AFFORBIL     TEST FOREIGN CURRENCY BILL                   
         BNOR  RE                                                               
         ZAP   BODUB1,PP$FCOMM     SET FOREIGN CURRENCY COMM PENDING            
         BR    RE                                                               
*                                                                               
SFWTGRS  ZAP   BODUB1,PP$AALLO     SET AGENCY CURRENCY GROSS PENDING            
         AP    BODUB1,PP$ACOMM                                                  
         TM    AFINDS,AFFORBIL     TEST FOREIGN CURRENCY BILL                   
         BNOR  RE                                                               
         ZAP   BODUB1,PP$FALLO     SET FOREIGN CURRENCY GROSS PENDING           
         AP    BODUB1,PP$FCOMM                                                  
         BR    RE                                                               
*                                                                               
SFWTHRS  ZAP   BODUB1,PP$HRSB      REGULAR HOURS PENDING BILL                   
         AP    BODUB1,PP$WUHRS     PLUS ANY WRITE-UP HOURS PENDING              
         BR    RE                                                               
*                                                                               
SFWTRAT  ZAP   BODUB1,PA$HOURS                                                  
         BZR   RE                                                               
         ZAP   BODUB1(16),PA$NET   TRANSACTION AMOUNT                           
         SRP   BODUB1(16),4,0                                                   
         DP    BODUB1(16),PA$HOURS TOTAL POSTED HOURS                           
         SRP   BODUB1,64-2,5                                                    
         BR    RE                                                               
*                                                                               
SFWTCMR  ZAP   BODUB1,PP$ACOMM                                                  
         BZR   RE                                                               
         L     RF,AGOPBLK                                                       
         ZAP   BODUB1,GOAGYCOM-GOBLOCK(L'GOAGYCOM,RF)                           
         SRP   BODUB1,64-2,5                                                    
         BR    RE                                                               
*                                  THIS CODE NOT EXECUTED FOR NOW               
         ZAP   BODUB1(16),PP$ACOMM ALLOCATED COMMISSION                         
         SRP   BODUB1(16),6,0                                                   
         DP    BODUB1(16),PP$AALLO ALLOCATED NET                                
         SRP   BODUB1,64-2,5       PERCENTAGE TO 2 D.P.                         
         BR    RE                                                               
*                                                                               
SFWTX    B     EXIT                                                             
*                                                                               
FWTTAB   DC    AL1(BLFFNETQ),X'80',AL4(SFWTNET)                                 
         DC    AL1(BLFFNET2),X'80',AL4(SFWTNET)                                 
         DC    AL1(BLFFCOMQ),X'80',AL4(SFWTCOM)                                 
         DC    AL1(BLFFGRSQ),X'80',AL4(SFWTGRS)                                 
         DC    AL1(BLFFGRS2),X'80',AL4(SFWTGRS)                                 
         DC    AL1(BLFFHRSQ),X'00',AL4(SFWTHRS)                                 
         DC    AL1(BLFFRATQ),X'00',AL4(SFWTRAT)                                 
         DC    AL1(BLFFCMRQ),X'00',AL4(SFWTCMR)                                 
         DC    AL1(0)                                                           
         DS    0H                                                               
         EJECT                                                                  
**********************************************************************          
*        READ TSAR RECORDS BACK                                      *          
*        ADD LINE RECORDS TO ACCOUNT FILE                            *          
**********************************************************************          
         SPACE 1                                                                
BLDBILL  NTR1                                                                   
         L     R2,AIO2             USE IO2 FOR ADDING RECORDS                   
         USING PBRRECD,R2                                                       
         MVC   PBRKEY,FPBRKEY      SAVED HEADER KEY                             
         XC    PBRRSTA,PBRRSTA                                                  
         MVI   PBRKPARA,1          SET FIRST PARAGRAPH                          
         XC    FLASTPAR,FLASTPAR                                                
         XC    FLASTSUB,FLASTSUB                                                
*                                                                               
         XC    TLKEY,TLKEY                                                      
         MVI   TLKSES,TLKALINQ                                                  
         LA    R1,TSARDH                                                        
         B     *+8                                                              
BBIL10   LA    R1,TSANXT                                                        
         MVC   PBRRLEN,=AL2(PBRRFST+1-PBRRECD)                                  
         MVI   PBRRFST,0                                                        
         GOTO1 ATSARIO,(R1)                                                     
         BL    BBIL12              E-O-F                                        
         CLI   TLKSES,TLKALINQ                                                  
         BNE   BBIL12              END OF TSAR LINE RECORDS                     
         OC    FLASTPAR,FLASTPAR   TEST FIRST TIME                              
         BNZ   *+16                                                             
         MVC   FLASTPAR,TLKAPARA   SET CURRENT PARAGRAPH KEY                    
         MVC   FTRNHTYP,TLKATYPE   SET PARAGRAPH/HEADING TYPE                   
         CLC   FLASTPAR,TLKAPARA   TEST IF PARAGRAPH KEY CHANGE                 
         BE    BBIL24                                                           
         B     *+8                                                              
BBIL12   MVI   FJOBOVER,C'Y'                                                    
         CLC   FFWTSUBT,FFWTPART   TEST IF SUB-TOTAL LINE PENDING               
         BE    BBIL14                                                           
         CLC   CUAALF,=C'XY'       OH BLIMEY! - HARD CODE                       
         BE    *+14                                                             
         CLC   CUAALF,=C'DS'       OH BLIMEY! - HARD CODE                       
         BNE   BBIL14                                                           
         GOTO1 ADDFWTEL,FFWTSUBT                                                
         GOTO1 VHELLO,BODMCB,(C'G',ACCMST),('FWTELQ',AIO2),0,0                  
         CLI   12(R1),0                                                         
         BNE   BBIL12A                                                          
         L     RF,BODMCB+12                                                     
         MVC   BOWORK1(REPWREGQ),BCSPACES                                       
         GOTO1 FMTFWT,BODMCB,(RF),BOWORK1                                       
BBIL12A  GOTO1 ADDFFTEL,BOWORK1                                                 
         BNE   BBILN                                                            
         XC    FFWTSUBT,FFWTSUBT   CLEAR SUB-TOTAL FWT ELEMENTS                 
         MVI   PBRRLSTA,PBRLSUB                                                 
         GOTO1 AIO,IO2+IOACCMST+IOADDREC                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   PBRRFST,0                                                        
         MVC   PBRRLEN,=AL2(PBRRFST+1-PBRRECD)                                  
BBIL14   CLI   FFWTPART,0          TEST PARAGRAPH TOTAL REQUIRED                
         BE    BBIL16              NO - JUST ADD PARAGRAPH HEADER               
         GOTO1 ADDFWTEL,FFWTPART                                                
         MVC   BOWORK1(REPWREGQ),BCSPACES                                       
         SR    RF,RF               GET SECTION SUBTOTAL NAME                    
         IC    RF,FTRNHTYP         SECTION NUMBER                               
         BCTR  RF,0                                                             
         MH    RF,=Y(L'SNAMTAB)                                                 
         LH    RE,=Y(SNAMTAB-FWORKD)                                            
         AR    RE,RC                                                            
         AR    RF,RE                                                            
         LA    RF,L'FBPBST(RF)                                                  
         MVC   BOWORK1(L'FBPBST),0(RF)                                          
BBIL14A  GOTO1 VHELLO,BODMCB,(C'G',ACCMST),('FWTELQ',AIO2),0,0                  
         CLI   12(R1),0                                                         
         BNE   BBIL15                                                           
         L     RF,BODMCB+12                                                     
         GOTO1 FMTFWT,BODMCB,(RF),BOWORK1                                       
BBIL15   CLI   BOWORK1,C'*'                                                     
         BE    BBIL15A                                                          
         GOTO1 ADDFFTEL,BOWORK1                                                 
         BNE   BBILN                                                            
BBIL15A  XC    FFWTPART,FFWTPART   CLEAR PARAGRAPH TOTAL FWT ELEMS              
         XC    FFWTSUBT,FFWTSUBT   CLEAR SUB-TOTAL FWT ELEMENTS                 
         CLI   BOWORK1,C'*'                                                     
         BE    BBIL15B                                                          
         MVI   PBRRLSTA,PBRLTOT                                                 
         GOTO1 AIO,IO2+IOACCMST+IOADDREC                                        
         BE    *+6                                                              
         DC    H'0'                                                             
BBIL15B  MVI   PBRRFST,0                                                        
         MVC   PBRRLEN,=AL2(PBRRFST+1-PBRRECD)                                  
BBIL16   GOTO1 ADDPGHEL            ADD PARAGRAPH HEADER ELEMENT                 
         MVC   BOWORK1(REPWREGQ),BCSPACES                                       
         SR    RF,RF               GET SECTION HEADING NAME                     
         IC    RF,FTRNHTYP         SECTION NUMBER                               
         BCTR  RF,0                                                             
         MH    RF,=Y(L'SNAMTAB)                                                 
         LH    RE,=Y(SNAMTAB-FWORKD)                                            
         AR    RE,RC                                                            
         AR    RF,RE                                                            
         MVC   BOWORK1(L'FBPBST),0(RF)                                          
BBIL22   GOTO1 ADDFFTEL,BOWORK1                                                 
         BNE   BBILN                                                            
         SR    RE,RE                                                            
         IC    RE,PBRKLINE                                                      
         BCTR  RE,0                                                             
         STC   RE,PBRKLINE         DECREMENT LINE NUMBER                        
         L     RF,BODMCB+16        SET CORRECT FFTEL TYPE                       
         MVI   FFTTYPE-FFTELD(RF),FFTTPGHC                                      
         GOTO1 ADDNDXEL,PBRKLINE   ADD LINE INDEX ELEMENT                       
         MVI   PBRKLINE,0          RESET LINE NUMBER                            
         GOTO1 AIO,IO2+IOACCMST+IOADDREC                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   PBRRFST,0                                                        
         MVC   PBRRLEN,=AL2(PBRRFST+1-PBRRECD)                                  
         MVC   FHIPARA,PBRKPARA                                                 
         SR    RE,RE               INCREMENT PARAGRAPH NUMBER                   
         IC    RE,PBRKPARA                                                      
         LA    RE,1(RE)                                                         
         STC   RE,PBRKPARA                                                      
         CLI   PBRKPARA,200                                                     
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TOOMP)                                           
         B     BBILN                                                            
         CLI   FJOBOVER,C'Y'                                                    
         BE    BBILY                                                            
         MVC   FLASTPAR,TLKAPARA   SAVE CURRENT PARAGRAPH TSAR KEY              
         MVC   FTRNHTYP,TLKATYPE   SET PARAGRAPH/HEADING TYPE                   
*                                                                               
BBIL24   CLC   CUAALF,=C'XY'       OH BLIMEY! - HARD CODE                       
         BE    *+14                                                             
         CLC   CUAALF,=C'DS'       OH BLIMEY! - HARD CODE                       
         BNE   BBIL30                                                           
         SR    R1,R1                                                            
         IC    R1,TLKATYPE                                                      
         BCTR  R1,0                                                             
         MH    R1,=Y(L'FTSAKEYS)   INDEX TO THIS SECTION KEYS                   
         LA    R1,FTSAKEYS(R1)     R1=A(SECTION FIRST KEY)                      
         LA    RE,1                                                             
         CLI   0(R1),0             TEST ANY KEY ITEM AT ALL                     
         BE    BBIL24B                                                          
         LA    RF,KEYITEMS         FIND KEY TABLE ENTRY                         
         LA    R0,MAXKEYIQ                                                      
         CLC   0(1,R1),0(RF)                                                    
         BE    *+14                                                             
         LA    RF,L'KEYITEMS(RF)                                                
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
         SR    RE,RE               RF=A(KEY ITEM TABLE ENTRY)                   
         IC    RE,KEYILEN-KEYITEMD(RF)                                          
BBIL24B  BCTR  RE,0                                                             
         OC    FLASTSUB,FLASTSUB   TEST FIRST ONE                               
         BNZ   BBIL25                                                           
         EX    RE,*+4                                                           
         MVC   FLASTSUB(0),TLKAREST                                             
         B     BBIL28              ADD FIRST SUB-TOTAL                          
*                                                                               
BBIL25   EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FLASTSUB(0),TLKAREST                                             
         EX    RE,*+4                                                           
         MVC   FLASTSUB(0),TLKAREST                                             
         BE    BBIL28              SAME SUBTOTAL - ADD THIS ITEM                
*                                                                               
         GOTO1 ADDFWTEL,FFWTSUBT                                                
         GOTO1 VHELLO,BODMCB,(C'G',ACCMST),('FWTELQ',AIO2),0,0                  
         CLI   12(R1),0                                                         
         BNE   BBIL27                                                           
         L     RF,BODMCB+12                                                     
         MVC   BOWORK1(REPWREGQ),BCSPACES                                       
         GOTO1 FMTFWT,BODMCB,(RF),BOWORK1                                       
BBIL27   GOTO1 ADDFFTEL,BOWORK1                                                 
         BNE   BBILN                                                            
         XC    FFWTSUBT,FFWTSUBT   CLEAR SUB-TOTAL FWT ELEMENTS                 
         MVI   PBRRLSTA,PBRLSUB                                                 
         GOTO1 AIO,IO2+IOACCMST+IOADDREC                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   PBRRFST,0                                                        
         MVC   PBRRLEN,=AL2(PBRRFST+1-PBRRECD)                                  
*                                                                               
BBIL28   GOTO1 SUBFWT,TLDAFWT                                                   
*                                                                               
BBIL30   GOTO1 ADDFFTEL,TLDALINE                                                
         BNE   BBILN                                                            
         CLI   TLDAFWT,0                                                        
         BE    BBIL40                                                           
         GOTO1 TOTFWT              ADD AMOUNTS INTO TOTALS                      
         GOTO1 ADDFWTEL,TLDAFWT                                                 
         MVI   PBRRLSTA,PBRLNUM    SET 'NUMBERS IN TEXT' STATUS                 
*                                                                               
BBIL40   GOTO1 AIO,IO2+IOACCMST+IOADDREC                                        
         BE    BBIL10                                                           
         DC    H'0'                                                             
*                                                                               
BBILY    B     EXITY                                                            
*                                                                               
BBILN    DS    0H                  ERROR - DELETE BILL HDR AND PASSIVE          
K        USING PBRRECD,IOKEY       (BETTER THAN UNWINDING 200 ADDRECS)          
         MVC   K.PBRKEY,FPBRKEY                                                 
         GOTO1 AIO,IORDUP+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO1                                                          
         OI    PBRRSTAT-PBRRECD(RF),PBRSDELT                                    
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    K.PBRKSTAT,PBRSDELT                                              
         GOTO1 AIO,IOWRITE+IOACCDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   K.PBRPAS,FPBRPAS                                                 
         GOTO1 AIO,IORDUP+IOACCDIR                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    K.PBRKSTAT,PBRSDELT                                              
         GOTO1 AIO,IOWRITE+IOACCDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXITN                                                            
         DROP  K                                                                
*                                                                               
ADDFFTEL NTR1                      ADD FFTEL TO LINE RECORD R1=A(DATA)          
         IC    RE,PBRKLINE                                                      
         LA    RE,1(RE)                                                         
         STC   RE,PBRKLINE         INCREMENT LINE NUMBER                        
         CLI   PBRKLINE,200                                                     
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$MPARA)                                           
         B     EXITN                                                            
         PUSH  USING                                                            
         USING FFTELD,BOELEM                                                    
         XC    FFTEL(FFTLN1Q),FFTEL                                             
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,REPWREGQ+(FFTDATA-FFTEL)                                   
         MVI   FFTTYPE,FFTTBLIN                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,REPWREGQ                                                 
         MVC   FFTDATA(REPWREGQ),0(R1)                                          
         GOTO1 VHELLO,BODMCB,(C'P',ACCMST),AIO2,FFTEL,0                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         POP   USING                                                            
         B     EXITY                                                            
*                                                                               
ADDFWTEL NTR1                      ADD FWT ELEMS TO LINE R1=A(FWTELS)           
         LA    R0,5                MAXIMUM NUMBER OF FWT ELEMENTS               
         LR    R3,R1                                                            
         USING FWTELD,BOELEM                                                    
AFWT20   CLI   0(R3),0                                                          
         BE    AFWTX                                                            
         TM    FWTCOLN,X'80'       TEST THIS REQUIRED ON LINE RECORD            
         BO    AFWT40                                                           
         MVC   FWTEL(FWTLNQ),0(R3)                                              
         GOTO1 VHELLO,BODMCB,(C'P',ACCMST),AIO2,FWTEL,0                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
AFWT40   LA    R3,FWTLNQ(R3)                                                    
         BCT   R0,AFWT20                                                        
AFWTX    B     EXIT                                                             
*                                                                               
ADDNDXEL NTR1                      ADD INDEX - R1=A(COUNT)                      
         USING NDXELD,BOELEM                                                    
         XC    NDXEL(NDXLNQ),NDXEL                                              
         MVI   NDXEL,NDXELQ                                                     
         MVI   NDXLN,NDXLNQ                                                     
         MVC   NDXHIGH,0(R1)                                                    
         MVC   NDXACTV,0(R1)                                                    
         SR    RF,RF                                                            
         ICM   RF,1,0(R1)                                                       
         BNZ   *+8                                                              
         LA    RF,1                PRECAUTION                                   
         LA    RE,NDXINDX(RF)                                                   
         BCTR  RE,0                                                             
         STC   RF,0(RE)                                                         
         BCT   RF,*-6                                                           
         GOTO1 VHELLO,BODMCB,(C'P',ACCMST),AIO2,NDXEL,0                         
         CLI   12(R1),0                                                         
         BE    ANDXX                                                            
         DC    H'0'                                                             
ANDXX    B     EXIT                                                             
*                                                                               
ADDPGHEL NTR1                      ADD PARAGRAPH HEADER ELEMENT                 
         USING PGHELD,BOELEM                                                    
         XC    PGHEL(PGHLNQ),PGHEL                                              
         MVI   PGHEL,PGHELQ                                                     
         MVI   PGHLN,PGHLNQ                                                     
         ZAP   PGHNET,FPARNET                                                   
         ZAP   PGHCOM,FPARCOM                                                   
         ZAP   PGHOALLC,FPAROALC                                                
         ZAP   FPARNET,BCPZERO                                                  
         ZAP   FPARCOM,BCPZERO                                                  
         ZAP   FPAROALC,BCPZERO                                                 
         MVI   PGHTAX,0                                                         
         MVC   PGHHTYP,FLASTPAR    SET PARAGRAPH TYPE                           
         MVI   PGHHTYP2,0                                                       
         GOTO1 VHELLO,BODMCB,(C'P',ACCMST),AIO2,PGHEL,0                         
         CLI   12(R1),0                                                         
         BE    APGHX                                                            
         DC    H'0'                                                             
APGHX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*        ADD FWT AMOUNTS INTO PARAGRAPH SUB-TOTAL AND TOTAL          *          
**********************************************************************          
         SPACE 1                                                                
TOTFWT   NTR1                                                                   
         AP    FPARNET,TLDANET                                                  
         AP    FPARCOM,TLDACOM                                                  
         AP    FPAROALC,TLDAOAC                                                 
*                                                                               
TFWT05   CLI   FFWTSUBT,0          TEST ANY SUB-TOTAL YET                       
*        BNE   TFWT10                                                           
         B     TFWT20                                                           
         GOTO1 TFWTSET,FFWTSUBT                                                 
         B     TFWT20                                                           
TFWT10   LA    R2,FFWTSUBT                                                      
         BAS   RE,TFWTADD                                                       
TFWT20   CLI   FFWTPART,0          TEST ANY PARAGRAPH TOTAL YET                 
         BNE   TFWT30                                                           
         GOTO1 TFWTSET,FFWTPART                                                 
         B     TFWTX                                                            
TFWT30   LA    R2,FFWTPART                                                      
         BAS   RE,TFWTADD                                                       
         B     TFWTX                                                            
*                                                                               
**********************************************************************          
*        ADD FWT AMOUNTS INTO SUB-TOTAL ONLY                         *          
**********************************************************************          
         SPACE 1                                                                
SUBFWT   NTR1                                                                   
*                                                                               
         CLI   FFWTSUBT,0          TEST ANY SUB-TOTAL YET                       
         BNE   SUFWT10                                                          
         GOTO1 TFWTSET,FFWTSUBT                                                 
         B     TFWTX                                                            
SUFWT10  LA    R2,FFWTSUBT                                                      
         BAS   RE,TFWTADD                                                       
         B     TFWTX                                                            
*                                                                               
         PUSH  USING                                                            
TFWTADD  LA    R1,TLDAFWT                                                       
         LA    R0,5                                                             
LINE     USING FWTELD,R1                                                        
TOT      USING FWTELD,R2                                                        
TFTWADD2 CLI   LINE.FWTEL,FWTELQ                                                
         BNER  RE                                                               
         CLC   LINE.FWTFLD,TOT.FWTFLD                                           
         BNE   *+14                NOT DEFINED FOR TOTAL                        
         AP    TOT.FWTAMT,LINE.FWTAMT                                           
         LA    R2,FWTLNQ(R2)                                                    
         LA    R1,FWTLNQ(R1)                                                    
         BCT   R0,TFTWADD2                                                      
         BR    RE                                                               
         DROP  LINE,TOT                                                         
         POP   USING                                                            
*                                                                               
         PUSH  USING                                                            
TFWTSET  LA    R2,TLDAFWT          R2=A(LINE FWTELS)                            
         LA    R0,5                                                             
LINE     USING FWTELD,R2                                                        
TOT      USING FWTELD,R1                                                        
TFTWSET2 CLI   LINE.FWTEL,FWTELQ                                                
         BNER  RE                                                               
         CLI   LINE.FWTFLD,BLFFHRSQ                                             
         BE    TFTWSET4            HOURS NOT REQUIRED ON TOTAL LINE             
         CLI   LINE.FWTFLD,BLFFRATQ                                             
         BE    TFTWSET4            RATE NOT REQUIRED ON TOTAL LINE              
         CLI   LINE.FWTFLD,BLFFCMRQ                                             
         BE    TFTWSET4            COMM RATE AS WELL                            
         CLI   LINE.FWTCOLF,0      IF START COLUMN IS ZERO                      
         BE    TFTWSET4            NOT REQUIRED ON TOTAL LINE                   
         MVC   TOT.FWTEL(FWTLNQ),LINE.FWTEL                                     
         NI    TOT.FWTCOLN,FF-X'80'   TAKE OFF 'TOTAL ONLY' FLAG                
         LA    R1,FWTLNQ(R1)                                                    
TFTWSET4 LA    R2,FWTLNQ(R2)                                                    
         BCT   R0,TFTWSET2                                                      
         BR    RE                                                               
         DROP  LINE,TOT                                                         
         POP   USING                                                            
*                                                                               
TFWTX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*        FORMAT FWTEL DATA INTO PRINT LINE                           *          
*        P1=A(FIRST FWTEL)                                           *          
*        P2=A(OUTPUT LINE)                                           *          
**********************************************************************          
         SPACE 1                                                                
FMTFWT   NTR1                                                                   
         LM    R3,R4,0(R1)                                                      
         USING FWTELD,R3                                                        
FFWT10   CLI   FWTEL,0                                                          
         BE    FFWTX                                                            
         TM    FWTCOLN,X'80'                                                    
         BO    FFWT30                                                           
         SR    R0,R0               OUTPUT LENGTH IN R0                          
         IC    R0,FWTCOLN                                                       
         SR    R2,R2               OUTPUT ADDRESS IN R2                         
         ICM   R2,1,FWTCOLF                                                     
         BZ    FFWT30              FWTEL FOR REFERENCE ONLY                     
         BCTR  R2,0                                                             
         LA    R2,0(R4,R2)                                                      
         CLI   FWTFLD,BLFFHRSQ     HOURS AND RATE ALWAYS 2DP                    
         BE    *+12                                                             
         CLI   FWTFLD,BLFFRATQ                                                  
         BNE   FFWT20                                                           
         CP    FWTAMT,BCPZERO      DO NOT PRINT ZERO                            
         BE    FFWT30                                                           
         CURED (P8,FWTAMT),((R0),(R2)),2,MINUS=YES,DMCB=BODMCB                  
         B     FFWT30                                                           
*                                  NET/COMM/GROSS USE CURRTAB                   
FFWT20   CURED (P8,FWTAMT),((R0),(R2)),CSCURBIL,MINUS=YES,DMCB=BODMCB, X        
               COMMAS=YES                                                       
*                                                                               
FFWT30   SR    R0,R0                                                            
         IC    R0,FWTLN                                                         
         AR    R3,R0                                                            
         B     FFWT10                                                           
*                                                                               
FFWTX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*        FORMAT THE NON-FWT DATA INTO PRINT BUFFER                   *          
*        TRANSFER PRINT BUFFER TO TSAR PRINT LINE                    *          
*        ADD TSAR RECORDS                                            *          
*                                                                    *          
**********************************************************************          
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
FMTDATA  NTR1                                                                   
         MVI   FFWTLINE,0          SET NO LINE FOR FWTELS                       
         LA    RF,TLDAFWT                                                       
FDAT020  CLI   0(RF),0             PUT FWTELS ON HIGHEST LINE NUMBER            
         BE    FDAT050                                                          
         CLC   FFWTLINE,FWTLINE-FWTELD(RF)                                      
         BH    *+10                                                             
         MVC   FFWTLINE,FWTLINE-FWTELD(RF)                                      
         LA    RF,FWTLNQ(RF)                                                    
         B     FDAT020                                                          
FDAT050  MVI   TLDAFWT,0           SET NO FWTELS                                
         LA    RE,FTXTTRN          CLEAR PRINT BUFFER                           
         LA    R0,FTXTLINS                                                      
         MVC   0(L'FTXTTRN,RE),BCSPACES                                         
         LA    RE,L'FTXTTRN(RE)                                                 
         BCT   R0,*-10                                                          
*                                                                               
         L     R2,AIO1             R2=A(TRANSACTION RECORD)                     
         L     R4,AMSTREC          R4=A(CONTROL RECORD)                         
         LA    R4,PBCRFST-PBCRECD(R4)                                           
         USING BLFELD,R4                                                        
FDAT100  CLI   BLFEL,0             TEST E-O-R                                   
         BE    FDAT200                                                          
         CLI   BLFEL,BLFELQ        TEST DATA FORMAT ELEMENT                     
         BNE   FDAT120                                                          
         CLC   BLFTYPE,TLKATYPE    MATCH ON PARAGRAPH FORMAT                    
         BE    FDAT150                                                          
FDAT120  SR    R0,R0                                                            
         IC    R0,BLFLN                                                         
         AR    R4,R0                                                            
         B     FDAT100                                                          
FDAT150  LA    R1,FORMTYPS         TEST FORMAT APPLIES TO THIS DATA             
*                                                                               
FDAT160  CLI   0(R1),0                                                          
         BE    FDAT120                                                          
         CLC   BLFFLD,0(R1)                                                     
         BE    *+12                                                             
         LA    R1,L'FORMTYPS(R1)                                                
         B     FDAT160                                                          
*                                                                               
         LA    R3,FTSAKTIM                                                      
         CLI   TLKATYPE,BLFTTIMQ                                                
         BE    *+8                                                              
         LA    R3,FTSAKOOP                                                      
         CLC   BLFFLD,0(R3)        TEST THIS IS THE FIRST KEY ITEM              
         BNE   FDAT180                                                          
*                                  ???? TESTING OUT ????                        
         BAS   RE,TSTKEY           TEST IF HAS ALREADY OCCURED                  
         BE    FDAT120             YES - DO NOT REPRINT                         
*                                                                               
FDAT180  SR    R3,R3                                                            
         IC    R3,BLFLINF          START LINE NUMBER                            
         BCTR  R3,0                                                             
         MH    R3,=Y(REPWREGQ)                                                  
         LA    R3,FTXTTRN(R3)                                                   
         SR    RF,RF                                                            
         IC    RF,BLFCOLF                                                       
         BCTR  RF,0                                                             
         AR    R3,RF               R3=A(DESTINATION)                            
*                                                                               
         TM    BLFOPT1,BLFONKYQ    TEST A SUPPRESSED KEY ITEM                   
         BNO   FDAT182                                                          
         CLC   FTSAKEY,TLKEY       TEST THIS TSAR RECORD FOUND                  
         BE    FDAT120             DO NOT OVERWRITE FIRST DETAILS               
*                                                                               
FDAT182  SR    RF,RF               GET DATA                                     
         ICM   RF,3,1(R1)                                                       
         LA    RF,CLB04(RF)                                                     
         BASR  RE,RF                                                            
         BNE   FDAT120             NOT FOUND/EDITED DIRECT TO LINE              
         STCM  R1,15,BODMCB        LENGTH/ADDRESS OF SOURCE                     
         STCM  R3,15,BODMCB+4      A(DESTINATION)                               
         MVC   BODMCB+4(1),BLFCOLN WIDTH OF DESTINATION                         
         SR    RE,RE                                                            
         IC    RE,BLFLINN          SET MAXIMUM LINES                            
         ST    RE,BODMCB+8                                                      
         MVI   BODMCB+8,REPWREGQ                                                
         GOTO1 VCHOPPER,BODMCB                                                  
         L     RF,BODMCB+8         SET HIGHEST NUMBER OF LINES USED             
         SR    RE,RE                                                            
         IC    RE,BLFLINF          ADD START LINE NUMBER                        
         BCTR  RE,0                                                             
         AR    RF,RE                                                            
         CLM   RF,1,FTXTNDX                                                     
         BL    FDAT120                                                          
         STC   RF,FTXTNDX                                                       
         B     FDAT120                                                          
*                                                                               
FDAT200  LA    R0,FTXTLINS         SET COUNT OF MAXIMUM LINES                   
         LA    R3,FTXTTRN                                                       
         CLI   FFWTLINE,1                                                       
         BNE   FDAT210                                                          
         MVI   TLDAFWT,FWTELQ                                                   
         MVI   FFWTLINE,0                                                       
         GOTO1 FMTFWT,BODMCB,TLDAFWT,TLDALINE                                   
FDAT210  LA    R1,TSAWRT                                                        
         CLC   FTSAKEY,TLKEY       TEST THIS TSAR RECORD FOUND                  
         BE    FDAT220                                                          
         LA    R1,TSAADD                                                        
         SR    RE,RE                                                            
         MVC   TLKSEQ,FHITSAR      SET HIGH TSAR RECNUM AS SEQUENCE             
         MVC   TLRLEN,=Y(TLAFMLNQ)                                              
FDAT220  OC    TLDALINE,0(R3)                                                   
         ZAP   BODUB1,TLDANET      SAVE ITEM AMOUNTS                            
         ZAP   BODUB2,TLDACOM                                                   
         ZAP   BODUB3,TLDAOAC                                                   
         CLI   TLDAFWT,0                                                        
         BNE   FDAT222                                                          
         ZAP   TLDANET,BCPZERO                                                  
         ZAP   TLDACOM,BCPZERO                                                  
         ZAP   TLDAOAC,BCPZERO                                                  
FDAT222  STC   R1,BOPARM           ATSARIO ACTION                               
         NI    BOPARM,FF-X'80'     RETURN ERROR IF BUFFER IS FULL               
         LA    RF,LSTLST                                                        
         STCM  RF,7,BOPARM+1       PASS A(TSAR RECORD)                          
         GOTO1 ATSARIO,BOPARM                                                   
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TOOMP)                                           
         B     EXITN                                                            
         ZAP   TLDANET,BODUB1      SAVE ITEM AMOUNTS                            
         ZAP   TLDACOM,BODUB2                                                   
         ZAP   TLDAOAC,BODUB3                                                   
         LA    R1,TSAADD                                                        
         ICM   RE,3,TLKSEQ                                                      
         LA    RE,1(RE)                                                         
         STCM  RE,3,TLKSEQ                                                      
         CLC   FHITSAR,TLKSEQ                                                   
         BNH   *+10                                                             
         MVC   TLKSEQ,FHITSAR                                                   
         XC    TLDALINE,TLDALINE   CLEAR PREVIOUS PRINT LINE                    
         CLI   TLDAFWT,0                                                        
         BE    FDAT240                                                          
         XC    TLDAFWT,TLDAFWT     CLEAR FWTELS FROM NEXT LINES                 
         ZAP   TLDANET,BCPZERO     CLEAR ITEM ALLOCATION AMOUNTS                
         ZAP   TLDACOM,BCPZERO                                                  
         ZAP   TLDAOAC,BCPZERO                                                  
FDAT240  LA    R3,REPWREGQ(R3)     GET NEXT LINE IN BLOCK                       
         BCT   R0,*+8                                                           
         B     FDAT260             FINISHED                                     
         LA    RF,FTXTLINS+1                                                    
         SR    RF,R0                                                            
         CLM   RF,1,FFWTLINE       TEST NEXT LINE NUMBER                        
         BNE   FDAT250                                                          
         MVI   TLDAFWT,FWTELQ                                                   
         MVI   FFWTLINE,0                                                       
         GOTO1 FMTFWT,BODMCB,TLDAFWT,TLDALINE                                   
         LA    R1,TSAADD                                                        
FDAT250  CLC   0(REPWREGQ,R3),BCSPACES                                          
         BNE   FDAT220                                                          
         OC    TLDALINE,TLDALINE                                                
         BNZ   FDAT220                                                          
         B     FDAT240             DO NOT ADD BLANK LINES                       
FDAT260  MVC   FHITSAR,TLKSEQ      SAVE NEW HIGH TSAR RECORD NUMBER             
*                                                                               
FDAT900  B     EXITY                                                            
*                                                                               
GETWC    LA    R1,TRNKWORK                                                      
         ICM   R1,8,=AL1(L'TRNKWORK)                                            
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
GETWCN   NTR1                                                                   
         GOTO1 RDWCODE             ENSURE WORKCODE IN IO4                       
         L     RE,AIO4             R1=A(WORKCODE RECORD)                        
         LA    RE,WCORFST-WCORECD(RE)                                           
         LA    R1,WCODESC-WCOELD(RE)                                            
         ICM   R1,8,=AL1(L'WCODESC)                                             
         SR    R0,R0                                                            
GWCN10   IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0             E-O-R                                        
         BE    GWCN40                                                           
         CLI   0(RE),NAMELQ        TEST EXTRA NAME ELEMENT                      
         BNE   GWCN20                                                           
         SR    R1,R1                                                            
         IC    R1,NAMLN-NAMELD(RE)                                              
         SH    R1,=Y(NAMEREC-NAMELD)                                            
         STC   R1,BCBYTE1                                                       
         LA    R1,NAMEREC-NAMELD(RE)                                            
         ICM   R1,8,BCBYTE1                                                     
         B     GWCN10                                                           
*                                                                               
GWCN20   CLI   0(RE),XNMELQ        TEST FOREIGN LANGUAGE NAME                   
         BNE   GWCN30                                                           
         CLI   CSFMLANG,0          TEST FORMATTING IN FOREIGN LANGUAGE          
         BE    GWCN10              NO                                           
         MVC   BCBYTE1,CSFMLANG    SET FORMAT LANGUAGE                          
         CLI   CSFMLANG,LANGEUK    TEST IF ENGLISH                              
         BNE   *+8                                                              
         MVI   BCBYTE1,LANGENG     SET DEFAULT ENGLISH                          
         CLC   BCBYTE1,XNMSTAT-XNMELD(RE)                                       
         BNE   GWCN10                                                           
         SR    R1,R1                                                            
         IC    R1,XNMSUBL-XNMELD(RE)                                            
         BCTR  R1,0                                                             
         STC   R1,BCBYTE1                                                       
         LA    R1,XNMSUBN-XNMELD(RE)                                            
         ICM   R1,8,BCBYTE1                                                     
         B     GWCN10                                                           
*                                                                               
GWCN30   CLI   0(RE),FFTELQ        TEST BILLING NAME                            
         BNE   GWCN10                                                           
         CLI   FFTTYPE-FFTELD(RE),FFTTFREE                                      
         BNE   GWCN10                                                           
         CLI   FFTSEQ-FFTELD(RE),0                                              
         BNE   GWCN10                                                           
         CLI   FFTDATA-FFTELD(RE),C'*'                                          
         BE    GWCN10              TWINKLE MEANS DO NOT USE                     
         LA    R1,FFTDATA-FFTELD(RE)                                            
         ICM   R1,8,FFTDLEN-FFTELD(RE)                                          
*                                                                               
GWCN40   CR    RB,RB                                                            
         B     XITR1                                                            
*                                                                               
GETREF   LA    R1,TRNKREF                                                       
         ICM   R1,8,=AL1(L'TRNKREF)                                             
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
GETDAT   NTR1                                                                   
         MVC   BOWORK1,BCSPACES    CLEAR WORK AREA                              
         GOTO1 VDATCON,BODMCB,(1,TRNKDATE),(8,BOWORK1)                          
         B     SETABOW                                                          
*                                                                               
*                                                                               
GETSUP   LA    R1,FCTRNAM+L'FCTRNAM-1                                           
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    RF,FCTRNAM                                                       
         SR    R1,RF                                                            
         LA    R1,1(R1)                                                         
         SLDL  R0,32                                                            
         LA    R1,FCTRNAM                                                       
         SLL   R1,8                                                             
         SRDL  R0,8                                                             
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
GETNAR   SR    R1,R1                                                            
         IC    R1,TRNRFST+(TRNLN-TRNEL)                                         
         SH    R1,=Y(TRNLN1Q)                                                   
         BNZ   *+8                                                              
         LTR   RB,RB                                                            
         BR    RE                                                               
         SLDL  R0,32               SAVE LENGTH                                  
         LA    R1,TRNRFST+(TRNNARR-TRNEL)                                       
         SLL   R1,8                                                             
         SRDL  R0,8                                                             
         CR    RB,RB                                                            
         BR    RE                                                               
*                                                                               
GETCDS   NTR1                                                                   
         SR    R0,R0               CASH DISCOUNT                                
         IC    R0,BLFCOLN                                                       
         CURED (P8,PA$DSC),((R0),0(R3)),CSCURBIL,MINUS=YES,            X        
               DMCB=BODMCB,COMMAS=YES                                           
         LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
GETKSV   NTR1                                                                   
         LA    R0,RATETAXQ         KSV RATE ELEMENT                             
         GOTO1 VHELLO,BODMCB,(C'G',ACCMST),((R0),AIO1),0,0                      
         CLI   BODMCB+12,0                                                      
         BNE   GETKSV2                                                          
         L     RE,BODMCB+12                                                     
         MVC   BCHALF,RATRATE-RATELD(RE)                                        
         SR    R0,R0                                                            
         IC    R0,BLFCOLN                                                       
         CURED (B2,BCHALF),((R0),0(R3)),2,MINUS=YES,DMCB=BODMCB,       X        
               COMMAS=YES                                                       
GETKSV2  LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
GETPLM   NTR1                                                                   
         LA    R0,TPRELQ           PRICE LIST ELEMENT                           
         GOTO1 VHELLO,BODMCB,(C'G',ACCMST),((R0),AIO1),0,0                      
         CLI   BODMCB+12,0                                                      
         BNE   GETPLM2                                                          
         L     RE,BODMCB+12                                                     
         CLI   TPRLN-TPRELD(RE),TPRLNQ                                          
         BNE   *+14                                                             
         PACK  BODUB1,TPRNUM-TPRELD(L'TPRNUM,RE)                                
         B     *+10                                                             
         ZAP   BODUB1,TPRNUMP-TPRELD(L'TPRNUMP,RE)                              
         ZAP   BODUB3(16),PP$AALLO ALLOCATED NET AMOUNT                         
         MP    BODUB3(16),BODUB1   * TOTAL NUMBER OF UNITS                      
         SRP   BODUB3(16),4,0      * 10000                                      
         DP    BODUB3(16),PA$NET   / TOTAL NET AMOUNT                           
         SRP   BODUB3,64-4,5       / 10000                                      
         ZAP   BODUB1,BODUB3                                                    
         SR    R0,R0                                                            
         IC    R0,BLFCOLN                                                       
         CURED (P8,BODUB1),((R0),0(R3)),2,MINUS=YES,DMCB=BODMCB,       X        
               COMMAS=YES                                                       
GETPLM2  LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
         PUSH  USING                                                            
GETPLV   NTR1  ,                   PRICE LIST ELEMENT                           
         GOTO1 VHELLO,BODMCB,(C'G',ACCMST),('TPRELQ',AIO1),0,0                  
         CLI   12(R1),0                                                         
         BNE   GETPLVX                                                          
         L     RF,12(R1)                                                        
         USING TPRELD,RF                                                        
         LH    R2,=Y(PLTAB-FWORKD)                                              
         AR    R2,RC                                                            
         LA    R0,PLMAXQ                                                        
         USING PLCODE,R2                                                        
GPLV02   CLI   PLCODE,0            TEST E-O-T                                   
         BE    GPLV04                                                           
         CLC   PLCODE,TPRCOD                                                    
         BE    GPLV10                                                           
         LA    R2,L'PLTAB(R2)                                                   
         BCT   R0,GPLV02                                                        
*                                                                               
         LH    R2,=Y(PLTAB-FWORKD) TABLE FULL                                   
         AR    R2,RC               EMPTY IT AND START AGAIN                     
         MVI   0(R2),EOT                                                        
*                                                                               
GPLV04   DS    0H                                                               
K        USING PRLRECD,IOKEY                                                    
         MVC   K.PRLKEY,BCSPACES                                                
         MVI   K.PRLKTYP,PRLKTYPQ                                               
         MVC   K.PRLKCPY,CUABIN                                                 
         MVC   K.PRLKNUM,TPRCOD                                                 
         DROP  RF                                                               
         GOTO1 AIO,IOREAD+IOACCDIR+IO3                                          
         BNE   GETPLVX                                                          
         GOTO1 AIO,IOGET+IOACCMST+IO3                                           
         BNE   GETPLVX                                                          
         GOTO1 VHELLO,BODMCB,(C'G',ACCMST),('FFNELQ',AIO3),0,0                  
         CLI   12(R1),0                                                         
         BNE   GETPLVX                                                          
         L     RF,12(R1)                                                        
         USING FFNELD,RF                                                        
         MVC   PLCODE,K.PRLKNUM                                                 
         PACK  PLAMOUNT,FFNUMBER                                                
         MVI   PLCODE+L'PLTAB,EOT                                               
         DROP  K,RF                                                             
*                                                                               
GPLV10   DS    0H                                                               
         ZAP   BODUB1,PLAMOUNT     READ RECORD FOR UNIT PRICE                   
         SR    R0,R0                                                            
         IC    R0,BLFCOLN                                                       
         CURED (P8,BODUB1),((R0),0(R3)),CSCURBIL,MINUS=YES,            X        
               DMCB=BODMCB,COMMAS=YES                                           
GETPLVX  LTR   RB,RB                                                            
         B     EXIT                                                             
         POP   USING                                                            
*                                                                               
*&&UK                                                                           
GETORG   NTR1                                                                   
         L     RE,AIO1                                                          
         LA    RE,TRNRFST-TRNRECD(RE)                                           
         LA    RF,CSCPYCUR         RF=A(CURRENCY OF INPUT)                      
         TM    TRNSTAT-TRNELD(RE),TRNS2NDC                                      
         BNO   *+8                                                              
         LA    RF,BCCPYSEC                                                      
         CLC   CSBILCUR,0(RF)      INPUT CURR=BILLING CURR                      
         BE    EXIT                DO NOT SHOW ORIGINAL INPUT                   
         ZAP   BODUB1,PP$FALLO                                                  
         SR    R0,R0                                                            
         IC    R0,BLFCOLN                                                       
         CURED (P8,BODUB1),((R0),0(R3)),CSCURBIL,MINUS=YES,            X        
               DMCB=BODMCB,COMMAS=YES                                           
         LTR   RB,RB                                                            
         B     EXIT                                                             
*&&                                                                             
*                                                                               
SETABOW  LA    R1,BOWORK1+L'BOWORK1-1                                           
         CLI   0(R1),C' '          FIND REAL OUTPUT LENGTH                      
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    RF,BOWORK1                                                       
         SR    R1,RF                                                            
         LA    R1,1(R1)            R1=(REAL O/P LENGTH)                         
         SLDL  R0,32               SHUNT INTO LOW R0 BYTE                       
         LA    R1,BOWORK1          SET ADDRESS OF SOURCE                        
         SLL   R1,8                SHIFT UP                                     
         SRDL  R0,8                SHIFT BACK WITH LENGTH                       
         CR    RB,RB                                                            
         B     XITR1                                                            
*                                                                               
XITR1    XIT1  REGS=(R1)                                                        
*                                                                               
FORMTYPS DS    0XL3                                                             
         DC    AL1(BLFFWCQ),AL2(GETWC-CLB04)      WORKCODE                      
         DC    AL1(BLFFWCNQ),AL2(GETWCN-CLB04)    WORKCODE NAME                 
         DC    AL1(BLFFREFQ),AL2(GETREF-CLB04)    TRANSACTION REFERENCE         
         DC    AL1(BLFFDATQ),AL2(GETDAT-CLB04)    TRANSACTION DATE              
         DC    AL1(BLFFSUPQ),AL2(GETSUP-CLB04)    SUPPLIER/PERSON NAME          
         DC    AL1(BLFFNARQ),AL2(GETNAR-CLB04)    NARRATIVE                     
         DC    AL1(BLFFCDSQ),AL2(GETCDS-CLB04)    DISCOUNT (CANT EDIT)          
         DC    AL1(BLFFKSVQ),AL2(GETKSV-CLB04)    KSV (CANT EDIT)               
         DC    AL1(BLFFPLMQ),AL2(GETPLM-CLB04)    PRICELIST MULTIPLIER          
         DC    AL1(BLFFPLVQ),AL2(GETPLV-CLB04)    PRICELIST VALUE               
*&&UK*&& DC    AL1(BLFFORGQ),AL2(GETORG-CLB04)    ORIGINAL AMOUNT               
         DC    AL1(0)                                                           
         EJECT                                                                  
**********************************************************************          
*        BUILD LIST OF TSAR KEY COMPONENTS FOR TIME AND OOPS         *          
*        SORT ORDER IS BASED ON LEFT TO RIGHT PRINT ORDER OF         *          
*        DATA ITEMS THAT ARE CONSIDERED KEY COMPONENTS               *          
**********************************************************************          
         SPACE 1                                                                
BLDKLIST NTR1                                                                   
         LA    R0,MAXSECTS                                                      
         LA    RF,FTXTTRN                                                       
BLDK05   XC    0(REPWREGQ,RF),0(RF)                                             
         LA    RF,REPWREGQ(RF)                                                  
         BCT   R0,BLDK05                                                        
*                                                                               
         L     R4,AMSTREC                                                       
         LA    R4,PBCRFST-PBCRECD(R4)                                           
         USING BLFELD,R4                                                        
BLDK10   CLI   BLFEL,0                                                          
         BE    BLDK40                                                           
         CLI   BLFEL,BLFELQ                                                     
         BE    BLDK30                                                           
BLDK20   SR    R0,R0                                                            
         IC    R0,BLFLN                                                         
         AR    R4,R0                                                            
         B     BLDK10                                                           
BLDK30   TM    BLFOPT1,BLFONKYQ    TEST EXCLUDED FROM THE KEY SEARCH            
         BO    BLDK20              YES                                          
         LA    RF,KEYITEMS         TEST THIS DATA IS A KEY COMPONENT            
         LA    R0,MAXKEYIQ                                                      
         CLC   BLFFLD,0(RF)                                                     
         BE    *+16                                                             
         LA    RF,L'KEYITEMS(RF)                                                
         BCT   R0,*-14                                                          
         B     BLDK20                                                           
         CLI   BLFLINF,1           KEY ITEMS MUST BE ON LINE 1                  
         BH    BLDK20                                                           
         SR    RF,RF               GET SECTION NUMBER                           
         IC    RF,BLFSECT                                                       
         BCTR  RF,0                                                             
         MH    RF,=Y(REPWREGQ)                                                  
         LA    RF,FTXTTRN(RF)      RF=A(SECTION LIST OF KEY ITEMS)              
         SR    RE,RE                                                            
         IC    RE,BLFCOLF                                                       
         AR    RE,RF               INDEX TO PRINT LINE                          
         MVC   0(1,RE),BLFFLD      SET CODE IN ITS FIRST PRINT COLUMN           
         B     BLDK20                                                           
*                                  COMPRESS THE LISTS OF KEY ITEMS              
BLDK40   XC    FTSAKEYS(MAXSECTS*L'FTSAKEYS),FTSAKEYS                           
         LA    R4,MAXSECTS         MAXIMUM NUMBER OF SECTIONS                   
         LA    RE,FTXTTRN          BLOCK OF KEY ITEMS                           
BLDK50   LA    R0,REPWREGQ                                                      
         LA    RF,MAXSECTS                                                      
         SR    RF,R4                                                            
         MH    RF,=Y(L'FTSAKEYS)   RF=INDEX INTO COMPRESSED LISTS               
         LA    RF,FTSAKEYS(RF)                                                  
BLDK60   CLI   0(RE),0                                                          
         BE    *+14                                                             
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,BLDK60                                                        
         BCT   R4,BLDK50                                                        
*                                                                               
BLDKX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*        BUILD TSAR KEY FOR THIS TRANSACTION                         *          
**********************************************************************          
         SPACE 1                                                                
BLDKEY   NTR1                                                                   
         USING TRNRECD,R2                                                       
         L     R2,AIO1                                                          
         USING GOBLOCKD,RE                                                      
         L     RE,AGOPBLK                                                       
         XC    TLKEY,TLKEY                                                      
         MVI   TLKSES,TLKALINQ                                                  
         CLC   GOSELWC,TRNKWORK    TEST GETOPT CALLED FOR WORKCODE              
         BE    BKEY10                                                           
         GOTO1 AGETOPT,BODMCB,AIO1                                              
         L     RE,AGOPBLK                                                       
*&&UK                                                                           
BKEY10   B     BKEY12                                                           
*&&                                                                             
*                                                                               
*&&US                                                                           
BKEY10   CLC   GOTWO,BCSPACES      TEST ANY GOTWO SETTING                       
         BH    *+12                YES                                          
         CLI   GOWRKTY,C'T'        NO - DEFAULT TIME TYPE IS T                  
         BE    BKEY12                                                           
         LA    RF,GOTWO            TEST W/C TYPE IN LIST OF TIME TYPES          
         LA    R0,L'GOTWO                                                       
         CLC   GOWRKTY,0(RF)                                                    
         BE    BKEY12                                                           
         LA    RF,L'GOWRKTY(RF)                                                 
         BCT   R0,*-14                                                          
******   CLI   P#SKLABR,C'Y'       PROFILE SK CONTRA LABOR TOO                  
******   BNE   BKEY14                                                           
******   CLC   =C'SK',TRNKULC                                                   
******   BNE   BKEY14                                                           
*&&                                                                             
BKEY12   BAS   RE,GETSECT          GET SECTION NUMBER FOR THIS ITEM             
         SR    R1,R1                                                            
         IC    R1,TLKATYPE                                                      
         BCTR  R1,0                                                             
         MH    R1,=Y(L'FTSAKEYS)   INDEX TO THIS SECTION KEYS                   
         LA    R1,FTSAKEYS(R1)                                                  
*                                                                               
         LA    R3,TLKAREST         R3=A(REMAINING KEY)                          
         USING KEYITEMD,R4                                                      
BKEY16   CLI   0(R1),0             TEST END OF KEY COMPONENTS                   
         BE    BKEY30                                                           
         LA    R4,KEYITEMS                                                      
         LA    R0,MAXKEYIQ                                                      
         CLC   KEYITYPE,0(R1)                                                   
         BE    BKEY18                                                           
         LA    R4,L'KEYITEMS(R4)                                                
         BCT   R0,*-14                                                          
         DC    H'0'                MUST BE A BUG                                
BKEY18   LA    RF,TRNRECD          SET RF=A(KEY)                                
         CLI   KEYIELEM,0          TEST IF DATA IN ELEMENT                      
         BE    BKEY20                                                           
         L     RF,AGOPBLK          SET RF=A(GOBLOCK)                            
         CLI   KEYIELEM,FF         TEST IF DATA IN GOBLOCK                      
         BE    BKEY20                                                           
         LR    R0,R1               SAVE R1                                      
         GOTO1 VHELLO,BODMCB,(C'G',ACCMST),(KEYIELEM,AIO1),0,0                  
         LR    R1,R0               RESTORE R1                                   
         SR    RF,RF                                                            
         CLI   BODMCB+12,0                                                      
         BE    BKEY19                                                           
         CLI   KEYITYPE,BLFFRATQ                                                
         BNE   BKEY22                                                           
         ZAP   BODUB1,PA$HOURS                                                  
         BZ    BKEY22                                                           
         ZAP   BODUB1(16),PA$NET   TRANSACTION AMOUNT                           
         SRP   BODUB1(16),4,0                                                   
         DP    BODUB1(16),PA$HOURS TOTAL POSTED HOURS                           
         SRP   BODUB1,64-2,5                                                    
         LA    RF,BODUB1+4                                                      
         B     BKEY22                                                           
BKEY19   L     RF,BODMCB+12        SET RF=A(ELEMENT)                            
BKEY20   SR    RE,RE                                                            
         ICM   RE,3,KEYIDISP       ADD IN DISPLACEMENT                          
         AR    RF,RE                                                            
BKEY22   SR    RE,RE                                                            
         ICM   RE,1,KEYILEN        RE=DATA LENGTH                               
         BNZ   BKEY23                                                           
         IC    RE,TRNRFST+(TRNLN-TRNELD)                                        
         SH    RE,=Y(TRNNARR-TRNELD)                                            
         BNZ   *+12                                                             
         LA    RE,1                                                             
         LA    RF,=C'A'                                                         
         CH    RE,=Y(20)           SET 20 AS THE MAXIMUM                        
         BNH   BKEY23                                                           
         LA    RE,20                                                            
BKEY23   LA    R0,TLKSEQ           TEST END OF KEY MARKER                       
         CR    R3,R0                                                            
         BNL   BKEY30              NO MORE KEY AREA LEFT                        
         SR    R0,R3               R0=REMAINING KEY AREA                        
         CR    R0,RE                                                            
         BH    *+6                                                              
         LR    RE,R0               ONLY USE WHATS AVAILABLE                     
         LTR   RF,RF                                                            
         BZ    BKEY30                                                           
         LTR   RE,RE                                                            
         BNP   BKEY30                                                           
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R3),0(RF)       MOVE DATA TO TSAR KEY                        
         LA    RE,1(RE)            RESTORE LENGTH                               
         CLI   KEYILEN,0           TEST FIXED LENGTH                            
         BNE   BKEY24                                                           
         LA    RE,20               USE THE MAXIMUM                              
BKEY24   AR    R3,RE               R3=A(NEXT SLOT)                              
         LA    R1,1(R1)            R1=A(NEXT KEY COMPONENT CODE)                
         B     BKEY16                                                           
*                                                                               
BKEY30   MVC   FTSAKEY,TLKEY       SAVE KEY UP TO SEQUENCE                      
         GOTO1 ATSARIO,TSARDH                                                   
         BL    *+14                E-O-F                                        
         CLC   FTSAKEY,TLKEY                                                    
         BE    BKEYX                                                            
         XC    TLKEY,TLKEY         CLEAR KEY                                    
         MVC   TLKEY(L'FTSAKEY),FTSAKEY                                         
         XC    FTSAKEY,FTSAKEY     CLEAR SAVED KEY AS INDICATOR                 
         XC    TLDAFWT,TLDAFWT                                                  
         ZAP   TLDANET,BCPZERO     CLEAR ITEM ALLOCATION AMOUNTS                
         ZAP   TLDACOM,BCPZERO                                                  
         ZAP   TLDAOAC,BCPZERO                                                  
         XC    TLDALINE,TLDALINE                                                
*                                                                               
BKEYX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*        TEST IF FIRST KEY ITEM ALREADY IN TSAR                      *          
**********************************************************************          
         SPACE 1                                                                
TSTKEY   NTR1                                                                   
         LA    R0,FSAVTSAR         SAVE THIS TSAR RECORD                        
         LA    R1,TLAFMLNQ                                                      
         LA    RE,TLREC                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LA    R1,FTSAKTIM                                                      
         CLI   TLKATYPE,BLFTTIMQ                                                
         BE    *+8                                                              
         LA    R1,FTSAKOOP         R1=A(FIRST KEY COMPONENT)                    
         CLI   0(R1),0             TEST ANY                                     
         BE    TKEYXNO             SET CC NEQ                                   
         LA    R4,KEYITEMS         R4=A(KEY ITEM TABLE)                         
         LA    R0,MAXKEYIQ                                                      
         USING KEYITEMD,R4                                                      
TKEY10   CLC   KEYITYPE,0(R1)                                                   
         BE    TKEY20                                                           
         LA    R4,L'KEYITEMS(R4)                                                
         BCT   R0,TKEY10                                                        
         DC    H'0'                MUST BE A BUG                                
TKEY20   SR    R3,R3                                                            
         IC    R3,KEYILEN          R3=KEY ITEM LENGTH)                          
         LA    R3,TLKAREST(R3)                                                  
         LR    R1,R3               R1=A(FIRST UNWANTED KEY CHARACTER)           
         LA    RE,TLKAREST                                                      
         SR    R3,RE               R3=LENGTH OF SIGNIFICANT KEY                 
         LA    RE,(L'TLKSRT-L'TLKAPARA)                                         
         SR    RE,R3               RE=LENGTH OF UNWANTED PART                   
         MVI   0(R1),0                                                          
         LA    R1,1(R1)                                                         
         BCT   RE,*-8                                                           
*                                                                               
         GOTO1 ATSARIO,TSARDH                                                   
         BL    TKEY30                                                           
         LA    R3,(TLKAREST-TLKEY-1)(R3)                                        
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   TLKEY(0),FSAVTSAR+(TLKEY-TLREC)                                  
         MVI   BCBYTE1,C'Y'                                                     
         BE    *+8                                                              
TKEY30   MVI   BCBYTE1,C'N'                                                     
         LA    R0,TLREC            RESTORE ORIGINAL TSAR RECORD                 
         LA    R1,TLAFMLNQ                                                      
         LA    RE,FSAVTSAR                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         CLI   BCBYTE1,C'Y'                                                     
         BE    TKEYXYES                                                         
*                                                                               
TKEYXNO  LTR   RB,RB                                                            
         B     *+6                                                              
TKEYXYES CR    RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*        INITIALISE TABLE OF SECTION DEFINITIONS                     *          
**********************************************************************          
         SPACE 1                                                                
SETDEFN  NTR1                                                                   
*                                  CLEAR DEFINITION TABLES                      
         LH    R0,=Y(DEFTAB-FWORKD)                                             
         AR    R0,RC                                                            
         LA    R1,SECTTABL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R2,IOKEY            BUILD KEY OF SECTION DEFINITION              
         USING PBSRECD,R2                                                       
         XC    PBSKEY,PBSKEY                                                    
         MVI   PBSKTYP,PBSKTYPQ                                                 
         MVC   PBSKCPY,CUABIN                                                   
         MVI   PBSKSUB,PBSKDEFQ                                                 
         MVC   PBSKFMT,CSFORMAT                                                 
*        MVC   PBSKLANG,GOBILLNG   BILL LANGUAGE MAY BE SET IN OPT MNT          
         MVC   PBSKLANG,CSFMLANG                                                
         LA    R1,IO2+IOACCMST+IOHI                                             
         B     *+8                                                              
SETD20   LA    R1,IO2+IOACCMST+IOSEQ                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
         CLC   PBSKEY(PBSKSEC-PBSKEY),IOKEYSAV                                  
         BNE   SETDX               FINISHED                                     
         CLC   PBSKLANG,CSFMLANG   MATCH ON LANGUAGE                            
         BNE   SETD20                                                           
         MVC   BCBYTE1,PBSKSEC     SAVE SECTION CODE                            
         LA    R2,PBSRFST                                                       
         USING FFTELD,R2                                                        
SETD30   CLI   FFTEL,0             TEST E-O-R                                   
         BE    SETD20              GET NEXT RECORD                              
         CLI   FFTEL,BSDELQ        TEST DEFINITION ELEMENT                      
         BE    SETD40                                                           
         CLI   FFTEL,FFTELQ        TEST HEADER/SUBTOTAL NAME ELEMENT            
         BNE   SETD34                                                           
         SR    R0,R0                                                            
         CLI   FFTTYPE,FFTTSECN    SECTION NAME                                 
         BE    SETD32                                                           
         LA    R0,L'FBPBST                                                      
         CLI   FFTTYPE,FFTTSUBT                                                 
         BNE   SETD34                                                           
SETD32   SR    RF,RF                                                            
         IC    RF,BCBYTE1          SECTION NUMBER                               
         BCTR  RF,0                                                             
         MH    RF,=Y(L'SNAMTAB)                                                 
         LH    RE,=Y(SNAMTAB-FWORKD)                                            
         AR    RE,RC                                                            
         AR    RF,RE                                                            
         AR    RF,R0                                                            
         IC    RE,FFTLN                                                         
         SH    RE,=Y(FFTLN1Q+1)                                                 
         EX    RE,*+4                                                           
         MVC   0(0,RF),FFTDATA     MOVE NAME                                    
*                                                                               
SETD34   SR    R0,R0                                                            
         IC    R0,FFTLN                                                         
         AR    R2,R0                                                            
         B     SETD30                                                           
         USING BSDELD,R2                                                        
SETD40   SR    R3,R3               DATA TYPE INDEXES TO TABLE                   
         ICM   R3,1,BSDSTYP                                                     
         BNZ   *+14                                                             
         MVC   FALLSECT,BCBYTE1    THIS SECTION CATCHES OTHERS                  
         B     SETD34                                                           
         BCTR  R3,0                                                             
         SLL   R3,2                                                             
         LA    R3,SETDTABS(R3)                                                  
         SR    RF,RF                                                            
         ICM   RF,7,1(R3)                                                       
         AR    RF,RC               RF=A(THIS ITEM TABLE)                        
         SR    R1,R1                                                            
         ICM   R1,1,0(R3)          R1=THIS ITEM DATA LENGTH                     
         BNZ   *+12                                                             
         IC    R1,BSDLN                                                         
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         SR    RE,RE                                                            
         ICM   RE,3,0(RF)          RE=DISP OF NEXT ITEM                         
         AR    RE,R1               BUMP FOR NEXT                                
         STCM  RE,3,0(RF)          SET NEW DISP                                 
         SR    RE,R1                                                            
         LA    RF,2(RE,RF)         RF=A(CURRENT SLOT)                           
         CLI   0(R3),0             TEST ANY FIXED ITEM LENGTH                   
         BNE   SETD50                                                           
*                                                                               
         MVC   0(L'PBSKSEC,RF),BCBYTE1                                          
         STC   R1,1(RF)            SET TABLE ENTRY LENGTH                       
         SH    R1,=Y(3)                                                         
         EX    R1,*+4                                                           
         MVC   2(0,RF),BSDDATA     SET DATA                                     
         B     SETD34                                                           
*                                                                               
SETD50   SH    R1,=Y(2)                                                         
         EX    R1,*+4                                                           
         MVC   1(0,RF),BSDDATA                                                  
         MVC   0(L'PBSKSEC,RF),BCBYTE1                                          
         B     SETD34                                                           
*                                                                               
SETDX    CLI   FALLSECT,0          TEST ANY SECTION DEFINED FOR ALL             
         BNE   *+8                                                              
         MVI   FALLSECT,1          DEFAULT TO LOWEST NUMBERED SECTION           
         B     EXIT                                                             
*                                                                               
SETDTABS EQU   *                                                                
         DC    AL1(2),AL3(WCTTAB-FWORKD)                                        
         DC    AL1(2),AL3(WCGTAB-FWORKD)                                        
         DC    AL1(3),AL3(WCCTAB-FWORKD)                                        
         DC    AL1(2),AL3(TRNTAB-FWORKD)                                        
         DC    AL1(0),AL3(CACTAB-FWORKD)                                        
         DC    AL1(11),AL3(CATTAB-FWORKD)                                       
*                                                                               
         EJECT                                                                  
**********************************************************************          
*        GET SECTION NUMBER FOR THIS TRANSACTION                     *          
**********************************************************************          
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
         USING TRNEL,TRNRFST                                                    
GETSECT  NTR1                                                                   
*                                  FIRST SEARCH FOR CONTRA ACC MATCH            
         LH    RF,=Y(CACTAB-FWORKD)                                             
         AR    RF,RC                                                            
         LA    RF,2(RF)                                                         
GSECT100 CLI   0(RF),0             E-O-T                                        
         BE    GSECT200                                                         
         SR    R1,R1                                                            
         IC    R1,1(RF)                                                         
         SH    R1,=Y(3)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TRNKULC(0),2(RF)                                                 
         BE    GSECT900                                                         
         IC    R1,1(RF)                                                         
         AR    RF,R1                                                            
         B     GSECT100                                                         
*                                  SEARCH FOR INPUT TYPE MATCH                  
GSECT200 LH    RF,=Y(TRNTAB-FWORKD)                                             
         AR    RF,RC                                                            
         LA    RF,2(RF)                                                         
GSECT202 CLI   0(RF),0                                                          
         BE    GSECT300                                                         
         CLC   TRNTYPE,1(RF)                                                    
         BE    GSECT900                                                         
         LA    RF,2(RF)                                                         
         B     GSECT202                                                         
*                                  SEARCH FOR WORKCODE MATCH                    
GSECT300 LH    RF,=Y(WCCTAB-FWORKD)                                             
         AR    RF,RC                                                            
         LA    RF,2(RF)                                                         
GSECT302 CLI   0(RF),0                                                          
         BE    GSECT400                                                         
         CLC   TRNKWORK,1(RF)                                                   
         BE    GSECT900                                                         
         LA    RF,3(RF)                                                         
         B     GSECT302                                                         
*                                  SEARCH FOR WORKCODE GROUP MATCH              
GSECT400 LH    RF,=Y(WCGTAB-FWORKD)                                             
         AR    RF,RC                                                            
         LA    RF,2(RF)                                                         
GSECT402 CLI   0(RF),0                                                          
         BE    GSECT500                                                         
         CLC   FTRNWGRP,1(RF)                                                   
         BE    GSECT900                                                         
         LA    RF,2(RF)                                                         
         B     GSECT402                                                         
*                                  SEARCH FOR WORKCODE TYPE MATCH               
GSECT500 LH    RF,=Y(WCTTAB-FWORKD)                                             
         AR    RF,RC                                                            
         LA    RF,2(RF)                                                         
GSECT502 CLI   0(RF),0                                                          
         BE    GSECT600                                                         
         CLC   FTRNWTYP,1(RF)                                                   
         BE    GSECT900                                                         
         LA    RF,2(RF)                                                         
         B     GSECT502                                                         
*                                                                               
GSECT600 LA    RF,FALLSECT         GET SECTION NUMBER FOR ALL OTHERS            
*                                                                               
GSECT900 MVC   TLKATYPE,0(RF)      SET SECTION TYPE IN TSAR RECORD              
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
**********************************************************************          
*        SUNDRY TABLES ETC                                           *          
**********************************************************************          
         SPACE 1                                                                
ACCMST   DC    C'ACCMST '                                                       
FF       EQU   X'FF'                                                            
*                                                                               
KEYITEMS DS    0XL5                                                             
         DC    AL1(BLFFWCQ,0),AL1(L'TRNKWORK),AL2(TRNKWORK-TRNRECD)             
         DC    AL1(BLFFWCNQ,0),AL1(L'TRNKWORK),AL2(TRNKWORK-TRNRECD)            
         DC    AL1(BLFFSUPQ,0),AL1(L'TRNKULC),AL2(TRNKULC-TRNRECD)              
         DC    AL1(BLFFDATQ,0),AL1(L'TRNKDATE),AL2(TRNKDATE-TRNRECD)            
         DC    AL1(BLFFREFQ,0),AL1(L'TRNKREF),AL2(TRNKREF-TRNRECD)              
         DC    AL1(BLFFRATQ,PRTELQ),AL1(L'PRTRATE),AL2(PRTRATE-PRTELD)          
         DC    AL1(BLFFNARQ,TRNELQ),AL1(0),AL2(TRNNARR-TRNELD)                  
         DC    AL1(BLFFCMRQ,FF),AL1(L'GOAGYCOM),AL2(GOAGYCOM-GOBLOCK)           
MAXKEYIQ EQU   (*-KEYITEMS)/(L'KEYITEMS)                                        
*                                                                               
KEYITEMD DSECT                                                                  
KEYITYPE DS    XL1                 DATA TYPE                                    
KEYIELEM DS    XL1                 ELEMENT OR 0                                 
KEYILEN  DS    XL1                 DATA LENGTH FOR KEY                          
KEYIDISP DS    XL2                 DISPLACEMENT IN ELEMENT/KEY                  
         EJECT                                                                  
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORKC                                                                    
       ++INCLUDE ACCLBWORKB                                                     
         SPACE 1                                                                
*                                                                               
FWORKD   DSECT                                                                  
PRATBLK  DS    CL(PR$LNQ)                                                       
FPBRKEY  DS    CL(L'PBRKEY)        PREVIOUS KEY                                 
FPBRPAS  DS    CL(L'PBRPAS)        PASSIVE KEY                                  
AMSTREC  DS    A                                                                
FIRECACT DS    0XL2                CALLING PROGRAM RECORD/ACTION                
FIREC    DS    XL1                                                              
FIACT    DS    XL1                                                              
AFINDS   DS    XL1                                                              
AFFORBIL EQU   X'80'               FOREIGN CURRENCY BILLING                     
FBILLNO  DS    CL6                 NEW DRAFT BILL NUMBER                        
FHDRDA   DS    XL4                 HEADER RECORD DISK ADDRESS                   
FJOBNET  DS    PL6                 NET ALLOCATION - BILL                        
FJOBCOM  DS    PL6                 COMM ALLOCATION - BILL                       
FPARNET  DS    PL6                 NET ALLOCATION - PARAGRAPH                   
FPARCOM  DS    PL6                 COMM ALLOCATION - PARAGRAPH                  
FPAROALC DS    PL6                 OVERALLOCATION - PARAGRAPH                   
FPARCDSC DS    PL6                 CASH DISCOUNT ALLOCATED (US)                 
FTRNNET  DS    PL6                 NET ALLOCATION - ITEM                        
FTRNCOM  DS    PL6                 COMM ALLOCATION - ITEM                       
FTRNOALC DS    PL6                 OVERALLOCATION - ITEM                        
PREAALLO DS    PL6                 PRIOR BILLING AGENCY NET                     
PREACOMM DS    PL6                 PRIOR BILLING AGENCY COMMISSION              
PREFALLO DS    PL6                 PRIOR BILLINF FOREIGN NET                    
PREFCOMM DS    PL6                 PRIOR BILLING FOREIGN COMMISSION             
PREHRSB  DS    PL6                 PRIOR BILLING HOURS                          
PREWUHRS DS    PL6                 PRIOR BILLOING WRITE-UP HOURS                
PREWUAMT DS    PL6                 PRIOR BILLING WRITE-UP AMOUNT                
FTXTPAR  DS    CL36                PARAGRAPH DESCRIPTION                        
FCTRACC  DS    CL15                LAST CONTRA ACCOUNT CODE                     
FCTRNAM  DS    CL36                LAST CONTRA NAME                             
FTRNWGRP DS    CL1                 TRANSACTION WORKCODE TYPE                    
FTRNWTYP DS    CL1                 TRANSACTION WORKCODE TYPE                    
FALLSECT DS    XL1                 SECTION NUMBER FOR ALL CHARGES               
FTRNHTYP DS    XL1                 WORKCODE PARAGRAPH TYPE (TIME/COST)          
FTRNHTP2 DS    XL1                 WORKCODE PARAGRAPH TYPE (INT/EXT)            
FPREVB   DS    CL1                 Y=PREVIOUS BILLING REQUIRED                  
FHRSPARA DS    XL1                 KEY FOR HOURLY PARA                          
FOOPPARA DS    XL1                 KEY FOR OOP PARA                             
FJOBOVER DS    CL1                 Y=CHANGE OF JOB DETECTED                     
FHIPARA  DS    CL1                 HIGH PARAGRAPH NUMBER                        
FPARHDR  DS    CL(PGHLNQ)          PARAGRAPH HEADER ELEMENT                     
FPARCODE DS    CL1                 PARAGRAPH CODE FOR CURRENT XACTN             
FLASTPAR DS    CL(L'TLKAPARA)      LAST PARAGRAPH TSAR KEY                      
FLASTSUB DS    XL(L'TLKSRT)        LAST SUB-TOTAL KEY                           
FNPARDES DS    CL1                 Y=NO PARAGRAPH DESCRIPTION WANTED            
FANYALLC DS    CL1                 N=NO ALLOCATED TRANSACTIONS FOUND            
FFWTLINE DS    XL1                 LINE FOR FWTELS                              
*                                                                               
FTXTLINS EQU   9                   MAXIMUM NUMBER OF TEXT LINES                 
FTXTNDX  DS    CL1                 NUMBER OF CHOPPER OUTPUT LINES               
FTXTTRN  DS    (FTXTLINS+1)CL(REPWREGQ)                                         
*                                                                               
FSAVTNUM DS    XL(L'TLNUM)         CALLER'S TSAR RECORD NUMBER                  
FSAVTSAR DS    XL(TLAFMLNQ)                                                     
FTSAKEY  DS    XL(TLKSEQ-TLKEY)                                                 
FHITSAR  DS    XL(L'TLKSEQ)                                                     
FFWTPART DS    CL(5*FWTLNQ)                                                     
FFWTSUBT DS    CL(5*FWTLNQ)                                                     
*                                  TABLE OF PARAS ADDED OUT OF SEQ              
FTSAKEYS DS    (MAXSECTS)XL(MAXKEYIQ+1)                                         
MAXSECTS EQU   8                                                                
FTSAKTIM DS    XL(MAXKEYIQ+1)                                                   
FTSAKOOP DS    XL(MAXKEYIQ+1)                                                   
*                                                                               
FWCNTAB  DS    (FWCNMAX)CL(L'TRNKWORK+L'NAMEREC)                                
FWCNMAX  EQU   25                                                               
*                                                                               
FFMTBLK  DS    XL(L'FBLK)                                                       
*                                  TABLE OF PARAS ADDED OUT OF SEQ              
MAXPARAQ EQU   200                 MAXIMUM NUMBER OF PARAGRAPHS                 
MAXLINEQ EQU   200                 MAXIMUM LINES PER PARAGRAPH                  
MAXADDRS EQU   498*2               MAXIMUM D/AS IN AIO5-6                       
*                                                                               
DEFTAB   DS    0C                  SECTION DEFINITION DATA                      
WCTTAB   DS    XL2,5XL(L'FTRNWTYP+1)                                            
WCGTAB   DS    XL2,10XL(L'WGRKCODE+1)                                           
WCCTAB   DS    XL2,100XL(L'WCOKWRK+1)                                           
TRNTAB   DS    XL2,50XL(L'TRNTYPE+1)                                            
CACTAB   DS    XL2,20XL(L'CACKULA+1)                                            
CATTAB   DS    XL2,20XL(L'CATKSCH+L'CATKCODE+1)                                 
*                                                                               
SNAMTAB  DS    8CL(2*L'FBPBST)     SECTION HEADINGS AND SUBTOTAL NAMES          
SECTTABL EQU   *-DEFTAB                                                         
*                                                                               
PLTAB    DS    (PLMAXQ)XL10                                                     
PLCODE   DS    CL4                                                              
PLAMOUNT DS    PL6                                                              
PLMAXQ   EQU   20                                                               
PLTABX   DS    XL1                 EXTRA EOT BYTE                               
*                                                                               
MSTREC   DS    4000C                                                            
FWORKL   EQU   *-FWORKD                                                         
OSVALSD  DSECT                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'181ACCLB04B  12/21/99'                                      
         END                                                                    
