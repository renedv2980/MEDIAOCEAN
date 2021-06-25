*          DATA SET ACCLB5B    AT LEVEL 020 AS OF 08/16/00                      
*PHASE T6215BA                                                                  
CLB5B    TITLE '- PC COMMS - REVALUE CURRENCIES'                                
CLB5B    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB5B**,R8,RR=RE                                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         ST    RE,BORELO                                                        
         L     RC,ALINK                                                         
         USING LINKD,RC                                                         
*                                                                               
         CLI   LINKMODE,ROUTSN                                                  
         BNL   EXITY                                                            
         XR    RF,RF                                                            
         IC    RF,LINKMODE                                                      
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     SETMAP              SET A(MAP TABLE)                             
         B     RCVFST              FIRST FOR RECEIVE                            
         B     RCVHDRF             FIRST FOR MAP HEADER RECEIVE                 
         B     RCVDATA             DATA RECEIVE                                 
         B     RCVHDRL             LAST FOR MAP HEADER RECEIVE                  
         B     RCVLST              LAST FOR RECEIVE                             
         B     SND                 SEND                                         
ROUTSN   EQU   (*-ROUTS)/L'ROUTS                                                
         SPACE 1                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITN    LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITY    CR    RB,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* SET A(MAP TABLE)                                                    *         
***********************************************************************         
         SPACE 1                                                                
SETMAP   DS    0H                                                               
         LA    RF,MAPTAB           SET A(MAP TABLE)                             
         ST    RF,AMAPTAB                                                       
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR RECEIVE                                                   *         
***********************************************************************         
         SPACE 1                                                                
RCVFST   DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR MAP HEADER RECEIVE                                        *         
***********************************************************************         
         SPACE 1                                                                
RCVHDRF  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* ELEMENT DATA RECEIVE                                                *         
***********************************************************************         
         SPACE 1                                                                
RCVDATA  DS    0H                                                               
         ICM   RF,15,ORCVDATA                                                   
         BNZR  RF                                                               
         B     EXITY                                                            
                                                                                
RCVJOB   MVC   THISJOB,DATA                                                     
         B     EXITY                                                            
                                                                                
RCVDA    MVC   TRNDA,DATA                                                       
         B     EXITY                                                            
                                                                                
RCVACTN  MVI   RINDS,0                                                          
         XC    NUMTSAR,NUMTSAR                                                  
         ZAP   OSTOTCHG,BCPZERO                                                 
         CLI   DATA,ACTDRAQ                                                     
         BNE   *+8                                                              
         OI    RINDS,RIDRAFT                                                    
         CLI   DATA,ACTUPDQ                                                     
         BNE   *+8                                                              
         OI    RINDS,RILIVE                                                     
         B     EXITY                                                            
                                                                                
RCVCUR   MVC   THISCUR,DATA                                                     
         B     EXITY                                                            
                                                                                
RCVBREF  MVC   OSREF#,DATA                                                      
         B     EXITY                                                            
                                                                                
RCVBMON  BAS   RE,VALMON                                                        
         BE    EXITY                                                            
         B     EXITN                                                            
                                                                                
RCVXDAT  BAS   RE,VALDAT                                                        
         BE    UPDATE                                                           
         B     EXITN                                                            
                                                                                
RCVNAMNT MVC   NEWAMNT,DATA                                                     
         B     EXITY                                                            
                                                                                
RCVRATE  MVC   THISRATE,DATA                                                    
         CLI   RINDS,0             TEST DRAFT/UPDATE                            
         BNZ   BLDTSAR                                                          
                                                                                
         L     R5,ALSVALS          NO, JUST VALIDATING RATE                     
         USING LSVALSD,R5                                                       
         USING PRORATAD,LSPRATA                                                 
         USING TLSTD,LSTLST                                                     
                                                                                
         GOTO1 ASETUP,BODMCB,THISJOB,0,0                                        
                                                                                
         MVC   IODAOVER,TRNDA     GET TRANSACTION INTO IO1                      
         GOTO1 AIO,IOACCMST+IOGET+IO1 UPDATE ?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
VNRAT02  CLI   DATALEN+1,0         TEST NO INPUT                                
         BNE   VNRAT04                                                          
*        MVC   TLRNVAL,TLROVAL     YES - RESET TO OLD VALUES                    
*        SP    OSTOTCHG,TLRNAMT                                                 
*        MVC   TLRNAMT,TLROAMT                                                  
*        AP    OSTOTCHG,TLRNAMT                                                 
         B     VALNRATY                                                         
*                                                                               
VNRAT04  GOTO1 GETCUR,BODMCB,THISCUR CHECK EXCH RATE IN VALID RANGE             
         L     R2,0(R1)                                                         
         USING CCYTABD,R2                                                       
         CLC   THISRATE,CCYTMIN                                                 
         BL    *+14                                                             
         CLC   THISRATE,CCYTMAX                                                 
         BNH   VNRAT06                                                          
         MVC   FVMSGNO,=AL2(AE$EXRNV)                                           
         GOTO1 AEDTRAT,BOPARM,(L'FVXTRA,FVXTRA),CCYTMIN,CCYTMAX                 
         B     EXITN                                                            
         DROP  R2                                                               
*                                                                               
VNRAT06  DS    0H                                                               
*        ZAP   BODUB1,TLRCAMT      CALCULATE NEW AMOUNT                         
*        MVC   BOWORK1(L'TLRNVAL),TLRNVAL                                       
*        XI    BOWORK1+(TLRNIND-TLRNVAL),X'01'                                  
*        ICM   RE,8,BOWORK1+(TLRNSHF-TLRNVAL)                                   
*        SRA   RE,32-8                                                          
*        LCR   RE,RE                                                            
*        STC   RE,BOWORK1+(TLRNSHF-TLRNVAL)                                     
*        EXCHP BODUB1,BOWORK1,DUB=BODUB2,WRK=BOWORK2                            
*        SP    OSTOTCHG,TLRNAMT                                                 
*        ZAP   TLRNAMT,BODUB2                                                   
*        AP    OSTOTCHG,TLRNAMT                                                 
*                                                                               
VALNRATY B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* LAST FOR MAP HEADER RECEIVE                                         *         
***********************************************************************         
         SPACE 1                                                                
RCVHDRL  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* LAST FOR RECEIVE                                                    *         
***********************************************************************         
         SPACE 1                                                                
RCVLST   DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* SEND                                                                *         
***********************************************************************         
         SPACE 1                                                                
SND      DS    0H                                                               
         ICM   RF,15,OSND                                                       
         BNZR  RF                                                               
         B     EXITY                                                            
         SPACE 1                                                                
SNDRVCUR DS    0H                                                               
         CLI   RINDS,0             TEST DOING DRAFT/UPDATE                      
         BE    SNDRVC00                                                         
         GOTO1 ASNDHDR,BODMCB,MH#RVC       SEND SPOOLED REPORT ID               
         GOTO1 ASNDDATA,BODMCB,16,REPID                                         
         B     EXITY                                                            
                                                                                
SNDRVC00 OC    TRNDA,TRNDA         TEST IF DONE AN AMENDMENT                    
         BNZ   EXITY                                                            
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         USING TRNRECD,IOKEY                                                    
                                                                                
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   TRNKACT,THISJOB                                                  
                                                                                
         LA    R1,IOHIGH+IOACCDIR                                               
         B     *+8                                                              
GETNEXT  LA    R1,IOSEQ+IOACCDIR                                                
         GOTO1 AIO                                                              
         BNE   EXITN                                                            
         CLC   TRNKEY(TRNKWORK-TRNKEY),IOKEYSAV                                 
         BNE   EXITY               FINSHED WITH THIS JOB                        
         CLC   TRNKWORK,REVUL                                                   
         BNL   EXITY               FINISHED                                     
         OC    TRNKDATE,TRNKDATE   ENSURE HAVE TRANSACTION RECORD               
         BZ    GETNEXT                                                          
         CLC   TRNKDATE,BCSPACES                                                
         BE    GETNEXT                                                          
*                                                                               
         TM    TRNKSTAT,TRNSREVS+TRNSDRFT                                       
         BNZ   GETNEXT             EXCLUDE REVERSALS AND DRAFTS                 
         GOTO1 ASETTRN,BOPARM,(C'D',TRNRECD)                                    
         BNE   GETNEXT                                                          
*                                                                               
         MVC   IODAOVER,TRNKDA                                                  
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         USING TRNELD,TRNRFST                                                   
*                                                                               
         LA    R3,TRNELD           TEST FOREIGN CURRENCY ON RECORD              
         USING AFCELD,R3                                                        
         XR    RF,RF                                                            
GNEXT02  CLI   AFCEL,0             TEST EOR                                     
         BE    GETNEXT                                                          
         CLI   AFCEL,AFCELQ                                                     
         BE    *+12                                                             
         IC    RF,AFCLN                                                         
         BXH   R3,RF,GNEXT02                                                    
*                                                                               
         GOTO1 ASETTRN,BOPARM,(C'M',TRNRECD)                                    
         BNE   GETNEXT                                                          
*                                                                               
         MVI   TLRINDS,0           TEST ANYTHING PENDING                        
         TM    TLXPEND,TLXPALL+TLXPWOF+TLXPXFR+TLXPRCV+TLXPREV                  
         BNZ   GETNEXT             ITEM CAN'T BE REVAUED                        
                                                                                
         GOTO1 ASNDHDR,BODMCB,MH#RVC                                            
         GOTO1 ASNDDATA,BODMCB,1,IOKEY+(TRNKDA-TRNRECD)                         
         GOTO1 (RF),(R1),2,TRNKWORK                                             
         GOTO1 (RF),(R1),3,(L'TRNKULC,TRNKULC)                                  
         GOTO1 (RF),(R1),4,TRNKDATE                                             
         GOTO1 (RF),(R1),5,TRNKREF                                              
         GOTO1 (RF),(R1),6,AFCCURR                                              
         GOTO1 (RF),(R1),7,AFCXRATE                                             
         GOTO1 (RF),(R1),8,AFCAMNT                                              
         GOTO1 (RF),(R1),9,TRNAMNT                                              
         GOTO1 GETCUR,BOPARM,AFCCURR                                            
         L     RF,0(R1)                                                         
         LA    RF,CCYTTAB-CCYTABD(RF)                                           
         GOTO1 ASNDDATA,BODMCB,10,CURTDECP-CURTABD(RF)                          
*&&UK                                                                           
         LA    R4,BOELEM                                                        
         USING EURKBLKD,R4                                                      
         XC    0(EURKBLKL,R4),0(R4)                                             
         MVC   EURKCUFR,CSCPYCUR                                                
         MVC   EURKCUTO,AFCCURR                                                 
         MVC   EURKALPH,CUAALF                                                  
         MVC   EURKACT,BCCLICOD                                                 
         MVC   EURKDATE,BCTODAYC                                                
         MVC   EURKAFAC,ACOM                                                    
         MVI   EURKTYPE,ACCQ                                                    
         TM    BCCPYST6,CPYSFTXR                                                
         BZ    *+8                                                              
         OI    EURKTYPE,ALLOWFTQ+SWAPQ                                          
         GOTO1 VEUREKA,BCPARM,('GETQ',EURKBLKD)                                 
         CLI   0(R1),0                                                          
         BNE   GNEXT04                                                          
         GOTO1 ASNDDATA,BODMCB,11,AFCXRATE-AFCX+EURKRULE                        
         DROP  R3,R4                                                            
*&&                                                                             
GNEXT04  GOTO1 AIO,IOREAD+IOACCDIR  BLDCUR DOES IO                              
         B     GETNEXT                                                          
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET CURRENCY CODE INFO                                   *         
*                                                                     *         
* NTRY: P1=A(CURRENCY CODE)                                           *         
* EXIT: P1=A(CCYTABD TABLE ENTRY)                                     *         
*       CC=NOT EQUAL IF VBLDCUR CALLED                                *         
***********************************************************************         
         SPACE 1                                                                
GETCUR   NTR1  ,                                                                
         LR    R3,R1               R3=A(PARAMETER LIST)                         
         L     R2,0(R3)            R2=A(CURRENCY CODE)                          
*                                                                               
         LA    R4,CCYTAB           SEARCH TABLE FOR ENTRY                       
         USING CCYTABD,R4                                                       
         LA    R0,CCYTABL                                                       
         LA    R1,CCYTABX-1                                                     
CTAB02   CLI   CCYTABD,EOT                                                      
         BE    CTAB04                                                           
         CLC   CCYTCOD,0(R2)                                                    
         BNE   *+12                                                             
         ST    R4,0(R3)                                                         
         B     EXITY                                                            
         BXLE  R4,R0,CTAB02                                                     
         SR    R4,R0               USE LAST ENTRY IF TABLE FULL                 
*                                                                               
CTAB04   MVC   CCYTCOD,0(R2)       ADD NEW ENTRY                                
         GOTO1 AGETCUR,BOPARM,(X'A0',CCYTCOD),CCYTTAB,,CCYTMIN,CCYTMAX          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   CCYTABD+CCYTABL,EOT                                              
         ST    R4,0(R3)                                                         
         B     EXITN                                                            
         DROP  R4                                                               
***********************************************************************         
BLDTSAR  DS    0H                                                               
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         MVC   IODAOVER,TRNDA                                                   
         GOTO1 AIO,IOACCMST+IOGET+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
         USING TRNRECD,R2                                                       
         USING TRNELD,TRNRFST                                                   
                                                                                
         LA    R3,TRNELD           TEST FOREIGN CURRENCY ON RECORD              
         USING AFCELD,R3                                                        
         XR    RF,RF                                                            
BLDTS02  CLI   AFCEL,0             TEST EOR                                     
         BNE   *+6                                                              
         DC    H'0'                NO AFCEL                                     
         CLI   AFCEL,AFCELQ                                                     
         BE    *+12                                                             
         IC    RF,AFCLN                                                         
         BXH   R3,RF,BLDTS02                                                    
*                                  SET UP TSAR KEY                              
         XC    TLST1,TLST1                                                      
         XC    TLST2,TLST2                                                      
         XC    TLST3,TLST3                                                      
         LA    R1,TLRVLLNQ                                                      
         STCM  R1,3,TLRLEN                                                      
         OI    TLKSES,TLKSTEMP                                                  
         MVC   TLRECACT,CSRECACT                                                
         MVC   TLKWC,TRNKWORK                                                   
         MVC   TLKULC,TRNKCULC                                                  
         MVC   TLKDATE,TRNKDATE                                                 
         MVC   TLKREF,TRNKREF                                                   
         MVC   TLKSBR,TRNKSBR                                                   
*                                  FILL IN RECORD VALUES                        
         MVC   TLDA,TRNDA                                                       
         MVC   TLRWC,TRNKWORK                                                   
         MVC   TLRSUP,TRNKCULC                                                  
         MVC   TLRREF#,TRNKREF                                                  
         MVC   TLRDATE,TRNKDATE                                                 
                                                                                
         MVC   TLRCUR,AFCCURR                                                   
         GOTO1 GETCUR,BOPARM,AFCCURR                                            
         L     RF,0(R1)                                                         
         LA    RF,CCYTTAB-CCYTABD(RF)                                           
         MVC   TLRCTAB,0(RF)                                                    
         MVC   TLRCAMT,AFCAMNT                                                  
                                                                                
         MVC   TLROVAL,AFCX        OLD RATE VALUES                              
         MVC   TLRNVAL,AFCX        NEW RATE VALUES SAME AS OLD                  
         MVC   TLRNEXC,THISRATE    EXCEPT RATE                                  
         MVC   TLROAMT,TRNAMNT     OLD AMOUNT IS TRNAMNT                        
         MVC   TLRNAMT,NEWAMNT     NEW AMOUNT SENT                              
                                                                                
         AP    OSTOTCHG,TLRNAMT    ADJUST TOTAL EXCH DIFF                       
         SP    OSTOTCHG,TLROAMT                                                 
                                                                                
         XR    RE,RE               BUMP NUMBER OF TSARS ADDED                   
         ICM   RE,3,NUMTSAR                                                     
         LA    RE,1(RE)                                                         
         STCM  RE,3,NUMTSAR                                                     
                                                                                
         GOTO1 ATSARIO,TSAADD                                                   
         BE    EXITY                                                            
         DC    H'0'                                                             
                                                                                
UPDATE   DS    0H                                                               
         USING POSTVALS,RPOSTVAL                                                
         L     R6,AREP                                                          
         USING REPD,R6                                                          
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
                                                                                
         TM    RINDS,RILIVE                                                     
         BZ    PRTN01                                                           
         GOTO1 AADDOBH,BOPARM,('POSTRVAL',OSREF#),BOBYTE1 NON ZERO=VAL          
         BNE   EXITN               TEST BATCH HEADER IS UNIQUE                  
*                                                                               
PRTN01   LA    R2,IOKEY            READ CASH LEDGER                             
         USING LDGRECD,R2                                                       
         MVC   LDGKEY,BCSPACES                                                  
         MVC   LDGKCPY,CUABIN                                                   
         MVC   LDGKUNT(L'CASHUL),CASHUL                                         
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO1                                                          
         LA    R1,LDGRFST-LDGRECD(R1)                                           
         USING SPAELD,R1           FIND EXCHANGE/DIFF SPAEL                     
         XR    RF,RF                                                            
PRTN02   CLI   SPAEL,0                                                          
         BE    PRTN04                                                           
         CLI   SPAEL,SPAELQ                                                     
         BNE   *+12                                                             
         CLI   SPATYPE,SPATEXDF                                                 
         BE    PRTN10                                                           
         IC    RF,SPALN                                                         
         BXH   R1,RF,PRTN02                                                     
*                                                                               
PRTN04   MVC   FVXTRA(L'CASHUL),CASHUL                                          
         MVC   FVMSGNO,=AL2(AE$EDACL)                                           
         B     PFKRTNN                                                          
*                                                                               
PRTN10   MVC   REXDCPY,CUABIN      SAVE EXCHANGE DIFFERENCE ACCOUNT             
         MVC   REXDULA,SPAAULA                                                  
         MVC   LDGKEY(LDGKEND),REXDCULA                                         
         GOTO1 AGETLDG                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   R1,15,ACALDG        DEAL WITH OFFPOS                             
         USING LDGTABD,R1                                                       
         CLI   LDGTOFFP,LDGONONE                                                
         BE    PRTN20                                                           
         CLI   LDGTOFFP,LDGOKEY                                                 
         BNH   PRTN12                                                           
         CLI   LDGTOFFP,LDGONKLO                                                
         BL    PRTN20                                                           
         CLI   LDGTOFFP,LDGONKHI                                                
         BH    PRTN20                                                           
PRTN12   IC    RF,LDGTOFFP                                                      
         SLL   RF,32-4                                                          
         SRL   RF,32-4                                                          
         LA    RF,REXDACT-1(RF)                                                 
         MVC   0(1,RF),CSOFFICE                                                 
         TM    LDGTOFFP,LDGOKEY2                                                
         BZ    PRTN20                                                           
         MVC   1(1,RF),CSOFFICE+1                                               
         DROP  R1,R2                                                            
*                                                                               
PRTN20   LA    R2,IOKEY            GET EXCHANGE DIFFERENCE ACCOUNT              
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCULA,REXDCULA                                                
         GOTO1 AGETACT,0                                                        
         BE    PRTN22                                                           
         MVC   FVXTRA(L'REXDULA),REXDULA                                        
         MVC   FVMSGNO,=AL2(AE$EDANF)                                           
         B     EXITN                                                            
PRTN22   MVC   REXDNAM,ACNAME                                                   
         CLC   REXDUL,EXPUL        TEST EXPENSE UNIT LEDGER, NOT INCOME         
         BNE   *+8                                                              
         OI    RINDS,RIEXP                                                      
*                                                                               
         CLI   ACCOST,C' '         TEST COSTING POSTINGS REQUIRED               
         BNH   PRTN30                                                           
         OI    RINDS,RICOST                                                     
         MVC   RCSTCPY,CUABIN      GET INCOME/EXPENSE COSTING ACCOUNT           
         MVC   RCSTUL,INCCSTUL                                                  
         TM    RINDS,RIEXP                                                      
         BZ    *+10                                                             
         MVC   RCSTUL,EXPCSTUL                                                  
         MVC   RCSTACT,BCSPACES                                                 
         MVC   RCSTACT(L'ACCOST),ACCOST                                         
         MVC   ACTKCULA,RCSTCULA                                                
         GOTO1 AGETACT,0                                                        
         BNE   EXITN                                                            
         MVC   RCSTNAM,ACNAME                                                   
*                                                                               
         MVC   RCLICULA,BCCMPPRF+(PPRCOST-PPRELD)                               
         MVC   ACTKCULA,RCLICULA   GET CLIENT COSTING ACCOUNT                   
         GOTO1 AGETACT,0                                                        
         BNE   EXITN                                                            
         MVC   RCLINAM,ACNAME                                                   
*                                                                               
         TM    RINDS,RIEXP         NEED PERSONNEL ACCOUNT IF EXPENSE            
         BZ    PRTN30                                                           
         MVC   RPSNCPY,CUABIN                                                   
         MVC   RPSNUL,PSNCSTUL                                                  
         MVC   RPSNACT,BCSPACES                                                 
         MVC   RPSNACT(L'C99),C99                                               
         MVC   RPSNACT+L'C99(L'ACCOST),RCSTACT                                  
         MVC   ACTKCULA,RPSNCULA                                                
         GOTO1 AGETACT,0                                                        
         BNE   EXITN                                                            
         MVC   RPSNNAM,ACNAME                                                   
         DROP  R2                                                               
*                                                                               
PRTN30   BAS   RE,PRTOUT           PRINT REVALUATION REPORT                     
         BNE   PFKRTNN                                                          
         BAS   RE,POST             MAKE DRAFT/LIVE POSTINGS                     
         BNE   EXITN                                                            
*                                  SAVE SPOOLED REPORT ID                       
         MVC   REPID(L'REPSUBID),REPSUBID                                       
         MVC   REPID+L'REPSUBID(1),BCCOMMA                                      
         LA    RF,REPID+L'REPSUBID+1                                            
         EDIT (2,REPREPNO),(5,(RF)),ALIGN=LEFT,DUB=BODUB1,WRK=BOWORK1           
                                                                                
*                                                                               
*        CLI   P#DQU,C'Y'          TEST ALWAYS OUTPUT DQU                       
*        BE    PRTN34                                                           
*        TM    RINDS,RILIVE                                                     
*        BZ    PRTN32                                                           
*        CLI   P#DQU,C'L'          TEST ONLY FOR LIVE                           
*        BE    PRTN34                                                           
*        B     PFKRTNY                                                          
*RTN32   CLI   P#DQU,C'D'          TEST ONLY FOR DRAFT                          
*        BNE   PFKRTNY                                                          
*RTN34   MVC   BASSRV,DQU                                                       
*        OI    BASSRVH+FHOID,FHOITR                                             
*                                                                               
PFKRTNY  B     EXITY                                                            
*                                                                               
PFKRTNN  B     EXITN                                                            
*FKRTNN  L     R1,AINP                                                          
*        USING TIOBD,R1                                                         
*        MVC   TIOBCURS,CSCURDSP                                                
*        XC    TIOBCURD,TIOBCURD                                                
*        OI    TIOBINDS,TIOBSETC                                                
*        B     EXITN                                                            
*        DROP  R1                                                               
         EJECT                                                                  
*          DATA SET ACCLB10B   AT LEVEL 060 AS OF 18/03/99                      
***********************************************************************         
* PRINT OUT REPORT                                                    *         
***********************************************************************         
         SPACE 1                                                                
PRTOUT   NTR1  ,                                                                
         USING PLINE,P1                                                         
*                                                                               
*        MVC   TLNUM,CSPSRECN                                                   
         XC    TLNUM,TLNUM                                                      
POUT02   ICM   RE,3,TLNUM                                                       
         LA    RE,1(RE)                                                         
         STCM  RE,3,TLNUM                                                       
*        CLC   TLNUM,CSHIRECN                                                   
         CLC   TLNUM,NUMTSAR                                                    
         BH    POUT10                                                           
         GOTO1 ATSARIO,TSAGET                                                   
*        CLC   TLRNAMT,TLROAMT     ONLY CHANGES ARE UPLOADED                    
*        BE    POUT02                                                           
         TM    REPIND1,REPINIT                                                  
         BO    POUT04                                                           
         BAS   RE,PRTINIT                                                       
         MVC   REPCUR,TLRCUR                                                    
         MVC   REPCTAB,TLRCTAB                                                  
         ZAP   RAMTCUR,BCPZERO                                                  
         ZAP   RAMTOLD,BCPZERO                                                  
         ZAP   RAMTNEW,BCPZERO                                                  
         B     POUT06                                                           
*                                                                               
POUT04   CLC   REPCUR,TLRCUR                                                    
         BE    POUT06                                                           
         OI    RINDS,RIDIFCUR                                                   
*                                                                               
POUT06   AP    RAMTOLD,TLROAMT                                                  
         AP    RAMTNEW,TLRNAMT                                                  
         TM    RINDS,RIDIFCUR                                                   
         BO    *+10                                                             
         AP    RAMTCUR,TLRCAMT                                                  
*                                                                               
         MVC   PWC,TLRWC                                                        
*                                                                               
         MVC   BOWORK1,BCSPACES                                                 
         MVC   BOWORK1(L'TLRSUP-1),TLRSUP+1                                     
         LA    RF,BOWORK1+L'TLRSUP-1                                            
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         GOTO1 GETNAM,BOPARM,TLRSUP,2(RF)                                       
         GOTO1 VCHOPPER,(R1),(L'BOWORK1,BOWORK1),(L'PSUP,PSUP),(C'P',3)         
*                                                                               
         MVC   PREF#,TLRREF#                                                    
         MVC   PCURR(L'TLRCUR),TLRCUR                                           
         GOTO1 VDATCON,BOPARM,(1,TLRDATE),(17,PDATE)                            
         GOTO1 FMTAMT,(R1),TLRCAMT,PCAMT,TLRCTAB                                
         GOTO1 (RF),(R1),TLROAMT,POAMT,CSCURCPY                                 
         GOTO1 (RF),(R1),TLRNAMT,PNAMT,                                         
         ZAP   BOWORK1(6),TLRNAMT                                               
         SP    BOWORK1(6),TLROAMT                                               
         GOTO1 (RF),(R1),BOWORK1,PEXD,                                          
         GOTO1 FMTRAT,(R1),TLROEXC,PORAT                                        
         GOTO1 (RF),(R1),TLRNEXC,PNRAT                                          
         OI    RINDS,RIPRTSPC                                                   
         BAS   RE,PRINT                                                         
         B     POUT02                                                           
*                                                                               
POUT10   TM    REPIND1,REPINIT                                                  
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$YHDAY)                                           
         B     EXITN                                                            
         MVC   P1,BOXMID                                                        
         NI    RINDS,FF-RIPRTSPC                                                
         LA    R3,P3                                                            
         USING PLINE,R3                                                         
         MVCDD PREF#(L'PREF#+L'PDATE+1),AC#JBTOT                                
         GOTO1 VDICTAT,BOPARM,C'SL  ',PREF#                                     
         GOTO1 FMTAMT,(R1),RAMTOLD,POAMT,CSCURCPY                               
         GOTO1 (RF),(R1),RAMTNEW,PNAMT,                                         
         GOTO1 (RF),(R1),OSTOTCHG,PEXD,                                         
         TM    RINDS,RIDIFCUR                                                   
         BO    POUT12                                                           
         MVC   PCURR(L'REPCUR),REPCUR                                           
         GOTO1 (RF),(R1),RAMTCUR,PCAMT,REPCTAB                                  
         DROP  R3                                                               
POUT12   MVC   P4,BOXBOT                                                        
         BAS   RE,PRINT                                                         
*                                                                               
PRTOUTX  B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* PRINT LINE                                                          *         
***********************************************************************         
         SPACE 1                                                                
PRINT    NTR1  ,                                                                
         LA    R1,P5               SET R1 TO LAST PRINT LINE                    
         LA    RF,P1                                                            
         LA    RE,L'P1                                                          
         LNR   RE,RE                                                            
         CLC   0(L'P1,R1),BCSPACES                                              
         BNE   *+8                                                              
         BXH   R1,RE,*-10                                                       
         TM    RINDS,RIPRTSPC                                                   
         BZ     *+8                                                             
         LA    R1,L'REPP1(R1)                                                   
*                                                                               
         IC    R2,REPLINE          R2=NO. OF PRINT LINES COUNT                  
         BCTR  RF,0                                                             
PRINT02  CLI   0(R1),LEFTT                                                      
         BE    PRINT08                                                          
         CLI   0(R1),BOTL                                                       
         BE    PRINT08                                                          
         NC    0(L'P1,R1),BOXMSK                                                
         OC    0(L'P1,R1),BOXPRT                                                
PRINT08  LA    R2,1(R2)                                                         
         BXH   R1,RE,PRINT02                                                    
*                                                                               
         TM    REPIND1,REPIPUT                                                  
         BZ    PRINT10                                                          
         LA    R2,3(R2)                                                         
         CLM   R2,1,REPMAXL                                                     
         BL    PRINT10                                                          
         MVC   REPP1,BOXBOT                                                     
         GOTO1 VREPORT,REPD                                                     
         MVC   REPLINE,REPMAXL                                                  
*                                                                               
PRINT10  LA    RE,REPP1                                                         
         LA    RF,PS                                                            
         LA    R0,P1                                                            
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         GOTO1 VREPORT,REPD                                                     
         LA    RE,P1                                                            
         LA    RF,PS                                                            
         XR    R1,R1                                                            
         ICM   R1,8,BCSPACES                                                    
         MVCL  RE,R0                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* MAKE POSTINGS                                                       *         
***********************************************************************         
         SPACE 1                                                                
POST     NTR1  ,                                                                
         MVC   REPLINE,REPMAXL                                                  
         MVC   REPM1,BCSPACES                                                   
         MVC   REPM2,BCSPACES                                                   
         MVC   REPM3,BCSPACES                                                   
         MVC   REPM4,BCSPACES                                                   
*                                                                               
         MVI   POSTTYPE,POSTRVAL                                                
         MVI   POSTMODE,POSTDFQ                                                 
         MVI   REPSUBPG,REPSDPST                                                
         TM    RINDS,RILIVE                                                     
         BZ    POST10                                                           
         GOTO1 AADDOBH,BOPARM,('POSTRVAL',OSREF#),0                             
         MVI   POSTMODE,POSTLVQ                                                 
         MVI   REPSUBPG,REPSLPST                                                
         GOTO1 AINIADT                                                          
*                                                                               
*OST10   MVC   TLNUM,CSPSRECN                                                   
POST10   XC    TLNUM,TLNUM                                                      
POST12   ICM   RE,3,TLNUM                                                       
         LA    RE,1(RE)                                                         
         STCM  RE,3,TLNUM                                                       
*        CLC   TLNUM,CSHIRECN                                                   
         CLC   TLNUM,NUMTSAR                                                    
         BH    POST30                                                           
         GOTO1 ATSARIO,TSAGET                                                   
*        CLC   TLRNAMT,TLROAMT     ONLY CHANGES UPLOADED                        
*        BE    POST12                                                           
*                                                                               
         MVC   IODAOVER,TLDA                                                    
         GOTO1 AIO,IOACCMST+IOGET+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
         USING TRNRECD,R2                                                       
         USING TRNELD,TRNRFST                                                   
*                                                                               
         BAS   RE,BLDXTRA          BUILD IDENTICAL -DEBIT TRANSACTION           
         ICM   R1,15,POSTXTRA                                                   
         XR    RF,RF                                                            
POST14   CLI   0(R1),0                                                          
         BE    POST20                                                           
         USING SCIELD,R1                                                        
         CLI   SCIEL,SCIELQ                                                     
         BNE   POST16                                                           
         MP    SCIAMNT,PMINUS1                                                  
         CLI   SCILN,SCILN1Q                                                    
         BNH   *+10                                                             
         MP    SCINET,PMINUS1                                                   
         B     POST18                                                           
*                                                                               
         USING AFCELD,R1                                                        
POST16   CLI   AFCEL,AFCELQ                                                     
         BNE   POST18                                                           
         MP    AFCAMNT,PMINUS1                                                  
*                                                                               
POST18   IC    RF,1(R1)                                                         
         BXH   R1,RF,POST14                                                     
         DROP  R1                                                               
*                                                                               
POST20   MVC   POSTACT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   POSTACT+L'BCCPYPRD(L'BCJOBCOD),BCJOBCOD                          
         MVC   POSTACTN,BCJOBNAM                                                
         MVC   POSTOFFC,TRNKOFF                                                 
         MVC   POSTBTMC,OSMONC                                                  
         MVC   POSTBTRF,OSREF#                                                  
         MVC   POSTDATE,TRNKDATE                                                
         MVC   POSTREF,TRNKREF                                                  
         MVC   POSTAMNT,TLROAMT                                                 
         MP    POSTAMNT,PMINUS1                                                 
         MVC   POSTSTAT,TRNSTAT                                                 
         MVC   POSTCAC,TRNKCULC                                                 
         GOTO1 GETNAM,BOPARM,TRNKCULC,POSTCACN                                  
         XR    RE,RE                                                            
         IC    RE,TRNLN                                                         
         SH    RE,=Y(TRNLN1Q+1)                                                 
         BM    *+14                                                             
         EX    RE,*+4                                                           
         MVC   POSTNARR(0),TRNNARR                                              
         GOTO1 ABLDTRN,BOPARM,POSTVALS                                          
*                                                                               
         BAS   RE,BLDXTRA                                                       
         L     R1,POSTXTRA         PUT NEW EXCHANGE RATE IN                     
         USING AFCELD,R1             FOREIGN CURRENCY ELEMENT                   
         XR    RF,RF                                                            
         CLI   AFCEL,AFCELQ                                                     
         BE    *+12                                                             
         IC    RF,AFCLN                                                         
         BXH   R1,RF,*-12                                                       
         MVC   AFCX,TLRNVAL                                                     
         DROP  R1                                                               
         MVC   POSTAMNT,TLRNAMT                                                 
         GOTO1 ABLDTRN,BOPARM,POSTVALS                                          
*                                                                               
         TM    RINDS,RILIVE                                                     
         BZ    POST22                                                           
         L     RE,AIO3             SAVE D/A OF NEW TRANSACTION RECORD           
         MVC   IOKEY,0(RE)                                                      
         GOTO1 AIO,IOREAD+IOACCDIR                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TLDA,IOKEY+(TRNKDA-TRNRECD)                                      
*                                                                               
POST22   LA    RF,BOELEM                                                        
         XC    BOELEM,BOELEM                                                    
         ST    RF,POSTXTRA                                                      
         USING SORELD,RF                                                        
         USING FFTELD,SORELD+SORALNQ                                            
         MVI   SOREL,SORELQ        SAVE JOB CODE ON SOURCE ELEMENT              
         MVI   SORLN,SORALNQ                                                    
         MVI   SORSYS,SORSACC                                                   
         MVC   SORAULA,POSTACT                                                  
         MVI   FFTEL,FFTELQ        SAVE CURRENCY CODE ON TEXT ELEMENT           
         MVI   FFTLN,FFTLN1Q+L'FFTDLEN+L'TLRCUR                                 
         MVI   FFTTYPE,FFTTACUR                                                 
         MVI   FFTDLEN,L'TLRCUR                                                 
         MVC   FFTDATA(L'TLRCUR),TLRCUR                                         
         DROP  RF                                                               
*                                                                               
         MVC   POSTACT,REXDULA                                                  
         MVC   POSTACTN,REXDNAM                                                 
         MVC   POSTOFFC,CSOFFICE                                                
         MVC   POSTBTRF,OSREF#                                                  
         MVC   POSTBTMC,OSMONC                                                  
         MVC   POSTDATE,OSDATP                                                  
         MVC   POSTAMNT,TLRNAMT                                                 
         SP    POSTAMNT,TLROAMT                                                 
         MVI   POSTSTAT,0                                                       
         TM    RINDS,RIEXP         TEST EXHANGE DIFF. A/C ON EXPENSE            
         BZ    *+14                                                             
         OI    POSTSTAT,TRNSDR                                                  
         MP    POSTAMNT,PMINUS1                                                 
         MVC   POSTCAC(L'ACTKCPY),CUABIN                                        
         MVC   POSTCAC+(ACTKUNT-ACTKEY)(L'BCCPYPRD),BCCPYPRD                    
         MVC   POSTCAC+(ACTKACT-ACTKEY)(L'BCPROCOD),BCPROCOD                    
         MVC   POSTCACN,BCPRONAM                                                
         XC    POSTNARR,POSTNARR                                                
         GOTO1 ABLDTRN,BOPARM,POSTVALS                                          
*                                                                               
         TM    RINDS,RICOST        TEST COSTING POSTINGS REQUIRED               
         BZ    POST24                                                           
         XC    POSTXTRA,POSTXTRA                                                
         ZAP   POSTAMNT,TLROAMT                                                 
         SP    POSTAMNT,TLRNAMT                                                 
*                                                                               
         MVC   POSTACT,RCLIULA                                                  
         MVC   POSTACTN,RCLINAM                                                 
         MVC   POSTCAC,RCSTCULA                                                 
         MVC   POSTCACN,RCSTNAM                                                 
         MVI   POSTSTAT,0                                                       
         TM    RINDS,RIEXP         TEST INCOME                                  
         BO    *+14                                                             
         OI    POSTSTAT,TRNSDR     IS A CREDIT                                  
         MP    POSTAMNT,PMINUS1    WITH THE REVERSE SIGN                        
         GOTO1 ABLDTRN,BOPARM,POSTVALS                                          
         LA    RF,RCSTCULA                                                      
         TM    RINDS,RIEXP                                                      
         BZ    *+8                                                              
         LA    RF,RPSNCULA         USE PERSONNEL IF EXPENSE                     
         MVC   POSTACT,RCSTULA-RCSTCULA(RF)                                     
         MVC   POSTACTN,RCSTNAM-RCSTCULA(RF)                                    
         MVC   POSTCAC,RCLICULA                                                 
         MVC   POSTCACN,RCLINAM                                                 
         XI    POSTSTAT,TRNSDR     REVERSE DEBIT/CREDIT                         
         GOTO1 ABLDTRN,BOPARM,POSTVALS                                          
*                                                                               
POST24   TM    RINDS,RILIVE                                                     
*        BZ    POST28                                                           
*        MVC   TLROVAL,TLRNVAL     OLD RATE = NEW RATE                          
*        MVC   TLROAMT,TLRNAMT     OLD AMOUNT = NEW AMOUNT                      
*        GOTO1 ATSARIO,TSAPUT      TSAR RECORD NOW FOR NEW POSTING              
*                                                                               
POST28   B     POST12                                                           
*                                                                               
POST30   GOTO1 ABLDTRN,BOPARM,(X'FF',POSTVALS)                                  
         MVI   REPACTN,REPACLO     CLOSE REPORT                                 
         GOTO1 VREPORT,REPD                                                     
         L     RF,ATIA             CLEAR TIA FOR NEXT GETOPT CALL               
         XC    0(256,RF),0(RF)                                                  
*        TM    RINDS,RILIVE                                                     
*        BZ    EXITY                                                            
*        NI    OSINDS,FF-OSIDAY                                                 
*        ZAP   OSTOTCHG,BCPZERO                                                 
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* BUILD EXTRA ELEMENT LIST                                            *         
***********************************************************************         
         SPACE 1                                                                
BLDXTRA  NTR1  ,                                                                
         LA    R0,TRNRECD          COPY RECORD INTO IO1                         
         XR    R1,R1                                                            
         ICM   R1,3,TRNRLEN                                                     
         L     RE,AIO1                                                          
         LA    RF,TRNRFST-TRNRECD(RE)                                           
         ST    RF,POSTXTRA         SAVE A(COPIED 1ST ELEMENT)                   
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('TRNELQ',AIO1),0                    
         MVI   4(R1),TRSELQ        DELETE TRNEL/TRSEL                           
         BASR  RE,RF                                                            
*                                                                               
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE PRINTING                                                 *         
***********************************************************************         
         SPACE 1                                                                
PRTINIT  NTR1  ,                                                                
         LA    RE,P1                                                            
         LA    RF,PS                                                            
         XR    R1,R1                                                            
         ICM   R1,8,BCSPACES                                                    
         MVCL  RE,R0                                                            
         MVC   BOXTOP,BOXCOLS                                                   
         TR    BOXTOP,CONVTOP                                                   
         MVC   BOXMID,BOXCOLS                                                   
         TR    BOXMID,CONVMID                                                   
         MVC   BOXBOT,BOXCOLS                                                   
         TR    BOXBOT,CONVBOT                                                   
         MVC   BOXMSK,BOXCOLS                                                   
         TR    BOXMSK,CONVMSK                                                   
         MVC   BOXPRT,BOXCOLS                                                   
         TR    BOXPRT,CONVPRT                                                   
*                                                                               
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         LA    RE,=C'ARD'                                                       
         TM    RINDS,RILIVE                                                     
         BNO   *+8                                                              
         LA    RE,=C'ARV'                                                       
         XC    REPSUBID,REPSUBID                                                
         OC    REPSUBID,CSREPID                                                 
         BNZ   *+10                                                             
         MVC   REPSUBID,0(RE)                                                   
         MVCDD REPDESC,AC#DRVRP                                                 
         MVI   REPSUBPG,REPSDREP                                                
         TM    RINDS,RILIVE                                                     
         BNO   *+14                                                             
         MVCDD REPDESC,AC#LRVRP                                                 
         MVI   REPSUBPG,REPSLREP                                                
         GOTO1 VDICTAT,BOPARM,C'SL  ',REPDESC                                   
*                                                                               
         MVI   REPACTN,REPAINI                                                  
         GOTO1 VREPORT,REPD                                                     
         MVI   REPACTN,REPAOPN     OPEN THE REPORT                              
         GOTO1 (RF),(R1)                                                        
         CLI   REPERRS,0           TEST FOR OPEN ERRORS                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   REPACTN,REPAPUT                                                  
*                                                                               
         LA    R3,REPH3                                                         
         USING HLINE,R3                                                         
         MVC   HDATA1(L'CUAALF),CUAALF                                          
         L     RF,AGOPBLK                                                       
         L     RF,GOACOMP-GOBLOCKD(RF)                                          
         GOTO1 AGETELS,BOPARM,ACCORFST(RF)                                      
         MVC   HNAME1,ACNAME                                                    
         MVC   HDATA2(L'BCUSERID),BCUSERID                                      
*                                                                               
         LA    R3,REPH4                                                         
         MVC   HDATA1(L'CSOFFICE),CSOFFICE                                      
         LA    R2,IOKEY                                                         
         USING OGRRECD,R2                                                       
         XC    OGRKEY,OGRKEY                                                    
         MVI   OGRKTYP,OGRKTYPQ                                                 
         MVI   OGRKSUB,OGRKOFFQ                                                 
         MVC   OGRKCPY,CUABIN                                                   
         MVC   OGRKUNT(L'OGRKUNT+L'OGRKLDG),BCCPYPRD                            
         MVC   OGRKOFC,CSOFFICE                                                 
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         L     R2,AIO1                                                          
         GOTO1 AGETELS,BOPARM,OGRRFST                                           
         MVC   HNAME1,ACNAME                                                    
         DROP  R2                                                               
         MVC   HDATA2(L'CSBPID),CSBPID                                          
*                                                                               
         LA    R3,REPH5                                                         
         MVC   HDATA1,BCCLICOD                                                  
         MVC   HNAME1,BCCLINAM                                                  
*                                                                               
         LA    R3,REPH6                                                         
         XR    RE,RE                                                            
         IC    RE,BCCLILEN                                                      
         LA    RF,L'BCPROCOD-1                                                  
         SR    RF,RE                                                            
         LA    RE,BCPROCOD(RE)                                                  
         EX    RF,*+4                                                           
         MVC   HDATA1(0),0(RE)                                                  
         MVC   HNAME1,BCPRONAM                                                  
*                                                                               
         LA    R3,REPH7                                                         
         XR    RE,RE                                                            
         IC    RE,BCPROLEN                                                      
         LA    RF,L'BCJOBCOD-1                                                  
         SR    RF,RE                                                            
         LA    RE,BCJOBCOD(RE)                                                  
         EX    RF,*+4                                                           
         MVC   HDATA1(0),0(RE)                                                  
         MVC   HNAME1,BCJOBNAM                                                  
         DROP  R3                                                               
*                                                                               
         MVC   REPM1,BOXTOP                                                     
         MVC   REPM2,MID2                                                       
         NC    REPM2,BOXMSK                                                     
         OC    REPM2,BOXPRT                                                     
         GOTO1 VDICTAT,BOPARM,C'TL  ',(L'REPM2,REPM2)                           
         MVC   REPM3,MID3                                                       
         NC    REPM3,BOXMSK                                                     
         OC    REPM3,BOXPRT                                                     
         GOTO1 (RF),(R1),,(L'REPM3,REPM3)                                       
         GOTO1 CENCUR,(R1),(L'POAMT-1,REPM3+(POAMT-PLINE))                      
         GOTO1 (RF),(R1),(L'PNAMT-1,REPM3+(PNAMT-PLINE))                        
         GOTO1 (RF),(R1),(L'PEXD-1,REPM3+(PEXD-PLINE))                          
         MVC   REPM4,BOXMID                                                     
PRTINITX B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CENTRE CURRENCY IN HEADLINE                              *         
*                                                                     *         
* NTRY: P1=(L'HEADLINE, A(HEADLINE))                                  *         
***********************************************************************         
         SPACE 1                                                                
CENCUR   NTR1 ,                                                                 
         XR    RF,RF                                                            
         IC    RF,0(R1)            RF=L'HEADLINE                                
         XR    R4,R4                                                            
         ICM   R4,7,1(R1)          R4=A(HEADLINE)                               
*                                                                               
         BCTR  RF,0                                                             
         LA    RE,0(RF,R4)                                                      
         CLI   0(R4),C' '          ENSURE R4=A(FIRST CHAR)                      
         BH    *+12                                                             
         LA    R4,1(R4)                                                         
         B     *-12                                                             
CCUR100  CLI   0(RE),C' '                                                       
         BH    CCUR200                                                          
         BCTR  RE,0                                                             
         BCT   RF,CCUR100                                                       
CCUR200  LR    RF,R4                                                            
         SR    RE,R4                                                            
         SH    RE,=Y(L'CSCPYCUR+1)                                              
         BNP   CCUR300                                                          
         SRDL  RE,32                                                            
         D     RE,=F'2'                                                         
         AR    RF,R4                                                            
         AR    RF,RE                                                            
CCUR300  MVI   0(RF),C'('                                                       
         MVC   1(L'CSCPYCUR,RF),CSCPYCUR                                        
         MVI   L'CSCPYCUR+1(RF),C')'                                            
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT CURRECNCY AMOUNT                                  *         
*                                                                     *         
* NTRY: P1=A(P6 AMOUNT)                                               *         
*       P2=A(14 BYTE OUTPUT FIELD)                                    *         
*       P3=A(CURRENCY TABLE ENTRY)                                    *         
***********************************************************************         
         SPACE 1                                                                
FMTAMT   NTR1  ,                                                                
         LM    R2,R4,0(R1)                                                      
         CURED (P6,(R2)),(14,(R3)),(R4),MINUS=YES,ZERO=NOBLANK,        *        
               DMCB=BODMCB                                                      
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO FORMAT RATE                                              *         
*                                                                     *         
* NTRY: P1=A(PL5 RATE)                                                *         
*       P2=A(11 BYTE OUTPUT)                                          *         
***********************************************************************         
         SPACE 1                                                                
FMTRAT   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         ZAP   BODUB1,BCPZERO                                                   
         MVO   BODUB1,0(5,R2)                                                   
         CURED (P8,BODUB1),(11,(R3)),5,DMCB=BODMCB                              
         LA    R3,10(R3)                                                        
         LA    R0,4                                                             
FMTRAT02 CLI   0(R3),C'0'                                                       
         BNE   FMTRATX                                                          
         MVI   0(R3),C' '                                                       
         BCTR  R3,0                                                             
         BCT   R0,FMTRAT02                                                      
FMTRATX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET ACCOUNT NAME                                         *         
*                                                                     *         
* NTRY: P1=A(ACCOUNT CODE)                                            *         
*       P2=A(ACCOUNT NAME)                                            *         
***********************************************************************         
         SPACE 1                                                                
GETNAM   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
*                                                                               
         LA    R4,NAMTAB                                                        
         USING NAMTABD,R4                                                       
         LA    R0,NAMTABL                                                       
         LA    R1,NAMTABX-1                                                     
GNAM02   CLI   NAMTABD,EOT                                                      
         BE    GNAM04                                                           
         CLC   NAMTACC,0(R2)                                                    
         BE    GETNAMX                                                          
         BXLE  R4,R0,GNAM02                                                     
         SR    R4,R0               USE LAST ENTRY IF TABLE FULL                 
*                                                                               
GNAM04   MVC   NAMTACC,0(R2)                                                    
         MVC   NAMTNAM,BCSPACES                                                 
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(L'NAMTACC),NAMTACC                                         
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BNE   EXITN                                                            
         L     R1,AIO1                                                          
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         USING NAMELD,R1                                                        
         XR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         CLI   NAMEL,NAMELQ                                                     
         BE    *+8                                                              
         BXH   R1,RF,*-12                                                       
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+4                                                           
         MVC   NAMTNAM(0),NAMEREC                                               
         DROP  R1                                                               
GNAM06   MVI   NAMTABD+NAMTABL,EOT                                              
*                                                                               
GETNAMX  MVC   0(L'NAMTNAM,R3),NAMTNAM                                          
         B     EXITY                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE DATE FIELD                                             *         
***********************************************************************         
         SPACE                                                                  
VALDAT   NTR1  ,                                                                
         GOTO1 VPERVAL,BOPARM,(DATALEN+1,DATA),(X'40',BOELEM)                   
         CLI   4(R1),0             TEST VALID DATE                              
         BE    VDAT04                                                           
         CLI   4(R1),4                                                          
         BE    VDAT04                                                           
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         B     EXITN                                                            
*                                                                               
VDAT04   LA    R2,BOELEM                                                        
         USING PERVALD,R2                                                       
         MVC   OSDATC,PVALCSTA                                                  
         MVC   OSDATP,PVALPSTA                                                  
         MVC   BOWORK1+00(6),PVALESTA                                           
         GOTO1 VDATCON,BOPARM,(2,BCTODAYC),(0,BOWORK1+6)                        
         XR    RF,RF                                                            
         ICM   RF,1,BCP204         NUMBER OF DAYS BACKWARDS                     
         BNZ   *+8                                                              
         LA    RF,60               DEFAULT IS 60 DAYS                           
         MVC   BOWORK1+4(2),=C'28' SET DAY TO 28                                
         LA    RF,3(RF)            AND ALLOW 3 MORE DAYS                        
         GOTO1 VADDAY,BOPARM,BOWORK1,BOWORK1+12,(RF)                            
         CLC   BOWORK1+6(6),BOWORK1+12                                          
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$DOPSP)                                           
         B     EXITN                                                            
         MVC   BOWORK1+4(2),=C'01' SET DAY TO 1                                 
         GOTO1 VADDAY,BOPARM,BOWORK1,BOWORK1+12,-60                             
         CLC   BOWORK1+6(6),BOWORK1+12                                          
         BNL   EXITY                                                            
         MVC   FVMSGNO,=AL2(AE$DOPSP)                                           
         B     EXITN                                                            
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE THE MONTH FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
VALMON   NTR1  ,                                                                
         MVC   CSBSECL,BCCPYEL+(CPYBSEC-CPYELD)                                 
         LA    R3,BOWORK1                                                       
         USING BMONVALD,R3                                                      
         GOTO1 VBMONVAL,BOPARM,(DATALEN+1,DATA),(79,ACOM),             *        
               (CULANG,BMONVALD),(CUABIN,0)                                     
         MVC   BOBYTE1,0(R1)       BATCH SECURITY LEVEL                         
         CLI   BMOERR,BMOEOKQ      TEST ANY ERROR                               
         BE    *+14                                                             
         MVC   FVMSGNO,BMOMSG                                                   
         B     EXITN                                                            
*                                                                               
         MVC   OSMONP,BMOMOSP                                                   
         MVC   OSMONC,BMOMOSC                                                   
         MVC   CSBSECL,BOBYTE1     SET BATCH SECURITY                           
*                                                                               
         B     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* MAP TABLE                                                           *         
***********************************************************************         
         SPACE 1                                                                
MAPTAB   DS    0X                                                               
*                                  ** REVALUE CURRENCIES **                     
RCEL     DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(MH#RVC)         ELEMENT CODE                                 
         DC    AL2(RCELX+1-RCEL)   DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(SNDRVCUR-CLB5B) SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(255)            MAPPING CODE                                 
         DC    CL5'JOB  '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKACT)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVJOB-CLB5B)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'DA'             TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKDA)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVDA-CLB5B)    RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'WC'             TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKWORK)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE                                 
         DC    CL5'CONTR'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(04)             MAPPING CODE                                 
         DC    CL5'DATE'           TEXT IDENTIFIER                              
         DC    AL1(MDTDTQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKDATE)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(05)             MAPPING CODE                                 
         DC    CL5'REF'            TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKREF)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(06)             MAPPING CODE                                 
         DC    CL5'CUR'            TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'AFCCURR)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVCUR-CLB5B)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(07)             MAPPING CODE                                 
         DC    CL5'EXCHR'          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'AFCXRATE)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVRATE-CLB5B)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(08)             MAPPING CODE                                 
         DC    CL5'FCAMT'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'AFCAMNT)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVNAMNT-CLB5B) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(09)             MAPPING CODE                                 
         DC    CL5'AGAMT'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNAMNT)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(10)             MAPPING CODE                                 
         DC    CL5'CURDP'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'CURTDECP)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(11)             MAPPING CODE                                 
         DC    CL5'FTRAT'          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'AFCXRATE)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(12)             MAPPING CODE                                 
         DC    CL5'BREF'           TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(4)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVBREF-CLB5B)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(13)             MAPPING CODE                                 
         DC    CL5'BMON'           TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(6)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVBMON-CLB5B)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(14)             MAPPING CODE                                 
         DC    CL5'ACTN'           TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVACTN-CLB5B)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(15)             MAPPING CODE                                 
         DC    CL5'XDAT'           TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(12)             DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVXDAT-CLB5B)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(16)             MAPPING CODE                                 
         DC    CL5'REPID'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(7)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
RCELX    DC    XL1'00'             END OF ELEMENT FIELDS                        
*                                                                               
MAPTABX  DC    X'00'               E-O-T                                        
*                                                                               
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         LTORG                                                                  
ACCMST   DC    C'ACCMST '                                                       
REVUL    DC    C'99'                                                            
C99      DC    C'99'                                                            
INCUL    DC    C'SI'               INCOME UNIT/LEDGER                           
EXPUL    DC    C'SE'               EXPENSE UNIT/LEDGER                          
CASHUL   DC    C'SC'               CASH UNTIL/LEDGER                            
INCCSTUL DC    C'12'               INCOME COSTING UNIT/LEDGER                   
EXPCSTUL DC    C'13'               EXPENSE COSTING UNIT/LEDGER                  
PSNCSTUL DC    C'1P'               PERSONNEL COSTING UNIT/LEDGER                
FF       EQU   X'FF'                                                            
PMINUS1  DC    P'-1'                                                            
*                                  * REPSUBPG EQUATES *                         
REPSDREP EQU   01                  DRAFT REPORT                                 
REPSLREP EQU   02                  LIVE REPORT                                  
REPSDPST EQU   03                  DRAFT POSTINGS                               
REPSLPST EQU   04                  LIVE POSTINGS                                
*                                                                               
REPSPEC  DS    0X                                                               
         SPROG 1,2,3,4                                                          
         SPEC  H1,002,RUN                                                       
         SPEC  H1,123,PAGE                                                      
         SPEC  H3,002,AC#CPY,8,L                                                
         SPEC  H3,111,AC#USRID,10,L                                             
         SPEC  H4,002,AC#OFF,8,L                                                
         SPEC  H4,111,AC#REQR,10,L                                              
         SPEC  H5,002,AC#CLINT,8,L                                              
         SPEC  H6,002,AC#PRO,8,L                                                
         SPEC  H7,002,AC#JOB,8,L                                                
         SPROG 1                                                                
         SPEC  H1,046,AC#DRVRP,40,C                                             
         SPEC  H2,046,AC#DRVRP,40,CU                                            
         SPROG 2                                                                
         SPEC  H1,046,AC#LRVRP,40,C                                             
         SPEC  H2,046,AC#LRVRP,40,CU                                            
         SPROG 3                                                                
         SPEC  H1,046,AC#DPOS,40,C                                              
         SPEC  H2,046,AC#DPOS,40,CU                                             
         SPROG 4                                                                
         SPEC  H1,046,AC#APOS,40,C                                              
         SPEC  H2,046,AC#APOS,40,CU                                             
         SPEC  END                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* REPORT MIDLINES                                                     *         
***********************************************************************         
         SPACE 1                                                                
BOXCORNL EQU   X'01'               LEFT HAND CORNER                             
BOXCORNR EQU   X'02'               RIGHT HAND CORNER                            
BOXINTER EQU   X'03'               INTERSECTION                                 
BOXLINE  EQU   X'04'               LINE                                         
*                                                                               
BOXCOLS  DC    XL(L'REPP1)'00'                                                  
         ORG   BOXCOLS                                                          
         DC    AL1(BOXCORNL),(PCURR-PWC-1)AL1(BOXLINE)                          
         DC    AL1(BOXINTER),(L'PCURR)AL1(BOXLINE)                              
         DC    AL1(BOXINTER),(L'PCAMT)AL1(BOXLINE)                              
         DC    AL1(BOXINTER),(L'PORAT)AL1(BOXLINE)                              
         DC    AL1(BOXINTER),(L'POAMT)AL1(BOXLINE)                              
         DC    AL1(BOXINTER),(L'PNRAT)AL1(BOXLINE)                              
         DC    AL1(BOXINTER),(L'PNAMT)AL1(BOXLINE)                              
         DC    AL1(BOXINTER),(L'PEXD)AL1(BOXLINE)                               
         DC    AL1(BOXCORNR)                                                    
         ORG   BOXCOLS+L'BOXCOLS                                                
*                                                                               
CONVTOP  DC    AL1(0,TOPL,TOPR,TOPT,TOPFLAT)                                    
CONVMID  DC    AL1(0,LEFTT,RIGHTT,CROSS,TOPFLAT)                                
CONVBOT  DC    AL1(0,BOTL,BOTR,BOTT,BOTF)                                       
CONVMSK  DC    AL1(0,0,0,0,FF)                                                  
CONVPRT  DC    AL1(0,VERT,VERT,VERT,0)                                          
*                                                                               
MID2     DC    CL(L'REPP1)' '                                                   
         ORG   MID2+(PWC-PLINE)                                                 
         DCDDL AC#WC,2                                                          
         ORG   MID2+(PSUP-PLINE)                                                
         DCDDL AC#SUPCN,L'PSUP                                                  
         ORG   MID2+(PREF#-PLINE)                                               
         DCDDL AC#REFN,L'PREF#                                                  
         ORG   MID2+(PDATE-PLINE)                                               
         DCDDL AC#DATE,L'PDATE                                                  
         ORG   MID2+(PCURR-PLINE)                                               
         DCDDL AC#CURRY,L'PCURR                                                 
         ORG   MID2+(PCAMT-PLINE)                                               
         DCDDL AC#CURRY,L'PCAMT-1,R                                             
         ORG   MID2+(PORAT-PLINE)                                               
         DCDDL AC#OLDRT,L'PORAT,C                                               
         ORG   MID2+(POAMT-PLINE)                                               
         DCDDL AC#OLDAM,L'POAMT-1,R                                             
         ORG   MID2+(PNRAT-PLINE)                                               
         DCDDL AC#NEWRT,L'PNRAT,C                                               
         ORG   MID2+(PNAMT-PLINE)                                               
         DCDDL AC#NEWAM,L'PNAMT-1,R                                             
         ORG   MID2+(PEXD-PLINE)                                                
         DCDDL AC#EXDIF,L'PEXD-1,R                                              
         ORG   MID2+L'MID2                                                      
*                                                                               
MID3     DC    CL(L'REPP1)' '                                                   
         ORG   MID3+(PWC-PLINE)                                                 
         DC    AL1(AC#ESUL2),AL2(AC#WC)                                         
         ORG   MID3+(PSUP-PLINE)                                                
         DCDDL AC#SUPCN,L'PSUP,LU                                               
         ORG   MID3+(PREF#-PLINE)                                               
         DCDDL AC#REFN,L'PREF#,LU                                               
         ORG   MID3+(PDATE-PLINE)                                               
         DCDDL AC#DATE,L'PDATE,LU                                               
         ORG   MID3+(PCURR-PLINE)                                               
         DCDDL AC#CURRY,L'PCURR,LU                                              
         ORG   MID3+(PCAMT-PLINE)                                               
         DCDDL AC#AMT,L'PCAMT-1,R                                               
         ORG   MID3+(PORAT-PLINE)                                               
         DCDDL AC#OLDRT,L'PORAT,CU                                              
         ORG   MID3+(POAMT-PLINE)                                               
         DCDDL AC#OLDAM,L'POAMT-1,RU                                            
         ORG   MID3+(PNRAT-PLINE)                                               
         DCDDL AC#NEWRT,L'PNRAT,CU                                              
         ORG   MID3+(PNAMT-PLINE)                                               
         DCDDL AC#NEWAM,L'PNAMT-1,RU                                            
         ORG   MID3+(PEXD-PLINE)                                                
         DCDDL AC#EXDIF,L'PEXD-1,RU                                             
         ORG   MID3+L'MID3                                                      
         EJECT                                                                  
**********************************************************************          
* LOCAL DSECTS                                                       *          
**********************************************************************          
*                                                                               
CCYTABD  DSECT                     ** CURRENCY TABLE **                         
CCYTCOD  DS    CL3                 CODE                                         
CCYTTAB  DS    XL(CURTABL)         TABLE ENTRY                                  
CCYTMIN  DS    PL5                 MINIMUM EXCHANGE RATE                        
CCYTMAX  DS    PL5                 MAXIMUM EXCHANGE RATE                        
CCYTABL  EQU   *-CCYTABD                                                        
         SPACE 1                                                                
NAMTABD  DSECT                     ** ACCOUNT CODE/NAME TABLE **                
NAMTACC  DS    XL15                ACCOUNT CODE                                 
NAMTNAM  DS    CL36                NAME                                         
NAMTABL  EQU   *-NAMTABD                                                        
         SPACE 1                                                                
HLINE    DSECT                     ** HEAD LINE **                              
         ORG   HLINE+01                                                         
HWORD1   DS    CL08,CL1                                                         
HDATA1   DS    CL08,CL1                                                         
HNAME1   DS    CL36                                                             
         ORG   HLINE+111                                                        
HWORD2   DS    CL10,CL1                                                         
HDATA2   DS    CL08                                                             
         SPACE                                                                  
PLINE    DSECT                     ** PRINT LINE **                             
         DS    CL01                                                             
PWC      DS    CL02,CL1            WORK CODE                                    
PSUP     DS    CL20,CL1            SUPPLIER CODE AND NAME                       
PREF#    DS    CL06,CL1            REFERENCE NUMBER                             
PDATE    DS    CL08,CL1            DATE                                         
PCURR    DS    CL04,CL1            CURRENCY                                     
PCAMT    DS    CL14,CL1            CURRENCY AMOUNT                              
PORAT    DS    CL11,CL1            OLD RATE                                     
POAMT    DS    CL14,CL1            OLD AMOUNT                                   
PNRAT    DS    CL11,CL1            NEW RATE                                     
PNAMT    DS    CL14,CL1            NEW AMOUNT                                   
PEXD     DS    CL14,CL1            EXCHANGE DIFFERENCE                          
PLINEX   DS    0C                                                               
         ORG   PLINE+L'REPP1                                                    
         SPACE 1                                                                
* DDBOXEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
***********************************************************************         
* SAVED WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   OSVALS                                                           
OSINDS   DS    XL1                 INDICATOR BYTE                               
OSIDAY   EQU   X'80'               DONE ANYTHING YET                            
OSIMAT   EQU   X'40'               CAME HERE FROM MATCH INVOICE SCREEN          
*                                                                               
OSTOTCHG DS    PL6                 TOTAL AMOUNT CHANGED                         
*                                                                               
OSDATC   DS    XL2                                                              
OSDATP   DS    PL3                                                              
OSREF#   DS    CL4                                                              
OSMONP   DS    PL2                                                              
OSMONC   DS    CL2                                                              
         DS    XL(OSVALSL-(*-OSVALS))                                           
         EJECT                                                                  
* ACCLBLINK                                                                     
       ++INCLUDE ACCLBLINK                                                      
         SPACE 1                                                                
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
LINKD    DSECT                                                                  
         ORG   OVRWS                                                            
THISJOB  DS    XL(L'TRNKACT)                                                    
THISCUR  DS    XL(L'AFCCURR)                                                    
TRNDA    DS    XL(L'TRNKDA)        TRANSACTION DA                               
THISRATE DS    XL(L'AFCXRATE)                                                   
ACTDRAQ  EQU   9                                                                
ACTUPDQ  EQU   10                                                               
REPID    DS    CL7                                                              
NUMTSAR  DS    XL2                 NUMBER OF TSAR RECORDS ADDED                 
OLDAMNT  DS    PL6                                                              
NEWAMNT  DS    PL6                                                              
*                                                                               
BOXTOP   DS    CL132               TOP LINE OF BOX                              
BOXMID   DS    CL132               MID LINE OF BOX                              
BOXBOT   DS    CL132               BOTTOM LINE OF BOX                           
BOXMSK   DS    CL132               MASK FOR PRINT LINE                          
BOXPRT   DS    CL132               BOX VALUES FOR PRINT LINE                    
*                                                                               
P1       DS    CL132               PRINT LINE 1                                 
P2       DS    CL132               PRINT LINE 2                                 
P3       DS    CL132               PRINT LINE 3                                 
P4       DS    CL132               PRINT LINE 4                                 
P5       DS    CL132               PRINT LINE 5                                 
PS       EQU   *-P1                                                             
*                                                                               
RINDS    DS    XL1                 INDICATOR BYTE                               
RIDIFCUR EQU   X'80'               DIFFERENT CURRENCIES IN REPORT               
RIPRTSPC EQU   X'40'               EXTRA SPACE AFTER PRINTING                   
RIDRAFT  EQU   X'20'               DRAFT TRANSACTIONS REQUIRED                  
RILIVE   EQU   X'10'               LIVE TRANSACTIONS REQUIRED                   
RICOST   EQU   X'08'               COSTING POSTINGS REQUIRED                    
RIEXP    EQU   X'04'               EXCHANGE DIFFERENCE A/C ON EXPENSE           
REPCUR   DS    CL3                 CURRENCY USED ON REPORT                      
REPCTAB  DS    XL(L'CSCURCPY)      CURRENCY TABLE ENTRY ON REPORT               
*                                                                               
REXDCULA DS    0XL15               EXCHANGE DIFFERENCE ACCOUNT                  
REXDCPY  DS    XL1                                                              
REXDULA  DS    0XL14                                                            
REXDUL   DS    CL2                                                              
REXDACT  DS    CL12                                                             
REXDNAM  DS    CL36                EXCHANGE DIFFERENCE ACCOUNT NAME             
*                                                                               
RCSTCULA DS    0XL15               INCOME/EXPENSE COSTING ACCOUNT               
RCSTCPY  DS    XL1                                                              
RCSTULA  DS    0XL14                                                            
RCSTUL   DS    CL2                                                              
RCSTACT  DS    CL12                                                             
RCSTNAM  DS    CL36                INCOME/EXPENSE COSTING ACCOUNT NAME          
*                                                                               
RCLICULA DS    0XL15               CLIENT COSTING ACCOUNT                       
RCLICPY  DS    XL1                                                              
RCLIULA  DS    0XL14                                                            
RCLIUL   DS    CL2                                                              
RCLIACT  DS    CL12                                                             
RCLINAM  DS    CL36                CLIENT COSTING ACCOUNT NAME                  
*                                                                               
RPSNCULA DS    0XL15               PERSONNEL COSTING ACCOUNT                    
RPSNCPY  DS    XL1                                                              
RPSNULA  DS    0XL14                                                            
RPSNUL   DS    CL2                                                              
RPSNACT  DS    CL12                                                             
RPSNNAM  DS    CL36                PERSONNEL COSTING ACCOUNT NAME               
*                                                                               
RAMTCUR  DS    PL6                 TOTAL CURRENCY AMOUNT                        
RAMTOLD  DS    PL6                 TOTAL OLD AMOUNT                             
RAMTNEW  DS    PL6                 TOTAL NEW AMOUNT                             
*                                                                               
         DS    0A                                                               
RPOSTVAL DS    XL(POSTVALL)                                                     
                                                                                
CCYTAB   DS    5XL(CCYTABL)                                                     
CCYTABN  EQU   (*-CCYTAB)/CCYTABL                                               
CCYTABX  DS    XL1                                                              
*                                                                               
NAMTAB   DS    10XL(NAMTABL)                                                    
NAMTABN  EQU   (*-NAMTAB)/NAMTABL                                               
NAMTABX  DS    XL1                                                              
*                                                                               
         DS    (L'OVRWS-(*-OVRWS))X                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020ACCLB5B   08/16/00'                                      
         END                                                                    
