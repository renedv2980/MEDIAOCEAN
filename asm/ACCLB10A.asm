*          DATA SET ACCLB10A   AT LEVEL 060 AS OF 12/23/99                      
*PHASE T62110A                                                                  
CLB10    TITLE '- BILL PROGRAM - REVALUE LIST'                                  
CLB10    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB10**,R8,R7,RR=RE                                           
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     RC,AOVERWRK                                                      
         USING RWORKD,RC                                                        
         USING POSTVALS,RPOSTVAL                                                
         L     R6,AREP                                                          
         USING REPD,R6                                                          
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
M        USING TLSTD,BCLSTMAT                                                   
         USING TRNRECD,IOKEY                                                    
         ST    RE,BORELO                                                        
         SRL   RF,24                                                            
         SLL   RF,2                                                             
*                                                                               
         B     *+4(RF)                                                          
         B     LSTFRST             FIRST FOR THIS LIST                          
         B     SCRFRST             FIRST FOR THIS SCREEN                        
         B     SCRLAST             LAST FOR THIS SCREEN                         
         B     EXITY               FIRST FOR VALIDATE THIS LINE                 
         B     VALCLM              VALIDATE COLUMN                              
         B     EXITY               LAST FOR VALIDATE THIS LINE                  
         B     DISCLM              DISPLAY COLUMN                               
         B     GETFRST             GET FIRST RECORD FOR LIST                    
         B     GETNEXT             GET NEXT RECORD                              
         B     SETHEAD             SET UP MY OWN HEADING                        
         B     VALSEL              VALIDATE LIST SELECTION                      
         B     EXITY               DISPLAY SUB-TOTAL                            
         B     DISSCR              DISPLAY SCREEN TOTAL                         
         B     PFKRTN              PFKEY ROUTINE                                
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITY    CR    RB,RB               SET CC EQUAL                                 
*                                                                               
EXIT     XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* FIRST FOR SCREEN                                                    *         
***********************************************************************         
         SPACE 1                                                                
SCRFRST  TM    BCINDS2,BCINTRS     TEST FIRST TIME                              
         BZ    SFRST02                                                          
         MVC   BASJOBC,BCJOBCOD                                                 
         MVC   BASJOBN,BCJOBNAM                                                 
         MVI   OSINDS,0            TEST CAME FROM MATCH 2 INVIOCE LIST          
         XR    RE,RE                                                            
         IC    RE,TWASESNL                                                      
         SLL   RE,1                                                             
         LA    RE,TWASESRA-L'TWASESRA(RE)                                       
         CLI   CSACT-CSRECACT(RE),ACTMT2                                        
         BNE   SFRST02                                                          
         OI    OSINDS,OSIMAT       YES - SET CURRENCY= OPTION                   
         LH    RF,=Y(UC@CURRY-TWAD)                                             
         LA    RF,TWAD(RF)                                                      
         MVC   BASOPT(L'UC@CURRY),0(RF)                                         
         LA    RF,BASOPT+L'UC@CURRY-1                                           
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   1(L'BCEQUAL,RF),BCEQUAL                                          
         MVC   1+L'BCEQUAL(L'TLMCUR,RF),M.TLMCUR                                
*                                                                               
SFRST02  CLI   BCPFKEY,PFKRDFTQ    TEST DRAFT PFKEY                             
         BNE   *+8                                                              
         OI    RINDS,RIDRAFT                                                    
         CLI   BCPFKEY,PFKRLIVQ    TEST LIVE PFKEY                              
         BNE   *+8                                                              
         OI    RINDS,RILIVE                                                     
*                                                                               
         BAS   RE,VALTRN           VALIDATE INPUT FIELDS                        
         BNE   EXITL                                                            
*                                                                               
         MVC   RLSOPS,LSOPS        EVALUATE FILTERS/OPTIONS                     
         XC    LSOPS(LSOPL),LSOPS                                               
*                                                                               
         L     RE,=A(VALOPT)                                                    
         A     RE,BORELO                                                        
         ST    RE,AOVERVAL                                                      
         GOTO1 AFVAL,BASOPTH                                                    
         MVC   AOVEROUT,ALSVALS                                                 
*                                                                               
         XC    BOWORK2,BOWORK2                                                  
         L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         OC    BOWORK2(L'GOCBDRVL),GOCBDRVL-GOBBLOCK(RF)                        
         BNZ   *+10                                                             
         MVC   BOWORK2(DEFCLML),DEFCLM                                          
         GOTO1 AVALOPT,BOPARM,OPTTAB,BOWORK2,0                                  
         BNE   SCRFRSTN                                                         
*                                                                               
         CLC   RLSOPS,LSOPS        SET CC=HIGH IF FILTERS HAVE CHANGED          
         BNE   EXITH                                                            
         B     EXITY                                                            
*                                                                               
SCRFRSTN MVC   LSOPS(LSOPL),RLSOPS RESTORE PREVIOUS FILTERS                     
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXITL                                                            
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL                                                            
         SPACE 1                                                                
***********************************************************************         
* LAST FOR SCREEN                                                     *         
***********************************************************************         
         SPACE 1                                                                
SCRLAST  MVCDD FHDAD(28,R1),AC#TCHA,R                                           
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY SCREEN TOTALS (TOTAL AMOUNT CHANGED)                        *         
***********************************************************************         
         SPACE 1                                                                
DISSCR   GOTO1 FMTAMT,BOPARM,OSTOTCHG,FVIFLD,CSCURCPY                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PFKEY ROUTINE                                                       *         
***********************************************************************         
         SPACE 1                                                                
PFKRTN   TM    OSINDS,OSIDAY       TEST ANYTHING DONE YET                       
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$YHDAY)                                           
         B     PFKRTNN                                                          
*                                                                               
         TM    RINDS,RILIVE                                                     
         BZ    PRTN01                                                           
         GOTO1 AADDOBH,BOPARM,('POSTRVAL',OSREF#),BASREFH                       
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
         BNE   EXITL                                                            
*                                                                               
         MVI   FVOMTYP,GTMINF      REPORT SPOOLED MESSAGE                       
         MVC   FVMSGNO,=AL2(AI$RPSPL)                                           
         MVI   FVPARMS,9                                                        
         MVC   FVPARMS+1(L'REPSUBID),REPSUBID                                   
         MVC   FVPARMS+1+L'REPSUBID(1),BCCOMMA                                  
         LA    RF,FVPARMS+1+L'REPSUBID+1                                        
         EDIT (2,REPREPNO),(5,(RF)),ALIGN=LEFT,DUB=BODUB1,WRK=BOWORK1           
*                                                                               
         CLI   P#DQU,C'Y'          TEST ALWAYS OUTPUT DQU                       
         BE    PRTN34                                                           
         TM    RINDS,RILIVE                                                     
         BZ    PRTN32                                                           
         CLI   P#DQU,C'L'          TEST ONLY FOR LIVE                           
         BE    PRTN34                                                           
         B     PFKRTNY                                                          
PRTN32   CLI   P#DQU,C'D'          TEST ONLY FOR DRAFT                          
         BNE   PFKRTNY                                                          
PRTN34   MVC   BASSRV,DQU                                                       
         OI    BASSRVH+FHOID,FHOITR                                             
*                                                                               
PFKRTNY  B     EXITY                                                            
*                                                                               
PFKRTNN  L     R1,AINP                                                          
         USING TIOBD,R1                                                         
         MVC   TIOBCURS,CSCURDSP                                                
         XC    TIOBCURD,TIOBCURD                                                
         OI    TIOBINDS,TIOBSETC                                                
         B     EXITL                                                            
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE COLUMN                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALCLM   OI    OSINDS,OSIDAY                                                    
         SLL   R1,2                                                             
         B     *+4(R1)                                                          
         DC    2H'0'               WORKCODE                                     
         DC    2H'0'               SUPPLIER/CONTRA                              
         DC    2H'0'               TRANSACTION REFERENCE                        
         DC    2H'0'               CURRENCY CODE                                
         DC    2H'0'               CURRENCY AMOUNT                              
         B     VALNRAT             NEW RATE                                     
         DC    2H'0'               NEW AMOUNT                                   
         DC    2H'0'               OLD RATE                                     
         DC    2H'0'               OLD AMOUNT                                   
         DC    2H'0'               EXCHANGE DIFFERENCE                          
         SPACE 1                                                                
VALNRAT  ICM   RF,15,LSAMIFEL      TEST MULTI-INPUT FIELD                       
         BZ    VNRAT02                                                          
         USING MIFELD,RF                                                        
         CLC   TLNUM,MIF#LO        TEST FIRST FOR MULTI-INPUT                   
         BNE   VNRAT01                                                          
         MVI   MIFQUALN,L'TLRCUR   YES - SAVE CURRENCY                          
         MVC   MIFQUA(L'TLRCUR),TLRCUR                                          
         B     VNRAT02                                                          
VNRAT01  CLC   MIFQUA(L'TLRCUR),TLRCUR    NO - COMPARE CURRENCY                 
         BNE   EXITN                                                            
         DROP  RF                                                               
*                                                                               
VNRAT02  CLI   FVILEN,0            TEST NO INPUT                                
         BNE   VNRAT04                                                          
         MVC   TLRNVAL,TLROVAL     YES - RESET TO OLD VALUES                    
         SP    OSTOTCHG,TLRNAMT                                                 
         MVC   TLRNAMT,TLROAMT                                                  
         AP    OSTOTCHG,TLRNAMT                                                 
         B     VALNRATY                                                         
*                                                                               
VNRAT04  GOTO1 AVALAMT,BOPARM,(X'85',FVIHDR),(6,BOWORK1)                        
         BNE   EXITN                                                            
         SRP   BOWORK1(6),1,0                                                   
         MVC   TLRNEXC,BOWORK1                                                  
         OC    TLRNEXC,TLRNEXC     TEST ZERO AMOUNT                             
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     EXITN                                                            
         GOTO1 GETCUR,(R1),TLRCUR  CHECK EXCHANGE RATE IN VALID RANGE           
         L     R2,0(R1)                                                         
         USING CCYTABD,R2                                                       
         CLC   TLRNEXC,CCYTMIN                                                  
         BL    *+14                                                             
         CLC   TLRNEXC,CCYTMAX                                                  
         BNH   VNRAT06                                                          
         MVC   FVMSGNO,=AL2(AE$EXRNV)                                           
         GOTO1 AEDTRAT,BOPARM,(L'FVXTRA,FVXTRA),CCYTMIN,CCYTMAX                 
         B     EXITN                                                            
         DROP  R2                                                               
*                                                                               
VNRAT06  ZAP   BODUB1,TLRCAMT      CALCULATE NEW AMOUNT                         
         MVC   BOWORK1(L'TLRNVAL),TLRNVAL                                       
         XI    BOWORK1+(TLRNIND-TLRNVAL),X'01'                                  
         ICM   RE,8,BOWORK1+(TLRNSHF-TLRNVAL)                                   
         SRA   RE,32-8                                                          
         LCR   RE,RE                                                            
         STC   RE,BOWORK1+(TLRNSHF-TLRNVAL)                                     
         EXCHP BODUB1,BOWORK1,DUB=BODUB2,WRK=BOWORK2                            
         SP    OSTOTCHG,TLRNAMT                                                 
         ZAP   TLRNAMT,BODUB2                                                   
         AP    OSTOTCHG,TLRNAMT                                                 
*                                                                               
VALNRATY B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY COLUMN                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISCLM   L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         USING TRNELD,TRNRFST                                                   
         SLL   R1,2                                                             
         B     *+4(R1)                                                          
         B     DISWC               WORKCODE                                     
         B     DISSUP              SUPPLIER/CONTRA                              
         B     DISREF              TRANSACTION REFERENCE                        
         B     DISCUR              CURRENCY CODE                                
         B     DISCAMT             CURRENCY AMOUNT                              
         B     DISNRAT             NEW RATE                                     
         B     DISNAMT             NEW AMOUNT                                   
         B     DISORAT             OLD RATE                                     
         B     DISOAMT             OLD AMOUNT                                   
         B     DISEXD              EXCHANGE DIFFERENCE                          
         SPACE 1                                                                
***********************************************************************         
* DISPLAY WORKCODE                                                    *         
***********************************************************************         
         SPACE 1                                                                
DISWC    MVC   FVIFLD(L'TLRWC),TLRWC                                            
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY SUPPLIER/CONTRA                                             *         
***********************************************************************         
         SPACE 1                                                                
DISSUP   MVC   FVIFLD(L'TLRSUP-1),TLRSUP+1                                      
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY REFERENCE                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISREF   MVC   FVIFLD(L'TLRREF#),TLRREF#                                        
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY CURRENCY CODE                                               *         
***********************************************************************         
         SPACE 1                                                                
DISCUR   MVC   FVIFLD(L'TLRCUR),TLRCUR                                          
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY CURRENCY AMOUNT                                             *         
***********************************************************************         
         SPACE 1                                                                
DISCAMT  GOTO1 FMTAMT,BOPARM,TLRCAMT,FVIFLD,TLRCTAB                             
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY NEW RATE                                                    *         
***********************************************************************         
         SPACE 1                                                                
DISNRAT  GOTO1 FMTRAT,BOPARM,TLRNEXC,FVIFLD                                     
         LA    RF,FVIFLD           LEFT ALIGN RATE                              
         CLI   0(RF),C' '                                                       
         BH    *+14                                                             
         MVC   FVIFLD(11),FVIFLD+1                                              
         B     *-14                                                             
         NI    FVIHDR+FHATD,FF-FHATHI                                           
         CLC   TLRNAMT,TLROAMT     HIGHLIGHT IF AMOUNTS DIFFERENT               
         BE    *+8                                                              
         OI    FVIHDR+FHATD,FHATHI                                              
         TM    TLRINDS,TLRIREV     PROTECT IF NOT REVALUABLE                    
         BO    *+8                                                              
         OI    FVIHDR+FHATD,FHATPR                                              
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY NEW AMOUNT                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISNAMT  GOTO1 FMTAMT,BOPARM,TLRNAMT,FVIFLD,CSCURCPY                            
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY OLD RATE                                                    *         
***********************************************************************         
         SPACE 1                                                                
DISORAT  GOTO1 FMTRAT,BOPARM,TLROEXC,FVIFLD                                     
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY OLD AMOUNT                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISOAMT  GOTO1 FMTAMT,BOPARM,TLROAMT,FVIFLD,CSCURCPY                            
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY EXCHANGE DIFFERENCE                                         *         
***********************************************************************         
         SPACE 1                                                                
DISEXD   ZAP   BOWORK1(6),TLRNAMT                                               
         SP    BOWORK1(6),TLROAMT                                               
         GOTO1 FMTAMT,BOPARM,BOWORK1,FVIFLD,CSCURCPY                            
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
LSTFRST  MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   TRNKACT,BCJOBCOD                                                 
         NI    OSINDS,FF-OSIDAY                                                 
         ZAP   OSTOTCHG,BCPZERO                                                 
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* GET FIRST/NEXT RECORD FOR LIST                                      *         
***********************************************************************         
         SPACE 1                                                                
GETFRST  LA    R1,IOHIGH+IOACCDIR                                               
         B     *+8                                                              
GETNEXT  LA    R1,IOSEQ+IOACCDIR                                                
         GOTO1 AIO                                                              
         BNE   EXITN                                                            
         CLC   TRNKEY(TRNKWORK-TRNKEY),IOKEYSAV                                 
         BNE   EXITN                                                            
         CLC   TRNKWORK,REVUL                                                   
         BNL   EXITN                                                            
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
         OC    LSFLTCUR,LSFLTCUR   TEST CURRENCY CODE FILTER                    
         BZ    *+14                                                             
         CLC   LSFLTCUR,AFCCURR                                                 
         BNE   GETNEXT                                                          
*                                                                               
         GOTO1 ASETTRN,BOPARM,(C'M',TRNRECD)                                    
         BNE   GETNEXT                                                          
*                                                                               
         MVI   TLRINDS,0           TEST ANYTHING PENDING                        
         TM    TLXPEND,TLXPALL+TLXPWOF+TLXPXFR+TLXPRCV+TLXPREV                  
         BNZ   *+8                                                              
         OI    TLRINDS,TLRIREV     NO - ITEM CAN BE REVALUED                    
*                                                                               
         CLI   LSREV,C'Y'          REVALUE=YES/NO/ONLY                          
         BE    GNEXT08                                                          
         CLI   LSREV,C'O'                                                       
         BNE   GNEXT06                                                          
         TM    TLRINDS,TLRIREV                                                  
         BO    GNEXT08                                                          
         B     GETNEXT                                                          
GNEXT06  TM    TLRINDS,TLRIREV                                                  
         BO    GETNEXT                                                          
*                                                                               
GNEXT08  MVC   TLRWC,TRNKWORK                                                   
         MVC   TLRSUP,TRNKCULC                                                  
         MVC   TLRREF#,TRNKREF                                                  
         MVC   TLRDATE,TRNKDATE                                                 
         MVC   TLRCUR,AFCCURR                                                   
         MVC   TLRCAMT,AFCAMNT                                                  
         MVC   TLRNVAL,AFCX                                                     
         MVC   TLROVAL,TLRNVAL                                                  
         MVC   TLRNAMT,TRNAMNT                                                  
         MVC   TLROAMT,TLRNAMT                                                  
         GOTO1 GETCUR,BOPARM,TLRCUR                                             
         L     RF,0(R1)                                                         
         MVC   TLRCTAB,CCYTTAB-CCYTABD(RF)                                      
         BE    GETNEXTX                                                         
         GOTO1 AIO,IOREAD+IOACCDIR  BLDCUR DOES IO                              
*                                                                               
GETNEXTX B     EXITY                                                            
         DROP  R3                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET HEADLINE                                             *         
***********************************************************************         
         SPACE 1                                                                
SETHEAD  LM    R2,R4,0(R1)         R2=CLMTAB,R3=HEAD1,R4=HEAD2                  
         USING CLMTABD,R2                                                       
         GOTO1 CENCUR,BOPARM,(CLMHWDTH,(R4))                                    
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECTION                                                  *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   OI    OSINDS,OSIDAY                                                    
         SLL   R1,2                                                             
         B     *(R1)                                                            
         B     VALCLR              CLEAR                                        
         B     VALBIL              BILL                                         
         B     VALMAT              MATCH                                        
         B     VALYES              YES                                          
         SPACE 1                                                                
VALCLR   MVC   TLRNVAL,TLROVAL     CLEAR - RESET TO OLD VALUES                  
         SP    OSTOTCHG,TLRNAMT                                                 
         MVC   TLRNAMT,TLROAMT                                                  
         AP    OSTOTCHG,TLRNAMT                                                 
         B     EXITY                                                            
         SPACE 1                                                                
VALBIL   TM    TLRINDS,TLRIREV                                                  
         BZ    EXITN                                                            
         CLC   TLRCUR,CSBILCUR                                                  
         BNE   EXITN                                                            
         MVC   TLRNVAL,CSEXCVAL                                                 
         B     VALYES10                                                         
         SPACE 1                                                                
VALMAT   TM    OSINDS,OSIMAT                                                    
         BZ    EXITN                                                            
         CLC   TLRCUR,M.TLMCUR                                                  
         BNE   EXITN                                                            
         MVC   TLRNVAL,M.TLMX                                                   
         B     VALYES10                                                         
         SPACE 1                                                                
VALYES   TM    TLRINDS,TLRIREV                                                  
         BZ    EXITN                                                            
         LA    R2,BOELEM                                                        
         USING GEXCD,R2            BUILD KEY OF EXCHANGE RECORD                 
         XC    GEKEY,GEKEY                                                      
         MVC   GEKAGY,CUAALF                                                    
         MVC   GEKCURF,CSCPYCUR                                                 
         MVC   GEKCURT,TLRCUR                                                   
         MVC   GEKACT,BCCLICOD                                                  
         MVC   GEKPEND,BCTODAYC                                                 
         LA    RF,X'20'            SET TO GET ACCOUNTING RATE                   
         TM    BCCPYST6,CPYSFTXR                                                
         BZ    *+8                                                              
         LA    RF,X'0C'(RF)        SET FT RATES ARE PERMITTED                   
         GOTO1 VGETCUR,BOPARM,((RF),GEXCD),ACOM                                 
         CLI   0(R1),0                                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$EXCNF)                                           
         B     EXITN                                                            
         L     R2,0(R1)            R2=A(GENFIL RECORD)                          
         MVC   TLRNIND,GESTAT      EXTRACT EXCHANGE RATE VALUES                 
         MVC   TLRNEXC,GEXRATE                                                  
         MVC   TLRNSHF,GEXSHFT                                                  
         DROP  R2                                                               
*                                                                               
VALYES10 ZAP   BODUB1,TLRCAMT      CALCULATE NEW AMOUNT                         
         MVC   BOWORK1(L'TLRNVAL),TLRNVAL                                       
         XI    BOWORK1+(TLRNIND-TLRNVAL),X'01'                                  
         ICM   RE,8,BOWORK1+(TLRNSHF-TLRNVAL)                                   
         SRA   RE,32-8                                                          
         LCR   RE,RE                                                            
         STC   RE,BOWORK1+(TLRNSHF-TLRNVAL)                                     
         EXCHP BODUB1,BOWORK1,DUB=BODUB2,WRK=BOWORK2                            
         SP    OSTOTCHG,TLRNAMT                                                 
         ZAP   TLRNAMT,BODUB2                                                   
         AP    OSTOTCHG,TLRNAMT                                                 
*                                                                               
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE TRANSACTION DETAILS (DATE/REF#/MONTH)           *         
***********************************************************************         
         SPACE 1                                                                
VALTRN   NTR1  ,                                                                
*                                                                               
         TM    BASDATH+FHIID,FHIIVA                                             
         BNZ   VTRN02                                                           
         OI    BASDATH+FHOID,FHOITR                                             
         BAS   RE,VALDAT                                                        
         BNE   EXITN                                                            
         OI    BASDATH+FHIID,FHIIVA                                             
         OI    BCINDS2,BCIHLDLP                                                 
*                                                                               
VTRN02   CLI   BASREFH+FHILD,0                                                  
         BNE   VTRN04                                                           
         CLI   BASMONH+FHILD,0                                                  
         BNE   VTRN04                                                           
         TM    RINDS,RILIVE+RIDRAFT                                             
         BZ    EXITY                                                            
*                                                                               
VTRN04   TM    BASREFH+FHIID,FHIIVA                                             
         BNZ   VTRN06                                                           
         OI    BASREFH+FHOID,FHOITR                                             
         BAS   RE,VALREF                                                        
         BNE   EXITN                                                            
         OI    BASREFH+FHIID,FHIIVA                                             
         OI    BCINDS2,BCIHLDLP                                                 
*                                                                               
VTRN06   TM    BASMONH+FHIID,FHIIVA                                             
         BNZ   VTRN08                                                           
         OI    BASMONH+FHOID,FHOITR                                             
         BAS   RE,VALMON                                                        
         BNE   EXITN                                                            
         OI    BASMONH+FHIID,FHIIVA                                             
         OI    BCINDS2,BCIHLDLP                                                 
*                                                                               
VTRN08   B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE THE DATE FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
VALDAT   NTR1  ,                                                                
         CLI   BASDATH+FHILD,0                                                  
         BNE   VDAT02                                                           
         GOTO1 VDATCON,BOPARM,(2,BCTODAYC),(17,BASDAT)                          
VDAT02   GOTO1 AFVAL,BASDATH                                                    
         GOTO1 VPERVAL,BOPARM,(FVILEN,FVIFLD),(X'40',BOELEM)                    
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
         GOTO1 VDATCON,BOPARM,(0,PVALESTA),(X'20',BASDAT)                       
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
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$DOPSP)                                           
         B     EXITN                                                            
         XC    BOWORK1,BOWORK1                                                  
         GOTO1 VDATCON,BOPARM,(2,OSDATC),(17,BOWORK1)                           
         MVC   BASDAT,BCSPACES                                                  
         MVC   BASDAT,BOWORK1                                                   
         B     EXITY                                                            
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDTE THE REFERENCE # FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
VALREF   NTR1  ,                                                                
         MVI   FVMINL,2                                                         
         GOTO1 AFVAL,BASREFH                                                    
         BNE   EXITN                                                            
         XR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         LA    R1,FVIFLD                                                        
VREF02   CLI   0(R1),C'A'                                                       
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXITN                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,VREF02                                                        
         MVC   OSREF#,FVIFLD                                                    
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE THE MONTH FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
VALMON   NTR1  ,                                                                
         CLI   BASMONH+FHILD,0     VALIDATE MONTH                               
         BNE   VMON02                                                           
         GOTO1 VDATCON,BOPARM,(2,BCTODAYC),(9,BASMON)                           
VMON02   GOTO1 AFVAL,BASMONH                                                    
         BNE   EXITN                                                            
         MVC   CSBSECL,BCCPYEL+(CPYBSEC-CPYELD)                                 
         LA    R3,BOWORK1                                                       
         USING BMONVALD,R3                                                      
         GOTO1 VBMONVAL,BOPARM,(FVILEN,FVIFLD),(79,ACOM),              *        
               (CULANG,BMONVALD),(CUABIN,0)                                     
         MVC   BOBYTE1,0(R1)       BATCH SECURITY LEVEL                         
         CLI   BMOERR,BMOEOKQ      TEST ANY ERROR                               
         BE    *+14                                                             
         MVC   FVMSGNO,BMOMSG                                                   
         B     EXITN                                                            
*                                                                               
         MVC   BODUB1,BMOMOSP                                                   
         MVI   BODUB1+L'BMOMOSP,X'01'                                           
         MVC   BASMON,BCSPACES                                                  
         GOTO1 VDATCON,BOPARM,(1,BODUB1),(9,BASMON)                             
*                                                                               
         CLI   BMOERR,BMOEOKQ      TEST ANY ERROR                               
         BNE   EXITN                                                            
         MVC   OSMONP,BMOMOSP                                                   
         MVC   OSMONC,BMOMOSC                                                   
         MVC   CSBSECL,BOBYTE1     SET BATCH SECURITY                           
*                                                                               
         B     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT OUT REPORT                                                    *         
***********************************************************************         
         SPACE 1                                                                
PRTOUT   NTR1  ,                                                                
         USING PLINE,P1                                                         
*                                                                               
         MVC   TLNUM,CSPSRECN                                                   
POUT02   ICM   RE,3,TLNUM                                                       
         LA    RE,1(RE)                                                         
         STCM  RE,3,TLNUM                                                       
         CLC   TLNUM,CSHIRECN                                                   
         BH    POUT10                                                           
         GOTO1 ATSARIO,TSAGET                                                   
         CLC   TLRNAMT,TLROAMT                                                  
         BE    POUT02                                                           
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
POST10   MVC   TLNUM,CSPSRECN                                                   
POST12   ICM   RE,3,TLNUM                                                       
         LA    RE,1(RE)                                                         
         STCM  RE,3,TLNUM                                                       
         CLC   TLNUM,CSHIRECN                                                   
         BH    POST30                                                           
         GOTO1 ATSARIO,TSAGET                                                   
         CLC   TLRNAMT,TLROAMT                                                  
         BE    POST12                                                           
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
         BZ    POST28                                                           
         MVC   TLROVAL,TLRNVAL     OLD RATE = NEW RATE                          
         MVC   TLROAMT,TLRNAMT     OLD AMOUNT = NEW AMOUNT                      
         GOTO1 ATSARIO,TSAPUT      TSAR RECORD NOW FOR NEW POSTING              
*                                                                               
POST28   B     POST12                                                           
*                                                                               
POST30   GOTO1 ABLDTRN,BOPARM,(X'FF',POSTVALS)                                  
         MVI   REPACTN,REPACLO     CLOSE REPORT                                 
         GOTO1 VREPORT,REPD                                                     
         L     RF,ATIA             CLEAR TIA FOR NEXT GETOPT CALL               
         XC    0(256,RF),0(RF)                                                  
         TM    RINDS,RILIVE                                                     
         BZ    EXITY                                                            
         NI    OSINDS,FF-OSIDAY                                                 
         ZAP   OSTOTCHG,BCPZERO                                                 
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
         CLI   BCPFKEY,PFKRLIVQ                                                 
         BNE   *+8                                                              
         LA    RE,=C'ARV'                                                       
         XC    REPSUBID,REPSUBID                                                
         OC    REPSUBID,CSREPID                                                 
         BNZ   *+10                                                             
         MVC   REPSUBID,0(RE)                                                   
         MVCDD REPDESC,AC#DRVRP                                                 
         MVI   REPSUBPG,REPSDREP                                                
         CLI   BCPFKEY,PFKRLIVQ                                                 
         BNE   *+14                                                             
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
         SPACE 1                                                                
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
FF       EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
ACCMST   DC    C'ACCMST '                                                       
REVUL    DC    C'99'               REVERSAL UNIT/LEDGER                         
INCUL    DC    C'SI'               INCOME UNIT/LEDGER                           
EXPUL    DC    C'SE'               EXPENSE UNIT/LEDGER                          
CASHUL   DC    C'SC'               CASH UNTIL/LEDGER                            
INCCSTUL DC    C'12'               INCOME COSTING UNIT/LEDGER                   
EXPCSTUL DC    C'13'               EXPENSE COSTING UNIT/LEDGER                  
PSNCSTUL DC    C'1P'               PERSONNEL COSTING UNIT/LEDGER                
C99      DC    C'99'                                                            
DQU      DC    CL(L'BASSRV)'=DQU'                                               
PMINUS1  DC    P'-1'                                                            
         SPACE 1                                                                
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
*                                                                               
*        SPROG 3,4                                                              
*        SPEC  M1,1,AC#ACC,8                                                    
*        SPEC  M2,1,AC#ACC,8,LU                                                 
*        SPEC  M1,16,AC#CTRA,10                                                 
*        SPEC  M2,16,AC#CTRA,10,LU                                              
*        SPEC  M1,32,AC#OFF,3                                                   
*        SPEC  M2,32,AC#OFF,3,LU                                                
*        SPEC  M1,36,AC#DATE,5                                                  
*        SPEC  M2,36,AC#DATE,5,LU                                               
*        SPEC  M1,45,AC#REFN,6                                                  
*        SPEC  M2,45,AC#REFN,6,LU                                               
*        SPEC  M1,52,AC#MOA,5                                                   
*        SPEC  M2,52,AC#MOA,5,LU                                                
*        SPEC  M1,58,AC#DRS,10,R                                                
*        SPEC  M2,58,AC#DRS,10,RU                                               
*        SPEC  M1,70,AC#CRS,10,R                                                
*        SPEC  M2,70,AC#CRS,10,RU                                               
*                                                                               
         SPEC  END                                                              
*                                                                               
DEFCLM   DS    0XL1                                                             
         DC    AL1(RVL#CUR)                                                     
         DC    AL1(RVL#CAMT)                                                    
         DC    AL1(RVL#NRAT)                                                    
         DC    AL1(RVL#NAMT)                                                    
         DC    AL1(RVL#ORAT)                                                    
         DC    AL1(RVL#OAMT)                                                    
         DC    AL1(RVL#EXD)                                                     
         DC    AL1(EOT)                                                         
DEFCLML  EQU   *-DEFCLM                                                         
*                                                                               
         SPACE 1                                                                
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
***********************************************************************         
* OPTION TABLE                                                        *         
***********************************************************************         
         SPACE 1                                                                
CLB10    CSECT                                                                  
OPTTAB   DS    0X                                                               
*                                  DISPLAY=COLUMN CODES                         
         DC    AL2(UC8DSP-TWAD,UC3DSP-TWAD)                                     
         DC    AL1(OPTNRTN+OPTDFLTI,0)                                          
         DC    AL1(0,0,0,0,0,1,L'LSDISLST,L'LSDISLST)                           
         DC    AL1((*-OPTTAB)/OPTTABL)                                          
         DC    AL2(OPTDISQ,LSDIS-LSVALSD)                                       
         DC    CL4'+'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  GENERAL TRANSACTION OPTIONS                  
         DC    AL2(TRNOPTQ)                                                     
*                                  CURRENCY=CODE                                
         DC    AL2(UC@CURRY-TWAD,UC@CURRY-TWAD)                                 
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,L'LSFLTCUR,L'LSFLTCUR,L'LSFLTCUR)                  
         DC    AL1((*-OPTTAB)/OPTTABL)                                          
         DC    AL2(VALOCURQ,LSFLTCUR-LSVALSD)                                   
         DC    XL4'00'                                                          
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  REVALUE=Y/N/O                                
         DC    AL2(UC@REVLU-TWAD,UC@REVLU-TWAD)                                 
         DC    AL1(OPTNRTN+OPTDFLTO,0)                                          
         DC    AL1(0,0,0,0,0,1,4,L'LSREV)                                       
         DC    AL1((*-OPTTAB)/OPTTABL)                                          
         DC    AL2(OPTYNOQ,LSREV-LSVALSD)                                       
         DC    CL4'Y'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
OPTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
*********************************************************************           
* VALIDATE OPTION INPUT                                             *           
*********************************************************************           
         SPACE 1                                                                
         DROP                                                                   
         DS    0D                                                               
VALOPT   NMOD1 0,**VALO**                                                       
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING TWAD,RA             RA=A(TWA)                                    
         LR    R4,R2               R4=A(OPTTAB ENTRY)                           
         USING OPTTABD,R4                                                       
         L     RC,AOVERWRK                                                      
         USING RWORKD,RC                                                        
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *(RF)                                                            
VALROUTS DS    0XL4                                                             
         B     VALOCUR             CUR=USD                                      
VALOCURQ EQU   (*-VALROUTS)/4                                                   
         SPACE 1                                                                
VALX     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE CURRENCY=CODE                                              *         
***********************************************************************         
         SPACE 1                                                                
VALOCUR  GOTO1 VBLDCUR,BOPARM,FVIFLD,(X'80',BOWORK1),ACOM                       
         CLI    0(R1),0                                                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INCUR)                                           
         B     VALX                                                             
         MVC   BCWORK(3),FVIFLD                                                 
         B     VALX                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORK                                                      
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBEFD                                                       
         SPACE 1                                                                
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
*                                                                               
LSVALSD  DSECT                     * FILTERS *                                  
         ORG LSOVER                                                             
LSFLTCUR DS    CL3                 CURRENCY                                     
LSREV    DS    CL1                 REVALUE=Y/N/O                                
         ORG   LSOVER+L'LSOVER                                                  
         EJECT                                                                  
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBCOLS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBCOLS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDBOXEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
         SPACE 1                                                                
* GEGENEXC                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEGENEXC                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
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
         SPACE 1                                                                
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
***********************************************************************         
* LOCAL W/S                                                           *         
***********************************************************************         
         SPACE 1                                                                
RWORKD   DSECT                                                                  
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
RAAFCEL  DS    A                   A(FOREIGN CURRENCY ELEMENT)                  
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
RLSOPS   DS    XL(LSOPL)           SAVED FILTERS                                
*                                                                               
         DS    0A                                                               
RPOSTVAL DS    XL(POSTVALL)                                                     
*                                                                               
SCANBYTE DS    CL1                                                              
SCANBLK  DS    5CL(SCBLKLQ)                                                     
*                                                                               
CCYTAB   DS    10XL(CCYTABL)                                                    
CCYTABN  EQU   (*-CCYTAB)/CCYTABL                                               
CCYTABX  DS    XL1                                                              
NAMTAB   DS    15XL(NAMTABL)                                                    
NAMTABN  EQU   (*-NAMTAB)/NAMTABL                                               
NAMTABX  DS    XL1                                                              
*                                                                               
         ORG   RWORKD+OVERWRKL                                                  
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060ACCLB10A  12/23/99'                                      
         END                                                                    
