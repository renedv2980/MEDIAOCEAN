*          DATA SET ACENQ20    AT LEVEL 018 AS OF 09/20/07                      
*PHASE T62020A                                                                  
T62020   TITLE 'REFERENCE NUMBER SEARCH'                                        
         SPACE 2                                                                
* USER LVL DATE    LEVEL CHANGE COMMENTS                                        
* ---- --- ------- ---------------------------------------------------          
* TKLU 001 02JUL03 NEW REFERENCE NUMBER SEARCH OVERLAY <LO01-2611>              
* TKLU 002 30JUL03 BUG FIX WHEN REREADING TRX (ARCHIVE/MASTER)                  
* TKLU 003 30JUL03 ENABLE GRID COLUMNS                                          
* TKLU 004 16SEP03 BUG FIX (ACCMST/ACCARC)                                      
* TKLU 005 19SEP03 REFERENCE SEARCH EXTENSION 9/45/100 <LO01-2747>              
* YNGX 006 27FEB04 USE  NEW EQU                                                 
* TKLU 007 10MAY04 ADD TYPE 10 TO OPTIONS                                       
* TKLU 008 13MAY04 <ACCQ FT79> SKIP DELETED TRANSACTIONS IN FILTREC             
* TKLU 009 28DEC04 <DU01-0784> ADD BT89 (DEMEDA INTERFACE)                      
* TKLU 010 09DEC05 <DU01-4910> - EXTEND RNSPASD PASSIVES                        
* TKLU 011 01FEB06 <TK 1006961> - CURED B1 TYPE BUG FIX IF > X'7F'              
* TKLU 012 06FEB06 <TK 1026985> - OFFICE SECURITY ON NON SJ TRXS                
* TKLU 013 12FEB06 <TK 1026961> - LVL *11 FOR GRID, TOO                         
* TKLU 014 23JAN07 <DU01-5661> - SUPPORT FOR NEW DRAFT=Y/N/O OPTION             
* TKLU 015 29JAN07 <DU01-6138> - CHANGE DATE RANGE DISPLAY                      
         SPACE 2                                                                
T62020   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         NMOD1 0,**ENQ20**,R7,R8,CLEAR=YES,RR=RE                                
         USING TWAD,RA             RA=A(TWA)                                    
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         LHI   RC,OVERWORK-WORKD                                                
         LA    RC,WORKD(RC)        RC=A(LOCAL WORKING STORAGE)                  
         USING OVERWRKD,RC                                                      
         ST    RD,BASERD                                                        
         LA    R2,BASHEADH                                                      
         MVC   BASHEAD(25),=C'PROGRAM NOT AVAILABLE YET'                        
         OI    6(R2),X'40'         INSERT CURSOR                                
         OI    6(R2),X'80'                                                      
         L     RD,BASERD           GET OUT                                      
         XIT1                                                                   
*&&DO                                                                           
         ST    RE,ORELO                                                         
         L     RE,=A(FGRTSAR)                                                   
         A     RE,ORELO                                                         
         ST    RE,AFGRTSAR                                                      
         L     RE,=A(COLTBL)                                                    
         A     RE,ORELO                                                         
         ST    RE,ACOLTBL                                                       
*                                                                               
         GOTO1 VDICTATE,DMCB,C'LL  ',DCMIX,DSMIX                                
*                                                                               
         TM    DISPFLAG,NOTFRSTQ   FIRST TIME FOR DISPLAY?                      
         BO    MAIN20                                                           
         BAS   RE,FSTDIS           PERFORM FIRST DISPLAY FUNCTIONS              
         BNE   ERRXIT                                                           
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1 GET AN ACC DIRECTORY RECOR               
         BE    MAIN10                                                           
         TM    IOERR,IOMAX         MAX IO?                                      
         BO    MAIN140                                                          
         DC    H'0'                                                             
*                                                                               
MAIN10   TM    DISPFLAG,DISIOMAX   MAX IO'S BEEN REACHED?                       
         BO    MAINX                                                            
         TM    DISPFLAG,NORECQ     NO RECORDS FOUND?                            
         BO    MAINX                                                            
         OI    DISPFLAG,NOTFRSTQ   SET NOT FIRST TIME FLAG ON                   
         B     MAIN60                                                           
*                                                                               
MAIN20   TM    STATFLAG,STPGRTOT   PRINT GRID TOTAL?                            
         BNZ   MAIN170                                                          
         CLC   TSCURRNO,TSLSTREC   HAVE WE ALLREADY GOT RECORD IN TSAR?         
         BH    MAIN30              NO                                           
*                                                                               
         GOTO1 ATSARGET,TSCURRNO   GET TSAR RECORD                              
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BZ    MAIN22                                                           
         GOTO1 AFGRTSAR,(RC)       FORMAT TSAR ONTO GRID SCREEN LINES           
         B     MAIN24                                                           
*                                                                               
MAIN22   BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
*                                                                               
MAIN24   GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BNE   MAINX               IS SCREEN FULL?                              
         MVC   TSLSTLIN,TSCURRNO   KEEP TRACK OF LAST TSAR REC USED             
         XR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO       BUMP UP THE CURRENT TSAR NUMBER              
         B     MAIN20                                                           
*                                                                               
MAIN30   TM    DISPFLAG,TSARFULQ   TSAR BLOCK FULL?                             
         BNO   MAIN40                                                           
         LA    R2,BASKEYH                                                       
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$TIRES)                                           
         B     ERRXIT                                                           
*                                                                               
MAIN40   TM    DISPFLAG,ALLREADQ   HAVE ALL RECORDS BEEN READ?                  
         BO    MAINX                                                            
         MVC   IOKEY,KEYSAVE                                                    
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1 GET AN ACC DIRECTORY RECORD              
         BE    MAIN50                                                           
         TM    IOERR,IOMAX         MAX IO?                                      
         BO    MAIN140                                                          
         DC    H'0'                                                             
*                                                                               
MAIN50   GOTO1 AIO,IOSEQ+IOACCDIR+IO1 READ SEQ FOR NEXT RECORD                  
         BE    MAIN60                                                           
         TM    IOERR,IOMAX         MAX IOS REACHED?                             
         BO    MAIN140                                                          
         DC    H'0'                                                             
*                                                                               
         USING RNSPASD,R3                                                       
MAIN60   LA    R3,IOKEY            R3=A(ACCOUNT KEY)                            
         CLC   MYKEYSV,RNSPKEY                                                  
         BNE   MAIN150             MATCH ON RELEVANT PART OF KEY?               
         MVC   KEYSAVE,IOKEY       SAVE KEY FOR SEQUENCE RESTORE                
*                                                                               
         BAS   RE,FILTKEY          FILTER ON DIRECTORY RECORD                   
         BNE   MAIN130                                                          
         MVC   DADDRESS,RNSPKDA    DISK ADDRESS                                 
*                                                                               
         LA    RF,IOGET+IOACCMST+IO1                                            
         TM    RNSPSTA,TRNSARCH    IS RECORD ON ARCHIVE FILE                    
         BZ    *+8                 NO                                           
         LA    RF,IOGET+IOACCARC+IO1   YES THEN READ ARCHIVE NOT MST            
         GOTO1 AIO,(RF)                                                         
         BE    MAIN70                                                           
         TM    IOERR,IOMAX         MAX IO?                                      
         BO    MAIN140                                                          
         DC    H'0'                                                             
*                                                                               
MAIN70   MVI   OFFLFLAG,0                                                       
         MVI   SKIPIND,0                                                        
         MVC   KEYSAVE,IOKEY                                                    
         L     R3,AIO1             R3=A(IOAREA1)                                
         LA    RE,LDGOPTAB                                                      
MAIN71   CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(2,RE),TRNKULA-TRNRECD(R3)                                      
         BE    MAIN72                                                           
         AHI   RE,6                                                             
         B     MAIN71                                                           
MAIN72   MVC   LDGTOFFP,2(RE)                                                   
         MVC   LEDGTLVA(3),3(RE)                                                
         CLI   TERMACCS,C'*'       LIMIT ACCESS?                                
         BE    MAIN72A                                                          
         CLI   TERMACCS,C'$'                                                    
         BNE   MAIN80                                                           
MAIN72A  CLI   POFSECRE,C'Y'       FULL OFFICE SECURITY?                        
         BNE   MAIN80                                                           
         CLC   TRNKULA-TRNRECD(2,R3),=C'SJ'                                     
         BE    MAIN73                                                           
         CLI   LDGTOFFP,LDGONKHI                                                
         BNH   MAIN73                                                           
         GOTO1 AOFFTRN             APPLY SECURITY CHECK/FILTER                  
         CLI   OFFLFLAG,0                                                       
         BE    *+8                                                              
         MVI   SKIPIND,2                                                        
         TM    IOERR,IOMAX         MAX IO?                                      
         BO    MAIN140                                                          
         B     MAIN74                                                           
*                                                                               
         USING ACTRECD,R3                                                       
MAIN73   L     R3,AIO1             OFF SEC FOR SJ                               
         MVC   IOKEY,0(R3)                                                      
         LA    R3,IOKEY                                                         
         MVC   ACTKEY+ACTKEND(L'ACTKEY-ACTKEND),SPACES                          
         GOTO1 AIO,IORD+IOACCDIR+IO1                                            
         BE    MAIN73A                                                          
         TM    IOERR,IOMAX         MAX IO?                                      
         BO    MAIN140                                                          
         B     MAIN130                                                          
MAIN73A  GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    MAIN73B                                                          
         TM    IOERR,IOMAX         MAX IO?                                      
         BO    MAIN140                                                          
         DC    H'0'                                                             
MAIN73B  GOTO1 AOFFACC                                                          
         BE    *+8                                                              
         MVI   SKIPIND,1                                                        
MAIN74   MVC   IOKEY,KEYSAVE       REREAD LAST RECORD                           
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1                                          
         BE    MAIN75                                                           
         TM    IOERR,IOMAX         MAX IO?                                      
         BO    MAIN140                                                          
         DC    H'0'                                                             
MAIN75   LA    RF,IOGET+IOACCMST+IO1                                            
         LA    RE,IOKEY            IS RECORD ON ARCHIVE FILE                    
         TM    RNSPSTA-RNSPASD(RE),TRNSARCH                                     
         BZ    *+8                 NO                                           
         LA    RF,IOGET+IOACCARC+IO1   YES THEN READ ARCHIVE NOT MST            
         GOTO1 AIO,(RF)                                                         
         BE    MAIN76                                                           
         TM    IOERR,IOMAX         MAX IO?                                      
         BO    MAIN140                                                          
         DC    H'0'                                                             
MAIN76   CLI   SKIPIND,0                                                        
         BNE   MAIN130                                                          
         DROP  R3                                                               
*                                                                               
MAIN80   L     R3,AIO1                                                          
         BAS   RE,FILTREC                                                       
         BNE   MAIN130                                                          
         BAS   RE,BLDDIS           BUILD/DISPLAY ACCOUNT TSAR REC(S)            
         BNE   MAINX                                                            
*                                                                               
MAIN100  TM    DISPFLAG,DISIOMAX                                                
         BO    MAINX                                                            
*                                                                               
MAIN130  B     MAIN50                                                           
*                                                                               
         USING RNSPASD,R3                                                       
MAIN140  LA    R3,IOKEY                                                         
         MVC   KEYSAVE,RNSPKEY                                                  
         OI    DISPFLAG,DISIOMAX                                                
         B     MAINX                                                            
*                                                                               
MAIN150  OC    TSLSTREC,TSLSTREC   ANY RECORDS DISPLAYED?                       
         BNZ   MAIN160                                                          
         OI    DISPFLAG,NORECQ     NO RECORDS TO DISPLAY                        
         B     MAINX                                                            
*                                                                               
MAIN160  OI    DISPFLAG,ALLREADQ                                                
         BAS   RE,TOTAL                                                         
         B     MAINX                                                            
*                                                                               
MAIN170  TM    PCDRIVEN,PCGRIDQ          TEST RUNNING UNDER GRID?               
         BZ    MAIN180                                                          
         OI    PCDRIVEN,PCGMSG1    SHOW GRID MESSAGE 1?                         
         GOTO1 AFGRTSAR,(RC)                                                    
         GOTO1 ADISPLAY,DISATRIB         DISPLAY DUMMY SCREEN LINES             
         MVC   TSLSTLIN,TSCURRNO                                                
         MVI   DISPFLAG,0                                                       
*                                                                               
MAIN180  NI    STATFLAG,X'FF'-STPGRTOT   ENSURE NEW PAGE                        
*                                                                               
MAINX    B     OKXIT                                                            
         EJECT                                                                  
***********************************************************************         
*        DEAL WITH THE TOTAL LINE                                     *         
***********************************************************************         
         SPACE 1                                                                
TOTAL    NTR1                                                                   
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BZ    XIT                                                              
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LHI   R1,TSARRECL                                                      
         XR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R3,ATSARREC                                                      
         USING TSARRECD,R3                                                      
         LHI   RF,TSDLENQ                                                       
         AHI   RF,TSARDATA-TSARRECD                                             
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO   SET TSAR REC NUMBER                          
         LA    R3,TSARDATA         R3=A(TSAR RECORD DATA)                       
         USING TSARDATD,R3                                                      
         MVI   TSDLINES,1                                                       
         MVI   TSDFMT,TSITEMT      TOTAL LINE TYPE                              
         MVC   TSDLINES,LINSUSED                                                
         GOTO1 AFGRTSAR,(RC)       FORMAT TSAR ONTO GRID SCREEN LINES           
         GOTO1 ATSARADD                                                         
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY TOTAL LINE                     
         BNE   TOTAL02                                                          
         MVC   TSLSTLIN,TSCURRNO                                                
         MVI   DISPFLAG,0                                                       
         B     TOTALX                                                           
*                                                                               
TOTAL02  OI    STATFLAG,STPGRTOT   PRINT GRID TOTAL NEXT TIME IN                
*        OI    INTFLAG,SCRFULLQ    SCREEN IS FULL                               
*                                                                               
TOTALX   B     XIT                                                              
         DROP  R3                                                               
***********************************************************************         
*              FIRST FOR DISPLAY FUNCTIONS                            *         
***********************************************************************         
         SPACE 1                                                                
FSTDIS   NTR1                                                                   
         USING FLDHDRD,R2                                                       
         LA    R2,BASCACH          R2=A(CONTRA ACCOUNT FIELD)                   
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         CLI   FLDILEN,0           INPUT NOT ALLOWED IN THIS FIELD              
         BNE   ERRXIT                                                           
         OI    FLDIIND,FINPVAL                                                  
         OI    FLDOIND,FOUTTRN                                                  
*                                                                               
         LA    R2,BASKEYH          R2=A(KEY FIELD)                              
         ST    R2,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$FLDTS)                                           
         CLI   FLDILEN,1           ENSURE 6 CHAR REFERENCE NUMBER               
         BL    ERRXIT                                                           
         MVC   FVMSGNO,=AL2(AE$FLDTL)                                           
         CLI   FLDILEN,6                                                        
         BH    ERRXIT                                                           
         MVC   REFNUM,FLDDATA      SAVE REFERENCE                               
         OC    REFNUM,SPACES                                                    
*                                                                               
         LA    R2,BASOPTH          R2=A(KEY FIELD)                              
         ST    R2,FVADDR                                                        
         USING OPTVALSD,RF                                                      
         L     RF,AOPTVALS         CHECK FOR VALID OPTIONS                      
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         CLI   ODRAFT,0                                                         
         BNE   *+8                                                              
         MVI   ODRAFT,C'N'                                                      
         OC    OBTYPEVL,OBTYPEVL                                                
         BZ    FSTD10              ALL ARE VALID                                
*                                                                               
FSTD10   DS    0H                  NO MORE YET                                  
         LA    R2,BASKEYH          R2=A(KEY FIELD)                              
         ST    R2,FVADDR                                                        
         DROP  RF                                                               
*                                                                               
         USING ACCRECD,R3                                                       
         LA    R3,IOKEY            READ FILE HEADER                             
         XC    ACCKEY,ACCKEY                                                    
         MVI   ACCKEY+L'ACCKEY-1,1                                              
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BE    *+6                                                              
         DC    H'0'                A REALLY FATAL ERROR                         
*                                                                               
         L     R3,AIO1             DETERMINE RANGE OF PASSIVES                  
         LA    R3,ACCRFST                                                       
         USING FHDELD,R3                                                        
         CLI   FHDEL,FHDELQ                                                     
         BE    *+6                                                              
         DC    H'0'                A REALLY FATAL ERROR                         
         GOTO1 VDATCON,DMCB,(4,FHDLDAT),(0,LASTLOAD)                            
         GOTO1 VADDAY,DMCB,(C'Y',LASTLOAD),MYTEMP,F'-7'                         
         GOTO1 VDATCON,DMCB,(0,MYTEMP),(13,RNGSTA)                              
*        GOTO1 VADDAY,DMCB,(C'D',LASTLOAD),MYTEMP,F'-1'                         
*        GOTO1 VDATCON,DMCB,(0,MYTEMP),(13,RNGEND)                              
         GOTO1 VDATCON,DMCB,(5,0),(13,RNGEND)                                   
*                                                                               
         LA    R4,LDGOPTAB        BUILD LEDGER OFFPOS TABLE                     
         XC    0(20*6,R4),0(R4)                                                 
         USING LDGRECD,R3                                                       
         LA    R3,IOKEY           READ LEDGER                                   
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,MYCO                                                     
         MVI   LDGKUNT,C'S'                                                     
FSTD20   LA    R3,IOKEY                                                         
         MVI   LDGKEY+LDGKEND,X'FF'                                             
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1                                          
         BE    FSTD30                                                           
         TM    IOERR,IOMAX         MAX IO?                                      
         BO    ERRXIT                                                           
         DC    H'0'                                                             
FSTD30   CLC   LDGKCPY,MYCO                                                     
         BNE   FSTD80                                                           
         CLI   LDGKUNT,C'S'                                                     
         BNE   FSTD80                                                           
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    FSTD40                                                           
         TM    IOERR,IOMAX         MAX IO?                                      
         BO    ERRXIT                                                           
         DC    H'0'                                                             
FSTD40   L     R3,AIO1                                                          
         MVC   0(2,R4),LDGKUNT                                                  
         LA    RE,LDGRFST                                                       
         XR    R0,R0                                                            
         USING LDGELD,RE                                                        
FSTD50   CLI   LDGEL,0                                                          
         BE    FSTD70                                                           
         CLI   LDGEL,LDGELQ                                                     
         BE    FSTD60                                                           
         CLI   LDGEL,ACLELQ                                                     
         BE    FSTD65                                                           
FSTD55   IC    R0,LDGLN                                                         
         AR    RE,R0                                                            
         B     FSTD50                                                           
FSTD60   MVC   2(1,R4),LDGOPOS                                                  
         B     FSTD55                                                           
         USING ACLELD,RE                                                        
FSTD65   MVC   3(1,R4),ACLELLVA                                                 
         MVC   4(1,R4),ACLELLVB                                                 
         MVC   5(1,R4),ACLELLVC                                                 
         B     FSTD55                                                           
FSTD70   AHI   R4,6                                                             
         B     FSTD20                                                           
         DROP  RE                                                               
*                                                                               
         USING RNSPASD,R3                                                       
FSTD80   LA    R3,IOKEY            BUILD START KEY                              
         XC    RNSPKEY,RNSPKEY                                                  
         MVI   RNSPTYP,RNSPTYPQ                                                 
         MVI   RNSPSUB,RNSPSUBQ                                                 
         MVC   RNSPCPY,MYCO                                                     
         MVI   RNSPIND,RNSPRFQ                                                  
         MVC   RNSPREF,REFNUM                                                   
         MVC   MYKEYSV,RNSPASD     SAVE RELEVANT PART OF KEY                    
*                                                                               
         MVI   STATFLAG,0                                                       
*                                                                               
         MVC   INFTXT,SPACES       BUILD SCREEN HEADING IN STORAGE              
         MVC   INFTXT(L'MX@RNSPR),MX@RNSPR                                      
         LA    R5,INFTXT+L'MX@RNSPR-1                                           
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVI   2(R5),C'='                                                       
         AHI   R5,4                                                             
         MVC   0(L'RNGSTA,R5),RNGSTA                                            
         AHI   R5,L'RNGSTA                                                      
         MVI   0(R5),C'-'                                                       
         AHI   R5,1                                                             
         MVC   0(L'RNGEND,R5),RNGEND                                            
         AHI   R5,L'RNGEND+1                                                    
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BNZ   FSTD90                                                           
         LA    R2,ENQDAT1H         DISPLAY SCREEN HEADINGS                      
         OI    FLDOIND,FOUTPRT                                                  
         OI    FLDOIND,FOUTTRN                                                  
         MVC   FLDDATA(L'ENQDAT1),INFTXT                                        
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         MVC   FLDDATA(L'MX@ENH38),MX@ENH38                                     
         OI    FLDOIND,FOUTPRT                                                  
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBHIGH                                                  
         LA    R2,ENQDAT2H-ENQDAT1H(R2)                                         
         B     FSTD92                                                           
*                                                                               
FSTD90   LA    R2,GRDDAT1H                                                      
*                                                                               
FSTD92   GOTO1 ASCRNDIM,DMCB,(0,(R2)) GET SCREEN DIMENSIONS                     
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BNZ   FSTDX                                                            
         L     RF,ADISPFK                                                       
         BASR  RE,RF               DISPLAY PFKEY LINE                           
*                                                                               
FSTDX    CR    RB,RB                                                            
         B     *+6                                                              
FSTDERR  LTR   RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        FILTER ACCOUNT DIRECTORY RECORD                              *         
* ON ENTRY R3=A(ACCOUNT KEY)                                          *         
* ON EXIT  CC IS SET TO EQUAL IF WE WANT RECORD                       *         
*          CC IS SET TO UNEQUAL IF RECORD IS REJECTED                 *         
***********************************************************************         
         SPACE 1                                                                
FILTKEY  NTR1                                                                   
         USING RNSPASD,R3                                                       
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
*                                                                               
         OC    OBTYPEVL,OBTYPEVL   BATCH TYPE FILTER?                           
         BZ    FILTK04                                                          
         CLI   OBTYPEFI,NEGFILTR                                                
         BE    FILTK02                                                          
         CLC   OBTYPEVL,RNSPBTY                                                 
         BNE   FILTKRJX                                                         
         B     FILTK04                                                          
*                                                                               
FILTK02  CLC   OBTYPEVL,RNSPBTY                                                 
         BE    FILTKRJX                                                         
*                                                                               
FILTK04  DS    0H                                                               
*                                                                               
FILTKX   CR    RB,RB                                                            
         B     XIT                                                              
FILTKRJX LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R3                                                            
***********************************************************************         
*        FILTER ACCOUNT DATA RECORD                                   *         
* ON ENTRY R3=A(ACCOUNT RECORD)                                       *         
* ON EXIT  CC IS SET TO EQUAL IF WE WANT RECORD                       *         
*          CC IS SET TO UNEQUAL IF RECORD IS REJECTED                 *         
***********************************************************************         
         SPACE 1                                                                
FILTREC  NTR1                                                                   
         USING TRNRECD,R3                                                       
         TM    TRNRSTAT,TRNSDELT   SKIP DELETED                                 
         BNZ   FILTRRJX                                                         
         L     R2,AOPTVALS         R2=A(OPTION VALUES)                          
         USING OPTVALSD,R2                                                      
*                                                                               
         OC    ODATE,ODATE         FILTERING ON TRANSACTION DATE?               
         BZ    FILTR02                                                          
         GOTO1 ADCOMP,DMCB,(L'TRNKDATE,TRNKDATE),ODATEST,ODATEEN,      *        
               ODATEFI                                                          
         BNE   FILTRRJX                                                         
*                                                                               
         USING TRNELD,R4                                                        
FILTR02  LA    R4,TRNRFST          POINT TO TRNELD                              
         CLI   TRNEL,TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                FATAL ERROR                                  
*                                                                               
         CLC   TRNKULA(2),=C'SJ'                                                
         BE    FILTR08             SKIP SJ TRANSACTIONS                         
         MVC   CURROFF,TRNOFFC                                                  
FILTR04  OC    OOFFICVL,OOFFICVL   OFFICE CODE FILTER?                          
         BZ    FILTR10                                                          
         LA    RE,OOFFICVL+2                                                    
         TM    COMPSTA4,CPYSOFF2                                                
         BZ    *+8                                                              
         LA    RE,OOFFICVL                                                      
         CLI   OOFFICFI,NEGFILTR                                                
         BE    FILTR06                                                          
         CLC   CURROFF,0(RE)                                                    
         BNE   FILTRRJX                                                         
         B     FILTR10                                                          
*                                                                               
FILTR06  CLC   CURROFF,0(RE)                                                    
         BE    FILTRRJX                                                         
         B     FILTR10                                                          
*                                                                               
FILTR08  OC    OOFFICVL,OOFFICVL   OFFICE CODE FILTER?                          
         BZ    FILTR10                                                          
         USING ACTRECD,RE                                                       
         L     RE,AIO1             OFF SEC FOR SJ                               
         MVC   IOKEY,0(RE)                                                      
         LA    RE,IOKEY                                                         
         MVC   ACTKEY+ACTKEND(L'ACTKEY-ACTKEND),SPACES                          
         GOTO1 AIO,IORD+IOACCDIR+IO1                                            
         BNE   FILTRRJX                                                         
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BNE   FILTRRJX                                                         
         GOTO1 AOFFACC                                                          
         MVC   IOKEY,KEYSAVE       REREAD LAST RECORD                           
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1                                          
         BNE   FILTRRJX                                                         
         LA    RF,IOGET+IOACCMST+IO1                                            
         LA    RE,IOKEY            IS RECORD ON ARCHIVE FILE                    
         TM    RNSPSTA-RNSPASD(RE),TRNSARCH                                     
         BZ    *+8                 NO                                           
         LA    RF,IOGET+IOACCARC+IO1   YES THEN READ ARCHIVE NOT MST            
         GOTO1 AIO,(RF)                                                         
         BNE   FILTRRJX                                                         
         L     R3,AOFFBLK          SET OFFICE FROM OFFBLK                       
         MVC   CURROFF,OFFAOFFC-OFFALD(R3)                                      
         OC    CURROFF,SPACES                                                   
         L     R3,AIO1                                                          
         B     FILTR04                                                          
         DROP  RE                                                               
*                                                                               
FILTR10  OC    OAMOUNT,OAMOUNT     FILTERING TRANSACTION AMOUNT?                
         BZ    FILTR20                                                          
         CP    OAMTVL,TRNAMNT      FILTER AMOUNT=TRANS AMOUNT?                  
         BE    FILTR18                                                          
         CLI   OAMTSIGN,0          HAS A '+' OR '-' BEEN SPECIFIED?             
         BNE   FILTR12                                                          
         CLI   OAMTRNG,0           HAS A '>' OR '<' BEEN SPECIFIED?             
         BNE   FILTR12                                                          
         ZAP   TEMPNUM,OAMTVL      SEE IF AMOUNT IS -VE EQUIVALENT              
         AP    TEMPNUM,TRNAMNT                                                  
         BZ    FILTR18             YES IT IS                                    
         B     FILTR16                                                          
FILTR12  CLI   OAMTRNG,C'>'        MORE THAN SIGN?                              
         BNE   FILTR14                                                          
         CP    OAMTVL,TRNAMNT                                                   
         BL    FILTR18                                                          
         B     FILTR16                                                          
FILTR14  CLI   OAMTRNG,C'<'        LESS THAN SIGN?                              
         BNE   FILTR16                                                          
         CP    OAMTVL,TRNAMNT                                                   
         BH    FILTR18                                                          
FILTR16  CLI   OAMTFI,NEGFILTR     NEGATIVE FILTER SPECIFIED?                   
         BNE   FILTRRJX                                                         
         B     FILTR20                                                          
FILTR18  CLI   OAMTFI,NEGFILTR     NEGATIVE FILTER SPECIFIED?                   
         BE    FILTRRJX                                                         
*                                                                               
FILTR20  OC    OBATCH,OBATCH       BATCH REF FILTER?                            
         BZ    FILTR22                                                          
         GOTO1 ASCOMP,DMCB,TRNBTCH,(OBATLN1,OBATVL1),(OBATLN2,OBATVL2),C        
               OBATFI                                                           
         BNE   FILTRRJX                                                         
*                                                                               
FILTR22  CLI   ODRAFT,C'Y'         DRAFT OPTION                                 
         BE    FILTR30                                                          
         CLI   ODRAFT,C'N'                                                      
         BNE   FILTR24                                                          
         TM    TRNRSTAT,TRNSDRFT   SKIP DRAFT TRXS                              
         BZ    FILTR30                                                          
         B     FILTRRJX                                                         
*                                                                               
FILTR24  CLI   ODRAFT,C'O'                                                      
         BNE   FILTR30                                                          
         TM    TRNRSTAT,TRNSDRFT   SKIP LIVE TRXS                               
         BZ    FILTRRJX                                                         
*                                                                               
FILTR30  DS    0H                  NO MORE YET                                  
*                                                                               
FILTRX   CR    RB,RB                                                            
         B     XIT                                                              
FILTRRJX LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
*        BUILD TSAR RECORDS AND DISPLAY IF REQUIRED                   *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R3                                                       
         USING TRNELD,R5                                                        
BLDDIS   NTR1                                                                   
         LA    R5,TRNRFST                                                       
         L     R0,ATSARREC         CLEAR TSAR RECORD                            
         LHI   R1,TSARRECL                                                      
         XR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ATSARREC         R2=A(TSAR RECORD)                            
         USING TSARRECD,R2                                                      
         LA    R4,TSARDATA         R4=A(TSAR DATA)                              
         USING TSARDATD,R4                                                      
         LHI   RF,TSDLENQ          LENGTH OF REC FOR HIGHER LEVS                
         AHI   RF,TSARDATA-TSARRECD                                             
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO   RECORD NUMBER                                
         MVI   TSDFMT,TSITEM1      SCREEN DATA ITEM 1                           
*                                                                               
         MVC   TSDTACT,TRNKULA                                                  
         MVC   TSDTCAC,TRNKULC                                                  
         MVC   TSDOFWC,TRNOFFC                                                  
         MVC   TSDDATE,TRNKDATE                                                 
         MVC   TSDBTYP,TRNTYPE                                                  
         MVC   TSDBTCH,TRNBTCH                                                  
         MVC   TSDTRDA,DADDRESS                                                 
         ZAP   TSDAMNT,TRNAMNT                                                  
         MVC   TSDDCIN,TRNSTAT                                                  
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRID?                     
         BZ    BLDD02                                                           
         GOTO1 AFGRTSAR,(RC)       FORMAT TSAR ONTO GRID SCREEN LINES           
         B     BLDD04                                                           
*                                                                               
BLDD02   BAS   RE,FORMTSAR         FORMAT TSAR ONTO DUMMY SCREEN LINES          
*                                                                               
BLDD04   MVC   TSDLINES,LINSUSED   NUMBER OF SCREEN LINES USED                  
*                                                                               
         GOTO1 ATSARADD            ADD RECORD                                   
         BE    *+12                                                             
         TM    DISPFLAG,DISIOMAX                                                
         BNO   BLDTERRX                                                         
*                                                                               
         MVC   TSLSTREC,TSCURRNO   KEEP TRACK OF LAST TSAR REC NUMBER           
         TM    DISPFLAG,DISIOMAX                                                
         BO    BLDTERRX                                                         
         B     BLDOK                                                            
*                                                                               
BLDTERRX TM    DISPFLAG,DISIOMAX                                                
         BNO   BLDDERRX                                                         
BLDOK    TM    LSTINDS,LSTIRTN     TEST RETURN BACK TO LI                       
         BZ    BLDD14                                                           
         L     R1,ASVSES                   A(SAVED SESSION)                     
         CLC   TSCURRNO,SESCURNO-SESD(R1)  DO WE WANT IT?                       
         BNL   BLDD14                                                           
         MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUMBER            
         B     *+14                NO - GET NEXT                                
BLDD14   CLC   TSCURRNO,TSNEXTST   DO WE WANT TO DISPLAY THIS RECORD?           
         BNL   BLDD20              YES                                          
         XR    RF,RF               INCREMENT CURRENT TSAR REC NUMBER            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
         B     BLDD50                                                           
*                                                                               
BLDD20   TM    INTFLAG,SCRFULLQ    MAY BE FULL FROM HIGHER LEVELS               
         BO    BLDD50                                                           
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         BE    *+12                SCREEN FULL?                                 
         OI    INTFLAG,SCRFULLQ                                                 
         B     BLDD50                                                           
         MVC   TSLSTLIN,TSCURRNO   INCREMENT CURRENT TSAR REC NUMBER            
         XR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
*                                                                               
BLDD50   TM    DISPFLAG,DISIOMAX   MAX IO?                                      
         BO    BLDDERRX                                                         
         TM    INTFLAG,SCRFULLQ    SCREEN FULL?                                 
         BO    BLDDERRX                                                         
*                                                                               
BLDDX    CR    RB,RB                                                            
         B     XIT                                                              
BLDDERRX LTR   RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO DUMMY SCREEN LINES                 *         
***********************************************************************         
         SPACE 1                                                                
FORMTSAR NTR1                                                                   
         MVI   LINSUSED,0          NUMBER OF LINES DISPLAYED                    
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         L     R0,ADUMLINE         CLEAR DUMMY LINES                            
         LHI   R1,DUMLINLN                                                      
         XR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R2,ADUMLINE         R2=A(FIRST DUMMY SCREEN LINE)                
         L     R4,ATSARREC         R4=A(TSAR RECORD AREA)                       
         USING SCRLIN1D,R2         ACCOUNT DATA LINE                            
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARDATD,R4                                                      
         MVC   SCR1ACT,TSDTACT                                                  
         MVC   SCR1CAC,TSDTCAC                                                  
         MVC   SCR1OFF,TSDOFWC                                                  
         GOTO1 VDATCON,DMCB,(1,TSDDATE),(17,SCR1DATE)                           
*        CURED (B1,TSDBTYP),(L'SCR1BTYP,SCR1BTYP),0,ZERO=YES                    
         EDIT  (B1,TSDBTYP),(L'SCR1BTYP,SCR1BTYP),0,ZERO=YES                    
         MVC   SCR1BREF,TSDBTCH                                                 
         MVC   SCR1DCI,MX@CR                                                    
         TM    TSDDCIN,TRNSDR                                                   
         BZ    FORM02                                                           
         MVC   SCR1DCI,MX@DR                                                    
FORM02   CURED (P8,TSDAMNT),(L'SCR1AMNT,SCR1AMNT),2,MINUS=YES,ZERO=YES          
         MVI   LINSUSED,1                                                       
         CLI   TWAOFFC,C'*'        DDS?                                         
         BNE   FORMX                                                            
         XOUT  TSDTRDA,SCR1DA,4                                                 
         SPACE 1                                                                
FORMX    B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
OKXIT    CR    RB,RB                                                            
         B     XIT                                                              
ERRXIT   LTR   RB,RB                                                            
XIT      XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
AMTLNQ   EQU   17                                                               
         SPACE 1                                                                
DCMIX    DS    0X                                                               
         DCDDL AC#RNSPR,L'MX@RNSPR,L                                            
         DCDDL AC#TODAY,L'MX@TODAY,L                                            
         DCDDL AC#ENH38,L'MX@ENH38,L                                            
         DCDDL AC#SUBAC,L'MX@SUB,L                                              
         DCDDL AC#CR,L'MX@CR                                                    
         DCDDL AC#DR,L'MX@DR                                                    
         DCDDL AC#ACC,L'MX@ACC                                                  
         DCDDL AC#CTRA,L'MX@CTRA                                                
         DCDDL AC#OFF,L'MX@OFF                                                  
         DCDDL AC#DATE,L'MX@DATE                                                
         DCDDL AC#BATTY,L'MX@BATTY                                              
         DCDDL AC#RSBRF,L'MX@RSBRF                                              
         DCDDL AC#AMT,L'MX@AMT                                                  
         DCDDL AC#DRCR,L'MX@DRCR                                                
         DCDDL AC#REF,L'MX@REF                                                  
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
*        GRID COLUMN TABLE                                            *         
***********************************************************************         
COLTBL   DS    0F                                                               
*                                                                               
COL1     DC    AL1(COLACC)               COLUMN 1                               
COLACC   EQU   X'01'                                                            
         DC    AL2(MX@ACC-OVERWRKD)      ACCOUNT                                
         DC    AL1(L'MX@ACC-1,0,0,0,0,0)                                        
*                                                                               
COL2     DC    AL1(COLCAC)               COLUMN 2                               
COLCAC   EQU   X'02'                                                            
         DC    AL2(MX@CTRA-OVERWRKD)     CONTRA ACCOUNT                         
         DC    AL1(L'MX@CTRA-1,0,0,0,0,0)                                       
*                                                                               
COL3     DC    AL1(COLOFF)               COLUMN 3                               
COLOFF   EQU   X'03'                                                            
         DC    AL2(MX@OFF-OVERWRKD)      OFFICE                                 
         DC    AL1(L'MX@OFF-1,0,0,0,0,0)                                        
*                                                                               
COL4     DC    AL1(COLDAT)               COLUMN 4                               
COLDAT   EQU   X'04'                                                            
         DC    AL2(MX@DATE-OVERWRKD)     DATE                                   
         DC    AL1(L'MX@DATE-1,0,COLFDAT,COLARGHT,0,0)                          
*                                                                               
COL5     DC    AL1(COLBTY)               COLUMN 5                               
COLBTY   EQU   X'05'                                                            
         DC    AL2(MX@BATTY-OVERWRKD)    BATCH TYPE                             
         DC    AL1(L'MX@BATTY-1,0,0,0,0,0)                                      
*                                                                               
COL6     DC    AL1(COLBRF)               COLUMN 6                               
COLBRF   EQU   X'06'                                                            
         DC    AL2(MX@RSBRF-OVERWRKD)    BATCH REFERENCE                        
         DC    AL1(L'MX@RSBRF-1,0,0,0,0,0)                                      
*                                                                               
COL7     DC    AL1(COLAMT)               COLUMN 7                               
COLAMT   EQU   X'07'                                                            
         DC    AL2(MX@AMT-OVERWRKD)      AMOUNT                                 
         DC    AL1(L'MX@AMT-1,0,COLFNUM,COLARGHT,0,0)                           
*                                                                               
COL8     DC    AL1(COLDOC)               COLUMN 8                               
COLDOC   EQU   X'08'                                                            
         DC    AL2(MX@DRCR-OVERWRKD)     DEBIT CREDIT                           
         DC    AL1(L'MX@DRCR-1,0,0,0,0,0)                                       
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
*        FORMAT A TSAR RECORD INTO GRID SCREEN LINES                  *         
*                                                                     *         
* REGS USED: R3=A(TSAR DATA)                                          *         
*            R4=A(DOWNLOAD BLOCK)                                     *         
***********************************************************************         
         SPACE 1                                                                
FGRTSAR  CSECT                                                                  
         NMOD1 0,**FGRT**                                                       
         LR    RC,R1                                                            
         MVI   DISATRIB,0          DISPLAY ATTRIBUTES                           
         TM    STATFLAG,STPGRTOT   PRINT GRID TOTAL NEXT TIME IN                
         BNZ   FGRT010             NO - SHOW HEADINGS                           
         CLC   TSCURRNO,=H'1'      FIRST TSAR RECORD?                           
         BNE   FGRT010             NO - DON'T INITIALIZE GRID                   
         LR    R1,RC                                                            
         L     RF,=A(GRDINIT)                                                   
         A     RF,ORELO                                                         
         BASR  RE,RF               INITIALIZE GRID PRINT LINES                  
*                                                                               
FGRT010  MVI   LINSUSED,0          RESET NUMBER OF LINES USED                   
         L     R0,ADUMLINE         CLEAR DUMMY SCREEN LINES                     
         LHI   R1,DUMLINLN                                                      
         XR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R4,ADLCB            BUILD DOWNLOAD CONTROL BLOCK                 
         USING DLCBD,R4                                                         
         XC    DLCBD(DLCBL),DLCBD                                               
         LA    RF,FGRT300          DUMMY PRINT RTN                              
         ST    RF,DLCBAPR                                                       
         L     R2,ADUMLINE         RE=A(FIRST SCREEN LINE)                      
         ST    R2,DLCBAPL                                                       
         MVI   DLCBACT,DLCBSOR     INITIALIZE CALL                              
         GOTO1 VDLFLD,DLCBD        FIRST FOR SCREEN LINE                        
*                                                                               
         TM    STATFLAG,STPGRTOT                                                
         BNZ   FGRT030                                                          
*                                                                               
         L     R3,ATSARREC         R3=A(TSAR RECORD )                           
         LA    R3,TSARDATA-TSARRECD(R3)                                         
         USING TSARDATD,R3                                                      
         CLI   TSDFMT,TSITEMT      TOTAL LINE TYPE                              
         BE    FGRT030                                                          
*                                                                               
         MVC   DLCBFLD+1(L'TSDTACT),TSDTACT                                     
         BAS   RE,FGDLTXT          COLUMN 1: ACCOUNT CODE                       
         MVC   DLCBFLD(L'TSDTCAC),TSDTCAC                                       
         BAS   RE,FGDLTXT          COLUMN 2: CONTRA ACCOUNT CODE                
         MVC   DLCBFLD(L'TSDOFWC),TSDOFWC                                       
         BAS   RE,FGDLTXT          COLUMN 3: OFFICE (OR W/C)                    
         GOTO1 VDATCON,DMCB,(1,TSDDATE),(10,DLCBFLD)                            
         BAS   RE,FGDLTXT          COLUMN 4: DATE                               
*        CURED (B1,TSDBTYP),(3,DLCBFLD),0,ZERO=YES                              
         EDIT  (B1,TSDBTYP),(3,DLCBFLD),0,ZERO=YES                              
         BAS   RE,FGDLTXT          COLUMN 5: BATCH TYPE                         
         MVC   DLCBFLD(L'TSDBTCH),TSDBTCH                                       
         BAS   RE,FGDLTXT          COLUMN 6: BATCH REFERENCE                    
         CURED (P8,TSDAMNT),(AMTLNQ,DLCBFLD),2,MINUS=YES,ALIGN=LEFT,   C        
               ZERO=YES                                                         
         BAS   RE,FGDLNUM          COLUMN 7: AMOUNT                             
         MVC   DLCBFLD(L'MX@CR),MX@CR                                           
         TM    TSDDCIN,TRNSDR                                                   
         BZ    *+10                                                             
         MVC   DLCBFLD(L'MX@DR),MX@DR                                           
         BAS   RE,FGDLTXT          COLUMN 8: DEBIT OR CREDIT                    
*                                                                               
         MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 VDLFLD,DLCBD                                                     
         XR    RF,RF                                                            
         IC    RF,LINSUSED                                                      
         A     RF,DLCBTOTL         NUM OF LINES PUT TO DUMMY SCREEN             
         STC   RF,LINSUSED         UPDATE TOTAL NUM OF LINES                    
         B     FGRTXIT                                                          
*                                                                               
FGRT030  L     RF,ATSARREC         RF=A(TSAR RECORD )                           
         MVI   DLCBACT,DLCBEOR                                                  
         GOTO1 VDLFLD,DLCBD        END OF RECORD                                
         XR    RF,RF                                                            
         IC    RF,LINSUSED                                                      
         AHI   RF,1                                                             
         STC   RF,LINSUSED         UPDATE TOTAL NUM OF LINES                    
*                                                                               
FGRTXIT  XIT1                                                                   
*                                                                               
FGRT300  L     RF,DLCBAPL          BUMP TO NEXT DOWNLOAD PRINT LINE             
         LA    RF,L'DUMLIN1(,RF)                                                
         ST    RF,DLCBAPL                                                       
         BR    RE                                                               
         DROP  R3,R5                                                            
         SPACE 2                                                                
***********************************************************************         
*        BUILD DOWNLOAD DATA                                          *         
* ON ENTRY     R2=A(DUMMY SCREEN LINES)                               *         
*              R4=A(DOWNLOAD BLOCK)                                   *         
*              FOR FGDLTXT/FGDLNUM                                    *         
*                  - DLCBFLD = TEXT/NUMERIC DATA                      *         
*              FOR FGDLLTXT (PUT LONG TEXT DOWNLOAD FIELD)            *         
*                  - TEMP    = TEXT DATA UPTO 200 CHARACTERS          *         
*                  - MYBYTE1 = LENGTH OF TEXT DATA                    *         
***********************************************************************         
         SPACE 1                                                                
FGDLTXT  LR    R0,RE               SAVE RETURN REGITER                          
         MVI   DLCBTYP,DLCBTXT     TEXT DATA                                    
         B     FGDLPUT                                                          
*                                                                               
FGDLNUM  LR    R0,RE                                                            
         MVI   DLCBTYP,DLCBNUM     NUMERIC DATA                                 
         CLC   DLCBFLD,SPACES      ANY DATA                                     
         BH    FGDLPUT                                                          
         MVI   DLCBTYP,DLCBTXT     MOVE IN "" IF NOT APPLICABLE                 
         B     FGDLPUT                                                          
*                                                                               
FGDLLTXT LR    R0,RE               BUILD LONG TEXT FIELD                        
         MVI   DLCBTYP,DLCBTXT     TEXT DATA                                    
         XR    RF,RF                                                            
         ICM   RF,1,MYBYTE1                                                     
         BZ    FGDLPUT                                                          
         CHI   RF,L'DLCBFLD                                                     
         BH    FGLTXT02                                                         
         MVC   DLCBFLD,TEMP        MOVE DATA TO DOWNLOAD FIELD                  
         B     FGLTXT04                                                         
*                                                                               
FGLTXT02 CHI   RF,L'DUMLIN1-4      MAKE SURE IT'S NOT TOO LONG                  
         BH    FGLTXT06            YES - DON'T CALL DLFLD                       
         OI    DLCBFLG1,DLCBFXFL   USE EXTENDED FIELD                           
         MVC   DLCBFLX,TEMP        MOVE DATA TO EXTENDED FIELD                  
                                                                                
FGLTXT04 STC   RF,DLCBLEN          SET LENGTH                                   
         B     FGDLPUT                                                          
*                                                                               
FGLTXT06 LA    RE,TEMP             HANDLE LONG FIELD                            
FGLTXT08 CLI   0(RE),C'"'          TEST IF (EOTCHR) DEFINED                     
         BNE   *+8                                                              
         MVI   0(RE),C''''         YES REPLACE BY ALTERNATE CHAR                
         LA    RE,1(,RE)                                                        
         BCT   RF,FGLTXT08                                                      
*                                                                               
         L     RE,DLCBAPL          CURRENT DOWNLOAD PRINT LINES                 
         AH    RE,DLCBNUMC         RE=A(NEXT AVAIL CHR IN PRINT LINE)           
         MVI   0(RE),C'"'                                                       
         IC    RF,MYBYTE1                                                       
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   1(0,RE),TEMP        MOVE IN FIELD                                
         LA    RE,1+1(RF,RE)       END OF FIELD                                 
         MVI   0(RE),C'"'                                                       
         L     RF,DLCBAPL          CURRENT DOWNLOAD PRINT LINES                 
         SR    RE,RF                                                            
         LA    RE,2(,RE)           TOTAL LENGTH FROM START OF LINE              
         L     RF,DLCBTOTC         BUMP TOTAL NUMBER OF CHARS                   
         AR    RF,RE                                                            
         ST    RF,DLCBTOTC                                                      
         SRDL  RE,32                                                            
         D     RE,=A(L'DUMLIN1)                                                 
         STH   RE,DLCBNUMC         BUMP NUM OF CHARS THIS LINE                  
         MVC   DLCBNUMF,=H'1'      BUMP NUM OF FIELDS THIS LINE                 
         L     RE,DLCBTOTL         BUMP TOTAL NUMBER OF LINES                   
         AR    RE,RF                                                            
         ST    RE,DLCBTOTL                                                      
         MHI   RF,L'DUMLIN1                                                     
         L     RE,DLCBAPL          BUMP NUM OF DOWNLOAD PRINT LINES             
         AR    RE,RF                                                            
         ST    RE,DLCBAPL                                                       
         LR    RE,R0               RESTORE RETURN ADDR.                         
         BR    RE                                                               
*                                                                               
FGDLPUT  MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD,DLCBD                                                     
         NI    DLCBFLG1,X'FF'-DLCBFXFL                                          
         LR    RE,R0               RESTORE RETURN ADDR.                         
         BR    RE                                                               
         DROP  R4                                                               
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* INITIALIZE SCREEN LINES - BUILD GRID DETAIL LINES AND HEADERS       *         
* ON ENTRY     R2=A(NEXT AVAILABLE SCREEN LINE)                       *         
* ON EXIT      R2=A(NEXT AVAILABLE SCREEN LINE)                       *         
*              'LINSUSED' UPDATED WITH NUMBER OF LINES USED           *         
* REGS USED: R2=A(DUMMY SCREEN LINES)                                 *         
*            R3=A(COLUMN TABLE)                                       *         
*            R4=A(DOWNLOAD BLOCK)                                     *         
***********************************************************************         
         SPACE 1                                                                
GRDINIT  CSECT                                                                  
         NMOD1 0,**GINI**                                                       
         LR    RC,R1                                                            
         MVI   LINSUSED,0          RESET NUMBER OF LINES USED                   
         L     R0,ADUMLINE         CLEAR DUMMY SCREEN LINES                     
         LHI   R1,DUMLINLN                                                      
         XR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,ADUMLINE         RE=A(FIRST SCREEN LINE)                      
*        MVI   MYBYTE2,L'INFTXT-1                                               
*        GOTO1 ABLDDOR,DMCB,(R2),(L'MX@REF,MX@REF),(MYBYTE2,INFTXT)             
         GOTO1 ABLDDOR,DMCB,(R2),(L'MX@REF,MX@REF),0                            
*                                                                               
         L     R4,ADLCB            BUILD DOWNLOAD CONTROL BLOCK                 
         USING DLCBD,R4                                                         
         XC    DLCBD(DLCBL),DLCBD                                               
         LA    RF,GRDI40           DUMMY PRINT RTN FOR INIT                     
         ST    RF,DLCBAPR                                                       
         ST    R2,DLCBAPL                                                       
         MVI   DLCBACT,DLCBSOR     INITIALIZE CALL                              
         GOTO1 VDLFLD,DLCBD        FIRST FOR REPORT                             
*                                                                               
         L     R3,ACOLTBL          R3=A(COLUMN TABLE)                           
         USING COLTBLD,R3                                                       
GRDI10   CLI   COLETRY,EOT         END OF TABLE                                 
         BE    GRDIX                                                            
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         XR    RE,RE                                                            
         ICM   RE,3,COLNAME        RE=COLUMN NAME IN STORAGE                    
         LA    RE,OVERWRKD(RE)                                                  
         XR    RF,RF                                                            
         IC    RF,COLNMLN          LENGTH OF COLUMN - 1                         
         EX    RF,*+4                                                           
         MVC   DLCBFLD(0),0(RE)    MOVE COLUMN HEADING                          
         LA    R5,DLCBFLD(RF)      END OF COLUMN HEADING                        
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVI   1(R5),C'*'                                                       
*                                                                               
         GOTO1 VHEXOUT,DMCB,COLETRY,2(R5),L'COLETRY,0                           
*                                                                               
         OC    COLINF(COLINFLQ),COLINF TEST ANY COLUMN INFO.                    
         BZ    GRDI28                  NO - CALL DLFLD                          
         MVI   4(R5),C'*'                                                       
         LA    R5,5(,R5)                                                        
         LA    RF,COLINF           COLUMN INFO.                                 
         LA    RE,COLINFLQ         LENGTH OF COLUMN INFO.                       
GRDI20   CLI   0(RF),0                                                          
         BE    *+14                                                             
         MVC   0(L'COLINF,R5),0(RF)                                             
         LA    R5,L'COLINF(,R5)                                                 
         LA    RF,L'COLINF(,RF)                                                 
         BCT   RE,GRDI20                                                        
GRDI28   GOTO1 VDLFLD,DLCBD                                                     
*                                                                               
GRDI30   LA    R3,COLLN1Q(,R3)     NEXT COLUMN                                  
         B     GRDI10                                                           
*                                                                               
GRDI40   L     RF,DLCBAPL          BUMP TO NEXT DOWNLOAD PRINT LINE             
         LA    RF,L'DUMLIN1(,RF)                                                
         ST    RF,DLCBAPL                                                       
         BR    RE                                                               
*                                                                               
GRDIX    MVI   DLCBACT,DLCBEOL     END OF HEADING TEXT LINE                     
         GOTO1 VDLFLD,DLCBD                                                     
         L     RF,DLCBTOTL             NUM OF HEADING LINES                     
         MHI   RF,L'DUMLIN1            RF=LENGTH OF HEADINGS                    
         L     R2,ADUMLINE             R2=A(FIRST SCREEN LINE)                  
         XR    R1,R1                                                            
         IC    R1,GRDNDOR                                                       
         MHI   R1,L'DUMLIN1                                                     
         LA    R2,0(R1,R2)             R2=A(FIRST HEADING LINE)                 
         GOTO1 VSQASHER,DMCB,(R2),(RF) SQUASH THE HEADINGS                      
         ICM   RF,15,DMCB+4                                                     
         LA    RF,2(,RF)           RF=NEW LENGTH OF HEADINGS                    
         XR    RE,RE                                                            
         D     RE,=A(L'DUMLIN1)                                                 
         XR    R1,R1                                                            
         IC    R1,GRDNDOR          NO. OF DETAIL OF REQ. LINES                  
         LA    RF,0(R1,RF)                                                      
         LTR   RE,RE               ANY REMAINDER?                               
         BZ    *+8                                                              
         LA    RF,1(,RF)           RF=NEW TOTAL NUMBER OF LINES                 
         STC   RF,LINSUSED         UPDATE TOTAL NUM OF LINES                    
*                                                                               
         GOTO1 ADISPLAY,DISATRIB   DISPLAY DUMMY SCREEN LINES ON SCREEN         
         XIT1                                                                   
         DROP  R3,R4                                                            
*&&                                                                             
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
OVERWRKD DSECT                                                                  
ORELO    DS    A                   OVERLAY RELOCATION                           
AFGRTSAR DS    A                   FORMAT TSAR RECORD FOR GRID                  
ACOLTBL  DS    A                   A(GRID COLUMN TABLE)                         
*                                                                               
BASERD   DS    F                                                                
*                                                                               
RNGSTA   DS    CL8                                                              
RNGEND   DS    CL8                                                              
LASTLOAD DS    CL8                                                              
MYTEMP   DS    XL64                                                             
REFNUM   DS    CL6                                                              
TEMPNUM  DS    PL8                                                              
DADDRESS DS    XL4                                                              
SKIPIND  DS    XL1                                                              
CURROFF  DS    CL2                                                              
MYBYTE1  DS    XL1                                                              
MYBYTE2  DS    XL1                                                              
*                                                                               
INTFLAG  DS    X     INTERNAL FLAG                                              
SCRFULLQ EQU   1     SCREEN IS FULL                                             
         SPACE 1                                                                
DSMIX    DS    0C                                                               
MX@RNSPR DS    CL50                                                             
MX@TODAY DS    CL5                                                              
MX@ENH38 DS    CL(L'LSTDAT2)                                                    
MX@SUB   DS    CL3                                                              
MX@CR    DS    CL4                                                              
MX@DR    DS    CL4                                                              
MX@ACC   DS    CL13                                                             
MX@CTRA  DS    CL16                                                             
MX@OFF   DS    CL7                                                              
MX@DATE  DS    CL6                                                              
MX@BATTY DS    CL11                                                             
MX@RSBRF DS    CL13                                                             
MX@AMT   DS    CL7                                                              
MX@DRCR  DS    CL13                                                             
MX@REF   DS    CL12                                                             
         EJECT                                                                  
SCRLIN1D DSECT                     COVER SCREEN ITEM LINE1                      
SCR1ACT  DS    CL14                ACCOUNT                  +04                 
         DS    CL1                                                              
SCR1CAC  DS    CL14                CONTRA ACCOUNT           +19                 
         DS    CL1                                                              
SCR1OFF  DS    CL2                 OFFICE OR WORK CODE      +34                 
         DS    CL1                                                              
SCR1DATE DS    CL8                 REF DATE                 +37                 
         DS    CL1                                                              
SCR1BTYP DS    CL3                 BATCH TYPE               +46                 
         DS    CL1                                                              
SCR1BREF DS    CL6                 FULL BATCH REF           +50                 
         DS    CL1                                                              
SCR1AMNT DS    CL11                AMOUNT                   +57                 
         DS    CL1                                                              
SCR1DCI  DS    CL1                 CREDIT/DEBIT             +69                 
         DS    CL1                                                              
SCR1DA   DS    CL8                 DDS ONLY D/A             +71                 
SCR1LNQ  EQU   *-SCRLIN1D                                                       
         SPACE 1                                                                
TSARDATD DSECT                     TSAR DATA ITEM                               
TSDLINES DS    CL1                 NUMBER OF SCREEN LINES USED                  
TSDFMT   DS    CL1                 ITEM FORMAT TYPE                             
TSITEM1  EQU   1                   ITEM 1 FORMAT                                
TSITEMT  EQU   2                   TOTAL FORMAT                                 
TSDTACT  DS    CL(L'TRNKULA)                                                    
TSDTCAC  DS    CL(L'TRNKULC)                                                    
TSDOFWC  DS    CL(L'TRNOFFC)                                                    
TSDDATE  DS    XL(L'TRNKDATE)                                                   
TSDBTYP  DS    XL1                                                              
TSDBTCH  DS    CL(L'TRNBTCH)                                                    
TSDTRDA  DS    XL4                 D/A                                          
TSDDCIN  DS    XL1                                                              
TSDAMNT  DS    PL8                                                              
TSDLENQ  EQU   *-TSARDATD                                                       
         SPACE 1                                                                
COLTBLD  DSECT                                                                  
COLETRY  DS    XL1                 COLUMN NUMBER (FROM 1 TO 255, 0=EOT)         
COLNAME  DS    AL2                 A(COLUMN NAME)                               
COLNMLN  DS    AL1                 LENGTH OF COLUMN - 1                         
COLINDS  DS    AL1                 COLUMN INDICATOR                             
COLINF   DS    0CL1                COLUMN INFORMATION                           
COLIFRM  DS    CL1                 COLUMN FORMAT - TEXT DEFAULT                 
COLFDAT  EQU   C'D'                                DATE                         
COLFNUM  EQU   C'N'                                NUMERIC                      
COLIALGN DS    CL1                 ALIGN - LEFT DEFAULT                         
COLARGHT EQU   C'R'                        RIGHT                                
COLINAM  DS    CL1                 COLUMN NAME -                                
COLNRENM EQU   C'F'                              NO COLUMN RENAME               
COLITOT  DS    CL1                 COLUMN TOTAL -                               
COLNOTOT EQU   C'G'                               NO TOTAL                      
COLINFLQ EQU   *-COLINF                                                         
COLLN1Q  EQU   *-COLTBLD           LENGTH OF ENTRY                              
         EJECT                                                                  
* ACENQWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACENQWORK                                                      
         PRINT ON                                                               
         SPACE 2                                                                
TWAD     DSECT                                                                  
         ORG   OSSAVE              OVERLAY SAVE AREA                            
STATFLAG DS    X                   STATEMENT FLAG                               
STPGRTOT EQU   X'08'               PRINT GRID TOTAL                             
*&&DO                                                                           
MYKEYSV  DS    XL(RNSPBTY-RNSPASD)                                              
*&&                                                                             
INFTXT   DS    CL(L'ENQDAT1)                                                    
LDGOPTAB DS    20XL6                                                            
OSSNDQ   DS    XL(L'OSSAVE-(*-OSSAVE)) SPARE OVERLAY SAVE AREA                  
OSSAVEX  DS    0H                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018ACENQ20   09/20/07'                                      
         END                                                                    
