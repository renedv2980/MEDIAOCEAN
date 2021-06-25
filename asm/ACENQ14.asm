*          DATA SET ACENQ14    AT LEVEL 009 AS OF 01/07/15                      
*PHASE T62014C                                                                  
T62014   TITLE 'FIS - INVOICE CASHFLOW'                                         
T62014   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NMOD1 0,*ENQ14**,R7,CLEAR=YES,RR=RE                                    
         USING TWAD,RA                   RA=A(TWA)                              
         USING WORKD,R9                  R9=A(GLOBAL WORKING STORAGE)           
         LHI   RC,OVERWORK-WORKD                                                
         LA    RC,WORKD(RC)              R8=A(LOCAL WORKIN STORAGE)             
         USING OVERWRKD,RC                                                      
         ST    RE,ORELO                                                         
*                                                                               
IK       USING TRNRECD,SVINVKEY                                                 
CK       USING TRNRECD,SVCHGKEY                                                 
BK       USING TRNRECD,SVBILKEY                                                 
RK       USING TRNRECD,SVRCVKEY                                                 
*                                                                               
         L     RE,=A(GCTBL)              SET UP GRID COLUMN TABLE               
         A     RE,ORELO                                                         
         ST    RE,AGCTBL                                                        
         L     RE,=A(GCTBL2)             SET UP GRID COLUMN TABLE               
         A     RE,ORELO                                                         
         ST    RE,AGCTBL2                                                       
         L     RE,=A(GRDSP)              SET UP GRID SPECIAL ROUTINE            
         A     RE,ORELO                                                         
         ST    RE,AGRDSP                                                        
*                                                                               
         GOTO1 VDICTATE,DMCB,C'LL  ',DCMIX,DSMIX                                
*                                                                               
         TM    DISPFLAG,NOTFRSTQ         FIRST TIME FOR DISPLAY?                
         BO    MAIN10                    . NO                                   
         BAS   RE,FSTDIS                 . YES, PERFORM FIRST DISPLAY           
         BNE   ERXIT                                                            
         OI    DISPFLAG,NOTFRSTQ         SET NOT FIRST TIME FLAG ON             
         B     MAIN50                                                           
*                                                                               
MAIN10   TM    OVRSTAT,OVRGDONE          GRIDS PROCESSING FINISHED?             
         BO    MAINXGX                                                          
         TM    DISPFLAG,ALLREADQ   ALL RECORDS READ?                            
         BO    MAIN20              . YES                                        
*                                                                               
         MVC   IOKEY,KEYSAVE                                                    
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    MAIN20                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         B     MAINX                                                            
*                                                                               
MAIN20   CLC   TSCURRNO,TSLSTREC   HAVE WE ALLREADY GOT RECORD IN TSAR?         
         BH    MAIN50              NO                                           
*                                                                               
         L     R0,ATSARREC         RESTORE DUMMY TSAR RECORD                    
         LHI   R1,TSARRECL                                                      
         L     RE,ASVTSAR                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         BAS   RE,FGRMTSAR                                                      
         B     MAIN65                                                           
*                                                                               
MAIN40   LA    R3,IOKEY                                                         
         GOTO1 AIO,IOSEQ+IOACCDIR+IO1                                           
         BE    MAIN50                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX         MAX IO'S                               
         B     MAINX                                                            
*                                                                               
MAIN50   LA    R3,IOKEY                                                         
         USING INVPASD,R3                                                       
         MVC   KEYSAVE,INVPKEY           SAVE KEY FOR SEQ RESTORE               
         CLC   INVPKEY(INVPINV-INVPKEY),IOKEYSAV                                
         BNE   MAIN70                                                           
*                                        CHECK IF SUBTOTAL NEEDED               
         TM    DETFLAG,DETDATAQ          DATA FOUND AND DISPLAYED               
         BZ    MAIN58                                                           
         CLC   INVPINV,SVINV                                                    
         BE    MAIN58                                                           
         TM    DETFLAG,DETMULTQ                                                 
         BZ    MAIN56                                                           
         BAS   RE,SUBTOT                 SUB SUB                                
         BNE   MAINX                                                            
*                                                                               
MAIN56   NI    DETFLAG,X'FF'-(DETDATAQ+DETMULTQ)                                
         ZAP   SUBTRN,=P'0'                                                     
         ZAP   SUBDISC,=P'0'                                                    
         ZAP   SUBBNET,=P'0'                                                    
         ZAP   SUBCPA,=P'0'                                                     
*                                                                               
MAIN58   BAS   RE,FILTKEY                APPLY FILTERING TO INVOICE KEY         
         BNE   MAIN40                    KEEP THIS RECORD?                      
*                                                                               
         MVC   DADDRESS,INVPKDA          GET DISK ADDRESS                       
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    MAIN60                                                           
         TM    IOERR,IOMAX               MAX IOS REACHED?                       
         BNO   *+12                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     MAINX                                                            
*                                                                               
         USING TRNRECD,R3                                                       
MAIN60   L     R3,AIO1                                                          
         CLI   TRNRFST,TRNELQ      TEST THIS IS A TRANSACTION RECORD            
         BNE   MAIN40                                                           
         MVC   SVINVKEY,0(R3)                                                   
         GOTO1 AOFFTRN                   APPLY SECURITY CHECK                   
         CLI   OFFLFLAG,0                                                       
         BNE   MAIN40                                                           
         BAS   RE,FILTER                 APPLY FILTERING TO TRANS REC           
         BNE   MAIN40                    WANT TO KEEP THIS RECORD?              
         BAS   RE,BLDDATA                BUILD TSAR RECORD                      
         BNE   MAIN68                    MUST RE-ESTABLISH SEQUENCE             
         TM    DISPFLAG,DISIOMAX                                                
         BO    MAINX                                                            
*                                                                               
MAIN65   GOTO1 ADISPLAY,DISATRIB         DISPLAY SCREEN LINES                   
         BNE   MAINX                     SCREEN IS FULL                         
         MVC   TSLSTLIN,TSCURRNO         INCREMENT CURRENT TSAR REC NUM         
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
*                                                                               
         L     R1,ATSARREC               R4=A(TSAR JUST DISPLAYED)              
         USING TSARRECD,R1                                                      
         CLI   TSARFMT,TSITM             LINE ITEM?                             
         BE    MAIN66                                                           
         CLI   TSARFMT,TSTOTITM          TOTAL LINE ITEM?                       
         BE    MAINX                                                            
*                                                                               
         NI    DETFLAG,X'FF'-(DETDATAQ+DETMULTQ)                                
         ZAP   SUBTRN,=P'0'                                                     
         ZAP   SUBDISC,=P'0'                                                    
         ZAP   SUBBNET,=P'0'                                                    
         ZAP   SUBCPA,=P'0'                                                     
*                                                                               
         TM    DISPFLAG,ALLREADQ                                                
         BO    MAIN78                                                           
         B     MAIN58                                                           
MAIN66   TM    DETFLAG,DETDATAQ                                                 
         BZ    *+8                                                              
         OI    DETFLAG,DETMULTQ                                                 
         OI    DETFLAG,DETDATAQ          DATA FOUND AND DISPLAYED               
*                                                                               
         LA    R1,TSARDATA-TSARRECD(R1)                                         
         USING TSARDATD,R1                                                      
         MVC   SVINV,TSDINVN                                                    
         DROP  R1                                                               
*                                                                               
MAIN68   MVC   IOKEY,KEYSAVE                                                    
         GOTO1 AIO,IOREAD+IOACCDIR+IO1   RE-ESTABLISH IO SEQUENCE               
         BE    MAIN40                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX         MAX IO'S                               
         B     MAINX                                                            
*                                                                               
MAIN70   OI    DISPFLAG,ALLREADQ         ALL RECORDS READ                       
         OC    TSLSTREC,TSLSTREC         ANY RECORDS DISPLAYED SO FAR?          
         BNZ   MAIN76                                                           
         OI    DISPFLAG,NORECQ           NO RECORDS TO DISPLAY                  
         B     MAINX                                                            
MAIN76   TM    DETFLAG,DETMULTQ                                                 
         BZ    MAIN78                                                           
         BAS   RE,SUBTOT                 DEAL WITH SUBTOTAL LINE                
         BNE   MAINX                                                            
MAIN78   BAS   RE,TOTAL                  DEAL WITH TOTAL LINE                   
*                                                                               
MAINX    B     OKXIT                                                            
MAINXGX  GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL)                                   
         GOTO1 ADISPLAY,DISATRIB         DISPLAY SCREEN LINES                   
         B     OKXIT                                                            
         EJECT                                                                  
***********************************************************************         
*              FIRST FOR DISPLAY FUNCTIONS                            *         
***********************************************************************         
FSTDIS   NTR1                                                                   
*                                                                               
         LA    R1,VCVALS                                                        
         LHI   R0,(VCVALLNQ/L'VCVALS)                                           
FSTD02   ZAP   0(L'VCVALS,R1),=P'0'                                             
         LA    R1,L'VCVALS(R1)                                                  
         BCT   R0,FSTD02                                                        
*                                                                               
         MVC   FVMSGNO,=AL2(EALDGINV)    FIRST POSSIBLE ERROR                   
*                                                                               
         MVI   DETFLAG,0                 INIT DETAIL FLAG                       
         MVI   NXTRMODE,0                NEXT RECODE MODE                       
         XC    SVCPJ,SVCPJ               JOB SAVED                              
         MVC   SVINV,SPACES              INVOICE SAVED                          
*                                                                               
         GOTO1 AUNITLDG,SPROUNIT         READ PRODUCTION UNIT/LEDG              
         CLI   LEDGTLVA,0                                                       
         BE    FSTDERR                                                          
         MVC   CLEN(L'CLEN+L'CPLEN+L'CPJLEN),LEDGTLVA LEDGER STRUCTURE          
*                                                                               
         LA    R2,BASKEYH                R2=A(KEY FIELD)                        
         USING FLDHDRD,R2                                                       
         ST    R2,FVADDR                                                        
         CLC   FLDDATA(L'SPROUL),SPROUL  PROD NOT ALLOWED                       
         BE    FSTDERR                                                          
         CLI   FLDDATA,C'S'              SUBSIDIARY LEDGER                      
         BNE   FSTDERR                                                          
         GOTO1 AUNITLDG,FLDDATA          READ UNIT/LEDGER RECORDS               
         BNE   FSTDERR                                                          
*                                                                               
         CLC   =C'SV',FLDDATA            RIGHT NOW TYPE VC IS ONLY              
         BE    FSTD10                    AVAILABLE FOR THESE LEDGERS            
         CLC   =C'SW',FLDDATA                                                   
         BE    FSTD10                                                           
         CLC   =C'SX',FLDDATA                                                   
         BE    FSTD10                                                           
         CLC   =C'SY',FLDDATA                                                   
         BNE   FSTDERR                                                          
*                                                                               
FSTD10   MVC   FVMSGNO,=AL2(EAIFNTFN)    ACCOUNT NOT FOUND                      
         GOTO1 AGETACC,0                                                        
         BNE   FSTDERR                                                          
*                                                                               
         GOTO1 ADISACC                   DISPLAY ACCOUNT INFORMATION            
         GOTO1 ASCRNDIM,DMCB,(0,(R2))    # OF SCREEN LINES AVAILABLE            
         L     RF,ADISPFK                                                       
         BASR  RE,RF                     DISPLAY PFKEY LINE                     
*                                                                               
         USING OPTVALSD,RF                                                      
         L     RF,AOPTVALS         R2=A(OPTION VALUES)                          
         CLI   OREVERSE,0                                                       
         BNE   FSTD20                                                           
         CLI   PREVS,C'Y'          PROFILE TO SHOW REVERSALS                    
         BNE   FSTD20                                                           
         MVI   OREVERSE,C'Y'                                                    
         DROP  RF                                                               
*                                                                               
FSTD20   XC    IOKEY,IOKEY                                                      
         LA    R3,IOKEY                                                         
         USING INVPASD,R3                                                       
         MVI   INVPTYP,INVPTYPQ          INVOICE NUMBER PASSIVE POINTER         
         MVC   INVPCPY,MYCO              COMPANY                                
         MVC   INVPLDG,FLDDATA+1         LEDGER                                 
         MVC   INVPACT,FLDDATA+2         SUPPLIER ACCOUNT CODE                  
         OC    INVPACT,SPACES                                                   
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1                                          
         BNE   FSTDERR                                                          
*                                                                               
FSTDX    J     OKXIT                                                            
FSTDERR  J     ERXIT                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        FILTER TRANSACTION KEYS (ACCDIR)                             *         
* ON ENTRY R3=A(TRANSACTION KEY)                                      *         
* ON EXIT  CC IS SET TO EQUAL IF WE WANT RECORD                       *         
*          CC IS SET TO UNEQUAL IF RECORD IS REJECTED                 *         
***********************************************************************         
         USING OPTVALSD,R2                                                      
         USING INVPASD,R3                                                       
FILTKEY  NTR1                                                                   
         L     R2,AOPTVALS               R2=A(OPTION VALUES)                    
*                                                                               
         OC    OSERIAL,OSERIAL           INVOICE NUMBER FILTER ?                
         BZ    FILTK10                                                          
         GOTO1 ASCOMP,DMCB,INVPINV,(OSERLN1,OSERVL1),                  X        
               (OSERLN2,OSERVL2),OSERFI                                         
         BNE   FILTKRJX                                                         
*                                                                               
FILTK10  OC    ODATE,ODATE               FILTERING ON TRANSACTION DATE?         
         BZ    FILTKX                                                           
         GOTO1 ADCOMP,DMCB,(L'INVPDAT,INVPDAT),ODATEST,ODATEEN,ODATEFI          
         BNE   FILTKRJX                                                         
*                                                                               
FILTKX   J     OKXIT                                                            
FILTKRJX J     ERXIT                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        FILTER ACCMST RECORD                                         *         
* ON ENTRY AIO1 CONTAINS ACCMST RECORD                                *         
* ON EXIT  CC IS SET TO EQUAL IF WE WANT RECORD                       *         
*          CC IS SET TO UNEQUAL IF RECORD IS REJECTED                 *         
***********************************************************************         
         USING OPTVALSD,R2                                                      
         USING TRNRECD,R3                                                       
         USING TRNELD,R4                                                        
FILTER   NTR1                                                                   
         L     R2,AOPTVALS               R2=A(OPTION VALUES)                    
         LA    R4,TRNRFST                R3=A(1ST ELEM OF TRANS RECORD)         
         GOTO1 ASETELE,TRNRFST           SET ELEMENT ADDRESSES                  
*                                                                               
         TM    TRNSTAT,TRNSDR                                                   
         BO    FILTRJX                                                          
*                                                                               
         USING TRSELD,R1                                                        
         SR    R1,R1                                                            
         ICM   R1,15,ATRSELD             Show only undisbursed                  
         BZ    FILTRJX                                                          
         OC    TRSUDAT,TRSUDAT           Any Used date?                         
         BNZ   FILTRJX                   . Yes, reject disbursed item           
         DROP  R1                                                               
*------------------------------                                                 
* Draft transactions                                                            
*------------------------------                                                 
         TM    TRNRSTAT,TRNSDRFT         DRAFT?                                 
         BZ    FILT02                                                           
         CLI   ODRAFT,C'Y'                                                      
         BE    FILT40                                                           
         CLI   ODRAFT,C'O'                                                      
         BE    FILT40                                                           
         B     FILTRJX                                                          
FILT02   CLI   ODRAFT,C'O'                                                      
         BE    FILTRJX                                                          
*------------------------------                                                 
* Peeled transactions                                                           
*------------------------------                                                 
FILT40   TM    TRNRSTA2,TRNSPEEL         PEELED?                                
         BZ    FILT42                                                           
         CLI   OPEELED,C'Y'                                                     
         BE    FILT50                                                           
         CLI   OPEELED,C'O'                                                     
         BE    FILT50                                                           
         B     FILTRJX                                                          
FILT42   CLI   OPEELED,C'O'                                                     
         BE    FILTRJX                                                          
*------------------------------                                                 
* Contra account                                                                
*------------------------------                                                 
FILT50   SR    R1,R1                                                            
         ICM   R1,1,CONTLEN              R1=L'(CONTRA CODE)                     
         BZ    FILT60                                                           
         LA    RF,L'TRNKULC                                                     
         LA    RE,TRNKULC                                                       
FILT54   CLI   0(RE),C' '                FIND NON-BLANK CHARACTER               
         BH    FILT56                                                           
         LA    RE,1(,RE)                                                        
         BCT   RF,FILT54                                                        
         B     FILT58                                                           
FILT56   CR    R1,RF                     CONTRA FILTER LONGER ?                 
         BH    FILT58                                                           
         BCTR  R1,0                                                             
         EXCLC R1,CONTRA,0(RE)                                                  
         BNE   FILT58                                                           
         CLI   NEGCONT,NEGFILTR          NEGATIVE FILTER ?                      
         BNE   FILT60                                                           
         B     FILTRJX                                                          
FILT58   CLI   NEGCONT,NEGFILTR          NEGATIVE FILTER ?                      
         BNE   FILTRJX                                                          
*------------------------------                                                 
* Reference                                                                     
*------------------------------                                                 
FILT60   OC    OREF,OREF                                                        
         BZ    FILT70                                                           
         LA    RF,TRNKREF                RF=A(TRANSACTION REF)                  
         USING FFTELD,RE                                                        
         ICM   RE,15,AFFTELD             RE=A(FREE FORM TEXT ELEMENT)           
         BZ    FILT68                                                           
         SR    R0,R0                                                            
FILT64   CLI   FFTEL,0                                                          
         BNE   FILT68                                                           
         CLI   FFTEL,FFTELQ                                                     
         BNE   *+12                                                             
         CLI   FFTTYPE,FFTTKREF          KEY REF NUMBER FOR BANK VOID?          
         BE    FILT66                                                           
         IC    R0,FFTLN                                                         
         AR    RE,R0                                                            
         B     FILT64                                                           
FILT66   LA    RF,FFTDATA                                                       
         DROP  RE                                                               
FILT68   GOTO1 ASCOMP,DMCB,(RF),(OREFLN1,OREFVL1),(OREFLN2,OREFVL2),   X        
               OREFFI                                                           
         BNE   FILTRJX                                                          
*------------------------------                                                 
* Month                                                                         
*------------------------------                                                 
FILT70   OC    OMOS,OMOS                                                        
         BZ    FILT80                                                           
         GOTO1 ADCOMP,DMCB,(L'TRNRSMOS,TRNRSMOS),OMOSST,OMOSEN,OMOSFI           
         BNE   FILTRJX                                                          
*------------------------------                                                 
* Transaction Date                                                              
*------------------------------                                                 
FILT80   OC    ODATE,ODATE                                                      
         BZ    FILT90                                                           
         GOTO1 ADCOMP,DMCB,(L'TRNKDATE,TRNKDATE),ODATEST,ODATEEN,      X        
               ODATEFI                                                          
         BNE   FILTRJX                                                          
*------------------------------                                                 
* Transaction Status                                                            
*------------------------------                                                 
FILT90   TM    TRNSTAT,TRNSURG           URGENT?                                
         BZ    FILT96                                                           
         CLI   OURGENT,C'N'                                                     
         BE    FILTRJX                                                          
         B     FILT100                                                          
FILT96   CLI   OURGENT,C'O'                                                     
         BE    FILTRJX                                                          
*                                                                               
FILT100  TM    TRNSTAT,TRNSHOLD          HELD ?                                 
         BZ    FILT110                                                          
         CLI   OHELD,C'N'                                                       
         BE    FILTRJX                                                          
         B     FILT120                                                          
FILT110  CLI   OHELD,C'O'                                                       
         BE    FILTRJX                                                          
*                                                                               
FILT120  TM    TRNSTAT,TRNSAPPR          SELECTED ?                             
         BZ    FILT130                                                          
         CLI   OSELECT,C'N'                                                     
         BE    FILTRJX                                                          
         B     FILT140                                                          
FILT130  CLI   OSELECT,C'O'                                                     
         BE    FILTRJX                                                          
*------------------------------                                                 
* Batch Reference                                                               
*------------------------------                                                 
FILT140  OC    OBATCH,OBATCH                                                    
         BZ    FILT150                                                          
         GOTO1 ASCOMP,DMCB,TRNBREF,(OBATLN1,OBATVL1),(OBATLN2,OBATVL2),C        
               OBATFI                                                           
         BNE   FILTRJX                                                          
*------------------------------                                                 
* Input Type                                                                    
*------------------------------                                                 
FILT150  SR    RF,RF                                                            
         ICM   RF,1,OBTYPEVL                                                    
         BZ    FILT170                                                          
         SR    RE,RE                                                            
         IC    RE,TRNTYPE                                                       
         CR    RE,RF                                                            
         BE    FILT160                                                          
         CLI   OBTYPEFI,NEGFILTR                                                
         BNE   FILTRJX                                                          
         B     FILT170                                                          
*                                                                               
FILT160  CLI   OBTYPEFI,NEGFILTR                                                
         BE    FILTRJX                                                          
*------------------------------                                                 
* TRANSACTION FILTERS                                                           
*------------------------------                                                 
FILT170  MVI   FLTFLAG,0                 INITIALIZE OTHERS FLAG                 
         MVC   TEMPEXP,SPACES            INITIALIZE EXPENSE FIELD               
*                                                                               
         OC    OCLIENT,OCLIENT           FILTERING ON CLIENT ?                  
         BNZ   FILT180                                                          
         OC    OPROD,OPROD               FILTERING ON PRODUCT ?                 
         BNZ   FILT180                                                          
         OC    OJOB,OJOB                 FILTERING ON JOB ?                     
         BNZ   FILT180                                                          
         OC    OESTIMAT,OESTIMAT         FILTERING ON ESTIMATE ?                
         BNZ   FILT180                                                          
         OC    OSRC,OSRC                 FILTERING ON SOURCE ?                  
         BZ    FILT230                                                          
*                                                                               
FILT180  GOTO1 ABLDSRC                                                          
         OC    OCLIENT,OCLIENT           FILTERING ON CLIENT ?                  
         BZ    FILT190                                                          
         CLC   SPROUL,SRCWORK                                                   
         BNE   FILTRJX                                                          
         GOTO1 ASCOMP,DMCB,SRCWORK+L'SPROUL,(OCLINLN1,OCLINVL1),       X        
               (OCLINLN2,OCLINVL2),OCLINFI                                      
         BNE   FILTRJX                                                          
*                                                                               
FILT190  OC    OPROD,OPROD               FILTERING ON PRODUCT?                  
         BZ    FILT200                                                          
         CLC   SPROUL,SRCWORK                                                   
         BNE   FILTRJX                                                          
         GOTO1 ASCOMP,DMCB,SRCWORK+L'SPROUL,(OPRODLN1,OPRODVL1),       X        
               (OPRODLN2,OPRODVL2),OPRODFI                                      
         BNE   FILTRJX                                                          
*                                                                               
FILT200  OC    OJOB,OJOB                 FILTERING ON JOB?                      
         BZ    FILT210                                                          
         CLC   SPROUL,SRCWORK                                                   
         BNE   FILTRJX                                                          
         GOTO1 ASCOMP,DMCB,SRCWORK+L'SPROUL,(OJOBLN1,OJOBVL1),         X        
               (OJOBLN2,OJOBVL2),OJOBFI                                         
         BNE   FILTRJX                                                          
*                                                                               
FILT210  OC    OESTIMAT,OESTIMAT         FILTERING ON ESTIMATE?                 
         BZ    FILT220                                                          
         CLC   SPROUL,SRCWORK                                                   
         BNE   FILTRJX                                                          
         GOTO1 ASCOMP,DMCB,SRCWORK+L'SPROUL,(OESTLN1,OESTVL1),         X        
               (OESTLN2,OESTVL2),OESTFI                                         
         BNE   FILTRJX                                                          
*                                                                               
FILT220  OC    OSRC,OSRC                FILTERING ON SOURCE?                    
         BZ    FILT230                                                          
         CLC   SRCWORK,SPACES                                                   
         BE    FILTRJX                                                          
         GOTO1 ASCOMP,DMCB,SRCWORK,(OSRCLN1,OSRCVL1),(OSRCLN2,OSRCVL2),X        
               OSRCFI                                                           
         BNE   FILTRJX                                                          
*-----------------------------------                                            
* CHECK ELEMENTS ON TRANSACTION                                                 
*-----------------------------------                                            
FILT230  LA    R4,TRNRFST                R4=A(TRANSACTION ELEMENT)              
         MVC   TRANCURR,COMPCURR         SET TRANSACTION CURRENCY               
         ZAP   TRANAMNT,TRNAMNT          AGENCY TRANSACTION AMOUNT              
*                                                                               
FILT250  SR    R0,R0                                                            
         IC    R0,1(,R4)                                                        
         AR    R4,R0                                                            
*                                                                               
         CLI   0(R4),EOR                 END OF RECORD?                         
         BE    FILT390                                                          
         CLI   0(R4),TRSELQ              TRANSACTION STATUS ELEMENT             
         BE    FILT270                                                          
         CLI   0(R4),SPDELQ              SUBSIDIARY POSTING ELEMENT             
         BE    FILT280                                                          
         CLI   0(R4),APEELQ              ANALYSIS POINTER ELEMENT               
         BE    FILT290                                                          
         CLI   0(R4),DUEELQ              DUE DATE ELEMENT                       
         BE    FILT330                                                          
         CLI   0(R4),MXPELQ              MEDIA EXTRA PAYMENT ELEMENT            
         BE    FILT340                                                          
         CLI   0(R4),AFCELQ              FOREIGN CURRENCY ELEMENT               
         BE    FILT350                                                          
         CLI   0(R4),AFPELQ              ARTISTE FEE ELEMENT                    
         BE    FILT360                                                          
         CLI   0(R4),FFTELQ              FREE FORM TEXT ELEMENT                 
         BE    FILT380                                                          
         B     FILT250                                                          
*-----------------------------------                                            
* TRANSACTION STATUS ELEMENT X'60'                                              
*-----------------------------------                                            
         USING TRSELD,R4                                                        
FILT270  OC    OACT,OACT                 FILTERING ON ACTIVITY DATE?            
         BZ    FILT274                                                          
         OC    TRSDATE,TRSDATE                                                  
         BZ    FILTRJX                                                          
         GOTO1 VDATCON,DMCB,(2,TRSDATE),(1,TEMPDAT) ACTIVITY DATE               
         GOTO1 ADCOMP,DMCB,(L'TEMPDAT,TEMPDAT),OACTST,OACTEN,OACTFI             
         BNE   FILTRJX                                                          
*                                                                               
FILT274  TM    TRNRSTAT,TRNSREVS         REVERSAL?                              
         BZ    FILT278                                                          
         CLI   OREVERSE,C'Y'                                                    
         BE    FILT250                                                          
         CLI   OREVERSE,C'O'                                                    
         BE    FILT250                                                          
         OC    OMOS,OMOS                 FILTERING ON TRANSACTION MOS?          
         BZ    FILTRJX                                                          
         OC    TRSRMOS,TRSRMOS           NO REV MOS ASSUME SAME AS TRAN         
         BZ    FILTRJX                                                          
         GOTO1 ADCOMP,DMCB,(L'TRSRMOS,TRSRMOS),OMOSST,OMOSEN,OMOSFI             
         BE    FILTRJX                                                          
         B     FILT250                                                          
FILT278  CLI   OREVERSE,C'O'                                                    
         BE    FILTRJX                                                          
         B     FILT250                                                          
         DROP  R4                                                               
*-------------------------------                                                
* SUBSIDIARY POSTING X'4C'                                                      
*-------------------------------                                                
         USING SPDELD,R4                                                        
FILT280  SR    R1,R1                                                            
         IC    R1,SPDLN                                                         
         SHI   R1,SPDLN1Q                Get length of account                  
         BNP   FILT250                   . none found                           
         CHI   R1,L'TRNKULA              Make sure not too big                  
         BH    FILT250                   . Too big                              
         CLC   SEXPUL,SPDACCS            Make sure expense account              
         BNE   FILT250                   . not expense account                  
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMPEXP(0),SPDACCS                                               
         B     FILT250                                                          
*-------------------------------                                                
* ANALYSIS POSTING X'C0'                                                        
*-------------------------------                                                
         USING APEELD,R4                                                        
FILT290  SR    R1,R1                                                            
         ICM   R1,1,APENUM                                                      
         BZ    FILT250                                                          
         LA    R5,APELN1Q(R4)                                                   
         DROP  R4                                                               
         USING APENTRY,R5                                                       
FILT292  SR    R6,R6                                                            
         IC    R6,APENLEN                                                       
         CLC   SEXPUL,APENACT                                                   
         BE    FILT294                                                          
         AR    R5,R6                                                            
         BCT   R1,FILT292                                                       
         B     FILT250                                                          
FILT294  SHI   R6,APELN2Q                                                       
         BNP   FILT250                                                          
         SHI   R6,1                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   TEMPEXP(0),APENACT                                               
         B     FILT250                                                          
         DROP  R5                                                               
*-----------------------------------                                            
* DUE DATE ELEMENT X'61'                                                        
*-----------------------------------                                            
         USING DUEELD,R4                                                        
FILT330  OC    ODUE,ODUE                 FILTERING ON DUE DATE?                 
         BZ    FILT250                                                          
         GOTO1 VDATCON,DMCB,(2,DUEDATE),(1,TEMPDAT) ACTIVITY DATE               
         GOTO1 ADCOMP,DMCB,(L'TEMPDAT,TEMPDAT),ODUEST,ODUEEN,ODUEFI             
         BNE   FILTRJX                                                          
         OI    FLTFLAG,DUEELEQ           DUE DATE ELEMENT FOUND                 
         B     FILT250                                                          
         DROP  R4                                                               
*-----------------------------------                                            
* MEDIA EXTRA PAYEMENT ELEMENT                                                  
*-----------------------------------                                            
         USING MXPELD,R4                                                        
FILT340  SR    RF,RF                                                            
         ICM   RF,7,MXPSER+1                                                    
         BZ    FILT250                                                          
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(7,RE),DUB                                                      
         MVC   0(1,RE),MXPSER                                                   
         GOTO1 ASCOMP,DMCB,WORK,(OSERLN1,OSERVL1),(OSERLN2,OSERVL2),   X        
               OSERFI                                                           
         BNE   FILTRJX                                                          
         OI    FLTFLAG,MXPELEQ           MATCHING MEDIA SERIAL # FOUND          
         B     FILT250                                                          
         DROP  R4                                                               
*-----------------------------------                                            
* ACCOUNT FOREIGN CURRENCY ELEM                                                 
*-----------------------------------                                            
         USING AFCELD,R4                                                        
FILT350  TM    AFCXSTAT,AFCXSMEM         MEMO ITEM?                             
         BO    FILT250                                                          
         MVC   TRANCURR,AFCCURR          OVERRIDE TRANSACTION CURRENCY          
         B     FILT250                                                          
         DROP  R4                                                               
*-----------------------------------                                            
* ARTISTE FEE ELEMENT                                                           
*-----------------------------------                                            
         USING AFPELD,R4                                                        
FILT360  TM    FLTFLAG,AFPRQDQ           ARTISTE FEE ELE REQUIRED?              
         BZ    FILT250                                                          
         CLC   ORUNVL,AFPPAYNO           MATCH ON RUN NUMBER?                   
         BE    FILT370                                                          
         CLI   ORUNFI,NEGFILTR                                                  
         BNE   FILTRJX                                                          
         B     FILT375                                                          
FILT370  CLI   OBTYPEFI,NEGFILTR                                                
         BE    FILTRJX                                                          
FILT375  OI    FLTFLAG,AFPELEQ           ARTISTE FEE ELEMENT FOUND              
         B     FILT250                                                          
         DROP  R4                                                               
*-----------------------------------                                            
* FREE FORM TEXT ELEMENT X'DB'                                                  
*-----------------------------------                                            
         USING FFTELD,R4                 FREE FORM TEXT ELEMENT                 
FILT380  CLI   FFTTYPE,FFTTACUR          ASSOCIATED CURRENCY?                   
         BNE   FILT250                                                          
         MVC   TRANCURR,FFTDATA          OVERRIDE TRANSACTION CURRENCY          
         B     FILT250                                                          
         DROP  R4                                                               
*------------------------------                                                 
* Subsidiary Cash Info X'50'                                                    
*------------------------------                                                 
         USING SCIELD,R4                 SUBSIDIARY CASH ELEMENT                
FILT390  ICM   R4,15,ASCIELD                                                    
         BZ    FILT396                                                          
FILT392  CLI   SCIEL,SCIELQ                                                     
         BNE   FILT396                                                          
         CLI   SCITYPE,SCITCDSC          CASH DISCOUNT?                         
         BE    FILT394                                                          
         SR    RF,RF                                                            
         IC    RF,SCILN                                                         
         AR    R4,RF                                                            
         B     FILT392                                                          
FILT394  CLI   ODISC,C'N'                CASH DISCOUNT ITEMS REQUIRED?          
         BE    FILTRJX                                                          
         B     *+12                                                             
FILT396  CLI   ODISC,C'O'                ONLY DISCOUNT ITEMS REQUIRED?          
         BE    FILTRJX                                                          
         DROP  R4                                                               
*------------------------------                                                 
* EXPENSE CONTRA                                                                
*------------------------------                                                 
FILT400  CLC   SEXPUL,TRNKULC            EXPENSE CONTRA ACCOUNT                 
         BNE   *+10                      . NO                                   
         MVC   TEMPEXP,TRNKULC                                                  
*                                                                               
         CLC   TEMPEXP,SPACES            EXPENSE ACCOUNT?                       
         BNH   FILT408                   . NO                                   
         CLI   OEXP,C'N'                                                        
         BE    FILTRJX                                                          
         B     FILT408                                                          
FILT402  CLI   OEXP,C'O'                                                        
         BE    FILTRJX                                                          
*------------------------------                                                 
* AMOUNT FILTER                                                                 
*------------------------------                                                 
FILT408  OC    OAMOUNT,OAMOUNT           FILTERING TRANSACTION AMOUNT?          
         BZ    FILT450                                                          
         CP    OAMTVL,TRANAMNT           FILTER AMOUNT=TRANS AMOUNT?            
         BE    FILT440                                                          
         CLI   OAMTSIGN,0                '+' OR '-' SPECIFIED?                  
         BNE   FILT410                                                          
         CLI   OAMTRNG,0                 '>' OR '<' SPECIFIED?                  
         BNE   FILT410                                                          
         ZAP   TEMPNUM,OAMTVL            IS AMOUNT -VE EQUIVALENT               
         AP    TEMPNUM,TRANAMNT                                                 
         BZ    FILT440                   . YES IT IS                            
         B     FILT430                                                          
FILT410  CLI   OAMTRNG,C'>'              MORE THAN SIGN?                        
         BNE   FILT420                                                          
         CP    OAMTVL,TRANAMNT                                                  
         BL    FILT440                                                          
         B     FILT430                                                          
FILT420  CLI   OAMTRNG,C'<'              LESS THAN SIGN?                        
         BNE   FILT430                                                          
         CP    OAMTVL,TRANAMNT                                                  
         BH    FILT440                                                          
FILT430  CLI   OAMTFI,NEGFILTR           NEGATIVE FILTER SPECIFIED?             
         BNE   FILTRJX                                                          
         B     FILT450                                                          
FILT440  CLI   OAMTFI,NEGFILTR           NEGATIVE FILTER SPECIFIED?             
         BE    FILTRJX                                                          
FILT450  CP    TRANAMNT,=P'0'                                                   
         BNL   FILT460                                                          
         CLI   ONEGATIV,C'N'             ONLY WANT +VE NUMBERS/ZERO?            
         BE    FILTRJX                                                          
         B     FILT500                                                          
FILT460  CLI   ONEGATIV,C'O'             ONLY WANT -VE NUMBERS?                 
         BE    FILTRJX                                                          
*                                                                               
FILT500  TM    FLTFLAG,DUERQDQ           IS DUE ELEMENT REQUIRED ?              
         BZ    FILT510                   . NO, SKIP FOUND CHECK                 
         TM    FLTFLAG,DUEELEQ           HAS DUE ELEMENT BEEN FOUND ?           
         BZ    FILTRJX                   . NO, REJECT RECORD                    
FILT510  TM    FLTFLAG,MXPRQDQ           IS MXP ELEMENT REQUIRED ?              
         BZ    FILT520                   . NO, SKIP FOUND CHECK                 
         TM    FLTFLAG,MXPELEQ           HAS MXP ELEMENT BEEN FOUND ?           
         BZ    FILTRJX                   . NO, REJECT RECORD                    
FILT520  TM    FLTFLAG,AFPRQDQ           ARTISTE FEE ELEMENT REQUIRED           
         BZ    FILTX                     . NO, SKIP FOUND CHECK                 
         TM    FLTFLAG,AFPELEQ           HAS AFP ELEMENT BEEN FOUND ?           
         BZ    FILTRJX                   . NO, REJECT RECORD                    
*                                                                               
FILTX    B     OKXIT                                                            
FILTRJX  B     ERXIT                                                            
         DROP  R2,R3                                                            
***********************************************************************         
*        BUILD STORAGE DATA FOR SCREEN DISPLAY                                  
*        FILL DUMMY SCREEN LINES                                                
*        . ON ENTRY R3=A(AIO AREA CONTAINING TRANSACTION RECORD)                
*        . ON EXIT  CC EQUAL-     TSAR RECORD ADDED OK                          
*                   CC NOT EQUAL- TSAR BLOCK FULL                               
***********************************************************************         
INVP     USING INVPASD,KEYSAVE                                                  
         USING TRNRECD,R3                                                       
BLDDATA  NTR1                                                                   
         GOTO1 ASETELE,TRNRFST           SET ELEMENT ADDRESSES                  
*                                                                               
         L     R0,ATSARREC               CLEAR TSAR RECORD                      
         LHI   R1,TSARRECL                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         ZAP   UNAUNA,=P'0'              UNADJUSTED NUMBERS                     
         ZAP   UNAAPP,=P'0'              USED TO CHECK FOR FULL                 
         ZAP   UNAPBU,=P'0'              PAYMENT INDICATORS                     
         ZAP   UNAPBA,=P'0'                                                     
*                                                                               
         L     R2,ATSARREC               R2=A(TSAR RECORD)                      
         USING TSARRECD,R2                                                      
         MVC   TSARKYNO,TSCURRNO                                                
*                                                                               
         LA    R2,TSARDATA                                                      
         USING TSARDATD,R2                                                      
         MVI   TSDFMT,TSITM                                                     
         MVC   TSDINVN,INVP.INVPINV      INVOICE NUMBER                         
         MVC   TSDDATE,INVP.INVPDAT      INVOICE DATE                           
         MVC   TSDDA,INVP.INVPKDA                                               
         MVC   TSDSREF,TRNKSBR                                                  
         MVC   TSDTMOA,TRNRSMOS          MOA                                    
         MVC   TSDTRST,TRNRSTAT                                                 
         MVC   TSDEXP,TEMPEXP            EXPENSE ACCOUNT                        
*                                                                               
         MVC   TSDULC(2),TRNKULC                                                
         CLC   SPROUL,TSDULC             EXPENSE ACCOUNT?                       
         BNE   *+10                      . NO                                   
         MVC   TSDJCLI,TRNKCACT                                                 
*                                                                               
         ZAP   TSDBNET,=P'0'                                                    
         ZAP   TSDCPA,=P'0'                                                     
         ZAP   TSDPBA,=P'0'                                                     
         ZAP   TSDPBCPA,=P'0'                                                   
         ZAP   TSDCEST,=P'0'                                                    
         ZAP   TSDISC,=P'0'                                                     
         XC    TSDDTAPP,TSDDTAPP                                                
         XC    TSDACDAT,TSDACDAT         ACTIVITY DATE                          
         XC    TSDCSTAT,TSDCSTAT                                                
         XC    TSDRSTAT,TSDRSTAT         RECEIVABLE STATUS                      
         XC    TSDUNK,TSDUNK                                                    
         LA    R0,CFTABS                 CLEAR CASHFLOW TEMP TABLES             
         LHI   R1,CFTLNQ                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R3,TRNRFST                R3=A(1ST ELEM FOR TRANSACTION)         
         LA    R4,TSDRFST                R4=A(1ST ELEM FOR TSAR RECORD)         
         B     *+12                                                             
BLDT20   SR    R0,R0                     BUMP TO NEXT ELEMENT                   
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
         CLI   0(R3),EOR                 END OF RECORD?                         
         BE    BLDT420                                                          
         CLI   0(R3),OTHELQ              X'23' OTHERS ELEMENT                   
         BE    BLDT40                                                           
         CLI   0(R3),TRNELQ              X'44' TRANSACTION ELEMENT              
         BE    BLDT50                                                           
         CLI   0(R3),TRSELQ              X'60' TRANSACTION STATUS ELEM          
         BE    BLDT60                                                           
         CLI   0(R3),DUEELQ              X'61' DUE DATE ELEMENT                 
         BE    BLDT70                                                           
         CLI   0(R3),SCIELQ              X'50' SUBSIDIARY CASH ELEMENT          
         BE    BLDT80                                                           
         CLI   0(R3),SPDELQ              X'4C' SUBSIDIARY POSTING               
         BE    BLDT90                                                           
         B     BLDT20                                                           
*-------------------------------                                                
* OTHERS ELEMENT X'23'                                                          
*-------------------------------                                                
         USING OTHELD,R3                                                        
BLDT40   MVC   TSDJPRD,OTHNUM                                                   
         MVC   TSDJJOB,OTHNUM+6                                                 
         B     BLDT20                                                           
*-------------------------------                                                
* TRANSACTION ELEMENT X'44'                                                     
*-------------------------------                                                
         USING TRNELD,R3                                                        
BLDT50   ZAP   TSDAMNT,TRNAMNT           TRANSACTION AMOUNT                     
         MVC   TSDOFF,TRNOFFC            OFFICE                                 
         MVC   TSDBREF,TRNBREF           BATCH REFERENCE                        
         MVC   TSDTEST,TRNSTAT           TRANSACTION STATUS                     
         B     BLDT20                                                           
*-------------------------------                                                
* TRANSACTION STATUS ELEMENT                                                    
*-------------------------------                                                
         USING TRSELD,R3                                                        
BLDT60   MVC   TSDACDAT,TRSDATE          ACTIVITY DATE                          
         MVC   TSDSEST,TRSSTAT           TRANSACTION STATUS                     
         B     BLDT20                                                           
*-------------------------------                                                
* DUE DATE ELEMENT X'61'                                                        
*-------------------------------                                                
         USING DUEELD,R3                                                        
BLDT70   SR    RF,RF                                                            
         IC    RF,DUELN                                                         
         B     BLDT400                                                          
*-------------------------------                                                
* SUBSIDIARY CASH INFO X'50'                                                    
*-------------------------------                                                
         USING SCIELD,R3                                                        
BLDT80   CLI   SCITYPE,SCITCDSC          CASH DISCOUNT?                         
         BNE   BLDT20                                                           
         AP    TSDISC,SCIAMNT                                                   
         B     BLDT20                                                           
*-------------------------------                                                
* SUBSIDIARY POSTING X'4C'                                                      
*-------------------------------                                                
         USING SPDELD,R3                                                        
BLDT90   SR    R1,R1                                                            
         IC    R1,SPDLN                                                         
         SHI   R1,SPDLN1Q                Get length of account                  
         BNP   BLDT20                    . none found                           
         CHI   R1,L'TRNKULA              Make sure not too big                  
         BH    BLDT20                    . Too big                              
         CLC   SEXPUL,SPDACCS            Make sure expense account              
         BNE   BLDT20                    . not expense account                  
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TSDEXP(0),SPDACCS                                                
         B     BLDT20                                                           
*-----------------------------------                                            
* MOVE WHOLE ELEMENT ONTO RECORD                                                
*-----------------------------------                                            
BLDT400  BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)                                                    
         LA    R4,1(RF,R4)                                                      
         B     BLDT20                                                           
         DROP  R3                                                               
*----------------------------------------                                       
* PROCESS ESTIMATE INFORMATION                                                  
*----------------------------------------                                       
BLDT420  CLC   TSDJCLI,SPACES            Make sure we have a client             
         BNH   BLDT452                   . NO                                   
         CLC   TSDJPRD,SPACES            product                                
         BNH   BLDT452                   . NO                                   
         CLC   TSDJJOB,SPACES            and job                                
         BNH   BLDT452                   . NO                                   
         CLC   SVCPJ,TSDCPJ              Already have estimate info?            
         BNE   BLDT422                   . NO                                   
         ZAP   TSDCEST,SVCEST            . YES                                  
         MVC   TSDDTAPP,SVAPPDT                                                 
         B     BLDT450                   No processing needed                   
*                                                                               
         USING ACTRECD,R3                                                       
BLDT422  LA    R3,IOKEY                                                         
         MVC   IOKEY,SPACES                                                     
         MVC   ACTKCPY,MYCO              READ FOR JOB RECORD                    
         MVC   ACTKULA,TSDULC                                                   
         GOTO1 AIO,IOREAD+IOACCDIR+IO2                                          
         BE    BLDT426                                                          
         TM    IOERR,IOMAX               MAX IOS REACHED?                       
         BZ    BLDT452                                                          
         OI    DISPFLAG,DISIOMAX                                                
         B     BLDTX                                                            
*                                                                               
BLDT426  GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BE    BLDT428                                                          
         TM    IOERR,IOMAX               MAX IOS REACHED?                       
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX                                                
         B     BLDTX                                                            
         DROP  R3                                                               
*                                                                               
BLDT428  BRAS  RE,RDOPT                  OPTIONS FOR JOBBER (GETOPT)            
         TM    DISPFLAG,DISIOMAX                                                
         BO    BLDTX                                                            
         BRAS  RE,LOOKUP                 READ FOR ESTIMATES (JOBBER)            
         TM    DISPFLAG,DISIOMAX                                                
         BO    BLDTX                                                            
*----------------------------------------                                       
* BUILD CASHFLOW INFORMATION                                                    
*----------------------------------------                                       
BLDT450  BRAS  RE,GPCR                                                          
         TM    DISPFLAG,DISIOMAX                                                
         BO    BLDTX                                                            
*                                                                               
BLDT452  BRAS  RE,FILTCASH                                                      
         BNE   BLDTERX                                                          
*                                                                               
         USING CFWELD,R4                                                        
         SR    RF,RF                                                            
         ICM   RF,1,PBCHKS                                                      
         BZ    BLDT454                                                          
         MVI   CFWEL,ELETSARQ            INTERNAL ELEMENT                       
         MVI   CFWELTP,CFWVELQ           PRE-BILLING CASHFLOW                   
         MHI   RF,TCTLQ                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CFWVEL(0),PBCHKS                                                 
         AHI   RF,1                                                             
         LA    RF,CFWVEL-CFWELD(RF)                                             
         STC   RF,CFWLN                  LENGTH OF ELEMENT                      
         LA    R4,0(RF,R4)                                                      
*                                                                               
BLDT454  SR    RF,RF                                                            
         ICM   RF,1,CPCHKS                                                      
         BZ    BLDT456                                                          
         MVI   CFWEL,ELETSARQ            INTERNAL ELEMENT                       
         MVI   CFWELTP,CFWRELQ           PRE-BILLING CASHFLOW                   
         MHI   RF,TCTLQ                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CFWREL(0),CPCHKS                                                 
         AHI   RF,1                                                             
         LA    RF,CFWREL-CFWELD(RF)                                             
         STC   RF,CFWLN                  LENGTH OF ELEMENT                      
         LA    R4,0(RF,R4)                                                      
         DROP  R4                                                               
*                                                                               
BLDT456  AP    TOTBNET,TSDBNET           VENDOR INVOICE BILLED AMOUNT           
         AP    TOTCPA,TSDCPA             CLIENT PAYMENT AMOUNT                  
         AP    SUBBNET,TSDBNET           VENDOR INVOICE BILLED AMOUNT           
         AP    SUBCPA,TSDCPA             CLIENT PAYMENT AMOUNT                  
*---------------------------------------                                        
* MARK END OF TSAR REC, FORMAT FOR GRIDS                                        
*----------------------------------------                                       
         MVI   0(R4),EOR                                                        
         LA    R4,1(R4)                                                         
         LHI   RF,TSDLENQ                                                       
         AHI   RF,TSARDATA-TSARRECD                                             
         LA    RE,TSDRFST                                                       
         SR    R4,RE                                                            
         AR    RF,R4                     RF=L'(TSAR RECORD)                     
         L     RE,ATSARREC                                                      
         STCM  RF,3,TSARLEN-TSARRECD(RE)                                        
*                                                                               
         BAS   RE,FGRMTSAR               FORMAT TSAR FOR GRIDS                  
*                                                                               
         L     R0,ASVTSAR                                                       
         LHI   R1,TSARRECL                                                      
         L     RE,ATSARREC               SAVE DUMMY TSAR RECORD                 
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVC   TSLSTREC,TSCURRNO                                                
*                                                                               
         AP    TRNTOT,TSDAMNT                                                   
         AP    SUBTRN,TSDAMNT                                                   
         AP    TOTDISC,TSDISC                                                   
         AP    SUBDISC,TSDISC                                                   
*                                                                               
BLDTX    J     OKXIT                                                            
BLDTERX  J     ERXIT                                                            
         DROP  R2,INVP                                                          
         EJECT                                                                  
***********************************************************************         
*        FORMAT DATE INTO DUMMY SCREEN LINES                                    
***********************************************************************         
         USING TRNRECD,R3                                                       
FGRMTSAR NTR1                                                                   
*                                                                               
FGRM01   MVI   LINSUSED,0                NUMBER OF LINES USED                   
         MVI   DISATRIB,0                DISPLAY ATTRIBUTES                     
         L     R0,ADUMLINE               CLEAR DUMMY SCREEN LINES               
         LHI   R1,DUMLINLN                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,ADUMLINE               R2=A(FIRST DUMMY SCREEN LINE)          
         L     R4,ATSARREC               R4=A(TSAR RECORD )                     
         USING TSARRECD,R4                                                      
*                                                                               
         TM    DETFLAG,DETGRINQ                                                 
         BO    FGRM20                                                           
         GOTO1 ADISGRD,DMCB,('DWNINIT',AGCTBL)                                  
         GOTO1 ADISPLAY,DISATRIB         DISPLAY DUMMY SCREEN LINES             
         OI    DETFLAG,DETGRINQ                                                 
         B     FGRM01                                                           
*                                                                               
FGRM20   MVC   TEMPHEAD,SPACES                                                  
*                                                                               
         CLI   TSARFMT,TSTOTITM          TOTAL LINE ITEM?                       
         BE    FGRM40                                                           
         CLI   TSARFMT,TSSUBITM          SUBTOTAL LINE ITEM?                    
         BE    FGRM30                                                           
*                                                                               
         LA    R4,TSARDATA-TSARRECD(R4)                                         
         USING TSARDATD,R4                                                      
         GOTO1 ASETELE,TSDRFST           SET ELEMENT ADDRESSES                  
         CLC   TSDUNK,=C'N/A'            UNKNOWN CHARGES?                       
         BE    FGRM24                    . YES, DISPLAY UNKNOW COLUMNS          
         GOTO1 ADISGRD,DMCB,(0,AGCTBL)                                          
         B     FGRMX                                                            
FGRM24   GOTO1 ADISGRD,DMCB,(0,AGCTBL2)                                         
         B     FGRMX                                                            
*                                                                               
FGRM30   MVC   TEMPHEAD(L'MX@TFOR),MX@TFOR                                      
         MVC   TEMPHEAD+L'MX@TFOR(L'SVINV),SVINV                                
         GOTO1 ADISGRD,DMCB,('DWNSBT',AGCTBL)                                   
         B     FGRMX                                                            
*                                                                               
FGRM40   MVC   TEMPHEAD(L'MX@ACCT),MX@ACCT                                      
         GOTO1 ADISGRD,DMCB,('DWNEOR',AGCTBL)                                   
*                                                                               
FGRMX    B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* SUBTOTAL LINE                                                                 
***********************************************************************         
SUBTOT   NTR1                                                                   
         L     R0,ATSARREC               CLEAR TSAR RECORD                      
         LHI   R1,TSARRECL                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R3,ATSARREC                                                      
         USING TSARRECD,R3                                                      
         LHI   RF,TSTLNQ                                                        
         AHI   RF,TSARDATA-TSARRECD                                             
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO         SET TSAR REC NUMBER                    
         LA    R3,TSARDATA               R3=A(TSAR RECORD DATA)                 
         USING TSARTOTD,R3                                                      
         MVI   TSTFMT,TSSUBITM           TOTAL ITEM                             
*                                                                               
         ZAP   TSTTOT,SUBTRN                                                    
         ZAP   TSTDISC,SUBDISC                                                  
         ZAP   TSTBNET,SUBBNET                                                  
         ZAP   TSTCPA,SUBCPA                                                    
*                                                                               
         BAS   RE,FGRMTSAR               FORMAT TSAR ONTO DUMMY LINES           
*                                                                               
         ZAP   SUBTRN,=P'0'                                                     
         ZAP   SUBDISC,=P'0'                                                    
         ZAP   SUBBNET,=P'0'                                                    
         ZAP   SUBCPA,=P'0'                                                     
*                                                                               
         MVC   TSTLINES,LINSUSED         NUMBER OF LINES USED BY TOTAL          
         L     R0,ASVTSAR                                                       
         LHI   R1,TSARRECL                                                      
         L     RE,ATSARREC               SAVE DUMMY TSAR RECORD                 
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   TSLSTREC,TSCURRNO                                                
*                                                                               
         GOTO1 ADISPLAY,DISATRIB         DISPLAY SCREEN LINES                   
         BNE   SUBTERX                   SCREEN IS FULL                         
         MVC   TSLSTLIN,TSCURRNO         INCREMENT CURRENT TSAR REC NUM         
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
*                                                                               
SUBTOTX  B     OKXIT                                                            
SUBTERX  B     ERXIT                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* TOTAL LINE                                                                    
***********************************************************************         
TOTAL    NTR1                                                                   
         L     R0,ATSARREC               CLEAR TSAR RECORD                      
         LHI   R1,TSARRECL                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         L     R3,ATSARREC                                                      
         USING TSARRECD,R3                                                      
         LHI   RF,TSTLNQ                                                        
         AHI   RF,TSARDATA-TSARRECD                                             
         STCM  RF,3,TSARLEN                                                     
         MVC   TSARKYNO,TSCURRNO         SET TSAR REC NUMBER                    
         LA    R3,TSARDATA               R3=A(TSAR RECORD DATA)                 
         USING TSARTOTD,R3                                                      
         MVI   TSTFMT,TSTOTITM           TOTAL ITEM                             
*                                                                               
         ZAP   TSTTOT,TRNTOT                                                    
         ZAP   TSTDISC,TOTDISC                                                  
         ZAP   TSTBNET,TOTBNET                                                  
         ZAP   TSTCPA,TOTCPA                                                    
*                                                                               
         BAS   RE,FGRMTSAR               FORMAT TSAR ONTO DUMMY LINES           
*                                                                               
         MVC   TSTLINES,LINSUSED         NUMBER OF LINES USED BY TOTAL          
         L     R0,ASVTSAR                                                       
         LHI   R1,TSARRECL                                                      
         L     RE,ATSARREC               SAVE DUMMY TSAR RECORD                 
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   TSLSTREC,TSCURRNO                                                
*                                                                               
         GOTO1 ADISPLAY,DISATRIB         DISPLAY SCREEN LINES                   
         BNE   TOTX                      SCREEN IS FULL                         
         MVC   TSLSTLIN,TSCURRNO         INCREMENT CURRENT TSAR REC NUM         
         SR    RF,RF                                                            
         ICM   RF,3,TSCURRNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,TSCURRNO                                                    
*                                                                               
TOTX     B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
OKXIT    CR    RB,RB                                                            
         J     XIT                                                              
ERXIT    LTR   RB,RB                                                            
XIT      XIT1                                                                   
XITR1    XIT1  REGS=(R1)                                                        
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                                     
***********************************************************************         
         LTORG                                                                  
SI       DC    C'SI'                                                            
SR       DC    C'SR'                                                            
*                                                                               
MBVOFFSQ EQU   3                         OFFSET TO 127+ TRANSACTIONS            
MBVINDIQ EQU   C'*'                      INDICATOR FOR 127+ TRANS               
         EJECT                                                                  
         DS    0D                                                               
*                                                                               
DCMIX    DS    0X                                                               
         DCDDL AC#HELP,L'MX@HELP         Help                                   
         DCDDL AC#URG,L'MX@URG           URGENT                                 
         DCDDL AC#HELD,L'MX@HELD         HELD                                   
         DCDDL AC#INT,L'MX@INT           INTERNAL                               
         DCDDL AC#APRVD,L'MX@APRVD       APPROVED                               
         DCDDL AC#RVRSL,L'MX@RVRSL       REVERSAL                               
         DCDDL AC#BNKVD,L'MX@BNKVD       VOIDED                                 
         DCDDL AC#QUERD,L'MX@QUERD       QUERIED                                
         DCDDL AC#WRTF,L'MX@WRTF         WRITE OFF                              
         DCDDL AC#XFR,L'MX@XFR           TRANSFER                               
         DCDDL AC#INVC2,L'MX@INVC2       Invoice Number                         
         DCDDL AC#RSIDT,L'MX@RSIDT       Invoice Date                           
         DCDDL AC#STT,L'MX@STT           Status                                 
         DCDDL AC#RSRCK,L'MX@RSRCK       Client Check Number                    
         DCDDL AC#RSRCD,L'MX@RSRCD       Client Check date                      
         DCDDL AC#OFFST,L'MX@OFFST       OFFSET                                 
         DCDDL AC#ACCT,L'MX@ACCT         Account Total                          
         DCDDL AC#CLIPK,L'MX@CLIPK       Client/Product/Job                     
         DCDDL AC#RSBRF,L'MX@RSBRF       Batch Reference                        
         DCDDL AC#ACTDT,L'MX@ACTDT       Activity Date                          
         DCDDL AC#DUEDT,L'MX@DUEDT       Due Date                               
         DCDDL AC#OFF,L'MX@OFF           Office                                 
         DCDDL AC#MOA,L'MX@MOA           MOA                                    
         DCDDL AC#DSCAD,L'MX@DSCAD       D/A                                    
         DCDDL AC#RSRCS,L'MX@RSRCS       Receivable Status                      
         DCDDL AC#CEST,L'MX@CEST         Current Estimate                       
         DCDDL AC#PART,L'MX@PART         Partial                                
         DCDDL AC#BLD,L'MX@BLD           Billed                                 
         DCDDL AC#PYMNT,L'MX@PYMNT       Payment                                
         DCDDL AC#EXPA,L'MX@EXPA         Expense                                
         DCDDL AC#ACC,L'MX@ACC           Account                                
         DCDDL AC#VIN,L'MX@VIN           Vendor Invoice Number                  
         DCDDL AC#VID,L'MX@VID           Vendor Invoice Date                    
         DCDDL AC#VIA,L'MX@VIA           Vendor Invoice Amount                  
         DCDDL AC#VIB,L'MX@VIB           Vendor Invoice Billed                  
         DCDDL AC#VIBA,L'MX@VIBA         Vendor Incoice Billed Amount           
         DCDDL AC#CPM,L'MX@CPM           Client Payment                         
         DCDDL AC#CPAMT,L'MX@CPAMT       Client Payment Amount                  
         DCDDL AC#PBLAM,L'MX@PBLAM       Pre-Billed Amount                      
         DCDDL AC#PBCPM,L'MX@PBCPM       Pre-Billed Client Payment              
         DCDDL AC#PBCPA,L'MX@PBCPA       Pre-Billed Client Payment Amnt         
         DCDDL AC#PBCCD,L'MX@PBCCD       Pre-Billed Client Check Date           
         DCDDL AC#PBCCN,L'MX@PBCCN       Pre-Billed Client Check Number         
         DCDDL AC#AEDTE,L'MX@AEDTE       Approved Estimate Date                 
         DCDDL AC#DISAM,L'MX@DISAM       Discount Amount                        
         DCDDL AC#TFOR,L'MX@TFOR         Total for                              
         DCDDL AC#TT034,L'MX@TT034       Job Transfer                           
         DCDDL AC#SEQ,L'MX@SEQ           Sequence                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* CALL GETOPT FOR JOBBER CALL                                                   
***********************************************************************         
RDOPT    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,IOKEY                                                         
         USING CPYRECD,R3                                                       
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,MYCO                                                     
         GOTO1 AIO,IOREAD+IOACCMST+IO3                                          
         BE    RDOP10                                                           
         TM    IOERR,IOMAX               HAVE WE EXCEEDED MAX IO'S?             
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX         . YES                                  
         B     RDOPX                                                            
*                                                                               
RDOP10   GOTO1 VACCEMU,DMCB,=C'NEWO',,,AIO3                                     
         ORG   *-2                                                              
         LR    R2,R1                     EMU REQUIRES R2=A(DMCB)                
         BASR  RE,RF                                                            
*                                                                               
         L     R5,AGOBLOCK                                                      
         USING GOBLOCKD,R5                                                      
         MVC   GOADM,VDATAMGR                                                   
         L     RE,AOPTBUFF                                                      
         ST    RE,GOABUFF                                                       
         LA    RE,L'OPTBUFF                                                     
         ST    RE,GOLBUFF                                                       
         L     RF,AIO3                   RF=A(COMPANY RECORD)                   
         ST    RF,GOACOMP                                                       
         XC    GOAKEY,GOAKEY             CLEAR SEQUENCE RESET                   
         L     R4,AIO2                                                          
         ST    R4,GOAKEY                                                        
         MVC   GOSELCUL,0(R4)                                                   
         LA    R4,3(R4)                                                         
         MVC   GOSELCLI,SPACES                                                  
         SR    R1,R1                                                            
         IC    R1,CLEN                                                          
         LR    RF,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GOSELCLI(0),0(R4)                                                
         LA    R4,0(RF,R4)                                                      
         MVC   GOSELPRO,SPACES                                                  
         IC    R1,CPLEN                                                         
         SR    R1,RF                                                            
         LR    RF,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GOSELPRO(0),0(R4)                                                
         LA    R4,0(RF,R4)                                                      
         MVC   GOSELJOB,SPACES                                                  
         IC    R1,CPJLEN                                                        
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GOSELJOB(0),0(R4)                                                
         MVI   GOWHICH,0                                                        
         MVI   GOANYWC,C'N'                                                     
         GOTO1 VGETOPT,DMCB,AGOBLOCK                                            
*                                                                               
         MVC   NEEDAE,GONEEDAE                                                  
*                                                                               
RDOPX    J     OKXIT                                                            
         DROP  R3,R5                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CALL JOBBER FOR CURRENT ESTIMATE                                              
***********************************************************************         
LOOKUP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,ACOLIST                                                       
         OC    0(L'COLIST,RF),0(RF)                                             
         BNZ   LOOK10                                                           
         GOTO1 VJOBCOL,DMCB,LOOKFLDH,ACOLIST,ACOMFACS                           
*                                                                               
         USING JBLOCKD,R5                                                       
LOOK10   L     R5,AJOBLOCK                                                      
         LR    RE,R5                                                            
         LA    RF,JBLOCKL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         GOTO1 VACCEMU,DMCB,=C'NEWO',,,AIO2                                     
         ORG   *-2                                                              
         LR    R2,R1                     EMU REQUIRES R2=A(DMCB)                
         BASR  RE,RF                                                            
*                                                                               
         MVC   JBAJOB,AIO2                                                      
         XC    JBAKEY,JBAKEY             CLEAR SEQUENCE RESET                   
         L     R1,AIO2                                                          
         ST    R1,JBAKEY                                                        
         MVC   JBACOLS,ACOLIST                                                  
         MVC   JBACOM,ACOMFACS                                                  
         L     R1,AGOBLOCK                                                      
         ST    R1,JBAGOBLK                                                      
         MVC   JBGETOPT,VGETOPT                                                 
         L     RE,AIO3                                                          
         ST    RE,JBAIO                                                         
         LR    RF,RE                                                            
         L     RF,ACOLTAB                                                       
         ST    RF,JBACOLTB                                                      
         LA    RF,L'COLTAB                                                      
         ST    RF,JBLCOLTB                                                      
         L     RF,AOPTVTAB                                                      
         ST    RF,JBAOPVTB                                                      
         LA    RF,L'OPTVTAB                                                     
         ST    RF,JBLOPVTB                                                      
*                                                                               
         MVI   JBSELFUN,JBGETDE    GET BO DETAILS                               
         LA    RE,LOOKFLDH                                                      
         ST    RE,JBORICLI                                                      
*                                                                               
         MVI   JBSELWRK,JBNOZERO   SKIP OVER ZERO WORKCODES                     
*                                                                               
         GOTO1 VJOBBER,DMCB,AJOBLOCK                                            
         CLI   JBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    SVAPPDT,SVAPPDT           HIGH EST REV APP DATE                  
         CLI   NEEDAE,C'Y'               NEED APPROVAL                          
         BNE   *+10                      . NO                                   
         MVC   SVAPPDT,JBHIAPDT          HIGH EST REV APP DATE                  
         L     RF,JBACOLTB                                                      
         CLI   JBNEWEST,JBMCSQ     IS THIS A BO JOB?                            
         BNE   *+14                                                             
         ZAP   SVCEST,MJETVAL+L'MJETVAL-MJETABD(L'MJETVAL,RF)                   
         B     *+10                                                             
         ZAP   SVCEST,JBCOLVAL+L'JBCOLVAL-JBCOLD(L'JBCOLVAL,RF)                 
*                                                                               
         L     R2,ATSARREC               R2=A(TSAR RECORD)                      
         USING TSARRECD,R2                                                      
         LA    R2,TSARDATA                                                      
         USING TSARDATD,R2                                                      
         MVC   TSDDTAPP,SVAPPDT                                                 
         ZAP   TSDCEST,SVCEST                                                   
         MVC   SVCPJ,TSDCPJ              SAVE CLIENT/PRODUCT/JOB                
         DROP  R2                                                               
*                                                                               
         GOTO1 VACCEMU,DMCB,=C'OLDN',,,AIO2                                     
         ORG   *-2                                                              
         LR    R2,R1                     EMU REQUIRES R2=A(DMCB)                
         BASR  RE,RF                                                            
         J     OKXIT                                                            
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
LOOKFLDH DC    AL1(8+L'LOOKFLD)                                                 
         DC    XL4'00'                                                          
         DC    AL1(L'LOOKFLD)                                                   
         DC    XL2'00'                                                          
LOOKFLD  DC    C'OE,CE'                                                         
         EJECT                                                                  
***********************************************************************         
* GET THE PRODUCTION CHARGE RECORD                                              
***********************************************************************         
         USING TSARDATD,R2                                                      
GPCR     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    CK.TRNKEY,CK.TRNKEY       BUILD JOB CHARGE KEY                   
         MVC   CK.TRNKCPY,MYCO                                                  
         MVC   CK.TRNKULA,TSDULC                                                
         MVC   CK.TRNKWORK,SPACES                                               
         MVC   CK.TRNKCCPY,MYCO                                                 
         MVC   CK.TRNKULC,IK.TRNKULA                                            
         MVC   CK.TRNKDATE,IK.TRNKDATE                                          
         MVC   CK.TRNKREF,IK.TRNKREF                                            
*        MVC   CK.TRNKSBR                                                       
*----------------------------------------                                       
* WORKCODES FOR JOB CHARGES                                                     
*----------------------------------------                                       
         USING FFTELD,R5                                                        
         ICM   R5,15,AFFTELD                                                    
         BZ    GPCRUX                                                           
GPCR06   CLI   FFTEL,0                   END OF RECORD                          
         BE    GPCRUX                    . YES, NO WORKCODE FREE FORMS          
         CLI   FFTEL,FFTELQ              FREE FORM ELEMENT                      
         BNE   *+12                      . NO, GET NEXT                         
         CLI   FFTTYPE,FFTTWRKC          WORKCODE (91)                          
         BE    GPCR10                    . YES                                  
         SR    R0,R0                                                            
         IC    R0,FFTLN                                                         
         AR    R5,R0                                                            
         B     GPCR06                                                           
*                                                                               
GPCR10   SR    R1,R1                                                            
         IC    R1,FFTLN                                                         
         SHI   R1,FFTDATA-FFTELD         LENGTH OF WORKCODE DATA                
         BNP   GPCRUX                                                           
         LA    RF,L'FFTWORK+L'FFTWAMT    LENGTH OF WORKCODE DATA ENTRY          
         SR    R0,R0                     CLEAR ZERO FOR DIVIDE                  
         DR    R0,RF                     DIVIDE FOR # OF ENTRIES                
         CHI   R1,MAXWCDES               MORE THAN WC TABLE CAN HANDLE?         
         BNH   *+6                       . NO                                   
         DC    H'0'                                                             
         STC   R1,SVNOW                  SAVE NUMBER OF WORKCODES               
         LA    RE,SVWTAB                 WORKCODE LIST                          
         LA    RF,FFTWORK                                                       
GPCR12   MVC   0(L'FFTWORK,RE),0(RF)     ADD WORKCODE TO LIST                   
         LA    RF,L'FFTWORK+L'FFTWAMT(RF)                                       
         LA    RE,L'FFTWORK(RE)                                                 
         BCT   R1,GPCR12                                                        
         DROP  R5                                                               
*----------------------------------------                                       
* PROCESS THE JOB                                                               
*----------------------------------------                                       
         SR    R5,R5                                                            
         ICM   R5,1,SVNOW                NUMBER OF WORKCODES                    
         BZ    GPCRUX                                                           
         NI    VCWFLAG,X'FF'-(VCWJBFOQ+VCWJBXFQ)                                
*                                                                               
         LA    R6,SVWTAB                 START OF WORKCODE LIST                 
GPCR30   MVC   CK.TRNKWORK,0(R6)         JOB CHARGE KEY                         
         MVC   IOKEY,CK.TRNKEY                                                  
         GOTO1 AIO,IOHIGH+IOACCDIR+IO2                                          
         BE    GPCR34                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX         MAX IO'S                               
         B     GPCRX                                                            
*                                                                               
GPCR32   GOTO1 AIO,IOSEQ+IOACCDIR+IO2                                           
         BE    GPCR34                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX         MAX IO'S                               
         B     GPCRX                                                            
*                                                                               
GPCR34   CLC   IOKEY(TRNKSBR-TRNKEY),CK.TRNKEY                                  
         BNE   GPCR80                                                           
         GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BE    GPCR36                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX         MAX IO'S                               
         B     GPCRX                                                            
*                                                                               
GPCR36   L     R3,AIO2                                                          
         USING TRNRECD,R3                                                       
         LA    R3,TRNRFST                R4=A(1ST ELEM OF TRANS REC)            
         USING PTAELD,R3                                                        
GPCR40   CLI   0(R3),0                   end of record                          
         BE    GPCR32                                                           
         CLI   0(R3),TRNELQ              X'44' - Transaction elem               
         BE    GPCR50                                                           
         CLI   0(R3),TRSELQ              X'60' - Transaction status             
         BE    GPCR60                                                           
         CLI   0(R3),PTAELQ              X'77' -                                
         BNE   GPCR44                                                           
         CLI   PTATYPE,PTATRAL                                                  
         BE    GPCR70                                                           
GPCR44   SR    RF,RF                                                            
         IC    RF,1(,R3)                 next element                           
         AR    R3,RF                                                            
         B     GPCR40                                                           
*        --------------------------------                                       
*        X'44' - Transaction Elem                                               
*        --------------------------------                                       
         USING TRNELD,R3                                                        
GPCR50   CLI   TRNTYPE,TRNTJBTX                                                 
         BNE   *+12                                                             
         OI    VCWFLAG,VCWJBXFQ                                                 
         B     GPCR44                                                           
         ICM   R1,15,ATRNELD                                                    
         BZ    GPCRUX                                                           
         CLC   TRNBTCH,TRNBTCH-TRNELD(R1)  Match on batch ref                   
         BNE   GPCR32                                                           
         B     GPCR44                                                           
*        --------------------------------                                       
*        X'60' - Transaction Status                                             
*        --------------------------------                                       
         USING TRSELD,R3                                                        
GPCR60   ICM   R1,15,ATRSELD             Vendor status element                  
         BZ    GPCRUX                                                           
         CLC   TRSBSEQ,TRSBSEQ-TRSELD(R1) Match on batch line #                 
         BNE   GPCR32                                                           
         OI    VCWFLAG,VCWJBFOQ                                                 
         B     GPCR44                                                           
*        --------------------------------                                       
*        X'77' - Transaction Activity                                           
*        --------------------------------                                       
         USING PTAELD,R3                                                        
GPCR70   TM    PTASTAT1,PTASPEND         Pending Billing                        
         BO    GPCR44                                                           
         AP    TSDBNET,PTANET            Net amount of Billing                  
*        --------------------------------                                       
*        Save Bills for Cashflow                                                
*        --------------------------------                                       
         USING TBTABD,R1                 Save Bills for Cashflow                
         LA    R1,SVBTAB                                                        
GPCR72   CLI   SVNOB,MAXBILLS            Reached bill maximum?                  
         BL    *+6                       . No, still room                       
         DC    H'0'                      . Yes, dump for now                    
         OC    0(TBTLQ,R1),0(R1)         Empty spot?                            
         BZ    GPCR76                    . yes, save bill info                  
         CLC   PTARBLDT,TBTDT            Match on date?                         
         BNE   GPCR74                    . no, check next                       
         CLC   PTARBLNO,TBTNO            Match on number?                       
         BNE   GPCR74                    . no                                   
         AP    TBTNET,PTANET                                                    
         AP    TBTCOM,PTARCOM                                                   
         B     GPCR44                                                           
GPCR74   LA    R1,TBTLQ(R1)              next table entry                       
         B     GPCR72                                                           
GPCR76   MVC   TBTDT,PTARBLDT            Add bill to table                      
         MVC   TBTNO,PTARBLNO                                                   
         ZAP   TBTNET,PTANET                                                    
         ZAP   TBTCOM,PTARCOM                                                   
         SR    RF,RF                     keep count of bills                    
         IC    RF,SVNOB                                                         
         AHI   RF,1                                                             
         STC   RF,SVNOB                                                         
         B     GPCR44                    next ptael                             
         DROP  R1,R3                                                            
*                                                                               
GPCR80   TM    VCWFLAG,VCWJBXFQ+VCWJBFOQ Charge found and transferred?          
         BNO   *+8                       . no                                   
         OI    TSDCSTAT,TSDCJXFQ         . yes, mark it as such                 
*                                                                               
         LA    R6,L'TRNKWORK(R6)         Next workcode in list                  
         BCT   R5,GPCR30                                                        
*----------------------------------------                                       
* PROCESS THE BILLS                                                             
*----------------------------------------                                       
         BRAS  RE,PROCBILS               Process Bills                          
         BNE   GPCRUX                                                           
         TM    DISPFLAG,DISIOMAX         MAX IO'S                               
         BO    GPCRX                                                            
*----------------------------------------                                       
* SET BILLING AND PAYMENT INDICATORS                                            
*----------------------------------------                                       
         CP    UNAPBA,=P'0'              Unadjusted pre-billed applied          
         BE    GPCR90                    . no                                   
         OI    TSDCSTAT,TSDCPBQ                                                 
         CP    UNAPBU,UNAPBA             Fully applied?                         
         BE    *+12                      . Yes                                  
         NI    TSDCSTAT,X'FF'-TSDCPBQ    . No, turn off client payment          
         OI    TSDCSTAT,TSDCPBPQ               turn on partial payment          
*                                                                               
GPCR90   CP    TSDBNET,=P'0'             Any billing?                           
         BZ    GPCR92                    . no                                   
         OI    TSDCSTAT,TSDCIBQ                                                 
         CP    TSDAMNT,TSDBNET                                                  
         BE    *+12                      . Yes                                  
         NI    TSDCSTAT,X'FF'-TSDCIBQ    . No, turn off billed                  
         OI    TSDCSTAT,TSDCIPBQ               turn on partial                  
GPCR92   CP    UNAAPP,=P'0'              Unadjusted applied                     
         BE    GPCRX                     . no                                   
         OI    TSDCSTAT,TSDCCPQ                                                 
         CP    UNAUNA,UNAAPP             Fully applied?                         
         BE    *+12                      . Yes                                  
         NI    TSDCSTAT,X'FF'-TSDCCPQ    . No, turn off client payment          
         OI    TSDCSTAT,TSDCPCQ                turn on partial payment          
         B     GPCRX                                                            
*                                                                               
GPCRUX   MVC   TSDUNK,=C'N/A'                                                   
         ZAP   TSDBNET,=P'0'             Billing info not supported             
GPCRX    J     OKXIT                                                            
GPCRERX  J     ERXIT                                                            
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
* FILTER CASHFLOW INFORMATION                                                   
*----------------------------------------------------------------------         
         USING OPTVALSD,R1                                                      
         USING TSARDATD,R2                                                      
FILTCASH NTR1  BASE=*,LABEL=*                                                   
         L     R1,AOPTVALS               R2=A(OPTION VALUES)                    
*        --------------------------------                                       
*        Billed items only?                                                     
*        --------------------------------                                       
         CP    TSDBNET,=P'0'                                                    
         BZ    FICA16                                                           
FICA10   CLI   OBILLED,C'N'                                                     
         BE    FICARJX                                                          
         B     FICA20                                                           
FICA16   CLI   OBILLED,C'O'                                                     
         BE    FICARJX                                                          
*        --------------------------------                                       
*        Billed and Paid items only                                             
*        --------------------------------                                       
FICA20   CP    TSDBNET,=P'0'                                                    
         BZ    FICA24                                                           
         CP    TSDCPA,=P'0'                                                     
         BZ    FICA24                                                           
         CLI   OPAID,C'N'                                                       
         BE    FICARJX                                                          
         B     FICAX                                                            
FICA24   CLI   OPAID,C'O'                                                       
         BE    FICARJX                                                          
*                                                                               
FICAX    J     OKXIT                                                            
FICARJX  J     ERXIT                                                            
         DROP  R1,R2                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESS BILL DATA                                                             
*        ON ENTRY - R2=A(TSAR DATA)                                             
***********************************************************************         
         USING TSARDATD,R2                                                      
PROCBILS NTR1  BASE=*,LABEL=*                                                   
         XC    BK.TRNKEY,BK.TRNKEY                                              
         MVC   BK.TRNKCPY,MYCO                                                  
         MVC   BK.TRNKULA,TSDULC                                                
         MVC   BK.TRNKWORK,=C'99'                                               
         MVC   BK.TRNKCCPY,MYCO                                                 
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(TRNKCULC-TRNKEY),BK.TRNKEY                                 
         GOTO1 AIO,IOHIGH+IOACCDIR+IO3                                          
         BE    PRCUB10                                                          
         TM    IOERR,IOMAX               HAVE WE EXCEEDED MAX IO'S?             
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX         . YES                                  
         B     PRCUBGOX                                                         
*                                                                               
PRCUB10  NI    VCWFLAG,X'FF'-(VCWSBILL+VCWPBILL+VCWUBILL+VCWRBILL)              
         CLC   IOKEY(TRNKCULC-TRNKEY),IOKEYSAV                                  
         BNE   PRCUBGOX                                                         
         TM    IOKEY+(TRNKSTAT-TRNRECD),TRNSDRFT   DRAFT? X'40'                 
         BO    PRCUB24                             . YES, SKIP                  
*                                                                               
         GOTO1 AIO,IOGET+IOACCMST+IO3                                           
         BE    PRCUB12                                                          
         TM    IOERR,IOMAX               HAVE WE EXCEEDED MAX IO'S?             
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX         . YES                                  
         B     PRCUBGOX                                                         
*                                                                               
PRCUB12  L     R3,AIO3                                                          
         USING TRNRECD,R3                                                       
         LA    R4,TRNRFST                                                       
         USING TRNELD,R4                                                        
         CLI   0(R4),TRNELQ                                                     
         BNE   PRCUB24                                                          
*-----------------------------------                                            
* PRE-Billing (% Est, Special, Manual)                                          
*-----------------------------------                                            
         CLI   TRNBTYPE,TRNBTPER         Estimate billing                       
         BE    PRCUB13                                                          
         CLI   TRNBTYPE,TRNBTSPE         Special billing                        
         BE    PRCUB13                                                          
         CLI   TRNBTYPE,TRNBTMAN         Manual billing                         
         BNE   PRCUB14                                                          
PRCUB13  OI    VCWFLAG,VCWPBILL          PRE-BILLING                            
         AP    TSDPBA,TRNAMNT                                                   
         B     PRCUB22                                                          
*-----------------------------------                                            
* Standard (progressive) billing                                                
*-----------------------------------                                            
         USING TBTABD,R5                                                        
PRCUB14  LA    R5,SVBTAB                                                        
         ZICM  R6,SVNOB                                                         
         BNP   PRCUB20                                                          
PRCUB16  CLC   TRNKREF,TBTNO             Match on number?                       
         BNE   PRCUB18                                                          
         GOTO1 VDATCON,DMCB,(1,TRNKDATE),(2,TEMPDAT)                            
         CLC   TEMPDAT(L'TBTDT),TBTDT    Match on date?                         
         BNE   PRCUB18                                                          
         OI    VCWFLAG,VCWSBILL          Standard (progressive) billing         
         B     PRCUB20                                                          
PRCUB18  LA    R5,TBTLQ(R5)              Next table entry                       
         BCT   R6,PRCUB16                                                       
*-----------------------------------                                            
* Up-Front (type 47) billing                                                    
*-----------------------------------                                            
         USING FFTELD,R4                                                        
PRCUB20  SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                   End of record?                         
         BE    PRCUB22                   . Yes                                  
         CLI   FFTEL,FFTELQ              Free form element                      
         BNE   PRCUB20                                                          
         CLI   FFTTYPE,FFTTESTP          Estimated Production (TY47)            
         BNE   PRCUB20                                                          
         OI    VCWFLAG,VCWUBILL          UPFRONT 47 BILLING                     
         ZAP   UFBAMNT,FFTESTN           save upfront billing amount            
         ZAP   UFBCOMM,FFTESTC           save upfront billing amount            
         AP    TSDPBA,FFTESTN                                                   
         B     PRCUB22                                                          
*-----------------------------------                                            
* Process this bill?                                                            
*-----------------------------------                                            
PRCUB22  TM    VCWFLAG,VCWSBILL+VCWPBILL+VCWUBILL                               
         BNZ   PRCUB30                                                          
*-----------------------------------                                            
* Next Bill                                                                     
*-----------------------------------                                            
PRCUB24  GOTO1 AIO,IOSEQ+IOACCDIR+IO3                                           
         BE    PRCUB10                                                          
         TM    IOERR,IOMAX               Have we exceeded max IO's?             
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX         YES                                    
         B     PRCUBGOX                                                         
*-----------------------------------                                            
* Process bill for cashflow                                                     
*-----------------------------------                                            
         USING TRNELD,R4                                                        
PRCUB30  LA    R4,TRNRFST                                                       
         MVC   BK.TRNKEY,IOKEY                                                  
*                                                                               
         BRAS  RE,PROCRCF                Process receivable cashflow            
         BNE   PRCUB90                                                          
*                                                                               
         TM    VCWFLAG,VCWPBILL+VCWUBILL Any Pre-billing/upfront 47?            
         BZ    PRCUB50                   . no                                   
         AP    UNAPBU,CFUNAA             Unapplied amount                       
         AP    UNAPBA,CFAPPA             Applied amount                         
         ZAP   PRPRAMT,CFAPPA                                                   
*                                                                               
         TM    VCWFLAG,VCWPBILL          Pre-billing                            
         BZ    PRCUB32                   . yes                                  
         ZAP   PRCFGRS,TRNAMNT                                                  
         AP    PRCFGRS,TRNBLCOM                                                 
         B     PRCUB34                                                          
PRCUB32  ZAP   PRCFGRS,UFBAMNT           Upfront 47                             
         AP    PRCFGRS,UFBCOMM                                                  
PRCUB34  BZ    PRCUB50                   . no                                   
         CP    CFUNAA,=P'0'              Any Unapplied amount?                  
         BE    PRCUB50                   . no                                   
         CP    CFAPPA,=P'0'              Any applied amount?                    
         BE    PRCUB48                   . no                                   
         CP    CFUNAA,PRCFGRS            Unapplied equal to gross?              
         BE    PRCUB46                   . Yes, no adjustment needed            
*                                                                               
         ZAP   TEMPNUM,PRCFGRS                                                  
         SRP   TEMPNUM,6,0               Multiply for percent                   
         DP    TEMPNUM,CFUNAA            percent of gross to unapp              
         ZAP   DUB,TEMPNUM(L'TEMPNUM-L'PRCFGRS) Save percent                    
*                                                                               
         ZAP   TEMPNUM,PRPRAMT           Applied total in big field             
         MP    TEMPNUM,DUB               multiply by percent                    
         SRP   TEMPNUM,64-6,5            divide to get actual number            
         ZAP   PRPRAMT,TEMPNUM           put adjusted number in                 
*                                                                               
PRCUB46  ZAP   TEMPNUM,TRNAMNT                                                  
         TM    VCWFLAG,VCWPBILL          Pre-billing                            
         BO    *+10                      . yes                                  
         ZAP   TEMPNUM,UFBAMNT                                                  
         SRP   TEMPNUM,6,0               Multiply for percent                   
         DP    TEMPNUM,PRCFGRS           percent of net to gross                
         ZAP   DUB,TEMPNUM(L'TEMPNUM-L'PRCFGRS) Save percent                    
*                                                                               
         ZAP   TEMPNUM,PRPRAMT           Applied total in big field             
         MP    TEMPNUM,DUB               multiply by percent                    
         SRP   TEMPNUM,64-6,5            divide to get actual number            
         ZAP   PRPRAMT,TEMPNUM           put adjusted number in                 
*                                                                               
PRCUB48  AP    TSDPBCPA,PRPRAMT                                                 
*----------------------------------------                                       
* REGULAR BILLING                                                               
*----------------------------------------                                       
PRCUB50  TM    VCWFLAG,VCWSBILL          Standard billing                       
         BZ    PRCUB90                                                          
         AP    UNAUNA,CFUNAA             Unapplied amount                       
         AP    UNAAPP,CFAPPA             Applied amount                         
         ZAP   PRPRAMT,CFAPPA                                                   
*                                                                               
         ZAP   PRCFGRS,TBTNET            Net                                    
         AP    PRCFGRS,TBTCOM            Commission                             
         BZ    PRCUB90                                                          
         CP    CFUNAA,=P'0'             Any Unapplied amount?                   
         BE    PRCUB90                   . no                                   
         CP    CFAPPA,=P'0'              Any applied amount?                    
         BE    PRCUB58                   . no                                   
*                                                                               
         ZAP   TEMPNUM,PRCFGRS                                                  
         SRP   TEMPNUM,6,0               Multiply for percent                   
         DP    TEMPNUM,CFUNAA            percent of gross to unapp              
         ZAP   DUB,TEMPNUM(L'TEMPNUM-L'CFUNAA) Save percent                     
*                                                                               
         ZAP   TEMPNUM,PRPRAMT           Applied total in big field             
         MP    TEMPNUM,DUB               multiply by percent                    
         SRP   TEMPNUM,64-6,5            divide to get actual number            
         ZAP   PRPRAMT,TEMPNUM           put adjusted number in                 
*                                                                               
PRCUB56  ZAP   TEMPNUM,TBTNET                                                   
         SRP   TEMPNUM,6,0               Multiply for percent                   
         DP    TEMPNUM,PRCFGRS           percent of net to gross                
         ZAP   DUB,TEMPNUM(L'TEMPNUM-L'PRCFGRS) Save percent                    
*                                                                               
         ZAP   TEMPNUM,PRPRAMT           Applied total in big field             
         MP    TEMPNUM,DUB               multiply by percent                    
         SRP   TEMPNUM,64-6,5            divide to get actual number            
         ZAP   PRPRAMT,TEMPNUM           put adjusted number in                 
*                                                                               
PRCUB58  AP    TSDCPA,PRPRAMT                                                   
         OC    TSDRSTAT,CFRSTA                                                  
*----------------------------------------                                       
* Restore sequence                                                              
*----------------------------------------                                       
PRCUB90  TM    VCWFLAG,VCWRBILL          Retail Billing                         
         BO    PRCUBNOX                                                         
         MVC   IOKEY,BK.TRNKEY                                                  
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    PRCUB24                                                          
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX         MAX IO'S                               
*                                                                               
PRCUBGOX J     OKXIT                                                            
PRCUBNOX J     ERXIT                                                            
         DROP  R2,R3,R4                                                         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*  PROCESS PRODUCTION RECEIVABLE INFORMATION                          *         
***********************************************************************         
JOBB     USING TRNRECD,R2                                                       
JOB44    USING TRNELD,R3                                                        
PROCRCF  NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIO3                                                          
         LA    R3,JOBB.TRNRFST                                                  
         XC    CFREL(CFRLNQ),CFREL                                              
         XC    CFRCEL(CFRCLQ),CFRCEL                                            
         XC    RK.TRNKEY,RK.TRNKEY                                              
         ZAP   PRCFGRS,JOB44.TRNAMNT     Net amount of prod transaction         
         AP    PRCFGRS,JOB44.TRNBLCOM    Add commission for gross               
*                                                                               
         MVC   RK.TRNKCULA,JOBB.TRNKCULC Receivable account                     
         MVC   RK.TRNKREF,JOBB.TRNKREF                                          
         MVC   RK.TRNKDATE,JOBB.TRNKDATE                                        
         DROP  JOBB                                                             
*                                                                               
         LR    R2,R3                     A(TRNELQ)                              
PRCF50   CLI   0(R2),0                   end of record                          
         BE    PRCF80                                                           
         CLI   0(R2),CPJELQ              X'4F'-RCV account for retail           
         BE    PRCF60                                                           
         CLI   0(R2),BSCELQ              X'E3'-Billing Source element           
         BE    PRCF70                                                           
PRCF55   SR    RF,RF                                                            
         IC    RF,1(,R2)                 next element                           
         AR    R2,RF                                                            
         B     PRCF50                                                           
*        --------------------------------                                       
*        Receivable Account for Retail Billing                                  
*        --------------------------------                                       
         USING CPJELD,R2                 X'4F'                                  
PRCF60   CLI   CPJTYPE,CPJTREC           elment type = receivable?              
         BNE   PRCF55                                                           
         MVC   RK.TRNKULA,CPJRULA                                               
         OI    VCWFLAG,VCWRBILL          Retail Billing                         
         B     PCFERX                                                           
         DROP  R2                                                               
*        --------------------------------                                       
*        Prod Billing Source Element  X'E3'                                     
*        --------------------------------                                       
         USING BSCELD,R2                                                        
PRCF70   MVC   RK.TRNKOFF(L'TRNKOFF+L'TRNKCULC),SPACES                          
         MVC   RK.TRNKCACT,BSCBSRC       billing Source                         
         TM    COMPSTA4,CPYSOFF2         2 character offices                    
         BZ    *+10                                                             
         MVC   RK.TRNKOFF,BSCBOFF        Office                                 
         B     PRCF55                                                           
         DROP  R2                                                               
*----------------------------------------                                       
* Read for receivable                                                           
*----------------------------------------                                       
PRCF80   XC    PRCPRTAB(MAXRCVTQ*L'PRCPRTAB),PRCPRTAB Transfer Table            
PRCF82   MVC   IOKEY,RK.TRNKEY                                                  
         GOTO1 AIO,IOHIGH+IOACCDIR+IO2                                          
         BE    PRCF90                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         ZAP   CFUNAA,=P'0'                                                     
         ZAP   CFAPPA,=P'0'                                                     
         OI    DISPFLAG,DISIOMAX   MAX IO'S                                     
         J     PCFOKX                                                           
*                                                                               
PRCF90   ZAP   CFUNAA,=P'0'                                                     
         ZAP   CFAPPA,=P'0'                                                     
*                                                                               
         USING TRNRECD,R2                                                       
PRCF96   LA    R2,IOKEY                                                         
         CLC   IOKEY(TRNKEND-1),RK.TRNKEY      Receivable found?                
         BNE   PRCF240                                                          
*                                                                               
         LA    RF,PRCPRTAB               Receivable transfer table              
PRCF100  CLI   0(RF),0                   Any more transfer entries?             
         BE    PRCF114                   . no,process this receivable           
         CLI   0(RF),X'FF'               End of transfer table?                 
         BE    PRCF114                   . yes,process this receivable          
         CLC   TRNKULA,0(RF)             Did we Transfer From?                  
         BE    PRCF110                   . yes                                  
         AHI   RF,L'PRCPRTAB             . no, check next entry                 
         B     PRCF100                                                          
*                                                                               
PRCF110  CLC   TRNKSBR,L'TRNKULA(RF)     Transfer From exact record?            
         BNH   PRCF210                   . yes, get next record                 
*                                                                               
PRCF114  GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BE    PRCF120                                                          
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX         MAX IO'S                               
         J     PCFOKX                                                           
*                                                                               
PRCF120  L     R2,AIO2                   first element                          
         LA    R2,TRNRFST                first element                          
PRCF122  CLI   0(R2),EOR                 end of record                          
         BE    PRCF210                                                          
         CLI   0(R2),TRNELQ              X'44'-Transaction element              
         BE    PRCF130                                                          
**AW     CLI   0(R2),TRSELQ              X'60'-Trans Status Element             
*        BE    PRCF150                                                          
*        CLI   0(R2),DUEELQ              X'61'-Rcvable Due Date                 
*        BE    PRCF160                                                          
         CLI   0(R2),TRXELQ              X'75'-Transaction Extra Status         
         BE    PRCF170                                                          
         CLI   0(R2),RALELQ              X'D9'-Rcvable Allocation elem          
         BE    PRCF180                                                          
PRCF125  SR    RF,RF                                                            
         IC    RF,1(,R2)                 next element                           
         AR    R2,RF                                                            
         B     PRCF122                                                          
*        --------------------------------                                       
*        Receivable Transaction Element                                         
*        --------------------------------                                       
         USING TRNELD,R2                                                        
PRCF130  TM    TRNSTAT,TRNSREV                                                  
         BZ    PRCF134                                                          
         OI    CFRSTA,TRNSREV            Only want reversal status              
         CLI   TRNTYPE,TRNTCALC          TYPE 30 TRNS                           
         BE    PRCF236                   Skip this one                          
PRCF134  TM    TRNSTAT,TRNSDR                                                   
         BZ    PRCF140                                                          
         AP    CFUNAA,TRNAMNT                                                   
         B     PRCF125                                                          
PRCF140  AP    CFAPPA,TRNAMNT            Applied amount                         
         ZAP   PRPRAMT,TRNAMNT                                                  
         B     PRCF125                                                          
**AW     --------------------------------                                       
*        Transaction Status Element                                             
*        --------------------------------                                       
*        USING TRSELD,R2                                                        
*RCF150  TM    CFRSTA,TRNSREV                                                   
*        BZ    PRCF125                                                          
*        GOTO1 VDATCON,DMCB,(2,TRSREVD),(1,CFCCDT)                              
*        B     PRCF125                                                          
*        --------------------------------                                       
*        Receivable Due Date Element                                            
*        --------------------------------                                       
*        USING DUEELD,R2                 Receivable Due Date Element            
*RCF160  MVC   CFCDDT,DUEDATE                                                   
*        B     PRCF125                                                          
*        --------------------------------                                       
*        Transaction Extra Status Element                                       
*        --------------------------------                                       
         USING TRXELD,R2                                                        
PRCF170  TM    TRXSTA1,TRXSRQRD          Queried?                               
         BZ    *+8                       . no                                   
         OI    CFRSTA,TRXSRQRD                                                  
         TM    TRXSTA1,TRXSRHLD          Held?                                  
         BZ    *+8                       . no                                   
         OI    CFRSTA,TRXSRHLD                                                  
         B     PRCF125                                                          
*        --------------------------------                                       
*        Receivable Allocation Element                                          
*        --------------------------------                                       
         USING RALELD,R2                 Receivable Allocation Element          
PRCF180  CLI   RALTYPE,RALTALC           Type 1 = Regular Allocation            
         BNE   PRCF184                                                          
         MVC   CFCCNO,RALAREF            Client Check Number                    
         MVC   CFCCDT,RALADAT            Client Check Date                      
**AW     MVC   CFCPDT,RALADEP            Client Deposit Date                    
         B     PRCF125                                                          
*                                                                               
PRCF184  CLI   RALTYPE,RALTOFS           Type 2 = Offset/Contra'd               
         BNE   PRCF188                                                          
**AW     MVC   CFCCDT,RALODAT            Offset Date                            
         OI    CFRSTA,CFRSOF             Status Offset                          
         SP    CFAPPA,PRPRAMT            Applied amount                         
         B     PRCF125                                                          
*                                                                               
PRCF188  CLI   RALTYPE,RALTWOF           Type 3 = Write-Off                     
         BNE   PRCF192                                                          
**AW     MVC   CFCCNO,RALWREF            Write-Off Reference                    
**AW     MVC   CFCCDT,RALWDAT            Write-Off Date                         
         OI    CFRSTA,CFRSWO             Status W/O                             
         SP    CFAPPA,PRPRAMT            Applied amount                         
         B     PRCF125                                                          
*                                                                               
PRCF192  CLI   RALTYPE,RALTTTO           Type 4 = Transfer To                   
         BNE   PRCF204                                                          
         L     RE,AIO2                   A(Current receivable tran)             
         LA    RF,PRCPRTAB               Receivable transfer table              
         XC    CFREL(CFRLNQ),CFREL       Init the element                       
PRCF196  CLI   0(RF),0                   Next available space                   
         BE    PRCF200                                                          
         CLI   0(RF),X'FF'               End of transfer table?                 
         BE    PCFERX                                                           
         CLC   0(L'TRNKULA,RF),1(RE)     Did we transfer From before?           
         BE    PRCF200                   . yes                                  
         AHI   RF,L'PRCPRTAB             . no, check next entry                 
         B     PRCF196                                                          
*                                                                               
PRCF200  MVC   0(L'TRNKULA,RF),1(RE)     put account into rcvble tab            
         MVC   L'TRNKULA(L'TRNKSBR,RF),L'TRNKEY-1(RE) and subref                
         MVC   RK.TRNKULA,RALTULA                                               
         B     PRCF82                                                           
*                                                                               
PRCF204  CLI   RALTYPE,RALTTFR           Type 5 = Transfer From                 
         BNE   PRCF234                                                          
         OI    CFRSTA,CFRSTR             Status Transfer                        
         B     PRCF125                                                          
*----------------------------------------                                       
*Save Client Check Information                                                  
*----------------------------------------                                       
PRCF210  OC    CFCCDT,CFCCDT             Any date?                              
         BNZ   PRCF212                   . no                                   
         OC    CFCCNO,CFCCNO             Any Number?                            
         BZ    PRCF234                   . no                                   
PRCF212  TM    VCWFLAG,VCWPBILL+VCWUBILL Pre-billing                            
         BZ    PRCF214                                                          
         LA    R1,PBCHKS                 Pre-bill client pay checks             
         BAS   RE,CFADDCHK                                                      
PRCF214  TM    VCWFLAG,VCWSBILL          Standard billing                       
         BZ    PRCF234                                                          
         LA    R1,CPCHKS                 Client payment checks                  
         BAS   RE,CFADDCHK                                                      
*        --------------------------------                                       
PRCF234  XC    CFRCEL(CFRCLQ),CFRCEL                                            
PRCF236  GOTO1 AIO,IOSEQ+IOACCDIR+IO2    Read for receivable                    
         BE    PRCF96                                                           
         TM    IOERR,IOMAX                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    DISPFLAG,DISIOMAX         MAX IO'S                               
         J     PCFOKX                                                           
*----------------------------------------                                       
* ADJUST FOR BILLING                                                            
*----------------------------------------                                       
PRCF240  CP    CFUNAA,=P'0'              Any Unapplied amount?                  
         BE    PRCF244                   . no                                   
         CP    CFUNAA,PRCFGRS            Unapplied equal to gross?              
         BE    PRCF244                   . Yes, no adjustment needed            
*                                                                               
         ZAP   TEMPNUM,PRCFGRS                                                  
         SRP   TEMPNUM,6,0               Multiply for percent                   
         DP    TEMPNUM,CFUNAA            percent of gross to unapp              
         ZAP   DUB,TEMPNUM(L'TEMPNUM-L'CFUNAA) Save percent                     
*                                                                               
         ZAP   TEMPNUM,CFUNAA            Move unapplied into big field          
         MP    TEMPNUM,DUB               multiply by percent                    
         SRP   TEMPNUM,64-6,5            divide to get actual number            
         ZAP   CFUNAA,TEMPNUM            put adjusted number in                 
*                                                                               
         ZAP   TEMPNUM,CFAPPA            Applied total in big field             
         MP    TEMPNUM,DUB               multiply by percent                    
         SRP   TEMPNUM,64-6,5            divide to get actual number            
         ZAP   CFAPPA,TEMPNUM            put adjusted number in                 
*----------------------------------------                                       
PRCF244  J     PCFOKX                                                           
*----------------------------------------------------------------------         
* Add check info to check table - R1=A(Check table)                             
*----------------------------------------------------------------------         
         USING TCTABD,R1                 Bill Table                             
CFADDCHK LR    RF,R1                                                            
         LA    R1,1(R1)                                                         
CFAC10   CLI   0(RF),MAXCHKS             Reached bill maximum?                  
         BL    *+6                       . No, still room                       
         DC    H'0'                      . Yes, dump for now                    
         OC    0(TCTLQ,R1),0(R1)         Empty spot?                            
         BZ    CFAC30                    . yes, save bill info                  
         CLC   CFCCDT,TCTDT              Match on date?                         
         BNE   CFAC20                    . no, check next                       
         CLC   CFCCNO,TCTNO              Match on number?                       
         BE    CFACX                     . yes, then next ptael                 
CFAC20   LA    R1,TCTLQ(R1)              next table entry                       
         B     CFAC10                                                           
CFAC30   MVC   TCTDT,CFCCDT              Add bill to table                      
         MVC   TCTNO,CFCCNO                                                     
         SR    R0,R0                     keep count of bills                    
         IC    R0,0(RF)                                                         
         AHI   R0,1                                                             
         STC   R0,0(RF)                                                         
CFACX    BR    RE                                                               
         DROP  R1                                                               
*----------------------------------------------------------------------         
PCFOKX   J     OKXIT                                                            
PCFERX   J     ERXIT                                                            
         LTORG                                                                  
         DROP  JOB44                                                            
         EJECT                                                                  
***********************************************************************         
*        DISPLAY DIFFERENCE COLUMN FOR GRIDS                                    
*         R3 = ADDRESS OF GRID TAB ENTRY                                        
***********************************************************************         
         USING GCTBLD,R3                                                        
         USING TSARRECD,R4                                                      
GRDSP    NTR1  BASE=*,LABEL=*                                                   
         LHI   RC,OVERWORK-WORKD                                                
         LA    RC,WORKD(RC)              R8=RE-ESTABLISH WORK STORAGE           
         XC    TEMP,TEMP                                                        
         MVI   TEMP,C' '                                                        
         LHI   R1,1                                                             
*                                                                               
         L     R4,ATSARREC               R2=A(TSAR RECORD)                      
         LA    R4,TSARDATA                                                      
         USING TSARDATD,R4                                                      
*-----------------------------------                                            
* Client Check Date                                                             
*-----------------------------------                                            
         USING CFWELD,R4                                                        
GRDSP10  CLI   GCTCOID,GCCCDT            CLIENT CHECK DATE?                     
         BNE   GRDSP20                                                          
         ICM   R4,15,ACFRELD                                                    
         BZ    GRDSPX                                                           
         SR    R5,R5                                                            
         ICM   R5,1,CFWREL                                                      
         BZ    GRDSPX                                                           
         LA    R4,CFWREL+1                                                      
         USING TCTABD,R4                                                        
         LA    R2,TEMP                                                          
GRDSP11  OC    TCTDT,TCTDT                                                      
         BNZ   GRDSP14                                                          
         LA    R4,TCTLQ(R4)                                                     
         BCT   R5,GRDSP11                                                       
         B     GRDSPX                                                           
*                                                                               
GRDSP12  OC    TCTDT,TCTDT                                                      
         BZ    GRDSP15                                                          
         MVC   0(2,R2),=C', '                                                   
         LA    R2,2(R2)                                                         
*                                                                               
GRDSP14  GOTO1 VDATCON,DMCB,(1,TCTDT),(8,(R2))                                  
         LA    R2,8(R2)                                                         
GRDSP15  LA    R4,TCTLQ(R4)                                                     
         BCT   R5,GRDSP12                                                       
*                                                                               
         LR    R1,R2                                                            
         LA    R2,TEMP                                                          
         SR    R1,R2                                                            
         B     GRDSPX                                                           
*-----------------------------------                                            
* Client Check NUMBER                                                           
*-----------------------------------                                            
         USING CFWELD,R4                                                        
GRDSP20  CLI   GCTCOID,GCCCNO            CLIENT CHECK NUMBER?                   
         BNE   GRDSP30                                                          
         ICM   R4,15,ACFRELD                                                    
         BZ    GRDSPX                                                           
         SR    R5,R5                                                            
         ICM   R5,1,CFWREL                                                      
         BZ    GRDSPX                                                           
         LA    R4,CFWREL+1                                                      
         USING TCTABD,R4                                                        
         LA    R2,TEMP                                                          
GRDSP21  OC    TCTNO,TCTNO                                                      
         BNZ   GRDSP24                                                          
         LA    R4,TCTLQ(R4)                                                     
         BCT   R5,GRDSP21                                                       
         B     GRDSPX                                                           
*                                                                               
GRDSP22  OC    TCTNO,TCTNO                                                      
         BZ    GRDSP25                                                          
         MVC   0(2,R2),=C', '                                                   
         LA    R2,2(R2)                                                         
*                                                                               
GRDSP24  MVC   0(L'TCTNO,R2),TCTNO                                              
         LA    R2,L'TCTNO(R2)                                                   
GRDSP25  LA    R4,TCTLQ(R4)                                                     
         BCT   R5,GRDSP22                                                       
*                                                                               
         LR    R1,R2                                                            
         LA    R2,TEMP                                                          
         SR    R1,R2                                                            
         B     GRDSPX                                                           
*-----------------------------------                                            
* Pre-Billed Client Check Date                                                  
*-----------------------------------                                            
         USING CFWELD,R4                                                        
GRDSP30  CLI   GCTCOID,GCPBCCD                                                  
         BNE   GRDSP40                                                          
         ICM   R4,15,ACFVELD                                                    
         BZ    GRDSPX                                                           
         SR    R5,R5                                                            
         ICM   R5,1,CFWVEL                                                      
         BZ    GRDSPX                                                           
         LA    R4,CFWVEL+1                                                      
         USING TCTABD,R4                                                        
         LA    R2,TEMP                                                          
GRDSP31  OC    TCTDT,TCTDT                                                      
         BNZ   GRDSP34                                                          
         LA    R4,TCTLQ(R4)                                                     
         BCT   R5,GRDSP31                                                       
         B     GRDSPX                                                           
*                                                                               
GRDSP32  OC    TCTDT,TCTDT                                                      
         BZ    GRDSP35                                                          
         MVC   0(2,R2),=C', '                                                   
         LA    R2,2(R2)                                                         
*                                                                               
GRDSP34  GOTO1 VDATCON,DMCB,(1,TCTDT),(8,(R2))                                  
         LA    R2,8(R2)                                                         
GRDSP35  LA    R4,TCTLQ(R4)                                                     
         BCT   R5,GRDSP32                                                       
*                                                                               
         LR    R1,R2                                                            
         LA    R2,TEMP                                                          
         SR    R1,R2                                                            
         B     GRDSPX                                                           
*-----------------------------------                                            
* Pre-Billed Client Check Number                                                
*-----------------------------------                                            
         USING CFWELD,R4                                                        
GRDSP40  CLI   GCTCOID,GCPBCCN                                                  
         BNE   GRDSP70                                                          
         ICM   R4,15,ACFVELD                                                    
         BZ    GRDSPX                                                           
         SR    R5,R5                                                            
         ICM   R5,1,CFWVEL                                                      
         BZ    GRDSPX                                                           
         LA    R4,CFWVEL+1                                                      
         USING TCTABD,R4                                                        
         LA    R2,TEMP                                                          
GRDSP41  OC    TCTNO,TCTNO                                                      
         BNZ   GRDSP44                                                          
         LA    R4,TCTLQ(R4)                                                     
         BCT   R5,GRDSP41                                                       
         B     GRDSPX                                                           
*                                                                               
GRDSP42  OC    TCTNO,TCTNO                                                      
         BZ    GRDSP45                                                          
         MVC   0(2,R2),=C', '                                                   
         LA    R2,2(R2)                                                         
*                                                                               
GRDSP44  MVC   0(L'TCTNO,R2),TCTNO                                              
         LA    R2,L'TCTNO(R2)                                                   
GRDSP45  LA    R4,TCTLQ(R4)                                                     
         BCT   R5,GRDSP42                                                       
*                                                                               
         LR    R1,R2                                                            
         LA    R2,TEMP                                                          
         SR    R1,R2                                                            
         B     GRDSPX                                                           
*-----------------------------------                                            
* Status                                                                        
*-----------------------------------                                            
         USING TSARDATD,R4                                                      
GRDSP70  CLI   GCTCOID,GCSTAT            VENDOR STATUS                          
         BNE   GRDSP90                                                          
*                                                                               
         TM    TSDTEST,TRNSURG+TRNSAPPR+TRNSREV+TRNSHOLD                        
         BNZ   GRDSP78                                                          
         TM    TSDSEST,TRSSOFFS+TRSSVOID                                        
         BZ    GRDSPX                                                           
*                                                                               
GRDSP78  LA    R2,TEMP                                                          
         TM    TSDTEST,TRNSURG                                                  
         BZ    GRDSP79                                                          
         MVC   0(L'MX@URG,R2),MX@URG                                            
         MVI   L'MX@URG(R2),C','                                                
         LA    R2,L'MX@URG+1(R2)                                                
*                                                                               
GRDSP79  TM    TSDTEST,TRNSAPPR                                                 
         BZ    GRDSP80                                                          
         MVC   0(L'MX@APRVD,R2),MX@APRVD                                        
         MVI   L'MX@APRVD(R2),C','                                              
         LA    R2,L'MX@APRVD+1(R2)                                              
*                                                                               
GRDSP80  TM    TSDTEST,TRNSREV                                                  
         BZ    GRDSP81                                                          
         MVC   0(L'MX@RVRSL,R2),MX@RVRSL                                        
         MVI   L'MX@RVRSL(R2),C','                                              
         LA    R2,L'MX@RVRSL+1(R2)                                              
*                                                                               
GRDSP81  TM    TSDTEST,TRNSHOLD                                                 
         BZ    GRDSP82                                                          
         MVC   0(L'MX@HELD,R2),MX@HELD                                          
         MVI   L'MX@HELD(R2),C','                                               
         LA    R2,L'MX@HELD+1(R2)                                               
*                                                                               
GRDSP82  TM    TSDSEST,TRSSOFFS                                                 
         BZ    GRDSP83                                                          
         MVC   0(L'MX@OFFST,R2),MX@OFFST                                        
         MVI   L'MX@OFFST(R2),C','                                              
         LA    R2,L'MX@OFFST+1(R2)                                              
*                                                                               
GRDSP83  TM    TSDSEST,TRSSVOID                                                 
         BZ    GRDSP84                                                          
         MVC   0(L'MX@BNKVD,R2),MX@BNKVD                                        
         MVI   L'MX@BNKVD(R2),C','                                              
         LA    R2,L'MX@BNKVD+1(R2)                                              
*                                                                               
GRDSP84  BCTR  R2,0                                                             
         LR    R1,R2                                                            
         LA    R2,TEMP                                                          
         SR    R1,R2                                                            
         B     GRDSPX                                                           
*-----------------------------------                                            
* Receivable Status                                                             
*-----------------------------------                                            
         USING TSARDATD,R4                                                      
GRDSP90  CLI   GCTCOID,GCRST             Receivable Status                      
         BNE   GRDSPX                                                           
         TM    TSDRSTAT,TRXSRHLD+TRXSRQRD+TRNSREV+CFWRSWO+CFWRSOF               
         BZ    GRDSPX                                                           
         LA    R2,TEMP                                                          
*                                                                               
         TM    TSDRSTAT,TRXSRHLD                                                
         BZ    GRDSP92                                                          
         MVC   0(L'MX@HELD,R2),MX@HELD                                          
         MVI   L'MX@HELD(R2),C','                                               
         LA    R2,L'MX@HELD+1(R2)                                               
*                                                                               
GRDSP92  TM    TSDRSTAT,TRXSRQRD                                                
         BZ    GRDSP96                                                          
         MVC   0(L'MX@QUERD,R2),MX@QUERD                                        
         MVI   L'MX@QUERD(R2),C','                                              
         LA    R2,L'MX@QUERD+1(R2)                                              
*                                                                               
GRDSP96  TM    TSDRSTAT,TRNSREV                                                 
         BZ    GRDSP98                                                          
         MVC   0(L'MX@RVRSL,R2),MX@RVRSL                                        
         MVI   L'MX@RVRSL(R2),C','                                              
         LA    R2,L'MX@RVRSL+1(R2)                                              
*                                                                               
GRDSP98  TM    TSDRSTAT,CFWRSWO                                                 
         BZ    GRDSP100                                                         
         MVC   0(L'MX@WRTF,R2),MX@WRTF                                          
         MVI   L'MX@WRTF(R2),C','                                               
         LA    R2,L'MX@WRTF+1(R2)                                               
*                                                                               
GRDSP100 TM    TSDRSTAT,CFWRSOF                                                 
         BZ    GRDSP110                                                         
         MVC   0(L'MX@OFFST,R2),MX@OFFST                                        
         MVI   L'MX@OFFST(R2),C','                                              
         LA    R2,L'MX@OFFST+1(R2)                                              
*                                                                               
GRDSP110 TM    TSDRSTAT,CFWRSTR                                                 
         BZ    GRDSP112                                                         
         MVC   0(L'MX@XFR,R2),MX@XFR                                            
         MVI   L'MX@XFR(R2),C','                                                
         LA    R2,L'MX@XFR+1(R2)                                                
*                                                                               
GRDSP112 BCTR  R2,0                                                             
         LR    R1,R2                                                            
         LA    R2,TEMP                                                          
         SR    R1,R2                                                            
         B     GRDSPX                                                           
*                                                                               
GRDSPX   LTR   R1,R1                                                            
         BP    *+8                                                              
         LHI   R1,1                                                             
         J     XITR1                                                            
         LTORG                                                                  
         DROP  R3,R4,RB                                                         
         EJECT                                                                  
***********************************************************************         
* DETAIL GRID COLUMN TABLE - COVERED BY GCTBLD                                  
*        under this table is GCTBL2.  The columns for GCTBL2 must match         
*        the columns for GCTBL                                                  
***********************************************************************         
GCTBL    DS    0H                                                               
*        --------------------------------------------------                     
GCTINO   DC    AL1(GINOLQ,GCINO,L'MX@VIN,L'TSDINVN)        VENDOR               
         DC    AL2(MX@VIN-OVERWRKD,TSDINVN-TSARDATD)       INVOICE              
         DC    AL1(GCTIOVER+GCTITOT,0,0,0)                 NUMBER               
         DC    AL1(0,L'TEMPHEAD),AL2(TEMPHEAD-OVERWRKD)                         
GINOLQ   EQU   *-GCTINO                                                         
*        --------------------------------------------------                     
GCTIDAT  DC    AL1(GIDATLQ,GCIDAT,L'MX@VID,1)              VENDOR               
         DC    AL2(MX@VID-OVERWRKD,TSDDATE-TSARDATD)       INVOICE              
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)          DATE                 
         DC    AL1(0,0),AL2(0)                                                  
GIDATLQ  EQU   *-GCTIDAT                                                        
*        --------------------------------------------------                     
GCTIAMT  DC    AL1(GIAMTLQ,GCIAMT,L'MX@VIA,L'TSDAMNT)      VENDOR               
         DC    AL2(MX@VIA-OVERWRKD),AL2(TSDAMNT-TSARDATD)  INVOICE              
         DC    AL1(GCTIOVER+GCTITOT,0,GCTFNUM+GCTFRGHT,0)  AMOUNT               
         DC    AL1(0,L'TSTTOT),AL2(TSTTOT-TSARTOTD)                             
GIAMTLQ  EQU   *-GCTIAMT                                                        
*        --------------------------------------------------                     
GCTJOB   DC    AL1(GJOBLQ,GCCPJ,L'MX@CLIPK,L'TSDCPJ)       CLI/PRD/JOB          
         DC    AL2(MX@CLIPK-OVERWRKD),AL2(TSDCPJ-TSARDATD)                      
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
GJOBLQ   EQU   *-GCTJOB                                                         
*        --------------------------------------------------                     
GCTEXP   DC    AL1(GEXPLQ,GCEXP,L'MX@EXPAC,L'TSDEXP)       EXPENSE              
         DC    AL2(MX@EXPAC-OVERWRKD),AL2(TSDEXP-TSARDATD) ACCOUNT              
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
GEXPLQ   EQU   *-GCTEXP                                                         
*        --------------------------------------------------                     
GCTSVIB  DC    AL1(GSVIBLQ,GCSVIB,L'MX@VIB,0)              VENDOR               
         DC    AL2(MX@VIB-OVERWRKD,TSDCSTAT-TSARDATD)      INVOICE              
         DC    AL1(GCTIOVER,GCTIYON,0,0)                   BILLED               
         DC    AL1(0,0),AL2(0)                                                  
GSVIBLQ  EQU   *-GCTSVIB                                                        
GCTSVI1  DC    AL1(GSVI1LQ,GCSVIB,L'MX@BLD,TSDCIBQ)        BILLED               
         DC    AL2(MX@BLD-OVERWRKD,TSDCSTAT-TSARDATD)                           
         DC    AL1(GCTIOVER,GCTIYON,0,0)                                        
         DC    AL1(0,0),AL2(0)                                                  
GSVI1LQ  EQU   *-GCTSVI1                                                        
GCTSVI2  DC    AL1(GSVI2LQ,GCSVIB,L'MX@PART,TSDCIPBQ)      PARTIAL              
         DC    AL2(MX@PART-OVERWRKD,TSDCSTAT-TSARDATD)                          
         DC    AL1(GCTIOVER,GCTIYON,0,0)                                        
         DC    AL1(0,0),AL2(0)                                                  
GSVI2LQ  EQU   *-GCTSVI2                                                        
*        --------------------------------------------------                     
GCTBNET  DC    AL1(GBNETLQ,GCBNET,L'MX@VIBA,L'TSDBNET)     Amount               
         DC    AL2(MX@VIBA-OVERWRKD),AL2(TSDBNET-TSARDATD) Billed               
         DC    AL1(GCTIOVER+GCTITOT,0)                                          
         DC    AL1(GCTFNUM+GCTFRGHT,GCTIZAB)                                    
         DC    AL1(0,L'TSTBNET),AL2(TSTBNET-TSARTOTD)                           
GBNETLQ  EQU   *-GCTBNET                                                        
*        --------------------------------------------------                     
GCTCPAY  DC    AL1(GCPAYLQ,GCCPAY,L'MX@CPM,0)              Client               
         DC    AL2(MX@CPM-OVERWRKD),AL2(TSDCSTAT-TSARDATD) Payment              
         DC    AL1(GCTIOVER,GCTIYON,0,0)                                        
         DC    AL1(0,0),AL2(0)                                                  
GCPAYLQ  EQU   *-GCTCPAY                                                        
GCTCPA2  DC    AL1(GCPA2LQ,GCCPAY,L'MX@PYMNT,TSDCCPQ)      Payment              
         DC    AL2(MX@PYMNT-OVERWRKD),AL2(TSDCSTAT-TSARDATD)                    
         DC    AL1(GCTIOVER,GCTIYON,0,0)                                        
         DC    AL1(0,0),AL2(0)                                                  
GCPA2LQ  EQU   *-GCTCPA2                                                        
GCTPCP   DC    AL1(GPCPLQ,GCCPAY,L'MX@PART,TSDCPCQ)        Partial              
         DC    AL2(MX@PART-OVERWRKD),AL2(TSDCSTAT-TSARDATD)                     
         DC    AL1(GCTIOVER,GCTIYON,0,0)                                        
         DC    AL1(0,0),AL2(0)                                                  
GPCPLQ   EQU   *-GCTPCP                                                         
*        --------------------------------------------------                     
GCTCPAMT DC    AL1(GCPAMTLQ,GCCPAMT,L'MX@CPAMT,L'TSDCPA)   Client               
         DC    AL2(MX@CPAMT-OVERWRKD),AL2(TSDCPA-TSARDATD) Payment              
         DC    AL1(GCTIOVER+GCTITOT,0)                     Amount               
         DC    AL1(GCTFNUM+GCTFRGHT,GCTIZAB)                                    
         DC    AL1(0,L'TSTCPA),AL2(TSTCPA-TSARTOTD)                             
GCPAMTLQ EQU   *-GCTCPAMT                                                       
*        --------------------------------------------------                     
GCTCCDT  DC    AL1(GCCDTLQ,GCCCDT,L'MX@RSRCD,0)            Client               
         DC    AL2(MX@RSRCD-OVERWRKD,AGRDSP-OVERWRKD)      Check Date           
         DC    AL1(GCTIOVER+GCTIROUT,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
GCCDTLQ  EQU   *-GCTCCDT                                                        
*        --------------------------------------------------                     
GCTCCNO  DC    AL1(GCCNOLQ,GCCCNO,L'MX@RSRCK,0)            Client               
         DC    AL2(MX@RSRCK-OVERWRKD,AGRDSP-OVERWRKD)      Check                
         DC    AL1(GCTIOVER+GCTIROUT,0,0,0)                Number               
         DC    AL1(0,0),AL2(0)                                                  
GCCNOLQ  EQU   *-GCTCCNO                                                        
*        --------------------------------------------------                     
GCTJBXF  DC    AL1(GJBXFLQ,GCJBXF,L'MX@TT034,0)              Job                
         DC    AL2(MX@TT034-OVERWRKD),AL2(TSDCSTAT-TSARDATD) Transfer           
         DC    AL1(GCTIOVER,GCTIYON,0,0)                                        
         DC    AL1(0,0),AL2(0)                                                  
GJBXFLQ  EQU   *-GCTJBXF                                                        
GCTJBX2  DC    AL1(GJBX2LQ,GCJBXF,L'MX@XFR,TSDCJXFQ)         Transfer           
         DC    AL2(MX@XFR-OVERWRKD),AL2(TSDCSTAT-TSARDATD)                      
         DC    AL1(GCTIOVER,GCTIYON,0,0)                                        
         DC    AL1(0,0),AL2(0)                                                  
GJBX2LQ  EQU   *-GCTJBX2                                                        
*        --------------------------------------------------                     
GCTPBLAM DC    AL1(GPBLAMLQ,GCPBLAM,L'MX@PBLAM,L'TSDPBA)   Pre-Billed           
         DC    AL2(MX@PBLAM-OVERWRKD),AL2(TSDPBA-TSARDATD) Amount               
         DC    AL1(GCTIOVER,0,GCTFNUM+GCTFRGHT,GCTIZAB)                         
         DC    AL1(0,0),AL2(0)                                                  
GPBLAMLQ EQU   *-GCTPBLAM                                                       
*        --------------------------------------------------                     
GCTPBCPM DC    AL1(GPBCPMLQ,GCPBCPM,L'MX@PBCPM,0)            Pre-Billed         
         DC    AL2(MX@PBCPM-OVERWRKD),AL2(TSDCSTAT-TSARDATD) Client             
         DC    AL1(GCTIOVER,GCTIYON,0,0)                     Payment            
         DC    AL1(0,0),AL2(0)                                                  
GPBCPMLQ EQU   *-GCTPBCPM                                                       
GCTPBCP2 DC    AL1(GPBCP2LQ,GCPBCPM,L'MX@PYMNT,TSDCPBQ)      Pre-Billed         
         DC    AL2(MX@PYMNT-OVERWRKD),AL2(TSDCSTAT-TSARDATD) Client             
         DC    AL1(GCTIOVER,GCTIYON,0,0)                     Payment            
         DC    AL1(0,0),AL2(0)                                                  
GPBCP2LQ EQU   *-GCTPBCP2                                                       
GCTPBCPP DC    AL1(GPBCPPLQ,GCPBCPM,L'MX@PART,TSDCPBPQ)      Partial            
         DC    AL2(MX@PART-OVERWRKD),AL2(TSDCSTAT-TSARDATD)                     
         DC    AL1(GCTIOVER,GCTIYON,0,0)                                        
         DC    AL1(0,0),AL2(0)                                                  
GPBCPPLQ EQU   *-GCTPBCPP                                                       
*        --------------------------------------------------                     
GCTPBCPA DC    AL1(GPBCPALQ,GCPBCPA,L'MX@PBCPA,L'TSDPBCPA)   Pre-Billed         
         DC    AL2(MX@PBCPA-OVERWRKD),AL2(TSDPBCPA-TSARDATD) Client             
         DC    AL1(GCTIOVER,0)                               Payment            
         DC    AL1(GCTFNUM+GCTFRGHT,GCTIZAB)                 Amount             
         DC    AL1(0,0),AL2(0)                                                  
GPBCPALQ EQU   *-GCTPBCPA                                                       
*        --------------------------------------------------                     
GCTPCCD  DC    AL1(GPCCDLQ,GCPBCCD,L'MX@PBCCD,0)           Pre-Billed           
         DC    AL2(MX@PBCCD-OVERWRKD,AGRDSP-OVERWRKD)      Client               
         DC    AL1(GCTIOVER+GCTIROUT,0,0,0)                Check Date           
         DC    AL1(0,0),AL2(0)                                                  
GPCCDLQ  EQU   *-GCTPCCD                                                        
*        --------------------------------------------------                     
GCTPCCN  DC    AL1(GPCCNLQ,GCPBCCN,L'MX@PBCCN,0)           Pre-Billed           
         DC    AL2(MX@PBCCN-OVERWRKD,AGRDSP-OVERWRKD)      Client               
         DC    AL1(GCTIOVER+GCTIROUT,0,0,0)                Check                
         DC    AL1(0,0),AL2(0)                             Number               
GPCCNLQ  EQU   *-GCTPCCN                                                        
*        --------------------------------------------------                     
GCTCEST  DC    AL1(GCESTLQ,GCCEST,L'MX@CEST,L'TSDCEST)     Current              
         DC    AL2(MX@CEST-OVERWRKD),AL2(TSDCEST-TSARDATD) Estimate             
         DC    AL1(GCTIOVER,0)                                                  
         DC    AL1(GCTFNUM+GCTFRGHT,GCTIZAB)                                    
         DC    AL1(0,0),AL2(0)                                                  
GCESTLQ  EQU   *-GCTCEST                                                        
*        --------------------------------------------------                     
GCTAPED  DC    AL1(GAPEDLQ,GCAPED,L'MX@AEDTE,1)            Approved             
         DC    AL2(MX@AEDTE-OVERWRKD,TSDDTAPP-TSARDATD)    Est                  
         DC    AL1(GCTIOVER,0)                             Date                 
         DC    AL1(GCTFDAT+GCTFRGHT,0)                                          
         DC    AL1(0,0),AL2(0)                                                  
GAPEDLQ  EQU   *-GCTAPED                                                        
*        --------------------------------------------------                     
GCTRST   DC    AL1(GRSTLQ,GCRST,L'MX@RSRCS,0)              Receivalbe           
         DC    AL2(MX@RSRCS-OVERWRKD,AGRDSP-OVERWRKD)      Status               
         DC    AL1(GCTIOVER+GCTIROUT,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
GRSTLQ   EQU   *-GCTRST                                                         
*        --------------------------------------------------                     
GCTADT   DC    AL1(GADTLQ,GCADTE,L'MX@ACTDT,2)             Activity             
         DC    AL2(MX@ACTDT-OVERWRKD,TSDACDAT-TSARDATD)    Date                 
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                               
         DC    AL1(0,0),AL2(0)                                                  
GADTLQ   EQU   *-GCTADT                                                         
*        --------------------------------------------------                     
GCTGBR   DC    AL1(GGBRLQ,GCBATR,L'MX@RSBRF,L'TSDBREF)     Batch Ref            
         DC    AL2(MX@RSBRF-OVERWRKD,TSDBREF-TSARDATD)                          
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
GGBRLQ   EQU   *-GCTGBR                                                         
*        --------------------------------------------------                     
GCTDISC  DC    AL1(GDISCLQ,GCDISC,L'MX@DISAM,L'TSDISC)     Discount             
         DC    AL2(MX@DISAM-OVERWRKD),AL2(TSDISC-TSARDATD)                      
         DC    AL1(GCTIOVER+GCTITOT,0)                                          
         DC    AL1(GCTFNUM+GCTFRGHT,GCTIZAB)                                    
         DC    AL1(0,L'TSTDISC),AL2(TSTDISC-TSARTOTD)                           
         DC    C'SR',C'SP',C'SQ',C'SS',C'ST'                                    
         DC    C'SU',C'SV',C'SW',C'SX',C'SY'                                    
GDISCLQ  EQU   *-GCTDISC                                                        
*        --------------------------------------------------                     
GCTDUDT  DC    AL1(GDUDLQ,GCDUE,L'MX@DUEDT,2)              Due Date             
         DC    AL2(MX@DUEDT-OVERWRKD),AL1(DUEELQ,DUEDATE-DUEELD)                
         DC    AL1(GCTIOVER+GCTIELEM,0)                                         
         DC    AL1(GCTFDAT+GCTFRGHT,0)                                          
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SP',C'SQ',C'SS',C'ST'                                          
         DC    C'SU',C'SV',C'SW',C'SX',C'SY'                                    
GDUDLQ   EQU   *-GCTDUDT                                                        
*        --------------------------------------------------                     
GCTMOA   DC    AL1(GMOALQ,GCMOA,L'MX@MOA,1)                MOA                  
         DC    AL2(MX@MOA-OVERWRKD,TSDTMOA-TSARDATD)                            
         DC    AL1(GCTIOVER,GCTIMONO,GCTFDAT+GCTFRGHT,0)                        
         DC    AL1(0,0),AL2(0)                                                  
GMOALQ   EQU   *-GCTMOA                                                         
*        --------------------------------------------------                     
GCTOFF   DC    AL1(GOFFLQ,GCOFF,L'MX@OFF,L'TSDOFF)         Office               
         DC    AL2(MX@OFF-OVERWRKD,TSDOFF-TSARDATD)                             
         DC    AL1(GCTIOVER,0,GCTFCENT,0)                                       
         DC    AL1(0,0),AL2(0)                                                  
GOFFLQ   EQU   *-GCTOFF                                                         
*        --------------------------------------------------                     
GCTSTAT  DC    AL1(GSTATLQ,GCSTAT,L'MX@STT,0)              Status               
         DC    AL2(MX@STT-OVERWRKD,AGRDSP-OVERWRKD)                             
         DC    AL1(GCTIOVER+GCTIROUT,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
GSTATLQ  EQU   *-GCTSTAT                                                        
*        --------------------------------------------------                     
GCTSEQU  DC    AL1(GSEQULQ,GCSEQ,L'MX@SEQ,1)          SEQUENCE NUMBER           
         DC    AL2(MX@SEQ-OVERWRKD,TSDSREF-TSARDATD)                            
         DC    AL1(GCTIOVER,GCTIBIN+GCTIDDS,GCTFNUM+GCTFRGHT,0)                 
         DC    AL1(0,0),AL2(0)                                                  
GSEQULQ  EQU   *-GCTSEQU                                                        
*        --------------------------------------------------                     
GCTDA    DC    AL1(GDALQ,GCDA,L'MX@DSCAD,L'TSDDA)          DISK                 
         DC    AL2(MX@DSCAD-OVERWRKD,TSDDA-TSARDATD)       ADDRESS              
         DC    AL1(GCTIOVER,GCTIHEX+GCTIDDS,0,0)                                
         DC    AL1(0,0),AL2(0)                                                  
GDALQ    EQU   *-GCTDA                                                          
*        --------------------------------------------------                     
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*----------------------------------------------------------------------         
* DETAIL GRID COLUMN TABLE - COVERED BY GCTBLD                                  
*        This table redifines the columns for the above table.                  
*        The column for this table must match GCTBL (same order etc.)           
*        This table is used for invoices prior to jan/2003, when                
*        workcodes were not stored on the vendor transaction                    
*----------------------------------------------------------------------         
GCTBL2   DS    0H                                                               
*        --------------------------------------------------                     
         DC    AL1(GINOLQ,GCINO,L'MX@VIN,L'TSDINVN)        VENDOR               
         DC    AL2(MX@VIN-OVERWRKD,TSDINVN-TSARDATD)       INVOICE              
         DC    AL1(GCTIOVER+GCTITOT,0,0,0)                 NUMBER               
         DC    AL1(0,L'TEMPHEAD),AL2(TEMPHEAD-OVERWRKD)                         
*        --------------------------------------------------                     
         DC    AL1(GIDATLQ,GCIDAT,L'MX@VID,1)              VENDOR               
         DC    AL2(MX@VID-OVERWRKD,TSDDATE-TSARDATD)       INVOICE              
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)          DATE                 
         DC    AL1(0,0),AL2(0)                                                  
*        --------------------------------------------------                     
         DC    AL1(GIAMTLQ,GCIAMT,L'MX@VIA,L'TSDAMNT)      VENDOR               
         DC    AL2(MX@VIA-OVERWRKD),AL2(TSDAMNT-TSARDATD)  INVOICE              
         DC    AL1(GCTIOVER+GCTITOT,0,GCTFNUM+GCTFRGHT,0)  AMOUNT               
         DC    AL1(0,L'TSTTOT),AL2(TSTTOT-TSARTOTD)                             
*        --------------------------------------------------                     
         DC    AL1(GJOBLQ,GCCPJ,L'MX@CLIPK,L'TSDCPJ)       CLI/PRD/JOB          
         DC    AL2(MX@CLIPK-OVERWRKD),AL2(TSDCPJ-TSARDATD)                      
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
*        --------------------------------------------------                     
         DC    AL1(GEXPLQ,GCEXP,L'MX@EXPAC,L'TSDEXP)       EXPENSE              
         DC    AL2(MX@EXPAC-OVERWRKD),AL2(TSDEXP-TSARDATD) ACCOUNT              
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
*        ----------------------------------------------DEAD                     
         DC    AL1(GSVIBLQ,GCSVIB,0,L'TSDUNK)              VENDOR               
         DC    AL2(0),AL2(TSDUNK-TSARDATD)                 INVOICE              
         DC    AL1(0,0,0,0)                                BILLED               
         DC    AL1(0,0),AL2(0)                                                  
*        ----------------------------------------------DEAD                     
         DC    AL1(GBNETLQ,GCBNET,0,L'TSDUNK)              Amount               
         DC    AL2(0),AL2(TSDUNK-TSARDATD)                 Billed               
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*        ----------------------------------------------DEAD                     
         DC    AL1(GCPAYLQ,GCCPAY,0,L'TSDUNK)              Client               
         DC    AL2(0),AL2(TSDUNK-TSARDATD)                 Payment              
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*        ----------------------------------------------DEAD                     
         DC    AL1(GCPAMTLQ,GCCPAMT,0,L'TSDUNK)            Client               
         DC    AL2(0),AL2(TSDUNK-TSARDATD)                 Payment              
         DC    AL1(0,0,0,0)                                Amount               
         DC    AL1(0,0),AL2(0)                                                  
*        ----------------------------------------------DEAD                     
         DC    AL1(GCCDTLQ,GCCCDT,0,L'TSDUNK)              Client               
         DC    AL2(0),AL2(TSDUNK-TSARDATD)                 Check Date           
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*        ----------------------------------------------DEAD                     
         DC    AL1(GCCNOLQ,GCCCNO,0,L'TSDUNK)              Client               
         DC    AL2(0),AL2(TSDUNK-TSARDATD)                 Check                
         DC    AL1(0,0,0,0)                                Number               
         DC    AL1(0,0),AL2(0)                                                  
*        ----------------------------------------------DEAD                     
         DC    AL1(GJBXFLQ,GCJBXF,0,L'TSDUNK)              Job                  
         DC    AL2(0),AL2(TSDUNK-TSARDATD)                 Transfer             
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*        ----------------------------------------------DEAD                     
         DC    AL1(GPBLAMLQ,GCPBLAM,0,L'TSDUNK)            Pre-Billed           
         DC    AL2(0),AL2(TSDUNK-TSARDATD)                 Amount               
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*        ----------------------------------------------DEAD                     
         DC    AL1(GPBCPMLQ,GCPBCPM,0,L'TSDUNK)            Pre-Billed           
         DC    AL2(0),AL2(TSDUNK-TSARDATD)                 Client               
         DC    AL1(0,0,0,0)                                Payment              
         DC    AL1(0,0),AL2(0)                                                  
*        ----------------------------------------------DEAD                     
         DC    AL1(GPBCPALQ,GCPBCPA,0,L'TSDUNK)            Pre-Billed           
         DC    AL2(0),AL2(TSDUNK-TSARDATD)                 Client               
         DC    AL1(0,0,0,0)                                Payment              
         DC    AL1(0,0),AL2(0)                             Amount               
*        ----------------------------------------------DEAD                     
         DC    AL1(GPCCDLQ,GCPBCCD,0,L'TSDUNK)             Pre-Billed           
         DC    AL2(0),AL2(TSDUNK-TSARDATD)                 Client               
         DC    AL1(0,0,0,0)                                Check Date           
         DC    AL1(0,0),AL2(0)                                                  
*        ----------------------------------------------DEAD                     
         DC    AL1(GPCCNLQ,GCPBCCN,0,L'TSDUNK)             Pre-Billed           
         DC    AL2(0),AL2(TSDUNK-TSARDATD)                 Client               
         DC    AL1(0,0,0,0)                                Check                
         DC    AL1(0,0),AL2(0)                             Number               
*        --------------------------------------------------                     
         DC    AL1(GCESTLQ,GCCEST,L'MX@CEST,L'TSDCEST)     Current              
         DC    AL2(MX@CEST-OVERWRKD),AL2(TSDCEST-TSARDATD) Estimate             
         DC    AL1(GCTIOVER,0)                                                  
         DC    AL1(GCTFNUM+GCTFRGHT,GCTIZAB)                                    
         DC    AL1(0,0),AL2(0)                                                  
*        --------------------------------------------------                     
         DC    AL1(GAPEDLQ,GCAPED,L'MX@AEDTE,1)            Estimate             
         DC    AL2(MX@AEDTE-OVERWRKD,TSDDTAPP-TSARDATD)    Approval             
         DC    AL1(GCTIOVER,0)                             Date                 
         DC    AL1(GCTFDAT+GCTFRGHT,0)                                          
         DC    AL1(0,0),AL2(0)                                                  
*        ----------------------------------------------DEAD                     
         DC    AL1(GRSTLQ,GCRST,0,L'TSDUNK)                Receivalbe           
         DC    AL2(0),AL2(TSDUNK-TSARDATD)                 Status               
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*        --------------------------------------------------                     
         DC    AL1(GADTLQ,GCADTE,L'MX@ACTDT,2)             Activity             
         DC    AL2(MX@ACTDT-OVERWRKD,TSDACDAT-TSARDATD)    Date                 
         DC    AL1(GCTIOVER,0,GCTFDAT+GCTFRGHT,0)                               
         DC    AL1(0,0),AL2(0)                                                  
*        --------------------------------------------------                     
         DC    AL1(GGBRLQ,GCBATR,L'MX@RSBRF,L'TSDBREF)     Batch Ref            
         DC    AL2(MX@RSBRF-OVERWRKD,TSDBREF-TSARDATD)                          
         DC    AL1(GCTIOVER,0,0,0)                                              
         DC    AL1(0,0),AL2(0)                                                  
*        --------------------------------------------------                     
         DC    AL1(GDISCLQ,GCDISC,L'MX@DISAM,L'TSDISC)     Discount             
         DC    AL2(MX@DISAM-OVERWRKD),AL2(TSDISC-TSARDATD)                      
         DC    AL1(GCTIOVER+GCTITOT,0)                                          
         DC    AL1(GCTFNUM+GCTFRGHT,GCTIZAB)                                    
         DC    AL1(0,L'TSTDISC),AL2(TSTDISC-TSARTOTD)                           
         DC    C'SR',C'SP',C'SQ',C'SS',C'ST'                                    
         DC    C'SU',C'SV',C'SW',C'SX',C'SY'                                    
*        --------------------------------------------------                     
         DC    AL1(GDUDLQ,GCDUE,L'MX@DUEDT,2)              Due Date             
         DC    AL2(MX@DUEDT-OVERWRKD),AL1(DUEELQ,DUEDATE-DUEELD)                
         DC    AL1(GCTIOVER+GCTIELEM,0)                                         
         DC    AL1(GCTFDAT+GCTFRGHT,0)                                          
         DC    AL1(0,0),AL2(0)                                                  
         DC    C'SP',C'SQ',C'SS',C'ST'                                          
         DC    C'SU',C'SV',C'SW',C'SX',C'SY'                                    
*        --------------------------------------------------                     
         DC    AL1(GMOALQ,GCMOA,L'MX@MOA,1)                MOA                  
         DC    AL2(MX@MOA-OVERWRKD,TSDTMOA-TSARDATD)                            
         DC    AL1(GCTIOVER,GCTIMONO,GCTFDAT+GCTFRGHT,0)                        
         DC    AL1(0,0),AL2(0)                                                  
*        --------------------------------------------------                     
         DC    AL1(GOFFLQ,GCOFF,L'MX@OFF,L'TSDOFF)         Office               
         DC    AL2(MX@OFF-OVERWRKD,TSDOFF-TSARDATD)                             
         DC    AL1(GCTIOVER,0,GCTFCENT,0)                                       
         DC    AL1(0,0),AL2(0)                                                  
*        --------------------------------------------------                     
         DC    AL1(GSTATLQ,GCSTAT,L'MX@STT,0)              Status               
         DC    AL2(MX@STT-OVERWRKD,AGRDSP-OVERWRKD)                             
         DC    AL1(GCTIOVER+GCTIROUT,0,0,0)                                     
         DC    AL1(0,0),AL2(0)                                                  
*        --------------------------------------------------                     
         DC    AL1(GSEQULQ,GCSEQ,L'MX@SEQ,1)               Sq                   
         DC    AL2(MX@SEQ-OVERWRKD,TSDSREF-TSARDATD)                            
         DC    AL1(GCTIOVER,GCTIBIN+GCTIDDS,GCTFNUM+GCTFRGHT,0)                 
         DC    AL1(0,0),AL2(0)                                                  
*        --------------------------------------------------                     
         DC    AL1(GDALQ,GCDA,L'MX@DSCAD,L'TSDDA)          DISK                 
         DC    AL2(MX@DSCAD-OVERWRKD,TSDDA-TSARDATD)       ADDRESS              
         DC    AL1(GCTIOVER,GCTIHEX+GCTIDDS,0,0)                                
         DC    AL1(0,0),AL2(0)                                                  
*        --------------------------------------------------------------         
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*----------------------------------------------------------------------         
* GRID COLUMN EQUATES                                                           
*----------------------------------------------------------------------         
GCINO    EQU   99         Vendor Invoice Number                                 
GCIDAT   EQU   1          Vendor Invoice Date                                   
GCIAMT   EQU   2          Vendor Invoice Amount                                 
GCCPJ    EQU   3          Client/Product/Job                                    
GCSVIB   EQU   4          Vendor Invoice Billed                                 
GCBNET   EQU   5          Amount Billed                                         
GCCPAY   EQU   6          Client Payment                                        
GCCPAMT  EQU   7          Client Payment Amount                                 
GCCCDT   EQU   8          Cliend Check Date                                     
GCCCNO   EQU   9          Client Check Number                                   
GCPBLAM  EQU   10         Pre-Billed Amount                                     
GCPBCPM  EQU   11         Pre-Billed Client Payment                             
GCPBCPA  EQU   12         Pre-Billed Client Payment Amount                      
GCPBCCD  EQU   13         Pre-Billed Client Check Date                          
GCPBCCN  EQU   14         Pre-Billed Client Check Number                        
GCCEST   EQU   15         Current Estimate                                      
GCAPED   EQU   16         Approved Estimate Date                                
GCBATR   EQU   17         Batch Reference                                       
GCADTE   EQU   18         Activity Date                                         
GCDUE    EQU   19         Due Date                                              
GCOFF    EQU   20         Office                                                
GCMOA    EQU   21         MOA                                                   
GCSTAT   EQU   22         Status                                                
GCRST    EQU   23         Receivable Status                                     
GCEXP    EQU   24         Expense Account                                       
GCDISC   EQU   25         Discount Amount                                       
GCSEQ    EQU   26         Transaction Sequence Sub-reference                    
GCJBXF   EQU   27         Job Transfer                                          
* DDS ONLY                                                                      
GCDA     EQU   71         Disk Address                                          
**********************************************************************          
* DETAIL WORKING STORAGE                                                        
**********************************************************************          
OVERWRKD DSECT                                                                  
ORELO    DS    A                   OVERLAY RELOCATION                           
AGCTBL   DS    A                   ADDRESS OF GRID COLUMN TABLE                 
AGCTBL2  DS    A                   ADDRESS OF GRID COLUMN TABLE                 
AGRDSP   DS    A                   ADDRESS OF GRID SPECIAL ROUTINE              
DADDRESS DS    AL(L'TRNKDA)        TRANSACTION DISK ADDRESS                     
TRANAMNT DS    CL(L'TRNAMNT)       AGENCY/LOCAL CURRENCY AMOUNT                 
TEMPNUM  DS    PL16                TEMPORARY NUMBER STORAGE                     
JOBCEST  DS    PL8                 CURRENT ESTIMATE FOR JOB                     
TEMPDAT  DS    PL3                 TEMPORARY DATE STORAGE                       
TEMPHEAD DS    CL30                TEMPORARY HEADER STORAGE                     
TEMPEXP  DS    CL(L'TSDEXP)        TEMPORARY EXPENSE ACCOUNT STORAGE            
TRANCURR DS    CL(L'AFCCURR)       TRANSACTION CURRENCY                         
NEEDAE   DS    C                   NEED APPROVAL                                
*                                                                               
FLTFLAG  DS    X                                                                
DUEELEQ  EQU   X'20'               . DUE DATE ELEMENT FOUND                     
DUERQDQ  EQU   X'10'               . DUE DATE ELEMENT REQUIRED                  
AFPELEQ  EQU   X'08'               . ARTISTE FEE ELEMENT FOUND                  
AFPRQDQ  EQU   X'04'               . ARTISTE FEE ELEMENT REQUIRED               
MXPELEQ  EQU   X'02'               . MXPELD ELEMENT FOUND                       
MXPRQDQ  EQU   X'01'               . MXPELD ELEMENT REQUIRED                    
*                                                                               
VCWFLAG  DS    X                   VENDOR CASHFLOW WORKING FLAG                 
VCWSBILL EQU   X'80'               . STANDARD BILLING                           
VCWPBILL EQU   X'40'               . PROCESSING (%) PRE-BILLING                 
VCWUBILL EQU   X'20'               . PROCESSING 47 PRE-BILLING                  
VCWRBILL EQU   X'10'               . RETAIL BILLING                             
VCWJBXFQ EQU   X'08'               . JOB TRANSFER                               
VCWJBFOQ EQU   X'04'               . JOB BILLING FOUND                          
*-----------------------------------                                            
*      STORAGE FOR CASHFLOW                                                     
*-----------------------------------                                            
CFREL    DS    0X                                                               
CFAPPA   DS    PL8                                                              
CFUNAA   DS    PL8                                                              
CFRSTA   DS    XL1                 RECEIVABLE STATUS                            
*        EQU   X'80'               .  HELD      (X'80'-TRXSRHLD)                
*        EQU   X'40'               .  QUERIED   (X'40'-TRXSRQRD)                
*        EQU   X'20'               .  REVERSAL  (X'20'-TRNSREV)                 
CFRSWO   EQU   X'08'               .  WRITE OFF                                 
CFRSOF   EQU   X'04'               .  OFFSET                                    
CFRSTR   EQU   X'02'               .  TRANSFER                                  
CFRLNQ   EQU   *-CFREL                                                          
CFRCEL   DS    0X                                                               
CFCCNO   DS    CL6                 CLIENT CHECK NUMBER                          
CFCCDT   DS    PL3                 CLIENT CHECK DATE                            
*CFWCPDT DS    PL3                 CLIENT DEPOSIT DATE                          
*CFWCDDT DS    PL2                 CLIENT DUE DATE                              
CFRCLQ   EQU   *-CFRCEL                                                         
*                                                                               
UNAUNA   DS    PL8                  Unadjusted Unapplied                        
UNAAPP   DS    PL8                  Unadjusted Applied                          
UNAPBU   DS    PL8                  Unadjusted Pre-Bill Unapplied               
UNAPBA   DS    PL8                  Unadjusted Pre-Bill Applied                 
PRCFGRS  DS    PL8                  Gross Amount                                
PRPRAMT  DS    PL8                  Processing Amount                           
UFBAMNT  DS    PL8                  Up-front billing amount (net)               
UFBCOMM  DS    PL8                  Up-front billed commission                  
PRCPRSTA DS    X                    Process prod receivable status              
PRCPAPB  EQU   X'40'                . Already Processed Bill                    
PRCPNOAD EQU   X'20'                . No adjusting needed                       
PRCPRUB  EQU   X'10'                . Already read for upfront billing          
*                                   Receivable Transfer Table                   
PRCPRTAB DS    (MAXRCVTQ)XL(L'TRNKULA+L'TRNKSBR)                                
         DC    X'FF'                END OF TABLE                                
MAXRCVTQ EQU   5                                                                
*        ---------------------------                                            
*        CASHFLOW TEMPORARY TABLES                                              
*        ---------------------------                                            
CFTABS   DS    0H                                                               
*                                                                               
SVNOB    DS    X                    NUMBER OF BILLS                             
SVBTAB   DS    (MAXBILLS)XL(TBTLQ)  BILL TABLE                                  
SVBTABLQ EQU   *-SVBTAB                                                         
MAXBILLS EQU   24                                                               
*                                                                               
SVNOW    DS    X                    NUMBER OF Workcodes                         
SVWTAB   DS    (MAXWCDES)XL(L'TRNKWORK) Workcode Table                          
SVWTABLQ EQU   *-SVWTAB                                                         
MAXWCDES EQU   20                                                               
*                                                                               
CPCHKS   DS    X                    NUMBER OF CLIENT PAYMENT CHECKS             
CPCTAB   DS    (MAXCHKS)XL(TCTLQ)   CHECK TABLE                                 
CPCLQ    EQU   *-CPCHKS                                                         
MAXCHKS  EQU   30                                                               
*                                                                               
PBCHKS   DS    X                    # OF PRE-BILLED CLI PAY CHECKS              
PBCTAB   DS    (MAXCHKS)XL(TCTLQ)                                               
PBCLQ    EQU   *-PBCHKS                                                         
*                                                                               
CFTLNQ   EQU   *-CFTABS                                                         
*-----------------------------------                                            
* DATA DICTIONARY ENTRIES                                                       
*-----------------------------------                                            
DSMIX    DS    0C                                                               
MX@HELP  DS    CL4               Help                                           
MX@URG   DS    CL6               Urgent                                         
MX@HELD  DS    CL4               Held                                           
MX@INT   DS    CL8               Internal                                       
MX@APRVD DS    CL8               Approved                                       
MX@RVRSL DS    CL8               Reversal                                       
MX@BNKVD DS    CL6               Voided                                         
MX@QUERD DS    CL7               Queried                                        
MX@WRTF  DS    CL9               Write off                                      
MX@XFR   DS    CL8               Transfer                                       
MX@INVC2 DS    CL14              Invoice Number                                 
MX@RSIDT DS    CL12              Invoice Date                                   
MX@STT   DS    CL6               Status                                         
MX@RSRCK DS    CL14              Client Chk Num                                 
MX@RSRCD DS    CL15              Client Chk date                                
MX@OFFST DS    CL6               OFFSET                                         
MX@ACCT  DS    CL15              Account Total                                  
MX@CLIPK DS    CL18              Client/Product/Job                             
MX@RSBRF DS    CL15              Batch Reference                                
MX@ACTDT DS    CL13              Activity Date                                  
MX@DUEDT DS    CL8               Due Date                                       
MX@OFF   DS    CL6               Office                                         
MX@MOA   DS    CL3               MOA                                            
MX@DSCAD DS    CL3               D/A                                            
MX@RSRCS DS    CL18              Receivable Status                              
MX@CEST  DS    CL16              Current Estimate                               
MX@PART  DS    CL7               Partial                                        
MX@BLD   DS    CL6               Billed                                         
MX@PYMNT DS    CL7               Payment                                        
MX@EXPAC DS    0CL15             Expense Account                                
MX@EXPA  DS    CL8               Expense                                        
MX@ACC   DS    CL7               Account                                        
MX@VIN   DS    CL14              Invoice Number                                 
MX@VID   DS    CL12              Invoice Date                                   
MX@VIA   DS    CL14              Invoice Amount                                 
MX@VIB   DS    CL14              Invoice Billed                                 
MX@VIBA  DS    CL18              Invoice Billed Amt                             
MX@CPM   DS    CL14              Client Payment                                 
MX@CPAMT DS    CL18              Client Payment Amt                             
MX@PBLAM DS    CL17              Pre-Billed Amount                              
MX@PBCPM DS    CL18              Pre-Billed Payment                             
MX@PBCPA DS    CL22              Pre-Billed Payment Amt                         
MX@PBCCD DS    CL19              Pre-Billed Chk Date                            
MX@PBCCN DS    CL18              Pre-Billed Chk Num                             
MX@AEDTE DS    CL17              Approved Est Date                              
MX@DISAM DS    CL15              Discount Amount                                
MX@TFOR  DS    CL10              Total for                                      
MX@TT034 DS    CL12              Job Transfer                                   
MX@SEQ   DS    CL2,CL1           Sq                                             
         EJECT                                                                  
**********************************************************************          
* TEMPORARY BILL TABLE                                                          
**********************************************************************          
TBTABD   DSECT                                                                  
TBTNO    DS    CL(L'PTARBLNO)                                                   
TBTDT    DS    CL(L'PTARBLDT)                                                   
TBTNET   DS    PL(L'PTANET)                                                     
TBTCOM   DS    PL(L'PTARCOM)                                                    
TBTLQ    EQU   *-TBTABD                                                         
         EJECT                                                                  
**********************************************************************          
* TEMPORARY CHECK TABLE                                                         
**********************************************************************          
TCTABD   DSECT                                                                  
TCTNO    DS    CL(L'CFCCNO)                                                     
TCTDT    DS    CL(L'CFCCDT)                                                     
TCTLQ    EQU   *-TCTABD                                                         
         EJECT                                                                  
**********************************************************************          
* TSAR RECORD. NOTE THIS RECORD MAY HAVE ELEMENTS ATTACHED                      
* POSSIBLE GENFILE ELEMENTS INCLUDE                                             
* CPJELD 'CLI/PRO/JOB' ELEMENT                                                  
* SPAELD 'SPECIAL POSTING ACCOUNT' ELEMENT                                      
* APEELD 'ANALYSIS POINTER' ELEMENT                                             
* OTHELD 'OTHERS' ELEMENT                                                       
* FFNELD 'FREE FORM NUMBER' ELEMENT                                             
* DUEELD 'DUE DATE' ELEMENT                                                     
* SCIELD 'SUBSIDIARY CASH' ELEMENT                                              
* AFPELD 'ARTISTE FEE PAYMENT' ELEMENT                                          
* MPYELD 'MANUAL PAYMENT' ELEMENT                                               
*                                                                               
* POSSIBLE INTERNAL ELEMENTS INCLUDE                                            
* NARELD 'TRANSACTION NARRATIVE ELEMENT                                         
**********************************************************************          
TSARDATD DSECT                      TSAR DATA ITEM                              
TSDLINES DS    XL1                                                              
TSDFMT   DS    CL1                                                              
TSITM    EQU   1                    ITEM TYPE                                   
TSDINVN  DS    CL20                 INVOICE NUMBER                              
TSDDATE  DS    PL3                  INVOICE DATE                                
TSDAMNT  DS    PL8                  INVOICE AMOUNT                              
TSDISC   DS    PL8                  CASH DISCOUNT                               
TSDOFF   DS    CL(L'TRNOFFC)        OFFICE                                      
TSDACDAT DS    XL(L'TRSDATE)        ACTIVITY DATE                               
TSDTMOA  DS    PL(L'TRNKSMOS)       MOA                                         
TSDBREF  DS    CL(L'TRNBREF)        BATCH REFERENCE                             
TSDTRST  DS    XL(L'TRNRSTAT)       TRANSACTION RECORD STATUS                   
TSDTEST  DS    XL(L'TRNSTAT)        TRANSACTION ELEMENT STATUS                  
TSDSEST  DS    XL(L'TRSSTAT)        TRANSACTION STATUS ELEMENT STATUS           
TSDSREF  DS    CL(L'TRNKSBR)        TRANSACTION SUB-REFERENCE NUMBER            
TSDDA    DS    XL4                  DISK ADDRESS                                
*                                                                               
TSDUNK   DS    CL3                  CASHFLOW INFORMATION UNKNOWN                
*                                                                               
TSDEXP   DS    CL14                 EXPENSE ACCOUNT                             
TSDULC   DS    0CL14                                                            
TSDUL    DS    CL2                                                              
TSDCPJ   DS    0CL12                CLIENT/PRODUCT/JOB                          
TSDJCLI  DS    CL3                  CLIENT                                      
TSDJPRD  DS    CL3                  PRODUCT                                     
TSDJJOB  DS    CL6                  JOB                                         
*                                                                               
TSDCSTAT DS    X                    CASHFLOW STATUS                             
TSDCIBQ  EQU   X'80'                . INVOICE BILLED                            
TSDCIPBQ EQU   X'40'                . INVOICE PARTIALLY BILLED                  
TSDCCPQ  EQU   X'20'                . CLIENT PAYMENT                            
TSDCPCQ  EQU   X'10'                . PARTIAL CLIENT PAYMENT                    
TSDCPBQ  EQU   X'08'                . PRE-BILL CLIENT PAYMENT                   
TSDCPBPQ EQU   X'04'                . PARTIAL PRE-BILL CLIENT PAYMENT           
TSDCJXFQ EQU   X'02'                . JOB TRANSFER                              
TSDDTAPP DS    PL3                  HIGHEST ESTIMATE REV APPROVAL DATE          
TSDRSTAT DS    X                    RECEIVABLE STATUS                           
TSDBNET  DS    PL8                  VENDOR INVOICE BILLED AMOUNT                
TSDCPA   DS    PL8                  CLIENT PAYMENT AMOUNT                       
TSDPBA   DS    PL8                  PRE-BILLED AMOUNT                           
TSDPBCPA DS    PL8                  PRE-BILLED CLIENT PAYMENT AMOUNT            
TSDCEST  DS    PL8                  CURRENT ESTIMATE                            
*                                                                               
TSDRFST  EQU   *                    FIRST ELEMENT ON TSAR RECORD                
TSDLENQ  EQU   *-TSARDATD                                                       
***********************************************************************         
* TSAR TOTAL LINE                                                               
***********************************************************************         
TSARTOTD DSECT                      COVER SCREEN LINE 1                         
TSTLINES DS    XL1                  NUMBER OF SCREEN LINES USED                 
TSTFMT   DS    CL1                  ITME FOMAT TYPE                             
TSSUBITM EQU   2                    SUBTOTAL ITEM TYPE                          
TSTOTITM EQU   3                    TOTAL ITEM TYPE                             
*                                                                               
TSTTOT   DS    PL8                  TRANSACTION TOTAL                           
TSTDISC  DS    PL8                  CASH DISCOUNT                               
TSTBNET  DS    PL8                  VENDOR INVOICE BILLED AMOUNT                
TSTCPA   DS    PL8                  CLIENT PAYMENT AMOUNT                       
TSTLNQ   EQU   *-TSARTOTD                                                       
         EJECT                                                                  
**********************************************************************          
       ++INCLUDE ACENQWORK                                                      
         EJECT                                                                  
**********************************************************************          
* TWA SAVE AREA                                                                 
**********************************************************************          
TWAD     DSECT                                                                  
         ORG   OSSAVE              OVERLAY SAVE AREA                            
DETFLAG  DS    X                   GENERAL FLAG FOR OVERLAYS                    
DETSNARQ EQU   X'80'               SUPPRESS NARRATIVE                           
DETCOLMQ EQU   X'40'               COLUMN OVERRIDE                              
DETGRINQ EQU   X'20'               SCREEN INTIALIZED FOR GRIDS                  
DETREADQ EQU   X'10'               NEED TO RE-READ RECORD                       
DETDATAQ EQU   X'08'               DATA FOUND                                   
DETMULTQ EQU   X'04'               MULTIPLE DATA LINES FOR INVOICE              
*                                                                               
VCVALS   DS    0PL8                                                             
VCTOTS   DS    0PL8                TOTAL VALUES                                 
TRNTOT   DS    PL8                 TRANSACTION TOTAL                            
TOTDISC  DS    PL8                 DISCOUNT TOTAL                               
TOTBNET  DS    PL8                 VENDOR INVOICE BILLED AMOUNT                 
TOTBCOM  DS    PL8                 VENDOR INVOICE BILLED COMMISSION             
TOTCPA   DS    PL8                 CLIENT PAYMENT AMOUNT                        
VCTOTLNQ EQU   *-VCTOTS                                                         
*                                                                               
VCSUBS   DS    0PL8                SUBTOTAL VALUES                              
SUBTRN   DS    PL8                 TRANSACTION SUBTOTAL                         
SUBDISC  DS    PL8                 DISCOUNT SUBTOTAL                            
SUBBNET  DS    PL8                 VENDOR INVOICE BILLED AMOUNT                 
SUBCPA   DS    PL8                 CLIENT PAYMENT AMOUNT                        
VCSUBLNQ EQU   *-VCSUBS                                                         
VCVALLNQ EQU   *-VCVALS                                                         
*                                                                               
SVINV    DS    CL(L'INVPINV)       INVOICE                                      
SVCPJ    DS    CL(L'TSDCPJ)        JOB                                          
SVAPPDT  DS    PL3                 HIGHEST ESTIMATE REV APPROVAL DATE           
SVCEST   DS    PL8                 CURRENT ESTIMATE                             
*                                                                               
SVINPKEY DS    CL(L'KEYSAVE)                                                    
SVINVKEY DS    CL(L'KEYSAVE)                                                    
SVCHGKEY DS    CL(L'KEYSAVE)                                                    
SVBILKEY DS    CL(L'KEYSAVE)                                                    
SVRCVKEY DS    CL(L'KEYSAVE)                                                    
*                                                                               
OSSNDQ   DS    XL(L'OSSAVE-(*-OSSAVE))   SPARE OVERLAY SAVE AREA                
OSSAVEX  DS    0H                                                               
**********************************************************************          
       ++INCLUDE SEACSFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACENQ14   01/07/15'                                      
         END                                                                    
